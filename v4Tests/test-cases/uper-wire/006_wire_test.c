/* uPER and ACN checks of 006.asn1 (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2; Issues 155 / 156):
   OBJECT IDENTIFIER (T), RELATIVE-OID (R) and an OBJECT IDENTIFIER followed by another component (S). Each row gives
   the stream as hex tokens ("<hex>*n" = the token n times) and, for a valid stream, the arcs ("A9" = 2^62, "A10" =
   2^63, "M" = 2^64 - 1, "*n" = n times) and the tail of S. Every row is decoded by the uPER and by the ACN decoder
   from a buffer of max(stream, REQUIRED_BYTES) octets (zero padded): a rejected stream must fail with the type's error
   code (S: the one of its component o) and stop at the given bit position; a valid one must decode to the arcs, stop
   at the end of the stream, and the arcs must encode to the same bytes (byte-exact). The Ada twin 006_wire_test.adb
   checks the same rows. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "006.h"

enum { REJECT, ACCEPT };

static int failures;

static void fail(const char* name, const char* codec, const char* what, long a, long b)
{
    printf("FAILED %s (%s): %s (got %ld, expected %ld)\n", name, codec, what, a, b);
    failures++;
}

static int hexval(char c)
{
    return c <= '9' ? c - '0' : (c | 0x20) - 'a' + 10;
}

/* "<hex>[*n] ..." -> octets */
static size_t parse_stream(const char* spec, byte* out)
{
    size_t n = 0;
    const char* p = spec;
    while (*p) {
        while (*p == ' ')
            p++;
        const char* q = p;
        while (*q && *q != ' ' && *q != '*')
            q++;
        long rep = 1;
        if (*q == '*')
            rep = strtol(q + 1, NULL, 10);
        for (long r = 0; r < rep; r++)
            for (const char* h = p; h < q; h += 2)
                out[n++] = (byte)(hexval(h[0]) * 16 + hexval(h[1]));
        while (*q && *q != ' ')
            q++;
        p = q;
    }
    return n;
}

/* "<arc>[*n] ..." -> arcs */
static int parse_arcs(const char* spec, asn1SccUint* out)
{
    int n = 0;
    char buf[64];
    const char* p = spec;
    while (*p) {
        while (*p == ' ')
            p++;
        const char* q = p;
        while (*q && *q != ' ' && *q != '*')
            q++;
        memcpy(buf, p, (size_t)(q - p));
        buf[q - p] = '\0';
        asn1SccUint v = strcmp(buf, "A9") == 0    ? 1ULL << 62
                      : strcmp(buf, "A10") == 0 ? 1ULL << 63
                      : strcmp(buf, "M") == 0   ? 0xFFFFFFFFFFFFFFFFULL
                                                : strtoull(buf, NULL, 10);
        long rep = 1;
        if (*q == '*')
            rep = strtol(q + 1, NULL, 10);
        for (long r = 0; r < rep; r++)
            out[n++] = v;
        while (*q && *q != ' ')
            q++;
        p = q;
    }
    return n;
}

static void row(const char* name, char typ, int expect, long expPos, const char* hex, const char* arcs, int tail)
{
    static byte stream[400];
    memset(stream, 0, sizeof stream);
    size_t n = parse_stream(hex, stream);
    T x;
    S xs;
    memset(&x, 0, sizeof x);
    memset(&xs, 0, sizeof xs);
    x.nCount = parse_arcs(arcs, x.values);
    xs.o = x;
    xs.tail = (asn1SccUint)tail;
    if (expect == ACCEPT && (long)n * 8 != expPos)
        fail(name, "row", "stream bits", (long)n * 8, expPos);
    for (int acn = 0; acn < 2; acn++) {
        const char* codec = acn ? "ACN" : "uPER";
        size_t req = typ == 'S' ? (acn ? S_REQUIRED_BYTES_FOR_ACN_ENCODING : S_REQUIRED_BYTES_FOR_ENCODING)
                   : typ == 'R' ? (acn ? R_REQUIRED_BYTES_FOR_ACN_ENCODING : R_REQUIRED_BYTES_FOR_ENCODING)
                                : (acn ? T_REQUIRED_BYTES_FOR_ACN_ENCODING : T_REQUIRED_BYTES_FOR_ENCODING);
        size_t cap = n > req ? n : req;
        byte* buf = calloc(cap, 1);
        memcpy(buf, stream, n);
        BitStream s;
        BitStream_AttachBuffer(&s, buf, cap);
        int e = 0;
        flag ok;
        T d;
        S ds;
        memset(&d, 0, sizeof d);
        memset(&ds, 0, sizeof ds);
        int expErr;
        switch (typ) {
        case 'T':
            ok = acn ? T_ACN_Decode(&d, &s, &e) : T_Decode(&d, &s, &e);
            expErr = acn ? ERR_ACN_DECODE_T : ERR_UPER_DECODE_T;
            break;
        case 'R':
            ok = acn ? R_ACN_Decode(&d, &s, &e) : R_Decode(&d, &s, &e);
            expErr = acn ? ERR_ACN_DECODE_R : ERR_UPER_DECODE_R;
            break;
        default:
            ok = acn ? S_ACN_Decode(&ds, &s, &e) : S_Decode(&ds, &s, &e);
            expErr = acn ? ERR_ACN_DECODE_S_O : ERR_UPER_DECODE_S_O;
            d = ds.o;
            break;
        }
        long at = s.currentByte * 8L + s.currentBit;
        if (expect == REJECT) {
            if (ok)
                fail(name, codec, "accepted", 1, 0);
            if (e != expErr)
                fail(name, codec, "error code", e, expErr);
            if (at != expPos)
                fail(name, codec, "bit position after the decoder", at, expPos);
            free(buf);
            continue;
        }
        if (!ok)
            fail(name, codec, "rejected, error code", e, 0);
        if (at != expPos)
            fail(name, codec, "bit position after the decoder", at, expPos);
        if (ok && !T_Equal(&x, &d))
            fail(name, codec, "decoded arcs differ (count)", d.nCount, x.nCount);
        if (ok && typ == 'S' && ds.tail != xs.tail)
            fail(name, codec, "decoded tail", (long)ds.tail, (long)xs.tail);
        memset(buf, 0, cap);
        BitStream_Init(&s, buf, cap);
        e = 0;
        switch (typ) {
        case 'T':
            ok = acn ? T_ACN_Encode(&x, &s, &e, TRUE) : T_Encode(&x, &s, &e, TRUE);
            break;
        case 'R':
            ok = acn ? R_ACN_Encode(&x, &s, &e, TRUE) : R_Encode(&x, &s, &e, TRUE);
            break;
        default:
            ok = acn ? S_ACN_Encode(&xs, &s, &e, TRUE) : S_Encode(&xs, &s, &e, TRUE);
            break;
        }
        at = s.currentByte * 8L + s.currentBit;
        if (!ok)
            fail(name, codec, "encode failed, error code", e, 0);
        if (at != expPos)
            fail(name, codec, "encoded bits", at, expPos);
        else if (memcmp(buf, stream, n) != 0)
            fail(name, codec, "encoded bytes differ from the stream", 0, 1);
        free(buf);
    }
}

int main(void)
{
    /* valid values: one- and two-octet contents lengths up to the largest values (190 / 200 octets), largest arcs */
    row("A1 T 127 octets: length 7F", 'T', ACCEPT, 1024, "7f 2a c08080808080808000*14", "1 2 A9*14", -1);
    row("A2 T 128 octets: length 80 80", 'T', ACCEPT, 1040, "8080 2a c08080808080808000*14 05", "1 2 A9*14 5", -1);
    row("A3 T 129 octets", 'T', ACCEPT, 1048, "8081 2a c08080808080808000*14 05 06", "1 2 A9*14 5 6", -1);
    row("A4 T 163 octets, 20 arcs", 'T', ACCEPT, 1320, "80a3 2a c08080808080808000*18", "1 2 A9*18", -1);
    row("A5 T 189 octets", 'T', ACCEPT, 1528, "80bd 81808080808080808000*18 c08080808080808000", "2 9223372036854775728 A10*17 A9", -1);
    row("A6 T 190 octets (largest OID contents)", 'T', ACCEPT, 1536, "80be 81808080808080808050 81808080808080808000*18", "2 A10*19", -1);
    row("A7 T 190 octets, largest arcs", 'T', ACCEPT, 1536, "80be 81ffffffffffffffff7f*19", "2 18446744073709551535 M*18", -1);
    row("A8 R 127 octets", 'R', ACCEPT, 1024, "7f c08080808080808000*14 07", "A9*14 7", -1);
    row("A9 R 128 octets: length 80 80", 'R', ACCEPT, 1040, "8080 c08080808080808000*14 07 08", "A9*14 7 8", -1);
    row("A10 R 191 octets", 'R', ACCEPT, 1544, "80bf 81808080808080808000*19 03", "A10*19 3", -1);
    row("A11 R 199 octets", 'R', ACCEPT, 1608, "80c7 81808080808080808000*19 c08080808080808000", "A10*19 A9", -1);
    row("A12 R 200 octets (largest RELATIVE-OID contents)", 'R', ACCEPT, 1616, "80c8 81808080808080808000*20", "A10*20", -1);
    row("A13 R 200 octets, largest arcs", 'R', ACCEPT, 1616, "80c8 81ffffffffffffffff7f*20", "M*20", -1);
    row("A14 S o of 128 octets, tail A5", 'S', ACCEPT, 1048, "8080 2a c08080808080808000*14 05 a5", "1 2 A9*14 5", 165);
    /* small values, 20 arcs, arcs of 2^64 - 1, 0x80 octets inside a subidentifier */
    row("A15 T {1 2}", 'T', ACCEPT, 16, "01 2a", "1 2", -1);
    row("A16 T 20 arcs of one octet", 'T', ACCEPT, 160, "13 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13", "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19", -1);
    row("A17 R 20 arcs of one octet", 'R', ACCEPT, 168, "14 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14", "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20", -1);
    row("A18 R arc 2^64 - 1", 'R', ACCEPT, 88, "0a 81ffffffffffffffff7f", "M", -1);
    row("A19 T first subidentifier 2^64 - 1", 'T', ACCEPT, 88, "0a 81ffffffffffffffff7f", "2 18446744073709551535", -1);
    row("A20 R arc 2^32", 'R', ACCEPT, 48, "05 9080808000", "4294967296", -1);
    row("A21 R 0x80 inside a subidentifier", 'R', ACCEPT, 32, "03 818000", "16384", -1);
    row("A22 T 0x80 inside the first subidentifier", 'T', ACCEPT, 32, "03 818000", "2 16304", -1);
    row("A23 T 0x80 inside a subidentifier", 'T', ACCEPT, 48, "05 2a 81808000", "1 2 2097152", -1);
    /* a two-octet length below 128 (X.691 10.9.3.6, Issue 156): rejected after the second octet */
    row("N1 T 80 03", 'T', REJECT, 16, "80032a0304", "", -1);
    row("N2 T 80 7F + 127 octets", 'T', REJECT, 16, "807f 2a c08080808080808000*14", "", -1);
    row("N3 T 80 00", 'T', REJECT, 16, "8000", "", -1);
    row("N4 T 80 01", 'T', REJECT, 16, "80012a", "", -1);
    row("N5 R 80 01", 'R', REJECT, 16, "800105", "", -1);
    row("N6 R 80 7F + 127 octets", 'R', REJECT, 16, "807f 01*127", "", -1);
    row("N7 S 80 03", 'S', REJECT, 16, "80032a0304a5", "", -1);
    /* a fragment header (11xxxxxx): no value of at most 20 arcs has 16K contents octets; rejected after it */
    row("F1 T C0", 'T', REJECT, 8, "c0002a", "", -1);
    row("F2 T C1", 'T', REJECT, 8, "c1 00*16", "", -1);
    row("F3 T C4", 'T', REJECT, 8, "c42a", "", -1);
    row("F4 T E0", 'T', REJECT, 8, "e02a", "", -1);
    row("F5 R FE", 'R', REJECT, 8, "fe01", "", -1);
    row("F6 R FF FF", 'R', REJECT, 8, "ffff", "", -1);
    /* lengths no value reaches, more than 20 arcs: rejected after the 20th arc */
    row("L1 R 81 00 + 256 octets", 'R', REJECT, 176, "8100 01*256", "", -1);
    row("L2 T 81 00 + 256 octets", 'T', REJECT, 168, "8100 01*256", "", -1);
    row("L3 T BF FF + 300 octets", 'T', REJECT, 168, "bfff 01*300", "", -1);
    row("L4 R 21 arcs", 'R', REJECT, 168, "15 01*21", "", -1);
    row("L5 T 21 arcs", 'T', REJECT, 160, "14 01*20", "", -1);
    row("L6 T 80 BE + 128 octets (zero arcs follow)", 'T', REJECT, 1064, "80be 2a c08080808080808000*14 05", "", -1);
    /* a length of 0 (no subidentifier) */
    row("E1 T 00", 'T', REJECT, 8, "00", "", -1);
    row("E2 R 00", 'R', REJECT, 8, "00", "", -1);
    /* the contents length ends inside a subidentifier */
    row("E3 T 02 2A 85", 'T', REJECT, 24, "022a85", "", -1);
    row("E4 R 01 85", 'R', REJECT, 16, "0185", "", -1);
    row("E5 T 80 80 + 190 octets (length ends inside an arc)", 'T', REJECT, 1040, "8080 81808080808080808050 81808080808080808000*18", "", -1);
    /* an arc over 64 bits: rejected at the octet that overflows */
    row("E6 R arc 2^64", 'R', REJECT, 88, "0a 82808080808080808000", "", -1);
    row("E7 T first subidentifier 2^64", 'T', REJECT, 88, "0a 82808080808080808000", "", -1);
    row("E8 R arc 2^70", 'R', REJECT, 96, "0b 8180808080808080808000", "", -1);
    row("E9 R 80 80 + 128 continuation octets", 'R', REJECT, 104, "8080 81*128", "", -1);

    if (failures != 0) {
        printf("uper-wire 006: %d checks failed\n", failures);
        return 1;
    }
    return 0;
}
