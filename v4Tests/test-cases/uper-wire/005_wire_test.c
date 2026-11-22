/* uPER checks of 005.asn1 (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2; Issue 154): the general length
   (X.691 10.9) of an OCTET / BIT STRING (CONTAINING X) of 16K units or more. Each row encodes the contents value
   (the region), then lays the region out in the stream behind the given length fields: H(x) one header / length
   octet, L2(n) a two-octet length, P(k) the next k units of the region (octets; bits for B32K; zeros past the end of
   the region). The decoder must reject the stream with the given error code and stop at the given bit position, or
   accept it, decode the contents value and encode it to the same bytes (byte-exact). The Ada twin 005_wire_test.adb
   checks the same rows; its decoder restores the position to the start of the CONTAINING when a fragmented length is
   rejected (its own column there). */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "005.h"

typedef enum { T_C32K, T_B32K, T_CV, T_B80K } Ty;

typedef struct {
    int kind; /* 0: H, 1: L2, 2: P */
    long v;
} Seg;

#define H(x) {0, (x)}
#define L2(n) {1, (n)}
#define P(k) {2, (k)}

enum { REJECT, ACCEPT };

static int failures;

static void fail(const char* name, const char* what, long a, long b)
{
    printf("FAILED %s: %s (got %ld, expected %ld)\n", name, what, a, b);
    failures++;
}

/* the contents value: octets (i * 7 + seed) mod 256; CV with n octets */
static void make_value(Ty t, void* x, long n, int seed)
{
    switch (t) {
    case T_C32K:
        for (int i = 0; i < 32768; i++)
            ((C32K*)x)->arr[i] = (byte)(i * 7 + seed);
        break;
    case T_B32K:
        for (int i = 0; i < 4096; i++)
            ((B32K*)x)->arr[i] = (byte)(i * 7 + seed);
        break;
    case T_CV:
        ((CV*)x)->nCount = (int)n;
        for (int i = 0; i < n; i++)
            ((CV*)x)->arr[i] = (byte)(i * 7 + seed);
        break;
    case T_B80K:
        for (int i = 0; i < 10240; i++)
            ((B80K*)x)->arr[i] = (byte)(i * 7 + seed);
        break;
    }
}

static void check(const char* name, Ty t, long n, int expectAccept, int expErr, long expPos, const Seg* segs,
                  size_t nSegs)
{
    static C32K x32;
    static B32K xb;
    static CV xv;
    static C32K d32;
    static B32K db;
    static CV dv;
    static B80K x80;
    static B80K d80;
    void* x = t == T_C32K ? (void*)&x32 : t == T_B32K ? (void*)&xb : t == T_CV ? (void*)&xv : (void*)&x80;
    void* d = t == T_C32K ? (void*)&d32 : t == T_B32K ? (void*)&db : t == T_CV ? (void*)&dv : (void*)&d80;
    int unitBits = t == T_B32K || t == T_B80K ? 1 : 8;
    memset(x, 0, t == T_C32K ? sizeof x32 : t == T_B32K ? sizeof xb : t == T_CV ? sizeof xv : sizeof x80);
    memset(d, 0, t == T_C32K ? sizeof d32 : t == T_B32K ? sizeof db : t == T_CV ? sizeof dv : sizeof d80);
    make_value(t, x, n, 1);

    /* the region: the contents' own encoding */
    static byte region[40000];
    memset(region, 0, sizeof region);
    BitStream s;
    int e = 0;
    BitStream_Init(&s, region, sizeof region);
    flag ok = t == T_C32K ? O32768_Encode(x, &s, &e, TRUE)
            : t == T_B32K ? O4096_Encode(x, &s, &e, TRUE)
            : t == T_CV   ? OV_Encode(x, &s, &e, TRUE)
                          : O10240_Encode(x, &s, &e, TRUE);
    if (!ok) {
        fail(name, "contents encode failed, error code", e, 0);
        return;
    }
    long regionBits = (long)sizeof region * 8;

    static byte stream[40000];
    memset(stream, 0, sizeof stream);
    long pos = 0;
    long rpos = 0;
    for (size_t k = 0; k < nSegs; k++) {
        long v = segs[k].v;
        if (segs[k].kind == 0) {
            stream[pos / 8] = (byte)v;
            pos += 8;
        } else if (segs[k].kind == 1) {
            stream[pos / 8] = (byte)(0x80 | (v >> 8));
            stream[pos / 8 + 1] = (byte)v;
            pos += 16;
        } else {
            for (long b = 0; b < v * unitBits; b++, pos++, rpos++)
                if (rpos < regionBits && ((region[rpos / 8] >> (7 - rpos % 8)) & 1))
                    stream[pos / 8] |= (byte)(0x80 >> (pos % 8));
        }
    }
    long nBytes = (pos + 7) / 8;

    BitStream_AttachBuffer(&s, stream, sizeof stream);
    e = 0;
    ok = t == T_C32K ? C32K_Decode(d, &s, &e)
       : t == T_B32K ? B32K_Decode(d, &s, &e)
       : t == T_CV   ? CV_Decode(d, &s, &e)
                     : B80K_Decode(d, &s, &e);
    long at = s.currentByte * 8 + s.currentBit;
    if (expectAccept == REJECT) {
        if (ok)
            fail(name, "accepted", 1, 0);
        if (e != expErr)
            fail(name, "error code", e, expErr);
        if (at != expPos)
            fail(name, "bit position after the decoder", at, expPos);
        return;
    }
    if (pos != expPos)
        fail(name, "row: stream bits", pos, expPos);
    if (!ok)
        fail(name, "rejected, error code", e, 0);
    if (at != pos)
        fail(name, "bit position after the decoder", at, pos);
    if (ok && !(t == T_C32K ? C32K_Equal(x, d)
                : t == T_B32K ? B32K_Equal(x, d)
                : t == T_CV   ? CV_Equal(x, d)
                              : B80K_Equal(x, d)))
        fail(name, "decoded value differs from the expected one", 0, 1);
    static byte enc[40000];
    memset(enc, 0, sizeof enc);
    BitStream_Init(&s, enc, sizeof enc);
    e = 0;
    ok = t == T_C32K ? C32K_Encode(x, &s, &e, TRUE)
       : t == T_B32K ? B32K_Encode(x, &s, &e, TRUE)
       : t == T_CV   ? CV_Encode(x, &s, &e, TRUE)
                     : B80K_Encode(x, &s, &e, TRUE);
    if (!ok)
        fail(name, "encode failed, error code", e, 0);
    at = s.currentByte * 8 + s.currentBit;
    if (at != pos)
        fail(name, "encoded bits", at, pos);
    else if (memcmp(enc, stream, (size_t)nBytes) != 0)
        fail(name, "encoded bytes differ from the stream", 0, 1);
}

#define CHECK(name, t, n, accept, err, pos, ...)                                                      \
    check(name, t, n, accept, err, pos, (const Seg[]){__VA_ARGS__},                                  \
          sizeof((const Seg[]){__VA_ARGS__}) / sizeof(Seg))

int main(void)
{
    /* the canonical layouts (C32K / B32K: 32768 units = C2 + final length 00; CV: a final length after a C1, in one
       and in two octets) */
    CHECK("V1 C32K C2 00", T_C32K, 0, ACCEPT, 0, 262160, H(0xC2), P(32768), H(0x00));
    CHECK("V2 B32K C2 00 (bits)", T_B32K, 0, ACCEPT, 0, 32784, H(0xC2), P(32768), H(0x00));
    CHECK("V3 CV 32766 octets: region C2 00", T_CV, 32766, ACCEPT, 0, 262160, H(0xC2), P(32768), H(0x00));
    CHECK("V4 CV 16384 octets: region C1 + 02", T_CV, 16384, ACCEPT, 0, 131104, H(0xC1), P(16384), H(0x02), P(2));
    CHECK("V5 CV 16510 octets: region C1 + 80 80", T_CV, 16510, ACCEPT, 0, 132120,
          H(0xC1), P(16384), L2(128), P(128));
    /* (1) a fragment header after a fragment of 16K .. 48K units: the same region, cut into two 16K fragments */
    CHECK("J1 C32K C1 C1 00", T_C32K, 0, REJECT, ERR_UPER_DECODE_C32K_2, 131088,
          H(0xC1), P(16384), H(0xC1), P(16384), H(0x00));
    CHECK("J2 B32K C1 C1 00 (bits)", T_B32K, 0, REJECT, ERR_UPER_DECODE_B32K_2, 16400,
          H(0xC1), P(16384), H(0xC1), P(16384), H(0x00));
    CHECK("J3 CV 32766 octets: region C1 C1 00", T_CV, 32766, REJECT, ERR_UPER_DECODE_CV_2, 131088,
          H(0xC1), P(16384), H(0xC1), P(16384), H(0x00));
    /* (2) a two-octet length below 128: after a C1, and alone */
    CHECK("J4 CV 16384 octets: region C1 + 80 02", T_CV, 16384, REJECT, ERR_UPER_DECODE_CV_2, 131096,
          H(0xC1), P(16384), L2(2), P(2));
    CHECK("J5 CV two-octet 5", T_CV, 16384, REJECT, ERR_UPER_DECODE_CV_2, 16, L2(5), P(5));
    /* a fragment header after a 64K fragment (C4): a C1 and a final length 0 fill B80K's 81920 bits exactly; one bit
       more is over the maximum */
    CHECK("V6 B80K C4 C1 00 (bits)", T_B80K, 0, ACCEPT, 0, 81944, H(0xC4), P(65536), H(0xC1), P(16384), H(0x00));
    CHECK("J6 B80K C4 C1 01 (over the maximum)", T_B80K, 0, REJECT, ERR_UPER_DECODE_B80K_2, 81944,
          H(0xC4), P(65536), H(0xC1), P(16384), H(0x01), P(1));

    if (failures != 0) {
        printf("uper-wire 005: %d checks failed\n", failures);
        return 1;
    }
    return 0;
}
