/* uPER checks of 007.asn1 (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2; Issue 151): fixed-size
   fragmented lengths (SIZE(N), N >= 64K). When the last fragment holds a multiple of 16K items, a final fragment that
   is a single length octet 0 follows (X.691 10.9.3.8.3 NOTE). Each row builds one stream from runs of fields; the
   decoder must reject it with the given error code and stop at the given bit position, or accept it, decode to the
   value the runs describe (Equal) and that value must encode to the same bytes (byte-exact; the valid streams are the
   asn1tools 0.169 encodings of the same values). Before the fix the encoders wrote no final octet and the decoders
   did not read it: every valid row with a final 00 failed, a following field was read from the final octet (W1) and
   any octet after the last block was accepted. The Ada twin 007_wire_test.adb checks the same rows (except the
   E rows: from a zero-padded buffer the Ada reads take the padding); the C results are the reference.
   With the argument --dump DIR every row's stream is written to DIR/<row number>.bin and described in DIR/index.txt
   (the Rust / Python / asn1tools cross-checks of the cycle use it). */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "007.h"

typedef enum { T_F64, T_F80, T_F112, T_F128, T_F70, T_B64, T_B80, T_S80, T_Q80, T_QE, T_QN, T_QR, T_W, T_WI } Ty;

static const char* const tyName[] = {"F64", "F80", "F112", "F128", "F70", "B64", "B80", "S80", "Q80", "QE", "QN",
                                     "QR", "W", "WI"};

/* n times the value v in nBits bits; comp = 0: a length / header field, comp = k > 0: n items of the k-th component
   (S80: the index in "A".."Z", QE / QR: the bits a (3) and b (1) of an Elm, QN: the index of the En value; W / WI:
   comp 1 = the octets of a, comp 2 = b) */
typedef struct {
    int comp;
    unsigned v;
    int nBits;
    long n;
} Seg;

#define H(x) {0, (x), 8, 1}
#define L2(n) {0, 0x8000u | (n), 16, 1}
#define I(v, w, n) {1, (v), (w), (n)}
#define C(k, v, w, n) {(k), (v), (w), (n)}

enum { UPER, ACN };
/* EXACT: rejected from a buffer of exactly the stream's length (no padding after it) */
enum { REJECT, ACCEPT, EXACT };

static int failures;
static FILE* dumpIndex;
static const char* dumpDir;
static int rowNo;

static void fail(const char* name, const char* what, long a, long b)
{
    printf("FAILED %s: %s (got %ld, expected %ld)\n", name, what, a, b);
    failures++;
}

static size_t type_size(Ty t)
{
    switch (t) {
    case T_F64: return sizeof(F64);
    case T_F80: return sizeof(F80);
    case T_F112: return sizeof(F112);
    case T_F128: return sizeof(F128);
    case T_F70: return sizeof(F70);
    case T_B64: return sizeof(B64);
    case T_B80: return sizeof(B80);
    case T_S80: return sizeof(S80);
    case T_Q80: return sizeof(Q80);
    case T_QE: return sizeof(QE);
    case T_QN: return sizeof(QN);
    case T_QR: return sizeof(QR);
    case T_W: return sizeof(W);
    case T_WI: return sizeof(WI);
    }
    return 0;
}

static long required_bytes(Ty t, int acn)
{
    switch (t) {
    case T_F64: return F64_REQUIRED_BYTES_FOR_ENCODING;
    case T_F80: return F80_REQUIRED_BYTES_FOR_ENCODING;
    case T_F112: return F112_REQUIRED_BYTES_FOR_ENCODING;
    case T_F128: return F128_REQUIRED_BYTES_FOR_ENCODING;
    case T_F70: return F70_REQUIRED_BYTES_FOR_ENCODING;
    case T_B64: return B64_REQUIRED_BYTES_FOR_ENCODING;
    case T_B80: return B80_REQUIRED_BYTES_FOR_ENCODING;
    case T_S80: return acn ? S80_REQUIRED_BYTES_FOR_ACN_ENCODING : S80_REQUIRED_BYTES_FOR_ENCODING;
    case T_Q80: return Q80_REQUIRED_BYTES_FOR_ENCODING;
    case T_QE: return QE_REQUIRED_BYTES_FOR_ENCODING;
    case T_QN: return QN_REQUIRED_BYTES_FOR_ENCODING;
    case T_QR: return QR_REQUIRED_BYTES_FOR_ENCODING;
    case T_W: return W_REQUIRED_BYTES_FOR_ENCODING;
    case T_WI: return WI_REQUIRED_BYTES_FOR_ENCODING;
    }
    return 0;
}

static flag decode(Ty t, int acn, void* v, BitStream* s, int* e)
{
    switch (t) {
    case T_F64: return F64_Decode(v, s, e);
    case T_F80: return F80_Decode(v, s, e);
    case T_F112: return F112_Decode(v, s, e);
    case T_F128: return F128_Decode(v, s, e);
    case T_F70: return F70_Decode(v, s, e);
    case T_B64: return B64_Decode(v, s, e);
    case T_B80: return B80_Decode(v, s, e);
    case T_S80: return acn ? S80_ACN_Decode(v, s, e) : S80_Decode(v, s, e);
    case T_Q80: return Q80_Decode(v, s, e);
    case T_QE: return QE_Decode(v, s, e);
    case T_QN: return QN_Decode(v, s, e);
    case T_QR: return QR_Decode(v, s, e);
    case T_W: return W_Decode(v, s, e);
    case T_WI: return WI_Decode(v, s, e);
    }
    return FALSE;
}

static flag encode(Ty t, int acn, const void* v, BitStream* s, int* e)
{
    switch (t) {
    case T_F64: return F64_Encode(v, s, e, TRUE);
    case T_F80: return F80_Encode(v, s, e, TRUE);
    case T_F112: return F112_Encode(v, s, e, TRUE);
    case T_F128: return F128_Encode(v, s, e, TRUE);
    case T_F70: return F70_Encode(v, s, e, TRUE);
    case T_B64: return B64_Encode(v, s, e, TRUE);
    case T_B80: return B80_Encode(v, s, e, TRUE);
    case T_S80: return acn ? S80_ACN_Encode(v, s, e, TRUE) : S80_Encode(v, s, e, TRUE);
    case T_Q80: return Q80_Encode(v, s, e, TRUE);
    case T_QE: return QE_Encode(v, s, e, TRUE);
    case T_QN: return QN_Encode(v, s, e, TRUE);
    case T_QR: return QR_Encode(v, s, e, TRUE);
    case T_W: return W_Encode(v, s, e, TRUE);
    case T_WI: return WI_Encode(v, s, e, TRUE);
    }
    return FALSE;
}

static flag equal(Ty t, const void* a, const void* b)
{
    switch (t) {
    case T_F64: return F64_Equal(a, b);
    case T_F80: return F80_Equal(a, b);
    case T_F112: return F112_Equal(a, b);
    case T_F128: return F128_Equal(a, b);
    case T_F70: return F70_Equal(a, b);
    case T_B64: return B64_Equal(a, b);
    case T_B80: return B80_Equal(a, b);
    case T_S80: return S80_Equal(a, b);
    case T_Q80: return Q80_Equal(a, b);
    case T_QE: return QE_Equal(a, b);
    case T_QN: return QN_Equal(a, b);
    case T_QR: return QR_Equal(a, b);
    case T_W: return W_Equal(a, b);
    case T_WI: return WI_Equal(a, b);
    }
    return FALSE;
}

static void set_bit(byte* arr, long i, unsigned v)
{
    if (v)
        arr[i / 8] |= (byte)(0x80 >> (i % 8));
}

/* set item i of component comp of the value v (which starts zeroed) */
static void add_item(Ty t, void* v, int comp, unsigned x, long i)
{
    switch (t) {
    case T_F64: ((F64*)v)->arr[i] = (byte)x; break;
    case T_F80: ((F80*)v)->arr[i] = (byte)x; break;
    case T_F112: ((F112*)v)->arr[i] = (byte)x; break;
    case T_F128: ((F128*)v)->arr[i] = (byte)x; break;
    case T_F70: ((F70*)v)->arr[i] = (byte)x; break;
    case T_B64: set_bit(((B64*)v)->arr, i, x); break;
    case T_B80: set_bit(((B80*)v)->arr, i, x); break;
    case T_S80: ((char*)v)[i] = (char)('A' + x); break;
    case T_Q80: ((Q80*)v)->arr[i] = x; break;
    case T_QE: ((QE*)v)->arr[i].a = x >> 1; ((QE*)v)->arr[i].b = (flag)(x & 1); break;
    case T_QN: ((QN*)v)->arr[i] = (En)x; break;
    case T_QR: ((QR*)v)->arr[i].a = x >> 1; ((QR*)v)->arr[i].b = (flag)(x & 1); break;
    case T_W:
        if (comp == 1)
            ((W*)v)->a.arr[i] = (byte)x;
        else
            ((W*)v)->b = x;
        break;
    case T_WI:
        if (comp == 1)
            ((WI*)v)->a.arr[i] = (byte)x;
        else
            ((WI*)v)->b = x;
        break;
    }
}

static void dump(const char* name, Ty t, int acn, int kind, int expErr, long expPos, const Seg* segs, size_t nSegs,
                 const byte* stream, long nBytes)
{
    char path[4096];
    snprintf(path, sizeof path, "%s/%03d.bin", dumpDir, rowNo);
    FILE* f = fopen(path, "wb");
    if (f == NULL || fwrite(stream, 1, (size_t)nBytes, f) != (size_t)nBytes) {
        printf("FAILED %s: cannot write %s\n", name, path);
        exit(1);
    }
    fclose(f);
    /* row|type|uper/acn|accept/reject/exact|error code|bit position|runs comp:v:bits:n,...|name */
    fprintf(dumpIndex, "%03d|%s|%s|%s|%d|%ld|", rowNo, tyName[t], acn ? "acn" : "uper",
            kind == ACCEPT ? "accept" : kind == EXACT ? "exact" : "reject", expErr, expPos);
    for (size_t k = 0; k < nSegs; k++)
        fprintf(dumpIndex, "%s%d:%u:%d:%ld", k ? "," : "", segs[k].comp, segs[k].v, segs[k].nBits, segs[k].n);
    fprintf(dumpIndex, "|%s\n", name);
}

static void check(const char* name, Ty t, int acn, int kind, int expErr, long expPos, const Seg* segs, size_t nSegs)
{
    long nBits = 0;
    for (size_t k = 0; k < nSegs; k++)
        nBits += segs[k].nBits * segs[k].n;
    long nBytes = (nBits + 7) / 8;
    long cap = kind == EXACT ? nBytes : (nBytes > required_bytes(t, acn) ? nBytes : required_bytes(t, acn));
    byte* stream = calloc((size_t)cap, 1);
    void* v = calloc(1, type_size(t));
    if (stream == NULL || v == NULL) {
        printf("FAILED %s: out of memory\n", name);
        exit(1);
    }
    long pos = 0;
    for (size_t k = 0; k < nSegs; k++)
        for (long j = 0; j < segs[k].n; j++)
            for (int b = segs[k].nBits - 1; b >= 0; b--, pos++)
                if ((segs[k].v >> b) & 1)
                    stream[pos / 8] |= (byte)(0x80 >> (pos % 8));
    rowNo++;
    if (dumpDir != NULL)
        dump(name, t, acn, kind, expErr, expPos, segs, nSegs, stream, nBytes);

    BitStream s;
    int e = 0;
    BitStream_AttachBuffer(&s, stream, cap);
    flag ok = decode(t, acn, v, &s, &e);
    long at = s.currentByte * 8 + s.currentBit;
    if (kind != ACCEPT) {
        if (ok)
            fail(name, "accepted", 1, 0);
        if (e != expErr)
            fail(name, "error code", e, expErr);
        if (at != expPos)
            fail(name, "bit position after the decoder", at, expPos);
    } else {
        if (nBits != expPos)
            fail(name, "row: stream bits", nBits, expPos);
        if (!ok)
            fail(name, "rejected, error code", e, 0);
        if (at != nBits)
            fail(name, "bit position after the decoder", at, nBits);
        /* the value the runs describe: Equal to the decoded one, and encoded byte for byte to the stream */
        void* x = calloc(1, type_size(t));
        long encCap = required_bytes(t, acn) + 8;
        byte* enc = calloc((size_t)encCap, 1);
        long count[3] = {0, 0, 0};
        if (x == NULL || enc == NULL) {
            printf("FAILED %s: out of memory\n", name);
            exit(1);
        }
        for (size_t k = 0; k < nSegs; k++)
            if (segs[k].comp > 0)
                for (long j = 0; j < segs[k].n; j++)
                    add_item(t, x, segs[k].comp, segs[k].v, count[segs[k].comp]++);
        if (ok && !equal(t, x, v))
            fail(name, "decoded value differs from the expected one", 0, 1);
        BitStream_Init(&s, enc, encCap);
        e = 0;
        if (!encode(t, acn, x, &s, &e))
            fail(name, "encode failed, error code", e, 0);
        at = s.currentByte * 8 + s.currentBit;
        if (at != nBits)
            fail(name, "encoded bits", at, nBits);
        else if (memcmp(enc, stream, (size_t)nBytes) != 0)
            fail(name, "encoded bytes differ from the stream", 0, 1);
        free(x);
        free(enc);
    }
    free(v);
    free(stream);
}

#define CHECK(name, t, acn, kind, err, pos, ...)                                                      \
    check(name, t, acn, kind, err, pos, (const Seg[]){__VA_ARGS__},                                  \
          sizeof((const Seg[]){__VA_ARGS__}) / sizeof(Seg))

/* the S80 rows that run through the uPER and the ACN decoder (ACN without a specification: the uPER layout) */
static void s80_rows(int acn, int err)
{
    CHECK("V8 S80 C4 C1 00", T_S80, acn, ACCEPT, 0, 409624, H(0xC4), I(3, 5, 65536), H(0xC1), I(25, 5, 16383),
          I(0, 5, 1), H(0x00));
    CHECK("Z8 S80 final octet 01", T_S80, acn, REJECT, err, 409624, H(0xC4), I(3, 5, 65536), H(0xC1),
          I(25, 5, 16384), H(0x01));
    CHECK("Z8 S80 final octet 7F", T_S80, acn, REJECT, err, 409624, H(0xC4), I(3, 5, 65536), H(0xC1),
          I(25, 5, 16384), H(0x7F));
    CHECK("Z8 S80 final octet 80", T_S80, acn, REJECT, err, 409624, H(0xC4), I(3, 5, 65536), H(0xC1),
          I(25, 5, 16384), H(0x80));
    CHECK("Z8 S80 final octet C1", T_S80, acn, REJECT, err, 409624, H(0xC4), I(3, 5, 65536), H(0xC1),
          I(25, 5, 16384), H(0xC1));
    CHECK("Z8 S80 final octet FF", T_S80, acn, REJECT, err, 409624, H(0xC4), I(3, 5, 65536), H(0xC1),
          I(25, 5, 16384), H(0xFF));
    CHECK("E8 S80 no final octet, exact buffer", T_S80, acn, EXACT, err, 409616, H(0xC4), I(3, 5, 65536), H(0xC1),
          I(25, 5, 16384));
}

/* the final octet 01, 7F, 80, C1, FF after the blocks of a row (the 00 of a valid stream replaced) */
#define FINAL_ROWS(nm, t, err, pos, ...)                                                                  \
    do {                                                                                                  \
        CHECK("Z " nm " final octet 01", t, UPER, REJECT, err, pos, __VA_ARGS__, H(0x01));                \
        CHECK("Z " nm " final octet 7F", t, UPER, REJECT, err, pos, __VA_ARGS__, H(0x7F));                \
        CHECK("Z " nm " final octet 80", t, UPER, REJECT, err, pos, __VA_ARGS__, H(0x80));                \
        CHECK("Z " nm " final octet C1", t, UPER, REJECT, err, pos, __VA_ARGS__, H(0xC1));                \
        CHECK("Z " nm " final octet FF", t, UPER, REJECT, err, pos, __VA_ARGS__, H(0xFF));                \
    } while (0)

int main(int argc, char** argv)
{
    if (argc == 3 && strcmp(argv[1], "--dump") == 0) {
        char path[4096];
        dumpDir = argv[2];
        snprintf(path, sizeof path, "%s/index.txt", dumpDir);
        dumpIndex = fopen(path, "w");
        if (dumpIndex == NULL) {
            printf("cannot write %s\n", path);
            return 1;
        }
    }

    /* valid: every fixed size that is a multiple of 16K ends with a final 00 (F70 / QR: the length of a rest) */
    CHECK("V1 F64 C4 00", T_F64, UPER, ACCEPT, 0, 524304, H(0xC4), I(0x5A, 8, 65535), I(0x01, 8, 1), H(0x00));
    CHECK("V2 F80 C4 C1 00", T_F80, UPER, ACCEPT, 0, 655384, H(0xC4), I(0x5A, 8, 65536), H(0xC1), I(0x02, 8, 16384),
          H(0x00));
    CHECK("V3 F112 C4 C3 00", T_F112, UPER, ACCEPT, 0, 917528, H(0xC4), I(0x5A, 8, 65536), H(0xC3),
          I(0x3C, 8, 49151), I(0xFF, 8, 1), H(0x00));
    CHECK("V4 F128 C4 C4 00", T_F128, UPER, ACCEPT, 0, 1048600, H(0xC4), I(0x5A, 8, 65536), H(0xC4),
          I(0xA5, 8, 65536), H(0x00));
    CHECK("V5 F70 C4 + 4464 (no final octet)", T_F70, UPER, ACCEPT, 0, 560024, H(0xC4), I(0x5A, 8, 65536), L2(4464),
          I(0xA5, 8, 4464));
    CHECK("V6 B64 C4 00", T_B64, UPER, ACCEPT, 0, 65552, H(0xC4), I(1, 1, 3), I(0, 1, 65532), I(1, 1, 1), H(0x00));
    CHECK("V7 B80 C4 C1 00", T_B80, UPER, ACCEPT, 0, 81944, H(0xC4), I(1, 1, 65536), H(0xC1), I(0, 1, 16383),
          I(1, 1, 1), H(0x00));
    CHECK("V9 Q80 C4 C1 00", T_Q80, UPER, ACCEPT, 0, 245784, H(0xC4), I(5, 3, 65536), H(0xC1), I(2, 3, 16384),
          H(0x00));
    CHECK("V10 QE C4 C4 C1 00", T_QE, UPER, ACCEPT, 0, 589856, H(0xC4), I(9, 4, 65536), H(0xC4), I(4, 4, 65536),
          H(0xC1), I(11, 4, 16383), I(1, 4, 1), H(0x00));
    CHECK("V11 QN C4 C1 00", T_QN, UPER, ACCEPT, 0, 163864, H(0xC4), I(2, 2, 65536), H(0xC1), I(1, 2, 16384),
          H(0x00));
    CHECK("V12 QR C4 + 4464 (no final octet)", T_QR, UPER, ACCEPT, 0, 280024, H(0xC4), I(7, 4, 65536), L2(4464),
          I(2, 4, 4464));
    CHECK("V13 W a = C4 C1 00, b = 5A", T_W, UPER, ACCEPT, 0, 655392, H(0xC4), C(1, 0x5A, 8, 65536), H(0xC1),
          C(1, 0x02, 8, 16384), H(0x00), C(2, 0x5A, 8, 1));
    CHECK("V14 WI a = C4 C1 00, b = 5A", T_WI, UPER, ACCEPT, 0, 655392, H(0xC4), C(1, 0x5A, 8, 65536), H(0xC1),
          C(1, 0x02, 8, 16384), H(0x00), C(2, 0x5A, 8, 1));
    s80_rows(UPER, ERR_UPER_DECODE_S80);
    s80_rows(ACN, ERR_ACN_DECODE_S80);

    /* a final octet other than 00: rejected with the type's error code, after the octet */
    FINAL_ROWS("F64", T_F64, ERR_UPER_DECODE_F64, 524304, H(0xC4), I(0x5A, 8, 65536));
    FINAL_ROWS("F80", T_F80, ERR_UPER_DECODE_F80, 655384, H(0xC4), I(0x5A, 8, 65536), H(0xC1), I(0x02, 8, 16384));
    FINAL_ROWS("F112", T_F112, ERR_UPER_DECODE_F112, 917528, H(0xC4), I(0x5A, 8, 65536), H(0xC3), I(0x3C, 8, 49152));
    FINAL_ROWS("F128", T_F128, ERR_UPER_DECODE_F128, 1048600, H(0xC4), I(0x5A, 8, 65536), H(0xC4),
               I(0xA5, 8, 65536));
    FINAL_ROWS("B64", T_B64, ERR_UPER_DECODE_B64, 65552, H(0xC4), I(1, 1, 65536));
    FINAL_ROWS("B80", T_B80, ERR_UPER_DECODE_B80, 81944, H(0xC4), I(1, 1, 65536), H(0xC1), I(0, 1, 16384));
    FINAL_ROWS("Q80", T_Q80, ERR_UPER_DECODE_Q80, 245784, H(0xC4), I(5, 3, 65536), H(0xC1), I(2, 3, 16384));
    /* elements with their own decoder (the Ada element decoder sets the error code to 0 when it succeeds) */
    FINAL_ROWS("QE", T_QE, ERR_UPER_DECODE_QE, 589856, H(0xC4), I(9, 4, 65536), H(0xC4), I(4, 4, 65536), H(0xC1),
               I(11, 4, 16384));
    FINAL_ROWS("QN", T_QN, ERR_UPER_DECODE_QN, 163864, H(0xC4), I(2, 2, 65536), H(0xC1), I(1, 2, 16384));
    /* a component: the error code of the referenced type (W) / of the inline component (WI) */
    FINAL_ROWS("W", T_W, ERR_UPER_DECODE_F80, 655384, H(0xC4), C(1, 0x5A, 8, 65536), H(0xC1), C(1, 0x02, 8, 16384));
    FINAL_ROWS("WI", T_WI, ERR_UPER_DECODE_WI_A, 655384, H(0xC4), C(1, 0x5A, 8, 65536), H(0xC1),
               C(1, 0x02, 8, 16384));

    /* the layout without the final octet (asn1scc up to 4.9.161.0): the next field's octet is taken as the final
       length (before: accepted with b = 0, position 8 bits early) */
    CHECK("W1 W old layout, b = 5A", T_W, UPER, REJECT, ERR_UPER_DECODE_F80, 655384, H(0xC4), C(1, 0x5A, 8, 65536),
          H(0xC1), C(1, 0x02, 8, 16384), C(2, 0x5A, 8, 1));
    CHECK("W2 WI old layout, b = 5A", T_WI, UPER, REJECT, ERR_UPER_DECODE_WI_A, 655384, H(0xC4),
          C(1, 0x5A, 8, 65536), H(0xC1), C(1, 0x02, 8, 16384), C(2, 0x5A, 8, 1));
    /* the old layout at the end of a buffer of exactly its length: nothing left to read */
    CHECK("E1 F64 no final octet, exact buffer", T_F64, UPER, EXACT, ERR_UPER_DECODE_F64, 524296, H(0xC4),
          I(0x5A, 8, 65536));
    CHECK("E2 F80 no final octet, exact buffer", T_F80, UPER, EXACT, ERR_UPER_DECODE_F80, 655376, H(0xC4),
          I(0x5A, 8, 65536), H(0xC1), I(0x02, 8, 16384));
    CHECK("E3 QE no final octet, exact buffer", T_QE, UPER, EXACT, ERR_UPER_DECODE_QE, 589848, H(0xC4),
          I(9, 4, 65536), H(0xC4), I(4, 4, 65536), H(0xC1), I(11, 4, 16384));

    /* other headers (unchanged behaviour, pins the codes and positions): a wrong 64K header, a wrong small block
       header, a wrong length of the rest; QE / QR after a block of elements with their own decoder */
    CHECK("H1 F80 first header C3", T_F80, UPER, REJECT, ERR_UPER_DECODE_F80, 8, H(0xC3), I(0x5A, 8, 65536));
    CHECK("H2 F80 small block header C2", T_F80, UPER, REJECT, ERR_UPER_DECODE_F80, 524304, H(0xC4),
          I(0x5A, 8, 65536), H(0xC2), I(0x02, 8, 16384), H(0x00));
    CHECK("H3 F80 small block header 00", T_F80, UPER, REJECT, ERR_UPER_DECODE_F80, 524304, H(0xC4),
          I(0x5A, 8, 65536), H(0x00));
    CHECK("H4 F70 rest length 4463", T_F70, UPER, REJECT, ERR_UPER_DECODE_F70, 524312, H(0xC4), I(0x5A, 8, 65536),
          L2(4463), I(0xA5, 8, 4463));
    CHECK("H5 F70 one-octet rest length", T_F70, UPER, REJECT, ERR_UPER_DECODE_F70, 524312, H(0xC4),
          I(0x5A, 8, 65536), H(0x7F), I(0xA5, 8, 4464));
    CHECK("H6 QE second 64K header C1", T_QE, UPER, REJECT, ERR_UPER_DECODE_QE, 262160, H(0xC4), I(9, 4, 65536),
          H(0xC1), I(4, 4, 65536));
    CHECK("H7 QE small block header C2", T_QE, UPER, REJECT, ERR_UPER_DECODE_QE, 524312, H(0xC4), I(9, 4, 65536),
          H(0xC4), I(4, 4, 65536), H(0xC2), I(11, 4, 16384), H(0x00));
    CHECK("H8 QN small block header C2", T_QN, UPER, REJECT, ERR_UPER_DECODE_QN, 131088, H(0xC4), I(2, 2, 65536),
          H(0xC2), I(1, 2, 16384), H(0x00));
    CHECK("H9 QR rest length 4465", T_QR, UPER, REJECT, ERR_UPER_DECODE_QR, 262168, H(0xC4), I(7, 4, 65536),
          L2(4465), I(2, 4, 4465));
    /* an element that fails (Elm.a = 6, En = 3): the element's error code, before the final octet */
    CHECK("H10 QE last element a = 6", T_QE, UPER, REJECT, ERR_UPER_DECODE_ELM_A, 589847, H(0xC4), I(9, 4, 65536),
          H(0xC4), I(4, 4, 65536), H(0xC1), I(11, 4, 16383), I(12, 4, 1), H(0x00));
    CHECK("H11 QN last element 3", T_QN, UPER, REJECT, ERR_UPER_DECODE_EN, 163856, H(0xC4), I(2, 2, 65536), H(0xC1),
          I(1, 2, 16383), I(3, 2, 1), H(0x00));

    if (dumpIndex != NULL)
        fclose(dumpIndex);
    if (failures != 0) {
        printf("%d FAILED checks\n", failures);
        return 1;
    }
    printf("uper-wire 007: %d rows OK\n", rowNo);
    return 0;
}
