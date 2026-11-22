/* uPER checks of 004.asn1 (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2; Issue 152): fragmented lengths
   (X.691 11.9.3.8) of variable-size types whose maximum is 64K or more. Each row builds one stream from runs of
   fields; the decoder must reject it with the given error code and stop at the given bit position, or accept it,
   decode to the value the runs describe (Equal) and that value must encode to the same bytes (byte-exact). The Ada
   twin 004_wire_test.adb checks the same rows; the C results are the reference. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "004.h"

typedef enum { T_VOCT, T_VSTR, T_VBIT, T_VSEQ, T_VOCTM, T_VOCTN, T_VBIT3, T_VSEQR, T_POCT, T_PINL } Ty;

/* n times the value v in nBits bits; comp = 0: a length / header field, comp = k > 0: n items of the k-th
   variable-size component (the value: VStr / PInl.s the index in "A".."Z", VSeqR the bits a (3) and b (1)) */
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
enum { REJECT, ACCEPT };

#define TAIL_FILL 0xC3

static int failures;

static void fail(const char* name, const char* what, long a, long b)
{
    printf("FAILED %s: %s (got %ld, expected %ld)\n", name, what, a, b);
    failures++;
}

static size_t type_size(Ty t)
{
    switch (t) {
    case T_VOCT: return sizeof(VOct);
    case T_VSTR: return sizeof(VStr);
    case T_VBIT: return sizeof(VBit);
    case T_VSEQ: return sizeof(VSeq);
    case T_VOCTM: return sizeof(VOctM);
    case T_VOCTN: return sizeof(VOctN);
    case T_VBIT3: return sizeof(VBit3);
    case T_VSEQR: return sizeof(VSeqR);
    case T_POCT: return sizeof(POct);
    case T_PINL: return sizeof(PInl);
    }
    return 0;
}

static long required_bytes(Ty t, int acn)
{
    switch (t) {
    case T_VOCT: return VOct_REQUIRED_BYTES_FOR_ENCODING;
    case T_VSTR: return acn ? VStr_REQUIRED_BYTES_FOR_ACN_ENCODING : VStr_REQUIRED_BYTES_FOR_ENCODING;
    case T_VBIT: return VBit_REQUIRED_BYTES_FOR_ENCODING;
    case T_VSEQ: return VSeq_REQUIRED_BYTES_FOR_ENCODING;
    case T_VOCTM: return VOctM_REQUIRED_BYTES_FOR_ENCODING;
    case T_VOCTN: return VOctN_REQUIRED_BYTES_FOR_ENCODING;
    case T_VBIT3: return VBit3_REQUIRED_BYTES_FOR_ENCODING;
    case T_VSEQR: return VSeqR_REQUIRED_BYTES_FOR_ENCODING;
    case T_POCT: return POct_REQUIRED_BYTES_FOR_ENCODING;
    case T_PINL: return PInl_REQUIRED_BYTES_FOR_ENCODING;
    }
    return 0;
}

static flag decode(Ty t, int acn, void* v, BitStream* s, int* e)
{
    switch (t) {
    case T_VOCT: return VOct_Decode(v, s, e);
    case T_VSTR: return acn ? VStr_ACN_Decode(v, s, e) : VStr_Decode(v, s, e);
    case T_VBIT: return VBit_Decode(v, s, e);
    case T_VSEQ: return VSeq_Decode(v, s, e);
    case T_VOCTM: return VOctM_Decode(v, s, e);
    case T_VOCTN: return VOctN_Decode(v, s, e);
    case T_VBIT3: return VBit3_Decode(v, s, e);
    case T_VSEQR: return VSeqR_Decode(v, s, e);
    case T_POCT: return POct_Decode(v, s, e);
    case T_PINL: return PInl_Decode(v, s, e);
    }
    return FALSE;
}

static flag encode(Ty t, int acn, const void* v, BitStream* s, int* e)
{
    switch (t) {
    case T_VOCT: return VOct_Encode(v, s, e, TRUE);
    case T_VSTR: return acn ? VStr_ACN_Encode(v, s, e, TRUE) : VStr_Encode(v, s, e, TRUE);
    case T_VBIT: return VBit_Encode(v, s, e, TRUE);
    case T_VSEQ: return VSeq_Encode(v, s, e, TRUE);
    case T_VOCTM: return VOctM_Encode(v, s, e, TRUE);
    case T_VOCTN: return VOctN_Encode(v, s, e, TRUE);
    case T_VBIT3: return VBit3_Encode(v, s, e, TRUE);
    case T_VSEQR: return VSeqR_Encode(v, s, e, TRUE);
    case T_POCT: return POct_Encode(v, s, e, TRUE);
    case T_PINL: return PInl_Encode(v, s, e, TRUE);
    }
    return FALSE;
}

static flag equal(Ty t, const void* a, const void* b)
{
    switch (t) {
    case T_VOCT: return VOct_Equal(a, b);
    case T_VSTR: return VStr_Equal(a, b);
    case T_VBIT: return VBit_Equal(a, b);
    case T_VSEQ: return VSeq_Equal(a, b);
    case T_VOCTM: return VOctM_Equal(a, b);
    case T_VOCTN: return VOctN_Equal(a, b);
    case T_VBIT3: return VBit3_Equal(a, b);
    case T_VSEQR: return VSeqR_Equal(a, b);
    case T_POCT: return POct_Equal(a, b);
    case T_PINL: return PInl_Equal(a, b);
    }
    return FALSE;
}

static void set_bit(byte* arr, int i, unsigned v)
{
    if (v)
        arr[i / 8] |= (byte)(0x80 >> (i % 8));
}

/* append one item of component comp to the value v (which starts zeroed) */
static void add_item(Ty t, void* v, int comp, unsigned x, long* count)
{
    long i = count[comp]++;
    switch (t) {
    case T_VOCT: ((VOct*)v)->arr[i] = (byte)x; ((VOct*)v)->nCount++; break;
    case T_VSTR: ((char*)v)[i] = (char)('A' + x); break;
    case T_VBIT: set_bit(((VBit*)v)->arr, (int)i, x); ((VBit*)v)->nCount++; break;
    case T_VSEQ: ((VSeq*)v)->arr[i] = x; ((VSeq*)v)->nCount++; break;
    case T_VOCTM: ((VOctM*)v)->arr[i] = (byte)x; ((VOctM*)v)->nCount++; break;
    case T_VOCTN: ((VOctN*)v)->arr[i] = (byte)x; ((VOctN*)v)->nCount++; break;
    case T_VBIT3: set_bit(((VBit3*)v)->arr, (int)i, x); ((VBit3*)v)->nCount++; break;
    case T_VSEQR: ((VSeqR*)v)->arr[i].a = x >> 1; ((VSeqR*)v)->arr[i].b = (flag)(x & 1); ((VSeqR*)v)->nCount++; break;
    case T_POCT:
        if (comp == 1) {
            ((POct*)v)->o.arr[i] = (byte)x;
            ((POct*)v)->o.nCount++;
        } else {
            ((POct*)v)->tail.arr[i] = (byte)x;
        }
        break;
    case T_PINL: {
        PInl* p = v;
        if (comp == 1) { p->o.arr[i] = (byte)x; p->o.nCount++; }
        else if (comp == 2) p->s[i] = (char)('A' + x);
        else if (comp == 3) { p->q.arr[i] = x; p->q.nCount++; }
        else p->tail = x;
        break;
    }
    }
}

static void check(const char* name, Ty t, int acn, int expectAccept, int expErr, long expPos, const Seg* segs,
                  size_t nSegs)
{
    long nBits = 0;
    for (size_t k = 0; k < nSegs; k++)
        nBits += segs[k].nBits * segs[k].n;
    long nBytes = (nBits + 7) / 8;
    long cap = nBytes > required_bytes(t, acn) ? nBytes : required_bytes(t, acn);
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

    if (t == T_POCT)
        memset(((POct*)v)->tail.arr, TAIL_FILL, sizeof ((POct*)v)->tail.arr);
    BitStream s;
    int e = 0;
    BitStream_AttachBuffer(&s, stream, cap);
    flag ok = decode(t, acn, v, &s, &e);
    long at = s.currentByte * 8 + s.currentBit;
    if (expectAccept == REJECT) {
        if (ok)
            fail(name, "accepted", 1, 0);
        if (e != expErr)
            fail(name, "error code", e, expErr);
        if (at != expPos)
            fail(name, "bit position after the decoder", at, expPos);
        if (t == T_POCT)
            for (size_t k = 0; k < sizeof ((POct*)v)->tail.arr; k++)
                if (((POct*)v)->tail.arr[k] != TAIL_FILL) {
                    fail(name, "tail changed at octet", (long)k, -1);
                    break;
                }
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
        long count[5] = {0, 0, 0, 0, 0};
        if (x == NULL || enc == NULL) {
            printf("FAILED %s: out of memory\n", name);
            exit(1);
        }
        for (size_t k = 0; k < nSegs; k++)
            if (segs[k].comp > 0)
                for (long j = 0; j < segs[k].n; j++)
                    add_item(t, x, segs[k].comp, segs[k].v, count);
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

#define CHECK(name, t, acn, accept, err, pos, ...)                                                    \
    check(name, t, acn, accept, err, pos, (const Seg[]){__VA_ARGS__},                                \
          sizeof((const Seg[]){__VA_ARGS__}) / sizeof(Seg))

/* the VStr rows that run through the uPER and the ACN decoder (ACN without a specification: the uPER layout) */
static void vstr_rows(int acn, int err)
{
    /* an item that fails: the first item of a final part, of a block, the last item of a final part after a block,
       the last item of a block, an item inside a final part after a block (indexes 26 .. 31 are not in "A".."Z") */
    CHECK("R1 VStr length 2, first item fails", T_VSTR, acn, REJECT, err, 13, H(0x02), I(31, 5, 1), I(0, 5, 1));
    CHECK("R2 VStr C1, first item fails", T_VSTR, acn, REJECT, err, 13,
          H(0xC1), I(31, 5, 1), I(0, 5, 16383), H(0x00));
    CHECK("R3 VStr C1, last item of the final part fails", T_VSTR, acn, REJECT, err, 81941,
          H(0xC1), I(0, 5, 16384), H(0x01), I(31, 5, 1));
    CHECK("R3b VStr C4, last item of the block fails", T_VSTR, acn, REJECT, err, 327688,
          H(0xC4), I(0, 5, 65535), I(26, 5, 1), H(0x00));
    CHECK("R3c VStr C4, item 100 of the final part fails", T_VSTR, acn, REJECT, err, 328204,
          H(0xC4), I(0, 5, 65536), L2(4464), I(0, 5, 99), I(27, 5, 1), I(0, 5, 4364));
    CHECK("R3d VStr C3, last item of the block fails", T_VSTR, acn, REJECT, err, 245768,
          H(0xC3), I(0, 5, 49151), I(29, 5, 1), H(0x00));
    /* a block whose items pass the maximum 70000, a final part that passes it */
    CHECK("R4 VStr C4 C1: 81920 items", T_VSTR, acn, REJECT, err, 327696,
          H(0xC4), I(0, 5, 65536), H(0xC1), I(0, 5, 16384), H(0x00));
    CHECK("R5 VStr C4 + 8191: 73727 items", T_VSTR, acn, REJECT, err, 327704,
          H(0xC4), I(0, 5, 65536), L2(8191), I(0, 5, 8191));
    /* headers that are no length and no C1 .. C4 */
    CHECK("R6 VStr header C0", T_VSTR, acn, REJECT, err, 8, H(0xC0), I(0, 8, 8));
    CHECK("R6 VStr header C5", T_VSTR, acn, REJECT, err, 8, H(0xC5), I(0, 8, 8));
    CHECK("R6 VStr header FF", T_VSTR, acn, REJECT, err, 8, H(0xFF), I(0, 8, 8));
    CHECK("R7 VStr C4 then header C0", T_VSTR, acn, REJECT, err, 327696, H(0xC4), I(0, 5, 65536), H(0xC0));
    CHECK("R8 VStr length 0 (SIZE min 1)", T_VSTR, acn, REJECT, err, 8, H(0x00));
    /* valid: a block and a two-octet final length, a block and a one-octet final length, a block and an empty final
       part (SIZE min 1) */
    CHECK("A1 VStr 65536 B + 4464 C", T_VSTR, acn, ACCEPT, 0, 350024,
          H(0xC4), I(1, 5, 65536), L2(4464), I(2, 5, 4464));
    CHECK("A2 VStr 16384 Z + 3 A", T_VSTR, acn, ACCEPT, 0, 81951, H(0xC1), I(25, 5, 16384), H(0x03), I(0, 5, 3));
    CHECK("A2b VStr 65536 D, empty final part", T_VSTR, acn, ACCEPT, 0, 327696, H(0xC4), I(3, 5, 65536), H(0x00));
    CHECK("A2c VStr C3: 49152 E + 5 F", T_VSTR, acn, ACCEPT, 0, 245801, H(0xC3), I(4, 5, 49152), H(0x05), I(5, 5, 5));
}

int main(void)
{
    vstr_rows(UPER, ERR_UPER_DECODE_VSTR);
    vstr_rows(ACN, ERR_ACN_DECODE_VSTR);

    /* OCTET STRING (SIZE(0..70000)) */
    CHECK("R9 VOct C4 C2: 98304 items", T_VOCT, UPER, REJECT, ERR_UPER_DECODE_VOCT, 524304,
          H(0xC4), I(0x5A, 8, 65536), H(0xC2), I(0x5A, 8, 32768), H(0x00));
    CHECK("R10 VOct C4 + 4465: 70001 items", T_VOCT, UPER, REJECT, ERR_UPER_DECODE_VOCT, 524312,
          H(0xC4), I(0x5A, 8, 65536), L2(4465), I(0x5A, 8, 4465));
    CHECK("R11 VOct C4 then header C5", T_VOCT, UPER, REJECT, ERR_UPER_DECODE_VOCT, 524304,
          H(0xC4), I(0x5A, 8, 65536), H(0xC5));
    CHECK("A3 VOct 70000", T_VOCT, UPER, ACCEPT, 0, 560024, H(0xC4), I(0x5A, 8, 65536), L2(4464), I(0xA5, 8, 4464));
    CHECK("A4 VOct 16384", T_VOCT, UPER, ACCEPT, 0, 131088, H(0xC1), I(0x11, 8, 16384), H(0x00));
    CHECK("A4b VOct empty", T_VOCT, UPER, ACCEPT, 0, 8, H(0x00));
    CHECK("A4c VOct C3 + 848: 50000", T_VOCT, UPER, ACCEPT, 0, 400024,
          H(0xC3), I(0x3C, 8, 49152), L2(848), I(0xC3, 8, 848));

    /* OCTET STRING (SIZE(0..81920)): blocks that end exactly at the maximum */
    CHECK("M1 VOctM C4 C1 00: 81920", T_VOCTM, UPER, ACCEPT, 0, 655384,
          H(0xC4), I(0x5A, 8, 65536), H(0xC1), I(0x02, 8, 16384), H(0x00));
    CHECK("M2 VOctM C4 C1 + 1: 81921", T_VOCTM, UPER, REJECT, ERR_UPER_DECODE_VOCTM, 655384,
          H(0xC4), I(0x5A, 8, 65536), H(0xC1), I(0x02, 8, 16384), H(0x01), I(0x09, 8, 1));
    CHECK("M3 VOctM C4 C2: 98304", T_VOCTM, UPER, REJECT, ERR_UPER_DECODE_VOCTM, 524304,
          H(0xC4), I(0x5A, 8, 65536), H(0xC2), I(0x02, 8, 32768));
    CHECK("M4 VOctM C4 + 16383", T_VOCTM, UPER, ACCEPT, 0, 655376,
          H(0xC4), I(0x5A, 8, 65536), L2(16383), I(0x07, 8, 16383));

    /* OCTET STRING (SIZE(0..81919)): a block that passes the maximum by one item */
    CHECK("M5 VOctN C4 C1: 81920, one over the maximum", T_VOCTN, UPER, REJECT, ERR_UPER_DECODE_VOCTN, 524304,
          H(0xC4), I(0x5A, 8, 65536), H(0xC1), I(0x02, 8, 16384), H(0x00));
    CHECK("M6 VOctN C4 + 16383: 81919", T_VOCTN, UPER, ACCEPT, 0, 655376,
          H(0xC4), I(0x5A, 8, 65536), L2(16383), I(0x07, 8, 16383));

    /* BIT STRING (SIZE(0..70000)) */
    CHECK("R12 VBit C4 C1: 81920 bits", T_VBIT, UPER, REJECT, ERR_UPER_DECODE_VBIT, 65552,
          H(0xC4), I(1, 1, 65536), H(0xC1), I(1, 1, 16384), H(0x00));
    CHECK("R13 VBit C4 + 4465: 70001 bits", T_VBIT, UPER, REJECT, ERR_UPER_DECODE_VBIT, 65560,
          H(0xC4), I(1, 1, 65536), L2(4465), I(1, 1, 4465));
    CHECK("R14 VBit header C0", T_VBIT, UPER, REJECT, ERR_UPER_DECODE_VBIT, 8, H(0xC0));
    CHECK("A5 VBit 70000", T_VBIT, UPER, ACCEPT, 0, 70024,
          H(0xC4), I(1, 1, 65536), L2(4464), I(0, 1, 4463), I(1, 1, 1));
    CHECK("A5b VBit 16387 (not a multiple of 8)", T_VBIT, UPER, ACCEPT, 0, 16403,
          H(0xC1), I(1, 1, 16384), H(0x03), I(1, 1, 1), I(0, 1, 1), I(1, 1, 1));

    /* BIT STRING (SIZE(3..200000)): three blocks */
    CHECK("B1 VBit3 C4 C4 C4 + 3392: 200000", T_VBIT3, UPER, ACCEPT, 0, 200040,
          H(0xC4), I(1, 1, 65536), H(0xC4), I(0, 1, 65536), H(0xC4), I(1, 1, 65536), L2(3392), I(1, 1, 3392));
    CHECK("B2 VBit3 C4 C4 C4 + 3393: 200001", T_VBIT3, UPER, REJECT, ERR_UPER_DECODE_VBIT3, 196648,
          H(0xC4), I(1, 1, 65536), H(0xC4), I(0, 1, 65536), H(0xC4), I(1, 1, 65536), L2(3393), I(1, 1, 3393));
    CHECK("B3 VBit3 C4 C4 C4 C1: 212992", T_VBIT3, UPER, REJECT, ERR_UPER_DECODE_VBIT3, 196640,
          H(0xC4), I(1, 1, 65536), H(0xC4), I(0, 1, 65536), H(0xC4), I(1, 1, 65536), H(0xC1), I(1, 1, 16384));
    CHECK("B4 VBit3 C4 + 4467: 70003 (not a multiple of 8)", T_VBIT3, UPER, ACCEPT, 0, 70027,
          H(0xC4), I(1, 1, 65536), L2(4467), I(0, 1, 4466), I(1, 1, 1));
    CHECK("B5 VBit3 length 2 (SIZE min 3)", T_VBIT3, UPER, REJECT, ERR_UPER_DECODE_VBIT3, 10, H(0x02), I(1, 1, 2));

    /* SEQUENCE (SIZE(0..70000)) OF INTEGER (0..5): 3-bit items, 6 and 7 are invalid */
    CHECK("R15 VSeq length 2, first element fails", T_VSEQ, UPER, REJECT, ERR_UPER_DECODE_VSEQ_ELM, 11,
          H(0x02), I(7, 3, 1), I(0, 3, 1));
    CHECK("R16 VSeq C1, first element fails", T_VSEQ, UPER, REJECT, ERR_UPER_DECODE_VSEQ_ELM, 11,
          H(0xC1), I(6, 3, 1), I(0, 3, 16383), H(0x00));
    CHECK("R17 VSeq C4 C1: 81920", T_VSEQ, UPER, REJECT, ERR_UPER_DECODE_VSEQ, 196624,
          H(0xC4), I(5, 3, 65536), H(0xC1), I(5, 3, 16384), H(0x00));
    CHECK("R18 VSeq C4 + 8191", T_VSEQ, UPER, REJECT, ERR_UPER_DECODE_VSEQ, 196632,
          H(0xC4), I(5, 3, 65536), L2(8191));
    CHECK("A6 VSeq 70000", T_VSEQ, UPER, ACCEPT, 0, 210024, H(0xC4), I(4, 3, 65536), L2(4464), I(3, 3, 4464));

    /* SEQUENCE (SIZE(0..150000)) OF Elm: 4-bit items (a = 0..5 in 3 bits, b); 13 = a 6, 15 = a 7 */
    CHECK("Q1 VSeqR element 101 of the second block fails", T_VSEQR, UPER, REJECT, ERR_UPER_DECODE_ELM_A, 262563,
          H(0xC4), I(5, 4, 65536), H(0xC4), I(5, 4, 100), I(13, 4, 1), I(5, 4, 65435), H(0x00));
    CHECK("Q2 VSeqR last element of the first block fails", T_VSEQR, UPER, REJECT, ERR_UPER_DECODE_ELM_A, 262151,
          H(0xC4), I(5, 4, 65535), I(13, 4, 1), H(0x00));
    CHECK("Q3 VSeqR element 2 of the final part after two blocks fails", T_VSEQR, UPER, REJECT,
          ERR_UPER_DECODE_ELM_A, 524319,
          H(0xC4), I(5, 4, 65536), H(0xC4), I(5, 4, 65536), H(0x03), I(5, 4, 1), I(15, 4, 1), I(5, 4, 1));
    CHECK("Q4 VSeqR C4 C4 C1 + 2544: 150000", T_VSEQR, UPER, ACCEPT, 0, 600040,
          H(0xC4), I(5, 4, 65536), H(0xC4), I(4, 4, 65536), H(0xC1), I(1, 4, 16384), L2(2544), I(3, 4, 2544));
    CHECK("Q5 VSeqR C4 C4 C1 + 2545: 150001", T_VSEQR, UPER, REJECT, ERR_UPER_DECODE_VSEQR, 589864,
          H(0xC4), I(5, 4, 65536), H(0xC4), I(4, 4, 65536), H(0xC1), I(1, 4, 16384), L2(2545), I(3, 4, 2545));
    CHECK("Q6 VSeqR C4 C4 C2: 163840", T_VSEQR, UPER, REJECT, ERR_UPER_DECODE_VSEQR, 524312,
          H(0xC4), I(5, 4, 65536), H(0xC4), I(5, 4, 65536), H(0xC2), I(5, 4, 32768));

    /* a component of type VOct, then a fixed-size tail of 32768 octets: a block over the maximum leaves the tail
       untouched (the 28304 octets a decoder without the block check writes past o would stay inside the tail) */
    CHECK("P1 POct o = C4 C2, tail kept", T_POCT, UPER, REJECT, ERR_UPER_DECODE_VOCT, 524304,
          H(0xC4), C(1, 0xAA, 8, 65536), H(0xC2), C(1, 0xBB, 8, 32768), H(0x00), C(2, 0xBB, 8, 32768));
    CHECK("P2 POct o = 3 octets", T_POCT, UPER, ACCEPT, 0, 262176, H(0x03), C(1, 0xAA, 8, 3), C(2, 0xCC, 8, 32768));

    /* three inline fragmented components (o OCTET, s IA5String, q SEQUENCE OF) and an INTEGER */
    CHECK("N1 PInl a block in o, s and q", T_PINL, UPER, ACCEPT, 0, 262203,
          H(0xC1), C(1, 7, 8, 16384), H(0x00), H(0xC1), C(2, 2, 5, 16384), H(0x00),
          H(0xC1), C(3, 4, 3, 16384), H(0x01), C(3, 5, 3, 1), C(4, 0x77, 8, 1));
    CHECK("N2 PInl s: item 101 of the block fails", T_PINL, UPER, REJECT, ERR_UPER_DECODE_PINL_S, 529,
          H(0x01), C(1, 7, 8, 1), H(0xC1), C(2, 2, 5, 100), C(2, 30, 5, 1), C(2, 2, 5, 16283), H(0x00),
          H(0x00), C(4, 0x77, 8, 1));
    CHECK("N3 PInl q = C4 C1: 81920", T_PINL, UPER, REJECT, ERR_UPER_DECODE_PINL_Q, 196640,
          H(0x00), H(0x00), H(0xC4), C(3, 4, 3, 65536), H(0xC1), C(3, 4, 3, 16384), H(0x00), C(4, 0x77, 8, 1));
    CHECK("N4 PInl q: first element fails", T_PINL, UPER, REJECT, ERR_UPER_DECODE_PINL_Q_ELM, 45,
          H(0x01), C(1, 7, 8, 1), H(0x02), C(2, 1, 5, 1), C(2, 0, 5, 1), H(0x02), C(3, 7, 3, 1), C(3, 1, 3, 1),
          C(4, 0x77, 8, 1));

    if (failures != 0) {
        printf("uper-wire 004: %d checks failed\n", failures);
        return 1;
    }
    return 0;
}
