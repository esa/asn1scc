/* Checks of 021.asn1 / 021.acn (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, built with -DNDEBUG and
   ASan / UBSan; Issue 208): CONTAINING regions whose contents are SEQUENCE OFs of variable-size elements. Each value is
   encoded with ACN from bit 0 into a heap buffer of exactly REQUIRED_BYTES and from bit 3 into a buffer of exactly
   (3 + REQUIRED_BITS + 7) / 8 octets: the result, the error code, the end bit position and the bytes are pinned (from
   bit 3: the bit-0 bytes shifted by 3, since a region is whole octets from its own first bit), REQUIRED_BITS pinned,
   then decoded from the written octets only (Equal, end position). Rejected rows: a region length above the new
   maximum, decoded from exactly REQUIRED_BYTES: FALSE, the region's error code, the position after the length field.
   Before the fix: R6 / B1 short values were not encodable (C read past a mask table), R5 / N1 legacy values failed to
   decode, R5 / E3 acn-v2 wrote regions that were not whole octets (E3 V2 decoded to another value), and every value of
   R6 / B1 / R5 / N1 had other bytes. CHECK, not assert: NDEBUG removes assert. The Ada twin 021_wire_test.adb checks the
   same rows. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "021.h"

#define CHECK(c)                                                                          \
    do {                                                                                  \
        if (!(c)) {                                                                       \
            fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #c);         \
            abort();                                                                      \
        }                                                                                 \
    } while (0)

/* src (nBits bits) shifted right by off bits into dst ((off + nBits + 7) / 8 octets, the rest zero) */
static void shifted(const byte* src, int nBits, int off, byte* dst)
{
    int n = (off + nBits + 7) / 8;
    memset(dst, 0, (size_t)n);
    for (int i = 0; i < nBits; i++) {
        if (src[i / 8] & (0x80 >> (i % 8))) {
            dst[(off + i) / 8] |= (byte)(0x80 >> ((off + i) % 8));
        }
    }
}

/* encode v from bit OFF into exactly the octets REQUIRED_BITS needs from there: nBits bits, the bytes `expected`
   shifted by OFF; decode from the written octets only */
#define ROUND_TRIP(T, OFF, v, reqBits, expected, nBits)                                   \
    do {                                                                                  \
        const int size_ = ((OFF) + T##_REQUIRED_BITS_FOR_ACN_ENCODING + 7) / 8;           \
        const int used_ = ((OFF) + (nBits) + 7) / 8;                                      \
        byte* buf_ = calloc((size_t)size_, 1);                                            \
        byte exp_[16];                                                                    \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(buf_ != NULL);                                                              \
        CHECK(T##_REQUIRED_BITS_FOR_ACN_ENCODING == (reqBits));                           \
        CHECK((OFF) != 0 || size_ == T##_REQUIRED_BYTES_FOR_ACN_ENCODING);                \
        CHECK((nBits) <= (reqBits));                                                      \
        CHECK(sizeof(expected) == ((nBits) + 7) / 8);                                     \
        CHECK(used_ <= (int)sizeof exp_);                                                 \
        shifted((expected), (nBits), (OFF), exp_);                                        \
        BitStream_Init(&s_, buf_, size_);                                                 \
        s_.currentBit = (OFF);                                                            \
        CHECK(T##_ACN_Encode(&(v), &s_, &e_, TRUE));                                      \
        CHECK(e_ == 0);                                                                   \
        CHECK(s_.currentByte * 8 + s_.currentBit == (OFF) + (nBits));                     \
        CHECK(memcmp(buf_, exp_, (size_t)used_) == 0);                                    \
        BitStream_AttachBuffer(&s_, buf_, used_);                                         \
        s_.currentBit = (OFF);                                                            \
        memset(&dec_, 0, sizeof dec_);                                                    \
        CHECK(T##_ACN_Decode(&dec_, &s_, &e_));                                           \
        CHECK(e_ == 0);                                                                   \
        CHECK(T##_Equal(&(v), &dec_));                                                    \
        CHECK(s_.currentByte * 8 + s_.currentBit == (OFF) + (nBits));                     \
        free(buf_);                                                                       \
    } while (0)

#define ROW(T, v, reqBits, expected, nBits)                                               \
    do {                                                                                  \
        ROUND_TRIP(T, 0, v, reqBits, expected, nBits);                                    \
        ROUND_TRIP(T, 3, v, reqBits, expected, nBits);                                    \
    } while (0)

/* decode src (zero-padded to exactly REQUIRED_BYTES): FALSE, error code err, end bit position pos */
#define REJECT(T, src, err, pos)                                                          \
    do {                                                                                  \
        byte* buf_ = calloc(T##_REQUIRED_BYTES_FOR_ACN_ENCODING, 1);                      \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(buf_ != NULL);                                                              \
        memcpy(buf_, (src), sizeof(src));                                                 \
        BitStream_AttachBuffer(&s_, buf_, T##_REQUIRED_BYTES_FOR_ACN_ENCODING);           \
        CHECK(!T##_ACN_Decode(&dec_, &s_, &e_));                                          \
        CHECK(e_ == (err));                                                               \
        CHECK(s_.currentByte * 8 + s_.currentBit == (pos));                               \
        free(buf_);                                                                       \
    } while (0)

static void elem_ia(Elem* e, int v) { e->kind = ia_PRESENT; e->u.ia = v; }
static void elem_ib(Elem* e, int v) { e->kind = ib_PRESENT; e->u.ib = v; }
static void elem_oc(Elem* e, byte a, byte b)
{
    e->kind = oc_PRESENT;
    e->u.oc.nCount = 2;
    e->u.oc.arr[0] = a;
    e->u.oc.arr[1] = b;
}

int main(void)
{
    /* R6: OCTET STRING (CONTAINING Lst), Lst = 1..2 elements of 4..20 bits = 5..41 bits: region 1..6 octets, 3-bit
       length field (was 3..6 octets, 2 bits: {ia:1} and {ib:5, ia:3} were not encodable, max F424694ACF003FC0) */
    static R6 r6;
    static const byte r6a[] = {0x01, 0x15, 0x40};
    static const byte r6b[] = {0x34, 0x29, 0x8A, 0xA0};
    static const byte r6c[] = {0xBA, 0x12, 0x34, 0xA5, 0x67, 0x80, 0x1F, 0xE0};
    r6.o.nCount = 1;
    elem_ia(&r6.o.arr[0], 1);
    r6.z = 0xAA;
    ROW(R6, r6, 59, r6a, 19);
    r6.o.nCount = 2;
    elem_ib(&r6.o.arr[0], 5);
    elem_ia(&r6.o.arr[1], 3);
    r6.z = 0x55;
    ROW(R6, r6, 59, r6b, 27);
    elem_oc(&r6.o.arr[0], 0x12, 0x34);
    elem_oc(&r6.o.arr[1], 0x56, 0x78);
    r6.z = 0xFF;
    ROW(R6, r6, 59, r6c, 59);

    /* R5: OCTET STRING (CONTAINING L2), L2 = 2 elements, 8..40 bits: 3-bit length field + 1..5 octets (was a FIXED
       5-octet region: legacy 12AA / 429855 not decodable, acn-v2 12AA / 429AA8 without padding, max A1234A5678FF) */
    static R5 r5;
    static const byte r5a[] = {0x02, 0x55, 0x40};
    static const byte r5b[] = {0x28, 0x53, 0x0A, 0xA0};
    static const byte r5c[] = {0x94, 0x24, 0x69, 0x4A, 0xCF, 0x1F, 0xE0};
    elem_ia(&r5.o.arr[0], 1);
    elem_ia(&r5.o.arr[1], 2);
    r5.z = 0xAA;
    ROW(R5, r5, 51, r5a, 19);
    elem_ib(&r5.o.arr[0], 5);
    elem_ia(&r5.o.arr[1], 3);
    r5.z = 0x55;
    ROW(R5, r5, 51, r5b, 27);
    elem_oc(&r5.o.arr[0], 0x12, 0x34);
    elem_oc(&r5.o.arr[1], 0x56, 0x78);
    r5.z = 0xFF;
    ROW(R5, r5, 51, r5c, 51);

    /* B1: BIT STRING (CONTAINING Lst): 6-bit length of 5..41 bits (was 21..41, 5 bits; max A6848D2959E3FC) */
    static B1 b1;
    static const byte b1a[] = {0x00, 0x35, 0x40};
    static const byte b1b[] = {0x26, 0x85, 0x35, 0x50};
    static const byte b1c[] = {0x93, 0x42, 0x46, 0x94, 0xAC, 0xF1, 0xFE};
    b1.o.nCount = 1;
    elem_ia(&b1.o.arr[0], 1);
    b1.z = 0xAA;
    ROW(B1, b1, 55, b1a, 19);
    b1.o.nCount = 2;
    elem_ib(&b1.o.arr[0], 5);
    elem_ia(&b1.o.arr[1], 3);
    b1.z = 0x55;
    ROW(B1, b1, 55, b1b, 28);
    elem_oc(&b1.o.arr[0], 0x12, 0x34);
    elem_oc(&b1.o.arr[1], 0x56, 0x78);
    b1.z = 0xFF;
    ROW(B1, b1, 55, b1c, 55);

    /* N1: OCTET STRING (CONTAINING S), S = BOOLEAN + 2 x (1..2 elements of 4 / 9 bits) = 7..39 bits: 2-bit length field +
       2..5 octets (was FIXED 5 octets: legacy 8080AA not decodable, acn-v2 8080000000AA; max F9397C9CA055) */
    static N1 n1;
    static const byte n1a[] = {0x20, 0x20, 0x2A, 0x80};
    static const byte n1b[] = {0xFE, 0x4E, 0x5F, 0x27, 0x28, 0x15, 0x40};
    n1.o.h = TRUE;
    for (int k = 0; k < 2; k++) {
        n1.o.ll.arr[k].nCount = 1;
        n1.o.ll.arr[k].arr[0].kind = sa_PRESENT;
        n1.o.ll.arr[k].arr[0].u.sa = k;
    }
    n1.z = 0xAA;
    ROW(N1, n1, 50, n1a, 26);
    for (int k = 0; k < 2; k++) {
        n1.o.ll.arr[k].nCount = 2;
        for (int j = 0; j < 2; j++) {
            n1.o.ll.arr[k].arr[j].kind = sb_PRESENT;
            n1.o.ll.arr[k].arr[j].u.sb = 100 + j;
        }
    }
    n1.z = 0x55;
    ROW(N1, n1, 50, n1b, 50);

    /* E3: external size n (octets) of OCTET STRING (CONTAINING S3), S3 = SEQUENCE { L2 }: the region is padded to
       whole octets (acn-v2 wrote 02429AA8, 29 bits, which decoded to another value); legacy unchanged */
    static E3 e3;
    static const byte e3a[] = {0x01, 0x12, 0xAA};
    static const byte e3b[] = {0x02, 0x42, 0x98, 0x55};
    static const byte e3c[] = {0x05, 0xA1, 0x23, 0x4A, 0x56, 0x78, 0xFF};
    elem_ia(&e3.o.l.arr[0], 1);
    elem_ia(&e3.o.l.arr[1], 2);
    e3.z = 0xAA;
    ROW(E3, e3, 56, e3a, 24);
    elem_ib(&e3.o.l.arr[0], 5);
    elem_ia(&e3.o.l.arr[1], 3);
    e3.z = 0x55;
    ROW(E3, e3, 56, e3b, 32);
    elem_oc(&e3.o.l.arr[0], 0x12, 0x34);
    elem_oc(&e3.o.l.arr[1], 0x56, 0x78);
    e3.z = 0xFF;
    ROW(E3, e3, 56, e3c, 56);

    /* rejected: region lengths above the new maximum (R6: 110 = 7 octets > 6, 111 = 8; R5: 101 = 6 octets > 5;
       B1: 111111 = 68 bits > 41), after the length field (before the fix R6 accepted 11 = 6 octets as the maximum,
       R5 had no length field) */
    static const byte r6len7[] = {0xC0};
    static const byte r6len8[] = {0xE0};
    static const byte r5len6[] = {0xA0};
    static const byte b1len68[] = {0xFC};
    REJECT(R6, r6len7, ERR_ACN_DECODE_R6_O_2, 3);
    REJECT(R6, r6len8, ERR_ACN_DECODE_R6_O_2, 3);
    REJECT(R5, r5len6, ERR_ACN_DECODE_R5_O_2, 3);
    REJECT(B1, b1len68, ERR_ACN_DECODE_B1_O_2, 6);

    printf("021 wire checks passed\n");
    return 0;
}
