/* --acn-v2 checks of 015.asn1 / 015.acn (run by v4Tests/scripts/runWireTests.sh, built with -DNDEBUG and ASan / UBSan;
   Issue 141): maximal values encoded into a heap buffer of exactly REQUIRED_BYTES (before the fix one octet too small
   for an OCTET STRING region: with NDEBUG the runtime's assert is gone and the encoder wrote past the buffer, an ASan
   heap-buffer-overflow), the bytes compared with the ones written by hand, decoded back. CHECK, not assert: NDEBUG removes assert. The Ada twin
   015_wire_test.adb checks the same values and bytes. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "015.h"

#define CHECK(c)                                                                          \
    do {                                                                                  \
        if (!(c)) {                                                                       \
            fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #c);         \
            abort();                                                                      \
        }                                                                                 \
    } while (0)

#define ROUND_TRIP(T, v, expected, nBits)                                                 \
    do {                                                                                  \
        byte* buf_ = calloc(T##_REQUIRED_BYTES_FOR_ACN_ENCODING, 1);                      \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(buf_ != NULL);                                                              \
        BitStream_Init(&s_, buf_, T##_REQUIRED_BYTES_FOR_ACN_ENCODING);                   \
        CHECK(T##_ACN_Encode(&(v), &s_, &e_, TRUE));                                      \
        CHECK(sizeof(expected) == T##_REQUIRED_BYTES_FOR_ACN_ENCODING);                   \
        CHECK(e_ == 0);                                                                   \
        CHECK(s_.currentByte * 8 + s_.currentBit == (nBits));                             \
        CHECK(memcmp(buf_, (expected), sizeof(expected)) == 0);                           \
        BitStream_AttachBuffer(&s_, buf_, sizeof(expected));                              \
        CHECK(T##_ACN_Decode(&dec_, &s_, &e_));                                           \
        CHECK(e_ == 0);                                                                   \
        CHECK(T##_Equal(&(v), &dec_));                                                    \
        CHECK(s_.currentByte * 8 + s_.currentBit == (nBits));                             \
        free(buf_);                                                                       \
    } while (0)

int main(void)
{
    /* length 3, the region: index 1 (other), number FFFF, 7 bits of padding; trailer FF; extra */
    Doc d;
    memset(&d, 0, sizeof d);
    d.pl.kind = Payload_other_PRESENT;
    d.pl.u.other.number = 0xFFFF;
    d.trailer = 0xFF;
    d.extra = TRUE;
    static const byte doc[] = {0x00, 0x03, 0xFF, 0xFF, 0x80, 0xFF, 0x80};
    ROUND_TRIP(Doc, d, doc, 49);
    /* the BIT STRING alternative in the OCTET STRING region: index 00, number ABCD, 6 bits of padding */
    Doc13 e;
    memset(&e, 0, sizeof e);
    e.pl.kind = Payload13_value_PRESENT;
    e.pl.u.value.number = 0xABCD;
    e.trailer = 0x11;
    e.extra = TRUE;
    static const byte doc13[] = {0x00, 0x03, 0x2A, 0xF3, 0x40, 0x11, 0x80};
    ROUND_TRIP(Doc13, e, doc13, 49);
    /* a BIT STRING alternative last: a BIT STRING region of 18 bits (length 0x12), index 01, number FFFF, no padding */
    DocL l;
    memset(&l, 0, sizeof l);
    l.pl.kind = PayloadL_other_PRESENT;
    l.pl.u.other.number = 0xFFFF;
    l.trailer = 0xFF;
    l.extra = TRUE;
    static const byte docL[] = {0x00, 0x12, 0x7F, 0xFF, 0xFF, 0xE0};
    ROUND_TRIP(DocL, l, docL, 43);
    return 0;
}
