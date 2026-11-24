/* Checks of 014.asn1 / 014.acn (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, ASan / UBSan; Issue
   138): the bytes of CONTAINING regions whose contents are an alias / a WITH COMPONENTS subtype sharing another type's
   typedef, written by hand, encoded into a buffer of exactly REQUIRED_BYTES and decoded. Before the fix --acn-v2 wrote
   the shared typedef's encoding (Q12 05 05 80; Q9 00 00 12 34 50, 36 bits for 25 required) and legacy rejected the
   sites. The Ada twin 014_wire_test.adb checks the same values and bytes. */
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "014.h"

/* encode v into a heap buffer of exactly REQUIRED_BYTES, compare with the expected bytes and length in bits, decode
   them and compare */
#define ROUND_TRIP(T, v, expected, nBits)                                                 \
    do {                                                                                  \
        byte* buf_ = calloc(T##_REQUIRED_BYTES_FOR_ACN_ENCODING, 1);                      \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        assert(buf_ != NULL && sizeof(expected) == T##_REQUIRED_BYTES_FOR_ACN_ENCODING);  \
        BitStream_Init(&s_, buf_, T##_REQUIRED_BYTES_FOR_ACN_ENCODING);                   \
        assert(T##_ACN_Encode(&(v), &s_, &e_, TRUE));                                     \
        assert(e_ == 0);                                                                  \
        assert(s_.currentByte * 8 + s_.currentBit == (nBits));                            \
        assert(memcmp(buf_, (expected), sizeof(expected)) == 0);                          \
        BitStream_AttachBuffer(&s_, buf_, sizeof(expected));                              \
        assert(T##_ACN_Decode(&dec_, &s_, &e_));                                          \
        assert(e_ == 0);                                                                  \
        assert(T##_Equal(&(v), &dec_));                                                   \
        assert(s_.currentByte * 8 + s_.currentBit == (nBits));                            \
        free(buf_);                                                                       \
    } while (0)

int main(void)
{
    /* Sub in 24 bits: an OCTET STRING region of 3 octets, a BIT STRING region of 24 bits, z */
    Q12 q;
    memset(&q, 0, sizeof q);
    q.f = 5;
    q.g = 5;
    q.z = TRUE;
    static const byte q12[] = {0x00, 0x00, 0x05, 0x00, 0x00, 0x05, 0x80};
    ROUND_TRIP(Q12, q, q12, 49);
    /* WSub: r in 16 bits (TR: 32), k in 3 bits: a region of 3 octets, z */
    Q9 n;
    memset(&n, 0, sizeof n);
    n.f.r = 0x1234;
    n.f.k = 2;
    n.z = TRUE;
    static const byte q9[] = {0x12, 0x34, 0x40, 0x80};
    ROUND_TRIP(Q9, n, q9, 25);
    return 0;
}
