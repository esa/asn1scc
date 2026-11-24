/* uPER and ACN checks of 002.asn1 (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2; Issue 133): the bytes
   of one value, and one stream per encoding whose OCTET STRING (CONTAINING T) region is too short for the contents it
   holds, which every decoder must reject. The Ada twin 002_wire_test.adb checks the same streams. */
#include <assert.h>
#include <string.h>

#include "002.h"

/* encode v with the codec (<T>_Encode / <T>_ACN_Encode) and compare with the expected bytes, decode them and compare */
#define ROUND_TRIP(T, CODEC, v, expected, nBits)                                          \
    do {                                                                                  \
        static byte buf_[sizeof(expected) + 8];                                           \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        BitStream_Init(&s_, buf_, sizeof buf_);                                           \
        assert(T##CODEC##_Encode(&(v), &s_, &e_, TRUE));                                  \
        assert(s_.currentByte * 8 + s_.currentBit == (nBits));                            \
        assert(memcmp(buf_, (expected), sizeof(expected)) == 0);                          \
        BitStream_AttachBuffer(&s_, buf_, sizeof(expected));                              \
        assert(T##CODEC##_Decode(&dec_, &s_, &e_));                                       \
        assert(T##_Equal(&(v), &dec_));                                                   \
    } while (0)

/* decode the given bytes: the decoder must reject them with an error code */
#define REJECT(T, CODEC, bytes)                                                           \
    do {                                                                                  \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        BitStream_AttachBuffer(&s_, (byte*)(bytes), sizeof(bytes));                       \
        assert(!T##CODEC##_Decode(&dec_, &s_, &e_));                                      \
        assert(e_ != 0);                                                                  \
    } while (0)

int main(void)
{
    PDU v;
    memset(&v, 0, sizeof v);
    v.f.a = 5;
    v.f.b = TRUE;
    v.f.c = 7;
    v.f.exist.b = 1;
    v.f.exist.c = 1;
    v.z = TRUE;

    /* uPER: length 3 octets, T = presence 11, a, b, c, padding; z */
    static const byte uper[] = {0x03, 0xC1, 0x60, 0xE0, 0x80};
    ROUND_TRIP(PDU, , v, uper, 33);
    /* ACN (no ACN specification): the length is a 1-bit field for 2 .. 3 octets; T = presence 11, a, b, c, padding */
    static const byte acn[] = {0xE0, 0xB0, 0x70, 0x40};
    ROUND_TRIP(PDU, _ACN, v, acn, 26);

    /* uPER: f with a length of 0 octets (T needs at least 10 bits), then z = TRUE */
    static const byte uper_short[] = {0x00, 0x80, 0x00, 0x00, 0x00};
    REJECT(PDU, , uper_short);
    /* ACN: f with a length of 2 octets and T = presence 01 (c present: 18 bits), then z */
    static const byte acn_short[] = {0x20, 0x00, 0x00, 0x00};
    REJECT(PDU, _ACN, acn_short);
    return 0;
}
