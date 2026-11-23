/* uPER and ACN checks of 003.asn1 (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, ASan / UBSan;
   Issue 134). Expected bytes written by hand from X.691 (uPER: general length, contents = Box's encoding padded to
   whole octets) and the ACN default encoding (a length field for the octets of the region's range), cross-checked
   with pycrate's uPER encoder. The Ada twin 003_wire_test.adb checks the same values and streams. */
#include <assert.h>
#include <string.h>

#include "003.h"

/* encode v with the codec (<T>_Encode / <T>_ACN_Encode) and compare with the expected bytes, decode them and compare */
#define ROUND_TRIP(T, CODEC, v, expected, nBits)                                          \
    do {                                                                                  \
        static byte buf_[sizeof(expected) + 8];                                           \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        memset(buf_, 0, sizeof buf_);                                                     \
        BitStream_Init(&s_, buf_, sizeof buf_);                                           \
        assert(T##CODEC##_Encode(&(v), &s_, &e_, TRUE));                                  \
        assert(e_ == 0);                                                                  \
        assert(s_.currentByte * 8 + s_.currentBit == (nBits));                            \
        assert(memcmp(buf_, (expected), sizeof(expected)) == 0);                          \
        BitStream_AttachBuffer(&s_, buf_, sizeof(expected));                              \
        assert(T##CODEC##_Decode(&dec_, &s_, &e_));                                       \
        assert(T##_Equal(&(v), &dec_));                                                   \
    } while (0)

/* decode the given bytes: the decoder must reject them with the given error code */
#define REJECT(T, CODEC, bytes, err)                                                      \
    do {                                                                                  \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        BitStream_AttachBuffer(&s_, (byte*)(bytes), sizeof(bytes));                       \
        assert(!T##CODEC##_Decode(&dec_, &s_, &e_));                                      \
        assert(e_ == (err));                                                              \
    } while (0)

static P10 p10(asn1SccUint a, int hasB, flag b, int hasC, asn1SccUint c)
{
    P10 v;
    memset(&v, 0, sizeof v);
    v.f.a = a;
    v.f.exist.b = hasB;
    v.f.b = b;
    v.f.exist.c = hasC;
    v.f.c = c;
    v.z = TRUE;
    return v;
}

int main(void)
{
    /* {a 5, b TRUE}: T = presence 10, a, b (11 bits) = 81 60; Box = length 2 + 81 60; f = length 3 + Box; z */
    P10 v1 = p10(5, 1, TRUE, 0, 0);
    static const byte uper1[] = {0x03, 0x02, 0x81, 0x60, 0x80};
    ROUND_TRIP(P10, , v1, uper1, 33);
    /* {a 5}: T = presence 00, a (10 bits) = 01 40 */
    P10 v2 = p10(5, 0, FALSE, 0, 0);
    static const byte uper2[] = {0x03, 0x02, 0x01, 0x40, 0x80};
    ROUND_TRIP(P10, , v2, uper2, 33);
    /* {a 0xAB, b FALSE, c 0xCD}: T (19 bits) = EA D9 A0; Box = length 3 + T; f = length 4 + Box */
    P10 v3 = p10(0xAB, 1, FALSE, 1, 0xCD);
    static const byte uper3[] = {0x04, 0x03, 0xEA, 0xD9, 0xA0, 0x80};
    ROUND_TRIP(P10, , v3, uper3, 41);

    /* ACN (no ACN specification): f's length is 1 bit for 3 .. 4 octets, Box's 1 bit for 2 .. 3 octets, then T padded
       to Box's octets and Box padded to f's octets; z */
    static const byte acn1[] = {0x20, 0x58, 0x00, 0x40};
    ROUND_TRIP(P10, _ACN, v1, acn1, 26);
    static const byte acn2[] = {0x00, 0x50, 0x00, 0x40};
    ROUND_TRIP(P10, _ACN, v2, acn2, 26);
    static const byte acn3[] = {0xFA, 0xB6, 0x68, 0x00, 0x40};
    ROUND_TRIP(P10, _ACN, v3, acn3, 34);

    /* uPER: f of 4 octets holding a Box of 0 octets: T's decoder (called by Box's) fails on the empty region */
    static const byte uper_empty_box[] = {0x04, 0x00, 0x00, 0x00, 0x00, 0x00};
    REJECT(P10, , uper_empty_box, ERR_UPER_DECODE_T);
    /* uPER: f of 5 octets, above its maximum of 4: the code of P10.f's length */
    static const byte uper_long[] = {0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    REJECT(P10, , uper_long, ERR_UPER_DECODE_P10_F_2_2);

    /* uPER: f of 4 octets (the decoders wrote them into a buffer of 3) holding Box with a length of 3 octets and
       T = {a 0}, then z TRUE: a valid encoding */
    {
        static const byte uper_four[] = {0x04, 0x03, 0x00, 0x00, 0x00, 0x80};
        static P10 dec;
        P10 expected = p10(0, 0, FALSE, 0, 0);
        BitStream s;
        int e = 0;
        BitStream_AttachBuffer(&s, (byte*)uper_four, sizeof uper_four);
        assert(P10_Decode(&dec, &s, &e));
        assert(P10_Equal(&expected, &dec));
        assert(s.currentByte * 8 + s.currentBit == 41);
    }

    /* ACN: f of 4 octets (the legacy decoder wrote them into a buffer of 3), holding Box with a length of 2 octets and
       T = {a 0}, then z FALSE: a valid encoding */
    {
        static const byte acn_four[] = {0x80, 0x00, 0x00, 0x00, 0x00};
        static P10 dec;
        P10 expected = p10(0, 0, FALSE, 0, 0);
        BitStream s;
        int e = 0;
        expected.z = FALSE;
        BitStream_AttachBuffer(&s, (byte*)acn_four, sizeof acn_four);
        assert(P10_ACN_Decode(&dec, &s, &e));
        assert(P10_Equal(&expected, &dec));
        assert(s.currentByte * 8 + s.currentBit == 34);
    }

    /* fixed-size contents: uPER f = length 3 + BoxF (length 2 + Fx), g = 24 bits + BBoxF (16 bits + Fx); z FALSE */
    P70 w;
    memset(&w, 0, sizeof w);
    w.f.a = 255;
    w.f.b = 255;
    w.g.a = 255;
    w.g.b = 255;
    w.z = FALSE;
    static const byte uper70[] = {0x03, 0x02, 0xFF, 0xFF, 0x18, 0x10, 0xFF, 0xFF, 0x00};
    ROUND_TRIP(P70, , w, uper70, 65);
    /* ACN: fixed-size regions have no length, so the bytes are Fx's in both components (unchanged by Issue 134) */
    static const byte acn70[] = {0xFF, 0xFF, 0xFF, 0xFF, 0x00};
    ROUND_TRIP(P70, _ACN, w, acn70, 33);
    return 0;
}
