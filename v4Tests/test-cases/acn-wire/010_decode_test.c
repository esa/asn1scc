/* ACN checks of 010.asn1 / 010.acn (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, ASan / UBSan;
   Issue 132), with 010_choice.asn1 / 010_choice.acn under --acn-v2 (WIRE_ACN_V2 defined): a CONTAINING region longer
   than the largest encoding of its contents, and BIT STRING contents that end past their region, must be rejected;
   the maximal values round trip and stop at their encoded length. Every stream is padded with zeros to the type's
   REQUIRED_BYTES (the size of the Ada twin's stream). The Ada twin 010_decode_test.adb checks the same streams. */
#include <stdio.h>
#include <string.h>

#include "010.h"
#ifdef WIRE_ACN_V2
#include "010_choice.h"
#endif

static int failures = 0;

/* decode the given bytes, zeros up to T's REQUIRED_BYTES: the decoder must fail with a nonzero error code */
#define REJECT(T, ...)                                                                                  \
    do {                                                                                                \
        static const byte stream_[] = {__VA_ARGS__};                                                    \
        static byte buf_[T##_REQUIRED_BYTES_FOR_ACN_ENCODING];                                          \
        static T dec_;                                                                                  \
        BitStream s_;                                                                                   \
        int e_ = 0;                                                                                     \
        flag ok_;                                                                                       \
        memset(buf_, 0, sizeof buf_);                                                                   \
        memcpy(buf_, stream_, sizeof stream_ < sizeof buf_ ? sizeof stream_ : sizeof buf_);             \
        BitStream_AttachBuffer(&s_, buf_, sizeof buf_);                                                 \
        ok_ = T##_ACN_Decode(&dec_, &s_, &e_);                                                          \
        if (ok_ || e_ == 0) {                                                                           \
            printf("%s line %d: ok=%d err=%d, expected a rejection\n", #T, __LINE__, ok_, e_);          \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

/* encode v, decode it back from a stream of T's REQUIRED_BYTES: equal value, stopped at the encoded length */
#define ROUND_TRIP(T, v)                                                                                \
    do {                                                                                                \
        static byte buf_[T##_REQUIRED_BYTES_FOR_ACN_ENCODING];                                          \
        static T dec_;                                                                                  \
        BitStream s_;                                                                                   \
        int e_ = 0;                                                                                     \
        long end_;                                                                                      \
        memset(buf_, 0, sizeof buf_);                                                                   \
        BitStream_Init(&s_, buf_, sizeof buf_);                                                         \
        if (!T##_ACN_Encode(&(v), &s_, &e_, TRUE) || e_ != 0) {                                         \
            printf("%s line %d: encoding failed, err %d\n", #T, __LINE__, e_);                          \
            failures++;                                                                                 \
            break;                                                                                      \
        }                                                                                               \
        end_ = s_.currentByte * 8L + s_.currentBit;                                                     \
        BitStream_AttachBuffer(&s_, buf_, sizeof buf_);                                                 \
        if (!T##_ACN_Decode(&dec_, &s_, &e_) || e_ != 0 || !T##_Equal(&(v), &dec_) ||                   \
            s_.currentByte * 8L + s_.currentBit != end_) {                                              \
            printf("%s line %d: round trip failed, err %d\n", #T, __LINE__, e_);                        \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

int main(void)
{
    /* T: presence bits of b and c, a (8 bits), b (1 bit), c (8 bits): 10 .. 19 bits; Inner: 10 bits */
    T t;
    memset(&t, 0, sizeof t);
    t.a = 0xAB;
    t.exist.b = 1;
    t.b = TRUE;
    t.exist.c = 1;
    t.c = 0xCD;

    {
        BBox v = t;
        ROUND_TRIP(BBox, v);
        /* length 0 (10 bits): presence bits say c present, the contents need 18 bits */
        REJECT(BBox, 0x04, 0x00, 0x00);
        /* length 0 (10 bits): b present, the contents need 11 bits (C legacy: they end inside the region's last
           octet) */
        REJECT(BBox, 0x08, 0x00, 0x00);
    }
    {
        OBox v = t;
        ROUND_TRIP(OBox, v);
    }
    {
        PBitEmb v;
        memset(&v, 0, sizeof v);
        v.f = t;
        v.tail = 9;
        ROUND_TRIP(PBitEmb, v);
        REJECT(PBitEmb, 0x08, 0x00, 0x00, 0x00);
    }
    {
        PBitExt v;
        memset(&v, 0, sizeof v);
        v.f = t;
        v.tail = 9;
        ROUND_TRIP(PBitExt, v);
        /* a region of 21 bits (> 19): it ends past the stream with the tail (Ada: success past the end) */
        REJECT(PBitExt, 0x15, 0x00, 0x00, 0x00);
        /* a region of 20 bits (> 19), the tail still inside the stream (--acn-v2 C skipped the unused bit) */
        REJECT(PBitExt, 0x14, 0x00, 0x00, 0x00);
        /* a region of 9 bits holding contents of 10 */
        REJECT(PBitExt, 0x09, 0x00, 0x00, 0x00);
    }
    {
        POctExt v;
        memset(&v, 0, sizeof v);
        v.v.a = 0x5A;
        v.v.b = 3;
        v.tail = 9;
        ROUND_TRIP(POctExt, v);
        /* a region of 3 octets (> 2): the tail past the end of the stream */
        REJECT(POctExt, 0x03, 0x00, 0x00, 0x00);
    }
    {
        PBitExt2 v;
        memset(&v, 0, sizeof v);
        v.v.a = 0x5A;
        v.v.b = 3;
        v.tail = 9;
        ROUND_TRIP(PBitExt2, v);
        REJECT(PBitExt2, 0x0D, 0x00, 0x00); /* 13 bits (> 10), the tail past the end of the stream */
        REJECT(PBitExt2, 0x0B, 0x00, 0x00); /* 11 bits (> 10), the tail inside the stream */
        REJECT(PBitExt2, 0x09, 0x00, 0x00); /* 9 bits holding contents of 10 */
    }
#ifdef WIRE_ACN_V2
    {
        /* the region around Payload1 holds up to 4 octets (big), its CONTAINING alternatives 1 */
        Doc1 v;
        memset(&v, 0, sizeof v);
        v.payload.kind = big_PRESENT;
        v.payload.u.big = 0xDEADBEEF;
        v.trailer = 0x55;
        ROUND_TRIP(Doc1, v);
        v.payload.kind = Payload1_other_PRESENT;
        v.payload.u.other.number = 7;
        ROUND_TRIP(Doc1, v);
        REJECT(Doc1, 0x00, 0x05, 0x03, 0x00); /* a region of 5 octets */
    }
    {
        /* the region around Payload2B holds 17 bits: its index and the contents (16 bits) */
        Doc2B v;
        memset(&v, 0, sizeof v);
        v.payload.kind = Payload2B_other_PRESENT;
        v.payload.u.other.number = 0x1234;
        v.trailer = 0x55;
        ROUND_TRIP(Doc2B, v);
        REJECT(Doc2B, 0x00, 0x12, 0x00); /* a region of 18 bits, the trailer inside the stream */
        REJECT(Doc2B, 0x00, 0x19, 0x00); /* a region of 25 bits, the trailer past the end of the stream */
    }
#endif
    if (failures != 0) {
        printf("%d failures\n", failures);
        return 1;
    }
    return 0;
}
