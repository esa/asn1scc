/* ACN checks of 008.asn1 / 008.acn (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, ASan / UBSan;
   Issue 134): the bytes of aligned CONTAINING components, written by hand (the alignment precedes the region and does
   not enter the contents' sizes), streams that must decode and streams that must be rejected. The Ada twin
   008_wire_test.adb checks the same values and streams. */
#include <assert.h>
#include <string.h>

#include "008.h"

/* encode v and compare with the expected bytes and length in bits, decode them and compare */
#define ROUND_TRIP(T, v, expected, nBits)                                                 \
    do {                                                                                  \
        static byte buf_[sizeof(expected) + 8];                                           \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        memset(buf_, 0, sizeof buf_);                                                     \
        BitStream_Init(&s_, buf_, sizeof buf_);                                           \
        assert(T##_ACN_Encode(&(v), &s_, &e_, TRUE));                                     \
        assert(e_ == 0);                                                                  \
        assert(s_.currentByte * 8 + s_.currentBit == (nBits));                            \
        assert(memcmp(buf_, (expected), sizeof(expected)) == 0);                          \
        BitStream_AttachBuffer(&s_, buf_, sizeof(expected));                              \
        assert(T##_ACN_Decode(&dec_, &s_, &e_));                                          \
        assert(T##_Equal(&(v), &dec_));                                                   \
    } while (0)

/* decode the given bytes: the decoder must accept them, give v and stop at nBits */
#define ACCEPT(T, bytes, v, nBits)                                                        \
    do {                                                                                  \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        BitStream_AttachBuffer(&s_, (byte*)(bytes), sizeof(bytes));                       \
        assert(T##_ACN_Decode(&dec_, &s_, &e_));                                          \
        assert(T##_Equal(&(v), &dec_));                                                   \
        assert(s_.currentByte * 8 + s_.currentBit == (nBits));                            \
    } while (0)

/* decode the given bytes: the decoder must reject them with a nonzero error code */
#define REJECT(T, bytes)                                                                  \
    do {                                                                                  \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        BitStream_AttachBuffer(&s_, (byte*)(bytes), sizeof(bytes));                       \
        assert(!T##_ACN_Decode(&dec_, &s_, &e_));                                         \
        assert(e_ != 0);                                                                  \
    } while (0)

int main(void)
{
    Q6 q;
    memset(&q, 0, sizeof q);
    q.y = TRUE;
    q.z = TRUE;
    /* {a 5, b TRUE}: y, 7 bits of alignment; Box's length 1 bit (2 .. 3 octets) = 0, T = 81 60; z */
    q.f.a = 5;
    q.f.exist.b = 1;
    q.f.b = TRUE;
    static const byte q6a[] = {0x80, 0x40, 0xB0, 0x40};
    ROUND_TRIP(Q6, q, q6a, 26);
    /* {a 0xAB, b FALSE, c 0xCD}: Box's length = 1 (3 octets), T = EA D9 A0 */
    q.f.a = 0xAB;
    q.f.b = FALSE;
    q.f.exist.c = 1;
    q.f.c = 0xCD;
    static const byte q6b[] = {0x80, 0xF5, 0x6C, 0xD0, 0x40};
    ROUND_TRIP(Q6, q, q6b, 34);

    /* y FALSE, alignment, Box's length 1 (3 octets) holding T = {a 0}, z FALSE: a valid encoding (before the fix the
       2-bit length 2 meant 4 octets, which the legacy decoder wrote into a buffer of 3) */
    {
        static const byte q6_three[] = {0x00, 0x80, 0x00, 0x00, 0x00, 0x00};
        Q6 zero;
        memset(&zero, 0, sizeof zero);
        ACCEPT(Q6, q6_three, zero, 34);
    }
    /* Box's length 0 (2 octets) holding T with c present (18 bits): rejected */
    static const byte q6_short[] = {0x00, 0x20, 0x00, 0x00, 0x00};
    REJECT(Q6, q6_short);

    /* fixed-size contents, aligned: no length field; y, alignment, f = 01 02, g = 03 04, z */
    QF w;
    memset(&w, 0, sizeof w);
    w.y = TRUE;
    w.f.a = 1;
    w.f.b = 2;
    w.g.a = 3;
    w.g.b = 4;
    w.z = TRUE;
    static const byte qf[] = {0x80, 0x01, 0x02, 0x03, 0x04, 0x80};
    ROUND_TRIP(QF, w, qf, 41);

    /* MyMsg b (index 1, alignment): an 8-bit length for 2 .. 150 octets (1200 bits of contents), here 2 octets with one
       12-bit element 4000 = FA0 */
    MyMsg m;
    memset(&m, 0, sizeof m);
    m.kind = b_PRESENT;
    m.u.b.nCount = 1;
    m.u.b.arr[0] = 4000;
    static const byte msg[] = {0x80, 0x00, 0xFA, 0x00};
    ROUND_TRIP(MyMsg, m, msg, 32);
    /* a length of 151 octets (0x95 + 2), one more than the contents' maximum: rejected (before the fix the range was
       2 .. 151 and the legacy decoder wrote 151 octets into a buffer of 150) */
    {
        static byte msg151[2 + 151];
        memset(msg151, 0, sizeof msg151);
        msg151[0] = 0x80;
        msg151[1] = 0x95;
        REJECT(MyMsg, msg151);
    }
    return 0;
}
