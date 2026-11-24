/* Checks of 019.asn1 / 019.acn (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, built with -DNDEBUG and
   ASan / UBSan; Issue 173): the ACN side of MIN / MAX against a range of the parent (uper-wire/008: uPER).
   - Valid values ACN-encoded into a heap buffer of exactly REQUIRED_BYTES_FOR_ACN: bits and bytes (the default ACN
     encoding of a type assignment = its uPER bytes, pycrate; the WITH COMPONENTS sites: length field of the narrowed
     range), REQUIRED_BITS pinned, decoded back from the written octets only. Before the fix U1 took 72 bits, U1p / U2p
     and W4 did not compile, the site length fields of W3 / WC were 3 bits (2..6, a length 1 encoded with a negative
     offset), WA's 3 bits.
   - Rejected row: W1, a length above the narrowed maximum of a default-encoding string, right after its length field
     (Issue 171 check, absent before because the narrowed maximum was the base's).
   CHECK, not assert: NDEBUG removes assert. The Ada twin 019_wire_test.adb checks the same values, bytes and rows. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "019.h"

static int failures = 0;

#define CHECK(c)                                                                          \
    do {                                                                                  \
        if (!(c)) {                                                                       \
            fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #c);         \
            failures++;                                                                   \
        }                                                                                 \
    } while (0)

#define MAX1(n) ((n) > 0 ? (size_t)(n) : (size_t)1)

/* encode v with ENC into exactly REQB octets (one octet for a type of 0 bits): nBits bits, the first (nBits + 7) / 8
   bytes of `expected`; decode from those octets only. A is & (empty for the string types, which are arrays) */
#define ROUND_TRIP(T, ENC, DEC, REQB, REQb, A, v, reqBits, expected, nBits)               \
    do {                                                                                  \
        byte* buf_ = calloc(MAX1(REQB), 1);                                               \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(buf_ != NULL);                                                              \
        CHECK((REQb) == (reqBits));                                                       \
        CHECK((nBits) <= (REQb));                                                         \
        CHECK(sizeof(expected) == MAX1(((nBits) + 7) / 8));                               \
        BitStream_Init(&s_, buf_, (REQB));                                                \
        CHECK(ENC(A(v), &s_, &e_, TRUE));                                                 \
        CHECK(e_ == 0);                                                                   \
        CHECK(s_.currentByte * 8 + s_.currentBit == (nBits));                             \
        CHECK(memcmp(buf_, (expected), ((nBits) + 7) / 8) == 0);                          \
        BitStream_AttachBuffer(&s_, buf_, ((nBits) + 7) / 8);                             \
        memset(&dec_, 0, sizeof dec_);                                                    \
        CHECK(DEC(A dec_, &s_, &e_));                                                     \
        CHECK(T##_Equal(A(v), A dec_));                                                   \
        CHECK(s_.currentByte * 8 + s_.currentBit == (nBits));                             \
        free(buf_);                                                                       \
    } while (0)

#define ACN_S(T, v, reqBits, expected, nBits)                                             \
    ROUND_TRIP(T, T##_ACN_Encode, T##_ACN_Decode, T##_REQUIRED_BYTES_FOR_ACN_ENCODING,    \
               T##_REQUIRED_BITS_FOR_ACN_ENCODING, , v, reqBits, expected, nBits)
#define ACN(T, v, reqBits, expected, nBits)                                               \
    ROUND_TRIP(T, T##_ACN_Encode, T##_ACN_Decode, T##_REQUIRED_BYTES_FOR_ACN_ENCODING,    \
               T##_REQUIRED_BITS_FOR_ACN_ENCODING, &, v, reqBits, expected, nBits)

/* decode the row (zero-padded) from exactly the type's REQUIRED_BYTES (ACN): FALSE, error code ERR, end bit
   position POS; A as for ROUND_TRIP */
#define REJECT(T, A, ERR, POS, ...)                                                       \
    do {                                                                                  \
        static const byte src_[] = {__VA_ARGS__};                                         \
        static T v_;                                                                      \
        byte* buf_ = calloc(T##_REQUIRED_BYTES_FOR_ACN_ENCODING, 1);                      \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(buf_ != NULL);                                                              \
        CHECK(sizeof(src_) <= T##_REQUIRED_BYTES_FOR_ACN_ENCODING);                       \
        memcpy(buf_, src_, sizeof(src_));                                                 \
        BitStream_AttachBuffer(&s_, buf_, T##_REQUIRED_BYTES_FOR_ACN_ENCODING);           \
        memset(&v_, 0, sizeof v_);                                                        \
        CHECK(!T##_ACN_Decode(A v_, &s_, &e_));                                           \
        CHECK(e_ == (ERR));                                                               \
        CHECK(s_.currentByte * 8 + s_.currentBit == (POS));                               \
        free(buf_);                                                                       \
    } while (0)

/* an INTEGER value: ACN round trip */
#define INT_A(T, val, reqBits, nBits, ...)                                                \
    do {                                                                                  \
        static const byte x_[] = {__VA_ARGS__};                                           \
        T v_ = (val);                                                                     \
        ACN(T, v_, reqBits, x_, nBits);                                                   \
    } while (0)

int main(void)
{
    /* default ACN = the uPER body: the reference chain U1 ::= U0 (0..255) (was 72 bits) and the A / B shapes */
    INT_A(U1, 0, 8, 8, 0x00);
    INT_A(U1, 255, 8, 8, 0xFF);
    INT_A(I1, 0, 3, 3, 0x00);
    INT_A(I1, 5, 3, 3, 0xA0);
    INT_A(I2, 3, 3, 3, 0x00);
    INT_A(I2, 10, 3, 3, 0xE0);
    {
        static S1 s1;
        static O1 o1;
        static const byte s1a[] = {0x61};
        static const byte s1b[] = {0xE1, 0xC4};
        static const byte o1a[] = {0x00, 0x80};
        static const byte o1b[] = {0x80, 0x81, 0x00};
        strcpy(s1, "a");
        ACN_S(S1, s1, 15, s1a, 8);
        strcpy(s1, "ab");
        ACN_S(S1, s1, 15, s1b, 15);
        o1.arr[0] = 1;
        o1.arr[1] = 2;
        o1.nCount = 1;
        ACN(O1, o1, 17, o1a, 9);
        o1.nCount = 2;
        ACN(O1, o1, 17, o1b, 17);
    }

    /* explicit ACN specs that fit the real range (rejected before) */
    INT_A(U1p, 0, 8, 8, 0x00);
    INT_A(U1p, 255, 8, 8, 0xFF);
    INT_A(U2p, 5, 8, 8, 0x05);
    INT_A(U2p, 10, 8, 8, 0x0A);

    /* WITH COMPONENTS sites: OCTET STRING / SEQUENCE OF / BIT STRING (SIZE(MIN..2)): 1-bit length fields */
    {
        static W3 w3;
        static const byte w3a[] = {0x00, 0xA8};
        static const byte w3b[] = {0x80, 0x81, 0x6C};
        w3.x.o.arr[0] = 1;
        w3.x.o.arr[1] = 2;
        w3.x.q.arr[0] = TRUE;
        w3.x.q.arr[1] = FALSE;
        w3.x.b.arr[0] = 0x80;
        w3.x.o.nCount = 1;
        w3.x.q.nCount = 1;
        w3.x.b.nCount = 1;
        ACN(W3, w3, 23, w3a, 13);
        w3.x.o.nCount = 2;
        w3.x.q.nCount = 2;
        w3.x.b.nCount = 2;
        ACN(W3, w3, 23, w3b, 23);
    }
    /* SIZE(0..MAX) / SIZE(1..MAX) at a site: the base's 1..6 (3-bit length fields) */
    {
        static W4 w4;
        static const byte w4a[] = {0x00, 0x22, 0x20};
        static const byte w4b[] = {0xA0, 0x20, 0x40, 0x60, 0x80, 0xA0, 0xD6, 0xAB, 0x50};
        int i;
        for (i = 0; i < 6; i++) {
            w4.x.o.arr[i] = (byte)(i + 1);
            w4.x.q.arr[i] = (i % 2) == 0;
        }
        w4.x.b.arr[0] = 0xA8;
        w4.x.o.nCount = 1;
        w4.x.q.nCount = 1;
        w4.x.b.nCount = 1;
        ACN(W4, w4, 69, w4a, 19);
        w4.x.o.nCount = 6;
        w4.x.q.nCount = 6;
        w4.x.b.nCount = 6;
        ACN(W4, w4, 69, w4b, 69);
    }
    /* a default-encoding string narrowed at a site: the uPER length field (1..6) stays, REQUIRED_BITS 22 (was 50) */
    {
        static W1 w1;
        static const byte w1a[] = {0x18, 0x4E};
        static const byte w1b[] = {0x38, 0x71, 0x2C};
        strcpy(w1.x.s, "a");
        w1.x.i = 3;
        w1.x.b = TRUE;
        ACN(W1, w1, 22, w1a, 15);
        strcpy(w1.x.s, "ab");
        w1.x.i = 5;
        ACN(W1, w1, 22, w1b, 22);
    }
    /* an ASCII string with an internal length narrowed at a site: SIZE(MIN..1) 0 bits, SIZE(MIN..2) 1 bit */
    {
        static WA wa;
        static WC wc;
        static const byte waa[] = {0x61, 0x80};
        static const byte wca[] = {0x30, 0x80};
        static const byte wcb[] = {0xB0, 0xB1, 0x40};
        strcpy(wa.x.s, "a");
        wa.x.b = TRUE;
        ACN(WA, wa, 9, waa, 9);
        strcpy(wc.x.s, "a");
        wc.x.b = FALSE;
        ACN(WC, wc, 18, wca, 10);
        strcpy(wc.x.s, "ab");
        wc.x.b = TRUE;
        ACN(WC, wc, 18, wcb, 18);
    }

    /* rejected: W1 length 3 > 2, after the 3-bit length field */
    REJECT(W1, &, ERR_ACN_DECODE_W1_X_S, 3, 0x58, 0x71, 0x60);

    if (failures != 0) {
        fprintf(stderr, "%d checks failed\n", failures);
        return 1;
    }
    puts("acn-wire 019: all checks passed");
    return 0;
}
