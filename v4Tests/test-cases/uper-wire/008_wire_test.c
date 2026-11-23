/* Checks of 008.asn1 / 008.acn (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, built with -DNDEBUG and
   ASan / UBSan; Issue 173): ranges and sizes given with MIN / MAX against a range of the parent.
   - Valid values encoded into a heap buffer of exactly REQUIRED_BYTES: number of bits and bytes = pycrate 0.8.1,
     REQUIRED_BITS pinned, decoded back from the written octets only. Before the fix MIN..m on lo..hi gave m..hi (the
     values below m were encoded with a negative offset: with NDEBUG the C encoder wrote past the buffer, ASan; the
     decoder returned another value), and n..MAX on lo..hi (n <= lo) gave a semi-constrained INTEGER (72 bits, U1 0 was
     0100) or "Declared type may have infinite size" (S2, O2); I8 / S8 were rejected ("do not allow any value").
   - Rejected rows: a field above the new maximum, decoded from exactly REQUIRED_BYTES: FALSE with the type's error
     code at the bit after the field.
   - R1 / R2: the ACN scaled-integer REAL maps the corrected range (R1 low 0.0, was 5.0; R2 was a compile error).
   CHECK, not assert: NDEBUG removes assert. The Ada twin 008_wire_test.adb checks the same values, bytes and rows. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "008.h"

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

#define UPER(T, v, reqBits, expected, nBits)                                              \
    ROUND_TRIP(T, T##_Encode, T##_Decode, T##_REQUIRED_BYTES_FOR_ENCODING,                \
               T##_REQUIRED_BITS_FOR_ENCODING, &, v, reqBits, expected, nBits)
#define UPER_S(T, v, reqBits, expected, nBits)                                            \
    ROUND_TRIP(T, T##_Encode, T##_Decode, T##_REQUIRED_BYTES_FOR_ENCODING,                \
               T##_REQUIRED_BITS_FOR_ENCODING, , v, reqBits, expected, nBits)
#define ACN(T, v, reqBits, expected, nBits)                                               \
    ROUND_TRIP(T, T##_ACN_Encode, T##_ACN_Decode, T##_REQUIRED_BYTES_FOR_ACN_ENCODING,    \
               T##_REQUIRED_BITS_FOR_ACN_ENCODING, &, v, reqBits, expected, nBits)

/* decode the row (zero-padded) from exactly the type's REQUIRED_BYTES (uPER): FALSE, error code ERR, end bit
   position POS; A as for ROUND_TRIP */
#define REJECT(T, A, ERR, POS, ...)                                                       \
    do {                                                                                  \
        static const byte src_[] = {__VA_ARGS__};                                         \
        static T v_;                                                                      \
        byte* buf_ = calloc(T##_REQUIRED_BYTES_FOR_ENCODING, 1);                          \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(buf_ != NULL);                                                              \
        CHECK(sizeof(src_) <= T##_REQUIRED_BYTES_FOR_ENCODING);                           \
        memcpy(buf_, src_, sizeof(src_));                                                 \
        BitStream_AttachBuffer(&s_, buf_, T##_REQUIRED_BYTES_FOR_ENCODING);               \
        memset(&v_, 0, sizeof v_);                                                        \
        CHECK(!T##_Decode(A v_, &s_, &e_));                                               \
        CHECK(e_ == (ERR));                                                               \
        CHECK(s_.currentByte * 8 + s_.currentBit == (POS));                               \
        free(buf_);                                                                       \
    } while (0)

/* an INTEGER value: uPER round trip */
#define INT_U(T, val, reqBits, nBits, ...)                                                \
    do {                                                                                  \
        static const byte x_[] = {__VA_ARGS__};                                           \
        T v_ = (val);                                                                     \
        UPER(T, v_, reqBits, x_, nBits);                                                  \
    } while (0)

int main(void)
{
    /* the reference chain: U0 ::= INTEGER (0..MAX), U1 ::= U0 (0..255) was semi-constrained (72 bits) */
    INT_U(U1, 0, 8, 8, 0x00);
    INT_U(U1, 255, 8, 8, 0xFF);
    INT_U(U2, 5, 3, 3, 0x00);
    INT_U(U2, 10, 3, 3, 0xA0);
    INT_U(V1, 5, 3, 3, 0x00);
    INT_U(V1, 10, 3, 3, 0xA0);
    {
        static const byte su0[] = {0x00, 0x80};
        static const byte su1[] = {0xFF, 0x00};
        static SU su;
        su.f = 0;
        su.g = TRUE;
        UPER(SU, su, 9, su0, 9);
        su.f = 255;
        su.g = FALSE;
        UPER(SU, su, 9, su1, 9);
    }

    /* MIN..m on lo..hi: lo..m (was m..hi) */
    INT_U(I1, 0, 3, 3, 0x00);
    INT_U(I1, 3, 3, 3, 0x60);
    INT_U(I1, 5, 3, 3, 0xA0);
    INT_U(I3, 0, 3, 3, 0x00);
    INT_U(I3, 5, 3, 3, 0xA0);
    INT_U(I4, 0, 3, 3, 0x00);
    INT_U(I4, 5, 3, 3, 0xA0);
    INT_U(I5, -10, 3, 3, 0x00);
    INT_U(I5, -5, 3, 3, 0xA0);
    INT_U(I3b, 0, 0, 0, 0x00);
    INT_U(B64, 0, 3, 3, 0x00);
    INT_U(B64, 5, 3, 3, 0xA0);
    INT_U(I8, 0, 2, 2, 0x00);
    INT_U(I8, 2, 2, 2, 0x80);

    /* n..MAX on lo..hi with n <= lo: lo..hi (was semi-constrained from n) */
    INT_U(P64, 0, 64, 64, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00);
    INT_U(P64, 18446744073709551615ULL, 64, 64, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF);
    INT_U(I2, 3, 3, 3, 0x00);
    INT_U(I2, 10, 3, 3, 0xE0);
    INT_U(I6, 5, 3, 3, 0x00);
    INT_U(I6, 10, 3, 3, 0xA0);
    INT_U(I7, 3, 3, 3, 0x00);
    INT_U(I7, 10, 3, 3, 0xE0);
    INT_U(X1, 5, 0, 0, 0x00);

    /* the root of an extensible constraint: extension bit + 0..5 (was 5..10) */
    INT_U(E3, 0, 73, 4, 0x00);
    INT_U(E3, 5, 73, 4, 0x50);

    /* controls (unchanged) */
    INT_U(C1, 5, 3, 3, 0x00);
    INT_U(C1, 10, 3, 3, 0xA0);
    INT_U(C2, 0, 4, 4, 0x00);
    INT_U(C2, 10, 4, 4, 0xA0);

    /* SIZE: MIN..m on SIZE(1..6) gives 1..m (was m..6); 0..MAX gives 1..6 (was "infinite size") */
    {
        static S1 s1;
        static S3 s3;
        static S2 s2;
        static S8 s8;
        static N1 n1;
        static const byte s1a[] = {0x61};
        static const byte s1b[] = {0xE1, 0xC4};
        static const byte s3a[] = {0x30, 0x80};
        static const byte s3b[] = {0xB0, 0xE2, 0xC6};
        static const byte s2a[] = {0x18, 0x40};
        static const byte s2b[] = {0xB8, 0x71, 0x63, 0xC9, 0x97, 0x30};
        static const byte s8a[] = {0xC2};
        static const byte n1a[] = {0x10};
        static const byte n1b[] = {0x91, 0x80};
        strcpy(s1, "a");
        UPER_S(S1, s1, 15, s1a, 8);
        strcpy(s1, "ab");
        UPER_S(S1, s1, 15, s1b, 15);
        strcpy(s3, "a");
        UPER_S(S3, s3, 23, s3a, 9);
        strcpy(s3, "abc");
        UPER_S(S3, s3, 23, s3b, 23);
        strcpy(s2, "a");
        UPER_S(S2, s2, 45, s2a, 10);
        strcpy(s2, "abcdef");
        UPER_S(S2, s2, 45, s2b, 45);
        strcpy(s8, "a");
        UPER_S(S8, s8, 7, s8a, 7);
        strcpy(n1, "1");
        UPER_S(N1, n1, 9, n1a, 5);
        strcpy(n1, "12");
        UPER_S(N1, n1, 9, n1b, 9);
    }
    {
        static O1 o1;
        static O3 o3;
        static O2 o2;
        static const byte o1a[] = {0x00, 0x80};
        static const byte o1b[] = {0x80, 0x81, 0x00};
        static const byte o3a[] = {0x00, 0x40};
        static const byte o3b[] = {0x80, 0x40, 0x80, 0xC0};
        static const byte o2a[] = {0x00, 0x20};
        static const byte o2b[] = {0xA0, 0x20, 0x40, 0x60, 0x80, 0xA0, 0xC0};
        int i;
        for (i = 0; i < 6; i++) {
            o2.arr[i] = (byte)(i + 1);
        }
        o1.arr[0] = 1;
        o1.arr[1] = 2;
        o3.arr[0] = 1;
        o3.arr[1] = 2;
        o3.arr[2] = 3;
        o1.nCount = 1;
        UPER(O1, o1, 17, o1a, 9);
        o1.nCount = 2;
        UPER(O1, o1, 17, o1b, 17);
        o3.nCount = 1;
        UPER(O3, o3, 26, o3a, 10);
        o3.nCount = 3;
        UPER(O3, o3, 26, o3b, 26);
        o2.nCount = 1;
        UPER(O2, o2, 51, o2a, 11);
        o2.nCount = 6;
        UPER(O2, o2, 51, o2b, 51);
    }
    {
        static BS1 b1;
        static BS3 b3;
        static Q2 q2;
        static Q3 q3;
        static const byte b1a[] = {0x40};
        static const byte b1b[] = {0xC0};
        static const byte b3a[] = {0x20};
        static const byte b3b[] = {0xA8};
        b1.nCount = 1;
        b1.arr[0] = 0x80;
        UPER(BS1, b1, 3, b1a, 2);
        b1.nCount = 2;
        b1.arr[0] = 0x80;
        UPER(BS1, b1, 3, b1b, 3);
        b3.nCount = 1;
        b3.arr[0] = 0x80;
        UPER(BS3, b3, 5, b3a, 3);
        b3.nCount = 3;
        b3.arr[0] = 0xA0;
        UPER(BS3, b3, 5, b3b, 5);
        q2.nCount = 1;
        q2.arr[0] = TRUE;
        UPER(Q2, q2, 3, b1a, 2);
        q2.nCount = 2;
        q2.arr[1] = FALSE;
        UPER(Q2, q2, 3, b1b, 3);
        q3.nCount = 1;
        q3.arr[0] = TRUE;
        UPER(Q3, q3, 5, b3a, 3);
        q3.nCount = 3;
        q3.arr[1] = FALSE;
        q3.arr[2] = TRUE;
        UPER(Q3, q3, 5, b3b, 5);
    }

    /* ACN scaled-integer REAL: R1 0.0..5.0, R2 3.0..10.0 in 8 bits */
    {
        static const byte r00[] = {0x00};
        static const byte rff[] = {0xFF};
        R1 r1 = 0.0;
        R2 r2 = 3.0;
        ACN(R1, r1, 8, r00, 8);
        r1 = 5.0;
        ACN(R1, r1, 8, rff, 8);
        ACN(R2, r2, 8, r00, 8);
        r2 = 10.0;
        ACN(R2, r2, 8, rff, 8);
    }

    /* rejected: a field above the new maximum */
    REJECT(I1, &, ERR_UPER_DECODE_I1, 3, 0xC0);
    REJECT(I1, &, ERR_UPER_DECODE_I1, 3, 0xE0);
    REJECT(I3, &, ERR_UPER_DECODE_I3, 3, 0xC0);
    REJECT(I4, &, ERR_UPER_DECODE_I4, 3, 0xE0);
    REJECT(I5, &, ERR_UPER_DECODE_I5, 3, 0xC0);
    REJECT(B64, &, ERR_UPER_DECODE_B64, 3, 0xE0);
    REJECT(I8, &, ERR_UPER_DECODE_I8, 2, 0xC0);
    REJECT(U2, &, ERR_UPER_DECODE_U2, 3, 0xC0);
    REJECT(V1, &, ERR_UPER_DECODE_V1, 3, 0xE0);
    REJECT(E3, &, ERR_UPER_DECODE_E3, 4, 0x60);
    /* C: the type's code (Ada: ERR_INSUFFICIENT_DATA for a length outside the range) */
    REJECT(S3, , ERR_UPER_DECODE_S3, 2, 0xF0, 0xE2, 0xC6);
    REJECT(O3, &, ERR_UPER_DECODE_O3, 2, 0xC0, 0x40, 0x80, 0xC0);
    REJECT(BS3, &, ERR_UPER_DECODE_BS3, 2, 0xF8);
    REJECT(Q3, &, ERR_UPER_DECODE_Q3, 2, 0xF8);

    if (failures != 0) {
        fprintf(stderr, "%d checks failed\n", failures);
        return 1;
    }
    puts("uper-wire 008: all checks passed");
    return 0;
}
