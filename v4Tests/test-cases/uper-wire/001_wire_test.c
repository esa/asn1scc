/* Byte-exact uPER checks of 001.asn1 (run by v4Tests/scripts/runWireTests.sh, Issue 94): OCTET / BIT STRING
   (CONTAINING X) has a general length (X.691 10.9; bits for a BIT STRING) and the encoding of X padded to whole
   octets, one zero octet when it is empty (10.1.3). Expected bytes: X.691, cross-checked with pycrate 0.8.1 (the large
   ones are built by the formulas below, which were checked against pycrate's encoder). */
#include <assert.h>
#include <string.h>

#include "001.h"

/* encode v, compare with the expected bytes, decode them and compare with v */
#define ROUND_TRIP(T, v, expected, n)                                                     \
    do {                                                                                  \
        static byte buf_[T##_REQUIRED_BYTES_FOR_ENCODING];                                \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        BitStream_Init(&s_, buf_, sizeof buf_);                                           \
        assert(T##_Encode(&(v), &s_, &e_, TRUE));                                         \
        assert(BitStream_GetLength(&s_) == (long)(n));                                    \
        assert(memcmp(buf_, (expected), (n)) == 0);                                       \
        BitStream_AttachBuffer(&s_, buf_, (n));                                           \
        assert(T##_Decode(&dec_, &s_, &e_));                                              \
        assert(T##_Equal(&(v), &dec_));                                                   \
    } while (0)

/* decode the given bytes; ok: whether the decoder must accept them */
#define DECODE(T, bytes, ok)                                                              \
    do {                                                                                  \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        BitStream_AttachBuffer(&s_, (byte*)(bytes), sizeof(bytes));                       \
        assert(T##_Decode(&dec_, &s_, &e_) == (ok));                                      \
        assert((ok) || e_ != 0);                                                          \
    } while (0)

static byte expected[80000];

/* the length octets of a last part of n < 16K units (X.691 10.9.3.6, 10.9.3.7) */
static int glen(byte* p, int n)
{
    if (n < 128) {
        p[0] = (byte)n;
        return 1;
    }
    p[0] = (byte)(0x80 | (n >> 8));
    p[1] = (byte)(n & 0xFF);
    return 2;
}

static void fill(byte* p, int n, int k)
{
    for (int i = 0; i < n; i++) {
        p[i] = (byte)(i % k);
    }
}

int main(void)
{
    {
        static const byte x[] = {0x02, 0x05, 0x80, 0x10, 0x01, 0x1d, 0x01, 0x00, 0x80};
        P v;
        P_Initialize(&v);
        v.f.a = 5;
        v.f.b = TRUE;
        v.g.a = 1;
        v.g.s.nCount = 3;
        v.g.s.arr[0] = TRUE;
        v.g.s.arr[1] = FALSE;
        v.g.s.arr[2] = TRUE;
        v.z = TRUE;
        ROUND_TRIP(P, v, x, sizeof x);
    }
    {
        static const byte x[] = {0x01, 0x00};
        static const byte y[] = {0x08, 0x00};
        NulOct v;
        NulBit w;
        NulOct_Initialize(&v);
        NulBit_Initialize(&w);
        ROUND_TRIP(NulOct, v, x, sizeof x);
        ROUND_TRIP(NulBit, w, y, sizeof y);
    }
    {
        static const byte x[] = {0x02, 0x05, 0x80, 0x00};
        T91 v;
        T91_Initialize(&v);
        v.x.a = 5;
        v.x.b = TRUE;
        v.y = FALSE;
        ROUND_TRIP(T91, v, x, sizeof x);
    }
    {
        static const byte x[] = {0x03, 0x10, 0x05, 0x80};
        Nested v;
        Nested_Initialize(&v);
        v.o.m.a = 5;
        v.o.m.b = TRUE;
        ROUND_TRIP(Nested, v, x, sizeof x);
    }
    {
        static const byte x[] = {0x40, 0x80, 0x40, 0x00, 0x80, 0xa0, 0x00};
        Lst v;
        Lst_Initialize(&v);
        v.nCount = 2;
        v.arr[0].a = 1;
        v.arr[0].b = FALSE;
        v.arr[1].a = 2;
        v.arr[1].b = TRUE;
        ROUND_TRIP(Lst, v, x, sizeof x);
    }
    {
        static const byte x[] = {0x88, 0x03, 0xc0, 0x00};
        Ch v;
        Ch_Initialize(&v);
        v.kind = c2_PRESENT;
        v.u.c2.a = 7;
        v.u.c2.b = TRUE;
        ROUND_TRIP(Ch, v, x, sizeof x);
    }
    {
        static const byte x[] = {0x40};
        static const byte y[] = {0x81, 0x55, 0x80, 0x00};
        Opt v;
        Opt_Initialize(&v);
        v.exist.o = 0;
        v.t = TRUE;
        ROUND_TRIP(Opt, v, x, sizeof x);
        v.exist.o = 1;
        v.o.a = 0xAB;
        v.o.b = FALSE;
        v.t = FALSE;
        ROUND_TRIP(Opt, v, y, sizeof y);
    }
    {
        static const byte x[] = {0x01, 0x00};
        static CBig v;
        CBig_Initialize(&v);
        v.d.nCount = 0;
        ROUND_TRIP(CBig, v, x, sizeof x);
    }
    {
        static C127 v127;
        static C128 v128;
        static C16383 v16383;
        static C16384 v16384;
        static B16 v16;
        static B2048 v2048;
        int n;
        fill(v127.arr, 127, 256);
        n = glen(expected, 127);
        fill(expected + n, 127, 256);
        ROUND_TRIP(C127, v127, expected, n + 127);
        fill(v128.arr, 128, 256);
        n = glen(expected, 128);
        fill(expected + n, 128, 256);
        ROUND_TRIP(C128, v128, expected, n + 128);
        fill(v16383.arr, 16383, 256);
        n = glen(expected, 16383);
        fill(expected + n, 16383, 256);
        ROUND_TRIP(C16383, v16383, expected, n + 16383);
        /* one part of 16K octets, then a last part of length 0 */
        fill(v16384.arr, 16384, 256);
        expected[0] = 0xC1;
        fill(expected + 1, 16384, 256);
        expected[16385] = 0x00;
        ROUND_TRIP(C16384, v16384, expected, 16386);
        /* BIT STRING: 128 bits */
        fill(v16.arr, 16, 256);
        n = glen(expected, 128);
        fill(expected + n, 16, 256);
        ROUND_TRIP(B16, v16, expected, n + 16);
        /* BIT STRING: one part of 16K bits, then a last part of length 0 */
        fill(v2048.arr, 2048, 256);
        expected[0] = 0xC1;
        fill(expected + 1, 2048, 256);
        expected[2049] = 0x00;
        ROUND_TRIP(B2048, v2048, expected, 2050);
    }
    {
        /* CBig with 70000 octets: the contents are Big (C4, 64K octets, the length of the last 4464, them: 70003
           octets), sent as C4, their first 64K octets, the length of the last 4467, them */
        static CBig v;
        static byte contents[70003];
        int n;
        v.d.nCount = 70000;
        fill(v.d.arr, 70000, 251);
        contents[0] = 0xC4;
        fill(contents + 1, 65536, 251);
        n = 1 + 65536;
        n += glen(contents + n, 70000 - 65536);
        for (int i = 65536; i < 70000; i++) {
            contents[n++] = (byte)(i % 251);
        }
        assert(n == 70003);
        expected[0] = 0xC4;
        memcpy(expected + 1, contents, 65536);
        n = 1 + 65536;
        n += glen(expected + n, 70003 - 65536);
        memcpy(expected + n, contents + 65536, 70003 - 65536);
        ROUND_TRIP(CBig, v, expected, n + 70003 - 65536);
    }

    /* decoders: any length up to the maximum, also 0 and bits that are not a multiple of 8 (the decoder of X decides
       on the contents) */
    {
        static const byte nul0[] = {0x00};               /* NULL sent with no octet (as asn1tools does) */
        static const byte ch9[] = {0x84, 0x83, 0xc0};    /* c2: 9 bits, a = 7, b = TRUE */
        static const byte short1[] = {0x01, 0x05};       /* Inner needs 9 bits, the region has 8 */
        static const byte above[] = {0x03, 0x05, 0x80, 0x00}; /* 3 octets, the maximum is 2 */
        static const byte twoOctets[] = {0x80, 0x02, 0x05, 0x80}; /* a length below 128 in two octets */
        static const byte c0[] = {0xC0, 0x00};           /* not a fragment header */
        static const byte c5[] = {0xC5, 0x00};
        static const byte bitAbove[] = {0x0a, 0x05, 0x80}; /* 10 bits, the maximum is 8 */
        DECODE(NulOct, nul0, TRUE);
        DECODE(Ch, ch9, TRUE);
        DECODE(OctBox, short1, FALSE);
        DECODE(OctBox, above, FALSE);
        DECODE(OctBox, twoOctets, FALSE);
        DECODE(NulOct, c0, FALSE);
        DECODE(NulOct, c5, FALSE);
        DECODE(NulBit, bitAbove, FALSE);
    }
    return 0;
}
