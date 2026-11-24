/* Checks of 016.asn1 / 016.acn (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, built with -DNDEBUG and
   ASan / UBSan; Issue 168, Issue 92): valid values (the longest the narrowing allows) encoded with uPER and ACN into a
   heap buffer of exactly REQUIRED_BYTES (before the fix too small: with NDEBUG the runtime's assert is gone and the
   encoder wrote past the buffer, an ASan heap-buffer-overflow, or skipped a write and returned a short stream), the
   number of bits and the bytes compared with the ones the compiler wrote before the fix (except OctR / BitR: the ACN
   wire change of a CONTAINING of a range-narrowed string, approved), REQUIRED_BITS
   pinned, decoded back from a buffer of the written octets only (a 'size deduced' decoder deduces the count from the
   end of the buffer). CHECK, not assert: NDEBUG removes assert. The Ada twin 016_wire_test.adb checks the same values
   and bytes. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "016.h"

#define CHECK(c)                                                                          \
    do {                                                                                  \
        if (!(c)) {                                                                       \
            fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #c);         \
            abort();                                                                      \
        }                                                                                 \
    } while (0)

/* encode v with ENC into exactly REQB octets: nBits bits, the bytes `expected`; decode from those octets only */
#define ROUND_TRIP(T, ENC, DEC, REQB, REQb, v, reqBits, expected, nBits)                  \
    do {                                                                                  \
        byte* buf_ = calloc((REQB), 1);                                                   \
        static T dec_;                                                                    \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(buf_ != NULL);                                                              \
        CHECK((REQb) == (reqBits));                                                       \
        CHECK((nBits) <= (REQb));                                                         \
        CHECK(sizeof(expected) == ((nBits) + 7) / 8);                                     \
        BitStream_Init(&s_, buf_, (REQB));                                                \
        CHECK(ENC(&(v), &s_, &e_, TRUE));                                                 \
        CHECK(e_ == 0);                                                                   \
        CHECK(s_.currentByte * 8 + s_.currentBit == (nBits));                             \
        CHECK(memcmp(buf_, (expected), sizeof(expected)) == 0);                           \
        BitStream_AttachBuffer(&s_, buf_, sizeof(expected));                              \
        memset(&dec_, 0, sizeof dec_);                                                    \
        CHECK(DEC(&dec_, &s_, &e_));                                                      \
        CHECK(T##_Equal(&(v), &dec_));                                                    \
        CHECK(s_.currentByte * 8 + s_.currentBit == (nBits));                             \
        free(buf_);                                                                       \
    } while (0)

#define UPER(T, v, reqBits, expected, nBits)                                              \
    ROUND_TRIP(T, T##_Encode, T##_Decode, T##_REQUIRED_BYTES_FOR_ENCODING,                \
               T##_REQUIRED_BITS_FOR_ENCODING, v, reqBits, expected, nBits)
#define ACN(T, v, reqBits, expected, nBits)                                               \
    ROUND_TRIP(T, T##_ACN_Encode, T##_ACN_Decode, T##_REQUIRED_BYTES_FOR_ACN_ENCODING,    \
               T##_REQUIRED_BITS_FOR_ACN_ENCODING, v, reqBits, expected, nBits)

int main(void)
{
    /* Issue 168 (p9): value set "ab" | "a" on an IA5String (SIZE(1..6)) at a site: ACN 3-bit length (1..6) + 2 chars */
    static P9 p9;
    static const byte p9b[] = {0x38, 0x71, 0x40};
    strcpy(p9.x.s, "ab");
    p9.x.b = TRUE;
    UPER(P9, p9, 46, p9b, 18);
    ACN(P9, p9, 18, p9b, 18);

    /* Issue 92: fixed sizes given to the components of a variable-size SEQUENCE; the uPER site calls Base92's encoder */
    static Fixed92 f92;
    static const byte f92u[] = {0x18, 0x55, 0x2D, 0x2D, 0x2D, 0x2D, 0x2D, 0x2D, 0x2D, 0x2D, 0x2D, 0x28, 0xA7, 0xFF,
                                0xFF, 0x80};
    static const byte f92a[] = {0xD2, 0xD2, 0xD2, 0xD2, 0xD2, 0xD2, 0xD2, 0xD2, 0xD2, 0xD2, 0xFF, 0xFF, 0xF8};
    f92.cmds.nCount = 1;
    f92.cmds.arr[0] = TRUE;
    f92.buf.nCount = 10;
    memset(f92.buf.arr, 0xA5, 10);
    f92.bits.nCount = 20;
    memset(f92.bits.arr, 0xFF, 2);
    f92.bits.arr[2] = 0xF0;
    CHECK(Fixed92_REQUIRED_BITS_FOR_ENCODING == Base92_REQUIRED_BITS_FOR_ENCODING);
    UPER(Fixed92, f92, 1830, f92u, 121);
    ACN(Fixed92, f92, 101, f92a, 101);

    /* CONTAINING of a subtype that narrows IA5String (SIZE(1..3)) to SIZE(3): the contents stay 25 bits = 4 octets
       (OCTET) / 25 bits (BIT), no length field appears; Bit18 did not fit before (23 bits) */
    static Oct18 o18;
    static Bit18 b18;
    static const byte o18u[] = {0x03, 0xB0, 0xE2, 0xC7, 0x80};
    static const byte b18u[] = {0x18, 0xB0, 0xE2, 0xC7, 0x80};
    static const byte x18a[] = {0xB0, 0xE2, 0xC7, 0x80};
    strcpy(o18.o.s, "abc");
    o18.o.k = TRUE;
    o18.b = TRUE;
    strcpy(b18.p.s, "abc");
    b18.p.k = TRUE;
    b18.b = TRUE;
    UPER(Oct18, o18, 33, o18u, 33);
    ACN(Oct18, o18, 25, x18a, 25);
    UPER(Bit18, b18, 33, b18u, 33);
    ACN(Bit18, b18, 25, x18a, 25);

    /* 'size deduced' data followed by the narrowed string (4-bit length + 2 chars = 18 bits): maximal and short data */
    static Sub15 s15;
    static const byte s15u[] = {0xA5, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x51, 0xC3, 0x88};
    static const byte s15a[] = {0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x1C, 0x38, 0x80};
    static const byte s15s[] = {0x55, 0x55, 0x55, 0x1C, 0x38, 0x80};
    s15.data.nCount = 10;
    memset(s15.data.arr, 0x55, 10);
    strcpy(s15.s, "ab");
    UPER(Sub15, s15, 158, s15u, 102);
    ACN(Sub15, s15, 98, s15a, 98);
    s15.data.nCount = 3;
    ACN(Sub15, s15, 98, s15s, 42);

    /* the string child through a type assignment (s Str10) */
    static Outer10 o10;
    static const byte o10b[] = {0x16, 0x1C, 0x58, 0xE0};
    strcpy(o10.x.s, "abc");
    o10.x.b = TRUE;
    UPER(Outer10, o10, 146, o10b, 27);
    ACN(Outer10, o10, 27, o10b, 27);

    /* OPTIONAL components made PRESENT and narrowed (string "ab", BIT STRING SIZE(3)) */
    static Outer12 o12;
    static const byte o12u[] = {0xC7, 0x0E, 0x20, 0xFC};
    static const byte o12a[] = {0x1C, 0x38, 0xBC};
    o12.x.exist.s = 1;
    o12.x.exist.bs = 1;
    strcpy(o12.x.s, "ab");
    o12.x.bs.nCount = 3;
    o12.x.bs.arr[0] = 0xE0;
    o12.x.b = TRUE;
    UPER(Outer12, o12, 133, o12u, 30);
    ACN(Outer12, o12, 22, o12a, 22);

    /* BIT STRINGs with named bits, one through a type assignment (Flags9): uPER did not fit before (12 bits) */
    static Outer9 o9;
    static const byte o9u[] = {0x11, 0xFE, 0x1F, 0x80};
    static const byte o9a[] = {0xFF, 0xF0};
    o9.x.f.nCount = 8;
    o9.x.f.arr[0] = 0xFF;
    o9.x.g.nCount = 4;
    o9.x.g.arr[0] = 0xF0;
    UPER(Outer9, o9, 177, o9u, 25);
    ACN(Outer9, o9, 12, o9a, 12);

    /* OCTET and BIT STRING CONTAINING a narrowed subtype (ACN: 4-bit length + 2 chars + k) */
    static Both8 b8;
    static const byte b8u[] = {0x03, 0x1C, 0x38, 0xA0, 0x18, 0x1C, 0x38, 0xA0, 0x80};
    static const byte b8a[] = {0x1C, 0x38, 0xA0, 0x1C, 0x38, 0xB0};
    strcpy(b8.o.s, "ab");
    b8.o.k = TRUE;
    strcpy(b8.p.s, "ab");
    b8.p.k = TRUE;
    b8.b = TRUE;
    UPER(Both8, b8, 177, b8u, 65);
    ACN(Both8, b8, 44, b8a, 44);

    /* a base of SIZE(1..70000) narrowed to SIZE(2): a general length of one octet + 2 chars */
    static Outer7 o7;
    static const byte o7b[] = {0x02, 0xF5, 0xEA};
    strcpy(o7.x.s, "zz");
    o7.x.b = TRUE;
    UPER(Outer7, o7, 490033, o7b, 23);
    ACN(Outer7, o7, 23, o7b, 23);

    /* the base's 'encoding ASCII' is not the subtype's (Issue 83): default encoding, 5-bit length + 2 chars */
    static Sub5 s5;
    static const byte s5b[] = {0x0E, 0x1C, 0x50};
    strcpy(s5.s, "ab");
    s5.b = TRUE;
    UPER(Sub5, s5, 146, s5b, 20);
    ACN(Sub5, s5, 20, s5b, 20);

    /* the elements of a SEQUENCE OF narrowed to BIT STRING SIZE(8): uPER inlines the narrowed body, which wrote the
       7-bit length field of 0..100 but counted none (17 bits; the octets were skipped, the encoder returned TRUE) */
    static Outer5 o5;
    static const byte o5u[] = {0x88, 0xFF, 0x10, 0x1E};
    static const byte o5a[] = {0xFF, 0x87, 0x80};
    o5.x.nCount = 2;
    o5.x.arr[0].nCount = 8;
    o5.x.arr[0].arr[0] = 0xFF;
    o5.x.arr[1].nCount = 8;
    o5.x.arr[1].arr[0] = 0x0F;
    UPER(Outer5, o5, 215, o5u, 31);
    ACN(Outer5, o5, 17, o5a, 17);

    /* the ACN wire change of the fix (approved): a CONTAINING of a subtype that narrows a default-encoding string to a
       range ("ab" | "a" of SIZE(1..6)). The contents now take 11 .. 18 bits = 2 .. 3 octets (OCTET) / 11 .. 18 bits
       (BIT), so the region gets a 1-bit length field. Before: 9 .. 16 bits, i.e. always 2 octets without a length
       field (OCTET: "a" = 186080, 17 bits; BIT: "a" = 430E, a 3-bit length of 9..16), and "ab" did not fit (25 > 17,
       22 > 20 bits). uPER unchanged (X.691 general length). */
    static OctR oR;
    static BitR bR;
    static const byte oRu1[] = {0x02, 0x18, 0x60, 0x80};
    static const byte oRa1[] = {0x0C, 0x30, 0x40};
    static const byte bRu1[] = {0x10, 0x18, 0x60, 0x80};
    static const byte bRa1[] = {0x03, 0x0E};
    static const byte oRu2[] = {0x03, 0x38, 0x71, 0x40, 0x80};
    static const byte oRa2[] = {0x9C, 0x38, 0xA0, 0x40};
    static const byte bRu2[] = {0x18, 0x38, 0x71, 0x40, 0x80};
    static const byte bRa2[] = {0xE7, 0x0E, 0x2C};
    strcpy(oR.o.s, "a");
    oR.o.b = TRUE;
    oR.z = TRUE;
    strcpy(bR.p.s, "a");
    bR.p.b = TRUE;
    bR.z = TRUE;
    UPER(OctR, oR, 57, oRu1, 25);
    ACN(OctR, oR, 26, oRa1, 18);
    UPER(BitR, bR, 57, bRu1, 25);
    ACN(BitR, bR, 22, bRa1, 15);
    strcpy(oR.o.s, "ab");
    strcpy(bR.p.s, "ab");
    UPER(OctR, oR, 57, oRu2, 33);
    ACN(OctR, oR, 26, oRa2, 26);
    UPER(BitR, bR, 57, bRu2, 33);
    ACN(BitR, bR, 22, bRa2, 22);
    return 0;
}
