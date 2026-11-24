/* --acn-v2 checks of 013.asn1 / 013.acn (run by v4Tests/scripts/runWireTests.sh with ASan / UBSan; Issue 135): the
   bytes of CONTAINING regions whose ACN specification overrides the encoding of the contents, written by hand, encoded
   and decoded. Before the fix the regions held T's own encoding (FixO 05 80 00 00 00 80, FixB 05 C0, Eq 00 AB 80 00
   CD 40 ...) and the embedded-length forms wrote a length outside their range (ASan: a read past masksb[]). Ctl and Ext
   are controls whose bytes did not change. The Ada twin 013_wire_test.adb checks the same values and bytes. */
#include <assert.h>
#include <string.h>

#include "013.h"

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
        BitStream_AttachBuffer(&s_, (byte*)(expected), sizeof(expected));                 \
        assert(T##_ACN_Decode(&dec_, &s_, &e_));                                          \
        assert(e_ == 0);                                                                  \
        assert(T##_Equal(&(v), &dec_));                                                   \
        assert(s_.currentByte * 8 + s_.currentBit == (nBits));                            \
    } while (0)

int main(void)
{
    /* fixed-size OCTET STRING: Tf with a in 32 bits (33 bits, 5 octets) */
    FixO fo;
    memset(&fo, 0, sizeof fo);
    fo.f.a = 5;
    fo.f.b = TRUE;
    fo.z = TRUE;
    static const byte fixO[] = {0x00, 0x00, 0x00, 0x05, 0x80, 0x80};
    ROUND_TRIP(FixO, fo, fixO, 41);

    /* embedded-length OCTET STRING: a 2-bit length (5 .. 7 octets), c absent (33 bits: 5 octets) / present (49: 7) */
    EmbO eo;
    memset(&eo, 0, sizeof eo);
    eo.f.a = 5;
    eo.z = TRUE;
    static const byte embO1[] = {0x00, 0x00, 0x00, 0x00, 0xA0, 0x20};
    ROUND_TRIP(EmbO, eo, embO1, 43);
    eo.f.a = 0xAB;
    eo.f.exist.c = 1;
    eo.f.c = 0x1234;
    eo.z = FALSE;
    static const byte embO2[] = {0xA0, 0x00, 0x00, 0x15, 0x62, 0x46, 0x80, 0x00};
    ROUND_TRIP(EmbO, eo, embO2, 59);

    /* fixed-size BIT STRING: 33 bits, no padding */
    FixB fb;
    memset(&fb, 0, sizeof fb);
    fb.f.a = 5;
    fb.f.b = TRUE;
    fb.z = TRUE;
    static const byte fixB[] = {0x00, 0x00, 0x00, 0x05, 0xC0};
    ROUND_TRIP(FixB, fb, fixB, 34);

    /* embedded-length BIT STRING: a 1-bit length (33 .. 34 bits) */
    EmbB eb;
    memset(&eb, 0, sizeof eb);
    eb.f.a = 5;
    eb.z = TRUE;
    static const byte embB1[] = {0x00, 0x00, 0x00, 0x01, 0x60};
    ROUND_TRIP(EmbB, eb, embB1, 35);
    eb.f.exist.b = 1;
    eb.f.b = TRUE;
    eb.z = FALSE;
    static const byte embB2[] = {0xC0, 0x00, 0x00, 0x01, 0x60};
    ROUND_TRIP(EmbB, eb, embB2, 36);

    /* an override of the same width: a little-endian 16-bit a instead of T16's big-endian one (sizes unchanged) */
    Eq q;
    memset(&q, 0, sizeof q);
    q.f.a = 0xAB;
    q.f.b = TRUE;
    q.g.a = 0xCD;
    q.g.b = FALSE;
    q.z = TRUE;
    static const byte eq[] = {0xAB, 0x00, 0x80, 0xCD, 0x00, 0x40};
    ROUND_TRIP(Eq, q, eq, 42);

    /* the specification on the CONTAINING type assignments Box (fixed, 5 octets) and BBox (embedded, 33 .. 34 bits) */
    Tas ta;
    memset(&ta, 0, sizeof ta);
    ta.x.a = 5;
    ta.x.exist.b = 1;
    ta.x.b = TRUE;
    ta.y.a = 6;
    ta.z = TRUE;
    static const byte tas[] = {0x80, 0x00, 0x00, 0x02, 0xC0, 0x00, 0x00, 0x00, 0x01, 0xA0};
    ROUND_TRIP(Tas, ta, tas, 75);

    /* controls: present-when only (T's own encoding, 2 octets) and an external size (already the site's encoding) */
    Ctl c;
    memset(&c, 0, sizeof c);
    c.exist.f = 1;
    c.f.a = 5;
    c.f.exist.b = 1;
    c.f.b = TRUE;
    c.z = TRUE;
    static const byte ctl[] = {0xC1, 0x60, 0x40};
    ROUND_TRIP(Ctl, c, ctl, 18);
    Ext x;
    memset(&x, 0, sizeof x);
    x.f.a = 5;
    x.f.exist.b = 1;
    x.f.b = TRUE;
    x.z = TRUE;
    static const byte ext[] = {0x05, 0x80, 0x00, 0x00, 0x02, 0xC0, 0x80};
    ROUND_TRIP(Ext, x, ext, 49);
    return 0;
}
