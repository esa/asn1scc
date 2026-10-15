/* OCTET / BIT STRING (CONTAINING X) [size <field>]: the length read from the
   stream sizes the temporary stream view of the contained type. It must be
   checked against the capacity of that view before it is used: a length above
   the capacity, or a negative one, must make the decoder fail with a non-zero
   error code without writing past the static buffer. Valid streams must still
   decode. */
#include <stdio.h>
#include <string.h>
#include "a.h"

static int failures = 0;

#define DECODE(T, bytes, expectOk) do {                                         \
    static byte buf[256];                                                       \
    BitStream bs;                                                               \
    T v;                                                                        \
    int err = 0;                                                                \
    flag ok;                                                                    \
    memset(buf, 0x41, sizeof(buf));                                             \
    memcpy(buf, bytes, sizeof(bytes));                                          \
    BitStream_AttachBuffer(&bs, buf, sizeof(buf));                              \
    memset(&v, 0, sizeof(v));                                                   \
    ok = T##_ACN_Decode(&v, &bs, &err);                                         \
    if ((ok != 0) != (expectOk) || (!(expectOk) && err == 0)) {                 \
        printf("FAIL %-10s %-24s ok=%d err=%d\n", #T, #bytes, (int)ok, err);    \
        failures++;                                                             \
    } else {                                                                    \
        printf("ok   %-10s %-24s ok=%d err=%d\n", #T, #bytes, (int)ok, err);    \
    }                                                                           \
} while (0)

int main(void)
{
    /* 16-bit length: 65535 and 256 > capacity 2; 2 is valid */
    static const byte oct16_len65535[] = { 0xFF, 0xFF };
    static const byte oct16_len256[]   = { 0x01, 0x00 };
    static const byte oct16_len2[]     = { 0x00, 0x02, 0x11, 0x22 };
    /* 32-bit length: 0x80000000 and 0xFFFFFFFF are negative after a cast to int */
    static const byte oct32_len2p31[]  = { 0x80, 0x00, 0x00, 0x00 };
    static const byte oct32_lenmax[]   = { 0xFF, 0xFF, 0xFF, 0xFF };
    static const byte oct32_len2[]     = { 0x00, 0x00, 0x00, 0x02, 0x11, 0x22 };
    /* 8-bit two's complement length: -3 */
    static const byte twos8_lenm3[]    = { 0xFD };
    static const byte twos8_len2[]     = { 0x02, 0x11, 0x22 };
    /* 'size deduced' contents: 65535 > capacity 15; 3 bytes = items 1, 2, then tail 7 */
    static const byte ded_len65535[]   = { 0xFF, 0xFF };
    static const byte ded_len3[]       = { 0x00, 0x03, 0x00, 0x10, 0x02, 0x07 };
    /* BIT STRING, 16-bit length in bits: 65535 > capacity 16; 16 is valid */
    static const byte bit16_len65535[] = { 0xFF, 0xFF };
    static const byte bit16_len16[]    = { 0x00, 0x10, 0x11, 0x22 };

    DECODE(OctLen16, oct16_len65535, 0);
    DECODE(OctLen16, oct16_len256, 0);
    DECODE(OctLen16, oct16_len2, 1);
    DECODE(OctLen32, oct32_len2p31, 0);
    DECODE(OctLen32, oct32_lenmax, 0);
    DECODE(OctLen32, oct32_len2, 1);
    DECODE(OctTwos8, twos8_lenm3, 0);
    DECODE(OctTwos8, twos8_len2, 1);
    DECODE(OctDeduced, ded_len65535, 0);
    DECODE(OctDeduced, ded_len3, 1);
    DECODE(BitLen16, bit16_len65535, 0);
    DECODE(BitLen16, bit16_len16, 1);

    if (failures != 0) {
        printf("%d test(s) failed\n", failures);
        return 1;
    }
    printf("all tests passed\n");
    return 0;
}
