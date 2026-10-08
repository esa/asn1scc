/* ESACERT #74995: an ACN size determinant whose encoding is wider than its
   ASN.1 constraint must not reach the OCTET / BIT STRING copy unchecked.
   Each malformed stream must make the decoder fail without writing past
   the 10-element array; each valid stream must still decode. */
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
        printf("FAIL %-6s %-24s ok=%d err=%d\n", #T, #bytes, (int)ok, err);     \
        failures++;                                                             \
    } else {                                                                    \
        printf("ok   %-6s %-24s ok=%d err=%d\n", #T, #bytes, (int)ok, err);     \
    }                                                                           \
} while (0)

int main(void)
{
    /* 4-bit pos-int determinant: 15 > SIZE max 10 */
    static const byte oct4_len15[] = { 0xF0 };
    static const byte oct4_len10[] = { 0xA0 };
    /* 32-bit pos-int determinant: 64 > 10 */
    static const byte oct32_len64[] = { 0x00, 0x00, 0x00, 0x40 };
    static const byte oct32_len10[] = { 0x00, 0x00, 0x00, 0x0A };
    /* BIT STRING, 4-bit determinant: 15 bits into a 10-bit array */
    static const byte bit4_len15[] = { 0xF0 };
    static const byte bit4_len10[] = { 0xA0 };
    /* 8-bit two's complement determinant: 100 and -3 */
    static const byte twos8_len100[] = { 0x64 };
    static const byte twos8_lenm3[] = { 0xFD };
    static const byte twos8_len10[] = { 0x0A };
    /* determinant passed to the OCTET STRING as an ACN parameter */
    static const byte param4_len15[] = { 0xF0 };
    static const byte param4_len10[] = { 0xA0 };
    /* 8-bit determinant for SIZE (0..255): every wire value is valid (#360) */
    static const byte full8_len255[] = { 0xFF };

    DECODE(Oct4, oct4_len15, 0);
    DECODE(Oct4, oct4_len10, 1);
    DECODE(Oct32, oct32_len64, 0);
    DECODE(Oct32, oct32_len10, 1);
    DECODE(Bit4, bit4_len15, 0);
    DECODE(Bit4, bit4_len10, 1);
    DECODE(Twos8, twos8_len100, 0);
    DECODE(Twos8, twos8_lenm3, 0);
    DECODE(Twos8, twos8_len10, 1);
    DECODE(Param4, param4_len15, 0);
    DECODE(Param4, param4_len10, 1);
    DECODE(Full8, full8_len255, 1);

    if (failures != 0) {
        printf("%d test(s) failed\n", failures);
        return 1;
    }
    printf("all tests passed\n");
    return 0;
}
