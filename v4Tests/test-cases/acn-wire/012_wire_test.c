/* Dependents without bits present when a field of an absent component says so (run by v4Tests/scripts/runWireTests.sh,
   ASan / UBSan, legacy and --acn-v2): the encoder fails with the dependent's error code right after flg (bit 1);
   it used to succeed with error code 0 and the decoder read the dependent absent (q/issues.md Issue 108). Valid values
   encode to the bytes given and decode back. */
#include <stdio.h>
#include <string.h>
#include "012.h"

static int failures = 0;

#define REJECT_AT(val, expErr, expBit)                                                                  \
    do {                                                                                                \
        static byte buf[ZeroPDU_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                  \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        flag ok;                                                                                        \
        long pos;                                                                                       \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        ok = ZeroPDU_ACN_Encode(&(val), &bs, &err, TRUE);                                               \
        pos = (long)bs.currentByte * 8 + bs.currentBit;                                                 \
        if (ok || err != (expErr) || pos != (expBit)) {                                                 \
            printf("line %d: encode ok=%d err=%d at bit %ld, expected err %d at bit %d\n", __LINE__, ok, err, \
                   pos, (int)(expErr), (int)(expBit));                                                  \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

#define ENCODE(val, ...)                                                                                \
    do {                                                                                                \
        static const byte expected[] = {__VA_ARGS__};                                                   \
        static byte buf[ZeroPDU_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                  \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        ZeroPDU dec;                                                                                    \
        memset(buf, 0, sizeof(buf));                                                                    \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        if (!ZeroPDU_ACN_Encode(&(val), &bs, &err, TRUE) || err != 0 ||                                 \
            BitStream_GetLength(&bs) != (int)sizeof(expected) || memcmp(buf, expected, sizeof(expected)) != 0) { \
            printf("line %d: encode failed, err %d\n", __LINE__, err);                                 \
            failures++;                                                                                 \
            break;                                                                                      \
        }                                                                                               \
        BitStream_AttachBuffer(&bs, buf, sizeof(buf));                                                  \
        if (!ZeroPDU_ACN_Decode(&dec, &bs, &err) || !ZeroPDU_Equal(&dec, &(val))) {                     \
            printf("line %d: round trip failed, err %d\n", __LINE__, err);                              \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

int main(void)
{
    ZeroPDU v;
    memset(&v, 0, sizeof v);
    v.tail = 7;
    ENCODE(v, 0x03, 0x80);                                  /* flg 0, tail 7 */
    v.exist.header = 1;
    v.header.a = 1;
    v.exist.z = 1;
    v.exist.e = 1;
    ENCODE(v, 0x80, 0xE0, 0xE0);                            /* flg 1, a 1, more 1, more2 1, tail 7 */
    v.exist.header = 0;
    REJECT_AT(v, ERR_ACN_ENCODE_ZEROPDU_Z, 1);
    v.exist.z = 0;
    REJECT_AT(v, ERR_ACN_ENCODE_ZEROPDU_E, 1);
    if (failures == 0)
        printf("all checks passed\n");
    return failures == 0 ? 0 : 1;
}
