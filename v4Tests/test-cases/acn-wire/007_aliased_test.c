/* --acn-v2: one ACN field passed twice to a parameterized type, consumed directly and inside SEQUENCE OF elements (run
   by v4Tests/scripts/runWireTests.sh, ASan / UBSan). Agreeing consumers encode to the bytes computed by hand (n, d,
   the 2-bit count, the elements) and decode back; an element that disagrees with d fails with
   ERR_ACN_DET_CONSISTENCY_MISMATCH. */
#include <stdio.h>
#include <string.h>
#include "007_aliased.h"

static int failures = 0;

static void encode(const T *v, const byte *expected, int nExpected, int line)
{
    static byte buf[T_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];
    static T dec;
    BitStream bs;
    int err = 0;
    memset(buf, 0, sizeof(buf));
    memset(&dec, 0, sizeof(dec));
    BitStream_Init(&bs, buf, sizeof(buf));
    flag ok = T_ACN_Encode(v, &bs, &err, TRUE);
    if (!ok || err != 0 || BitStream_GetLength(&bs) != nExpected || memcmp(buf, expected, (size_t)nExpected) != 0) {
        printf("line %d: encode ok=%d err=%d, bytes:", line, ok, err);
        for (int k = 0; k < BitStream_GetLength(&bs); k++)
            printf(" %02X", buf[k]);
        printf("\n");
        failures++;
        return;
    }
    BitStream_AttachBuffer(&bs, buf, sizeof(buf));
    if (!T_ACN_Decode(&dec, &bs, &err) || !T_Equal(&dec, v)) {
        printf("line %d: decoding back failed, err %d\n", line, err);
        failures++;
    }
}

static void reject(const T *v, int line)
{
    static byte buf[T_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];
    BitStream bs;
    int err = 0;
    BitStream_Init(&bs, buf, sizeof(buf));
    flag ok = T_ACN_Encode(v, &bs, &err, TRUE);
    if (ok || err != ERR_ACN_DET_CONSISTENCY_MISMATCH) {
        printf("line %d: encode ok=%d err=%d, expected FALSE, 203\n", line, ok, err);
        failures++;
    }
}

int main(void)
{
    static T v;
    /* d 1 octet, one element of 1 octet (a stale second slot of 2 octets): n = 1 */
    memset(&v, 0, sizeof v);
    v.payload.d.nCount = 1; v.payload.d.arr[0] = 0x01;
    v.payload.items.nCount = 1;
    v.payload.items.arr[0].nCount = 1; v.payload.items.arr[0].arr[0] = 0x05;
    v.payload.items.arr[1].nCount = 2;
    {
        static const byte expected[] = {0x01, 0x01, 0x41, 0x40};
        encode(&v, expected, (int)sizeof(expected), __LINE__);
    }
    /* d 2 octets, no element: n = 2 */
    v.payload.d.nCount = 2; v.payload.d.arr[0] = 0xAA; v.payload.d.arr[1] = 0xBB;
    v.payload.items.nCount = 0;
    {
        static const byte expected[] = {0x02, 0xAA, 0xBB, 0x00};
        encode(&v, expected, (int)sizeof(expected), __LINE__);
    }
    /* d 2 octets, one element of 1 octet: inconsistent */
    v.payload.items.nCount = 1;
    reject(&v, __LINE__);
    if (failures != 0) {
        printf("007_aliased: %d failures\n", failures);
        return 1;
    }
    return 0;
}
