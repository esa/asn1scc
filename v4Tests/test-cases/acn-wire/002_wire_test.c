/* ACN fields of a referenced unsigned INTEGER type encoded as ASCII (run by v4Tests/scripts/runWireTests.sh): the
   encoders write the digits without a sign, as a component of the type does, and the decoders read them back. */
#include <stdio.h>
#include <string.h>
#include "002.h"

static int failures = 0;

#define CHECK(T, val, expected, nExpected)                                                              \
    do {                                                                                                \
        byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING];                                                  \
        BitStream bs;                                                                                   \
        T dec;                                                                                          \
        int err = 0;                                                                                    \
        memset(buf, 0xAA, sizeof(buf));                                                                 \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        if (!T##_ACN_Encode(&(val), &bs, &err, TRUE) || BitStream_GetLength(&bs) != (nExpected) ||      \
            memcmp(buf, expected, nExpected) != 0) {                                                    \
            printf("%s: unexpected encoding\n", #T);                                                    \
            failures++;                                                                                 \
        }                                                                                               \
        BitStream_AttachBuffer(&bs, buf, sizeof(buf));                                                  \
        if (!T##_ACN_Decode(&dec, &bs, &err) || !T##_Equal(&dec, &(val))) {                             \
            printf("%s: unexpected decoding\n", #T);                                                    \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

int main(void)
{
    Fn fn;
    Fc fc;
    Cm cm;
    Fn_Initialize(&fn);
    Fc_Initialize(&fc);
    Cm_Initialize(&cm);
    fn.s.nCount = 2;
    fn.s.arr[0] = 'A';
    fn.s.arr[1] = 'B';
    fc.s.nCount = 2;
    fc.s.arr[0] = 'A';
    fc.s.arr[1] = 'B';
    cm.a = 2;
    CHECK(Fn, fn, "2\0AB", 4);
    CHECK(Fc, fc, "02AB", 4);
    CHECK(Cm, cm, "2\0", 2);
    fn.s.nCount = 12;
    memset(fn.s.arr, 'x', 12);
    CHECK(Fn, fn, "12\0xxxxxxxxxxxx", 15);
    return failures == 0 ? 0 : 1;
}
