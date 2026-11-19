/* --acn-v2: determinant fields exposed by an OPTIONAL referenced component to its siblings (run by
   v4Tests/scripts/runWireTests.sh, ASan / UBSan). A stream with hdr2 absent and data2 present does not hold the size of
   data2: the decoder must fail with the error code of data2 (it decoded data2 with size 0); x, present when the exposed
   BOOLEAN field says so, is absent. Streams with hdr2 present are checked by a round trip. */
#include <stdio.h>
#include <string.h>
#include "004.h"

static int failures = 0;

/* a stack with garbage where the decoders' locals will be */
static void dirty(void)
{
    volatile unsigned long long junk[512];
    for (int i = 0; i < 512; i++)
        junk[i] = 0x400ULL;
    (void)junk[7];
}

/* decode T from the bytes given, the rest of the buffer 0xAB; expect failure with expErr (expErr > 0), failure with
   any nonzero code (expErr = -1), or success (expErr = 0); the decoded value in dec */
#define DECODE(T, dec, expErr, ...)                                                                     \
    do {                                                                                                \
        static const byte stream[] = {__VA_ARGS__};                                                     \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        flag ok;                                                                                        \
        memset(buf, 0xAB, sizeof(buf));                                                                 \
        memcpy(buf, stream, sizeof(stream));                                                            \
        BitStream_AttachBuffer(&bs, buf, sizeof(buf));                                                  \
        dirty();                                                                                        \
        ok = T##_ACN_Decode(&(dec), &bs, &err);                                                         \
        if ((expErr) == 0 ? !ok : (ok || err == 0 || ((expErr) > 0 && err != (expErr)))) {             \
            printf("%s line %d: ok=%d err=%d, expected err %d\n", #T, __LINE__, ok, err, (int)(expErr)); \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

/* encode val, decode it back, compare */
#define ROUND_TRIP(T, val)                                                                              \
    do {                                                                                                \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING];                                           \
        BitStream bs;                                                                                   \
        T dec;                                                                                          \
        int err = 0;                                                                                    \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        if (!T##_ACN_Encode(&(val), &bs, &err, TRUE)) {                                                 \
            printf("%s line %d: encoding failed, err %d\n", #T, __LINE__, err);                         \
            failures++;                                                                                 \
            break;                                                                                      \
        }                                                                                               \
        BitStream_AttachBuffer(&bs, buf, sizeof(buf));                                                  \
        if (!T##_ACN_Decode(&dec, &bs, &err) || !T##_Equal(&dec, &(val))) {                             \
            printf("%s line %d: round trip failed, err %d\n", #T, __LINE__, err);                       \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

#define EXPECT(cond)                                                                                    \
    do {                                                                                                \
        if (!(cond)) {                                                                                  \
            printf("line %d: %s\n", __LINE__, #cond);                                                   \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

int main(void)
{
    MyPDU v;
    MyPDU_Initialize(&v);
    DECODE(MyPDU, v, ERR_ACN_DECODE_MYPDU_DATA2, 0x40);                /* hdr2 absent, data2 present */
    DECODE(MyPDU, v, 0, 0x0B, 0x40);                                    /* hdr2 absent: x absent, tail 0x5A */
    EXPECT(!v.exist.hdr2 && !v.exist.data2 && !v.exist.pl2 && !v.exist.x && v.tail == 0x5A);
    /* hdr2 absent, pl2 present: pl2.d checks that the reference it receives holds a field of the stream (q/issues.md
       Issue 107; its error code ERR_ACN_DECODE_MYPDU_PL2_D is private to 004.c: any nonzero code) */
    DECODE(MyPDU, v, -1, 0x20);
    MyPDU_Initialize(&v);
    v.exist.hdr2 = 1;
    v.hdr2.a = 3;
    v.exist.data2 = 1;
    v.data2.nCount = 2;
    v.data2.arr[0] = 0x12;
    v.data2.arr[1] = 0x34;
    v.exist.pl2 = 1;
    v.pl2.d.nCount = 1;
    v.pl2.d.arr[0] = 0x56;
    v.exist.x = 1;
    v.x = 7;
    v.tail = 9;
    ROUND_TRIP(MyPDU, v);
    if (failures == 0)
        printf("all checks passed\n");
    return failures == 0 ? 0 : 1;
}
