/* Issue 125 leftover (cycle i108a round 2, run by v4Tests/scripts/runWireTests.sh, ASan / UBSan, --acn-v2): an OPTIONAL
   fixed-size consumer inside the elements of a callee's SEQUENCE OF; see 011_v2.asn1. */
#include <stdio.h>
#include <string.h>
#include "011_v2.h"

static int failures = 0;

/* encode val; expect success with a zero error code, exactly the bytes given, and the value back from a decode */
#define ENCODE(T, val, ...)                                                                             \
    do {                                                                                                \
        static const byte expected[] = {__VA_ARGS__};                                                   \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        static T dec;                                                                                   \
        flag ok;                                                                                        \
        memset(buf, 0, sizeof(buf));                                                                    \
        memset(&dec, 0, sizeof(dec));                                                                   \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        ok = T##_ACN_Encode(&(val), &bs, &err, TRUE);                                                   \
        if (!ok || err != 0 || BitStream_GetLength(&bs) != (int)sizeof(expected) ||                     \
            memcmp(buf, expected, sizeof(expected)) != 0) {                                             \
            printf("%s line %d: encode ok=%d err=%d, bytes:", #T, __LINE__, ok, err);                   \
            for (int k = 0; k < BitStream_GetLength(&bs); k++)                                          \
                printf(" %02X", buf[k]);                                                                \
            printf("\n");                                                                               \
            failures++;                                                                                 \
            break;                                                                                      \
        }                                                                                               \
        BitStream_AttachBuffer(&bs, buf, sizeof(buf));                                                  \
        if (!T##_ACN_Decode(&dec, &bs, &err) || !T##_Equal(&dec, &(val))) {                             \
            printf("%s line %d: decoding back failed, err %d\n", #T, __LINE__, err);                    \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

/* n octets 1, 2, ... */
#define FILL(s, n) do { (s).nCount = (n); for (int k = 0; k < (n); k++) (s).arr[k] = (byte)(k + 1); } while (0)
#define FIXED3(arr) do { (arr)[0] = 1; (arr)[1] = 2; (arr)[2] = 3; } while (0)

int main(void)
{
    {   /* no element: n = 0; one element without x: n = 0 (was 3); one element with x: n = 3 */
        static S17 v;
        memset(&v, 0, sizeof v);
        v.tail = 7;
        ENCODE(S17, v, 0x00, 0x1C);
        v.pl.items.nCount = 1; v.pl.items.arr[0].e = 5;
        ENCODE(S17, v, 0x04, 0x0A, 0x0E);
        v.pl.items.arr[0].exist.x = 1; FIXED3(v.pl.items.arr[0].x.arr);
        ENCODE(S17, v, 0x36, 0x02, 0x04, 0x06, 0x0A, 0x0E);
        /* two elements, x only in the second: every element is patched (n = 3) */
        v.pl.items.nCount = 2; v.pl.items.arr[0].exist.x = 0; v.pl.items.arr[0].e = 5;
        v.pl.items.arr[1].exist.x = 1; FIXED3(v.pl.items.arr[1].x.arr); v.pl.items.arr[1].e = 6;
        ENCODE(S17, v, 0x38, 0x0B, 0x01, 0x02, 0x03, 0x06, 0x07);
    }
    if (failures != 0) {
        printf("%d failures\n", failures);
        return 1;
    }
    printf("011_v2 wire test OK\n");
    return 0;
}
