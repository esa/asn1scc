/* Issues 123 / 125 / 142 (cycle i108a, run by v4Tests/scripts/runWireTests.sh, ASan / UBSan, legacy and --acn-v2): a
   determinant field whose consumers are all absent takes its default value, the same bytes in both modes, with a zero
   error code, and decodes back. Before, these values failed in legacy (Issue 123: ERR_ACN_ENCODE_..._UNINITIALIZED /
   ERR_ACN_ENCODE_UPDATE_...) and --acn-v2 wrote other bytes or failed with 203 (Issues 125 / 142); see 011.asn1. */
#include <stdio.h>
#include <string.h>
#include "011.h"

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
    {   /* the only consumer absent: n = 0; present: its size */
        static S7 v;
        memset(&v, 0, sizeof v);
        v.tail = 7;
        ENCODE(S7, v, 0x00, 0x38);
        v.exist.p = 1; FILL(v.p, 3);
        ENCODE(S7, v, 0x98, 0x08, 0x10, 0x18, 0x38);
    }
    {   /* the CHOICE consumer absent: k = 0 */
        static S8 v;
        memset(&v, 0, sizeof v);
        v.tail = 7;
        ENCODE(S8, v, 0x00, 0x38);
    }
    {   /* the holder present, its field's CHOICE consumer absent: k = 0 */
        static S4 v;
        memset(&v, 0, sizeof v);
        v.tail = 7;
        v.exist.h = 1; v.h.a = 0;
        ENCODE(S4, v, 0x40, 0x00, 0x1C);
    }
    {   /* the callee's only consumer absent */
        static S9 v;
        memset(&v, 0, sizeof v);
        v.pl.e = 5; v.tail = 7;
        ENCODE(S9, v, 0x00, 0x28, 0x38);
    }
    {   /* both consumers of the callee absent */
        static S12 v;
        memset(&v, 0, sizeof v);
        v.pl.e = 5; v.tail = 7;
        ENCODE(S12, v, 0x00, 0x14, 0x1C);
    }
    {   /* the fixed-size o absent and the callee's d of 5 octets (--acn-v2 failed with 203); both absent: n = 0 */
        static S10 v;
        memset(&v, 0, sizeof v);
        v.pl.e = 5; v.tail = 7;
        v.pl.exist.d = 1; FILL(v.pl.d, 5);
        ENCODE(S10, v, 0x2C, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x14, 0x1C);
        v.pl.exist.d = 0;
        ENCODE(S10, v, 0x00, 0x14, 0x1C);
    }
    {   /* the callee's fixed-size o absent and d of 5 octets (--acn-v2 failed with 203 in the callee) */
        static S13 v;
        memset(&v, 0, sizeof v);
        v.tail = 7;
        v.exist.d = 1; FILL(v.d, 5);
        ENCODE(S13, v, 0xA8, 0x00, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x1C);
    }
    {   /* the callee's only consumer, fixed-size, absent: n = 0 (--acn-v2 wrote 3); present: 3 */
        static S14 v;
        memset(&v, 0, sizeof v);
        v.tail = 7;
        ENCODE(S14, v, 0x00, 0x00, 0x38);
        v.pl.exist.o = 1; FIXED3(v.pl.o.arr);
        ENCODE(S14, v, 0x38, 0x08, 0x10, 0x18, 0x00, 0x38);
    }
    {   /* a fixed-size consumer inside the callee's elements: 3 also for an empty list (as legacy) */
        static S15 v;
        memset(&v, 0, sizeof v);
        v.tail = 7;
        ENCODE(S15, v, 0x30, 0x1C);
        v.pl.items.nCount = 1; FIXED3(v.pl.items.arr[0].arr);
        ENCODE(S15, v, 0x34, 0x04, 0x08, 0x0C, 0x1C);
    }
    {   /* o present, both callee consumers absent: n = 3 (legacy failed with ERR_ACN_ENCODE_UPDATE_S17_PL_LEN); all absent */
        static S17 v;
        memset(&v, 0, sizeof v);
        v.pl.e = 5; v.tail = 7;
        v.exist.o = 1; FILL(v.o, 3);
        ENCODE(S17, v, 0x98, 0x08, 0x10, 0x18, 0x0A, 0x0E);
        v.exist.o = 0;
        ENCODE(S17, v, 0x00, 0x0A, 0x0E);
    }
    {   /* the same one level deeper (the callee passes the parameter on next to its own consumer) */
        static S19 v;
        memset(&v, 0, sizeof v);
        v.pl.q.e = 5; v.tail = 7;
        v.exist.o = 1; FILL(v.o, 3);
        ENCODE(S19, v, 0x98, 0x08, 0x10, 0x18, 0x05, 0x07);
        v.exist.o = 0;
        ENCODE(S19, v, 0x00, 0x05, 0x07);
        v.pl.q.exist.d2 = 1; FILL(v.pl.q.d2, 2);
        ENCODE(S19, v, 0x11, 0x01, 0x02, 0x05, 0x07);
    }
    {   /* Len (1..8), uPER: the default is 1 (wire 0) */
        static T1 v;
        memset(&v, 0, sizeof v);
        v.tail = 7;
        ENCODE(T1, v, 0x00, 0x70);
    }
    {   /* Len5 (5..8) passed to a callee: the default is 5, wire 0 (--acn-v2 wrote 5 - 5 + 5 = wire 1, i.e. 6) */
        static T2 v;
        memset(&v, 0, sizeof v);
        v.pl.e = 5; v.tail = 7;
        ENCODE(T2, v, 0x00, 0xA0, 0xE0);
    }
    {   /* the CHOICE selects z: no element, n = 0 (legacy failed with ERR_ACN_ENCODE_TA_N_UNINITIALIZED) */
        static TA v;
        memset(&v, 0, sizeof v);
        v.c.kind = z_PRESENT; v.c.u.z = TRUE;
        ENCODE(TA, v, 0x00, 0xC0);
        v.c.kind = list_PRESENT; v.c.u.list.nCount = 1; FIXED3(v.c.u.list.arr[0].q.o.arr);
        ENCODE(TA, v, 0x03, 0x00, 0x40, 0x80, 0xC0);
    }
    if (failures != 0) {
        printf("%d failures\n", failures);
        return 1;
    }
    printf("011 wire test OK\n");
    return 0;
}
