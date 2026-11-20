/* --acn-v2: an ACN parameter of a referenced type with several consumers inside it (run by
   v4Tests/scripts/runWireTests.sh, ASan / UBSan, --acn-v2 and legacy). Valid values encode to the bytes legacy writes
   with a zero error code and decode back; values whose consumers disagree fail with a nonzero error code (the
   specialised encoders patched one consumer per parameter: q/issues.md Issue 121). A value with no consumer present
   encodes the default 0 of the size field in both modes (legacy failed before Issue 123). */
#include <stdio.h>
#include <string.h>
#include "006.h"

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

/* encode val (ACN-inconsistent: its consumers of one ACN field disagree); expect a failure with a nonzero code */
#define REJECT(T, val)                                                                                  \
    do {                                                                                                \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        flag ok;                                                                                        \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        ok = T##_ACN_Encode(&(val), &bs, &err, TRUE);                                                   \
        if (ok || err == 0) {                                                                           \
            printf("%s line %d: encode ok=%d err=%d, expected a failure\n", #T, __LINE__, ok, err);     \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

/* n octets 1, 2, ... */
#define FILL(s, n) do { (s).nCount = (n); for (int k = 0; k < (n); k++) (s).arr[k] = (byte)(k + 1); } while (0)
#define FIXED3(arr) do { (arr)[0] = 1; (arr)[1] = 2; (arr)[2] = 3; } while (0)

int main(void)
{
    /* two OPTIONAL consumers: the first present, the second (the one patched before) absent, and the other way */
    {
        static TB v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.o = 1; FILL(v.pl.o, 3); v.pl.e = 5; v.tail = 7;
        ENCODE(TB, v, 0x01, 0x38, 0x04, 0x08, 0x0C, 0x14, 0x1C);
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.d = 1; FILL(v.pl.d, 3); v.pl.e = 5; v.tail = 7;
        ENCODE(TB, v, 0x01, 0x34, 0x04, 0x08, 0x0C, 0x14, 0x1C);
        v.pl.exist.o = 1; FILL(v.pl.o, 2);
        REJECT(TB, v);
    }
    /* two mandatory consumers */
    {
        static TG v;
        memset(&v, 0, sizeof v);
        v.a = 1; FILL(v.pl.o, 2); FILL(v.pl.d, 2); v.pl.e = 5; v.tail = 7;
        ENCODE(TG, v, 0x01, 0x20, 0x10, 0x20, 0x10, 0x20, 0x50, 0x70);
        FILL(v.pl.d, 3);
        REJECT(TG, v);
    }
    /* the alternatives of a CHOICE */
    {
        static TH v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.kind = CH_a_PRESENT; FILL(v.pl.u.a, 3); v.tail = 7;
        ENCODE(TH, v, 0x01, 0x30, 0x04, 0x08, 0x0C, 0x1C);
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.kind = CH_b_PRESENT; FILL(v.pl.u.b, 3); v.tail = 7;
        ENCODE(TH, v, 0x01, 0x34, 0x04, 0x08, 0x0C, 0x1C);
    }
    /* a consumer inside an OPTIONAL inline child */
    {
        static TI v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.inner = 1; FILL(v.pl.inner.o, 3); v.pl.e = 5; v.tail = 7;
        ENCODE(TI, v, 0x01, 0x38, 0x04, 0x08, 0x0C, 0x14, 0x1C);
    }
    /* the callee passes the parameter on */
    {
        static TK v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.x = 2; v.pl.q.exist.o = 1; FILL(v.pl.q.o, 3); v.pl.q.e = 5; v.tail = 7;
        ENCODE(TK, v, 0x01, 0x35, 0x00, 0x81, 0x01, 0x82, 0x83, 0x80);
    }
    /* a mandatory consumer and an OPTIONAL one, absent */
    {
        static TL v;
        memset(&v, 0, sizeof v);
        v.a = 1; FILL(v.pl.o, 3); v.tail = 7;
        ENCODE(TL, v, 0x01, 0x30, 0x08, 0x10, 0x18, 0x38);
    }
    /* the callee in a SEQUENCE OF element */
    {
        static TM v;
        memset(&v, 0, sizeof v);
        v.nCount = 2;
        v.arr[0].a = 1; v.arr[0].pl.exist.o = 1; FILL(v.arr[0].pl.o, 3); v.arr[0].pl.e = 5; v.arr[0].tail = 7;
        v.arr[1].a = 2; v.arr[1].pl.exist.d = 1; FILL(v.arr[1].pl.d, 1); v.arr[1].pl.e = 6; v.arr[1].tail = 8;
        ENCODE(TM, v, 0x80, 0x9C, 0x02, 0x04, 0x06, 0x0A, 0x0E, 0x04, 0x28, 0x08, 0x30, 0x40);
    }
    /* two CHOICE determinants: the first present, and both disagreeing */
    {
        static TN v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.c1 = 1; v.pl.c1.kind = ChK_k2_PRESENT; v.pl.c1.u.k2 = TRUE; v.tail = 7;
        ENCODE(TN, v, 0x01, 0x68, 0x38);
        v.pl.exist.c2 = 1; v.pl.c2.kind = c2_k1_PRESENT; v.pl.c2.u.k1 = 1;
        REJECT(TN, v);
    }
    /* two present-when flags of one BOOLEAN parameter */
    {
        static TP v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.x = 1; v.pl.x = 2; v.pl.exist.y = 1; v.pl.y = 3; v.pl.e = 5; v.tail = 7;
        ENCODE(TP, v, 0x01, 0xA6, 0x0A, 0x0E);
        v.pl.exist.y = 0;
        REJECT(TP, v);
    }
    /* fixed-size consumers: o (SIZE(3)) absent, d present with 5 octets (master rejected the value / encoded n = 3);
       o present, d absent; o present and d of 5 octets (inconsistent); both absent (the default 0 in both modes since
       Issue 123) */
    {
        static TF1 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.d = 1; FILL(v.pl.d, 5); v.pl.e = 5; v.tail = 7;
        ENCODE(TF1, v, 0x01, 0x54, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x14, 0x1C);
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.o = 1; FIXED3(v.pl.o.arr); v.pl.e = 5; v.tail = 7;
        ENCODE(TF1, v, 0x01, 0x38, 0x04, 0x08, 0x0C, 0x14, 0x1C);
        v.pl.exist.d = 1; FILL(v.pl.d, 5);
        REJECT(TF1, v);
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.e = 5; v.tail = 7;
        ENCODE(TF1, v, 0x01, 0x00, 0x14, 0x1C);
    }
    {
        static TF2 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.d = 1; FILL(v.pl.d, 5); v.pl.e = 5; v.tail = 7;
        ENCODE(TF2, v, 0x01, 0x58, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x14, 0x1C);
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.o = 1; FIXED3(v.pl.o.arr); v.pl.e = 5; v.tail = 7;
        ENCODE(TF2, v, 0x01, 0x34, 0x04, 0x08, 0x0C, 0x14, 0x1C);
        v.pl.exist.d = 1; FILL(v.pl.d, 5);
        REJECT(TF2, v);
    }
    /* fixed-size alternative of a CHOICE next to a variable one: alternative b (5 octets) gave TRUE with a nonzero
       code when the constant was patched unguarded */
    {
        static TC1 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.kind = CF1_b_PRESENT; FILL(v.pl.u.b, 5); v.tail = 7;
        ENCODE(TC1, v, 0x01, 0x54, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x1C);
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.kind = CF1_a_PRESENT; FIXED3(v.pl.u.a.arr); v.tail = 7;
        ENCODE(TC1, v, 0x01, 0x30, 0x04, 0x08, 0x0C, 0x1C);
    }
    {
        static TC2 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.kind = CF2_b_PRESENT; FILL(v.pl.u.b, 5); v.tail = 7;
        ENCODE(TC2, v, 0x01, 0x50, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x1C);
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.kind = CF2_a_PRESENT; FIXED3(v.pl.u.a.arr); v.tail = 7;
        ENCODE(TC2, v, 0x01, 0x34, 0x04, 0x08, 0x0C, 0x1C);
    }
    /* no consumer present: the size field takes the fallback 0 (master --acn-v2 wrote 3 when the fixed-size consumer
       was the last component; legacy rejected these values before Issue 123) */
    {
        static TF2 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.e = 5; v.tail = 7;
        ENCODE(TF2, v, 0x01, 0x00, 0x14, 0x1C);
    }
    {
        static TC2 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.kind = CF2_c_PRESENT; v.pl.u.c = 4; v.tail = 7;
        ENCODE(TC2, v, 0x01, 0x0A, 0x03, 0x80);
    }
    /* a direct consumer and one inside a SEQUENCE OF of the callee: d absent with elements, and d of 3 octets */
    {
        static TQ v;
        memset(&v, 0, sizeof v);
        v.a = 1; FIXED3(v.pl.items.arr[0].arr); v.tail = 7;
        ENCODE(TQ, v, 0x01, 0x30, 0x08, 0x10, 0x18, 0x38);
        v.pl.exist.d = 1; FILL(v.pl.d, 3);
        ENCODE(TQ, v, 0x01, 0x38, 0x08, 0x10, 0x18, 0x08, 0x10, 0x18, 0x38);
    }
    {
        static TQV v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.items.nCount = 2; FIXED3(v.pl.items.arr[0].arr); FIXED3(v.pl.items.arr[1].arr); v.tail = 7;
        ENCODE(TQV, v, 0x01, 0x34, 0x04, 0x08, 0x0C, 0x04, 0x08, 0x0C, 0x1C);
    }
    /* an OPTIONAL consumer before a mandatory one */
    {
        static TL2 v;
        memset(&v, 0, sizeof v);
        v.a = 1; FILL(v.pl.d, 3); v.tail = 7;
        ENCODE(TL2, v, 0x01, 0x30, 0x08, 0x10, 0x18, 0x38);
        v.pl.exist.o = 1; FILL(v.pl.o, 3);
        ENCODE(TL2, v, 0x01, 0x38, 0x08, 0x10, 0x18, 0x08, 0x10, 0x18, 0x38);
        FILL(v.pl.o, 2);
        REJECT(TL2, v);
    }
    if (failures == 0)
        printf("all checks passed\n");
    return failures == 0 ? 0 : 1;
}
