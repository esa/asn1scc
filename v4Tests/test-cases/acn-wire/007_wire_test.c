/* --acn-v2: an ACN parameter of a referenced type consumed inside the elements of a SEQUENCE OF of that type (run by
   v4Tests/scripts/runWireTests.sh, ASan / UBSan, --acn-v2). Valid values encode to the bytes computed by hand from
   the ACN, with a zero error code, and decode back; values whose elements disagree on the determinant (or disagree
   with a direct consumer) fail with ERR_ACN_DET_CONSISTENCY_MISMATCH. Before (q/issues.md Issue 124) the specialised
   encoder patched the determinant once, after the loop, from the element after the last one. */
#include <stdio.h>
#include <string.h>
#include "007.h"

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

/* encode val (ACN-inconsistent: the consumers of one determinant disagree); expect FALSE with
   ERR_ACN_DET_CONSISTENCY_MISMATCH */
#define REJECT(T, val)                                                                                  \
    do {                                                                                                \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        flag ok;                                                                                        \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        ok = T##_ACN_Encode(&(val), &bs, &err, TRUE);                                                   \
        if (ok || err != ERR_ACN_DET_CONSISTENCY_MISMATCH) {                                            \
            printf("%s line %d: encode ok=%d err=%d, expected FALSE, 203\n", #T, __LINE__, ok, err);    \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

/* n octets first, first + 1, ... */
#define FILL(s, first, n) do { (s).nCount = (n); for (int k = 0; k < (n); k++) (s).arr[k] = (byte)((first) + k); } while (0)

int main(void)
{
    /* inline element SEQUENCE: full list (before: the size of arr[2], out of the array), one element (before: the
       size of the stale arr[1]), empty list (the fallback 0) */
    {
        static TA v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.items.nCount = 2; FILL(v.pl.items.arr[0].o, 0x10, 4); FILL(v.pl.items.arr[1].o, 0x20, 4); v.tail = 7;
        ENCODE(TA, v, 0x01, 0x48, 0x40, 0x44, 0x48, 0x4C, 0x80, 0x84, 0x88, 0x8C, 0x1C);
        v.pl.items.nCount = 1; FILL(v.pl.items.arr[0].o, 1, 3); FILL(v.pl.items.arr[1].o, 1, 9);
        ENCODE(TA, v, 0x01, 0x34, 0x04, 0x08, 0x0C, 0x1C);
        v.pl.items.nCount = 0;
        ENCODE(TA, v, 0x01, 0x00, 0x1C);
        v.pl.items.nCount = 2; FILL(v.pl.items.arr[0].o, 1, 2); FILL(v.pl.items.arr[1].o, 1, 4);
        REJECT(TA, v);
    }
    /* a direct consumer next to the loop one: the direct one alone (before: the stale element's size), both, the
       elements alone; disagreements */
    {
        static TF v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.exist.d = 1; FILL(v.pl.d, 1, 5); v.pl.items.nCount = 0; FILL(v.pl.items.arr[0], 1, 9); v.tail = 7;
        ENCODE(TF, v, 0x01, 0x58, 0x08, 0x10, 0x18, 0x20, 0x28, 0x0E);
        FILL(v.pl.d, 1, 2); v.pl.items.nCount = 2; FILL(v.pl.items.arr[0], 3, 2); FILL(v.pl.items.arr[1], 5, 2);
        ENCODE(TF, v, 0x01, 0x28, 0x08, 0x14, 0x06, 0x08, 0x0A, 0x0C, 0x0E);
        v.pl.exist.d = 0; FILL(v.pl.items.arr[0], 3, 3); FILL(v.pl.items.arr[1], 6, 3);
        ENCODE(TF, v, 0x01, 0x34, 0x06, 0x08, 0x0A, 0x0C, 0x0E, 0x10, 0x0E);
        v.pl.exist.d = 1; FILL(v.pl.d, 1, 3); v.pl.items.nCount = 1; FILL(v.pl.items.arr[0], 1, 4);
        REJECT(TF, v);
        v.pl.exist.d = 0; v.pl.items.nCount = 2; FILL(v.pl.items.arr[0], 1, 3); FILL(v.pl.items.arr[1], 1, 4);
        REJECT(TF, v);
    }
    /* present-when in the element: all present, all absent (before: exist.x of arr[2], out of the array); mixed */
    {
        static TD v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.pl.items.nCount = 2; v.tail = 7;
        v.pl.items.arr[0].exist.x = 1; v.pl.items.arr[0].x = 3; v.pl.items.arr[0].y = 1;
        v.pl.items.arr[1].exist.x = 1; v.pl.items.arr[1].x = 4; v.pl.items.arr[1].y = 2;
        ENCODE(TD, v, 0x01, 0xD9, 0x88, 0x1C);
        v.pl.items.arr[0].exist.x = 0; v.pl.items.arr[0].x = 0; v.pl.items.arr[1].exist.x = 0; v.pl.items.arr[1].x = 0;
        ENCODE(TD, v, 0x01, 0x4A, 0x07);
        v.pl.items.arr[0].exist.x = 1; v.pl.items.arr[0].x = 3;
        REJECT(TD, v);
    }
    /* two callees: they agree, the second one empty (the first decides); they disagree */
    {
        static TH v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.tail = 7;
        v.p1.items.nCount = 1; FILL(v.p1.items.arr[0].o, 1, 2);
        v.p2.items.nCount = 2; FILL(v.p2.items.arr[0].o, 3, 2); FILL(v.p2.items.arr[1].o, 5, 2);
        ENCODE(TH, v, 0x01, 0x24, 0x04, 0x0A, 0x03, 0x04, 0x05, 0x06, 0x07);
        v.p2.items.nCount = 0;
        ENCODE(TH, v, 0x01, 0x24, 0x04, 0x08, 0x07);
        v.p2.items.nCount = 1; FILL(v.p2.items.arr[0].o, 3, 3);
        REJECT(TH, v);
    }
    /* the SEQUENCE OF under an OPTIONAL component: absent (its nCount is not read), present; disagreement */
    {
        static TX2 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.tail = 7; v.pl.exist.items = 0; v.pl.items.nCount = 2000000000;
        ENCODE(TX2, v, 0x01, 0x00, 0x38);
        v.pl.exist.items = 1; v.pl.items.nCount = 2; FILL(v.pl.items.arr[0].o, 1, 3); FILL(v.pl.items.arr[1].o, 4, 3);
        ENCODE(TX2, v, 0x01, 0x3C, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x18, 0x1C);
        FILL(v.pl.items.arr[1].o, 4, 2);
        REJECT(TX2, v);
    }
    /* the SEQUENCE OF in a CHOICE alternative: the other alternative selected (the list is not read), the list */
    {
        static TX3 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.tail = 7; v.pl.kind = a_PRESENT; v.pl.u.a = 5;
        ENCODE(TX3, v, 0x01, 0x05, 0x07);
        v.pl.kind = items_PRESENT; v.pl.u.items.nCount = 2; FILL(v.pl.u.items.arr[0].o, 1, 2); FILL(v.pl.u.items.arr[1].o, 3, 2);
        ENCODE(TX3, v, 0x01, 0x2C, 0x04, 0x08, 0x0C, 0x10, 0x1C);
        FILL(v.pl.u.items.arr[1].o, 3, 1);
        REJECT(TX3, v);
    }
    /* an inline CHOICE element selected by the parameter: all k1, all k2; mixed */
    {
        static TX4 v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.tail = 7; v.pl.items.nCount = 2;
        v.pl.items.arr[0].kind = k1_PRESENT; v.pl.items.arr[0].u.k1 = 5;
        v.pl.items.arr[1].kind = k1_PRESENT; v.pl.items.arr[1].u.k1 = 2;
        ENCODE(TX4, v, 0x01, 0x35, 0x03, 0x80);
        v.pl.items.arr[0].kind = k2_PRESENT; v.pl.items.arr[0].u.k2 = TRUE;
        v.pl.items.arr[1].kind = k2_PRESENT; v.pl.items.arr[1].u.k2 = FALSE;
        ENCODE(TX4, v, 0x01, 0x70, 0x38);
        v.pl.items.arr[1].kind = k1_PRESENT; v.pl.items.arr[1].u.k1 = 2;
        REJECT(TX4, v);
    }
    /* nested loops; an inner element that disagrees */
    {
        static TE v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.tail = 7; v.pl.rows.nCount = 2;
        v.pl.rows.arr[0].cols.nCount = 2; FILL(v.pl.rows.arr[0].cols.arr[0], 1, 2); FILL(v.pl.rows.arr[0].cols.arr[1], 3, 2);
        v.pl.rows.arr[1].cols.nCount = 1; FILL(v.pl.rows.arr[1].cols.arr[0], 5, 2);
        ENCODE(TE, v, 0x01, 0x2C, 0x04, 0x08, 0x0C, 0x10, 0x0A, 0x0C, 0x0E);
        FILL(v.pl.rows.arr[1].cols.arr[0], 5, 3);
        REJECT(TE, v);
    }
    /* the callee in an outer SEQUENCE OF element: each outer element its own determinant (2, then 3; empty, then 1);
       a disagreement inside the second element */
    {
        static M1 v;
        memset(&v, 0, sizeof v);
        v.nCount = 2;
        v.arr[0].a = 1; v.arr[0].pl.items.nCount = 1; FILL(v.arr[0].pl.items.arr[0].o, 1, 2);
        v.arr[1].a = 2; v.arr[1].pl.items.nCount = 2; FILL(v.arr[1].pl.items.arr[0].o, 1, 3); FILL(v.arr[1].pl.items.arr[1].o, 4, 3);
        ENCODE(M1, v, 0x80, 0x92, 0x02, 0x04, 0x04, 0x70, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30);
        v.arr[0].pl.items.nCount = 0; v.arr[1].pl.items.nCount = 1; FILL(v.arr[1].pl.items.arr[0].o, 9, 1);
        ENCODE(M1, v, 0x80, 0x80, 0x04, 0x28, 0x48);
        v.arr[1].pl.items.nCount = 2; FILL(v.arr[1].pl.items.arr[1].o, 9, 2);
        REJECT(M1, v);
    }
    /* the field's holder: present (the loop patches it), absent with the callee absent */
    {
        static TV v;
        memset(&v, 0, sizeof v);
        v.a = 1; v.tail = 7; v.exist.h = 1; v.h.b = 3; v.exist.pl = 1;
        v.pl.items.nCount = 2; FILL(v.pl.items.arr[0].o, 1, 2); FILL(v.pl.items.arr[1].o, 3, 2);
        ENCODE(TV, v, 0x01, 0xD9, 0x40, 0x20, 0x40, 0x60, 0x80, 0xE0);
        v.exist.h = 0; v.exist.pl = 0;
        ENCODE(TV, v, 0x01, 0x01, 0xC0);
        v.exist.pl = 1; v.pl.items.nCount = 0;
        ENCODE(TV, v, 0x01, 0x40, 0x70);
        /* holder absent, elements present (no field in the stream for them to read: ACN-inconsistent): every patch
           of the loop finds the field not in the stream (q/issues.md Issue 107) and writes nothing, in particular not
           at bit 0 of the buffer; the decoder rejects the stream (a dependent present while the holder is absent) */
        {
            static byte buf[TV_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];
            static const byte expected[] = {0x01, 0x60, 0x10, 0x20, 0x30, 0x40, 0x70};
            BitStream bs;
            int err = 0;
            static TV dec;
            flag ok;
            v.pl.items.nCount = 2; FILL(v.pl.items.arr[0].o, 1, 2); FILL(v.pl.items.arr[1].o, 3, 2);
            memset(buf, 0, sizeof(buf));
            BitStream_Init(&bs, buf, sizeof(buf));
            ok = TV_ACN_Encode(&v, &bs, &err, TRUE);
            if (!ok || err != 0 || BitStream_GetLength(&bs) != (int)sizeof(expected) || memcmp(buf, expected, sizeof(expected)) != 0) {
                printf("TV line %d: encode ok=%d err=%d, bytes:", __LINE__, ok, err);
                for (int k = 0; k < BitStream_GetLength(&bs); k++)
                    printf(" %02X", buf[k]);
                printf("\n");
                failures++;
            }
            BitStream_AttachBuffer(&bs, buf, sizeof(buf));
            if (TV_ACN_Decode(&dec, &bs, &err) || err == 0) {
                printf("TV line %d: decoded a stream with the holder absent and elements present\n", __LINE__);
                failures++;
            }
        }
    }
    if (failures != 0) {
        printf("007: %d failures\n", failures);
        return 1;
    }
    return 0;
}
