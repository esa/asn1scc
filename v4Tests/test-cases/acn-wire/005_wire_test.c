/* --acn-v2: a determinant field in a component the stream may say absent, read outside it by a specialised function
   (run by v4Tests/scripts/runWireTests.sh, ASan / UBSan, --acn-v2 only). Valid values whose holder is absent encode
   to the bytes legacy writes (checked against the legacy encoder of the same types; R3PDU / LxPDU are --acn-v2 only),
   with a zero error code, and decode back; streams with the holder absent and the dependent present are rejected
   (q/issues.md Issue 107). The error codes of the dependents inside specialised decoders are defined in 005.c: those
   checks accept any nonzero code. */
#include <stdio.h>
#include <string.h>
#include "005.h"

static int failures = 0;

/* encode val; expect success with a zero error code, exactly the bytes given, and the value back from a decode */
#define ENCODE(T, val, ...)                                                                             \
    do {                                                                                                \
        static const byte expected[] = {__VA_ARGS__};                                                   \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        T dec;                                                                                          \
        flag ok;                                                                                        \
        memset(buf, 0, sizeof(buf));                                                                    \
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

/* decode T from the bytes given (the rest of the buffer 0xAB): expect failure with expErr (> 0) or with any nonzero
   code (expErr = -1) */
#define REJECT(T, expErr, ...)                                                                          \
    do {                                                                                                \
        static const byte stream[] = {__VA_ARGS__};                                                     \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        T dec;                                                                                          \
        int err = 0;                                                                                    \
        flag ok;                                                                                        \
        memset(buf, 0xAB, sizeof(buf));                                                                 \
        memcpy(buf, stream, sizeof(stream));                                                            \
        BitStream_AttachBuffer(&bs, buf, sizeof(buf));                                                  \
        ok = T##_ACN_Decode(&dec, &bs, &err);                                                           \
        if (ok || err == 0 || ((expErr) > 0 && err != (expErr))) {                                      \
            printf("%s line %d: decode ok=%d err=%d, expected a failure\n", #T, __LINE__, ok, err);     \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

int main(void)
{
    /* a size passed as an ACN argument: header absent, pl present without d (the fallback of header.n wrote 4 zero
       bits at bit 0: 00 A0 E0); header present, d of 3 octets */
    {
        ArgPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(ArgPDU, v, 0x40, 0xA0, 0xE0);
        memset(&v, 0, sizeof v);
        v.exist.header = 1;
        v.header.a = 0x13;
        v.exist.pl = 1;
        v.pl.exist.d = 1;
        v.pl.d.nCount = 3;
        v.pl.d.arr[0] = 1;
        v.pl.d.arr[1] = 2;
        v.pl.d.arr[2] = 3;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(ArgPDU, v, 0xC4, 0xCE, 0x02, 0x04, 0x06, 0x0A, 0x0E);
        REJECT(ArgPDU, -1, 0x60, 0x00, 0x00, 0xA0, 0xE0);               /* header absent, pl.d present */
    }
    /* a BOOLEAN passed as an ACN argument, read by a present-when in the callee (its patch wrote bit 0: 01 41 C0) */
    {
        BoolPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(BoolPDU, v, 0x81, 0x41, 0xC0);
    }
    /* a fixed-size consumer in the callee, absent (its patch has no path checks: 30 A0 E0) */
    {
        FixPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(FixPDU, v, 0x80, 0xA0, 0xE0);
    }
    /* nested holders: h1 present, h2 absent */
    {
        NestPDU v;
        memset(&v, 0, sizeof v);
        v.exist.h1 = 1;
        v.h1.a = 1;
        v.exist.pl = 1;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(NestPDU, v, 0xC0, 0x40, 0x50, 0x70);
        REJECT(NestPDU, -1, 0xC0, 0x50, 0x00, 0x00, 0x50, 0x70);        /* h2 absent, pl.d present */
    }
    /* a CHOICE determinant in the callee */
    {
        ChPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(ChPDU, v, 0x80, 0xA0, 0xE0);
        REJECT(ChPDU, -1, 0xB0, 0x50, 0x70);                            /* header absent, pl.ch present */
    }
    /* the reference passed through two specialised functions to a SEQUENCE OF */
    {
        DeepPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.x = 9;
        v.pl.inr.e = 5;
        v.tail = 7;
        ENCODE(DeepPDU, v, 0x82, 0x40, 0xA0, 0xE0);
        REJECT(DeepPDU, -1, 0x82, 0x60, 0x00, 0x00, 0xA0, 0xE0);        /* header absent, pl.inr.s present */
    }
    /* an IA5String size in the callee */
    {
        StrPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(StrPDU, v, 0x80, 0xA0, 0xE0);
        REJECT(StrPDU, -1, 0xB0, 0x61, 0x02, 0x83, 0x80);               /* header absent, pl.s present */
    }
    /* a CONTAINING sized by the field: the check is at the call of cont's decoder (its error code is public) */
    {
        ContPDU v;
        memset(&v, 0, sizeof v);
        v.tail = 0xF7;
        ENCODE(ContPDU, v, 0x3D, 0xC0);
        memset(&v, 0, sizeof v);
        v.exist.header = 1;
        v.header.a = 2;
        v.exist.cont = 1;
        v.cont.v = 0x42;
        v.tail = 7;
        ENCODE(ContPDU, v, 0xC0, 0x85, 0x08, 0x1C);
        REJECT(ContPDU, ERR_ACN_DECODE_CONTPDU_CONT_2_2, 0x82, 0x41, 0xC0); /* header absent, cont present */
    }
    /* a CONTAINING inside the callee, sized by the ACN argument */
    {
        CwPDU v;
        memset(&v, 0, sizeof v);
        v.exist.w = 1;
        v.w.y = 5;
        v.tail = 7;
        ENCODE(CwPDU, v, 0x80, 0xA0, 0xE0);
        memset(&v, 0, sizeof v);
        v.exist.header = 1;
        v.header.a = 2;
        v.exist.w = 1;
        v.w.exist.p = 1;
        v.w.p.v = 0x42;
        v.w.y = 5;
        v.tail = 7;
        ENCODE(CwPDU, v, 0xC0, 0x86, 0x84, 0x0A, 0x0E);
        REJECT(CwPDU, -1, 0xA1, 0x20, 0xA0, 0xE0);                      /* header absent, w.p present */
    }
    /* a field of an OPTIONAL component inside a mandatory referenced component, exposed to a sibling's argument */
    {
        R3PDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(R3PDU, v, 0x80, 0xA0, 0xE0);
        REJECT(R3PDU, -1, 0xA0, 0x00, 0x00, 0xA0, 0xE0);                /* h.hin absent, pl.d present */
    }
    /* the component is ALWAYS ABSENT: a present dependent always fails */
    {
        AbsPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.e = 5;
        v.tail = 7;
        ENCODE(AbsPDU, v, 0x81, 0x41, 0xC0);
        REJECT(AbsPDU, -1, 0xC0, 0x00, 0x01, 0x41, 0xC0);               /* pl.d present */
    }
    /* SEQUENCE OF elements: element 1 holds the field (more = 1, y present), element 2 does not (y absent); element 2
       saw element 1's reference (encoder: TRUE with error code 203, the decoder read y) */
    {
        SqBPDU v;
        memset(&v, 0, sizeof v);
        v.arr[0].exist.h = 1;
        v.arr[0].h.a = 1;
        v.arr[0].exist.pl = 1;
        v.arr[0].pl.exist.y = 1;
        v.arr[0].pl.y = 9;
        v.arr[0].pl.e = 5;
        v.arr[0].tail = 7;
        v.arr[1].exist.pl = 1;
        v.arr[1].pl.e = 6;
        v.arr[1].tail = 8;
        ENCODE(SqBPDU, v, 0xC0, 0x61, 0x20, 0xA0, 0xF0, 0x30, 0x40);
    }
    /* the same with the field exposed by an OPTIONAL referenced component */
    {
        LxPDU v;
        memset(&v, 0, sizeof v);
        v.arr[0].exist.hd = 1;
        v.arr[0].hd.a = 1;
        v.arr[0].exist.pl = 1;
        v.arr[0].pl.exist.y = 1;
        v.arr[0].pl.y = 9;
        v.arr[0].pl.e = 5;
        v.arr[0].tail = 7;
        v.arr[1].exist.pl = 1;
        v.arr[1].pl.e = 6;
        v.arr[1].tail = 8;
        ENCODE(LxPDU, v, 0xC0, 0x61, 0x20, 0xA0, 0xF0, 0x30, 0x40);
    }
    if (failures == 0)
        printf("all checks passed\n");
    return failures == 0 ? 0 : 1;
}
