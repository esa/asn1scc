/* A determinant field inside an OPTIONAL component, its dependent outside it (run by v4Tests/scripts/runWireTests.sh,
   ASan / UBSan, legacy and --acn-v2; WIRE_ACN_V2 is defined for --acn-v2). A value whose component is absent while the
   dependent is present cannot be encoded: the field is not written, so no stream holds that value (q/issues.md Issue
   108, rule R4). The encoder must fail with a nonzero error code (the dependent's own where it is public); it used to
   succeed (the decoder then rejected its own stream, or decoded another value), and the legacy CONTAINING returned
   FALSE with error code 0 after reading a length it never set. Valid values with the component absent still encode. */
#include <stdio.h>
#include <string.h>
#include "003.h"

static int failures = 0;

/* encode val: expect failure with expErr (expErr > 0) or any nonzero code (expErr = -1) */
#define REJECT(T, val, expErr)                                                                          \
    do {                                                                                                \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        flag ok;                                                                                        \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        ok = T##_ACN_Encode(&(val), &bs, &err, TRUE);                                                   \
        if (ok || err == 0 || ((expErr) > 0 && err != (expErr))) {                                      \
            printf("%s line %d: encode ok=%d err=%d, expected err %d\n", #T, __LINE__, ok, err, (int)(expErr)); \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

/* REJECT, and the encoder stopped at bit expBit of the stream (nothing of the dependent written) */
#define REJECT_AT(T, val, expErr, expBit)                                                               \
    do {                                                                                                \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        flag ok;                                                                                        \
        long pos;                                                                                       \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        ok = T##_ACN_Encode(&(val), &bs, &err, TRUE);                                                   \
        pos = (long)bs.currentByte * 8 + bs.currentBit;                                                 \
        if (ok || err == 0 || ((expErr) > 0 && err != (expErr)) || pos != (expBit)) {                   \
            printf("%s line %d: encode ok=%d err=%d at bit %ld, expected err %d at bit %d\n", #T, __LINE__, \
                   ok, err, pos, (int)(expErr), (int)(expBit));                                         \
            failures++;                                                                                 \
        }                                                                                               \
    } while (0)

/* encode val: expect success with a zero error code and the value back from a decode */
#define ACCEPT(T, val)                                                                                  \
    do {                                                                                                \
        static byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 16];                                      \
        BitStream bs;                                                                                   \
        int err = 0;                                                                                    \
        T dec;                                                                                          \
        BitStream_Init(&bs, buf, sizeof(buf));                                                          \
        if (!T##_ACN_Encode(&(val), &bs, &err, TRUE) || err != 0) {                                     \
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

int main(void)
{
    /* sizes of an OCTET STRING and of an IA5String (both present when flg2 says so: data fails first, so str's guard
       is never reached here; StrOnlyPDU below reaches it) */
    {
        MyPDU v;
        memset(&v, 0, sizeof v);
        ACCEPT(MyPDU, v);
        v.exist.data = 1;
        v.data.nCount = 2;
        v.exist.str = 1;
        strcpy(v.str, "AB");
        REJECT(MyPDU, v, ERR_ACN_ENCODE_MYPDU_DATA);
    }
    /* CHOICE determinant */
    {
        ChPDU v;
        memset(&v, 0, sizeof v);
        v.exist.ch = 1;
        v.ch.kind = c2_PRESENT;
        v.ch.u.c2 = TRUE;
        REJECT(ChPDU, v, ERR_ACN_ENCODE_CHPDU_CH);
    }
    /* CHOICE determinant of a referenced CHOICE (an ACN argument of the CHOICE) */
    {
        RefChPDU v;
        memset(&v, 0, sizeof v);
        v.exist.ch = 1;
        v.ch.kind = two_PRESENT;
        v.ch.u.two = TRUE;
        REJECT(RefChPDU, v, -1);
    }
    /* CHOICE present-when an INTEGER field, a string field */
    {
        PwPDU v;
        memset(&v, 0, sizeof v);
        v.exist.cp = 1;
        v.cp.kind = p1_PRESENT;
        v.cp.u.p1 = 5;
        REJECT(PwPDU, v, ERR_ACN_ENCODE_PWPDU_CP);
    }
    {
        PwStrPDU v;
        memset(&v, 0, sizeof v);
        v.exist.cs = 1;
        v.cs.kind = s2_PRESENT;
        v.cs.u.s2 = TRUE;
        REJECT(PwStrPDU, v, ERR_ACN_ENCODE_PWSTRPDU_CS);
    }
    /* present-when a BOOLEAN field of the component: x present without it (the stream would decode x absent); the
       field passed to p, whose y is present when it says so */
    {
        BoolPDU v;
        memset(&v, 0, sizeof v);
        v.exist.p = 1;
        ACCEPT(BoolPDU, v);
        v.exist.x = 1;
        v.x = 3;
        REJECT(BoolPDU, v, ERR_ACN_ENCODE_BOOLPDU_X);
        memset(&v, 0, sizeof v);
        v.exist.p = 1;
        v.p.exist.y = 1;
#ifndef WIRE_ACN_V2
        REJECT(BoolPDU, v, ERR_ACN_ENCODE_BOOLPDU_P_Y);
#else
        REJECT(BoolPDU, v, -1);                                     /* y's code is private to 003.c */
#endif
    }
    /* SEQUENCE OF elements: element 2 has no component but x (present when the component's field says so, sized by it) */
    {
        SqPDU v;
        memset(&v, 0, sizeof v);
        v.arr[0].exist.h = 1;
        v.arr[0].exist.x = 1;
        v.arr[0].x.nCount = 1;
        ACCEPT(SqPDU, v);
        v.arr[1].exist.x = 1;
        REJECT(SqPDU, v, ERR_ACN_ENCODE_SQPDU_ELM_X);
    }
    {
        ElemPDU v;
        memset(&v, 0, sizeof v);
        v.elems.nCount = 2;
        v.elems.arr[0].exist.hh = 1;
        v.elems.arr[0].exist.dd = 1;
        v.elems.arr[0].dd.nCount = 1;
        ACCEPT(ElemPDU, v);
        v.elems.arr[1].exist.dd = 1;
        REJECT(ElemPDU, v, ERR_ACN_ENCODE_ELEMPDU_ELEMS_ELM_DD);
    }
    /* nested components: h1 absent; h1 present without h2 */
    {
        NestPDU v;
        memset(&v, 0, sizeof v);
        v.exist.d = 1;
        v.d.nCount = 1;
        REJECT(NestPDU, v, ERR_ACN_ENCODE_NESTPDU_D);
        v.exist.h1 = 1;
        REJECT(NestPDU, v, ERR_ACN_ENCODE_NESTPDU_D);
        v.h1.exist.h2 = 1;
        ACCEPT(NestPDU, v);
    }
    /* the dependent is an alternative of a mandatory CHOICE */
    {
        AltPDU v;
        memset(&v, 0, sizeof v);
        v.alt.kind = o2_PRESENT;
        ACCEPT(AltPDU, v);
        v.alt.kind = o1_PRESENT;
        v.alt.u.o1.nCount = 1;
        REJECT(AltPDU, v, ERR_ACN_ENCODE_ALTPDU_ALT_O1);
    }
    /* a CONTAINING sized by the field: legacy encoded the contents with a length it never set (FALSE with error code 0,
       or a read past the contents' buffer) */
    {
        ContPDU v;
        memset(&v, 0, sizeof v);
        ACCEPT(ContPDU, v);
        v.exist.cont = 1;
        v.cont.v = 0x42;
        REJECT(ContPDU, v, -1);
    }
    /* the field as an ACN argument (--acn-v2: the callee tests the reference; its code is private to 003.c) */
    {
        ArgPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.d.nCount = 1;
#ifndef WIRE_ACN_V2
        REJECT(ArgPDU, v, ERR_ACN_ENCODE_ARGPDU_PL_D);
#else
        REJECT(ArgPDU, v, -1);
#endif
    }
    {
        OptArgPDU v;
        memset(&v, 0, sizeof v);
        v.exist.pl = 1;
        v.pl.e = 5;
        ACCEPT(OptArgPDU, v);
        v.pl.exist.d = 1;
        v.pl.d.nCount = 1;
#ifndef WIRE_ACN_V2
        REJECT(OptArgPDU, v, ERR_ACN_ENCODE_OPTARGPDU_PL_D);
#else
        REJECT(OptArgPDU, v, -1);
#endif
    }
    /* the component is ALWAYS ABSENT: a present dependent always fails */
    {
        AbsPDU v;
        memset(&v, 0, sizeof v);
        ACCEPT(AbsPDU, v);
        v.exist.data = 1;
        REJECT(AbsPDU, v, ERR_ACN_ENCODE_ABSPDU_DATA);
    }
    /* the dependent present when the same field says so as the component, the field also passed to p: legacy's update
       of f rejects the disagreement before f; --acn-v2 defers f (checked after the value is written), so d's guard
       rejects it after f's bit, before d */
    {
        DefPDU v;
        memset(&v, 0, sizeof v);
        ACCEPT(DefPDU, v);
        v.exist.d = 1;
        v.d.nCount = 1;
#ifndef WIRE_ACN_V2
        REJECT_AT(DefPDU, v, -1, 0);
#else
        REJECT_AT(DefPDU, v, ERR_ACN_ENCODE_DEFPDU_D, 1);
#endif
    }
    /* an aligned dependent: rejected after flg and g, before the padding to the next byte */
    {
        AlPDU v;
        memset(&v, 0, sizeof v);
        ACCEPT(AlPDU, v);
        v.exist.d = 1;
        v.d.nCount = 1;
        REJECT_AT(AlPDU, v, ERR_ACN_ENCODE_ALPDU_D, 2);
    }
    /* an IA5String size */
    {
        StrOnlyPDU v;
        memset(&v, 0, sizeof v);
        ACCEPT(StrOnlyPDU, v);
        v.exist.str = 1;
        strcpy(v.str, "AB");
        REJECT_AT(StrOnlyPDU, v, ERR_ACN_ENCODE_STRONLYPDU_STR, 2);
    }
    /* a referenced CHOICE whose ENUMERATED determinant is an ACN argument */
    {
        EnChPDU v;
        memset(&v, 0, sizeof v);
        ACCEPT(EnChPDU, v);
        v.exist.ch = 1;
        v.ch.kind = e2_PRESENT;
        v.ch.u.e2 = TRUE;
        REJECT_AT(EnChPDU, v, -1, 2);
    }
    /* component and dependent present when the same field says so: the field's update rejects the disagreement */
    {
        SamePDU v;
        memset(&v, 0, sizeof v);
        v.exist.data = 1;
        REJECT(SamePDU, v, -1);
    }
    if (failures == 0)
        printf("all encode checks passed\n");
    return failures == 0 ? 0 : 1;
}
