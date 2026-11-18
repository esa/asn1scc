/* A determinant field inside an OPTIONAL component, its dependent outside it (run by v4Tests/scripts/runWireTests.sh,
   ASan / UBSan, legacy and --acn-v2; WIRE_ACN_V2 is defined for --acn-v2). A stream that says the component is absent
   and the dependent present does not hold the field: the decoder must fail with the dependent's error code, not read
   the field's unset local (which the stack is first filled with garbage for). Streams with the component present are
   checked by a round trip. */
#include <stdio.h>
#include <string.h>
#include "003.h"

static int failures = 0;

/* ASan's fake stack for use-after-return would give the decoders fresh (zeroed) frames, which dirty() cannot reach */
const char *__asan_default_options(void);
const char *__asan_default_options(void) { return "detect_stack_use_after_return=0"; }

/* a stack with garbage where the decoders' locals will be: 2 is not a valid C bool (UBSan), a large size overflows */
static void dirty(void)
{
    volatile unsigned long long junk[512];
    for (int i = 0; i < 512; i++)
        junk[i] = 0x0202020202020202ULL;
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
    /* sizes of an OCTET STRING (no range check: C wrote past data.arr) and of an IA5String */
    {
        MyPDU v;
        MyPDU_Initialize(&v);
        DECODE(MyPDU, v, ERR_ACN_DECODE_MYPDU_DATA, 0x40);              /* header absent, data present */
        DECODE(MyPDU, v, 0, 0x00);                                      /* header, data, str absent */
        EXPECT(!v.exist.header && !v.exist.data && !v.exist.str);
        MyPDU_Initialize(&v);
        v.exist.header = 1;
        v.header.a = 0x11;
        v.exist.data = 1;
        v.data.nCount = 2;
        v.data.arr[0] = 1;
        v.data.arr[1] = 2;
        v.exist.str = 1;
        strcpy(v.str, "AB");
        ROUND_TRIP(MyPDU, v);
    }
    /* CHOICE determinant (Ada raised Constraint_Error on the case of its local) */
    {
        ChPDU v;
        ChPDU_Initialize(&v);
        DECODE(ChPDU, v, ERR_ACN_DECODE_CHPDU_CH, 0x40);
        DECODE(ChPDU, v, 0, 0x00);
        EXPECT(!v.exist.header && !v.exist.ch);
        ChPDU_Initialize(&v);
        v.exist.header = 1;
        v.exist.ch = 1;
        v.ch.kind = c2_PRESENT;
        v.ch.u.c2 = TRUE;
        ROUND_TRIP(ChPDU, v);
    }
    /* CHOICE present-when an INTEGER field, a string field */
    {
        PwPDU v;
        PwPDU_Initialize(&v);
        DECODE(PwPDU, v, ERR_ACN_DECODE_PWPDU_CP, 0x40);
        DECODE(PwPDU, v, 0, 0x00);
        EXPECT(!v.exist.header && !v.exist.cp);
        PwPDU_Initialize(&v);
        v.exist.header = 1;
        v.exist.cp = 1;
        v.cp.kind = p1_PRESENT;
        v.cp.u.p1 = 5;
        ROUND_TRIP(PwPDU, v);
    }
    {
        PwStrPDU v;
        PwStrPDU_Initialize(&v);
        DECODE(PwStrPDU, v, ERR_ACN_DECODE_PWSTRPDU_CS, 0x40);
        DECODE(PwStrPDU, v, 0, 0x00);
        EXPECT(!v.exist.header && !v.exist.cs);
        PwStrPDU_Initialize(&v);
        v.exist.header = 1;
        v.exist.cs = 1;
        v.cs.kind = s2_PRESENT;
        v.cs.u.s2 = TRUE;
        ROUND_TRIP(PwStrPDU, v);
    }
    /* present-when a BOOLEAN field of the component: with the component absent, x is absent */
    {
        BoolPDU v;
        BoolPDU_Initialize(&v);
        DECODE(BoolPDU, v, 0, 0x16, 0x80);                              /* header, p absent, tail 0x5A */
        EXPECT(!v.exist.header && !v.exist.x && !v.exist.p && v.tail == 0x5A);
        BoolPDU_Initialize(&v);
        v.exist.header = 1;
        v.exist.x = 1;
        v.x = 3;
        v.tail = 9;
        ROUND_TRIP(BoolPDU, v);
    }
    /* SEQUENCE OF elements, present-when a BOOLEAN field: element 1 has h, more TRUE, x empty; element 2 has no h,
       so no x (it used to take element 1's more) */
    {
        SqPDU v;
        SqPDU_Initialize(&v);
        DECODE(SqPDU, v, 0, 0xA0, 0x40);
        EXPECT(v.arr[0].exist.h && v.arr[0].exist.x && v.arr[0].x.nCount == 0 && v.arr[0].tail == 0);
        EXPECT(!v.arr[1].exist.h && !v.arr[1].exist.x && v.arr[1].tail == 1);
        SqPDU_Initialize(&v);
        v.arr[0].exist.h = 1;
        v.arr[0].exist.x = 1;
        v.arr[0].x.nCount = 3;
        v.arr[0].x.arr[2] = 3;
        v.arr[1].tail = 1;
        ROUND_TRIP(SqPDU, v);
    }
    /* SEQUENCE OF elements: element 1 sets the size (1), element 2 has no component but a dependent */
    {
        ElemPDU v;
        ElemPDU_Initialize(&v);
        DECODE(ElemPDU, v, ERR_ACN_DECODE_ELEMPDU_ELEMS_ELM_DD, 0x70, 0x01, 0x55, 0x40);
        DECODE(ElemPDU, v, 0, 0x00);                                    /* one element, both absent */
        EXPECT(v.elems.nCount == 1 && !v.elems.arr[0].exist.hh && !v.elems.arr[0].exist.dd);
        ElemPDU_Initialize(&v);
        v.elems.nCount = 2;
        v.elems.arr[0].exist.hh = 1;
        v.elems.arr[0].exist.dd = 1;
        v.elems.arr[0].dd.nCount = 1;
        v.elems.arr[0].dd.arr[0] = 0x55;
        v.elems.arr[1].exist.hh = 1;
        v.elems.arr[1].exist.dd = 1;
        v.elems.arr[1].dd.nCount = 2;
        ROUND_TRIP(ElemPDU, v);
    }
    /* nested components: h1 absent, or h1 present without h2 */
    {
        NestPDU v;
        NestPDU_Initialize(&v);
        DECODE(NestPDU, v, ERR_ACN_DECODE_NESTPDU_D, 0x40);
        DECODE(NestPDU, v, ERR_ACN_DECODE_NESTPDU_D, 0xC2, 0x40);       /* h1 (a = 0x12) without h2, d present */
        DECODE(NestPDU, v, 0, 0x00);
        EXPECT(!v.exist.h1 && !v.exist.d);
        NestPDU_Initialize(&v);
        v.exist.h1 = 1;
        v.h1.exist.h2 = 1;
        v.exist.d = 1;
        v.d.nCount = 3;
        ROUND_TRIP(NestPDU, v);
    }
    /* nested components present when the same field says so as the dependent */
    {
        Nest2PDU v;
        Nest2PDU_Initialize(&v);
        DECODE(Nest2PDU, v, 0, 0x00);
        EXPECT(!v.exist.h1 && !v.exist.d);
        Nest2PDU_Initialize(&v);
        v.exist.h1 = 1;
        v.h1.exist.h2 = 1;
        v.exist.d = 1;
        v.d.nCount = 2;
        ROUND_TRIP(Nest2PDU, v);
    }
    /* the dependent is an alternative of a mandatory CHOICE */
    {
        AltPDU v;
        AltPDU_Initialize(&v);
        DECODE(AltPDU, v, ERR_ACN_DECODE_ALTPDU_ALT_O1, 0x00);         /* header absent, alternative o1 */
        DECODE(AltPDU, v, 0, 0x40);                                     /* header absent, alternative o2 */
        EXPECT(!v.exist.header && v.alt.kind == o2_PRESENT);
        AltPDU_Initialize(&v);
        v.exist.header = 1;
        v.alt.kind = o1_PRESENT;
        v.alt.u.o1.nCount = 2;
        v.alt.u.o1.arr[0] = 7;
        v.alt.u.o1.arr[1] = 8;
        ROUND_TRIP(AltPDU, v);
    }
    /* a CONTAINING sized by the field: legacy checks the component (the error code of cont); --acn-v2 passes the
       field's reference (value 0) to cont's decoder, whose region is then too short for Inner */
    {
        ContPDU v;
        ContPDU_Initialize(&v);
        DECODE(ContPDU, v, -1, 0x40);
        DECODE(ContPDU, v, 0, 0x00);
        EXPECT(!v.exist.header && !v.exist.cont);
        ContPDU_Initialize(&v);
        v.exist.header = 1;
        v.exist.cont = 1;
        v.cont.v = 0x42;
        ROUND_TRIP(ContPDU, v);
    }
    /* the field as an ACN argument: legacy decodes Payload in place and checks at d; --acn-v2 passes the field's
       reference (value 0) to Payload's decoder, which takes size 0 (memory-safe; q/issues.md Issue 107: it should fail) */
    {
        ArgPDU v;
        ArgPDU_Initialize(&v);
#ifndef WIRE_ACN_V2
        DECODE(ArgPDU, v, ERR_ACN_DECODE_ARGPDU_PL_D, 0x40);
#else
        DECODE(ArgPDU, v, 0, 0x40);                                     /* today's --acn-v2 behaviour */
        EXPECT(!v.exist.header && v.exist.pl && v.pl.d.nCount == 0);
#endif
        DECODE(ArgPDU, v, 0, 0x16, 0x80);                               /* header, pl absent, tail 0x5A */
        EXPECT(!v.exist.header && !v.exist.pl && v.tail == 0x5A);
        ArgPDU_Initialize(&v);
        v.exist.header = 1;
        v.exist.pl = 1;
        v.pl.d.nCount = 2;
        ROUND_TRIP(ArgPDU, v);
    }
    /* an ACN argument used only by an absent part of the referenced type: header absent, pl present without d, e = 5,
       tail = 7 is valid (legacy writes 40 A0 E0), both modes decode it */
    {
        OptArgPDU v;
        OptArgPDU_Initialize(&v);
        DECODE(OptArgPDU, v, 0, 0x40, 0xA0, 0xE0);
        EXPECT(!v.exist.header && v.exist.pl && !v.pl.exist.d && v.pl.e == 5 && v.tail == 7);
        /* the same with d present (60 A0 E0): d's size is header.n, not in the stream */
#ifndef WIRE_ACN_V2
        DECODE(OptArgPDU, v, ERR_ACN_DECODE_OPTARGPDU_PL_D, 0x60, 0xA0, 0xE0);
#else
        DECODE(OptArgPDU, v, 0, 0x60, 0xA0, 0xE0);                      /* today's --acn-v2 behaviour, Issue 107 */
        EXPECT(!v.exist.header && v.exist.pl && v.pl.exist.d && v.pl.d.nCount == 0 && v.pl.e == 5 && v.tail == 7);
#endif
    }
    /* two ACN fields with one decode local (a.b-h.more and a-b.h.more are both ColPDU_a_b_h_more): the local is not
       reset, so the value decoded for a.b-h.more reaches y (a-b.h absent) */
    {
        ColPDU v;
        ColPDU_Initialize(&v);
        DECODE(ColPDU, v, 0, 0xE2);
        EXPECT(v.exist.a && v.a.exist.b_h && !v.a_b.exist.h && !v.a_b.exist.w && v.exist.y && v.y == 1 && v.tail == 0);
    }
    /* the component is ALWAYS ABSENT */
    {
        AbsPDU v;
        AbsPDU_Initialize(&v);
        DECODE(AbsPDU, v, ERR_ACN_DECODE_ABSPDU_DATA, 0x80);
        DECODE(AbsPDU, v, 0, 0x00);
        EXPECT(!v.exist.header && !v.exist.data);
    }
    /* component and dependent present when the same field says so */
    {
        SamePDU v;
        SamePDU_Initialize(&v);
        DECODE(SamePDU, v, 0, 0x00);
        EXPECT(!v.exist.header && !v.exist.data);
        SamePDU_Initialize(&v);
        v.exist.header = 1;
        v.exist.data = 1;
        v.data.nCount = 4;
        ROUND_TRIP(SamePDU, v);
    }
    if (failures == 0)
        printf("all checks passed\n");
    return failures == 0 ? 0 : 1;
}
