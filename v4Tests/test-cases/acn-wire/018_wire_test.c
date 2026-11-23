/* Checks of 018.asn1 (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, built with -DNDEBUG and ASan /
   UBSan; Issue 174). Generated from the row table of the i174 cycle; the Ada twin 018_wire_test.adb checks the same
   rows and the same bytes.
   - INVALID rows: a value outside a constraint at the composite level (a WITH COMPONENT narrowing of SEQUENCE OF
     elements, a union / intersection / EXCEPT of such loops and of SIZE, a WITH COMPONENTS union with an OPTIONAL
     child, a value-set string longer than the maximum): IsConstraintValid and the checked uPER and ACN encoders (into
     heap buffers of exactly REQUIRED_BYTES) return FALSE with the constraint's error code, before writing anything
     (stream position 0).
   - VALID rows: IsConstraintValid TRUE, uPER and ACN bits and bytes pinned, decoded back from the written octets (Equal).
   - DECODE rows: the stream an unchecked encoder writes for an INVALID value; the public decoders end with the
     validator, so they reject it with the constraint's error code, at the end of the decoded value (bit position
     pinned; the Ada decoders accepted these values before).
   CHECK, not assert: NDEBUG removes assert. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "018.h"

#define CHECK(c)                                                                          \
    do {                                                                                  \
        if (!(c)) {                                                                       \
            fprintf(stderr, "%s:%d: %s: check failed: %s\n", __FILE__, __LINE__, row, #c);\
            abort();                                                                      \
        }                                                                                 \
    } while (0)

static const char* row = "";

#define INVALID(T, V, ERR)                                                                \
    do {                                                                                  \
        int e_ = 0;                                                                       \
        byte* b_;                                                                         \
        BitStream s_;                                                                     \
        CHECK(!T##_IsConstraintValid(V, &e_) && e_ == (ERR));                             \
        b_ = calloc(T##_REQUIRED_BYTES_FOR_ENCODING, 1);                                  \
        CHECK(b_ != NULL);                                                                \
        BitStream_Init(&s_, b_, T##_REQUIRED_BYTES_FOR_ENCODING);                         \
        e_ = 0;                                                                           \
        CHECK(!T##_Encode(V, &s_, &e_, TRUE) && e_ == (ERR));                             \
        CHECK(s_.currentByte * 8 + s_.currentBit == 0);                                   \
        free(b_);                                                                         \
        b_ = calloc(T##_REQUIRED_BYTES_FOR_ACN_ENCODING, 1);                              \
        CHECK(b_ != NULL);                                                                \
        BitStream_Init(&s_, b_, T##_REQUIRED_BYTES_FOR_ACN_ENCODING);                     \
        e_ = 0;                                                                           \
        CHECK(!T##_ACN_Encode(V, &s_, &e_, TRUE) && e_ == (ERR));                         \
        CHECK(s_.currentByte * 8 + s_.currentBit == 0);                                   \
        free(b_);                                                                         \
    } while (0)

/* encode with ENC into exactly REQB octets: nBits bits, the bytes `expected`; decode from those octets only */
#define ROUND_TRIP(T, ENC, DEC, REQB, V, expected, nBits)                                 \
    do {                                                                                  \
        byte* b_ = calloc((REQB), 1);                                                     \
        static T d_;                                                                      \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(b_ != NULL);                                                                \
        CHECK(sizeof(expected) == ((nBits) + 7) / 8);                                     \
        BitStream_Init(&s_, b_, (REQB));                                                  \
        CHECK(ENC(V, &s_, &e_, TRUE) && e_ == 0);                                         \
        CHECK(s_.currentByte * 8 + s_.currentBit == (nBits));                             \
        CHECK(memcmp(b_, (expected), sizeof(expected)) == 0);                             \
        BitStream_AttachBuffer(&s_, b_, sizeof(expected));                                \
        memset(&d_, 0, sizeof d_);                                                        \
        CHECK(DEC(PTR_##T(d_), &s_, &e_));                                                \
        CHECK(T##_Equal(V, PTR_##T(d_)));                                                 \
        CHECK(s_.currentByte * 8 + s_.currentBit == (nBits));                             \
        free(b_);                                                                         \
    } while (0)

#define VALID(T, V, uBytes, uBits, aBytes, aBits)                                         \
    do {                                                                                  \
        int e_ = 0;                                                                       \
        CHECK(T##_IsConstraintValid(V, &e_) && e_ == 0);                                  \
        ROUND_TRIP(T, T##_Encode, T##_Decode, T##_REQUIRED_BYTES_FOR_ENCODING, V, uBytes, uBits); \
        ROUND_TRIP(T, T##_ACN_Encode, T##_ACN_Decode, T##_REQUIRED_BYTES_FOR_ACN_ENCODING, V, aBytes, aBits); \
    } while (0)

/* the public decoders end with IsConstraintValid */
#define DECODE(T, DEC, stream, ERR, POS)                                                  \
    do {                                                                                  \
        static T d_;                                                                      \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        byte* b_ = calloc(sizeof(stream), 1);                                             \
        CHECK(b_ != NULL);                                                                \
        memcpy(b_, (stream), sizeof(stream));                                             \
        BitStream_AttachBuffer(&s_, b_, sizeof(stream));                                  \
        memset(&d_, 0, sizeof d_);                                                        \
        CHECK(!DEC(PTR_##T(d_), &s_, &e_) && e_ == (ERR));                                \
        CHECK(s_.currentByte * 8 + s_.currentBit == (POS));                               \
        free(b_);                                                                         \
    } while (0)


#define PTR_OutK(x) &(x)
#define PTR_SubK(x) &(x)
#define PTR_OutV(x) &(x)
#define PTR_Out2(x) &(x)
#define PTR_OutO(x) &(x)
#define PTR_OutB(x) &(x)
#define PTR_OutS(x) &(x)
#define PTR_OutKF(x) &(x)
#define PTR_OutN(x) &(x)
#define PTR_DirK(x) &(x)
#define PTR_OutI(x) &(x)
#define PTR_OutE(x) &(x)
#define PTR_SubU(x) &(x)
#define PTR_OutU(x) &(x)
#define PTR_OptU(x) &(x)
#define PTR_SubX(x) &(x)
#define PTR_OptK(x) &(x)
#define PTR_ChK(x) &(x)
#define PTR_OutEO(x) &(x)
#define PTR_MixA(x) &(x)
#define PTR_MixB(x) &(x)
#define PTR_MixC(x) &(x)
#define PTR_MixD(x) &(x)
#define PTR_MixE(x) &(x)
#define PTR_LngT(x) &(x)
#define PTR_LngU(x) x
#define PTR_LngW(x) &(x)
#define PTR_LngV(x) &(x)

int main(void)
{
    row = "OutK";
    {
        static OutK v; memset(&v, 0, sizeof v);
        v.k.nCount = 3;
        strcpy(v.k.arr[0], "abcdef");
        strcpy(v.k.arr[1], "abcdef");
        strcpy(v.k.arr[2], "abcdef");
        v.b = TRUE;
        INVALID(OutK, &v, ERR_OUTK_K_2);
    }
    row = "SubK";
    {
        static SubK v; memset(&v, 0, sizeof v);
        v.nCount = 3;
        strcpy(v.arr[0], "abcdef");
        strcpy(v.arr[1], "abcdef");
        strcpy(v.arr[2], "abcdef");
        INVALID(SubK, &v, ERR_SUBK_2);
    }
    row = "OutV";
    {
        static OutV v; memset(&v, 0, sizeof v);
        v.k.nCount = 3;
        strcpy(v.k.arr[0], "abcdef");
        strcpy(v.k.arr[1], "abcdef");
        strcpy(v.k.arr[2], "abcdef");
        v.b = TRUE;
        INVALID(OutV, &v, ERR_OUTV_K_2);
    }
    row = "OutVba";
    {
        static OutV v; memset(&v, 0, sizeof v);
        v.k.nCount = 2;
        strcpy(v.k.arr[0], "ab");
        strcpy(v.k.arr[1], "ba");
        v.b = TRUE;
        INVALID(OutV, &v, ERR_OUTV_K_2);
    }
    row = "Out2";
    {
        static Out2 v; memset(&v, 0, sizeof v);
        v.x.k.nCount = 3;
        strcpy(v.x.k.arr[0], "abcdef");
        strcpy(v.x.k.arr[1], "abcdef");
        strcpy(v.x.k.arr[2], "abcdef");
        v.x.b = TRUE;
        INVALID(Out2, &v, ERR_OUT2_X_2);
    }
    row = "OutO";
    {
        static OutO v; memset(&v, 0, sizeof v);
        v.k.nCount = 3;
        v.k.arr[0].nCount = 6;
        memset(v.k.arr[0].arr, 0x55, 6);
        v.k.arr[1].nCount = 6;
        memset(v.k.arr[1].arr, 0x55, 6);
        v.k.arr[2].nCount = 6;
        memset(v.k.arr[2].arr, 0x55, 6);
        v.b = TRUE;
        INVALID(OutO, &v, ERR_OUTO_K_2);
    }
    row = "OutB";
    {
        static OutB v; memset(&v, 0, sizeof v);
        v.k.nCount = 3;
        v.k.arr[0].nCount = 20;
        memset(v.k.arr[0].arr, 0, 3);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 0);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 1);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 2);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 3);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 4);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 5);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 6);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 7);
        v.k.arr[0].arr[1] |= (byte)(0x80 >> 0);
        v.k.arr[0].arr[1] |= (byte)(0x80 >> 1);
        v.k.arr[0].arr[1] |= (byte)(0x80 >> 2);
        v.k.arr[0].arr[1] |= (byte)(0x80 >> 3);
        v.k.arr[0].arr[1] |= (byte)(0x80 >> 4);
        v.k.arr[0].arr[1] |= (byte)(0x80 >> 5);
        v.k.arr[0].arr[1] |= (byte)(0x80 >> 6);
        v.k.arr[0].arr[1] |= (byte)(0x80 >> 7);
        v.k.arr[0].arr[2] |= (byte)(0x80 >> 0);
        v.k.arr[0].arr[2] |= (byte)(0x80 >> 1);
        v.k.arr[0].arr[2] |= (byte)(0x80 >> 2);
        v.k.arr[0].arr[2] |= (byte)(0x80 >> 3);
        v.k.arr[1].nCount = 20;
        memset(v.k.arr[1].arr, 0, 3);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 0);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 1);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 2);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 3);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 4);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 5);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 6);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 7);
        v.k.arr[1].arr[1] |= (byte)(0x80 >> 0);
        v.k.arr[1].arr[1] |= (byte)(0x80 >> 1);
        v.k.arr[1].arr[1] |= (byte)(0x80 >> 2);
        v.k.arr[1].arr[1] |= (byte)(0x80 >> 3);
        v.k.arr[1].arr[1] |= (byte)(0x80 >> 4);
        v.k.arr[1].arr[1] |= (byte)(0x80 >> 5);
        v.k.arr[1].arr[1] |= (byte)(0x80 >> 6);
        v.k.arr[1].arr[1] |= (byte)(0x80 >> 7);
        v.k.arr[1].arr[2] |= (byte)(0x80 >> 0);
        v.k.arr[1].arr[2] |= (byte)(0x80 >> 1);
        v.k.arr[1].arr[2] |= (byte)(0x80 >> 2);
        v.k.arr[1].arr[2] |= (byte)(0x80 >> 3);
        v.k.arr[2].nCount = 20;
        memset(v.k.arr[2].arr, 0, 3);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 0);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 1);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 2);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 3);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 4);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 5);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 6);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 7);
        v.k.arr[2].arr[1] |= (byte)(0x80 >> 0);
        v.k.arr[2].arr[1] |= (byte)(0x80 >> 1);
        v.k.arr[2].arr[1] |= (byte)(0x80 >> 2);
        v.k.arr[2].arr[1] |= (byte)(0x80 >> 3);
        v.k.arr[2].arr[1] |= (byte)(0x80 >> 4);
        v.k.arr[2].arr[1] |= (byte)(0x80 >> 5);
        v.k.arr[2].arr[1] |= (byte)(0x80 >> 6);
        v.k.arr[2].arr[1] |= (byte)(0x80 >> 7);
        v.k.arr[2].arr[2] |= (byte)(0x80 >> 0);
        v.k.arr[2].arr[2] |= (byte)(0x80 >> 1);
        v.k.arr[2].arr[2] |= (byte)(0x80 >> 2);
        v.k.arr[2].arr[2] |= (byte)(0x80 >> 3);
        v.b = TRUE;
        INVALID(OutB, &v, ERR_OUTB_K_2);
    }
    row = "OutS";
    {
        static OutS v; memset(&v, 0, sizeof v);
        v.k.nCount = 3;
        strcpy(v.k.arr[0].s, "abcdef");
        v.k.arr[0].n = 1000;
        strcpy(v.k.arr[1].s, "abcdef");
        v.k.arr[1].n = 1000;
        strcpy(v.k.arr[2].s, "abcdef");
        v.k.arr[2].n = 1000;
        v.b = TRUE;
        INVALID(OutS, &v, ERR_OUTS_K_2);
    }
    row = "OutSn";
    {
        static OutS v; memset(&v, 0, sizeof v);
        v.k.nCount = 1;
        strcpy(v.k.arr[0].s, "ab");
        v.k.arr[0].n = 11;
        v.b = TRUE;
        INVALID(OutS, &v, ERR_OUTS_K_2);
    }
    row = "OutKF";
    {
        static OutKF v; memset(&v, 0, sizeof v);
        strcpy(v.k.arr[0], "abcdef");
        strcpy(v.k.arr[1], "abcdef");
        v.b = TRUE;
        INVALID(OutKF, &v, ERR_OUTKF_K_2);
    }
    row = "OutN";
    {
        static OutN v; memset(&v, 0, sizeof v);
        v.k.nCount = 2;
        v.k.arr[0].nCount = 3;
        strcpy(v.k.arr[0].arr[0], "abcdef");
        strcpy(v.k.arr[0].arr[1], "abcdef");
        strcpy(v.k.arr[0].arr[2], "abcdef");
        v.k.arr[1].nCount = 3;
        strcpy(v.k.arr[1].arr[0], "abcdef");
        strcpy(v.k.arr[1].arr[1], "abcdef");
        strcpy(v.k.arr[1].arr[2], "abcdef");
        v.b = TRUE;
        INVALID(OutN, &v, ERR_OUTN_K_2);
    }
    row = "DirK";
    {
        static DirK v; memset(&v, 0, sizeof v);
        v.nCount = 3;
        strcpy(v.arr[0], "abcdef");
        strcpy(v.arr[1], "abcdef");
        strcpy(v.arr[2], "abcdef");
        INVALID(DirK, &v, ERR_DIRK);
    }
    row = "OutI";
    {
        static OutI v; memset(&v, 0, sizeof v);
        v.k.nCount = 2;
        v.k.arr[0] = 1000;
        v.k.arr[1] = 5;
        v.b = TRUE;
        INVALID(OutI, &v, ERR_OUTI_K_2);
    }
    row = "OutE";
    {
        static OutE v; memset(&v, 0, sizeof v);
        v.k.nCount = 2;
        v.k.arr[0] = e1;
        v.k.arr[1] = e3;
        v.b = TRUE;
        INVALID(OutE, &v, ERR_OUTE_K_2);
    }
    row = "SubUm";
    {
        static SubU v; memset(&v, 0, sizeof v);
        v.nCount = 2;
        strcpy(v.arr[0], "a");
        strcpy(v.arr[1], "bc");
        INVALID(SubU, &v, ERR_SUBU_2);
    }
    row = "OutUm";
    {
        static OutU v; memset(&v, 0, sizeof v);
        v.k.nCount = 2;
        strcpy(v.k.arr[0], "ab");
        strcpy(v.k.arr[1], "c");
        v.b = TRUE;
        INVALID(OutU, &v, ERR_OUTU_K_2);
    }
    row = "OptUa";
    {
        static OptU v; memset(&v, 0, sizeof v);
        v.exist.a = 1;
        v.a = 15;
        v.b = 55;
        INVALID(OptU, &v, ERR_OPTU);
    }
    row = "SubXs";
    {
        static SubX v; memset(&v, 0, sizeof v);
        v.nCount = 1;
        strcpy(v.arr[0], "abcd");
        INVALID(SubX, &v, ERR_SUBX_2);
    }
    row = "SubXe";
    {
        static SubX v; memset(&v, 0, sizeof v);
        v.nCount = 2;
        strcpy(v.arr[0], "abc");
        strcpy(v.arr[1], "abc");
        INVALID(SubX, &v, ERR_SUBX_2);
    }
    row = "OptK";
    {
        static OptK v; memset(&v, 0, sizeof v);
        v.exist.k = 1;
        v.k.nCount = 1;
        strcpy(v.k.arr[0], "abc");
        v.b = TRUE;
        INVALID(OptK, &v, ERR_OPTK);
    }
    row = "ChKk";
    {
        static ChK v; memset(&v, 0, sizeof v);
        v.kind = k_PRESENT;
        v.u.k.nCount = 1;
        strcpy(v.u.k.arr[0], "abc");
        INVALID(ChK, &v, ERR_CHK);
    }
    row = "OutEOa";
    {
        static OutEO v; memset(&v, 0, sizeof v);
        v.k.nCount = 1;
        v.k.arr[0].exist.s = 0;
        v.k.arr[0].n = 3;
        v.b = TRUE;
        INVALID(OutEO, &v, ERR_OUTEO_K_2);
    }
    row = "OutEOs";
    {
        static OutEO v; memset(&v, 0, sizeof v);
        v.k.nCount = 1;
        v.k.arr[0].exist.s = 1;
        strcpy(v.k.arr[0].s, "abc");
        v.k.arr[0].n = 3;
        v.b = TRUE;
        INVALID(OutEO, &v, ERR_OUTEO_K_2);
    }
    row = "MixAm";
    {
        static MixA v; memset(&v, 0, sizeof v);
        v.nCount = 2;
        strcpy(v.arr[0], "abc");
        strcpy(v.arr[1], "d");
        INVALID(MixA, &v, ERR_MIXA_2);
    }
    row = "MixBm";
    {
        static MixB v; memset(&v, 0, sizeof v);
        v.nCount = 2;
        strcpy(v.arr[0], "abc");
        strcpy(v.arr[1], "d");
        INVALID(MixB, &v, ERR_MIXB_2);
    }
    row = "MixCl";
    {
        static MixC v; memset(&v, 0, sizeof v);
        v.nCount = 1;
        strcpy(v.arr[0], "abc");
        INVALID(MixC, &v, ERR_MIXC_2);
    }
    row = "MixCs";
    {
        static MixC v; memset(&v, 0, sizeof v);
        v.nCount = 3;
        strcpy(v.arr[0], "a");
        strcpy(v.arr[1], "b");
        strcpy(v.arr[2], "c");
        INVALID(MixC, &v, ERR_MIXC_2);
    }
    row = "MixDm";
    {
        static MixD v; memset(&v, 0, sizeof v);
        v.nCount = 2;
        strcpy(v.arr[0], "abc");
        strcpy(v.arr[1], "def");
        INVALID(MixD, &v, ERR_MIXD_2);
    }
    row = "MixEs";
    {
        static MixE v; memset(&v, 0, sizeof v);
        v.nCount = 3;
        strcpy(v.arr[0], "a");
        strcpy(v.arr[1], "b");
        strcpy(v.arr[2], "c");
        INVALID(MixE, &v, ERR_MIXE_2);
    }
    row = "MixEl";
    {
        static MixE v; memset(&v, 0, sizeof v);
        v.nCount = 1;
        strcpy(v.arr[0], "abc");
        INVALID(MixE, &v, ERR_MIXE_2);
    }
    row = "LngTm";
    {
        static LngT v; memset(&v, 0, sizeof v);
        v.nCount = 2;
        strcpy(v.arr[0], "a");
        strcpy(v.arr[1], "zz");
        INVALID(LngT, &v, ERR_LNGT_2);
    }
    row = "LngUm";
    {
        LngU v; memset(v, 0, sizeof(v));
        strcpy(v, "zz");
        INVALID(LngU, v, ERR_LNGU_2);
    }
    row = "LngWm";
    {
        static LngW v; memset(&v, 0, sizeof v);
        strcpy(v.x, "zz");
        INVALID(LngW, &v, ERR_LNGW_X_2);
    }
    row = "LngVm";
    {
        static LngV v; memset(&v, 0, sizeof v);
        strcpy(v.x, "zz");
        INVALID(LngV, &v, ERR_LNGV);
    }
    row = "OutKv";
    {
        static OutK v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x8E, 0x1C, 0x43, 0x09, 0xC5, 0x86};
        static const byte ab[] = {0x8E, 0x1C, 0x43, 0x09, 0xC5, 0x86};
        v.k.nCount = 3;
        strcpy(v.k.arr[0], "ab");
        strcpy(v.k.arr[1], "a");
        strcpy(v.k.arr[2], "ba");
        v.b = TRUE;
        VALID(OutK, &v, ub, 47, ab, 47);
    }
    row = "SubKv";
    {
        static SubK v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x4E, 0x1C, 0x43, 0xD0};
        static const byte ab[] = {0x4E, 0x1C, 0x43, 0xD0};
        v.nCount = 2;
        strcpy(v.arr[0], "ab");
        strcpy(v.arr[1], "z");
        VALID(SubK, &v, ub, 29, ab, 29);
    }
    row = "OutVv";
    {
        static OutV v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x46, 0x13, 0x87, 0x14};
        static const byte ab[] = {0x46, 0x13, 0x87, 0x14};
        v.k.nCount = 2;
        strcpy(v.k.arr[0], "a");
        strcpy(v.k.arr[1], "ab");
        v.b = TRUE;
        VALID(OutV, &v, ub, 30, ab, 30);
    }
    row = "Out2v";
    {
        static Out2 v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x0E, 0x1C, 0x40};
        static const byte ab[] = {0x0E, 0x1C, 0x40};
        v.x.k.nCount = 1;
        strcpy(v.x.k.arr[0], "ab");
        v.x.b = FALSE;
        VALID(Out2, &v, ub, 20, ab, 20);
    }
    row = "OutOv";
    {
        static OutO v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x4D, 0x2D, 0x29, 0xA5, 0xA5, 0x80};
        static const byte ab[] = {0x74, 0xB4, 0xBA, 0x5A, 0x58};
        v.k.nCount = 2;
        v.k.arr[0].nCount = 2;
        memset(v.k.arr[0].arr, 0xA5, 2);
        v.k.arr[1].nCount = 2;
        memset(v.k.arr[1].arr, 0xA5, 2);
        v.b = TRUE;
        VALID(OutO, &v, ub, 41, ab, 37);
    }
    row = "OutBv";
    {
        static OutB v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x87, 0x03, 0x81, 0xC4};
        static const byte ab[] = {0xB8, 0xE3, 0x88};
        v.k.nCount = 3;
        v.k.arr[0].nCount = 4;
        memset(v.k.arr[0].arr, 0, 3);
        v.k.arr[0].arr[0] |= (byte)(0x80 >> 0);
        v.k.arr[1].nCount = 4;
        memset(v.k.arr[1].arr, 0, 3);
        v.k.arr[1].arr[0] |= (byte)(0x80 >> 0);
        v.k.arr[2].nCount = 4;
        memset(v.k.arr[2].arr, 0, 3);
        v.k.arr[2].arr[0] |= (byte)(0x80 >> 0);
        v.b = TRUE;
        VALID(OutB, &v, ub, 30, ab, 21);
    }
    row = "OutSv";
    {
        static OutS v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x4E, 0x1C, 0x40, 0x50, 0xC2, 0x00, 0x40};
        static const byte ab[] = {0x4E, 0x1C, 0x40, 0x50, 0xC2, 0x00, 0x40};
        v.k.nCount = 2;
        strcpy(v.k.arr[0].s, "ab");
        v.k.arr[0].n = 10;
        strcpy(v.k.arr[1].s, "a");
        v.k.arr[1].n = 0;
        v.b = TRUE;
        VALID(OutS, &v, ub, 50, ab, 50);
    }
    row = "OutKFv";
    {
        static OutKF v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x38, 0x71, 0x0C, 0x70};
        static const byte ab[] = {0x38, 0x71, 0x0C, 0x70};
        strcpy(v.k.arr[0], "ab");
        strcpy(v.k.arr[1], "c");
        v.b = TRUE;
        VALID(OutKF, &v, ub, 28, ab, 28);
    }
    row = "OutNv";
    {
        static OutN v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x87, 0x0E, 0x24, 0x61, 0x18, 0xA0};
        static const byte ab[] = {0x87, 0x0E, 0x24, 0x61, 0x18, 0xA0};
        v.k.nCount = 2;
        v.k.arr[0].nCount = 1;
        strcpy(v.k.arr[0].arr[0], "ab");
        v.k.arr[1].nCount = 2;
        strcpy(v.k.arr[1].arr[0], "a");
        strcpy(v.k.arr[1].arr[1], "b");
        v.b = TRUE;
        VALID(OutN, &v, ub, 43, ab, 43);
    }
    row = "DirKv";
    {
        static DirK v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x0F, 0x8F, 0x20};
        static const byte ab[] = {0x0F, 0x8F, 0x20};
        v.nCount = 1;
        strcpy(v.arr[0], "xy");
        VALID(DirK, &v, ub, 19, ab, 19);
    }
    row = "OutIv";
    {
        static OutI v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x80, 0xA0, 0x00, 0x07, 0x80};
        static const byte ab[] = {0x80, 0xA0, 0x00, 0x07, 0x80};
        v.k.nCount = 3;
        v.k.arr[0] = 10;
        v.k.arr[1] = 0;
        v.k.arr[2] = 7;
        v.b = TRUE;
        VALID(OutI, &v, ub, 33, ab, 33);
    }
    row = "OutEv";
    {
        static OutE v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x44};
        static const byte ab[] = {0x44};
        v.k.nCount = 2;
        v.k.arr[0] = e1;
        v.k.arr[1] = e2;
        v.b = FALSE;
        VALID(OutE, &v, ub, 7, ab, 7);
    }
    row = "SubU1";
    {
        static SubU v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x46, 0x11, 0x88};
        static const byte ab[] = {0x46, 0x11, 0x88};
        v.nCount = 2;
        strcpy(v.arr[0], "a");
        strcpy(v.arr[1], "b");
        VALID(SubU, &v, ub, 22, ab, 22);
    }
    row = "SubU2";
    {
        static SubU v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x4E, 0x1C, 0x47, 0x1E, 0x40};
        static const byte ab[] = {0x4E, 0x1C, 0x47, 0x1E, 0x40};
        v.nCount = 2;
        strcpy(v.arr[0], "ab");
        strcpy(v.arr[1], "cd");
        VALID(SubU, &v, ub, 36, ab, 36);
    }
    row = "OutU2";
    {
        static OutU v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x8E, 0x1C, 0x47, 0x1E, 0x43, 0x97, 0x34};
        static const byte ab[] = {0x8E, 0x1C, 0x47, 0x1E, 0x43, 0x97, 0x34};
        v.k.nCount = 3;
        strcpy(v.k.arr[0], "ab");
        strcpy(v.k.arr[1], "cd");
        strcpy(v.k.arr[2], "ef");
        v.b = TRUE;
        VALID(OutU, &v, ub, 54, ab, 54);
    }
    row = "OptUb";
    {
        static OptU v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x37};
        static const byte ab[] = {0x37};
        v.exist.a = 0;
        v.b = 55;
        VALID(OptU, &v, ub, 8, ab, 8);
    }
    row = "OptUp";
    {
        static OptU v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x99, 0x6E};
        static const byte ab[] = {0x99, 0x6E};
        v.exist.a = 1;
        v.a = 25;
        v.b = 55;
        VALID(OptU, &v, ub, 15, ab, 15);
    }
    row = "OptU1";
    {
        static OptU v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x85, 0x0A};
        static const byte ab[] = {0x85, 0x0A};
        v.exist.a = 1;
        v.a = 5;
        v.b = 5;
        VALID(OptU, &v, ub, 15, ab, 15);
    }
    row = "SubXv";
    {
        static SubX v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x4E, 0x1C, 0x4B, 0x0E, 0x2C, 0x60};
        static const byte ab[] = {0x4E, 0x1C, 0x4B, 0x0E, 0x2C, 0x60};
        v.nCount = 2;
        strcpy(v.arr[0], "ab");
        strcpy(v.arr[1], "abc");
        VALID(SubX, &v, ub, 43, ab, 43);
    }
    row = "OptKa";
    {
        static OptK v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x00};
        static const byte ab[] = {0x00};
        v.exist.k = 0;
        v.b = FALSE;
        VALID(OptK, &v, ub, 2, ab, 2);
    }
    row = "OptKp";
    {
        static OptK v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0xA7, 0x0E, 0x21, 0x8E};
        static const byte ab[] = {0xA7, 0x0E, 0x21, 0x8E};
        v.exist.k = 1;
        v.k.nCount = 2;
        strcpy(v.k.arr[0], "ab");
        strcpy(v.k.arr[1], "c");
        v.b = TRUE;
        VALID(OptK, &v, ub, 31, ab, 31);
    }
    row = "ChKkv";
    {
        static ChK v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x27, 0x0E, 0x21, 0x8C};
        static const byte ab[] = {0x27, 0x0E, 0x21, 0x8C};
        v.kind = k_PRESENT;
        v.u.k.nCount = 2;
        strcpy(v.u.k.arr[0], "ab");
        strcpy(v.u.k.arr[1], "c");
        VALID(ChK, &v, ub, 30, ab, 30);
    }
    row = "ChKn";
    {
        static ChK v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0xD0};
        static const byte ab[] = {0xD0};
        v.kind = n_PRESENT;
        v.u.n = 5;
        VALID(ChK, &v, ub, 4, ab, 4);
    }
    row = "OutEOv";
    {
        static OutEO v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x67, 0x0E, 0x23, 0x8F, 0x40};
        static const byte ab[] = {0x4E, 0x1C, 0x46, 0x3D, 0x00};
        v.k.nCount = 2;
        v.k.arr[0].exist.s = 1;
        strcpy(v.k.arr[0].s, "ab");
        v.k.arr[0].n = 3;
        v.k.arr[1].exist.s = 1;
        strcpy(v.k.arr[1].s, "z");
        v.k.arr[1].n = 0;
        v.b = FALSE;
        VALID(OutEO, &v, ub, 40, ab, 38);
    }
    row = "MixA1";
    {
        static MixA v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x16, 0x1C, 0x58, 0xC0};
        static const byte ab[] = {0x16, 0x1C, 0x58, 0xC0};
        v.nCount = 1;
        strcpy(v.arr[0], "abc");
        VALID(MixA, &v, ub, 26, ab, 26);
    }
    row = "MixA2";
    {
        static MixA v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x4E, 0x1C, 0x47, 0x1E, 0x40};
        static const byte ab[] = {0x4E, 0x1C, 0x47, 0x1E, 0x40};
        v.nCount = 2;
        strcpy(v.arr[0], "ab");
        strcpy(v.arr[1], "cd");
        VALID(MixA, &v, ub, 36, ab, 36);
    }
    row = "MixB1";
    {
        static MixB v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x16, 0x1C, 0x58, 0xC0};
        static const byte ab[] = {0x16, 0x1C, 0x58, 0xC0};
        v.nCount = 1;
        strcpy(v.arr[0], "abc");
        VALID(MixB, &v, ub, 26, ab, 26);
    }
    row = "MixB2";
    {
        static MixB v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x4E, 0x1C, 0x47, 0x1E, 0x40};
        static const byte ab[] = {0x4E, 0x1C, 0x47, 0x1E, 0x40};
        v.nCount = 2;
        strcpy(v.arr[0], "ab");
        strcpy(v.arr[1], "cd");
        VALID(MixB, &v, ub, 36, ab, 36);
    }
    row = "MixCv";
    {
        static MixC v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x9C, 0x38, 0x86, 0x30};
        static const byte ab[] = {0x9C, 0x38, 0x86, 0x30};
        v.nCount = 2;
        strcpy(v.arr[0], "ab");
        strcpy(v.arr[1], "c");
        VALID(MixC, &v, ub, 28, ab, 28);
    }
    row = "MixDv";
    {
        static MixD v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x4E, 0x1C, 0x4B, 0x0E, 0x2C, 0x60};
        static const byte ab[] = {0x4E, 0x1C, 0x4B, 0x0E, 0x2C, 0x60};
        v.nCount = 2;
        strcpy(v.arr[0], "ab");
        strcpy(v.arr[1], "abc");
        VALID(MixD, &v, ub, 43, ab, 43);
    }
    row = "MixEv";
    {
        static MixE v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x0E, 0x1C, 0x40};
        static const byte ab[] = {0x0E, 0x1C, 0x40};
        v.nCount = 1;
        strcpy(v.arr[0], "ab");
        VALID(MixE, &v, ub, 19, ab, 19);
    }
    row = "LngTv";
    {
        static LngT v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x58, 0x58, 0x40};
        static const byte ab[] = {0x58, 0x58, 0x40};
        v.nCount = 2;
        strcpy(v.arr[0], "a");
        strcpy(v.arr[1], "a");
        VALID(LngT, &v, ub, 18, ab, 18);
    }
    row = "LngUv";
    {
        LngU v; memset(v, 0, sizeof(v));
        static const byte ub[] = {0x61};
        static const byte ab[] = {0x61};
        strcpy(v, "a");
        VALID(LngU, v, ub, 8, ab, 8);
    }
    row = "LngWv";
    {
        static LngW v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x61};
        static const byte ab[] = {0x61};
        strcpy(v.x, "a");
        VALID(LngW, &v, ub, 8, ab, 8);
    }
    row = "LngVv";
    {
        static LngV v; memset(&v, 0, sizeof v);
        static const byte ub[] = {0x61};
        static const byte ab[] = {0x61};
        strcpy(v.x, "a");
        VALID(LngV, &v, ub, 8, ab, 8);
    }
    row = "DecOutVba";
    {
        static const byte st[] = {0x4E, 0x1C, 0x47, 0x16, 0x18}; /* 37 bits */
        DECODE(OutV, OutV_ACN_Decode, st, ERR_OUTV_K_2, 37);
    }
    row = "DecOutI";
    {
        static const byte st[] = {0x7E, 0x80, 0x16}; /* 23 bits */
        DECODE(OutI, OutI_ACN_Decode, st, ERR_OUTI_K_2, 23);
    }
    row = "DecOutE";
    {
        static const byte st[] = {0x4A}; /* 7 bits */
        DECODE(OutE, OutE_ACN_Decode, st, ERR_OUTE_K_2, 7);
    }
    row = "DecOutK";
    {
        static const byte st[] = {0xAE, 0x1C, 0x58, 0xF2, 0x65, 0xCD, 0x70, 0xE2, 0xC7, 0x93, 0x2E, 0x6B, 0x87, 0x16, 0x3C, 0x99, 0x73, 0x40}; /* 138 bits */
        DECODE(OutK, OutK_Decode, st, ERR_OUTK_K_2, 138);
    }
    row = "DecOutS";
    {
        static const byte st[] = {0xAE, 0x1C, 0x58, 0xF2, 0x65, 0xCD, 0xF4, 0x5C, 0x38, 0xB1, 0xE4, 0xCB, 0x9B, 0xE8, 0xB8, 0x71, 0x63, 0xC9, 0x97, 0x37, 0xD1}; /* 168 bits */
        DECODE(OutS, OutS_Decode, st, ERR_OUTS_K_2, 168);
    }
    row = "DecOptUa";
    {
        static const byte st[] = {0x8F, 0x6E}; /* 15 bits */
        DECODE(OptU, OptU_ACN_Decode, st, ERR_OPTU, 15);
    }
    row = "DecChKk";
    {
        static const byte st[] = {0x0B, 0x0E, 0x2C, 0x60}; /* 27 bits */
        DECODE(ChK, ChK_Decode, st, ERR_CHK, 27);
    }
    row = "DecMixAm";
    {
        static const byte st[] = {0x56, 0x1C, 0x58, 0xC6, 0x40}; /* 36 bits */
        DECODE(MixA, MixA_Decode, st, ERR_MIXA_2, 36);
    }
    row = "DecLngTm";
    {
        static const byte st[] = {0x58, 0x7E, 0xBD, 0x00}; /* 25 bits */
        DECODE(LngT, LngT_ACN_Decode, st, ERR_LNGT_2, 25);
    }
    printf("acn-wire 018: 34 invalid, 35 valid, 9 decode rows OK\n");
    return 0;
}
