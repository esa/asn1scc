/* Checks of 017.asn1 / 017.acn (run by v4Tests/scripts/runWireTests.sh, legacy and --acn-v2, built with -DNDEBUG and
   ASan / UBSan; Issue 171, Issue 172): streams decoded from a heap buffer of exactly the type's REQUIRED_BYTES.
   - Rejected rows (a length above the maximum to which a WITH COMPONENTS constraint narrows a default-encoding
     IA5String / NumericString): the decoder must fail with the error code and at the bit position given, i.e. right
     after the length field. Before the fix C read the characters and failed later (at the end of the stream, or in the
     validation), with another code; Ada read past REQUIRED_BITS (-gnata: precondition / postcondition failures).
   - Rows that the validation rejects (a length below the narrowed minimum, inside a union's hull, a BIT / OCTET STRING
     value set given length 0): code and position pinned as well; Issue 172 (Ada: BitString_equal / OctetString_equal
     with a decoded length 0) must not raise there.
   - Valid rows: decoded at the position given, then encoded again into exactly REQUIRED_BYTES: the same bytes.
   The Ada twin 017_wire_test.adb decodes the same rows and must give the same results. The rows are generated (with
   the expected results, C = Ada in both modes); CHECK, not assert: NDEBUG removes assert. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "017.h"

#define CHECK(c)                                                                          \
    do {                                                                                  \
        if (!(c)) {                                                                       \
            fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #c);         \
            abort();                                                                      \
        }                                                                                 \
    } while (0)

/* decode the row from exactly REQB octets (a longer row is cut): result, error code, end bit position; a
   valid row is encoded again into exactly REQB octets and must give the row's bytes */
#define ROW(T, DEC, ENC, REQB, OK, ERR, POS, ...)                                         \
    do {                                                                                  \
        static const byte src_[] = {__VA_ARGS__};                                         \
        static T v_;                                                                      \
        byte* buf_ = calloc((REQB), 1);                                                   \
        BitStream s_;                                                                     \
        int e_ = 0;                                                                       \
        CHECK(buf_ != NULL);                                                              \
        memcpy(buf_, src_, sizeof(src_) < (size_t)(REQB) ? sizeof(src_) : (size_t)(REQB)); \
        BitStream_AttachBuffer(&s_, buf_, (REQB));                                        \
        memset(&v_, 0, sizeof v_);                                                        \
        CHECK(DEC(&v_, &s_, &e_) == (OK));                                                \
        CHECK((ERR) < 0 ? e_ != 0 : e_ == (ERR));                                         \
        CHECK(s_.currentByte * 8 + s_.currentBit == (POS));                               \
        if (OK) {                                                                         \
            CHECK(sizeof(src_) == ((POS) + 7) / 8);                                       \
            memset(buf_, 0, (REQB));                                                      \
            BitStream_Init(&s_, buf_, (REQB));                                            \
            CHECK(ENC(&v_, &s_, &e_, TRUE));                                              \
            CHECK(s_.currentByte * 8 + s_.currentBit == (POS));                           \
            CHECK(memcmp(buf_, src_, sizeof(src_)) == 0);                                 \
        }                                                                                 \
        free(buf_);                                                                       \
    } while (0)

/* the error codes of the --acn-v2 specialised functions are declared in 017.c: any nonzero code there */
#ifdef ERR_ACN_DECODE_OUTERQ_W1_P_S
#define EXP_ERR_ACN_DECODE_OUTERQ_W1_P_S ERR_ACN_DECODE_OUTERQ_W1_P_S
#else
#define EXP_ERR_ACN_DECODE_OUTERQ_W1_P_S -1
#endif
#ifdef ERR_ACN_DECODE_OUTERQ_W2_P_S
#define EXP_ERR_ACN_DECODE_OUTERQ_W2_P_S ERR_ACN_DECODE_OUTERQ_W2_P_S
#else
#define EXP_ERR_ACN_DECODE_OUTERQ_W2_P_S -1
#endif

int main(void)
{
    /* S_a: valid */
    ROW(OuterS, OuterS_ACN_Decode, OuterS_ACN_Encode, OuterS_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 11, 0x18, 0x60);
    /* S_ab: valid */
    ROW(OuterS, OuterS_ACN_Decode, OuterS_ACN_Encode, OuterS_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 18, 0x38, 0x71, 0x00);
    /* S_len3: length 3 > 2: rejected after the length field */
    ROW(OuterS, OuterS_ACN_Decode, OuterS_ACN_Encode, OuterS_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERS_X_S, 3, 0x58, 0x71, 0x63);
    /* S_len6: length 6 */
    ROW(OuterS, OuterS_ACN_Decode, OuterS_ACN_Encode, OuterS_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERS_X_S, 3, 0xB8, 0x71, 0x63);
    /* S_len7: length 7, outside the PER-visible range too */
    ROW(OuterS, OuterS_ACN_Decode, OuterS_ACN_Encode, OuterS_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERS_X_S, 3, 0xD8, 0x71, 0x63);
    /* N_12: valid */
    ROW(OuterN, OuterN_ACN_Decode, OuterN_ACN_Encode, OuterN_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 13, 0x12, 0x38);
    /* N_345: valid */
    ROW(OuterN, OuterN_ACN_Decode, OuterN_ACN_Encode, OuterN_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 17, 0x24, 0x56, 0x00);
    /* N_len4: length 4 (fits the stream slack) */
    ROW(OuterN, OuterN_ACN_Decode, OuterN_ACN_Encode, OuterN_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERN_X_N, 4, 0x32, 0x34, 0x58);
    /* N_len12: length 12 */
    ROW(OuterN, OuterN_ACN_Decode, OuterN_ACN_Encode, OuterN_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERN_X_N, 4, 0xB2, 0x34, 0x56);
    /* P_ok: valid */
    ROW(PDU, PDU_ACN_Decode, PDU_ACN_Encode, PDU_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 17, 0x12, 0x3D, 0x00);
    /* P_len4: length 4 */
    ROW(PDU, PDU_ACN_Decode, PDU_ACN_Encode, PDU_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERN_X_N, 4, 0x32, 0x34, 0x5C);
    /* P_len5: length 5 */
    ROW(PDU, PDU_ACN_Decode, PDU_ACN_Encode, PDU_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERN_X_N, 4, 0x42, 0x34, 0x56);
    /* F_ab: valid */
    ROW(OuterF, OuterF_ACN_Decode, OuterF_ACN_Encode, OuterF_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 18, 0x58, 0x71, 0x40);
    /* F_len0: length 0 < 1: the validation rejects it */
    ROW(OuterF, OuterF_ACN_Decode, OuterF_ACN_Encode, OuterF_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_OUTERF_X_2, 4, 0x10);
    /* F_len3: length 3 */
    ROW(OuterF, OuterF_ACN_Decode, OuterF_ACN_Encode, OuterF_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERF_X_S, 3, 0x78, 0x71, 0x63);
    /* F_len7: length 7 */
    ROW(OuterF, OuterF_ACN_Decode, OuterF_ACN_Encode, OuterF_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERF_X_S, 3, 0xF8, 0x71, 0x63);
    /* E_empty: valid */
    ROW(OuterE, OuterE_ACN_Decode, OuterE_ACN_Encode, OuterE_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 3, 0x20);
    /* E_c: valid */
    ROW(OuterE, OuterE_ACN_Decode, OuterE_ACN_Encode, OuterE_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 5, 0x68);
    /* E_len2: length 2 */
    ROW(OuterE, OuterE_ACN_Decode, OuterE_ACN_Encode, OuterE_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERE_X_S, 2, 0x98);
    /* E_len3: length 3 */
    ROW(OuterE, OuterE_ACN_Decode, OuterE_ACN_Encode, OuterE_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERE_X_S, 2, 0xDB);
    /* M_abc: valid */
    ROW(OuterM, OuterM_ACN_Decode, OuterM_ACN_Encode, OuterM_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 27, 0x16, 0x1C, 0x58, 0xE0);
    /* M_len1: length 1 < 3: the validation rejects it */
    ROW(OuterM, OuterM_ACN_Decode, OuterM_ACN_Encode, OuterM_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_OUTERM_X_2, 13, 0x06, 0x18);
    /* M_len5: length 5 */
    ROW(OuterM, OuterM_ACN_Decode, OuterM_ACN_Encode, OuterM_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERM_X_S, 5, 0x26, 0x1C, 0x58, 0xF2, 0x65);
    /* M_len20: length 20 */
    ROW(OuterM, OuterM_ACN_Decode, OuterM_ACN_Encode, OuterM_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERM_X_S, 5, 0x9E, 0x1C, 0x58, 0xF2, 0x65);
    /* M2_a: valid */
    ROW(OuterM2, OuterM2_ACN_Decode, OuterM2_ACN_Encode, OuterM2_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 11, 0x18, 0x60);
    /* M2_abcde: valid */
    ROW(OuterM2, OuterM2_ACN_Decode, OuterM2_ACN_Encode, OuterM2_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 39, 0x98, 0x71, 0x63, 0xC9, 0x96);
    /* M2_abc: length 3, inside the hull: the validation rejects it */
    ROW(OuterM2, OuterM2_ACN_Decode, OuterM2_ACN_Encode, OuterM2_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_OUTERM2_X_2, 25, 0x58, 0x71, 0x63, 0x80);
    /* M2_len6: length 6 */
    ROW(OuterM2, OuterM2_ACN_Decode, OuterM2_ACN_Encode, OuterM2_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERM2_X_S, 3, 0xB8, 0x71, 0x63, 0xC9, 0x94);
    /* M5_a: valid */
    ROW(OuterM5, OuterM5_ACN_Decode, OuterM5_ACN_Encode, OuterM5_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 11, 0x18, 0x60);
    /* M5_ab: valid */
    ROW(OuterM5, OuterM5_ACN_Decode, OuterM5_ACN_Encode, OuterM5_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 18, 0x38, 0x71, 0x40);
    /* G_ab: valid */
    ROW(OuterG, OuterG_ACN_Decode, OuterG_ACN_Encode, OuterG_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 23, 0x02, 0xC3, 0x8A);
    /* G_len1: length 1 < 2: the validation rejects it */
    ROW(OuterG, OuterG_ACN_Decode, OuterG_ACN_Encode, OuterG_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_OUTERG_X_2, 16, 0x01, 0xC3);
    /* G_len3: length 3 */
    ROW(OuterG, OuterG_ACN_Decode, OuterG_ACN_Encode, OuterG_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERG_X_S, 8, 0x03, 0xC3, 0x88);
    /* G_C1: a 16K fragment */
    ROW(OuterG, OuterG_ACN_Decode, OuterG_ACN_Encode, OuterG_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERG_X_S, 8, 0xC1, 0xC3, 0x88);
    /* G_8080: a two-octet length 128 */
    ROW(OuterG, OuterG_ACN_Decode, OuterG_ACN_Encode, OuterG_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERG_X_S, 16, 0x80, 0x80);
    /* G2_200: valid */
    ROW(OuterG2, OuterG2_ACN_Decode, OuterG2_ACN_Encode, OuterG2_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 1417, 0x80, 0xC8, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0x80);
    /* G2_199: length 199: the validation rejects it */
    ROW(OuterG2, OuterG2_ACN_Decode, OuterG2_ACN_Encode, OuterG2_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_OUTERG2_X_2, 1410, 0x80, 0xC7, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xC0);
    /* G2_201: length 201 */
    ROW(OuterG2, OuterG2_ACN_Decode, OuterG2_ACN_Encode, OuterG2_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERG2_X_S, 16, 0x80, 0xC9, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x70, 0xE1, 0xC3, 0x87, 0x0E, 0x1C, 0x38, 0x40);
    /* H_a: valid */
    ROW(OuterH, OuterH_ACN_Decode, OuterH_ACN_Encode, OuterH_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 12, 0x0C, 0x30);
    /* H_z: valid */
    ROW(OuterH, OuterH_ACN_Decode, OuterH_ACN_Encode, OuterH_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 2, 0xC0);
    /* H_len3: length 3 */
    ROW(OuterH, OuterH_ACN_Decode, OuterH_ACN_Encode, OuterH_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERH_A_S, 4, 0x2C, 0x38, 0xB1, 0x80);
    /* J_ok: valid */
    ROW(OuterJ, OuterJ_ACN_Decode, OuterJ_ACN_Encode, OuterJ_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 29, 0x18, 0x67, 0x0E, 0x20);
    /* J_2nd: second element, length 3 */
    ROW(OuterJ, OuterJ_ACN_Decode, OuterJ_ACN_Encode, OuterJ_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERJ_ELM_S, 14, 0x18, 0x6B, 0x0E, 0x2C, 0x60);
    /* J_1st: first element, length 6 */
    ROW(OuterJ, OuterJ_ACN_Decode, OuterJ_ACN_Encode, OuterJ_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERJ_ELM_S, 3, 0xB8, 0x71, 0x63, 0xC8);
    /* K_ok: valid */
    ROW(OuterK, OuterK_ACN_Decode, OuterK_ACN_Encode, OuterK_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 30, 0x46, 0x13, 0x87, 0x14);
    /* K_len3: element length 3 */
    ROW(OuterK, OuterK_ACN_Decode, OuterK_ACN_Encode, OuterK_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERK_K_ELM, 5, 0x16, 0x1C, 0x58, 0xC0);
    /* K_3rd: third element, length 6 */
    ROW(OuterK, OuterK_ACN_Decode, OuterK_ACN_Encode, OuterK_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, ERR_ACN_DECODE_OUTERK_K_ELM, 39, 0x8E, 0x1C, 0x47, 0x0E, 0x2B, 0x87, 0x10);
    /* Q_ok: valid */
    ROW(OuterQ, OuterQ_ACN_Decode, OuterQ_ACN_Encode, OuterQ_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 35, 0x00, 0x61, 0x9C, 0x38, 0xA0);
    /* Q_ok2: valid */
    ROW(OuterQ, OuterQ_ACN_Decode, OuterQ_ACN_Encode, OuterQ_REQUIRED_BYTES_FOR_ACN_ENCODING, TRUE, 0, 59, 0x28, 0xE1, 0xC5, 0x01, 0x18, 0x60, 0x40, 0x60);
    /* Q_w2len4: w2 length 4 */
    ROW(OuterQ, OuterQ_ACN_Decode, OuterQ_ACN_Encode, OuterQ_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, EXP_ERR_ACN_DECODE_OUTERQ_W2_P_S, 20, 0x00, 0x61, 0xBC, 0x38, 0xB1, 0xE4);
    /* Q_w1len5: w1 length 5 */
    ROW(OuterQ, OuterQ_ACN_Decode, OuterQ_ACN_Encode, OuterQ_REQUIRED_BYTES_FOR_ACN_ENCODING, FALSE, EXP_ERR_ACN_DECODE_OUTERQ_W1_P_S, 9, 0x92, 0x61, 0xC5, 0x8F, 0x26, 0x50);
    /* B4_1010: valid */
    ROW(OuterB4, OuterB4_Decode, OuterB4_Encode, OuterB4_REQUIRED_BYTES_FOR_ENCODING, TRUE, 0, 11, 0x12, 0xA0);
    /* B4_len0: length 0: the value set rejects it */
    ROW(OuterB4, OuterB4_Decode, OuterB4_Encode, OuterB4_REQUIRED_BYTES_FOR_ENCODING, FALSE, ERR_OUTERB4_X_2, 7, 0x02);
    /* B4_len3: length 3: the value set rejects it */
    ROW(OuterB4, OuterB4_Decode, OuterB4_Encode, OuterB4_REQUIRED_BYTES_FOR_ENCODING, FALSE, ERR_OUTERB4_X_2, 10, 0x0E, 0xC0);
    /* O4_0102: valid */
    ROW(OuterO4, OuterO4_Decode, OuterO4_Encode, OuterO4_REQUIRED_BYTES_FOR_ENCODING, TRUE, 0, 21, 0x20, 0x10, 0x28);
    /* O4_len0: length 0: the value set rejects it */
    ROW(OuterO4, OuterO4_Decode, OuterO4_Encode, OuterO4_REQUIRED_BYTES_FOR_ENCODING, FALSE, ERR_OUTERO4_X_2, 5, 0x08);
    return 0;
}
