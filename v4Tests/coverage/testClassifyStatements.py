#!/usr/bin/env python3
"""Unit tests for the exemption checks of classifyStatements.py (stdlib only).

Each exemption of Docs/statement-coverage-exemptions.md must accept its own
generated shape and reject every near miss, because a wrong acceptance would
count a reachable statement as proven unreachable."""
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
from classifyStatements import function_kind, proven_unreachable  # noqa: E402

UPER_ENUM = """\
{
    asn1SccSint enumIndex;
    ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &enumIndex, 0, 2);
    *pErrCode = ret ? 0 : ERR_UPER_DECODE_COLOR;
    if (ret) {
        switch(enumIndex)
        {
            case 0:
                (*(pVal)) = red;
                break;
            case 1:
                (*(pVal)) = green;
                break;
            case 2:
                (*(pVal)) = blue;
                break;
            default:                        /*COVERAGE_IGNORE*/
                *pErrCode = ERR_UPER_DECODE_COLOR;     /*COVERAGE_IGNORE*/
                ret = FALSE;                /*COVERAGE_IGNORE*/
        }
    }
}"""

# A CHOICE whose second alternative holds an ENUMERATED: the outer default arm
# must be checked against the outer switch, not the nearest (inner) one.
UPER_CHOICE = """\
ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &Shape_index_tmp, 0, 1);
*pErrCode = ret ? 0 : ERR_UPER_DECODE_SHAPE;
if (ret) {
    switch(Shape_index_tmp)
    {
    case 0:
        pVal->kind = circle_PRESENT;
        break;
    case 1:
        pVal->kind = color_PRESENT;
        {
            asn1SccSint enumIndex;
            ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &enumIndex, 0, 4);
            if (ret) {
                switch(enumIndex)
                {
                    case 0:
                        pVal->u.color = red;
                        break;
                    default:                        /*COVERAGE_IGNORE*/
                        ret = FALSE;                /*COVERAGE_IGNORE*/
                }
            }
        }
        break;
    default:                        /*COVERAGE_IGNORE*/
        *pErrCode = ERR_UPER_DECODE_SHAPE;     /*COVERAGE_IGNORE*/
        ret = FALSE;                /*COVERAGE_IGNORE*/
    }
}  /*COVERAGE_IGNORE*/"""

ACN_ENUM = """\
ret = BitStream_DecodeConstraintPosWholeNumber(pBitStrm, (&(intVal_pVal)), 0, 1);
*pErrCode = ret ? 0 : ERR_ACN_DECODE_MYPDU;
if (ret) {
    switch (intVal_pVal) {
        case 0:
            (*(pVal)) = MyPDU_alpha;
            break;
        case 1:
            (*(pVal)) = MyPDU_beta;
            break;
    default:                                    /*COVERAGE_IGNORE*/
        ret = FALSE;                            /*COVERAGE_IGNORE*/
        *pErrCode = ERR_ACN_DECODE_MYPDU;                 /*COVERAGE_IGNORE*/
    }
} /*COVERAGE_IGNORE*/"""

ADA_ACN_ENUM = """\
    result.ErrorCode := ERR_ACN_DECODE_MYPDU;
    adaasn1rtl.encoding.uper.UPER_Dec_ConstraintPosWholeNumber(bs, intVal_val, 0, 1, 1, result.Success);
    if result.Success then
        case intVal_val is
            when 0 => val := MyPDU_alpha;
            when 1 => val := MyPDU_beta;
        when others =>                                  -- COVERAGE_IGNORE
            val := MyPDU_alpha;                         -- COVERAGE_IGNORE
            result := adaasn1rtl.ASN1_RESULT'(Success => False, ErrorCode => ERR_ACN_DECODE_MYPDU);    -- COVERAGE_IGNORE
        end case;
    else
        val := MyPDU_alpha;                             -- COVERAGE_IGNORE
    end if;"""

CHAR_GUARD = """\
asn1SccSint charIndex = 0;
ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &charIndex, 0, 2);
*pErrCode = ret ? 0 : ERR_UPER_DECODE_SHAPE_LABEL;
if (ret && (charIndex < 0 || charIndex > 2)) { ret = FALSE; *pErrCode = ERR_UPER_DECODE_SHAPE_LABEL; }
pVal->u.label[i1] = ret ? allowedCharSet[charIndex] : '\\0' ;"""


def line_of(lines, text, occurrence=1):
    """1-based number of the `occurrence`-th line containing `text`."""
    seen = 0
    for number, line in enumerate(lines, 1):
        if text in line:
            seen += 1
            if seen == occurrence:
                return number
    raise AssertionError(f"{text!r} not found")


def check(source, text, occurrence=1, src=None, language="c"):
    lines = source.splitlines()
    first = line_of(lines, text, occurrence)
    return proven_unreachable(language, lines, first, src if src is not None else text)


class Cov001(unittest.TestCase):
    def test_uper_enum_default_is_proven(self):
        self.assertEqual(check(UPER_ENUM, "*pErrCode = ERR_UPER_DECODE_COLOR;"), "ASN1SCC-COV-001")
        self.assertEqual(check(UPER_ENUM, "ret = FALSE;"), "ASN1SCC-COV-001")

    def test_missing_case_is_not_proven(self):
        source = UPER_ENUM.replace("            case 2:\n", "")
        self.assertEqual(check(source, "ret = FALSE;"), "")

    def test_wider_decoded_range_is_not_proven(self):
        source = UPER_ENUM.replace("&enumIndex, 0, 2)", "&enumIndex, 0, 3)")
        self.assertEqual(check(source, "ret = FALSE;"), "")

    def test_other_decoder_is_not_proven(self):
        # A fixed-size ACN integer admits every value of its bit width.
        source = UPER_ENUM.replace("BitStream_DecodeConstraintWholeNumber(pBitStrm, &enumIndex, 0, 2)",
                                   "Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm, &enumIndex)")
        self.assertEqual(check(source, "ret = FALSE;"), "")

    def test_selector_assigned_after_decode_is_not_proven(self):
        source = UPER_ENUM.replace("    if (ret) {\n", "    enumIndex = enumIndex + 1;\n    if (ret) {\n", 1)
        self.assertEqual(check(source, "ret = FALSE;"), "")

    def test_other_selector_is_not_proven(self):
        source = UPER_ENUM.replace("switch(enumIndex)", "switch(otherIndex)")
        self.assertEqual(check(source, "ret = FALSE;"), "")

    def test_outer_choice_default_uses_outer_switch(self):
        self.assertEqual(check(UPER_CHOICE, "*pErrCode = ERR_UPER_DECODE_SHAPE;"), "ASN1SCC-COV-001")
        # The inner switch has only case 0 for 0..4: not proven.
        self.assertEqual(check(UPER_CHOICE, "ret = FALSE;", 1), "")

    def test_acn_pos_whole_number_is_proven(self):
        self.assertEqual(check(ACN_ENUM, "ret = FALSE;"), "ASN1SCC-COV-001")

    def test_sparse_acn_values_are_not_proven(self):
        source = ACN_ENUM.replace("case 1:", "case 5:").replace("0, 1);", "0, 5);")
        self.assertEqual(check(source, "ret = FALSE;"), "")

    def test_c_shape_is_not_classified_as_ada(self):
        self.assertEqual(check(UPER_ENUM, "ret = FALSE;", language="Ada"), "")


class Cov001Ada(unittest.TestCase):
    ARM = "val := MyPDU_alpha;                         -- COVERAGE_IGNORE"

    def test_dense_acn_enum_others_is_proven(self):
        self.assertEqual(check(ADA_ACN_ENUM, self.ARM, language="Ada"), "ASN1SCC-COV-001")
        self.assertEqual(check(ADA_ACN_ENUM, "result := adaasn1rtl", language="Ada"), "ASN1SCC-COV-001")

    def test_failure_branch_is_not_exempted(self):
        self.assertEqual(check(ADA_ACN_ENUM, "val := MyPDU_alpha;", 3, language="Ada"), "")

    def test_sparse_values_are_not_proven(self):
        source = ADA_ACN_ENUM.replace("when 1 =>", "when 5 =>").replace("0, 1, 1,", "0, 5, 3,")
        self.assertEqual(check(source, self.ARM, language="Ada"), "")

    def test_fixed_size_decode_with_dense_values_is_proven(self):
        # The ACN fixed-size decoders check minVal .. maxVal (postcondition).
        source = ADA_ACN_ENUM.replace(
            "adaasn1rtl.encoding.uper.UPER_Dec_ConstraintPosWholeNumber(bs, intVal_val, 0, 1, 1, result.Success)",
            "adaasn1rtl.encoding.acn.Acn_Dec_Int_PositiveInteger_ConstSize(bs, intVal_val, 0, 1, 10, result)")
        self.assertEqual(check(source, self.ARM, language="Ada"), "ASN1SCC-COV-001")
        source = source.replace("ConstSize(bs, intVal_val, 0, 1, 10, result)", "ConstSize_8(bs, intVal_val, 0, 1, result)")
        self.assertEqual(check(source, self.ARM, language="Ada"), "ASN1SCC-COV-001")

    def test_fixed_size_decode_with_wider_bounds_is_not_proven(self):
        source = ADA_ACN_ENUM.replace(
            "adaasn1rtl.encoding.uper.UPER_Dec_ConstraintPosWholeNumber(bs, intVal_val, 0, 1, 1, result.Success)",
            "adaasn1rtl.encoding.acn.Acn_Dec_Int_PositiveInteger_ConstSize(bs, intVal_val, 0, 1023, 10, result)")
        self.assertEqual(check(source, self.ARM, language="Ada"), "")

    def test_unbounded_decoder_is_not_proven(self):
        source = ADA_ACN_ENUM.replace(
            "adaasn1rtl.encoding.uper.UPER_Dec_ConstraintPosWholeNumber(bs, intVal_val, 0, 1, 1, result.Success)",
            "adaasn1rtl.encoding.acn.Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(bs, intVal_val, 0, 1, result)")
        self.assertEqual(check(source, self.ARM, language="Ada"), "")

    def test_case_outside_success_branch_is_not_proven(self):
        source = ADA_ACN_ENUM.replace("    if result.Success then\n", "    if True then\n")
        self.assertEqual(check(source, self.ARM, language="Ada"), "")


class Cov002(unittest.TestCase):
    def test_guard_body_is_proven(self):
        guard = "if (ret && (charIndex"
        self.assertEqual(check(CHAR_GUARD, guard, src="ret = FALSE;"), "ASN1SCC-COV-002")
        self.assertEqual(check(CHAR_GUARD, guard, src="*pErrCode = ERR_UPER_DECODE_SHAPE_LABEL;"),
                         "ASN1SCC-COV-002")

    def test_guard_condition_itself_is_not_exempted(self):
        self.assertEqual(check(CHAR_GUARD, "if (ret && (charIndex"), "")

    def test_mismatched_bound_is_not_proven(self):
        source = CHAR_GUARD.replace("charIndex > 2", "charIndex > 3")
        self.assertEqual(check(source, "if (ret && (charIndex", src="ret = FALSE;"), "")


class FunctionKind(unittest.TestCase):
    def test_ber_needs_the_encoding_suffix(self):
        self.assertEqual(function_kind("ASN1SCC_TC_number_of_cycles_Decode"), "uper-decode")
        self.assertEqual(function_kind("T_BER_Decode"), "ber-decode")
        self.assertEqual(function_kind("T_XER_Encode"), "xer-encode")


if __name__ == "__main__":
    unittest.main()
