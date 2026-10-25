#!/usr/bin/env python3
"""Unit tests for the exemption checks of classifyStatements.py (stdlib only).

Each exemption of Docs/statement-coverage-exemptions.md must accept its own
generated shape and reject every near miss, because a wrong acceptance would
count a reachable statement as proven unreachable."""
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
from classifyStatements import exemption_mismatches, function_kind, proven_unreachable  # noqa: E402

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


class Mismatch(unittest.TestCase):
    def test_region_and_classifier_must_agree(self):
        rows = [{"exempted": True, "proven": "ASN1SCC-COV-001"}, {"exempted": False, "proven": ""},
                {"exempted": None, "proven": "ASN1SCC-COV-001"},  # run measured before step 07
                {"exempted": True, "proven": ""}, {"exempted": False, "proven": "ASN1SCC-COV-003"}]
        self.assertEqual(exemption_mismatches(rows), rows[3:])


class Cov001(unittest.TestCase):
    def test_uper_enum_default_is_proven(self):
        self.assertEqual(check(UPER_ENUM, "*pErrCode = ERR_UPER_DECODE_COLOR;"), "ASN1SCC-COV-001")
        self.assertEqual(check(UPER_ENUM, "ret = FALSE;"), "ASN1SCC-COV-001")

    def test_marked_shape_with_slim_decoder_is_proven(self):
        # Exemption markers on the arm lines (step 07) and the UInt8 / Int16 ... wrappers.
        source = (UPER_ENUM.replace("BitStream_DecodeConstraintWholeNumber(", "BitStream_DecodeConstraintWholeNumberInt8(")
                  .replace("default:                        /*COVERAGE_IGNORE*/",
                           'default: /*COVERAGE_IGNORE*/ /* GNATCOV_EXEMPT_ON "ASN1SCC-COV-001: x" */')
                  .replace("ret = FALSE;                /*COVERAGE_IGNORE*/",
                           "ret = FALSE; /*COVERAGE_IGNORE*/ /* GNATCOV_EXEMPT_OFF */"))
        self.assertEqual(check(source, "ret = FALSE;"), "ASN1SCC-COV-001")

    def test_other_suffix_is_not_proven(self):
        source = UPER_ENUM.replace("BitStream_DecodeConstraintWholeNumber(", "BitStream_DecodeConstraintWholeNumberInt64(")
        self.assertEqual(check(source, "ret = FALSE;"), "")

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


C_FIXED_SIZE_ENUM = """\
ret = Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, (&(intVal_pVal)), 1);
*pErrCode = ret ? 0 : ERR_ACN_DECODE_DELIVERYCODE;
if (ret) {
    switch (intVal_pVal) {
        case 0:
            (*(pVal)) = data_complete;
            break;
        case 1:
            (*(pVal)) = data_incomplete;
            break;
    default:                                    /*COVERAGE_IGNORE*/
        ret = FALSE;                            /*COVERAGE_IGNORE*/
        *pErrCode = ERR_ACN_DECODE_DELIVERYCODE;                 /*COVERAGE_IGNORE*/
    }
} /*COVERAGE_IGNORE*/"""


class Cov003(unittest.TestCase):
    def test_every_value_of_the_width_has_a_case(self):
        self.assertEqual(check(C_FIXED_SIZE_ENUM, "ret = FALSE;"), "ASN1SCC-COV-003")

    def test_wider_encoding_is_not_proven(self):
        source = C_FIXED_SIZE_ENUM.replace("(&(intVal_pVal)), 1);", "(&(intVal_pVal)), 2);")
        self.assertEqual(check(source, "ret = FALSE;"), "")

    def test_eight_bit_variant_needs_256_cases(self):
        source = C_FIXED_SIZE_ENUM.replace("ConstSize(pBitStrm, (&(intVal_pVal)), 1)", "ConstSize_8(pBitStrm, (&(intVal_pVal)))")
        self.assertEqual(check(source, "ret = FALSE;"), "")

    def test_signed_decoder_is_not_proven(self):
        source = C_FIXED_SIZE_ENUM.replace("Acn_Dec_Int_PositiveInteger_ConstSize", "Acn_Dec_Int_TwosComplement_ConstSize")
        self.assertEqual(check(source, "ret = FALSE;"), "")


class Cov001Ada(unittest.TestCase):
    ARM = "val := MyPDU_alpha;                         -- COVERAGE_IGNORE"

    def test_dense_acn_enum_others_is_proven(self):
        self.assertEqual(check(ADA_ACN_ENUM, self.ARM, language="Ada"), "ASN1SCC-COV-001")
        self.assertEqual(check(ADA_ACN_ENUM, "result := adaasn1rtl", language="Ada"), "ASN1SCC-COV-001")

    def test_marked_shape_with_slim_decoder_is_proven(self):
        source = (ADA_ACN_ENUM.replace("UPER_Dec_ConstraintPosWholeNumber(", "UPER_Dec_ConstraintPosWholeNumberUInt8(")
                  .replace("when others =>  ", 'when others => pragma Annotate (Xcov, Exempt_On, "ASN1SCC-COV-001: x");')
                  .replace("ErrorCode => ERR_ACN_DECODE_MYPDU);", "ErrorCode => ERR_ACN_DECODE_MYPDU); pragma Annotate (Xcov, Exempt_Off);"))
        self.assertIn("Exempt_On", source)
        self.assertEqual(check(source, self.ARM, language="Ada"), "ASN1SCC-COV-001")
        self.assertEqual(check(source, "result := adaasn1rtl", language="Ada"), "ASN1SCC-COV-001")

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


# Legacy ACN encoder: the update of the determinant switches on the CHOICE selector
# (failing default), its encoding maps the local, and the CHOICE switch follows.
LEGACY_CHOICE_DET = """\
flag Msg_ACN_Encode(const Msg* pVal, BitStream* pBitStrm, int* pErrCode, flag bCheckConstraints)
{
    flag ret = TRUE;

	Kind Msg_kind;
	asn1SccUint intVal_Msg_kind;
    *pErrCode = 0;
	ret = bCheckConstraints ? Msg_IsConstraintValid(pVal, pErrCode) : TRUE ;
	if (ret) {
	    switch (pVal->chr.kind) {
	        case Ch_a_PRESENT:
	            Msg_kind = Kind_a;
	            break;
	        case Ch_b_PRESENT:
	            Msg_kind = Kind_b;
	            break;
	        default:
	            ret = FALSE;                            /*COVERAGE_IGNORE*/
	            *pErrCode = ERR_INVALID_ENUM_VALUE;     /*COVERAGE_IGNORE*/
	    }
	    if (ret) {
	        /*Encode Msg_kind */
	        switch(Msg_kind) {
	            case Kind_a:
	                intVal_Msg_kind = 0UL;
	                break;
	            case Kind_b:
	                intVal_Msg_kind = 1UL;
	                break;
	            default:                                    /*COVERAGE_IGNORE*/
	                ret = FALSE;                            /*COVERAGE_IGNORE*/
	                *pErrCode = ERR_ACN_ENCODE_MSG_KIND;                 /*COVERAGE_IGNORE*/
	        }
	        if (ret) {
	        	Acn_Enc_Int_PositiveInteger_ConstSize_8(pBitStrm, intVal_Msg_kind);
	        }
	    }   /*COVERAGE_IGNORE*/
	    if (ret) {
	        /*Encode chr */
	        switch(pVal->chr.kind)
	        {
	            case Ch_a_PRESENT:
	            	BitStream_EncodeConstraintPosWholeNumber(pBitStrm, pVal->chr.u.a, 0, 10);
	            	break;
	            case Ch_b_PRESENT:
	            	BitStream_AppendBit(pBitStrm,pVal->chr.u.b);
	            	break;
	        default:
	            *pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/
	            ret = FALSE;                    /*COVERAGE_IGNORE*/
	        }
	    }   /*COVERAGE_IGNORE*/
    } /*COVERAGE_IGNORE*/
    return ret;
}"""

# acn-v2 specialised encoder: the CHOICE switch, then the PatchDet value switch.
V2_PATCHDET = """\
flag Msg_chr_ACN_Encode(const Ch* pVal, BitStream* pBitStrm, int* pErrCode, flag bCheckConstraints, AcnInsertedFieldRef* Msg_chr_kind)
{
    flag ret = TRUE;

    switch(pVal->kind)
    {
        case Ch_a_PRESENT:
        	BitStream_EncodeConstraintPosWholeNumber(pBitStrm, pVal->u.a, 0, 10);
        	break;
        case Ch_b_PRESENT:
        	break;
    default:
        *pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/
        ret = FALSE;                    /*COVERAGE_IGNORE*/
    }
    if (ret) {
        {
        asn1SccUint patchDetVal;
        switch(pVal->kind) {
            case Ch_a_PRESENT: patchDetVal = 0; break;
            case Ch_b_PRESENT: patchDetVal = 1; break;
            default:                                    /*COVERAGE_IGNORE*/
                *pErrCode = ERR_INVALID_CHOICE_ALTERNATIVE; /*COVERAGE_IGNORE*/
                return FALSE;                           /*COVERAGE_IGNORE*/
        }
        ret = Acn_PatchDet_U8((asn1SccUint)patchDetVal, pBitStrm, Msg_chr_kind, pErrCode);
        }
    }   /*COVERAGE_IGNORE*/
    return ret;
}"""

# Legacy: the CHOICE sits in an alternative; the update is guarded by `if (E == L)`,
# the CHOICE by `case L:` of `switch (E)`.
LEGACY_IN_ALTERNATIVE = """\
flag O3_ACN_Encode(const O3* pVal, BitStream* pBitStrm, int* pErrCode, flag bCheckConstraints)
{
    flag ret = TRUE;
	K2 O3_k;
	flag O3_k_is_initialized=FALSE;
	asn1SccUint intVal_O3_k;
	ret = bCheckConstraints ? O3_IsConstraintValid(pVal, pErrCode) : TRUE ;
	if (ret) {
	    if (pVal->i3.kind == p_PRESENT) {
	        switch (pVal->i3.u.p.y.kind) {
	            case p_y_a_PRESENT:
	                O3_k = a;
	                O3_k_is_initialized = TRUE;
	                break;
	            case p_y_b_PRESENT:
	                O3_k = b;
	                O3_k_is_initialized = TRUE;
	                break;
	            default:
	                ret = FALSE;                            /*COVERAGE_IGNORE*/
	                *pErrCode = ERR_INVALID_ENUM_VALUE;     /*COVERAGE_IGNORE*/
	        }
	    }
	    if (ret) {
	        /*Encode O3_k */
	        if (O3_k_is_initialized) {
	            ret = TRUE;
	            switch(O3_k) {
	                case a:
	                    intVal_O3_k = 0UL;
	                    break;
	                case b:
	                    intVal_O3_k = 1UL;
	                    break;
	                default:                                    /*COVERAGE_IGNORE*/
	                    ret = FALSE;                            /*COVERAGE_IGNORE*/
	                    *pErrCode = ERR_ACN_ENCODE_O3_K;                 /*COVERAGE_IGNORE*/
	            }
	        } else {
	            *pErrCode = ERR_ACN_ENCODE_O3_K_UNINITIALIZED;         /*COVERAGE_IGNORE*/
	            ret = FALSE;                    /*COVERAGE_IGNORE*/
	        }
	    }   /*COVERAGE_IGNORE*/
	    if (ret) {
	        switch(pVal->i3.kind)
	        {
	        case p_PRESENT:
	        	switch(pVal->i3.u.p.y.kind)
	        	{
	        	    case p_y_a_PRESENT:
	        	    	break;
	        	    case p_y_b_PRESENT:
	        	    	break;
	        	default:
	        	    *pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/
	        	    ret = FALSE;                    /*COVERAGE_IGNORE*/
	        	}
	        	break;
	        case q_PRESENT:
	        	break;
	        default: /*COVERAGE_IGNORE*/
	            *pErrCode = ERR_ACN_ENCODE_O3_I3;         /*COVERAGE_IGNORE*/
	            ret = FALSE;                    /*COVERAGE_IGNORE*/
	        } /*COVERAGE_IGNORE*/
	    }   /*COVERAGE_IGNORE*/
    } /*COVERAGE_IGNORE*/
    return ret;
}"""

# Legacy: one determinant, two CHOICEs (MultiAcnUpdate).
LEGACY_MULTI = """\
flag Direct_ACN_Encode(const Direct* pVal, BitStream* pBitStrm, int* pErrCode, flag bCheckConstraints)
{
    flag ret = TRUE;

	SelKind Direct_kind;
	asn1SccUint intVal_Direct_kind;
	ret = bCheckConstraints ? Direct_IsConstraintValid(pVal, pErrCode) : TRUE ;
	if (ret) {
	    {
	        SelKind Direct_kind00;
	        flag Direct_kind00_is_initialized=FALSE;
	        SelKind Direct_kind01;
	        flag Direct_kind01_is_initialized=FALSE;

	        switch (pVal->status.kind) {
	            case status_alpha_PRESENT:
	                Direct_kind00 = SelKind_alpha;
	                Direct_kind00_is_initialized = TRUE;
	                break;
	            default:
	                ret = FALSE;                            /*COVERAGE_IGNORE*/
	                *pErrCode = ERR_INVALID_ENUM_VALUE;     /*COVERAGE_IGNORE*/
	        }
	        switch (pVal->mask.kind) {
	            case mask_alpha_PRESENT:
	                Direct_kind01 = SelKind_alpha;
	                Direct_kind01_is_initialized = TRUE;
	                break;
	            default:
	                ret = FALSE;                            /*COVERAGE_IGNORE*/
	                *pErrCode = ERR_INVALID_ENUM_VALUE;     /*COVERAGE_IGNORE*/
	        }

	        if (ret) {
	            if (Direct_kind00_is_initialized) { /*COVERAGE_IGNORE*/
	                Direct_kind = Direct_kind00; /*COVERAGE_IGNORE*/
	            } /*COVERAGE_IGNORE*/ else if (Direct_kind01_is_initialized) { /*COVERAGE_IGNORE*/
	                Direct_kind = Direct_kind01; /*COVERAGE_IGNORE*/
	            } /*COVERAGE_IGNORE*/ else {
	                ret = FALSE; /*COVERAGE_IGNORE*/
	            }
	            if (ret) {
	                ret = (((Direct_kind00_is_initialized && Direct_kind == Direct_kind00) || !Direct_kind00_is_initialized));
	            }
	            if (!ret) {
	                *pErrCode = ERR_ACN_ENCODE_UPDATE_DIRECT_KIND; /*COVERAGE_IGNORE*/
	            }
	        }
	    }
	    if (ret) {
	        /*Encode Direct_kind */
	        switch(Direct_kind) {
	            case SelKind_alpha:
	                intVal_Direct_kind = 0UL;
	                break;
	            default:                                    /*COVERAGE_IGNORE*/
	                ret = FALSE;                            /*COVERAGE_IGNORE*/
	                *pErrCode = ERR_ACN_ENCODE_DIRECT_KIND;                 /*COVERAGE_IGNORE*/
	        }
	        if (ret) {
	            switch(pVal->mask.kind)
	            {
	                case mask_alpha_PRESENT:
	                	break;
	            default:
	                *pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/
	                ret = FALSE;                    /*COVERAGE_IGNORE*/
	            }
	        }   /*COVERAGE_IGNORE*/
	    }   /*COVERAGE_IGNORE*/
    } /*COVERAGE_IGNORE*/
    return ret;
}"""

CHOICE_ARM = "*pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/"


class Cov004(unittest.TestCase):
    def test_choice_switch_after_the_update_is_proven(self):
        self.assertEqual(check(LEGACY_CHOICE_DET, CHOICE_ARM), "ASN1SCC-COV-004")
        self.assertEqual(check(LEGACY_CHOICE_DET, "ret = FALSE;                    /*COVERAGE_IGNORE*/"),
                         "ASN1SCC-COV-004")

    def test_the_update_switch_itself_is_not_exempted(self):
        self.assertEqual(check(LEGACY_CHOICE_DET, "ret = FALSE;", 1), "")

    def test_patchdet_switch_after_the_choice_is_proven(self):
        self.assertEqual(check(V2_PATCHDET, "*pErrCode = ERR_INVALID_CHOICE_ALTERNATIVE;"), "ASN1SCC-COV-004")
        self.assertEqual(check(V2_PATCHDET, "return FALSE;"), "ASN1SCC-COV-004")

    def test_choice_in_an_alternative_is_proven(self):
        self.assertEqual(check(LEGACY_IN_ALTERNATIVE, CHOICE_ARM), "ASN1SCC-COV-004")

    def test_multi_determinant_update_is_proven(self):
        self.assertEqual(check(LEGACY_MULTI, CHOICE_ARM), "ASN1SCC-COV-004")

    def test_earlier_switch_under_another_condition_is_not_proven(self):
        # acn-v2, OPTIONAL CHOICE: the CHOICE switch runs only when present, the PatchDet switch always
        source = V2_PATCHDET.replace("    switch(pVal->kind)\n    {", "    if (pVal->exist.y) {\n    switch(pVal->kind)\n    {", 1)
        source = source.replace("        ret = FALSE;                    /*COVERAGE_IGNORE*/\n    }\n",
                                "        ret = FALSE;                    /*COVERAGE_IGNORE*/\n    }\n    }\n", 1)
        self.assertEqual(check(source, "return FALSE;"), "")
        source = LEGACY_IN_ALTERNATIVE.replace("if (pVal->i3.kind == p_PRESENT) {", "if (pVal->i3.kind == q_PRESENT) {")
        self.assertEqual(check(source, CHOICE_ARM), "")

    def test_update_under_the_exist_flag_of_an_always_present_choice_is_not_proven(self):
        # ALWAYS PRESENT: the update checks the exist flag, the CHOICE encoding does not
        source = LEGACY_CHOICE_DET.replace("\t    switch (pVal->chr.kind) {", "\t    if (pVal->exist.chr) {\n\t    switch (pVal->chr.kind) {", 1)
        source = source.replace("\t            *pErrCode = ERR_INVALID_ENUM_VALUE;     /*COVERAGE_IGNORE*/\n\t    }\n",
                                "\t            *pErrCode = ERR_INVALID_ENUM_VALUE;     /*COVERAGE_IGNORE*/\n\t    }\n\t    }\n", 1)
        self.assertIn("if (pVal->exist.chr) {", source)
        self.assertEqual(check(source, CHOICE_ARM), "")

    def test_label_missing_in_the_later_switch_is_not_proven(self):
        source = LEGACY_CHOICE_DET.replace(
            "\t            case Ch_b_PRESENT:\n\t            \tBitStream_AppendBit(pBitStrm,pVal->chr.u.b);\n\t            \tbreak;\n", "")
        self.assertEqual(check(source, CHOICE_ARM), "")

    def test_earlier_default_that_does_not_fail_is_not_proven(self):
        source = LEGACY_CHOICE_DET.replace("ret = FALSE;                            /*COVERAGE_IGNORE*/\n\t            *pErrCode = ERR_INVALID_ENUM_VALUE;",
                                           "Msg_kind = Kind_a;\n\t            *pErrCode = ERR_INVALID_ENUM_VALUE;", 1)
        self.assertEqual(check(source, CHOICE_ARM), "")

    def test_ret_set_again_before_the_check_is_not_proven(self):
        source = LEGACY_CHOICE_DET.replace("\t    if (ret) {\n\t        /*Encode chr */", "\t    ret = TRUE;\n\t    if (ret) {\n\t        /*Encode chr */")
        self.assertEqual(check(source, CHOICE_ARM), "")

    def test_later_switch_not_under_a_check_is_not_proven(self):
        source = V2_PATCHDET.replace("    if (ret) {\n        {\n", "    {\n        {\n")
        self.assertEqual(check(source, "return FALSE;"), "")

    def test_non_const_value_is_not_proven(self):
        source = LEGACY_CHOICE_DET.replace("(const Msg* pVal", "(Msg* pVal")
        self.assertEqual(check(source, CHOICE_ARM), "")


DET_ARM = "*pErrCode = ERR_ACN_ENCODE_MSG_KIND;"


class Cov005(unittest.TestCase):
    def test_local_set_by_the_update_on_every_path_is_proven(self):
        self.assertEqual(check(LEGACY_CHOICE_DET, DET_ARM), "ASN1SCC-COV-005")

    def test_local_read_under_its_flag_is_proven(self):
        self.assertEqual(check(LEGACY_IN_ALTERNATIVE, "*pErrCode = ERR_ACN_ENCODE_O3_K;"), "ASN1SCC-COV-005")

    def test_local_of_a_multi_determinant_update_is_proven(self):
        self.assertEqual(check(LEGACY_MULTI, "*pErrCode = ERR_ACN_ENCODE_DIRECT_KIND;"), "ASN1SCC-COV-005")

    def test_value_from_the_input_is_not_proven(self):
        source = LEGACY_CHOICE_DET.replace("Msg_kind = Kind_b;", "Msg_kind = pVal->k;")
        self.assertEqual(check(source, DET_ARM), "")

    def test_item_without_a_label_is_not_proven(self):
        source = LEGACY_CHOICE_DET.replace("\t            case Kind_b:\n\t                intVal_Msg_kind = 1UL;\n\t                break;\n", "")
        self.assertEqual(check(source, DET_ARM), "")

    def test_update_that_may_not_assign_is_not_proven(self):
        source = LEGACY_CHOICE_DET.replace("\t            Msg_kind = Kind_b;\n", "")
        self.assertEqual(check(source, DET_ARM), "")

    def test_conditional_assignment_in_the_update_is_not_proven(self):
        source = LEGACY_CHOICE_DET.replace("\t            Msg_kind = Kind_b;\n",
                                           "\t            if (pVal->x) {\n\t                Msg_kind = Kind_b;\n\t            }\n")
        self.assertEqual(check(source, DET_ARM), "")

    def test_flag_set_without_the_value_is_not_proven(self):
        source = LEGACY_IN_ALTERNATIVE.replace("\t                O3_k = b;\n", "")
        self.assertEqual(check(source, "*pErrCode = ERR_ACN_ENCODE_O3_K;"), "")

    def test_multi_source_read_without_its_flag_is_not_proven(self):
        source = LEGACY_MULTI.replace("else if (Direct_kind01_is_initialized) {", "else if (TRUE) {")
        self.assertEqual(check(source, "*pErrCode = ERR_ACN_ENCODE_DIRECT_KIND;"), "")

    def test_chain_without_a_failing_else_is_not_proven(self):
        source = LEGACY_MULTI.replace("\t                ret = FALSE; /*COVERAGE_IGNORE*/\n", "")
        self.assertEqual(check(source, "*pErrCode = ERR_ACN_ENCODE_DIRECT_KIND;"), "")


class FunctionKind(unittest.TestCase):
    def test_ber_needs_the_encoding_suffix(self):
        self.assertEqual(function_kind("ASN1SCC_TC_number_of_cycles_Decode"), "uper-decode")
        self.assertEqual(function_kind("T_BER_Decode"), "ber-decode")
        self.assertEqual(function_kind("T_XER_Encode"), "xer-encode")


if __name__ == "__main__":
    unittest.main()
