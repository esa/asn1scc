#!/usr/bin/env python3
"""Unit tests for the labels and the decision exemption of classifyDecisions.py (stdlib only)."""
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
from classifyDecisions import construct, decision_proven, missing_outcome  # noqa: E402

CHAR_GUARD = """\
{
    ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &charIndex, 0, 94);
    *pErrCode = ret ? 0 : ERR_UPER_DECODE_TEXT;
    if (ret && (charIndex < 0 || charIndex > 94)) {
        *pErrCode = ERR_UPER_DECODE_TEXT;
        ret = FALSE;
    }
}"""


class Constructs(unittest.TestCase):
    def test_labels(self):
        cases = {
            "if (ret) {": "ret check",
            "} else if (ret) {": "ret check",
            "if ret.Success then": "ret check",
            "if ret then": "ret check",
            "*pErrCode = ret ? 0 :  ERR_MYPDU_INT1;": "errcode select",
            "ret.ErrorCode := (if ret.Success then 0 else ERR_PACKETDATALENGTH);": "errcode select",
            "if (ret && *pErrCode == 0) {": "ret and no error code",
            "return ret  && ASN1SCC_MyPDU_IsConstraintValid(pVal, pErrCode);": "validity call",
            "if ret and then val1.Exist.packetErrorControl = 1 then": "presence / CHOICE selection",
            "if (ret && (charIndex < 0 || charIndex > 94)) {": "charIndex guard",
            "ret = ((1UL <= (*(pVal))) && ((*(pVal)) <= 255UL));": "range check",
            "for(i1=0; i1 < (int)pVal->nCount; i1++)": "size / length",
        }
        for src, label in cases.items():
            self.assertEqual(construct(src), label, src)

    def test_missing_outcome(self):
        self.assertEqual(missing_outcome({"status": "!", "messages": ["outcome FALSE never exercised"]}), "FALSE")
        self.assertEqual(missing_outcome({"status": "!", "messages": ["outcome TRUE never exercised"]}), "TRUE")
        self.assertEqual(missing_outcome({"status": "-", "messages": []}), "never evaluated")


class DecisionExemption(unittest.TestCase):
    def setUp(self):
        self.lines = CHAR_GUARD.splitlines()

    def test_char_guard_true_outcome_is_proven(self):
        self.assertEqual(decision_proven("c", self.lines, 4, "TRUE"), "ASN1SCC-COV-002")

    def test_false_outcome_is_not_proven(self):
        self.assertEqual(decision_proven("c", self.lines, 4, "FALSE"), "")

    def test_bound_mismatch_is_not_proven(self):
        lines = CHAR_GUARD.replace("&charIndex, 0, 94", "&charIndex, 0, 95").splitlines()
        self.assertEqual(decision_proven("c", lines, 4, "TRUE"), "")

    def test_other_decision_is_not_proven(self):
        self.assertEqual(decision_proven("c", self.lines, 3, "TRUE"), "")

    def test_ada_is_never_proven_here(self):
        self.assertEqual(decision_proven("Ada", self.lines, 4, "TRUE"), "")


if __name__ == "__main__":
    unittest.main()
