#!/usr/bin/env python3
"""Unit tests for checkExemptionMarkers.py (stdlib only)."""
from pathlib import Path
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
from checkExemptionMarkers import check_generated, check_sources, documented_ids  # noqa: E402

DOC = "# Exemptions\n\n## ASN1SCC-COV-001: a\n\ntext\n\n## ASN1SCC-COV-002: b\n"
ON1 = '/* GNATCOV_EXEMPT_ON "ASN1SCC-COV-001: range, see Docs" */'
ON2 = '/* GNATCOV_EXEMPT_ON "ASN1SCC-COV-002: guard, see Docs" */'
OFF = "/* GNATCOV_EXEMPT_OFF */"


class Markers(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        (self.root / "Docs").mkdir()
        self.doc = self.root / "Docs" / "statement-coverage-exemptions.md"
        self.doc.write_text(DOC)
        (self.root / "StgC").mkdir()
        (self.root / "BackendAst").mkdir()

    def tearDown(self):
        self.temp.cleanup()

    def sources(self, stg, fs=""):
        (self.root / "StgC" / "uper_c.stg").write_text(stg)
        (self.root / "BackendAst" / "A.fs").write_text(fs)
        return check_sources(self.root, self.doc)

    def generated(self, name, text):
        out = self.root / "out"
        out.mkdir(exist_ok=True)
        (out / name).write_text(text)
        return check_generated([out], documented_ids(self.doc))

    def test_consistent_sources_pass(self):
        self.assertEqual(self.sources(f"x {ON1}\ny {ON2}\n", f'let t = "{ON1[22:-4]}"\n'), [])

    def test_undocumented_unused_and_divergent_ids(self):
        errors = self.sources(f'{ON1}\n/* GNATCOV_EXEMPT_ON "ASN1SCC-COV-009: new" */\n',
                              '"ASN1SCC-COV-001: other text"')
        self.assertTrue(any("ASN1SCC-COV-009 is used" in e for e in errors))
        self.assertTrue(any("ASN1SCC-COV-002 is documented but no" in e for e in errors))
        self.assertTrue(any("ASN1SCC-COV-001 has 2 different justifications" in e for e in errors))

    def test_balanced_regions_in_c_and_ada(self):
        errors, regions = self.generated("a.c", f"default: {ON1}\n  ret = FALSE; {OFF}\n{ON2} if (x) {{ }} {OFF}\n")
        self.assertEqual((errors, regions), ([], 2))
        errors, regions = self.generated(
            "a.adb", 'when others => pragma Annotate (Xcov, Exempt_On, "ASN1SCC-COV-001: r");\n'
                     "   x := 1; pragma Annotate (Xcov, Exempt_Off);\n")
        self.assertEqual((errors, regions), ([], 3))

    def test_unbalanced_nested_and_unknown_regions(self):
        errors, _ = self.generated("b.c", f"{ON1}\n{ON1}\n{OFF}\n{OFF}\n"
                                          '/* GNATCOV_EXEMPT_ON "ASN1SCC-COV-007: x" */\n')
        self.assertTrue(any("opened inside" in e for e in errors))
        self.assertTrue(any("closed but not opened" in e for e in errors))
        self.assertTrue(any("ASN1SCC-COV-007 has no entry" in e for e in errors))
        self.assertTrue(any("never closed" in e for e in errors))


if __name__ == "__main__":
    unittest.main()
