"""Failure-oriented tests; executable with the Python standard library."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

SCRIPT = Path(__file__).with_name("coverageBaseline.py")
if not SCRIPT.exists():
    SCRIPT = Path(__file__).resolve().parent.parent / "scripts/coverageBaseline.py"
SPEC = importlib.util.spec_from_file_location("collector", SCRIPT)
c = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(c)


class CollectorTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)

    def source_stats(self, marker=False):
        source = self.root / "sample1.c"
        source.write_text("ret = ret ? 0 : ERR; " + ("/*COVERAGE_IGNORE*/" if marker else "") + "\n")
        entry = {"functions": [{"name": "P_ACN_Decode", "blocks": 3, "blocks_executed": 2}],
                 "lines": [{"line_number": 1, "count": 2, "function_name": "P_ACN_Decode",
                            "unexecuted_block": True,
                            "branches": [{"count": 2, "fallthrough": True, "throw": False},
                                         {"count": 0, "fallthrough": False, "throw": False}]}]}
        return c.analyse_file(entry, source, ["unit", "c"], "c")

    def test_partial_line_is_not_full_statement_coverage(self):
        stat = self.source_stats()
        self.assertEqual((stat["lines_hit"], stat["lines_total"]), (1, 1))
        self.assertEqual(stat["partially_executed_lines"], 1)
        self.assertEqual((stat["branches_taken"], stat["branches_total"]), (1, 2))
        self.assertEqual((stat["blocks_hit"], stat["blocks_total"]), (2, 3))

    def test_ignore_marker_does_not_exclude_or_prove_branch(self):
        stat = self.source_stats(marker=True)
        self.assertEqual(stat["legacy_excluded_lines"], 1)
        self.assertEqual(stat["branches_total"], 2)
        self.assertEqual(stat["branch_exclusions"], [])
        arm = stat["branches"][1]
        self.assertEqual(arm["pattern_category"], "B6")
        self.assertEqual(arm["reachability"], "unresolved")
        self.assertIsNone(arm["proof"])

    def test_branch_ids_reproducible_but_source_scoped(self):
        first = self.source_stats()
        same = self.source_stats()
        changed = self.source_stats(marker=True)
        self.assertEqual(first["branches"][0]["id"], same["branches"][0]["id"])
        self.assertNotEqual(first["branches"][0]["id"], changed["branches"][0]["id"])
        self.assertNotEqual(first["branches"][0]["id"], first["branches"][1]["id"])

    def test_legacy_gate_keeps_real_misses(self):
        text = "#####:1:do_work();\n#####:2:}\n#####:3:bad(); /*COVERAGE_IGNORE*/\n"
        self.assertEqual(c.text_gcov_misses(text, "c"), [1])

    def test_hashes_in_comments_are_only_literal_legacy_misses(self):
        text = "-:1:/* #### decorative comment */\n#####:2:do_work();\n"
        self.assertEqual(c.text_gcov_misses(text, "c"), [2])
        self.assertEqual(c.text_gcov_misses(text, "c", literal_legacy=True), [1, 2])

    def test_inventory_includes_exempt_and_accounts_for_no_atc(self):
        (self.root / "normal.asn1").write_text("--NOCOVERAGE\n--TCLS A[]\n-- TCLS disabled[]\n--TCLS B[]\n")
        (self.root / "compile_only.asn1").write_text("-- NO_AUTOMATIC_TEST_CASES\n--TCLS C[]\n")
        units = c.enumerate_units(self.root)
        args = c.arguments(["--test-root", str(self.root)])
        chosen = c.select_units(units, args, None)
        self.assertEqual(len(units), 3)
        self.assertEqual(len(chosen), 2)
        self.assertTrue(all(u["nocoverage"] for u in chosen))
        self.assertEqual(units[0]["selection"], "NO_AUTOMATIC_TEST_CASES")

    def test_missing_acn_recorded_and_outside_corpus_rejected(self):
        (self.root / "a.asn1").write_text("--TCFS missing.acn\n--TCFS ../outside.acn\n")
        units = c.enumerate_units(self.root)
        self.assertTrue(all("input_error" in u for u in units))

    def test_failed_runtime_never_contributes_success_metrics(self):
        stat = self.source_stats()
        stat["component"] = "codec"
        failed = {"status": "run_error", "nocoverage": False, "files": {"sample1.c": stat}}
        self.assertEqual(c.aggregate([failed])["components"], {})

    def test_process_failure_keeps_code_and_logs(self):
        logs = self.root / "logs"
        logs.mkdir()
        steps = []
        with self.assertRaises(c.StageError):
            c.run_step([sys.executable, "-c", "import sys; print('failure', file=sys.stderr); sys.exit(7)"],
                       self.root, logs, "run", 10, steps)
        self.assertEqual(steps[0]["returncode"], 7)
        self.assertIn("failure", (logs / "run.stderr").read_text())

    def test_timeout_is_failure(self):
        logs = self.root / "logs"
        logs.mkdir()
        with self.assertRaises(c.StageError) as raised:
            c.run_step([sys.executable, "-c", "import time; time.sleep(60)"],
                       self.root, logs, "run", 1, [])
        self.assertTrue(raised.exception.timeout)

    def test_success_exit_with_error_diagnostics_is_failure(self):
        logs = self.root / "logs"
        logs.mkdir()
        with self.assertRaises(c.StageError):
            c.run_step([sys.executable, "-c", "import sys; print('profile corrupt', file=sys.stderr)"],
                       self.root, logs, "gcov", 10, [], strict_stderr=True)

    def test_main_fails_and_writes_report_on_runtime_failure(self):
        corpus, output = self.root / "corpus", self.root / "out"
        corpus.mkdir()
        (corpus / "a.asn1").write_text("--TCLS A[]\n")
        failed = {"unit": "a.asn1#1", "status": "run_error", "nocoverage": False,
                  "files": {}, "detail": "test program failed"}
        with mock.patch.object(c, "tool_version", return_value="test"), \
             mock.patch.object(c, "tree_manifest", return_value={}), \
             mock.patch.object(c, "measure_unit", return_value=failed), \
             contextlib.redirect_stdout(io.StringIO()):
            result = c.main(["--compiler", sys.executable, "--test-root", str(corpus),
                             "--outdir", str(output)])
        self.assertEqual(result, 1)
        summary = json.loads(next(output.glob("*/summary.json")).read_text())
        self.assertEqual(summary["status"], "failed")
        self.assertEqual(summary["unit_statuses"], {"run_error": 1})
        self.assertEqual(summary["failures"], 1)

    def test_missing_profile_is_not_zero_percent_success(self):
        corpus = self.root / "corpus"
        corpus.mkdir()
        (corpus / "a.asn1").write_text("--TCLS A[]\n")
        args = c.arguments(["--test-root", str(corpus), "--compiler", sys.executable])
        unit = c.enumerate_units(corpus)[0]
        with mock.patch.object(c, "run_step", return_value=""):
            result = c.measure_unit(unit, args, self.root / "run")
        self.assertEqual(result["status"], "gcov_error")

    def test_empty_translation_unit_has_no_missing_coverage(self):
        diagnostic = "empty.gcno:no functions found\nempty.gcda:cannot open data file, assuming not executed\n"
        self.assertTrue(c.validate_profile({"files": []}, False, diagnostic))

    def test_missing_runtime_data_for_real_code_is_failure(self):
        payload = {"files": [{"lines": [{"line_number": 1, "count": 0}],
                              "functions": []}]}
        with self.assertRaises(c.StageError):
            c.validate_profile(payload, False, "missing.gcda:cannot open data file, assuming not executed\n")

    def test_empty_profile_does_not_hide_unrelated_gcov_error(self):
        with self.assertRaises(c.StageError):
            c.validate_profile({"files": []}, False, "version mismatch")

    def test_only_empty_source_mappings_may_omit_text_report(self):
        self.assertEqual(c.source_text_report({"lines": [], "functions": []}, {}, "types.ads"), "")
        with self.assertRaises(c.StageError):
            c.source_text_report({"lines": [{"count": 1}], "functions": []}, {}, "types.ads")

    def test_reference_cannot_match_missing_or_changed_input(self):
        reference = {"units": {"a#1": {"input_sha256": "old"}}}
        self.assertFalse(c.compare_reference([], reference)["matched"])
        rec = {"unit": "a#1", "status": "ok", "input_sha256": "new"}
        result = c.compare_reference([rec], reference)
        self.assertEqual(result["differences"][0]["reason"], "input_changed")

    def test_incomplete_comparison_rejected(self):
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            c.arguments(["--cohort", "historical", "--compare-baseline", "--limit", "1"])

    def test_inventory_cannot_claim_to_check_baseline(self):
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            c.arguments(["--inventory-only", "--compare-baseline"])


if __name__ == "__main__":
    unittest.main(verbosity=2)
