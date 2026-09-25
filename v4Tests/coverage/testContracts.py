"""Check coverage integration against the actual runner and Ada build template."""
import contextlib
import importlib.util
import io
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

from testCollector import c

ROOT = Path(__file__).resolve().parents[2]
CONTRACTS = Path(__file__).with_name("contracts")
RUNNER = CONTRACTS / "runTests.py" if CONTRACTS.exists() else ROOT / "v4Tests/scripts/runTests.py"
TEMPLATE = CONTRACTS / "aux_a.stg" if CONTRACTS.exists() else ROOT / "StgAda/aux_a.stg"
spec = importlib.util.spec_from_file_location("regression_runner", RUNNER)
runner = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = runner
spec.loader.exec_module(runner)


class CoverageContracts(unittest.TestCase):
    def test_ada_build_flags_match_generated_coverage_recipe(self):
        recipe = TEMPLATE.read_text().split("coverage:\n", 1)[1].split("clean:\n", 1)[0]
        commands = [shlex.split(line.strip().replace("<sProjectName>", "asn1_x86.gpr"))
                    for line in recipe.splitlines() if line.strip().startswith("gprbuild ")]
        self.assertEqual(len(commands), 1)
        expected = ["--subdirs=coverage" if x == "--subdirs=debug" else x for x in commands[0]]
        actual = [x for x in c.ada_build_command(2) if x != "-j2"]
        self.assertEqual(actual, expected)

    def test_nocoverage_still_detects_ada_runtime_failure(self):
        for marker in ("-- NOCOVERAGE", "--NOCOVERAGE"):
            with self.subTest(marker=marker), tempfile.TemporaryDirectory() as temp:
                root = Path(temp)
                (root / "sample1.asn1").write_text(marker + "\n")
                (root / "tmp.err").write_text("")
                # The default target succeeds; only the runtime target reveals the defect.
                (root / "Makefile").write_text("all:\n\t@true\ncoverage:\n\t@exit 7\n")
                cfg = runner.TestConfig(language="Ada", rootDir=temp, path_to_asn1scc="compiler")
                item = runner.WorkItem("test", "test", 0, "", temp, "test", "")

                def execute(cfg, target, command, *unused):
                    if command.startswith("compiler "):
                        return 0
                    return subprocess.call(command, shell=True, cwd=target,
                                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

                results = runner.TestRunResults()
                with mock.patch.object(runner, "mysystem", side_effect=execute), \
                     mock.patch.object(runner, "results", results), \
                     contextlib.redirect_stdout(io.StringIO()), self.assertRaisesRegex(Exception, "TestFailed"):
                    runner.RunTestCase(cfg, item)
                self.assertEqual(len(results.get_errors()), 1)

    def test_marker_helper_and_warning_rules_match_regression_runner(self):
        samples = ["-- ACNV2_ONLY\n", "-- ACNV2_ONLY C_ONLY (Ada: ...)\n", "-- NOCOVERAGE\n"]
        with tempfile.TemporaryDirectory() as temp:
            for index, first in enumerate(samples):
                path = Path(temp) / f"{index:03d}.asn1"
                path.write_text(first + "--TCLS A[]\n")
                unit = next(u for u in c.enumerate_units(Path(temp)) if u["asn1"] == path.name)
                for language, v2 in (("c", ""), ("c", "--acn-v2"), ("Ada", "--acn-v2")):
                    with self.subTest(first=first, language=language, v2=v2):
                        cfg = runner.TestConfig(language=language, rootDir=temp,
                                                path_to_asn1scc="compiler", acnV2=v2)
                        args = c.arguments(["--test-root", temp, "--language", language,
                                            *([v2] if v2 else [])])
                        c.select_units([unit], args, None)
                        self.assertEqual(runner.isTestSelected(cfg, str(path)),
                                         unit["selection"] == "selected")
                self.assertEqual(Path(runner.helpersDir(str(path))),
                                 (Path(temp) / unit["asn1"]).with_suffix(".helpers"))
        for text in ("", "x.asn1:1:1: warning: w\n\n", "x.asn1:1:1: error: e\n",
                     "x: warning: w\nplain text\n"):
            with self.subTest(stderr=text):
                self.assertEqual(c.is_warning_only(text), runner.isWarningOnly(text))

    def test_default_compiler_flags_match_regression_runner(self):
        for language in ("c", "Ada"):
            with self.subTest(language=language), tempfile.TemporaryDirectory() as temp:
                (Path(temp) / "sample1.asn1").write_text("-- test\n")
                cfg = runner.TestConfig(language=language, rootDir=temp, path_to_asn1scc="compiler")
                item = runner.WorkItem("test", "test", 0, "", temp, "test", "")
                commands = []

                def capture(cfg, target, command, *unused):
                    commands.append(shlex.split(command))
                    raise RuntimeError("captured")

                with mock.patch.object(runner, "mysystem", side_effect=capture), \
                     contextlib.redirect_stdout(io.StringIO()), self.assertRaisesRegex(RuntimeError, "captured"):
                    runner.RunTestCase(cfg, item)
                tokens = commands[0][1:]
                del tokens[tokens.index("-x"):tokens.index("-x") + 2]  # diagnostic AST export only
                tokens = tokens[:tokens.index("-o")]
                self.assertEqual(tokens, c.compiler_flags(c.arguments(["--language", language])))


if __name__ == "__main__":
    unittest.main(verbosity=2)
