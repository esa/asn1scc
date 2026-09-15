"""Calibrate source-statement measurement and exercise strict report accounting."""
import argparse
import os
from pathlib import Path
import tempfile
import xml.etree.ElementTree as ET

from statementCollector import c, measure, read_report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--language", choices=("c", "Ada"), required=True)
    parser.add_argument("--outdir", type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="statement-check-") as temp:
        work = args.outdir.resolve() if args.outdir else Path(temp)
        if args.outdir:
            work.mkdir(parents=True, exist_ok=False)
        logs = work / "logs"
        logs.mkdir()
        steps = []
        source = "main.c" if args.language == "c" else "main.adb"
        language = "C" if args.language == "c" else "Ada"
        (work / "probe.gpr").write_text(
            'project Probe is\n for Languages use ("' + language + '");\n'
            ' for Source_Dirs use (".");\n for Object_Dir use "obj";\n'
            ' for Exec_Dir use ".";\n for Main use ("' + source + '");\nend Probe;\n')
        (work / source).write_text(
            'int main(int argc, char **argv) { volatile int x=0; (void)argv; '
            'if(argc==1) x=1; else x=2; return x==1 ? 0 : 0; }\n'
            if args.language == "c" else
            'with Ada.Command_Line;\nprocedure Main is\n X : Integer := 0;\n'
            ' pragma Volatile (X);\nbegin\n'
            ' if Ada.Command_Line.Argument_Count = 0 then X := 1; else X := 2; end if;\nend Main;\n')
        gnatcov = os.environ.get("GNATCOV", "gnatcov")
        c.run_step([gnatcov, "instrument", "-Pprobe.gpr", "--level=stmt",
                    "--restricted-to-languages=" + language, "--dump-trigger=atexit", "--dump-channel=bin-file"],
                   work, logs, "instrument", 120, steps)
        c.run_step(["gprbuild", "-p", "-Pprobe.gpr", "--src-subdirs=gnatcov-instr",
                    "--implicit-with=gnatcov_rts", "-cargs", "-O0"], work, logs, "build", 120, steps)
        results = []
        for index, arguments in enumerate(([], ["other"])):
            c.run_step([str(work / "main"), *arguments], work, logs, f"run-{index}", 30, steps, strict_stderr=True)
            traces = sorted(work.glob("*.srctrace"))
            if len(traces) != index + 1 or any(not t.stat().st_size for t in traces):
                raise ValueError("Unexpected source trace inventory")
            report = work / f"report-{index}"
            c.run_step([gnatcov, "coverage", "-Pprobe.gpr", "--level=stmt", "--annotate=xml,xcov,report",
                        "--output-dir=" + str(report), *[str(t) for t in traces]],
                       work, logs, f"coverage-{index}", 120, steps)
            results.append(read_report(report, work)[source])
        before, after = results
        assert before["statements_total"] == after["statements_total"] > 2
        assert before["statements_uncovered"] == 1
        assert after["statements_uncovered"] == 0
        assert after["statements_covered"] == before["statements_covered"] + 1
        assert {x["id"] for x in before["obligations"]} == {x["id"] for x in after["obligations"]}
        # A covered line must not conceal an uncovered statement on that line.
        missed = next(x for x in before["obligations"] if x["status"] == "-")
        missed_line = missed["spans"][0]["num"]
        assert any(x["status"] == "+" and x["spans"][0]["num"] == missed_line for x in before["obligations"])

        report = work / "report-0"
        source_xml = report / "xml" / (source + ".xml")
        original = source_xml.read_bytes()
        mutations = ("unknown-status", "wrong-count", "duplicate", "missing-source")
        for mutation in mutations:
            tree = ET.fromstring(original)
            statement = next(x for x in tree.findall(".//statement") if x.get("coverage") == "+")
            if mutation == "unknown-status":
                statement.set("coverage", "?")
            elif mutation == "wrong-count":
                statement.set("coverage", "-")
            elif mutation == "duplicate":
                tree.append(ET.fromstring(ET.tostring(statement)))
            else:
                tree.set("file", str(work / "missing-source"))
            source_xml.write_bytes(ET.tostring(tree))
            try:
                read_report(report, work)
            except ValueError:
                pass
            else:
                raise AssertionError("Accepted invalid report: " + mutation)
        source_xml.write_bytes(original)
        invalid = measure({"unit": "missing-input#1", "nocoverage": False,
                           "input_error": "Missing ACN input"}, c.arguments([]), work)
        assert invalid["status"] == "failed" and invalid["failed_stage"] == "setup"
        assert invalid["input_sha256"] is None and not invalid["files"]
        c.write_json(work / "steps.json", steps)
        c.write_json(work / "calibration.json", {"language": args.language, "before": before, "after": after,
                                                "rejected_report_mutations": list(mutations),
                                                "input_failure_recorded": True})
        print(f"{args.language}: {before['statements_covered']}/{before['statements_total']} → "
              f"{after['statements_covered']}/{after['statements_total']}; four invalid reports rejected")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
