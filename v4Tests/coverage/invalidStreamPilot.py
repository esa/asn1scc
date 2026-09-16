"""Paired C statement/gcov measurements of fixed-layout ACN stream mutations."""
import argparse
import json
from pathlib import Path

from encodePilot import c
from decodePilot import compare_gcov, positives
from statementPilot import compare as compare_statements
import statementCollector
from invalidStreamHarness import UNIT, target_lines

CONFIGURATIONS = [("acn", []), ("acn-v2", ["--acn-v2"])]


def main():
    defaults = c.arguments([])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--metric", choices=("stmt", "gcov"), required=True)
    parser.add_argument("--outdir", type=Path, required=True)
    parser.add_argument("--compiler", type=Path, default=defaults.compiler)
    parser.add_argument("--test-root", type=Path, default=defaults.test_root)
    args = parser.parse_args()
    root = args.outdir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    result = {"status": "running", "metric": args.metric, "comparisons": [], "runs": []}
    measure = statementCollector.main if args.metric == "stmt" else c.main
    try:
        for index, (mode, flags) in enumerate(CONFIGURATIONS):
            pair = []
            for enabled in (False, True):
                stage = "mutations" if enabled else "baseline"
                out = root / str(index) / stage
                # The known pilot cohort contains only directive #1, preventing
                # the substring filter from also selecting #10 and #11.
                command = ["--outdir", str(out), "--compiler", str(args.compiler), "--test-root", str(args.test_root),
                           "--language", "c", "--encodings", "acn", "--cohort", "pilot", "--filter", UNIT,
                           "--timeout", "30", *flags]
                if enabled:
                    command.append("--check-invalid-streams")
                if args.metric == "gcov":
                    command.append("--enforce-legacy-line-gate")
                if measure(command):
                    raise ValueError(f"Measurement failed: {mode} {stage}")
                run, = out.iterdir()
                record, = map(json.loads, (run / "units.jsonl").read_text().splitlines())
                if record["unit"] != UNIT:
                    raise ValueError("Unexpected pilot fixture")
                record["positive_tests"] = positives(record, run)
                seconds = next(s["seconds"] for s in record["steps"] if s["stage"] == "run")
                result["runs"].append({"mode": mode, "unit": UNIT, "stage": stage,
                                       "positive_tests": record["positive_tests"], "run_seconds": seconds,
                                       "invalid_stream_checks": record.get("invalid_stream_checks")})
                pair.append((record, run))
            before, after = pair[0][0], pair[1][0]
            if before["positive_tests"] != after["positive_tests"]:
                raise ValueError("Positive test count drift")
            delta = compare_statements(before, after) if args.metric == "stmt" else compare_gcov(before, after)
            targets = target_lines(pair[0][1] / before["directory"] / "work")
            if targets != after["invalid_stream_checks"]["target_lines"]:
                raise ValueError("Target source drift")
            obligations = []
            if args.metric == "stmt":
                for line in targets:
                    old, = [o for o in before["files"]["sample1.c"]["obligations"]
                            if any(int(s["num"]) == line for s in o["spans"])]
                    new, = [o for o in after["files"]["sample1.c"]["obligations"] if o["id"] == old["id"]]
                    if old["status"] != "-" or new["status"] != "+":
                        raise ValueError("Selected decoder obligation was not newly covered")
                    obligations.append(new)
            result["comparisons"].append({"mode": mode, "unit": UNIT, "positive_tests": before["positive_tests"],
                                           "target_lines": targets, "target_obligations": obligations, **delta})
            print(mode, delta["before"], "->", delta["after"], flush=True)
        result["status"] = "ok"
    except (ValueError, OSError, KeyError) as error:
        result.update(status="failed", error=str(error))
        print(error, flush=True)
    finally:
        c.write_json(root / "pilot.json", result)
    return int(result["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
