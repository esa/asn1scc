"""Compare positive-only and bounded invalid-value C coverage independently."""
import argparse
import json
from pathlib import Path

from encodePilot import c
from decodePilot import compare_gcov, positives
from statementPilot import compare as compare_statements
import statementCollector
from invalidValueHarness import CASES, target_lines


CONFIGURATIONS = [(mode, unit, ["--encodings", "both"] + (["--acn-v2"] if mode == "acn-v2" else []))
                  for mode in ("acn", "acn-v2") for unit in CASES]


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
        for index, (mode, unit, flags) in enumerate(CONFIGURATIONS):
            pair = []
            for enabled in (False, True):
                stage = "invalid-values" if enabled else "baseline"
                out = root / str(index) / stage
                command = ["--outdir", str(out), "--compiler", str(args.compiler), "--test-root", str(args.test_root),
                           "--language", "c", "--cohort", "all", "--filter", unit, "--timeout", "30", *flags]
                if enabled:
                    command.append("--check-invalid-values")
                if args.metric == "gcov":
                    command.append("--enforce-legacy-line-gate")
                if measure(command):
                    raise ValueError(f"Measurement failed: {mode} {unit} {stage}")
                run, = out.iterdir()
                record, = map(json.loads, (run / "units.jsonl").read_text().splitlines())
                record["positive_tests"] = positives(record, run)
                seconds = next(s["seconds"] for s in record["steps"] if s["stage"] == "run")
                result["runs"].append({"mode": mode, "unit": unit, "stage": stage,
                                       "positive_tests": record["positive_tests"], "run_seconds": seconds,
                                       "invalid_value_checks": record.get("invalid_value_checks")})
                pair.append((record, run))
            before, after = pair[0][0], pair[1][0]
            if before["positive_tests"] != after["positive_tests"]:
                raise ValueError("Positive test count drift")
            delta = compare_statements(before, after) if args.metric == "stmt" else compare_gcov(before, after)
            targets = target_lines(pair[0][1] / before["directory"] / "work", CASES[unit])
            if targets != after["invalid_value_checks"]["target_lines"]:
                raise ValueError("Target source drift")
            target_obligations = []
            if args.metric == "stmt":
                for line in targets:
                    old, = [o for o in before["files"]["sample1.c"]["obligations"]
                            if any(int(s["num"]) == line for s in o["spans"])]
                    new, = [o for o in after["files"]["sample1.c"]["obligations"] if o["id"] == old["id"]]
                    if old["status"] != "-" or new["status"] != "+":
                        raise ValueError("Selected validator obligation was not newly covered")
                    target_obligations.append(new)
            result["comparisons"].append({"mode": mode, "unit": unit, "positive_tests": before["positive_tests"],
                                           "target_lines": targets, "target_obligations": target_obligations, **delta})
            print(mode, unit, delta["before"], "->", delta["after"], flush=True)
        result["status"] = "ok"
    except (ValueError, OSError, KeyError) as error:
        result.update(status="failed", error=str(error))
        print(error, flush=True)
    finally:
        c.write_json(root / "pilot.json", result)
    return int(result["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
