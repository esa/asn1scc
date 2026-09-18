"""Paired C statement/gcov measurements of fixed-layout ACN stream mutations."""
import argparse
import json
from pathlib import Path

from encodePilot import c
from decodePilot import compare_gcov, positives
from statementPilot import compare as compare_statements
import statementCollector
from invalidStreamHarness import (SUPPORTED_UNITS, POSITIVE_INITIALIZERS, target_lines,
                                  prepare_positive_controls, verify_output)

CONFIGURATIONS = [("acn", []), ("acn-v2", ["--acn-v2"])]


def configurations():
    # Keep the original unit's two indices stable as the registry grows.
    return [(unit, mode, flags) for unit in SUPPORTED_UNITS for mode, flags in CONFIGURATIONS]


def measure_exact(measure, command, unit, positive_only=False):
    # Collectors share this module and already support exact cohort membership.
    # Scope the cohort override to one call, including restoration on failure.
    original = c.PILOT
    original_loader = c.harness_support
    class PositiveControls:
        prepare = staticmethod(prepare_positive_controls)
        verify_output = staticmethod(verify_output)
    try:
        c.PILOT = (unit,)
        if positive_only:
            c.harness_support = lambda name: PositiveControls if name == "invalidStreamHarness" else original_loader(name)
        return measure(command)
    finally:
        c.PILOT = original
        c.harness_support = original_loader


def main():
    defaults = c.arguments([])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--metric", choices=("stmt", "gcov"), required=True)
    parser.add_argument("--outdir", type=Path, required=True)
    parser.add_argument("--compiler", type=Path, default=defaults.compiler)
    parser.add_argument("--test-root", type=Path, default=defaults.test_root)
    parser.add_argument("--unit", choices=SUPPORTED_UNITS, help="Measure exactly one registered unit")
    args = parser.parse_args()
    root = args.outdir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    result = {"status": "running", "metric": args.metric, "comparisons": [], "runs": []}
    measure = statementCollector.main if args.metric == "stmt" else c.main
    try:
        for index, (unit, mode, flags) in enumerate(configurations()):
            if args.unit and unit != args.unit:
                continue
            pair = []
            for enabled in (False, True):
                stage = "mutations" if enabled else "baseline"
                out = root / str(index) / stage
                command = ["--outdir", str(out), "--compiler", str(args.compiler), "--test-root", str(args.test_root),
                           "--language", "c", "--encodings", "acn", "--cohort", "pilot", "--filter", unit,
                           "--timeout", "30", *flags]
                positive_only = not enabled and unit in POSITIVE_INITIALIZERS
                if enabled or positive_only:
                    command.append("--check-invalid-streams")
                if args.metric == "gcov":
                    command.append("--enforce-legacy-line-gate")
                if measure_exact(measure, command, unit, positive_only=positive_only):
                    raise ValueError(f"Measurement failed: {unit} {mode} {stage}")
                run, = out.iterdir()
                record, = map(json.loads, (run / "units.jsonl").read_text().splitlines())
                if record["unit"] != unit:
                    raise ValueError("Unexpected pilot fixture")
                record["positive_tests"] = positives(record, run)
                seconds = next(s["seconds"] for s in record["steps"] if s["stage"] == "run")
                result["runs"].append({"mode": mode, "unit": unit, "stage": stage,
                                       "directory": str((run / record["directory"]).relative_to(root)),
                                       "positive_tests": record["positive_tests"], "run_seconds": seconds,
                                       "positive_initializer_controls": record.get("invalid_stream_checks", {}).get("positive_initializers", []),
                                       "invalid_stream_checks": record.get("invalid_stream_checks")})
                pair.append((record, run))
            before, after = pair[0][0], pair[1][0]
            if before["positive_tests"] != after["positive_tests"]:
                raise ValueError("Positive test count drift")
            delta = compare_statements(before, after) if args.metric == "stmt" else compare_gcov(before, after)
            targets = target_lines(pair[0][1] / before["directory"] / "work", unit)
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
            result["comparisons"].append({"mode": mode, "unit": unit, "positive_tests": before["positive_tests"],
                                           "work_directories": {stage: str((run / record["directory"] / "work").relative_to(root))
                                                                for stage, (record, run) in zip(("baseline", "mutations"), pair)},
                                           "target_lines": targets, "target_obligations": obligations, **delta})
            print(unit, mode, delta["before"], "->", delta["after"], flush=True)
        result["status"] = "ok"
    except (ValueError, OSError, KeyError) as error:
        result.update(status="failed", error=str(error))
        print(error, flush=True)
    finally:
        c.write_json(root / "pilot.json", result)
    return int(result["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
