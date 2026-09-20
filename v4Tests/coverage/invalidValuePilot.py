"""Compare positive-only and bounded invalid-value C coverage independently."""
import argparse
import json
from pathlib import Path

from encodePilot import c
from decodePilot import compare_gcov, positives
from statementPilot import compare as compare_statements
import statementCollector
from invalidValueHarness import (CASES, EXTRA_GOALS, EXTRA_PROFILES, target_lines,
                                 prepare_extra, verify_extra_output)


CONFIGURATIONS = [(mode, unit, ["--encodings", "both"] + (["--acn-v2"] if mode == "acn-v2" else []))
                  for mode in ("acn", "acn-v2") for unit in CASES]


def configurations():
    # Historical indices 0..3 remain stable; append one pair per extra unit.
    return CONFIGURATIONS + [(mode, unit, ["--encodings", "acn"] + flags)
                             for unit in dict.fromkeys(EXTRA_GOALS.values())
                             for mode, flags in (("acn", []), ("acn-v2", ["--acn-v2"]))]


def measure_exact(measure, command, unit, extra=False):
    original_cohort, original_loader = c.PILOT, c.harness_support
    streams = original_loader("invalidStreamHarness")

    class Composed:
        @staticmethod
        def prepare(work, selected, args):
            metadata = streams.prepare(work, selected, args)
            metadata["extra_value_checks"] = prepare_extra(work, selected, args)
            return metadata

        @staticmethod
        def verify_output(output, metadata):
            streams.verify_output(output, metadata)
            verify_extra_output(output, metadata["extra_value_checks"])

    try:
        c.PILOT = (unit,)
        if extra:
            c.harness_support = lambda name: Composed if name == "invalidStreamHarness" else original_loader(name)
        return measure(command)
    finally:
        c.PILOT, c.harness_support = original_cohort, original_loader


def main():
    defaults = c.arguments([])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--metric", choices=("stmt", "gcov"), required=True)
    parser.add_argument("--outdir", type=Path, required=True)
    parser.add_argument("--compiler", type=Path, default=defaults.compiler)
    parser.add_argument("--test-root", type=Path, default=defaults.test_root)
    parser.add_argument("--unit", choices=list(CASES) + list(dict.fromkeys(EXTRA_GOALS.values())))
    args = parser.parse_args()
    root = args.outdir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    result = {"status": "running", "metric": args.metric, "extra_goals": EXTRA_GOALS,
              "comparisons": [], "runs": []}
    measure = statementCollector.main if args.metric == "stmt" else c.main
    try:
        for index, (mode, unit, flags) in enumerate(configurations()):
            if args.unit and args.unit != unit:
                continue
            historical = unit in CASES
            pair = []
            for enabled in (False, True):
                stage = "invalid-values" if enabled else "baseline"
                out = root / str(index) / stage
                command = ["--outdir", str(out), "--compiler", str(args.compiler), "--test-root", str(args.test_root),
                           "--language", "c", "--cohort", "pilot", "--filter", unit, "--timeout", "30", *flags]
                if not historical:
                    command.append("--check-invalid-streams")
                elif enabled:
                    command.append("--check-invalid-values")
                if args.metric == "gcov":
                    command.append("--enforce-legacy-line-gate")
                if measure_exact(measure, command, unit, extra=enabled and not historical):
                    raise ValueError(f"Measurement failed: {mode} {unit} {stage}")
                run, = out.iterdir()
                record, = map(json.loads, (run / "units.jsonl").read_text().splitlines())
                if record["unit"] != unit:
                    raise ValueError("Unexpected pilot fixture")
                record["positive_tests"] = positives(record, run)
                seconds = next(s["seconds"] for s in record["steps"] if s["stage"] == "run")
                result["runs"].append({"mode": mode, "unit": unit, "stage": stage,
                                       "directory": str((run / record["directory"]).relative_to(root)),
                                       "positive_tests": record["positive_tests"], "run_seconds": seconds,
                                       "invalid_value_checks": record.get("invalid_value_checks"),
                                       "invalid_stream_checks": record.get("invalid_stream_checks"),
                                       "extra_value_checks": record.get("invalid_stream_checks", {}).get("extra_value_checks")})
                pair.append((record, run))
            before, after = pair[0][0], pair[1][0]
            if before["positive_tests"] != after["positive_tests"]:
                raise ValueError("Positive test count drift")
            delta = compare_statements(before, after) if args.metric == "stmt" else compare_gcov(before, after)
            goals = [goal for goal, selected in EXTRA_GOALS.items() if selected == unit]
            work = pair[0][1] / before["directory"] / "work"
            targets = (target_lines(work, CASES[unit]) if historical else
                       [line for goal in goals for line in target_lines(work, EXTRA_PROFILES[goal])])
            metadata = after.get("invalid_stream_checks", {}).get("extra_value_checks")
            recorded_targets = (after["invalid_value_checks"]["target_lines"] if historical else
                                [line for goal in goals for line in metadata["target_lines"][goal]])
            if targets != recorded_targets:
                raise ValueError("Target source drift")
            target_obligations = []
            if args.metric == "stmt":
                for line in targets:
                    old, = [o for o in before["files"]["sample1.c"]["obligations"]
                            if any(int(s["num"]) == line for s in o["spans"])]
                    new, = [o for o in after["files"]["sample1.c"]["obligations"] if o["id"] == old["id"]]
                    if old["status"] != "-" or new["status"] != "+":
                        raise ValueError("Selected value-goal obligation was not newly covered")
                    target_obligations.append(new)
            result["comparisons"].append({"mode": mode, "unit": unit, "positive_tests": before["positive_tests"],
                                           "scope": "historical-positive-only" if historical else "extra-over-streams",
                                           "goal_ids": goals, "extra_value_checks": metadata,
                                           "work_directories": {stage: str((run / record["directory"] / "work").relative_to(root))
                                                                for stage, (record, run) in zip(("baseline", "invalid-values"), pair)},
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
