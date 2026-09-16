"""Measure baseline, actual-length and explicit capped C truncation separately."""
import argparse
import json
from pathlib import Path
import re

from encodePilot import c
from statementPilot import compare as compare_statements
import statementCollector


CONFIGURATIONS = [
    ("uper", "10-SEQEUENCE/008.asn1#1", ["--encodings", "uper"]),
    *[(mode, unit, ["--encodings", "acn"] + (["--acn-v2"] if mode == "acn-v2" else []))
      for mode in ("acn", "acn-v2")
      for unit in ("24-DEDUCED-SIZE/002.asn1#1", "24-DEDUCED-SIZE/001.asn1#2")],
]
STAGES = ("baseline", "actual", "truncate")


def positives(record, directory):
    output = (directory / record["directory"] / "logs/run.stdout").read_text()
    found = re.search(r"All test cases \((\d+)\) run successfully", output)
    if not found or int(found[1]) == 0:
        raise ValueError("Missing positive executions")
    return int(found[1])


def compare_gcov(before, after):
    old = {k: v for k, v in before["files"].items() if v["component"] == "codec"}
    new = {k: v for k, v in after["files"].items() if v["component"] == "codec"}
    if not old or old.keys() != new.keys() or before["input_sha256"] != after["input_sha256"]:
        raise ValueError("Codec/input inventory drift")
    gained = []
    for name, file in old.items():
        other = new[name]
        if file["source_sha256"] != other["source_sha256"]:
            raise ValueError("Codec source drift")
        for field in ("lines_total", "branches_total", "blocks_total"):
            if file[field] != other[field]:
                raise ValueError("Gcov instrumentation drift")
        a = {o["id"]: o for o in file["branches"]}
        b = {o["id"]: o for o in other["branches"]}
        if a.keys() != b.keys():
            raise ValueError("Branch identity drift")
        for identity, arm in a.items():
            if arm["count"] and not b[identity]["count"]:
                raise ValueError("Lost covered branch")
            if not arm["count"] and b[identity]["count"]:
                gained.append({"source": file["source"], **b[identity]})
        old_lines = {x["line_number"] for x in file["lines"] if x["count"]}
        new_lines = {x["line_number"] for x in other["lines"] if x["count"]}
        if old_lines - new_lines or set(other["official_line_misses"]) - set(file["official_line_misses"]):
            raise ValueError("Lost line coverage")
    totals = lambda files: {key: sum(f[key] for f in files.values()) for key in c.METRICS}
    return {"before": totals(old), "after": totals(new), "newly_covered_branches": gained}


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
            previous = None
            for stage in STAGES:
                out = root / str(index) / stage
                command = ["--outdir", str(out), "--compiler", str(args.compiler),
                           "--test-root", str(args.test_root), "--language", "c", "--cohort", "all",
                           "--filter", unit, "--decode-stage", stage, "--timeout", "30", *flags]
                if args.metric == "gcov":
                    command.append("--enforce-legacy-line-gate")
                if measure(command):
                    raise ValueError(f"Measurement failed: {mode} {unit} {stage}")
                run, = out.iterdir()
                record, = map(json.loads, (run / "units.jsonl").read_text().splitlines())
                count = positives(record, run)
                seconds = next(s["seconds"] for s in record["steps"] if s["stage"] == "run")
                if seconds > 30:
                    raise ValueError("Decode pilot runtime exceeded 30 seconds")
                result["runs"].append({"mode": mode, "unit": unit, "stage": stage,
                                       "positive_tests": count, "run_seconds": seconds,
                                       "decode_checks": record["decode_checks"]})
                if previous:
                    if count != previous["positive_tests"]:
                        raise ValueError("Positive test count drift")
                    delta = compare_statements(previous, record) if args.metric == "stmt" else compare_gcov(previous, record)
                    if args.metric == "stmt" and stage == "truncate":
                        gained = delta["newly_covered_statements"]
                        text = "\n".join(span.get("src", "") for o in gained for span in o["spans"])
                        if index == 0 and "pVal->enm = MyPDU_enm_one;" not in text:
                            raise ValueError("Target enum fallback statement was not newly covered")
                        if "/002.asn1" in unit and not ("ret = FALSE;" in text and "*pErrCode = ERR_ACN_DECODE_ELIST;" in text):
                            raise ValueError("Target residual-length error statements were not newly covered")
                    result["comparisons"].append({"mode": mode, "unit": unit, "stage": stage, **delta})
                    print(mode, unit, stage, delta["before"], "->", delta["after"], flush=True)
                record["positive_tests"] = count
                previous = record
        result["status"] = "ok"
    except (ValueError, OSError, KeyError) as error:
        result.update(status="failed", error=str(error))
        print(error, flush=True)
    finally:
        c.write_json(root / "pilot.json", result)
    return int(result["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
