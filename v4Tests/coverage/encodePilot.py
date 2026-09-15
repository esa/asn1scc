"""Compare the bounded C checked/unchecked encode pilot using the strict collector."""
import argparse
import importlib.util
import json
from pathlib import Path
import re

SCRIPT = Path(__file__).with_name("coverageCollector.py")
if not SCRIPT.exists():
    SCRIPT = Path(__file__).resolve().parent.parent / "scripts/coverageCollector.py"
SPEC = importlib.util.spec_from_file_location("collector", SCRIPT)
c = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(c)

UNITS = ("01-INTEGER/001.asn1#1", "06-OCTET-STRING/004.asn1#2",
         "10-SEQEUENCE/008.asn1#1")
MODES = {"uper": ["--encodings", "uper"],
         "acn": ["--encodings", "acn"],
         "acn-v2": ["--encodings", "acn", "--acn-v2"]}


def compare(before, after, before_dir, after_dir):
    if before["input_sha256"] != after["input_sha256"]:
        raise ValueError("Input drift")
    old = {k: f for k, f in before["files"].items() if f["component"] == "codec"}
    new = {k: f for k, f in after["files"].items() if f["component"] == "codec"}
    if not old or old.keys() != new.keys():
        raise ValueError("Codec inventory drift")
    gained = []
    for key in old:
        a, b = old[key], new[key]
        if a["source_sha256"] != b["source_sha256"]:
            raise ValueError("Codec source drift")
        aa = {arm["id"]: arm for arm in a["branches"]}
        bb = {arm["id"]: arm for arm in b["branches"]}
        if aa.keys() != bb.keys():
            raise ValueError("Codec branch identity drift")
        for identity, arm in aa.items():
            if arm["count"] and not bb[identity]["count"]:
                raise ValueError("Lost previously covered codec branch")
            if not arm["count"] and bb[identity]["count"]:
                gained.append({"source": b["source"], **bb[identity]})
        for metric in ("lines_total", "branches_total", "blocks_total"):
            if a[metric] != b[metric]:
                raise ValueError("Codec instrumentation drift")
        if b["official_line_misses"] != a["official_line_misses"]:
            raise ValueError("Legacy line-gate drift")

    def tests(record, directory):
        output = (directory / record["directory"] / "logs/run.stdout").read_text()
        match = re.search(r"All test cases \((\d+)\) run successfully", output)
        if not match or int(match[1]) == 0:
            raise ValueError("Missing positive test executions")
        return int(match[1])

    count = tests(before, before_dir)
    if tests(after, after_dir) != count:
        raise ValueError("Positive test count drift")
    calls = sum(line["count"] for f in after["files"].values()
                if f["component"] == "harness" for line in f["lines"]
                if "&uncheckedStrm, &uncheckedError, FALSE);" in line["text"])
    if calls != count:
        raise ValueError(f"Expected {count} additional encodes, measured {calls}")
    if not gained:
        raise ValueError("No newly covered codec branches in this pilot unit")
    totals = lambda files: {k: sum(f[k] for f in files.values()) for k in c.METRICS}
    runtime = lambda record: next(s["seconds"] for s in record["steps"] if s["stage"] == "run")
    return {"unit": before["unit"], "positive_tests": count,
            "additional_encode_calls": calls, "before": totals(old), "after": totals(new),
            "newly_covered_branches": gained,
            "run_seconds_before": runtime(before), "run_seconds_after": runtime(after)}


def main():
    defaults = c.arguments([])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--outdir", type=Path, required=True)
    parser.add_argument("--compiler", type=Path, default=defaults.compiler)
    parser.add_argument("--test-root", type=Path, default=defaults.test_root)
    parser.add_argument("--gcc", default=defaults.gcc)
    parser.add_argument("--gcov", default=defaults.gcov)
    args = parser.parse_args()
    root = args.outdir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    report = {"status": "running", "statement_coverage_measured": False, "comparisons": []}
    try:
        for mode, flags in MODES.items():
            for index, unit in enumerate(UNITS):
                pair = []
                for enabled in (False, True):
                    output = root / mode / str(index) / ("checked-unchecked" if enabled else "checked")
                    command = ["--outdir", str(output), "--compiler", str(args.compiler),
                               "--test-root", str(args.test_root), "--gcc", args.gcc,
                               "--gcov", args.gcov, "--filter", unit,
                               "--enforce-legacy-line-gate", *flags]
                    if enabled:
                        command.append("--check-encode")
                    if c.main(command):
                        raise ValueError(f"Collector failed: {mode} {unit} {enabled}")
                    directory, = output.iterdir()
                    records = [json.loads(s) for s in (directory / "units.jsonl").read_text().splitlines()]
                    record, = records
                    pair.append((record, directory))
                comparison = compare(pair[0][0], pair[1][0], pair[0][1], pair[1][1])
                report["comparisons"].append({"mode": mode, **comparison})
                print(f"{mode} {unit}: +{len(comparison['newly_covered_branches'])} codec branch arms", flush=True)
        report["status"] = "ok"
    except (ValueError, OSError) as error:
        report.update(status="failed", error=str(error))
        print(error)
    finally:
        c.write_json(root / "pilot.json", report)
    return int(report["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
