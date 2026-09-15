"""Compare checked/unchecked C statement coverage on the bounded encode pilot."""
import argparse
import json
from pathlib import Path

from encodePilot import c, UNITS, MODES
import statementCollector as statements


def compare(before, after):
    if before["input_sha256"] != after["input_sha256"] or before["positive_tests"] != after["positive_tests"]:
        raise ValueError("Input or positive-test inventory drift")
    old = {k: f for k, f in before["files"].items() if f["component"] == "codec"}
    new = {k: f for k, f in after["files"].items() if f["component"] == "codec"}
    if old.keys() != new.keys():
        raise ValueError("Codec inventory drift")
    gained = []
    for name, file in old.items():
        if file["source_sha256"] != new[name]["source_sha256"]:
            raise ValueError("Codec source drift")
        a = {x["id"]: x for x in file["obligations"]}
        b = {x["id"]: x for x in new[name]["obligations"]}
        if a.keys() != b.keys():
            raise ValueError("Statement obligation identity drift")
        for identity, obligation in a.items():
            if obligation["status"] == "+" and b[identity]["status"] != "+":
                raise ValueError("Lost covered statement")
            if obligation["status"] == "-" and b[identity]["status"] == "+":
                gained.append({"source": name, **b[identity]})
    def total(files):
        return {key: sum(f[key] for f in files.values()) for key in
                ("statements_total", "statements_covered", "statements_uncovered")}
    return {"unit": before["unit"], "positive_tests": before["positive_tests"],
            "before": total(old), "after": total(new), "newly_covered_statements": gained}


def main():
    defaults = c.arguments([])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--outdir", type=Path, required=True)
    parser.add_argument("--compiler", type=Path, default=defaults.compiler)
    parser.add_argument("--test-root", type=Path, default=defaults.test_root)
    args = parser.parse_args()
    root = args.outdir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    result = {"status": "running", "metric": "statement", "comparisons": []}
    try:
        for mode, flags in MODES.items():
            for index, unit in enumerate(UNITS):
                pair = []
                for enabled in (False, True):
                    out = root / mode / str(index) / ("checked-unchecked" if enabled else "checked")
                    command = ["--language", "c", "--outdir", str(out), "--compiler", str(args.compiler),
                               "--test-root", str(args.test_root), "--filter", unit, *flags]
                    if enabled:
                        command.append("--check-encode")
                    if statements.main(command):
                        raise ValueError(f"Statement measurement failed: {mode} {unit}")
                    run, = out.iterdir()
                    record, = [json.loads(line) for line in (run / "units.jsonl").read_text().splitlines()]
                    pair.append(record)
                row = {"mode": mode, **compare(*pair)}
                result["comparisons"].append(row)
                print(mode, unit, row["before"], "->", row["after"], flush=True)
        result["status"] = "ok"
    except (ValueError, OSError) as error:
        result.update(status="failed", error=str(error))
        print(error)
    finally:
        c.write_json(root / "pilot.json", result)
    return int(result["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
