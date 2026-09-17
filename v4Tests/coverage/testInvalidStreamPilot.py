"""Sanitizers and deliberate defects for the fixed-layout stream-mutation pilot."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess

from encodePilot import c
from invalidStreamHarness import target_lines, verify_output
from invalidStreamPilot import configurations


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pilot-root", type=Path, required=True)
    parser.add_argument("--outdir", type=Path, required=True)
    args = parser.parse_args()
    args.outdir.mkdir(parents=True, exist_ok=False)
    anchor = "            if (accepted != success[test]"
    patch = "                coverage_write_code(mutated, replacements[test]);"
    mutations = {
        "unexpected-acceptance": (anchor, "            if (test == 1) accepted = TRUE;\n" + anchor),
        "wrong-error": (anchor, "            if (!success[test]) error = 0;\n" + anchor),
        "wrong-positive-value": (anchor, "            if (test == 4) decoded = seeds[seed];\n" + anchor),
        "decoder-writes": (anchor, "            mutated[0] ^= 1u;\n" + anchor),
        "wrong-consumption": (anchor, "            stream.currentBit ^= 1;\n" + anchor),
        "wrong-view-count": (anchor, "            stream.count += 1;\n" + anchor),
        "wrong-field-offset": (patch, patch + "\n                mutated[1] = (byte)((mutated[1] >> 1) | (mutated[0] << 7));"
                                "\n                mutated[0] >>= 1;"),
        "changed-padding": (patch, patch + "\n                mutated[1] ^= 1u;"),
        "short-view": ("BitStream_AttachBuffer(&stream, mutated, sizeof mutated);",
                       "BitStream_AttachBuffer(&stream, mutated, sizeof mutated - 1);"),
        "missing-case": ("test < 6", "test < 5"),
        "missing-error-assignment": None,
        "missing-rejection-assignment": None,
    }
    report = {"status": "running", "checks": []}
    try:
        pilot = json.loads((args.pilot_root / "pilot.json").read_text())
        if pilot["status"] != "ok":
            raise ValueError("Pilot did not pass")
        runs = [r for r in pilot["runs"] if r["stage"] == "mutations"]
        expected = {(unit, mode) for unit, mode, _ in configurations()}
        if len(runs) != len(expected) or {(r["unit"], r["mode"]) for r in runs} != expected:
            raise ValueError("Missing or duplicate registered unit/mode")
        jobs = [(index, run, name) for index, run in enumerate(runs)
                for name in ("sanitizer", *mutations)]
        for index, record, name in jobs:
            source = (args.pilot_root / record["directory"] / "work").resolve()
            if not source.is_relative_to(args.pilot_root.resolve()):
                raise ValueError("Pilot work directory escapes pilot root")
            work = args.outdir / str(index) / name
            work.mkdir(parents=True)
            for p in source.iterdir():
                if p.suffix in (".c", ".h"):
                    shutil.copy2(p, work)
            if name in mutations:
                mutation = mutations[name]
                if mutation:
                    path = work / "mainprogram.c"
                    text = path.read_text()
                    if text.count(mutation[0]) != 1:
                        raise ValueError("Missing fault-injection point: " + name)
                    path.write_text(text.replace(*mutation))
                else:
                    path = work / "sample1.c"
                    lines = path.read_text().splitlines()
                    targets = target_lines(work)
                    line = targets[1] if name == "missing-error-assignment" else targets[0]
                    lines[line - 1] = "        ; /* deliberately removed target */"
                    path.write_text("\n".join(lines) + "\n")
            logs = work / "logs"
            logs.mkdir()
            steps = []
            c.run_step(["gcc-13", "-g", "-O0", "-Wall", "-Wextra", "-Werror", "-D_DEBUG",
                        "-fsanitize=address,undefined", "-fno-sanitize-recover=all", "-fno-omit-frame-pointer", "-no-pie",
                        *[p.name for p in sorted(work.glob("*.c"))], "-lm", "-o", "oracle"],
                       work, logs, "build", 120, steps)
            run = subprocess.run([str(work / "oracle")], cwd=work, text=True, capture_output=True, timeout=30)
            (logs / "run.stdout").write_text(run.stdout)
            (logs / "run.stderr").write_text(run.stderr)
            rejected_output = False
            try:
                verify_output(run.stdout, record["invalid_stream_checks"])
            except ValueError:
                rejected_output = True
            if name == "sanitizer":
                if run.returncode or run.stderr or rejected_output:
                    raise ValueError(f"{name}: sanitizer/oracle failure: {run.stdout} {run.stderr}")
            elif name == "missing-case":
                if run.returncode or run.stderr or not rejected_output:
                    raise ValueError("Incomplete execution was not rejected by collector")
            elif run.returncode != 1 or run.stderr or "unexpected" not in run.stdout:
                raise ValueError(f"{name}: injected defect was not detected by oracle")
            c.write_json(logs / "steps.json", steps)
            report["checks"].append({"unit": record["unit"], "mode": record["mode"],
                                     "name": name, "status": "ok", "exit_code": run.returncode})
            print(record["unit"], record["mode"], name, "PASS", flush=True)
        report["status"] = "ok"
    except (ValueError, OSError, KeyError, subprocess.SubprocessError) as error:
        report.update(status="failed", error=str(error))
        print(error, flush=True)
    finally:
        c.write_json(args.outdir / "checks.json", report)
    return int(report["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
