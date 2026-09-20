"""ASan/UBSan and fault-injection tests for the bounded invalid-value oracles."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess

from encodePilot import c
from invalidValueHarness import (CASES, EXTRA_GOALS, EXTRA_PROFILES, EXTRA_OPERATIONS,
                                 target_lines, verify_output, verify_extra_output)
from invalidValuePilot import CONFIGURATIONS
from invalidStreamHarness import verify_output as verify_stream_output


def extra_faults(goal):
    yield "sanitizer", "all", None
    for operation in EXTRA_OPERATIONS:
        yield "missing-case", operation, "omit"
        yield "wrong-result", operation, "accepted = TRUE;"
        yield "wrong-error", operation, "error = 0;"
    yield "encoder-writes", "encode", "buffer[0] ^= 1;"
    for field in ("currentByte", "currentBit", "count"):
        yield "encoder-advances", field, f"stream.{field}++;"
    yield "missing-error-assignment", "source", 0
    yield "missing-rejection-assignment", "source", 1


def check_extra(args, report):
    pilot = json.loads((args.pilot_root / "pilot.json").read_text())
    if pilot["status"] != "ok" or pilot["extra_goals"] != EXTRA_GOALS:
        raise ValueError("Incomplete or incompatible value pilot")
    for goal, unit in EXTRA_GOALS.items():
        for mode in ("acn", "acn-v2"):
            record, = [r for r in pilot["runs"] if r["unit"] == unit
                       and r["mode"] == mode and r["stage"] == "invalid-values"]
            source = args.pilot_root / record["directory"] / "work"
            metadata = record["extra_value_checks"]
            for name, operation, fault in extra_faults(goal):
                work = args.outdir / goal / mode / (name + "-" + operation)
                work.mkdir(parents=True)
                for path in source.iterdir():
                    if path.suffix in (".c", ".h"):
                        shutil.copy2(path, work)
                if operation == "source":
                    path = work / "sample1.c"
                    lines = path.read_text().splitlines()
                    targets = target_lines(work, EXTRA_PROFILES[goal])
                    lines[targets[fault] - 1] = "        ; /* deliberately removed target */"
                    path.write_text("\n".join(lines) + "\n")
                elif fault is not None:
                    path = work / "mainprogram.c"
                    text = path.read_text()
                    if fault == "omit":
                        start = f"    /* begin {goal}/{operation} */"
                        end = f"    /* end {goal}/{operation} */"
                        if text.count(start) != 1 or text.count(end) != 1:
                            raise ValueError("Missing omission markers")
                        first, last = text.index(start), text.index(end) + len(end)
                        text = text[:first] + text[last:]
                    else:
                        observed = "encode" if name == "encoder-advances" else operation
                        marker = f"        /* observe {goal}/{observed} */"
                        if text.count(marker) != 1:
                            raise ValueError("Missing fault-injection point")
                        text = text.replace(marker, marker + "\n        " + fault)
                    path.write_text(text)
                logs = work / "logs"
                logs.mkdir()
                steps = []
                c.run_step(["gcc-13", "-g", "-O0", "-Wall", "-Wextra", "-Werror", "-D_DEBUG",
                            "-fsanitize=address,undefined", "-fno-sanitize-recover=all",
                            "-fno-omit-frame-pointer", "-no-pie",
                            *[p.name for p in sorted(work.glob("*.c"))], "-lm", "-o", "oracle"],
                           work, logs, "build", 120, steps)
                run = subprocess.run([str(work.resolve() / "oracle")], cwd=work,
                                     text=True, capture_output=True, timeout=30)
                (logs / "run.stdout").write_text(run.stdout)
                (logs / "run.stderr").write_text(run.stderr)
                c.write_json(logs / "steps.json", steps)
                # Old positives/stream negatives must succeed even in fault copies.
                verify_stream_output(run.stdout, record["invalid_stream_checks"])
                rejected = False
                try:
                    verify_extra_output(run.stdout, metadata)
                except ValueError:
                    rejected = True
                if name == "sanitizer":
                    if run.returncode or run.stderr or rejected:
                        raise ValueError(f"{goal}/{mode}: sanitizer/oracle failure")
                    # Exact whole-line matching also rejects duplicate/absent goals.
                    for extra in (f"Value goal {goal}/validate: OK\n",
                                  "Value goal absent/validate: OK\n"):
                        try:
                            verify_extra_output(run.stdout + extra, metadata)
                        except ValueError:
                            pass
                        else:
                            raise ValueError("Unexpected transcript accepted")
                elif name == "missing-case":
                    if run.returncode or run.stderr or not rejected:
                        raise ValueError("Omitted operation was not rejected by transcript validation")
                elif run.returncode != 1 or run.stderr or not rejected or "unexpected" not in run.stdout:
                    raise ValueError(f"{goal}/{mode}/{name}/{operation}: fault not detected")
                report["checks"].append({"goal": goal, "mode": mode, "name": name,
                                         "operation": operation, "status": "ok", "exit_code": run.returncode})
                print(goal, mode, name, operation, "PASS", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pilot-root", type=Path, required=True)
    parser.add_argument("--outdir", type=Path, required=True)
    args = parser.parse_args()
    args.outdir.mkdir(parents=True, exist_ok=False)
    mutations = {
        "validator-accepts": ("        if (accepted || error != errors[i]) {", "        accepted = TRUE;\n        if (accepted || error != errors[i]) {"),
        "wrong-validator-error": ("        if (accepted || error != errors[i]) {", "        error = 0;\n        if (accepted || error != errors[i]) {"),
        "encoder-accepts": ("            if (accepted || error != errors[i] ||", "            accepted = TRUE;\n            if (accepted || error != errors[i] ||"),
        "wrong-encoder-error": ("            if (accepted || error != errors[i] ||", "            error = 0;\n            if (accepted || error != errors[i] ||"),
        "encoder-writes": ("            if (accepted || error != errors[i] ||", "            buffer[0] ^= 1;\n            if (accepted || error != errors[i] ||"),
        "encoder-advances": ("            if (accepted || error != errors[i] ||", "            stream.currentBit = 1;\n            if (accepted || error != errors[i] ||"),
        "missing-value": ("i < 2", "i < 1"),
        "missing-error-assignment": None,
        "missing-rejection-assignment": None,
    }
    jobs = [(f"config-{i}", i) for i in range(len(CONFIGURATIONS))] + [(name, 0) for name in mutations]
    report = {"status": "running", "checks": []}
    try:
        for name, index in jobs:
            source, = (args.pilot_root / str(index) / "invalid-values").glob("*/units/*/work")
            work = args.outdir / name
            work.mkdir()
            for p in source.iterdir():
                if p.suffix in (".c", ".h"):
                    shutil.copy2(p, work)
            case = CASES[CONFIGURATIONS[index][1]]
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
                    targets = target_lines(work, case)
                    line = targets[0] if name == "missing-error-assignment" else targets[1]
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
                verify_output(run.stdout, {"case": case})
            except ValueError:
                rejected_output = True
            if name.startswith("config-"):
                if run.returncode or run.stderr or rejected_output:
                    raise ValueError(f"{name}: sanitizer/oracle failure: {run.stdout} {run.stderr}")
            elif name == "missing-value":
                if run.returncode or run.stderr or not rejected_output:
                    raise ValueError("Incomplete execution was not rejected by collector")
            elif run.returncode != 1 or run.stderr or "unexpected" not in run.stdout:
                raise ValueError(f"{name}: injected defect was not detected by oracle")
            c.write_json(logs / "steps.json", steps)
            report["checks"].append({"name": name, "status": "ok", "exit_code": run.returncode})
            print(name, "PASS", flush=True)
        check_extra(args, report)
        report["status"] = "ok"
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        report.update(status="failed", error=str(error))
        print(error, flush=True)
    finally:
        c.write_json(args.outdir / "checks.json", report)
    return int(report["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
