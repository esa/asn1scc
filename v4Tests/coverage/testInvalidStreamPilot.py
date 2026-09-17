"""Sanitizers and deliberate defects for the fixed-layout stream-mutation pilot."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess

from encodePilot import c
from invalidStreamHarness import (LAYOUTS, STREAM_PROFILES, cases_for, padding_bits,
                                  source_fault_lines, verify_output)
from invalidStreamPilot import configurations


def mutations_for(unit):
    if unit in STREAM_PROFILES:
        return profile_mutations(unit)
    cases = cases_for(unit)
    count = len(cases)
    other_valid = cases.index("other-valid")
    first_invalid = next(i for i, case in enumerate(cases) if case.startswith("code-"))
    anchor = "            if (accepted != success[test]"
    patch = "                coverage_write_code(mutated, replacements[test]);"
    mutations = {
        "unexpected-acceptance": (anchor, f"            if (test == {first_invalid}) accepted = TRUE;\n" + anchor),
        "wrong-error": (anchor, "            if (!success[test]) error = 0;\n" + anchor),
        "wrong-positive-value": (anchor, f"            if (test == {other_valid}) decoded = seeds[seed];\n" + anchor),
        "decoder-writes": (anchor, "            mutated[0] ^= 1u;\n" + anchor),
        "wrong-consumption": (anchor, "            stream.currentBit ^= 1;\n" + anchor),
        "wrong-view-count": (anchor, "            stream.count += 1;\n" + anchor),
        "wrong-field-offset": (patch, patch + "\n                mutated[1] = (byte)((mutated[1] >> 1) | (mutated[0] << 7));"
                                "\n                mutated[0] >>= 1;"),
        "short-view": ("BitStream_AttachBuffer(&stream, mutated, sizeof mutated);",
                       "BitStream_AttachBuffer(&stream, mutated, sizeof mutated - 1);"),
        "missing-case": (f"test < {count}", f"test < {count - 1}"),
        "missing-error-assignment": None,
        "missing-rejection-assignment": None,
    }
    if padding_bits(unit):
        mutations["changed-padding"] = (patch, patch + "\n                mutated[1] ^= 1u;")
    if LAYOUTS[unit]["format"] == "twos-complement":
        mutations["unsigned-interpretation"] = (
            "return raw >= 512u ? (int)raw - 1024 : (int)raw;", "return (int)raw;")
        mutations["lost-sign-bit"] = (patch, patch + "\n                mutated[0] &= 0x7Fu;")
    if LAYOUTS[unit]["format"] == "signed-ascii":
        mutations["unsigned-interpretation"] = (
            "return buffer[0] == 0x2D ? -magnitude : magnitude;", "return magnitude;")
        mutations["wrong-sign"] = (patch, patch + "\n                mutated[0] = 0x2B;")
        # Numerically equal to zero, but not the required canonical +000 wire.
        zero_case = cases.index("code-0")
        mutations["negative-zero"] = (
            patch, patch + f"\n                if (test == {zero_case}) mutated[0] = 0x2D;")
    return mutations


def profile_mutations(unit):
    """Shared faults target semantic operations, never fixture case positions."""
    anchor = "        if (accepted != test->success"
    padding = "        if (test->padding) input[size - 1] ^= 1u;"
    profile = STREAM_PROFILES[unit]
    mutations = {
        "unexpected-acceptance": (anchor, "        if (!test->success) accepted = TRUE;\n" + anchor),
        "wrong-error": (anchor, "        if (!test->success) error ^= 1;\n" + anchor),
        "decoder-writes": (anchor, "        input[0] ^= 1u;\n" + anchor),
        "wrong-consumption": (anchor, "        stream.currentBit ^= 1;\n" + anchor),
        "wrong-view-count": (anchor, "        stream.count += 1;\n" + anchor),
        "wrong-field-offset": ("unsigned int position = field.offset + bit;",
                               "unsigned int position = field.offset + bit + 1;"),
        "short-view": ("BitStream_AttachBuffer(&stream, input, size);",
                       "BitStream_AttachBuffer(&stream, input, size - 1);"),
        # Actually omit execution; the process succeeds but transcript checking fails.
        "missing-case": ("case_index < case_count;", "case_index < case_count - 1;"),
    }
    if profile.get("value_fault"):
        mutations["wrong-positive-value"] = (anchor,
            "        if (test->success) { " + profile["value_fault"] + " }\n" + anchor)
    if any(seed["bits"] % 8 for seed in profile["seeds"]):
        mutations["changed-padding"] = (padding,
            "        if (test->field_count) input[size - 1] ^= 1u;\n" + padding)
        mutations["missing-padding-flip"] = ("if (test->padding) input[size - 1] ^= 1u;",
                                             "if (test->padding) input[size - 1] ^= 0u;")
    if profile.get("target_error"):
        mutations.update({"missing-error-assignment": None, "missing-rejection-assignment": None})
    return mutations


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pilot-root", type=Path, required=True)
    parser.add_argument("--outdir", type=Path, required=True)
    args = parser.parse_args()
    args.outdir.mkdir(parents=True, exist_ok=False)
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
                for name in ("sanitizer", *mutations_for(run["unit"]))]
        for index, record, name in jobs:
            mutations = mutations_for(record["unit"])
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
                    line = source_fault_lines(work, record["unit"])[name]
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
