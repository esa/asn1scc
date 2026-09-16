"""Check bounded decode oracles under ASan/UBSan, including deliberate failures."""
import argparse
from pathlib import Path
import shutil
import subprocess

from encodePilot import c
from decodePilot import CONFIGURATIONS


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pilot-root", type=Path, required=True)
    parser.add_argument("--outdir", type=Path, required=True)
    args = parser.parse_args()
    args.outdir.mkdir(parents=True, exist_ok=False)
    # Change behavior immediately after the real decode; the oracle must fail.
    mutations = {
        "unexpected-accept": (1, "if (i == 1) { accepted = TRUE; error = 0; }", False),
        "unexpected-reject": (1, "if (i == 0) { accepted = FALSE; error = 1; }", False),
        "wrong-error": (1, "if (i == 1) error = 0;", False),
        "wrong-value": (1, "if (i == 3) decoded.arr[0].a ^= 1;", False),
        "wrong-consumption": (1, "if (i == 3) view.currentBit ^= 1;", False),
        "missing-prefix-run": (1, "", False),
        "out-of-view-read": (1, "", False),
        "missing-enum-fallback": (0, "", False),
    }
    jobs = [(f"config-{i}", i, "", True) for i in range(len(CONFIGURATIONS))]
    jobs += [(name, index, mutation, success) for name, (index, mutation, success) in mutations.items()]
    result = {"status": "running", "checks": []}
    try:
        for name, index, mutation, success in jobs:
            sources, = (args.pilot_root / str(index) / "truncate").glob("*/units/*/work")
            work = args.outdir / name
            work.mkdir()
            for source in sources.iterdir():
                if source.suffix in (".c", ".h"):
                    shutil.copy2(source, work)
            main = work / "mainprogram.c"
            text = main.read_text()
            anchor = "        int oracle = (accepted =="
            if text.count(anchor) != 1:
                raise ValueError("Missing bounded oracle")
            if mutation:
                text = text.replace(anchor, "        " + mutation + "\n" + anchor)
            if name == "missing-prefix-run":
                text = text.replace("i < 4", "i < 3")
            if name == "out-of-view-read":
                text = text.replace("BitStream_AttachBuffer(&view, prefix, prefixes[i]);",
                                    "BitStream_AttachBuffer(&view, prefix, prefixes[i] + 8);")
            if name == "missing-enum-fallback":
                codec = work / "sample1.c"
                old = "pVal->enm = MyPDU_enm_one;             /*COVERAGE_IGNORE*/"
                if codec.read_text().count(old) != 1:
                    raise ValueError("Missing enum fallback target")
                codec.write_text(codec.read_text().replace(old, "; /* removed fallback */"))
            main.write_text(text)
            logs = work / "logs"
            logs.mkdir()
            steps = []
            c.run_step(["gcc-13", "-g", "-O0", "-Wall", "-Wextra", "-Werror",
                        "-DASN1SCC_DECODE_ACTUAL_LENGTH", "-fsanitize=address,undefined",
                        "-fno-sanitize-recover=all", "-fno-omit-frame-pointer", "-no-pie",
                        *[p.name for p in sorted(work.glob("*.c"))], "-lm", "-o", "oracle"],
                       work, logs, "build", 120, steps)
            run = subprocess.run([str(work / "oracle")], cwd=work, text=True, capture_output=True, timeout=30)
            (logs / "run.stdout").write_text(run.stdout)
            (logs / "run.stderr").write_text(run.stderr)
            checks = {"stage": "truncate", "prefix_checks": 4,
                      "oracle": c.decode_support().CASES[CONFIGURATIONS[index][1]]}
            rejected_output = False
            try:
                c.decode_support().verify_output(run.stdout, checks)
            except ValueError:
                rejected_output = True
            if success:
                if run.returncode or run.stderr or rejected_output:
                    raise ValueError(f"{name}: sanitizer/oracle failure: {run.stderr} {run.stdout}")
            elif name == "out-of-view-read":
                if not run.returncode or "AddressSanitizer" not in run.stderr:
                    raise ValueError("Exact prefix allocation did not detect out-of-view read")
            elif name == "missing-prefix-run":
                if run.returncode or run.stderr or not rejected_output:
                    raise ValueError("Missing check was not rejected by the collector")
            elif run.returncode != 1 or run.stderr or "Decode prefix" not in run.stdout:
                raise ValueError(f"{name}: injected failure not detected by oracle")
            c.write_json(work / "steps.json", steps)
            result["checks"].append({"name": name, "exit_code": run.returncode, "status": "ok"})
            print(name, "PASS", flush=True)
        result["status"] = "ok"
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        result.update(status="failed", error=str(error))
        print(error, flush=True)
    finally:
        c.write_json(args.outdir / "checks.json", result)
    return int(result["status"] != "ok")


if __name__ == "__main__":
    raise SystemExit(main())
