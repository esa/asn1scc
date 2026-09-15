"""Fault-inject a generated C pilot harness and check its encode comparison oracle."""
import argparse
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

from encodePilot import c


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work", type=Path, required=True,
                        help="Generated integer pilot work directory (with a partial final byte)")
    parser.add_argument("--gcc", default="gcc-13")
    parser.add_argument("--outdir", type=Path)
    args = parser.parse_args()
    mutations = {
        "unchanged": ("", True),
        "encode-failure": ("ret = FALSE;", False),
        "success-with-error-status": ("uncheckedError = 123;", True),
        "byte-length": ("uncheckedStrm.currentByte++;", False),
        "bit-length": ("uncheckedStrm.currentBit ^= 1;", False),
        "full-byte": ("uncheckedBuff[0] ^= 0x80;", False),
        "partial-byte": ("uncheckedBuff[uncheckedStrm.currentByte] ^= 0x80;", False),
        "unused-padding": ("uncheckedBuff[uncheckedStrm.currentByte] ^= 1;", True),
    }
    pattern = r"(    ret = [^\n]+&uncheckedStrm, &uncheckedError, FALSE\);)"
    with tempfile.TemporaryDirectory(prefix="encode-oracle-") as temp:
        root = args.outdir.resolve() if args.outdir else Path(temp)
        if args.outdir:
            root.mkdir(parents=True, exist_ok=False)
        for name, (mutation, success) in mutations.items():
            work = root / name
            work.mkdir()
            for source in args.work.iterdir():
                if source.suffix in (".c", ".h"):
                    shutil.copy2(source, work)
            changes = 0
            for source in work.glob("*_auto_tcs.c"):
                content, count = re.subn(pattern, lambda m: m[1] + "\n    " + mutation, source.read_text())
                changes += count
                source.write_text(content)
            if not changes:
                raise ValueError("No generated unchecked encode call found")
            logs = work / "logs"
            logs.mkdir()
            steps = []
            c.run_step([args.gcc, "-g", "-O0", "-Wall", "-Wextra", "-Werror",
                        "-DASN1SCC_CHECK_ENCODE", "-fsanitize=address,undefined",
                        "-fno-sanitize-recover=all", "-fno-omit-frame-pointer", "-no-pie",
                        *[p.name for p in sorted(work.glob("*.c"))], "-lm", "-o", "oracle"],
                       work, logs, "build", 120, steps)
            result = subprocess.run([str(work / "oracle")], cwd=work,
                                    capture_output=True, text=True, timeout=30)
            (logs / "run.stdout").write_text(result.stdout)
            (logs / "run.stderr").write_text(result.stderr)
            c.write_json(work / "steps.json", steps)
            if result.stderr or result.returncode != (0 if success else 1):
                raise ValueError(f"{name}: unexpected exit {result.returncode}: {result.stderr}")
            if not success and "Checked and unchecked encodings differ" not in result.stdout:
                raise ValueError(f"{name}: comparison did not report the failure")
            print(f"{name}: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
