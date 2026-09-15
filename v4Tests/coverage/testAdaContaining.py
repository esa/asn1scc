"""Compile and run the Ada/v2 CONTAINING wire-format and malformed-length regressions."""
import argparse
import importlib.util
from pathlib import Path
import shutil
import tempfile

SCRIPT = Path(__file__).with_name("coverageCollector.py")
if not SCRIPT.exists():
    SCRIPT = Path(__file__).resolve().parent.parent / "scripts/coverageCollector.py"
SPEC = importlib.util.spec_from_file_location("collector", SCRIPT)
c = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(c)


def main():
    defaults = c.arguments(["--language", "Ada", "--acn-v2"])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", type=Path, default=defaults.compiler)
    parser.add_argument("--test-root", type=Path, default=defaults.test_root)
    parser.add_argument("--outdir", type=Path)
    parser.add_argument("--slim", action="store_true")
    args = parser.parse_args()
    defaults.compiler, defaults.test_root, defaults.slim = args.compiler.resolve(), args.test_root.resolve(), args.slim
    unit = next(u for u in c.enumerate_units(defaults.test_root)
                if u["unit"] == "24-DEDUCED-SIZE/007.asn1#1")
    with tempfile.TemporaryDirectory(prefix="ada-containing-") as temp:
        work = args.outdir.resolve() if args.outdir else Path(temp)
        if args.outdir:
            work.mkdir(parents=True, exist_ok=False)
        logs = work / "logs"
        logs.mkdir()
        asn1, acn = c.input_bytes(unit, defaults.test_root)
        (work / "sample1.asn1").write_bytes(asn1)
        (work / "sample1.acn").write_bytes(acn)
        steps = []
        try:
            c.run_step([str(defaults.compiler), *c.compiler_flags(defaults), "-o", str(work),
                        "sample1.asn1", "sample1.acn"], work, logs, "compile", 120, steps, strict_stderr=True)
            shutil.copy2(Path(__file__).with_name("containing_bounds.adb"), work)
            c.run_step(["gprbuild", "-P", "asn1_x86.gpr", "containing_bounds.adb", "--subdirs=bounds",
                        "-j2", "-cargs", "-gnat2012", "-g", "-O0", "-gnatwa", "-gnatwe"],
                       work, logs, "build", 120, steps)
            print(c.run_step([str(work / "obj_x86/bounds/containing_bounds")], work,
                             logs, "run", 30, steps, strict_stderr=True).strip())
        except c.StageError as error:
            print(f"{error.stage}: {error.detail}")
            return 1
        finally:
            c.write_json(work / "steps.json", steps)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
