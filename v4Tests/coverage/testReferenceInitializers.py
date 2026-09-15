"""Require automatic execution of Ada initializers below constrained references."""
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

INITIALIZERS = ("MySeqArray_elem", "MyPDU_data_elem",
                "MyChoice_data1_elem", "MyChoice_data2_elem")


def main():
    defaults = c.arguments(["--language", "Ada"])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", type=Path, default=defaults.compiler)
    parser.add_argument("--test-root", type=Path, default=defaults.test_root)
    parser.add_argument("--outdir", type=Path)
    args = parser.parse_args()
    defaults.compiler = args.compiler.resolve()
    defaults.test_root = args.test_root.resolve()
    unit = next(u for u in c.enumerate_units(defaults.test_root)
                if u["unit"] == "16-mantis/0231.asn1#1")
    with tempfile.TemporaryDirectory(prefix="reference-initializers-") as temp:
        root = args.outdir.resolve() if args.outdir else Path(temp)
        if args.outdir:
            root.mkdir(parents=True, exist_ok=False)
        for v2, slim in ((v2, slim) for v2 in (False, True) for slim in (False, True)):
            defaults.acn_v2, defaults.slim = v2, slim
            mode = f"{'v2' if v2 else 'legacy'}-{'slim' if slim else 'normal'}"
            rec = c.measure_unit(unit, defaults, root / mode)
            assert rec["status"] == "ok", rec.get("detail", rec["status"])
            directory = root / mode / rec["directory"]
            work, logs = directory / "work", directory / "logs"
            assert "All test cases (12) run successfully." in (logs / "run.stdout").read_text()
            # Use only the unmodified generated ATC execution. The separate driver
            # below must not supply the coverage missing from the automatic suite.
            for name in INITIALIZERS:
                function = f"test_case__asn1scc_{name.lower()}_init"
                hits = [line["count"] for stat in rec["files"].values()
                        if stat["component"] == "codec" for line in stat["lines"]
                        if line.get("function_name") == function]
                assert hits and all(count > 0 for count in hits), f"ATCs did not execute {name}_Init"
            misses = c.aggregate([rec])["legacy_line_misses"]
            assert misses["enforced"] == 0, misses
            shutil.copy2(Path(__file__).with_name("reference_initializers.adb"), work)
            steps = []
            try:
                c.run_step(["gprbuild", "-P", "asn1_x86.gpr", "reference_initializers.adb",
                            "--subdirs=initializer-driver", "-j2", "-cargs", "-gnat2012",
                            "-g", "-O0", "-gnatwa", "-gnatwe"],
                           work, logs, "build-driver", 120, steps)
                output = c.run_step([str(work / "obj_x86/initializer-driver/reference_initializers")],
                                    work, logs, "run-driver", 30, steps, strict_stderr=True)
                print(f"{mode}: 12 automatic tests; four initializers executed; {output.strip()}")
            finally:
                c.write_json(directory / "driver-steps.json", steps)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
