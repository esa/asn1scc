"""Check bounded fixed-width deduced decoding and deferred patch contracts."""
import argparse
import importlib.util
from pathlib import Path
import re
import shutil
import tempfile

SCRIPT = Path(__file__).with_name("coverageCollector.py")
if not SCRIPT.exists():
    SCRIPT = Path(__file__).resolve().parent.parent / "scripts/coverageCollector.py"
SPEC = importlib.util.spec_from_file_location("collector", SCRIPT)
c = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(c)
HERE = Path(__file__).resolve().parent


def main():
    defaults = c.arguments(["--language", "Ada"])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", type=Path, default=defaults.compiler)
    parser.add_argument("--test-root", type=Path, default=defaults.test_root)
    parser.add_argument("--outdir", type=Path)
    args = parser.parse_args()
    defaults.compiler, defaults.test_root = args.compiler.resolve(), args.test_root.resolve()
    units = {u["unit"]: u for u in c.enumerate_units(defaults.test_root)}
    with tempfile.TemporaryDirectory(prefix="ada-contracts-") as temp:
        root = args.outdir.resolve() if args.outdir else Path(temp)
        if args.outdir:
            root.mkdir(parents=True, exist_ok=False)
        for v2 in (False, True):
            for slim in (False, True):
                defaults.acn_v2, defaults.slim = v2, slim
                mode = f"{'v2' if v2 else 'legacy'}-{'slim' if slim else 'normal'}"
                for fixture, count in (("minimal", None), ("004", 32), ("007", 26)):
                    work = root / mode / fixture
                    logs = work / "logs"
                    logs.mkdir(parents=True)
                    if fixture == "minimal":
                        asn1 = (HERE / "ada_contracts.asn1").read_bytes()
                        acn = (HERE / "ada_contracts.acn").read_bytes()
                    else:
                        asn1, acn = c.input_bytes(units[f"24-DEDUCED-SIZE/{fixture}.asn1#1"], defaults.test_root)
                    (work / "sample1.asn1").write_bytes(asn1)
                    (work / "sample1.acn").write_bytes(acn)
                    steps = []
                    try:
                        c.run_step([str(defaults.compiler), *c.compiler_flags(defaults), "-o", str(work),
                                    "sample1.asn1", "sample1.acn"], work, logs, "compile", 120, steps,
                                   strict_stderr=True)
                        command = c.ada_build_command(2)
                        command.insert(command.index("-largs"), "-gnata")
                        c.run_step(command, work, logs, "build-atcs", 120, steps)
                        output = c.run_step([str(work / "obj_x86/coverage/mainprogram")], work,
                                            logs, "run-atcs", 30, steps, strict_stderr=True)
                        match = re.search(r"All test cases \((\d+)\) run successfully", output)
                        assert match and int(match[1]) > 0, output
                        if count is not None:
                            assert int(match[1]) == count, output
                        drivers = ["decoder_contracts", "patch_contracts"] if fixture == "minimal" else []
                        if fixture == "007":
                            drivers = ["containing_bounds"]
                        for driver in drivers:
                            shutil.copy2(HERE / (driver + ".adb"), work)
                            c.run_step(["gprbuild", "-P", "asn1_x86.gpr", driver + ".adb",
                                        "--subdirs=driver-" + driver, "-j2", "-cargs", "-gnat2012",
                                        "-g", "-O0", "-gnatwa", "-gnatwe", "-gnata"],
                                       work, logs, "build-" + driver, 120, steps)
                            result = c.run_step([str(work / ("obj_x86/driver-" + driver) / driver)],
                                                work, logs, "run-" + driver, 30, steps, strict_stderr=True)
                            print(mode, fixture, result.strip(), flush=True)
                        print(mode, fixture, match[0], "with assertions", flush=True)
                    finally:
                        c.write_json(work / "steps.json", steps)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
