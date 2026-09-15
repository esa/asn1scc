"""Require NULL ATCs and defined Ada outputs, including selective PDU generation."""
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
                if u["unit"] == "18-NULL/001.asn1#1")
    asn1, acn = c.input_bytes(unit, defaults.test_root)
    # Value-assignment tests must not mask absent automatically synthesized tests.
    asn1, removed = re.subn(rb"(?m)^\s*pdu1 MyPDU ::= NULL[^\n]*", b"", asn1)
    assert removed == 1, "NULL fixture no longer has the expected value assignment"
    with tempfile.TemporaryDirectory(prefix="null-codecs-") as temp:
        root = args.outdir.resolve() if args.outdir else Path(temp)
        if args.outdir:
            root.mkdir(parents=True, exist_ok=False)
        modes = [(v2, slim, selected, atcs) for v2 in (False, True)
                 for slim in (False, True) for selected in (False, True)
                 for atcs in ((True, False) if selected else (True,))]
        for v2, slim, selected, atcs in modes:
            defaults.acn_v2, defaults.slim = v2, slim
            mode = f"{'v2' if v2 else 'legacy'}-{'slim' if slim else 'normal'}"
            if selected:
                mode += '-selected-alias'
            if not atcs:
                mode += '-no-atcs'
            work = root / mode
            logs = work / "logs"
            logs.mkdir(parents=True)
            source = asn1.replace(b'MyPDU ::= NULL', b'BaseNull ::= NULL\nMyPDU ::= BaseNull') if selected else asn1
            (work / "sample1.asn1").write_bytes(source)
            (work / "sample1.acn").write_bytes(acn)
            steps = []
            try:
                flags = [f for f in c.compiler_flags(defaults) if atcs or f != '-atc']
                c.run_step([str(defaults.compiler), *flags,
                            *(["-icdPdus", "MyPDU"] if selected else []),
                            "-o", str(work), "sample1.asn1", "sample1.acn"],
                           work, logs, "compile", 120, steps, strict_stderr=True)
                activation = 'direct driver only'
                if atcs:
                    c.run_step(c.ada_build_command(2), work, logs, "build-atcs", 120, steps)
                    output = c.run_step([str(work / "obj_x86/coverage/mainprogram")],
                                        work, logs, "run-atcs", 30, steps, strict_stderr=True)
                    count = 4 if selected else 2
                    assert f"All test cases ({count}) run successfully." in output, output
                    activation = f'{count} automatic tests'
                shutil.copy2(Path(__file__).with_name("null_codecs.adb"), work)
                c.run_step(["gprbuild", "-P", "asn1_x86.gpr", "null_codecs.adb",
                            "--subdirs=null-driver", "-j2", "-cargs", "-gnat2012",
                            "-g", "-O0", "-gnatwa", "-gnatwe"],
                           work, logs, "build-driver", 120, steps)
                output = c.run_step([str(work / "obj_x86/null-driver/null_codecs")],
                                    work, logs, "run-driver", 30, steps, strict_stderr=True)
                print(f"{mode}: {activation}; {output.strip()}")
            except c.StageError as error:
                print(f"{mode}: {error.stage}: {error.detail}")
                return 1
            finally:
                c.write_json(work / "steps.json", steps)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
