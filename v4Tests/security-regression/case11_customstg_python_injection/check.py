"""Execute the python.stg output like OpenGEODE does (compile + exec), then check that it ran no injected code
(no PROOF* file in the current directory) and that the given variables read back exactly.
Usage: check.py <generated.py> <expected asn1Files entry> injection|special"""
import glob, importlib.util, sys

path, expected_file, what = sys.argv[1], sys.argv[2], sys.argv[3]
source = open(path, 'rb').read()
spec = importlib.util.spec_from_file_location("dataview_ast", path)
module = importlib.util.module_from_spec(spec)
try:
    exec(compile(source, path, 'exec'), module.__dict__)
except SyntaxError as err:
    module = None
    print("FAIL: generated file does not compile:", err)

errors = [f"injected code ran: {glob.glob('PROOF*')}"] if glob.glob('PROOF*') else []
if module is None:
    errors.append("no module")
else:
    if module.asn1Files != [expected_file]:
        errors.append(f"asn1Files = {module.asn1Files!r}, expected [{expected_file!r}]")
    for t in module.types.values():
        if t.type.AsnFile != expected_file:
            errors.append(f"AsnFile = {t.type.AsnFile!r}")
    if what == "injection":
        value = module.variables["evilValue"].value
        if value != 'x"+str(__import__("os").system("echo pwned>PROOF_VALUE.txt"))+"y':
            errors.append(f"evilValue = {value!r}")
    else:
        value = module.variables["specialValue"].value
        if value != 'a\\b\'c"d\n\r\t\x00e-é-😀':
            errors.append(f"specialValue = {value!r}")
        default = module.types["WithDefault"].type.Children["x"].DefaultValue
        if "PROOF_DEFAULT" not in default or not default.startswith('{a "\\"'):
            errors.append(f"DefaultValue = {default!r}")
for e in errors:
    print("FAIL:", e)
print("OK" if not errors else "FAILED")
sys.exit(1 if errors else 0)
