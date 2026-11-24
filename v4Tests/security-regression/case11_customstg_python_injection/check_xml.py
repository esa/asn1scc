"""Parse the xml.stg output and check that the file path and the string value read back exactly.
Usage: check_xml.py <generated.xml> <expected file path>"""
import sys
import xml.etree.ElementTree as ET

path, expected_file = sys.argv[1], sys.argv[2]
errors = []
try:
    root = ET.parse(path).getroot()
except ET.ParseError as err:
    root = None
    errors.append(f"not well-formed: {err}")
if root is not None:
    names = [f.get("FileName") for f in root.iter("Asn1File")]
    if names != [expected_file]:
        errors.append(f"FileName = {names!r}, expected [{expected_file!r}]")
    if list(root.iter("Injected")):
        errors.append("injected element found")
    values = {v.get("Name"): "".join(v.find("Value").itertext()) for v in root.iter("VariableAssignment")}
    if values.get("cdataValue") != "a]]><Injected/><![CDATA[b":
        errors.append(f"cdataValue = {values.get('cdataValue')!r}")
for e in errors:
    print("FAIL:", e)
print("OK" if not errors else "FAILED")
sys.exit(1 if errors else 0)
