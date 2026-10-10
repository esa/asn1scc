"""The PUS-C cohort manifest must list exactly the service groups of the interop tests."""
import json
from pathlib import Path
import re
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = Path(__file__).with_name("pusc-units.json")
SERVICE_FILES = ROOT / "PUSCInteropTest/Support/ServiceFiles.cs"
INPUT = ROOT / "PUSCInteropTest/asn1-pusc-lib-asn1CompilerTestInput"


def service_groups(text):
    """Service group -> file list, read from ServiceFiles.cs."""
    methods = dict(re.findall(r"PUS_C_Service\.(\w+)\s*=>\s*(\w+)\(\)", text))
    bodies = {m: re.findall(r'"([^"]+)"', body)
              for m, body in re.findall(r"static string\[\] (\w+)\(\) =>\s*\[(.*?)\];", text, re.S)}
    return {group: bodies[method] for group, method in methods.items()}


class PuscUnits(unittest.TestCase):
    def test_manifest_matches_interop_tests(self):
        expected = service_groups(SERVICE_FILES.read_text(encoding="utf-8-sig"))
        self.assertEqual(json.loads(MANIFEST.read_text())["groups"], expected)

    def test_every_asn1_file_exists(self):
        for group, names in json.loads(MANIFEST.read_text())["groups"].items():
            for name in names:
                self.assertTrue((INPUT / (name + ".asn1")).is_file(), f"{group}: {name}")


if __name__ == "__main__":
    if sys.argv[1:] == ["--write"]:
        groups = service_groups(SERVICE_FILES.read_text(encoding="utf-8-sig"))
        MANIFEST.write_text(json.dumps({
            "source": "PUSCInteropTest/Support/ServiceFiles.cs (N7 Space asn1-pusc-lib 1.1.1)",
            "groups": groups}, indent=1) + "\n")
    else:
        unittest.main()
