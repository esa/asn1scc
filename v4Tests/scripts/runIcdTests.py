#!/usr/bin/env python3
"""ICD regression tests (roadmap E2/E3/E4 of q/details-docs/icd-analysis.md).

For every test-case directory under v4Tests/icd-tests/NN-<name>/ this script
compiles the grammar with C output + -icdAcn + -icdRaw + -x (AST xml) into a
temporary directory and then runs:

  1. golden          - byte diff of the -icdRaw JSON against the committed
                       golden file <name>.icd.json (unified diff on mismatch)
  2. json-structure  - navOrder/table ids consistent; every type-reference row
                       resolves to an emitted table
  3. nav-labels      - no two tables render the same nav label (display name)
  4. presence-mask   - presence-mask row width == count of OPTIONAL children
                       without ACN present-when (from the AST xml)
  5. choice-alts     - every CHOICE table lists exactly the non-AlwaysAbsent
                       alternatives of the ASN.1 CHOICE (from the AST xml)
  6. c-macros        - table max bytes == <TAS>_REQUIRED_BYTES_FOR_ACN_ENCODING
                       parsed from the generated C headers (where the TAS name
                       matches), and BYTES == ceil(BITS/8)
  7. tas-coverage    - every TAS that received REQUIRED_* macros in the C
                       headers has an ICD table (catches zero-bit dropout)
  8. html-anchors    - every href="#..." in the _new.html resolves to an
                       anchor in the same file
  9. v2-parity       - (opt-in, marker file 'acn-v2-parity.marker') the -icdRaw
                       JSON produced with --acn-v2 (deferred patching / closure
                       conversion) is byte-identical to the legacy one, so the
                       internal transform does not leak into the ICD (roadmap
                       B8).  Skipped for test dirs without the marker.
 10. mapping-fns     - the ACN mapping-functions of the AST xml are those named
                       by the row notes of the -icdRaw JSON
 11. generator       - contrib/icd-generator.py renders the case (HTML from
                       the grammar and from the dump, Excel with openpyxl)
 12. units           - every value row carries the unit of measure of its
                       ASN.1 node (the use-site unit first), and length /
                       presence / choice-index / padding rows carry none

Some checks FAIL today because of documented bugs (icd-analysis par.2: R1
zero-bit dropout, R2 presence mask, R6 dead anchors, R7 duplicate names). A
test dir may contain an 'xfails.txt' file marking such expected failures,
annotated with the roadmap id; the suite is green while they fail and turns
red (XPASS) once the bug is fixed, forcing the fixer to remove the marker and
update the golden.

Fail-fast like runTests.py: the first unexpected failure stops the run.
Usage details: v4Tests/icd-tests/README.md
"""

import argparse
import difflib
import importlib.util
import math
import os
import re
import shutil
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
V4TESTS_DIR = SCRIPT_DIR.parent
TESTS_DIR = V4TESTS_DIR / "icd-tests"
DEFAULT_COMPILER = V4TESTS_DIR.parent / "asn1scc" / "bin" / "Debug" / "net10.0" / "asn1scc"

CHECK_NAMES = [
    "golden", "json-structure", "nav-labels", "presence-mask",
    "choice-alts", "c-macros", "tas-coverage", "html-anchors",
    "v2-parity", "mapping-fns", "generator", "units",
]

# The HTML / Excel ICD generator (contrib/icd-generator.py) is run on every
# test case by the 'generator' check.
ICD_GENERATOR = V4TESTS_DIR.parent / "contrib" / "icd-generator.py"

# Row comment that asn1scc puts on an INTEGER with an ACN mapping-function
# (BackendAst/Acn/AcnPrimitives.fs, icdMappingFunctionNote).
MAPPING_NOTE_RE = re.compile(r"^mapping-function ([\w.]+): (.*)$")
MAPPING_NO_BITS = "occupies no bits on the wire"

# A test dir opts into the legacy-vs-v2 ICD comparison by containing this
# (empty) marker file.  --acn-v2 is C-only, so only grammars that compile in
# both modes should carry it.
V2_PARITY_MARKER = "acn-v2-parity.marker"

RED, GREEN, YELLOW, RESET = "\033[31m", "\033[32m", "\033[93m", "\033[0m"


def load_json(path):
    import json
    with open(path, encoding="utf-8") as f:
        return json.load(f)


# ---------------------------------------------------------------------------
# AST xml helpers (asn1scc -x output)
# ---------------------------------------------------------------------------

def parse_ast_xml(path):
    return ET.parse(path).getroot()


def find_concrete_nodes(ast_root, usage_path, kind_tag):
    """All <kind_tag> elements that are the concrete type at 'usage_path'.

    A reference chain repeats the same id on nested Asn1Type elements, so the
    concrete node is any direct <kind_tag> child of an Asn1Type carrying the id.
    """
    nodes = []
    for asn1_type in ast_root.iter("Asn1Type"):
        if asn1_type.get("id") != usage_path:
            continue
        nodes.extend(asn1_type.findall(kind_tag))
    return nodes


def tas_entries(ast_root):
    """Yield (moduleName, tasName, cName) for every type assignment."""
    for module in ast_root.iter("Module"):
        mod_name = module.get("Name")
        for tas in module.iter("TypeAssignment"):
            yield (mod_name, tas.get("Name"), tas.get("CName"))


def uper_optional_count(seq_node):
    """Direct children of a SEQUENCE covered by the uPER presence bit mask:
    OPTIONAL (or DEFAULT) components WITHOUT an ACN present-when."""
    count = 0
    for comp in seq_node.findall("SEQUENCE_COMPONENT"):
        if comp.get("present-when") is not None:
            continue
        is_optional = comp.get("Optional") == "true" or comp.find("Default") is not None
        if comp.get("ALWAYS-ABSENT") == "true" or comp.get("ALWAYS-PRESENT") == "true":
            is_optional = False
        if is_optional:
            count += 1
    return count


def choice_alternatives(choice_node):
    """Names of the non-AlwaysAbsent alternatives of a CHOICE node."""
    return [alt.get("Name")
            for alt in choice_node.findall("CHOICE_ALTERNATIVE")
            if alt.get("ALWAYS-ABSENT") != "true"]


# ---------------------------------------------------------------------------
# The individual checks: each returns a list of error strings (empty = pass)
# ---------------------------------------------------------------------------

def check_golden(golden_path, generated_path, update_goldens):
    generated = generated_path.read_text(encoding="utf-8")
    if update_goldens:
        old = golden_path.read_text(encoding="utf-8") if golden_path.exists() else None
        golden_path.write_text(generated, encoding="utf-8")
        if old is None:
            print(f"    golden created: {golden_path.name}")
        elif old != generated:
            print(f"    golden updated: {golden_path.name}")
        return []
    if not golden_path.exists():
        return [f"golden file {golden_path} does not exist "
                f"(run with --update-goldens to create it)"]
    golden = golden_path.read_text(encoding="utf-8")
    if golden == generated:
        return []
    diff = difflib.unified_diff(
        golden.splitlines(keepends=True), generated.splitlines(keepends=True),
        fromfile=f"golden/{golden_path.name}", tofile=f"generated/{golden_path.name}")
    return ["golden mismatch:\n" + "".join(diff)]


def check_json_structure(icd):
    errors = []
    table_ids = [t["id"] for t in icd["tables"]]
    dup = {i for i in table_ids if table_ids.count(i) > 1}
    if dup:
        errors.append(f"duplicate table ids: {sorted(dup)}")
    if icd["navOrder"] != table_ids:
        errors.append(f"navOrder does not match the emitted tables:\n"
                      f"  navOrder: {icd['navOrder']}\n  tables:   {table_ids}")
    id_set = set(table_ids)
    for table in icd["tables"]:
        for row in table["rows"]:
            stype = row["sType"]
            if stype["kind"] != "reference":
                continue
            if stype["id"] is None:
                errors.append(f"table '{table['id']}' row '{row['fieldName']}': "
                              f"dangling type reference (null id)")
            elif stype["id"] not in id_set:
                errors.append(f"table '{table['id']}' row '{row['fieldName']}': "
                              f"reference to unknown table '{stype['id']}'")
    return errors


def check_nav_labels(icd):
    seen = {}
    for table in icd["tables"]:
        seen.setdefault(table["name"], []).append(table["id"])
    return [f"duplicate nav label '{name}' used by tables: {ids}"
            for name, ids in sorted(seen.items()) if len(ids) > 1]


PRESENCE_MASK_FIELD = "Presence Mask"

# Rows synthesized by the ACN encoders in addition to the ASN.1 fields
# (choice determinant, embedded length determinants and termination patterns
# of sizeable alternatives, uPER presence mask).  Their capitalized names can
# never collide with ASN.1 alternative names, which start with a lowercase
# letter.
SYNTHETIC_ROW_FIELDS = {"ChoiceIndex", "Length", "Terminator", PRESENCE_MASK_FIELD}


def check_presence_mask(icd, ast_root):
    errors = []
    for table in icd["tables"]:
        if table["kind"] != "SEQUENCE":
            continue
        seq_nodes = find_concrete_nodes(ast_root, table["usagePath"], "SEQUENCE")
        if not seq_nodes:
            continue  # e.g. a CONTAINING table: usage path is the OCTET STRING
        expected = uper_optional_count(seq_nodes[0])
        mask_rows = [r for r in table["rows"] if r["fieldName"] == PRESENCE_MASK_FIELD]
        actual = sum(r["maxLengthInBits"] for r in mask_rows)
        if actual != expected:
            errors.append(
                f"table '{table['id']}': presence mask is {actual} bit(s) but the "
                f"type has {expected} OPTIONAL child(ren) without present-when")
    return errors


def check_choice_alts(icd, ast_root):
    errors = []
    for table in icd["tables"]:
        if table["kind"] != "CHOICE":
            continue
        choice_nodes = find_concrete_nodes(ast_root, table["usagePath"], "CHOICE")
        if not choice_nodes:
            errors.append(f"table '{table['id']}': cannot resolve usage path "
                          f"'{table['usagePath']}' to a CHOICE in the AST xml")
            continue
        expected = set(choice_alternatives(choice_nodes[0]))
        actual = {r["fieldName"] for r in table["rows"]
                  if r["rowType"] not in ("ThreeDOTs", "PaddingRow")
                  and r["fieldName"] not in SYNTHETIC_ROW_FIELDS}
        missing = expected - actual
        unexpected = actual - expected
        if missing:
            errors.append(f"table '{table['id']}': CHOICE alternatives missing "
                          f"from the table: {sorted(missing)}")
        if unexpected:
            errors.append(f"table '{table['id']}': rows that are not alternatives "
                          f"of the ASN.1 CHOICE: {sorted(unexpected)}")
    return errors


MACRO_RE = re.compile(
    r"#define\s+(\w+)_REQUIRED_(BYTES|BITS)_FOR_ACN_ENCODING\s+(\d+)")


def parse_required_macros(temp_dir):
    """{cName: {'BYTES': n, 'BITS': n}} from the generated (non-RTL) headers."""
    macros = {}
    for header in sorted(temp_dir.glob("*.h")):
        if header.name.startswith("asn1crt"):
            continue
        for m in MACRO_RE.finditer(header.read_text(encoding="utf-8", errors="replace")):
            macros.setdefault(m.group(1), {})[m.group(2)] = int(m.group(3))
    return macros


def tables_by_tas(icd):
    return {(t["tasInfo"]["modName"], t["tasInfo"]["tasName"]): t
            for t in icd["tables"] if t["tasInfo"] is not None}


def check_c_macros(icd, ast_root, macros):
    errors = []
    cname_of = {(mod, name): cname for mod, name, cname in tas_entries(ast_root)}
    for (mod, name), table in sorted(tables_by_tas(icd).items()):
        cname = cname_of.get((mod, name))
        if cname is None or cname not in macros:
            errors.append(f"table '{table['id']}': no REQUIRED_* macros found "
                          f"in the generated headers for TAS {mod}.{name}")
            continue
        tas_macros = macros[cname]
        if "BYTES" in tas_macros and "BITS" in tas_macros:
            if tas_macros["BYTES"] != math.ceil(tas_macros["BITS"] / 8):
                errors.append(f"{cname}: REQUIRED_BYTES {tas_macros['BYTES']} != "
                              f"ceil(REQUIRED_BITS {tas_macros['BITS']} / 8)")
        if "BYTES" in tas_macros and table["maxLengthInBytes"] != tas_macros["BYTES"]:
            errors.append(
                f"table '{table['id']}': max {table['maxLengthInBytes']} byte(s) but "
                f"C header says {cname}_REQUIRED_BYTES_FOR_ACN_ENCODING = "
                f"{tas_macros['BYTES']}")
        if table["minLengthInBytes"] > table["maxLengthInBytes"]:
            errors.append(f"table '{table['id']}': min bytes > max bytes")
    return errors


def check_tas_coverage(icd, ast_root, macros):
    errors = []
    tas_tables = tables_by_tas(icd)
    for mod, name, cname in tas_entries(ast_root):
        if cname not in macros:
            continue
        if (mod, name) not in tas_tables:
            errors.append(f"TAS {mod}.{name} has C encoding macros "
                          f"({cname}_REQUIRED_*) but no ICD table")
    return errors


HREF_RE = re.compile(r'href="#([^"]+)"')
ANCHOR_RE = re.compile(r'(?:\bid|\bname)="([^"]+)"')


def check_html_anchors(html_path):
    html = html_path.read_text(encoding="utf-8", errors="replace")
    anchors = set(ANCHOR_RE.findall(html))
    missing = {}
    for target in HREF_RE.findall(html):
        if target not in anchors:
            missing[target] = missing.get(target, 0) + 1
    return [f"{html_path.name}: href '#{t}' ({n} occurrence(s)) has no matching anchor"
            for t, n in sorted(missing.items())]


def mapping_notes(icd):
    """(table id, field name, function, note text, max bits) of every row that
    carries the mapping-function comment."""
    notes = []
    for table in icd.get("tables", []):
        for row in table.get("rows", []):
            for comment in row.get("comments") or []:
                m = MAPPING_NOTE_RE.match(comment)
                if m:
                    notes.append((table.get("id", "?"), row.get("fieldName", ""),
                                  m.group(1), m.group(2), row.get("maxLengthInBits")))
    return notes


def check_mapping_fns(icd, ast_root):
    """The mapping-functions of the -x AST (attribute on INTEGER and on ACN
    INTEGER fields) are exactly those named by the -icdRaw row notes, and the
    note says 'no bits on the wire' exactly for the rows of size 0."""
    errors = []
    in_xml = {e.get("mapping-function") for e in ast_root.iter()
              if e.get("mapping-function")}
    notes = mapping_notes(icd)
    in_icd = {fn for _, _, fn, _, _ in notes}
    for fn in sorted(in_xml - in_icd):
        errors.append(f"mapping-function '{fn}' is in the AST xml but on no ICD row")
    for fn in sorted(in_icd - in_xml):
        errors.append(f"mapping-function '{fn}' is on an ICD row but not in the AST xml")
    for table_id, field, fn, text, max_bits in notes:
        if (MAPPING_NO_BITS in text) != (str(max_bits) == "0"):
            errors.append(f"{table_id} / {field}: note of '{fn}' does not match its "
                          f"size ({max_bits} bits): '{text}'")
    return errors


def xml_units(ast_root):
    """Unit of measure by node id. A reference repeats its id on the nested
    node of the referenced type; the outer node comes first in document order
    and carries the unit written at the use site ('a Angle --{deg}--'), which
    wins over the referenced type's own unit."""
    units = {}
    for node in ast_root.iter("Asn1Type"):
        unit = (node.get("unitsOfMeasure") or "").strip()
        if unit and node.get("id"):
            units.setdefault(node.get("id"), unit)
    return units


def value_row_node_id(usage_path, row):
    """Id of the ASN.1 node of a value row of the table at 'usage_path':
    a type's own single row (field name '') is the type itself, a field 'f'
    is 'usage_path.f', an element row ('f[k]' collapsed into the parent,
    'Item #k' in the SEQUENCE OF's own table) is the element node '.#'."""
    field = row.get("fieldName", "")
    if row.get("rowType") == "SubItemRow":
        m = re.match(r"^(.*)\[\d+\]$", field)
        return f"{usage_path}.{m.group(1)}.#" if m else None
    if not field:
        return usage_path
    if re.match(r"^Item #\d+$", field):
        return f"{usage_path}.#"
    return f"{usage_path}.{field}"


def check_units(icd, ast_root):
    """Every value row (FieldRow, SubItemRow) has the unit of its ASN.1 node,
    or none when the node has none; a length / presence / choice-index /
    padding row has no unit (a LengthDeterminantRow that refers to a
    composite type's table may carry the component's unit)."""
    errors = []
    units = xml_units(ast_root)
    for table in icd.get("tables", []):
        usage_path = table.get("usagePath", "")
        for row in table.get("rows", []):
            row_type = row.get("rowType")
            got = (row.get("sUnits") or "").strip()
            where = f"{table.get('name', '?')} / {row.get('fieldName', '')!r}"
            if row_type in ("FieldRow", "SubItemRow"):
                node_id = value_row_node_id(usage_path, row)
                expected = units.get(node_id, "") if node_id else ""
                if got != expected:
                    errors.append(f"{where}: unit {got!r}, expected {expected!r} "
                                  f"(AST node {node_id})")
            elif (row.get("sType") or {}).get("kind") != "reference" and got:
                errors.append(f"{where}: {row_type} carries the unit {got!r}")
    return errors


def run_icd_generator(compiler, out_path, out_type, inputs):
    cmd = [sys.executable, "-I", str(ICD_GENERATOR), "-t", out_type,
           "-o", str(out_path)] + [str(f) for f in inputs]
    env = dict(os.environ, ASN1SCC=str(compiler))
    proc = subprocess.run(cmd, capture_output=True, text=True, env=env)
    if proc.returncode != 0 or not out_path.exists():
        return [f"icd-generator.py failed (exit {proc.returncode}): {' '.join(cmd)}\n"
                f"{proc.stdout}{proc.stderr}".rstrip()]
    return []


def check_generator(compiler, asn1_files, acn_files, json_path, icd, temp_dir,
                    require_excel):
    """contrib/icd-generator.py renders the test case: HTML from the grammar
    and from the -icdRaw dump, and Excel when openpyxl is installed.  The HTML
    must have no dead internal link and must show every mapping-function."""
    errors = []
    html_path = temp_dir / "generator.html"
    errors += run_icd_generator(compiler, html_path, "html", asn1_files + acn_files)
    if not errors:
        errors += check_html_anchors(html_path)
        html = html_path.read_text(encoding="utf-8", errors="replace")
        for fn in sorted({fn for _, _, fn, _, _ in mapping_notes(icd)}):
            if fn not in html:
                errors.append(f"generator.html does not show mapping-function '{fn}'")
    errors += run_icd_generator(compiler, temp_dir / "generator-from-json.html", "html",
                                [json_path])
    if importlib.util.find_spec("openpyxl") is not None:
        errors += run_icd_generator(compiler, temp_dir / "generator.xlsx", "excel",
                                    asn1_files + acn_files)
    elif require_excel:
        errors.append("openpyxl is not installed: Excel output cannot be checked "
                      "(--require-excel)")
    else:
        print("    note: openpyxl not installed, Excel output of the generator not checked")
    return errors


def check_v2_parity(test_dir, compiler, asn1_files, acn_files, legacy_json_path,
                    keep_temp):
    """Compile with --acn-v2 and diff the -icdRaw JSON against the legacy one.

    Returns None when the test dir does not carry the opt-in marker (the check
    is then skipped, not reported).  Otherwise returns the usual error list
    ([] on a byte-identical match, a unified diff on mismatch)."""
    if not (test_dir / V2_PARITY_MARKER).exists():
        return None
    name = re.sub(r"^\d+-", "", test_dir.name)
    temp_dir = Path(tempfile.mkdtemp(prefix=f"icd-test-{name}-v2-"))
    json_path = temp_dir / f"{name}.icd.json"
    cmd = [str(compiler), "-c", "-ACN", "--acn-v2",
           "-icdRaw", json_path.name, "-o", "."]
    cmd += [str(f) for f in asn1_files] + [str(f) for f in acn_files]
    proc = subprocess.run(cmd, capture_output=True, text=True, cwd=temp_dir)
    if proc.returncode != 0:
        return [f"--acn-v2 compile failed:\n{' '.join(cmd)}\n{proc.stdout}{proc.stderr}\n"
                f"outputs kept in {temp_dir}"]
    legacy = legacy_json_path.read_text(encoding="utf-8")
    v2 = json_path.read_text(encoding="utf-8")
    if legacy == v2:
        if not keep_temp:
            shutil.rmtree(temp_dir, ignore_errors=True)
        return []
    diff = difflib.unified_diff(
        legacy.splitlines(keepends=True), v2.splitlines(keepends=True),
        fromfile="legacy/-icdRaw", tofile="acn-v2/-icdRaw")
    return [f"--acn-v2 -icdRaw differs from legacy (outputs kept in {temp_dir}):\n"
            + "".join(diff)]


# ---------------------------------------------------------------------------
# xfail markers
# ---------------------------------------------------------------------------

def load_xfails(test_dir):
    """xfails.txt lines: '<check-name> <roadmap-id> [free-text reason...]'."""
    xfails = {}
    xfail_file = test_dir / "xfails.txt"
    if not xfail_file.exists():
        return xfails
    for lineno, line in enumerate(xfail_file.read_text(encoding="utf-8").splitlines(), 1):
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split(None, 2)
        if len(parts) < 2 or parts[0] not in CHECK_NAMES:
            sys.exit(f"{xfail_file}:{lineno}: expected '<check-name> <roadmap-id> "
                     f"[reason]' with check-name one of {CHECK_NAMES}, got: {line}")
        xfails[parts[0]] = (parts[1], parts[2] if len(parts) > 2 else "")
    return xfails


# ---------------------------------------------------------------------------
# Test-case driver
# ---------------------------------------------------------------------------

def run_test_case(test_dir, compiler, update_goldens, keep_temp, require_excel):
    name = re.sub(r"^\d+-", "", test_dir.name)
    asn1_files = sorted(test_dir.glob("*.asn1"))
    acn_files = sorted(test_dir.glob("*.acn"))
    if not asn1_files:
        sys.exit(f"{test_dir}: no .asn1 files")
    xfails = load_xfails(test_dir)

    temp_dir = Path(tempfile.mkdtemp(prefix=f"icd-test-{name}-"))
    json_path = temp_dir / f"{name}.icd.json"
    html_path = temp_dir / "icd.html"
    new_html_path = temp_dir / "icd_new.html"
    ast_path = temp_dir / "ast.xml"

    # Output paths are relative and the compiler runs with cwd=temp_dir: the
    # -x export prepends 'backend_' to the given path (breaks with absolute
    # paths).
    cmd = [str(compiler), "-c", "-ACN",
           "-icdAcn", html_path.name,
           "-icdRaw", json_path.name,
           "-x", ast_path.name,
           "-o", "."]
    cmd += [str(f) for f in asn1_files] + [str(f) for f in acn_files]
    proc = subprocess.run(cmd, capture_output=True, text=True, cwd=temp_dir)
    if proc.returncode != 0:
        print(f"{RED}FAILED: asn1scc failed for {test_dir.name}{RESET}")
        print(" ".join(cmd))
        print(proc.stdout)
        print(proc.stderr)
        print(f"outputs kept in {temp_dir}")
        sys.exit(1)

    icd = load_json(json_path)
    ast_root = parse_ast_xml(ast_path)
    macros = parse_required_macros(temp_dir)

    results = {
        "golden": check_golden(test_dir / f"{name}.icd.json", json_path, update_goldens),
        "json-structure": check_json_structure(icd),
        "nav-labels": check_nav_labels(icd),
        "presence-mask": check_presence_mask(icd, ast_root),
        "choice-alts": check_choice_alts(icd, ast_root),
        "c-macros": check_c_macros(icd, ast_root, macros),
        "tas-coverage": check_tas_coverage(icd, ast_root, macros),
        "html-anchors": check_html_anchors(new_html_path),
        "v2-parity": check_v2_parity(test_dir, compiler, asn1_files, acn_files,
                                     json_path, keep_temp),
        "mapping-fns": check_mapping_fns(icd, ast_root),
        "generator": check_generator(compiler, asn1_files, acn_files, json_path, icd,
                                     temp_dir, require_excel),
        "units": check_units(icd, ast_root),
    }

    failed = False
    for check in CHECK_NAMES:
        errors = results[check]
        if errors is None:
            continue  # check not applicable to this test dir (e.g. no v2 marker)
        xfail = xfails.get(check)
        if errors and xfail is not None:
            print(f"  {YELLOW}XFAIL{RESET} {check:<15} (expected: {xfail[0]} {xfail[1]})".rstrip())
        elif errors:
            print(f"  {RED}FAIL {RESET} {check}")
            for err in errors:
                print("    " + err.replace("\n", "\n    "))
            failed = True
        elif xfail is not None:
            print(f"  {RED}XPASS{RESET} {check:<15} - the expected failure "
                  f"({xfail[0]}) no longer occurs; remove the marker from "
                  f"{test_dir.name}/xfails.txt and update the golden")
            failed = True
        else:
            print(f"  {GREEN}PASS {RESET} {check}")

    if failed:
        print(f"{RED}FAILED: {test_dir.name}{RESET} (outputs kept in {temp_dir})")
        sys.exit(1)
    if keep_temp:
        print(f"  outputs kept in {temp_dir}")
    else:
        shutil.rmtree(temp_dir, ignore_errors=True)
    print(f"{GREEN}PASSED: {test_dir.name}{RESET}")


def main():
    parser = argparse.ArgumentParser(
        description="Run the ICD golden-file / invariant tests (v4Tests/icd-tests).")
    parser.add_argument("-t", "--test", metavar="NAME",
                        help="run only test dirs whose name contains NAME")
    parser.add_argument("--compiler", type=Path, default=DEFAULT_COMPILER,
                        help=f"asn1scc executable (default: {DEFAULT_COMPILER})")
    parser.add_argument("--update-goldens", action="store_true",
                        help="regenerate the golden .icd.json files from current "
                             "compiler behavior instead of diffing against them")
    parser.add_argument("--keep-temp", action="store_true",
                        help="keep the per-test temporary output directories")
    parser.add_argument("--require-excel", action="store_true",
                        help="fail the 'generator' check when openpyxl is missing "
                             "(by default the Excel output is then not checked)")
    args = parser.parse_args()

    if not args.compiler.exists():
        sys.exit(f"asn1scc not found at {args.compiler} - build the solution first "
                 f"(dotnet build asn1scc.sln) or pass --compiler")
    if not TESTS_DIR.is_dir():
        sys.exit(f"test directory {TESTS_DIR} does not exist")

    test_dirs = sorted(d for d in TESTS_DIR.iterdir()
                       if d.is_dir() and re.match(r"^\d+-", d.name))
    if args.test:
        test_dirs = [d for d in test_dirs if args.test in d.name]
    if not test_dirs:
        sys.exit("no matching test directories")

    for test_dir in test_dirs:
        print(f"{test_dir.name}:")
        run_test_case(test_dir, args.compiler, args.update_goldens, args.keep_temp,
                      args.require_excel)
    print(f"{GREEN}All {len(test_dirs)} ICD test case(s) passed.{RESET}")


if __name__ == "__main__":
    main()
