#!/usr/bin/env python3
"""Consistency of the GNATcoverage exemption markers with Docs/statement-coverage-exemptions.md.

Sources (default): every ASN1SCC-COV-NNN justification in the templates (Stg*/*.stg) and in
the backend (BackendAst/**/*.fs) must be an entry of the document, each entry must be used
by at least one source, and all justifications of one ID must have the same text.

Generated code (--generated DIR...): in every .c / .h / .adb / .ads file the exemption
regions must be well formed (ON then OFF, never nested, none left open) and cite a
documented ID.
"""
import argparse
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "Docs" / "statement-coverage-exemptions.md"
ID = r"ASN1SCC-COV-\d{3}"
JUSTIFICATION = re.compile(r'"(' + ID + r':[^"]*)"')
C_ON = re.compile(r'/\*\s*GNATCOV_EXEMPT_ON\s+"(' + ID + r')[^"]*"\s*\*/')
C_OFF = re.compile(r"/\*\s*GNATCOV_EXEMPT_OFF\s*\*/")
ADA_ON = re.compile(r'pragma\s+Annotate\s*\(\s*Xcov\s*,\s*Exempt_On\s*,\s*"(' + ID + r')[^"]*"\s*\)\s*;', re.I)
ADA_OFF = re.compile(r"pragma\s+Annotate\s*\(\s*Xcov\s*,\s*Exempt_Off\s*\)\s*;", re.I)


def documented_ids(doc=DOC):
    return set(re.findall(r"^## (" + ID + r")\b", doc.read_text(), re.M))


def source_justifications(root=ROOT):
    """{ID: {justification text: [file:line, ...]}} of the templates and the backend."""
    found = {}
    files = sorted(root.glob("Stg*/*.stg")) + sorted((root / "BackendAst").rglob("*.fs"))
    for path in files:
        for number, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
            for text in JUSTIFICATION.findall(line):
                ident = text.split(":", 1)[0]
                found.setdefault(ident, {}).setdefault(text, []).append(f"{path.relative_to(root)}:{number}")
    return found


def check_sources(root=ROOT, doc=DOC):
    errors = []
    documented, found = documented_ids(doc), source_justifications(root)
    for ident in sorted(found.keys() - documented):
        errors.append(f"{ident} is used in {sorted(found[ident].values())[0][0]} but has no entry in {doc.name}")
    for ident in sorted(documented - found.keys()):
        errors.append(f"{ident} is documented but no template or backend source emits it")
    for ident, texts in sorted(found.items()):
        if len(texts) > 1:
            errors.append(f"{ident} has {len(texts)} different justifications: "
                          + "; ".join(f"{where[0]}: {text!r}" for text, where in texts.items()))
    return errors


def check_generated_file(path, documented):
    errors, open_at = [], None
    is_c = path.suffix in {".c", ".h"}
    on, off = (C_ON, C_OFF) if is_c else (ADA_ON, ADA_OFF)
    for number, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
        events = sorted([(m.start(), "on", m[1]) for m in on.finditer(line)] +
                        [(m.start(), "off", None) for m in off.finditer(line)])
        for _, kind, ident in events:
            if kind == "on":
                if open_at:
                    errors.append(f"{path}:{number}: exemption region opened inside the region of line {open_at}")
                if ident not in documented:
                    errors.append(f"{path}:{number}: {ident} has no entry in {DOC.name}")
                open_at = number
            elif open_at is None:
                errors.append(f"{path}:{number}: exemption region closed but not opened")
            else:
                open_at = None
    if open_at:
        errors.append(f"{path}:{open_at}: exemption region never closed")
    return errors


def check_generated(directories, documented):
    errors, regions = [], 0
    for directory in directories:
        for path in sorted(p for p in Path(directory).rglob("*") if p.suffix in {".c", ".h", ".adb", ".ads"}):
            errors += check_generated_file(path, documented)
            text = path.read_text(errors="replace")
            regions += len((C_ON if path.suffix in {".c", ".h"} else ADA_ON).findall(text))
    return errors, regions


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--generated", nargs="*", type=Path, default=[],
                    help="directories of generated code to check instead of the sources")
    args = ap.parse_args(argv)
    documented = documented_ids()
    if args.generated:
        errors, regions = check_generated(args.generated, documented)
        print(f"{regions} exemption regions checked")
    else:
        errors = check_sources()
        print(f"{len(source_justifications())} exemption IDs used, {len(documented)} documented")
    for error in errors:
        print(error, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
