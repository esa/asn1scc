#!/bin/bash
# Generate Python code for one representative grammar per regression-corpus
# category and check the generated type modules for complete public-interface
# type annotations (ASN1SCC-OPER-REQ-005) and for consistent indentation and
# naming conventions (ASN1SCC-OPER-REQ-001).
#
# Usage:
#   ./check_generated_code.sh
#   ASN1SCC_DLL=/path/to/asn1scc.dll ./check_generated_code.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ASN1SCC_DLL="${ASN1SCC_DLL:-$REPO_ROOT/asn1scc/bin/Debug/net10.0/asn1scc.dll}"
CORPUS_DIR="$REPO_ROOT/v4Tests/test-cases"
CHECKER="$SCRIPT_DIR/check_public_type_hints.py"

if [ ! -f "$ASN1SCC_DLL" ]; then
    echo "ERROR: asn1scc.dll not found at $ASN1SCC_DLL (build asn1scc.sln first, or set ASN1SCC_DLL)" >&2
    exit 1
fi

WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

echo "Generating one representative grammar per test-case category into $WORK_DIR ..."
mapfile -t category_dirs < <(find "$CORPUS_DIR" -mindepth 2 -maxdepth 2 -type d ! -iname '*ExpectToFail*' | sort)
gen_failures=0
for category_dir in "${category_dirs[@]}"; do
    grammar="$(find "$category_dir" -iname '*.asn1' | sort | head -1)"
    [ -z "$grammar" ] && continue

    slug="${category_dir#"$CORPUS_DIR"/}"
    out_dir="$WORK_DIR/${slug//\//_}"
    mkdir -p "$out_dir"

    if ! dotnet "$ASN1SCC_DLL" -python -uPER -ACN -typePrefix ASN1SCC_ -renamePolicy 3 -fp AUTO \
            -o "$out_dir" "$grammar" > "$out_dir/gen.log" 2>&1; then
        echo "  GENERATION FAILED: $grammar (see $out_dir/gen.log)" >&2
        gen_failures=$((gen_failures + 1))
    fi
done

if [ "$gen_failures" -gt 0 ]; then
    echo "ERROR: $gen_failures categor(y/ies) failed to generate; see logs above" >&2
    exit 1
fi

mapfile -t asn1src_dirs < <(find "$WORK_DIR" -type d -name asn1src)
if [ "${#asn1src_dirs[@]}" -eq 0 ]; then
    echo "ERROR: no generated asn1src/ directories found - nothing was checked" >&2
    exit 1
fi

echo "Checking public-interface type-hint completeness across ${#asn1src_dirs[@]} generated module(s) ..."
python3 "$CHECKER" --summary "${asn1src_dirs[@]}"

# Indentation (E1, W191) and naming conventions (N) for every identifier the
# codegen itself authors - functions, methods, arguments, globals.
#
# Excluded: N801 (class names), N806 (local variables), N815 (dataclass
# fields), N999 (module/file names) - all four fire on ASN.1-spec-derived
# identifiers
echo "Checking indentation and naming conventions across ${#asn1src_dirs[@]} generated module(s) ..."
uvx ruff check --isolated --select=E1,W191,N --ignore=N801,N806,N815,N999 "${asn1src_dirs[@]}"
