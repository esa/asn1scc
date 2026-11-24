#!/usr/bin/env bash
# ESACERT #75543: python.stg (-customStg) output is executed by OpenGEODE; ASN.1 string values, file and directory
# names and -typePrefix were placed unescaped in Python string literals.
# Generate the Python AST with the options OpenGEODE uses, from a directory and a file whose names carry Python code,
# then execute it (check.py) and verify that nothing was injected and that every value reads back unchanged.
set -euo pipefail
cd "$(dirname "$0")"
case_dir="$PWD"
compiler="${ASN1SCC:-asn1scc}"
# OpenGEODE uses the python.stg installed next to the compiler
compiler_dir="$(dirname "$(readlink -f "$(command -v "$compiler")")")"
stg="${PYTHON_STG:-$compiler_dir/python.stg}"
xml_stg="${XML_STG:-$compiler_dir/xml.stg}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
# names with quotes cannot exist on Windows file systems: build them in a temporary Linux directory
dir_name='d"+str(__import__("os").system("echo pwned>PROOF_DIR.txt"))+"d'
file_name='f"+str(__import__("os").system("echo pwned>PROOF_FILE.txt"))+"f.asn'
xml_dir='x"&<y'
xml_file="f'<&>\".asn"
mkdir -p "$work/$dir_name" "$work/$xml_dir"
cp dataview.asn "$work/$dir_name/$file_name"
cp xml.asn "$work/$xml_dir/$xml_file"
status=0
generate() {   # generate <output.py> <input file> [extra compiler options]
    local output="$1" input="$2"
    shift 2
    rm -f "$work/$output" "$work"/PROOF*
    (cd "$work" && "$compiler" -customStgAstVersion 4 --field-prefix AUTO -customStg "$stg::$output" -renamePolicy 0 "$@" "$input")
}
echo "== injection: string value, directory and file name"
generate injection.py "$dir_name/$file_name"
(cd "$work" && python3 -I "$case_dir/check.py" injection.py "$dir_name/$file_name" injection) || status=1

echo "== special characters: backslash, quotes, LF, CR, HT, NUL, non-ASCII, DEFAULT value"
cp special.asn "$work/"
generate special.py special.asn
(cd "$work" && python3 -I "$case_dir/check.py" special.py special.asn special) || status=1

echo "== -typePrefix"
printf 'PR DEFINITIONS AUTOMATIC TAGS ::= BEGIN\nT ::= SEQUENCE { a INTEGER (0..5) }\nEND\n' > "$work/prefix.asn"
generate prefix.py prefix.asn -typePrefix 'P"+str(__import__("os").system("echo pwned>PROOF_PREFIX.txt"))+"'
(cd "$work" && python3 -I -c 'import sys; exec(compile(open(sys.argv[1], "rb").read(), sys.argv[1], "exec"), {})' prefix.py) || { echo "FAIL: generated file does not run"; status=1; }
if ls "$work"/PROOF* >/dev/null 2>&1; then echo "FAIL: injected code ran"; status=1; else echo "OK"; fi

echo "== xml.stg: file name in XML attributes, ']]>' in a CDATA string value"
(cd "$work" && "$compiler" -customStgAstVersion 4 -customStg "$xml_stg::out.xml" "$xml_dir/$xml_file")
(cd "$work" && python3 -I "$case_dir/check_xml.py" out.xml "$xml_dir/$xml_file") || status=1
exit $status
