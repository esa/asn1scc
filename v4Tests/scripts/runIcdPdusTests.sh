#!/bin/sh
# -icdPdus generates the functions of the listed PDUs and of what they call.
# Each test-cases/icd-pdus/<n>.asn1 names its PDUs in its first line
# ("-- -icdPdus A,B: ..."); <n>_test.c must compile and link against the
# generated C with them only.
# ASN1SCC overrides the compiler (executable or .dll), as for runTests.py.

set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cases=$root/test-cases/icd-pdus
compiler=${ASN1SCC:-$root/../asn1scc/bin/Debug/net10.0/asn1scc}
work=$(mktemp -d "${TMPDIR:-/tmp}/asn1scc-icdpdus.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM

run_compiler() {
    case $compiler in
        *.dll) dotnet "$compiler" "$@" ;;
        *) "$compiler" "$@" ;;
    esac
}

for asn1 in "$cases"/*.asn1; do
    n=$(basename "$asn1" .asn1)
    pdus=$(sed -n '1s/^-- -icdPdus \([^:]*\):.*/\1/p' "$asn1")
    out=$work/$n
    mkdir "$out"
    run_compiler -c -icdPdus "$pdus" -o "$out" "$asn1" >/dev/null
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -I "$out" \
        "$out"/*.c "$cases/${n}_test.c" -o "$out/test"
    "$out/test"
    echo "icdPdus $n OK"
done

echo "icdPdus checks passed"
