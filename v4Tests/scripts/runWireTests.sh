#!/bin/sh
# acn-v2 checks that the -atc round trip cannot make (C only):
#  - byte-exact wire checks: a wrong but self-consistent encoding (e.g. an
#    unmapped length) survives encode+decode, so <n>_wire_test.c asserts the
#    encoded bytes;
#  - the warning printed for types that export a determinant to their parent.
# Test inputs: v4Tests/test-cases/acn/25-ACNV2-BOUNDARIES.
# ASN1SCC overrides the compiler (executable or .dll), as for runTests.py.

set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cases=$root/test-cases/acn/25-ACNV2-BOUNDARIES
compiler=${ASN1SCC:-$root/../asn1scc/bin/Debug/net10.0/asn1scc}
work=$(mktemp -d "${TMPDIR:-/tmp}/asn1scc-wire.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM

run_compiler() {
    case $compiler in
        *.dll) dotnet "$compiler" "$@" ;;
        *) "$compiler" "$@" ;;
    esac
}

for n in 004 010 011; do
    out=$work/$n
    mkdir "$out"
    run_compiler -c -ACN --acn-v2 -fp AUTO -equal -o "$out" "$cases/$n.asn1" "$cases/$n.acn"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -I "$out" \
        "$out"/*.c "$cases/$n.helpers"/*.c "$cases/${n}_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "wire $n OK"
done

expect_warning() {
    n=$1
    text=$2
    out=$work/warn$n
    mkdir "$out"
    run_compiler -c -ACN --acn-v2 -fp AUTO -o "$out" "$cases/$n.asn1" "$cases/$n.acn" 2>"$out/stderr.txt"
    if ! grep -qF "$text" "$out/stderr.txt"; then
        echo "warning check $n FAILED: expected '$text', got:"
        cat "$out/stderr.txt"
        exit 1
    fi
    echo "warning $n OK"
}

expect_warning 001 "001.acn:9:4: warning: ACN field 'more' determines a field outside type 'Byte'; with --acn-v2 its value is determined by the enclosing type, so no standalone ACN encoder/decoder is generated for 'Byte'."
expect_warning 006 "006.acn:4:4: warning: ACN field 'b.more' determines a field outside type 'Outer'"

echo "acn-v2 wire checks passed"
