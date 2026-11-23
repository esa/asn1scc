#!/usr/bin/env bash
# Ada uPER: decoders of a fragmented length (OCTET / BIT STRING CONTAINING with contents above 64K units).
# Generate the Ada uPER code, build the tests once with run-time checks and once with checks suppressed (-gnatp),
# and run them.
set -euo pipefail
cd "$(dirname "$0")"
case_dir="$PWD"
compiler="${ASN1SCC:-asn1scc}"
mkdir -p build
status=0
out="$case_dir/build/generated"
rm -rf "$out"
mkdir -p "$out"
"$compiler" -Ada -uPER -o "$out" a.asn
for checks in on off; do
    obj="$case_dir/build/obj-$checks"
    rm -rf "$obj"
    mkdir -p "$obj"
    flags=(-gnat2012 -g)
    if [[ "$checks" == off ]]; then flags+=(-gnatp); fi
    (cd "$obj" && gnatmake -q "${flags[@]}" -I"$case_dir" -I"$out" generated_tests.adb -o "$case_dir/build/tests-checks-$checks")
    echo "== checks $checks"
    (ulimit -s unlimited; timeout 60s "build/tests-checks-$checks") || status=1
done
exit $status
