#!/usr/bin/env bash
# OCTET STRING CONTAINING regions shorter than their contents: generate the Ada decoders (legacy, slim, --acn-v2,
# --acn-v2 slim), build the tests once with run-time checks and once with checks suppressed (-gnatp), and run them.
set -euo pipefail
cd "$(dirname "$0")"
case_dir="$PWD"
compiler="${ASN1SCC:-asn1scc}"
mkdir -p build
status=0
for mode in legacy slim v2 v2-slim; do
    out="$case_dir/build/generated-$mode"
    rm -rf "$out"
    mkdir -p "$out"
    extra=()
    if [[ "$mode" == *slim ]]; then extra+=(-slim); fi
    if [[ "$mode" == v2* ]]; then extra+=(--acn-v2); fi
    "$compiler" -Ada -ACN "${extra[@]}" -o "$out" a.asn a.acn
    for checks in on off; do
        obj="$case_dir/build/obj-$mode-$checks"
        rm -rf "$obj"
        mkdir -p "$obj"
        flags=(-gnat2012 -g)
        if [[ "$checks" == off ]]; then flags+=(-gnatp); fi
        (cd "$obj" && gnatmake -q "${flags[@]}" -I"$case_dir" -I"$out" generated_tests.adb -o "$case_dir/build/generated-$mode-checks-$checks-tests")
        echo "== $mode, checks $checks"
        timeout 20s "build/generated-$mode-checks-$checks-tests" || status=1
    done
done
exit $status
