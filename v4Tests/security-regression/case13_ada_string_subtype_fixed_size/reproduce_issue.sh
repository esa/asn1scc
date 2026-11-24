#!/usr/bin/env bash
# Issue 86: an Ada fixed-size IA5String subtype (ACN encoding ASCII) that shares a longer typedef was encoded with the
# typedef's length. Generate the Ada codecs (legacy, --acn-v2), build the tests once with run-time checks and once with checks
# suppressed (-gnatp), and run them.
set -euo pipefail
cd "$(dirname "$0")"
case_dir="$PWD"
compiler="${ASN1SCC:-asn1scc}"
mkdir -p build
status=0
for mode in legacy v2; do
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
