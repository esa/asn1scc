#!/usr/bin/env bash
# Issue 85: Ada ACN null-terminated ASCII decoders (IA5String, INTEGER) accepted a stream without the terminator after
# the longest value. Generate the decoders (legacy, --acn-v2), build the Ada tests once with run-time checks and once
# with checks suppressed (-gnatp), build the C tests, and run them. Every case must PASS.
set -euo pipefail
cd "$(dirname "$0")"
case_dir="$PWD"
compiler="${ASN1SCC:-asn1scc}"
mkdir -p build
status=0
for mode in legacy v2; do
    extra=()
    if [[ "$mode" == v2 ]]; then extra+=(--acn-v2); fi
    out="$case_dir/build/generated-$mode"
    rm -rf "$out"; mkdir -p "$out"
    "$compiler" -Ada -ACN "${extra[@]}" -o "$out" a.asn a.acn
    for checks in on off; do
        obj="$case_dir/build/obj-$mode-$checks"
        rm -rf "$obj"; mkdir -p "$obj"
        flags=(-gnat2012 -g)
        if [[ "$checks" == off ]]; then flags+=(-gnatp); fi
        (cd "$obj" && gnatmake -q "${flags[@]}" -I"$case_dir" -I"$out" generated_tests.adb -o "$case_dir/build/generated-$mode-checks-$checks-tests")
        echo "== Ada $mode, checks $checks"
        timeout 20s "build/generated-$mode-checks-$checks-tests" || status=1
    done
    cout="$case_dir/build/generated-c-$mode"
    rm -rf "$cout"; mkdir -p "$cout"
    "$compiler" -c -ACN "${extra[@]}" -o "$cout" a.asn a.acn
    gcc -g -Wall -Wextra -I"$cout" -o "build/generated-c-$mode-tests" generated_tests.c "$cout"/a.c "$cout"/asn1crt*.c
    echo "== C $mode"
    timeout 20s "build/generated-c-$mode-tests" || status=1
done
exit $status
