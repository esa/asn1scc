#!/usr/bin/env bash
# ESACERT #74995: generate the C decoders (legacy, slim, --acn-v2, --acn-v2 slim)
# and decode malformed size determinants under AddressSanitizer.
set -euo pipefail
cd "$(dirname "$0")"
case_dir="$PWD"
compiler="${ASN1SCC:-asn1scc}"
flags=(-std=c99 -g -O0 -Wall -Wextra -fsanitize=address,undefined -fno-sanitize-recover=all -fno-omit-frame-pointer)
mkdir -p build
for mode in legacy slim v2 v2-slim; do
    out="$case_dir/build/generated-$mode"
    rm -rf "$out"
    mkdir -p "$out"
    extra=()
    if [[ "$mode" == *slim ]]; then extra+=(-slim); fi
    if [[ "$mode" == v2* ]]; then extra+=(--acn-v2); fi
    "$compiler" -c -ACN "${extra[@]}" -o "$out" a.asn a.acn
    gcc "${flags[@]}" -I"$out" generated_tests.c "$out"/*.c -o "build/generated-$mode-tests"
    echo "== $mode"
    timeout 20s "build/generated-$mode-tests"
done
