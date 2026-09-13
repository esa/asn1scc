#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
case_dir="$PWD"
repo_root="$(cd ../../.. && pwd)"
rtl_dir="${RTL_DIR:-$repo_root/asn1crt}"
mkdir -p build
flags=(-std=c99 -g -O0 -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=all -fno-omit-frame-pointer -fno-pie -no-pie)
sources=("$rtl_dir/asn1crt.c" "$rtl_dir/asn1crt_encoding.c" "$rtl_dir/asn1crt_encoding_acn.c" "$rtl_dir/asn1crt_encoding_uper.c" "$rtl_dir/asn1crt_encoding_xer.c" "$rtl_dir/asn1crt_encoding_ber.c")
tests=(bits byte-arrays octet-status signed ascii bcd narrowing acn-string xer-bits xer-octets xer-scalars xer-output oid real ber)
for word_size in 8 4; do
    for mode in debug release; do
        extra=()
        if [[ "$mode" == release ]]; then extra+=(-DNDEBUG); fi
        exe="build/runtime-$word_size-$mode"
        gcc "${flags[@]}" "${extra[@]}" -DWORD_SIZE="$word_size" -DFP_WORD_SIZE="$word_size" -I"$rtl_dir" runtime_tests.c "${sources[@]}" -o "$exe"
        for test in "${tests[@]}"; do timeout 20s "$exe" "$test"; done
        gcc "${flags[@]}" "${extra[@]}" -DWORD_SIZE="$word_size" -DFP_WORD_SIZE="$word_size" -DASN1SCC_STREAMING -I"$rtl_dir" streaming_tests.c "$rtl_dir/asn1crt.c" "$rtl_dir/asn1crt_encoding.c" -o "$exe-streaming"
        timeout 20s "$exe-streaming"
    done
done
if [[ "${1:-}" == --direct-only ]]; then exit 0; fi
compiler="${ASN1SCC:-asn1scc}"
for mode in legacy slim v2; do
    out="$case_dir/build/generated-$mode"
    mkdir -p "$out"
    extra=()
    if [[ "$mode" == slim ]]; then extra+=(-slim); fi
    if [[ "$mode" == v2 ]]; then extra+=(--acn-v2); fi
    "$compiler" -c -uPER -ACN -XER -atc "${extra[@]}" -o "$out" a.asn a.acn
    gcc "${flags[@]}" -DNDEBUG -I"$out" generated_tests.c "$out/a.c" "$out/asn1crt.c" "$out/asn1crt_encoding.c" "$out/asn1crt_encoding_uper.c" "$out/asn1crt_encoding_acn.c" "$out/asn1crt_encoding_xer.c" -o "build/generated-$mode-tests"
    for test in xer-bits xer-scalars binary-bounds acn-string xer-real; do
        timeout 20s "build/generated-$mode-tests" "$test"
    done
done
