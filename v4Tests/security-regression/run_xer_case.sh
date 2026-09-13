#!/usr/bin/env bash
set -euo pipefail
case_dir="$1"
shift
cd "$case_dir"
mkdir -p c_out
"${ASN1SCC:-asn1scc}" -XER -c -o c_out a.asn
gcc -std=c99 -g -O0 -DNDEBUG -Wall -Wextra -Werror \
    -fsanitize=address,undefined -fno-sanitize-recover=all -fno-omit-frame-pointer \
    -fno-pie -no-pie -Ic_out regression_test.c \
    c_out/a.c c_out/asn1crt.c c_out/asn1crt_encoding.c c_out/asn1crt_encoding_xer.c \
    -o c_out/regression_test
for test in "$@"; do timeout 20s c_out/regression_test "$test"; done
