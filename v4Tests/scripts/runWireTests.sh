#!/bin/sh
# acn-v2 checks that the -atc round trip cannot make (C only; the cases that
# are not specific to acn-v2 also run without --acn-v2):
#  - byte-exact wire checks: a wrong but self-consistent encoding (e.g. an
#    unmapped length) survives encode+decode, so <n>_wire_test.c asserts the
#    encoded bytes; for 016 to 018, whose automatic test cases cannot run
#    (NO_AUTOMATIC_TEST_CASES), also the round trip and the rejected values;
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

run_wire_test() {
    n=$1
    mode=$2
    out=$work/$n$mode
    mkdir "$out"
    if [ "$mode" = -legacy ]; then
        run_compiler -c -ACN -fp AUTO -equal -o "$out" "$cases/$n.asn1" "$cases/$n.acn"
    elif [ "$mode" = -ws4 ]; then
        run_compiler -c -ACN -fp AUTO -equal -wordSize 4 -fpWordSize 4 -o "$out" "$cases/$n.asn1" "$cases/$n.acn"
    else
        run_compiler -c -ACN --acn-v2 -fp AUTO -equal -o "$out" "$cases/$n.asn1" "$cases/$n.acn"
    fi
    helpers=
    helperInc=
    if [ -d "$cases/$n.helpers" ]; then
        helpers=$(ls "$cases/$n.helpers"/*.c)
        # headers of the helpers (e.g. the module of the mapping functions, test_mappers.h)
        helperInc="-I $cases/$n.helpers"
    fi
    sanitize=
    [ "$n" = 022 ] && sanitize=-fsanitize=address
    [ "$n" = 023 ] && sanitize="-fsanitize=undefined -fno-sanitize-recover=undefined"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror $sanitize -I "$out" $helperInc \
        "$out"/*.c $helpers "$cases/${n}_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "wire $n$mode OK"
}

for n in 004 010 011 016 017 018 019 020 021 022 023 024 025 028 030 033 035 036; do
    run_wire_test $n ""
done

# The fixes checked by these cases are not specific to --acn-v2.
for n in 019 020 022 023 024 025 030 033 035 036; do
    run_wire_test $n -legacy
done

# 64-bit integer fields with -wordSize 4: sign extension in the upper bytes.
run_wire_test 036 -ws4

# XER: a decoded value outside the constraints of its type is rejected.
out=$work/xer001
mkdir "$out"
run_compiler -c -XER -fp AUTO -equal -o "$out" "$root/test-cases/xer/02-constraints/001.asn1"
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -I "$out" \
    "$out"/*.c "$root/test-cases/xer/02-constraints/001_decode_test.c" -o "$out/decode_test"
"$out/decode_test"
echo "xer 001 OK"

# XER, Ada: a length outside the SIZE of an OCTET / BIT STRING is an error code, not Constraint_Error.
out=$work/xer002
mkdir "$out"
run_compiler -Ada -XER -o "$out" "$root/test-cases/xer/02-constraints/002.asn1"
cp "$root/test-cases/xer/02-constraints/002_decode_test.adb" "$out/decode_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata decode_test.adb && ./decode_test)
echo "xer 002 (Ada) OK"

# Decoding truncated input must not read a deferred determinant's temporary
# that the failed read never wrote. UBSan's bool check reports that read.
out=$work/truncated001
mkdir "$out"
run_compiler -c -ACN --acn-v2 -fp AUTO -o "$out" "$cases/001.asn1" "$cases/001.acn" 2>/dev/null
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror \
    -fsanitize=bool -fno-sanitize-recover=bool -I "$out" \
    "$out"/*.c "$cases/001_truncated_test.c" -o "$out/truncated_test"
"$out/truncated_test"
echo "truncated 001 OK"

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

# Without --acn-v2, an ACN field passed to SEQUENCE OF elements is rejected.
out=$work/legacy016
mkdir "$out"
if run_compiler -c -ACN -o "$out" "$cases/016.asn1" "$cases/016.acn" 2>"$out/stderr.txt"; then
    echo "legacy 016 FAILED: accepted without --acn-v2"
    exit 1
fi
if ! grep -qF "016.acn:8:4: error: ACN field 'len' is passed as an argument to the elements of SEQUENCE OF 'items'. This is supported only with --acn-v2." "$out/stderr.txt"; then
    echo "legacy 016 FAILED, got:"
    cat "$out/stderr.txt"
    exit 1
fi
echo "legacy 016 rejection OK"

echo "acn-v2 wire checks passed"
