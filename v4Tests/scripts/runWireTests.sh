#!/bin/sh
# acn-v2 checks that the -atc round trip cannot make (C only; the cases that
# are not specific to acn-v2 also run without --acn-v2):
#  - byte-exact wire checks: a wrong but self-consistent encoding (e.g. an
#    unmapped length) survives encode+decode, so <n>_wire_test.c asserts the
#    encoded bytes; for 016 to 018 also the round trip and the rejected
#    values (elements that disagree on a shared determinant);
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

for n in 004 010 011 016 017 018 019 020 021 022 023 024 025 028 030 033 035 036 037 038 039; do
    run_wire_test $n ""
done

# The fixes checked by these cases are not specific to --acn-v2.
for n in 019 020 022 023 024 025 030 033 035 036 037; do
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

# XER: <T>_REQUIRED_BYTES_FOR_XER_ENCODING of an ENUMERATED holds its longest item (GitHub #272).
out=$work/xer003
mkdir "$out"
run_compiler -c -XER -o "$out" "$root/test-cases/xer/02-constraints/003.asn1"
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -I "$out" \
    "$out"/*.c "$root/test-cases/xer/02-constraints/003_size_test.c" -o "$out/size_test"
"$out/size_test"
echo "xer 003 OK"

# uPER OCTET / BIT STRING (CONTAINING X): the X.691 general length and the contents padded to whole octets (Issue 94),
# bytes cross-checked with pycrate; C (with ASan / UBSan) and Ada (-gnata: the RTL contracts are checked too).
out=$work/uper001
mkdir "$out"
run_compiler -c -uPER -equal -o "$out" "$root/test-cases/uper-wire/001.asn1"
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/uper-wire/001_wire_test.c" -o "$out/wire_test"
"$out/wire_test"
echo "uper-wire 001 OK"
out=$work/uper001ada
mkdir "$out"
run_compiler -Ada -uPER -equal -o "$out" "$root/test-cases/uper-wire/001.asn1"
cp "$root/test-cases/uper-wire/001_wire_test.adb" "$out/wire_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
echo "uper-wire 001 (Ada) OK"

# ACN INTEGER as ASCII digits with a terminator: a value outside the range of the type, or a digit string beyond the
# integer type, is an error code (Ada used to raise Constraint_Error, and accepted digit strings that wrapped around);
# C, Ada x86 and Ada msp430 (-gnata: the RTL contracts are checked too).
out=$work/acn001
mkdir "$out"
run_compiler -c -ACN -o "$out" "$root/test-cases/acn-wire/001.asn1" "$root/test-cases/acn-wire/001.acn"
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/acn-wire/001_decode_test.c" -o "$out/decode_test"
"$out/decode_test"
echo "acn-wire 001 OK"
out=$work/acn001ada
mkdir "$out"
run_compiler -Ada -ACN -o "$out" "$root/test-cases/acn-wire/001.asn1" "$root/test-cases/acn-wire/001.acn"
cp "$root/test-cases/acn-wire/001_decode_test.adb" "$out/decode_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata decode_test.adb && ./decode_test)
echo "acn-wire 001 (Ada) OK"
out=$work/acn001msp430
mkdir "$out"
run_compiler -Ada -ACN -t msp430 -wordSize 4 -fpWordSize 4 -o "$out" "$root/test-cases/acn-wire/001.asn1" \
    "$root/test-cases/acn-wire/001.acn"
cp "$root/test-cases/acn-wire/001_decode_test.adb" "$out/decode_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata -aIasn1rtl -aIsrc -aIboards/msp430 decode_test.adb && ./decode_test)
echo "acn-wire 001 (Ada msp430) OK"

# ACN fields of a referenced unsigned INTEGER type as ASCII: digits without a sign, as a component of the type
# (q: Issue 98); C (ASan / UBSan) and Ada x86 (-gnata), legacy and acn-v2.
for mode in "" "--acn-v2"; do
    out=$work/acn002c$mode
    mkdir "$out"
    run_compiler -c -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/002.asn1" "$root/test-cases/acn-wire/002.acn"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/acn-wire/002_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "acn-wire 002 $mode OK"
    out=$work/acn002ada$mode
    mkdir "$out"
    run_compiler -Ada -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/002.asn1" "$root/test-cases/acn-wire/002.acn"
    cp "$root/test-cases/acn-wire/002_wire_test.adb" "$out/wire_test.adb"
    (cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
    echo "acn-wire 002 (Ada) $mode OK"
done

# A determinant field inside an OPTIONAL component, its dependent outside it: with the component absent and the
# dependent present the decoders fail with the dependent's error code (they read the field's unset local), and a
# present-when on a BOOLEAN field of an absent component reads FALSE (also in SEQUENCE OF elements); C (ASan / UBSan)
# and Ada x86 (-gnata, 003 also -gnatVa), legacy and acn-v2 (003), acn-v2 only for fields exposed by a referenced
# component (004).
for mode in "" "--acn-v2"; do
    def=
    [ "$mode" = --acn-v2 ] && def=-DWIRE_ACN_V2
    out=$work/acn003c$mode
    mkdir "$out"
    run_compiler -c -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/003.asn1" "$root/test-cases/acn-wire/003.acn"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        $def -I "$out" "$out"/*.c "$root/test-cases/acn-wire/003_decode_test.c" -o "$out/decode_test"
    "$out/decode_test"
    echo "acn-wire 003 $mode OK"
    out=$work/acn003ada$mode
    mkdir "$out"
    run_compiler -Ada -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/003.asn1" "$root/test-cases/acn-wire/003.acn"
    cp "$root/test-cases/acn-wire/003_decode_test.adb" "$out/decode_test.adb"
    # -gnatVa: a decoder that reads a Boolean local it did not set (the test dirties the stack with 2) raises;
    # ACN_V2 selects the mode's expectations (GNAT preprocessor)
    v2=False
    [ "$mode" = --acn-v2 ] && v2=True
    (cd "$out" && gnatmake -q -gnat2012 -gnata -gnatVa -gnateDACN_V2=$v2 decode_test.adb && ./decode_test)
    echo "acn-wire 003 (Ada) $mode OK"
done
out=$work/acn004c
mkdir "$out"
run_compiler -c -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/004.asn1" "$root/test-cases/acn-wire/004.acn" 2>/dev/null
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/acn-wire/004_decode_test.c" -o "$out/decode_test"
"$out/decode_test"
echo "acn-wire 004 --acn-v2 OK"
out=$work/acn004ada
mkdir "$out"
run_compiler -Ada -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/004.asn1" "$root/test-cases/acn-wire/004.acn" 2>/dev/null
cp "$root/test-cases/acn-wire/004_decode_test.adb" "$out/decode_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata decode_test.adb && ./decode_test)
echo "acn-wire 004 (Ada) --acn-v2 OK"

# --acn-v2: a determinant field in a component the stream may say absent, read outside it by a specialised function
# (an ACN argument, a CONTAINING size, a field exposed by a referenced component; also in SEQUENCE OF elements): valid
# values encode to the bytes legacy writes, streams with the component absent and the dependent present are rejected
# (q: Issue 107); C (ASan / UBSan) and Ada x86 (-gnata).
out=$work/acn005c
mkdir "$out"
run_compiler -c -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/005.asn1" "$root/test-cases/acn-wire/005.acn" 2>/dev/null
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/acn-wire/005_wire_test.c" -o "$out/wire_test"
"$out/wire_test"
echo "acn-wire 005 --acn-v2 OK"
out=$work/acn005ada
mkdir "$out"
run_compiler -Ada -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/005.asn1" "$root/test-cases/acn-wire/005.acn" 2>/dev/null
cp "$root/test-cases/acn-wire/005_wire_test.adb" "$out/wire_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
echo "acn-wire 005 (Ada) --acn-v2 OK"

# --acn-v2: an ACN parameter of a referenced type with several consumers inside it (OPTIONAL, mandatory, CHOICE
# alternatives, fixed-size next to variable, determinants, present-when flags): valid values encode to the bytes legacy
# writes with a zero error code, values whose consumers disagree fail with a nonzero code (q: Issue 121); C (ASan /
# UBSan) acn-v2 and legacy (the same bytes; -DLEGACY skips the one value legacy does not encode), Ada x86 (-gnata,
# acn-v2: a false pragma Assume after a patch raises).
for mode in "" "--acn-v2"; do
    legacy=
    [ -z "$mode" ] && legacy=-DLEGACY
    out=$work/acn006c$mode
    mkdir "$out"
    run_compiler -c -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/006.asn1" "$root/test-cases/acn-wire/006.acn" 2>/dev/null
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        $legacy -I "$out" "$out"/*.c "$root/test-cases/acn-wire/006_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "acn-wire 006 $mode OK"
done
out=$work/acn006ada
mkdir "$out"
run_compiler -Ada -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/006.asn1" "$root/test-cases/acn-wire/006.acn" 2>/dev/null
cp "$root/test-cases/acn-wire/006_wire_test.adb" "$out/wire_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
echo "acn-wire 006 (Ada) --acn-v2 OK"

# acn-wire/007 (q/issues.md Issue 124): an ACN parameter of a referenced type consumed inside the elements of a
# SEQUENCE OF of that type: every element patches the determinant (--acn-v2; legacy rejects the shapes, below).
out=$work/acn007c
mkdir "$out"
run_compiler -c -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/007.asn1" "$root/test-cases/acn-wire/007.acn" 2>/dev/null
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/acn-wire/007_wire_test.c" -o "$out/wire_test"
"$out/wire_test"
echo "acn-wire 007 --acn-v2 OK"
out=$work/acn007ada
mkdir "$out"
run_compiler -Ada -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/007.asn1" "$root/test-cases/acn-wire/007.acn" 2>/dev/null
cp "$root/test-cases/acn-wire/007_wire_test.adb" "$out/wire_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
echo "acn-wire 007 (Ada) --acn-v2 OK"

# A determinant consumed inside the elements of a SEQUENCE OF that does not hold it (Issues 122 / 124, cycle i124a):
# legacy rejects every such shape but constant consumers; --acn-v2 rejects a field consumed with no type assignment
# between it and the consumer (no per-element patch for it yet). Grammars in acn-wire/007_reject/.
reject_dir=$root/test-cases/acn-wire/007_reject
expect_rejection() {
    n=$1
    mode=$2
    text=$3
    out=$work/reject007$n$mode
    mkdir "$out"
    if run_compiler -c -ACN $mode -o "$out" "$reject_dir/$n.asn1" "$reject_dir/$n.acn" 2>"$out/stderr.txt"; then
        echo "007_reject $n $mode FAILED: accepted"
        exit 1
    fi
    if ! grep -qF "$text" "$out/stderr.txt"; then
        echo "007_reject $n $mode FAILED, got:"
        cat "$out/stderr.txt"
        exit 1
    fi
    echo "007_reject $n $mode rejection OK"
}
expect_accepted() {
    n=$1
    mode=$2
    out=$work/accept007$n$mode
    mkdir "$out"
    if ! run_compiler -c -ACN $mode -o "$out" "$reject_dir/$n.asn1" "$reject_dir/$n.acn" 2>"$out/stderr.txt"; then
        echo "007_reject $n $mode FAILED: rejected"
        cat "$out/stderr.txt"
        exit 1
    fi
    echo "007_reject $n $mode accepted OK"
}
legacy_only="This is supported only with --acn-v2."
legacy_inline="Legacy ACN does not support this; --acn-v2 supports it when the consumer, or a component that contains it inside the elements, is a reference to a type assignment."
v2_inline="with no type assignment between them. This is not supported yet: make the consumer, or a component that contains it inside the elements, a reference to a type assignment."
expect_rejection size "" "size.acn:2:17: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#'). $legacy_inline"
expect_rejection size --acn-v2 "size.acn:2:17: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#') $v2_inline"
expect_rejection presence "" "presence.acn:2:17: error: ACN field 'f' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#.x'). $legacy_inline"
expect_rejection presence --acn-v2 "presence.acn:2:17: error: ACN field 'f' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#.x') $v2_inline"
expect_rejection choice "" "choice.acn:2:17: error: ACN field 'kk' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#'). $legacy_inline"
expect_rejection choice --acn-v2 "choice.acn:2:17: error: ACN field 'kk' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#') $v2_inline"
expect_rejection ia5 "" "ia5.acn:2:17: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#'). $legacy_inline"
expect_rejection ia5 --acn-v2 "ia5.acn:2:17: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#') $v2_inline"
expect_rejection outer "" "outer.acn:2:22: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#'). $legacy_inline"
expect_rejection outer --acn-v2 "outer.acn:2:22: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#') $v2_inline"
expect_rejection containing "" "containing.acn:2:17: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#'). $legacy_only"
expect_accepted containing --acn-v2
expect_rejection named "" "named.acn:2:17: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#'). $legacy_only"
expect_accepted named --acn-v2
expect_rejection param "" "param.acn:2:15: error: ACN parameter 'len' of type 'P' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#.o'). $legacy_only"
expect_accepted param --acn-v2
expect_rejection argument "" "argument.acn:3:15: error: ACN parameter 'len' of type 'P' is passed as an argument to the elements of SEQUENCE OF 'items' (to 'items.#'). $legacy_only"
expect_accepted argument --acn-v2
expect_rejection nested "" "nested.acn:3:61: error: ACN field 'n' is passed as an argument to the elements of SEQUENCE OF 'items' (to 'items.#.pl'). $legacy_only"
expect_accepted nested --acn-v2
expect_accepted fixed ""
expect_accepted fixed --acn-v2
out=$work/legacy007
mkdir "$out"
if run_compiler -c -ACN -o "$out" "$root/test-cases/acn-wire/007.asn1" "$root/test-cases/acn-wire/007.acn" 2>"$out/stderr.txt"; then
    echo "legacy 007 FAILED: accepted without --acn-v2"
    exit 1
fi
if ! grep -qF "$legacy_only" "$out/stderr.txt"; then
    echo "legacy 007 FAILED, got:"
    cat "$out/stderr.txt"
    exit 1
fi
echo "legacy 007 rejection OK"

# 036 in Ada on the 32-bit board msp430: sign extension in the upper bytes of 64-bit fields
# (compiled with -gnata: the RTL's pragma Assume are checked too).
out=$work/ada036
mkdir "$out"
run_compiler -Ada -ACN -t msp430 -wordSize 4 -fpWordSize 4 -o "$out" "$cases/036.asn1" "$cases/036.acn"
cp "$cases/036_decode_test_msp430.adb" "$out/decode_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata -aIasn1rtl -aIsrc -aIboards/msp430 decode_test.adb && ./decode_test)
echo "wire 036 (Ada msp430) OK"

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

# Without --acn-v2, two CONTAINING components with external sizes in one encoding function are rejected (Issue 80:
# legacy encodes them through one shared temporary buffer).
out=$work/legacy039
mkdir "$out"
if run_compiler -c -ACN -o "$out" "$cases/039.asn1" "$cases/039.acn" 2>"$out/stderr.txt"; then
    echo "legacy 039 FAILED: accepted without --acn-v2"
    exit 1
fi
if ! grep -qF "039.acn:11:4: error: OCTET / BIT STRING (CONTAINING ...) components 'p1' and 'p2' take their sizes from ACN fields of one encoding function (here 'l'), which legacy ACN encodes through one shared temporary buffer. This is supported only with --acn-v2." "$out/stderr.txt"; then
    echo "legacy 039 FAILED, got:"
    cat "$out/stderr.txt"
    exit 1
fi
echo "legacy 039 rejection OK"

echo "acn-v2 wire checks passed"
