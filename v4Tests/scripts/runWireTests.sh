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
for n in 019 020 022 023 024 025 030 033 035 036 037 038; do
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

# A component whose type is an OCTET STRING (CONTAINING X) type assignment: a region too short for its contents must
# make the whole decoder fail (Issue 133: the Ada decoders asserted that the call of the type assignment's decoder
# cannot fail and accepted the stream). uPER and ACN, legacy and --acn-v2; Ada also without -gnata, where only the
# result shows the defect (with -gnata the old code stops at its pragma Assert).
for mode in "" "--acn-v2"; do
    out=$work/uper002$mode
    mkdir "$out"
    run_compiler -c -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/002.asn1"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/uper-wire/002_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "uper-wire 002 $mode OK"
    for gnata in "" "-gnata"; do
        out=$work/uper002ada$mode$gnata
        mkdir "$out"
        run_compiler -Ada -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/002.asn1"
        cp "$root/test-cases/uper-wire/002_wire_test.adb" "$out/wire_test.adb"
        (cd "$out" && gnatmake -q -gnat2012 $gnata wire_test.adb && ./wire_test)
        echo "uper-wire 002 (Ada) $mode $gnata OK"
    done
done

# An inline OCTET / BIT STRING (CONTAINING Box) whose contents are a CONTAINING type assignment: the contents are Box's
# encoding, and the region never exceeds the temporary buffer (Issue 134: the call and the buffer were named after the
# typedef Box shares with its contents, the decoders wrote past the buffer). uPER and ACN, legacy and --acn-v2; C with
# ASan / UBSan, Ada with and without -gnata.
for mode in "" "--acn-v2"; do
    out=$work/uper003$mode
    mkdir "$out"
    run_compiler -c -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/003.asn1"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/uper-wire/003_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "uper-wire 003 $mode OK"
    for gnata in "" "-gnata"; do
        out=$work/uper003ada$mode$gnata
        mkdir "$out"
        run_compiler -Ada -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/003.asn1"
        cp "$root/test-cases/uper-wire/003_wire_test.adb" "$out/wire_test.adb"
        (cd "$out" && gnatmake -q -gnat2012 $gnata wire_test.adb && ./wire_test)
        echo "uper-wire 003 (Ada) $mode $gnata OK"
    done
done

# Fragmented lengths (X.691 11.9.3.8) of variable-size types whose maximum is 64K or more: every rejected stream pins
# the error code and the bit position after the decoder, every valid one decodes to the expected value and encodes to
# the same bytes (Issue 152: the Ada decoders read blocks past the maximum size, took headers from C0 on as lengths,
# skipped a final part over the maximum and lost item failures). uPER and the ACN IA5String path, legacy and
# --acn-v2; C with ASan / UBSan; Ada without and with -gnata, and with -gnatp (no run-time checks: rows G1 / P1
# compare the guard regions around a value and the next component, which a write past the value inside the same
# allocation changes without any sanitizer report; ASan on top, with a finite stack). Issue 154: the rows NC* reject
# the layouts no X.691 encoder writes (a fragment header after a C1 .. C3 fragment, a two-octet length below 128);
# the -gnata build runs once more with every stream in a buffer of exactly REQUIRED_BYTES (--exact-size), where a
# decoder reading past REQUIRED_BITS fails an RTL precondition instead of returning FALSE.
for mode in "" "--acn-v2"; do
    out=$work/uper004$mode
    mkdir "$out"
    run_compiler -c -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/004.asn1"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/uper-wire/004_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "uper-wire 004 $mode OK"
    for gnat in "" "-gnata" "-gnatp"; do
        out=$work/uper004ada$mode$gnat
        mkdir "$out"
        run_compiler -Ada -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/004.asn1"
        cp "$root/test-cases/uper-wire/004_wire_test.adb" "$out/wire_test.adb"
        if [ "$gnat" = -gnatp ]; then
            (cd "$out" && gnatmake -q -gnat2012 -gnatp -g wire_test.adb -cargs -fsanitize=address \
                -largs -fsanitize=address && if [ "$(ulimit -s)" = unlimited ]; then ulimit -s 524288; fi && ./wire_test)
        else
            (cd "$out" && gnatmake -q -gnat2012 $gnat wire_test.adb && ./wire_test)
        fi
        if [ "$gnat" = -gnata ]; then
            (cd "$out" && ./wire_test --exact-size)
        fi
        echo "uper-wire 004 (Ada) $mode $gnat OK"
    done
done

# uPER OCTET / BIT STRING (CONTAINING X) of 16K units or more (Issue 154): the general length rejects a fragment
# header after a C1 .. C3 fragment and a two-octet length below 128, and accepts a final length after a C1 .. C3;
# rejections pin the error code and the bit position (Ada restores the position to the start of the CONTAINING when
# a fragmented length is rejected), valid streams are byte-exact; B80K: a fragment header after a C4.
# uPER, legacy and --acn-v2; C with ASan / UBSan, Ada without and with -gnata.
for mode in "" "--acn-v2"; do
    out=$work/uper005$mode
    mkdir "$out"
    run_compiler -c -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/005.asn1"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/uper-wire/005_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "uper-wire 005 $mode OK"
    for gnata in "" "-gnata"; do
        out=$work/uper005ada$mode$gnata
        mkdir "$out"
        run_compiler -Ada -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/005.asn1"
        cp "$root/test-cases/uper-wire/005_wire_test.adb" "$out/wire_test.adb"
        (cd "$out" && gnatmake -q -gnat2012 $gnata wire_test.adb && ./wire_test)
        echo "uper-wire 005 (Ada) $mode $gnata OK"
    done
done

# uPER OBJECT IDENTIFIER / RELATIVE-OID (Issues 155 / 156): the contents length is a general length (one octet below
# 128, two octets from 128: Ada read 80 xx as a one-octet length of 128), and the decoders reject what no encoder
# writes (a two-octet length below 128, a fragment header, length 0, a length that ends inside a subidentifier, an arc
# beyond 64 bits, a subidentifier whose leading octet is 0x80) with the type's error code and C's bit position; valid
# streams up to 190 / 200 contents octets are byte-exact. uPER and ACN decoders, legacy and --acn-v2; C with ASan / UBSan, Ada without and with -gnata.
for mode in "" "--acn-v2"; do
    out=$work/uper006$mode
    mkdir "$out"
    run_compiler -c -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/006.asn1"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/uper-wire/006_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "uper-wire 006 $mode OK"
    for gnata in "" "-gnata"; do
        out=$work/uper006ada$mode$gnata
        mkdir "$out"
        run_compiler -Ada -uPER -ACN -equal $mode -o "$out" "$root/test-cases/uper-wire/006.asn1"
        cp "$root/test-cases/uper-wire/006_wire_test.adb" "$out/wire_test.adb"
        (cd "$out" && gnatmake -q -gnat2012 $gnata wire_test.adb && ./wire_test)
        echo "uper-wire 006 (Ada) $mode $gnata OK"
    done
done

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
# component (004). The encoders reject a value with the component absent and the dependent present (003 encode).
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
    # the encoders: a value with the component absent and the dependent present fails with a nonzero error code
    # (q/issues.md Issue 108, rule R4)
    out=$work/acn003enc$mode
    mkdir "$out"
    run_compiler -c -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/003.asn1" "$root/test-cases/acn-wire/003.acn"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        $def -I "$out" "$out"/*.c "$root/test-cases/acn-wire/003_encode_test.c" -o "$out/encode_test"
    "$out/encode_test"
    echo "acn-wire 003 encode $mode OK"
    out=$work/acn003encada$mode
    mkdir "$out"
    run_compiler -Ada -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/003.asn1" "$root/test-cases/acn-wire/003.acn"
    cp "$root/test-cases/acn-wire/003_encode_test.adb" "$out/encode_test.adb"
    (cd "$out" && gnatmake -q -gnat2012 -gnata -gnateDACN_V2=$v2 encode_test.adb && ./encode_test)
    echo "acn-wire 003 encode (Ada) $mode OK"
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
# UBSan) acn-v2 and legacy (the same bytes, also with no consumer present since Issue 123), Ada x86 (-gnata,
# acn-v2: a false pragma Assume after a patch raises).
for mode in "" "--acn-v2"; do
    out=$work/acn006c$mode
    mkdir "$out"
    run_compiler -c -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/006.asn1" "$root/test-cases/acn-wire/006.acn" 2>/dev/null
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/acn-wire/006_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "acn-wire 006 $mode OK"
done
out=$work/acn006ada
mkdir "$out"
run_compiler -Ada -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/006.asn1" "$root/test-cases/acn-wire/006.acn" 2>/dev/null
cp "$root/test-cases/acn-wire/006_wire_test.adb" "$out/wire_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
echo "acn-wire 006 (Ada) --acn-v2 OK"

# acn-wire/012 (q/issues.md Issue 108): dependents without bits (a NULL, an empty SEQUENCE) present when a field of an
# absent component says so are rejected right after the fields before them; C (ASan / UBSan, no -pedantic-errors: the
# empty SEQUENCE is an empty struct) and Ada (-gnata), legacy and --acn-v2.
for mode in "" "--acn-v2"; do
    out=$work/acn012c$mode
    mkdir "$out"
    run_compiler -c -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/012.asn1" "$root/test-cases/acn-wire/012.acn"
    cc -std=c11 -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/acn-wire/012_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "acn-wire 012 $mode OK"
    out=$work/acn012ada$mode
    mkdir "$out"
    run_compiler -Ada -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/012.asn1" "$root/test-cases/acn-wire/012.acn"
    cp "$root/test-cases/acn-wire/012_wire_test.adb" "$out/wire_test.adb"
    (cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
    echo "acn-wire 012 (Ada) $mode OK"
done

# acn-wire/011 (q/issues.md Issues 123 / 125 / 142, cycle i108a): a determinant field none of whose consumers is in the
# value takes its default value, the same bytes in both modes; C (ASan / UBSan) and Ada (-gnata), legacy and --acn-v2.
for mode in "" "--acn-v2"; do
    out=$work/acn011c$mode
    mkdir "$out"
    run_compiler -c -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/011.asn1" "$root/test-cases/acn-wire/011.acn" 2>/dev/null
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/acn-wire/011_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "acn-wire 011 $mode OK"
    out=$work/acn011ada$mode
    mkdir "$out"
    run_compiler -Ada -ACN $mode -equal -o "$out" "$root/test-cases/acn-wire/011.asn1" "$root/test-cases/acn-wire/011.acn" 2>/dev/null
    cp "$root/test-cases/acn-wire/011_wire_test.adb" "$out/wire_test.adb"
    legacy=False
    [ -z "$mode" ] && legacy=True
    printf 'package Wire_Mode is\n   Legacy : constant Boolean := %s;\nend Wire_Mode;\n' "$legacy" > "$out/wire_mode.ads"
    (cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
    echo "acn-wire 011 (Ada) $mode OK"
done

# acn-wire/011_v2 (cycle i108a round 2, --acn-v2 only: legacy rejects the grammar): an OPTIONAL fixed-size consumer
# inside the elements of a callee's SEQUENCE OF is patched per element (no element holding it: the default 0).
out=$work/acn011v2c
mkdir "$out"
run_compiler -c -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/011_v2.asn1" "$root/test-cases/acn-wire/011_v2.acn" 2>/dev/null
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/acn-wire/011_v2_wire_test.c" -o "$out/wire_test"
"$out/wire_test"
echo "acn-wire 011_v2 --acn-v2 OK"
out=$work/acn011v2ada
mkdir "$out"
run_compiler -Ada -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/011_v2.asn1" "$root/test-cases/acn-wire/011_v2.acn" 2>/dev/null
cp "$root/test-cases/acn-wire/011_v2_wire_test.adb" "$out/wire_test.adb"
(cd "$out" && gnatmake -q -gnat2012 -gnata wire_test.adb && ./wire_test)
echo "acn-wire 011_v2 (Ada) --acn-v2 OK"

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
# One field passed twice (`payload <n, n>`), consumed directly and in the elements: C only (Ada does not compile the
# aliased refs under -gnatwe, Issue 126).
out=$work/acn007aliased
mkdir "$out"
run_compiler -c -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/007_aliased.asn1" "$root/test-cases/acn-wire/007_aliased.acn" 2>/dev/null
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/acn-wire/007_aliased_test.c" -o "$out/wire_test"
"$out/wire_test"
echo "acn-wire 007_aliased --acn-v2 OK"

# A determinant consumed inside the elements of a SEQUENCE OF that does not hold it (Issues 122 / 124, cycle i124a):
# legacy rejects every such shape but constant consumers (not behind an OPTIONAL / CHOICE check inside the element); --acn-v2 rejects a field consumed with no type assignment
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
expect_rejection fixedopt "" "fixedopt.acn:2:11: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#.o'). $legacy_inline"
expect_rejection fixedopt --acn-v2 "fixedopt.acn:2:11: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#.o') $v2_inline"
expect_rejection fixedopt2 "" "fixedopt2.acn:2:11: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#.o'). $legacy_inline"
expect_rejection fixedopt2 --acn-v2 "fixedopt2.acn:2:11: error: ACN field 'n' is consumed inside the elements of SEQUENCE OF 'items' (by 'items.#.o') $v2_inline"
expect_rejection argparam "" "argparam.acn:3:15: error: ACN parameter 'len' of type 'P' is passed as an argument to the elements of SEQUENCE OF 'items' (to 'items.#.q'). $legacy_only"
expect_accepted argparam --acn-v2
expect_rejection argopt "" "argopt.acn:3:11: error: ACN field 'n' is passed as an argument to the elements of SEQUENCE OF 'list' (to 'c.list.#.q'). $legacy_only"
expect_accepted argopt --acn-v2
expect_rejection argchoice "" "argchoice.acn:3:11: error: ACN field 'n' is passed as an argument to the elements of SEQUENCE OF 'list' (to 'c.list.#.q'). $legacy_only"
expect_accepted argchoice --acn-v2
expect_rejection argchain "" "argchain.acn:4:11: error: ACN field 'n' is passed as an argument to the elements of SEQUENCE OF 'list' (to 'c.list.#'). $legacy_only"
expect_accepted argchain --acn-v2
expect_accepted argmand ""
expect_accepted argmand --acn-v2
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

# An ACN align-to-next of a CONTAINING aligns the octet / bit string, not the contents (Issue 134: it entered the
# contents' sizes, the region's length range grew by one octet and the legacy decoders wrote past their temporary
# buffer). Legacy and --acn-v2; C with ASan / UBSan, Ada with and without -gnata.
for mode in "" "--acn-v2"; do
    out=$work/acn008$mode
    mkdir "$out"
    run_compiler -c -ACN -equal $mode -o "$out" "$root/test-cases/acn-wire/008.asn1" "$root/test-cases/acn-wire/008.acn"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/acn-wire/008_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "acn-wire 008 $mode OK"
    for gnata in "" "-gnata"; do
        out=$work/acn008ada$mode$gnata
        mkdir "$out"
        run_compiler -Ada -ACN -equal $mode -o "$out" "$root/test-cases/acn-wire/008.asn1" \
            "$root/test-cases/acn-wire/008.acn"
        cp "$root/test-cases/acn-wire/008_wire_test.adb" "$out/wire_test.adb"
        (cd "$out" && gnatmake -q -gnat2012 $gnata wire_test.adb && ./wire_test)
        echo "acn-wire 008 (Ada) $mode $gnata OK"
    done
done

# A CONTAINING region longer than the largest encoding of its contents (--acn-v2, external size: Ada read the fields
# after it past the end of the stream, C skipped its unused part), BIT STRING contents that end past their region (Ada
# both modes, C legacy inside the region's last octet), the region around a CHOICE bounded by the CHOICE's own maximum
# (010_choice, --acn-v2 only): rejected streams and round trips (Issue 132). Legacy and --acn-v2; C with ASan / UBSan,
# Ada with and without -gnata.
for mode in "" "--acn-v2"; do
    grammars="$root/test-cases/acn-wire/010.asn1 $root/test-cases/acn-wire/010.acn"
    def=
    v2=False
    if [ "$mode" = --acn-v2 ]; then
        grammars="$grammars $root/test-cases/acn-wire/010_choice.asn1 $root/test-cases/acn-wire/010_choice.acn"
        def=-DWIRE_ACN_V2
        v2=True
    fi
    out=$work/acn010$mode
    mkdir "$out"
    run_compiler -c -ACN -equal $mode -o "$out" $grammars
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        $def -I "$out" "$out"/*.c "$root/test-cases/acn-wire/010_decode_test.c" -o "$out/decode_test"
    "$out/decode_test"
    echo "acn-wire 010 $mode OK"
    for gnata in "" "-gnata"; do
        out=$work/acn010ada$mode$gnata
        mkdir "$out"
        run_compiler -Ada -ACN -equal $mode -o "$out" $grammars
        cp "$root/test-cases/acn-wire/010_decode_test.adb" "$out/decode_test.adb"
        (cd "$out" && gnatmake -q -gnat2012 $gnata -gnateDACN_V2=$v2 decode_test.adb && ./decode_test)
        echo "acn-wire 010 (Ada) $mode $gnata OK"
    done
done

# Without --acn-v2, an OCTET / BIT STRING (CONTAINING T) with an embedded length or a fixed size whose length range at
# the site does not hold T's encoding (a larger or smaller maximum, a larger minimum: a site ACN specification with
# child overrides) is rejected (Issue 134: legacy decodes the region into a buffer of T's maximum and writes T's length
# under the site's range); --acn-v2 accepts it. C and Ada.
site_spec_msg="error: the length of this CONTAINING region"
site_spec_end="whose functions the region calls; not supported without --acn-v2 (use a type assignment for the contents)"
for variant in "emb 4:15 5..5 T 2..2 octets" "fix 5:15 5..5 Fx 2..2 octets" "bits 6:16 33..34 T 9..10 bits" \
    "bits12 6:16 13..14 T 9..10 bits" "emb6 8:16 3..4 T6 2..4 octets" "narrow 10:18 9..10 T13 13..14 bits"; do
    set -- $variant
    for lang in c Ada; do
        out=$work/acn009$1$lang
        mkdir "$out"
        if run_compiler -$lang -ACN -o "$out" "$root/test-cases/acn-wire/009.asn1" \
            "$root/test-cases/acn-wire/009_$1.acn" 2>"$out/stderr.txt"; then
            echo "acn-wire 009 $1 $lang FAILED: accepted without --acn-v2"
            exit 1
        fi
        site=$(echo "$3" | sed 's/\.\./ .. /')
        callee=$(echo "$5" | sed 's/\.\./ .. /')
        if ! grep -qF "009_$1.acn:$2: $site_spec_msg ($site $6) does not match the encoding of $4 ($callee $6), $site_spec_end" \
            "$out/stderr.txt"; then
            echo "acn-wire 009 $1 $lang FAILED, got:"
            cat "$out/stderr.txt"
            exit 1
        fi
        out=$work/acn009$1${lang}v2
        mkdir "$out"
        run_compiler -$lang -ACN --acn-v2 -o "$out" "$root/test-cases/acn-wire/009.asn1" \
            "$root/test-cases/acn-wire/009_$1.acn"
        echo "acn-wire 009 $1 $lang rejection without --acn-v2 OK"
    done
done
for mode in "" "--acn-v2"; do
    out=$work/acn009same$mode
    mkdir "$out"
    run_compiler -c -ACN $mode -o "$out" "$root/test-cases/acn-wire/009.asn1" "$root/test-cases/acn-wire/009_same.acn"
    echo "acn-wire 009 same $mode OK"
done

# An embedded-length / fixed-size CONTAINING whose ACN specification (at the site or on the CONTAINING type assignment)
# overrides the encoding of the contents encodes them as specified (Issue 135, --acn-v2: before, the region's sizes
# followed the specification but the code called T's own function; the embedded forms wrote a length outside their
# range). Controls with unchanged bytes: present-when only, an external size. C with ASan / UBSan, Ada with and without
# -gnata. Without --acn-v2 such specifications are rejected (009).
out=$work/acn013
mkdir "$out"
run_compiler -c -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/013.asn1" "$root/test-cases/acn-wire/013.acn"
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/acn-wire/013_wire_test.c" -o "$out/wire_test"
"$out/wire_test"
echo "acn-wire 013 OK"
for gnata in "" "-gnata"; do
    out=$work/acn013ada$gnata
    mkdir "$out"
    run_compiler -Ada -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/013.asn1" "$root/test-cases/acn-wire/013.acn"
    cp "$root/test-cases/acn-wire/013_wire_test.adb" "$out/wire_test.adb"
    (cd "$out" && gnatmake -q -gnat2012 $gnata wire_test.adb && ./wire_test)
    echo "acn-wire 013 (Ada) $gnata OK"
done

# The contents of a CONTAINING are encoded by the referenced type assignment's functions, also when it shares another
# type's typedef (an alias with an ACN specification of its own, a WITH COMPONENTS subtype; Issue 138: before, the call
# and its buffer followed the typedef, --acn-v2 wrote another encoding, legacy rejected the sites). Legacy and
# --acn-v2; C with ASan / UBSan into buffers of exactly REQUIRED_BYTES, Ada with and without -gnata.
for mode in "" "--acn-v2"; do
    out=$work/acn014$mode
    mkdir "$out"
    run_compiler -c -ACN -equal $mode -o "$out" "$root/test-cases/acn-wire/014.asn1" "$root/test-cases/acn-wire/014.acn"
    cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -fsanitize=address,undefined -fno-sanitize-recover=undefined \
        -I "$out" "$out"/*.c "$root/test-cases/acn-wire/014_wire_test.c" -o "$out/wire_test"
    "$out/wire_test"
    echo "acn-wire 014 $mode OK"
    for gnata in "" "-gnata"; do
        out=$work/acn014ada$mode$gnata
        mkdir "$out"
        run_compiler -Ada -ACN -equal $mode -o "$out" "$root/test-cases/acn-wire/014.asn1" \
            "$root/test-cases/acn-wire/014.acn"
        cp "$root/test-cases/acn-wire/014_wire_test.adb" "$out/wire_test.adb"
        (cd "$out" && gnatmake -q -gnat2012 $gnata wire_test.adb && ./wire_test)
        echo "acn-wire 014 (Ada) $mode $gnata OK"
    done
done

# The REQUIRED_BITS of an --acn-v2 OCTET STRING region around a CHOICE count its padding to whole octets (Issue 141:
# before, the C encoder of a maximal value wrote one octet past a buffer of REQUIRED_BYTES). C built with -DNDEBUG (the
# runtime's assert would stop first) and ASan / UBSan, Ada with and without -gnata. --acn-v2 only.
out=$work/acn015
mkdir "$out"
run_compiler -c -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/015.asn1" "$root/test-cases/acn-wire/015.acn"
cc -std=c11 -pedantic-errors -Wall -Wextra -Werror -DNDEBUG -fsanitize=address,undefined -fno-sanitize-recover=undefined \
    -I "$out" "$out"/*.c "$root/test-cases/acn-wire/015_wire_test.c" -o "$out/wire_test"
"$out/wire_test"
echo "acn-wire 015 OK"
for gnata in "" "-gnata"; do
    out=$work/acn015ada$gnata
    mkdir "$out"
    run_compiler -Ada -ACN --acn-v2 -equal -o "$out" "$root/test-cases/acn-wire/015.asn1" "$root/test-cases/acn-wire/015.acn"
    cp "$root/test-cases/acn-wire/015_wire_test.adb" "$out/wire_test.adb"
    (cd "$out" && gnatmake -q -gnat2012 $gnata wire_test.adb && ./wire_test)
    echo "acn-wire 015 (Ada) $gnata OK"
done

echo "acn-v2 wire checks passed"
