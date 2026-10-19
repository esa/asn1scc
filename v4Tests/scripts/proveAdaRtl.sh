#!/bin/bash
# Proves the SPARK units of the Ada runtime library (adaasn1rtl, .encoding,
# .encoding.uper, .encoding.acn) with gnatprove, as generated for each board.
#
# Usage (from v4Tests/): ./scripts/proveAdaRtl.sh [board ...]
#   boards: x86 (default target, Ada ws=8 regression legs) and msp430
#   (-t msp430, Ada ws=4 legs); default: both.
#   ASN1SCC   compiler to use (default ../asn1scc/bin/Debug/net10.0/asn1scc)
#   GNATPROVE_JOBS  gnatprove -j value (default 0 = all cores)
#
# A board passes when gnatprove exits with 0, the Total row of gnatprove.out
# has no unproved check, and the output has no high / medium / low message.
# .encoding.xer has no SPARK_Mode and .encoding.test_cases_aux is the -atc
# harness; neither is proved.

ASN1SCC=${ASN1SCC:-../asn1scc/bin/Debug/net10.0/asn1scc}
JOBS=${GNATPROVE_JOBS:-0}
BOARDS=("$@")
[ ${#BOARDS[@]} -eq 0 ] && BOARDS=(x86 msp430)

if ! command -v gnatprove > /dev/null; then
    echo "proveAdaRtl: gnatprove not found in PATH"
    exit 1
fi

WORK=$(mktemp -d)
cat > "$WORK/rtl.asn1" <<'EOF'
RTL-PROOF DEFINITIONS AUTOMATIC TAGS ::= BEGIN
    T ::= SEQUENCE { a INTEGER (0..1000), b BOOLEAN, c OCTET STRING (SIZE(1..10)) }
END
EOF
cat > "$WORK/rtl.acn" <<'EOF'
RTL-PROOF DEFINITIONS ::= BEGIN
    T []
END
EOF

UNITS="adaasn1rtl.adb adaasn1rtl-encoding.adb adaasn1rtl-encoding-uper.adb adaasn1rtl-encoding-acn.adb"
STATUS=0
for board in "${BOARDS[@]}"; do
    out="$WORK/$board"
    case "$board" in
        x86) target="" ; gpr="asn1_x86.gpr" ;;
        *)   target="-t $board" ; gpr="asn1_$board.gpr" ;;
    esac
    echo "=== Ada RTL proof: $board ==="
    # shellcheck disable=SC2086
    if ! "$ASN1SCC" -Ada -uPER -ACN -atc $target -o "$out" "$WORK/rtl.asn1" "$WORK/rtl.acn"; then
        echo "proveAdaRtl: code generation failed for $board"
        STATUS=1
        continue
    fi
    start=$(date +%s)
    # shellcheck disable=SC2086
    (cd "$out" && gnatprove -P"$gpr" -j"$JOBS" --level=4 -u $UNITS > gnatprove.log 2>&1)
    rc=$?
    summary=$(find "$out" -name gnatprove.out | head -1)
    if [ -z "$summary" ]; then
        echo "proveAdaRtl: no gnatprove.out for $board (exit $rc)"
        tail -20 "$out/gnatprove.log"
        STATUS=1
        continue
    fi
    sed -n '/SPARK Analysis results/,/^Total/p' "$summary"
    # last column: "." or "N (P%)"
    unproved=$(awk '/^Total/ { print ($NF ~ /^\(/) ? $(NF-1) : $NF }' "$summary")
    messages=$(grep -cE '(^|: )[[:space:]]*(high|medium|low): ' "$out/gnatprove.log")
    echo "board $board: gnatprove exit $rc, unproved ${unproved:-?}, messages $messages, $(( $(date +%s) - start )) s"
    if [ "$rc" -ne 0 ] || [ "$messages" -ne 0 ] || { [ "$unproved" != "." ] && [ "$unproved" != "0" ]; }; then
        grep -E -A1 '(^|: )[[:space:]]*(high|medium|low): ' "$out/gnatprove.log"
        echo "proveAdaRtl: $board FAILED"
        STATUS=1
    fi
done

if [ $STATUS -eq 0 ]; then
    rm -rf "$WORK"
    echo "Ada RTL proof OK (${BOARDS[*]})"
else
    echo "Ada RTL proof FAILED; work directory kept: $WORK"
fi
exit $STATUS
