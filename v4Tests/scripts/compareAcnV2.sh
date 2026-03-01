#!/bin/bash
#
# Compare generated code with and without --acn-v2.
#
# Usage:
#   ./scripts/compareAcnV2.sh -l c -t test-cases/acn/01-INTEGER/001.asn1
#   ./scripts/compareAcnV2.sh -l c -t test-cases/acn/12-PARAM/001.asn1 -w /tmp/cmp
#   ./scripts/compareAcnV2.sh -l c -t test-cases/acn/12-PARAM/001_01.asn1 -p "MyPDU,Header"
#
# Output structure:
#   <work_dir>/<test-case-name>/without_v2/   (legacy)
#   <work_dir>/<test-case-name>/with_v2/      (--acn-v2)
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
WORK_DIR="work_dir"
LANG=""
TEST_CASE=""
ICD_PDUS=""

usage() {
    echo "Usage: $0 -l <lang> -t <test_case> [-w <work_dir>] [-p <pdus>]"
    echo ""
    echo "  -l <lang>        Language: c or Ada"
    echo "  -t <test_case>   Path to .asn1 test file"
    echo "  -w <work_dir>    Base output directory (default: work_dir)"
    echo "  -p <pdus>        Comma-separated list of PDU type names (passed as -icdPdus to asn1scc)"
    exit 1
}

while getopts "l:t:w:p:" opt; do
    case $opt in
        l) LANG="$OPTARG" ;;
        t) TEST_CASE="$OPTARG" ;;
        w) WORK_DIR="$OPTARG" ;;
        p) ICD_PDUS="$OPTARG" ;;
        *) usage ;;
    esac
done

if [ -z "$LANG" ] || [ -z "$TEST_CASE" ]; then
    usage
fi

# Derive test case name from path: test-cases/acn/01-INTEGER/001.asn1 -> 01-INTEGER_001
TC_BASENAME="$(basename "$TEST_CASE" .asn1)"
TC_PARENT="$(basename "$(dirname "$TEST_CASE")")"
TC_NAME="${TC_PARENT}_${TC_BASENAME}"

DIR_WITHOUT="${WORK_DIR}/${TC_NAME}/without_v2"
DIR_WITH="${WORK_DIR}/${TC_NAME}/with_v2"

mkdir -p "$DIR_WITHOUT" "$DIR_WITH"

PDUS_ARG=""
if [ -n "$ICD_PDUS" ]; then
    PDUS_ARG="--icd-pdus $ICD_PDUS"
fi

echo "=== Running WITHOUT --acn-v2 ==="
"${SCRIPT_DIR}/runTests.py" -l "$LANG" -t "$TEST_CASE" -o "$DIR_WITHOUT" $PDUS_ARG
echo ""

echo "=== Running WITH --acn-v2 ==="
"${SCRIPT_DIR}/runTests.py" -l "$LANG" -t "$TEST_CASE" --acn-v2 -o "$DIR_WITH" $PDUS_ARG
echo ""

echo "=== Comparing generated code ==="
echo "Without: $DIR_WITHOUT/"
echo "With:    $DIR_WITH/"
echo ""

# Find common .c and .h files and diff them
DIFF_FOUND=0
for f in "$DIR_WITHOUT"/*.c "$DIR_WITHOUT"/*.h; do
    [ -f "$f" ] || continue
    FNAME="$(basename "$f")"
    if [ -f "$DIR_WITH/$FNAME" ]; then
        if ! diff -q "$f" "$DIR_WITH/$FNAME" > /dev/null 2>&1; then
            echo "--- DIFF: $FNAME ---"
            diff -u "$f" "$DIR_WITH/$FNAME" || true
            echo ""
            DIFF_FOUND=1
        else
            echo "  IDENTICAL: $FNAME"
        fi
    fi
done

if [ "$DIFF_FOUND" -eq 0 ]; then
    echo "No differences found in generated code."
fi
