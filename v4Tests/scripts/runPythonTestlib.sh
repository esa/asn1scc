#!/bin/bash
# Python backend on the Asn1AcnTestLib corpus: generates Python code with automatic
# test cases (-atc) for all Asn1AcnTestLib files, once for ACN and once for uPER, and
# runs the generated tests with pytest. Used by local-build.sh and by the CI workflow
# "Python Unit Tests" (.github/workflows/asn1scc-pytest-local.yml); the corpus
# regression (runTests.py, regression tool) does not use Asn1AcnTestLib.
#
# Usage: runPythonTestlib.sh [-o <output dir>] [-j <JUnit XML dir>]
#   -o  where the code is generated (default v4Tests/tmp_python_testlib)
#   -j  write test-results-testlib.xml and test-results-uper-testlib.xml there
# ASN1SCC overrides the compiler (default asn1scc/bin/Debug/net10.0/asn1scc);
# PYTEST overrides the pytest command (default: uvx --python=3.11 --with=pytest-xdist
# pytest, as in the Docker image). Both parts always run; exit 1 if either fails.

set -u

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
ASN1SCC=${ASN1SCC:-$ROOT/asn1scc/bin/Debug/net10.0/asn1scc}
read -r -a PYTEST <<< "${PYTEST:-uvx --python=3.11 --with=pytest-xdist pytest}"
OUT=$ROOT/v4Tests/tmp_python_testlib
JUNIT=""
while getopts "o:j:" opt; do
    case $opt in
        o) OUT=$(mkdir -p "$OPTARG" && cd "$OPTARG" && pwd) ;;
        j) JUNIT=$(mkdir -p "$OPTARG" && cd "$OPTARG" && pwd) ;;
        *) echo "Usage: $0 [-o <output dir>] [-j <JUnit XML dir>]"; exit 2 ;;
    esac
done

# Generated tests that fail for known reasons of the Python backend
SKIP_ACN="not (test_case_ACN_000519 or test_case_ACN_000520 or test_case_ACN_000527 or test_case_ACN_000528 or test_case_ACN_000794 or test_case_ACN_000796 or test_case_ACN_000798 or test_case_ACN_000800 or test_case_ACN_000802 or test_case_ACN_000804 or test_case_ACN_000806 or test_case_ACN_000808 or test_case_ACN_000810 or test_case_ACN_000812 or test_case_ACN_000814 or test_case_ACN_000816)"
SKIP_UPER="not (test_case_UPER_000531 or test_case_UPER_000532 or test_case_UPER_000539 or test_case_UPER_000540)"

cd "$ROOT" || exit 1
mapfile -t FILES < <(find Asn1AcnTestLib \( -name "*.asn1" -o -name "*.acn" \) | sort)

rc=0
for enc in ACN uPER; do
    case $enc in
        ACN)  dir=testlib;      skip=$SKIP_ACN;  xml=test-results-testlib.xml ;;
        uPER) dir=testlib_uper; skip=$SKIP_UPER; xml=test-results-uper-testlib.xml ;;
    esac
    echo "=== Python testlib, $enc ==="
    rm -rf "${OUT:?}/$dir"
    if ! "$ASN1SCC" -python "-$enc" -atc -fp AUTO -typePrefix T -o "$OUT/$dir" "${FILES[@]}"; then
        echo "Python testlib, $enc: code generation failed"
        rc=1
        continue
    fi
    "${PYTEST[@]}" -n "$(nproc)" ${JUNIT:+--junitxml="$JUNIT/$xml"} -k "$skip" "$OUT/$dir" || rc=1
done
exit $rc
