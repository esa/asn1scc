#!/bin/bash
# Test group runner for CI matrix jobs.
# Usage: ./github-test-group.sh <group>
# Groups: c, ada, python, rust, scala, icd
#
# Each group runs a subset of the test suite.  All groups are dispatched
# as parallel GitHub Actions matrix jobs.

set -e

GROUP="$1"

# Source sdkman (needed for Scala/Java)
source "$HOME/.sdkman/bin/sdkman-init.sh" 2>/dev/null || true

# ── Build the solution first ──────────────────────────────────────────
# The Docker image has the SDK and toolchain but does NOT pre-build
# the .NET solution.  Every matrix job needs the compiler (asn1scc) and
# the regression tool, so build them once at the top.
#
# Antlr and parseStg2 must be built before the main solution because
# several F# projects run parseStg2 as a pre-build codegen step.
echo "=== Building .NET solution ==="
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"

REGRESSION="../regression/bin/Debug/net10.0/regression"

case "$GROUP" in

  c)
    echo "=== C regression tests ==="
    cd v4Tests || exit 1
    # Run all C configurations serially (each uses -p 4 internally)
    "$REGRESSION" -l c -ws 4 -s false -p 4 || exit 1
    "$REGRESSION" -l c -ws 8 -s true -p 4 -ig || exit 1
    "$REGRESSION" -l c -ws 8 -s true -p 4 || exit 1
    "$REGRESSION" -l c -ws 8 -s false -p 4 || exit 1
    # ACN v2
    "$REGRESSION" -l c -ws 4 -s false -p 4 -acnv2 || exit 1
    "$REGRESSION" -l c -ws 8 -s true -p 4 -acnv2 || exit 1
    "$REGRESSION" -l c -ws 8 -s false -p 4 -acnv2 || exit 1
    ;;

  ada)
    echo "=== Ada regression tests ==="
    cd v4Tests || exit 1
    "$REGRESSION" -l Ada -ws 4 -s false -p 4 || exit 1
    "$REGRESSION" -l Ada -ws 8 -s false -p 4 || exit 1
    # ACN v2
    "$REGRESSION" -l Ada -ws 4 -s false -p 4 -acnv2 || exit 1
    "$REGRESSION" -l Ada -ws 8 -s false -p 4 -acnv2 || exit 1
    ;;

  python)
    echo "=== Python regression tests ==="
    cd v4Tests || exit 1
    "$REGRESSION" -l python -ws 4 -s false -p 4 || exit 1
    "$REGRESSION" -l python -ws 8 -s false -p 4 || exit 1
    # Python runtime unit tests
    cd ../asn1python || exit 1
    uvx --python=3.11 pytest tests -v || exit 1
    # Python code readability and type-hint checks
    ./tools/check_generated_code.sh || exit 1
    ;;

  rust)
    echo "=== Rust regression tests ==="
    cd v4Tests || exit 1
    "$REGRESSION" -l Rust -ws 4 -s false -p 4 || exit 1
    "$REGRESSION" -l Rust -ws 8 -s false -p 4 || exit 1
    ;;

  scala)
    echo "=== Scala & Interop tests ==="
    cd PUSCInteropTest || exit 1
    dotnet test || exit 1
    ;;

  icd)
    echo "=== ICD (ACN Interface Control Document) tests ==="
    cd v4Tests || exit 1
    python3 ./scripts/runIcdTests.py || exit 1
    ;;

  *)
    echo "Unknown test group: $GROUP"
    echo "Usage: $0 <c|ada|python|rust|scala|icd>"
    exit 1
    ;;
esac

echo "=== $GROUP tests passed ==="
