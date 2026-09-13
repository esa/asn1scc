#!/bin/bash

# Source sdkman initialization script
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Build and test commands
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"

REGRESSION="regression/bin/Debug/net10.0/regression"
cd v4Tests || exit 1

# ── Run all regression groups in parallel background jobs ──────────────
# Each invocation runs independently in the background.  All 14
# invocations start simultaneously, each using -p 4 to parallelise
# test-case compilation.  This maximises CPU utilisation on multi-core
# runners (4 vCPUs on GitHub Actions).
#
# We use a per-job status file instead of checking exit codes with `wait`
# because some shells don't propagate the exit code of background jobs.

RESULTS_DIR=$(mktemp -d)

run_bg() {
  local name="$1"; shift
  ( "$@" && echo 0 > "$RESULTS_DIR/$name" || echo 1 > "$RESULTS_DIR/$name" ) &
}

# C tests
run_bg c_4_ns      "$REGRESSION" -l c -ws 4 -s false -p 4
run_bg c_8_s_ig    "$REGRESSION" -l c -ws 8 -s true  -p 4 -ig
run_bg c_8_s       "$REGRESSION" -l c -ws 8 -s true  -p 4
run_bg c_8_ns      "$REGRESSION" -l c -ws 8 -s false -p 4
run_bg c_av2_4     "$REGRESSION" -l c -ws 4 -s false -p 4 -acnv2
run_bg c_av2_8_s   "$REGRESSION" -l c -ws 8 -s true  -p 4 -acnv2
run_bg c_av2_8_ns  "$REGRESSION" -l c -ws 8 -s false -p 4 -acnv2

# Ada tests
run_bg ada_4       "$REGRESSION" -l Ada -ws 4 -s false -p 4
run_bg ada_8       "$REGRESSION" -l Ada -ws 8 -s false -p 4
run_bg ada_av2_4   "$REGRESSION" -l Ada -ws 4 -s false -p 4 -acnv2
run_bg ada_av2_8   "$REGRESSION" -l Ada -ws 8 -s false -p 4 -acnv2

# Python tests
run_bg py_4        "$REGRESSION" -l python -ws 4 -s false -p 4
run_bg py_8        "$REGRESSION" -l python -ws 8 -s false -p 4

# Rust tests
run_bg rust_4      "$REGRESSION" -l Rust -ws 4 -s false -p 4
run_bg rust_8      "$REGRESSION" -l Rust -ws 8 -s false -p 4

# Wait for all background jobs to finish
wait

# Check results
FAILED=0
for f in "$RESULTS_DIR"/*; do
  code=$(cat "$f")
  if [ "$code" != "0" ]; then
    echo "FAILED: $(basename "$f")"
    FAILED=1
  fi
done
rm -rf "$RESULTS_DIR"

if [ "$FAILED" != "0" ]; then
  echo "Some regression tests failed"
  exit 1
fi

# ICD tests (serial, ~30s)
python3 ./scripts/runIcdTests.py || exit 1

# Scala & Interop tests
cd ../PUSCInteropTest || exit 1
dotnet test || exit 1

# Python runtime unit tests
cd ../asn1python || exit 1
uvx --python=3.11 pytest tests -v || exit 1
./tools/check_generated_code.sh || exit 1

echo "All tests passed."
