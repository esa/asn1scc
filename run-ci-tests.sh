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
RESULTS_DIR=$(mktemp -d)

run_bg() {
  local name="$1"; shift
  ( "$@" && echo 0 > "$RESULTS_DIR/$name" || echo 1 > "$RESULTS_DIR/$name" ) &
}

# Rust tests (non-slim only; slim mode is C-only in the regression tool)
run_bg rust_4 "$REGRESSION" -l Rust -ws 4 -s false -p 12
run_bg rust_8 "$REGRESSION" -l Rust -ws 8 -s false -p 12

# Wait for all background jobs
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

# Scala & Interop tests
cd ../PUSCInteropTest || exit 1
dotnet test || exit 1
