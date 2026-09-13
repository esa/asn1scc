#!/bin/bash
echo "****"
echo $1
echo "****"
source "$HOME/.sdkman/bin/sdkman-init.sh"
echo "git config --global --add safe.directory /app/.git"

cd /workdir/ || exit
git config --global --add safe.directory /app/.git || exit
git config --global --add safe.directory /app//.git || exit
git config --global --add safe.directory /app/asn1scc/.git || exit
echo "git clone /app/ asn1scc"
git clone /app/ asn1scc || exit
cd asn1scc || exit
echo "git checkout $1"
git checkout $1 || exit
echo "git config --global --add safe.directory /app/.git"
git config --global --add safe.directory /app/.git || exit
echo "git pull"
git pull || exit
echo "git rev-parse --abbrev-ref HEAD"
git rev-parse --abbrev-ref HEAD || exit
echo "dotnet build"
dotnet build Antlr/ || exit 1
dotnet build parseStg2/ || exit 1
dotnet build "asn1scc.sln" || exit 1
cd v4Tests || exit 1
echo "run local tests"

REGRESSION="../regression/bin/Debug/net10.0/regression"

# ── Run all regression groups in parallel background jobs ──────────────
# On a 48-core self-hosted runner, -p 48 keeps each group fully
# parallelised while the groups run simultaneously.
RESULTS_DIR=$(mktemp -d)

run_bg() {
  local name="$1"; shift
  ( "$@" && echo 0 > "$RESULTS_DIR/$name" || echo 1 > "$RESULTS_DIR/$name" ) &
}

# Ada tests
run_bg ada_4_av2    "$REGRESSION" -l Ada -ws 4 -s false -p 48 -acnv2
run_bg ada_8_av2    "$REGRESSION" -l Ada -ws 8 -s false -p 48 -acnv2
run_bg ada_4        "$REGRESSION" -l Ada -ws 4 -s false -p 48
run_bg ada_8        "$REGRESSION" -l Ada -ws 8 -s false -p 48

# C tests
run_bg c_4_av2      "$REGRESSION" -l c -ws 4 -s false -p 48 -acnv2
run_bg c_8_s_av2    "$REGRESSION" -l c -ws 8 -s true  -p 48 -acnv2
run_bg c_8_ns_av2   "$REGRESSION" -l c -ws 8 -s false -p 48 -acnv2
run_bg c_4          "$REGRESSION" -l c -ws 4 -s false -p 48
run_bg c_8_s_ig     "$REGRESSION" -l c -ws 8 -s true  -p 48 -ig
run_bg c_8_s        "$REGRESSION" -l c -ws 8 -s true  -p 48

# Rust tests
run_bg rust_4       "$REGRESSION" -l Rust -ws 4 -s false -p 12
run_bg rust_8       "$REGRESSION" -l Rust -ws 8 -s false -p 12

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
echo "run scala tests"
cd ../PUSCInteropTest || exit 1
dotnet test || exit 1
