#!/bin/bash
# Quick local check before pushing a branch (the full regression runs on GitHub:
# .github/workflows/asn1scc-tests-parallel.yml).
#
# Usage (from v4Tests/): ./scripts/quick-check.sh [-j N] [--no-build] [folder|file.asn1 ...]
#   folder     a folder of test-cases/acn (e.g. 09-CHOICE) or a path to a test .asn1 file;
#              default: a small smoke set
#   -j N       parallel test cases (default 4: keeps the machine cool)
#   --no-build skip dotnet build
#
# Runs C and Ada, ws=8, legacy and --acn-v2, without the SPARK proofs (they run in CI).
# Exit 1 if a case failed; the failed cases keep their work directory under work_dir/.

set -u
cd "$(dirname "$0")/.." || exit 1

JOBS=4
BUILD=1
TARGETS=()
while [ $# -gt 0 ]; do
    case "$1" in
        -j) JOBS="$2"; shift 2 ;;
        --no-build) BUILD=0; shift ;;
        *) TARGETS+=("$1"); shift ;;
    esac
done
[ ${#TARGETS[@]} -eq 0 ] && TARGETS=(09-CHOICE 12-PARAM 23-CONTAINING)

REG=../regression/bin/Debug/net10.0/regression
start=$(date +%s)
case "$PWD" in
    /mnt/*) echo "warning: $PWD is on the Windows filesystem: ~8x slower than a worktree on the Linux filesystem" ;;
esac

if [ $BUILD -eq 1 ]; then
    echo "=== dotnet build"
    (cd .. && dotnet build asn1scc.sln -v q -nologo 2>&1 | grep -E " error |Build succeeded|FAILED") || true
    [ -f ../asn1scc/bin/Debug/net10.0/asn1scc.dll ] || { echo "build failed"; exit 1; }
fi

rc=0
for t in "${TARGETS[@]}"; do
    if [ -f "$t" ]; then sel=(-t "$t"); elif [ -d "test-cases/acn/$t" ]; then sel=(-tcd "test-cases/acn/$t"); else sel=(-tcd "$t"); fi
    for cfg in "-l c" "-l c -acnv2" "-l Ada" "-l Ada -acnv2"; do
        out=$($REG $cfg -ws 8 -s false -p "$JOBS" --spark exclude "${sel[@]}" 2>&1)
        r=$?
        ok=$(grep -c -- '--> OK' <<<"$out")
        bad=$(grep -c -- '--> Failed' <<<"$out")
        printf '%-28s %-14s OK %4d  failed %3d\n' "$t" "$cfg" "$ok" "$bad"
        if [ $((ok + bad)) -eq 0 ] && ! grep -q 'ACNV2_ONLY\|C_ONLY' -r "${sel[1]}" 2>/dev/null; then
            echo "    no test cases selected by ${sel[*]}"; rc=1
        fi
        if [ $r -ne 0 ]; then
            rc=1
            grep -A3 -- '--> Failed' <<<"$out" | head -40
        fi
    done
done
echo "=== $(( $(date +%s) - start )) s, $([ $rc -eq 0 ] && echo PASSED || echo FAILED)"
exit $rc
