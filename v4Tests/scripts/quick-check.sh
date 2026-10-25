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
#
# Run from the Windows filesystem (/mnt/...), the script copies the source files (git ls-files: tracked
# and untracked, not ignored) to $QUICK_CHECK_MIRROR (default ~/asn1scc-work/quick-check-mirror, Linux
# filesystem, ~8x faster) and runs there; build outputs stay in the mirror, so later builds are
# incremental. QUICK_CHECK_MIRROR=none runs in place.

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

MIRROR=${QUICK_CHECK_MIRROR:-$HOME/asn1scc-work/quick-check-mirror}
ROOT=$(cd .. && pwd)
case "$ROOT" in
    /mnt/*)
        if [ "$MIRROR" != none ]; then
            echo "=== mirror $ROOT -> $MIRROR"
            mkdir -p "$MIRROR"
            (cd "$ROOT" && git ls-files -co --exclude-standard) | sort > "$MIRROR/.quick-check-files.new"
            # files that left the list since the last run (deleted or renamed)
            if [ -f "$MIRROR/.quick-check-files" ]; then
                comm -23 "$MIRROR/.quick-check-files" "$MIRROR/.quick-check-files.new" | (cd "$MIRROR" && xargs -r -d '\n' rm -f --)
            fi
            rsync -a --files-from="$MIRROR/.quick-check-files.new" "$ROOT/" "$MIRROR/" || exit 1
            mv "$MIRROR/.quick-check-files.new" "$MIRROR/.quick-check-files"
            cd "$MIRROR/v4Tests" || exit 1
            mapped=()
            for t in "${TARGETS[@]}"; do mapped+=("${t/#$ROOT/$MIRROR}"); done
            TARGETS=("${mapped[@]}")
        else
            echo "warning: $ROOT is on the Windows filesystem: ~8x slower than the Linux filesystem"
        fi ;;
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
