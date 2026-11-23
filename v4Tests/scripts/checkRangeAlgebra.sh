#!/bin/sh
# Property test of the range algebra of the PER-visible constraints (rangeAlgebra.fsx, Issue 173) against the
# CommonTypes.dll of a build. Usage: checkRangeAlgebra.sh [bin dir], default ../asn1scc/bin/Debug/net10.0 next to
# v4Tests; exit code 0 = no failure. Needs the .NET SDK (dotnet fsi).
set -eu
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
bin=${1:-$here/../../asn1scc/bin/Debug/net10.0}
bin=$(CDPATH= cd -- "$bin" && pwd)
tmp=$(mktemp -d "${TMPDIR:-/tmp}/asn1scc-range.XXXXXX")
trap 'rm -rf "$tmp"' EXIT HUP INT TERM
sed "s|@BIN@|$bin|g" "$here/rangeAlgebra.fsx" > "$tmp/rangeAlgebra.fsx"
dotnet fsi --quiet "$tmp/rangeAlgebra.fsx"
