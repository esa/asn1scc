#!/usr/bin/env bash
set -euo pipefail
case_dir="$(cd "$(dirname "$0")" && pwd)"
exec bash "$case_dir/../run_xer_case.sh" "$case_dir" valid oversize
