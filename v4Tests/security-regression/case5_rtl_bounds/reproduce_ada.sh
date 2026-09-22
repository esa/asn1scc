#!/usr/bin/env bash
# Ada leg of case 5: the XER BIT STRING / OCTET STRING decoders of the Ada
# runtime must fail closed on oversized or malformed input (ESACERT #74626).
set -euo pipefail
cd "$(dirname "$0")"
repo_root="$(cd ../../.. && pwd)"
mkdir -p build/ada
cp "$repo_root/ADA_RTL2/boards/x86_board_config.ads" build/ada/board_config.ads
gprbuild -q -P ada_runtime_tests.gpr
for test in xer-bits xer-octets; do timeout 20s build/ada/obj/ada_runtime_tests "$test"; done
