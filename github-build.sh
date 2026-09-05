#!/bin/bash

# Source sdkman initialization script
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Build and test commands
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"
cd v4Tests || exit 1
../regression/bin/Debug/net10.0/regression -l c -ws 4 -s false -p 12 || exit 1
../regression/bin/Debug/net10.0/regression -l c -ws 8 -s true -p 12 -ig || exit 1
../regression/bin/Debug/net10.0/regression -l c -ws 8 -s true -p 12 || exit 1
../regression/bin/Debug/net10.0/regression -l Ada -ws 4 -s false -p 12 || exit 1
../regression/bin/Debug/net10.0/regression -l Ada -ws 8 -s false -p 12 || exit 1
../regression/bin/Debug/net10.0/regression -l python -ws 4 -s false -p 12 || exit 1
../regression/bin/Debug/net10.0/regression -l python -ws 8 -s false -p 12 || exit 1

# ACN v2 (deferred patching) regression runs - C and Ada (mirrors local-build.sh)
../regression/bin/Debug/net10.0/regression -l c -ws 4 -s false -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l c -ws 8 -s true -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l c -ws 8 -s false -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l Ada -ws 4 -s false -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l Ada -ws 8 -s false -p 12 -acnv2 || exit 1

# ACN v2 (deferred patching) regression runs - C and Ada (mirrors local-build.sh)
../regression/bin/Debug/net10.0/regression -l c -ws 4 -s false -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l c -ws 8 -s true -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l c -ws 8 -s false -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l Ada -ws 4 -s false -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l Ada -ws 8 -s true -p 12 -acnv2 || exit 1
../regression/bin/Debug/net10.0/regression -l Ada -ws 8 -s false -p 12 -acnv2 || exit 1

# ICD (ACN Interface Control Document) regression tests - C compiler only, ~30s
python3 ./scripts/runIcdTests.py || exit 1

# Scala & Interop tests
cd ../PUSCInteropTest || exit 1
dotnet test || exit 1

# Python runtime unit tests
cd ../asn1python || exit 1
uvx --python=3.11 pytest tests -v || exit 1
# Python code readability and type-hint checks
./tools/check_generated_code.sh || exit 1