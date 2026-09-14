#!/bin/bash

# Source sdkman initialization script
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Build and test commands
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"

REGRESSION="../regression/bin/Debug/net10.0/regression"
cd v4Tests || exit 1

# Rust tests (non-slim only; slim mode is C-only in the regression tool)
echo "run Rust tests, with word-size=4"
$REGRESSION -l Rust -ws 4 -s false -p 12 || exit 1

echo "run Rust tests, with word-size=8"
$REGRESSION -l Rust -ws 8 -s false -p 12 || exit 1

# Scala & Interop tests
cd ../PUSCInteropTest || exit 1
dotnet test || exit 1
