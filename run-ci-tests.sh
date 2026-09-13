#!/bin/bash

# Source sdkman initialization script
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Build and test commands
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"

# Regression tests
cd v4Tests || exit 1
../regression/bin/Debug/net10.0/regression -l Rust -ws 4 -s false -p 12 || exit 1
../regression/bin/Debug/net10.0/regression -l Rust -ws 4 -s true -p 12 || exit 1

# Scala & Interop tests
cd ../PUSCInteropTest || exit 1
dotnet test || exit 1
