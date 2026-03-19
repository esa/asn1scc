#!/bin/bash

# Source sdkman initialization script
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Build and test commands
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"

# Scala  & Interop tests
cd PUSCScalaTest || exit 1
dotnet test || exit 1
