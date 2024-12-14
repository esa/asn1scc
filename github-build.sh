#!/bin/bash

# Fix ownership of /app
chown -R myuser:myuser /app

# Ensure HOME is set correctly for sdkman
export HOME=/home/myuser

# Source sdkman initialization script
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Build and test commands
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"
cd v4Tests || exit 1
../regression/bin/Debug/net9.0/regression -l c -ws 4 -s false -p 12 || exit 1
../regression/bin/Debug/net9.0/regression -l Ada -ws 4 -s false -p 12 || exit 1
../regression/bin/Debug/net9.0/regression -l c -ws 8 -s true -p 12 -ig || exit 1
../regression/bin/Debug/net9.0/regression -l c -ws 8 -s true -p 12 || exit 1
../regression/bin/Debug/net9.0/regression -l Ada -ws 8 -s true -p 12 || exit 1

# Scala tests
cd ../PUSCScalaTest || exit 1
dotnet test || exit 1
