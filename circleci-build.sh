#!/bin/bash

source "$HOME/.sdkman/bin/sdkman-init.sh"

dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"
cd v4Tests || exit 1
../regression/bin/Debug/net9.0/regression -l c -ws 4 -s false -p 16 || exit 1
../regression/bin/Debug/net9.0/regression -l Ada -ws 4 -s false -p 16 || exit 1
../regression/bin/Debug/net9.0/regression -l c -ws 8 -s true -p 16 || exit 1
../regression/bin/Debug/net9.0/regression -l Ada -ws 8 -s true -p 8 || exit 1

