#!/bin/bash
cd /workdir/
git -C asn1scc pull || git clone /app/ asn1scc
cd asn1scc
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"
cd v4Tests || exit 1
../regression/bin/Debug/net7.0/regression -l c -ws 4 -s false -p 16 || exit 1
../regression/bin/Debug/net7.0/regression -l Ada -ws 4 -s false -p 16 || exit 1
../regression/bin/Debug/net7.0/regression -l c -ws 8 -s true -p 16 || exit 1
../regression/bin/Debug/net7.0/regression -l Ada -ws 8 -s true -p 8 || exit 1


