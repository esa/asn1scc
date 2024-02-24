#!/bin/bash
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"
cd v4Tests || exit 1
../regression/bin/Debug/net7.0/regression -l c -ws 4 -s false -p 12 || exit 1
../regression/bin/Debug/net7.0/regression -l Ada -ws 4 -s false -p 12 || exit 1
../regression/bin/Debug/net7.0/regression -l c -ws 8 -s true -p 12 -ig || exit 1
../regression/bin/Debug/net7.0/regression -l c -ws 8 -s true -p 12 || exit 1
../regression/bin/Debug/net7.0/regression -l Ada -ws 8 -s true -p 12 || exit 1


