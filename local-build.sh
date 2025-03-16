#!/bin/bash
echo "****"
echo $1
echo "****"
source "$HOME/.sdkman/bin/sdkman-init.sh"
echo "git config --global --add safe.directory /app/.git"

cd /workdir/ || exit
git config --global --add safe.directory /app/.git || exit
git config --global --add safe.directory /app//.git || exit
git config --global --add safe.directory /app/asn1scc/.git || exit
echo "git clone /app/ asn1scc"
git clone /app/ asn1scc || exit
cd asn1scc || exit
echo "git checkout $1"
git checkout $1 || exit
echo "git config --global --add safe.directory /app/.git"
git config --global --add safe.directory /app/.git || exit
echo "git pull"
git pull || exit
echo "git rev-parse --abbrev-ref HEAD"
git rev-parse --abbrev-ref HEAD || exit
echo "dotnet build"
dotnet build Antlr/ || exit 1
dotnet build parseStg2/ || exit 1
dotnet build "asn1scc.sln" || exit 1
cd v4Tests || exit 1
echo "run local tests"
../regression/bin/Debug/net9.0/regression -l c -ws 4 -s false -p 48 || exit 1
../regression/bin/Debug/net9.0/regression -l Ada -ws 4 -s false -p 48 || exit 1
../regression/bin/Debug/net9.0/regression -l c -ws 8 -s true -p 48 -ig || exit 1
../regression/bin/Debug/net9.0/regression -l c -ws 8 -s true -p 48 || exit 1
../regression/bin/Debug/net9.0/regression -l Ada -ws 8 -s true -p 48 || exit 1

#scala tests
echo "run scala tests"
cd ../PUSCScalaTest || exit 1
dotnet test || exit 1
