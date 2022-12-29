#!/bin/bash
#docker run -ti --rm -v $(pwd):/app -v asn1scc_workdir:/workdir myspark
cd /workdir/
git -C asn1scc pull || git clone /app/ asn1scc
cd asn1scc
dotnet build Antlr/
dotnet build parseStg2/
dotnet build "asn1scc.sln"
dotnet build regression/
cd ../v4Tests || exit 1
../regression/bin/Debug/net7.0/regression -l Ada -ws 8 -s true

