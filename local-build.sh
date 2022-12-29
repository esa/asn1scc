#!/bin/bash
#docker run -ti --rm -v $(pwd):/app -v asn1scc_workdir:/workdir myspark
cd /workdir/
git -C asn1scc pull || git clone /app/ asn1scc
cd asn1scc
dotnet build Antlr/ || exit 1
#dotnet build "asn1scc.sln"|| exit 1
cd v4Tests-32 || exit 1
make || exit 1
cd ../v4Tests || exit 1
make || exit 1
