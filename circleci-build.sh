#!/bin/bash
#dotnet build Antlr/ || exit 1
#dotnet build "asn1scc.sln"|| exit 1
cd v4Tests-32 || exit 1
make || exit 1
cd ../v4Tests || exit 1
make || exit 1
