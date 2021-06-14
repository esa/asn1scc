#!/bin/bash
dotnet publish -c Release --self-contained true -r linux-x64  asn1scc.sln || exit 1
cd v4Tests-32 || exit 1
make || exit 1
cd ../v4Tests || exit 1
make || exit 1
