#!/bin/bash
nuget restore || exit 1
xbuild /p:TargetFrameworkVersion="v4.5" || exit 1
cd v4Tests || exit 1
make || exit 1
