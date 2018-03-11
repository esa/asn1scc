#!/bin/bash
nuget restore || exit 1
cd packages/GitVersionTask.3.6.5/lib/linux/x86_64 || exit 1
mv libgit2-baa87df.so libgit2-baa87df.so.Windows.people.hardcode.everything
LIBGIT2_DIR=$(pkg-config --variable=libdir libgit2)
ln -s $(/bin/ls ${LIBGIT2_DIR}/libgit2.so.* | sort | head -1) libgit2-baa87df.so  || exit 1
cd - || exit 1
xbuild /p:TargetFrameworkVersion="v4.5" || exit 1
cd v4Tests || exit 1
make || exit 1
