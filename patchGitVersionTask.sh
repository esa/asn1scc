#!/bin/bash
nuget restore || exit 1
sudo apt-get install -y --force-yes libgit2-dev libssl-dev
ARCH=$(uname -m)
cd packages/GitVersionTask.3.6.5/lib/linux || exit 1
mkdir -p ${ARCH}
cd ${ARCH}
if [ -f libgit2-baa87df.so ] ; then
    mv libgit2-baa87df.so libgit2-baa87df.so.Windows.people.hardcode.everything
fi
if [ ! -h libgit2-baa87df.so ] ; then
    LIBGIT2_DIR=$(pkg-config --variable=libdir libgit2)
    ln -s $(/bin/ls ${LIBGIT2_DIR}/libgit2.so.* | sort | head -1) libgit2-baa87df.so  || exit 1
fi
cd ../../.. || exit 1
if [ ! -h libgit2-baa87df.so ] ; then
    ln -s $(/bin/ls ${LIBGIT2_DIR}/libgit2.so.* | sort | head -1) libgit2-baa87df.so  || exit 1
fi
