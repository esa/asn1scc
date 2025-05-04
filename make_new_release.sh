#!/bin/bash
if [ $# -ne 1 ] ; then
    echo "Usage: $0 previousVersionTarball"
    exit 1
fi
OLD_TARBALL="$1"
echo "[-] Building release version (including lsp)..."
make -f Makefile.debian publish >make.log 2>&1 || {
    echo "[x] Failed - please check make.log"
    exit 1
}
rm -f make.log
echo "[-] Extracting $OLD_TARBALL ..."
mkdir -p old
cd old
tar xpf "../$OLD_TARBALL" || {
    echo "[x] Failed to decompress $OLD_TARBALL..."
    exit 1
}
echo "[-] Updating contents..."
cd asn1scc || {
    echo "[x] $OLD_TARBALL did not contain an asn1scc folder!"
    exit 1
}
rm -rf ./*.stg
cp -a ../../asn1scc/bin/Release/net9.0/linux-x64/publish/* .
cp -a ../../lsp/Server/Server/bin/Release/net9.0/linux-x64/publish/* .
rm -f asn1.exe
VERSION="$(./asn1scc -v | head -1 | awk '{print $NF}')"
cd ../
echo "[-] Packing new release tarball..."
FINAL_TARBALL=../asn1scc-bin-${VERSION}.tar.bz2
tar jcpf "${FINAL_TARBALL}" asn1scc || {
    echo "[x] Failed to create $FINAL_TARBALL ..."
    exit 1
}
rm -rf asn1scc
cd ..
rmdir old
echo "[-] Created $FINAL_TARBALL - all good."
echo "[-] To create a new tag:"
echo "[-]    $ git tag -a ${VERSION}"
echo "[-]    $ git push --tags"
echo "[-] Then go on github.com, create a new release, and upload the archive"
