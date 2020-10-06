#!/bin/bash
if [ $# -ne 1 ] ; then
    echo "Usage: $0 previousVersionTarball"
    exit 1
fi
OLD_TARBALL="$1"
echo "[-] Building debug version... (mandatory pre-requisite)"
make >make.log 2>&1 || { 
    echo "[x] Failed - please check make.log"
    exit 1
}
echo "[-] Building release version..."
make >make.log 2>&1 || { 
    echo "[x] Failed - please check make.log"
    exit 1
}
rm -f make.log
echo "[-] Extracting $OLD_TARBALL ..."
tar xpf "$OLD_TARBALL" || {
    echo "[x] Failed to decompress $OLD_TARBALL..."
    exit 1
}
echo "[-] Updating contents..."
cd asn1scc || {
    echo "[x] $OLD_TARBALL did not contain an asn1scc folder!"
    exit 1
}
rm -rf ./*.stg
cp ../Asn1f4/bin/Release/*.stg .
cp ../Asn1f4/bin/Release/*.dll .
cp ../Asn1f4/bin/Release/*.exe .
mv Asn1f4.exe asn1.exe
cd ..
echo "[-] Packing new release tarball..."
VERSION="$(./Asn1f4/bin/Release/asn1.exe -v | head -1 | awk '{print $NF}')"
FINAL_TARBALL=asn1scc-bin-${VERSION}.tar.bz2
tar jcpf "${FINAL_TARBALL}" asn1scc || {
    echo "[x] Failed to create $FINAL_TARBALL ..."
    exit 1
}
rm -rf asn1scc
echo "[-] Created $FINAL_TARBALL - all good."
