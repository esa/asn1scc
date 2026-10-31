#!/bin/bash

set -e

echo "[-] Building Linux release (including lsp)..."

make -f Makefile.debian publish >make.log 2>&1 || {
    echo "[x] Linux build failed - please check make.log"
    exit 1
}

echo "[-] Building Windows release (including lsp)..."

make -f Makefile.debian publish-win64 >>make.log 2>&1 || {
    echo "[x] Windows build failed - please check make.log"
    exit 1
}

rm -f make.log

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

LINUX_DIR="${TMPDIR}/asn1scc"
WIN_DIR="${TMPDIR}/asn1scc-win64"

echo "[-] Creating Linux release directory..."

mkdir -p "${LINUX_DIR}"

cp -a asn1scc/bin/Release/net10.0/linux-x64/publish/* "${LINUX_DIR}/"
cp -a lsp/Server/Server/bin/Release/net10.0/linux-x64/publish/* "${LINUX_DIR}/"
cp -a contrib/icd-generator.py "${LINUX_DIR}/"

echo "[-] Creating Windows release directory..."

mkdir -p "${WIN_DIR}"

cp -a asn1scc/bin/Release/net10.0/win-x64/publish/* "${WIN_DIR}/"
cp -a lsp/Server/Server/bin/Release/net10.0/win-x64/publish/* "${WIN_DIR}/"
cp -a contrib/icd-generator.py "${WIN_DIR}/"

VERSION="$("${LINUX_DIR}/asn1scc" -v | head -1 | awk '{print $NF}')"

LINUX_TARBALL="asn1scc-bin-${VERSION}.tar.bz2"
WIN_TARBALL="asn1scc-bin-win64-${VERSION}.tar.bz2"

echo "[-] Packing Linux release..."

tar jcpf "${LINUX_TARBALL}" -C "${TMPDIR}" asn1scc || {
    echo "[x] Failed to create ${LINUX_TARBALL}"
    exit 1
}

echo "[-] Packing Windows release..."

tar jcpf "${WIN_TARBALL}" -C "${TMPDIR}" asn1scc-win64 || {
    echo "[x] Failed to create ${WIN_TARBALL}"
    exit 1
}

echo "[-] Created:"
echo "[-]    ${LINUX_TARBALL}"
echo "[-]    ${WIN_TARBALL}"
echo "[-]"
echo "[-] To create a new tag:"
echo "[-]    git tag -a ${VERSION}"
echo "[-]    git push --tags"
echo "[-] Then go to GitHub, create a new release, and upload both archives"
