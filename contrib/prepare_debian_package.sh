#!/bin/bash -e

#Change this to desired install path: /opt/asn1scc or /usr/local
INSTALL_ROOT=/usr
if [ $# -eq 1 ] ; then
  INSTALL_ROOT=$1
fi
echo "Preparing deb with path $INSTALL_ROOT"

INITIAL_FOLDER=$(pwd)
SHARE_FOLDER=$INSTALL_ROOT/share/asn1scc
BIN_FOLDER=$INSTALL_ROOT/bin
cd ..
BINARY_PATH=./asn1scc/bin/Release/net7.0/linux-x64/publish
VERSION="$(${BINARY_PATH}/asn1scc -v | head -1 | awk '{print $NF}')"
echo "[-] Creating a Debian package for ASN1SCC version ${VERSION}"
test -f ${BINARY_PATH}/asn1scc || (echo 'You must first build ASN1SCC (make -C .. -f Makefile.debian publish)' && false)
rm -rf asn1scc_deb
mkdir -p asn1scc_deb/DEBIAN
mkdir -p asn1scc_deb/$BIN_FOLDER
mkdir -p asn1scc_deb/$SHARE_FOLDER
cp -r ${BINARY_PATH}/* asn1scc_deb/$SHARE_FOLDER/
cd asn1scc_deb/$BIN_FOLDER
ln -s ../share/asn1scc/asn1scc .
cd $INITIAL_FOLDER/..
echo "Package: asn1scc
Version: ${VERSION}
Section: base
Priority: optional
Architecture: all
Depends: 
Maintainer: George Mamais
Description: ASN.1 Space Certified Compiler - Generate Spark/Ada/Scala/C code for ASN.1 uPER or custom binary encoders/decoders
Homepage: http://github.com/esa/asn1scc" > asn1scc_deb/DEBIAN/control
echo '#!/bin/sh
echo "All done!"
' >> asn1scc_deb/DEBIAN/postinst
chmod +x asn1scc_deb/DEBIAN/postinst
dpkg-deb --build asn1scc_deb
cd $INITIAL_FOLDER
mv ../*.deb .
rm -rf ../asn1scc_deb
echo -e '[-] Done...\n\nThe .deb file is here:\n'
pwd
ls -l *deb
