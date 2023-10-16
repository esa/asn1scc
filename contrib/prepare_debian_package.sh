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
VERSION=3.2.$(cat Asn1f2/SvnVersion.cs | cut -d \" -f 2)
echo Creating a Debian package for ASN1Scc version $VERSION
test -f Asn1f2/bin/Debug/Asn1f2.exe || (echo 'Run ./build.sh first to build ASN1SCC' && false)
rm -rf asn1scc
mkdir -p asn1scc/DEBIAN
mkdir -p asn1scc/$BIN_FOLDER
mkdir -p asn1scc/$SHARE_FOLDER
cp -r Asn1f2/bin/Debug/* asn1scc/$SHARE_FOLDER/
rm -f asn1scc/$SHARE_FOLDER/*.mdb
mv asn1scc/$SHARE_FOLDER/Asn1f2.exe asn1scc/$SHARE_FOLDER/asn1.exe
cd asn1scc/$BIN_FOLDER
ln -s ../share/asn1scc/* .
cd $INITIAL_FOLDER/..
echo "Package: asn1scc
Version: ${VERSION}
Section: base
Priority: optional
Architecture: all
Depends: libmono-system-runtime4.0-cil,libmono-corlib4.0-cil,libmono-system-runtime-serialization-formatters-soap4.0-cil, libmono-system-web4.0-cil,libmono-system-xml4.0-cil,libmono-system4.0-cil,mono-runtime,libmono-system-numerics4.0-cil,libmono-system-data-linq4.0-cil,libmono-corlib2.0-cil,libmono-system2.0-cil,libmono-corlib4.5-cil
Maintainer: Thanassis Tsiodras
Description: ASN.1 Space Certified Compiler - Generate Spark/Ada and C code for ASN.1 uPER or custom binary encoders/decoders
Homepage: http://github.com/maxime-esa/asn1scc" > asn1scc/DEBIAN/control
echo '#!/bin/sh
echo "All done!"
' >> asn1scc/DEBIAN/postinst
chmod +x asn1scc/DEBIAN/postinst
dpkg-deb --build asn1scc
cd $INITIAL_FOLDER
mv ../*.deb .
rm -rf ../asn1scc
echo -e 'Done...\n\nThe .deb file is here:\n'
pwd
ls -l *deb
