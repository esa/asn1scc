# ASN.1 PUS-C Types Library
ASN.1 implementation of the PUS-C ECSS standard using ACN encoding.

[![Build Status](https://travis-ci.org/n7space/asn1-pusc-lib.svg?branch=master)](https://travis-ci.org/n7space/asn1-pusc-lib)

### [Project Home Page](https://n7space.github.io/asn1-pusc-lib/)

See [ECSS-E-ST-70-41C](http://ecss.nl/standard/ecss-e-st-70-41c-space-engineering-telemetry-and-telecommand-packet-utilization-15-april-2016/) for details about the standard.

See [ASN1SCC](https://github.com/ttsiodras/asn1scc) for details about ACN encoding.

ASN.1 components are ment to work with any ASN.1 based tool,
but project itself is prepared to be included as "ASN.1 Components Library" in
[QtCreator](https://www.qt.io/download)'s plugin [asn1scc.IDE](https://github.com/n7space/asn1scc.IDE).

## Installation for asn1scc.IDE
0. Install desired version of QtCreator and [Plugin](https://github.com/n7space/asn1scc.IDE/releases)
1. Download library [release](https://github.com/n7space/asn1-pusc-lib/releases)
2. Unpack library package
3. If you want plugin to automatically detect library, copy it to specific directory:
   * To be visible by every plugin user
        (assuming QtCreator was installed under default path):
        - Linux: `/opt/Qt/Tools/QtCreator/share/qtcreator/asn1acn/libs`
        - Windows: `C:\Qt\Tools\QtCreator\share\qtcreator\asn1acn\libs`
   * To be visible only by current user:
        - Linux: `~/.config/QtProject/qtcreator/asn1acn/libs`
        - Windows: `%APPDATA%\QtProject\qtcreator\asn1acn\libs`
