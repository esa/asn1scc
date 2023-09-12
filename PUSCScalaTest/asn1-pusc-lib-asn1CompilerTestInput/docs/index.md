**ASN.1 PUS-C Types Library** is an ASN.1 implementation of the PUS-C ECSS standard using ACN encoding.

See [ECSS-E-ST-70-41C](http://ecss.nl/standard/ecss-e-st-70-41c-space-engineering-telemetry-and-telecommand-packet-utilization-15-april-2016/) for details about the PUS-C standard.

See [ASN1SCC](https://github.com/ttsiodras/asn1scc) for details about ACN encoding.

Library was developed by [N7 Space](http://www.n7space.com) and funded by European Space Agency ([ESA](http://www.esa.int)) and is distributed under [GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.html) license.

# Requirements
ASN.1 components are ment to work with any ASN.1 based tool,
but project itself is prepared to be included as "ASN.1 Components Library" in
[Qt Creator](https://www.qt.io/download)'s plugin [asn1scc.IDE](https://github.com/n7space/asn1scc.IDE).
ACN encoding is supported by [ASN1SCC](https://github.com/ttsiodras/asn1scc) - ASN.1/ACN compiler for embedded systems
(distributed with asn1scc.IDE).

Library includes CCSDS utility functions, which were implemented in C in C99 standard.

# Installation for asn1scc.IDE
0. Install desired version of [Qt Creator](https://www.qt.io/download) and [Plugin](https://github.com/n7space/asn1scc.IDE/releases)
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
		
# Development
 * Library itself is an asn1scc.IDE project and should be developed in it.
 * Library metadata follows [JSON schema](https://github.com/n7space/asn1scc.IDE/tree/4.7.0/schemas) defined by asn1scc.IDE.
 * Library and it's metadata is validated using Bash script `sanity-check.sh`, it requires Python and ASN1SCC to run.
 * See [Coding Standard](CodingStandard.html)