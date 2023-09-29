ASN.1 PUS-C Types Library -- Coding Standard
============================================

## Folders structure
```
asn1-pusc-lib
 |- service-01
 |- service-02
 |- ...
 |- service-NN
 |- common
 |- system-objects
 |- ccsds
```

* `service-NN` - folder containing models of messages defined by PUS Service NN
                 (separate folder for each service included in library)
* `common` - folder containing models used by more than one service
* `system-objects` - folder containing models of types defined as System Objects
                     or their properties in ECSS
* `ccsds` - models representing CCSDS packets

## Service folder structure
```
service-NN
 |- PUS-NN-1.asn1
 |- PUS-NN-1.acn
 |- ...
 |- ServiceCommonObject.asn1
 |- ServiceCommonObject.acn
 |- ...
 |- meta.json
```

* `PUS-NN-MM.asn1` and `PUS-NN-MM.acn` - files containing models of message MM from service NN
                                         (separate files for each message type modeled from service)
* `ServiceCommonObjectX.asn1` and `ServiceCommonObjectX.acn` - optional files defining objects
                                      used by more than one message in service
                                      (if required there can be more than one pair of such files,
                                      names should describe what models are included in that file).
* `meta.json` - file containing metadata describing service NN

## Message file layout
```asn1
PUS-NN-MM DEFINITIONS AUTOMATIC TAGS ::= BEGIN
EXPORTS ALL;
Tx-NN-MM-MessageNameFromEcss ::= NULL
END
```

* `PUS-NN-MM` - definitions block named after file name (message `MM` from service `NN`)
* `Tx-NN-MM-MessageNameFromEcss` - main type assignment representing message type
                                   (prefix should be `TC` for telecommands or `TM` for telemetry)

## ASN.1/ACN files Coding Standard
Each file should begin with
[License template](https://github.com/n7space/asn1-pusc-lib/blob/master/license-template.txt)
as found in project's repository.

Each file should contain only single `DEFINITIONS` block, named after file name.

Type assignments and definitions block names should follow `CamelCaseStartingWithUpperCase`,
if required - with prefixes defined in previous paragraphs.

Type fields should follow `camelCaseStartingWithLowerCase`.

Hyphen (`-`) is allowed to separate numbers in names or upper-cased abbreviations (like ID).

Lines should not exceed 100 characters.