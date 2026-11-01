# Change Log

## 4.9.64 - Maintenance release
- New ICD generator (contrib/icd-generator.py): renders an ICD in HTML from
  the new `asn1scc -icdRaw` JSON dump, with an embedded asn1scc invocation
  (the user passes the .asn/.acn files directly), optional `--root` for PDU-
  focused documents or a sequential mode covering all types, a compact
  bit-level representation of the encoding, nested tables with offsets
  relative to the first item, type hyperlinks, units, and all implicit
  fields (length/presence/choice determinants, padding).
- ICD dumps now expose ACN mapping functions: a mapped INTEGER (e.g. a size-0
  length determinant whose value is computed at run time) carries a note in
  the ICD (`-icdRaw` row comments and the `-x` XML `mapping-function`
  attribute), and the generator marks it with a star in the table and
  compact views.
- Example grammars and reference output in contrib/icd-test (CCSDS space
  packet, flight software ASW).

## 4.5.0.12 - Maintenance release
- Add Asn1Real_Equal function in the C backend to align with Ada

## 4.5.0.11 - Maintenance release
- Fix the -ig output for C code (generate #define instead of "const" in C).

## 4.5.0.2 - Maintenance release
- Bugfix on the command line with parameter ordering 
- Bugfix: missing include in Ada generated code
- Bugfix in OCTET STRING (CONTAINING Other-Type)
- Use "long" for the .exist field (for optional fields) in C to align with Ada
- Fix cppcheck findings
- Detect inconsistencies in WITH COMPONENTS subtypes
- Support IA5String values according to the standard
- Generate const globals with -ig for type initialization
- Improve stack usage for Ada initialization functions


## 3.2.75
- Removed the pragma Export in the Ada backend when using ASN.1 constants

## 3.2.61
- Minor bugfixes and improvements of the custom Stg backends
- New rename policy option
- New VDM backend

## 3.2.3
- Added error message for unsupported string and UTCTime types

## 3.2.2
- Various minor bugfixes
- Improvements of ICD backend for ACN

## 3.2.0
- Minor API change in ICD backends - templates now contain code to customize
  the formatted output of the grammar.

## 3.1.4
- When adding fields to a SEQUENCE in an ACN model, the comments are now
  propagated to the ICD backends.

# 3.1.1/2/3
- Various minor bugfixes, in particular related to the handling of cyclic
  dependencies

# 3.1.0
- Change in the API of the ICD backends: the comments are not only sent as a
  string but also as a list, allowing to extract the first line
- Added a Latex template for ICDs (in contrib) / Experimental
