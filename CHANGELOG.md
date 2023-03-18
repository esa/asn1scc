# Change Log

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
