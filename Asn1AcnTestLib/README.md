# ASN.1/ACN Test Library (Asn1AcnTestLib)

## Overview

This test library provides comprehensive coverage of ASN.1 and ACN (ASN.1 Constraints Notation) specifications for validating the asn1scc compiler implementation. The library is designed to systematically test all requirements specified for the compiler's language backends (particularly Python).

## Purpose

Unlike complex real-world libraries (such as PUS-C), this test library focuses on:
- **Systematic Coverage**: Each requirement is tested in isolation
- **Comprehensive Testing**: All ASN.1/ACN features are covered
- **Modularity**: Small, focused test files organized by feature
- **Clarity**: Easy to understand what each test validates

## Directory Structure

```
Asn1AcnTestLib/
├── primitives/              # Primitive ASN.1 types
│   ├── boolean/            # BOOLEAN tests (Reqs 1-3)
│   ├── integer/            # INTEGER tests (Reqs 4-13)
│   ├── real/               # REAL tests (Reqs 14-17)
│   ├── bitstring/          # BIT STRING tests (Reqs 18-20)
│   ├── octetstring/        # OCTET STRING tests (Reqs 21-23)
│   ├── ia5string/          # IA5String tests (Reqs 24-25)
│   ├── enumerated/         # ENUMERATED tests (Reqs 34-35, 44)
│   └── null/               # NULL tests (Req 36)
│
├── structured/              # Structured ASN.1 types
│   ├── sequence/           # SEQUENCE tests (Reqs 26-28)
│   ├── sequence-of/        # SEQUENCE OF tests (Reqs 29-31)
│   └── choice/             # CHOICE tests (Reqs 32-33, 49)
│
├── advanced/                # Advanced ASN.1 features
│   ├── subtyping/          # Subtyping tests (Reqs 37, 52)
│   ├── parameterized/      # Parameterized types (Req 38)
│   ├── imports/            # Module imports (Reqs 39-40)
│   └── nested/             # Nested ACN specs (Reqs 46-48)
│
├── acn-attributes/          # ACN-specific attributes
│   ├── alignment/          # align-to-next tests (Reqs 41-42)
│   ├── present-when/       # present-when tests (Reqs 43, 49)
│   ├── acn-fields/         # ACN-only fields (Req 50)
│   └── save-position/      # save-position tests (Req 51)
│
└── encoding/                # Encoding format tests
    ├── acn/                # ACN encoding (Req 53)
    ├── uper/               # UPER encoding (Req 54)
    └── xer/                # XER encoding (Req 55, optional)
```

## Test Categories

### 1. Primitive Types (Requirements 1-25, 34-36, 44)

#### BOOLEAN (Reqs 1-3)
- `boolean-basic.asn1`: Basic BOOLEAN support
- `boolean-false-value.*`: BOOLEAN with custom false-value ACN encoding
- `boolean-true-value.*`: BOOLEAN with custom true-value ACN encoding
- `boolean-combined.*`: Combined true/false value patterns

#### INTEGER (Reqs 4-13)
- `integer-basic.asn1`: Unconstrained INTEGER
- `integer-range-zero-start.asn1`: Ranges starting at 0 (0..255, 0..65535)
- `integer-range-positive.asn1`: Ranges starting with positive numbers (1..31, 100..1000)
- `integer-range-negative.asn1`: Ranges with negative numbers (-128..127, -1000..1000)
- `integer-acn-size.*`: ACN size attribute (1, 2, 3, 4, 5, 6, 7, 8, 12, 14, 16, 24, 32, 48, 64 bits)
- `integer-endianness-big.*`: Big-endian encoding
- `integer-endianness-little.*`: Little-endian encoding
- `integer-encoding-posint.*`: pos-int encoding attribute
- `integer-encoding-twoscomplement.*`: twos-complement encoding attribute

#### REAL (Reqs 14-17)
- `real-ieee754-32.*`: 32-bit IEEE 754-1985 format
- `real-ieee754-64.*`: 64-bit IEEE 754-1985 format
- `real-endianness-big.*`: Big-endian REAL encoding
- `real-endianness-little.*`: Little-endian REAL encoding

#### BIT STRING (Reqs 18-20)
- `bitstring-basic.asn1`: Unconstrained BIT STRING
- `bitstring-size-fixed.asn1`: Fixed size constraints SIZE(n)
- `bitstring-size-range.asn1`: Variable size constraints SIZE(min..max)
- `bitstring-acn-size.*`: ACN size attribute for dynamic length

#### OCTET STRING (Reqs 21-23)
- `octetstring-basic.asn1`: Unconstrained OCTET STRING
- `octetstring-size-fixed.asn1`: Fixed size constraints SIZE(n)
- `octetstring-size-range.asn1`: Variable size constraints SIZE(min..max)
- `octetstring-acn-size.*`: ACN size attribute for dynamic length

#### IA5String (Reqs 24-25)
- `ia5string-basic.asn1`: Unconstrained IA5String
- `ia5string-size-fixed.asn1`: Fixed size constraints SIZE(n)
- `ia5string-size-range.asn1`: Variable size constraints SIZE(min..max)

#### ENUMERATED (Reqs 34-35, 44)
- `enumerated-basic.asn1`: Basic ENUMERATED with named values
- `enumerated-acn-size.*`: ACN size attribute for encoding size
- `enumerated-encode-values.*`: encode-values attribute (values vs indices)

#### NULL (Req 36)
- `null-basic.asn1`: NULL type for absence of value

### 2. Structured Types (Requirements 26-33, 49)

#### SEQUENCE (Reqs 26-28)
- `sequence-basic.asn1`: Basic SEQUENCE structured types
- `sequence-optional.asn1`: SEQUENCE with OPTIONAL components
- `sequence-default.asn1`: SEQUENCE with DEFAULT values

#### SEQUENCE OF (Reqs 29-31)
- `sequence-of-basic.asn1`: Basic homogeneous arrays
- `sequence-of-size-fixed.asn1`: Fixed size arrays SIZE(n)
- `sequence-of-size-range.asn1`: Variable size arrays SIZE(min..max)
- `sequence-of-acn-size.*`: ACN size attribute for dynamic arrays

#### CHOICE (Reqs 32-33, 49)
- `choice-basic.asn1`: Basic CHOICE discriminated unions
- `choice-determinant.*`: ACN determinant attribute for choice selector

### 3. Advanced Features (Requirements 37-40, 46-48, 52)

#### Subtyping (Reqs 37, 52)
- `subtyping-value-constraints.asn1`: Value range constraints
- `subtyping-with-components.asn1`: WITH COMPONENTS constraints
- `subtyping-nested.asn1`: Nested constraint refinement
- `subtyping-pattern.asn1`: Permitted alphabet and pattern constraints

#### Parameterized Types (Req 38)
- `parameterized-basic.asn1`: Simple parameterized type definitions
- `parameterized-complex.asn1`: Complex multi-parameter scenarios

#### Module Imports (Reqs 39-40)
- `imports-base-types.asn1`: Base module with reusable types
- `imports-user-module.asn1`: IMPORTS declarations and cross-module references
- `imports-extended-types.asn1`: Additional types for multi-module testing
- `imports-multi-module.asn1`: Importing from multiple modules

#### Nested ACN (Reqs 46-48)
- `nested-acn-basic.*`: Nested ACN specifications
- `nested-acn-parametrization.*`: ACN parametrization from nested subfields
- `nested-acn-conditional.*`: Conditional presence in nested structures

### 4. ACN Attributes (Requirements 41-43, 49-51)

#### Alignment (Reqs 41-42)
- `alignment-byte.*`: align-to-next byte boundary
- `alignment-word.*`: align-to-next word/dword/qword boundaries

#### Present-When (Reqs 43, 49)
- `present-when-sequence.*`: Conditional encoding for optional SEQUENCE components
- `present-when-choice.*`: Conditional encoding for CHOICE alternatives

#### ACN-Only Fields (Req 50)
- `acn-fields-size.*`: ACN fields for size encoding (not in ASN.1)
- `acn-fields-present-when.*`: ACN fields for present-when conditions
- `acn-fields-combined.*`: Combined ACN-only fields

#### Save-Position (Req 51)
- `save-position-basic.*`: Basic save-position attribute
- `save-position-complex.*`: save-position with other ACN attributes

### 5. Encoding Formats (Requirements 53-55)

#### ACN Encoding (Req 53)
- `acn-encoding-comprehensive.*`: Comprehensive ACN encoding test

#### UPER Encoding (Req 54)
- `uper-encoding-basic.asn1`: Basic UPER encoding
- `uper-encoding-complex.asn1`: Advanced UPER scenarios

#### XER Encoding (Req 55, Optional)
- `xer-encoding-basic.asn1`: Basic XER (XML) encoding

## Usage

### Compiling Test Files

To use this test library with asn1scc:

```bash
# Compile a single test module
asn1scc -python path/to/test.asn1

# Compile test with ACN specification
asn1scc -python path/to/test.asn1 path/to/test.acn

# Compile module with imports (compile all related modules together)
asn1scc -python imports-base-types.asn1 imports-user-module.asn1
```

### Test Organization

Each test file is designed to:
1. Test a specific requirement or feature
2. Be compiled independently (except for import tests)
3. Include clear comments indicating which requirement is tested
4. Use realistic type names and structures

### Requirements Coverage

This library covers requirements 1-55 from the requirements specification:
- **Reqs 1-25**: Primitive types (BOOLEAN, INTEGER, REAL, strings)
- **Reqs 26-40**: Structured types and advanced features
- **Reqs 41-51**: ACN-specific attributes
- **Reqs 52-55**: Encoding formats and comprehensive constraints
- **Reqs 56-64**: Code generation quality (tested through generated code)

## Validation Strategy

The test library enables validation through:

1. **Compilation Testing**: Verify all files compile without errors
2. **Encoding/Decoding Testing**: Test round-trip encoding and decoding
3. **Interoperability Testing**: Verify Python output matches C implementation (Req 56)
4. **Constraint Validation**: Test that constraints are properly enforced (Reqs 60, 62)
5. **Error Handling**: Verify proper error messages (Reqs 58, 59, 63, 64)

## Notes

- Test files use `.asn1` extension for ASN.1 definitions
- ACN specifications use `.acn` extension
- Each test is self-contained with clear requirement references
- Files are organized by feature category for easy navigation
- Comments in files explain what is being tested

## Maintenance

When adding new requirements:
1. Create test files in the appropriate category directory
2. Name files descriptively: `<feature>-<variant>.asn1`
3. Add requirement number in file header comments
4. Update this README with new test descriptions
5. Ensure tests are isolated and focused on specific features

## References

- ASN.1 Standard: ITU-T X.680
- ACN Specification: ESA ACN documentation
- Requirements: See `../claude_input/requirements.txt`
