# ACN Parser Documentation

This document details the ACN (ASN.1 Control Notation) parser implementation using ANTLR v3.

## Overview

**Location**: `Antlr/acn.g`
**Purpose**: Parse ACN encoding specifications that complement ASN.1 type definitions
**Integration**: ACN specs are merged with ASN.1 AST to create unified Asn1AcnAst

## ACN Language Concepts

ACN (ASN.1 Control Notation) allows specifying custom binary encodings for ASN.1 types, enabling:
- **Legacy Protocol Support**: Interface with existing binary formats
- **Optimized Encodings**: Custom bit layouts for efficiency
- **Hardware Integration**: Match specific hardware requirements
- **Non-standard Formats**: Handle proprietary binary protocols

## Grammar Structure

### Top-Level Structure

```antlr
modules
    : module+
    ;

module
    : moduleHeader DEFINITIONS moduleBody
    ;

moduleHeader
    : UCASE_ID (LID ('.' UCASE_ID)*)?  // Must match corresponding ASN.1 module
    ;

moduleBody
    : ASSIGN BEGIN_ (imports)? assignments END
    ;
```

**Key Difference from ASN.1**: ACN modules must reference existing ASN.1 modules, not define new types.

### Assignment Types

#### Type Encoding Assignments
```antlr
typeEncodingAssignment
    : UCASE_ID typeEncodingSpec
    ;

typeEncodingSpec
    : L_BRACKET encodingSpec R_BRACKET
    | childrenEncodingSpec
    ;
```

**Example**:
```acn
-- ASN.1 definition
Temperature ::= INTEGER (-40..85)

-- ACN encoding specification
Temperature [] {
    encoding pos-int,  -- Positive integer encoding
    size 8,           -- 8 bits
    endianness little  -- Little-endian byte order
}
```

#### Field Encoding Specifications
```antlr
childrenEncodingSpec
    : L_BRACE childSpec* R_BRACE
    ;

childSpec
    : LID (L_BRACKET encodingSpec R_BRACKET)? (childrenEncodingSpec)?
    | LID parameterSpec
    | LID presentWhenSpec
    ;
```

### Encoding Specifications

#### Integer Encodings
```antlr
integerEncodingSpec
    : ENCODING encodingKind (COMMA sizeSpec)? (COMMA endiannessSpec)?
    ;

encodingKind
    : POS_INT          // Positive integer (unsigned)
    | TWOS_COMPLEMENT  // Two's complement (signed)
    | BCD              // Binary Coded Decimal
    | ASCII            // ASCII digits
    ;

sizeSpec
    : SIZE sizeValue
    ;

sizeValue
    : INT              // Fixed size in bits
    | LID              // Reference to parameter
    | NULL_KW          // Variable length
    ;

endiannessSpec
    : ENDIANNESS endianValue
    ;

endianValue
    : BIG              // Big-endian (network byte order)
    | LITTLE           // Little-endian
    ;
```

**Example Integer Encodings**:
```acn
Counter [] {
    encoding pos-int,
    size 32,
    endianness big
}

SignedValue [] {
    encoding twos-complement,
    size 16,
    endianness little
}

BCDDigits [] {
    encoding bcd,
    size 8  -- 2 BCD digits in 8 bits
}
```

#### Real Number Encodings
```antlr
realEncodingSpec
    : ENCODING realEncodingKind (COMMA endiannessSpec)?
    ;

realEncodingKind
    : IEEE754_32       // 32-bit IEEE 754 float
    | IEEE754_64       // 64-bit IEEE 754 double
    | FIXED_POINT      // Fixed-point representation
    ;
```

#### String Encodings
```antlr
stringEncodingSpec
    : ENCODING stringEncodingKind (COMMA sizeSpec)?
    ;

stringEncodingKind
    : ASCII            // ASCII encoding
    | UTF8             // UTF-8 encoding
    | UTF16            // UTF-16 encoding
    | UTF32            // UTF-32 encoding
    ;
```

#### Boolean Encodings
```antlr
booleanEncodingSpec
    : ENCODING booleanEncodingKind
    ;

booleanEncodingKind
    : TRUE_VALUE trueValueSpec FALSE_VALUE falseValueSpec
    | PATTERN bitPattern
    ;

trueValueSpec
    : STRING           // String representation
    | bitPattern       // Bit pattern
    ;
```

**Boolean Encoding Examples**:
```acn
Flag [] {
    encoding true-value "1", false-value "0"
}

StatusBit [] {
    encoding pattern '1'B  -- Single bit: 1=true, 0=false
}
```

### Alignment and Padding

```antlr
alignmentSpec
    : ALIGN alignmentValue
    ;

alignmentValue
    : BYTE             // Align to byte boundary
    | WORD             // Align to word boundary
    | DWORD            // Align to double-word boundary
    | INT              // Custom bit alignment
    ;
```

**Alignment Examples**:
```acn
Message [] {
    header [] { align byte },    -- Align header to byte boundary
    body [] { align dword },     -- Align body to 32-bit boundary
    footer []                    -- No specific alignment
}
```

### Present-When Conditions

ACN supports conditional encoding based on field values:

```antlr
presentWhenSpec
    : PRESENT_WHEN presentWhenCondition
    ;

presentWhenCondition
    : EQUAL_TO value
    | NOT_EQUAL_TO value
    | GREATER_THAN value
    | LESS_THAN value
    | IN valueSet
    | expression
    ;
```

**Present-When Examples**:
```acn
-- ASN.1 definition
Message ::= SEQUENCE {
    messageType INTEGER (1..10),
    priority INTEGER (0..7) OPTIONAL,
    payload OCTET STRING OPTIONAL
}

-- ACN encoding
Message [] {
    messageType [],
    priority present-when messageType = 1,      -- Only present for type 1
    payload present-when messageType IN {2..5}  -- Present for types 2-5
}
```

### Parameters and References

ACN supports parameterized encodings:

```antlr
parameterSpec
    : parameterName ASSIGN parameterValue
    ;

parameterValue
    : value
    | referencedValue
    | expression
    ;
```

**Parameter Examples**:
```acn
-- Length-prefixed arrays
DataArray [] {
    length [],
    data [] {
        size length * 8  -- Array size depends on length field
    }
}

-- Conditional size based on version
VersionedData [] {
    version [],
    data [] {
        size version = 1 ? 16 : 32  -- Different sizes for different versions
    }
}
```

### Pattern Specifications

```antlr
pattern
    : binaryPattern
    | hexPattern
    | stringPattern
    ;

binaryPattern
    : BINARY_STRING    // '10101010'B
    ;

hexPattern
    : HEX_STRING       // 'DEADBEEF'H
    ;
```

## AST Generation and Processing

### ACN AST Structure

**Location**: `FrontEndAst/AcnTypes.fs`

```fsharp
type AcnEncodingSpec =
    | AcnInteger of AcnIntEncodingSpec
    | AcnReal of AcnRealEncodingSpec
    | AcnString of AcnStringEncodingSpec
    | AcnBoolean of AcnBoolEncodingSpec
    | AcnEnum of AcnEnumEncodingSpec
    | AcnBitString of AcnBitStringEncodingSpec
    | AcnOctetString of AcnOctetStringEncodingSpec

type AcnIntEncodingSpec = {
    encoding: AcnIntEncoding
    size: AcnIntSize
    endianness: AcnEndianness option
    alignment: AcnAlignment option
}

type AcnIntEncoding =
    | PositiveInteger        // pos-int
    | TwosComplement        // twos-complement
    | BinaryCodedDecimal    // bcd
    | AsciiEncoding         // ascii

type AcnIntSize =
    | AcnIntSizeFixed of int           // Fixed bit count
    | AcnIntSizeParameter of string    // Parameter reference
    | AcnIntSizeNull                   // Variable length
```

### ACN Parser Integration

**Location**: `FrontEndAst/AcnCreateFromAntlr.fs`

```fsharp
let createAcnAstFromAntlrTree (tree: ITree) : AcnAst =
    match tree.Type with
    | ACN_MODULE -> processAcnModule tree
    | TYPE_ENCODING -> processTypeEncoding tree
    | CHILD -> processChildSpec tree
    | PRESENT_WHEN_EXP -> processPresentWhen tree
    // ... handle all ACN node types
```

### Merging with ASN.1 AST

**Location**: `FrontEndAst/Asn1AcnAst.fs`

The ACN specifications are merged with ASN.1 types to create a unified AST:

```fsharp
let mergeAsn1WithAcn (asn1Ast: Asn1Ast) (acnAst: AcnAst) : Asn1AcnAst =
    // 1. Validate that ACN references correspond to ASN.1 types
    validateAcnReferences asn1Ast acnAst

    // 2. For each ASN.1 type, find corresponding ACN specification
    let mergedTypes =
        asn1Ast.types
        |> Map.map (fun typeName asn1Type ->
            match Map.tryFind typeName acnAst.typeSpecs with
            | Some acnSpec ->
                { asn1Type with acnInfo = Some acnSpec }
            | None ->
                asn1Type  // No ACN spec, use default encoding
        )

    // 3. Handle ACN parameters and references
    resolveAcnParameters mergedTypes
```

## ACN Validation

### Reference Validation

**Location**: `FrontEndAst/CheckAsn1.fs:validateAcnReferences`

```fsharp
let validateAcnReferences (asn1Types: Map<string, Asn1Type>) (acnSpecs: AcnAst) : ValidationResult =
    // Check that every ACN type reference has corresponding ASN.1 type
    let unresolvedRefs =
        acnSpecs.typeSpecs
        |> Map.keys
        |> List.filter (fun typeName -> not (Map.containsKey typeName asn1Types))

    if List.isEmpty unresolvedRefs then
        Ok ()
    else
        Error (sprintf "ACN references undefined ASN.1 types: %A" unresolvedRefs)
```

### Encoding Compatibility

**Location**: `FrontEndAst/CheckAsn1.fs:validateEncodingCompatibility`

```fsharp
let validateEncodingCompatibility (asn1Type: Asn1Type) (acnSpec: AcnEncodingSpec) : ValidationResult =
    match (asn1Type.Kind, acnSpec) with
    | (Integer intType, AcnInteger acnIntSpec) ->
        // Validate that ACN integer encoding is compatible with ASN.1 integer constraints
        validateIntegerEncoding intType acnIntSpec

    | (Real realType, AcnReal acnRealSpec) ->
        // Validate real number encoding compatibility
        validateRealEncoding realType acnRealSpec

    | (Boolean, AcnBoolean acnBoolSpec) ->
        // Boolean encodings are generally compatible
        Ok ()

    | (asn1Kind, acnKind) ->
        Error (sprintf "ACN encoding %A incompatible with ASN.1 type %A" acnKind asn1Kind)
```

## Complex ACN Examples

### Length-Prefixed Variable Arrays

```acn
-- ASN.1
DataPacket ::= SEQUENCE {
    header Header,
    dataLength INTEGER (0..65535),
    data SEQUENCE (SIZE(0..dataLength)) OF INTEGER (0..255)
}

-- ACN encoding
DataPacket [] {
    header [],
    dataLength [] {
        encoding pos-int,
        size 16,
        endianness big
    },
    data [] {
        size dataLength * 8  -- Each element is 8 bits, total size = dataLength * 8
    }
}
```

### Conditional Field Encoding

```acn
-- Version-dependent message format
ProtocolMessage [] {
    version [] {
        encoding pos-int,
        size 8
    },
    standardFields [],
    extendedFields present-when version >= 2,
    legacyField present-when version = 1
}
```

### Custom Bit Layouts

```acn
-- Packed bit fields
StatusRegister [] {
    enabled [] {
        encoding pattern '1'B,  -- Single bit
        size 1
    },
    priority [] {
        encoding pos-int,
        size 3  -- 3 bits for priority (0-7)
    },
    reserved [] {
        encoding pattern '0000'B,
        size 4  -- 4 reserved bits, must be zero
    }
}
```

## Error Handling and Debugging

### Common ACN Issues

#### 1. Type Reference Mismatches
**Problem**: ACN references ASN.1 type that doesn't exist
**Error Location**: `FrontEndAst/AcnCreateFromAntlr.fs:validateTypeReference`

```fsharp
// Example error
ACN Error: Type 'MessageTypo' not found in ASN.1 module 'Protocol'
// Check: Ensure ACN type names exactly match ASN.1 type names
```

#### 2. Size Specification Conflicts
**Problem**: ACN size doesn't accommodate all ASN.1 constraint values
**Error Location**: `FrontEndAst/CheckAsn1.fs:validateSizeCompatibility`

```asn1
-- ASN.1: Can hold values 0..1000
Counter ::= INTEGER (0..1000)
```

```acn
-- ACN: Only 8 bits = max value 255
Counter [] {
    encoding pos-int,
    size 8  -- ERROR: Cannot represent values > 255
}
```

#### 3. Endianness Issues
**Problem**: Incorrect byte order for multi-byte values

**Debug Strategy**: Use hex dump comparison
```fsharp
let debugEndianness (value: int) (encoding: AcnIntEncodingSpec) : string =
    // Generate both big and little endian representations
    sprintf "Value %d: Big-endian=%s, Little-endian=%s"
        value (toBigEndianHex value) (toLittleEndianHex value)
```

#### 4. Present-When Expression Errors
**Problem**: Present-when conditions reference undefined fields or use invalid operators

**Error Location**: `FrontEndAst/AcnCreateFromAntlr.fs:validatePresentWhenExpression`

```acn
-- Error: 'unknownField' doesn't exist in the ASN.1 type
payload present-when unknownField = 1
```

### Debugging Tools

#### ACN AST Visualization
**Location**: `FrontEndAst/ExportToXml.fs:exportAcnAst`

```fsharp
let exportAcnAstToXml (acnAst: AcnAst) (filename: string) : unit =
    // Generate XML representation of ACN AST for inspection
    // Useful for debugging complex ACN specifications
```

#### Encoding Preview
**Location**: `BackendAst/PrintAcn.fs`

```fsharp
let previewAcnEncoding (asn1AcnType: Asn1AcnAst.Asn1Type) : string =
    // Show bit-level layout of ACN encoding
    // Helps verify encoding specifications before code generation
```

## Integration with Code Generation

The parsed and validated ACN specifications are used during backend code generation:

```
ACN AST → Asn1AcnAst → DAst → Code Generation
```

**Backend Integration**: `BackendAst/DAstACN.fs` uses the ACN specifications to generate custom encoding/decoding functions.

The ACN parser enables ASN1SCC to handle non-standard binary formats while maintaining the type safety and validation capabilities of the ASN.1 type system.