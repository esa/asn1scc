# Common Utilities and Types Overview

This document details the CommonTypes project, which provides foundational utilities, data structures, and cross-cutting concerns used throughout ASN1SCC.

## Project Structure

**Location**: `CommonTypes/`
**Purpose**: Shared functionality for type operations, constraint handling, and cross-language template generation

## Core Components

### 1. Fundamental Types and Utilities

#### FsUtils.fs
**Purpose**: F# language extensions and utility functions

```fsharp
// String manipulation utilities
module StringExtensions =
    let replace (oldStr: string) (newStr: string) (str: string) : string
    let splitLines (str: string) : string list
    let trim (str: string) : string
    let padLeft (width: int) (str: string) : string

// List processing utilities
module ListExtensions =
    let tryFindIndex (predicate: 'a -> bool) (list: 'a list) : int option
    let removeDuplicates (list: 'a list) : 'a list when 'a : comparison
    let splitAt (index: int) (list: 'a list) : ('a list * 'a list)

// Option type utilities
module OptionExtensions =
    let getOrElse (defaultValue: 'a) (option: 'a option) : 'a
    let bind2 (f: 'a -> 'b -> 'c option) (opt1: 'a option) (opt2: 'b option) : 'c option
```

**Key Functions**:
- File I/O operations with error handling
- Path manipulation utilities
- Generic fold operations
- Collection processing functions

#### CommonTypes.fs
**Purpose**: Core data types and fundamental operations

```fsharp
// Source location tracking for error reporting
type SrcLoc = {
    srcFilename: string
    line: int
    column: int
}

// String with location information
type StringLoc = {
    value: string
    location: SrcLoc
}

// Big integer operations for ASN.1 INTEGER handling
type BigInteger with
    static member ToHex (value: BigInteger) : string
    static member FromHex (hexStr: string) : BigInteger
    static member ToBinaryString (value: BigInteger) : string

// Error handling types
type CompilerMessage = {
    messageType: MessageType
    location: SrcLoc
    message: string
}

type MessageType =
    | Error
    | Warning
    | Info
```

**Language Support Types**:
```fsharp
type Language =
    | C
    | Ada
    | Scala

type TargetLangInfo = {
    language: Language
    supportsInitGlobals: bool
    hasPointers: bool
    supportsGenericTypes: bool
}
```

### 2. Set Operations Framework

ASN1SCC uses sophisticated set operations for constraint handling and validation. The system provides multiple set implementations optimized for different use cases.

#### RangeSets.fs
**Purpose**: Efficient range-based set operations for numeric constraints

```fsharp
type RangeSet<'T when 'T : comparison> = {
    ranges: ('T * 'T) list  // Sorted, non-overlapping (min, max) pairs
}

module RangeSet =
    // Core operations
    let empty<'T when 'T : comparison> : RangeSet<'T>
    let singleton (value: 'T) : RangeSet<'T>
    let range (min: 'T) (max: 'T) : RangeSet<'T>

    // Set operations
    let union (s1: RangeSet<'T>) (s2: RangeSet<'T>) : RangeSet<'T>
    let intersection (s1: RangeSet<'T>) (s2: RangeSet<'T>) : RangeSet<'T>
    let difference (s1: RangeSet<'T>) (s2: RangeSet<'T>) : RangeSet<'T>
    let complement (universe: RangeSet<'T>) (s: RangeSet<'T>) : RangeSet<'T>

    // Query operations
    let contains (value: 'T) (s: RangeSet<'T>) : bool
    let isEmpty (s: RangeSet<'T>) : bool
    let count (s: RangeSet<'T>) : BigInteger option  // None if infinite
    let min (s: RangeSet<'T>) : 'T option
    let max (s: RangeSet<'T>) : 'T option

    // Validation and utilities
    let isSubsetOf (s1: RangeSet<'T>) (s2: RangeSet<'T>) : bool
    let overlaps (s1: RangeSet<'T>) (s2: RangeSet<'T>) : bool
    let toList (s: RangeSet<'T>) : 'T list  // Only for finite, small sets
```

**Usage Example**: Integer constraint resolution
```fsharp
// ASN.1: INTEGER ((1..10) | (20..30))
let constraint1 = RangeSet.range 1I 10I
let constraint2 = RangeSet.range 20I 30I
let unionSet = RangeSet.union constraint1 constraint2

// ASN.1: INTEGER ((1..20) EXCEPT (5..15))
let baseRange = RangeSet.range 1I 20I
let excludeRange = RangeSet.range 5I 15I
let finalSet = RangeSet.difference baseRange excludeRange
// Result: {1..4, 16..20}
```

#### ValueSets.fs
**Purpose**: Set operations for discrete values (enumerated types, string alphabets)

```fsharp
type ValueSet<'T when 'T : comparison> = {
    values: Set<'T>
    isComplement: bool  // true for "all except these values"
}

module ValueSet =
    let fromList (values: 'T list) : ValueSet<'T>
    let complement (valueSet: ValueSet<'T>) : ValueSet<'T>
    let union (s1: ValueSet<'T>) (s2: ValueSet<'T>) : ValueSet<'T>
    let intersection (s1: ValueSet<'T>) (s2: ValueSet<'T>) : ValueSet<'T>

    // Convert from character ranges to value sets
    let fromCharRange (startChar: char) (endChar: char) : ValueSet<char>

    // String alphabet operations
    let stringAlphabetFromConstraint (constraint: StringConstraint) : ValueSet<char>
```

**Usage Example**: String alphabet constraints
```fsharp
// ASN.1: VisibleString (FROM("A".."Z"))
let upperCaseAlphabet = ValueSet.fromCharRange 'A' 'Z'

// ASN.1: VisibleString (FROM("A".."Z" | "0".."9"))
let alphaNumeric =
    ValueSet.union
        (ValueSet.fromCharRange 'A' 'Z')
        (ValueSet.fromCharRange '0' '9')
```

#### SimpleSets.fs
**Purpose**: Basic set operations for small, finite sets

```fsharp
type SimpleSet<'T when 'T : comparison> = Set<'T>

module SimpleSet =
    let combinations (sets: SimpleSet<'T> list) : SimpleSet<'T list>
    let cartesianProduct (s1: SimpleSet<'T>) (s2: SimpleSet<'U>) : SimpleSet<'T * 'U>
    let powerSet (s: SimpleSet<'T>) : SimpleSet<SimpleSet<'T>>
```

#### SizeableSet.fs
**Purpose**: Sets with size constraints (for SEQUENCE OF, SET OF)

```fsharp
type SizeConstraint = {
    minSize: BigInteger option
    maxSize: BigInteger option
}

type SizeableSet<'T> = {
    elementSet: ValueSet<'T>
    sizeConstraint: SizeConstraint
}

module SizeableSet =
    let applySizeConstraint (sizeConstraint: SizeConstraint) (valueSet: ValueSet<'T>) : SizeableSet<'T>
    let countPossibleValues (sizeableSet: SizeableSet<'T>) : BigInteger option
```

### 3. Abstract Macro System

#### AbstractMacros.fs
**Purpose**: Cross-language template abstraction system
**Generated From**: String template files via `parseStg2`

```fsharp
// Abstract interface implemented by all target languages
type IAbstractMacros =
    // Type definition macros
    abstract member TypeDefinition : string -> string -> string
    abstract member SequenceDefinition : string -> (string * string) list -> string
    abstract member ChoiceDefinition : string -> (string * string) list -> string

    // Value operations
    abstract member InitializeValue : string -> string -> string
    abstract member CompareValues : string -> string -> string -> string
    abstract member ValidateValue : string -> string list -> string

    // Encoding operations
    abstract member EncodeValue : string -> string -> string -> string
    abstract member DecodeValue : string -> string -> string -> string
```

**Target Language Implementations**:
- **C**: `StgC/AbstractMacrosImp.fs` - Implements C-specific macro generation
- **Ada**: `StgAda/AbstractMacrosImp.fs` - Implements Ada/SPARK macro generation
- **Scala**: `StgScala/AbstractMacrosImp.fs` - Implements Scala macro generation

**Usage Pattern**:
```fsharp
let generateTypeDefinition (macros: IAbstractMacros) (typeName: string) (typeBody: string) : string =
    macros.TypeDefinition typeName typeBody

// This works with any target language implementation
let cCode = generateTypeDefinition cMacros "MessageType" "struct { ... }"
let adaCode = generateTypeDefinition adaMacros "MessageType" "record ... end record"
```

### 4. ACN-Specific Types

#### AcnGenericTypes.fs
**Purpose**: Generic ACN type definitions used across frontend and backend

```fsharp
type AcnEncodingClass =
    | IntegerEncodingClass of AcnIntegerEncodingClass
    | RealEncodingClass of AcnRealEncodingClass
    | StringEncodingClass of AcnStringEncodingClass
    | BooleanEncodingClass of AcnBooleanEncodingClass

type AcnIntegerEncodingClass = {
    encodingKind: IntegerEncodingKind
    sizeInBits: AcnSizeSpec
    endianness: AcnEndianness option
    alignment: AcnAlignment option
}

type IntegerEncodingKind =
    | PositiveInteger       // Unsigned
    | TwosComplement       // Signed
    | BinaryCodedDecimal   // BCD
    | AsciiEncoding        // ASCII digits

type AcnSizeSpec =
    | FixedSize of int                    // Fixed number of bits
    | ParameterSize of string             // Size from ACN parameter
    | FieldSize of string                 // Size from another field
    | NullTerminated                      // Variable size, null-terminated
```

### 5. Runtime Library Management

#### RemoveUnusedRtlFunction.fs
**Purpose**: Optimize generated code by removing unused runtime library functions

```fsharp
type RuntimeFunction = {
    name: string
    language: Language
    dependencies: string list  // Other runtime functions this depends on
    usageCount: int           // How many times it's referenced
}

module RuntimeOptimizer =
    let analyzeUsage (generatedCode: string) (rtlFunctions: RuntimeFunction list) : RuntimeFunction list =
        // Scan generated code to find which RTL functions are actually used
        // Build dependency graph
        // Return only functions that are reachable from used functions

    let removeUnusedFunctions (rtlCode: string) (usedFunctions: RuntimeFunction list) : string =
        // Remove function definitions that are not in the used list
        // Preserve functions that are dependencies of used functions
```

**Integration**: Called during final code generation to produce minimal runtime libraries.

### 6. Validation Framework

#### isvalid_if.stg.fs
**Purpose**: Generated validation template functions
**Source**: Compiled from string templates

```fsharp
type ValidationContext = {
    variableName: string
    typeName: string
    constraints: ConstraintInfo list
    nestingLevel: int
}

type ConstraintInfo = {
    constraintType: ConstraintType
    values: string list
    errorMessage: string
}

// Generated validation functions
let generateRangeCheck (ctx: ValidationContext) (min: string) (max: string) : string
let generateSizeCheck (ctx: ValidationContext) (minSize: string) (maxSize: string) : string
let generateAlphabetCheck (ctx: ValidationContext) (allowedChars: string) : string
```

## Integration with Other Components

### Cross-Component Data Flow

```
ANTLR Parsers → CommonTypes (Basic types) → FrontEndAst (AST construction)
     ↓
Set Operations ← Constraint Resolution ← Type Validation
     ↓
Abstract Macros → BackendAst → Target Code Generation
```

### Error Propagation

```fsharp
type CompilerResult<'T> =
    | Success of 'T
    | Error of CompilerMessage list

module Result =
    let bind (f: 'T -> CompilerResult<'U>) (result: CompilerResult<'T>) : CompilerResult<'U>
    let map (f: 'T -> 'U) (result: CompilerResult<'T>) : CompilerResult<'U>
    let collect (results: CompilerResult<'T> list) : CompilerResult<'T list>
```

## Performance Considerations

### Set Operation Optimization

**RangeSet Performance**:
- **Union/Intersection**: O(n + m) where n, m are number of ranges
- **Contains**: O(log n) binary search
- **Count**: O(n) for finite sets, O(1) for infinite

**Memory Usage**:
- RangeSet stores only range boundaries, not individual values
- Efficient for large ranges like `INTEGER (1..1000000)`
- ValueSet better for small discrete sets like enumerations

### Template Generation Performance

**AbstractMacros Caching**:
```fsharp
// Memoize expensive template generation
let memoizedTemplateGeneration =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<string, string>()
    fun (templateName: string) (args: obj list) ->
        let key = sprintf "%s:%A" templateName args
        cache.GetOrAdd(key, fun _ -> generateTemplate templateName args)
```

## Common Usage Patterns

### Constraint Resolution Example

```fsharp
// Resolve complex ASN.1 constraint: INTEGER ((1..100) | (200..300)) EXCEPT (50..60)
let resolveComplexConstraint () =
    let range1 = RangeSet.range 1I 100I
    let range2 = RangeSet.range 200I 300I
    let unionSet = RangeSet.union range1 range2
    let excludeRange = RangeSet.range 50I 60I
    let finalSet = RangeSet.difference unionSet excludeRange
    // Result: {1..49, 61..100, 200..300}
    finalSet
```

### Cross-Language Code Generation

```fsharp
let generateForAllLanguages (typeName: string) (typeInfo: TypeInfo) : (Language * string) list =
    [
        (C, generateCCode typeName typeInfo)
        (Ada, generateAdaCode typeName typeInfo)
        (Scala, generateScalaCode typeName typeInfo)
    ]
```

This common utilities framework enables ASN1SCC to handle complex ASN.1/ACN specifications efficiently while maintaining consistency across all target languages and providing robust error handling throughout the compilation process.