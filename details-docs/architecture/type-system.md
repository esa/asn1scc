# Type System and Constraint Handling

This document details ASN1SCC's type system, constraint handling mechanisms, and template resolution processes.

## ASN.1 Type System Implementation

### Core Type Hierarchy

**Location**: `FrontEndAst/Asn1Ast.fs:50-200`

```fsharp
type Asn1TypeKind =
    // Primitive Types
    | Integer of IntegerType
    | Real of RealType
    | Boolean of BooleanType
    | Enumerated of EnumeratedType

    // String Types
    | IA5String of StringType
    | UTF8String of StringType
    | NumericString of StringType
    | PrintableString of StringType
    | VisibleString of StringType

    // Binary Types
    | BitString of BitStringType
    | OctetString of OctetStringType

    // Structured Types
    | Sequence of SequenceType
    | SequenceOf of SequenceOfType
    | Set of SetType
    | SetOf of SetOfType
    | Choice of ChoiceType

    // Special Types
    | ReferenceType of ReferenceToType
    | ObjectIdentifier of ObjectIdentifierType
```

### Primitive Type Implementations

#### INTEGER Type System
**Location**: `FrontEndAst/Asn1Ast.fs:INTEGER_TYPE`

```fsharp
type IntegerType = {
    // Named values like { small(1), medium(2), large(3) }
    namedValues: NamedValue list
    // Constraint information
    constraints: IntegerConstraint list
    // For PER encoding efficiency
    hasNBits: int option  // Fixed bit width
}

type NamedValue = {
    name: StringLoc
    value: Asn1Value
    location: SrcLoc
}

type IntegerConstraint =
    | IntSingleValueConstraint of BigInteger
    | IntRangeConstraint of (BigInteger option * BigInteger option)
    | IntUnionConstraint of (IntegerConstraint * IntegerConstraint)
    | IntIntersectionConstraint of (IntegerConstraint * IntegerConstraint)
```

**Constraint Resolution**: `CommonTypes/RangeSets.fs`
```fsharp
let resolveIntegerConstraints (constraints: IntegerConstraint list) : RangeSet<BigInteger> =
    // Convert constraints to range sets for efficient operations
    // Handle unions, intersections, and complements
    // Return canonical range representation
```

#### ENUMERATED Type System
**Location**: `FrontEndAst/Asn1Ast.fs:ENUMERATED_TYPE`

```fsharp
type EnumeratedType = {
    items: EnumeratedItem list
    // Support for extension markers
    hasExtensionMarker: bool
    // Additional values after extension marker
    additionalItems: EnumeratedItem list
}

type EnumeratedItem = {
    name: StringLoc
    value: Asn1Value option  // Optional explicit value
    location: SrcLoc
}
```

**Name Conflict Resolution**: `FrontEndAst/EnsureUniqueEnumNames.fs`
```fsharp
type RenamePolicy =
    | NoRenaming                    // Ada default
    | RenameOnlyConflicting        // C default
    | RenameAllInConflictingTypes  // Rename all if any conflict
    | RenameAll                    // Rename all enumerants

let resolveEnumConflicts (policy: RenamePolicy) (types: Asn1Type list) : Asn1Type list =
    // Detect name conflicts across enumerated types
    // Apply renaming policy to resolve conflicts
    // Generate type-prefixed names where needed
```

### Structured Type System

#### SEQUENCE Type Implementation
**Location**: `FrontEndAst/Asn1Ast.fs:SEQUENCE_TYPE`

```fsharp
type SequenceType = {
    children: SequenceChild list
    // Optimization flags for encoding
    hasAnyOptionalChild: bool
    hasAnyExtensionMarker: bool
    hasAnyChildWithDefault: bool
}

type SequenceChild =
    | SequenceChildComponent of SequenceChildComponent
    | SequenceChildExtensionMarker of SrcLoc

type SequenceChildComponent = {
    name: StringLoc
    chType: Asn1Type
    isOptional: bool
    defaultValue: Asn1Value option
    location: SrcLoc
    // ACN-specific information added later
    acnInfo: unit option
}
```

**Extension Handling**:
Extensions in ASN.1 allow adding new fields while maintaining backward compatibility:
```asn1
MessageV1 ::= SEQUENCE {
    field1 INTEGER,
    field2 BOOLEAN,
    ...  -- Extension marker
}

MessageV2 ::= SEQUENCE {
    field1 INTEGER,
    field2 BOOLEAN,
    ...,
    field3 REAL OPTIONAL  -- Added in version 2
}
```

#### CHOICE Type Implementation
**Location**: `FrontEndAst/Asn1Ast.fs:CHOICE_TYPE`

```fsharp
type ChoiceType = {
    children: ChoiceChild list
    hasExtensionMarker: bool
    additionalChildren: ChoiceChild list  // After extension marker
}

type ChoiceChild = {
    name: StringLoc
    chType: Asn1Type
    location: SrcLoc
}
```

**Choice Resolution**: Each CHOICE alternative must have a unique tag for encoding:
```fsharp
let resolveChoiceTags (choice: ChoiceType) : (ChoiceChild * int) list =
    // Assign unique tags to each alternative
    // Handle extension markers and tag conflicts
    // Generate canonical tag assignments for encoding
```

## Constraint System

### Constraint Types and Resolution

**Location**: `CommonTypes/RangeSets.fs`, `CommonTypes/ValueSets.fs`

```fsharp
type Asn1Constraint =
    | SingleValueConstraint of Asn1Value
    | RangeConstraint of (Asn1Value option * Asn1Value option)
    | SizeConstraint of Asn1Constraint     // SIZE(constraint)
    | AlphabetConstraint of Asn1Constraint  // String alphabet restriction
    | UnionConstraint of (Asn1Constraint * Asn1Constraint)
    | IntersectionConstraint of (Asn1Constraint * Asn1Constraint)
    | AllExceptConstraint of Asn1Constraint
    | ExceptConstraint of (Asn1Constraint * Asn1Constraint)
    | RootConstraint of Asn1Constraint     // Root constraint for extensions
    | RootConstraint2 of (Asn1Constraint * Asn1Constraint)
```

### Range Set Operations

**Implementation**: `CommonTypes/RangeSets.fs:100-300`

```fsharp
type RangeSet<'T when 'T : comparison> = {
    ranges: (('T * 'T) list)  // List of (min, max) pairs
}

module RangeSet =
    let union (s1: RangeSet<'T>) (s2: RangeSet<'T>) : RangeSet<'T> =
        // Merge overlapping ranges efficiently
        // Maintain canonical sorted representation

    let intersection (s1: RangeSet<'T>) (s2: RangeSet<'T>) : RangeSet<'T> =
        // Find overlapping portions of ranges

    let complement (universe: RangeSet<'T>) (s: RangeSet<'T>) : RangeSet<'T> =
        // All values in universe except those in s

    let isEmpty (s: RangeSet<'T>) : bool =
        List.isEmpty s.ranges

    let contains (value: 'T) (s: RangeSet<'T>) : bool =
        // Binary search through sorted ranges
```

### Constraint Resolution Examples

#### Integer Range Constraints
```asn1
-- ASN.1 source
SmallInt ::= INTEGER (1..100)
MediumInt ::= INTEGER (50..200)
UnionInt ::= INTEGER ((1..100) | (50..200))  -- Results in (1..200)
IntersectInt ::= INTEGER ((1..100) ∩ (50..200))  -- Results in (50..100)
```

**Resolution Process**: `FrontEndAst/ConstraintsMapping.fs:resolveIntegerConstraints`
```fsharp
let resolveConstraint (constraint: Asn1Constraint) : RangeSet<BigInteger> =
    match constraint with
    | SingleValueConstraint value ->
        RangeSet.singleton (getIntegerValue value)
    | RangeConstraint (min, max) ->
        let minVal = min |> Option.map getIntegerValue |> Option.defaultValue BigInteger.MinValue
        let maxVal = max |> Option.map getIntegerValue |> Option.defaultValue BigInteger.MaxValue
        RangeSet.range minVal maxVal
    | UnionConstraint (c1, c2) ->
        RangeSet.union (resolveConstraint c1) (resolveConstraint c2)
    | IntersectionConstraint (c1, c2) ->
        RangeSet.intersection (resolveConstraint c1) (resolveConstraint c2)
```

#### String Constraints
```asn1
-- SIZE constraints
ShortString ::= UTF8String (SIZE(1..10))
FixedString ::= IA5String (SIZE(16))

-- Alphabet constraints
DigitString ::= NumericString (FROM("0123456789"))
AlphaNumeric ::= VisibleString (FROM("A".."Z" | "a".."z" | "0".."9"))
```

**Implementation**: `CommonTypes/SimpleSets.fs`
```fsharp
type CharacterSet = {
    allowedChars: Set<char>
}

let resolveStringConstraints (constraints: Asn1Constraint list) : (RangeSet<int> * CharacterSet option) =
    // Returns (sizeConstraints, alphabetConstraints)
    // Size constraints limit string length
    // Alphabet constraints limit allowed characters
```

## Template/Generic Resolution System

### Parameterized Type Definition

**Location**: `FrontEndAst/ParameterizedAsn1Ast.fs`

```fsharp
type ParameterizedTypeDefinition = {
    name: string
    parameters: ParameterDefinition list
    typeBody: Asn1Type  // Contains parameter references
    constraints: Asn1Constraint list  // May reference parameters
}

type ParameterDefinition = {
    name: string
    parameterType: ParameterType
    constraints: Asn1Constraint list option
}

type ParameterType =
    | IntegerParameter
    | TypeParameter  // Type as parameter
    | ValueParameter of Asn1Type  // Value of specific type
```

### Template Resolution Process

**Location**: `FrontEndAst/RemoveParameterizedTypes.fs:200-500`

```fsharp
let resolveParameterizedTypeInstance
    (definition: ParameterizedTypeDefinition)
    (arguments: Asn1Value list) : Asn1Type =

    // 1. Validate argument count and types
    if List.length arguments <> List.length definition.parameters then
        failwith "Parameter count mismatch"

    // 2. Create substitution map
    let substitutions =
        List.zip definition.parameters arguments
        |> List.map (fun (param, arg) -> (param.name, arg))
        |> Map.ofList

    // 3. Substitute parameters in type body
    let substitutedType = substituteParametersInType substitutions definition.typeBody

    // 4. Substitute parameters in constraints
    let substitutedConstraints =
        definition.constraints
        |> List.map (substituteParametersInConstraint substitutions)

    // 5. Resolve substituted constraints
    { substitutedType with Constraints = substitutedConstraints }
```

### Template Examples

#### Matrix Template
```asn1
-- Parameterized type definition
Matrix{INTEGER:rows, INTEGER:cols} ::=
    SEQUENCE (SIZE(rows)) OF SEQUENCE (SIZE(cols)) OF REAL

-- Usage/Instantiation
ImageMatrix ::= Matrix{480, 640}  -- 480x640 matrix of REAL
SmallMatrix ::= Matrix{3, 3}      -- 3x3 matrix
```

**Resolution Result**:
```fsharp
// ImageMatrix becomes:
SEQUENCE (SIZE(480)) OF SEQUENCE (SIZE(640)) OF REAL

// SmallMatrix becomes:
SEQUENCE (SIZE(3)) OF SEQUENCE (SIZE(3)) OF REAL
```

#### Generic Container Template
```asn1
-- More complex template with type parameter
Container{Type} ::= SEQUENCE {
    size INTEGER (0..1000),
    items SEQUENCE (SIZE(0..size)) OF Type,
    metadata UTF8String OPTIONAL
}

-- Instantiations
IntContainer ::= Container{INTEGER}
StringContainer ::= Container{UTF8String}
```

### Parameter Substitution Algorithm

**Implementation**: `FrontEndAst/RemoveParameterizedTypes.fs:substituteParametersInType`

```fsharp
let rec substituteParametersInType (substitutions: Map<string, Asn1Value>) (asn1Type: Asn1Type) : Asn1Type =
    match asn1Type.Kind with
    | ReferenceType refType when substitutions.ContainsKey refType.modName.Value ->
        // Replace parameter reference with actual type
        createTypeFromValue substitutions[refType.modName.Value]

    | Sequence sequenceType ->
        // Recursively substitute in sequence children
        let newChildren =
            sequenceType.children
            |> List.map (substituteParametersInSequenceChild substitutions)
        { asn1Type with Kind = Sequence { sequenceType with children = newChildren } }

    | SequenceOf sequenceOfType ->
        // Substitute in element type and size constraints
        let newElementType = substituteParametersInType substitutions sequenceOfType.elementType
        let newConstraints =
            sequenceOfType.constraints
            |> List.map (substituteParametersInConstraint substitutions)
        { asn1Type with
            Kind = SequenceOf {
                sequenceOfType with
                    elementType = newElementType
                    constraints = newConstraints
            }
        }
    // ... handle other type kinds
```

## Type Equivalence and Compatibility

### Type Equivalence System

**Location**: `FrontEndAst/TypesEquivalence.fs`

```fsharp
type TypeEquivalence =
    | StructuralEquivalence  // Same structure, possibly different names
    | NominalEquivalence    // Same name and structure
    | AssignmentCompatible  // Can be assigned (subtype relationship)

let areTypesEquivalent (t1: Asn1Type) (t2: Asn1Type) (equivalenceType: TypeEquivalence) : bool =
    match equivalenceType with
    | StructuralEquivalence -> compareTypeStructure t1 t2
    | NominalEquivalence -> compareTypeNamesAndStructure t1 t2
    | AssignmentCompatible -> isAssignmentCompatible t1 t2
```

### Subtype Relationships

**Constraint-Based Subtyping**:
```asn1
BaseInt ::= INTEGER
SmallInt ::= INTEGER (1..100)    -- SmallInt is subtype of BaseInt
TinyInt ::= INTEGER (1..10)      -- TinyInt is subtype of SmallInt
```

**Implementation**: `CommonTypes/RangeSets.fs:isSubsetOf`
```fsharp
let isSubtype (subType: Asn1Type) (superType: Asn1Type) : bool =
    match (subType.Kind, superType.Kind) with
    | (Integer intSub, Integer intSuper) ->
        // Check if subtype constraints are subset of supertype constraints
        let subConstraints = resolveIntegerConstraints intSub.constraints
        let superConstraints = resolveIntegerConstraints intSuper.constraints
        RangeSet.isSubsetOf subConstraints superConstraints
    // ... handle other type combinations
```

## Error Handling and Debugging

### Common Type System Issues

#### 1. Circular Type Dependencies
**Detection**: `FrontEndAst/CheckAsn1.fs:checkForCircularDependencies`
```fsharp
let detectCircularDependencies (types: Map<string, Asn1Type>) : string list =
    // Use depth-first search to detect cycles in type reference graph
    // Return list of types involved in circular dependencies
```

**Example Problematic Definition**:
```asn1
TypeA ::= SEQUENCE { field1 TypeB }
TypeB ::= SEQUENCE { field2 TypeA }  -- Circular dependency
```

#### 2. Constraint Conflicts
**Detection**: `CommonTypes/RangeSets.fs:isEmpty`
```fsharp
let validateConstraints (constraints: Asn1Constraint list) : ValidationResult =
    let resolvedSet = resolveConstraints constraints
    if RangeSet.isEmpty resolvedSet then
        Error "Constraints result in empty set - no valid values"
    else
        Ok resolvedSet
```

**Example Conflict**:
```asn1
ConflictedInt ::= INTEGER ((1..10) INTERSECTION (20..30))  -- Empty set!
```

#### 3. Template Resolution Failures
**Location**: `FrontEndAst/RemoveParameterizedTypes.fs:validateTemplateArguments`

**Common Issues**:
- Parameter count mismatch
- Parameter type incompatibility
- Constraint violations after substitution

#### 4. Extension Compatibility
**Location**: `FrontEndAst/CheckAsn1.fs:checkExtensionCompatibility`

Ensure that extensions maintain backward compatibility:
```asn1
-- Valid extension
BaseType ::= SEQUENCE { field1 INTEGER, ... }
ExtendedType ::= SEQUENCE { field1 INTEGER, ..., field2 BOOLEAN OPTIONAL }

-- Invalid extension - changes existing field
BadExtension ::= SEQUENCE { field1 REAL, ..., field2 BOOLEAN }  -- field1 type changed!
```

This type system enables ASN1SCC to handle complex ASN.1 specifications while maintaining strong type safety and providing clear error messages for common mistakes.