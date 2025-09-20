# Frontend Validations

This document details the comprehensive validation system in FrontEndAst that ensures ASN.1/ACN specifications are semantically correct.

## Overview

**Location**: Primarily `FrontEndAst/CheckAsn1.fs`
**Purpose**: Perform semantic validation of ASN.1/ACN ASTs before code generation
**Philosophy**: Fail fast with clear error messages to prevent runtime issues

## Validation Pipeline

```
AST Construction → Type Validation → Constraint Validation →
Reference Resolution → ACN Compatibility → Cross-Module Validation
```

## Core Validation Types

```fsharp
type ValidationResult<'T> =
    | Success of 'T
    | Error of ValidationError list

type ValidationError = {
    location: SrcLoc
    severity: ErrorSeverity
    errorType: ValidationErrorType
    message: string
    suggestedFix: string option
}

type ErrorSeverity =
    | Warning
    | Error
    | Fatal

type ValidationErrorType =
    | DuplicateDefinition
    | UnresolvedReference
    | CircularDependency
    | ConstraintViolation
    | TypeMismatch
    | AcnIncompatibility
    | ExtensionViolation
    | ParameterError
```

## Type System Validation

### Duplicate Definition Detection

```fsharp
let checkForDuplicateDefinitions (module: Asn1Module) : ValidationResult<unit> =
    let typeNames =
        module.typeAssignments
        |> List.map (fun ta -> (ta.name.value, ta.name.location))

    let valueNames =
        module.valueAssignments
        |> List.map (fun va -> (va.name.value, va.name.location))

    let allNames = typeNames @ valueNames

    // Find duplicates
    let duplicates =
        allNames
        |> List.groupBy fst
        |> List.filter (fun (_, instances) -> List.length instances > 1)
        |> List.map (fun (name, instances) ->
            {
                location = snd (List.head instances)
                severity = Error
                errorType = DuplicateDefinition
                message = sprintf "Duplicate definition of '%s'" name
                suggestedFix = Some "Rename one of the conflicting definitions"
            })

    match duplicates with
    | [] -> Success ()
    | errors -> Error errors
```

### Circular Dependency Detection

```fsharp
let checkForCircularDependencies (astRoot: AstRoot) : ValidationResult<unit> =
    // Build dependency graph
    let dependencyGraph = buildTypeDependencyGraph astRoot

    // Use DFS to detect cycles
    let rec detectCycle (visited: Set<string>) (path: string list) (current: string) : string list option =
        if Set.contains current visited then
            // Found cycle - extract cycle from path
            let cycleStart = List.findIndex ((=) current) path
            Some (List.skip cycleStart path @ [current])
        else
            let newVisited = Set.add current visited
            let newPath = current :: path

            // Check all dependencies
            match Map.tryFind current dependencyGraph with
            | Some dependencies ->
                dependencies
                |> List.tryPick (detectCycle newVisited newPath)
            | None -> None

    let allTypes =
        astRoot.modules
        |> List.collect (fun m -> m.typeAssignments |> List.map (fun ta -> ta.name.value))

    // Check each type for cycles
    let cycles =
        allTypes
        |> List.choose (detectCycle Set.empty [])
        |> List.distinct

    match cycles with
    | [] -> Success ()
    | cyclePaths ->
        let errors =
            cyclePaths
            |> List.map (fun cycle ->
                {
                    location = SrcLoc.unknown // TODO: Get better location
                    severity = Error
                    errorType = CircularDependency
                    message = sprintf "Circular dependency detected: %s" (String.concat " -> " cycle)
                    suggestedFix = Some "Break the cycle by using forward references or restructuring types"
                })
        Error errors
```

## Constraint Validation

### Range Constraint Validation

```fsharp
let validateIntegerConstraints (intType: IntegerType) : ValidationResult<IntegerType> =
    // Resolve all constraints to range sets
    let rangeSet =
        intType.constraints
        |> List.map resolveIntegerConstraint
        |> List.fold RangeSet.intersection RangeSet.universal

    // Check if constraint set is empty
    if RangeSet.isEmpty rangeSet then
        Error [{
            location = intType.location
            severity = Error
            errorType = ConstraintViolation
            message = "Integer constraints result in empty set - no valid values"
            suggestedFix = Some "Check constraint logic for conflicts"
        }]
    else
        // Check if constraint set is reasonable for encoding
        let bitWidth = RangeSet.requiredBitWidth rangeSet
        if bitWidth > 64 then
            Warning [{
                location = intType.location
                severity = Warning
                errorType = ConstraintViolation
                message = sprintf "Integer constraint requires %d bits - may cause encoding issues" bitWidth
                suggestedFix = Some "Consider using smaller ranges or BigInteger support"
            }]
        else
            Success { intType with resolvedConstraints = Some rangeSet }

let validateStringConstraints (strType: StringType) : ValidationResult<StringType> =
    // Validate size constraints
    let sizeConstraints = extractSizeConstraints strType.constraints
    let alphabetConstraints = extractAlphabetConstraints strType.constraints

    // Check size constraints are reasonable
    match sizeConstraints with
    | Some sizeRange ->
        if RangeSet.min sizeRange < Some 0I then
            Error [{
                location = strType.location
                severity = Error
                errorType = ConstraintViolation
                message = "String size constraint cannot be negative"
                suggestedFix = Some "Use non-negative size constraints"
            }]
        elif RangeSet.max sizeRange > Some 1000000I then
            Warning [{
                location = strType.location
                severity = Warning
                errorType = ConstraintViolation
                message = "Very large string size constraint may cause memory issues"
                suggestedFix = Some "Consider smaller maximum sizes"
            }]
        else
            Success strType
    | None -> Success strType
```

### Sequence Validation

```fsharp
let validateSequenceType (seqType: SequenceType) : ValidationResult<SequenceType> =
    let validationResults = [
        validateSequenceComponentNames seqType
        validateSequenceOptionalFields seqType
        validateSequenceExtensions seqType
        validateSequenceDefaults seqType
    ]

    combineValidationResults validationResults seqType

let validateSequenceComponentNames (seqType: SequenceType) : ValidationResult<unit> =
    let componentNames =
        seqType.children
        |> List.choose (function
            | SequenceChildComponent comp -> Some (comp.name.value, comp.name.location)
            | SequenceChildExtensionMarker _ -> None)

    // Check for duplicate component names
    let duplicates =
        componentNames
        |> List.groupBy fst
        |> List.filter (fun (_, instances) -> List.length instances > 1)

    match duplicates with
    | [] -> Success ()
    | dups ->
        let errors =
            dups
            |> List.map (fun (name, instances) ->
                {
                    location = snd (List.head instances)
                    severity = Error
                    errorType = DuplicateDefinition
                    message = sprintf "Duplicate sequence component '%s'" name
                    suggestedFix = Some "Use unique component names within sequence"
                })
        Error errors

let validateSequenceExtensions (seqType: SequenceType) : ValidationResult<unit> =
    // Check extension marker usage
    let extensionMarkers =
        seqType.children
        |> List.choose (function
            | SequenceChildExtensionMarker loc -> Some loc
            | _ -> None)

    match extensionMarkers with
    | [] -> Success ()  // No extensions
    | [_] -> Success () // One extension marker is valid
    | multiple ->
        Error [{
            location = List.head multiple
            severity = Error
            errorType = ExtensionViolation
            message = "Multiple extension markers in sequence"
            suggestedFix = Some "Use only one extension marker per sequence"
        }]
```

## Reference Resolution and Validation

### Type Reference Resolution

```fsharp
let resolveTypeReferences (astRoot: AstRoot) : ValidationResult<AstRoot> =
    // Build global symbol table
    let globalSymbols = buildGlobalSymbolTable astRoot

    // Resolve references in each module
    let resolveModule (module: Asn1Module) : ValidationResult<Asn1Module> =
        let resolvedTypeAssignments =
            module.typeAssignments
            |> List.map (resolveTypeAssignmentReferences globalSymbols)
            |> combineValidationResults

        match resolvedTypeAssignments with
        | Success types -> Success { module with typeAssignments = types }
        | Error errors -> Error errors

    let resolvedModules =
        astRoot.modules
        |> List.map resolveModule
        |> combineValidationResults

    match resolvedModules with
    | Success modules -> Success { astRoot with modules = modules }
    | Error errors -> Error errors

let resolveTypeReference
    (globalSymbols: Map<string, TypeInfo>)
    (reference: ReferenceToType) : ValidationResult<ReferenceToType> =

    let fullName =
        match reference.moduleName with
        | Some modName -> sprintf "%s.%s" modName reference.typeName
        | None -> reference.typeName

    match Map.tryFind fullName globalSymbols with
    | Some typeInfo ->
        Success { reference with resolvedType = Some typeInfo }
    | None ->
        Error [{
            location = reference.location
            severity = Error
            errorType = UnresolvedReference
            message = sprintf "Unresolved type reference '%s'" fullName
            suggestedFix = Some "Check spelling and ensure type is defined"
        }]
```

## ACN Compatibility Validation

```fsharp
let validateAcnCompatibility (asn1Type: Asn1Type) (acnSpec: AcnTypeSpec) : ValidationResult<unit> =
    match (asn1Type.Kind, acnSpec.encoding) with
    | (Integer intType, AcnInteger acnInt) ->
        validateIntegerAcnCompatibility intType acnInt
    | (Real realType, AcnReal acnReal) ->
        validateRealAcnCompatibility realType acnReal
    | (Boolean, AcnBoolean acnBool) ->
        Success () // Boolean ACN is generally compatible
    | (Enumerated enumType, AcnEnum acnEnum) ->
        validateEnumAcnCompatibility enumType acnEnum
    | (asn1Kind, acnKind) ->
        Error [{
            location = asn1Type.Location
            severity = Error
            errorType = AcnIncompatibility
            message = sprintf "ACN encoding %A incompatible with ASN.1 type %A" acnKind asn1Kind
            suggestedFix = Some "Use compatible ACN encoding for this ASN.1 type"
        }]

let validateIntegerAcnCompatibility (intType: IntegerType) (acnInt: AcnIntegerSpec) : ValidationResult<unit> =
    // Check if ACN size can accommodate all constraint values
    let constraintRange = resolveIntegerConstraints intType.constraints
    let maxValue = RangeSet.max constraintRange
    let minValue = RangeSet.min constraintRange

    match (minValue, maxValue, acnInt.sizeInBits) with
    | (Some min, Some max, Some bits) ->
        let maxRepresentable =
            match acnInt.encoding with
            | PositiveInteger -> BigInteger.Pow(2I, bits) - 1I
            | TwosComplement -> BigInteger.Pow(2I, bits - 1) - 1I
            | _ -> BigInteger.MaxValue

        let minRepresentable =
            match acnInt.encoding with
            | PositiveInteger -> 0I
            | TwosComplement -> -BigInteger.Pow(2I, bits - 1)
            | _ -> BigInteger.MinValue

        if min < minRepresentable || max > maxRepresentable then
            Error [{
                location = intType.location
                severity = Error
                errorType = AcnIncompatibility
                message = sprintf "ACN encoding (%d bits, %A) cannot represent constraint range %A..%A"
                    bits acnInt.encoding min max
                suggestedFix = Some "Increase ACN size or adjust constraints"
            }]
        else
            Success ()
    | _ -> Success () // Cannot validate without complete information
```

## Extension and Compatibility Validation

```fsharp
let validateExtensionCompatibility (baseType: Asn1Type) (extendedType: Asn1Type) : ValidationResult<unit> =
    match (baseType.Kind, extendedType.Kind) with
    | (Sequence baseSeq, Sequence extSeq) ->
        validateSequenceExtensionCompatibility baseSeq extSeq
    | (Choice baseChoice, Choice extChoice) ->
        validateChoiceExtensionCompatibility baseChoice extChoice
    | (Enumerated baseEnum, Enumerated extEnum) ->
        validateEnumeratedExtensionCompatibility baseEnum extEnum
    | _ ->
        Error [{
            location = extendedType.Location
            severity = Error
            errorType = ExtensionViolation
            message = "Type cannot be extended in this way"
            suggestedFix = Some "Use compatible extension patterns"
        }]

let validateSequenceExtensionCompatibility (baseSeq: SequenceType) (extSeq: SequenceType) : ValidationResult<unit> =
    // Extract base components (before extension marker)
    let baseComponents =
        baseSeq.children
        |> List.takeWhile (function
            | SequenceChildExtensionMarker _ -> false
            | _ -> true)

    let extBaseComponents =
        extSeq.children
        |> List.takeWhile (function
            | SequenceChildExtensionMarker _ -> false
            | _ -> true)

    // Validate that base components are identical
    if List.length baseComponents <> List.length extBaseComponents then
        Error [{
            location = SrcLoc.unknown
            severity = Error
            errorType = ExtensionViolation
            message = "Extended sequence must preserve all base components"
            suggestedFix = Some "Do not modify components before extension marker"
        }]
    else
        // Check each component for compatibility
        List.zip baseComponents extBaseComponents
        |> List.mapi (fun i (baseComp, extComp) ->
            validateComponentCompatibility i baseComp extComp)
        |> combineValidationResults
        |> Result.map (fun _ -> ())
```

## Validation Error Aggregation

```fsharp
let combineValidationResults (results: ValidationResult<'T> list) : ValidationResult<'T list> =
    let (successes, errors) =
        results
        |> List.fold (fun (succ, errs) result ->
            match result with
            | Success value -> (value :: succ, errs)
            | Error errorList -> (succ, errorList @ errs)
        ) ([], [])

    match errors with
    | [] -> Success (List.rev successes)
    | errorList -> Error (List.distinct errorList)

let validateCompleteAst (astRoot: AstRoot) : ValidationResult<AstRoot> =
    [
        validateModuleStructure
        validateTypeDefinitions
        validateConstraints
        validateReferences
        validateAcnCompatibility
        validateExtensions
        validateCrossDependencies
    ]
    |> List.fold (fun acc validator ->
        acc |> Result.bind validator
    ) (Success astRoot)
```

## Common Validation Patterns and Debugging

### Debugging Validation Failures

```fsharp
let debugValidationError (error: ValidationError) : string =
    sprintf """
Validation Error:
  Location: %s:%d:%d
  Type: %A
  Severity: %A
  Message: %s
  Suggested Fix: %s
  """
    error.location.srcFilename
    error.location.line
    error.location.column
    error.errorType
    error.severity
    error.message
    (error.suggestedFix |> Option.defaultValue "None")

let generateValidationReport (errors: ValidationError list) : string =
    let groupedErrors =
        errors
        |> List.groupBy (fun e -> e.errorType)
        |> List.sortBy (fun (errorType, _) ->
            match errorType with
            | Fatal -> 0
            | Error -> 1
            | Warning -> 2)

    groupedErrors
    |> List.map (fun (errorType, errorList) ->
        sprintf "%A Errors (%d):\n%s"
            errorType
            (List.length errorList)
            (errorList |> List.map debugValidationError |> String.concat "\n"))
    |> String.concat "\n\n"
```

This comprehensive validation system ensures that ASN.1/ACN specifications are correct before expensive code generation begins, providing clear error messages that help developers quickly identify and fix specification issues.