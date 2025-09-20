# Frontend AST Builders

This document details the AST construction and transformation processes in the FrontEndAst component.

## Overview

The FrontEndAst project handles the complex transformation from generic ANTLR parse trees to strongly-typed, validated F# AST representations. This process involves multiple phases of refinement and validation.

## AST Construction Pipeline

```
ANTLR Tree → Generic F# AST → Parameterized AST → Template Resolution →
Validated ASN.1 AST → ACN Integration → Asn1AcnAst → DAst
```

## Phase 1: ANTLR Tree Processing

### AntlrParse.fs
**Purpose**: Interface between ANTLR C# runtime and F# AST construction

```fsharp
// Main parsing entry point
let parseAsn1Files (filenames: string list) : AstRoot =
    filenames
    |> List.map parseIndividualFile
    |> combineAstRoots
    |> validateCrossFileReferences

let parseIndividualFile (filename: string) : Asn1Module =
    try
        // 1. Create ANTLR input stream
        let input = new ANTLRFileStream(filename)

        // 2. Initialize lexer and parser
        let lexer = new asn1Lexer(input)
        let tokenStream = new CommonTokenStream(lexer)
        let parser = new asn1Parser(tokenStream)

        // 3. Parse starting from modules rule
        let parseResult = parser.modules()

        // 4. Check for parse errors
        if parser.NumberOfSyntaxErrors > 0 then
            failwithf "Parse errors in %s" filename

        // 5. Transform to F# AST
        CreateAsn1AstFromAntlrTree.createModule parseResult.Tree filename

    with
    | ex -> failwithf "Failed to parse %s: %s" filename ex.Message
```

**Error Handling**:
```fsharp
type ParseError = {
    filename: string
    line: int
    column: int
    message: string
    errorType: ParseErrorType
}

type ParseErrorType =
    | SyntaxError
    | LexicalError
    | UnexpectedToken
    | MissingToken
```

## Phase 2: Generic AST Construction

### CreateAsn1AstFromAntlrTree.fs
**Purpose**: Transform ANTLR parse trees into strongly-typed F# AST

This is one of the most complex files in the system, handling the transformation from untyped ANTLR trees to structured F# types.

#### Module-Level Construction

```fsharp
let createModule (antlrTree: ITree) (filename: string) : Asn1Module =
    match antlrTree.Type with
    | ASN1_MODULE ->
        let moduleName = extractModuleName antlrTree
        let imports = extractImports antlrTree
        let exports = extractExports antlrTree
        let assignments = extractAssignments antlrTree

        {
            name = moduleName
            fileName = filename
            imports = imports
            exports = exports
            typeAssignments = assignments.types
            valueAssignments = assignments.values
        }
    | _ -> failwithf "Expected ASN1_MODULE, got %d" antlrTree.Type

let extractAssignments (moduleTree: ITree) : AssignmentCollection =
    let typeAssignments = ResizeArray<TypeAssignment>()
    let valueAssignments = ResizeArray<ValueAssignment>()

    // Iterate through all children looking for assignments
    for i in 0 .. moduleTree.ChildCount - 1 do
        let child = moduleTree.GetChild(i)
        match child.Type with
        | TYPE_ASSIGNMENT ->
            typeAssignments.Add(createTypeAssignment child)
        | VALUE_ASSIGNMENT ->
            valueAssignments.Add(createValueAssignment child)
        | _ -> () // Ignore other node types

    {
        types = typeAssignments |> Seq.toList
        values = valueAssignments |> Seq.toList
    }
```

#### Type Construction

```fsharp
let createTypeAssignment (antlrNode: ITree) : TypeAssignment =
    // TYPE_ASSIGNMENT structure: typeName ASSIGN type
    let typeName = antlrNode.GetChild(0).Text
    let typeNode = antlrNode.GetChild(2)  // Skip ASSIGN token

    {
        name = { value = typeName; location = getSourceLocation antlrNode.GetChild(0) }
        asn1Type = createAsn1Type typeNode
        location = getSourceLocation antlrNode
    }

let createAsn1Type (typeNode: ITree) : Asn1Type =
    let location = getSourceLocation typeNode

    match typeNode.Type with
    | INTEGER_TYPE -> createIntegerType typeNode location
    | SEQUENCE_TYPE -> createSequenceType typeNode location
    | CHOICE_TYPE -> createChoiceType typeNode location
    | BIT_STRING_TYPE -> createBitStringType typeNode location
    | OCTET_STRING_TYPE -> createOctetStringType typeNode location
    | ENUMERATED_TYPE -> createEnumeratedType typeNode location
    | REFERENCE_TYPE -> createReferenceType typeNode location
    | _ -> failwithf "Unsupported type node: %d" typeNode.Type
```

#### Complex Type Construction

**SEQUENCE Type Building**:
```fsharp
let createSequenceType (seqNode: ITree) (location: SrcLoc) : Asn1Type =
    let components = ResizeArray<SequenceChild>()
    let mutable hasExtensionMarker = false
    let mutable extensionComponents = ResizeArray<SequenceChild>()
    let mutable inExtension = false

    // Process all sequence children
    for i in 0 .. seqNode.ChildCount - 1 do
        let child = seqNode.GetChild(i)
        match child.Type with
        | SEQUENCE_COMPONENT ->
            let component = createSequenceComponent child
            if inExtension then
                extensionComponents.Add(component)
            else
                components.Add(component)

        | EXTENSION_MARKER ->
            hasExtensionMarker <- true
            inExtension <- true
            components.Add(SequenceChildExtensionMarker(getSourceLocation child))

        | _ -> () // Ignore other nodes

    let sequenceType = {
        children = components |> Seq.toList
        hasExtensionMarker = hasExtensionMarker
        extensionChildren = extensionComponents |> Seq.toList
    }

    {
        Kind = Sequence sequenceType
        Constraints = []  // Extracted separately
        Location = location
        acnInfo = None   // Added later during ACN integration
    }

let createSequenceComponent (compNode: ITree) : SequenceChild =
    // SEQUENCE_COMPONENT structure: componentName type [OPTIONAL | DEFAULT value]
    let componentName = compNode.GetChild(0).Text
    let componentType = createAsn1Type (compNode.GetChild(1))

    // Check for OPTIONAL or DEFAULT
    let mutable isOptional = false
    let mutable defaultValue = None

    if compNode.ChildCount > 2 then
        let modifierNode = compNode.GetChild(2)
        match modifierNode.Type with
        | OPTIONAL_KW -> isOptional <- true
        | DEFAULT_KW ->
            isOptional <- true  // DEFAULT implies OPTIONAL
            defaultValue <- Some (createAsn1Value (compNode.GetChild(3)))
        | _ -> ()

    SequenceChildComponent {
        name = { value = componentName; location = getSourceLocation compNode.GetChild(0) }
        chType = componentType
        isOptional = isOptional
        defaultValue = defaultValue
        location = getSourceLocation compNode
    }
```

**CHOICE Type Building**:
```fsharp
let createChoiceType (choiceNode: ITree) (location: SrcLoc) : Asn1Type =
    let alternatives = ResizeArray<ChoiceChild>()
    let mutable hasExtensionMarker = false
    let mutable extensionAlternatives = ResizeArray<ChoiceChild>()
    let mutable inExtension = false

    for i in 0 .. choiceNode.ChildCount - 1 do
        let child = choiceNode.GetChild(i)
        match child.Type with
        | CHOICE_ALTERNATIVE ->
            let alternative = createChoiceAlternative child
            if inExtension then
                extensionAlternatives.Add(alternative)
            else
                alternatives.Add(alternative)

        | EXTENSION_MARKER ->
            hasExtensionMarker <- true
            inExtension <- true

        | _ -> ()

    let choiceType = {
        children = alternatives |> Seq.toList
        hasExtensionMarker = hasExtensionMarker
        extensionChildren = extensionAlternatives |> Seq.toList
    }

    {
        Kind = Choice choiceType
        Constraints = []
        Location = location
        acnInfo = None
    }
```

#### Constraint Extraction

```fsharp
let createConstraints (constraintNode: ITree) : Asn1Constraint list =
    match constraintNode.Type with
    | CONSTRAINT ->
        [processElementSetSpec (constraintNode.GetChild(0))]
    | _ -> []

let rec processElementSetSpec (elemSetNode: ITree) : Asn1Constraint =
    match elemSetNode.Type with
    | UNION_CONSTRAINT ->
        let left = processElementSetSpec (elemSetNode.GetChild(0))
        let right = processElementSetSpec (elemSetNode.GetChild(2))  // Skip UNION token
        UnionConstraint (left, right)

    | INTERSECTION_CONSTRAINT ->
        let left = processElementSetSpec (elemSetNode.GetChild(0))
        let right = processElementSetSpec (elemSetNode.GetChild(2))  // Skip INTERSECTION token
        IntersectionConstraint (left, right)

    | SINGLE_VALUE ->
        let value = createAsn1Value (elemSetNode.GetChild(0))
        SingleValueConstraint value

    | VALUE_RANGE ->
        let minValue =
            if elemSetNode.GetChild(0).Type = MIN_KW then None
            else Some (createAsn1Value (elemSetNode.GetChild(0)))
        let maxValue =
            if elemSetNode.GetChild(2).Type = MAX_KW then None
            else Some (createAsn1Value (elemSetNode.GetChild(2)))
        RangeConstraint (minValue, maxValue)

    | SIZE_CONSTRAINT ->
        let innerConstraint = processElementSetSpec (elemSetNode.GetChild(1))
        SizeConstraint innerConstraint

    | _ -> failwithf "Unsupported constraint type: %d" elemSetNode.Type
```

#### Value Construction

```fsharp
let createAsn1Value (valueNode: ITree) : Asn1Value =
    match valueNode.Type with
    | INTEGER_VALUE ->
        let intStr = valueNode.Text
        let bigInt = BigInteger.Parse(intStr)
        IntValue bigInt

    | REAL_VALUE ->
        let realStr = valueNode.Text
        let realVal = Double.Parse(realStr)
        RealValue realVal

    | STRING_VALUE ->
        let strVal = valueNode.Text.Trim('"')
        StringValue strVal

    | BOOLEAN_VALUE ->
        let boolVal = valueNode.Text.ToLower() = "true"
        BoolValue boolVal

    | BIT_STRING_VALUE ->
        let bitStr = valueNode.Text
        BitStringValue (parseBitString bitStr)

    | OCTET_STRING_VALUE ->
        let octetStr = valueNode.Text
        OctetStringValue (parseOctetString octetStr)

    | SEQUENCE_VALUE ->
        let components = ResizeArray<NamedValue>()
        for i in 0 .. valueNode.ChildCount - 1 do
            let child = valueNode.GetChild(i)
            if child.Type = NAMED_VALUE then
                components.Add(createNamedValue child)
        SeqValue (components |> Seq.toList)

    | REFERENCE_VALUE ->
        let refPath = extractReferencePath valueNode
        RefValue refPath

    | _ -> failwithf "Unsupported value type: %d" valueNode.Type
```

## Phase 3: Parameterized AST Handling

### ParameterizedAsn1Ast.fs
**Purpose**: Handle ASN.1 parameterized types (templates/generics)

```fsharp
type ParameterizedTypeDefinition = {
    name: StringLoc
    parameters: ParameterDefinition list
    typeBody: Asn1Type
    constraints: Asn1Constraint list
}

type ParameterDefinition = {
    name: string
    parameterType: ParameterType
    constraints: Asn1Constraint list option
}

let createParameterizedType (typeAssignment: TypeAssignment) : ParameterizedTypeDefinition option =
    // Check if this is a parameterized type definition
    match extractParameters typeAssignment with
    | Some parameters ->
        Some {
            name = typeAssignment.name
            parameters = parameters
            typeBody = typeAssignment.asn1Type
            constraints = typeAssignment.asn1Type.Constraints
        }
    | None -> None
```

### RemoveParameterizedTypes.fs
**Purpose**: Resolve parameterized type instantiations

```fsharp
let resolveAllParameterizedTypes (astRoot: AstRoot) : AstRoot =
    // 1. Identify parameterized type definitions
    let parameterizedTypes =
        astRoot.modules
        |> List.collect (fun m -> m.typeAssignments)
        |> List.choose createParameterizedType
        |> List.map (fun pt -> (pt.name.value, pt))
        |> Map.ofList

    // 2. Find all instantiations
    let instantiations =
        astRoot.modules
        |> List.collect findParameterizedInstantiations

    // 3. Resolve each instantiation to concrete type
    let resolvedTypes =
        instantiations
        |> List.map (resolveParameterizedInstance parameterizedTypes)

    // 4. Replace instantiations with resolved types
    replaceParameterizedTypesInAst astRoot resolvedTypes

let resolveParameterizedInstance
    (paramTypes: Map<string, ParameterizedTypeDefinition>)
    (instance: ParameterizedInstance) : TypeAssignment =

    match Map.tryFind instance.typeName paramTypes with
    | Some paramType ->
        // Validate argument count and types
        validateParameterArguments paramType instance.arguments

        // Create substitution map
        let substitutions =
            List.zip paramType.parameters instance.arguments
            |> List.map (fun (param, arg) -> (param.name, arg))
            |> Map.ofList

        // Substitute parameters in type body
        let resolvedType = substituteParametersInType substitutions paramType.typeBody

        // Create new type assignment
        {
            name = instance.instanceName
            asn1Type = resolvedType
            location = instance.location
        }
    | None ->
        failwithf "Parameterized type '%s' not found" instance.typeName

let rec substituteParametersInType (substitutions: Map<string, Asn1Value>) (asn1Type: Asn1Type) : Asn1Type =
    match asn1Type.Kind with
    | ReferenceType refType ->
        // Check if this is a parameter reference
        match Map.tryFind refType.moduleName substitutions with
        | Some paramValue ->
            // Convert parameter value to type
            convertValueToType paramValue
        | None ->
            asn1Type  // Not a parameter, keep as-is

    | Sequence seqType ->
        // Recursively substitute in sequence components
        let newChildren =
            seqType.children
            |> List.map (substituteParametersInSequenceChild substitutions)
        { asn1Type with Kind = Sequence { seqType with children = newChildren } }

    // Handle other type kinds...
```

## Phase 4: ASN.1/ACN Integration

### Asn1AcnAst.fs
**Purpose**: Merge ASN.1 structural information with ACN encoding specifications

```fsharp
type Asn1AcnAst = {
    modules: Asn1AcnModule list
    acnDependencies: AcnDependency list
}

type Asn1AcnModule = {
    name: StringLoc
    asn1Types: Map<string, Asn1Type>
    acnTypes: Map<string, AcnTypeSpec>
    // Merged information
    mergedTypes: Map<string, Asn1AcnType>
}

let mergeAsn1WithAcn (asn1Module: Asn1Module) (acnModule: AcnModule option) : Asn1AcnModule =
    let acnSpecs =
        match acnModule with
        | Some acn -> acn.typeSpecs |> Map.ofList
        | None -> Map.empty

    let mergedTypes =
        asn1Module.typeAssignments
        |> List.map (fun typeAssign ->
            let acnSpec = Map.tryFind typeAssign.name.value acnSpecs
            let mergedType = mergeTypeWithAcn typeAssign.asn1Type acnSpec
            (typeAssign.name.value, mergedType)
        )
        |> Map.ofList

    {
        name = asn1Module.name
        asn1Types = asn1Module.typeAssignments |> List.map (fun ta -> (ta.name.value, ta.asn1Type)) |> Map.ofList
        acnTypes = acnSpecs
        mergedTypes = mergedTypes
    }

let mergeTypeWithAcn (asn1Type: Asn1Type) (acnSpec: AcnTypeSpec option) : Asn1AcnType =
    {
        Kind = asn1Type.Kind
        Constraints = asn1Type.Constraints
        Location = asn1Type.Location
        acnInfo = acnSpec
        // Additional fields for merged information
        acnParameters = extractAcnParameters acnSpec
        acnInsertedFields = extractInsertedFields acnSpec
        acnDependencies = computeAcnDependencies acnSpec
    }
```

## Integration Points and Error Handling

### Cross-Reference Resolution

```fsharp
let resolveTypeReferences (astRoot: AstRoot) : AstRoot =
    // Build global type table
    let globalTypes = buildGlobalTypeTable astRoot

    // Resolve all type references
    let resolvedModules =
        astRoot.modules
        |> List.map (resolveModuleReferences globalTypes)

    { astRoot with modules = resolvedModules }

let buildGlobalTypeTable (astRoot: AstRoot) : Map<string, Asn1Type> =
    astRoot.modules
    |> List.collect (fun m ->
        m.typeAssignments
        |> List.map (fun ta -> (sprintf "%s.%s" m.name.value ta.name.value, ta.asn1Type))
    )
    |> Map.ofList
```

### Validation Integration

Each phase includes validation to catch errors early:

```fsharp
type ValidationError = {
    location: SrcLoc
    errorType: ValidationErrorType
    message: string
}

type ValidationErrorType =
    | DuplicateDefinition
    | UnresolvedReference
    | ParameterMismatch
    | ConstraintViolation
    | AcnMismatch

let validateAstPhase (astRoot: AstRoot) : ValidationResult<AstRoot> =
    [
        validateNoDuplicates astRoot
        validateAllReferencesResolved astRoot
        validateConstraintConsistency astRoot
        validateAcnCompatibility astRoot
    ]
    |> List.fold combineValidationResults (Success astRoot)
```

This AST building system provides the foundation for all subsequent compilation phases, ensuring that complex ASN.1/ACN specifications are accurately represented in a type-safe, validated form that enables reliable code generation.