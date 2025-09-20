# ASN.1 Parser Documentation

This document details the ASN.1 parser implementation using ANTLR v3.

## Overview

**Location**: `Antlr/asn1.g`
**Generated Files**: C# parser classes in `Antlr/` directory
**Purpose**: Transform ASN.1 source code into generic AST representation

## Grammar Structure

### Top-Level Productions

The ASN.1 grammar follows the ITU-T X.680 standard with some extensions:

```antlr
modules
    : module+
    ;

module
    : moduleHeader DEFINITIONS moduleBody
    ;

moduleHeader
    : UCASE_ID (LID ('.' UCASE_ID)*)?  // Module name with optional OID
    ;

moduleBody
    : ASSIGN BEGIN_ (imports)? assignments END
    ;
```

### Import/Export System

**Productions**: `imports`, `exports`
```antlr
imports
    : IMPORTS importedItems (FROM UCASE_ID)+ SEMICOLON
    ;

importedItems
    : importedItem (COMMA importedItem)*
    ;

importedItem
    : UCASE_ID     // Type names
    | LID          // Value names
    ;
```

**Processing Location**: `FrontEndAst/AntlrParse.fs:processImports`
```fsharp
let processImports (importNode: ITree) : Import list =
    // Extract imported type and value names
    // Associate with source module
    // Handle import resolution during semantic analysis
```

### Assignment Productions

#### Type Assignments
```antlr
typeAssignment
    : UCASE_ID ASSIGN type_
    ;

type_
    : (tag)? (OPTIONAL_KW | DEFAULT value)? type2
    ;

type2
    : builtinType
    | referencedType
    | constrainedType
    ;
```

#### Value Assignments
```antlr
valueAssignment
    : LID type_ ASSIGN value
    ;

value
    : builtinValue
    | referencedValue
    | definedValue
    ;
```

### Built-in Types

#### Primitive Types
```antlr
builtinType
    : integerType
    | realType
    | booleanType
    | enumeratedType
    | bitStringType
    | octetStringType
    | characterStringType
    | objectIdentifierType
    | sequenceType
    | sequenceOfType
    | setType
    | setOfType
    | choiceType
    ;
```

#### INTEGER Type
```antlr
integerType
    : INTEGER_KW (L_BRACE namedNumberList R_BRACE)?
    ;

namedNumberList
    : namedNumber (COMMA namedNumber)*
    ;

namedNumber
    : LID L_PAREN (MINUS_KW)? INT R_PAREN
    | LID L_PAREN referencedValue R_PAREN
    ;
```

**Example**:
```asn1
MessageType ::= INTEGER { request(1), response(2), error(3) }
```

**AST Generation**: `FrontEndAst/CreateAsn1AstFromAntlrTree.fs:createIntegerType`
```fsharp
let createIntegerType (antlrNode: ITree) : IntegerType =
    let namedValues = extractNamedValues antlrNode
    let constraints = extractConstraints antlrNode
    {
        namedValues = namedValues
        constraints = constraints
        hasNBits = None  // Computed later during constraint resolution
    }
```

#### ENUMERATED Type
```antlr
enumeratedType
    : ENUMERATED_KW L_BRACE enumerations R_BRACE
    ;

enumerations
    : enumeration (COMMA enumeration)*
    (COMMA ELLIPSIS (COMMA enumeration (COMMA enumeration)*)?)?
    ;

enumeration
    : LID (L_PAREN (MINUS_KW)? INT R_PAREN)?
    ;
```

**Extension Marker Handling**: The ELLIPSIS (`...`) indicates extensibility
```asn1
Color ::= ENUMERATED { red(0), green(1), blue(2), ... }
```

#### SEQUENCE Type
```antlr
sequenceType
    : SEQUENCE_KW L_BRACE (componentTypeLists)? R_BRACE
    ;

componentTypeLists
    : componentTypeList
    | componentTypeList COMMA extensionAndException extensionAdditions
    | extensionAndException extensionAdditions
    ;

componentType
    : LID type_ (OPTIONAL_KW | (DEFAULT_KW value))?
    | COMPONENTS_OF_KW type_
    ;

extensionAndException
    : ELLIPSIS (exceptionSpec)?
    ;
```

**Complex Example**:
```asn1
Message ::= SEQUENCE {
    header Header,
    body OCTET STRING OPTIONAL,
    timestamp INTEGER (0..4294967295) DEFAULT 0,
    ...,  -- Extension marker
    version INTEGER (1..10) OPTIONAL  -- Added in later version
}
```

**AST Processing**: `FrontEndAst/CreateAsn1AstFromAntlrTree.fs:createSequenceType`

#### CHOICE Type
```antlr
choiceType
    : CHOICE_KW L_BRACE alternativeTypeLists R_BRACE
    ;

alternativeTypeLists
    : alternativeTypeList
    | alternativeTypeList COMMA extensionAndException extensionAdditionAlternatives
    | extensionAndException extensionAdditionAlternatives
    ;

alternativeTypeList
    : alternativeType (COMMA alternativeType)*
    ;

alternativeType
    : LID type_
    ;
```

### Constraint System

#### Constraint Grammar
```antlr
constraint
    : L_PAREN elementSetSpecs R_PAREN
    ;

elementSetSpecs
    : elementSetSpec
    | elementSetSpec COMMA ELLIPSIS
    | elementSetSpec COMMA ELLIPSIS COMMA elementSetSpec
    ;

elementSetSpec
    : unions
    | ALL_KW EXCEPT_KW constraint
    ;

unions
    : intersections (unionMark intersections)*
    ;

intersections
    : intersectionElements (intersectionMark intersectionElements)*
    ;

intersectionElements
    : elements (EXCEPT_KW elements)?
    ;

elements
    : subtypeElements
    | objectSetElements
    | L_PAREN elementSetSpec R_PAREN
    ;
```

#### Subtype Elements
```antlr
subtypeElements
    : singleValue
    | containedSubtype
    | valueRange
    | sizeConstraint
    | typeConstraint
    | permitAlphabet
    | innerTypeConstraints
    ;

valueRange
    : lowerEndpoint DOUBLE_DOT upperEndpoint
    ;

sizeConstraint
    : SIZE_KW constraint
    ;
```

**Constraint Examples**:
```asn1
SmallInt ::= INTEGER (1..100)
MediumString ::= UTF8String (SIZE(1..255))
PositiveEvenInt ::= INTEGER ((2..MAX) INTERSECTION INCLUDES (0..MAX BY 2))
```

### String Types

```antlr
characterStringType
    : restrictedCharacterStringType
    | unrestrictedCharacterStringType
    ;

restrictedCharacterStringType
    : BMPString
    | GeneralString
    | GraphicString
    | IA5String
    | ISO646String
    | NumericString
    | PrintableString
    | TeletexString
    | T61String
    | UniversalString
    | UTF8String
    | VideotexString
    | VisibleString
    ;
```

### Tagging System

```antlr
tag
    : L_BRACKET (tagClass)? INT R_BRACKET (IMPLICIT_KW | EXPLICIT_KW)?
    ;

tagClass
    : UNIVERSAL_KW
    | APPLICATION_KW
    | PRIVATE_KW
    | CONTEXT_KW
    ;
```

**Tag Examples**:
```asn1
ExplicitTag ::= [0] EXPLICIT INTEGER
ImplicitTag ::= [APPLICATION 1] IMPLICIT UTF8String
ContextTag ::= [CONTEXT 5] BOOLEAN
```

## Error Handling

### Parse Error Recovery

**ANTLR Configuration**:
```antlr
options {
    output=AST;
    language=CSharp2;
    backtrack=true;  // Enable backtracking for ambiguous productions
}
```

### Error Reporting

**Location**: `Antlr/` generated error handlers
```csharp
public override void DisplayRecognitionError(string[] tokenNames, RecognitionException e) {
    string hdr = GetErrorHeader(e);
    string msg = GetErrorMessage(e, tokenNames);
    // Custom error reporting with source location
    ReportError(new ParseError {
        Line = e.Line,
        Column = e.CharPositionInLine,
        Message = hdr + " " + msg
    });
}
```

## AST Generation

### Token Types
**Location**: `Antlr/asn1.g:tokens` section

```antlr
tokens {
    MODULE_DEF;
    TYPE_ASSIG;
    VAL_ASSIG;
    TYPE_DEF;
    INTEGER_TYPE;
    SEQUENCE_TYPE;
    CHOICE_TYPE;
    // ... many more token types
}
```

### Tree Construction

The ANTLR parser generates tree nodes with specific token types:

```fsharp
// F# processing of ANTLR tree
let rec processAntlrNode (node: ITree) : GenericAstNode =
    match node.Type with
    | ASN1_MODULE -> processModule node
    | TYPE_ASSIGNMENT -> processTypeAssignment node
    | SEQUENCE_TYPE -> processSequenceType node
    | INTEGER_TYPE -> processIntegerType node
    // ... handle all node types
```

### Integration with F# Frontend

**Location**: `FrontEndAst/AntlrParse.fs`

```fsharp
let parseAsn1File (filename: string) : Asn1Ast.AstRoot =
    // 1. Read file and create ANTLR input stream
    let input = new ANTLRFileStream(filename)

    // 2. Create lexer and parser
    let lexer = new asn1Lexer(input)
    let tokens = new CommonTokenStream(lexer)
    let parser = new asn1Parser(tokens)

    // 3. Parse starting from 'modules' rule
    let tree = parser.modules()

    // 4. Transform ANTLR tree to F# AST
    let astRoot = CreateAsn1AstFromAntlrTree.createAstRoot tree.Tree

    // 5. Return processed AST
    astRoot
```

## Performance Considerations

### ANTLR v3 Limitations
- **Memory Usage**: ANTLR v3 can be memory-intensive for large files
- **Backtracking Cost**: Some grammar productions require expensive backtracking
- **C# Interop**: F#/C# boundary can impact performance

### Optimization Strategies

**Location**: `FrontEndAst/AntlrParse.fs:optimizeAstProcessing`

1. **Lazy AST Processing**: Don't process unused parts of the tree
2. **Caching**: Cache frequently accessed nodes
3. **Batch Processing**: Process multiple files in parallel

```fsharp
let processFilesInParallel (files: string list) : Asn1Ast.AstRoot list =
    files
    |> List.toArray
    |> Array.Parallel.map parseAsn1File
    |> Array.toList
```

## Common Parsing Issues and Debugging

### 1. Ambiguous Productions
**Problem**: Grammar rules that can match the same input in multiple ways

**Example Issue**:
```asn1
-- This can be ambiguous in some contexts
Value ::= INTEGER | SEQUENCE OF INTEGER
```

**Debug Location**: Enable ANTLR debug output in `Antlr.csproj`

### 2. Left Recursion
**Problem**: ANTLR v3 cannot handle left-recursive rules

**Wrong**:
```antlr
expression
    : expression PLUS term  // Left recursion!
    | term
    ;
```

**Fixed**:
```antlr
expression
    : term (PLUS term)*     // Right recursion
    ;
```

### 3. Lexer Conflicts
**Problem**: Multiple lexer rules can match the same token

**Debug**: Check `Antlr/` generated `.tokens` file for conflicts

### 4. Tree Structure Issues
**Problem**: AST doesn't match expected structure

**Debug Tool**: Use ANTLR's tree visualization
```fsharp
let debugAntlrTree (tree: ITree) : string =
    // Generate DOT graph representation for visualization
    tree.ToStringTree()
```

### 5. Memory Issues with Large Files
**Symptoms**: OutOfMemoryException during parsing
**Solution**: Implement streaming parser or split large files

**Location**: `FrontEndAst/AntlrParse.fs:streamingParseMode`
```fsharp
let parseFileInChunks (filename: string) (chunkSize: int) : Asn1Ast.AstRoot =
    // Split file into smaller chunks
    // Parse each chunk separately
    // Merge results with dependency resolution
```

## Integration with Frontend Processing

The ANTLR parser output feeds into the FrontEndAst pipeline:

```
ASN.1 Source → ANTLR Parser → Generic Tree → F# AST → Semantic Analysis
```

**Next Processing Stage**: `FrontEndAst/CreateAsn1AstFromAntlrTree.fs`

The parser provides the foundation for all subsequent compilation phases, so robust error handling and efficient processing are critical for the overall compiler performance.