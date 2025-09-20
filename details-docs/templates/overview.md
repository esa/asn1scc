# String Template System Overview

**Location**: `Stg*` projects and `parseStg2/`
**Purpose**: Language-specific code generation through templates

## Template Architecture

### 1. String Template Groups
- **StgC**: C language templates (.stg files)
- **StgAda**: Ada/SPARK language templates
- **StgScala**: Scala language templates
- **StgVarious**: Common templates and utilities

### 2. Code Generation Pipeline
```
template.stg → parseStg2.exe → template.stg.fs → F# Interface → BackendAst calls
```

### 3. Template Examples

**C Header Template** (`StgC/header_c.stg`):
```stg
type_definition(sName, iMaxBits) ::= <<
typedef struct {
    <sName>_data data;
    int exists;
} <sName>;
>>
```

**Generated F# Function** (`StgC/header_c.stg.fs`):
```fsharp
let type_definition (sName: string) (iMaxBits: int) : string = ...
```

### 4. Abstract Interface Pattern

Each language implements the same F# interface:
```fsharp
type ITypeDefinition_c =
    abstract member type_definition : string -> int -> string
    abstract member sequence_definition : string -> string list -> string
```

This ensures:
- **Consistency**: Same parameters across C/Ada/Scala
- **Type Safety**: Template calls are statically verified
- **Extensibility**: New parameters added uniformly

## Typing Conventions

Template parameters follow naming conventions:
- `sName` → string (type/variable names)
- `iCount` → int (counts, sizes)
- `bCondition` → bool (boolean flags)
- `lst*` → lists of items

## Template Processing

The `parseStg2` C# tool:
1. Parses .stg template files
2. Extracts template definitions and parameters
3. Generates strongly-typed F# wrapper functions
4. Creates interface definitions for type safety

This system enables ASN1SCC to generate idiomatic code for multiple target languages while maintaining a single, language-agnostic backend implementation.