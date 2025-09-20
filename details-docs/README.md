# ASN1SCC Detailed Documentation

ASN1SCC is the ASN.1 compiler of the European Space Agency, used extensively in space missions to support binary encoding needs in flight and ground software. Unlike typical ASN.1 compilers, ASN1SCC also supports ACN (ASN.1 Control Notation) for defining custom binary encodings, enabling communication with equipment that uses legacy data formats.

## Overview of the Compiler Toolchain

The ASN1SCC compiler consists of multiple interconnected components that transform ASN.1 and ACN source files into target language code (C, Ada/SPARK, or Scala). The compilation pipeline follows this general flow:

```
ASN.1/ACN Source Files
    ↓ (ANTLR Parsers)
Generic ASTs
    ↓ (FrontEndAst transformations)
Strongly Typed F# ASTs
    ↓ (Template resolution & merging)
Asn1AcnAst
    ↓ (BackendAst transformations)
DAst (with lambda functions)
    ↓ (String Template Generation)
Target Language Code (C/Ada/Scala)
```

## Core Architecture

### Major Components

1. **ANTLR Parsers (`Antlr/`)** - Transform source files into generic ASTs
   - `asn1.g` - ASN.1 grammar definition
   - `acn.g` - ACN grammar definition
   - Generated C# parsers that produce initial ASTs

2. **CommonTypes** - Shared utilities and fundamental types
   - Set operations, range handling, utility functions
   - Abstract macro system for template generation
   - Cross-language type definitions

3. **FrontEndAst** - Core AST definitions and transformations
   - Multiple AST representations (Generic → Parameterized → Final)
   - Template/generic resolution (similar to C++ templates or Java generics)
   - ASN.1/ACN merging and semantic validation
   - Constraint handling and type checking

4. **BackendAst** - Code generation and target-specific transformations
   - Transform Asn1AcnAst into DAst with lambda functions
   - Generate type definitions, encoding/decoding, validation
   - Create test cases and documentation (ICD)
   - Language-agnostic backend logic

5. **String Template Groups (Stg*)** - Target language code generation
   - `StgC`, `StgAda`, `StgScala` - Language-specific templates
   - `StgVarious` - Common templates and utilities
   - Template generation via `parseStg2` tool

6. **Main Executable (`asn1scc/`)** - Command-line interface and orchestration

## Key Data Flow Patterns

### AST Transformations
The compiler uses multiple AST representations, each serving specific purposes:

1. **Generic AST** (from ANTLR) - Raw parse trees
2. **Parameterized AST** - F# strongly-typed AST with templates/generics
3. **Non-Parameterized AST** - After template resolution
4. **Asn1AcnAst** - Merged ASN.1 and ACN information
5. **DAst** - Backend AST with lambda functions for code generation

### Lambda-Based Code Generation
The DAst contains lambda functions that generate code snippets:
```fsharp
funcBody : CallerScope -> ValidationStatement
```
This pattern allows the same AST to generate different target languages while maintaining type safety.

### Template System
- **String Templates (.stg)** define language-specific code patterns
- **Generated F# Functions (.stg.fs)** provide type-safe template invocation
- **Abstract Interfaces** ensure consistency across target languages
- **parseStg2** tool transforms .stg files into F# code

## Runtime Dependencies
The following directories contain runtime libraries (not compiler logic):
- **ADA_RTL2** - Ada runtime for primitive encodings
- **asn1crt** - C runtime for primitive encodings
- **asn1scala** - Scala runtime for primitive encodings

Generated code invokes functions from these runtime libraries but ASN1SCC itself does not build or link against them directly.

## Documentation Structure

This documentation is organized as follows:

- [`architecture/`](architecture/) - Detailed component descriptions and data flow
- [`parsers/`](parsers/) - ANTLR grammar analysis and AST generation
- [`common/`](common/) - Shared utilities and type systems
- [`frontend/`](frontend/) - AST transformations, validation, and merging
- [`backend/`](backend/) - Code generation and target language support
- [`templates/`](templates/) - String template system and conventions

Each section provides sufficient detail to:
- **Map problems to code locations** - Find where specific behavior is implemented
- **Understand information flow** - Track how data moves through transformation stages
- **Locate root causes** - Identify likely sources of bugs or issues
- **Plan extensions** - Understand how to add new features or target languages

## For Developers

### Common Debugging Scenarios

**Encoding/Decoding Issues**: Start in `BackendAst/DAstUPer.fs` or `BackendAst/DAstACN.fs`

**Type Definition Problems**: Check `FrontEndAst/Asn1Ast.fs` and `BackendAst/DAstTypeDefinition2.fs`

**Template/Generic Resolution**: Look in `FrontEndAst/RemoveParameterizedTypes.fs`

**Code Generation Issues**: Examine relevant `Stg*` projects and `BackendAst/GenerateFiles.fs`

**Validation Failures**: Start with `FrontEndAst/CheckAsn1.fs` and `BackendAst/DastValidate2.fs`

### Build Dependencies

The solution file shows these key dependencies:
- **parseStg2** (C#) - Must build before projects using generated templates
- **CommonTypes** - Foundation for most other projects
- **FrontEndAst** - Depends on CommonTypes and Antlr
- **BackendAst** - Depends on FrontEndAst and all Stg* projects
- **asn1scc** - Main executable, depends on BackendAst

The build process includes prebuild steps that generate F# code from .stg templates using the parseStg2 tool.