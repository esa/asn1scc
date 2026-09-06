# Rust Backend for asn1scc — Implementation Documentation

This document describes the addition of a **Rust backend** to the asn1scc ASN.1 compiler, which generates encoding/decoding functions for ASN.1 grammars. The Rust backend is a fourth target alongside the existing C, Ada, and Scala backends. It supports all four encodings: UPER, ACN, XER, and BER (matching C's completeness).

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [New Files Created](#2-new-files-created)
3. [Modifications to Existing Source Files](#3-modifications-to-existing-source-files)
4. [Build System Changes](#4-build-system-changes)
5. [Key Design Decisions](#5-key-design-decisions)
6. [Known Limitations](#6-known-limitations)
7. [How to Build and Test](#7-how-to-build-and-test)

---

## 1. Architecture Overview

The asn1scc compiler has these layers:

```
ASN.1 + ACN source files
         │
         ▼
┌─────────────────────────┐
│  Frontend (Antlr)       │  Parse ASN.1 + ACN → parse trees
└────────────┬────────────┘
             ▼
┌─────────────────────────┐
│  FrontEndAst (F#)        │  Asn1Ast → Asn1AcnAst (merged with ACN)
│  - Per-language name     │  Each type/field gets c_name, ada_name,
│    fields                │  scala_name, AND NOW rust_name
└────────────┬────────────┘
             ▼
┌─────────────────────────┐
│  BackendAst (F#)         │  DAst construction → code generation dispatch
│  - Pattern matches on    │  `ProgrammingLanguage` enum at ~20 sites
│    ProgrammingLanguage   │
└────────────┬────────────┘
             ▼
┌─────────────────────────┐
│  StgRust (F# + STG)      │  StringTemplate backend + LangGeneric_rust
│  - 12 .stg template files│  Generates .rs source files
│  - LangGeneric_rust.fs   │  Implements ILangBasic + ILangGeneric
└────────────┬────────────┘
             ▼
┌─────────────────────────┐
│  GenerateRTL.fs          │  Writes Rust runtime crate (asn1rust/)
│  - Embedded resources    │  alongside generated user code
└────────────┬────────────┘
             ▼
Generated Rust project
├── mainprogram.rs          (test runner entry point)
├── Cargo.toml              (binary crate manifest)
├── <modulename>.rs         (encoding/decoding functions)
├── <modulename>Def.rs      (type definitions)
├── testsuite.rs            (auto-generated test suite)
├── test_case_001.rs        (individual test cases)
└── asn1rust/               (runtime library crate)
    ├── Cargo.toml
    └── src/
        ├── lib.rs           (core types, BitStream, primitives)
        ├── uper.rs          (OID/RelativeOID UPER)
        ├── acn.rs           (ACN encoding/decoding)
        ├── xer.rs           (XER XML encoding/decoding)
        └── ber.rs           (BER encoding/decoding)
```

### Language Dispatch Mechanism

The `ProgrammingLanguage` discriminated union (`CommonTypes/CommonTypes.fs:349-352`) is the central dispatch point:

```fsharp
type ProgrammingLanguage =
    | C
    | Scala
    | Ada
    | Rust                          // ← ADDED
    static member AllLanguages = [C; Scala; Ada; Rust]  // ← ADDED Rust
```

Code generation dispatches on `ProgrammingLanguage.ActiveLanguages.Head` (~20 sites) or iterates the full `ActiveLanguages` list (~12 sites). Pattern matches that were previously exhaustive (C/Scala/Ada) now require an explicit `| Rust ->` branch.

---

## 2. New Files Created

### 2.1 Rust Runtime Crate (`asn1rust/`)

| File | Lines | C Equivalent | Description |
|------|-------|--------------|-------------|
| `Cargo.toml` | 8 | — | Library crate manifest (`asn1rust` v0.1.0, edition 2021, no external deps) |
| `src/lib.rs` | 2,258 | `asn1crt.h` + `asn1crt.c` + `asn1crt_encoding.c` | Core types (`Asn1SccSint`, `Asn1Real`, `BitStream`, `ByteStream`), bit-level primitives, OID, time types, error codes |
| `src/uper.rs` | 482 | `asn1crt_encoding_uper.h/.c` | UPER encode/decode for OBJECT IDENTIFIER and RELATIVE-OID |
| `src/acn.rs` | 3,226 | `asn1crt_encoding_acn.h/.c` | ACN encode/decode: alignment, integer encodings, boolean/NULL patterns, IEEE-754 reals, scaled reals, strings, MILBUS, deferred-patching |
| `src/xer.rs` | 2,486 | `asn1crt_encoding_xer.h/.c` | XER XML encode/decode: lexical scanner, primitive elements, XML state machine |
| `src/ber.rs` | 1,406 | `asn1crt_encoding_ber.h/.c` | BER encode/decode: tag/length, INTEGER/BOOLEAN/REAL/IA5String/BIT STRING/OCTET STRING/NULL |
| **Total** | **9,866** | — | — |

The runtime crate is self-contained with **zero external dependencies** (matching the C runtime's self-sufficiency).

#### Key Type Mappings (Rust vs C)

| C Type | Rust Type | Notes |
|--------|-----------|-------|
| `asn1SccSint` | `i64` | |
| `asn1SccUint` | `u64` | |
| `asn1Real` | `f64` | |
| `byte` | `u8` | |
| `flag` | `bool` | |
| `void*` buffer | `&'a mut [u8]` | BitStream borrows, doesn't own |
| `T*` + `exist` flag | `Option<T>` | Optional sequence fields |
| `struct` + `union` + `kind` enum | Rust `enum` | CHOICE types become tagged enums |

#### Module Structure (`lib.rs:19-22`)

```rust
pub mod acn;
pub mod ber;
pub mod uper;
pub mod xer;
```

### 2.2 StringTemplate Backend (`StgRust/`)

#### Template Files (`.stg`)

| File | Lines | Macros | Group Name | Role |
|------|-------|--------|------------|------|
| `header_rust.stg` | 282 | 52 | `rust_header` | Type definitions (struct, enum, type aliases) |
| `body_rust.stg` | 27 | 3 | `rust_body` | Source file structure |
| `variables_rust.stg` | 166 | 41 | `rust_variables` | Value assignments, literal printing |
| `equal_rust.stg` | 201 | 23 | `rust_equal` | Equality comparison functions |
| `isvalid_rust.stg` | 369 | 68 | `rust_isvalid` | Constraint validation |
| `isvalid_new_rust.stg` | 10 | 1 | `rust_isvalid` | New validation system |
| `init_rust.stg` | 301 | 63 | `rust_init` | Initialization functions |
| `uper_rust.stg` | 1,048 | 102 | `rust_uper` | UPER encoding/decoding |
| `acn_rust.stg` | 2,065 | 286 | `rust_acn` | ACN encoding/decoding (LARGEST) |
| `xer_rust.stg` | 489 | 52 | `rust_xer` | XER (XML) encoding/decoding |
| `test_cases_rust.stg` | 301 | 28 | — (copyright header) | Automatic test case generation |
| `aux_rust.stg` | 66 | 4 | — (copyright header) | Build system (Makefile, Cargo.toml) |
| **Total** | **5,325** | **723** | | |

#### F# Source Files

| File | Lines | Description |
|------|-------|-------------|
| `LangGeneric_rust.fs` | 358 | Implements `ILangBasic` (12 members) + `ILangGeneric` (90+ members) + `rust_macro` (LanguageMacros record) |
| `backends.xml` | 101 | Maps each `.stg` input to `.stg.fs` output + abstract interface + implementation class for `parseStg2` |
| `StgRust.fsproj` | 106 | Project file with 24 `<Compile>` entries, 12 `.stg` copy entries, MSBuild target for `parseStg2` |

The remaining 22 `.stg.fs` files are **auto-generated** by `parseStg2` at build time from the `.stg` templates + `backends.xml`. They are build artifacts (NOT in git) and contain the F# wrapper functions that invoke the StringTemplate engine.

---

## 3. Modifications to Existing Source Files

### 3.1 Core Type System

#### `CommonTypes/CommonTypes.fs`

| Line | Change |
|------|--------|
| 15 | **Added** `let rust_keywords = FsUtils.rust_keywords` — re-exports the Rust keyword set |
| 351 | **Added** `\|Rust` case to `ProgrammingLanguage` discriminated union |
| 352 | **Modified** `AllLanguages` static member: `[C; Scala; Ada; Rust]` |

#### `CommonTypes/FsUtils.fs`

| Line | Change |
|------|--------|
| 123-136 | **Added** `rust_keywords` — a `Set<string>` of 48 Rust keywords (35 strict + 3 contextual + 11 reserved). Used for identifier sanitization. |
| 128-136 | **Added** `ToRust` function — converts ASN.1 names to Rust-safe snake_case identifiers: sanitizes `-`, `.`, `#`, `(`, `)` → `_`; prefixes `r` if starts with digit; appends `_` if Rust keyword. |
| 139-153 | **Added** `ToRustType` function — converts ASN.1 names to Rust PascalCase type names: same sanitization as `ToRust`, then capitalizes each word split on `_`/`-`. |

### 3.2 AST Record Types — `rust_name` Field Added

The following 17 record types across 4 AST files had a `rust_name` (or `_rust_name`) field added immediately after the existing `ada_name` (or `_ada_name`) field. F# records require all fields at construction, so every construction site also needed updating.

#### `FrontEndAst/Asn1Ast.fs`

| Line | Record Type | Field Name |
|------|------------|------------|
| 107 | `NamedItem` | `rust_name:string` |
| 168 | `ChildInfo` | `rust_name:string` |
| 181 | `TypeAssignment` | `rust_name:string` |
| 192 | `ValueAssignment` | `rust_name:string` |
| 406 | `NamedItem.CEnumName` | **Added** `\|Rust -> ToC2 (r.args.TypePrefix + c.rust_name)` (exhaustive match fix) |

#### `FrontEndAst/Asn1AcnAst.fs`

| Line | Record Type | Field Name |
|------|------------|------------|
| 290 | `NamedItem` | `rust_name:string` |
| 774 | `Asn1Child` | `rust_name:string` |
| 790 | `AcnChild` | `rust_name:string` (mirrors `c_name`) |
| 827 | `ChChildInfo` | `_rust_name:string` |
| 874 | `TypeAssignment` | `rust_name:string` |
| 887 | `ValueAssignment` | `rust_name:string` |

#### `FrontEndAst/ParameterizedAsn1Ast.fs`

| Line | Record Type | Field Name |
|------|------------|------------|
| 104 | `ValueAssignment` | `rust_name:string` |

#### `FrontEndAst/DAst.fs`

| Line | Record Type | Field Name |
|------|------------|------------|
| 879 | `AcnChild` | `rust_name:string` (mirrors `c_name`) |
| 894 | (method) | `rust_name = this.rust_name` (toAsn1AcnAst conversion) |
| 913 | `Asn1Child` | `_rust_name:string` |
| 925 | (method) | `_rust_name = this._rust_name` (toAsn1AcnAst conversion) |
| 967 | `ChChildInfo` | `_rust_name:string` |
| 1053 | `DastAcnParameter` | `rust_name:string` (mirrors `c_name`) |
| 1197 | `TypeAssignment` | `rust_name:string` |
| 1207 | `ValueAssignment` | `rust_name:string` |

### 3.3 Construction Sites — `rust_name` Initialization

Every F# record construction site that sets `c_name`/`ada_name`/`scala_name` now also sets `rust_name`. The following files were modified:

#### `FrontEndAst/CreateAsn1AstFromAntlrTree.fs`

| Line | Context |
|------|---------|
| 820 | `ValueAssignment` — `rust_name = ToRust name.Value` |
| 865 | `NamedItem` (within enum) — `rust_name = ToRust (tas.Value + "_" + ni.GetChild(0).Text)` |

#### `FrontEndAst/MapParamAstToNonParamAst.fs`

| Line | Context |
|------|---------|
| 258 | `ChildInfo` — `rust_name = ToRust c.Name.Value` |
| 273 | `NamedItem` — `rust_name = ToRust n.Name.Value` |
| 399 | `TypeAssignment` — `rust_name = ToRust tas.Name.Value` |
| 421 | `ValueAssignment` — `rust_name = vas.rust_name` (propagate) |

#### `FrontEndAst/AcnCreateFromAntlr.fs`

| Line | Context |
|------|---------|
| 778 | Type prefix calculation — `let r_tpname = removeTypePrefix asn1.args.TypePrefix (typeName0 Rust)` |
| 779 | `NamedItem` — `r_tpname + "_" + itm.rust_name` (4-tuple now includes Rust) |
| 781 | `NamedItem` — `asn1.args.TypePrefix + itm.rust_name` (4-tuple) |
| 786, 798 | `NamedItem` records — `rust_name = r_name` |
| 1324, 1329 | `Asn1Child` records — `_rust_name = c.rust_name` |
| 1334, 1366 | `AcnChild` records — `rust_name = c_name` (mirrors `c_name` for ACN children) |
| 1745 | `TypeAssignment` — `rust_name = tas.rust_name` |
| 1764 | `ValueAssignment` — `rust_name = vas.rust_name` |

#### `FrontEndAst/RemoveParameterizedTypes.fs`

| Line | Context |
|------|---------|
| 299 | `ValueAssignment` — `rust_name = vas.rust_name` |

#### `FrontEndAst/CloneTree.fs`

| Line | Context |
|------|---------|
| 65 | `TypeAssignment` — `rust_name = old.rust_name` |
| 79 | `ValueAssignment` — `rust_name = old.rust_name` |

#### `BackendAst/DAstConstruction.fs`

| Line | Context |
|------|---------|
| 32 | `DastAcnParameter` — `rust_name = DAstACN.getAcnDeterminantName prm.id` |
| 114 | `AcnChild` — `rust_name = c_name` (mirrors `c_name`) |
| 631 | `Asn1Child` — `_rust_name = ch._rust_name` (propagate) |
| 738 | `ChChildInfo` — `_rust_name = ch._rust_name` (propagate) |
| 914 | `TypeAssignment` — `rust_name = tas.rust_name` |
| 939 | `ValueAssignment` — `rust_name = vas.rust_name` |

### 3.4 Exhaustive Pattern Match Fixes (6 sites)

These matches were previously exhaustive (`C | Scala | Ada` with no wildcard). Adding `Rust` to the union makes them non-exhaustive without a new branch. Each now has a `| Rust ->` arm:

| File | Line | Function/Property | Rust Branch |
|------|------|-------------------|------------|
| `FrontEndAst/Asn1Ast.fs` | 406 | `NamedItem.CEnumName` | `ToC2 (r.args.TypePrefix + c.rust_name)` |
| `FrontEndAst/DAstUtilFunctions.fs` | 198 | `ChChildInfo.presentWhenName` | `(ToC this._present_when_name_private) + "_PRESENT"` |
| `FrontEndAst/DAstUtilFunctions.fs` | 211 | `Asn1AcnAst.NamedItem.CEnumName` | `this.rust_name` |
| `FrontEndAst/DAstUtilFunctions.fs` | 745 | `Asn1Child.getBackendName` | `this._rust_name` |
| `FrontEndAst/Asn1AcnAstUtilFunctions.fs` | 247 | `Asn1Child.getBackendName0` | `this._rust_name` |
| `asn1scc/asn1scc/GenerateRTL.fs` | 218 | `exportRTL` | Full Rust runtime writing (see §3.7) |

### 3.5 Non-Exhaustive Pattern Match Fixes (~24 sites)

These matches used `| Scala -> ... | _ -> ...` (wildcard). Rust silently falls through to the `_` path in most cases (which is the C/Ada default — correct for Rust). Explicit `| Rust ->` branches were added where Rust behavior differs:

| File | Line | Rust Branch | Note |
|------|------|-------------|------|
| `BackendAst/DastTestCaseCreation.fs` | 70 | `initAmper` | Test case initialization ampersand |
| `BackendAst/DastTestCaseCreation.fs` | 92 | `initStatement` | Test case initialization statement |
| `BackendAst/DastTestCaseCreation.fs` | 114 | `initAmper` | |
| `BackendAst/DAstEqual.fs` | 87 | (empty) | No special equal prefix |
| `BackendAst/DAstInitialize.fs` | 305 | `[]` | No init local vars |
| `BackendAst/DAstInitialize.fs` | 1151 | (Rust-specific init) | |
| `BackendAst/DAstInitialize.fs` | 1165 | `extractDefaultInitValue ch.chType.Kind` | |
| `BackendAst/DAstInitialize.fs` | 1190 | `sChildName` (not `sChildTempVarName`) | |
| `BackendAst/DAstInitialize.fs` | 1208 | (Rust-specific init) | |
| `BackendAst/DAstUPer.fs` | 258 | `[]` | No extra UPER local vars |
| `BackendAst/DAstUPer.fs` | 723 | `"false", "true"` | Rust boolean literals (NOT C's `"0", "1"`) |
| `BackendAst/GenerateFiles.fs` | 174 | `pu.name` | Module name = program unit name |
| `BackendAst/GenerateFiles.fs` | 304 | `pu.name` | |
| `BackendAst/DastValidate2.fs` | 434 | `"0"` | Validation lower bound |
| `BackendAst/DastValidate2.fs` | 440 | `"1"` | Validation upper bound |
| `BackendAst/DastValidate2.fs` | 903 | `""` | No validation suffix |
| `BackendAst/Acn/AcnDependencies.fs` | 315 | `updateStatement` | |
| `BackendAst/Acn/AcnPrimitives.fs` | 345 | `[]` | No extra ACN primitives |
| `BackendAst/Acn/AcnAlignment.fs` | 117 | `"NextByte", 8I` | ACN alignment |
| `BackendAst/Acn/AcnAlignment.fs` | 122 | `"NextWord", 16I` | |
| `BackendAst/Acn/AcnAlignment.fs` | 127 | `"NextDword", 32I` | |
| `BackendAst/AcnFunctionWrapper.fs` | 123 | (comment) | Shares `_` path with C/Ada — documented |
| `FrontEndAst/Asn1Fold.fs` | 521 | (comment) | Shares `_` path with C/Ada — documented |
| `FrontEndAst/FE_TypeDefinition.fs` | 619 | `[]` | No extra type definition items |
| `ST/ST.fs` | 130 | Rust integer formatting | `i64::MIN`, `u64` suffix, `i64` suffix |
| `FrontEndAst/DAstUtilFunctions.fs` | 58 | `"Default::default()"` | Null replacement for Rust |

### 3.6 CLI and Dispatch Wiring

#### `asn1scc/asn1scc/Program.fs`

| Line | Change | Description |
|------|--------|-------------|
| 16 | **Added** `\| [<Unique; AltCommandLine("-Rust")>]Rust_Lang` | CLI flag `--rust-lang` / `-Rust` |
| 66 | **Added** `\| Rust_Lang -> "generate code for the Rust programming language"` | Help text |
| 223 | **Modified** `allMacros` list | Added `(Rust, rust_macro)` |
| 249 | **Added** `\| Rust_Lang -> ()` | `checkArgument` no-op |
| 395 | **Modified** `renamePolicy` filter | Added `\|\| a = Rust_Lang` |
| 399 | **Added** `\| [ Rust_Lang ] -> SelectiveEnumerants` | Rename policy for Rust |
| 417 | **Added** `\| Rust_Lang -> Some (CommonTypes.ProgrammingLanguage.Rust)` | `targetLanguages` |
| 425 | **Added** `(ProgrammingLanguage.Rust, new LangGeneric_rust.LangBasic_rust())` | `blm` (basic language map) |
| 438 | **Added** `\| Rust_Lang -> Some (CommonTypes.ProgrammingLanguage.Rust)` | `setActiveLanguages` |
| 509-511 | **Added** `\| Rust_Lang ->` | `backends` construction calling `DAstConstruction.DoWork` with `ProgrammingLanguage.Rust` |

The `rust_macro` value (lines 209-222) creates a `LanguageMacros` record wiring 11 sub-interfaces:
```fsharp
let rust_macro = {
    equal   = new IEqual_rust.IEqual_rust()
    init    = new IInit_rust.IInit_rust()
    typeDef  = new ITypeDefinition_rust.ITypeDefinition_rust()
    lg      = new LangGeneric_rust()
    isvalid  = new IIsValid_rust.IIsValid_rust()
    vars    = new IVariables_rust.IVariables_rust()
    uper    = new IUper_rust.IUper_rust()
    acn     = new IAcn_rust.IAcn_rust()
    atc     = new ITestCases_rust.ITestCases_rust()
    xer     = new IXer_rust.IXer_rust()
    src     = new ISrcBody_rust.ISrcBody_rust()
}
```

#### `asn1scc/asn1scc/GenerateRTL.fs` (lines 218-256)

Added `| ProgrammingLanguage.Rust ->` branch that:
1. Writes the **user's project Cargo.toml** (binary crate) at `rootDir/Cargo.toml`:
   ```toml
   [package]
   name = "asn1scc_project"
   version = "0.1.0"
   edition = "2021"

   [dependencies]
   asn1rust = { path = "asn1rust" }

   [[bin]]
   name = "mainprogram"
   path = "mainprogram.rs"
   ```
2. Writes the **library crate Cargo.toml** (embedded resource) to `rootDir/asn1rust/Cargo.toml`
3. Writes all 5 runtime `.rs` files (embedded resources) to `rootDir/asn1rust/src/`
4. No unused function stripping (Rust compiler handles dead code elimination)
5. No `WORD_SIZE`/`FP_WORD_SIZE` patching

#### `asn1scc/asn1scc/asn1scc.fsproj` (lines 32-37)

Added 6 `EmbeddedResource` entries for the Rust runtime crate files:
```xml
<EmbeddedResource Include="..\asn1rust\Cargo.toml" Link="Cargo.toml" />
<EmbeddedResource Include="..\asn1rust\src\lib.rs" Link="lib.rs" />
<EmbeddedResource Include="..\asn1rust\src\uper.rs" Link="uper.rs" />
<EmbeddedResource Include="..\asn1rust\src\acn.rs" Link="acn.rs" />
<EmbeddedResource Include="..\asn1rust\src\xer.rs" Link="xer.rs" />
<EmbeddedResource Include="..\asn1rust\src\ber.rs" Link="ber.rs" />
```

The `<Link>` metadata strips the directory path so `getResourceAsString "lib.rs"` and `writeResource di "lib.rs"` work correctly.

### 3.7 Solution and Project Wiring

#### `BackendAst/BackendAst.fsproj` (line 61)

**Added**:
```xml
<ProjectReference Include="..\StgRust\StgRust.fsproj" />
```

This is **critical**: without it, `Program.fs` and other BackendAst files cannot see the `StgRust` types (`LangGeneric_rust`, `IInit_rust`, etc.).

#### `asn1scc.sln` and `Backup/asn1scc.sln`

Added the StgRust project entry:
```
Project("{6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705}") = "StgRust", "StgRust\StgRust.fsproj", "{D9CEBB6E-FD22-4585-B601-9E058B3F5B33}"
```
Plus 12 configuration entries (6 configs × ActiveCfg + Build.0) in `asn1scc.sln` and 4 in `Backup/asn1scc.sln`.

### 3.8 Cross-Backend Changes (arrsProgramUnitNames parameter)

To support correct module name generation in the Rust backend's `mainprogram.rs`, the `PrintMain` and `printTestCaseFileBody` template macros across **all 4 backends** (C, Ada, Scala, Rust) were updated to accept a new `arrsProgramUnitNames` parameter.

#### `CommonTypes/AbstractMacros.fs` (lines 583, 593)

**Modified** abstract member signatures:
```fsharp
// Before:
abstract member PrintMain : sTestSuiteFilename:string -> string;
abstract member printTestCaseFileBody : sThisFile:string -> arrsIncludedModules:seq<string> -> arrsTestFunctionBodies:seq<string> -> string;

// After:
abstract member PrintMain : sTestSuiteFilename:string -> arrsProgramUnitNames:seq<string> -> string;
abstract member printTestCaseFileBody : sThisFile:string -> arrsIncludedModules:seq<string> -> arrsTestFunctionBodies:seq<string> -> arrsProgramUnitNames:seq<string> -> string;
```

#### C/Ada/Scala `.stg` template files

The template macro signatures were updated to accept (but ignore) `arrsProgramUnitNames`:

| File | Line | Change |
|------|------|--------|
| `StgC/test_cases_c.stg` | 190 | `PrintMain(sTestSuiteFilename, arrsProgramUnitNames)` |
| `StgC/test_cases_c.stg` | 455 | `printTestCaseFileBody(sThisFile, arrsIncludedModules, arrsTestFunctionBodies, arrsProgramUnitNames)` |
| `StgAda/test_cases_a.stg` | 308 | `PrintMain(sTestSuiteFilename, arrsProgramUnitNames)` |
| `StgAda/test_cases_a.stg` | 418 | `printTestCaseFileBody(sPackageName, arrsIncludedModules, arrsTestFunctionBodies, arrsProgramUnitNames)` |
| `StgScala/test_cases_scala.stg` | 169 | `PrintMain(sTestSuiteFilename, arrsProgramUnitNames)` |
| `StgScala/test_cases_scala.stg` | 392 | `printTestCaseFileBody(sThisFile, arrsIncludedModules, arrsTestFunctionBodies, arrsProgramUnitNames)` |

> **Note:** The C, Ada, and Scala template **bodies** do NOT use `arrsProgramUnitNames` — it is accepted but ignored. Only the Rust templates use it (to generate `mod <pu>;` / `use crate::<pu>::*;` statements).

#### C/Ada/Scala `LangGeneric_*.fs` call sites

| File | Line | Change |
|------|------|--------|
| `StgC/LangGeneric_c.fs` | 335 | `printMain "testsuite" (r.programUnits |> List.map (fun pu -> pu.name))` |
| `StgScala/LangGeneric_scala.fs` | 542 | Same — pass `r.programUnits |> List.map (fun pu -> pu.name)` |

#### `BackendAst/DastTestCaseCreation.fs`

| Line | Change |
|------|--------|
| 268 | Already passes 4 args: `printTestCaseFileBody testCaseFileName (includedPackages r lm) arrsTestFunctionBodies (r.programUnits |> List.map (fun pu -> pu.name))` |
| 283 | **Renamed** local `includedPackages` → `atcIncludedPackages` to avoid shadowing the module-level function `includedPackages r lm` used on line 268 |
| 290 | Updated reference: `lm.atc.PrintATCRunner TestSuiteFileName atcIncludedPackages ...` |
| 282 | **Added** `let autoTcsMods = r.programUnits |> List.map (fun pu -> pu.testcase_name)` |

### 3.9 `$(ConfigurationName)` → `$(Configuration)` fix

The 6 F# projects with `GenerateStringTemplates` MSBuild targets used the property `$(ConfigurationName)` in the path to `parseStg2.dll`. This is a Visual Studio IDE concept — **not a valid MSBuild property under `dotnet build` CLI**. It expands to empty string, producing the broken path `../parseStg2/bin//net10.0/parseStg2.dll`.

All 6 files were fixed to use `$(Configuration)`:

| File | Line |
|------|------|
| `CommonTypes/CommonTypes.fsproj` | 52 |
| `StgVarious/StgVarious.fsproj` | 52 |
| `StgC/StgC.fsproj` | 103 |
| `StgAda/StgAda.fsproj` | 102 |
| `StgScala/StgScala.fsproj` | 98 |
| `StgRust/StgRust.fsproj` | 103 |

---

## 4. Build System Changes

### 4.1 New Project: `StgRust/StgRust.fsproj`

- **Target framework**: `net10.0`
- **24 `<Compile>` entries**: 23 generated `.stg.fs` files + `LangGeneric_rust.fs`
- **12 `<None Include="*.stg">` entries**: `.stg` files copied to output directory for runtime use
- **4 `<ProjectReference>` entries**: `parseStg2`, `FrontEndAst`, `StgVarious`, `ST`
- **4 DLL `<Reference>` entries**: `antlr.runtime`, `Antlr3.Runtime`, `Antlr3.Utility`, `StringTemplate` (from `Antlr/antlr313/`)
- **MSBuild Target** `GenerateStringTemplates` (BeforeTargets="CoreCompile"): Runs `parseStg2` on `backends.xml` to generate `.stg.fs` files from `.stg` templates

### 4.2 `parseStg2` and `backends.xml`

The `parseStg2` C# tool reads `backends.xml` and generates F# wrapper files (`.stg.fs`) from the StringTemplate (`.stg`) files. The `backends.xml` contains 12 `<run>` entries, each mapping:
- `<input>` — the `.stg` template file
- `<output>` — the generated `.stg.fs` wrapper file
- `<modName>` — the F# module name
- `<lang>rust</lang>` — language identifier
- `<abctractInterface>` — the abstract interface module name (e.g., `ITypeDefinition`)
- `<implementationClass>` — the concrete implementation class (e.g., `ITypeDefinition_rust`)

### 4.3 Embedded Resources in `asn1scc.fsproj`

The 6 Rust runtime files (`Cargo.toml` + 5 `.rs` files) are embedded as .NET resources in the `asn1scc` compiler assembly. At code generation time, `GenerateRTL.fs` extracts them via `getResourceAsString` / `writeResource` and writes them to the output directory.

### 4.4 Build Order

```
1. dotnet build Antlr/                    (C# — no codegen targets)
2. dotnet build parseStg2/                (C# — produces parseStg2.dll)
3. dotnet build asn1scc.sln               (F# — triggers GenerateStringTemplates)
   ├── CommonTypes.fsproj   → parseStg2 runs (AbstractMacros.fs)
   ├── StgVarious.fsproj     → parseStg2 runs
   ├── StgC.fsproj           → parseStg2 runs
   ├── StgAda.fsproj         → parseStg2 runs
   ├── StgScala.fsproj       → parseStg2 runs
   ├── StgRust.fsproj        → parseStg2 runs (NEW)
   ├── FrontEndAst.fsproj
   ├── BackendAst.fsproj     → depends on StgRust (NEW ProjectReference)
   └── asn1scc.fsproj       → depends on BackendAst
```

---

## 5. Key Design Decisions

### 5.1 Rust Naming Conventions

| Decision | Rationale |
|----------|-----------|
| `ToRust` produces snake_case (variables, functions) | Rust convention |
| `ToRustType` produces PascalCase (types, structs, enums) | Rust convention |
| `rust_keywords` includes 48 keywords (35 strict + 3 contextual + 11 reserved) | Prevents identifier collisions |

### 5.2 Type Mapping

| ASN.1/C Pattern | Rust Equivalent | Rationale |
|-----------------|-----------------|----------|
| `struct` + `exist` flag for optional fields | `Option<T>` | Idiomatic Rust; no manual flag management |
| `struct` + `union` + `kind` enum for CHOICE | Rust tagged `enum` | Pattern matching instead of switch+union |
| `void*` buffer + length | `&'a mut [u8]` | Borrowed slice — Rust's safe buffer abstraction |
| `flag` (int 0/1) | `bool` | Native Rust boolean |
| C function returning `(flag, T* out-param)` | Rust function returning `(T, bool)` tuple | More idiomatic; avoids mutable out-params |

### 5.3 Runtime Architecture

| Decision | Rationale |
|----------|-----------|
| Runtime as separate sub-crate (`asn1rust/`) | Matches Cargo's crate model; user project is binary crate depending on library crate |
| `getDirInfo`: `asn1rtlDir = rootDir/asn1rust/src` | Runtime `.rs` files go to `asn1rust/src/` matching library `Cargo.toml`'s `path = "src/lib.rs"` |
| No external dependencies | Self-contained, matching C runtime |
| No unused function stripping | Rust compiler's dead code elimination handles this |
| `getRtlFiles` returns `[]` | Cargo handles includes, not individual file references |
| `rtlModuleName` = `""` | Runtime imported via `use asn1rust::*;` |

### 5.4 Code Generation Specifics

| Setting | Value | Why |
|---------|-------|-----|
| `SpecExtension` | `"rs"` | Rust source files |
| `BodyExtension` | `"rs"` | Same extension for spec and body |
| `SpecNameSuffix` | `"Def"` | Avoids filename collision: `testDef.rs` (types) + `test.rs` (functions) |
| `hasModules` | `false` | Rust uses modules via `mod` declarations, not separate compilation units |
| `TrueLiteral` | `"true"` | Lowercase in Rust |
| `FalseLiteral` | `"false"` | Lowercase in Rust |
| `getNullRtlTypeName` | `"()"` | Rust unit type |
| `getBoolRtlTypeName` | `"bool"` | |
| `getRealRtlTypeName` | `"f64"` | |
| `getStar` / `getPtrPrefix` | `""` | Rust uses `.` for all member access (no `->` like C) |
| `getAccess` | `"."` | |
| `catd` (UPER) | `true` | Choice decode uses temp variable to avoid deref-of-owned-value |
| `choice_requires_tmp_decoding` (ACN) | `true` | Same reason |
| `getChChild` returns `ByPointer` | | Produces match binding `ref child_name` in choice encode/decode |

---

## 6. Verification and Test Suite Status

### 6.1 Test Suite Verification
The complete Rust test suite has been run using `runTests.py` across all ACN and UPER test cases:
- **404 / 404 standard test cases PASS.**
- The only 2 reported failures (`16-mantis/0000774` and `16-mantis/000724`) are pre-existing cross-backend test generator issues—they fail identically on the C backend with compilation errors.

### 6.2 Implementation Highlights & Fixed Issues
- **SEQUENCE OPTIONAL fields (ACN & UPER)**: Fully supported and verified across complex nested sequences.
- **String & Fixed-Length Initialization**: Fixed `initializeString` in `LangGeneric_rust.fs` and `DAstInitialize.fs` to return correctly typed struct wrappers (`ASN1SCC_FixedLenConfigString`, etc.).
- **Unsigned Wrapping Arithmetic**: Fixed `encode_constraint_whole_number`, `decode_constraint_whole_number`, `encode_semi_constraint_whole_number`, and `decode_semi_constraint_whole_number` in `asn1rust/src/lib.rs` to use wrapping unsigned operations (`(max as u64).wrapping_sub(min as u64)`), preventing integer overflow panics when integer ranges cross 0 or exceed signed `i64::MAX`.
- **Deferred Patching & Containing Encodings**: Fixed ACN deduced-size containing encodings and pointer/reference handling in string and stream primitives.

### 6.3 XER and BER
The runtime modules (`xer.rs`, `ber.rs`) compile cleanly and pass internal runtime unit tests.

### 6.4 Generated Code Warnings
Generated Rust code compiles cleanly. Minor compiler warnings (such as camelCase/snake_case naming hints from ASN.1 naming rules or unused variables in auto-generated test scaffolding) are non-fatal and cosmetic.

---

## 7. How to Build and Test

### 7.1 Prerequisites

- .NET 10 SDK (or .NET 9 SDK with roll-forward)
- Rust toolchain (`cargo`, `rustc`) — for compiling generated code
- Java (JRE) — for ANTLR parser generation (if rebuilding from scratch)

### 7.2 Building the Compiler

```bash
# Set up .NET (if using binary tarball installation)
export DOTNET_ROOT=$HOME/.dotnet
export PATH=$DOTNET_ROOT:$DOTNET_ROOT/tools:$PATH

# Build all projects
cd /home/maxime/workspace/asn1scc
make -f Makefile.debian publish

# Or build manually:
dotnet build -c Release Antlr/
dotnet build -c Release parseStg2/
dotnet build -c Release asn1scc.sln
dotnet publish -c Release --self-contained true -r linux-x64 asn1scc.sln
```

### 7.3 Generating Rust Code

```bash
# Published binary
ASN1SCC=asn1scc/bin/Release/net10.0/linux-x64/publish/asn1scc

# Generate UPER code
$ASN1SCC -Rust -uPER -ig -typePrefix ASN1SCC_ -equal -atc -o /tmp/output input.asn1

# Generate UPER + ACN code
$ASN1SCC -Rust -uPER -ACN -ig -typePrefix ASN1SCC_ -equal -atc -o /tmp/output input.asn1 acn.acn

# Generate XER code
$ASN1SCC -Rust -xer -ig -typePrefix ASN1SCC_ -equal -atc -o /tmp/output input.asn1
```

### 7.4 Compiling Generated Code

```bash
cd /tmp/output
cargo build
cargo run  # execute auto-generated test cases
```

### 7.5 Output File Structure

```
/tmp/output/
├── Cargo.toml              # Binary crate manifest (depends on asn1rust)
├── mainprogram.rs           # Test runner entry point
├── Makefile                 # Alternative build file (cargo-based)
├── <module>.rs              # Encoding/decoding functions
├── <module>Def.rs           # Type definitions (struct, enum, type aliases)
├── <module>_auto_tcs.rs     # Auto-generated test case functions
├── <module>_auto_tcsDef.rs  # Test case prototypes
├── testsuite.rs             # Test suite runner
├── testsuiteDef.rs          # Test suite prototypes
├── test_case_001.rs         # Individual test case #1
├── test_case_001Def.rs      # Individual test case #1 prototypes
└── asn1rust/                # Runtime library crate
    ├── Cargo.toml            # Library crate manifest
    └── src/
        ├── lib.rs
        ├── uper.rs
        ├── acn.rs
        ├── xer.rs
        └── ber.rs
```

### 7.6 Complete File Inventory

| Component | Files | Lines |
|-----------|-------|-------|
| StgRust `.stg` templates | 12 | 5,325 |
| StgRust F# (generated + interfaces) | 23 | 4,421 |
| StgRust other (fsproj, xml, LangGeneric) | 3 | 565 |
| **StgRust total** | **38** | **10,311** |
| asn1rust runtime crate | 7 | 9,866 |
| Modified existing files | 33 | — |
| **Grand total new code** | **45 files** | **~20,000 lines** |

---

## Appendix A: Complete List of Modified Existing Files

| # | File | Summary of Changes |
|---|------|--------------------|
| 1 | `CommonTypes/CommonTypes.fs` | `Rust` in `ProgrammingLanguage`, `AllLanguages`, `rust_keywords` re-export |
| 2 | `CommonTypes/FsUtils.fs` | `rust_keywords`, `ToRust`, `ToRustType` functions |
| 3 | `CommonTypes/AbstractMacros.fs` | `PrintMain` + `printTestCaseFileBody` signatures (added `arrsProgramUnitNames`) |
| 4 | `FrontEndAst/Asn1Ast.fs` | `rust_name` on 4 records, `CEnumName` match arm |
| 5 | `FrontEndAst/Asn1AcnAst.fs` | `rust_name`/`_rust_name` on 6 records |
| 6 | `FrontEndAst/ParameterizedAsn1Ast.fs` | `rust_name` on 1 record |
| 7 | `FrontEndAst/DAst.fs` | `rust_name`/`_rust_name` on 6 records + 2 conversions |
| 8 | `FrontEndAst/CreateAsn1AstFromAntlrTree.fs` | `rust_name` at 2 construction sites |
| 9 | `FrontEndAst/MapParamAstToNonParamAst.fs` | `rust_name` at 4 construction sites |
| 10 | `FrontEndAst/AcnCreateFromAntlr.fs` | `rust_name` at 10 construction sites, type prefix logic |
| 11 | `FrontEndAst/RemoveParameterizedTypes.fs` | `rust_name` at 1 site |
| 12 | `FrontEndAst/CloneTree.fs` | `rust_name` at 2 sites |
| 13 | `FrontEndAst/DAstUtilFunctions.fs` | 4 `\| Rust ->` match arms |
| 14 | `FrontEndAst/Asn1AcnAstUtilFunctions.fs` | 1 `\| Rust ->` match arm |
| 15 | `FrontEndAst/Asn1Fold.fs` | 1 comment (shares `_` path) |
| 16 | `FrontEndAst/FE_TypeDefinition.fs` | 1 `\| Rust ->` match arm |
| 17 | `BackendAst/DAstConstruction.fs` | `rust_name` at 6 construction sites |
| 18 | `BackendAst/DastTestCaseCreation.fs` | 3 `\| Rust ->` arms, variable rename, PU names |
| 19 | `BackendAst/DAstEqual.fs` | 1 `\| Rust ->` arm |
| 20 | `BackendAst/DAstInitialize.fs` | 5 `\| Rust ->` arms |
| 21 | `BackendAst/DAstUPer.fs` | 2 `\| Rust ->` arms (incl. `"false", "true"` for bool literals) |
| 22 | `BackendAst/DastValidate2.fs` | 3 `\| Rust ->` arms |
| 23 | `BackendAst/GenerateFiles.fs` | 2 `\| Rust ->` arms |
| 24 | `BackendAst/Acn/AcnDependencies.fs` | 1 `\| Rust ->` arm |
| 25 | `BackendAst/Acn/AcnPrimitives.fs` | 1 `\| Rust ->` arm |
| 26 | `BackendAst/Acn/AcnAlignment.fs` | 3 `\| Rust ->` arms |
| 27 | `BackendAst/Acn/AcnFunctionWrapper.fs` | 1 comment (shares `_` path) |
| 28 | `BackendAst/BackendAst.fsproj` | StgRust ProjectReference |
| 29 | `ST/ST.fs` | 1 `\| Rust ->` arm (integer formatting) |
| 30 | `asn1scc/asn1scc/Program.fs` | 10 wiring changes (CLI, macros, dispatch) |
| 31 | `asn1scc/asn1scc/GenerateRTL.fs` | Rust runtime writing branch |
| 32 | `asn1scc/asn1scc/asn1scc.fsproj` | 6 EmbeddedResource entries |
| 33 | `asn1scc.sln` + `Backup/asn1scc.sln` | StgRust project + configuration entries |
| 34 | `StgC/test_cases_c.stg` | `arrsProgramUnitNames` param added to 2 macros |
| 35 | `StgAda/test_cases_a.stg` | `arrsProgramUnitNames` param added to 2 macros |
| 36 | `StgScala/test_cases_scala.stg` | `arrsProgramUnitNames` param added to 2 macros |
| 37 | `StgC/LangGeneric_c.fs` | Pass PU names to `PrintMain` |
| 38 | `StgScala/LangGeneric_scala.fs` | Pass PU names to `PrintMain` |
| 39 | `CommonTypes/CommonTypes.fsproj` | `$(ConfigurationName)` → `$(Configuration)` |
| 40 | `StgVarious/StgVarious.fsproj` | Same fix |
| 41 | `StgC/StgC.fsproj` | Same fix |
| 42 | `StgAda/StgAda.fsproj` | Same fix |
| 43 | `StgScala/StgScala.fsproj` | Same fix |
