# Rust Backend

The asn1scc Rust backend generates Rust source code for encoding and decoding
ASN.1 data structures. It is a fourth code-generation target alongside the
existing C, Ada, and Scala backends.

> **Experimental.** The Rust backend is still maturing. Generated code passes
> the full regression suite for uPER and ACN, but XER and BER support is
> incomplete (see [Known Limitations](#known-limitations)).

---

## Table of Contents

1. [Overview](#overview)
2. [Building](#building)
3. [Usage](#usage)
4. [Generated Code Layout](#generated-code-layout)
5. [Runtime Crate](#runtime-crate)
6. [Type Mappings](#type-mappings)
7. [C/Ada Memory Compatibility](#cada-memory-compatibility)
8. [Known Limitations](#known-limitations)
9. [Testing](#testing)

---

## Overview

The Rust backend produces a self-contained Cargo project from one or more
ASN.1 (and optionally ACN) specification files. The generated project contains:

- **Type definitions** — Rust structs and enums mirroring the ASN.1 type
  system, annotated with `#[repr(C)]` for cross-language interop.
- **Encoding/decoding functions** — one module per ASN.1 module, with
  functions for each requested encoding.
- **Auto-generated test cases** — a test runner binary that exercises
  round-trip encode/decode for every type.

### Supported Encodings

| Encoding | Status |
|----------|--------|
| **uPER** (Unaligned Packed Encoding Rules) | ✅ Fully supported and CI-tested |
| **ACN** (ASN.1 Control Notation) | ✅ Fully supported and CI-tested |
| **XER** (XML Encoding Rules) | ⚠️ Templates generate XER code, but XER is not exercised in CI regression tests |
| **BER** (Basic Encoding Rules) | ⚠️ Runtime library (`ber.rs`) exists, but no templates generate BER code yet |

### Architecture

```
ASN.1 + ACN source files
         │
         ▼
┌─────────────────────────┐
│  Frontend (ANTLR)        │  Parse ASN.1 + ACN → parse trees
└────────────┬────────────┘
             ▼
┌─────────────────────────┐
│  AST construction (F#)   │  Each type/field gets a rust_name
│                         │  alongside c_name, ada_name, scala_name
└────────────┬────────────┘
             ▼
┌─────────────────────────┐
│  Backend (F# + STG)      │  StringTemplate-based code generation
│                         │  Produces .rs source files
└────────────┬────────────┘
             ▼
┌─────────────────────────┐
│  Runtime embedding       │  Writes asn1rust crate alongside
│                         │  generated user code
└────────────┬────────────┘
             ▼
Generated Rust Cargo project
```

---

## Building

### Prerequisites

- **Rust toolchain** — `rustc` and `cargo` (for compiling generated code)
- **.NET SDK 10.0** — required to build the asn1scc compiler itself

### Build the Compiler

```bash
dotnet build asn1scc.sln
```

This builds the full solution including the `StgRust` template project. No
separate build step is needed for Rust support.

---

## Usage

Invoke asn1scc with the `-Rust` flag to generate Rust output. The flag is
**Experimental**.

### uPER Code Generation

```bash
asn1scc -Rust -uPER -ig -typePrefix ASN1SCC_ -equal -atc -o output/ input.asn1
```

### uPER + ACN Code Generation

```bash
asn1scc -Rust -uPER -ACN -ig -typePrefix ASN1SCC_ -equal -atc -o output/ input.asn1 acn.acn
```

### Compiling and Running Generated Code

```bash
cd output/
cargo build
cargo run    # executes the auto-generated test suite
```

---

## Generated Code Layout

The backend produces a Cargo binary project with the following structure:

```
output/
├── Cargo.toml                # Binary crate manifest (depends on asn1rust)
├── mainprogram.rs            # Test runner entry point
├── <module>.rs               # Encoding/decoding functions
├── <module>Def.rs            # Type definitions (structs, enums, aliases)
├── <module>_auto_tcs.rs      # Auto-generated test case functions
├── <module>_auto_tcsDef.rs   # Test case prototypes
├── testsuite.rs              # Test suite runner
├── testsuiteDef.rs           # Test suite prototypes
├── test_case_NNN.rs          # Individual test case files
├── test_case_NNNDef.rs       # Individual test case prototypes
└── asn1rust/                 # Runtime library crate (see below)
    ├── Cargo.toml
    └── src/
        ├── lib.rs
        ├── uper.rs
        ├── acn.rs
        ├── xer.rs
        └── ber.rs
```

The `Def` suffix on type-definition files avoids filename collisions with
their encoding/decoding counterparts (e.g., `testDef.rs` for types vs.
`test.rs` for functions). Each ASN.1 module maps to one pair of files.

---

## Runtime Crate

The `asn1rust` crate is the runtime support library for generated Rust code.
It is analogous to the C runtime (`asn1crt`).

### Contents

| File | Role |
|------|------|
| `lib.rs` | Core types (`BitStream`, `ByteStream`, integer/real aliases), bit-level primitives, OID handling, error codes |
| `uper.rs` | uPER encode/decode for `OBJECT IDENTIFIER` and `RELATIVE-OID` |
| `acn.rs` | ACN encode/decode: alignment, integer/boolean/NULL patterns, IEEE-754 reals, scaled reals, strings, deferred patching |
| `xer.rs` | XER XML encode/decode: lexical scanner, XML state machine |
| `ber.rs` | BER encode/decode: tag/length, INTEGER, BOOLEAN, REAL, IA5String, BIT STRING, OCTET STRING, NULL |

The crate has **zero external dependencies** — it uses only the Rust standard
library.

### Embedding Mechanism

The six runtime files (`Cargo.toml` + five `.rs` files) are embedded as .NET
resources in the asn1scc compiler assembly at build time. When you run
asn1scc with `-Rust`, the compiler extracts these resources and writes them
into your output directory under `asn1rust/`. The generated project's
`Cargo.toml` declares a path dependency on this crate:

```toml
[dependencies]
asn1rust = { path = "asn1rust" }
```

This means the generated project is fully self-contained — no `cargo install`
or crate registry access is needed.

---

## Type Mappings

ASN.1 types map to Rust types as follows:

| ASN.1 Type | Rust Type | Notes |
|------------|-----------|-------|
| `INTEGER` | `i8`, `i16`, `i32`, or `i64` | Width chosen based on the ASN.1 range constraint |
| `REAL` | `f64` or `f32` | `f64` by default; `f32` when constrained to single precision |
| `BOOLEAN` | `bool` | Native Rust boolean; literals are `true` / `false` |
| `IA5String` (fixed-length N) | `[u8; N]` | Fixed-size byte array |
| `OCTET STRING` (fixed-length N) | `[u8; N]` | Fixed-size byte array |
| `BIT STRING` | struct with `arr: [u8; N]` and `count: usize` | Separate count field tracks the number of valid bits |
| `SEQUENCE` | `#[repr(C)] struct` | Each component is a field; `OPTIONAL` components use `Option<T>` |
| `CHOICE` | `#[repr(C, i32)] enum` | Tagged union — Rust enum with a discriminant matching the C layout |
| `ENUMERATED` | `#[repr(C)] enum` | Variants map to C-compatible integer values |
| `SEQUENCE OF` (max length N) | struct with `arr: [T; N]` and `count: usize` | Fixed-capacity array with an element count |

### Identifier Naming

| ASN.1 element | Rust convention | Example |
|---------------|---------------|---------|
| Types, structs, enums | PascalCase (`ToRustType`) | `MySequence` |
| Variables, functions, fields | snake_case (`ToRust`) | `my_field` |

ASN.1 names are sanitized: hyphens, dots, and special characters become
underscores; identifiers starting with a digit are prefixed; Rust keywords
get a trailing underscore.

---

## C/Ada Memory Compatibility

All generated struct and enum types carry `#[repr(C)]`, which guarantees a
C-compatible memory layout. This means:

- **Identical struct field ordering and padding** to the equivalent C output
  from asn1scc, enabling zero-copy data exchange between Rust and C/Ada code.
- **CHOICE types use `#[repr(C, i32)]`** — a tagged enum with a C-compatible
  discriminant, matching the `struct + union + kind` pattern used by the C
  backend.
- **SEQUENCE OF and BIT STRING use `#[repr(C)]` structs** with explicit
  array and count fields, matching the C `arr`/`count` convention.

This design allows Rust-generated types to be passed across FFI boundaries to
C or Ada code generated from the same ASN.1 specification without
serialization or marshalling.

---

## Known Limitations

### No Statement-Coverage Gate

Unlike the C and Ada backends — which enforce a 100% statement-coverage gate
using `gcov` — the Rust backend does not enforce a coverage threshold. Rust
regression tests run `cargo build && cargo run` and verify pass/fail, but do
not measure code coverage. Consider integrating `cargo llvm-cov` or
`tarpaulin` in the future to establish an equivalent gate.

### XER Not Exercised in CI

XER encoding and decoding templates exist and generate code, but XER is not
included in the CI regression test suite. The runtime module (`xer.rs`) passes
its own internal unit tests, but end-to-end XER code generation has not been
verified against the full test corpus.

### BER Templates Not Implemented

The `ber.rs` runtime module implements BER encode/decode primitives for
common types (INTEGER, BOOLEAN, REAL, IA5String, BIT STRING, OCTET STRING,
NULL). However, no StringTemplate (`.stg`) files generate BER code for
user-defined types. BER cannot be selected as an output encoding.

### Warning Suppression May Hide Codegen Bugs

Generated Rust files begin with:

```rust
#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals, dead_code)]
```

This suppresses compiler warnings caused by asn1scc's naming conventions
(which follow ASN.1 rather than Rust style) and unused functions in the
runtime crate. The trade-off is that genuine code-generation mistakes
producing malformed identifiers would also be silently suppressed.

### Slim Mode Not Supported

Slim mode (which generates only the subset of runtime functions needed by the
types in use) is C-only in the regression tool. The Rust backend always emits
the complete `asn1rust` runtime crate. This increases compile time slightly
but simplifies the build — the Rust compiler's dead-code elimination removes
unused functions automatically.

---

## Testing

Rust regression tests run through the same `runTests.py` harness as the other
backends.

### Running Rust Regression Tests

```bash
cd v4Tests
python3 scripts/runTests.py -l Rust
```

This requires `cargo` to be installed and on your `PATH`. The harness compiles
each generated Cargo project and runs the auto-generated test cases.

### Word Size Options

The test harness supports selecting the integer word size:

```bash
python3 scripts/runTests.py -l Rust -ws 4    # 32-bit integers
python3 scripts/runTests.py -l Rust -ws 8    # 64-bit integers
```

### Test Scope

- All **uPER** and **ACN** test cases in the regression suite are exercised
  and expected to pass.
- XER and BER test cases are **not** included in the Rust regression run.
