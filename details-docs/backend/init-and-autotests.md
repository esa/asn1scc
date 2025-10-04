# Backend Initialization and Auto-Generated Tests

## Scope and Sources

This document precisely describes the backend initialization pipeline and automatic test case generation in ASN1SCC, focusing on the `InitFunction` record and related machinery in the Backend AST (DAst).

**Key sources:**
- `FrontEndAst/DAst.fs:332-348` – `InitFunction` record definition
- `FrontEndAst/DAst.fs:294-319` – `InitFunctionResult`, `AutomaticTestCase`, `InitProcedure0`, `InitGlobal`
- `BackendAst/DAstInitialize.fs:172-223` – `createInitFunctionCommon` (construction)
- `BackendAst/DAstInitialize.fs:225-1280` – Type-specific init function builders (Integer, Real, IA5String, OctetString, NullType, BitString, Boolean, Enumerated, ObjectIdentifier, SequenceOf, Sequence, Choice, ReferenceType)
- `BackendAst/DastTestCaseCreation.fs:100-221` – Test case generation using `automaticTestCases`
- `BackendAst/GenerateFiles.fs:23-28,105-234` – Emission of init procedures/functions/globals
- `BackendAst/DAstVariables.fs:53-183` – Use of `initByAsn1Value` for value assignment printing

## Executive Summary

ASN1SCC's backend generates initialization code that can default-initialize any ASN.1 type to a valid value or initialize it from a concrete ASN.1 value assignment. The `InitFunction` record (DAst.fs:332-348) unifies these concerns: it contains lambdas for emitting default initialization expressions/procedures/functions and for initializing from `Asn1ValueKind` values. It also bundles a list of `AutomaticTestCase` records that drive encode/decode round-trip test generation. The system distinguishes between local initialization paths (expressions) and global constant paths (const globals), with backend-specific nuances (C uses `{}` for empty sequences; Ada uses `(null record)`; Scala uses `TypeName()`). The `nonEmbeddedChildrenFuncs` field tracks child types requiring separate init procedures/functions rather than inline initialization. Every ASN.1 type has an `InitFunction` built during DAst construction (DAstConstruction.fs) and consumed during code emission (GenerateFiles.fs).

## Key Types and Fields (DAst.InitFunction)

| Field | Semantics | Relationships |
|-------|-----------|---------------|
| `initExpressionFnc` | Lambda returning a **local** default-initialization expression (e.g., `0`, `{}`, `TypeName()`) usable inline at variable declaration or assignment. | Used when inline init is sufficient; contrast with `initProcedure`. (DAst.fs:333) |
| `initExpressionGlobalFnc` | Lambda returning a **global const** default-initialization expression. Enables declaration of compile-time constant globals for default values (when `generateConstInitGlobals=true`). | Parallel to `initExpressionFnc`; typically identical except for child references (may use global constant names instead of function calls). (DAst.fs:334-335) |
| `initProcedure` | Optional record (`InitProcedure0`) with procedure name, definition, and body that mutates a parameter to default-initialize it. Language-specific: C/Ada use procedures when `initMethod=Procedure`. | Present when type requires procedural init (complex types); absent for simple types. Mutually exclusive selection with `initFunction` based on `lm.lg.initMethod`. (DAst.fs:336) |
| `initFunction` | Optional record (`InitProcedure0`) with function name, definition, and body that returns a default-initialized value. Language-specific: Scala uses functions when `initMethod=Function`. | Present when type requires functional init; typically Scala-only. Mirrors `initProcedure` for functional languages. (DAst.fs:337) |
| `initGlobal` | Optional record (`InitGlobal`) with global constant name, definition, and body. Only emitted when `generateConstInitGlobals=true` and `initMethod=Procedure`. | Enables global const instead of init procedure call at each initialization site (performance optimization in C/Ada). (DAst.fs:338; GenerateFiles.fs:115-119) |
| `initTas` | Lambda taking `CodegenScope` and returning `InitFunctionResult` with statements that default-initialize the type. Used **inside** the init procedure/function body. | Core implementation: produces statements for the type's default initialization. (DAst.fs:340) |
| `initByAsn1Value` | Lambda taking `CodegenScope` and `Asn1ValueKind`, returning string statements that initialize the type to match the given ASN.1 value. | Used when printing value assignments (DAstVariables.fs:53-183) and when initializing from default values (DAstInitialize.fs:999-1001). (DAst.fs:341) |
| `automaticTestCases` | List of `AutomaticTestCase` records, each providing a test-case initialization function and a type-ID map for ACN validation. | Drives encode/decode round-trip test generation (DastTestCaseCreation.fs:212-221); filtered by encoding validity (ACN checks via `isTestVaseValid`, line 209-211). (DAst.fs:343) |
| `user_aux_functions` | List of `(string*string)` pairs (definition, body) for auxiliary functions needed by this type's initialization (e.g., BitString/Enumerated helpers). | Emitted alongside main init procedure (GenerateFiles.fs:122,233-234). (DAst.fs:344) |
| `nonEmbeddedChildrenFuncs` | List of child `InitFunction` records for first-level children that require separate init procedures/functions rather than inline initialization. | Used to emit child init functions recursively (GenerateFiles.fs:25-28,110,222-224); prevents duplicate generation. (DAst.fs:345-346) |

## Lifecycle and Flow

```
AST/Type Info (Asn1AcnAst)
    ↓
[DAstConstruction.fs calls type-specific builders]
    ↓
DAstInitialize.fs: createIntegerInitFunc, createRealInitFunc, ...
    ↓ (all call createInitFunctionCommon)
InitFunction record constructed
    ├─ initExpressionFnc/initExpressionGlobalFnc: lambdas for inline/global expr
    ├─ initProcedure/initFunction/initGlobal: optional procedures/functions
    ├─ initTas: lambda for procedure body
    ├─ initByAsn1Value: lambda for value-driven init
    ├─ automaticTestCases: list of test-case generators
    ├─ user_aux_functions: auxiliary definitions
    └─ nonEmbeddedChildrenFuncs: child init functions
    ↓
[Stored in DAst.Asn1Type.initFunction]
    ↓
GenerateFiles.fs: emits code
    ├─ initProcedure/initFunction → header (.def) + source (.body)
    ├─ initGlobal → const global definitions (when enabled)
    └─ nonEmbeddedChildrenFuncs → recursive emission
    ↓
DastTestCaseCreation.fs: printAllTestCasesAndTestCaseRunner
    ├─ Iterates automaticTestCases (line 212)
    ├─ Filters by encoding validity (line 209-214)
    └─ Calls atc.initTestCaseFunc to generate test initialization code
    ↓
Test case files (test_case_NNN.c/.adb/.scala)
```

## Construction Sites and Use Sites

**Construction (DAstInitialize.fs):**
- `createIntegerInitFunc` (line 225): Constructs `InitFunction` for INTEGER types. Calls `createInitFunctionCommon` at line 265 with default-value lambdas and automatic test cases from `IntegerAutomaticTestCaseValues`.
- `createRealInitFunc` (line 267): Similar for REAL. Calls `createInitFunctionCommon` at line 306.
- `createIA5StringInitFunc` (line 319): For IA5String. Test cases include fragmentation sizes for large strings (line 355-358). Calls `createInitFunctionCommon` at line 370.
- `createOctetStringInitFunc` (line 372): For OCTET STRING. Handles fixed/variable size distinction (line 385-387, 391-392). Calls `createInitFunctionCommon` at line 457.
- `createNullTypeInitFunc` (line 459): Simplest case; single test case. Calls `createInitFunctionCommon` at line 471.
- `createBitStringInitFunc` (line 473): For BIT STRING. Similar to OCTET STRING. Calls `createInitFunctionCommon` at line 575, 602, 628, 675, 703 (multiple paths).
- `createSequenceOfInitFunc` (line 715): For SEQUENCE OF. Complex: bundles child test cases (line 746-816), tracks `nonEmbeddedChildrenFuncs` (line 843-848). Calls `createInitFunctionCommon` at line 860.
- `createSequenceInitFunc` (line 862): For SEQUENCE. Handles optional children (line 879-886, 927-931), empty sequence edge case (line 1042-1044, 1077). Calls `createInitFunctionCommon` at line 1083.
- `createChoiceInitFunc` (line 1087): For CHOICE. Tracks first alternative as default.
- `createReferenceTypeInitFunc` (DAstInitialize.fs:1240-1279): Wraps base type's `InitFunction`, potentially adding new test cases or forwarding.

**Use Sites (GenerateFiles.fs):**
- Line 110: `getInitializationFunctions tas.Type.initFunction |> List.choose(fun i_f -> i_f.initProcedure)` – Collects all init procedures (self + nonEmbeddedChildren) for header emission.
- Line 112: `getInitializationFunctions tas.Type.initFunction |> List.choose(fun i_f -> i_f.initFunction)` – Collects init functions (Scala).
- Line 119: `GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.initFunction.initGlobal)` – Collects global const definitions.
- Line 122: `tas.Type.initFunction.user_aux_functions |> List.map fst` – Collects auxiliary function definitions for header.
- Line 222-224: `getInitializationFunctions t.Type.initFunction |> List.choose(fun i_f -> i_f.initProcedure/initFunction) |> List.map(fun c -> c.body)` – Emits procedure/function bodies.
- Line 230: `GetMySelfAndChildren t.Type |> List.choose(fun t -> t.initFunction.initGlobal) |> List.map(fun c -> c.body)` – Emits global const bodies.
- Line 234: `t.Type.initFunction.user_aux_functions |> List.map snd` – Emits auxiliary function bodies.

**Use Sites (DastTestCaseCreation.fs):**
- Line 212: `for atc in t.Type.initFunction.automaticTestCases` – Iterates test cases for each type assignment.
- Line 216: `let initStatement = atc.initTestCaseFunc p` – Invokes test-case init lambda to generate initialization code for the test case.

**Use Sites (DAstVariables.fs):**
- Line 118: `so.childType.initFunction.initExpressionFnc ()` – Gets default child value for SEQUENCE OF printing.
- Line 151, 152: `x.Type.initFunction.initExpressionFnc ()` – Gets default value for optional SEQUENCE children.
- Line 728: `childType.initFunction.initByAsn1Value ({p with accessPath = new_arg}) chv.kind` – Initializes SEQUENCE OF child from value.
- Line 883: `seqChild.Type.initFunction.initByAsn1Value ... chv.Value.kind` – Initializes SEQUENCE child from value.

## Backend-specific Notes

### C Backend
- **Init method:** Procedure (mutation-based). `initProcedure` is emitted (GenerateFiles.fs:109-110).
- **Global constants:** When `generateConstInitGlobals=true`, `initGlobal` definitions are emitted and `initProcedure` bodies call assignment from the global (DAstInitialize.fs:195-200).
- **Empty sequences:** `initExpressionFnc` returns `{}` (StgC/LangGeneric_c.fs:177).
- **Local vs global init:** `initExpressionFnc` and `initExpressionGlobalFnc` are often identical, but for composite types, local uses function calls while global uses const globals (if enabled).

### Ada Backend
- **Init method:** Procedure (mutation-based). Similar to C.
- **Empty sequences:** `initExpressionFnc` returns `(null record)` (StgAda/LangGeneric_a.fs:97).
- **Pragma Annotate:** For SEQUENCE OF first element, emits pragma to suppress false positives in SPARK (DAstInitialize.fs:732-735).

### Scala Backend
- **Init method:** Function (return-value-based). `initFunction` is emitted instead of `initProcedure` (GenerateFiles.fs:112).
- **Empty sequences:** `initExpressionFnc` returns `TypeName()` (StgScala/LangGeneric_scala.fs:229).
- **Test cases:** Uses `initProcedure.funcName` (if present) as init function name (DastTestCaseCreation.fs:67-70, 106-112).

## Invariants and Edge Cases

**Invariants:**
1. Every `Asn1Type` has an `initFunction` field populated during DAst construction (enforced by construction functions in DAstInitialize.fs).
2. `initExpressionFnc` and `initExpressionGlobalFnc` are always present lambdas (never None); they may return identical or different strings depending on global const usage.
3. `initProcedure` and `initFunction` are mutually exclusive based on `lm.lg.initMethod` (Procedure vs Function); never both present.
4. `initGlobal` is only present when `generateConstInitGlobals=true` AND `initMethod=Procedure` (DAstInitialize.fs:195-196, 208-209; GenerateFiles.fs:117-118, 228-229).
5. `nonEmbeddedChildrenFuncs` is empty for primitive types; non-empty for SEQUENCE/SEQUENCE OF/CHOICE when children have `initProcedure` and `generateConstInitGlobals=false` (DAstInitialize.fs:843-848, 1013-1034).
6. Each `AutomaticTestCase` in `automaticTestCases` has a `testCaseTypeIDsMap` used for ACN validity checks (DastTestCaseCreation.fs:209-214).

**Edge Cases:**
1. **Empty SEQUENCE (C):** When a SEQUENCE has no ASN.1 children (only ACN children or fully absent optionals), `initExpressionFnc` returns `{}` (DAstInitialize.fs:1077; StgC/LangGeneric_c.fs:177). The comment at DAst.fs:335 ("usually present except ... empty sequence") is misleading—both `initExpressionFnc` and `initExpressionGlobalFnc` ARE present; they return `{}` rather than being absent.
2. **SEQUENCE OF with zero-length:** For SEQUENCE OF with minSize=maxSize=0, test cases include zero-length (DAstInitialize.fs:436-437, 807-808) and `initExpressionFnc` returns empty-sequence syntax.
3. **Fragmentation cases:** For sizeable types (IA5String, OCTET STRING, BIT STRING, SEQUENCE OF) with maxSize > 65536, additional test cases are generated at fragmentation boundaries (16384, 32768, 49152, 65535 + offsets) (DAstInitialize.fs:308-318, 420-423, 812-814).
4. **Optional SEQUENCE children with default values:** When an optional child has a default value, `initByAsn1Value` is invoked with the default (DAstInitialize.fs:998-1001) rather than `initTas`.
5. **Reference types:** For `ReferenceType` with additional constraints, a new `InitFunction` is created; otherwise, the base type's `InitFunction` is wrapped or forwarded (DAstInitialize.fs:1240-1279).

## Minimal Examples

### Example 1: Default Initialization (INTEGER)
**Type:** `MyInt ::= INTEGER (0..100)`

**Construction (DAstInitialize.fs:225-265):**
```fsharp
let createIntegerInitFunc ... =
    let constantInitExpression () = "0"  // Zero is allowed
    let tasInitFunc (p:CodegenScope) =
        {funcBody = initInteger (lm.lg.getValue p.accessPath) "0" ...; ...}
    createInitFunctionCommon r lm t typeDefinition funcBody tasInitFunc testCaseFuncs constantInitExpression constantInitExpression [] [] []
```

**Fields produced:**
- `initExpressionFnc () = "0"` (local expression)
- `initExpressionGlobalFnc () = "0"` (global expression)
- `initProcedure = Some {funcName="MyInt_Initialize"; def="void MyInt_Initialize(MyInt* pVal);"; body="void MyInt_Initialize(MyInt* pVal) { *pVal = 0; }"}` (C backend, Procedure mode)
- `initTas p = {funcBody="*pVal = 0;"; resultVar="pVal"; localVariables=[]}`
- `initByAsn1Value p (IntegerValue iv) = "*pVal = 42;"` (for iv=42)
- `automaticTestCases = [{initTestCaseFunc=...; testCaseTypeIDsMap=Map[(MyInt.id, TcvAnyValue)]}]` (default: 0, min, max test cases)
- `nonEmbeddedChildrenFuncs = []`

**Usage (GenerateFiles.fs:110,222):**
- Header: `void MyInt_Initialize(MyInt* pVal);`
- Source: `void MyInt_Initialize(MyInt* pVal) { *pVal = 0; }`

### Example 2: Initialization from ASN.1 Value
**Value assignment:** `myIntValue MyInt ::= 42`

**Usage (DAstVariables.fs:53-60):**
```fsharp
let printValue r lm curProgramUnitName t parentValue (IntegerValue v) =
    lm.lg.intValueToString v intClass  // Returns "42"
```
Alternatively, when printing via `initByAsn1Value`:
```fsharp
t.initFunction.initByAsn1Value p (IntegerValue 42I)  // Returns "*pVal = 42;"
```

**Emitted code (C):**
```c
const MyInt myIntValue = 42;
```

### Example 3: SEQUENCE OF with nonEmbeddedChildrenFuncs
**Type:** `MySeqOf ::= SEQUENCE (SIZE(0..10)) OF MyComplexType`

**Construction (DAstInitialize.fs:819-860):**
```fsharp
let nonEmbeddedChildrenFuncs =
    match childType.initFunction.initProcedure with
    | None  -> []
    | Some _ when r.args.generateConstInitGlobals  -> []  // Globals enabled: inline call to global
    | Some _  -> [childType.initFunction]  // Child needs separate init procedure
```

**Fields produced:**
- `nonEmbeddedChildrenFuncs = [childType.initFunction]` (when childType has `initProcedure` and globals disabled)
- `initTas` body calls child init procedure: `MyComplexType_Initialize(&pVal->arr[i]);`

**Usage (GenerateFiles.fs:25-28):**
```fsharp
let rec getInitializationFunctions (isValidFunction:InitFunction) =
    seq {
        for c in isValidFunction.nonEmbeddedChildrenFuncs do
            yield! getInitializationFunctions c
        yield isValidFunction
    } |> Seq.toList
```
This recursively collects `MyComplexType_Initialize` for emission before `MySeqOf_Initialize`.

## Open Questions / TODO

None. All aspects of `InitFunction` construction, field semantics, and emission are documented with specific references to source code. The comment at DAst.fs:335 regarding "usually present except ... empty sequence" is clarified: both `initExpressionFnc` and `initExpressionGlobalFnc` are always present; they return backend-specific empty-sequence literals (C: `{}`, Ada: `(null record)`, Scala: `TypeName()`).
