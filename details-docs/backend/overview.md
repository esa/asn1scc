# Backend Architecture Reference

The backend transforms the merged `Asn1AcnAst` tree into target-language-ready lambdas and artifacts, threading state across dedicated modules in `FrontEndAst/DAst.fs` and `BackendAst/*.fs`. The notes below map the major types and generators to the code so you can jump from a behavioural question straight to the responsible function.

## Call Context Types (FrontEndAst/DAst.fs)

- `CallerScope` (`FrontEndAst/DAst.fs:22`): couples the caller module name with the current `Selection`, letting generators know whether they are emitting in-place or copy semantics while keeping naming deterministic.
- `AlphaFunc` (`FrontEndAst/DAst.fs:27`): stores a lazily-evaluated helper lambda (`CallerScope -> string`) plus its stable name so templates can emit reusable snippets without immediate expansion.
- `FunctionType` (`FrontEndAst/DAst.fs:92`): discriminates which flavour of function (`Init`, `IsValid`, `Equal`, `UperEncDec`, `AcnEncDec`, `XerEncDec`, `BerEncDec`) a caller wants, enabling the call graph in `State` to filter by encoding.
- `Caller` / `Callee` (`FrontEndAst/DAst.fs:102` / `FrontEndAst/DAst.fs:107`): record which type assignment invoked which helper, forming the raw edges that `calculateFunctionToBeGenerated` later walks.

## Generation State and Wrappers (FrontEndAst/DAst.fs)

- `State` (`FrontEndAst/DAst.fs:112`): central mutable record that tracks allocated error codes, deferred alpha helpers, generated type ids, ICD hashes, and the cross-module `functionCalls` map consumed when pruning unused code.
- `ErrorCode` (`FrontEndAst/DAst.fs:275`): encapsulates the numeric id, symbolic name, and optional comment that validators and encoders push into the state.
- `InitFunction` (`FrontEndAst/DAst.fs:328`): bundles default expressions, optional global/procedure bodies, per-call `CallerScope` lambdas, child init references, and user-provided auxiliary functions.
- `IsValidFunction` (`FrontEndAst/DAst.fs:367`): captures generated validation statements, error codes, and local variables plus pointers to non-embedded child validators so code emission can decide between inlining and separate calls.
- `AutomaticTestCase` (`FrontEndAst/DAst.fs:312`): holds a lambda that creates initialization statements for a synthetic test plus the `testCaseTypeIDsMap` the ACN backend uses to check inserted-field coverage.

## Compilation Containers (FrontEndAst/DAst.fs)

- `ProgramUnit` (`FrontEndAst/DAst.fs:1176`): represents a generated compilation unit (spec/body/testcase files, imports, and sorted type assignments) after language-specific splitting.
- `AstRoot` (`FrontEndAst/DAst.fs:1191`): top-level backend artifact; it carries rewritten files, ACN constants, command-line settings, derived program units, accumulated ICD hashes, and the `callersSet` gate used by `GenerateFiles`.
- `State.callersSet` vs `ProgramUnit.imports`: `calculateFunctionToBeGenerated` populates the former so `GenerateFiles` knows which functions to emit per unit, while the latter arises from `DAstProgramUnit.sortTypes` ensuring dependency order.

## Generation Workflow

- Map each `Asn1AcnAst.TypeAssignment` through `DAstConstruction.mapTas` to attach init/validate/encode lambdas and record call graph edges.
- Re-map files to carry the enriched types and value assignments, then build `ProgramUnit` structures via `DAstProgramUnit.createProgramUnits`.
- Walk the call graph (`State.functionCalls`) to populate `AstRoot.callersSet`, filtering functions when users request partial emission or PDU detection.
- Feed `AstRoot` to `GenerateFiles.generateAll`, which pulls only the functions that survived the call-graph filter and writes both source files and optional auto tests.

## Module Reference

### DAstConstruction (`BackendAst/DAstConstruction.fs`)

- Purpose: orchestrates conversion from `Asn1AcnAst` to `DAst.Asn1Type`, wiring together init, validation, equality, encoding/decoding, test, and ICD helpers while threading `State`.
- Key entry points: `mapTas` (`BackendAst/DAstConstruction.fs:889`) builds a single type assignment; type-specific builders such as `createInteger` (`BackendAst/DAstConstruction.fs:113`) and `createSequenceFunction` calls wire per-kind pipelines; `calculateFunctionToBeGenerated` (`BackendAst/DAstConstruction.fs:1126`) and `DoWork` (`BackendAst/DAstConstruction.fs:1205`) compute the final `AstRoot`.
- AST interaction: consumes `Asn1AcnAst` modules, leverages `Asn1Fold` for child traversal, and supplies the enriched `AstRoot` plus `State.functionCalls` that downstream emission logic depends on.
- Naming watch: the catch-all record `State` would read clearer as `GenerationState`, and `mapTas` / `mapFile` could be renamed `buildTypeAssignment` / `buildFile` to emphasise that they return rewritten nodes rather than pure maps.

```fsharp
let private createInteger r deps lm _ t o state =
    let defOrRef            = lm.lg.definitionOrRef o.definitionOrRef
    let initFunction        = TL "DAstInitialize" (fun () -> DAstInitialize.createIntegerInitFunc r lm t o defOrRef)
    let isValidFunction, s1 = TL "DastValidate2" (fun () -> DastValidate2.createIntegerFunction r lm t o defOrRef state)
    let uperEncFunction, s2 = TL "DAstUPer"       (fun () -> DAstUPer.createIntegerFunction r lm Codec.Encode t o defOrRef None isValidFunction s1)
    let acnEncFunction, s4  = TL "DAstACN"        (fun () -> DAstACN.createIntegerFunction r deps lm Codec.Encode t o defOrRef isValidFunction uperEncFunction s2)
```

### DAstInitialize (`BackendAst/DAstInitialize.fs`)

- Purpose: materialises `InitFunction` records with default expressions, optional globals, and child initialisers per ASN.1 kind.
- Key entry points: functions like `createIntegerInitFunc` (`BackendAst/DAstInitialize.fs:225`), `createSequenceOfInitFunc` (`BackendAst/DAstInitialize.fs:715`), and `createChoiceInitFunc` (`BackendAst/DAstInitialize.fs:1087`) specialise initialization logic per type.
- AST interaction: interprets constraint sets via `RangeSet`, `ValueSet`, and `SizeableSet` builders, ensuring defaults honour ASN.1/ACN constraints when populating sequences, choices, or inserted fields.
- Naming watch: the many `createXYZInitFunc` helpers could share a common prefix (for example `build*`) to differentiate construction from the public `InitFunction.initTas` lambda naming.

### DastValidate2 (`BackendAst/DastValidate2.fs`)

- Purpose: constructs `IsValidFunction` structures that emit validation statements rooted in target-language macros.
- Key entry points: `createIntegerFunction` (`BackendAst/DastValidate2.fs:644`), `createSequenceFunction` (`BackendAst/DastValidate2.fs:791`), and `createChoiceFunction` (`BackendAst/DastValidate2.fs:863`) produce validators per type, while `ValidationCodeBlock_AND/OR` families compose constraint fragments.
- AST interaction: folds `Asn1AcnAst` constraints with `Asn1Fold`, emitting language-neutral code blocks before macros convert them into statements during template expansion.
- Naming watch: the `ValidationCodeBlock_*` functions could be grouped in a dedicated submodule to clarify they implement boolean algebra for constraints rather than final statement emission.

### DAstUPer (`BackendAst/DAstUPer.fs`)

- Purpose: generates uPER encode/decode lambdas, including pre/post conditions and error code wiring, for each ASN.1 type that requests uPER output.
- Key entry points: `createUperFunction` (`BackendAst/DAstUPer.fs:65`) centralises boilerplate, and type-specific helpers like `createIntegerFunction` (`BackendAst/DAstUPer.fs:220`), `createSequenceFunction` (`BackendAst/DAstUPer.fs:693`), and `createChoiceFunction` (`BackendAst/DAstUPer.fs:844`) tailor encoding strategies.
- AST interaction: inspects `IsValidFunction` presence to attach runtime checks, tracks `State.functionCalls` (via `addFunctionCallToState`) so `calculateFunctionToBeGenerated` knows encode/validate dependencies, and respects language macros for copy vs in-place decoding.
- Naming watch: the helper names `adaptArgument`, `adaptArgumentPtr`, and `adaptArgumentValue` could adopt verbs like `derive*` or `resolve*` to emphasise that they produce both expression strings and optional temporaries.

### DAstACN (`BackendAst/DAstACN.fs`)

- Purpose: emits ACN encode/decode lambdas, determinant helpers, and ICD metadata, honouring ACN-specific alignment, save-position, and dependency semantics.
- Key entry points: `createAcnIntegerFunction` (`BackendAst/DAstACN.fs:562`), `createEnumeratedFunction` (`BackendAst/DAstACN.fs:699`), `createSequenceFunction` (`BackendAst/DAstACN.fs:1788`), and `createChoiceFunction` (`BackendAst/DAstACN.fs:2285`) cover type varieties, while `handleSavePosition` (`BackendAst/DAstACN.fs:90`) and `handleAlignmentForAsn1Types` (`BackendAst/DAstACN.fs:115`) decorate emitted bodies.
- AST interaction: pulls determinant metadata from `Asn1AcnAst.AcnChild` entries, uses `AcnInsertedFieldDependencies` to propagate mapping-function requirements, and promises consistent variable naming back to `DAstConstruction.createAcnChild`.
- Naming watch: `FuncBody`/`FuncBodyStateless` could be renamed (`AcnEmitStateful`/`AcnEmitStateless`) to highlight the threaded `State`, and `deps` fields stored on `AcnChild` might read clearer as `insertedFieldDeps`.

### DAstXer (`BackendAst/DAstXer.fs`)

- Purpose: mirrors the uPER/ACN generators for XER, emitting XML encode/decode lambdas when the command line enables XER.
- Key entry points: `createIntegerFunction` (`BackendAst/DAstXer.fs:121`), `createSequenceFunction` (`BackendAst/DAstXer.fs:310`), and `createChoiceFunction` (`BackendAst/DAstXer.fs:367`) produce codec bodies, reusing validation builders where possible.
- AST interaction: delegates constraint checks to `DastValidate2.createIntegerFunctionByCons`, ensures deterministic element naming via language macros, and remains mostly side-effect-free unless XER is disabled.
- Naming watch: `XerFunction` vs `XerFunctionDummy` could be replaced with an `Option<XerFunctionRec>` to reduce pattern matching boilerplate downstream.

### DAstEqual and DAstEqualExp (`BackendAst/DAstEqual.fs`, `BackendAst/DAstEqualExp.fs`)

- Purpose: generate equality helpers used both by runtime APIs and encode/decode tests.
- Key entry points: `createEqualFunction_any` (`BackendAst/DAstEqual.fs:117`) consolidates emission; specialised builders such as `isEqualBodySequenceChild` (`BackendAst/DAstEqual.fs:43`) defer to child equality implementations; `DAstEqualExp` collects reusable expressions.
- AST interaction: reads child optionality, time classes, and mapping modules from `Asn1AcnAst`, then integrates with macros via `lm.equal.*`.
- Naming watch: consider renaming `isEqualBody*` helpers to `emitEquality*` to underline that they return code strings rather than booleans.

### DAstVariables (`BackendAst/DAstVariables.fs`)

- Purpose: render ASN.1 literal values into target-language initialisers used by value assignments, defaults, and generated tests.
- Key entry points: `printValue` (`BackendAst/DAstVariables.fs:31`), `printOctetStringValueAsCompoundLiteral` (`BackendAst/DAstVariables.fs:12`), and `convertStringValue2TargetLangStringLiteral` (`BackendAst/DAstVariables.fs:23`) cover the major literal families.
- AST interaction: requires the `Asn1Type.ActualType` to choose formatting strategy (e.g., enumerated names vs raw numbers) and respects constraints for fixed-size arrays.
- Naming watch: `printValue` could expose smaller helpers (e.g., `emitSequenceLiteral`) to minimise the sprawling match expression.

### DAstAsn1 (`BackendAst/DAstAsn1.fs`)

- Purpose: produce ASN.1 textual representations (constraints, values) for ICDs and diagnostics.
- Key entry points: `printAsn1Value` (`BackendAst/DAstAsn1.fs:16`), `foldGenericCon` (`BackendAst/DAstAsn1.fs:36`), and `createIntegerFunction` (`BackendAst/DAstAsn1.fs:110`) centralise the pretty printers.
- AST interaction: walks the same `Asn1AcnAst` structures as the generators but yields human-readable strings, bridging backend data to documentation templates.
- Naming watch: the module could expose a record of closures (e.g., `ConstraintPrinters`) to make the numerous `fold*` helpers easier to discover.

### EncodeDecodeTestCase (`BackendAst/EncodeDecodeTestCase.fs`)

- Purpose: fabricate encode/decode round-trip functions used by optional auto-generated tests.
- Key entry points: `_createUperEncDecFunction` (`BackendAst/EncodeDecodeTestCase.fs:49`), `_createAcnEncDecFunction` (`BackendAst/EncodeDecodeTestCase.fs:134`), and `_createXerEncDecFunction` (`BackendAst/EncodeDecodeTestCase.fs:223`) construct the bodies before the public wrappers gate them behind command-line switches.
- AST interaction: consumes the already-built `EqualFunction`, `IsValidFunction`, and codec lambdas to compose runtime tests without re-deriving constraints.
- Naming watch: the internal/external split (`_create*` vs `create*`) could collapse into a single function receiving a `GenerationFlags` record instead of boolean guards.

### DastTestCaseCreation (`BackendAst/DastTestCaseCreation.fs`)

- Purpose: emit per-type test case templates and optional runners when automatic tests are requested.
- Key entry points: `getTypeDecl` (`BackendAst/DastTestCaseCreation.fs:121`) resolves type names per language, while `printAllTestCasesAndTestCaseRunner` (`BackendAst/DastTestCaseCreation.fs:194`) writes the aggregated files.
- AST interaction: relies on `DAst.AutomaticTestCase` entries populated during construction and the call graph to know which tests to render.
- Naming watch: the module could clarify responsibilities by renaming to `TestCaseEmission` and pushing declaration helpers into a nested module.

### GenerateFiles (`BackendAst/GenerateFiles.fs`)

- Purpose: final emission stage that writes headers, sources, optional tests, and auxiliary files according to the active language macros.
- Key entry points: `printUnit` (`BackendAst/GenerateFiles.fs:73`) composes each program unit, `generateAll` (`BackendAst/GenerateFiles.fs:350`) drives emission across units, and `EmitDefaultACNGrammar` (`BackendAst/GenerateFiles.fs:363`) creates default ACN skeletons on request.
- AST interaction: filters function emission according to `AstRoot.callersSet`, requests code snippets via `LanguageMacros`, and consults `ProgramUnit` import lists for proper includes.
- Naming watch: `printUnit` primarily emits files; renaming to `emitProgramUnit` would make the side effect explicit.

### DAstProgramUnit (`BackendAst/DAstProgramUnit.fs`)

- Purpose: group enriched type assignments into language-specific compilation units, resolving import order and detecting cycles.
- Key entry points: `sortTypes` (`BackendAst/DAstProgramUnit.fs:64`) performs topological sorting, and `createProgramUnits` (`BackendAst/DAstProgramUnit.fs:105`) builds the final records depending on whether the target language supports modules.
- AST interaction: inspects `Asn1File` metadata, import statements, and ACN parameters to determine cross-unit dependencies before generation.
- Naming watch: `getTypeDependencies2` (`BackendAst/DAstProgramUnit.fs:42`) could move to a helper module and adopt a clearer name like `collectReferencedTypeAssignments`.

### DastFold (`BackendAst/DastFold.fs`)

- Purpose: backend-friendly fold helpers that mirror `Asn1Fold` but operate on the enriched `DAst.Asn1Type`.
- Key entry points: `foldAsn1Type` (`BackendAst/DastFold.fs:7`) is the main traversal used by modules like `DAstProgramUnit` and `GenerateAcnIcd`.
- AST interaction: allows consumers to traverse DAst while reusing user state and reassembling results, crucial for operations that need both original and derived type information.
- Naming watch: exposing higher-level wrappers (e.g., `mapChildren`) would reduce the repetitive lambdas found in callers.

### DAstBaseTypesEquivalence (`BackendAst/DAstBaseTypesEquivalence.fs`)

- Purpose: maintains per-language equivalence information between base types and generated type definitions, preventing redundant typedefs.
- Key entry points: helper functions populate the `BaseTypesEquivalence<'T>` record (`FrontEndAst/DAst.fs:282`) used inside `DAstConstruction`.
- AST interaction: cross-checks existing runtime library typedefs (e.g., C RTL) against generated ones so templates can reuse library definitions.
- Naming watch: consider moving the record-building helpers into `DAstConstruction` or renaming the module to `TypeDefinitionReuse` to clarify scope.

### DAstExportToXml (`BackendAst/DAstExportToXml.fs`)

- Purpose: emit XML diagnostics of the backend AST, aiding tooling and regression debugging.
- Key entry points: public functions export types and constraints for external inspection; they rely on traversals similar to `DastFold`.
- AST interaction: serialises the same `DAst.Asn1Type` that code generation consumes, ensuring debugging artifacts reflect the final state.
- Naming watch: function names like `PrintXxx` could be prefixed with `export` to make the side effect (file write) explicit.

### GenerateUperIcd (`BackendAst/GenerateUperIcd.fs`)

- Purpose: produce HTML ICD tables for uPER encodings, including colourised source snippets.
- Key entry points: helper factories such as `GetCommentLineFactory` and emission routines like `emitSequenceComponent` (`BackendAst/GenerateUperIcd.fs:158`) organise row-by-row ICD output.
- AST interaction: consumes `DAst.Asn1Type` metadata, size information, and saved ICD hashes to ensure deterministic documentation.
- Naming watch: grouping the many free functions into a record (`IcdPrinter`) would clarify which functions form the public surface.

### GenerateAcnIcd (`BackendAst/GenerateAcnIcd.fs`)

- Purpose: analogous to `GenerateUperIcd` but ACN-focused, including syntax highlighting and determinant explanations.
- Key entry points: `PrintAcnAsHTML` (`BackendAst/GenerateAcnIcd.fs:18`), `PrintAcnAsHTML2` (`BackendAst/GenerateAcnIcd.fs:41`), and `emitSequenceComponent` (`BackendAst/GenerateAcnIcd.fs:63`) drive the documentation pipeline.
- AST interaction: leverages token streams stored in `AstRoot.acnParseResults` and per-type ICD hashes, plus ACN-specific metadata from `DAstConstruction`.
- Naming watch: the duplicated logic with `GenerateUperIcd` suggests extracting a shared ICD writer module to avoid drift.

### GrammarGenerator, PrintAsn1, PrintAcn, CustomStgExport, CalculateIcdHash

- Purpose: supporting utilities—`GrammarGenerator` emits textual grammars, `PrintAsn1`/`PrintAcn` pretty-print types, `CustomStgExport` exposes template metadata, and `CalculateIcdHash` ensures ICD caching stays consistent.
- Key entry points: each module offers a small set of public functions that other backend modules call when exporting diagnostics or interfacing with STG.
- AST interaction: these modules consume `DAst` or `Asn1AcnAst` data but do not mutate global state, making them safe helpers across the pipeline.
- Naming watch: some modules (e.g., `CustomStgExport`) could document which consumers rely on them to avoid accidental removal during refactors.
