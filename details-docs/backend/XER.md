# XER Code Generation

## Overview
The XER backend mirrors the uPER/ACN pipeline by attaching encoder/decoder lambdas to each `DAst.Asn1Type` and letting `GenerateFiles` decide whether to emit concrete functions. XER-specific builders live in `BackendAst/DAstXer.fs` and reuse the shared `LanguageMacros` surface to format code the same way the other codecs do. The resulting `XerFunctionRec` records sit next to the uPER/ACN lambdas inside each type (`FrontEndAst/DAst.fs:490-512`) so later stages can render C/Ada/Scala artefacts alongside binary codecs.

## Core Entry Points
### `createXerFunction_any`
- Location: `BackendAst/DAstXer.fs:72`.
- Purpose: central constructor that turns a type-specific body generator (`ErrorCode -> CodegenScope -> XerTag option -> XERFuncBodyResult option`) into a `XerFunctionRec` plus the updated global `State`.
- Behaviour:
  - Derives a deterministic function name with `getFuncNameGeneric` and picks the correct parameter flavour via `lm.lg.getParamType` (`BackendAst/DAstXer.fs:69-80`).
  - Allocates a unique error code (`getNextValidErrorCode`, `FrontEndAst/DAst.fs:1119-1126`) and partially applies the body so callers can skip the error bookkeeping.
  - Invokes the supplied body once with a synthetic `xmlTag` parameter to pre-compute emitted statements, collect local variables, error codes, and aggregate the worst-case encoded size.
  - Uses XER macros (`lm.xer.EmitTypeAssignment*`) to build the public function and definition strings while uPER macros (`lm.uper.EmitTypeAssignment_def_err_code`) format error metadata.
  - Returns `(XerFunction ret, newState)` where `ret.funcBody` is the defaulted closure (used when child types embed this function) and `ret.funcBody_e` keeps the original arity for callers that need to inject a different error code.

### Type-specific creators
Each `create[ASN1Type]Function` wraps the common constructor to provide the actual XER statements. All builders share the signature `(AstRoot -> LanguageMacros -> Codec -> Asn1Type -> SpecificMeta -> TypeDefinitionOrReference -> IsValidFunction option -> State -> XerFunction * State)` and select encode vs decode forms using the `codec` argument.

- `createIntegerFunction` (`BackendAst/DAstXer.fs:121-148`) selects either `lm.xer.Integer` or `IntegerPos`, wraps constraint checks from `DastValidate2`, and computes tag/size defaults.
- `createBooleanFunction` (`BackendAst/DAstXer.fs:150-169`) emits `true`/`false` literals via `lm.xer.Boolean` and reserves `<false/>` as the size baseline.
- `createRealFunction` (`BackendAst/DAstXer.fs:171-182`) formats REAL values using a fixed maximum payload (`getMaxSizeInBytesForXER_Real = 50`).
- `createObjectIdentifierFunction` (`BackendAst/DAstXer.fs:184-196`) delegates to `lm.xer.ObjectIdentifier`, always treating the access path as a pointer because the runtime helpers expect contiguous bytes.
- `createTimeTypeFunction` (`BackendAst/DAstXer.fs:198-209`) threads the resolved time subtype (`DAstUPer.getTimeSubTypeByClass`) into `lm.xer.TimeType`.
- `createNullTypeFunction` (`BackendAst/DAstXer.fs:211-221`) produces self-closing NULL tags with zero content size.
- `createEnumeratedFunction` (`BackendAst/DAstXer.fs:223-237`) iterates over literals using `lm.xer.Enumerated_item`, capturing both the emitted statements and the maximum enumerant length to model buffer sizes.
- `createIA5StringFunction` (`BackendAst/DAstXer.fs:239-250`) and `createOctetStringFunction` (`BackendAst/DAstXer.fs:252-262`) convert max lengths into total XML size and honour numeric-string vs IA5 naming.
- `createBitStringFunction` (`BackendAst/DAstXer.fs:264-274`) mirrors octet strings but measures size in bits instead of bytes.
- `createSequenceOfFunction` (`BackendAst/DAstXer.fs:276-308`) creates an index variable (`SequenceOfIndex`) and multiplies the child’s encoded size by the container’s maximum length, inserting child tags unless the component comes from the runtime library.
- `createSequenceFunction` (`BackendAst/DAstXer.fs:310-365`) loops over ASN.1 children only, calling each child’s `XerFunctionRec.funcBody`, and wraps the nested statements with start/end tags via `lm.xer.SEQUENCE_start/end`.
- `createChoiceFunction` (`BackendAst/DAstXer.fs:365-404`) maps each alternative to `lm.xer.CHOICE_child` and optionally wraps them in parent tags if a literal tag was supplied.
- `createReferenceFunction` (`BackendAst/DAstXer.fs:406-447`) reuses the referenced type’s XER function when the resolved ASN.1 shapes are equivalent; otherwise it falls back to the base type’s already-built `XerFunction`.

These builders are invoked from `DAstConstruction` when populating `Asn1Type.xerEncFunction` / `.xerDecFunction`, after which `GenerateFiles` looks at `tas.Type.xerEncFunction.funcDef` / `.func` when emitting source headers (`BackendAst/GenerateFiles.fs:142-155`).

## Inputs and Outputs
- Common parameters:
  - `r : Asn1AcnAst.AstRoot` carries module-wide data and argument prefixes used for tag naming.
  - `lm : LanguageMacros` exposes language-specific helpers (`FrontEndAst/Language.fs:413-425`), notably `lg` for access-path rendering and `xer` for STG templates.
  - `codec : CommonTypes.Codec` selects encode (`Encode`) vs decode (`Decode`), which in turn picks by-value or by-pointer expressions (`lm.lg.getValue/getPointer`).
  - `t : Asn1AcnAst.Asn1Type` and optional `SpecificMeta` (e.g., `Asn1AcnAst.Integer`) provide AST metadata, optionality, and constraints.
  - `typeDefinition : TypeDefinitionOrReference` gives naming context (module + type) via `getAsn1Name` and `getLongTypedefName`.
  - `isValidFunc : IsValidFunction option` is available for embedding validation hooks, although the current `checkExp` helper is stubbed out.
  - `us : State` threads backend-global counters, returning the updated value together with the produced `XerFunction`.
- Outputs:
  - `XerFunction` wraps `XerFunctionRec`, storing both optional top-level function strings (`func`, `funcDef`) and a `funcBody` closure used when another type inlines this behaviour.
  - `XERFuncBodyResult` (returned by the closures) packages the rendered statements, dependent error codes, local variables, and a conservative encoded size so downstream tooling (e.g. automatic tests) can pre-allocate buffers.

## Examples
Example integer encoder selection (`BackendAst/DAstXer.fs:121-146`):
```fsharp
let funcBody (errCode:ErrorCode) (p:CodegenScope) (xmlTag:XerTag option) =
    let xmlTag = xmlTag |> orElse (XerLiteralConstant "INTEGER")
    let pp = match codec with Encode -> lm.lg.getValue p.accessPath | Decode -> lm.lg.getPointer p.accessPath
    let bodyStm =
        match o.isUnsigned with
        | true  -> IntegerPos pp xmlTag.p nLevel checkExp errCode.errCodeName codec
        | false -> Integer pp xmlTag.p nLevel checkExp errCode.errCodeName codec
    Some { XERFuncBodyResult.funcBody = bodyStm; errCodes = [errCode]; localVariables = []; encodingSizeInBytes = totalSize }
```
This renders either `<INTEGER>...</INTEGER>` or a pointer-based decode call depending on `codec`.

Sequence child handling (`BackendAst/DAstXer.fs:324-345`):
```fsharp
let chFunc =
    match child.Type.getXerFunction codec with
    | XerFunction z -> z
    | XerFunctionDummy  -> raise (BugErrorException "XerFunctionDummy")
let chP = { p with accessPath = lm.lg.getSeqChild p.accessPath (lm.lg.getAsn1ChildBackendName child) child.Type.isIA5String child.Optionality.IsSome }
match chFunc.funcBody chP (Some (XerLiteralConstant child.Name.Value)) with
| Some childContent -> sequence_optional_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody child.Name.Value codec, childContent.localVariables
| None -> None
```
The `AccessPath` machinery ensures deterministic member access, while the child’s XER function supplies the inner statements.

A corresponding XML fragment for an `INTEGER` field named `Temperature` would look like:
```xml
<Temperature>42</Temperature>
```
with the exact numeric formatting delegated to the language-specific STG macros behind `lm.xer.Integer`.

## Interactions with Shared Types
- `CodegenScope` (`FrontEndAst/DAst.fs:22-29`) supplies the current module and `AccessPath`, letting XER lambdas differentiate pointer vs value contexts through `lm.lg` helpers.
- `AccessPath` (`CommonTypes/CommonTypes.fs:21-116`) tracks the route to the current field. Helpers such as `AccessPath.joined` (`FrontEndAst/Language.fs:420-426`) convert it into stable lvalues referenced throughout the XER builders.
- `TypeDefinitionOrReference` (`FrontEndAst/DAst.fs:272-277`) gives each function access to original ASN.1 names (`getAsn1Name`) so tags and function identifiers stay aligned with the declarative type assignments.
- `ErrorCode` (`FrontEndAst/DAst.fs:279-283`) and `State` (`FrontEndAst/DAst.fs:112-264`) provide global error bookkeeping. `createXerFunction_any` reserves new error codes and returns the updated state for the caller to continue the chain.
- `XerFunctionRec` (`FrontEndAst/DAst.fs:501-508`) is stored on every `Asn1Type` alongside other codec functions, making XER generation a first-class citizen in the backend.
- `LanguageMacros` (`FrontEndAst/Language.fs:413-425`) dispatches to language-specific template groups (`IXer`), so the F# backend stays agnostic to concrete syntax while preserving consistent function signatures across C, Ada, and Scala.

## Naming & Determinism
- Function names follow `TypeName_XER_Encode/Decode`, produced by `getFuncNameGeneric` with the codec suffix (`BackendAst/DAstXer.fs:69-77`). When a language does not require a top-level function (`funcName = None`), the `XerFunctionRec` simply carries the inline closure.
- Error code identifiers adopt the pattern `ERR_XER[_ENCODE|_DECODE]_module-type`, normalised through `ToC` and guarded against collisions by `getNextValidErrorCode` (`BackendAst/DAstXer.fs:83-105`).
- Tag literals default either to the ASN.1 type name (`XerLiteralConstant`) or reuse a runtime-provided tag when the type lives in the RTL (`BackendAst/DAstXer.fs:290-300`). When a caller passes a `XerFunctionParameter`, the tag text is forwarded without alteration, keeping nested emissions deterministic.
- Access expressions use `AccessPath.joined` (`FrontEndAst/Language.fs:420-422`) so repeated invocations across encode/decode runs yield identical symbol names, which is critical for reproducible builds and hashing of generated artefacts.

## Limitations / TODO
- TODO `BackendAst/DAstXer.fs:150-154`: the `checkExp` helper currently discards the available `IsValidFunction` expression; re-enabling the commented logic would let XER reuse validation predicates.
- TODO `BackendAst/DAstXer.fs:184-193`: `createObjectIdentifierFunction` reuses the REAL size heuristic (`getMaxSizeInBytesForXER_Real`); confirm whether a dedicated OID estimator is needed.
- TODO `BackendAst/DAstXer.fs:406-447`: when `TypesEquivalence.uperEquivalence` returns `false`, the function simply falls back to `baseType.getXerFunction` without re-running `createXerFunction_any`. Verify that extended reference types still pick up fresh error codes.
