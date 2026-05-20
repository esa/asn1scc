module AcnPrimitiveFactory

open System.Numerics

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language

open AcnHelpers


// Common factory for the four "primitive" ACN/ASN.1 type pairs whose outer
// scaffolding is identical: BOOLEAN, NULL, ENUMERATED, IA5String/NumericString.
// Each type comes in two flavours:
//   * createAcnXFunction       — used when the type is referenced standalone
//                                via an AcnRef (no Asn1Type, just a typeId).
//                                Allocates an error code from typeId.AcnAbsPath
//                                and returns (funcBody errCode, ns).
//   * createXFunction          — used when the type is part of a normal
//                                Asn1Type tree.  Wraps the inner funcBody in
//                                AcnFunctionWrapper.createAcnFunction.
//
// INTEGER is intentionally excluded — its encoding-class explosion is
// handled by the table-based approach in AcnPrimitives.

/// Inner per-call body shape (errCode already captured via closure).
type PrimitiveAcnInnerFn =
    (RelativePath * AcnParameter) list -> NestingScope -> CodegenScope -> AcnFuncBodyResult option

/// Outer shape of a primitive funcBody: takes the error code and returns the
/// per-call inner function.
type PrimitiveAcnFuncBody = ErrorCode -> PrimitiveAcnInnerFn


/// Build the standard "ERR_ACN<E|D>_<dotted-path>" error code from a typeId.
let primitiveErrCode (codec:CommonTypes.Codec) (typeId:ReferenceToType) (us:State) =
    let errCodeName = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." (first :: rest)).Replace("#","elm")
    getNextValidErrorCode us errCodeName None errFieldPath


/// Wrapper for ACN-only primitives (AcnBoolean / AcnNullType / AcnReferenceToEnumerated
/// / AcnReferenceToIA5String).  Allocates the error code from the typeId and
/// applies the supplied builder.
let createAcnOnlyPrimitive (codec:CommonTypes.Codec)
                           (typeId:ReferenceToType)
                           (us:State)
                           (mkBody: ErrorCode -> PrimitiveAcnInnerFn) =
    let errCode, ns = primitiveErrCode codec typeId us
    (mkBody errCode), ns


/// Wrapper for ASN.1-side primitives (Boolean / NullType / Enumerated).
/// Builds the standard SPARK annotations and delegates to
/// AcnFunctionWrapper.createAcnFunction with the supplied funcBody.
let createAsn1Primitive (r:Asn1AcnAst.AstRoot)
                        (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
                        (lm:LanguageMacros)
                        (codec:CommonTypes.Codec)
                        (t:Asn1AcnAst.Asn1Type)
                        (typeDefinition:TypeDefinitionOrReference)
                        (isValidFunc:IsValidFunction option)
                        (funcDefAnnots:string list)
                        (us:State)
                        (funcBody:PrimitiveAcnFuncBody) =
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc
        (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us)
        (fun atc -> true) soSparkAnnotations funcDefAnnots us


/// State-threading variant of createAsn1Primitive for primitives whose inner
/// funcBody threads State (e.g. StringType, which calls uperFunc.funcBody_e
/// internally).
let createAsn1PrimitiveStateful (r:Asn1AcnAst.AstRoot)
                                (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
                                (lm:LanguageMacros)
                                (codec:CommonTypes.Codec)
                                (t:Asn1AcnAst.Asn1Type)
                                (typeDefinition:TypeDefinitionOrReference)
                                (isValidFunc:IsValidFunction option)
                                (funcDefAnnots:string list)
                                (us:State)
                                (funcBody:AcnAlignment.FuncBody) =
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc
        funcBody (fun atc -> true) soSparkAnnotations funcDefAnnots us


/// Build the standard 1-row IcdArgAux for a primitive type.  The default
/// rowsFunc emits a single FieldRow with the supplied type/constraint/units.
let buildPrimitiveIcdAux (icdType:string)
                         (icdName:string)
                         (sConstraint:string option)
                         (minBits:BigInteger)
                         (maxBits:BigInteger)
                         (units:string option) =
    let icdFnc fieldName sPresent comments =
        [{IcdRow.fieldName = fieldName; comments = comments; sPresent = sPresent;
          sType = IcdPlainType icdType; sConstraint = sConstraint;
          minLengthInBits = minBits; maxLengthInBits = maxBits; sUnits = units;
          rowType = IcdRowType.FieldRow; idxOffset = None}], []
    {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = icdName; rowsFunc = icdFnc;
     commentsForTas = []; scope = "type"; name = None}
