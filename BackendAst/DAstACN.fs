module DAstACN

open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open System.Globalization
open Language


// --- Re-exports from extracted modules (see BackendAst/Acn/) ---
// The helpers below were moved to BackendAst/Acn/Acn{Helpers,DeterminantDef,Alignment,Icd}.fs.
// They are re-exported here so external callers that reference them as
// `DAstACN.foo` keep working unchanged.
let foldMap = AcnHelpers.foldMap
let callBaseTypeFunc = AcnHelpers.callBaseTypeFunc
let sparkAnnotations = AcnHelpers.sparkAnnotations
let THREE_DOTS = AcnHelpers.THREE_DOTS
let getAcnDeterminantName = AcnHelpers.getAcnDeterminantName
let adaptArgument = AcnHelpers.adaptArgument
let adaptArgumentValue = AcnHelpers.adaptArgumentValue
let joinedOrAsIdentifier = AcnHelpers.joinedOrAsIdentifier

let getDeterminantTypeDefinitionBodyWithinSeq = AcnDeterminantDef.getDeterminantTypeDefinitionBodyWithinSeq
let getDeterminant_macro = AcnDeterminantDef.getDeterminant_macro
let getDeterminantTypeUpdateMacro = AcnDeterminantDef.getDeterminantTypeUpdateMacro
let getDeterminantTypeCheckEqual = AcnDeterminantDef.getDeterminantTypeCheckEqual

type FuncBody = AcnAlignment.FuncBody
type FuncBodyStateless = AcnAlignment.FuncBodyStateless
let handleSavePosition = AcnAlignment.handleSavePosition
let handleAlignmentForAsn1Types = AcnAlignment.handleAlignmentForAsn1Types
let handleAlignmentForAcnTypes = AcnAlignment.handleAlignmentForAcnTypes

let createIcdTas = AcnIcd.createIcdTas

// `createAcnFunction` is the generic dispatcher that wraps a per-type
// `funcBody` with alignment, save-position handling, error-code generation,
// ICD support, Spark annotations and test-case validation.  See
// BackendAst/Acn/AcnFunctionWrapper.fs for the implementation.
let createAcnFunction = AcnFunctionWrapper.createAcnFunction

// Primitive type encoders — moved to BackendAst/Acn/AcnPrimitives.fs.
// Re-exported here so external callers (DAstConstruction, DAstACNDeferred)
// keep working unchanged.
type AcnIntegerFuncBody = AcnPrimitives.AcnIntegerFuncBody
let createAcnIntegerFunctionInternal = AcnPrimitives.createAcnIntegerFunctionInternal
let getMappingFunctionModule = AcnPrimitives.getMappingFunctionModule
let createAcnIntegerFunction = AcnPrimitives.createAcnIntegerFunction
let createIntegerFunction = AcnPrimitives.createIntegerFunction
let createRealFunction = AcnPrimitives.createRealFunction
let createObjectIdentifierFunction = AcnPrimitives.createObjectIdentifierFunction
let createTimeTypeFunction = AcnPrimitives.createTimeTypeFunction
let nestChildItems = AcnPrimitives.nestChildItems
let createAcnBooleanFunction = AcnPrimitives.createAcnBooleanFunction
let createBooleanFunction = AcnPrimitives.createBooleanFunction
let createAcnNullTypeFunction = AcnPrimitives.createAcnNullTypeFunction
let createNullTypeFunction = AcnPrimitives.createNullTypeFunction

// ENUMERATED encoders — moved to BackendAst/Acn/AcnEnum.fs.
let enumComment = AcnEnum.enumComment
let createEnumCommon = AcnEnum.createEnumCommon
let createEnumeratedFunction = AcnEnum.createEnumeratedFunction
let createAcnEnumeratedFunction = AcnEnum.createAcnEnumeratedFunction

// IA5String / NumericString encoders — moved to BackendAst/Acn/AcnStrings.fs.
let createStringFunction = AcnStrings.createStringFunction
let createAcnStringFunction = AcnStrings.createAcnStringFunction

// OCTET STRING / BIT STRING encoders — moved to BackendAst/Acn/AcnOctetBitStrings.fs.
let createOctetStringFunction = AcnOctetBitStrings.createOctetStringFunction
let createBitStringFunction = AcnOctetBitStrings.createBitStringFunction

// SEQUENCE OF encoder — moved to BackendAst/Acn/AcnSequenceOf.fs.
let createSequenceOfFunction = AcnSequenceOf.createSequenceOfFunction

// Determinant update / dependency handling — moved to
// BackendAst/Acn/AcnDependencies.fs.  initExpr and resolveDepScope are
// also re-exported because they are referenced from AcnSequence (and the
// resolveDepScope helper is no longer private to a single file).
let initExpr = AcnDependencies.initExpr
let resolveDepScope = AcnDependencies.resolveDepScope
let handleSingleUpdateDependency = AcnDependencies.handleSingleUpdateDependency
let getUpdateFunctionUsedInEncoding = AcnDependencies.getUpdateFunctionUsedInEncoding

// SEQUENCE encoder — moved to BackendAst/Acn/AcnSequence.fs.
let createSequenceFunction_inline = AcnSequence.createSequenceFunction_inline

// CHOICE encoder — moved to BackendAst/Acn/AcnChoice.fs.
let createChoiceFunction = AcnChoice.createChoiceFunction

// Reference-type encoder — moved to BackendAst/Acn/AcnReference.fs.
let emptyIcdFnc = AcnReference.emptyIcdFnc
let createReferenceFunction_inline = AcnReference.createReferenceFunction_inline

// External-field / determinant lookup helpers — moved to BackendAst/Acn/AcnExternalField.fs.
let getExternalField0 = AcnExternalField.getExternalField0
let getExternalField0Type = AcnExternalField.getExternalField0Type
let getExternalFieldChoicePresentWhen = AcnExternalField.getExternalFieldChoicePresentWhen
let getExternalFieldTypeChoicePresentWhen = AcnExternalField.getExternalFieldTypeChoicePresentWhen
let getExternalField = AcnExternalField.getExternalField
let getExternalFieldType = AcnExternalField.getExternalFieldType









