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

let md5 = AcnIcd.md5
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

(*
If the type assignment has acnParameters, then no function is generated. This function can only be inlined by the calling function
(i.e. by the parent type encoding function).
Now, we have to make this rule recursive: 
A composite type (e.g SEQUENCE, choice etc ) may have references (i.e. reference types) to a type assignment that has acnParameters.
In this case, the reference must have arguments in the acn in the form <arg1,arg2, ...>
These argument can either ACN inserted fields or acnParameters.
If the reference type is written explicitly in the acn, by the user, then the arguments must be checked to be inline with the acnParameters.
If they are not, the user gets an error.

However, there are cases where the reference type is not written explicitly by the user in the acn grammar, 
but is infered by the compiler. For example, 

The following asn1 grammar define two types:
CfdpPDU ::= SEQUENCE {
   pdu-header PDUHeader,
   payload OCTET STRING (CONTAINING PayloadData)
}
PayloadData ::= CHOICE {
   file-directive FileDirectiveType,
   file-data FileDataType
}
FileDataType ::= SEQUENCE {
   file-data-pdu FileDataPDU
}

However the acn grammar provides defintions only for CfdpPDU and FileDataType, not PayloadData. In fact, the PayloadData acn spec
is provided inline in the CfdpPDU acn spec, not at the PayloadData Type Assignment Level. 
CfdpPDU [] {
   pdu-header                                [] {
      pdu-type                               PDUType [encoding pos-int, size 1],
      pdu-data-field-length                  PDUDataFieldLength [encoding pos-int, size 16]
   },
   payload                                   [size pdu-header.pdu-data-field-length] {
      file-directive                         [present-when pdu-header.pdu-type==0],
      file-data                              <pdu-header.pdu-data-field-length> [present-when pdu-header.pdu-type==1]
   }
}
FileDataType <PDUDataFieldLength:pdu-data-field-length> [] {
   file-data-pdu                             <pdu-data-field-length> []
}

Therefore, the compiler uses a defult acn specs for the PayloadData type assignment, which is not provided by the user.
In this case the file-data reference type has no acnArgs. This means that no acn function must be generated for the FileDataType type assignment.

*)




// External-field / determinant lookup helpers — moved to BackendAst/Acn/AcnExternalField.fs.
let getExternalField0 = AcnExternalField.getExternalField0
let getExternalField0Type = AcnExternalField.getExternalField0Type
let getExternalFieldChoicePresentWhen = AcnExternalField.getExternalFieldChoicePresentWhen
let getExternalFieldTypeChoicePresentWhen = AcnExternalField.getExternalFieldTypeChoicePresentWhen
let getExternalField = AcnExternalField.getExternalField
let getExternalFieldType = AcnExternalField.getExternalFieldType









