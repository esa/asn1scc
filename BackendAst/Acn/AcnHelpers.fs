module AcnHelpers

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language


let foldMap = Asn1Fold.foldMap

let callBaseTypeFunc (lm:LanguageMacros) = lm.uper.call_base_type_func

let sparkAnnotations (lm:LanguageMacros)  = lm.acn.sparkAnnotations

let THREE_DOTS = {IcdRow.fieldName = ""; comments = []; sPresent="";sType= IcdPlainType ""; sConstraint=None; minLengthInBits = 0I; maxLengthInBits=0I;sUnits=None; rowType = IcdRowType.ThreeDOTs; idxOffset = None}

let getAcnDeterminantName = AcnCreateFromAntlr.getAcnDeterminantName

let adaptArgument = DAstUPer.adaptArgument
let adaptArgumentValue = DAstUPer.adaptArgumentValue
let joinedOrAsIdentifier = DAstUPer.joinedOrAsIdentifier
