module init_rust
open System
open System.Numerics
open CommonTypes

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "rtlModuleName" []

let methodNameSuffix () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "methodNameSuffix" []

let initTypeAssignment_def (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTypeAssignment_def" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]

let initTypeAssignment (sVarName:string) (sPtrPrefix:string) (sPtrSuffix:string) (sFuncName:string) (sTypeDefName:string) (sContent:string) (arrsLocalVariables:seq<string>) (sDefaultInitValue:string) (arrsAnnots:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTypeAssignment" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sPtrPrefix",(if sPtrPrefix = null then null else ST.StrHelper sPtrPrefix:>Object) );("sPtrSuffix",(if sPtrSuffix = null then null else ST.StrHelper sPtrSuffix:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("arrsLocalVariables",(arrsLocalVariables|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sDefaultInitValue",(if sDefaultInitValue = null then null else ST.StrHelper sDefaultInitValue:>Object) );("arrsAnnots",(arrsAnnots|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let initInteger (sVal:string) (sValue:string) (bIsOptional:bool) (sResVar:string) (sType:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initInteger" [("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("bIsOptional",bIsOptional :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let initReal (sVal:string) (dValue:double) (bIsOptional:bool) (sResVar:string) (sType:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initReal" [("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("dValue",dValue :>Object);("bIsOptional",bIsOptional :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let initBoolean (sVal:string) (bValue:bool) (bIsOptional:bool) (sResVar:string) (sType:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initBoolean" [("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("bValue",bValue :>Object);("bIsOptional",bIsOptional :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let initObjectIdentifier_valid (p:string) (sAcc:string) (sI:string) (nIntVal:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initObjectIdentifier_valid" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sI",(if sI = null then null else ST.StrHelper sI:>Object) );("nIntVal",nIntVal :>Object)]

let initObjectIdentifier (p:string) (sAcc:string) (nSize:BigInteger) (arrsValues:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initObjectIdentifier" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nSize",nSize :>Object);("arrsValues",(arrsValues|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let init_Asn1LocalTime (p:string) (sAcc:string) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1LocalTime" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("tv",tv :>Object)]

let init_Asn1UtcTime (p:string) (sAcc:string) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1UtcTime" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("tv",tv :>Object)]

let init_Asn1LocalTimeWithTimeZone (p:string) (sAcc:string) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1LocalTimeWithTimeZone" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("tv",tv :>Object);("tz",tz :>Object)]

let init_Asn1Date (p:string) (sAcc:string) (dt:Asn1DateValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1Date" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("dt",dt :>Object)]

let init_Asn1Date_LocalTime (p:string) (sAcc:string) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1Date_LocalTime" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("dt",dt :>Object);("tv",tv :>Object)]

let init_Asn1Date_UtcTime (p:string) (sAcc:string) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1Date_UtcTime" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("dt",dt :>Object);("tv",tv :>Object)]

let init_Asn1Date_LocalTimeWithTimeZone (p:string) (sAcc:string) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1Date_LocalTimeWithTimeZone" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("dt",dt :>Object);("tv",tv :>Object);("tz",tz :>Object)]

let assignAny (p:string) (sValue:string) (sTypeDecl:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "assignAny" [("p",p :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sTypeDecl",(if sTypeDecl = null then null else ST.StrHelper sTypeDecl:>Object) )]

let assignString (p:string) (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "assignString" [("p",p :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let initIA5String (sPtr:string) (sValue:string) (bIsOptional:bool) (sResVar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initIA5String" [("sPtr",(if sPtr = null then null else ST.StrHelper sPtr:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("bIsOptional",bIsOptional :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) )]

let initEnumerated (sVal:string) (sValue:string) (sTypeDefName:string) (bIsOptional:bool) (sResVar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initEnumerated" [("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("bIsOptional",bIsOptional :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) )]

let initNull (sVal:string) (bIsOptional:bool) (sResVar:string) (sType:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initNull" [("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("bIsOptional",bIsOptional :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let initTestCaseIA5String (p:string) (sAcc:string) (nSize:BigInteger) (nMaxSizePlusOne:BigInteger) (i:string) (td:FE_StringTypeDefinition) (bAlpha:bool) (arrnAlphabetAsciiCodes:seq<BigInteger>) (nAlphabetLength:BigInteger) (bZero:bool) (sResVar:string) (sInitChar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTestCaseIA5String" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nSize",nSize :>Object);("nMaxSizePlusOne",nMaxSizePlusOne :>Object);("i",i :>Object);("td",td :>Object);("bAlpha",bAlpha :>Object);("arrnAlphabetAsciiCodes",arrnAlphabetAsciiCodes|>Seq.toArray :>Object);("nAlphabetLength",nAlphabetLength :>Object);("bZero",bZero :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) );("sInitChar",(if sInitChar = null then null else ST.StrHelper sInitChar:>Object) )]

let initBitOrOctStringFromCompoundLiteral (p:string) (sCompLiteral:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initBitOrOctStringFromCompoundLiteral" [("p",p :>Object);("sCompLiteral",(if sCompLiteral = null then null else ST.StrHelper sCompLiteral:>Object) )]

let initFixSizeBitOrOctString_bytei (p:string) (sAcc:string) (sI:string) (sByteHexVal:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initFixSizeBitOrOctString_bytei" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sI",(if sI = null then null else ST.StrHelper sI:>Object) );("sByteHexVal",(if sByteHexVal = null then null else ST.StrHelper sByteHexVal:>Object) )]

let initFixSizeBitOrOctString (p:string) (sAcc:string) (arrsBytes:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initFixSizeBitOrOctString" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsBytes",(arrsBytes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let initFixVarSizeBitOrOctString (p:string) (sAcc:string) (nSize:BigInteger) (arrsBytes:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initFixVarSizeBitOrOctString" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nSize",nSize :>Object);("arrsBytes",(arrsBytes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let initTestCaseOctetString (p:string) (sAcc:string) (sArrayHolderName:string) (nSize:BigInteger) (i:string) (bIsFixedSize:bool) (bZero:bool) (nMinSize:BigInteger) (bZeroSizedArray:bool) (sResVar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTestCaseOctetString" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sArrayHolderName",(if sArrayHolderName = null then null else ST.StrHelper sArrayHolderName:>Object) );("nSize",nSize :>Object);("i",i :>Object);("bIsFixedSize",bIsFixedSize :>Object);("bZero",bZero :>Object);("nMinSize",nMinSize :>Object);("bZeroSizedArray",bZeroSizedArray :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) )]

let initTestCaseBitString (p:string) (sAcc:string) (sArrayHolderName:string) (nSize:BigInteger) (nSizeCeiled:BigInteger) (i:string) (bIsFixedSize:bool) (bZero:bool) (nMinSize:BigInteger) (bIsOptionalField:bool) (sResVar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTestCaseBitString" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sArrayHolderName",(if sArrayHolderName = null then null else ST.StrHelper sArrayHolderName:>Object) );("nSize",nSize :>Object);("nSizeCeiled",nSizeCeiled :>Object);("i",i :>Object);("bIsFixedSize",bIsFixedSize :>Object);("bZero",bZero :>Object);("nMinSize",nMinSize :>Object);("bIsOptionalField",bIsOptionalField :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) )]

let initSequence_pragma (p:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initSequence_pragma" [("p",p :>Object)]

let initFixedSequenceOf (arrsInnerValues:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initFixedSequenceOf" [("arrsInnerValues",(arrsInnerValues|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let initVarSizeSequenceOf (p:string) (sAcc:string) (nSize:BigInteger) (arrsInnerValues:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initVarSizeSequenceOf" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nSize",nSize :>Object);("arrsInnerValues",(arrsInnerValues|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let initTestCaseSizeSequenceOf_innerItem (bFirst:bool) (bLastItem:bool) (nCaseIdx:BigInteger) (sChildCaseInit:string) (i:string) (nCaseLen:BigInteger) (sResVar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTestCaseSizeSequenceOf_innerItem" [("bFirst",bFirst :>Object);("bLastItem",bLastItem :>Object);("nCaseIdx",nCaseIdx :>Object);("sChildCaseInit",(if sChildCaseInit = null then null else ST.StrHelper sChildCaseInit:>Object) );("i",i :>Object);("nCaseLen",nCaseLen :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) )]

let initTestCaseSizeSequenceOf (p:string) (sAcc:string) (sArrayHolderName:string) (noMinSize:BigInteger option) (nSize:BigInteger) (bIsFixedSize:bool) (arrsInnerItems:seq<string>) (bMultiCases:bool) (i:string) (sResVar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTestCaseSizeSequenceOf" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sArrayHolderName",(if sArrayHolderName = null then null else ST.StrHelper sArrayHolderName:>Object) );("noMinSize",(if noMinSize.IsNone then null else noMinSize.Value:>Object) );("nSize",nSize :>Object);("bIsFixedSize",bIsFixedSize :>Object);("arrsInnerItems",(arrsInnerItems|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bMultiCases",bMultiCases :>Object);("i",i :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) )]

let initSequence_optionalChild (p:string) (sAcc:string) (sChName:string) (sPresentFlag:string) (sChildContent:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initSequence_optionalChild" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sPresentFlag",(if sPresentFlag = null then null else ST.StrHelper sPresentFlag:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) )]

let initSequence (arrsInnerValues:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initSequence" [("arrsInnerValues",(arrsInnerValues|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let initSequence_emptySeq (p:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initSequence_emptySeq" [("p",p :>Object)]

let initTestCase_sequence_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (bOptional:bool) (sInitExpr:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTestCase_sequence_child" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("bOptional",bOptional :>Object);("sInitExpr",(if sInitExpr = null then null else ST.StrHelper sInitExpr:>Object) )]

let initTestCase_sequence_child_opt (p:string) (sAcc:string) (sChName:string) (sChildTypedef:string) (sResVar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTestCase_sequence_child_opt" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) )]

let initChoice (p:string) (sAcc:string) (sChildContent:string) (sChildID:string) (sChildName:string) (sChildTypeName:string) (sChoiceTypeName:string) (sChildTempVarName:string) (sChildTempDefaultInit:string) (bComponentTempInit:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initChoice" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeName",(if sChildTypeName = null then null else ST.StrHelper sChildTypeName:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildTempVarName",(if sChildTempVarName = null then null else ST.StrHelper sChildTempVarName:>Object) );("sChildTempDefaultInit",(if sChildTempDefaultInit = null then null else ST.StrHelper sChildTempDefaultInit:>Object) );("bComponentTempInit",bComponentTempInit :>Object)]

let initTestCase_choice_child (p:string) (sAcc:string) (sChildContent:string) (sChildID:string) (sChildName:string) (sChildTypeName:string) (sChoiceTypeName:string) (sChildTempVarName:string) (sChildTempDefaultInit:string) (bIsOptional:bool) (sResVar:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTestCase_choice_child" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeName",(if sChildTypeName = null then null else ST.StrHelper sChildTypeName:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildTempVarName",(if sChildTempVarName = null then null else ST.StrHelper sChildTempVarName:>Object) );("sChildTempDefaultInit",(if sChildTempDefaultInit = null then null else ST.StrHelper sChildTempDefaultInit:>Object) );("bIsOptional",bIsOptional :>Object);("sResVar",(if sResVar = null then null else ST.StrHelper sResVar:>Object) )]

let initChildWithInitFunc (p:string) (sChildInitFuncName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initChildWithInitFunc" [("p",p :>Object);("sChildInitFuncName",(if sChildInitFuncName = null then null else ST.StrHelper sChildInitFuncName:>Object) )]

let initBitStringAtPos (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (sNamedBit:string) (nZeroBasedByteIndex:BigInteger) (sHexByteMax:string) (nZeroBasedBitIndex:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initBitStringAtPos" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("sNamedBit",(if sNamedBit = null then null else ST.StrHelper sNamedBit:>Object) );("nZeroBasedByteIndex",nZeroBasedByteIndex :>Object);("sHexByteMax",(if sHexByteMax = null then null else ST.StrHelper sHexByteMax:>Object) );("nZeroBasedBitIndex",nZeroBasedBitIndex :>Object)]

let initBitStringAtPos_def (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (sNamedBit:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initBitStringAtPos_def" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("sNamedBit",(if sNamedBit = null then null else ST.StrHelper sNamedBit:>Object) )]

let initTypeConstant_def (sTypeDecl:string) (sConstantName:string) (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTypeConstant_def" [("sTypeDecl",(if sTypeDecl = null then null else ST.StrHelper sTypeDecl:>Object) );("sConstantName",(if sConstantName = null then null else ST.StrHelper sConstantName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let initTypeConstant_body (sTypeDecl:string) (sConstantName:string) (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initTypeConstant_body" [("sTypeDecl",(if sTypeDecl = null then null else ST.StrHelper sTypeDecl:>Object) );("sConstantName",(if sConstantName = null then null else ST.StrHelper sConstantName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let initFixSizeOctetString (sTypeDefName:string) (nMax:BigInteger) (bZeroSizedArray:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initFixSizeOctetString" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("nMax",nMax :>Object);("bZeroSizedArray",bZeroSizedArray :>Object)]

let initVarSizeOctetString (sTypeDefName:string) (nMin:BigInteger) (nMax:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initVarSizeOctetString" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("nMin",nMin :>Object);("nMax",nMax :>Object)]

let initFixSizeBitString (sTypeDefName:string) (nMax:BigInteger) (nMaxOctets:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initFixSizeBitString" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("nMax",nMax :>Object);("nMaxOctets",nMaxOctets :>Object)]

let initVarSizeBitString (sTypeDefName:string) (nMin:BigInteger) (nMax:BigInteger) (nMaxOctets:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initVarSizeBitString" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("nMin",nMin :>Object);("nMax",nMax :>Object);("nMaxOctets",nMaxOctets :>Object)]

let initFixSizeSequenceOfExpr (sTypeDefName:string) (nMax:BigInteger) (sChildExp:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initFixSizeSequenceOfExpr" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("nMax",nMax :>Object);("sChildExp",(if sChildExp = null then null else ST.StrHelper sChildExp:>Object) )]

let initVarSizeSequenceOfExpr (sTypeDefName:string) (nMin:BigInteger) (nMax:BigInteger) (sChildExp:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initVarSizeSequenceOfExpr" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("nMin",nMin :>Object);("nMax",nMax :>Object);("sChildExp",(if sChildExp = null then null else ST.StrHelper sChildExp:>Object) )]

let initObjectIdentifierAsExpr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initObjectIdentifierAsExpr" []

let init_Asn1LocalTimeExpr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1LocalTimeExpr" []

let init_Asn1UtcTimeExpr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1UtcTimeExpr" []

let init_Asn1LocalTimeWithTimeZoneExpr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1LocalTimeWithTimeZoneExpr" []

let init_Asn1DateExpr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1DateExpr" []

let init_Asn1Date_LocalTimeExpr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1Date_LocalTimeExpr" []

let init_Asn1Date_UtcTimeExpr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1Date_UtcTimeExpr" []

let init_Asn1Date_LocalTimeWithTimeZoneExpr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "init_Asn1Date_LocalTimeWithTimeZoneExpr" []

let initSequenceChildExpr (sChildName:string) (sChildExpr:string) (bIsOptional:bool) (bIsAbsent:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initSequenceChildExpr" [("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildExpr",(if sChildExpr = null then null else ST.StrHelper sChildExpr:>Object) );("bIsOptional",bIsOptional :>Object);("bIsAbsent",bIsAbsent :>Object)]

let initSequenceOptionalChildExpr (sChildName:string) (nPresenceBit:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initSequenceOptionalChildExpr" [("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("nPresenceBit",nPresenceBit :>Object)]

let initSequenceExpr (sTypeDefName:string) (arrsChildren:seq<string>) (arrsOptionalChildren:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initSequenceExpr" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsOptionalChildren",(arrsOptionalChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let initChoiceExpr (sChildName:string) (sChildKind:string) (sChildExpr:string) (sChoiceTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "init_rust" "initChoiceExpr" [("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildKind",(if sChildKind = null then null else ST.StrHelper sChildKind:>Object) );("sChildExpr",(if sChildExpr = null then null else ST.StrHelper sChildExpr:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]

