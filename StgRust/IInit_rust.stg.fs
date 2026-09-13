module IInit_rust
open System
open System.Numerics
open CommonTypes

type IInit_rust() =
    inherit AbstractMacros.IInit()
        override this.rtlModuleName  () =
            init_rust.rtlModuleName  () 
        override this.methodNameSuffix  () =
            init_rust.methodNameSuffix  () 
        override this.initTypeAssignment_def  (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) =
            init_rust.initTypeAssignment_def  sVarName sStar sFuncName sTypeDefName 
        override this.initTypeAssignment  (sVarName:string) (sPtrPrefix:string) (sPtrSuffix:string) (sFuncName:string) (sTypeDefName:string) (sContent:string) (arrsLocalVariables:seq<string>) (sDefaultInitValue:string) (arrsAnnots:seq<string>) =
            init_rust.initTypeAssignment  sVarName sPtrPrefix sPtrSuffix sFuncName sTypeDefName sContent arrsLocalVariables sDefaultInitValue arrsAnnots 
        override this.initInteger  (sVal:string) (sValue:string) (bIsOptional:bool) (sResVar:string) (sType:string) =
            init_rust.initInteger  sVal sValue bIsOptional sResVar sType 
        override this.initReal  (sVal:string) (dValue:double) (bIsOptional:bool) (sResVar:string) (sType:string) =
            init_rust.initReal  sVal dValue bIsOptional sResVar sType 
        override this.initBoolean  (sVal:string) (bValue:bool) (bIsOptional:bool) (sResVar:string) (sType:string) =
            init_rust.initBoolean  sVal bValue bIsOptional sResVar sType 
        override this.initObjectIdentifier_valid  (p:string) (sAcc:string) (sI:string) (nIntVal:BigInteger) =
            init_rust.initObjectIdentifier_valid  p sAcc sI nIntVal 
        override this.initObjectIdentifier  (p:string) (sAcc:string) (nSize:BigInteger) (arrsValues:seq<string>) =
            init_rust.initObjectIdentifier  p sAcc nSize arrsValues 
        override this.init_Asn1LocalTime  (p:string) (sAcc:string) (tv:Asn1TimeValue) =
            init_rust.init_Asn1LocalTime  p sAcc tv 
        override this.init_Asn1UtcTime  (p:string) (sAcc:string) (tv:Asn1TimeValue) =
            init_rust.init_Asn1UtcTime  p sAcc tv 
        override this.init_Asn1LocalTimeWithTimeZone  (p:string) (sAcc:string) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            init_rust.init_Asn1LocalTimeWithTimeZone  p sAcc tv tz 
        override this.init_Asn1Date  (p:string) (sAcc:string) (dt:Asn1DateValue) =
            init_rust.init_Asn1Date  p sAcc dt 
        override this.init_Asn1Date_LocalTime  (p:string) (sAcc:string) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            init_rust.init_Asn1Date_LocalTime  p sAcc dt tv 
        override this.init_Asn1Date_UtcTime  (p:string) (sAcc:string) (dt:Asn1DateValue) (tv:Asn1TimeValue) =
            init_rust.init_Asn1Date_UtcTime  p sAcc dt tv 
        override this.init_Asn1Date_LocalTimeWithTimeZone  (p:string) (sAcc:string) (dt:Asn1DateValue) (tv:Asn1TimeValue) (tz:Asn1TimeZoneValue) =
            init_rust.init_Asn1Date_LocalTimeWithTimeZone  p sAcc dt tv tz 
        override this.assignAny  (p:string) (sValue:string) (sTypeDecl:string) =
            init_rust.assignAny  p sValue sTypeDecl 
        override this.assignString  (p:string) (sValue:string) =
            init_rust.assignString  p sValue 
        override this.initIA5String  (sPtr:string) (sValue:string) (bIsOptional:bool) (sResVar:string) =
            init_rust.initIA5String  sPtr sValue bIsOptional sResVar 
        override this.initEnumerated  (sVal:string) (sValue:string) (sTypeDefName:string) (bIsOptional:bool) (sResVar:string) =
            init_rust.initEnumerated  sVal sValue sTypeDefName bIsOptional sResVar 
        override this.initNull  (sVal:string) (bIsOptional:bool) (sResVar:string) (sType:string) =
            init_rust.initNull  sVal bIsOptional sResVar sType 
        override this.initTestCaseIA5String  (p:string) (sAcc:string) (nSize:BigInteger) (nMaxSizePlusOne:BigInteger) (i:string) (td:FE_StringTypeDefinition) (bAlpha:bool) (arrnAlphabetAsciiCodes:seq<BigInteger>) (nAlphabetLength:BigInteger) (bZero:bool) (sResVar:string) (sInitChar:string) =
            init_rust.initTestCaseIA5String  p sAcc nSize nMaxSizePlusOne i td bAlpha arrnAlphabetAsciiCodes nAlphabetLength bZero sResVar sInitChar 
        override this.initBitOrOctStringFromCompoundLiteral  (p:string) (sCompLiteral:string) =
            init_rust.initBitOrOctStringFromCompoundLiteral  p sCompLiteral 
        override this.initFixSizeBitOrOctString_bytei  (p:string) (sAcc:string) (sI:string) (sByteHexVal:string) =
            init_rust.initFixSizeBitOrOctString_bytei  p sAcc sI sByteHexVal 
        override this.initFixSizeBitOrOctString  (p:string) (sAcc:string) (arrsBytes:seq<string>) =
            init_rust.initFixSizeBitOrOctString  p sAcc arrsBytes 
        override this.initFixVarSizeBitOrOctString  (p:string) (sAcc:string) (nSize:BigInteger) (arrsBytes:seq<string>) =
            init_rust.initFixVarSizeBitOrOctString  p sAcc nSize arrsBytes 
        override this.initTestCaseOctetString  (p:string) (sAcc:string) (sArrayHolderName:string) (nSize:BigInteger) (i:string) (bIsFixedSize:bool) (bZero:bool) (nMinSize:BigInteger) (bZeroSizedArray:bool) (sResVar:string) =
            init_rust.initTestCaseOctetString  p sAcc sArrayHolderName nSize i bIsFixedSize bZero nMinSize bZeroSizedArray sResVar 
        override this.initTestCaseBitString  (p:string) (sAcc:string) (sArrayHolderName:string) (nSize:BigInteger) (nSizeCeiled:BigInteger) (i:string) (bIsFixedSize:bool) (bZero:bool) (nMinSize:BigInteger) (bIsOptionalField:bool) (sResVar:string) =
            init_rust.initTestCaseBitString  p sAcc sArrayHolderName nSize nSizeCeiled i bIsFixedSize bZero nMinSize bIsOptionalField sResVar 
        override this.initSequence_pragma  (p:string) =
            init_rust.initSequence_pragma  p 
        override this.initFixedSequenceOf  (arrsInnerValues:seq<string>) =
            init_rust.initFixedSequenceOf  arrsInnerValues 
        override this.initVarSizeSequenceOf  (p:string) (sAcc:string) (nSize:BigInteger) (arrsInnerValues:seq<string>) =
            init_rust.initVarSizeSequenceOf  p sAcc nSize arrsInnerValues 
        override this.initTestCaseSizeSequenceOf_innerItem  (bFirst:bool) (bLastItem:bool) (nCaseIdx:BigInteger) (sChildCaseInit:string) (i:string) (nCaseLen:BigInteger) (sResVar:string) =
            init_rust.initTestCaseSizeSequenceOf_innerItem  bFirst bLastItem nCaseIdx sChildCaseInit i nCaseLen sResVar 
        override this.initTestCaseSizeSequenceOf  (p:string) (sAcc:string) (sArrayHolderName:string) (noMinSize:BigInteger option) (nSize:BigInteger) (bIsFixedSize:bool) (arrsInnerItems:seq<string>) (bMultiCases:bool) (i:string) (sResVar:string) =
            init_rust.initTestCaseSizeSequenceOf  p sAcc sArrayHolderName noMinSize nSize bIsFixedSize arrsInnerItems bMultiCases i sResVar 
        override this.initSequence_optionalChild  (p:string) (sAcc:string) (sChName:string) (sPresentFlag:string) (sChildContent:string) =
            init_rust.initSequence_optionalChild  p sAcc sChName sPresentFlag sChildContent 
        override this.initSequence  (arrsInnerValues:seq<string>) =
            init_rust.initSequence  arrsInnerValues 
        override this.initSequence_emptySeq  (p:string) =
            init_rust.initSequence_emptySeq  p 
        override this.initTestCase_sequence_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (bOptional:bool) (sInitExpr:string) =
            init_rust.initTestCase_sequence_child  p sAcc sChName sChildContent bOptional sInitExpr 
        override this.initTestCase_sequence_child_opt  (p:string) (sAcc:string) (sChName:string) (sChildTypedef:string) (sResVar:string) =
            init_rust.initTestCase_sequence_child_opt  p sAcc sChName sChildTypedef sResVar 
        override this.initChoice  (p:string) (sAcc:string) (sChildContent:string) (sChildID:string) (sChildName:string) (sChildTypeName:string) (sChoiceTypeName:string) (sChildTempVarName:string) (sChildTempDefaultInit:string) (bComponentTempInit:bool) =
            init_rust.initChoice  p sAcc sChildContent sChildID sChildName sChildTypeName sChoiceTypeName sChildTempVarName sChildTempDefaultInit bComponentTempInit 
        override this.initTestCase_choice_child  (p:string) (sAcc:string) (sChildContent:string) (sChildID:string) (sChildName:string) (sChildTypeName:string) (sChoiceTypeName:string) (sChildTempVarName:string) (sChildTempDefaultInit:string) (bIsOptional:bool) (sResVar:string) =
            init_rust.initTestCase_choice_child  p sAcc sChildContent sChildID sChildName sChildTypeName sChoiceTypeName sChildTempVarName sChildTempDefaultInit bIsOptional sResVar 
        override this.initChildWithInitFunc  (p:string) (sChildInitFuncName:string) =
            init_rust.initChildWithInitFunc  p sChildInitFuncName 
        override this.initBitStringAtPos  (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (sNamedBit:string) (nZeroBasedByteIndex:BigInteger) (sHexByteMax:string) (nZeroBasedBitIndex:BigInteger) =
            init_rust.initBitStringAtPos  sVarName sStar sFuncName sTypeDefName sNamedBit nZeroBasedByteIndex sHexByteMax nZeroBasedBitIndex 
        override this.initBitStringAtPos_def  (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (sNamedBit:string) =
            init_rust.initBitStringAtPos_def  sVarName sStar sFuncName sTypeDefName sNamedBit 
        override this.initTypeConstant_def  (sTypeDecl:string) (sConstantName:string) (sValue:string) =
            init_rust.initTypeConstant_def  sTypeDecl sConstantName sValue 
        override this.initTypeConstant_body  (sTypeDecl:string) (sConstantName:string) (sValue:string) =
            init_rust.initTypeConstant_body  sTypeDecl sConstantName sValue 
        override this.initFixSizeOctetString  (sTypeDefName:string) (nMax:BigInteger) (bZeroSizedArray:bool) =
            init_rust.initFixSizeOctetString  sTypeDefName nMax bZeroSizedArray 
        override this.initVarSizeOctetString  (sTypeDefName:string) (nMin:BigInteger) (nMax:BigInteger) =
            init_rust.initVarSizeOctetString  sTypeDefName nMin nMax 
        override this.initFixSizeBitString  (sTypeDefName:string) (nMax:BigInteger) (nMaxOctets:BigInteger) =
            init_rust.initFixSizeBitString  sTypeDefName nMax nMaxOctets 
        override this.initVarSizeBitString  (sTypeDefName:string) (nMin:BigInteger) (nMax:BigInteger) (nMaxOctets:BigInteger) =
            init_rust.initVarSizeBitString  sTypeDefName nMin nMax nMaxOctets 
        override this.initFixSizeSequenceOfExpr  (sTypeDefName:string) (nMax:BigInteger) (sChildExp:string) =
            init_rust.initFixSizeSequenceOfExpr  sTypeDefName nMax sChildExp 
        override this.initVarSizeSequenceOfExpr  (sTypeDefName:string) (nMin:BigInteger) (nMax:BigInteger) (sChildExp:string) =
            init_rust.initVarSizeSequenceOfExpr  sTypeDefName nMin nMax sChildExp 
        override this.initObjectIdentifierAsExpr  () =
            init_rust.initObjectIdentifierAsExpr  () 
        override this.init_Asn1LocalTimeExpr  () =
            init_rust.init_Asn1LocalTimeExpr  () 
        override this.init_Asn1UtcTimeExpr  () =
            init_rust.init_Asn1UtcTimeExpr  () 
        override this.init_Asn1LocalTimeWithTimeZoneExpr  () =
            init_rust.init_Asn1LocalTimeWithTimeZoneExpr  () 
        override this.init_Asn1DateExpr  () =
            init_rust.init_Asn1DateExpr  () 
        override this.init_Asn1Date_LocalTimeExpr  () =
            init_rust.init_Asn1Date_LocalTimeExpr  () 
        override this.init_Asn1Date_UtcTimeExpr  () =
            init_rust.init_Asn1Date_UtcTimeExpr  () 
        override this.init_Asn1Date_LocalTimeWithTimeZoneExpr  () =
            init_rust.init_Asn1Date_LocalTimeWithTimeZoneExpr  () 
        override this.initSequenceChildExpr  (sChildName:string) (sChildExpr:string) (bIsOptional:bool) (bIsAbsent:bool) =
            init_rust.initSequenceChildExpr  sChildName sChildExpr bIsOptional bIsAbsent 
        override this.initSequenceOptionalChildExpr  (sChildName:string) (nPresenceBit:BigInteger) =
            init_rust.initSequenceOptionalChildExpr  sChildName nPresenceBit 
        override this.initSequenceExpr  (sTypeDefName:string) (arrsChildren:seq<string>) (arrsOptionalChildren:seq<string>) =
            init_rust.initSequenceExpr  sTypeDefName arrsChildren arrsOptionalChildren 
        override this.initChoiceExpr  (sChildName:string) (sChildKind:string) (sChildExpr:string) (sChoiceTypeName:string) =
            init_rust.initChoiceExpr  sChildName sChildKind sChildExpr sChoiceTypeName 
