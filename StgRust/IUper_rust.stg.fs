module IUper_rust
open System
open System.Numerics
open CommonTypes

type IUper_rust() =
    inherit AbstractMacros.IUper()
        override this.rtlModuleName  () =
            uper_rust.rtlModuleName  () 
        override this.call_base_type_func  (p:string) (sFuncName:string) (codec:Codec) =
            uper_rust.call_base_type_func  p sFuncName codec
        override this.call_superclass_func  (p:string) (sFuncName:string) (codec:Codec) =
            uper_rust.call_superclass_func  p sFuncName codec
        override this.EmitTypeAssignment_def_err_code  (sErrCode:string) (nErrValue:BigInteger) (sFieldPath:string) =
            uper_rust.EmitTypeAssignment_def_err_code  sErrCode nErrValue sFieldPath 
        override this.EmitTypeAssignment_def  (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) (bEmptyEncodingSpace:bool) (nMaxBytesInPER:BigInteger) (nMaxBitsInPER:BigInteger) (soSparkAnnotations:string option) (bReqBytesForEncodingIsZero:bool) (codec:Codec) =
            uper_rust.EmitTypeAssignment_def  sVarName sStar sFuncName sTypeDefName arrsErrcodes bEmptyEncodingSpace nMaxBytesInPER nMaxBitsInPER soSparkAnnotations bReqBytesForEncodingIsZero codec
        override this.EmitTypeAssignment  (sVarName:string) (sStar:string) (sFuncName:string) (soIValidFuncName:string option) (sTypeDefName:string) (arrsLocalVariables:seq<string>) (sContent:string) (soSparkAnnotations:string option) (sInitialExp:string) (bReqBytesForEncodingIsZero:bool) (bBsIsUnreferenced:bool) (bVarNameIsUnreferenced:bool) (soInitFuncName:string option) (arrsAnnots:seq<string>) (arrsPrecond:seq<string>) (arrsPostcond:seq<string>) (codec:Codec) =
            uper_rust.EmitTypeAssignment  sVarName sStar sFuncName soIValidFuncName sTypeDefName arrsLocalVariables sContent soSparkAnnotations sInitialExp bReqBytesForEncodingIsZero bBsIsUnreferenced bVarNameIsUnreferenced soInitFuncName arrsAnnots arrsPrecond arrsPostcond codec
        override this.InternalItem_oct_str  (p:string) (sAcc:string) (i:string) (sErrCode:string) (codec:Codec) =
            uper_rust.InternalItem_oct_str  p sAcc i sErrCode codec
        override this.InternalItem_string_with_alpha  (p:string) (sErrCode:string) (td:FE_StringTypeDefinition) (i:string) (nLastItemIndex:BigInteger) (arrnAlphabetAsciiCodes:seq<BigInteger>) (nAlphabetLength:BigInteger) (nCharIndexSize:BigInteger) (codec:Codec) =
            uper_rust.InternalItem_string_with_alpha  p sErrCode td i nLastItemIndex arrnAlphabetAsciiCodes nAlphabetLength nCharIndexSize codec
        override this.InternalItem_string_no_alpha  (p:string) (sErrCode:string) (i:string) (codec:Codec) =
            uper_rust.InternalItem_string_no_alpha  p sErrCode i codec
        override this.IntFullyConstraint  (p:string) (nMin:BigInteger) (nMax:BigInteger) (nBits:BigInteger) (sSsuffix:string) (sErrCode:string) (soType:string option) (codec:Codec) =
            uper_rust.IntFullyConstraint  p nMin nMax nBits sSsuffix sErrCode soType codec
        override this.IntFullyConstraintPos  (p:string) (nMin:BigInteger) (nMax:BigInteger) (nBits:BigInteger) (sSsuffix:string) (sErrCode:string) (soRangeAssert:string option) (soType:string option) (codec:Codec) =
            uper_rust.IntFullyConstraintPos  p nMin nMax nBits sSsuffix sErrCode soRangeAssert soType codec
        override this.IntUnconstrained  (p:string) (sErrCode:string) (bCoverageIgnore:bool) (soType:string option) (codec:Codec) =
            uper_rust.IntUnconstrained  p sErrCode bCoverageIgnore soType codec
        override this.IntUnconstrainedMax  (p:string) (nMax:BigInteger) (soCheckExp:string option) (sErrCode:string) (codec:Codec) =
            uper_rust.IntUnconstrainedMax  p nMax soCheckExp sErrCode codec
        override this.IntSemiConstraint  (p:string) (nMin:BigInteger) (sErrCode:string) (soType:string option) (codec:Codec) =
            uper_rust.IntSemiConstraint  p nMin sErrCode soType codec
        override this.IntSemiConstraintPos  (p:string) (nMin:BigInteger) (sErrCode:string) (soType:string option) (codec:Codec) =
            uper_rust.IntSemiConstraintPos  p nMin sErrCode soType codec
        override this.IntNoneRequired  (p:string) (sConst:string) (sErrCode:string) (soType:string option) (codec:Codec) =
            uper_rust.IntNoneRequired  p sConst sErrCode soType codec
        override this.IntRootExt  (p:string) (nMin:BigInteger) (sRootBaseConstraint:string) (sIntBody:string) (sErrCode:string) (codec:Codec) =
            uper_rust.IntRootExt  p nMin sRootBaseConstraint sIntBody sErrCode codec
        override this.IntRootExt2  (p:string) (nMin:BigInteger) (sRootBaseConstraint:string) (sIntBody:string) (sErrCode:string) (sType:string) (codec:Codec) =
            uper_rust.IntRootExt2  p nMin sRootBaseConstraint sIntBody sErrCode sType codec
        override this.Boolean  (p:string) (sErrCode:string) (sType:string) (codec:Codec) =
            uper_rust.Boolean  p sErrCode sType codec
        override this.Real  (p:string) (sSuffix:string) (sErrCode:string) (sType:string) (codec:Codec) =
            uper_rust.Real  p sSuffix sErrCode sType codec
        override this.ObjectIdentifier  (p:string) (sErrCode:string) (codec:Codec) =
            uper_rust.ObjectIdentifier  p sErrCode codec
        override this.RelativeOID  (p:string) (sErrCode:string) (codec:Codec) =
            uper_rust.RelativeOID  p sErrCode codec
        override this.Time  (p:string) (sTimeSubType:string) (sErrCode:string) (codec:Codec) =
            uper_rust.Time  p sTimeSubType sErrCode codec
        override this.Enumerated_item  (p:string) (sName:string) (nIndex:BigInteger) (nLastItemIndex:BigInteger) (codec:Codec) =
            uper_rust.Enumerated_item  p sName nIndex nLastItemIndex codec
        override this.Enumerated  (p:string) (td:FE_EnumeratedTypeDefinition) (arrsItem:seq<string>) (nMin:BigInteger) (nMax:BigInteger) (nBits:BigInteger) (sErrCode:string) (nLastItemIndex:BigInteger) (sFirstItemName:string) (codec:Codec) =
            uper_rust.Enumerated  p td arrsItem nMin nMax nBits sErrCode nLastItemIndex sFirstItemName codec
        override this.Enumerated_no_switch  (p:string) (td:FE_EnumeratedTypeDefinition) (sErrCode:string) (sEnumIndex:string) (nLastItemIndex:BigInteger) (sFirstItemName:string) (codec:Codec) =
            uper_rust.Enumerated_no_switch  p td sErrCode sEnumIndex nLastItemIndex sFirstItemName codec
        override this.choice_child  (p:string) (sAcc:string) (sChildID:string) (nChildIndex:BigInteger) (nIndexSizeInBits:BigInteger) (nLastItemIndex:BigInteger) (sChildContent:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (sChildInitExpr:string) (bIsSequence:bool) (bIsEnum:bool) (codec:Codec) =
            uper_rust.choice_child  p sAcc sChildID nChildIndex nIndexSizeInBits nLastItemIndex sChildContent sChildName sChildTypeDef sChoiceTypeName sChildInitExpr bIsSequence bIsEnum codec
        override this.choice  (p:string) (sAcc:string) (arrsChildren:seq<string>) (nLastItemIndex:BigInteger) (sChoiceIndexName:string) (sErrCode:string) (td:FE_ChoiceTypeDefinition) (nIndexSizeInBits:BigInteger) (bIntroSnap:bool) (codec:Codec) =
            uper_rust.choice  p sAcc arrsChildren nLastItemIndex sChoiceIndexName sErrCode td nIndexSizeInBits bIntroSnap codec
        override this.sequence_presence_bit  (p:string) (sAcc:string) (sChName:string) (soExistVar:string option) (sErrCode:string) (codec:Codec) =
            uper_rust.sequence_presence_bit  p sAcc sChName soExistVar sErrCode codec
        override this.sequence_presence_bit_fix  (p:string) (sAcc:string) (sChName:string) (soExistVar:string option) (sErrCode:string) (sVal:string) (codec:Codec) =
            uper_rust.sequence_presence_bit_fix  p sAcc sChName soExistVar sErrCode sVal codec
        override this.sequence_mandatory_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTypedef:string) (bIsPrimitive:bool) (codec:Codec) =
            uper_rust.sequence_mandatory_child  p sAcc sChName sChildContent sChildTypedef bIsPrimitive codec
        override this.sequence_optional_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (soExistVar:string option) (soChildExpr:string option) (sChildTypedef:string) (bIsPrimitive:bool) (codec:Codec) =
            uper_rust.sequence_optional_child  p sAcc sChName sChildContent soExistVar soChildExpr sChildTypedef bIsPrimitive codec
        override this.sequence_default_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (soExistVar:string option) (soChildExpr:string option) (sChildTypedef:string) (sInitWithDefaultValue:string) (bIsPrimitive:bool) (codec:Codec) =
            uper_rust.sequence_default_child  p sAcc sChName sChildContent soExistVar soChildExpr sChildTypedef sInitWithDefaultValue bIsPrimitive codec
        override this.sequence_build  (p:string) (sTypeDefName:string) (bIsOptional:bool) (arrsChildren:seq<string>) =
            uper_rust.sequence_build  p sTypeDefName bIsOptional arrsChildren 
        override this.str_FixedSize  (p:string) (sTasName:string) (i:string) (sInternalItem:string) (nFixedSize:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (soInitExpr:string option) (bIntroSnap:bool) (soCallAux:string option) (codec:Codec) =
            uper_rust.str_FixedSize  p sTasName i sInternalItem nFixedSize nIntItemMinSize nIntItemMaxSize nAlignSize soInitExpr bIntroSnap soCallAux codec
        override this.str_VarSize  (p:string) (sPIden:string) (sTasName:string) (i:string) (sInternalItem:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nSizeInBits:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (soInitExpr:string option) (soCallAux:string option) (sType:string) (codec:Codec) =
            uper_rust.str_VarSize  p sPIden sTasName i sInternalItem nSizeMin nSizeMax nSizeInBits nIntItemMinSize nIntItemMaxSize nAlignSize soInitExpr soCallAux sType codec
        override this.seqOf_FixedSize  (p:string) (sTasName:string) (i:string) (sInternalItem:string) (nFixedSize:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (sChildInitExpr:string) (soCallAux:string option) (codec:Codec) =
            uper_rust.seqOf_FixedSize  p sTasName i sInternalItem nFixedSize nIntItemMinSize nIntItemMaxSize nAlignSize sChildInitExpr soCallAux codec
        override this.seqOf_VarSize  (p:string) (sAcc:string) (sTasName:string) (i:string) (sInternalItem:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nSizeInBits:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (sChildInitExpr:string) (sErrCode:string) (nAbsOffset:BigInteger) (nRemainingMinBits:BigInteger) (nLevel:BigInteger) (nIx:BigInteger) (nOffset:BigInteger) (bIntroSnap:bool) (soCallAux:string option) (codec:Codec) =
            uper_rust.seqOf_VarSize  p sAcc sTasName i sInternalItem nSizeMin nSizeMax nSizeInBits nIntItemMinSize nIntItemMaxSize nAlignSize sChildInitExpr sErrCode nAbsOffset nRemainingMinBits nLevel nIx nOffset bIntroSnap soCallAux codec
        override this.octet_FixedSize  (sTypeDefName:string) (p:string) (sAcc:string) (nFixedSize:BigInteger) (codec:Codec) =
            uper_rust.octet_FixedSize  sTypeDefName p sAcc nFixedSize codec
        override this.octet_VarSize  (sTypeDefName:string) (p:string) (sAcc:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nSizeInBits:BigInteger) (sErrCode:string) (codec:Codec) =
            uper_rust.octet_VarSize  sTypeDefName p sAcc nSizeMin nSizeMax nSizeInBits sErrCode codec
        override this.bitString_FixSize  (sTypeDefName:string) (p:string) (sAcc:string) (nFixedSize:BigInteger) (sErrCode:string) (codec:Codec) =
            uper_rust.bitString_FixSize  sTypeDefName p sAcc nFixedSize sErrCode codec
        override this.bitString_VarSize  (sTypeDefName:string) (p:string) (sAcc:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (sErrCode:string) (nSizeInBits:BigInteger) (codec:Codec) =
            uper_rust.bitString_VarSize  sTypeDefName p sAcc nSizeMin nSizeMax sErrCode nSizeInBits codec
        override this.FixedSize_Fragmentation_sqf_64K  (p:string) (sAcc:string) (sCurOffset:string) (sCurBlockSize:string) (sBlockIndex:string) (nBlocks64K:BigInteger) (sInternalItem:string) (sBLI:string) (sRemainingItemsVar:string) (bIsBitStringType:bool) (sErrCodeName:string) (codec:Codec) =
            uper_rust.FixedSize_Fragmentation_sqf_64K  p sAcc sCurOffset sCurBlockSize sBlockIndex nBlocks64K sInternalItem sBLI sRemainingItemsVar bIsBitStringType sErrCodeName codec
        override this.FixedSize_Fragmentation_sqf_small_block  (p:string) (sAcc:string) (sInternalItem:string) (nBlockSize:BigInteger) (sBlockId:string) (sCurOffset:string) (sCurBlockSize:string) (sBLI:string) (sRemainingItemsVar:string) (bIsBitStringType:bool) (sErrCodeName:string) (codec:Codec) =
            uper_rust.FixedSize_Fragmentation_sqf_small_block  p sAcc sInternalItem nBlockSize sBlockId sCurOffset sCurBlockSize sBLI sRemainingItemsVar bIsBitStringType sErrCodeName codec
        override this.FixedSize_Fragmentation_sqf_remaining  (p:string) (sAcc:string) (sInternalItem:string) (bRemainingItemsWithinByte:bool) (nRemainingItemsVar:BigInteger) (sCurOffset:string) (sBLI:string) (sRemainingItemsVar:string) (bIsBitStringType:bool) (sErrCodeName:string) (codec:Codec) =
            uper_rust.FixedSize_Fragmentation_sqf_remaining  p sAcc sInternalItem bRemainingItemsWithinByte nRemainingItemsVar sCurOffset sBLI sRemainingItemsVar bIsBitStringType sErrCodeName codec
        override this.FixedSize_Fragmentation_sqf  (p:string) (sAcc:string) (arrsEncodingParts:seq<string>) (nFixedSize:BigInteger) (bIsBitStringType:bool) (codec:Codec) =
            uper_rust.FixedSize_Fragmentation_sqf  p sAcc arrsEncodingParts nFixedSize bIsBitStringType codec
        override this.Fragmentation_sqf  (p:string) (sAcc:string) (sInternalItem:string) (nIntItemMaxSize:BigInteger) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nRequiredBitsForUPerEncoding:BigInteger) (bIsVariableSize:bool) (sErrCodeName:string) (sRemainingItemsVar:string) (sCurBlockSize:string) (sBlockIndex:string) (sCurOffset:string) (sBLJ:string) (sBLI:string) (sLengthTmp:string) (bIsBitStringType:bool) (bIsAsciiString:bool) (codec:Codec) =
            uper_rust.Fragmentation_sqf  p sAcc sInternalItem nIntItemMaxSize nSizeMin nSizeMax nRequiredBitsForUPerEncoding bIsVariableSize sErrCodeName sRemainingItemsVar sCurBlockSize sBlockIndex sCurOffset sBLJ sBLI sLengthTmp bIsBitStringType bIsAsciiString codec
        override this.octet_string_containing_func  (p:string) (sFuncName:string) (sContainedType:string) (sReqBytesForUperEncoding:string) (nBits:BigInteger) (nMinSize:BigInteger) (nMaxSize:BigInteger) (codec:Codec) =
            uper_rust.octet_string_containing_func  p sFuncName sContainedType sReqBytesForUperEncoding nBits nMinSize nMaxSize codec
        override this.bit_string_containing_func  (p:string) (sFuncName:string) (sContainedType:string) (sReqBytesForUperEncoding:string) (sReqBitsForUperEncoding:string) (nBits:BigInteger) (nMinSize:BigInteger) (nMaxSize:BigInteger) (codec:Codec) =
            uper_rust.bit_string_containing_func  p sFuncName sContainedType sReqBytesForUperEncoding sReqBitsForUperEncoding nBits nMinSize nMaxSize codec
        override this.sparkAnnotations  (sTypeDefName:string) (codec:Codec) =
            uper_rust.sparkAnnotations  sTypeDefName codec
        override this.Null_declare  (p:string) (sType:string) =
            uper_rust.Null_declare  p sType 
        override this.decode_nullType  (p:string) =
            uper_rust.decode_nullType  p 
        override this.decode_empty_sequence_emptySeq  (p:string) =
            uper_rust.decode_empty_sequence_emptySeq  p 
        override this.JoinItems  (sPart:string) (soNestedPart:string option) =
            uper_rust.JoinItems  sPart soNestedPart 
        override this.update_array_item  (p:string) (sI:string) (sExpr:string) =
            uper_rust.update_array_item  p sI sExpr 
        override this.InternalItem_bit_str  (p:string) (i:string) (sErrCode:string) (codec:Codec) =
            uper_rust.InternalItem_bit_str  p i sErrCode codec
