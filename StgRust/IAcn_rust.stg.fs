module IAcn_rust
open System
open System.Numerics
open CommonTypes

type IAcn_rust() =
    inherit AbstractMacros.IAcn()
        override this.getStringSize  (p:string) =
            acn_rust.getStringSize  p 
        override this.getSizeableSize  (p:string) (sAcc:string) (bIsUnsigned:bool) =
            acn_rust.getSizeableSize  p sAcc bIsUnsigned 
        override this.EmitTypeAssignment_def_err_code  (sErrCode:string) (nErrValue:BigInteger) (soErrorCodeComment:string option) (sFieldPath:string) =
            acn_rust.EmitTypeAssignment_def_err_code  sErrCode nErrValue soErrorCodeComment sFieldPath 
        override this.EmitAcnParameter  (sName:string) (sType:string) (sDefaultValue:string) =
            acn_rust.EmitAcnParameter  sName sType sDefaultValue 
        override this.EmitEncodingSizeConstants  (sTypeDefName:string) (nMaxBytesInACN:BigInteger) (nMaxBitsInACN:BigInteger) =
            acn_rust.EmitEncodingSizeConstants  sTypeDefName nMaxBytesInACN nMaxBitsInACN 
        override this.EmitTypeAssignment_primitive_def  (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) (bEmptyEncodingSpace:bool) (nMaxBytesInACN:BigInteger) (nMaxBitsInACN:BigInteger) (arrsAcnPrms:seq<string>) (soSparkAnnotations:string option) (codec:Codec) =
            acn_rust.EmitTypeAssignment_primitive_def  sVarName sStar sFuncName sTypeDefName arrsErrcodes bEmptyEncodingSpace nMaxBytesInACN nMaxBitsInACN arrsAcnPrms soSparkAnnotations codec
        override this.EmitTypeAssignment_primitive  (sVarName:string) (sStar:string) (sFuncName:string) (soIValidFuncName:string option) (sTypeDefName:string) (arrsLocalVariables:seq<string>) (sContent:string) (soSparkAnnotations:string option) (sInitialExp:string) (arrsAcnPrms:seq<string>) (arrsAcnParamNames:seq<string>) (bEmptyEncodingSpace:bool) (bBsIsUnreferenced:bool) (bVarNameIsUnreferenced:bool) (bHasAcnChildrenToReturn:bool) (soInitFuncName:string option) (arrsAnnots:seq<string>) (arrsPrecond:seq<string>) (arrsPostcond:seq<string>) (codec:Codec) =
            acn_rust.EmitTypeAssignment_primitive  sVarName sStar sFuncName soIValidFuncName sTypeDefName arrsLocalVariables sContent soSparkAnnotations sInitialExp arrsAcnPrms arrsAcnParamNames bEmptyEncodingSpace bBsIsUnreferenced bVarNameIsUnreferenced bHasAcnChildrenToReturn soInitFuncName arrsAnnots arrsPrecond arrsPostcond codec
        override this.MappingFunctionDeclaration  (sTypeName:string) (sMF:string) (codec:Codec) =
            acn_rust.MappingFunctionDeclaration  sTypeName sMF codec
        override this.alignToNext  (sMainBody:string) (sAlignmentValue:string) (nAlignmentValue:BigInteger) (nAbsOffset:BigInteger) (nRemainingMinBits:BigInteger) (nLevel:BigInteger) (nIx:BigInteger) (nOffset:BigInteger) (codec:Codec) =
            acn_rust.alignToNext  sMainBody sAlignmentValue nAlignmentValue nAbsOffset nRemainingMinBits nLevel nIx nOffset codec
        override this.PositiveInteger_ConstSize  (p:string) (sSsuffix:string) (sErrCode:string) (nFixedSize:BigInteger) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_ConstSize  p sSsuffix sErrCode nFixedSize soMF soMFM nUperMin nUperMax sType codec
        override this.PositiveInteger_ConstSize_8  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_ConstSize_8  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.PositiveInteger_ConstSize_big_endian_16  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_ConstSize_big_endian_16  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.PositiveInteger_ConstSize_big_endian_32  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_ConstSize_big_endian_32  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.PositiveInteger_ConstSize_big_endian_64  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_ConstSize_big_endian_64  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.PositiveInteger_ConstSize_little_endian_16  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_ConstSize_little_endian_16  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.PositiveInteger_ConstSize_little_endian_32  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_ConstSize_little_endian_32  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.PositiveInteger_ConstSize_little_endian_64  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_ConstSize_little_endian_64  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.PositiveInteger_VarSize_LengthEmbedded  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.PositiveInteger_VarSize_LengthEmbedded  p sSsuffix sErrCode soMF soMFM nUperMin sType codec
        override this.TwosComplement_ConstSize  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nFixedSize:BigInteger) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_ConstSize  p sSsuffix sErrCode soMF soMFM nFixedSize nUperMin nUperMax sType codec
        override this.TwosComplement_ConstSize_8  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_ConstSize_8  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.TwosComplement_ConstSize_big_endian_16  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_ConstSize_big_endian_16  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.TwosComplement_ConstSize_big_endian_32  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_ConstSize_big_endian_32  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.TwosComplement_ConstSize_big_endian_64  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_ConstSize_big_endian_64  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.TwosComplement_ConstSize_little_endian_16  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_ConstSize_little_endian_16  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.TwosComplement_ConstSize_little_endian_32  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_ConstSize_little_endian_32  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.TwosComplement_ConstSize_little_endian_64  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_ConstSize_little_endian_64  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.TwosComplement_VarSize_LengthEmbedded  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (sType:string) (codec:Codec) =
            acn_rust.TwosComplement_VarSize_LengthEmbedded  p sSsuffix sErrCode soMF soMFM sType codec
        override this.BCD_ConstSize  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (nNibbles:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.BCD_ConstSize  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax nNibbles sType codec
        override this.BCD_VarSize_LengthEmbedded  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (sType:string) (codec:Codec) =
            acn_rust.BCD_VarSize_LengthEmbedded  p sSsuffix sErrCode soMF soMFM sType codec
        override this.BCD_VarSize_NullTerminated  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.BCD_VarSize_NullTerminated  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax sType codec
        override this.ASCII_ConstSize  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (nSizeInBytes:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.ASCII_ConstSize  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax nSizeInBytes sType codec
        override this.ASCII_VarSize_LengthEmbedded  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (sType:string) (codec:Codec) =
            acn_rust.ASCII_VarSize_LengthEmbedded  p sSsuffix sErrCode soMF soMFM sType codec
        override this.ASCII_VarSize_NullTerminated  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (arruNullBytes:seq<byte>) (sType:string) (codec:Codec) =
            acn_rust.ASCII_VarSize_NullTerminated  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax arruNullBytes sType codec
        override this.ASCII_UINT_ConstSize  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (nSizeInBytes:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.ASCII_UINT_ConstSize  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax nSizeInBytes sType codec
        override this.ASCII_UINT_VarSize_NullTerminated  (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (arruNullBytes:seq<byte>) (sType:string) (codec:Codec) =
            acn_rust.ASCII_UINT_VarSize_NullTerminated  p sSsuffix sErrCode soMF soMFM nUperMin nUperMax arruNullBytes sType codec
        override this.Real_32_big_endian  (p:string) (sSuffix:string) (sErrCode:string) (sType:string) (codec:Codec) =
            acn_rust.Real_32_big_endian  p sSuffix sErrCode sType codec
        override this.Real_64_big_endian  (p:string) (sErrCode:string) (sType:string) (codec:Codec) =
            acn_rust.Real_64_big_endian  p sErrCode sType codec
        override this.Real_32_little_endian  (p:string) (sSuffix:string) (sErrCode:string) (sType:string) (codec:Codec) =
            acn_rust.Real_32_little_endian  p sSuffix sErrCode sType codec
        override this.Real_64_little_endian  (p:string) (sErrCode:string) (sType:string) (codec:Codec) =
            acn_rust.Real_64_little_endian  p sErrCode sType codec
        override this.Real_ScaledInt_uint  (p:string) (sTmpVar:string) (sIntBody:string) (sLow:string) (sScale:string) (sUintMax:string) (sErrCode:string) (codec:Codec) =
            acn_rust.Real_ScaledInt_uint  p sTmpVar sIntBody sLow sScale sUintMax sErrCode codec
        override this.Real_ScaledInt_sint  (p:string) (sTmpVar:string) (sIntBody:string) (sLow:string) (sScale:string) (sIntMin:string) (sIntMax:string) (sErrCode:string) (codec:Codec) =
            acn_rust.Real_ScaledInt_sint  p sTmpVar sIntBody sLow sScale sIntMin sIntMax sErrCode codec
        override this.Boolean  (p:string) (ptr:string) (bEncValIsTrue:bool) (nSize:BigInteger) (arruTrueValueAsByteArray:seq<byte>) (arruFalseValueAsByteArray:seq<byte>) (arrsBits:seq<string>) (sErrCode:string) (sType:string) (codec:Codec) =
            acn_rust.Boolean  p ptr bEncValIsTrue nSize arruTrueValueAsByteArray arruFalseValueAsByteArray arrsBits sErrCode sType codec
        override this.BooleanTrueFalse  (p:string) (ptr:string) (nSize:BigInteger) (arruTrueValueAsByteArray:seq<byte>) (arruFalseValueAsByteArray:seq<byte>) (arrsTrueBits:seq<string>) (arrsFalseBits:seq<string>) (sErrCode:string) (sType:string) (codec:Codec) =
            acn_rust.BooleanTrueFalse  p ptr nSize arruTrueValueAsByteArray arruFalseValueAsByteArray arrsTrueBits arrsFalseBits sErrCode sType codec
        override this.Null_declare  (p:string) (sType:string) =
            acn_rust.Null_declare  p sType 
        override this.Null_pattern  (p:string) (arruNullValueAsByteArray:seq<byte>) (nSize:BigInteger) (arrsBits:seq<string>) (sErrCode:string) (bSavePosition:bool) (codec:Codec) =
            acn_rust.Null_pattern  p arruNullValueAsByteArray nSize arrsBits sErrCode bSavePosition codec
        override this.Null_pattern2  (p:string) (arruNullValueAsByteArray:seq<byte>) (nSize:BigInteger) (arrsBits:seq<string>) (sErrCode:string) (bSavePosition:bool) (codec:Codec) =
            acn_rust.Null_pattern2  p arruNullValueAsByteArray nSize arrsBits sErrCode bSavePosition codec
        override this.Enumerated_item  (p:string) (sName:string) (sEnumHolder:string) (nItemIdx:BigInteger) (sItemVal:string) (sIntVal:string) (codec:Codec) =
            acn_rust.Enumerated_item  p sName sEnumHolder nItemIdx sItemVal sIntVal codec
        override this.EnumeratedEncIdx  (p:string) (td:FE_EnumeratedTypeDefinition) (arrsItem:seq<string>) (sActualCodecFunc:string) (sIntVal:string) (codec:Codec) =
            acn_rust.EnumeratedEncIdx  p td arrsItem sActualCodecFunc sIntVal codec
        override this.EnumeratedEncValues  (p:string) (td:FE_EnumeratedTypeDefinition) (arrsItem:seq<string>) (sActualCodecFunc:string) (sErrCode:string) (sFirstItemName:string) (sIntVal:string) (codec:Codec) =
            acn_rust.EnumeratedEncValues  p td arrsItem sActualCodecFunc sErrCode sFirstItemName sIntVal codec
        override this.EnumeratedEncValues_no_switch  (p:string) (td:FE_EnumeratedTypeDefinition) (sActualCodecFunc:string) (sErrCode:string) (sFirstItemName:string) (sIntVal:string) (sEnumIndex:string) (nLastItemIndex:BigInteger) (bEncodeValues:bool) (codec:Codec) =
            acn_rust.EnumeratedEncValues_no_switch  p td sActualCodecFunc sErrCode sFirstItemName sIntVal sEnumIndex nLastItemIndex bEncodeValues codec
        override this.Acn_String_Ascii_FixSize  (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (codec:Codec) =
            acn_rust.Acn_String_Ascii_FixSize  p sErrCode nAsn1Max codec
        override this.Acn_String_Ascii_Null_Terminated  (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (arruNullBytes:seq<byte>) (sType:string) (codec:Codec) =
            acn_rust.Acn_String_Ascii_Null_Terminated  p sErrCode nAsn1Max arruNullBytes sType codec
        override this.Acn_String_Ascii_External_Field_Determinant  (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (sExtFld:string) (sType:string) (codec:Codec) =
            acn_rust.Acn_String_Ascii_External_Field_Determinant  p sErrCode nAsn1Max sExtFld sType codec
        override this.Acn_String_Ascii_Internal_Field_Determinant  (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (nAsn1Min:BigInteger) (nInternalLengthDeterminantSizeInBits:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.Acn_String_Ascii_Internal_Field_Determinant  p sErrCode nAsn1Max nAsn1Min nInternalLengthDeterminantSizeInBits sType codec
        override this.Acn_String_CharIndex_FixSize  (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (arrnAlphabetAsciiCodes:seq<BigInteger>) (nCharSetSize:BigInteger) (td:FE_StringTypeDefinition) (nCharSize:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.Acn_String_CharIndex_FixSize  p sErrCode nAsn1Max arrnAlphabetAsciiCodes nCharSetSize td nCharSize sType codec
        override this.Acn_String_CharIndex_External_Field_Determinant  (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (arrnAlphabetAsciiCodes:seq<BigInteger>) (nCharSetSize:BigInteger) (sExtFld:string) (td:FE_StringTypeDefinition) (nCharSize:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.Acn_String_CharIndex_External_Field_Determinant  p sErrCode nAsn1Max arrnAlphabetAsciiCodes nCharSetSize sExtFld td nCharSize sType codec
        override this.Acn_IA5String_CharIndex_External_Field_Determinant  (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (sExtFld:string) (td:FE_StringTypeDefinition) (nCharSize:BigInteger) (nRemainingBits:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.Acn_IA5String_CharIndex_External_Field_Determinant  p sErrCode nAsn1Max sExtFld td nCharSize nRemainingBits sType codec
        override this.oct_external_field  (sTypedefName:string) (p:string) (sAcc:string) (noSizeMin:BigInteger option) (noSizeMax:BigInteger option) (sExtFld:string) (bIsUnsigned:bool) (nAlignSize:BigInteger) (sErrCode:string) (codec:Codec) =
            acn_rust.oct_external_field  sTypedefName p sAcc noSizeMin noSizeMax sExtFld bIsUnsigned nAlignSize sErrCode codec
        override this.oct_external_field_fix_size  (sTypedefName:string) (p:string) (sAcc:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (sExtFld:string) (bIsUnsigned:bool) (nAlignSize:BigInteger) (sErrCode:string) (codec:Codec) =
            acn_rust.oct_external_field_fix_size  sTypedefName p sAcc noSizeMin nSizeMax sExtFld bIsUnsigned nAlignSize sErrCode codec
        override this.seqOf_VarSize  (p:string) (sAcc:string) (sTasName:string) (i:string) (sInternalItem:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nSizeInBits:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (sChildInitExpr:string) (sErrCode:string) (nAbsOffset:BigInteger) (nRemainingMinBits:BigInteger) (nLevel:BigInteger) (nIx:BigInteger) (nOffset:BigInteger) (bIntroSnap:bool) (soCallAux:string option) (sType:string) (codec:Codec) =
            acn_rust.seqOf_VarSize  p sAcc sTasName i sInternalItem nSizeMin nSizeMax nSizeInBits nIntItemMinSize nIntItemMaxSize nAlignSize sChildInitExpr sErrCode nAbsOffset nRemainingMinBits nLevel nIx nOffset bIntroSnap soCallAux sType codec
        override this.sqf_external_field  (sTypeDefName:string) (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (sExtFld:string) (bIsUnsigned:bool) (nAlignSize:BigInteger) (sErrCode:string) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (sChildInitExpr:string) (bIntroSnap:bool) (soCallAux:string option) (codec:Codec) =
            acn_rust.sqf_external_field  sTypeDefName p sAcc i sInternalItem noSizeMin nSizeMax sExtFld bIsUnsigned nAlignSize sErrCode nIntItemMinSize nIntItemMaxSize sChildInitExpr bIntroSnap soCallAux codec
        override this.sqf_external_field_fix_size  (sTypeDefName:string) (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (sExtFld:string) (bIsUnsigned:bool) (nAlignSize:BigInteger) (sErrCode:string) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (sChildInitExpr:string) (bIntroSnap:bool) (soCallAux:string option) (codec:Codec) =
            acn_rust.sqf_external_field_fix_size  sTypeDefName p sAcc i sInternalItem noSizeMin nSizeMax sExtFld bIsUnsigned nAlignSize sErrCode nIntItemMinSize nIntItemMaxSize sChildInitExpr bIntroSnap soCallAux codec
        override this.oct_sqf_null_terminated  (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (arruNullBytes:seq<byte>) (nBitPatternLength:BigInteger) (sErrCode:string) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.oct_sqf_null_terminated  p sAcc i sInternalItem noSizeMin nSizeMax arruNullBytes nBitPatternLength sErrCode nIntItemMinSize nIntItemMaxSize sType codec
        override this.str_ascii_deduced  (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (nTrailingBits:BigInteger) (sType:string) (codec:Codec) =
            acn_rust.str_ascii_deduced  p sErrCode nAsn1Max nTrailingBits sType codec
        override this.oct_deduced  (sTypedefName:string) (p:string) (sAcc:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (nTrailingBits:BigInteger) (sErrCode:string) (codec:Codec) =
            acn_rust.oct_deduced  sTypedefName p sAcc noSizeMin nSizeMax nTrailingBits sErrCode codec
        override this.sqf_deduced_fix_elem  (sTypeDefName:string) (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (nTrailingBits:BigInteger) (nIntItemSize:BigInteger) (sErrCode:string) (codec:Codec) =
            acn_rust.sqf_deduced_fix_elem  sTypeDefName p sAcc i sInternalItem noSizeMin nSizeMax nTrailingBits nIntItemSize sErrCode codec
        override this.sqf_deduced_var_elem  (sTypeDefName:string) (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (nTrailingBits:BigInteger) (nIntItemMinSize:BigInteger) (sErrCode:string) (codec:Codec) =
            acn_rust.sqf_deduced_var_elem  sTypeDefName p sAcc i sInternalItem noSizeMin nSizeMax nTrailingBits nIntItemMinSize sErrCode codec
        override this.bit_string_external_field  (sTypeDefName:string) (p:string) (sErrCode:string) (sAcc:string) (noSizeMin:BigInteger option) (noSizeMax:BigInteger option) (sExtFld:string) (codec:Codec) =
            acn_rust.bit_string_external_field  sTypeDefName p sErrCode sAcc noSizeMin noSizeMax sExtFld codec
        override this.bit_string_external_field_fixed_size  (sTypeDefName:string) (p:string) (sErrCode:string) (sAcc:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (sExtFld:string) (codec:Codec) =
            acn_rust.bit_string_external_field_fixed_size  sTypeDefName p sErrCode sAcc noSizeMin nSizeMax sExtFld codec
        override this.bit_string_null_terminated  (sTypeDefName:string) (p:string) (sErrCode:string) (sAcc:string) (i:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (arruNullBytes:seq<byte>) (nBitPatternLength:BigInteger) (bFixedSize:bool) (codec:Codec) =
            acn_rust.bit_string_null_terminated  sTypeDefName p sErrCode sAcc i noSizeMin nSizeMax arruNullBytes nBitPatternLength bFixedSize codec
        override this.RefTypeParam_tmpVar  (sName:string) (sTypeDecl:string) =
            acn_rust.RefTypeParam_tmpVar  sName sTypeDecl 
        override this.ReferenceType1  (p:string) (sName:string) (bAcnEncodeFuncRequiresResult:bool) (arrsArgs:seq<string>) (arrsLocalPrms:seq<string>) (codec:Codec) =
            acn_rust.ReferenceType1  p sName bAcnEncodeFuncRequiresResult arrsArgs arrsLocalPrms codec
        override this.sequence_presence_optChild  (p:string) (sAcc:string) (sChName:string) (soExistVar:string option) (sErrCode:string) (codec:Codec) =
            acn_rust.sequence_presence_optChild  p sAcc sChName soExistVar sErrCode codec
        override this.sequence_presence_optChild_pres_acn_expression  (p:string) (sAcc:string) (sChName:string) (sAcnExpression:string) (soExistVar:string option) (sErrCode:string) (codec:Codec) =
            acn_rust.sequence_presence_optChild_pres_acn_expression  p sAcc sChName sAcnExpression soExistVar sErrCode codec
        override this.sequence_presence_optChild_pres_bool  (p:string) (sAcc:string) (sChName:string) (sExtFldName:string) (codec:Codec) =
            acn_rust.sequence_presence_optChild_pres_bool  p sAcc sChName sExtFldName codec
        override this.sequence_presence_optChild_pres_int  (p:string) (sAcc:string) (sChName:string) (sExtFldName:string) (nIntVal:BigInteger) (codec:Codec) =
            acn_rust.sequence_presence_optChild_pres_int  p sAcc sChName sExtFldName nIntVal codec
        override this.sequence_presence_optChild_pres_str  (p:string) (sAcc:string) (sChName:string) (sExtFldName:string) (sVal:string) (codec:Codec) =
            acn_rust.sequence_presence_optChild_pres_str  p sAcc sChName sExtFldName sVal codec
        override this.sequence_save_bitStream_start  (sBitStreamPositionsLocalVar:string) (codec:Codec) =
            acn_rust.sequence_save_bitStream_start  sBitStreamPositionsLocalVar codec
        override this.sequence_save_bitstream  (sBitStreamPositionsLocalVar:string) (sChName:string) (codec:Codec) =
            acn_rust.sequence_save_bitstream  sBitStreamPositionsLocalVar sChName codec
        override this.sequence_acn_child  (sChName:string) (sChildContent:string) (sErrCode:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) (codec:Codec) =
            acn_rust.sequence_acn_child  sChName sChildContent sErrCode soSaveBitStrmPosStatement bIsPrimitive codec
        override this.sequence_mandatory_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (soSaveBitStrmPosStatement:string option) (sChildTypedef:string) (bIsPrimitive:bool) (arrsAcnParams:seq<string>) (bChildHasAcnChildrenToReturn:bool) (soAlignmentCode:string option) (bInlineRequired:bool) (codec:Codec) =
            acn_rust.sequence_mandatory_child  p sAcc sChName sChildContent soSaveBitStrmPosStatement sChildTypedef bIsPrimitive arrsAcnParams bChildHasAcnChildrenToReturn soAlignmentCode bInlineRequired codec
        override this.sequence_always_present_child  (p:string) (sAcc:string) (sChName:string) (soChildContent:string option) (soChildExpr:string option) (sChildTypedef:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) (arrsAcnParams:seq<string>) (bChildHasAcnChildrenToReturn:bool) (soAlignmentCode:string option) (bInlineRequired:bool) (codec:Codec) =
            acn_rust.sequence_always_present_child  p sAcc sChName soChildContent soChildExpr sChildTypedef soSaveBitStrmPosStatement bIsPrimitive arrsAcnParams bChildHasAcnChildrenToReturn soAlignmentCode bInlineRequired codec
        override this.sequence_always_absent_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTypedef:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) (codec:Codec) =
            acn_rust.sequence_always_absent_child  p sAcc sChName sChildContent sChildTypedef soSaveBitStrmPosStatement bIsPrimitive codec
        override this.sequence_optional_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (soExistVar:string option) (soChildExpr:string option) (sChildTypedef:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) (arrsAcnParams:seq<string>) (bChildHasAcnChildrenToReturn:bool) (soAlignmentCode:string option) (bInlineRequired:bool) (codec:Codec) =
            acn_rust.sequence_optional_child  p sAcc sChName sChildContent soExistVar soChildExpr sChildTypedef soSaveBitStrmPosStatement bIsPrimitive arrsAcnParams bChildHasAcnChildrenToReturn soAlignmentCode bInlineRequired codec
        override this.sequence_default_child  (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sInitWithDefaultValue:string) (soExistVar:string option) (soChildExpr:string option) (sChildTypedef:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) (bChildHasAcnChildrenToReturn:bool) (soAlignmentCode:string option) (codec:Codec) =
            acn_rust.sequence_default_child  p sAcc sChName sChildContent sInitWithDefaultValue soExistVar soChildExpr sChildTypedef soSaveBitStrmPosStatement bIsPrimitive bChildHasAcnChildrenToReturn soAlignmentCode codec
        override this.sequence_call_post_encoding_function  (p:string) (sFncName:string) (sBitStreamStartPos:string) (sBitStreamPositionsNullPos:string) =
            acn_rust.sequence_call_post_encoding_function  p sFncName sBitStreamStartPos sBitStreamPositionsNullPos 
        override this.sequence_call_post_decoding_validator  (p:string) (sFncName:string) (sBitStreamStartPos:string) (sBitStreamPositionsNullPos:string) =
            acn_rust.sequence_call_post_decoding_validator  p sFncName sBitStreamStartPos sBitStreamPositionsNullPos 
        override this.sequence_call_post_encoding_function_prototype  (sTypedefName:string) (sFncName:string) (sExtFuncsPositionsTypeName:string) =
            acn_rust.sequence_call_post_encoding_function_prototype  sTypedefName sFncName sExtFuncsPositionsTypeName 
        override this.sequence_call_post_decoding_validator_prototype  (sTypedefName:string) (sFncName:string) (sExtFuncsPositionsTypeName:string) =
            acn_rust.sequence_call_post_decoding_validator_prototype  sTypedefName sFncName sExtFuncsPositionsTypeName 
        override this.ChoiceChildAlwaysAbsent  (p:string) (sAcc:string) (sChildID:string) (nChildIndex:BigInteger) (sErrorCodeName:string) (sChoiceTypeName:string) (codec:Codec) =
            acn_rust.ChoiceChildAlwaysAbsent  p sAcc sChildID nChildIndex sErrorCodeName sChoiceTypeName codec
        override this.ChoiceChild  (p:string) (sAcc:string) (sChildID:string) (nChildIndex:BigInteger) (nIndexSizeInBits:BigInteger) (nLastItemIndex:BigInteger) (sChildContent:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (sChildInitExpr:string) (codec:Codec) =
            acn_rust.ChoiceChild  p sAcc sChildID nChildIndex nIndexSizeInBits nLastItemIndex sChildContent sChildName sChildTypeDef sChoiceTypeName sChildInitExpr codec
        override this.Choice  (p:string) (sAcc:string) (arrsChildren:seq<string>) (nLastItemIndex:BigInteger) (sChoiceIndexName:string) (td:FE_ChoiceTypeDefinition) (nIndexSizeInBits:BigInteger) (sErrCode:string) (codec:Codec) =
            acn_rust.Choice  p sAcc arrsChildren nLastItemIndex sChoiceIndexName td nIndexSizeInBits sErrCode codec
        override this.ChoiceChild_preWhen  (p:string) (sAcc:string) (sChildID:string) (sChildBody:string) (arrsConditions:seq<string>) (bFirst:bool) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (sChildInitExpr:string) (bIsPrimitive:bool) (arrsAcnParams:seq<string>) (codec:Codec) =
            acn_rust.ChoiceChild_preWhen  p sAcc sChildID sChildBody arrsConditions bFirst sChildName sChildTypeDef sChoiceTypeName sChildInitExpr bIsPrimitive arrsAcnParams codec
        override this.ChoiceChild_preWhen_bool_condition  (sExtFld:string) =
            acn_rust.ChoiceChild_preWhen_bool_condition  sExtFld 
        override this.ChoiceChild_preWhen_int_condition  (sExtFld:string) (sVal:string) =
            acn_rust.ChoiceChild_preWhen_int_condition  sExtFld sVal 
        override this.ChoiceChild_preWhen_str_condition  (sExtFld:string) (sVal:string) (arrsNullChars:seq<string>) (arruVal:seq<byte>) =
            acn_rust.ChoiceChild_preWhen_str_condition  sExtFld sVal arrsNullChars arruVal 
        override this.Choice_preWhen  (p:string) (sAcc:string) (arrsChildren:seq<string>) (td:FE_ChoiceTypeDefinition) (sErrCode:string) (codec:Codec) =
            acn_rust.Choice_preWhen  p sAcc arrsChildren td sErrCode codec
        override this.ChoiceChild_Enum  (p:string) (sAcc:string) (sEnmName:string) (sChildID:string) (sChildBody:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (sChildInitExpr:string) (codec:Codec) =
            acn_rust.ChoiceChild_Enum  p sAcc sEnmName sChildID sChildBody sChildName sChildTypeDef sChoiceTypeName sChildInitExpr codec
        override this.Choice_Enum  (p:string) (sAcc:string) (arrsChildren:seq<string>) (sEnmExtFld:string) (td:FE_ChoiceTypeDefinition) (sErrCode:string) (bIsDeferred:bool) (codec:Codec) =
            acn_rust.Choice_Enum  p sAcc arrsChildren sEnmExtFld td sErrCode bIsDeferred codec
        override this.SizeDependency  (v:string) (sCount:string) (nMin:BigInteger) (nMax:BigInteger) (bCheckRange:bool) (sTypedefName:string) =
            acn_rust.SizeDependency  v sCount nMin nMax bCheckRange sTypedefName 
        override this.SizeDependencyFixedSize  (v:string) (nFixedSize:BigInteger) =
            acn_rust.SizeDependencyFixedSize  v nFixedSize 
        override this.ChoiceDependencyEnum_Item  (v:string) (sChildCID:string) (sChildCIDHolder:string) (sEnumCName:string) (nChoiceIdx:BigInteger) (bIsOptional:bool) =
            acn_rust.ChoiceDependencyEnum_Item  v sChildCID sChildCIDHolder sEnumCName nChoiceIdx bIsOptional 
        override this.ChoiceDependencyEnum  (sV:string) (sChPath:string) (sAcc:string) (arrsChoiceEnumItems:seq<string>) (bIsOptional:bool) (sDefaultExpr:string) =
            acn_rust.ChoiceDependencyEnum  sV sChPath sAcc arrsChoiceEnumItems bIsOptional sDefaultExpr 
        override this.PresenceDependency  (v:string) (sSeqPath:string) (sAcc:string) (sChildName:string) =
            acn_rust.PresenceDependency  v sSeqPath sAcc sChildName 
        override this.ChoiceDependencyIntPres_child  (v:string) (sChildNamePresent:string) (sChildRetVal:string) (sChoiceTypeName:string) =
            acn_rust.ChoiceDependencyIntPres_child  v sChildNamePresent sChildRetVal sChoiceTypeName 
        override this.ChoiceDependencyStrPres_child  (v:string) (sChildNamePresent:string) (sChildRetVal:string) (arruChildRetValBytes:seq<byte>) (arrsNullChars:seq<string>) (sChoiceTypeName:string) (sInsertedFieldTypeName:string) =
            acn_rust.ChoiceDependencyStrPres_child  v sChildNamePresent sChildRetVal arruChildRetValBytes arrsNullChars sChoiceTypeName sInsertedFieldTypeName 
        override this.ChoiceDependencyPres  (v:string) (sChPath:string) (sAcc:string) (arrsChoiceItems:seq<string>) (sChoiceTypeName:string) =
            acn_rust.ChoiceDependencyPres  v sChPath sAcc arrsChoiceItems sChoiceTypeName 
        override this.MultiAcnUpdate_checkEqual_pri0  (p1:string) (p2:string) =
            acn_rust.MultiAcnUpdate_checkEqual_pri0  p1 p2 
        override this.MultiAcnUpdate_checkEqual_str0  (p1:string) (p2:string) =
            acn_rust.MultiAcnUpdate_checkEqual_str0  p1 p2 
        override this.MultiAcnUpdate_get_first_init_value_pri  (sV0:string) (sVi:string) (sChPath:string) (bIsFirst:bool) (bIsSingleElement:bool) =
            acn_rust.MultiAcnUpdate_get_first_init_value_pri  sV0 sVi sChPath bIsFirst bIsSingleElement 
        override this.MultiAcnUpdate_get_first_init_value_str  (sV0:string) (sVi:string) (sChPath:string) (bIsFirst:bool) (bIsSingleElement:bool) =
            acn_rust.MultiAcnUpdate_get_first_init_value_str  sV0 sVi sChPath bIsFirst bIsSingleElement 
        override this.MultiAcnUpdate_checkEqual_pri  (sV0:string) (sVi:string) (sChPath:string) (bIsAlwaysInit:bool) =
            acn_rust.MultiAcnUpdate_checkEqual_pri  sV0 sVi sChPath bIsAlwaysInit 
        override this.MultiAcnUpdate_checkEqual_str  (sV0:string) (sVi:string) (sChPath:string) (bIsAlwaysInit:bool) =
            acn_rust.MultiAcnUpdate_checkEqual_str  sV0 sVi sChPath bIsAlwaysInit 
        override this.MultiAcnUpdate  (v:string) (sV0:string) (sErrCode:string) (arrsLocalDeclarations:seq<string>) (arrsLocalUpdateStatements:seq<string>) (arrsGetFirstIntValue:seq<string>) (bIsFirstIntValueSingle:bool) (arrsLocalCheckEquality:seq<string>) (sDefaultExpr:string) =
            acn_rust.MultiAcnUpdate  v sV0 sErrCode arrsLocalDeclarations arrsLocalUpdateStatements arrsGetFirstIntValue bIsFirstIntValueSingle arrsLocalCheckEquality sDefaultExpr 
        override this.checkAccessPath  (arrsCheckPaths:seq<string>) (sUpdateStatement:string) (v:string) (sInitExpr:string) =
            acn_rust.checkAccessPath  arrsCheckPaths sUpdateStatement v sInitExpr 
        override this.SizeDependency_oct_str_containing  (p:string) (sFuncName:string) (sReqBytesForUperEncoding:string) (v:string) (bIsOctet:bool) (sInner:string) (sLocalVarType:string) =
            acn_rust.SizeDependency_oct_str_containing  p sFuncName sReqBytesForUperEncoding v bIsOctet sInner sLocalVarType 
        override this.octet_string_containing_ext_field_func  (p:string) (sFuncName:string) (sReqBytesForUperEncoding:string) (sExtField:string) (sErrCode:string) (soInner:string option) (codec:Codec) =
            acn_rust.octet_string_containing_ext_field_func  p sFuncName sReqBytesForUperEncoding sExtField sErrCode soInner codec
        override this.octet_string_containing_deferred_func  (p:string) (sFuncName:string) (sDetParamName:string) (sPatchFuncName:string) (soMF:string option) (soMFM:string option) (sErrCode:string) (bDeducedContent:bool) (codec:Codec) =
            acn_rust.octet_string_containing_deferred_func  p sFuncName sDetParamName sPatchFuncName soMF soMFM sErrCode bDeducedContent codec
        override this.octet_string_containing_deferred_fixed_func  (p:string) (sFuncName:string) (codec:Codec) =
            acn_rust.octet_string_containing_deferred_fixed_func  p sFuncName codec
        override this.octet_string_containing_deferred_embedded_func  (p:string) (sFuncName:string) (nMinSize:BigInteger) (nMaxSize:BigInteger) (nBits:BigInteger) (codec:Codec) =
            acn_rust.octet_string_containing_deferred_embedded_func  p sFuncName nMinSize nMaxSize nBits codec
        override this.octet_string_containing_deferred_wrapper  (sBody:string) (sDetParamName:string) (sPatchFuncName:string) (soMF:string option) (soMFM:string option) (sErrCode:string) (bDeducedContent:bool) (codec:Codec) =
            acn_rust.octet_string_containing_deferred_wrapper  sBody sDetParamName sPatchFuncName soMF soMFM sErrCode bDeducedContent codec
        override this.bit_string_containing_deferred_func  (p:string) (sFuncName:string) (sDetParamName:string) (sPatchFuncName:string) (soMF:string option) (soMFM:string option) (sErrCode:string) (codec:Codec) =
            acn_rust.bit_string_containing_deferred_func  p sFuncName sDetParamName sPatchFuncName soMF soMFM sErrCode codec
        override this.bit_string_containing_deferred_fixed_func  (p:string) (sFuncName:string) (codec:Codec) =
            acn_rust.bit_string_containing_deferred_fixed_func  p sFuncName codec
        override this.bit_string_containing_deferred_embedded_func  (p:string) (sFuncName:string) (nMinSize:BigInteger) (nMaxSize:BigInteger) (nBits:BigInteger) (codec:Codec) =
            acn_rust.bit_string_containing_deferred_embedded_func  p sFuncName nMinSize nMaxSize nBits codec
        override this.bit_string_containing_deferred_wrapper  (sBody:string) (sDetParamName:string) (sPatchFuncName:string) (soMF:string option) (soMFM:string option) (sErrCode:string) (codec:Codec) =
            acn_rust.bit_string_containing_deferred_wrapper  sBody sDetParamName sPatchFuncName soMF soMFM sErrCode codec
        override this.bit_string_containing_ext_field_func  (p:string) (sFuncName:string) (sReqBytesForUperEncoding:string) (sReqBitsForUperEncoding:string) (sExtField:string) (sErrCode:string) (codec:Codec) =
            acn_rust.bit_string_containing_ext_field_func  p sFuncName sReqBytesForUperEncoding sReqBitsForUperEncoding sExtField sErrCode codec
        override this.rtlModuleName  () =
            acn_rust.rtlModuleName  () 
        override this.sparkAnnotations  (sTypeDefName:string) (codec:Codec) =
            acn_rust.sparkAnnotations  sTypeDefName codec
        override this.octet_string_containing_func  (p:string) (sFuncName:string) (sContainedType:string) (sReqBytesForAcnEncoding:string) (nBits:BigInteger) (nMinSize:BigInteger) (nMaxSize:BigInteger) (bFixedSize:bool) (codec:Codec) =
            acn_rust.octet_string_containing_func  p sFuncName sContainedType sReqBytesForAcnEncoding nBits nMinSize nMaxSize bFixedSize codec
        override this.bit_string_containing_func  (p:string) (sFuncName:string) (sContainedType:string) (sReqBytesForAcnEncoding:string) (sReqBitsForUperEncoding:string) (nBits:BigInteger) (nMinSize:BigInteger) (nMaxSize:BigInteger) (bFixedSize:bool) (codec:Codec) =
            acn_rust.bit_string_containing_func  p sFuncName sContainedType sReqBytesForAcnEncoding sReqBitsForUperEncoding nBits nMinSize nMaxSize bFixedSize codec
        override this.acn_deferred_det_declare  (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_declare  sDetName codec
        override this.acn_deferred_det_init_value  (sInitFuncName:string) (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_init_value  sInitFuncName sDetName codec
        override this.acn_deferred_det_init_ptr  (sInitFuncName:string) (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_init_ptr  sInitFuncName sDetName codec
        override this.acn_deferred_det_patch_value  (sPatchFuncName:string) (sValue:string) (sDetName:string) (sErrCode:string) (codec:Codec) =
            acn_rust.acn_deferred_det_patch_value  sPatchFuncName sValue sDetName sErrCode codec
        override this.acn_deferred_det_patch_ptr  (sPatchFuncName:string) (sValue:string) (sDetName:string) (sErrCode:string) (codec:Codec) =
            acn_rust.acn_deferred_det_patch_ptr  sPatchFuncName sValue sDetName sErrCode codec
        override this.acn_deferred_det_init_value_with_size  (sInitFuncName:string) (nBits:BigInteger) (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_init_value_with_size  sInitFuncName nBits sDetName codec
        override this.acn_deferred_det_init_ptr_with_size  (sInitFuncName:string) (nBits:BigInteger) (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_init_ptr_with_size  sInitFuncName nBits sDetName codec
        override this.acn_deferred_det_patch_value_with_size  (sPatchFuncName:string) (nBits:BigInteger) (sValue:string) (sDetName:string) (sErrCode:string) (codec:Codec) =
            acn_rust.acn_deferred_det_patch_value_with_size  sPatchFuncName nBits sValue sDetName sErrCode codec
        override this.acn_deferred_det_patch_ptr_with_size  (sPatchFuncName:string) (nBits:BigInteger) (sValue:string) (sDetName:string) (sErrCode:string) (codec:Codec) =
            acn_rust.acn_deferred_det_patch_ptr_with_size  sPatchFuncName nBits sValue sDetName sErrCode codec
        override this.acn_deferred_det_patch_value_str  (sPatchFuncName:string) (nBits:BigInteger) (sValue:string) (sDetName:string) (sErrCode:string) (codec:Codec) =
            acn_rust.acn_deferred_det_patch_value_str  sPatchFuncName nBits sValue sDetName sErrCode codec
        override this.acn_deferred_det_patch_ptr_str  (sPatchFuncName:string) (nBits:BigInteger) (sValue:string) (sDetName:string) (sErrCode:string) (codec:Codec) =
            acn_rust.acn_deferred_det_patch_ptr_str  sPatchFuncName nBits sValue sDetName sErrCode codec
        override this.acn_deferred_det_formal_param  (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_formal_param  sDetName codec
        override this.acn_deferred_det_actual_param  (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_actual_param  sDetName codec
        override this.acn_deferred_det_save_pos  (sVarName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_save_pos  sVarName codec
        override this.acn_deferred_det_distance_bytes  (sStart:string) (sEnd:string) (codec:Codec) =
            acn_rust.acn_deferred_det_distance_bytes  sStart sEnd codec
        override this.acn_deferred_det_kind_access  (p:string) (sAcc:string) =
            acn_rust.acn_deferred_det_kind_access  p sAcc 
        override this.acn_deferred_det_value_presence_bool  (p:string) (sAcc:string) (sChildName:string) =
            acn_rust.acn_deferred_det_value_presence_bool  p sAcc sChildName 
        override this.acn_deferred_det_switch_case_int  (sCaseName:string) (sVarName:string) (sValue:string) =
            acn_rust.acn_deferred_det_switch_case_int  sCaseName sVarName sValue 
        override this.acn_deferred_det_switch_int  (sVarName:string) (sKindAccess:string) (arrsCaseItems:seq<string>) =
            acn_rust.acn_deferred_det_switch_int  sVarName sKindAccess arrsCaseItems 
        override this.acn_deferred_det_switch_case_str  (sCaseName:string) (sVarName:string) (sValue:string) =
            acn_rust.acn_deferred_det_switch_case_str  sCaseName sVarName sValue 
        override this.acn_deferred_det_switch_str  (sVarName:string) (sKindAccess:string) (arrsCaseItems:seq<string>) =
            acn_rust.acn_deferred_det_switch_str  sVarName sKindAccess arrsCaseItems 
        override this.acn_deferred_det_fallback_value  (sPatchFuncName:string) (sDefaultVal:string) (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_fallback_value  sPatchFuncName sDefaultVal sDetName codec
        override this.acn_deferred_det_fallback_value_with_size  (sPatchFuncName:string) (sDefaultVal:string) (nBits:BigInteger) (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_fallback_value_with_size  sPatchFuncName sDefaultVal nBits sDetName codec
        override this.acn_deferred_det_fallback_value_str  (sPatchFuncName:string) (sDefaultVal:string) (nBits:BigInteger) (sDetName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_fallback_value_str  sPatchFuncName sDefaultVal nBits sDetName codec
        override this.acn_deferred_det_copy_tmp  (sDetAccess:string) (sTmpName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_copy_tmp  sDetAccess sTmpName codec
        override this.acn_deferred_det_copy_bool_tmp  (sDetAccess:string) (sTmpName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_copy_bool_tmp  sDetAccess sTmpName codec
        override this.acn_deferred_det_copy_enum_tmp  (sDetAccess:string) (sTmpName:string) (sEnumTypeName:string) (codec:Codec) =
            acn_rust.acn_deferred_det_copy_enum_tmp  sDetAccess sTmpName sEnumTypeName codec
        override this.acn_deferred_det_access_value  (sDetName:string) =
            acn_rust.acn_deferred_det_access_value  sDetName 
        override this.acn_deferred_det_access_ptr  (sDetName:string) =
            acn_rust.acn_deferred_det_access_ptr  sDetName 
        override this.acn_deferred_det_access_bool_ptr  (sDetName:string) =
            acn_rust.acn_deferred_det_access_bool_ptr  sDetName 
        override this.acn_deferred_det_access_str_value  (sDetName:string) =
            acn_rust.acn_deferred_det_access_str_value  sDetName 
        override this.acn_deferred_det_access_str_ptr  (sDetName:string) =
            acn_rust.acn_deferred_det_access_str_ptr  sDetName 
        override this.acn_deferred_det_relative_access  (sRootId:string) (sFieldPath:string) =
            acn_rust.acn_deferred_det_relative_access  sRootId sFieldPath 
        override this.acn_deferred_det_uper_offset_sub  (sValue:string) (sOffset:string) =
            acn_rust.acn_deferred_det_uper_offset_sub  sValue sOffset 
        override this.acn_deferred_det_preblock_wrap  (sPreBlock:string) (sPatchCall:string) =
            acn_rust.acn_deferred_det_preblock_wrap  sPreBlock sPatchCall 
        override this.acn_deferred_det_type_name  () =
            acn_rust.acn_deferred_det_type_name  () 
        override this.acn_deferred_det_init_expr  () =
            acn_rust.acn_deferred_det_init_expr  () 
