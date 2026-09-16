module acn_rust
open System
open System.Numerics
open CommonTypes

let getStringSize (p:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "getStringSize" [("p",p :>Object)]

let getSizeableSize (p:string) (sAcc:string) (bIsUnsigned:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "getSizeableSize" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("bIsUnsigned",bIsUnsigned :>Object)]

let EmitTypeAssignment_def_err_code (sErrCode:string) (nErrValue:BigInteger) (soErrorCodeComment:string option) (sFieldPath:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "EmitTypeAssignment_def_err_code" [("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nErrValue",nErrValue :>Object);("soErrorCodeComment",(if soErrorCodeComment.IsNone then null else ST.StrHelper soErrorCodeComment.Value:>Object) );("sFieldPath",(if sFieldPath = null then null else ST.StrHelper sFieldPath:>Object) )]

let EmitAcnParameter (sName:string) (sType:string) (sDefaultValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "EmitAcnParameter" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) );("sDefaultValue",(if sDefaultValue = null then null else ST.StrHelper sDefaultValue:>Object) )]

let EmitEncodingSizeConstants (sTypeDefName:string) (nMaxBytesInACN:BigInteger) (nMaxBitsInACN:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "EmitEncodingSizeConstants" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("nMaxBytesInACN",nMaxBytesInACN :>Object);("nMaxBitsInACN",nMaxBitsInACN :>Object)]

let EmitTypeAssignment_primitive_def (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) (bEmptyEncodingSpace:bool) (nMaxBytesInACN:BigInteger) (nMaxBitsInACN:BigInteger) (arrsAcnPrms:seq<string>) (soSparkAnnotations:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "EmitTypeAssignment_primitive_def_encode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsErrcodes",(arrsErrcodes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bEmptyEncodingSpace",bEmptyEncodingSpace :>Object);("nMaxBytesInACN",nMaxBytesInACN :>Object);("nMaxBitsInACN",nMaxBitsInACN :>Object);("arrsAcnPrms",(arrsAcnPrms|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "EmitTypeAssignment_primitive_def_decode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsErrcodes",(arrsErrcodes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bEmptyEncodingSpace",bEmptyEncodingSpace :>Object);("nMaxBytesInACN",nMaxBytesInACN :>Object);("nMaxBitsInACN",nMaxBitsInACN :>Object);("arrsAcnPrms",(arrsAcnPrms|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) )]

let EmitTypeAssignment_primitive (sVarName:string) (sStar:string) (sFuncName:string) (soIValidFuncName:string option) (sTypeDefName:string) (arrsLocalVariables:seq<string>) (sContent:string) (soSparkAnnotations:string option) (sInitialExp:string) (arrsAcnPrms:seq<string>) (arrsAcnParamNames:seq<string>) (bEmptyEncodingSpace:bool) (bBsIsUnreferenced:bool) (bVarNameIsUnreferenced:bool) (bHasAcnChildrenToReturn:bool) (soInitFuncName:string option) (arrsAnnots:seq<string>) (arrsPrecond:seq<string>) (arrsPostcond:seq<string>) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "EmitTypeAssignment_primitive_encode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("soIValidFuncName",(if soIValidFuncName.IsNone then null else ST.StrHelper soIValidFuncName.Value:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsLocalVariables",(arrsLocalVariables|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) );("sInitialExp",(if sInitialExp = null then null else ST.StrHelper sInitialExp:>Object) );("arrsAcnPrms",(arrsAcnPrms|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsAcnParamNames",(arrsAcnParamNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bEmptyEncodingSpace",bEmptyEncodingSpace :>Object);("bBsIsUnreferenced",bBsIsUnreferenced :>Object);("bVarNameIsUnreferenced",bVarNameIsUnreferenced :>Object);("bHasAcnChildrenToReturn",bHasAcnChildrenToReturn :>Object);("soInitFuncName",(if soInitFuncName.IsNone then null else ST.StrHelper soInitFuncName.Value:>Object) );("arrsAnnots",(arrsAnnots|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPrecond",(arrsPrecond|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPostcond",(arrsPostcond|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]
    | Decode    ->
        ST.call "acn_rust" "EmitTypeAssignment_primitive_decode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("soIValidFuncName",(if soIValidFuncName.IsNone then null else ST.StrHelper soIValidFuncName.Value:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsLocalVariables",(arrsLocalVariables|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) );("sInitialExp",(if sInitialExp = null then null else ST.StrHelper sInitialExp:>Object) );("arrsAcnPrms",(arrsAcnPrms|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsAcnParamNames",(arrsAcnParamNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bEmptyEncodingSpace",bEmptyEncodingSpace :>Object);("bBsIsUnreferenced",bBsIsUnreferenced :>Object);("bVarNameIsUnreferenced",bVarNameIsUnreferenced :>Object);("bHasAcnChildrenToReturn",bHasAcnChildrenToReturn :>Object);("soInitFuncName",(if soInitFuncName.IsNone then null else ST.StrHelper soInitFuncName.Value:>Object) );("arrsAnnots",(arrsAnnots|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPrecond",(arrsPrecond|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPostcond",(arrsPostcond|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let MappingFunctionDeclaration (sTypeName:string) (sMF:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "MappingFunctionDeclaration_encode" [("sTypeName",(if sTypeName = null then null else ST.StrHelper sTypeName:>Object) );("sMF",(if sMF = null then null else ST.StrHelper sMF:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "MappingFunctionDeclaration_decode" [("sTypeName",(if sTypeName = null then null else ST.StrHelper sTypeName:>Object) );("sMF",(if sMF = null then null else ST.StrHelper sMF:>Object) )]

let alignToNext (sMainBody:string) (sAlignmentValue:string) (nAlignmentValue:BigInteger) (nAbsOffset:BigInteger) (nRemainingMinBits:BigInteger) (nLevel:BigInteger) (nIx:BigInteger) (nOffset:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "alignToNext_encode" [("sMainBody",(if sMainBody = null then null else ST.StrHelper sMainBody:>Object) );("sAlignmentValue",(if sAlignmentValue = null then null else ST.StrHelper sAlignmentValue:>Object) );("nAlignmentValue",nAlignmentValue :>Object);("nAbsOffset",nAbsOffset :>Object);("nRemainingMinBits",nRemainingMinBits :>Object);("nLevel",nLevel :>Object);("nIx",nIx :>Object);("nOffset",nOffset :>Object)]
    | Decode    ->
        ST.call "acn_rust" "alignToNext_decode" [("sMainBody",(if sMainBody = null then null else ST.StrHelper sMainBody:>Object) );("sAlignmentValue",(if sAlignmentValue = null then null else ST.StrHelper sAlignmentValue:>Object) );("nAlignmentValue",nAlignmentValue :>Object);("nAbsOffset",nAbsOffset :>Object);("nRemainingMinBits",nRemainingMinBits :>Object);("nLevel",nLevel :>Object);("nIx",nIx :>Object);("nOffset",nOffset :>Object)]

let PositiveInteger_ConstSize (p:string) (sSsuffix:string) (sErrCode:string) (nFixedSize:BigInteger) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nFixedSize",nFixedSize :>Object);("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nFixedSize",nFixedSize :>Object);("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PositiveInteger_ConstSize_8 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_8_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_8_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PositiveInteger_ConstSize_big_endian_16 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_big_endian_16_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_big_endian_16_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PositiveInteger_ConstSize_big_endian_32 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_big_endian_32_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_big_endian_32_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PositiveInteger_ConstSize_big_endian_64 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_big_endian_64_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_big_endian_64_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PositiveInteger_ConstSize_little_endian_16 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_little_endian_16_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_little_endian_16_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PositiveInteger_ConstSize_little_endian_32 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_little_endian_32_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_little_endian_32_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PositiveInteger_ConstSize_little_endian_64 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_little_endian_64_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_ConstSize_little_endian_64_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PositiveInteger_VarSize_LengthEmbedded (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "PositiveInteger_VarSize_LengthEmbedded_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "PositiveInteger_VarSize_LengthEmbedded_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_ConstSize (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nFixedSize:BigInteger) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nFixedSize",nFixedSize :>Object);("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nFixedSize",nFixedSize :>Object);("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_ConstSize_8 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_8_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_8_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_ConstSize_big_endian_16 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_big_endian_16_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_big_endian_16_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_ConstSize_big_endian_32 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_big_endian_32_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_big_endian_32_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_ConstSize_big_endian_64 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_big_endian_64_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_big_endian_64_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_ConstSize_little_endian_16 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_little_endian_16_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_little_endian_16_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_ConstSize_little_endian_32 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_little_endian_32_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_little_endian_32_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_ConstSize_little_endian_64 (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_little_endian_64_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_ConstSize_little_endian_64_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let TwosComplement_VarSize_LengthEmbedded (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "TwosComplement_VarSize_LengthEmbedded_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "TwosComplement_VarSize_LengthEmbedded_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let BCD_ConstSize (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (nNibbles:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "BCD_ConstSize_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("nNibbles",nNibbles :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "BCD_ConstSize_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("nNibbles",nNibbles :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let BCD_VarSize_LengthEmbedded (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "BCD_VarSize_LengthEmbedded_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "BCD_VarSize_LengthEmbedded_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let BCD_VarSize_NullTerminated (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "BCD_VarSize_NullTerminated_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "BCD_VarSize_NullTerminated_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let ASCII_ConstSize (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (nSizeInBytes:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ASCII_ConstSize_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("nSizeInBytes",nSizeInBytes :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "ASCII_ConstSize_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("nSizeInBytes",nSizeInBytes :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let ASCII_VarSize_LengthEmbedded (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ASCII_VarSize_LengthEmbedded_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "ASCII_VarSize_LengthEmbedded_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let ASCII_VarSize_NullTerminated (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (arruNullBytes:seq<byte>) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ASCII_VarSize_NullTerminated_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "ASCII_VarSize_NullTerminated_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let ASCII_UINT_ConstSize (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (nSizeInBytes:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ASCII_UINT_ConstSize_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("nSizeInBytes",nSizeInBytes :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "ASCII_UINT_ConstSize_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("nSizeInBytes",nSizeInBytes :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let ASCII_UINT_VarSize_NullTerminated (p:string) (sSsuffix:string) (sErrCode:string) (soMF:string option) (soMFM:string option) (nUperMin:BigInteger) (nUperMax:BigInteger) (arruNullBytes:seq<byte>) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ASCII_UINT_VarSize_NullTerminated_encode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "ASCII_UINT_VarSize_NullTerminated_decode" [("p",p :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soMF",(if soMF.IsNone then null else ST.StrHelper soMF.Value:>Object) );("soMFM",(if soMFM.IsNone then null else ST.StrHelper soMFM.Value:>Object) );("nUperMin",nUperMin :>Object);("nUperMax",nUperMax :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Real_32_big_endian (p:string) (sSuffix:string) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Real_32_big_endian_encode" [("p",p :>Object);("sSuffix",(if sSuffix = null then null else ST.StrHelper sSuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Real_32_big_endian_decode" [("p",p :>Object);("sSuffix",(if sSuffix = null then null else ST.StrHelper sSuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Real_64_big_endian (p:string) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Real_64_big_endian_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Real_64_big_endian_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Real_32_little_endian (p:string) (sSuffix:string) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Real_32_little_endian_encode" [("p",p :>Object);("sSuffix",(if sSuffix = null then null else ST.StrHelper sSuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Real_32_little_endian_decode" [("p",p :>Object);("sSuffix",(if sSuffix = null then null else ST.StrHelper sSuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Real_64_little_endian (p:string) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Real_64_little_endian_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Real_64_little_endian_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Real_ScaledInt_uint (p:string) (sTmpVar:string) (sIntBody:string) (sLow:string) (sScale:string) (sUintMax:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Real_ScaledInt_uint_encode" [("p",p :>Object);("sTmpVar",(if sTmpVar = null then null else ST.StrHelper sTmpVar:>Object) );("sIntBody",(if sIntBody = null then null else ST.StrHelper sIntBody:>Object) );("sLow",(if sLow = null then null else ST.StrHelper sLow:>Object) );("sScale",(if sScale = null then null else ST.StrHelper sScale:>Object) );("sUintMax",(if sUintMax = null then null else ST.StrHelper sUintMax:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Real_ScaledInt_uint_decode" [("p",p :>Object);("sTmpVar",(if sTmpVar = null then null else ST.StrHelper sTmpVar:>Object) );("sIntBody",(if sIntBody = null then null else ST.StrHelper sIntBody:>Object) );("sLow",(if sLow = null then null else ST.StrHelper sLow:>Object) );("sScale",(if sScale = null then null else ST.StrHelper sScale:>Object) );("sUintMax",(if sUintMax = null then null else ST.StrHelper sUintMax:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let Real_ScaledInt_sint (p:string) (sTmpVar:string) (sIntBody:string) (sLow:string) (sScale:string) (sIntMin:string) (sIntMax:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Real_ScaledInt_sint_encode" [("p",p :>Object);("sTmpVar",(if sTmpVar = null then null else ST.StrHelper sTmpVar:>Object) );("sIntBody",(if sIntBody = null then null else ST.StrHelper sIntBody:>Object) );("sLow",(if sLow = null then null else ST.StrHelper sLow:>Object) );("sScale",(if sScale = null then null else ST.StrHelper sScale:>Object) );("sIntMin",(if sIntMin = null then null else ST.StrHelper sIntMin:>Object) );("sIntMax",(if sIntMax = null then null else ST.StrHelper sIntMax:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Real_ScaledInt_sint_decode" [("p",p :>Object);("sTmpVar",(if sTmpVar = null then null else ST.StrHelper sTmpVar:>Object) );("sIntBody",(if sIntBody = null then null else ST.StrHelper sIntBody:>Object) );("sLow",(if sLow = null then null else ST.StrHelper sLow:>Object) );("sScale",(if sScale = null then null else ST.StrHelper sScale:>Object) );("sIntMin",(if sIntMin = null then null else ST.StrHelper sIntMin:>Object) );("sIntMax",(if sIntMax = null then null else ST.StrHelper sIntMax:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let Boolean (p:string) (ptr:string) (bEncValIsTrue:bool) (nSize:BigInteger) (arruTrueValueAsByteArray:seq<byte>) (arruFalseValueAsByteArray:seq<byte>) (arrsBits:seq<string>) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Boolean_encode" [("p",p :>Object);("ptr",ptr :>Object);("bEncValIsTrue",bEncValIsTrue :>Object);("nSize",nSize :>Object);("arruTrueValueAsByteArray",arruTrueValueAsByteArray|>Seq.toArray :>Object);("arruFalseValueAsByteArray",arruFalseValueAsByteArray|>Seq.toArray :>Object);("arrsBits",(arrsBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Boolean_decode" [("p",p :>Object);("ptr",ptr :>Object);("bEncValIsTrue",bEncValIsTrue :>Object);("nSize",nSize :>Object);("arruTrueValueAsByteArray",arruTrueValueAsByteArray|>Seq.toArray :>Object);("arruFalseValueAsByteArray",arruFalseValueAsByteArray|>Seq.toArray :>Object);("arrsBits",(arrsBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let BooleanTrueFalse (p:string) (ptr:string) (nSize:BigInteger) (arruTrueValueAsByteArray:seq<byte>) (arruFalseValueAsByteArray:seq<byte>) (arrsTrueBits:seq<string>) (arrsFalseBits:seq<string>) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "BooleanTrueFalse_encode" [("p",p :>Object);("ptr",ptr :>Object);("nSize",nSize :>Object);("arruTrueValueAsByteArray",arruTrueValueAsByteArray|>Seq.toArray :>Object);("arruFalseValueAsByteArray",arruFalseValueAsByteArray|>Seq.toArray :>Object);("arrsTrueBits",(arrsTrueBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsFalseBits",(arrsFalseBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "BooleanTrueFalse_decode" [("p",p :>Object);("ptr",ptr :>Object);("nSize",nSize :>Object);("arruTrueValueAsByteArray",arruTrueValueAsByteArray|>Seq.toArray :>Object);("arruFalseValueAsByteArray",arruFalseValueAsByteArray|>Seq.toArray :>Object);("arrsTrueBits",(arrsTrueBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsFalseBits",(arrsFalseBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Null_declare (p:string) (sType:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "Null_declare" [("p",p :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Null_pattern (p:string) (arruNullValueAsByteArray:seq<byte>) (nSize:BigInteger) (arrsBits:seq<string>) (sErrCode:string) (bSavePosition:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Null_pattern_encode" [("p",p :>Object);("arruNullValueAsByteArray",arruNullValueAsByteArray|>Seq.toArray :>Object);("nSize",nSize :>Object);("arrsBits",(arrsBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bSavePosition",bSavePosition :>Object)]
    | Decode    ->
        ST.call "acn_rust" "Null_pattern_decode" [("p",p :>Object);("arruNullValueAsByteArray",arruNullValueAsByteArray|>Seq.toArray :>Object);("nSize",nSize :>Object);("arrsBits",(arrsBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bSavePosition",bSavePosition :>Object)]

let Null_pattern2 (p:string) (arruNullValueAsByteArray:seq<byte>) (nSize:BigInteger) (arrsBits:seq<string>) (sErrCode:string) (bSavePosition:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Null_pattern2_encode" [("p",p :>Object);("arruNullValueAsByteArray",arruNullValueAsByteArray|>Seq.toArray :>Object);("nSize",nSize :>Object);("arrsBits",(arrsBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bSavePosition",bSavePosition :>Object)]
    | Decode    ->
        ST.call "acn_rust" "Null_pattern2_decode" [("p",p :>Object);("arruNullValueAsByteArray",arruNullValueAsByteArray|>Seq.toArray :>Object);("nSize",nSize :>Object);("arrsBits",(arrsBits|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bSavePosition",bSavePosition :>Object)]

let Enumerated_item (p:string) (sName:string) (sEnumHolder:string) (nItemIdx:BigInteger) (sItemVal:string) (sIntVal:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Enumerated_item_encode" [("p",p :>Object);("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sEnumHolder",(if sEnumHolder = null then null else ST.StrHelper sEnumHolder:>Object) );("nItemIdx",nItemIdx :>Object);("sItemVal",(if sItemVal = null then null else ST.StrHelper sItemVal:>Object) );("sIntVal",(if sIntVal = null then null else ST.StrHelper sIntVal:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Enumerated_item_decode" [("p",p :>Object);("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sEnumHolder",(if sEnumHolder = null then null else ST.StrHelper sEnumHolder:>Object) );("nItemIdx",nItemIdx :>Object);("sItemVal",(if sItemVal = null then null else ST.StrHelper sItemVal:>Object) );("sIntVal",(if sIntVal = null then null else ST.StrHelper sIntVal:>Object) )]

let EnumeratedEncIdx (p:string) (td:FE_EnumeratedTypeDefinition) (arrsItem:seq<string>) (sActualCodecFunc:string) (sIntVal:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "EnumeratedEncIdx_encode" [("p",p :>Object);("td",td :>Object);("arrsItem",(arrsItem|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sActualCodecFunc",(if sActualCodecFunc = null then null else ST.StrHelper sActualCodecFunc:>Object) );("sIntVal",(if sIntVal = null then null else ST.StrHelper sIntVal:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "EnumeratedEncIdx_decode" [("p",p :>Object);("td",td :>Object);("arrsItem",(arrsItem|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sActualCodecFunc",(if sActualCodecFunc = null then null else ST.StrHelper sActualCodecFunc:>Object) );("sIntVal",(if sIntVal = null then null else ST.StrHelper sIntVal:>Object) )]

let EnumeratedEncValues (p:string) (td:FE_EnumeratedTypeDefinition) (arrsItem:seq<string>) (sActualCodecFunc:string) (sErrCode:string) (sFirstItemName:string) (sIntVal:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "EnumeratedEncValues_encode" [("p",p :>Object);("td",td :>Object);("arrsItem",(arrsItem|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sActualCodecFunc",(if sActualCodecFunc = null then null else ST.StrHelper sActualCodecFunc:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sFirstItemName",(if sFirstItemName = null then null else ST.StrHelper sFirstItemName:>Object) );("sIntVal",(if sIntVal = null then null else ST.StrHelper sIntVal:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "EnumeratedEncValues_decode" [("p",p :>Object);("td",td :>Object);("arrsItem",(arrsItem|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sActualCodecFunc",(if sActualCodecFunc = null then null else ST.StrHelper sActualCodecFunc:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sFirstItemName",(if sFirstItemName = null then null else ST.StrHelper sFirstItemName:>Object) );("sIntVal",(if sIntVal = null then null else ST.StrHelper sIntVal:>Object) )]

let EnumeratedEncValues_no_switch (p:string) (td:FE_EnumeratedTypeDefinition) (sActualCodecFunc:string) (sErrCode:string) (sFirstItemName:string) (sIntVal:string) (sEnumIndex:string) (nLastItemIndex:BigInteger) (bEncodeValues:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "EnumeratedEncValues_no_switch_encode" [("p",p :>Object);("td",td :>Object);("sActualCodecFunc",(if sActualCodecFunc = null then null else ST.StrHelper sActualCodecFunc:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sFirstItemName",(if sFirstItemName = null then null else ST.StrHelper sFirstItemName:>Object) );("sIntVal",(if sIntVal = null then null else ST.StrHelper sIntVal:>Object) );("sEnumIndex",(if sEnumIndex = null then null else ST.StrHelper sEnumIndex:>Object) );("nLastItemIndex",nLastItemIndex :>Object);("bEncodeValues",bEncodeValues :>Object)]
    | Decode    ->
        ST.call "acn_rust" "EnumeratedEncValues_no_switch_decode" [("p",p :>Object);("td",td :>Object);("sActualCodecFunc",(if sActualCodecFunc = null then null else ST.StrHelper sActualCodecFunc:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sFirstItemName",(if sFirstItemName = null then null else ST.StrHelper sFirstItemName:>Object) );("sIntVal",(if sIntVal = null then null else ST.StrHelper sIntVal:>Object) );("sEnumIndex",(if sEnumIndex = null then null else ST.StrHelper sEnumIndex:>Object) );("nLastItemIndex",nLastItemIndex :>Object);("bEncodeValues",bEncodeValues :>Object)]

let Acn_String_Ascii_FixSize (p:string) (sErrCode:string) (nAsn1Max:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Acn_String_Ascii_FixSize_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object)]
    | Decode    ->
        ST.call "acn_rust" "Acn_String_Ascii_FixSize_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object)]

let Acn_String_Ascii_Null_Terminated (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (arruNullBytes:seq<byte>) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Acn_String_Ascii_Null_Terminated_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Acn_String_Ascii_Null_Terminated_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Acn_String_Ascii_External_Field_Determinant (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (sExtFld:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Acn_String_Ascii_External_Field_Determinant_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Acn_String_Ascii_External_Field_Determinant_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Acn_String_Ascii_Internal_Field_Determinant (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (nAsn1Min:BigInteger) (nInternalLengthDeterminantSizeInBits:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Acn_String_Ascii_Internal_Field_Determinant_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("nAsn1Min",nAsn1Min :>Object);("nInternalLengthDeterminantSizeInBits",nInternalLengthDeterminantSizeInBits :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Acn_String_Ascii_Internal_Field_Determinant_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("nAsn1Min",nAsn1Min :>Object);("nInternalLengthDeterminantSizeInBits",nInternalLengthDeterminantSizeInBits :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Acn_String_CharIndex_FixSize (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (arrnAlphabetAsciiCodes:seq<BigInteger>) (nCharSetSize:BigInteger) (td:FE_StringTypeDefinition) (nCharSize:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Acn_String_CharIndex_FixSize_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("arrnAlphabetAsciiCodes",arrnAlphabetAsciiCodes|>Seq.toArray :>Object);("nCharSetSize",nCharSetSize :>Object);("td",td :>Object);("nCharSize",nCharSize :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Acn_String_CharIndex_FixSize_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("arrnAlphabetAsciiCodes",arrnAlphabetAsciiCodes|>Seq.toArray :>Object);("nCharSetSize",nCharSetSize :>Object);("td",td :>Object);("nCharSize",nCharSize :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Acn_String_CharIndex_External_Field_Determinant (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (arrnAlphabetAsciiCodes:seq<BigInteger>) (nCharSetSize:BigInteger) (sExtFld:string) (td:FE_StringTypeDefinition) (nCharSize:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Acn_String_CharIndex_External_Field_Determinant_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("arrnAlphabetAsciiCodes",arrnAlphabetAsciiCodes|>Seq.toArray :>Object);("nCharSetSize",nCharSetSize :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("td",td :>Object);("nCharSize",nCharSize :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Acn_String_CharIndex_External_Field_Determinant_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("arrnAlphabetAsciiCodes",arrnAlphabetAsciiCodes|>Seq.toArray :>Object);("nCharSetSize",nCharSetSize :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("td",td :>Object);("nCharSize",nCharSize :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Acn_IA5String_CharIndex_External_Field_Determinant (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (sExtFld:string) (td:FE_StringTypeDefinition) (nCharSize:BigInteger) (nRemainingBits:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Acn_IA5String_CharIndex_External_Field_Determinant_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("td",td :>Object);("nCharSize",nCharSize :>Object);("nRemainingBits",nRemainingBits :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Acn_IA5String_CharIndex_External_Field_Determinant_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("td",td :>Object);("nCharSize",nCharSize :>Object);("nRemainingBits",nRemainingBits :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let oct_external_field (sTypedefName:string) (p:string) (sAcc:string) (noSizeMin:BigInteger option) (noSizeMax:BigInteger option) (sExtFld:string) (bIsUnsigned:bool) (nAlignSize:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "oct_external_field_encode" [("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("noSizeMax",(if noSizeMax.IsNone then null else noSizeMax.Value:>Object) );("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("bIsUnsigned",bIsUnsigned :>Object);("nAlignSize",nAlignSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "oct_external_field_decode" [("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("noSizeMax",(if noSizeMax.IsNone then null else noSizeMax.Value:>Object) );("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("bIsUnsigned",bIsUnsigned :>Object);("nAlignSize",nAlignSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let oct_external_field_fix_size (sTypedefName:string) (p:string) (sAcc:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (sExtFld:string) (bIsUnsigned:bool) (nAlignSize:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "oct_external_field_fix_size_encode" [("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("bIsUnsigned",bIsUnsigned :>Object);("nAlignSize",nAlignSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "oct_external_field_fix_size_decode" [("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("bIsUnsigned",bIsUnsigned :>Object);("nAlignSize",nAlignSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let seqOf_VarSize (p:string) (sAcc:string) (sTasName:string) (i:string) (sInternalItem:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nSizeInBits:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (sChildInitExpr:string) (sErrCode:string) (nAbsOffset:BigInteger) (nRemainingMinBits:BigInteger) (nLevel:BigInteger) (nIx:BigInteger) (nOffset:BigInteger) (bIntroSnap:bool) (soCallAux:string option) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "seqOf_VarSize_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nSizeInBits",nSizeInBits :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAbsOffset",nAbsOffset :>Object);("nRemainingMinBits",nRemainingMinBits :>Object);("nLevel",nLevel :>Object);("nIx",nIx :>Object);("nOffset",nOffset :>Object);("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "seqOf_VarSize_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nSizeInBits",nSizeInBits :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAbsOffset",nAbsOffset :>Object);("nRemainingMinBits",nRemainingMinBits :>Object);("nLevel",nLevel :>Object);("nIx",nIx :>Object);("nOffset",nOffset :>Object);("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let sqf_external_field (sTypeDefName:string) (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (sExtFld:string) (bIsUnsigned:bool) (nAlignSize:BigInteger) (sErrCode:string) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (sChildInitExpr:string) (bIntroSnap:bool) (soCallAux:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sqf_external_field_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("bIsUnsigned",bIsUnsigned :>Object);("nAlignSize",nAlignSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sqf_external_field_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("bIsUnsigned",bIsUnsigned :>Object);("nAlignSize",nAlignSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]

let sqf_external_field_fix_size (sTypeDefName:string) (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (sExtFld:string) (bIsUnsigned:bool) (nAlignSize:BigInteger) (sErrCode:string) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (sChildInitExpr:string) (bIntroSnap:bool) (soCallAux:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sqf_external_field_fix_size_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("bIsUnsigned",bIsUnsigned :>Object);("nAlignSize",nAlignSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sqf_external_field_fix_size_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("bIsUnsigned",bIsUnsigned :>Object);("nAlignSize",nAlignSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]

let oct_sqf_null_terminated (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (arruNullBytes:seq<byte>) (nBitPatternLength:BigInteger) (sErrCode:string) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "oct_sqf_null_terminated_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("nBitPatternLength",nBitPatternLength :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "oct_sqf_null_terminated_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("nBitPatternLength",nBitPatternLength :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let str_ascii_deduced (p:string) (sErrCode:string) (nAsn1Max:BigInteger) (nTrailingBits:BigInteger) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "str_ascii_deduced_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("nTrailingBits",nTrailingBits :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "str_ascii_deduced_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAsn1Max",nAsn1Max :>Object);("nTrailingBits",nTrailingBits :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let oct_deduced (sTypedefName:string) (p:string) (sAcc:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (nTrailingBits:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "oct_deduced_encode" [("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("nTrailingBits",nTrailingBits :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "oct_deduced_decode" [("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("nTrailingBits",nTrailingBits :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let sqf_deduced_fix_elem (sTypeDefName:string) (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (nTrailingBits:BigInteger) (nIntItemSize:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sqf_deduced_fix_elem_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("nTrailingBits",nTrailingBits :>Object);("nIntItemSize",nIntItemSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sqf_deduced_fix_elem_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("nTrailingBits",nTrailingBits :>Object);("nIntItemSize",nIntItemSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let sqf_deduced_var_elem (sTypeDefName:string) (p:string) (sAcc:string) (i:string) (sInternalItem:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (nTrailingBits:BigInteger) (nIntItemMinSize:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sqf_deduced_var_elem_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("nTrailingBits",nTrailingBits :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sqf_deduced_var_elem_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("nTrailingBits",nTrailingBits :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let bit_string_external_field (sTypeDefName:string) (p:string) (sErrCode:string) (sAcc:string) (noSizeMin:BigInteger option) (noSizeMax:BigInteger option) (sExtFld:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_external_field_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("noSizeMax",(if noSizeMax.IsNone then null else noSizeMax.Value:>Object) );("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "bit_string_external_field_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("noSizeMax",(if noSizeMax.IsNone then null else noSizeMax.Value:>Object) );("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) )]

let bit_string_external_field_fixed_size (sTypeDefName:string) (p:string) (sErrCode:string) (sAcc:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (sExtFld:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_external_field_fixed_size_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "bit_string_external_field_fixed_size_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) )]

let bit_string_null_terminated (sTypeDefName:string) (p:string) (sErrCode:string) (sAcc:string) (i:string) (noSizeMin:BigInteger option) (nSizeMax:BigInteger) (arruNullBytes:seq<byte>) (nBitPatternLength:BigInteger) (bFixedSize:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_null_terminated_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("nBitPatternLength",nBitPatternLength :>Object);("bFixedSize",bFixedSize :>Object)]
    | Decode    ->
        ST.call "acn_rust" "bit_string_null_terminated_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("noSizeMin",(if noSizeMin.IsNone then null else noSizeMin.Value:>Object) );("nSizeMax",nSizeMax :>Object);("arruNullBytes",arruNullBytes|>Seq.toArray :>Object);("nBitPatternLength",nBitPatternLength :>Object);("bFixedSize",bFixedSize :>Object)]

let RefTypeParam_tmpVar (sName:string) (sTypeDecl:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "RefTypeParam_tmpVar" [("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("sTypeDecl",(if sTypeDecl = null then null else ST.StrHelper sTypeDecl:>Object) )]

let ReferenceType1 (p:string) (sName:string) (bAcnEncodeFuncRequiresResult:bool) (arrsArgs:seq<string>) (arrsLocalPrms:seq<string>) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ReferenceType1_encode" [("p",p :>Object);("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("bAcnEncodeFuncRequiresResult",bAcnEncodeFuncRequiresResult :>Object);("arrsArgs",(arrsArgs|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsLocalPrms",(arrsLocalPrms|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]
    | Decode    ->
        ST.call "acn_rust" "ReferenceType1_decode" [("p",p :>Object);("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("bAcnEncodeFuncRequiresResult",bAcnEncodeFuncRequiresResult :>Object);("arrsArgs",(arrsArgs|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsLocalPrms",(arrsLocalPrms|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let sequence_presence_optChild (p:string) (sAcc:string) (sChName:string) (soExistVar:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_presence_optChild_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sequence_presence_optChild_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let sequence_presence_optChild_pres_acn_expression (p:string) (sAcc:string) (sChName:string) (sAcnExpression:string) (soExistVar:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_presence_optChild_pres_acn_expression_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sAcnExpression",(if sAcnExpression = null then null else ST.StrHelper sAcnExpression:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sequence_presence_optChild_pres_acn_expression_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sAcnExpression",(if sAcnExpression = null then null else ST.StrHelper sAcnExpression:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let sequence_presence_optChild_pres_bool (p:string) (sAcc:string) (sChName:string) (sExtFldName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_presence_optChild_pres_bool_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sExtFldName",(if sExtFldName = null then null else ST.StrHelper sExtFldName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sequence_presence_optChild_pres_bool_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sExtFldName",(if sExtFldName = null then null else ST.StrHelper sExtFldName:>Object) )]

let sequence_presence_optChild_pres_int (p:string) (sAcc:string) (sChName:string) (sExtFldName:string) (nIntVal:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_presence_optChild_pres_int_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sExtFldName",(if sExtFldName = null then null else ST.StrHelper sExtFldName:>Object) );("nIntVal",nIntVal :>Object)]
    | Decode    ->
        ST.call "acn_rust" "sequence_presence_optChild_pres_int_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sExtFldName",(if sExtFldName = null then null else ST.StrHelper sExtFldName:>Object) );("nIntVal",nIntVal :>Object)]

let sequence_presence_optChild_pres_str (p:string) (sAcc:string) (sChName:string) (sExtFldName:string) (sVal:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_presence_optChild_pres_str_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sExtFldName",(if sExtFldName = null then null else ST.StrHelper sExtFldName:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sequence_presence_optChild_pres_str_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sExtFldName",(if sExtFldName = null then null else ST.StrHelper sExtFldName:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) )]

let sequence_save_bitStream_start (sBitStreamPositionsLocalVar:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_save_bitStream_start_encode" [("sBitStreamPositionsLocalVar",(if sBitStreamPositionsLocalVar = null then null else ST.StrHelper sBitStreamPositionsLocalVar:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sequence_save_bitStream_start_decode" [("sBitStreamPositionsLocalVar",(if sBitStreamPositionsLocalVar = null then null else ST.StrHelper sBitStreamPositionsLocalVar:>Object) )]

let sequence_save_bitstream (sBitStreamPositionsLocalVar:string) (sChName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_save_bitstream_encode" [("sBitStreamPositionsLocalVar",(if sBitStreamPositionsLocalVar = null then null else ST.StrHelper sBitStreamPositionsLocalVar:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sequence_save_bitstream_decode" [("sBitStreamPositionsLocalVar",(if sBitStreamPositionsLocalVar = null then null else ST.StrHelper sBitStreamPositionsLocalVar:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) )]

let sequence_acn_child (sChName:string) (sChildContent:string) (sErrCode:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_acn_child_encode" [("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]
    | Decode    ->
        ST.call "acn_rust" "sequence_acn_child_decode" [("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]

let sequence_mandatory_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (soSaveBitStrmPosStatement:string option) (sChildTypedef:string) (bIsPrimitive:bool) (arrsAcnParams:seq<string>) (bChildHasAcnChildrenToReturn:bool) (soAlignmentCode:string option) (bInlineRequired:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_mandatory_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("arrsAcnParams",(arrsAcnParams|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bChildHasAcnChildrenToReturn",bChildHasAcnChildrenToReturn :>Object);("soAlignmentCode",(if soAlignmentCode.IsNone then null else ST.StrHelper soAlignmentCode.Value:>Object) );("bInlineRequired",bInlineRequired :>Object)]
    | Decode    ->
        ST.call "acn_rust" "sequence_mandatory_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("arrsAcnParams",(arrsAcnParams|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bChildHasAcnChildrenToReturn",bChildHasAcnChildrenToReturn :>Object);("soAlignmentCode",(if soAlignmentCode.IsNone then null else ST.StrHelper soAlignmentCode.Value:>Object) );("bInlineRequired",bInlineRequired :>Object)]

let sequence_always_present_child (p:string) (sAcc:string) (sChName:string) (soChildContent:string option) (soChildExpr:string option) (sChildTypedef:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) (arrsAcnParams:seq<string>) (bChildHasAcnChildrenToReturn:bool) (soAlignmentCode:string option) (bInlineRequired:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_always_present_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soChildContent",(if soChildContent.IsNone then null else ST.StrHelper soChildContent.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("arrsAcnParams",(arrsAcnParams|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bChildHasAcnChildrenToReturn",bChildHasAcnChildrenToReturn :>Object);("soAlignmentCode",(if soAlignmentCode.IsNone then null else ST.StrHelper soAlignmentCode.Value:>Object) );("bInlineRequired",bInlineRequired :>Object)]
    | Decode    ->
        ST.call "acn_rust" "sequence_always_present_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soChildContent",(if soChildContent.IsNone then null else ST.StrHelper soChildContent.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("arrsAcnParams",(arrsAcnParams|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bChildHasAcnChildrenToReturn",bChildHasAcnChildrenToReturn :>Object);("soAlignmentCode",(if soAlignmentCode.IsNone then null else ST.StrHelper soAlignmentCode.Value:>Object) );("bInlineRequired",bInlineRequired :>Object)]

let sequence_always_absent_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTypedef:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_always_absent_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]
    | Decode    ->
        ST.call "acn_rust" "sequence_always_absent_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]

let sequence_optional_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (soExistVar:string option) (soChildExpr:string option) (sChildTypedef:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) (arrsAcnParams:seq<string>) (bChildHasAcnChildrenToReturn:bool) (soAlignmentCode:string option) (bInlineRequired:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_optional_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("arrsAcnParams",(arrsAcnParams|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bChildHasAcnChildrenToReturn",bChildHasAcnChildrenToReturn :>Object);("soAlignmentCode",(if soAlignmentCode.IsNone then null else ST.StrHelper soAlignmentCode.Value:>Object) );("bInlineRequired",bInlineRequired :>Object)]
    | Decode    ->
        ST.call "acn_rust" "sequence_optional_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("arrsAcnParams",(arrsAcnParams|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bChildHasAcnChildrenToReturn",bChildHasAcnChildrenToReturn :>Object);("soAlignmentCode",(if soAlignmentCode.IsNone then null else ST.StrHelper soAlignmentCode.Value:>Object) );("bInlineRequired",bInlineRequired :>Object)]

let sequence_default_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sInitWithDefaultValue:string) (soExistVar:string option) (soChildExpr:string option) (sChildTypedef:string) (soSaveBitStrmPosStatement:string option) (bIsPrimitive:bool) (bChildHasAcnChildrenToReturn:bool) (soAlignmentCode:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sequence_default_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sInitWithDefaultValue",(if sInitWithDefaultValue = null then null else ST.StrHelper sInitWithDefaultValue:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("bChildHasAcnChildrenToReturn",bChildHasAcnChildrenToReturn :>Object);("soAlignmentCode",(if soAlignmentCode.IsNone then null else ST.StrHelper soAlignmentCode.Value:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sequence_default_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sInitWithDefaultValue",(if sInitWithDefaultValue = null then null else ST.StrHelper sInitWithDefaultValue:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("soSaveBitStrmPosStatement",(if soSaveBitStrmPosStatement.IsNone then null else ST.StrHelper soSaveBitStrmPosStatement.Value:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("bChildHasAcnChildrenToReturn",bChildHasAcnChildrenToReturn :>Object);("soAlignmentCode",(if soAlignmentCode.IsNone then null else ST.StrHelper soAlignmentCode.Value:>Object) )]

let sequence_call_post_encoding_function (p:string) (sFncName:string) (sBitStreamStartPos:string) (sBitStreamPositionsNullPos:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "sequence_call_post_encoding_function" [("p",p :>Object);("sFncName",(if sFncName = null then null else ST.StrHelper sFncName:>Object) );("sBitStreamStartPos",(if sBitStreamStartPos = null then null else ST.StrHelper sBitStreamStartPos:>Object) );("sBitStreamPositionsNullPos",(if sBitStreamPositionsNullPos = null then null else ST.StrHelper sBitStreamPositionsNullPos:>Object) )]

let sequence_call_post_decoding_validator (p:string) (sFncName:string) (sBitStreamStartPos:string) (sBitStreamPositionsNullPos:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "sequence_call_post_decoding_validator" [("p",p :>Object);("sFncName",(if sFncName = null then null else ST.StrHelper sFncName:>Object) );("sBitStreamStartPos",(if sBitStreamStartPos = null then null else ST.StrHelper sBitStreamStartPos:>Object) );("sBitStreamPositionsNullPos",(if sBitStreamPositionsNullPos = null then null else ST.StrHelper sBitStreamPositionsNullPos:>Object) )]

let sequence_call_post_encoding_function_prototype (sTypedefName:string) (sFncName:string) (sExtFuncsPositionsTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "sequence_call_post_encoding_function_prototype" [("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) );("sFncName",(if sFncName = null then null else ST.StrHelper sFncName:>Object) );("sExtFuncsPositionsTypeName",(if sExtFuncsPositionsTypeName = null then null else ST.StrHelper sExtFuncsPositionsTypeName:>Object) )]

let sequence_call_post_decoding_validator_prototype (sTypedefName:string) (sFncName:string) (sExtFuncsPositionsTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "sequence_call_post_decoding_validator_prototype" [("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) );("sFncName",(if sFncName = null then null else ST.StrHelper sFncName:>Object) );("sExtFuncsPositionsTypeName",(if sExtFuncsPositionsTypeName = null then null else ST.StrHelper sExtFuncsPositionsTypeName:>Object) )]

let ChoiceChildAlwaysAbsent (p:string) (sAcc:string) (sChildID:string) (nChildIndex:BigInteger) (sErrorCodeName:string) (sChoiceTypeName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ChoiceChildAlwaysAbsent_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("nChildIndex",nChildIndex :>Object);("sErrorCodeName",(if sErrorCodeName = null then null else ST.StrHelper sErrorCodeName:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "ChoiceChildAlwaysAbsent_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("nChildIndex",nChildIndex :>Object);("sErrorCodeName",(if sErrorCodeName = null then null else ST.StrHelper sErrorCodeName:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]

let ChoiceChild (p:string) (sAcc:string) (sChildID:string) (nChildIndex:BigInteger) (nIndexSizeInBits:BigInteger) (nLastItemIndex:BigInteger) (sChildContent:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (sChildInitExpr:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ChoiceChild_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("nChildIndex",nChildIndex :>Object);("nIndexSizeInBits",nIndexSizeInBits :>Object);("nLastItemIndex",nLastItemIndex :>Object);("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "ChoiceChild_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("nChildIndex",nChildIndex :>Object);("nIndexSizeInBits",nIndexSizeInBits :>Object);("nLastItemIndex",nLastItemIndex :>Object);("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) )]

let Choice (p:string) (sAcc:string) (arrsChildren:seq<string>) (nLastItemIndex:BigInteger) (sChoiceIndexName:string) (td:FE_ChoiceTypeDefinition) (nIndexSizeInBits:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Choice_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nLastItemIndex",nLastItemIndex :>Object);("sChoiceIndexName",(if sChoiceIndexName = null then null else ST.StrHelper sChoiceIndexName:>Object) );("td",td :>Object);("nIndexSizeInBits",nIndexSizeInBits :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Choice_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nLastItemIndex",nLastItemIndex :>Object);("sChoiceIndexName",(if sChoiceIndexName = null then null else ST.StrHelper sChoiceIndexName:>Object) );("td",td :>Object);("nIndexSizeInBits",nIndexSizeInBits :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let ChoiceChild_preWhen (p:string) (sAcc:string) (sChildID:string) (sChildBody:string) (arrsConditions:seq<string>) (bFirst:bool) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (sChildInitExpr:string) (bIsPrimitive:bool) (arrsAcnParams:seq<string>) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ChoiceChild_preWhen_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("arrsConditions",(arrsConditions|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bFirst",bFirst :>Object);("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("arrsAcnParams",(arrsAcnParams|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]
    | Decode    ->
        ST.call "acn_rust" "ChoiceChild_preWhen_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("arrsConditions",(arrsConditions|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bFirst",bFirst :>Object);("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("bIsPrimitive",bIsPrimitive :>Object);("arrsAcnParams",(arrsAcnParams|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let ChoiceChild_preWhen_bool_condition (sExtFld:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "ChoiceChild_preWhen_bool_condition" [("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) )]

let ChoiceChild_preWhen_int_condition (sExtFld:string) (sVal:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "ChoiceChild_preWhen_int_condition" [("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) )]

let ChoiceChild_preWhen_str_condition (sExtFld:string) (sVal:string) (arrsNullChars:seq<string>) (arruVal:seq<byte>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "ChoiceChild_preWhen_str_condition" [("sExtFld",(if sExtFld = null then null else ST.StrHelper sExtFld:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("arrsNullChars",(arrsNullChars|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arruVal",arruVal|>Seq.toArray :>Object)]

let Choice_preWhen (p:string) (sAcc:string) (arrsChildren:seq<string>) (td:FE_ChoiceTypeDefinition) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Choice_preWhen_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("td",td :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "Choice_preWhen_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("td",td :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let ChoiceChild_Enum (p:string) (sAcc:string) (sEnmName:string) (sChildID:string) (sChildBody:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (sChildInitExpr:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "ChoiceChild_Enum_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sEnmName",(if sEnmName = null then null else ST.StrHelper sEnmName:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "ChoiceChild_Enum_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sEnmName",(if sEnmName = null then null else ST.StrHelper sEnmName:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) )]

let Choice_Enum (p:string) (sAcc:string) (arrsChildren:seq<string>) (sEnmExtFld:string) (td:FE_ChoiceTypeDefinition) (sErrCode:string) (bIsDeferred:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "Choice_Enum_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sEnmExtFld",(if sEnmExtFld = null then null else ST.StrHelper sEnmExtFld:>Object) );("td",td :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bIsDeferred",bIsDeferred :>Object)]
    | Decode    ->
        ST.call "acn_rust" "Choice_Enum_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sEnmExtFld",(if sEnmExtFld = null then null else ST.StrHelper sEnmExtFld:>Object) );("td",td :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bIsDeferred",bIsDeferred :>Object)]

let SizeDependency (v:string) (sCount:string) (nMin:BigInteger) (nMax:BigInteger) (bCheckRange:bool) (sTypedefName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "SizeDependency" [("v",v :>Object);("sCount",(if sCount = null then null else ST.StrHelper sCount:>Object) );("nMin",nMin :>Object);("nMax",nMax :>Object);("bCheckRange",bCheckRange :>Object);("sTypedefName",(if sTypedefName = null then null else ST.StrHelper sTypedefName:>Object) )]

let SizeDependencyFixedSize (v:string) (nFixedSize:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "SizeDependencyFixedSize" [("v",v :>Object);("nFixedSize",nFixedSize :>Object)]

let ChoiceDependencyEnum_Item (v:string) (sChildCID:string) (sChildCIDHolder:string) (sEnumCName:string) (nChoiceIdx:BigInteger) (bIsOptional:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "ChoiceDependencyEnum_Item" [("v",v :>Object);("sChildCID",(if sChildCID = null then null else ST.StrHelper sChildCID:>Object) );("sChildCIDHolder",(if sChildCIDHolder = null then null else ST.StrHelper sChildCIDHolder:>Object) );("sEnumCName",(if sEnumCName = null then null else ST.StrHelper sEnumCName:>Object) );("nChoiceIdx",nChoiceIdx :>Object);("bIsOptional",bIsOptional :>Object)]

let ChoiceDependencyEnum (sV:string) (sChPath:string) (sAcc:string) (arrsChoiceEnumItems:seq<string>) (bIsOptional:bool) (sDefaultExpr:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "ChoiceDependencyEnum" [("sV",(if sV = null then null else ST.StrHelper sV:>Object) );("sChPath",(if sChPath = null then null else ST.StrHelper sChPath:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChoiceEnumItems",(arrsChoiceEnumItems|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bIsOptional",bIsOptional :>Object);("sDefaultExpr",(if sDefaultExpr = null then null else ST.StrHelper sDefaultExpr:>Object) )]

let PresenceDependency (v:string) (sSeqPath:string) (sAcc:string) (sChildName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "PresenceDependency" [("v",v :>Object);("sSeqPath",(if sSeqPath = null then null else ST.StrHelper sSeqPath:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) )]

let ChoiceDependencyIntPres_child (v:string) (sChildNamePresent:string) (sChildRetVal:string) (sChoiceTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "ChoiceDependencyIntPres_child" [("v",v :>Object);("sChildNamePresent",(if sChildNamePresent = null then null else ST.StrHelper sChildNamePresent:>Object) );("sChildRetVal",(if sChildRetVal = null then null else ST.StrHelper sChildRetVal:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]

let ChoiceDependencyStrPres_child (v:string) (sChildNamePresent:string) (sChildRetVal:string) (arruChildRetValBytes:seq<byte>) (arrsNullChars:seq<string>) (sChoiceTypeName:string) (sInsertedFieldTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "ChoiceDependencyStrPres_child" [("v",v :>Object);("sChildNamePresent",(if sChildNamePresent = null then null else ST.StrHelper sChildNamePresent:>Object) );("sChildRetVal",(if sChildRetVal = null then null else ST.StrHelper sChildRetVal:>Object) );("arruChildRetValBytes",arruChildRetValBytes|>Seq.toArray :>Object);("arrsNullChars",(arrsNullChars|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sInsertedFieldTypeName",(if sInsertedFieldTypeName = null then null else ST.StrHelper sInsertedFieldTypeName:>Object) )]

let ChoiceDependencyPres (v:string) (sChPath:string) (sAcc:string) (arrsChoiceItems:seq<string>) (sChoiceTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "ChoiceDependencyPres" [("v",v :>Object);("sChPath",(if sChPath = null then null else ST.StrHelper sChPath:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChoiceItems",(arrsChoiceItems|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]

let MultiAcnUpdate_checkEqual_pri0 (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "MultiAcnUpdate_checkEqual_pri0" [("p1",p1 :>Object);("p2",p2 :>Object)]

let MultiAcnUpdate_checkEqual_str0 (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "MultiAcnUpdate_checkEqual_str0" [("p1",p1 :>Object);("p2",p2 :>Object)]

let MultiAcnUpdate_get_first_init_value_pri (sV0:string) (sVi:string) (sChPath:string) (bIsFirst:bool) (bIsSingleElement:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "MultiAcnUpdate_get_first_init_value_pri" [("sV0",(if sV0 = null then null else ST.StrHelper sV0:>Object) );("sVi",(if sVi = null then null else ST.StrHelper sVi:>Object) );("sChPath",(if sChPath = null then null else ST.StrHelper sChPath:>Object) );("bIsFirst",bIsFirst :>Object);("bIsSingleElement",bIsSingleElement :>Object)]

let MultiAcnUpdate_get_first_init_value_str (sV0:string) (sVi:string) (sChPath:string) (bIsFirst:bool) (bIsSingleElement:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "MultiAcnUpdate_get_first_init_value_str" [("sV0",(if sV0 = null then null else ST.StrHelper sV0:>Object) );("sVi",(if sVi = null then null else ST.StrHelper sVi:>Object) );("sChPath",(if sChPath = null then null else ST.StrHelper sChPath:>Object) );("bIsFirst",bIsFirst :>Object);("bIsSingleElement",bIsSingleElement :>Object)]

let MultiAcnUpdate_checkEqual_pri (sV0:string) (sVi:string) (sChPath:string) (bIsAlwaysInit:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "MultiAcnUpdate_checkEqual_pri" [("sV0",(if sV0 = null then null else ST.StrHelper sV0:>Object) );("sVi",(if sVi = null then null else ST.StrHelper sVi:>Object) );("sChPath",(if sChPath = null then null else ST.StrHelper sChPath:>Object) );("bIsAlwaysInit",bIsAlwaysInit :>Object)]

let MultiAcnUpdate_checkEqual_str (sV0:string) (sVi:string) (sChPath:string) (bIsAlwaysInit:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "MultiAcnUpdate_checkEqual_str" [("sV0",(if sV0 = null then null else ST.StrHelper sV0:>Object) );("sVi",(if sVi = null then null else ST.StrHelper sVi:>Object) );("sChPath",(if sChPath = null then null else ST.StrHelper sChPath:>Object) );("bIsAlwaysInit",bIsAlwaysInit :>Object)]

let MultiAcnUpdate (v:string) (sV0:string) (sErrCode:string) (arrsLocalDeclarations:seq<string>) (arrsLocalUpdateStatements:seq<string>) (arrsGetFirstIntValue:seq<string>) (bIsFirstIntValueSingle:bool) (arrsLocalCheckEquality:seq<string>) (sDefaultExpr:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "MultiAcnUpdate" [("v",v :>Object);("sV0",(if sV0 = null then null else ST.StrHelper sV0:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("arrsLocalDeclarations",(arrsLocalDeclarations|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsLocalUpdateStatements",(arrsLocalUpdateStatements|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsGetFirstIntValue",(arrsGetFirstIntValue|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bIsFirstIntValueSingle",bIsFirstIntValueSingle :>Object);("arrsLocalCheckEquality",(arrsLocalCheckEquality|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sDefaultExpr",(if sDefaultExpr = null then null else ST.StrHelper sDefaultExpr:>Object) )]

let checkAccessPath (arrsCheckPaths:seq<string>) (sUpdateStatement:string) (v:string) (sInitExpr:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "checkAccessPath" [("arrsCheckPaths",(arrsCheckPaths|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sUpdateStatement",(if sUpdateStatement = null then null else ST.StrHelper sUpdateStatement:>Object) );("v",v :>Object);("sInitExpr",(if sInitExpr = null then null else ST.StrHelper sInitExpr:>Object) )]

let SizeDependency_oct_str_containing (p:string) (sFuncName:string) (sReqBytesForUperEncoding:string) (v:string) (bIsOctet:bool) (sInner:string) (sLocalVarType:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "SizeDependency_oct_str_containing" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("v",v :>Object);("bIsOctet",bIsOctet :>Object);("sInner",(if sInner = null then null else ST.StrHelper sInner:>Object) );("sLocalVarType",(if sLocalVarType = null then null else ST.StrHelper sLocalVarType:>Object) )]

let octet_string_containing_ext_field_func (p:string) (sFuncName:string) (sReqBytesForUperEncoding:string) (sExtField:string) (sErrCode:string) (soInner:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "octet_string_containing_ext_field_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("sExtField",(if sExtField = null then null else ST.StrHelper sExtField:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soInner",(if soInner.IsNone then null else ST.StrHelper soInner.Value:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "octet_string_containing_ext_field_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("sExtField",(if sExtField = null then null else ST.StrHelper sExtField:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soInner",(if soInner.IsNone then null else ST.StrHelper soInner.Value:>Object) )]

let octet_string_containing_deferred_func (p:string) (sFuncName:string) (sDetParamName:string) (sPatchFuncName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "octet_string_containing_deferred_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sDetParamName",(if sDetParamName = null then null else ST.StrHelper sDetParamName:>Object) );("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "octet_string_containing_deferred_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sDetParamName",(if sDetParamName = null then null else ST.StrHelper sDetParamName:>Object) );("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let octet_string_containing_deferred_fixed_func (p:string) (sFuncName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "octet_string_containing_deferred_fixed_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "octet_string_containing_deferred_fixed_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]

let octet_string_containing_deferred_embedded_func (p:string) (sFuncName:string) (nMinSize:BigInteger) (nMaxSize:BigInteger) (nBits:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "octet_string_containing_deferred_embedded_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object);("nBits",nBits :>Object)]
    | Decode    ->
        ST.call "acn_rust" "octet_string_containing_deferred_embedded_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object);("nBits",nBits :>Object)]

let octet_string_containing_deferred_wrapper (sBody:string) (sDetParamName:string) (sPatchFuncName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "octet_string_containing_deferred_wrapper_encode" [("sBody",(if sBody = null then null else ST.StrHelper sBody:>Object) );("sDetParamName",(if sDetParamName = null then null else ST.StrHelper sDetParamName:>Object) );("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "octet_string_containing_deferred_wrapper_decode" [("sBody",(if sBody = null then null else ST.StrHelper sBody:>Object) );("sDetParamName",(if sDetParamName = null then null else ST.StrHelper sDetParamName:>Object) );("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let bit_string_containing_deferred_func (p:string) (sFuncName:string) (sDetParamName:string) (sPatchFuncName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_containing_deferred_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sDetParamName",(if sDetParamName = null then null else ST.StrHelper sDetParamName:>Object) );("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "bit_string_containing_deferred_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sDetParamName",(if sDetParamName = null then null else ST.StrHelper sDetParamName:>Object) );("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let bit_string_containing_deferred_fixed_func (p:string) (sFuncName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_containing_deferred_fixed_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "bit_string_containing_deferred_fixed_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]

let bit_string_containing_deferred_embedded_func (p:string) (sFuncName:string) (nMinSize:BigInteger) (nMaxSize:BigInteger) (nBits:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_containing_deferred_embedded_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object);("nBits",nBits :>Object)]
    | Decode    ->
        ST.call "acn_rust" "bit_string_containing_deferred_embedded_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object);("nBits",nBits :>Object)]

let bit_string_containing_deferred_wrapper (sBody:string) (sDetParamName:string) (sPatchFuncName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_containing_deferred_wrapper_encode" [("sBody",(if sBody = null then null else ST.StrHelper sBody:>Object) );("sDetParamName",(if sDetParamName = null then null else ST.StrHelper sDetParamName:>Object) );("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "bit_string_containing_deferred_wrapper_decode" [("sBody",(if sBody = null then null else ST.StrHelper sBody:>Object) );("sDetParamName",(if sDetParamName = null then null else ST.StrHelper sDetParamName:>Object) );("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let bit_string_containing_ext_field_func (p:string) (sFuncName:string) (sReqBytesForUperEncoding:string) (sReqBitsForUperEncoding:string) (sExtField:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_containing_ext_field_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("sReqBitsForUperEncoding",(if sReqBitsForUperEncoding = null then null else ST.StrHelper sReqBitsForUperEncoding:>Object) );("sExtField",(if sExtField = null then null else ST.StrHelper sExtField:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "bit_string_containing_ext_field_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("sReqBitsForUperEncoding",(if sReqBitsForUperEncoding = null then null else ST.StrHelper sReqBitsForUperEncoding:>Object) );("sExtField",(if sExtField = null then null else ST.StrHelper sExtField:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "rtlModuleName" []

let sparkAnnotations (sTypeDefName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "sparkAnnotations_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "sparkAnnotations_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]

let sparkAnnotations_deducedFixed (sTypeDefName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "sparkAnnotations_deducedFixed" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]

let octet_string_containing_func (p:string) (sFuncName:string) (sContainedType:string) (sReqBytesForAcnEncoding:string) (nBits:BigInteger) (nMinSize:BigInteger) (nMaxSize:BigInteger) (bFixedSize:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "octet_string_containing_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sContainedType",(if sContainedType = null then null else ST.StrHelper sContainedType:>Object) );("sReqBytesForAcnEncoding",(if sReqBytesForAcnEncoding = null then null else ST.StrHelper sReqBytesForAcnEncoding:>Object) );("nBits",nBits :>Object);("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object);("bFixedSize",bFixedSize :>Object)]
    | Decode    ->
        ST.call "acn_rust" "octet_string_containing_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sContainedType",(if sContainedType = null then null else ST.StrHelper sContainedType:>Object) );("sReqBytesForAcnEncoding",(if sReqBytesForAcnEncoding = null then null else ST.StrHelper sReqBytesForAcnEncoding:>Object) );("nBits",nBits :>Object);("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object);("bFixedSize",bFixedSize :>Object)]

let bit_string_containing_func (p:string) (sFuncName:string) (sContainedType:string) (sReqBytesForAcnEncoding:string) (sReqBitsForUperEncoding:string) (nBits:BigInteger) (nMinSize:BigInteger) (nMaxSize:BigInteger) (bFixedSize:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "bit_string_containing_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sContainedType",(if sContainedType = null then null else ST.StrHelper sContainedType:>Object) );("sReqBytesForAcnEncoding",(if sReqBytesForAcnEncoding = null then null else ST.StrHelper sReqBytesForAcnEncoding:>Object) );("sReqBitsForUperEncoding",(if sReqBitsForUperEncoding = null then null else ST.StrHelper sReqBitsForUperEncoding:>Object) );("nBits",nBits :>Object);("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object);("bFixedSize",bFixedSize :>Object)]
    | Decode    ->
        ST.call "acn_rust" "bit_string_containing_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sContainedType",(if sContainedType = null then null else ST.StrHelper sContainedType:>Object) );("sReqBytesForAcnEncoding",(if sReqBytesForAcnEncoding = null then null else ST.StrHelper sReqBytesForAcnEncoding:>Object) );("sReqBitsForUperEncoding",(if sReqBitsForUperEncoding = null then null else ST.StrHelper sReqBitsForUperEncoding:>Object) );("nBits",nBits :>Object);("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object);("bFixedSize",bFixedSize :>Object)]

let acn_deferred_det_declare (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_declare_encode" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_declare_decode" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_init_value (sInitFuncName:string) (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_init_value_encode" [("sInitFuncName",(if sInitFuncName = null then null else ST.StrHelper sInitFuncName:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_init_value_decode" [("sInitFuncName",(if sInitFuncName = null then null else ST.StrHelper sInitFuncName:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_init_ptr (sInitFuncName:string) (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_init_ptr_encode" [("sInitFuncName",(if sInitFuncName = null then null else ST.StrHelper sInitFuncName:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_init_ptr_decode" [("sInitFuncName",(if sInitFuncName = null then null else ST.StrHelper sInitFuncName:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_patch_value (sPatchFuncName:string) (sValue:string) (sDetName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_value_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_value_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let acn_deferred_det_patch_ptr (sPatchFuncName:string) (sValue:string) (sDetName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_ptr_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_ptr_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let acn_deferred_det_init_value_with_size (sInitFuncName:string) (nBits:BigInteger) (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_init_value_with_size_encode" [("sInitFuncName",(if sInitFuncName = null then null else ST.StrHelper sInitFuncName:>Object) );("nBits",nBits :>Object);("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_init_value_with_size_decode" [("sInitFuncName",(if sInitFuncName = null then null else ST.StrHelper sInitFuncName:>Object) );("nBits",nBits :>Object);("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_init_ptr_with_size (sInitFuncName:string) (nBits:BigInteger) (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_init_ptr_with_size_encode" [("sInitFuncName",(if sInitFuncName = null then null else ST.StrHelper sInitFuncName:>Object) );("nBits",nBits :>Object);("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_init_ptr_with_size_decode" [("sInitFuncName",(if sInitFuncName = null then null else ST.StrHelper sInitFuncName:>Object) );("nBits",nBits :>Object);("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_patch_value_with_size (sPatchFuncName:string) (nBits:BigInteger) (sValue:string) (sDetName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_value_with_size_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("nBits",nBits :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_value_with_size_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("nBits",nBits :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let acn_deferred_det_patch_ptr_with_size (sPatchFuncName:string) (nBits:BigInteger) (sValue:string) (sDetName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_ptr_with_size_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("nBits",nBits :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_ptr_with_size_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("nBits",nBits :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let acn_deferred_det_patch_value_str (sPatchFuncName:string) (nBits:BigInteger) (sValue:string) (sDetName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_value_str_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("nBits",nBits :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_value_str_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("nBits",nBits :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let acn_deferred_det_patch_ptr_str (sPatchFuncName:string) (nBits:BigInteger) (sValue:string) (sDetName:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_ptr_str_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("nBits",nBits :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_patch_ptr_str_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("nBits",nBits :>Object);("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let acn_deferred_det_formal_param (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_formal_param_encode" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_formal_param_decode" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_actual_param (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_actual_param_encode" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_actual_param_decode" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_save_pos (sVarName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_save_pos_encode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_save_pos_decode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) )]

let acn_deferred_det_distance_bytes (sStart:string) (sEnd:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_distance_bytes_encode" [("sStart",(if sStart = null then null else ST.StrHelper sStart:>Object) );("sEnd",(if sEnd = null then null else ST.StrHelper sEnd:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_distance_bytes_decode" [("sStart",(if sStart = null then null else ST.StrHelper sStart:>Object) );("sEnd",(if sEnd = null then null else ST.StrHelper sEnd:>Object) )]

let acn_deferred_det_kind_access (p:string) (sAcc:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_kind_access" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) )]

let acn_deferred_det_value_presence_bool (p:string) (sAcc:string) (sChildName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_value_presence_bool" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) )]

let acn_deferred_det_switch_case_int (sCaseName:string) (sVarName:string) (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_switch_case_int" [("sCaseName",(if sCaseName = null then null else ST.StrHelper sCaseName:>Object) );("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let acn_deferred_det_switch_int (sVarName:string) (sKindAccess:string) (arrsCaseItems:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_switch_int" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sKindAccess",(if sKindAccess = null then null else ST.StrHelper sKindAccess:>Object) );("arrsCaseItems",(arrsCaseItems|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let acn_deferred_det_switch_case_str (sCaseName:string) (sVarName:string) (sValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_switch_case_str" [("sCaseName",(if sCaseName = null then null else ST.StrHelper sCaseName:>Object) );("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) )]

let acn_deferred_det_switch_str (sVarName:string) (sKindAccess:string) (arrsCaseItems:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_switch_str" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sKindAccess",(if sKindAccess = null then null else ST.StrHelper sKindAccess:>Object) );("arrsCaseItems",(arrsCaseItems|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let acn_deferred_det_fallback_value (sPatchFuncName:string) (sDefaultVal:string) (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_fallback_value_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sDefaultVal",(if sDefaultVal = null then null else ST.StrHelper sDefaultVal:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_fallback_value_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sDefaultVal",(if sDefaultVal = null then null else ST.StrHelper sDefaultVal:>Object) );("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_fallback_value_with_size (sPatchFuncName:string) (sDefaultVal:string) (nBits:BigInteger) (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_fallback_value_with_size_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sDefaultVal",(if sDefaultVal = null then null else ST.StrHelper sDefaultVal:>Object) );("nBits",nBits :>Object);("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_fallback_value_with_size_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sDefaultVal",(if sDefaultVal = null then null else ST.StrHelper sDefaultVal:>Object) );("nBits",nBits :>Object);("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_fallback_value_str (sPatchFuncName:string) (sDefaultVal:string) (nBits:BigInteger) (sDetName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_fallback_value_str_encode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sDefaultVal",(if sDefaultVal = null then null else ST.StrHelper sDefaultVal:>Object) );("nBits",nBits :>Object);("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_fallback_value_str_decode" [("sPatchFuncName",(if sPatchFuncName = null then null else ST.StrHelper sPatchFuncName:>Object) );("sDefaultVal",(if sDefaultVal = null then null else ST.StrHelper sDefaultVal:>Object) );("nBits",nBits :>Object);("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_copy_tmp (sDetAccess:string) (sTmpName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_copy_tmp_encode" [("sDetAccess",(if sDetAccess = null then null else ST.StrHelper sDetAccess:>Object) );("sTmpName",(if sTmpName = null then null else ST.StrHelper sTmpName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_copy_tmp_decode" [("sDetAccess",(if sDetAccess = null then null else ST.StrHelper sDetAccess:>Object) );("sTmpName",(if sTmpName = null then null else ST.StrHelper sTmpName:>Object) )]

let acn_deferred_det_copy_bool_tmp (sDetAccess:string) (sTmpName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_copy_bool_tmp_encode" [("sDetAccess",(if sDetAccess = null then null else ST.StrHelper sDetAccess:>Object) );("sTmpName",(if sTmpName = null then null else ST.StrHelper sTmpName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_copy_bool_tmp_decode" [("sDetAccess",(if sDetAccess = null then null else ST.StrHelper sDetAccess:>Object) );("sTmpName",(if sTmpName = null then null else ST.StrHelper sTmpName:>Object) )]

let acn_deferred_det_copy_enum_tmp (sDetAccess:string) (sTmpName:string) (sEnumTypeName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "acn_rust" "acn_deferred_det_copy_enum_tmp_encode" [("sDetAccess",(if sDetAccess = null then null else ST.StrHelper sDetAccess:>Object) );("sTmpName",(if sTmpName = null then null else ST.StrHelper sTmpName:>Object) );("sEnumTypeName",(if sEnumTypeName = null then null else ST.StrHelper sEnumTypeName:>Object) )]
    | Decode    ->
        ST.call "acn_rust" "acn_deferred_det_copy_enum_tmp_decode" [("sDetAccess",(if sDetAccess = null then null else ST.StrHelper sDetAccess:>Object) );("sTmpName",(if sTmpName = null then null else ST.StrHelper sTmpName:>Object) );("sEnumTypeName",(if sEnumTypeName = null then null else ST.StrHelper sEnumTypeName:>Object) )]

let acn_deferred_det_access_value (sDetName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_access_value" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_access_ptr (sDetName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_access_ptr" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_access_bool_ptr (sDetName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_access_bool_ptr" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_access_str_value (sDetName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_access_str_value" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_access_str_ptr (sDetName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_access_str_ptr" [("sDetName",(if sDetName = null then null else ST.StrHelper sDetName:>Object) )]

let acn_deferred_det_relative_access (sRootId:string) (sFieldPath:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_relative_access" [("sRootId",(if sRootId = null then null else ST.StrHelper sRootId:>Object) );("sFieldPath",(if sFieldPath = null then null else ST.StrHelper sFieldPath:>Object) )]

let acn_deferred_det_uper_offset_sub (sValue:string) (sOffset:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_uper_offset_sub" [("sValue",(if sValue = null then null else ST.StrHelper sValue:>Object) );("sOffset",(if sOffset = null then null else ST.StrHelper sOffset:>Object) )]

let acn_deferred_det_preblock_wrap (sPreBlock:string) (sPatchCall:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_preblock_wrap" [("sPreBlock",(if sPreBlock = null then null else ST.StrHelper sPreBlock:>Object) );("sPatchCall",(if sPatchCall = null then null else ST.StrHelper sPatchCall:>Object) )]

let acn_deferred_det_type_name () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_type_name" []

let acn_deferred_det_init_expr () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "acn_rust" "acn_deferred_det_init_expr" []

