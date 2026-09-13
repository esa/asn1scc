module uper_rust
open System
open System.Numerics
open CommonTypes

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "uper_rust" "rtlModuleName" []

let call_base_type_func (p:string) (sFuncName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "call_base_type_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "call_base_type_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]

let call_superclass_func (p:string) (sFuncName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "call_superclass_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "call_superclass_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]

let EmitTypeAssignment_def_err_code (sErrCode:string) (nErrValue:BigInteger) (sFieldPath:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "uper_rust" "EmitTypeAssignment_def_err_code" [("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nErrValue",nErrValue :>Object);("sFieldPath",(if sFieldPath = null then null else ST.StrHelper sFieldPath:>Object) )]

let EmitTypeAssignment_def (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) (bEmptyEncodingSpace:bool) (nMaxBytesInPER:BigInteger) (nMaxBitsInPER:BigInteger) (soSparkAnnotations:string option) (bReqBytesForEncodingIsZero:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "EmitTypeAssignment_def_encode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsErrcodes",(arrsErrcodes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bEmptyEncodingSpace",bEmptyEncodingSpace :>Object);("nMaxBytesInPER",nMaxBytesInPER :>Object);("nMaxBitsInPER",nMaxBitsInPER :>Object);("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) );("bReqBytesForEncodingIsZero",bReqBytesForEncodingIsZero :>Object)]
    | Decode    ->
        ST.call "uper_rust" "EmitTypeAssignment_def_decode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsErrcodes",(arrsErrcodes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bEmptyEncodingSpace",bEmptyEncodingSpace :>Object);("nMaxBytesInPER",nMaxBytesInPER :>Object);("nMaxBitsInPER",nMaxBitsInPER :>Object);("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) );("bReqBytesForEncodingIsZero",bReqBytesForEncodingIsZero :>Object)]

let EmitTypeAssignment (sVarName:string) (sStar:string) (sFuncName:string) (soIValidFuncName:string option) (sTypeDefName:string) (arrsLocalVariables:seq<string>) (sContent:string) (soSparkAnnotations:string option) (sInitialExp:string) (bReqBytesForEncodingIsZero:bool) (bBsIsUnreferenced:bool) (bVarNameIsUnreferenced:bool) (soInitFuncName:string option) (arrsAnnots:seq<string>) (arrsPrecond:seq<string>) (arrsPostcond:seq<string>) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "EmitTypeAssignment_encode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("soIValidFuncName",(if soIValidFuncName.IsNone then null else ST.StrHelper soIValidFuncName.Value:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsLocalVariables",(arrsLocalVariables|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) );("sInitialExp",(if sInitialExp = null then null else ST.StrHelper sInitialExp:>Object) );("bReqBytesForEncodingIsZero",bReqBytesForEncodingIsZero :>Object);("bBsIsUnreferenced",bBsIsUnreferenced :>Object);("bVarNameIsUnreferenced",bVarNameIsUnreferenced :>Object);("soInitFuncName",(if soInitFuncName.IsNone then null else ST.StrHelper soInitFuncName.Value:>Object) );("arrsAnnots",(arrsAnnots|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPrecond",(arrsPrecond|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPostcond",(arrsPostcond|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]
    | Decode    ->
        ST.call "uper_rust" "EmitTypeAssignment_decode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("soIValidFuncName",(if soIValidFuncName.IsNone then null else ST.StrHelper soIValidFuncName.Value:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsLocalVariables",(arrsLocalVariables|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) );("sInitialExp",(if sInitialExp = null then null else ST.StrHelper sInitialExp:>Object) );("bReqBytesForEncodingIsZero",bReqBytesForEncodingIsZero :>Object);("bBsIsUnreferenced",bBsIsUnreferenced :>Object);("bVarNameIsUnreferenced",bVarNameIsUnreferenced :>Object);("soInitFuncName",(if soInitFuncName.IsNone then null else ST.StrHelper soInitFuncName.Value:>Object) );("arrsAnnots",(arrsAnnots|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPrecond",(arrsPrecond|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsPostcond",(arrsPostcond|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let InternalItem_oct_str (p:string) (sAcc:string) (i:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "InternalItem_oct_str_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "InternalItem_oct_str_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let InternalItem_string_with_alpha (p:string) (sErrCode:string) (td:FE_StringTypeDefinition) (i:string) (nLastItemIndex:BigInteger) (arrnAlphabetAsciiCodes:seq<BigInteger>) (nAlphabetLength:BigInteger) (nCharIndexSize:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "InternalItem_string_with_alpha_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("td",td :>Object);("i",i :>Object);("nLastItemIndex",nLastItemIndex :>Object);("arrnAlphabetAsciiCodes",arrnAlphabetAsciiCodes|>Seq.toArray :>Object);("nAlphabetLength",nAlphabetLength :>Object);("nCharIndexSize",nCharIndexSize :>Object)]
    | Decode    ->
        ST.call "uper_rust" "InternalItem_string_with_alpha_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("td",td :>Object);("i",i :>Object);("nLastItemIndex",nLastItemIndex :>Object);("arrnAlphabetAsciiCodes",arrnAlphabetAsciiCodes|>Seq.toArray :>Object);("nAlphabetLength",nAlphabetLength :>Object);("nCharIndexSize",nCharIndexSize :>Object)]

let InternalItem_string_no_alpha (p:string) (sErrCode:string) (i:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "InternalItem_string_no_alpha_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("i",i :>Object)]
    | Decode    ->
        ST.call "uper_rust" "InternalItem_string_no_alpha_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("i",i :>Object)]

let IntFullyConstraint (p:string) (nMin:BigInteger) (nMax:BigInteger) (nBits:BigInteger) (sSsuffix:string) (sErrCode:string) (soType:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntFullyConstraint_encode" [("p",p :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("nBits",nBits :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntFullyConstraint_decode" [("p",p :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("nBits",nBits :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]

let IntFullyConstraintPos (p:string) (nMin:BigInteger) (nMax:BigInteger) (nBits:BigInteger) (sSsuffix:string) (sErrCode:string) (soRangeAssert:string option) (soType:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntFullyConstraintPos_encode" [("p",p :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("nBits",nBits :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soRangeAssert",(if soRangeAssert.IsNone then null else ST.StrHelper soRangeAssert.Value:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntFullyConstraintPos_decode" [("p",p :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("nBits",nBits :>Object);("sSsuffix",(if sSsuffix = null then null else ST.StrHelper sSsuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soRangeAssert",(if soRangeAssert.IsNone then null else ST.StrHelper soRangeAssert.Value:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]

let IntUnconstrained (p:string) (sErrCode:string) (bCoverageIgnore:bool) (soType:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntUnconstrained_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bCoverageIgnore",bCoverageIgnore :>Object);("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntUnconstrained_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bCoverageIgnore",bCoverageIgnore :>Object);("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]

let IntUnconstrainedMax (p:string) (nMax:BigInteger) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntUnconstrainedMax_encode" [("p",p :>Object);("nMax",nMax :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntUnconstrainedMax_decode" [("p",p :>Object);("nMax",nMax :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let IntSemiConstraint (p:string) (nMin:BigInteger) (sErrCode:string) (soType:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntSemiConstraint_encode" [("p",p :>Object);("nMin",nMin :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntSemiConstraint_decode" [("p",p :>Object);("nMin",nMin :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]

let IntSemiConstraintPos (p:string) (nMin:BigInteger) (sErrCode:string) (soType:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntSemiConstraintPos_encode" [("p",p :>Object);("nMin",nMin :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntSemiConstraintPos_decode" [("p",p :>Object);("nMin",nMin :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]

let IntNoneRequired (p:string) (sConst:string) (sErrCode:string) (soType:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntNoneRequired_encode" [("p",p :>Object);("sConst",(if sConst = null then null else ST.StrHelper sConst:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntNoneRequired_decode" [("p",p :>Object);("sConst",(if sConst = null then null else ST.StrHelper sConst:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("soType",(if soType.IsNone then null else ST.StrHelper soType.Value:>Object) )]

let IntRootExt (p:string) (nMin:BigInteger) (sRootBaseConstraint:string) (sIntBody:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntRootExt_encode" [("p",p :>Object);("nMin",nMin :>Object);("sRootBaseConstraint",(if sRootBaseConstraint = null then null else ST.StrHelper sRootBaseConstraint:>Object) );("sIntBody",(if sIntBody = null then null else ST.StrHelper sIntBody:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntRootExt_decode" [("p",p :>Object);("nMin",nMin :>Object);("sRootBaseConstraint",(if sRootBaseConstraint = null then null else ST.StrHelper sRootBaseConstraint:>Object) );("sIntBody",(if sIntBody = null then null else ST.StrHelper sIntBody:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let IntRootExt2 (p:string) (nMin:BigInteger) (sRootBaseConstraint:string) (sIntBody:string) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "IntRootExt2_encode" [("p",p :>Object);("nMin",nMin :>Object);("sRootBaseConstraint",(if sRootBaseConstraint = null then null else ST.StrHelper sRootBaseConstraint:>Object) );("sIntBody",(if sIntBody = null then null else ST.StrHelper sIntBody:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "IntRootExt2_decode" [("p",p :>Object);("nMin",nMin :>Object);("sRootBaseConstraint",(if sRootBaseConstraint = null then null else ST.StrHelper sRootBaseConstraint:>Object) );("sIntBody",(if sIntBody = null then null else ST.StrHelper sIntBody:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Boolean (p:string) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "Boolean_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "Boolean_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let Real (p:string) (sSuffix:string) (sErrCode:string) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "Real_encode" [("p",p :>Object);("sSuffix",(if sSuffix = null then null else ST.StrHelper sSuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "Real_decode" [("p",p :>Object);("sSuffix",(if sSuffix = null then null else ST.StrHelper sSuffix:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let ObjectIdentifier (p:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "ObjectIdentifier_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "ObjectIdentifier_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let RelativeOID (p:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "RelativeOID_encode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "RelativeOID_decode" [("p",p :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let Time (p:string) (sTimeSubType:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "Time_encode" [("p",p :>Object);("sTimeSubType",(if sTimeSubType = null then null else ST.StrHelper sTimeSubType:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "Time_decode" [("p",p :>Object);("sTimeSubType",(if sTimeSubType = null then null else ST.StrHelper sTimeSubType:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let Enumerated_item (p:string) (sName:string) (nIndex:BigInteger) (nLastItemIndex:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "Enumerated_item_encode" [("p",p :>Object);("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("nIndex",nIndex :>Object);("nLastItemIndex",nLastItemIndex :>Object)]
    | Decode    ->
        ST.call "uper_rust" "Enumerated_item_decode" [("p",p :>Object);("sName",(if sName = null then null else ST.StrHelper sName:>Object) );("nIndex",nIndex :>Object);("nLastItemIndex",nLastItemIndex :>Object)]

let Enumerated (p:string) (td:FE_EnumeratedTypeDefinition) (arrsItem:seq<string>) (nMin:BigInteger) (nMax:BigInteger) (nBits:BigInteger) (sErrCode:string) (nLastItemIndex:BigInteger) (sFirstItemName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "Enumerated_encode" [("p",p :>Object);("td",td :>Object);("arrsItem",(arrsItem|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("nBits",nBits :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nLastItemIndex",nLastItemIndex :>Object);("sFirstItemName",(if sFirstItemName = null then null else ST.StrHelper sFirstItemName:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "Enumerated_decode" [("p",p :>Object);("td",td :>Object);("arrsItem",(arrsItem|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nMin",nMin :>Object);("nMax",nMax :>Object);("nBits",nBits :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nLastItemIndex",nLastItemIndex :>Object);("sFirstItemName",(if sFirstItemName = null then null else ST.StrHelper sFirstItemName:>Object) )]

let Enumerated_no_switch (p:string) (td:FE_EnumeratedTypeDefinition) (sErrCode:string) (sEnumIndex:string) (nLastItemIndex:BigInteger) (sFirstItemName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "Enumerated_no_switch_encode" [("p",p :>Object);("td",td :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sEnumIndex",(if sEnumIndex = null then null else ST.StrHelper sEnumIndex:>Object) );("nLastItemIndex",nLastItemIndex :>Object);("sFirstItemName",(if sFirstItemName = null then null else ST.StrHelper sFirstItemName:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "Enumerated_no_switch_decode" [("p",p :>Object);("td",td :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sEnumIndex",(if sEnumIndex = null then null else ST.StrHelper sEnumIndex:>Object) );("nLastItemIndex",nLastItemIndex :>Object);("sFirstItemName",(if sFirstItemName = null then null else ST.StrHelper sFirstItemName:>Object) )]

let choice_child (p:string) (sAcc:string) (sChildID:string) (nChildIndex:BigInteger) (nIndexSizeInBits:BigInteger) (nLastItemIndex:BigInteger) (sChildContent:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) (sChildInitExpr:string) (bIsSequence:bool) (bIsEnum:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "choice_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("nChildIndex",nChildIndex :>Object);("nIndexSizeInBits",nIndexSizeInBits :>Object);("nLastItemIndex",nLastItemIndex :>Object);("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("bIsSequence",bIsSequence :>Object);("bIsEnum",bIsEnum :>Object)]
    | Decode    ->
        ST.call "uper_rust" "choice_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChildID",(if sChildID = null then null else ST.StrHelper sChildID:>Object) );("nChildIndex",nChildIndex :>Object);("nIndexSizeInBits",nIndexSizeInBits :>Object);("nLastItemIndex",nLastItemIndex :>Object);("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("bIsSequence",bIsSequence :>Object);("bIsEnum",bIsEnum :>Object)]

let choice (p:string) (sAcc:string) (arrsChildren:seq<string>) (nLastItemIndex:BigInteger) (sChoiceIndexName:string) (sErrCode:string) (td:FE_ChoiceTypeDefinition) (nIndexSizeInBits:BigInteger) (bIntroSnap:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "choice_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nLastItemIndex",nLastItemIndex :>Object);("sChoiceIndexName",(if sChoiceIndexName = null then null else ST.StrHelper sChoiceIndexName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("td",td :>Object);("nIndexSizeInBits",nIndexSizeInBits :>Object);("bIntroSnap",bIntroSnap :>Object)]
    | Decode    ->
        ST.call "uper_rust" "choice_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nLastItemIndex",nLastItemIndex :>Object);("sChoiceIndexName",(if sChoiceIndexName = null then null else ST.StrHelper sChoiceIndexName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("td",td :>Object);("nIndexSizeInBits",nIndexSizeInBits :>Object);("bIntroSnap",bIntroSnap :>Object)]

let sequence_presence_bit (p:string) (sAcc:string) (sChName:string) (soExistVar:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "sequence_presence_bit_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "sequence_presence_bit_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let sequence_presence_bit_fix (p:string) (sAcc:string) (sChName:string) (soExistVar:string option) (sErrCode:string) (sVal:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "sequence_presence_bit_fix_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "sequence_presence_bit_fix_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) )]

let sequence_mandatory_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTypedef:string) (bIsPrimitive:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "sequence_mandatory_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]
    | Decode    ->
        ST.call "uper_rust" "sequence_mandatory_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]

let sequence_optional_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (soExistVar:string option) (soChildExpr:string option) (sChildTypedef:string) (bIsPrimitive:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "sequence_optional_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]
    | Decode    ->
        ST.call "uper_rust" "sequence_optional_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]

let sequence_default_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (soExistVar:string option) (soChildExpr:string option) (sChildTypedef:string) (sInitWithDefaultValue:string) (bIsPrimitive:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "sequence_default_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("sInitWithDefaultValue",(if sInitWithDefaultValue = null then null else ST.StrHelper sInitWithDefaultValue:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]
    | Decode    ->
        ST.call "uper_rust" "sequence_default_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("soExistVar",(if soExistVar.IsNone then null else ST.StrHelper soExistVar.Value:>Object) );("soChildExpr",(if soChildExpr.IsNone then null else ST.StrHelper soChildExpr.Value:>Object) );("sChildTypedef",(if sChildTypedef = null then null else ST.StrHelper sChildTypedef:>Object) );("sInitWithDefaultValue",(if sInitWithDefaultValue = null then null else ST.StrHelper sInitWithDefaultValue:>Object) );("bIsPrimitive",bIsPrimitive :>Object)]

let sequence_build (p:string) (sTypeDefName:string) (bIsOptional:bool) (arrsChildren:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "uper_rust" "sequence_build" [("p",p :>Object);("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("bIsOptional",bIsOptional :>Object);("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let str_FixedSize (p:string) (sTasName:string) (i:string) (sInternalItem:string) (nFixedSize:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (soInitExpr:string option) (bIntroSnap:bool) (soCallAux:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "str_FixedSize_encode" [("p",p :>Object);("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nFixedSize",nFixedSize :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("soInitExpr",(if soInitExpr.IsNone then null else ST.StrHelper soInitExpr.Value:>Object) );("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "str_FixedSize_decode" [("p",p :>Object);("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nFixedSize",nFixedSize :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("soInitExpr",(if soInitExpr.IsNone then null else ST.StrHelper soInitExpr.Value:>Object) );("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]

let str_VarSize (p:string) (sPIden:string) (sTasName:string) (i:string) (sInternalItem:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nSizeInBits:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (soInitExpr:string option) (soCallAux:string option) (sType:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "str_VarSize_encode" [("p",p :>Object);("sPIden",(if sPIden = null then null else ST.StrHelper sPIden:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nSizeInBits",nSizeInBits :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("soInitExpr",(if soInitExpr.IsNone then null else ST.StrHelper soInitExpr.Value:>Object) );("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "str_VarSize_decode" [("p",p :>Object);("sPIden",(if sPIden = null then null else ST.StrHelper sPIden:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nSizeInBits",nSizeInBits :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("soInitExpr",(if soInitExpr.IsNone then null else ST.StrHelper soInitExpr.Value:>Object) );("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let seqOf_FixedSize (p:string) (sTasName:string) (i:string) (sInternalItem:string) (nFixedSize:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (sChildInitExpr:string) (soCallAux:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "seqOf_FixedSize_encode" [("p",p :>Object);("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nFixedSize",nFixedSize :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "seqOf_FixedSize_decode" [("p",p :>Object);("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nFixedSize",nFixedSize :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]

let seqOf_VarSize (p:string) (sAcc:string) (sTasName:string) (i:string) (sInternalItem:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nSizeInBits:BigInteger) (nIntItemMinSize:BigInteger) (nIntItemMaxSize:BigInteger) (nAlignSize:BigInteger) (sChildInitExpr:string) (sErrCode:string) (nAbsOffset:BigInteger) (nRemainingMinBits:BigInteger) (nLevel:BigInteger) (nIx:BigInteger) (nOffset:BigInteger) (bIntroSnap:bool) (soCallAux:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "seqOf_VarSize_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nSizeInBits",nSizeInBits :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAbsOffset",nAbsOffset :>Object);("nRemainingMinBits",nRemainingMinBits :>Object);("nLevel",nLevel :>Object);("nIx",nIx :>Object);("nOffset",nOffset :>Object);("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "seqOf_VarSize_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("i",i :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nSizeInBits",nSizeInBits :>Object);("nIntItemMinSize",nIntItemMinSize :>Object);("nIntItemMaxSize",nIntItemMaxSize :>Object);("nAlignSize",nAlignSize :>Object);("sChildInitExpr",(if sChildInitExpr = null then null else ST.StrHelper sChildInitExpr:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nAbsOffset",nAbsOffset :>Object);("nRemainingMinBits",nRemainingMinBits :>Object);("nLevel",nLevel :>Object);("nIx",nIx :>Object);("nOffset",nOffset :>Object);("bIntroSnap",bIntroSnap :>Object);("soCallAux",(if soCallAux.IsNone then null else ST.StrHelper soCallAux.Value:>Object) )]

let octet_FixedSize (sTypeDefName:string) (p:string) (sAcc:string) (nFixedSize:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "octet_FixedSize_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nFixedSize",nFixedSize :>Object)]
    | Decode    ->
        ST.call "uper_rust" "octet_FixedSize_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nFixedSize",nFixedSize :>Object)]

let octet_VarSize (sTypeDefName:string) (p:string) (sAcc:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nSizeInBits:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "octet_VarSize_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nSizeInBits",nSizeInBits :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "octet_VarSize_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nSizeInBits",nSizeInBits :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let bitString_FixSize (sTypeDefName:string) (p:string) (sAcc:string) (nFixedSize:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "bitString_FixSize_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nFixedSize",nFixedSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "bitString_FixSize_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nFixedSize",nFixedSize :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let bitString_VarSize (sTypeDefName:string) (p:string) (sAcc:string) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (sErrCode:string) (nSizeInBits:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "bitString_VarSize_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nSizeInBits",nSizeInBits :>Object)]
    | Decode    ->
        ST.call "uper_rust" "bitString_VarSize_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nSizeInBits",nSizeInBits :>Object)]

let FixedSize_Fragmentation_sqf_64K (p:string) (sAcc:string) (sCurOffset:string) (sCurBlockSize:string) (sBlockIndex:string) (nBlocks64K:BigInteger) (sInternalItem:string) (sBLI:string) (sRemainingItemsVar:string) (bIsBitStringType:bool) (sErrCodeName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "FixedSize_Fragmentation_sqf_64K_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sCurOffset",(if sCurOffset = null then null else ST.StrHelper sCurOffset:>Object) );("sCurBlockSize",(if sCurBlockSize = null then null else ST.StrHelper sCurBlockSize:>Object) );("sBlockIndex",(if sBlockIndex = null then null else ST.StrHelper sBlockIndex:>Object) );("nBlocks64K",nBlocks64K :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("sBLI",(if sBLI = null then null else ST.StrHelper sBLI:>Object) );("sRemainingItemsVar",(if sRemainingItemsVar = null then null else ST.StrHelper sRemainingItemsVar:>Object) );("bIsBitStringType",bIsBitStringType :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "FixedSize_Fragmentation_sqf_64K_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sCurOffset",(if sCurOffset = null then null else ST.StrHelper sCurOffset:>Object) );("sCurBlockSize",(if sCurBlockSize = null then null else ST.StrHelper sCurBlockSize:>Object) );("sBlockIndex",(if sBlockIndex = null then null else ST.StrHelper sBlockIndex:>Object) );("nBlocks64K",nBlocks64K :>Object);("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("sBLI",(if sBLI = null then null else ST.StrHelper sBLI:>Object) );("sRemainingItemsVar",(if sRemainingItemsVar = null then null else ST.StrHelper sRemainingItemsVar:>Object) );("bIsBitStringType",bIsBitStringType :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) )]

let FixedSize_Fragmentation_sqf_small_block (p:string) (sAcc:string) (sInternalItem:string) (nBlockSize:BigInteger) (sBlockId:string) (sCurOffset:string) (sCurBlockSize:string) (sBLI:string) (sRemainingItemsVar:string) (bIsBitStringType:bool) (sErrCodeName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "FixedSize_Fragmentation_sqf_small_block_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nBlockSize",nBlockSize :>Object);("sBlockId",(if sBlockId = null then null else ST.StrHelper sBlockId:>Object) );("sCurOffset",(if sCurOffset = null then null else ST.StrHelper sCurOffset:>Object) );("sCurBlockSize",(if sCurBlockSize = null then null else ST.StrHelper sCurBlockSize:>Object) );("sBLI",(if sBLI = null then null else ST.StrHelper sBLI:>Object) );("sRemainingItemsVar",(if sRemainingItemsVar = null then null else ST.StrHelper sRemainingItemsVar:>Object) );("bIsBitStringType",bIsBitStringType :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "FixedSize_Fragmentation_sqf_small_block_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nBlockSize",nBlockSize :>Object);("sBlockId",(if sBlockId = null then null else ST.StrHelper sBlockId:>Object) );("sCurOffset",(if sCurOffset = null then null else ST.StrHelper sCurOffset:>Object) );("sCurBlockSize",(if sCurBlockSize = null then null else ST.StrHelper sCurBlockSize:>Object) );("sBLI",(if sBLI = null then null else ST.StrHelper sBLI:>Object) );("sRemainingItemsVar",(if sRemainingItemsVar = null then null else ST.StrHelper sRemainingItemsVar:>Object) );("bIsBitStringType",bIsBitStringType :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) )]

let FixedSize_Fragmentation_sqf_remaining (p:string) (sAcc:string) (sInternalItem:string) (bRemainingItemsWithinByte:bool) (nRemainingItemsVar:BigInteger) (sCurOffset:string) (sBLI:string) (sRemainingItemsVar:string) (bIsBitStringType:bool) (sErrCodeName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "FixedSize_Fragmentation_sqf_remaining_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("bRemainingItemsWithinByte",bRemainingItemsWithinByte :>Object);("nRemainingItemsVar",nRemainingItemsVar :>Object);("sCurOffset",(if sCurOffset = null then null else ST.StrHelper sCurOffset:>Object) );("sBLI",(if sBLI = null then null else ST.StrHelper sBLI:>Object) );("sRemainingItemsVar",(if sRemainingItemsVar = null then null else ST.StrHelper sRemainingItemsVar:>Object) );("bIsBitStringType",bIsBitStringType :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "FixedSize_Fragmentation_sqf_remaining_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("bRemainingItemsWithinByte",bRemainingItemsWithinByte :>Object);("nRemainingItemsVar",nRemainingItemsVar :>Object);("sCurOffset",(if sCurOffset = null then null else ST.StrHelper sCurOffset:>Object) );("sBLI",(if sBLI = null then null else ST.StrHelper sBLI:>Object) );("sRemainingItemsVar",(if sRemainingItemsVar = null then null else ST.StrHelper sRemainingItemsVar:>Object) );("bIsBitStringType",bIsBitStringType :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) )]

let FixedSize_Fragmentation_sqf (p:string) (sAcc:string) (arrsEncodingParts:seq<string>) (nFixedSize:BigInteger) (bIsBitStringType:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "FixedSize_Fragmentation_sqf_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsEncodingParts",(arrsEncodingParts|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nFixedSize",nFixedSize :>Object);("bIsBitStringType",bIsBitStringType :>Object)]
    | Decode    ->
        ST.call "uper_rust" "FixedSize_Fragmentation_sqf_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsEncodingParts",(arrsEncodingParts|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("nFixedSize",nFixedSize :>Object);("bIsBitStringType",bIsBitStringType :>Object)]

let Fragmentation_sqf (p:string) (sAcc:string) (sInternalItem:string) (nIntItemMaxSize:BigInteger) (nSizeMin:BigInteger) (nSizeMax:BigInteger) (nRequiredBitsForUPerEncoding:BigInteger) (bIsVariableSize:bool) (sErrCodeName:string) (sRemainingItemsVar:string) (sCurBlockSize:string) (sBlockIndex:string) (sCurOffset:string) (sBLJ:string) (sBLI:string) (sLengthTmp:string) (bIsBitStringType:bool) (bIsAsciiString:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "Fragmentation_sqf_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nIntItemMaxSize",nIntItemMaxSize :>Object);("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nRequiredBitsForUPerEncoding",nRequiredBitsForUPerEncoding :>Object);("bIsVariableSize",bIsVariableSize :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) );("sRemainingItemsVar",(if sRemainingItemsVar = null then null else ST.StrHelper sRemainingItemsVar:>Object) );("sCurBlockSize",(if sCurBlockSize = null then null else ST.StrHelper sCurBlockSize:>Object) );("sBlockIndex",(if sBlockIndex = null then null else ST.StrHelper sBlockIndex:>Object) );("sCurOffset",(if sCurOffset = null then null else ST.StrHelper sCurOffset:>Object) );("sBLJ",(if sBLJ = null then null else ST.StrHelper sBLJ:>Object) );("sBLI",(if sBLI = null then null else ST.StrHelper sBLI:>Object) );("sLengthTmp",(if sLengthTmp = null then null else ST.StrHelper sLengthTmp:>Object) );("bIsBitStringType",bIsBitStringType :>Object);("bIsAsciiString",bIsAsciiString :>Object)]
    | Decode    ->
        ST.call "uper_rust" "Fragmentation_sqf_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sInternalItem",(if sInternalItem = null then null else ST.StrHelper sInternalItem:>Object) );("nIntItemMaxSize",nIntItemMaxSize :>Object);("nSizeMin",nSizeMin :>Object);("nSizeMax",nSizeMax :>Object);("nRequiredBitsForUPerEncoding",nRequiredBitsForUPerEncoding :>Object);("bIsVariableSize",bIsVariableSize :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) );("sRemainingItemsVar",(if sRemainingItemsVar = null then null else ST.StrHelper sRemainingItemsVar:>Object) );("sCurBlockSize",(if sCurBlockSize = null then null else ST.StrHelper sCurBlockSize:>Object) );("sBlockIndex",(if sBlockIndex = null then null else ST.StrHelper sBlockIndex:>Object) );("sCurOffset",(if sCurOffset = null then null else ST.StrHelper sCurOffset:>Object) );("sBLJ",(if sBLJ = null then null else ST.StrHelper sBLJ:>Object) );("sBLI",(if sBLI = null then null else ST.StrHelper sBLI:>Object) );("sLengthTmp",(if sLengthTmp = null then null else ST.StrHelper sLengthTmp:>Object) );("bIsBitStringType",bIsBitStringType :>Object);("bIsAsciiString",bIsAsciiString :>Object)]

let octet_string_containing_func (p:string) (sFuncName:string) (sContainedType:string) (sReqBytesForUperEncoding:string) (nBits:BigInteger) (nMinSize:BigInteger) (nMaxSize:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "octet_string_containing_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sContainedType",(if sContainedType = null then null else ST.StrHelper sContainedType:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("nBits",nBits :>Object);("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object)]
    | Decode    ->
        ST.call "uper_rust" "octet_string_containing_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sContainedType",(if sContainedType = null then null else ST.StrHelper sContainedType:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("nBits",nBits :>Object);("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object)]

let bit_string_containing_func (p:string) (sFuncName:string) (sContainedType:string) (sReqBytesForUperEncoding:string) (sReqBitsForUperEncoding:string) (nBits:BigInteger) (nMinSize:BigInteger) (nMaxSize:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "bit_string_containing_func_encode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sContainedType",(if sContainedType = null then null else ST.StrHelper sContainedType:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("sReqBitsForUperEncoding",(if sReqBitsForUperEncoding = null then null else ST.StrHelper sReqBitsForUperEncoding:>Object) );("nBits",nBits :>Object);("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object)]
    | Decode    ->
        ST.call "uper_rust" "bit_string_containing_func_decode" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sContainedType",(if sContainedType = null then null else ST.StrHelper sContainedType:>Object) );("sReqBytesForUperEncoding",(if sReqBytesForUperEncoding = null then null else ST.StrHelper sReqBytesForUperEncoding:>Object) );("sReqBitsForUperEncoding",(if sReqBitsForUperEncoding = null then null else ST.StrHelper sReqBitsForUperEncoding:>Object) );("nBits",nBits :>Object);("nMinSize",nMinSize :>Object);("nMaxSize",nMaxSize :>Object)]

let sparkAnnotations (sTypeDefName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "sparkAnnotations_encode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "sparkAnnotations_decode" [("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]

let Null_declare (p:string) (sType:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "uper_rust" "Null_declare" [("p",p :>Object);("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let decode_nullType (p:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "uper_rust" "decode_nullType" [("p",p :>Object)]

let decode_empty_sequence_emptySeq (p:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "uper_rust" "decode_empty_sequence_emptySeq" [("p",p :>Object)]

let JoinItems (sPart:string) (soNestedPart:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "uper_rust" "JoinItems" [("sPart",(if sPart = null then null else ST.StrHelper sPart:>Object) );("soNestedPart",(if soNestedPart.IsNone then null else ST.StrHelper soNestedPart.Value:>Object) )]

let update_array_item (p:string) (sI:string) (sExpr:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "uper_rust" "update_array_item" [("p",p :>Object);("sI",(if sI = null then null else ST.StrHelper sI:>Object) );("sExpr",(if sExpr = null then null else ST.StrHelper sExpr:>Object) )]

let InternalItem_bit_str (p:string) (i:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "uper_rust" "InternalItem_bit_str_encode" [("p",p :>Object);("i",i :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "uper_rust" "InternalItem_bit_str_decode" [("p",p :>Object);("i",i :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

