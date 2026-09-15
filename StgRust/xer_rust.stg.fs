module xer_rust
open System
open System.Numerics
open CommonTypes

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "xer_rust" "rtlModuleName" []

let EmitTypeAssignment_def_err_code (sErrCode:string) (nErrValue:BigInteger) (sFieldPath:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "xer_rust" "EmitTypeAssignment_def_err_code" [("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nErrValue",nErrValue :>Object);("sFieldPath",(if sFieldPath = null then null else ST.StrHelper sFieldPath:>Object) )]

let EmitTypeAssignment_def (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) (bEmptyEncodingSpace:bool) (nMaxBytesInXER:BigInteger) (soSparkAnnotations:string option) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "EmitTypeAssignment_def_encode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsErrcodes",(arrsErrcodes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bEmptyEncodingSpace",bEmptyEncodingSpace :>Object);("nMaxBytesInXER",nMaxBytesInXER :>Object);("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "EmitTypeAssignment_def_decode" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsErrcodes",(arrsErrcodes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bEmptyEncodingSpace",bEmptyEncodingSpace :>Object);("nMaxBytesInXER",nMaxBytesInXER :>Object);("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) )]

let EmitTypeAssignment (sTasName:string) (sVarName:string) (sStar:string) (sFuncName:string) (soIValidFuncName:string option) (sTypeDefName:string) (arrsLocalVariables:seq<string>) (sContent:string) (soSparkAnnotations:string option) (sInitialExp:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "EmitTypeAssignment_encode" [("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("soIValidFuncName",(if soIValidFuncName.IsNone then null else ST.StrHelper soIValidFuncName.Value:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsLocalVariables",(arrsLocalVariables|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) );("sInitialExp",(if sInitialExp = null then null else ST.StrHelper sInitialExp:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "EmitTypeAssignment_decode" [("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("soIValidFuncName",(if soIValidFuncName.IsNone then null else ST.StrHelper soIValidFuncName.Value:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsLocalVariables",(arrsLocalVariables|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("soSparkAnnotations",(if soSparkAnnotations.IsNone then null else ST.StrHelper soSparkAnnotations.Value:>Object) );("sInitialExp",(if sInitialExp = null then null else ST.StrHelper sInitialExp:>Object) )]

let Integer (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Integer_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "Integer_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let IntegerPos (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "IntegerPos_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "IntegerPos_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let Boolean (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Boolean_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "Boolean_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let Real (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Real_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "Real_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let ObjectIdentifier (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "ObjectIdentifier_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "ObjectIdentifier_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let TimeType (p:string) (sTimeSubType:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "TimeType_encode" [("p",p :>Object);("sTimeSubType",(if sTimeSubType = null then null else ST.StrHelper sTimeSubType:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "TimeType_decode" [("p",p :>Object);("sTimeSubType",(if sTimeSubType = null then null else ST.StrHelper sTimeSubType:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let Null (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Null_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "Null_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let String (p:string) (sTag:string) (nLevel:BigInteger) (soCheckExp:string option) (sErrCode:string) (nMaxLength:BigInteger) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "String_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nMaxLength",nMaxLength :>Object)]
    | Decode    ->
        ST.call "xer_rust" "String_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nMaxLength",nMaxLength :>Object)]

let Enumerated_item (p:string) (sTag:string) (nLevel:BigInteger) (sItemID:string) (sXerValue:string) (sErrCode:string) (bFirst:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Enumerated_item_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sItemID",(if sItemID = null then null else ST.StrHelper sItemID:>Object) );("sXerValue",(if sXerValue = null then null else ST.StrHelper sXerValue:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bFirst",bFirst :>Object)]
    | Decode    ->
        ST.call "xer_rust" "Enumerated_item_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sItemID",(if sItemID = null then null else ST.StrHelper sItemID:>Object) );("sXerValue",(if sXerValue = null then null else ST.StrHelper sXerValue:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bFirst",bFirst :>Object)]

let Enumerated (p:string) (sTag:string) (nLevel:BigInteger) (arrsItems:seq<string>) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Enumerated_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("arrsItems",(arrsItems|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "Enumerated_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("arrsItems",(arrsItems|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let OctetString (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (nSizeMax:BigInteger) (bIsFixedSize:bool) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "OctetString_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("nSizeMax",nSizeMax :>Object);("bIsFixedSize",bIsFixedSize :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "OctetString_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("nSizeMax",nSizeMax :>Object);("bIsFixedSize",bIsFixedSize :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let BitString (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (nSizeMax:BigInteger) (bIsFixedSize:bool) (soCheckExp:string option) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "BitString_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("nSizeMax",nSizeMax :>Object);("bIsFixedSize",bIsFixedSize :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "BitString_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("nSizeMax",nSizeMax :>Object);("bIsFixedSize",bIsFixedSize :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let SequenceOf (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (sI:string) (nSizeMax:BigInteger) (sChildBody:string) (bFixedSize:bool) (soCheckExp:string option) (sErrCode:string) (sElemTypeDef:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "SequenceOf_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sI",(if sI = null then null else ST.StrHelper sI:>Object) );("nSizeMax",nSizeMax :>Object);("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("bFixedSize",bFixedSize :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sElemTypeDef",(if sElemTypeDef = null then null else ST.StrHelper sElemTypeDef:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "SequenceOf_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sI",(if sI = null then null else ST.StrHelper sI:>Object) );("nSizeMax",nSizeMax :>Object);("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("bFixedSize",bFixedSize :>Object);("soCheckExp",(if soCheckExp.IsNone then null else ST.StrHelper soCheckExp.Value:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sElemTypeDef",(if sElemTypeDef = null then null else ST.StrHelper sElemTypeDef:>Object) )]

let Sequence_mandatory_child (sChName:string) (sChildContent:string) (sChildTag:string) (sChildTypeDef:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Sequence_mandatory_child_encode" [("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTag",(if sChildTag = null then null else ST.StrHelper sChildTag:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "Sequence_mandatory_child_decode" [("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTag",(if sChildTag = null then null else ST.StrHelper sChildTag:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) )]

let Sequence_optional_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTag:string) (sChildTypeDef:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Sequence_optional_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTag",(if sChildTag = null then null else ST.StrHelper sChildTag:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "Sequence_optional_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTag",(if sChildTag = null then null else ST.StrHelper sChildTag:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) )]

let Sequence_default_child (p:string) (sAcc:string) (sChName:string) (sChildContent:string) (sChildTag:string) (sInitWithDefaultValue:string) (sChildTypeDef:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "Sequence_default_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTag",(if sChildTag = null then null else ST.StrHelper sChildTag:>Object) );("sInitWithDefaultValue",(if sInitWithDefaultValue = null then null else ST.StrHelper sInitWithDefaultValue:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "Sequence_default_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sChildContent",(if sChildContent = null then null else ST.StrHelper sChildContent:>Object) );("sChildTag",(if sChildTag = null then null else ST.StrHelper sChildTag:>Object) );("sInitWithDefaultValue",(if sInitWithDefaultValue = null then null else ST.StrHelper sInitWithDefaultValue:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) )]

let SEQUENCE_start (p:string) (sTag:string) (nLevel:BigInteger) (sErrCode:string) (bEmptySequence:bool) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "SEQUENCE_start_encode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bEmptySequence",bEmptySequence :>Object)]
    | Decode    ->
        ST.call "xer_rust" "SEQUENCE_start_decode" [("p",p :>Object);("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("bEmptySequence",bEmptySequence :>Object)]

let SEQUENCE_end (sTag:string) (nLevel:BigInteger) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "SEQUENCE_end_encode" [("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "SEQUENCE_end_decode" [("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let SEQUENCE_xer (sChildren:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "SEQUENCE_xer_encode" [("sChildren",(if sChildren = null then null else ST.StrHelper sChildren:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "SEQUENCE_xer_decode" [("sChildren",(if sChildren = null then null else ST.StrHelper sChildren:>Object) )]

let CHOICE_child (p:string) (sAcc:string) (sChID:string) (sChildBody:string) (bFirst:bool) (sChildTag:string) (sChildName:string) (sChildTypeDef:string) (sChoiceTypeName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "CHOICE_child_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChID",(if sChID = null then null else ST.StrHelper sChID:>Object) );("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("bFirst",bFirst :>Object);("sChildTag",(if sChildTag = null then null else ST.StrHelper sChildTag:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "CHOICE_child_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChID",(if sChID = null then null else ST.StrHelper sChID:>Object) );("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("bFirst",bFirst :>Object);("sChildTag",(if sChildTag = null then null else ST.StrHelper sChildTag:>Object) );("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChildTypeDef",(if sChildTypeDef = null then null else ST.StrHelper sChildTypeDef:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]

let CHOICE_no_tag (p:string) (sAcc:string) (arrsChildren:seq<string>) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "CHOICE_no_tag_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "CHOICE_no_tag_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let CHOICE (p:string) (sAcc:string) (sTag:string) (nLevel:BigInteger) (sMainBody:string) (sErrCode:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "CHOICE_encode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sMainBody",(if sMainBody = null then null else ST.StrHelper sMainBody:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "CHOICE_decode" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sTag",(if sTag = null then null else ST.StrHelper sTag:>Object) );("nLevel",nLevel :>Object);("sMainBody",(if sMainBody = null then null else ST.StrHelper sMainBody:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let call_base_type_func (p:string) (soXmlTag:string option) (sFuncName:string) (sBaseTypeName:string) codec =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    match codec with
    | Encode    ->
        ST.call "xer_rust" "call_base_type_func_encode" [("p",p :>Object);("soXmlTag",(if soXmlTag.IsNone then null else ST.StrHelper soXmlTag.Value:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sBaseTypeName",(if sBaseTypeName = null then null else ST.StrHelper sBaseTypeName:>Object) )]
    | Decode    ->
        ST.call "xer_rust" "call_base_type_func_decode" [("p",p :>Object);("soXmlTag",(if soXmlTag.IsNone then null else ST.StrHelper soXmlTag.Value:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sBaseTypeName",(if sBaseTypeName = null then null else ST.StrHelper sBaseTypeName:>Object) )]

