module isvalid_rust
open System
open System.Numerics
open CommonTypes

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "rtlModuleName" []

let JoinItems (sPart:string) (soNestedPart:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "JoinItems" [("sPart",(if sPart = null then null else ST.StrHelper sPart:>Object) );("soNestedPart",(if soNestedPart.IsNone then null else ST.StrHelper soNestedPart.Value:>Object) )]

let JoinTwoIfFirstOk (sStr1:string) (sStr2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "JoinTwoIfFirstOk" [("sStr1",(if sStr1 = null then null else ST.StrHelper sStr1:>Object) );("sStr2",(if sStr2 = null then null else ST.StrHelper sStr2:>Object) )]

let JoinItems2 (sPart:string) (sNestedPart:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "JoinItems2" [("sPart",(if sPart = null then null else ST.StrHelper sPart:>Object) );("sNestedPart",(if sNestedPart = null then null else ST.StrHelper sNestedPart:>Object) )]

let always_true_statement () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "always_true_statement" []

let always_false_statement (sErrorCodeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "always_false_statement" [("sErrorCodeName",(if sErrorCodeName = null then null else ST.StrHelper sErrorCodeName:>Object) )]

let makeExpressionToStatement0 (sIsValidExp:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "makeExpressionToStatement0" [("sIsValidExp",(if sIsValidExp = null then null else ST.StrHelper sIsValidExp:>Object) )]

let convertVCBExpressionToStatementAndUpdateErrCode (sIsValidExp:string) (sErrCode:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "convertVCBExpressionToStatementAndUpdateErrCode" [("sIsValidExp",(if sIsValidExp = null then null else ST.StrHelper sIsValidExp:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let convertVCBStatementToStatementAndUpdateErrCode (sStatement:string) (sErrCode:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "convertVCBStatementToStatementAndUpdateErrCode" [("sStatement",(if sStatement = null then null else ST.StrHelper sStatement:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let convertVCBTRUEToStatementAndUpdateErrCode () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "convertVCBTRUEToStatementAndUpdateErrCode" []

let convertVCBFalseToStatementAndUpdateErrCode (sErrCode:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "convertVCBFalseToStatementAndUpdateErrCode" [("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) )]

let EmitTypeAssignment_composite_def_err_code (sErrCode:string) (nErrValue:BigInteger) (arrsErrorCodeComments:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "EmitTypeAssignment_composite_def_err_code" [("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("nErrValue",nErrValue :>Object);("arrsErrorCodeComments",(arrsErrorCodeComments|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let EmitTypeAssignment_composite_def (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "EmitTypeAssignment_composite_def" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("arrsErrcodes",(arrsErrcodes|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let EmitTypeAssignment_composite (sVarName:string) (sPtrPrefix:string) (sPtrSuffix:string) (sFuncName:string) (sTypeDefName:string) (sContent:string) (arrsAlphaFuncs:seq<string>) (arrsLocalVars:seq<string>) (bUnreferenced:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "EmitTypeAssignment_composite" [("sVarName",(if sVarName = null then null else ST.StrHelper sVarName:>Object) );("sPtrPrefix",(if sPtrPrefix = null then null else ST.StrHelper sPtrPrefix:>Object) );("sPtrSuffix",(if sPtrSuffix = null then null else ST.StrHelper sPtrSuffix:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("arrsAlphaFuncs",(arrsAlphaFuncs|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsLocalVars",(arrsLocalVars|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bUnreferenced",bUnreferenced :>Object)]

let GetEnumIndexByName (sEnumValuesArray:string) (sEnumValuesArrayCount:string) (sExp:string) (bBinarySearch:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "GetEnumIndexByName" [("sEnumValuesArray",(if sEnumValuesArray = null then null else ST.StrHelper sEnumValuesArray:>Object) );("sEnumValuesArrayCount",(if sEnumValuesArrayCount = null then null else ST.StrHelper sEnumValuesArrayCount:>Object) );("sExp",(if sExp = null then null else ST.StrHelper sExp:>Object) );("bBinarySearch",bBinarySearch :>Object)]

let ExpEqual (sExp1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpEqual" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let ExpStringEqual (sExp1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpStringEqual" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let ExpGt (sExp1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpGt" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let ExpGte (sExp1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpGte" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let ExpLt (sExp1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpLt" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let ExpLte (sExp1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpLte" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let ExpOr (sExp1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpOr" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let ExpAnd (sExp1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpAnd" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let ExpAndMulti (arrsExp:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpAndMulti" [("arrsExp",(arrsExp|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let ExpNot (sExp:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpNot" [("sExp",(if sExp = null then null else ST.StrHelper sExp:>Object) )]

let StrLen (sExp:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StrLen" [("sExp",(if sExp = null then null else ST.StrHelper sExp:>Object) )]

let ArrayLen (sExp:string) (sAcc:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ArrayLen" [("sExp",(if sExp = null then null else ST.StrHelper sExp:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) )]

let ExpressionToStatement (sExp1:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpressionToStatement" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) )]

let StatementOrStatement (sStat1:string) (sStat2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StatementOrStatement" [("sStat1",(if sStat1 = null then null else ST.StrHelper sStat1:>Object) );("sStat2",(if sStat2 = null then null else ST.StrHelper sStat2:>Object) )]

let ExpressionOrStatement (sExp1:string) (sStat2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpressionOrStatement" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sStat2",(if sStat2 = null then null else ST.StrHelper sStat2:>Object) )]

let StatementOrExpression (sStat1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StatementOrExpression" [("sStat1",(if sStat1 = null then null else ST.StrHelper sStat1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let StatementAndStatement (sStat1:string) (sStat2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StatementAndStatement" [("sStat1",(if sStat1 = null then null else ST.StrHelper sStat1:>Object) );("sStat2",(if sStat2 = null then null else ST.StrHelper sStat2:>Object) )]

let ExpressionAndStatement (sExp1:string) (sStat2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpressionAndStatement" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sStat2",(if sStat2 = null then null else ST.StrHelper sStat2:>Object) )]

let StatementAndExpression (sStat1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StatementAndExpression" [("sStat1",(if sStat1 = null then null else ST.StrHelper sStat1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let StatementNot (sStat:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StatementNot" [("sStat",(if sStat = null then null else ST.StrHelper sStat:>Object) )]

let StatementExceptStatement (sStat1:string) (sStat2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StatementExceptStatement" [("sStat1",(if sStat1 = null then null else ST.StrHelper sStat1:>Object) );("sStat2",(if sStat2 = null then null else ST.StrHelper sStat2:>Object) )]

let ExpressionExceptStatement (sExp1:string) (sStat2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExpressionExceptStatement" [("sExp1",(if sExp1 = null then null else ST.StrHelper sExp1:>Object) );("sStat2",(if sStat2 = null then null else ST.StrHelper sStat2:>Object) )]

let StatementExceptExpression (sStat1:string) (sExp2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StatementExceptExpression" [("sStat1",(if sStat1 = null then null else ST.StrHelper sStat1:>Object) );("sExp2",(if sExp2 = null then null else ST.StrHelper sExp2:>Object) )]

let StatementForLoop (p:string) (sAcc:string) (i:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) (sInnerStatement:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "StatementForLoop" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("bIsFixedSize",bIsFixedSize :>Object);("nFixedSize",nFixedSize :>Object);("sInnerStatement",(if sInnerStatement = null then null else ST.StrHelper sInnerStatement:>Object) )]

let Print_AlphabetCheckFunc (sFuncName:string) (arrsAlphaConBody:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "Print_AlphabetCheckFunc" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("arrsAlphaConBody",(arrsAlphaConBody|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let SingleValConstraint (p:string) (v:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "SingleValConstraint" [("p",p :>Object);("v",v :>Object)]

let stringContainsChar (sStrVal:string) (p:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "stringContainsChar" [("sStrVal",(if sStrVal = null then null else ST.StrHelper sStrVal:>Object) );("p",p :>Object)]

let RangeConstraint (p:string) (v1:string) (v2:string) (bMin:bool) (bMax:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "RangeConstraint" [("p",p :>Object);("v1",v1 :>Object);("v2",v2 :>Object);("bMin",bMin :>Object);("bMax",bMax :>Object)]

let RangeConstraint_val_MAX (p:string) (v:string) (bMin:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "RangeConstraint_val_MAX" [("p",p :>Object);("v",v :>Object);("bMin",bMin :>Object)]

let RangeConstraint_MIN_val (p:string) (v:string) (bMax:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "RangeConstraint_MIN_val" [("p",p :>Object);("v",v :>Object);("bMax",bMax :>Object)]

let AND_Constraint (sCon1:string) (sCon2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "AND_Constraint" [("sCon1",(if sCon1 = null then null else ST.StrHelper sCon1:>Object) );("sCon2",(if sCon2 = null then null else ST.StrHelper sCon2:>Object) )]

let OR_Constraint (sCon1:string) (sCon2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "OR_Constraint" [("sCon1",(if sCon1 = null then null else ST.StrHelper sCon1:>Object) );("sCon2",(if sCon2 = null then null else ST.StrHelper sCon2:>Object) )]

let AllExceptConstraint (sCon:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "AllExceptConstraint" [("sCon",(if sCon = null then null else ST.StrHelper sCon:>Object) )]

let ExceptConstraint (sCon1:string) (sCon2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "ExceptConstraint" [("sCon1",(if sCon1 = null then null else ST.StrHelper sCon1:>Object) );("sCon2",(if sCon2 = null then null else ST.StrHelper sCon2:>Object) )]

let methodNameSuffix () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "methodNameSuffix" []

let callAlphaFunc (sFuncName:string) (p:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "callAlphaFunc" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("p",p :>Object)]

let callAlphaFuncWithIndex (sFuncName:string) (p:string) (nIdx:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "callAlphaFuncWithIndex" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("p",p :>Object);("nIdx",nIdx :>Object)]

let alphaFuncBaseName (p:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "alphaFuncBaseName" [("p",p :>Object)]

let alphaFuncNameWithIndex (p:string) (nIdx:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "alphaFuncNameWithIndex" [("p",p :>Object);("nIdx",nIdx :>Object)]

let PrintMultipleConstraints (arrsConstraints:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "PrintMultipleConstraints" [("arrsConstraints",(arrsConstraints|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let Emit_type (arrsConstraints:seq<string>) (sErrCodeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "Emit_type" [("arrsConstraints",(arrsConstraints|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCodeName",(if sErrCodeName = null then null else ST.StrHelper sErrCodeName:>Object) )]

let call_base_type_func (p:string) (sFuncName:string) (soTypeCasting:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "call_base_type_func" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("soTypeCasting",(if soTypeCasting.IsNone then null else ST.StrHelper soTypeCasting.Value:>Object) )]

let call_superclass_func (p:string) (sFuncName:string) (soTypeCasting:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "call_superclass_func" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("soTypeCasting",(if soTypeCasting.IsNone then null else ST.StrHelper soTypeCasting.Value:>Object) )]

let call_base_type_func_exp (p:string) (sFuncName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "call_base_type_func_exp" [("p",p :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]

let Sequence_OptionalChild (p:string) (sAcc:string) (sChName:string) (sInnerStatement:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "Sequence_OptionalChild" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sInnerStatement",(if sInnerStatement = null then null else ST.StrHelper sInnerStatement:>Object) )]

let Sequence_optional_child_always_present_or_absent (p:string) (sAcc:string) (sChName:string) (sErrCode:string) (sPresOrAbs:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "Sequence_optional_child_always_present_or_absent" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sErrCode",(if sErrCode = null then null else ST.StrHelper sErrCode:>Object) );("sPresOrAbs",(if sPresOrAbs = null then null else ST.StrHelper sPresOrAbs:>Object) )]

let Sequence_optional_child_always_present_or_absent_expr (p:string) (sAcc:string) (sChName:string) (sPresOrAbs:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "Sequence_optional_child_always_present_or_absent_expr" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("sPresOrAbs",(if sPresOrAbs = null then null else ST.StrHelper sPresOrAbs:>Object) )]

let Choice_OptionalChild (p:string) (sPLocal:string) (sAcc:string) (sChPresent:string) (sInnerStatement:string) (sChoiceTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "Choice_OptionalChild" [("p",p :>Object);("sPLocal",(if sPLocal = null then null else ST.StrHelper sPLocal:>Object) );("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChPresent",(if sChPresent = null then null else ST.StrHelper sChPresent:>Object) );("sInnerStatement",(if sInnerStatement = null then null else ST.StrHelper sInnerStatement:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]

let Choice_child_always_present_Exp (p:string) (sAcc:string) (sChPresent:string) (sChoiceTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "Choice_child_always_present_Exp" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChPresent",(if sChPresent = null then null else ST.StrHelper sChPresent:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]

let Choice_child_always_absent_Exp (p:string) (sAcc:string) (sChPresent:string) (sChoiceTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "Choice_child_always_absent_Exp" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("sChPresent",(if sChPresent = null then null else ST.StrHelper sChPresent:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) )]

let choice_child (sChPresent:string) (sChildBody:string) (bAlwaysAbsent:bool) (sChildName:string) (sChoiceTypeName:string) (sChildTypeName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "choice_child" [("sChPresent",(if sChPresent = null then null else ST.StrHelper sChPresent:>Object) );("sChildBody",(if sChildBody = null then null else ST.StrHelper sChildBody:>Object) );("bAlwaysAbsent",bAlwaysAbsent :>Object);("sChildName",(if sChildName = null then null else ST.StrHelper sChildName:>Object) );("sChoiceTypeName",(if sChoiceTypeName = null then null else ST.StrHelper sChoiceTypeName:>Object) );("sChildTypeName",(if sChildTypeName = null then null else ST.StrHelper sChildTypeName:>Object) )]

let choice (p:string) (sAccess:string) (arrsChildren:seq<string>) (sErrCodeForInvalidCase:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "choice" [("p",p :>Object);("sAccess",(if sAccess = null then null else ST.StrHelper sAccess:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sErrCodeForInvalidCase",(if sErrCodeForInvalidCase = null then null else ST.StrHelper sErrCodeForInvalidCase:>Object) )]

let sequenceOf (p:string) (sAcc:string) (i:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) (soIsValidSizeExp:string option) (soErrCode:string option) (soInnerStatement:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "sequenceOf" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("bIsFixedSize",bIsFixedSize :>Object);("nFixedSize",nFixedSize :>Object);("soIsValidSizeExp",(if soIsValidSizeExp.IsNone then null else ST.StrHelper soIsValidSizeExp.Value:>Object) );("soErrCode",(if soErrCode.IsNone then null else ST.StrHelper soErrCode.Value:>Object) );("soInnerStatement",(if soInnerStatement.IsNone then null else ST.StrHelper soInnerStatement.Value:>Object) )]

let sequenceOf2 (p:string) (sAcc:string) (i:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) (sInnerStatement:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "sequenceOf2" [("p",p :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("bIsFixedSize",bIsFixedSize :>Object);("nFixedSize",nFixedSize :>Object);("sInnerStatement",(if sInnerStatement = null then null else ST.StrHelper sInnerStatement:>Object) )]

let octet_var_string_equal (p:string) (sAccess:string) (nVarLength:BigInteger) (sOctArrayLiteral:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "octet_var_string_equal" [("p",p :>Object);("sAccess",(if sAccess = null then null else ST.StrHelper sAccess:>Object) );("nVarLength",nVarLength :>Object);("sOctArrayLiteral",(if sOctArrayLiteral = null then null else ST.StrHelper sOctArrayLiteral:>Object) )]

let octet_fix_string_equal (p:string) (sAccess:string) (nFixedSize:BigInteger) (nVarLength:BigInteger) (sOctArrayLiteral:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "octet_fix_string_equal" [("p",p :>Object);("sAccess",(if sAccess = null then null else ST.StrHelper sAccess:>Object) );("nFixedSize",nFixedSize :>Object);("nVarLength",nVarLength :>Object);("sOctArrayLiteral",(if sOctArrayLiteral = null then null else ST.StrHelper sOctArrayLiteral:>Object) )]

let bit_var_string_equal (p:string) (sAccess:string) (nVarLength:BigInteger) (sOctArrayLiteral:string) (sBitArrayLiteral:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "bit_var_string_equal" [("p",p :>Object);("sAccess",(if sAccess = null then null else ST.StrHelper sAccess:>Object) );("nVarLength",nVarLength :>Object);("sOctArrayLiteral",(if sOctArrayLiteral = null then null else ST.StrHelper sOctArrayLiteral:>Object) );("sBitArrayLiteral",(if sBitArrayLiteral = null then null else ST.StrHelper sBitArrayLiteral:>Object) )]

let bit_fix_string_equal (p:string) (sAccess:string) (nFixedSize:BigInteger) (nVarLength:BigInteger) (sOctArrayLiteral:string) (sBitArrayLiteral:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "bit_fix_string_equal" [("p",p :>Object);("sAccess",(if sAccess = null then null else ST.StrHelper sAccess:>Object) );("nFixedSize",nFixedSize :>Object);("nVarLength",nVarLength :>Object);("sOctArrayLiteral",(if sOctArrayLiteral = null then null else ST.StrHelper sOctArrayLiteral:>Object) );("sBitArrayLiteral",(if sBitArrayLiteral = null then null else ST.StrHelper sBitArrayLiteral:>Object) )]

let objId_equal (p:string) (sObjIdLiteral:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "isvalid_rust" "objId_equal" [("p",p :>Object);("sObjIdLiteral",(if sObjIdLiteral = null then null else ST.StrHelper sObjIdLiteral:>Object) )]

