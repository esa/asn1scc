module equal_rust
open System
open System.Numerics
open CommonTypes

let JoinItems (sPart:string) (soNestedPart:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "JoinItems" [("sPart",(if sPart = null then null else ST.StrHelper sPart:>Object) );("soNestedPart",(if soNestedPart.IsNone then null else ST.StrHelper soNestedPart.Value:>Object) )]

let JoinItems2_ret (sPart:string) (sNestedPart:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "JoinItems2_ret" [("sPart",(if sPart = null then null else ST.StrHelper sPart:>Object) );("sNestedPart",(if sNestedPart = null then null else ST.StrHelper sNestedPart:>Object) )]

let JoinItems2_ret_result (sPart:string) (sNestedPart:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "JoinItems2_ret_result" [("sPart",(if sPart = null then null else ST.StrHelper sPart:>Object) );("sNestedPart",(if sNestedPart = null then null else ST.StrHelper sNestedPart:>Object) )]

let PrintEqualDefinitionPrimitive (sFuncName:string) (sTypeDefName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "PrintEqualDefinitionPrimitive" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]

let PrintEqualDefinitionComposite (sFuncName:string) (sTypeDefName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "PrintEqualDefinitionComposite" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]

let PrintEqualPrimitive (sFuncName:string) (sTypeDefName:string) (sContent:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "PrintEqualPrimitive" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) )]

let PrintEqualComposite (sFuncName:string) (sTypeDefName:string) (sContent:string) (arrsLocalVars:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "PrintEqualComposite" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("arrsLocalVars",(arrsLocalVars|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let equalTypeAssignment_def (sVarName1:string) (sVarName2:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "equalTypeAssignment_def" [("sVarName1",(if sVarName1 = null then null else ST.StrHelper sVarName1:>Object) );("sVarName2",(if sVarName2 = null then null else ST.StrHelper sVarName2:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) )]

let equalTypeAssignment (sVarName1:string) (sVarName2:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (sContent:string) (arrsLocalVars:seq<string>) (bExpression:bool) (bUnreferenced:bool) (bInherit:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "equalTypeAssignment" [("sVarName1",(if sVarName1 = null then null else ST.StrHelper sVarName1:>Object) );("sVarName2",(if sVarName2 = null then null else ST.StrHelper sVarName2:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTypeDefName",(if sTypeDefName = null then null else ST.StrHelper sTypeDefName:>Object) );("sContent",(if sContent = null then null else ST.StrHelper sContent:>Object) );("arrsLocalVars",(arrsLocalVars|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bExpression",bExpression :>Object);("bUnreferenced",bUnreferenced :>Object);("bInherit",bInherit :>Object)]

let AssignTrue () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "AssignTrue" []

let isEqual_Primitive (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_Primitive" [("p1",p1 :>Object);("p2",p2 :>Object)]

let isEqual_String (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_String" [("p1",p1 :>Object);("p2",p2 :>Object)]

let isEqual_Integer (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_Integer" [("p1",p1 :>Object);("p2",p2 :>Object)]

let isEqual_Enumerated (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_Enumerated" [("p1",p1 :>Object);("p2",p2 :>Object)]

let isEqual_Boolean (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_Boolean" [("p1",p1 :>Object);("p2",p2 :>Object)]

let isEqual_Real (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_Real" [("p1",p1 :>Object);("p2",p2 :>Object)]

let isEqual_BitString (p1:string) (p2:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_BitString" [("p1",p1 :>Object);("p2",p2 :>Object);("bIsFixedSize",bIsFixedSize :>Object);("nFixedSize",nFixedSize :>Object)]

let isEqual_OctetString (p1:string) (p2:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_OctetString" [("p1",p1 :>Object);("p2",p2 :>Object);("bIsFixedSize",bIsFixedSize :>Object);("nFixedSize",nFixedSize :>Object)]

let isObjectIdentifier_equal (p1:string) (p2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isObjectIdentifier_equal" [("p1",p1 :>Object);("p2",p2 :>Object)]

let isEqual_Choice_Child (sChoiceTypeDefName:string) (sCid:string) (sInnerStatement:string) (sTmpVarName1:string) (sTmpVarName2:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_Choice_Child" [("sChoiceTypeDefName",(if sChoiceTypeDefName = null then null else ST.StrHelper sChoiceTypeDefName:>Object) );("sCid",(if sCid = null then null else ST.StrHelper sCid:>Object) );("sInnerStatement",(if sInnerStatement = null then null else ST.StrHelper sInnerStatement:>Object) );("sTmpVarName1",(if sTmpVarName1 = null then null else ST.StrHelper sTmpVarName1:>Object) );("sTmpVarName2",(if sTmpVarName2 = null then null else ST.StrHelper sTmpVarName2:>Object) )]

let isEqual_Choice (p1:string) (p2:string) (sAccess:string) (arrsChildren:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_Choice" [("p1",p1 :>Object);("p2",p2 :>Object);("sAccess",(if sAccess = null then null else ST.StrHelper sAccess:>Object) );("arrsChildren",(arrsChildren|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let isEqual_Sequence_child (p1:string) (p2:string) (sAcc:string) (bIsOptional:bool) (sChName:string) (soInnerStatement:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_Sequence_child" [("p1",p1 :>Object);("p2",p2 :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("bIsOptional",bIsOptional :>Object);("sChName",(if sChName = null then null else ST.StrHelper sChName:>Object) );("soInnerStatement",(if soInnerStatement.IsNone then null else ST.StrHelper soInnerStatement.Value:>Object) )]

let isEqual_SequenceOf_var_size (p1:string) (p2:string) (sAcc:string) (i:string) (soInnerStatement:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_SequenceOf_var_size" [("p1",p1 :>Object);("p2",p2 :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("soInnerStatement",(if soInnerStatement.IsNone then null else ST.StrHelper soInnerStatement.Value:>Object) )]

let isEqual_SequenceOf_fix_size (p1:string) (p2:string) (sAcc:string) (i:string) (nFixedSize:BigInteger) (sInnerStatement:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "isEqual_SequenceOf_fix_size" [("p1",p1 :>Object);("p2",p2 :>Object);("sAcc",(if sAcc = null then null else ST.StrHelper sAcc:>Object) );("i",i :>Object);("nFixedSize",nFixedSize :>Object);("sInnerStatement",(if sInnerStatement = null then null else ST.StrHelper sInnerStatement:>Object) )]

let call_base_type_func (p1:string) (p2:string) (sFuncName:string) (bIsP1Option:bool) (bIsP2Option:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "call_base_type_func" [("p1",p1 :>Object);("p2",p2 :>Object);("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("bIsP1Option",bIsP1Option :>Object);("bIsP2Option",bIsP2Option :>Object)]

let makeExpressionToStatement (sIsValidExp:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "makeExpressionToStatement" [("sIsValidExp",(if sIsValidExp = null then null else ST.StrHelper sIsValidExp:>Object) )]

let callChildEqualFunc (p1:string) (p2:string) (sChildEqualFuncName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "equal_rust" "callChildEqualFunc" [("p1",p1 :>Object);("p2",p2 :>Object);("sChildEqualFuncName",(if sChildEqualFuncName = null then null else ST.StrHelper sChildEqualFuncName:>Object) )]

