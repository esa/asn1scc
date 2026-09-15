module IEqual_rust
open System
open System.Numerics
open CommonTypes

type IEqual_rust() =
    inherit AbstractMacros.IEqual()
        override this.JoinItems  (sPart:string) (soNestedPart:string option) =
            equal_rust.JoinItems  sPart soNestedPart 
        override this.JoinItems2_ret  (sPart:string) (sNestedPart:string) =
            equal_rust.JoinItems2_ret  sPart sNestedPart 
        override this.JoinItems2_ret_result  (sPart:string) (sNestedPart:string) =
            equal_rust.JoinItems2_ret_result  sPart sNestedPart 
        override this.PrintEqualDefinitionPrimitive  (sFuncName:string) (sTypeDefName:string) =
            equal_rust.PrintEqualDefinitionPrimitive  sFuncName sTypeDefName 
        override this.PrintEqualDefinitionComposite  (sFuncName:string) (sTypeDefName:string) =
            equal_rust.PrintEqualDefinitionComposite  sFuncName sTypeDefName 
        override this.PrintEqualPrimitive  (sFuncName:string) (sTypeDefName:string) (sContent:string) =
            equal_rust.PrintEqualPrimitive  sFuncName sTypeDefName sContent 
        override this.PrintEqualComposite  (sFuncName:string) (sTypeDefName:string) (sContent:string) (arrsLocalVars:seq<string>) =
            equal_rust.PrintEqualComposite  sFuncName sTypeDefName sContent arrsLocalVars 
        override this.equalTypeAssignment_def  (sVarName1:string) (sVarName2:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) =
            equal_rust.equalTypeAssignment_def  sVarName1 sVarName2 sStar sFuncName sTypeDefName 
        override this.equalTypeAssignment  (sVarName1:string) (sVarName2:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (sContent:string) (arrsLocalVars:seq<string>) (bExpression:bool) (bUnreferenced:bool) (bInherit:bool) =
            equal_rust.equalTypeAssignment  sVarName1 sVarName2 sStar sFuncName sTypeDefName sContent arrsLocalVars bExpression bUnreferenced bInherit 
        override this.AssignTrue  () =
            equal_rust.AssignTrue  () 
        override this.isEqual_Primitive  (p1:string) (p2:string) =
            equal_rust.isEqual_Primitive  p1 p2 
        override this.isEqual_String  (p1:string) (p2:string) =
            equal_rust.isEqual_String  p1 p2 
        override this.isEqual_Integer  (p1:string) (p2:string) =
            equal_rust.isEqual_Integer  p1 p2 
        override this.isEqual_Enumerated  (p1:string) (p2:string) =
            equal_rust.isEqual_Enumerated  p1 p2 
        override this.isEqual_Boolean  (p1:string) (p2:string) =
            equal_rust.isEqual_Boolean  p1 p2 
        override this.isEqual_Real  (p1:string) (p2:string) =
            equal_rust.isEqual_Real  p1 p2 
        override this.isEqual_BitString  (p1:string) (p2:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) =
            equal_rust.isEqual_BitString  p1 p2 bIsFixedSize nFixedSize 
        override this.isEqual_OctetString  (p1:string) (p2:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) =
            equal_rust.isEqual_OctetString  p1 p2 bIsFixedSize nFixedSize 
        override this.isObjectIdentifier_equal  (p1:string) (p2:string) =
            equal_rust.isObjectIdentifier_equal  p1 p2 
        override this.isEqual_Choice_Child  (sChoiceTypeDefName:string) (sCid:string) (sInnerStatement:string) (sTmpVarName1:string) (sTmpVarName2:string) =
            equal_rust.isEqual_Choice_Child  sChoiceTypeDefName sCid sInnerStatement sTmpVarName1 sTmpVarName2 
        override this.isEqual_Choice  (p1:string) (p2:string) (sAccess:string) (arrsChildren:seq<string>) =
            equal_rust.isEqual_Choice  p1 p2 sAccess arrsChildren 
        override this.isEqual_Sequence_child  (p1:string) (p2:string) (sAcc:string) (bIsOptional:bool) (sChName:string) (soInnerStatement:string option) =
            equal_rust.isEqual_Sequence_child  p1 p2 sAcc bIsOptional sChName soInnerStatement 
        override this.isEqual_SequenceOf_var_size  (p1:string) (p2:string) (sAcc:string) (i:string) (soInnerStatement:string option) =
            equal_rust.isEqual_SequenceOf_var_size  p1 p2 sAcc i soInnerStatement 
        override this.isEqual_SequenceOf_fix_size  (p1:string) (p2:string) (sAcc:string) (i:string) (nFixedSize:BigInteger) (sInnerStatement:string) =
            equal_rust.isEqual_SequenceOf_fix_size  p1 p2 sAcc i nFixedSize sInnerStatement 
        override this.call_base_type_func  (p1:string) (p2:string) (sFuncName:string) (bIsP1Option:bool) (bIsP2Option:bool) =
            equal_rust.call_base_type_func  p1 p2 sFuncName bIsP1Option bIsP2Option 
        override this.makeExpressionToStatement  (sIsValidExp:string) =
            equal_rust.makeExpressionToStatement  sIsValidExp 
        override this.callChildEqualFunc  (p1:string) (p2:string) (sChildEqualFuncName:string) =
            equal_rust.callChildEqualFunc  p1 p2 sChildEqualFuncName 
