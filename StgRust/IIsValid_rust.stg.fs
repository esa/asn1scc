module IIsValid_rust
open System
open System.Numerics
open CommonTypes

type IIsValid_rust() =
    inherit AbstractMacros.IIsValid()
        override this.rtlModuleName  () =
            isvalid_rust.rtlModuleName  () 
        override this.JoinItems  (sPart:string) (soNestedPart:string option) =
            isvalid_rust.JoinItems  sPart soNestedPart 
        override this.JoinTwoIfFirstOk  (sStr1:string) (sStr2:string) =
            isvalid_rust.JoinTwoIfFirstOk  sStr1 sStr2 
        override this.JoinItems2  (sPart:string) (sNestedPart:string) =
            isvalid_rust.JoinItems2  sPart sNestedPart 
        override this.always_true_statement  () =
            isvalid_rust.always_true_statement  () 
        override this.always_false_statement  (sErrorCodeName:string) =
            isvalid_rust.always_false_statement  sErrorCodeName 
        override this.makeExpressionToStatement0  (sIsValidExp:string) =
            isvalid_rust.makeExpressionToStatement0  sIsValidExp 
        override this.convertVCBExpressionToStatementAndUpdateErrCode  (sIsValidExp:string) (sErrCode:string) =
            isvalid_rust.convertVCBExpressionToStatementAndUpdateErrCode  sIsValidExp sErrCode 
        override this.convertVCBStatementToStatementAndUpdateErrCode  (sStatement:string) (sErrCode:string) =
            isvalid_rust.convertVCBStatementToStatementAndUpdateErrCode  sStatement sErrCode 
        override this.convertVCBTRUEToStatementAndUpdateErrCode  () =
            isvalid_rust.convertVCBTRUEToStatementAndUpdateErrCode  () 
        override this.convertVCBFalseToStatementAndUpdateErrCode  (sErrCode:string) =
            isvalid_rust.convertVCBFalseToStatementAndUpdateErrCode  sErrCode 
        override this.EmitTypeAssignment_composite_def_err_code  (sErrCode:string) (nErrValue:BigInteger) (arrsErrorCodeComments:seq<string>) =
            isvalid_rust.EmitTypeAssignment_composite_def_err_code  sErrCode nErrValue arrsErrorCodeComments 
        override this.EmitTypeAssignment_composite_def  (sVarName:string) (sStar:string) (sFuncName:string) (sTypeDefName:string) (arrsErrcodes:seq<string>) =
            isvalid_rust.EmitTypeAssignment_composite_def  sVarName sStar sFuncName sTypeDefName arrsErrcodes 
        override this.EmitTypeAssignment_composite  (sVarName:string) (sPtrPrefix:string) (sPtrSuffix:string) (sFuncName:string) (sTypeDefName:string) (sContent:string) (arrsAlphaFuncs:seq<string>) (arrsLocalVars:seq<string>) (bUnreferenced:bool) =
            isvalid_rust.EmitTypeAssignment_composite  sVarName sPtrPrefix sPtrSuffix sFuncName sTypeDefName sContent arrsAlphaFuncs arrsLocalVars bUnreferenced 
        override this.GetEnumIndexByName  (sEnumValuesArray:string) (sEnumValuesArrayCount:string) (sExp:string) (bBinarySearch:bool) =
            isvalid_rust.GetEnumIndexByName  sEnumValuesArray sEnumValuesArrayCount sExp bBinarySearch 
        override this.ExpEqual  (sExp1:string) (sExp2:string) =
            isvalid_rust.ExpEqual  sExp1 sExp2 
        override this.ExpStringEqual  (sExp1:string) (sExp2:string) =
            isvalid_rust.ExpStringEqual  sExp1 sExp2 
        override this.ExpGt  (sExp1:string) (sExp2:string) =
            isvalid_rust.ExpGt  sExp1 sExp2 
        override this.ExpGte  (sExp1:string) (sExp2:string) =
            isvalid_rust.ExpGte  sExp1 sExp2 
        override this.ExpLt  (sExp1:string) (sExp2:string) =
            isvalid_rust.ExpLt  sExp1 sExp2 
        override this.ExpLte  (sExp1:string) (sExp2:string) =
            isvalid_rust.ExpLte  sExp1 sExp2 
        override this.ExpOr  (sExp1:string) (sExp2:string) =
            isvalid_rust.ExpOr  sExp1 sExp2 
        override this.ExpAnd  (sExp1:string) (sExp2:string) =
            isvalid_rust.ExpAnd  sExp1 sExp2 
        override this.ExpAndMulti  (arrsExp:seq<string>) =
            isvalid_rust.ExpAndMulti  arrsExp 
        override this.ExpNot  (sExp:string) =
            isvalid_rust.ExpNot  sExp 
        override this.StrLen  (sExp:string) =
            isvalid_rust.StrLen  sExp 
        override this.ArrayLen  (sExp:string) (sAcc:string) =
            isvalid_rust.ArrayLen  sExp sAcc 
        override this.ExpressionToStatement  (sExp1:string) =
            isvalid_rust.ExpressionToStatement  sExp1 
        override this.StatementOrStatement  (sStat1:string) (sStat2:string) =
            isvalid_rust.StatementOrStatement  sStat1 sStat2 
        override this.ExpressionOrStatement  (sExp1:string) (sStat2:string) =
            isvalid_rust.ExpressionOrStatement  sExp1 sStat2 
        override this.StatementOrExpression  (sStat1:string) (sExp2:string) =
            isvalid_rust.StatementOrExpression  sStat1 sExp2 
        override this.StatementAndStatement  (sStat1:string) (sStat2:string) =
            isvalid_rust.StatementAndStatement  sStat1 sStat2 
        override this.ExpressionAndStatement  (sExp1:string) (sStat2:string) =
            isvalid_rust.ExpressionAndStatement  sExp1 sStat2 
        override this.StatementAndExpression  (sStat1:string) (sExp2:string) =
            isvalid_rust.StatementAndExpression  sStat1 sExp2 
        override this.StatementNot  (sStat:string) =
            isvalid_rust.StatementNot  sStat 
        override this.StatementExceptStatement  (sStat1:string) (sStat2:string) =
            isvalid_rust.StatementExceptStatement  sStat1 sStat2 
        override this.ExpressionExceptStatement  (sExp1:string) (sStat2:string) =
            isvalid_rust.ExpressionExceptStatement  sExp1 sStat2 
        override this.StatementExceptExpression  (sStat1:string) (sExp2:string) =
            isvalid_rust.StatementExceptExpression  sStat1 sExp2 
        override this.StatementForLoop  (p:string) (sAcc:string) (i:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) (sInnerStatement:string) =
            isvalid_rust.StatementForLoop  p sAcc i bIsFixedSize nFixedSize sInnerStatement 
        override this.Print_AlphabetCheckFunc  (sFuncName:string) (arrsAlphaConBody:seq<string>) =
            isvalid_rust.Print_AlphabetCheckFunc  sFuncName arrsAlphaConBody 
        override this.SingleValConstraint  (p:string) (v:string) =
            isvalid_rust.SingleValConstraint  p v 
        override this.stringContainsChar  (sStrVal:string) (p:string) =
            isvalid_rust.stringContainsChar  sStrVal p 
        override this.RangeConstraint  (p:string) (v1:string) (v2:string) (bMin:bool) (bMax:bool) =
            isvalid_rust.RangeConstraint  p v1 v2 bMin bMax 
        override this.RangeConstraint_val_MAX  (p:string) (v:string) (bMin:bool) =
            isvalid_rust.RangeConstraint_val_MAX  p v bMin 
        override this.RangeConstraint_MIN_val  (p:string) (v:string) (bMax:bool) =
            isvalid_rust.RangeConstraint_MIN_val  p v bMax 
        override this.AND_Constraint  (sCon1:string) (sCon2:string) =
            isvalid_rust.AND_Constraint  sCon1 sCon2 
        override this.OR_Constraint  (sCon1:string) (sCon2:string) =
            isvalid_rust.OR_Constraint  sCon1 sCon2 
        override this.AllExceptConstraint  (sCon:string) =
            isvalid_rust.AllExceptConstraint  sCon 
        override this.ExceptConstraint  (sCon1:string) (sCon2:string) =
            isvalid_rust.ExceptConstraint  sCon1 sCon2 
        override this.methodNameSuffix  () =
            isvalid_rust.methodNameSuffix  () 
        override this.callAlphaFunc  (sFuncName:string) (p:string) =
            isvalid_rust.callAlphaFunc  sFuncName p 
        override this.callAlphaFuncWithIndex  (sFuncName:string) (p:string) (nIdx:BigInteger) =
            isvalid_rust.callAlphaFuncWithIndex  sFuncName p nIdx 
        override this.alphaFuncBaseName  (p:string) =
            isvalid_rust.alphaFuncBaseName  p 
        override this.alphaFuncNameWithIndex  (p:string) (nIdx:BigInteger) =
            isvalid_rust.alphaFuncNameWithIndex  p nIdx 
        override this.PrintMultipleConstraints  (arrsConstraints:seq<string>) =
            isvalid_rust.PrintMultipleConstraints  arrsConstraints 
        override this.Emit_type  (arrsConstraints:seq<string>) (sErrCodeName:string) =
            isvalid_rust.Emit_type  arrsConstraints sErrCodeName 
        override this.call_base_type_func  (p:string) (sFuncName:string) (soTypeCasting:string option) =
            isvalid_rust.call_base_type_func  p sFuncName soTypeCasting 
        override this.call_superclass_func  (p:string) (sFuncName:string) (soTypeCasting:string option) =
            isvalid_rust.call_superclass_func  p sFuncName soTypeCasting 
        override this.call_base_type_func_exp  (p:string) (sFuncName:string) =
            isvalid_rust.call_base_type_func_exp  p sFuncName 
        override this.Sequence_OptionalChild  (p:string) (sAcc:string) (sChName:string) (sInnerStatement:string) =
            isvalid_rust.Sequence_OptionalChild  p sAcc sChName sInnerStatement 
        override this.Sequence_optional_child_always_present_or_absent  (p:string) (sAcc:string) (sChName:string) (sErrCode:string) (sPresOrAbs:string) =
            isvalid_rust.Sequence_optional_child_always_present_or_absent  p sAcc sChName sErrCode sPresOrAbs 
        override this.Sequence_optional_child_always_present_or_absent_expr  (p:string) (sAcc:string) (sChName:string) (sPresOrAbs:string) =
            isvalid_rust.Sequence_optional_child_always_present_or_absent_expr  p sAcc sChName sPresOrAbs 
        override this.Choice_OptionalChild  (p:string) (sPLocal:string) (sAcc:string) (sChPresent:string) (sInnerStatement:string) (sChoiceTypeName:string) =
            isvalid_rust.Choice_OptionalChild  p sPLocal sAcc sChPresent sInnerStatement sChoiceTypeName 
        override this.Choice_child_always_present_Exp  (p:string) (sAcc:string) (sChPresent:string) (sChoiceTypeName:string) =
            isvalid_rust.Choice_child_always_present_Exp  p sAcc sChPresent sChoiceTypeName 
        override this.Choice_child_always_absent_Exp  (p:string) (sAcc:string) (sChPresent:string) (sChoiceTypeName:string) =
            isvalid_rust.Choice_child_always_absent_Exp  p sAcc sChPresent sChoiceTypeName 
        override this.choice_child  (sChPresent:string) (sChildBody:string) (bAlwaysAbsent:bool) (sChildName:string) (sChoiceTypeName:string) (sChildTypeName:string) =
            isvalid_rust.choice_child  sChPresent sChildBody bAlwaysAbsent sChildName sChoiceTypeName sChildTypeName 
        override this.choice  (p:string) (sAccess:string) (arrsChildren:seq<string>) (sErrCodeForInvalidCase:string) =
            isvalid_rust.choice  p sAccess arrsChildren sErrCodeForInvalidCase 
        override this.sequenceOf  (p:string) (sAcc:string) (i:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) (soIsValidSizeExp:string option) (soErrCode:string option) (soInnerStatement:string option) =
            isvalid_rust.sequenceOf  p sAcc i bIsFixedSize nFixedSize soIsValidSizeExp soErrCode soInnerStatement 
        override this.sequenceOf2  (p:string) (sAcc:string) (i:string) (bIsFixedSize:bool) (nFixedSize:BigInteger) (sInnerStatement:string) =
            isvalid_rust.sequenceOf2  p sAcc i bIsFixedSize nFixedSize sInnerStatement 
        override this.octet_var_string_equal  (p:string) (sAccess:string) (nVarLength:BigInteger) (sOctArrayLiteral:string) =
            isvalid_rust.octet_var_string_equal  p sAccess nVarLength sOctArrayLiteral 
        override this.octet_fix_string_equal  (p:string) (sAccess:string) (nFixedSize:BigInteger) (nVarLength:BigInteger) (sOctArrayLiteral:string) =
            isvalid_rust.octet_fix_string_equal  p sAccess nFixedSize nVarLength sOctArrayLiteral 
        override this.bit_var_string_equal  (p:string) (sAccess:string) (nVarLength:BigInteger) (sOctArrayLiteral:string) (sBitArrayLiteral:string) =
            isvalid_rust.bit_var_string_equal  p sAccess nVarLength sOctArrayLiteral sBitArrayLiteral 
        override this.bit_fix_string_equal  (p:string) (sAccess:string) (nFixedSize:BigInteger) (nVarLength:BigInteger) (sOctArrayLiteral:string) (sBitArrayLiteral:string) =
            isvalid_rust.bit_fix_string_equal  p sAccess nFixedSize nVarLength sOctArrayLiteral sBitArrayLiteral 
        override this.objId_equal  (p:string) (sObjIdLiteral:string) =
            isvalid_rust.objId_equal  p sObjIdLiteral 
