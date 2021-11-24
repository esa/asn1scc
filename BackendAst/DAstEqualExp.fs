module DAstEqualExp
open FsUtils
open CommonTypes
open Asn1AcnAstUtilFunctions

open DAst
open DAstUtilFunctions

let isEqualBodyPrimitive (e1:TC_Expression) (e2:TC_Expression) =
    (TC_EqExpression (e1,e2)), []


type TC_FunctionBody2 =
    | TC_EqualBodyExpression       of (TC_Expression -> TC_Expression -> (TC_Expression*(LocalVariable list)) option)
    | TC_EqualBodyStatementList    of (TC_Expression -> TC_Expression -> (TC_Expression*(LocalVariable list)) option)



type TC_EqualFunction = {
    funcBody         : TC_FunctionBody2           // a function that  returns an expression or a statement list
}

type TC_Function = {
    name : string
}


let getFuncName (r:Asn1AcnAst.AstRoot) (typeDefinition:TypeDefintionOrReference) =
    getFuncNameGeneric  typeDefinition "_Equal"

(*
let createEqualFunction_any (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) isEqualBody  =
    //let equalTypeAssignment      = match l with C -> equal_c.equalTypeAssignment     | Ada -> equal_a.equalTypeAssignment
    //let equalTypeAssignment_def  = match l with C -> equal_c.equalTypeAssignment_def | Ada -> equal_a.equalTypeAssignment_def
    let alwaysTrue               = TC_Literal(TC_BOOL, "true")
    let p1 = t.getParameterExpr "1" CommonTypes.Codec.Encode
    let p2 = t.getParameterExpr "2" CommonTypes.Codec.Encode

    let funcName            = getFuncName r  typeDefinition
    //let varName1 = p1.arg.p
    //let varName2 = p2.arg.p
    //let sStar = p1.arg.getStar l

    let  isEqualFunc, isEqualFuncDef  = 
            match funcName  with
            | None              -> None, None
            | Some funcName     -> 
                let content, lvars, bExp, bUnreferenced =
                    match isEqualBody with
                    | TC_EqualBodyExpression       expFunc ->
                        match expFunc p1 p2 with
                        | Some (content, lvars) -> content, lvars, true, false
                        | None                  -> alwaysTrue, [], true, true
                    | TC_EqualBodyStatementList    stmFunc ->
                        match stmFunc p1 p2 with
                        | Some (content, lvars) -> content, lvars, false, false
                        | None                  -> alwaysTrue, [], true, true
                let lvarsStr = lvars 
                let isEqualFunc = equalTypeAssignment varName1 varName2 sStar funcName (typeDefinition.longTypedefName l) content lvarsStr bExp bUnreferenced
                let isEqualFuncDef = equalTypeAssignment_def varName1 varName2 sStar funcName (typeDefinition.longTypedefName l) 
                Some  isEqualFunc, Some isEqualFuncDef

    {
        TC_Function.isEqualFuncName  = funcName
        isEqualBody                    = isEqualBody
    }    


let createIntegerEqualFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) =
    let isEqualBody         = TC_EqualBodyExpression isEqualBodyPrimitive
    createEqualFunction_any r t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 


*)