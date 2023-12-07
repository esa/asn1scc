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


let getFuncName (r:Asn1AcnAst.AstRoot) (typeDefinition:TypeDefinitionOrReference) =
    getFuncNameGeneric  typeDefinition "_Equal"

