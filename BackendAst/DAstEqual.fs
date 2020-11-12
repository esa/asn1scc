module DAstEqual
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open Asn1AcnAstUtilFunctions

open DAst
open DAstUtilFunctions


// TODO
// 1 In sequences with default components, default value must be taken into account when component not present.

let callBaseTypeFunc l          = match l with C -> equal_c.call_base_type_func         | Ada -> equal_c.call_base_type_func
let makeExpressionToStatement l = match l with C -> equal_c.makeExpressionToStatement   | Ada -> equal_a.makeExpressionToStatement


let isEqualBodyPrimitive (l:ProgrammingLanguage) (v1:CallerScope) (v2:CallerScope) =
    match l with
    | C         -> Some (sprintf "%s == %s" (v1.arg.getValue l) (v2.arg.getValue l)  , [])
    | Ada       -> Some (sprintf "%s = %s"  (v1.arg.getValue l) (v2.arg.getValue l)  , [])

let isEqualBodyString (l:ProgrammingLanguage) (v1:CallerScope) (v2:CallerScope) =
    match l with
    | C         -> Some (sprintf "strcmp(%s, %s) == 0" v1.arg.p v2.arg.p  , [])
    | Ada       -> Some (sprintf "%s = %s" v1.arg.p v2.arg.p   , [])

let isEqualBodyObjectIdentifier (l:ProgrammingLanguage) (v1:CallerScope) (v2:CallerScope) =
    let namespacePrefix = match l with C -> "" | Ada -> "adaasn1rtl."
    Some (sprintf "%sObjectIdentifier_equal(%s, %s)" namespacePrefix (v1.arg.getPointer l) (v2.arg.getPointer l)  , [])

let isEqualBodyTimeType (o:Asn1AcnAst.TimeType) (l:ProgrammingLanguage) (v1:CallerScope) (v2:CallerScope) =
    let namespacePrefix = match l with C -> "" | Ada -> "adaasn1rtl."
    let getRtlTypeName  = 
        match o.timeClass with
        |Asn1LocalTime                      _ -> match l with C -> header_c.Declare_Asn1LocalTime                   | Ada -> header_a.Declare_Asn1LocalTimeNoRTL                  
        |Asn1UtcTime                        _ -> match l with C -> header_c.Declare_Asn1UtcTime                     | Ada -> header_a.Declare_Asn1UtcTimeNoRTL                    
        |Asn1LocalTimeWithTimeZone          _ -> match l with C -> header_c.Declare_Asn1LocalTimeWithTimeZone       | Ada -> header_a.Declare_Asn1LocalTimeWithTimeZoneNoRTL      
        |Asn1Date                             -> match l with C -> header_c.Declare_Asn1Date                        | Ada -> header_a.Declare_Asn1DateNoRTL                     
        |Asn1Date_LocalTime                 _ -> match l with C -> header_c.Declare_Asn1Date_LocalTime              | Ada -> header_a.Declare_Asn1Date_LocalTimeNoRTL             
        |Asn1Date_UtcTime                   _ -> match l with C -> header_c.Declare_Asn1Date_UtcTime                | Ada -> header_a.Declare_Asn1Date_UtcTimeNoRTL               
        |Asn1Date_LocalTimeWithTimeZone     _ -> match l with C -> header_c.Declare_Asn1Date_LocalTimeWithTimeZone  | Ada -> header_a.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL 
    let timeTypeName = getRtlTypeName ()
    Some (sprintf "%s%s_equal(%s, %s)" namespacePrefix timeTypeName (v1.arg.getPointer l) (v2.arg.getPointer l)  , [])


let isEqualBodyOctetString (l:ProgrammingLanguage) sMin sMax (v1:CallerScope) (v2:CallerScope) =
    let v1 = sprintf "%s%s" v1.arg.p (v1.arg.getAcces l)
    let v2 = sprintf "%s%s" v2.arg.p (v2.arg.getAcces l)
    match l with
    | C         -> Some (equal_c.isEqual_OctetString v1 v2 (sMin = sMax) sMax, [])
    | Ada       -> Some (equal_a.isEqual_OctetString v1 v2 (sMin = sMax) sMax, [])

let isEqualBodyBitString (l:ProgrammingLanguage) sMin sMax (v1:CallerScope) (v2:CallerScope) =
    let v1 = sprintf "%s%s" v1.arg.p (v1.arg.getAcces l)
    let v2 = sprintf "%s%s" v2.arg.p (v2.arg.getAcces l)
    match l with
    | C         -> Some (equal_c.isEqual_BitString v1 v2 (sMin = sMax) sMax, [])
    | Ada       -> Some (equal_a.isEqual_BitString v1 v2 (sMin = sMax) sMax, [])

(*
                match c.Type.equalFunction.isEqualFuncName with
                | None  -> c.isEqualBodyStats  v1 v2 
                | Some fncName ->
                    let chp1 = {v1 with arg = v1.arg.getSeqChild l c.c_name c.Type.isIA5String}
                    let chp2 = {v2 with arg = v2.arg.getSeqChild l c.c_name c.Type.isIA5String}

*)

let isEqualBodySequenceChild   (l:ProgrammingLanguage)  (o:Asn1AcnAst.Asn1Child) (newChild:Asn1Type) (v1:CallerScope) (v2:CallerScope)  = 
    let c_name = o.getBackendName l
    let callChildEqualFunc  = match l with C -> equal_c.callChildEqualFunc | Ada -> equal_a.callChildEqualFunc
    let sInnerStatement = 
        let chp1 = {v1 with arg = v1.arg.getSeqChild l c_name newChild.isIA5String}
        let chp2 = {v2 with arg = v2.arg.getSeqChild l c_name newChild.isIA5String}
        match newChild.equalFunction.isEqualFuncName with
        | None  ->
            match newChild.equalFunction.isEqualBody with
            | EqualBodyExpression func  ->  
                match func chp1 chp2 with
                | Some (exp, lvars)  -> Some (sprintf "ret %s (%s);" l.AssignOperator exp, lvars)
                | None      -> None
            | EqualBodyStatementList  func   -> func ({v1 with arg = v1.arg.getSeqChild l c_name newChild.isIA5String}) ({v2 with arg = v2.arg.getSeqChild l c_name newChild.isIA5String})
        | Some  fncName ->
            Some ((callChildEqualFunc (chp1.arg.getPointer l) (chp2.arg.getPointer l) fncName), [])              

    match l with
    | C         -> 
        match sInnerStatement with
        | None  when  o.Optionality.IsSome  -> Some (equal_c.isEqual_Sequence_child v1.arg.p  v2.arg.p  (v1.arg.getAcces l) o.Optionality.IsSome c_name None, [])
        | None                              -> None
        | Some (sInnerStatement, lvars)     -> Some (equal_c.isEqual_Sequence_child v1.arg.p  v2.arg.p  (v1.arg.getAcces l) o.Optionality.IsSome c_name (Some sInnerStatement), lvars)
    | Ada       ->
        match sInnerStatement with
        | None  when  o.Optionality.IsSome  -> Some (equal_a.isEqual_Sequence_Child v1.arg.p  v2.arg.p  o.Optionality.IsSome c_name None, [])
        | None                              -> None
        | Some (sInnerStatement, lvars)     -> Some (equal_a.isEqual_Sequence_Child v1.arg.p  v2.arg.p  o.Optionality.IsSome c_name (Some sInnerStatement), lvars)



let isEqualBodyChoiceChild  (choiceTypeDefName:string) (l:ProgrammingLanguage) (o:Asn1AcnAst.ChChildInfo) (newChild:Asn1Type) (v1:CallerScope) (v2:CallerScope)  = 
    let sInnerStatement, lvars = 
        let p1,p2 =
            match l with
            | C    ->
                ({v1 with arg = v1.arg.getChChild l (o.getBackendName l) newChild.isIA5String}), ({v2 with arg = v2.arg.getChChild l (o.getBackendName l) newChild.isIA5String})
            | Ada  ->
                ({v1 with arg = v1.arg.getChChild l (o.getBackendName l) newChild.isIA5String}), ({v2 with arg = v2.arg.getChChild l (o.getBackendName l) newChild.isIA5String})
        match newChild.equalFunction.isEqualFuncName with
        | None  ->
            match newChild.equalFunction.isEqualBody with
            | EqualBodyExpression func  ->  
                match func p1 p2 with
                | Some (exp, lvars)     -> sprintf "ret %s (%s);" l.AssignOperator exp, lvars
                | None                  -> sprintf "ret %s TRUE;" l.AssignOperator, []
            | EqualBodyStatementList  func   -> 
                match func p1 p2 with
                | Some a    -> a
                | None      -> sprintf "ret %s TRUE;" l.AssignOperator, []
        | Some fncName  ->
            let exp = callBaseTypeFunc l (p1.arg.getPointer l) (p2.arg.getPointer l) fncName
            makeExpressionToStatement l exp, []

    match l with
    | C         -> 
        equal_c.isEqual_Choice_Child o.presentWhenName sInnerStatement, lvars
    | Ada       ->
        equal_a.isEqual_Choice_Child o.presentWhenName sInnerStatement, lvars





let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (typeDefinition:TypeDefintionOrReference) =
    //getFuncNameGeneric r "_Equal" tasInfo inhInfo typeKind typeDefinition
    getFuncNameGeneric  typeDefinition "_Equal"

let createEqualFunction_any (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) isEqualBody  =
    let equalTypeAssignment      = match l with C -> equal_c.equalTypeAssignment     | Ada -> equal_a.equalTypeAssignment
    let equalTypeAssignment_def  = match l with C -> equal_c.equalTypeAssignment_def | Ada -> equal_a.equalTypeAssignment_def
    let alwaysTrue               = match l with C -> "TRUE" | Ada -> "True"
    let p1 = t.getParamTypeSuffix l "1" CommonTypes.Codec.Encode
    let p2 = t.getParamTypeSuffix l "2" CommonTypes.Codec.Encode
    let funcName            = getFuncName r l  typeDefinition
    let varName1 = p1.arg.p
    let varName2 = p2.arg.p
    let sStar = p1.arg.getStar l

    let  isEqualFunc, isEqualFuncDef  = 
            match funcName  with
            | None              -> None, None
            | Some funcName     -> 
                let content, lvars, bExp, bUnreferenced =
                    match isEqualBody with
                    | EqualBodyExpression       expFunc ->
                        match expFunc p1 p2 with
                        | Some (content, lvars) -> content, lvars, true, false
                        | None                  -> alwaysTrue, [], true, true
                    | EqualBodyStatementList    stmFunc ->
                        match stmFunc p1 p2 with
                        | Some (content, lvars) -> content, lvars, false, false
                        | None                  -> alwaysTrue, [], true, true
                let lvarsStr = lvars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                let isEqualFunc = equalTypeAssignment varName1 varName2 sStar funcName (typeDefinition.longTypedefName l) content lvarsStr bExp bUnreferenced
                let isEqualFuncDef = equalTypeAssignment_def varName1 varName2 sStar funcName (typeDefinition.longTypedefName l) 
                Some  isEqualFunc, Some isEqualFuncDef

    {
        EqualFunction.isEqualFuncName  = funcName
        isEqualBody                    = isEqualBody
        isEqualFunc                    = isEqualFunc
        isEqualFuncDef                 = isEqualFuncDef
    }    



let stgPrintEqualPrimitive = function
    | C    -> equal_c.PrintEqualPrimitive
    | Ada  -> equal_a.PrintEqualPrimitive

let stgMacroPrimDefFunc = function
    | C    -> equal_c.PrintEqualDefintionPrimitive
    | Ada  -> equal_a.PrintEqualDefintion

let stgMacroCompDefFunc = function
    | C    -> equal_c.PrintEqualDefintionComposite
    | Ada  -> equal_a.PrintEqualDefintion

let createIntegerEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) =
    let isEqualBody         = EqualBodyExpression (isEqualBodyPrimitive l)
    createEqualFunction_any r l t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 

let createRealEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference) =
    let isEqualBodyPrimitive (l:ProgrammingLanguage) (v1:CallerScope) (v2:CallerScope) =
        match l with
        | C         -> Some (sprintf "%s == %s" (v1.arg.getValue l) (v2.arg.getValue l)  , [])
        | Ada       -> Some (sprintf "adaasn1rtl.Asn1Real_Equal(%s, %s)" v1.arg.p v2.arg.p   , [])
    let isEqualBody         = EqualBodyExpression (isEqualBodyPrimitive l)
    createEqualFunction_any r l t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 

let createBooleanEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference)  =
    let isEqualBody         = EqualBodyExpression (isEqualBodyPrimitive l)
    createEqualFunction_any r l t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 

let createEnumeratedEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference)  =
    let isEqualBody         = EqualBodyExpression (isEqualBodyPrimitive l)
    createEqualFunction_any r l t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 

let createObjectIdentifierEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference)  =
    let isEqualBody         = EqualBodyExpression (isEqualBodyObjectIdentifier l)
    createEqualFunction_any r l t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 

let createTimeTypeEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefintionOrReference)  =
    let isEqualBody         = EqualBodyExpression (isEqualBodyTimeType o l)
    createEqualFunction_any r l t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 


let createStringEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference)  =
    let isEqualBody = EqualBodyExpression (isEqualBodyString l)
    createEqualFunction_any r l t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 

let createNullTypeEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference) =
    let isEqualBody = EqualBodyExpression (fun  v1 v2 -> None)
    createEqualFunction_any r l t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l) 

let createOctetStringEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference)  =
    let isEqualBody = isEqualBodyOctetString l ( o.minSize.uper) ( o.maxSize.uper)
    createEqualFunction_any r l t typeDefinition (EqualBodyExpression isEqualBody)
    //createOctetOrBitStringEqualFunction r l  t typeDefinition isEqualBody (stgMacroCompDefFunc l) 

let createBitStringEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference)  =
    let isEqualBody = isEqualBodyBitString l ( o.minSize.uper) ( o.maxSize.uper)
    createEqualFunction_any r l t typeDefinition (EqualBodyExpression isEqualBody)
    //createOctetOrBitStringEqualFunction r l t typeDefinition isEqualBody (stgMacroCompDefFunc l) 



let createSequenceOfEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference) (childType:Asn1Type) =
    let isEqualBodySequenceOf  (childType:Asn1Type) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  (l:ProgrammingLanguage) (v1:CallerScope) (v2:CallerScope)  =
        let getInnerStatement i = 
            let childAccesPath (p:CallerScope) =  {p with arg = p.arg.getArrayItem l i childType.isIA5String} //v + childAccess + l.ArrName + (l.ArrayAccess i) //"[" + i + "]"
            match childType.equalFunction.isEqualFuncName with
            | None  ->
                match childType.equalFunction.isEqualBody with
                | EqualBodyExpression func  ->  
                    match func (childAccesPath v1) (childAccesPath v2) with
                    | Some (exp, lvars)  -> Some (sprintf "ret %s (%s);" l.AssignOperator exp, lvars)       // ret = (boolExp);
                    | None      -> None
                | EqualBodyStatementList  func   -> func (childAccesPath v1) (childAccesPath v2)
            | Some fncName  ->
                let p1 = childAccesPath v1
                let p2 = childAccesPath v2
                let exp = callBaseTypeFunc l (p1.arg.getPointer l) (p2.arg.getPointer l) fncName
                Some(makeExpressionToStatement l exp, [])

        let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
        let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
        match getInnerStatement i with
        | None when o.minSize.uper = o.maxSize.uper        -> None
        | None                                   ->
            match l with
            | C    -> Some (equal_c.isEqual_SequenceOf v1.arg.p v2.arg.p (v1.arg.getAcces l) i (o.minSize.uper = o.maxSize.uper) ( o.minSize.uper) None, [])
            | Ada  -> Some (equal_a.isEqual_SequenceOf_var_size v1.arg.p v2.arg.p i None, [])
        | Some (innerStatement, lvars)           ->
            match l with
            | C    -> Some (equal_c.isEqual_SequenceOf v1.arg.p v2.arg.p (v1.arg.getAcces l) i (o.minSize.uper = o.maxSize.uper) ( o.minSize.uper) (Some innerStatement), lv::lvars)
            | Ada  -> 
                match (o.minSize.uper = o.maxSize.uper) with
                | true  -> Some (equal_a.isEqual_SequenceOf_fix_size v1.arg.p v2.arg.p i  ( o.minSize.uper) innerStatement, lv::lvars)
                | false -> Some (equal_a.isEqual_SequenceOf_var_size v1.arg.p v2.arg.p i  (Some innerStatement), lv::lvars)


    let isEqualBody         = isEqualBodySequenceOf childType t o l
    createEqualFunction_any r l t typeDefinition (EqualBodyStatementList isEqualBody)
    //createCompositeEqualFunction r l  t typeDefinition isEqualBody (stgMacroCompDefFunc l) 

let createSequenceEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (children:SeqChildInfo list)  =
    let isEqualBodySequence  (l:ProgrammingLanguage) (children:SeqChildInfo list) (v1:CallerScope) (v2:CallerScope)  = 
        let printChild (content:string, lvars:LocalVariable list) (sNestedContent:string option) = 
            match sNestedContent with
            | None  -> content, lvars
            | Some c-> 
                match l with
                | C        -> equal_c.JoinItems content sNestedContent, lvars
                | Ada      -> equal_a.JoinItems content sNestedContent, lvars
        let rec printChildren children : Option<string*list<LocalVariable>> = 
            match children with
            |[]     -> None
            |x::xs  -> 
                match printChildren xs with
                | None                          -> Some (printChild x  None)
                | Some (childrenCont, lvars)    -> 
                    let content, lv = printChild x  (Some childrenCont)
                    Some (content, lv@lvars)
        
        let childrenConent =   
            children |> 
            List.choose(fun c -> match c with Asn1Child x -> Some x | AcnChild _ -> None) |> 
            List.choose(fun c -> c.isEqualBodyStats  v1 v2 )
        printChildren childrenConent
    let isEqualBody         = isEqualBodySequence l children
    createEqualFunction_any r l t typeDefinition (EqualBodyStatementList isEqualBody)
    //createCompositeEqualFunction r l  t typeDefinition isEqualBody (stgMacroCompDefFunc l) 

let createChoiceEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (children:ChChildInfo list)  =
    let isEqualBodyChoice  (typeDefinition:TypeDefintionOrReference) (l:ProgrammingLanguage) (children:ChChildInfo list) (v1:CallerScope) (v2:CallerScope)  = 
        let childrenConent,lvars =   
            children |> 
            List.map(fun c -> 
                match l with
                | C    ->
                    c.isEqualBodyStats v1 v2//(v1+childAccess+"u") (v2+childAccess+"u") 
                |Ada   ->
                    c.isEqualBodyStats v1 v2 
            )  |>
            List.unzip
        let lvars = lvars |> List.collect id
        match l with
        |C   -> Some(equal_c.isEqual_Choice v1.arg.p v2.arg.p (v1.arg.getAcces l) childrenConent, lvars)
        |Ada -> Some(equal_a.isEqual_Choice v1.arg.p v2.arg.p (typeDefinition.longTypedefName l) childrenConent, lvars)
    let isEqualBody         = isEqualBodyChoice typeDefinition l children
    createEqualFunction_any r l t typeDefinition (EqualBodyStatementList isEqualBody)
    //createCompositeEqualFunction r l  t typeDefinition isEqualBody (stgMacroCompDefFunc l) 

let createReferenceTypeEqualFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType)  (defOrRef:TypeDefintionOrReference) (baseType:Asn1Type) =
    //let isEqualFuncName     = getEqualFuncName r l t.id
    let isEqualFuncName            = getFuncName r l  defOrRef
    let typeDefinitionName = defOrRef.longTypedefName l


    let moduleName, typeDefinitionName0 = 
        let t1 = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        t1.FT_TypeDefintion.[l].programUnit, t1.FT_TypeDefintion.[l].typeName

    let baseTypeDefName = typeDefinitionName0//ToC2(r.args.TypePrefix + o.tasName.Value)


    let baseEqName = 
        match l with
        | C     -> baseTypeDefName + "_Equal"
        | Ada   -> 
            match t.id.ModName = o.modName.Value with
            | true  -> baseTypeDefName + "_Equal"
            | false -> moduleName + "." + baseTypeDefName + "_Equal"

    
    match baseType.Kind with
    | Integer _
    | Real _
    | Boolean _
    | Enumerated _
    | NullType _
    | IA5String _       
    | ReferenceType _ ->
        let bs = baseType.equalFunction
        createEqualFunction_any r l t defOrRef bs.isEqualBody
    | OctetString _
    | BitString  _      
    | ObjectIdentifier _
    | TimeType _
    | SequenceOf _
    | Sequence _
    | Choice   _      ->

        let    isEqualBody                   = 
                    let eqBody (p1:CallerScope) (p2:CallerScope) = 
                        let exp = callBaseTypeFunc l (p1.arg.getPointer l) (p2.arg.getPointer l) baseEqName
                        Some(makeExpressionToStatement l exp, [])
                    eqBody
        let val1, val2 =  
            match l with 
            | C     -> {CallerScope.modName = t.id.ModName; arg = POINTER "pVal1"}, {CallerScope.modName = t.id.ModName; arg = POINTER "pVal2"}
            | Ada   -> {CallerScope.modName = t.id.ModName; arg = VALUE "val1"}, {CallerScope.modName = t.id.ModName; arg = VALUE "val2"}
        let stgMacroDefFunc = (stgMacroCompDefFunc l)

        let    isEqualFunc, isEqualFuncDef                   = 
                match isEqualFuncName with
                | None  -> None, None
                | Some funcName ->
                    match isEqualBody val1 val2 with
                    | Some (funcBody,lvars) -> 
                        let lvars = lvars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                        match l with
                        | C    -> Some (equal_c.PrintEqualComposite funcName typeDefinitionName funcBody lvars), Some (stgMacroDefFunc funcName typeDefinitionName)
                        | Ada  -> Some (equal_a.PrintEqualComposite funcName typeDefinitionName funcBody lvars), Some (stgMacroDefFunc funcName typeDefinitionName)
                    | None     -> None, None
        {
            EqualFunction.isEqualFuncName  = isEqualFuncName
            isEqualBody                    = EqualBodyStatementList (isEqualBody )
            isEqualFunc                    = isEqualFunc
            isEqualFuncDef                 = isEqualFuncDef
        }    
