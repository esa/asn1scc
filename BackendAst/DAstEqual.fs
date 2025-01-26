module DAstEqual
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open Asn1AcnAstUtilFunctions

open DAst
open DAstUtilFunctions
open Language


// TODO
// 1 In sequences with default components, default value must be taken into account when component not present.

let callBaseTypeFunc (lm:LanguageMacros)          = lm.equal.call_base_type_func
let makeExpressionToStatement (lm:LanguageMacros) = lm.equal.makeExpressionToStatement


let isEqualBodyPrimitive (lm:LanguageMacros) (v1:CallerScope) (v2:CallerScope) =
    Some (lm.equal.isEqual_Primitive (lm.lg.getValue v1.arg) (lm.lg.getValue v2.arg)  , [])


let isEqualBodyString (lm:LanguageMacros) (v1:CallerScope) (v2:CallerScope) =
    Some (lm.equal.isEqual_String (v1.arg.joined lm.lg) (v2.arg.joined lm.lg)  , [])

let isEqualBodyObjectIdentifier (lm:LanguageMacros) (v1:CallerScope) (v2:CallerScope) =
    Some (lm.equal.isObjectIdentifier_equal (lm.lg.getPointer v1.arg) (lm.lg.getPointer v2.arg), [])

let isEqualBodyTimeType (o:Asn1AcnAst.TimeType) (lm:LanguageMacros) (v1:CallerScope) (v2:CallerScope) =
    let namespacePrefix = lm.lg.rtlModuleName
    let getRtlTypeName  =
        match o.timeClass with
        |Asn1LocalTime                      _ -> lm.typeDef.Declare_Asn1LocalTimeNoRTL
        |Asn1UtcTime                        _ -> lm.typeDef.Declare_Asn1UtcTimeNoRTL
        |Asn1LocalTimeWithTimeZone          _ -> lm.typeDef.Declare_Asn1LocalTimeWithTimeZoneNoRTL
        |Asn1Date                             -> lm.typeDef.Declare_Asn1DateNoRTL
        |Asn1Date_LocalTime                 _ -> lm.typeDef.Declare_Asn1Date_LocalTimeNoRTL
        |Asn1Date_UtcTime                   _ -> lm.typeDef.Declare_Asn1Date_UtcTimeNoRTL
        |Asn1Date_LocalTimeWithTimeZone     _ -> lm.typeDef.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL
    let timeTypeName = getRtlTypeName ()
    Some (sprintf "%s%s_equal(%s, %s)" namespacePrefix timeTypeName (lm.lg.getPointer v1.arg) (lm.lg.getPointer v2.arg)  , [])


let isEqualBodyOctetString (lm:LanguageMacros) sMin sMax (v1:CallerScope) (v2:CallerScope) =
    let v1 = sprintf "%s%s" (v1.arg.joined lm.lg) (lm.lg.getAccess v1.arg)
    let v2 = sprintf "%s%s" (v2.arg.joined lm.lg) (lm.lg.getAccess v2.arg)
    Some (lm.equal.isEqual_OctetString v1 v2 (sMin = sMax) sMax, [])

let isEqualBodyBitString  (lm:LanguageMacros) sMin sMax (v1:CallerScope) (v2:CallerScope) =
    let v1 = sprintf "%s%s" (v1.arg.joined lm.lg) (lm.lg.getAccess v1.arg)
    let v2 = sprintf "%s%s" (v2.arg.joined lm.lg) (lm.lg.getAccess v2.arg)
    Some (lm.equal.isEqual_BitString v1 v2 (sMin = sMax) sMax, [])


let isEqualBodySequenceChild   (lm:LanguageMacros)  (o:Asn1AcnAst.Asn1Child) (newChild:Asn1Type) (v1:CallerScope) (v2:CallerScope)  =
    let c_name = lm.lg.getAsn1ChildBackendName0 o
    let callChildEqualFunc  = lm.equal.callChildEqualFunc
    let sInnerStatement =
        let chp1 = {v1 with arg = lm.lg.getSeqChild v1.arg c_name newChild.isIA5String o.Optionality.IsSome}
        let chp2 = {v2 with arg = lm.lg.getSeqChild v2.arg c_name newChild.isIA5String o.Optionality.IsSome}
        match newChild.equalFunction.isEqualFuncName with
        | None  ->
            match newChild.equalFunction.isEqualBody with
            | EqualBodyExpression func  ->
                match func chp1 chp2 with
                | Some (exp, lvars)  -> Some (sprintf "ret %s (%s);" lm.lg.AssignOperator exp, lvars)
                | None      -> None
            | EqualBodyStatementList  func   -> func chp1 chp2
        | Some  fncName ->
            Some ((callChildEqualFunc (lm.lg.getPointerUnchecked chp1.arg FullAccess) (lm.lg.getPointerUnchecked chp2.arg FullAccess) fncName), [])

    match sInnerStatement with
    | None  when  o.Optionality.IsSome  -> Some (lm.equal.isEqual_Sequence_child (v1.arg.joined lm.lg) (v2.arg.joined lm.lg) (lm.lg.getAccess v1.arg) o.Optionality.IsSome c_name None, [])
    | None                              -> None
    | Some (sInnerStatement, lvars)     -> Some (lm.equal.isEqual_Sequence_child (v1.arg.joined lm.lg) (v2.arg.joined lm.lg) (lm.lg.getAccess v1.arg) o.Optionality.IsSome c_name (Some sInnerStatement), lvars)


let isEqualBodyChoiceChild  (choiceTypeDefName:string)  (lm:LanguageMacros) (o:Asn1AcnAst.ChChildInfo) (newChild:Asn1Type) (v1:CallerScope) (v2:CallerScope)  =
    let p1,p2 =
        match ProgrammingLanguage.ActiveLanguages.Head with
        | ProgrammingLanguage.Scala ->
            ({v1 with arg = lm.lg.getChChild v1.arg (sprintf "%s_%s_tmp" (v1.arg.joined lm.lg) (lm.lg.getAsn1ChChildBackendName0 o)) newChild.isIA5String}),
            ({v2 with arg = lm.lg.getChChild v2.arg (sprintf "%s_%s_tmp" (v2.arg.joined lm.lg) (lm.lg.getAsn1ChChildBackendName0 o)) newChild.isIA5String})
        | _ ->
            ({v1 with arg = lm.lg.getChChild v1.arg (lm.lg.getAsn1ChChildBackendName0 o) newChild.isIA5String}),
            ({v2 with arg = lm.lg.getChChild v2.arg (lm.lg.getAsn1ChChildBackendName0 o) newChild.isIA5String})

    let sInnerStatement, lvars =
        match newChild.equalFunction.isEqualFuncName with
        | None  ->
            match newChild.equalFunction.isEqualBody with
            | EqualBodyExpression func  ->
                match func p1 p2 with
                | Some (exp, lvars)     -> sprintf "ret %s (%s);" lm.lg.AssignOperator exp, lvars
                | None                  -> sprintf "ret %s %s;" lm.lg.AssignOperator lm.lg.TrueLiteral, []
            | EqualBodyStatementList  func   ->
                match func p1 p2 with
                | Some a    -> a
                | None      -> sprintf "ret %s %s;" lm.lg.AssignOperator lm.lg.TrueLiteral, []
        | Some fncName  ->
            let exp = callBaseTypeFunc lm (lm.lg.getPointer p1.arg) (lm.lg.getPointer p2.arg) fncName p1.arg.isOptional p2.arg.isOptional
            makeExpressionToStatement lm exp, []

    lm.equal.isEqual_Choice_Child choiceTypeDefName o.presentWhenName sInnerStatement (p1.arg.joined lm.lg) (p2.arg.joined lm.lg), lvars





let getFuncName (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros)  (typeDefinition:TypeDefinitionOrReference) =
    //getFuncNameGeneric r "_Equal" tasInfo inhInfo typeKind typeDefinition
    getFuncNameGeneric  typeDefinition "_Equal"

let createEqualFunction_any (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) isEqualBody  =
    let equalTypeAssignment      = lm.equal.equalTypeAssignment
    let equalTypeAssignment_def  = lm.equal.equalTypeAssignment_def
    let alwaysTrue               = lm.lg.TrueLiteral
    let p1 = lm.lg.getParamTypeSuffix t "1" CommonTypes.Codec.Encode
    let p2 = lm.lg.getParamTypeSuffix t "2" CommonTypes.Codec.Encode
    let funcName            = getFuncName r lm  typeDefinition
    let varName1 = p1.arg.receiverId
    let varName2 = p2.arg.receiverId
    let sStar = lm.lg.getStar p1.arg

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
                let lvarsStr = lvars |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct
                let isEqualFunc = equalTypeAssignment varName1 varName2 sStar funcName (lm.lg.getLongTypedefName typeDefinition) content lvarsStr bExp bUnreferenced
                let isEqualFuncDef = equalTypeAssignment_def varName1 varName2 sStar funcName (lm.lg.getLongTypedefName typeDefinition)
                Some  isEqualFunc, Some isEqualFuncDef

    {
        EqualFunction.isEqualFuncName  = funcName
        isEqualBody                    = isEqualBody
        isEqualFunc                    = isEqualFunc
        isEqualFuncDef                 = isEqualFuncDef
    }




let castRPp (lm:LanguageMacros) codec realClass pp =
    match codec with
    | CommonTypes.Encode ->
        match realClass with
        | Asn1AcnAst.ASN1SCC_REAL   -> pp
        | Asn1AcnAst.ASN1SCC_FP32   -> (lm.lg.castExpression pp (lm.typeDef.Declare_Real()))
        | Asn1AcnAst.ASN1SCC_FP64   -> pp
    | CommonTypes.Decode -> pp


let createIntegerEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionOrReference) =
    let isEqualBody         = EqualBodyExpression (isEqualBodyPrimitive lm )
    createEqualFunction_any r lm   t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l)

let createRealEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionOrReference) =
    let isEqualBodyPrimitive (lm:LanguageMacros) (v1:CallerScope) (v2:CallerScope) =
        let castPp pp = castRPp lm Encode (o.getClass r.args) pp
        let pp1 = (lm.lg.getValue v1.arg)
        let pp2 = (lm.lg.getValue v2.arg)
        Some(lm.equal.isEqual_Real (castPp pp1) (castPp pp2), [])
    let isEqualBody         = EqualBodyExpression (isEqualBodyPrimitive lm)
    createEqualFunction_any r lm t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l)

let createBooleanEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionOrReference)  =
    let isEqualBody         = EqualBodyExpression (isEqualBodyPrimitive lm)
    createEqualFunction_any r lm t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l)

let createEnumeratedEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionOrReference)  =
    let isEqualBody         = EqualBodyExpression (isEqualBodyPrimitive lm)
    createEqualFunction_any r lm t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l)

let createObjectIdentifierEqualFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefinitionOrReference)  =
    let isEqualBody         = EqualBodyExpression (isEqualBodyObjectIdentifier lm)
    createEqualFunction_any r lm t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l)

let createTimeTypeEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefinitionOrReference)  =
    let isEqualBody         = EqualBodyExpression (isEqualBodyTimeType o lm)
    createEqualFunction_any r lm t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l)


let createStringEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionOrReference)  =
    let isEqualBody = EqualBodyExpression (isEqualBodyString lm)
    createEqualFunction_any r lm t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l)

let createNullTypeEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefinitionOrReference) =
    let isEqualBody = EqualBodyExpression (fun  v1 v2 -> None)
    createEqualFunction_any r lm t typeDefinition isEqualBody //(stgPrintEqualPrimitive l) (stgMacroPrimDefFunc l)

let createOctetStringEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionOrReference)  =
    let isEqualBody = isEqualBodyOctetString lm ( o.minSize.uper) ( o.maxSize.uper)
    createEqualFunction_any r lm t typeDefinition (EqualBodyExpression isEqualBody)
    //createOctetOrBitStringEqualFunction r l lm  t typeDefinition isEqualBody (stgMacroCompDefFunc l)

let createBitStringEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionOrReference)  =
    let isEqualBody = isEqualBodyBitString lm ( o.minSize.uper) ( o.maxSize.uper)
    createEqualFunction_any r lm t typeDefinition (EqualBodyExpression isEqualBody)
    //createOctetOrBitStringEqualFunction r l lm t typeDefinition isEqualBody (stgMacroCompDefFunc l)



let createSequenceOfEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionOrReference) (childType:Asn1Type) =
    let isEqualBodySequenceOf  (childType:Asn1Type) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  (lm:LanguageMacros) (v1:CallerScope) (v2:CallerScope)  =
        let getInnerStatement i =
            let childAccesPath (p:CallerScope) =  {p with arg = lm.lg.getArrayItem p.arg i childType.isIA5String} //v + childAccess + l.ArrName + (l.ArrayAccess i) //"[" + i + "]"
            match childType.equalFunction.isEqualFuncName with
            | None  ->
                match childType.equalFunction.isEqualBody with
                | EqualBodyExpression func  ->
                    match func (childAccesPath v1) (childAccesPath v2) with
                    | Some (exp, lvars)  -> Some (sprintf "ret %s (%s);" lm.lg.AssignOperator exp, lvars)       // ret = (boolExp);
                    | None      -> None
                | EqualBodyStatementList  func   -> func (childAccesPath v1) (childAccesPath v2)
            | Some fncName  ->
                let p1 = childAccesPath v1
                let p2 = childAccesPath v2
                let exp = callBaseTypeFunc lm (lm.lg.getPointer p1.arg) (lm.lg.getPointer p2.arg) fncName p1.arg.isOptional p2.arg.isOptional
                Some(makeExpressionToStatement lm exp, [])

        let i = sprintf "i%d" (v1.arg.SequenceOfLevel + 1)
        let lv = SequenceOfIndex (v1.arg.SequenceOfLevel + 1, None)
        match getInnerStatement i with
        | None when o.minSize.uper = o.maxSize.uper        -> None
        | None                                   ->
            Some (lm.equal.isEqual_SequenceOf_var_size (v1.arg.joined lm.lg) (v2.arg.joined lm.lg) (lm.lg.getAccess v1.arg) i None, [])
        | Some (innerStatement, lvars)           ->
            match (o.minSize.uper = o.maxSize.uper) with
            | true  -> Some (lm.equal.isEqual_SequenceOf_fix_size (v1.arg.joined lm.lg) (v2.arg.joined lm.lg) (lm.lg.getAccess v1.arg) i  ( o.minSize.uper) innerStatement, lv::lvars)
            | false -> Some (lm.equal.isEqual_SequenceOf_var_size (v1.arg.joined lm.lg) (v2.arg.joined lm.lg) (lm.lg.getAccess v1.arg) i  (Some innerStatement), lv::lvars)


    let isEqualBody         = isEqualBodySequenceOf childType t o lm
    createEqualFunction_any r lm t typeDefinition (EqualBodyStatementList isEqualBody)
    //createCompositeEqualFunction r l lm  t typeDefinition isEqualBody (stgMacroCompDefFunc l)

let createSequenceEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionOrReference) (children:SeqChildInfo list)  =
    let isEqualBodySequence  (lm:LanguageMacros) (children:SeqChildInfo list) (v1:CallerScope) (v2:CallerScope)  =
        let printChild (content:string, lvars:LocalVariable list) (sNestedContent:string option) =
            match sNestedContent with
            | None  -> content, lvars
            | Some c-> lm.equal.JoinItems content sNestedContent, lvars
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
    let isEqualBody         = isEqualBodySequence lm children
    createEqualFunction_any r lm t typeDefinition (EqualBodyStatementList isEqualBody)
    //createCompositeEqualFunction r l lm  t typeDefinition isEqualBody (stgMacroCompDefFunc l)

let createChoiceEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionOrReference) (children:ChChildInfo list)  =
    let isEqualBodyChoice  (typeDefinition:TypeDefinitionOrReference) (lm:LanguageMacros) (children:ChChildInfo list) (v1:CallerScope) (v2:CallerScope)  =
        let childrenConent,lvars =
            children |>
            List.map(fun c -> c.isEqualBodyStats v1 v2)  |>
            List.unzip
        let lvars = lvars |> List.collect id
        Some(lm.equal.isEqual_Choice (v1.arg.joined lm.lg) (v2.arg.joined lm.lg) (lm.lg.getAccess v1.arg) childrenConent, lvars)
    let isEqualBody         = isEqualBodyChoice typeDefinition lm children
    createEqualFunction_any r lm t typeDefinition (EqualBodyStatementList isEqualBody)
    //createCompositeEqualFunction r l lm  t typeDefinition isEqualBody (stgMacroCompDefFunc l)

let createReferenceTypeEqualFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType)  (defOrRef:TypeDefinitionOrReference) (baseType:Asn1Type) (us:State) =
    //let isEqualFuncName     = getEqualFuncName r l lm t.id
    let isEqualFuncName            = getFuncName r lm  defOrRef
    let typeDefinitionName = lm.lg.getLongTypedefName defOrRef


    let moduleName, typeDefinitionName0 =
        match defOrRef with
        | ReferenceToExistingDefinition refToExist   ->
            match refToExist.programUnit with
            | Some md -> md, refToExist.typedefName
            | None    -> ToC t.id.ModName, refToExist.typedefName
        | TypeDefinition                tdDef        ->
            match tdDef.baseType with
            | None ->  ToC t.id.ModName, tdDef.typedefName
            | Some refToExist ->
                match refToExist.programUnit with
                | Some md -> md, refToExist.typedefName
                | None    -> ToC  t.id.ModName, refToExist.typedefName


    let baseTypeDefName = typeDefinitionName0//ToC2(r.args.TypePrefix + o.tasName.Value)


    let baseEqName =
        match lm.lg.hasModules with
        | false     -> baseTypeDefName + "_Equal"
        | true   ->
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
        createEqualFunction_any r lm t defOrRef bs.isEqualBody, us
    | OctetString _
    | BitString  _
    | ObjectIdentifier _
    | TimeType _
    | SequenceOf _
    | Sequence _
    | Choice _ ->
        let isEqualBody (p1:CallerScope) (p2:CallerScope) =
            let exp = callBaseTypeFunc lm (lm.lg.getPointer p1.arg) (lm.lg.getPointer p2.arg) baseEqName p1.arg.isOptional p2.arg.isOptional
            Some(makeExpressionToStatement lm exp, [])

        let val1 = lm.lg.getParamTypeSuffix t "1" CommonTypes.Codec.Encode
        let val2 = lm.lg.getParamTypeSuffix t "2" CommonTypes.Codec.Encode

        let stgMacroDefFunc = lm.equal.PrintEqualDefinitionComposite
        let ns =
            match t.id.topLevelTas with
            | None -> 
                printfn "No type assignment info for %A" t.id
                us
            | Some tasInfo ->
                let caller = {Caller.typeId = tasInfo; funcType=EqualFunctionType}
                let callee = {Callee.typeId = {TypeAssignmentInfo.modName = o.modName.Value; tasName=o.tasName.Value} ; funcType=EqualFunctionType}
                addFunctionCallToState us caller callee

        let isEqualFunc, isEqualFuncDef =
            match isEqualFuncName with
            | None  -> None, None
            | Some funcName ->
                match isEqualBody val1 val2 with
                | Some (funcBody,lvars) ->
                    let lvars = lvars |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct
                    Some (lm.equal.PrintEqualComposite funcName typeDefinitionName funcBody lvars), Some (stgMacroDefFunc funcName typeDefinitionName)
                | None -> None, None
        {
            EqualFunction.isEqualFuncName  = isEqualFuncName
            isEqualBody                    = EqualBodyStatementList (isEqualBody )
            isEqualFunc                    = isEqualFunc
            isEqualFuncDef                 = isEqualFuncDef
        }, ns

