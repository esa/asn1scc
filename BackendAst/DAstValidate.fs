﻿module DAstValidate

open System
open System.Numerics
open System.Globalization
open System.IO

open FsUtils
open CommonTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open Asn1Fold
open DAst
open DAstUtilFunctions

// TODO
// 1 single value constraints for composite types (SEQUENCE, SEQUENCE OF, CHOICE) by using the generated value and _equal function (like bit and octet strings)
// 2 simpify constraints. For example the constrains of the following type
// INT20 ::= INTEGER(-11..10 | 23 | 24)(1..20 EXCEPT 100)
// should be recalcualted as 
//   uPerRange is 1..10
// so the following simplifications must be performed
//    INT20 ::= INTEGER(1..10)(1..10)
// othwerwise the generated will have warnings



let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) =
    match l with
    | C
    | Ada       -> typeId.tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + "_IsConstraintValid"))
    | Python    -> 
        match typeId.tasInfo with
        | None      -> Some "check_constraints"
        | _         -> typeId.tasInfo |> Option.map (fun x -> ToCPy(r.args.TypePrefix + x.tasName + ".check_constraints"))

let Lte (l:ProgrammingLanguage) eqIsInc  e1 e2 =
    match eqIsInc with
    | true   -> l.ExpLte e1 e2        
    | false  -> l.ExpLt  e1 e2

let foldGenericCon (l:ProgrammingLanguage) valToStrFunc  (p:String)  (c:GenericConstraint<'v>)  =
    foldGenericConstraint
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s         -> l.ExpEqual p (valToStrFunc v) ,s)
        c
        0 |> fst

let foldRangeCon (l:ProgrammingLanguage) valToStrFunc1 valToStrFunc2 (p:String)  (c:RangeTypeConstraint<'v1,'v2>)  =
    foldRangeTypeConstraint        
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s         -> l.ExpEqual p (valToStrFunc2 v) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            l.ExpAnd (Lte l minIsIn (valToStrFunc1 v1) p) (Lte l maxIsIn p (valToStrFunc1 v2)), s)
        (fun v1 minIsIn s   -> Lte l minIsIn (valToStrFunc1 v1) p, s)
        (fun v2 maxIsIn s   -> Lte l maxIsIn p (valToStrFunc1 v2), s)
        c
        0 |> fst

// constraint simplification started here
type SimplifiedIntegerConstraint<'a> =
    | SicAlwaysTrue
    | SciConstraint of RangeTypeConstraint<'a, 'a>


let UintHandleEqual (r:Asn1AcnAst.AstRoot) zero v1 = 
    match v1 < zero with
    | true  -> SicAlwaysTrue
    | false -> SciConstraint (RangeSingleValueConstraint v1)

    
let SIntHandleEqual (r:Asn1AcnAst.AstRoot) v1 = 
    SciConstraint (RangeSingleValueConstraint v1)
    

(*  e.g. INTEGER (5..MAX)  ==> intVal >= 5 *)
let UintHandleRangeContraint_val_MAX (r:Asn1AcnAst.AstRoot) zero eqIsInc  v1 =
    match v1 < zero with
    | true  -> SicAlwaysTrue
    | false ->
        match eqIsInc with
        | true  when v1 = zero -> SicAlwaysTrue
        | true   -> SciConstraint (RangeContraint_val_MAX (v1,eqIsInc))
        | false  -> SciConstraint (RangeContraint_val_MAX (v1,eqIsInc))


let SIntHandleRangeContraint_val_MAX  (r:Asn1AcnAst.AstRoot) eqIsInc  v1 =
    match eqIsInc with
    | true  when v1 = r.args.SIntMin  -> SicAlwaysTrue
    | true   -> SciConstraint (RangeContraint_val_MAX (v1,eqIsInc))
    | false  -> SciConstraint (RangeContraint_val_MAX (v1,eqIsInc))


(* e.g INTEGER (MIN .. 40) --> intVal <= 40*)
let UintHandleRangeContraint_MIN_val (r:Asn1AcnAst.AstRoot) zero intMax eqIsInc  v1 =
    match v1 <= zero with
    | true  -> SicAlwaysTrue
    | false ->
        match eqIsInc with
        | true  when v1 = intMax -> SicAlwaysTrue
        | true   -> SciConstraint (RangeContraint_MIN_val (v1,eqIsInc))
        | false  -> SciConstraint (RangeContraint_MIN_val (v1,eqIsInc))


let SIntHandleRangeContraint_MIN_val (r:Asn1AcnAst.AstRoot)  eqIsInc  v1 =
    match eqIsInc with
    | true  when v1 = r.args.SIntMax -> SicAlwaysTrue
    | true   -> SciConstraint (RangeContraint_MIN_val (v1,eqIsInc))
    | false  -> SciConstraint (RangeContraint_MIN_val (v1,eqIsInc))
    
let simplifytIntegerTypeConstraint handleEqual handleRangeContraint_val_MAX handleRangeContraint_MIN_val  (c:RangeTypeConstraint<'a, 'a>) =
    let handleOr e1 e2 = 
        match e1, e2 with
        | SicAlwaysTrue, _                      -> SicAlwaysTrue
        | _          , SicAlwaysTrue            -> SicAlwaysTrue
        | SciConstraint e1, SciConstraint e2    -> SciConstraint(RangeUnionConstraint (e1,e2, false))
    let handleAnd e1 e2 =
        match e1, e2 with
        | SicAlwaysTrue, _             -> e2
        | _, SicAlwaysTrue             -> e1
        | SciConstraint e1, SciConstraint e2    -> SciConstraint(RangeIntersectionConstraint (e1,e2))
    let handleNot e = e

    foldRangeTypeConstraint        
        (fun e1 e2 b s      -> handleOr e1 e2, s)
        (fun e1 e2 s        -> handleAnd e1 e2, s)
        (fun e s            -> handleNot e, s)
        (fun e1 e2 s        -> handleAnd e1 (handleNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> handleOr e1 e2, s)
        (fun v  s           -> handleEqual v ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            let exp1 = handleRangeContraint_val_MAX minIsIn v1
            let exp2 = handleRangeContraint_MIN_val maxIsIn v2
            handleAnd exp1 exp2, s)
        (fun v1 minIsIn s   -> handleRangeContraint_val_MAX  minIsIn v1, s)
        (fun v2 maxIsIn s   -> handleRangeContraint_MIN_val maxIsIn v2, s)
        c
        0 |> fst




// constraint simplification ended here

let foldSizeRangeTypeConstraint (l:ProgrammingLanguage)  getSizeFunc (p:String) (c:PosIntTypeConstraint) = 
    foldRangeTypeConstraint        
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s         -> l.ExpEqual (getSizeFunc l p) (v.ToString()) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            l.ExpAnd (Lte l minIsIn (v1.ToString()) (getSizeFunc l p)) (Lte l maxIsIn (getSizeFunc l p) (v2.ToString())), s)
        (fun v1 minIsIn s   -> Lte l minIsIn (v1.ToString()) (getSizeFunc l p), s)
        (fun v2 maxIsIn s   -> Lte l maxIsIn (getSizeFunc l p) (v2.ToString()), s)
        c
        0 


let foldSizableConstraint (l:ProgrammingLanguage) compareSingValueFunc  getSizeFunc (p:String) (c:SizableTypeConstraint<'v>) =
    foldSizableTypeConstraint2
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s           -> (compareSingValueFunc p v) ,s)
        (fun intCon s       -> foldSizeRangeTypeConstraint l getSizeFunc p intCon)
        c
        0 |> fst



let foldStringCon (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) alphaFuncName (p:String)  (c:IA5StringConstraint)  =
    foldStringTypeConstraint2
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s         -> l.ExpStringEqual p v.IDQ ,s)
        (fun intCon s       -> 
            let aaa = [intCon] |> List.map (fun c -> simplifytIntegerTypeConstraint (UintHandleEqual r 0u) (UintHandleRangeContraint_val_MAX r 0u) (UintHandleRangeContraint_MIN_val r 0u UInt32.MaxValue) c) |> List.choose (fun sc -> match sc with SicAlwaysTrue -> None | SciConstraint c -> Some c)
            let bbb = aaa |> List.map (fun intCon -> foldSizeRangeTypeConstraint l (fun l p -> l.StrLen p) p intCon |> fst)
            l.ExpAndMulti bbb, s)
        (fun alphcon s      -> sprintf "%s(%s)" alphaFuncName p,s) 
        c
        0 |> fst

let hasValidationFunc allCons =
    match allCons with
    | []      -> false
    | _       -> true

let makeExpressionToStatement l = match l with C -> isvalid_c.makeExpressionToStatement | Ada -> isvalid_a.makeExpressionToStatement    | Python -> isvalid_p.makeExpressionToStatement
let callBaseTypeFunc l          = match l with C -> isvalid_c.call_base_type_func       | Ada -> isvalid_a.call_base_type_func          | Python -> isvalid_p.call_base_type_func
let callBaseTypeFuncExp l       = match l with C -> isvalid_c.call_base_type_func_exp   | Ada -> isvalid_a.call_base_type_func_exp      | Python -> isvalid_p.call_base_type_func_exp
let joinTwoIfFirstOk l          = match l with C -> isvalid_c.JoinTwoIfFirstOk          | Ada -> isvalid_a.JoinTwoIfFirstOk             | Python -> (fun _ _ -> "")

let getAddres = DAstEqual.getAddres

let createPrimitiveFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (typeId:ReferenceToType) allCons  conToStrFunc (typeDefinition:TypeDefinitionCommon) (alphaFuncs : AlphaFunc list) (us:State)  =
    let hasValidationFunc= hasValidationFunc allCons
    match allCons with
    | []            -> None, us
    | c::cs         ->
        let funcName            = getFuncName r l typeId
        let errCodeName         = ToC ("ERR_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errCode, ns = getNextValidErrorCode us errCodeName
        let funcExp (p:String) = 
            let allCons = allCons |> List.map (conToStrFunc p)
            match allCons with
            | []     -> raise(BugErrorException("Invalid case"))
            | c::cs  -> l.ExpAndMulti allCons 

        let funcBody (p:String) = 
            let allCons = allCons |> List.map (conToStrFunc p)
            match allCons with
            | []    -> raise(BugErrorException("Invalid case"))
            | c::cs ->
                makeExpressionToStatement l (l.ExpAndMulti allCons) errCode.errCodeName

        let  func  = 
                match funcName  with
                | None              -> None
                | Some funcName     -> 
                    let p = match l with C | Ada -> "val" | Python -> "value"
                    let exp = funcBody p  
                    match l with
                    |C     -> Some(isvalid_c.EmitTypeAssignment_primitive funcName  typeDefinition.name exp  (alphaFuncs |> List.map(fun x -> x.funcBody)) )
                    |Ada   -> Some(isvalid_a.EmitTypeAssignment_primitive funcName  typeDefinition.name exp  (alphaFuncs |> List.map(fun x -> x.funcBody)) )
                    |Python-> Some(isvalid_p.EmitTypeAssignment_primitive funcName  typeDefinition.name exp  (alphaFuncs |> List.map(fun x -> x.funcBody)) )
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     ->  Some(isvalid_c.EmitTypeAssignment_primitive_def funcName  typeDefinition.name errCode.errCodeName (BigInteger errCode.errCodeValue))
                    |Ada   ->  Some(isvalid_a.EmitTypeAssignment_primitive_def funcName  typeDefinition.name errCode.errCodeName (BigInteger errCode.errCodeValue))
                    |Python->  Some(isvalid_p.EmitTypeAssignment_primitive_def funcName  typeDefinition.name errCode.errCodeName (BigInteger errCode.errCodeValue))
        
        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = [errCode]
                func                        = func
                funcDef                     = funcDef
                funcExp                     = Some funcExp
                funcBody                    = funcBody 
                funcBody2                   = (fun p acc -> funcBody p)
                alphaFuncs                  = alphaFuncs
                localVariables              = []
                anonymousVariables          = []
            }    
        Some ret, ns

let createBitOrOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (typeId:ReferenceToType) allCons  conToStrFunc (typeDefinition:TypeDefinitionCommon) (alphaFuncs : AlphaFunc list)  anonymousVariables (us:State)  =
    match allCons with
    | []            -> None, us
    | _             ->
        let funcName            = getFuncName r l typeId
        let errCodeName         = ToC ("ERR_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errCode, ns = getNextValidErrorCode us errCodeName


        let funcBody (p:String) (childAccess:string)  = 
            let allCons = allCons |> List.map ((conToStrFunc childAccess) p )
            match allCons with
            | []    -> raise(BugErrorException("Invalid case"))
            | c::cs ->
                makeExpressionToStatement l (l.ExpAndMulti allCons) errCode.errCodeName


        let  func  = 
                match funcName  with
                | None              -> None
                | Some funcName     -> 
                    let topLevAcc, p =  match l with | C -> "->", "pVal" | Ada -> ".", "val" | Python -> ".", "value"
                    let exp = funcBody p  topLevAcc
                    match l with
                    |C     -> Some(isvalid_c.EmitTypeAssignment_oct_or_bit_string funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) )
                    |Ada   -> Some(isvalid_a.EmitTypeAssignment_primitive funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) )
                    |Python-> Some(isvalid_p.EmitTypeAssignment_primitive funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) )
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     ->  Some(isvalid_c.EmitTypeAssignment_oct_or_bit_string_def funcName  typeDefinition.name errCode.errCodeName (BigInteger errCode.errCodeValue))
                    |Ada   ->  Some(isvalid_a.EmitTypeAssignment_primitive_def funcName  typeDefinition.name errCode.errCodeName (BigInteger errCode.errCodeValue))
                    |Python->  Some(isvalid_c.EmitTypeAssignment_primitive_def funcName  typeDefinition.name errCode.errCodeName (BigInteger errCode.errCodeValue))
        
        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = [errCode]
                func                        = func
                funcExp                     = None
                funcDef                     = funcDef
                funcBody                    = (fun p -> funcBody p ".")
                funcBody2                   = funcBody
                alphaFuncs                  = alphaFuncs
                localVariables              = []
                anonymousVariables           = anonymousVariables
            }    
        Some ret, ns

let getIntSimplifiedConstraints (r:Asn1AcnAst.AstRoot) isUnsigned (allCons  : IntegerTypeConstraint list) =
    match isUnsigned with
    | true         -> allCons |> List.map (fun c -> simplifytIntegerTypeConstraint (UintHandleEqual r 0I) (UintHandleRangeContraint_val_MAX r 0I) (UintHandleRangeContraint_MIN_val r 0I r.args.UIntMax) c) |> List.choose (fun sc -> match sc with SicAlwaysTrue -> None | SciConstraint c -> Some c)
    | false        -> allCons |> List.map (fun c -> simplifytIntegerTypeConstraint (SIntHandleEqual r) (SIntHandleRangeContraint_val_MAX r) (SIntHandleRangeContraint_MIN_val r) c) |> List.choose (fun sc -> match sc with SicAlwaysTrue -> None | SciConstraint c -> Some c)
    

let integerToString (l:ProgrammingLanguage) isUnsigned (i:BigInteger) = 
    match l with
    | Ada   -> i.ToString()
    | C     ->
        match isUnsigned with
        | true   -> sprintf "%sUL" (i.ToString())
        | false  -> sprintf "%sLL" (i.ToString())
    | Python-> i.ToString()


let createIntegerFunctionByCons (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) isUnsigned (allCons  : IntegerTypeConstraint list) =
    let allCons = getIntSimplifiedConstraints r isUnsigned allCons
    let conToStrFunc = foldRangeCon l (integerToString l isUnsigned ) (integerToString l isUnsigned)
    match allCons with
    | []        -> None
    | _         ->
        let funcExp (p:String) = 
            let allCons = allCons |> List.map (conToStrFunc p)
            l.ExpAndMulti allCons 
        Some funcExp

let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionCommon) (us:State)  =
    let allCons = getIntSimplifiedConstraints r o.isUnsigned o.AllCons
    createPrimitiveFunction r l t.id allCons (foldRangeCon l (integerToString l o.isUnsigned ) (integerToString l o.isUnsigned)) typeDefinition []  us

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionCommon)  (us:State)  =
    createPrimitiveFunction r l t.id o.AllCons (foldRangeCon l (fun v -> v.ToString("E20", NumberFormatInfo.InvariantInfo)) (fun v -> v.ToString("E20", NumberFormatInfo.InvariantInfo))) typeDefinition [] us

let createStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionCommon) (us:State)  =
    let alphafuncName = match l with C | Ada -> ToC (((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_CharsAreValid") | Python -> "self._check_alphabet"
    let foldAlpha = (foldRangeCon l (fun v -> v.ToString().ISQ) (fun v -> v.ToString().ISQ))
    let accessChar = match l with C | Ada -> sprintf "str%s" (l.ArrayAccess "i") | Python -> "char"
    let alpaCons = o.AllCons |> List.choose(fun x -> match x with AlphabetContraint al-> Some al | _ -> None) |> List.map (foldAlpha (accessChar))
    let alphaFuncs = 
        match alpaCons with
        | []    -> []
        | _     ->
            let funcBody =
                match l with
                | C    -> isvalid_c.Print_AlphabetCheckFunc alphafuncName alpaCons
                | Ada  -> isvalid_a.Print_AlphabetCheckFunc alphafuncName alpaCons
                | Python-> isvalid_p.Print_AlphabetCheckFunc alphafuncName alpaCons
            let alphFunc = {AlphaFunc.funcName = alphafuncName; funcBody = funcBody }
            [alphFunc]
    createPrimitiveFunction r l t.id o.AllCons (foldStringCon r l alphafuncName) typeDefinition alphaFuncs us

let createBoolFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionCommon) (us:State)  =
    createPrimitiveFunction r l t.id (o.cons@o.withcons) (foldGenericCon l  (fun v -> v.ToString().ToLower())) typeDefinition [] us

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionCommon) (us:State)  =
    let printNamedItem (v:string) =
        let itm = o.items |> Seq.find (fun x -> x.Name.Value = v)
        itm.getBackendName l
    let cons = match l with C  | Ada -> o.AllCons | Python -> []
    createPrimitiveFunction r l t.id cons (foldGenericCon l  printNamedItem) typeDefinition [] us


let exlcudeSizeConstraintIfFixedSize minSize maxSize allCons = 
    match minSize = maxSize with
    | false -> allCons
    | true  -> allCons |> List.filter(fun x -> match x with SizeContraint al-> false | _ -> true)

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionCommon) (equalFunc:EqualFunction) (printValue  : (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let allCons = match l with C | Ada -> exlcudeSizeConstraintIfFixedSize o.minSize o.maxSize o.AllCons | Python -> o.AllCons
    let anonymousVariables =
        allCons |> 
        List.map DastFold.getValueFromSizeableConstraint 
        |> List.collect id |> 
        List.choose (fun (v:Asn1AcnAst.OctetStringValue, (id,loc)) ->
                    let recValue = {Asn1Value.kind = OctetStringValue (v |> List.map(fun z -> z.Value)); id=id;loc=loc}
                    match id with
                    | ReferenceToValue (typePath,(VA2 vasName)::[]) -> None
                    | ReferenceToValue(ts,vs)                       ->
                        let typeDefinitionName = 
                            match t.tasInfo with
                            | Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName)
                            | None            -> typeDefinition.typeDefinitionBodyWithinSeq
                        Some ({AnonymousVariable.valueName = (recValue.getBackendName l); valueExpresion = (printValue None recValue.kind); typeDefinitionName = typeDefinitionName}))
    let compareSingValueFunc (p:String) acc (v:Asn1AcnAst.OctetStringValue, (id,loc)) =
        let recValue = {Asn1Value.kind = OctetStringValue (v |> List.map(fun z -> z.Value)); id=id;loc=loc}
        let vstr = 
            match acc with
            | "->"  -> getAddres l (recValue.getBackendName l)
            | _     -> (recValue.getBackendName l)
        match equalFunc.isEqualBody2 with
        | EqualBodyExpression2 eqFunc    ->
            match eqFunc p vstr acc with
            | None          -> raise(BugErrorException "unexpected case")
            | Some (ret,_)      -> ret
        | EqualBodyStatementList2  _     -> raise(BugErrorException "unexpected case")
    let foldSizeCon childAccess = foldSizableConstraint l (fun p v -> compareSingValueFunc p childAccess v) (fun l p -> l.Length p childAccess)
    createBitOrOctetStringFunction r l t.id allCons foldSizeCon typeDefinition [] anonymousVariables us


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionCommon) (equalFunc:EqualFunction) (printValue  : (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let allCons = match l with C | Ada -> exlcudeSizeConstraintIfFixedSize o.minSize o.maxSize o.AllCons | Python -> o.AllCons
    let anonymousVariables =
        allCons |> 
        List.map DastFold.getValueFromSizeableConstraint 
        |> List.collect id |> 
        List.choose (fun (v:Asn1AcnAst.BitStringValue, (id,loc)) ->
                    let recValue = {Asn1Value.kind = BitStringValue (v.Value ); id=id;loc=loc}
                    match id with
                    | ReferenceToValue (typePath,(VA2 vasName)::[]) -> None
                    | ReferenceToValue(ts,vs)                       ->
                        let typeDefinitionName = 
                            match t.tasInfo with
                            | Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName)
                            | None            -> typeDefinition.typeDefinitionBodyWithinSeq
                        Some ({AnonymousVariable.valueName = (recValue.getBackendName l); valueExpresion = (printValue None recValue.kind); typeDefinitionName = typeDefinitionName}))
    let compareSingValueFunc (p:String) acc (v:Asn1AcnAst.BitStringValue, (id,loc)) =
        let recValue = {Asn1Value.kind = BitStringValue (v.Value ); id=id;loc=loc}
        let vstr = 
            match acc with
            | "->"  -> getAddres l (recValue.getBackendName l)
            | _     -> recValue.getBackendName l
        match equalFunc.isEqualBody2 with
        | EqualBodyExpression2 eqFunc    ->
            match eqFunc p vstr acc with
            | None          -> raise(BugErrorException "unexpected case")
            | Some (ret,_)      -> ret
        | EqualBodyStatementList2  _     -> raise(BugErrorException "unexpected case")
    let foldSizeCon childAccess = foldSizableConstraint l (fun p v -> compareSingValueFunc p childAccess v) (fun l p -> l.Length p childAccess)
    createBitOrOctetStringFunction r l t.id allCons foldSizeCon typeDefinition []  anonymousVariables us


(*  SEQUENCE *)

let isValidSequenceChild   (l:ProgrammingLanguage) (o:Asn1AcnAst.Asn1Child) (newChild:Asn1Type) (us:State)= 
    let c_name = ToC o.c_name
    let sInnerStatement = 
        match newChild.isValidFunction with
        | Some (isValidFunction)    ->
            Some((fun p childAccess ->
                    isValidFunction.funcBody (p + childAccess + c_name)), isValidFunction)
        | None      -> None
    let sInnerStatement =
        match sInnerStatement with
        | None                  -> None
        | Some (func, isValid)  ->
            match o.Optionality with
            | Some _    -> 
                match l with
                | C     -> 
                    let newFunc = (fun p childAccess -> isvalid_c.Sequence_OptionalChild p childAccess c_name (func p childAccess))
                    Some (newFunc, isValid)
                | Ada   -> 
                    let newFunc = (fun p childAccess -> isvalid_a.Sequence_OptionalChild p childAccess c_name (func p childAccess))
                    Some (newFunc, isValid)
                | Python     -> 
                    let newFunc = (fun p childAccess -> "")
                    Some (newFunc, isValid)
            | None      -> Some (func, isValid)
    let isAlwaysPresentStatement, finalState =
        let child_always_present_or_absent = 
            match l with 
            | C         -> isvalid_c.Sequence_optional_child_always_present_or_absent 
            | Ada       -> isvalid_a.Sequence_optional_child_always_present_or_absent 
            | Python    -> isvalid_c.Sequence_optional_child_always_present_or_absent // Don't need
            
        match o.Optionality with
        | Some(Asn1AcnAst.AlwaysAbsent)                     -> 
            let errCodeName = ToC ("ERR_" + ((newChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm"))) + "_IS_PRESENT"
            let errCode, ns = getNextValidErrorCode us errCodeName
            let isValidStatement = (fun p childAccess -> child_always_present_or_absent p childAccess c_name errCode.errCodeName "0")
            Some(isValidStatement, errCode), ns
        | Some(Asn1AcnAst.AlwaysPresent)                    -> 
            let errCodeName = ToC ("ERR_" + ((newChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm"))) + "_IS_ABSENT"
            let errCode, ns = getNextValidErrorCode us errCodeName
            let isValidStatement = (fun p childAccess -> child_always_present_or_absent p childAccess c_name errCode.errCodeName "1")
            Some(isValidStatement, errCode), ns
        | _         -> None, us

    match sInnerStatement, isAlwaysPresentStatement with
    | None, None                                       -> None , finalState
    | None, Some(isValid, errCode)                     -> 
        Some({SeqChoiceChildInfoIsValid.isValidStatement = isValid; localVars = []; alphaFuncs = []; errCode = [errCode]}), finalState
    | Some(isValid, chFunc), None                      -> 
        Some({SeqChoiceChildInfoIsValid.isValidStatement = isValid; localVars = chFunc.localVariables; alphaFuncs = chFunc.alphaFuncs; errCode = chFunc.errCodes}), finalState
    | Some(isValid1, chFunc), Some(isValid2, errCode)    -> 
        // isvalid_c.JoinTwo is language independent so it is used for both C and Ada
        let isValid = (fun p childAccess -> isvalid_c.JoinTwo (isValid2 p childAccess)  (isValid1 p childAccess)) 
        Some({SeqChoiceChildInfoIsValid.isValidStatement = isValid; localVars = chFunc.localVariables; alphaFuncs = chFunc.alphaFuncs; errCode = errCode::chFunc.errCodes}), finalState


let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionCommon) (children:SeqChildInfo list)  (us:State)  =
    let funcName     = getFuncName r l t.id
    let asn1Children = children |> List.choose(fun c -> match c with Asn1Child x -> Some x | AcnChild _ -> None)
    let body = 
        match l with 
        | C  
        | Ada ->
            let childrenConent, finalState =   
                asn1Children |>
                Asn1Fold.foldMap (fun errCode cc -> cc.isValidBodyStats errCode) us
            let childrenConent =  childrenConent |> List.choose id

            match childrenConent with
            | []    -> None
            | x::xs ->
                let alphaFuncs = childrenConent |> List.collect(fun x -> x.alphaFuncs)
                let localVars = childrenConent |> List.collect(fun x -> x.localVars)
                let ercCodes = childrenConent |> List.collect(fun x -> x.errCode)
                let funcBody  (p:string)  (childAccess:string) = 
                    let printChild (content:string) (sNestedContent:string option) = 
                        match sNestedContent with
                        | None  -> content
                        | Some c-> 
                            match l with
                            | C        -> equal_c.JoinItems content sNestedContent
                            | Ada      -> isvalid_a.JoinItems content sNestedContent
                            | Python   -> ""
                    let rec printChildren children : string option = 
                        match children with
                        |[]     -> None
                        |x::xs  -> 
                            match printChildren xs with
                            | None                 -> Some (printChild x  None)
                            | Some childrenCont    -> Some (printChild x  (Some childrenCont))

                    let isValidStatementX = x.isValidStatement p childAccess 
                    let isValidStatementXS = xs |> List.map(fun x -> x.isValidStatement  p childAccess)
                    printChild isValidStatementX (printChildren isValidStatementXS)
                Some(alphaFuncs, localVars, ercCodes, funcBody, finalState)
        | Python -> None // Don't need valid function for sequence
    match body with
    | None    -> None, us
    | Some(alphaFuncs, localVars, ercCodes, funcBody, finalState) ->
        let  func  = 
            let topLevAcc, p = match l with | C -> "->", "pVal" | Ada -> ".", "val" | Python -> ".", "value"
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                let exp = funcBody p topLevAcc
                let lvars = localVars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                match l with
                |C     -> Some(isvalid_c.EmitTypeAssignment_composite funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Ada   -> Some(isvalid_a.EmitTypeAssignment_composite funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Python-> None
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     -> 
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_c.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_c.EmitTypeAssignment_composite_def funcName  typeDefinition.name arrsErrcodes)
                    |Ada   -> 
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_a.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_a.EmitTypeAssignment_composite_def funcName  typeDefinition.name arrsErrcodes)
                    |Python     -> 
                        None

        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = ercCodes
                func                        = func
                funcDef                     = funcDef
                funcExp                     = None
                funcBody                    = (fun p -> funcBody p ".")
                funcBody2                   = funcBody
                alphaFuncs                  = alphaFuncs
                localVariables              = localVars
                anonymousVariables          = 
                    let ret = asn1Children |> List.collect(fun c -> match c.Type.isValidFunction with Some vf -> vf.anonymousVariables | None -> [])
                    ret |> Seq.distinctBy(fun x -> x.valueName) |> Seq.toList
            }    
        Some ret, finalState

(*  CHOICE *)
let isValidChoiceChild   (l:ProgrammingLanguage) (o:Asn1AcnAst.ChChildInfo) (newChild:Asn1Type) (us:State)= 
    let c_name = ToC o.c_name
    let sInnerStatement = 
        match newChild.isValidFunction with
        | Some (isValidFunction)    ->
             Some((fun p childAccess ->isValidFunction.funcBody (p + childAccess + c_name)), isValidFunction)
        | None      -> None
    

    match sInnerStatement with
    | None  -> None , us
    | Some(isValid, chFunc)                      -> 
        Some({SeqChoiceChildInfoIsValid.isValidStatement = isValid; localVars = chFunc.localVariables; alphaFuncs = chFunc.alphaFuncs; errCode = chFunc.errCodes}), us

let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionCommon) (children:ChChildInfo list) (baseTypeValFunc : IsValidFunction option) (us:State)  =
    let funcName            = getFuncName r l t.id

    let body = 
        match l with 
        | C  
        | Ada ->
            let errCodeName         = ToC ("ERR_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
            //let errCodeValue        = us.currErrCode
            //let errCode             = {ErroCode.errCodeName = errCodeName; errCodeValue = errCodeValue}
    
            let errCode, ns = getNextValidErrorCode us errCodeName
    
    
            let childrenConent, finalState =   
                children |> 
                Asn1Fold.foldMap (fun errCode cc -> 
                    let (vc,erc) = cc.isValidBodyStats errCode
                    ((cc,vc),erc)) ns
            //let deltaErrCode = finalErrCode - us.currErrCode
    
            let validatedComponenets = childrenConent |> List.map snd |> List.choose id
            let alphaFuncs = validatedComponenets |> List.collect(fun x -> x.alphaFuncs)
            let localVars =  validatedComponenets |> List.collect(fun x -> x.localVars)
            let ercCodes =   errCode::(validatedComponenets |> List.collect(fun x -> x.errCode))
            let funcBody  (p:string)  (childAccess:string) = 
                let childrenContent =
                    childrenConent |> 
                    List.map(fun (cc, vc) -> 
                    match l with
                    | C    -> 
                        let chBody =  
                            match vc with
                            | Some vc -> vc.isValidStatement (p+childAccess+"u")  "." 
                            | None    -> isvalid_c.always_true_statement ()
                        isvalid_c.choice_child cc.presentWhenName chBody
                    |Ada   -> 
                        let chBody = 
                            match vc with
                            | Some vc -> vc.isValidStatement p "."
                            | None    -> isvalid_a.always_true_statement ()
                        isvalid_a.choice_child cc.presentWhenName chBody
                    |Python-> "")
                match l with
                | C    -> isvalid_c.choice p childAccess childrenContent errCode.errCodeName
                |Ada   -> isvalid_a.choice p childAccess childrenContent errCode.errCodeName
                |Python-> ""
            Some(alphaFuncs, localVars, ercCodes, funcBody, finalState)
        | Python -> None // Don't need valid function for Choice
    match body with
    | None    -> None, us
    | Some(alphaFuncs, localVars, ercCodes, funcBody, finalState) ->
        let  func  = 
            let topLevAcc, p =  match l with | C -> "->", "pVal" | Ada -> ".", "val"  | Python -> ".", "value"
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                let exp = funcBody p topLevAcc
                let lvars = localVars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                match l with
                |C     -> Some(isvalid_c.EmitTypeAssignment_composite funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Ada   -> Some(isvalid_a.EmitTypeAssignment_composite funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Python-> None
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     ->  
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_c.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_c.EmitTypeAssignment_composite_def funcName  typeDefinition.name arrsErrcodes)
                    |Ada   ->  
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_a.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_a.EmitTypeAssignment_composite_def funcName  typeDefinition.name arrsErrcodes)
                    |Python     ->  
                        None
        
        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = ercCodes
                func                        = func
                funcDef                     = funcDef
                funcExp                     = None
                funcBody                    = (fun p -> funcBody p ".")
                funcBody2                   = funcBody
                alphaFuncs                  = alphaFuncs
                localVariables              = localVars
                anonymousVariables          = 
                    let ret =children |> List.collect(fun c -> match c.chType.isValidFunction with Some vf -> vf.anonymousVariables | None -> []) 
                    ret |> Seq.distinctBy(fun x -> x.valueName) |> Seq.toList
            }    
        Some ret, finalState


let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionCommon) (childType:Asn1Type) (baseTypeValFunc : IsValidFunction option) (us:State)  =
    let funcName            = getFuncName r l t.id
    let bIsFixedSize = o.minSize = o.maxSize
    let hasValidationFunc = 
        match bIsFixedSize with
        | true when l = Python  -> true
        | false                 -> true
        | true                  ->
            match childType.isValidFunction with
            | Some _  -> true
            | None    -> false

    let baseCallStatement l p baseFncName =
        callBaseTypeFunc l (getAddres l p) baseFncName

    (*alphaFuncs, localVars, ercCodes, funcBody, deltaErrCode*)
    let body =
        let allSizeCons = o.AllCons |> List.filter(fun x -> match x with SizeContraint al-> true | _ -> false)
        let foldSizeCon childAccess = foldSizableConstraint l (fun p v -> v.ToString()) (fun l p -> l.Length p childAccess)
        let sizeConstrData = 
            match bIsFixedSize with
            | true  -> None
            | false ->
                match allSizeCons with
                | []    -> None
                | _     ->
                    let errCodeName         = ToC ("ERR_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
                    let errCode, ns = getNextValidErrorCode us errCodeName
                    let sIsValidSizeExpFunc (p:string) (childAccess:string) =
                        let allCons = allSizeCons |> List.map ((foldSizeCon childAccess) p )
                        l.ExpAndMulti allCons
                    Some(errCode, sIsValidSizeExpFunc, ns)
        let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
        let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
        match childType.isValidFunction, sizeConstrData with
        | None, None when l = Python    -> 
            let funcBody (p:String) (childAccess:string)  = 
                isvalid_p.sequenceOf p childAccess i bIsFixedSize (BigInteger o.minSize) None None None
            Some([],[], [], funcBody, us)
        | None, None     -> None
        | Some cvf, None ->
            let funcBody (p:String) (childAccess:string)  = 
                let childAccesPath = p + childAccess + l.ArrName + (l.ArrayAccess i) //"[" + i + "]"
                let innerStatement = Some(cvf.funcBody childAccesPath )
                match l with
                | C         -> isvalid_c.sequenceOf p childAccess i bIsFixedSize (BigInteger o.minSize) None None innerStatement
                | Ada       -> isvalid_a.sequenceOf p childAccess i bIsFixedSize (BigInteger o.minSize) None None innerStatement
                | Python    -> isvalid_p.sequenceOf p childAccess i bIsFixedSize (BigInteger o.minSize) None None innerStatement
            Some(cvf.alphaFuncs, lv::cvf.localVariables , cvf.errCodes, funcBody, us)
        | None, Some(errCode, sIsValidSizeExpFunc, ns) ->
            let funcBody (p:String) (childAccess:string)  = 
                makeExpressionToStatement l (sIsValidSizeExpFunc p childAccess) errCode.errCodeName
            Some([],[], [errCode], funcBody, ns)
        | Some cvf, Some(errCode, sIsValidSizeExpFunc, ns) ->
            let funcBody (p:String) (childAccess:string)  = 
                let childAccesPath = p + childAccess + l.ArrName + (l.ArrayAccess i) //"[" + i + "]"
                let innerStatement = Some(cvf.funcBody childAccesPath )
                match l with
                | C     -> isvalid_c.sequenceOf p childAccess i bIsFixedSize (BigInteger o.minSize) (Some (sIsValidSizeExpFunc p childAccess)) (Some errCode.errCodeName) innerStatement
                | Ada   -> isvalid_a.sequenceOf p childAccess i bIsFixedSize (BigInteger o.minSize) (Some (sIsValidSizeExpFunc p childAccess)) (Some errCode.errCodeName) innerStatement
                | Python-> isvalid_p.sequenceOf p childAccess i bIsFixedSize (BigInteger o.minSize) (Some (sIsValidSizeExpFunc p childAccess)) (Some errCode.errCodeName) innerStatement
            Some(cvf.alphaFuncs, lv::cvf.localVariables , cvf.errCodes@[errCode], funcBody, ns)


    match body with
    | None -> None, us
    | Some(alphaFuncs, localVars, ercCodes, funcBody, newState) ->
        let  func  = 
            let topLevAcc, p =  match l with | C -> "->", "pVal" | Ada -> ".", "val" | Python -> ".", "value"
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                let exp = funcBody p topLevAcc
                let lvars = localVars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                match l with
                |C      -> Some(isvalid_c.EmitTypeAssignment_composite funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Ada    -> Some(isvalid_a.EmitTypeAssignment_composite funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Python -> Some(isvalid_p.EmitTypeAssignment_primitive funcName  typeDefinition.name exp (alphaFuncs |> List.map(fun x -> x.funcBody)))
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     ->  
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_c.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_c.EmitTypeAssignment_composite_def funcName  typeDefinition.name arrsErrcodes)
                    |Ada   ->  
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_a.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_a.EmitTypeAssignment_composite_def funcName  typeDefinition.name arrsErrcodes)
                    |Python     ->  
                        Some(isvalid_p.EmitTypeAssignment_primitive_def funcName  typeDefinition.name "" (BigInteger 0))

        
        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = ercCodes
                func                        = func
                funcDef                     = funcDef
                funcExp                     = None
                funcBody                    = (fun p -> funcBody p ".")
                funcBody2                   = funcBody
                alphaFuncs                  = alphaFuncs
                localVariables              = localVars
                anonymousVariables          = 
                    match childType.isValidFunction with
                    | Some v  -> v.anonymousVariables
                    | None    -> []
            }    
        Some ret, newState


let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionCommon) (baseType:Asn1Type)  (us:State)  =
    baseType.isValidFunction, us    
(*
    let typeDefinitionName = 
        match t.tasInfo with
        | Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName)
        | None            -> ToC2(r.args.TypePrefix + o.tasName.Value)
    let baseFncName = typeDefinitionName + "_IsConstraintValid"
    let baseCallStatement l p baseFncName =
        callBaseTypeFunc l (getAddres l p) baseFncName

    let funcBody (p:String) (childAccess:string)  = 
        baseCallStatement l p baseFncName

    let ret = 
        {
            IsValidFunction.funcName    = None
            errCodes                    = []
            func                        = None
            funcDef                     = None
            funcExp                     = None
            funcBody                    = (fun p -> funcBody p ".")
            funcBody2                   = funcBody
            alphaFuncs                  = []
            localVariables              = []
        }    
    Some ret, {us with currErrCode = us.currErrCode + 0}
*)