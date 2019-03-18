module DAstValidate

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



let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (td:FE_TypeDefinition) =
    match typeId.tasInfo with
    | None -> None
    | Some _ -> Some (td.typeName + "_IsConstraintValid")

let Lte (l:ProgrammingLanguage) eqIsInc  e1 e2 =
    match eqIsInc with
    | true   -> l.ExpLte e1 e2        
    | false  -> l.ExpLt  e1 e2

let foldGenericCon (l:ProgrammingLanguage) valToStrFunc  (p:CallerScope)  (c:GenericConstraint<'v>)  =
    foldGenericConstraint
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s           -> l.ExpEqual (p.arg.getValue l) (valToStrFunc p v) ,s)
        c
        0 |> fst

let foldRangeCon (l:ProgrammingLanguage) valToStrFunc1 valToStrFunc2 (p:CallerScope)  (c:RangeTypeConstraint<'v1,'v2>)  =
    foldRangeTypeConstraint        
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s         -> l.ExpEqual (p.arg.getValue l) (valToStrFunc2 v) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            l.ExpAnd (Lte l minIsIn (valToStrFunc1 v1) (p.arg.getValue l)) (Lte l maxIsIn (p.arg.getValue l) (valToStrFunc1 v2)), s)
        (fun v1 minIsIn s   -> Lte l minIsIn (valToStrFunc1 v1) (p.arg.getValue l), s)
        (fun v2 maxIsIn s   -> Lte l maxIsIn (p.arg.getValue l) (valToStrFunc1 v2), s)
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
    match v1 < zero with
    | true  -> SicAlwaysTrue
    | false ->
        match eqIsInc with
        | true  when v1 = intMax && intMax <> zero -> SicAlwaysTrue
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

let foldSizeRangeTypeConstraint (l:ProgrammingLanguage)  getSizeFunc (p:CallerScope) (c:PosIntTypeConstraint) = 
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


let foldSizableConstraint (l:ProgrammingLanguage) compareSingValueFunc  getSizeFunc (p:CallerScope) (c:SizableTypeConstraint<'v>) =
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




type FoldStringCon23State = {
    alphaIndex : int
    alphaFuncs : AlphaFunc list //func name, func body
}
let foldStringCon (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) alphaFuncName (p:CallerScope)  (c:IA5StringConstraint) (us0:FoldStringCon23State) =
    foldStringTypeConstraint2
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s         -> l.ExpStringEqual p.arg.p v.IDQ ,s)
        (fun intCon s       -> 
            let aaa = [intCon] |> List.map (fun c -> simplifytIntegerTypeConstraint (UintHandleEqual r 0u) (UintHandleRangeContraint_val_MAX r 0u) (UintHandleRangeContraint_MIN_val r 0u UInt32.MaxValue) c) |> List.choose (fun sc -> match sc with SicAlwaysTrue -> None | SciConstraint c -> Some c)
            let bbb = aaa |> List.map (fun intCon -> foldSizeRangeTypeConstraint l (fun l p -> l.StrLen p.arg.p) p intCon |> fst)
            l.ExpAndMulti bbb, s)
        (fun alphcon s      -> 
            let foldAlpha = (foldRangeCon l (fun v -> v.ToString().ISQ) (fun v -> v.ToString().ISQ))
            let alphaBody = foldAlpha ({CallerScope.modName = p.modName; arg = VALUE (sprintf "str%s" (l.ArrayAccess "i"))}) alphcon
            let alphaFuncName = sprintf "%s_%d" alphaFuncName s.alphaIndex
            let funcBody =
                match l with
                | C    -> isvalid_c.Print_AlphabetCheckFunc alphaFuncName [alphaBody]
                | Ada  -> isvalid_a.Print_AlphabetCheckFunc alphaFuncName [alphaBody]
            let alphFunc = {AlphaFunc.funcName = alphaFuncName; funcBody = funcBody }

            let newState = {FoldStringCon23State.alphaIndex = s.alphaIndex + 1; alphaFuncs = alphFunc::s.alphaFuncs}
            sprintf "%s(%s)" alphaFuncName p.arg.p, newState) 
        c
        us0 


let hasValidationFunc allCons =
    match allCons with
    | []      -> false
    | _       -> true

//let makeExpressionToStatement l = match l with C -> isvalid_new_c.makeExpressionToStatement | Ada -> isvalid_new_a.makeExpressionToStatement
let callBaseTypeFunc l = match l with C -> isvalid_c.call_base_type_func | Ada -> isvalid_a.call_base_type_func
let callBaseTypeFuncExp l = match l with C -> isvalid_c.call_base_type_func_exp | Ada -> isvalid_a.call_base_type_func_exp
let joinTwoIfFirstOk l = match l with C -> isvalid_c.JoinTwoIfFirstOk | Ada -> isvalid_a.JoinTwoIfFirstOk

//let getAddres = DAstEqual.getAddres

type ConstraintToStringFunc<'c,'state> =
    | StatelessConstraintToStringFunc of (CallerScope->'c->string)
    | StatefullConstraintToStringFunc of 'state * (CallerScope -> 'c -> 'state -> string*'state)


let createPrimitiveFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) allCons  conToStrFunc (typeDefinition:TypeDefintionOrReference) (alphaFuncs : AlphaFunc list) (us:State)  =
    let hasValidationFunc= hasValidationFunc allCons
    
//    match allCons with
//    | []            -> None, us
//    | _         ->
    let funcName            = getFuncName r l t.id (t.FT_TypeDefintion.[l])
    let errCodeName         = ToC ("ERR_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName
    let funcExp (p:CallerScope) = 
            let allCons = allCons |> List.map (conToStrFunc p)
            match allCons with
            | []     -> "TRUE"
            | c::cs  -> l.ExpAndMulti allCons 

    let funcBody (p:CallerScope) = 
        r.stg.is_valid.makeExpressionToStatement (funcExp p) errCode.errCodeName

    let p  = t.getParamType l Encode
    let varName = p.arg.p
    let sStar = p.arg.getStar l
    let  func  = 
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                let exp = funcBody p  
                match l with
                |C     -> Some(isvalid_c.EmitTypeAssignment_primitive varName sStar funcName  (typeDefinition.longTypedefName l) exp  (alphaFuncs |> List.map(fun x -> x.funcBody)) )
                |Ada   -> Some(isvalid_a.EmitTypeAssignment_primitive varName sStar funcName  (typeDefinition.longTypedefName l) exp  (alphaFuncs |> List.map(fun x -> x.funcBody)) )
    let  funcDef  = 
            match funcName with
            | None              -> None
            | Some funcName     -> 
                match l with
                |C     ->  Some(isvalid_c.EmitTypeAssignment_primitive_def varName sStar funcName  (typeDefinition.longTypedefName l) errCode.errCodeName (BigInteger errCode.errCodeValue))
                |Ada   ->  Some(isvalid_a.EmitTypeAssignment_primitive_def varName sStar funcName  (typeDefinition.longTypedefName l) errCode.errCodeName (BigInteger errCode.errCodeValue))
        
    let ret = 
        {
            IsValidFunction.funcName    = funcName
            errCodes                    = [errCode]
            func                        = func
            funcDef                     = funcDef
            funcExp                     = Some funcExp
            funcBody                    = funcBody 
            //funcBody2                   = (fun p acc -> funcBody p)
            alphaFuncs                  = alphaFuncs
            localVariables              = []
            anonymousVariables          = []
        }    
    Some ret, ns



let createBitOrOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) allCons  conToStrFunc (typeDefinition:TypeDefintionOrReference) (alphaFuncs : AlphaFunc list)  anonymousVariables (us:State)  =
//    match allCons with
//    | []            -> None, us
//    | _             ->
        let funcName            = getFuncName r l t.id (t.FT_TypeDefintion.[l])
        let errCodeName         = ToC ("ERR_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errCode, ns = getNextValidErrorCode us errCodeName

        let funcExp (p:CallerScope) = 
            let allCons = allCons |> List.map (conToStrFunc p)
            match allCons with
            //| []    -> raise(BugErrorException("Invalid case"))
            | []    -> "TRUE"
            | c::cs -> l.ExpAndMulti allCons

        let funcBody (p:CallerScope)  = 
            r.stg.is_valid.makeExpressionToStatement (funcExp p) errCode.errCodeName

        let p  = t.getParamType l Encode
        let varName = p.arg.p
        let sStar = p.arg.getStar l

        let  func  = 
                match funcName  with
                | None              -> None
                | Some funcName     -> 
                    let exp = funcBody p  
                    match l with
                    |C     -> Some(isvalid_c.EmitTypeAssignment_oct_or_bit_string varName sStar funcName  (typeDefinition.longTypedefName l) exp (alphaFuncs |> List.map(fun x -> x.funcBody)) )
                    |Ada   -> Some(isvalid_a.EmitTypeAssignment_primitive varName sStar funcName  (typeDefinition.longTypedefName l) exp (alphaFuncs |> List.map(fun x -> x.funcBody)) )
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     ->  Some(isvalid_c.EmitTypeAssignment_oct_or_bit_string_def varName sStar funcName  (typeDefinition.longTypedefName l) errCode.errCodeName (BigInteger errCode.errCodeValue))
                    |Ada   ->  Some(isvalid_a.EmitTypeAssignment_primitive_def varName sStar funcName  (typeDefinition.longTypedefName l) errCode.errCodeName (BigInteger errCode.errCodeValue))
        
        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = [errCode]
                func                        = func
                funcExp                     = Some funcExp
                funcDef                     = funcDef
                funcBody                    = (fun p -> funcBody p )
                //funcBody2                   = funcBody
                alphaFuncs                  = alphaFuncs
                localVariables              = []
                anonymousVariables           = anonymousVariables
            }    
        Some ret, ns

let getIntSimplifiedConstraints (r:Asn1AcnAst.AstRoot) isUnsigned (allCons  : IntegerTypeConstraint list) =
    match isUnsigned with
    | true         -> 
        allCons |> 
        List.map (fun c -> 
            let ret = simplifytIntegerTypeConstraint (UintHandleEqual r 0I) (UintHandleRangeContraint_val_MAX r 0I) (UintHandleRangeContraint_MIN_val r 0I r.args.UIntMax) c
            ret
        ) |> 
        List.choose (fun sc -> 
            match sc with 
            | SicAlwaysTrue -> None 
            | SciConstraint c -> Some c)
    | false        -> allCons |> List.map (fun c -> simplifytIntegerTypeConstraint (SIntHandleEqual r) (SIntHandleRangeContraint_val_MAX r) (SIntHandleRangeContraint_MIN_val r) c) |> List.choose (fun sc -> match sc with SicAlwaysTrue -> None | SciConstraint c -> Some c)
    

let integerToString (l:ProgrammingLanguage) isUnsigned (i:BigInteger) = 
    match l with
    | Ada   -> i.ToString()
    | C     ->
        match isUnsigned with
        | true   -> sprintf "%sUL" (i.ToString())
        | false  -> sprintf "%sLL" (i.ToString())


let createIntegerFunctionByCons (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) isUnsigned (allCons  : IntegerTypeConstraint list) =
    let allCons = getIntSimplifiedConstraints r isUnsigned allCons
    let conToStrFunc = foldRangeCon l (integerToString l isUnsigned ) (integerToString l isUnsigned)
    match allCons with
    | []        -> None
    | _         ->
        let funcExp (p:CallerScope) = 
            let allCons = allCons |> List.map (conToStrFunc p)
            l.ExpAndMulti allCons 
        Some funcExp

let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let allCons = getIntSimplifiedConstraints r o.isUnsigned o.AllCons
    createPrimitiveFunction r l t allCons (foldRangeCon l (integerToString l o.isUnsigned ) (integerToString l o.isUnsigned)) typeDefinition []  us

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference)  (us:State)  =
    createPrimitiveFunction r l t o.AllCons (foldRangeCon l (fun v -> v.ToString("E20", NumberFormatInfo.InvariantInfo)) (fun v -> v.ToString("E20", NumberFormatInfo.InvariantInfo))) typeDefinition [] us

let createStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let allCons = o.AllCons
    let alphafuncName = ToC (((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_CharsAreValid")
    let initialState = {FoldStringCon23State. alphaIndex = 0; alphaFuncs = [] }

    let funcName            = getFuncName r l t.id (t.FT_TypeDefintion.[l])
    let errCodeName         = ToC ("ERR_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName
    let funcExp (p:CallerScope) = 
        match allCons with
        | []     -> "TRUE", []
        | c::cs  -> 
            let allCons,fs = allCons |> foldMap (fun st c -> foldStringCon r l alphafuncName p c st) initialState
            l.ExpAndMulti allCons, fs.alphaFuncs 

    let funcBody (p:CallerScope) = 
        let exp, alphaFuncs = funcExp p
        r.stg.is_valid.makeExpressionToStatement exp errCode.errCodeName, alphaFuncs

    let funcExp2 (p:CallerScope) = funcExp p |> fst
    let funcBody2 (p:CallerScope) =  funcBody p |> fst

    let p  = t.getParamType l Encode
    let varName = p.arg.p
    let sStar = p.arg.getStar l
    let exp, alphaFuncs = funcBody p  
    let  func  = 
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                match l with
                |C     -> Some(isvalid_c.EmitTypeAssignment_primitive varName sStar funcName  (typeDefinition.longTypedefName l) exp  (alphaFuncs |> List.map(fun x -> x.funcBody)) ) 
                |Ada   -> Some(isvalid_a.EmitTypeAssignment_primitive varName sStar funcName  (typeDefinition.longTypedefName l) exp  (alphaFuncs |> List.map(fun x -> x.funcBody)) ) 
    let  funcDef  = 
            match funcName with
            | None              -> None
            | Some funcName     -> 
                match l with
                |C     ->  Some(isvalid_c.EmitTypeAssignment_primitive_def varName sStar funcName  (typeDefinition.longTypedefName l) errCode.errCodeName (BigInteger errCode.errCodeValue))
                |Ada   ->  Some(isvalid_a.EmitTypeAssignment_primitive_def varName sStar funcName  (typeDefinition.longTypedefName l) errCode.errCodeName (BigInteger errCode.errCodeValue))
        
    let ret = 
        {
            IsValidFunction.funcName    = funcName
            errCodes                    = [errCode]
            func                        = func
            funcDef                     = funcDef
            funcExp                     = Some funcExp2
            funcBody                    = funcBody2 
            //funcBody2                   = (fun p acc -> funcBody p)
            alphaFuncs                  = alphaFuncs
            localVariables              = []
            anonymousVariables          = []
        }    
    Some ret, ns



let createBoolFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    createPrimitiveFunction r l t (o.cons@o.withcons) (foldGenericCon l  (fun p v -> v.ToString().ToLower())) typeDefinition [] us

type ObjectIdConstraintOrBasicValidation =
    | ObjectIdConstraint_basic
    | ObjectIdConstraint_const of ObjectIdConstraint
let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let conToStrFunc_basic (p:CallerScope)  = 
        match o.relativeObjectId with
        | false -> sprintf "ObjectIdentifier_isValid(%s)" (p.arg.getPointer l) 
        | true -> sprintf "RelativeOID_isValid(%s)" (p.arg.getPointer l)  
    let conToStrFunc_c = (foldGenericCon l  (fun p v -> v.ToString().ToLower()))
    let conToStrFunc (p:CallerScope) (mc:ObjectIdConstraintOrBasicValidation) =
        match mc with
        | ObjectIdConstraint_basic      -> conToStrFunc_basic p
        | ObjectIdConstraint_const oc   -> conToStrFunc_c p oc

    let newCons = ObjectIdConstraint_basic :: (o.cons@o.withcons |> List.map ObjectIdConstraint_const)
    createPrimitiveFunction r l t newCons conToStrFunc typeDefinition [] us


let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference) (defOrDer:TypeDefintionOrReference) (us:State)  =
    let printNamedItem (p:CallerScope) (v:string) =
        let itm = o.items |> Seq.find (fun x -> x.Name.Value = v)
        let ret = itm.getBackendName (Some defOrDer) l
        ret
//        let dummy = typeDefinition
//        let dummy2 = defOrDer
//        if ret = "voltage_1" then
//            printfn "ret"
//        match l with
//        | C     ->  ret
//        | Ada   ->
//            match t.inheritInfo with
//            | None  -> ret
//            | Some inhInfo -> 
//                match inhInfo.modName = p.modName with
//                | true  -> ret
//                | false -> (ToC inhInfo.modName) + "." + ret
    createPrimitiveFunction r l t o.AllCons (foldGenericCon l  printNamedItem) typeDefinition [] us




let exlcudeSizeConstraintIfFixedSize minSize maxSize allCons = 
    match minSize = maxSize with
    | false -> allCons
    | true  -> allCons |> List.filter(fun x -> match x with SizeContraint al-> false | _ -> true)

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference) (equalFunc:EqualFunction) (printValue  : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let allCons = exlcudeSizeConstraintIfFixedSize o.minSize.uper o.maxSize.uper o.AllCons
    let curProgramUnit = ToC t.id.ModName
    let anonymousVariables =
        allCons |> 
        List.map DastFold.getValueFromSizeableConstraint 
        |> List.collect id |> 
        List.choose (fun (v:Asn1AcnAst.OctetStringValue, (id,loc)) ->
                    let valKind = OctetStringValue (v |> List.map(fun z -> z.Value))
                    let recValue = {Asn1Value.kind = valKind; id=id;loc=loc}
                    match id with
                    | ReferenceToValue (typePath,(VA2 vasName)::[]) -> None
                    | ReferenceToValue(ts,vs)                       ->
                        let typeDefinitionName = typeDefinition.longTypedefName l
                        Some ({AnonymousVariable.valueName = (recValue.getBackendName l); valueExpresion = (printValue curProgramUnit None recValue.kind); typeDefinitionName = typeDefinitionName; valKind=valKind}))
    let compareSingValueFunc (p:CallerScope) (v:Asn1AcnAst.OctetStringValue, (id,loc)) =
        let recValue = {Asn1Value.kind = OctetStringValue (v |> List.map(fun z -> z.Value)); id=id;loc=loc}
        let vstr = {CallerScope.modName = id.ModName; arg = VALUE (recValue.getBackendName l)}
            //match p.arg.getAcces l with
            //| "->"  -> getAddres l (recValue.getBackendName l)
            //| _     -> (recValue.getBackendName l)
        match equalFunc.isEqualBody with
        | EqualBodyExpression eqFunc    ->
            match eqFunc p vstr with
            | None          -> raise(BugErrorException "unexpected case")
            | Some (ret,_)      -> ret
        | EqualBodyStatementList  _     -> raise(BugErrorException "unexpected case")
    let foldSizeCon (p:CallerScope) c = foldSizableConstraint l (fun p v -> compareSingValueFunc p  v) (fun l p -> l.Length p.arg.p (p.arg.getAcces l)) p c
    createBitOrOctetStringFunction r l t allCons foldSizeCon typeDefinition [] anonymousVariables us


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (equalFunc:EqualFunction) (printValue  : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let allCons = exlcudeSizeConstraintIfFixedSize o.minSize.uper o.maxSize.uper o.AllCons
    let curProgramUnit = ToC t.id.ModName
    let anonymousVariables =
        allCons |> 
        List.map DastFold.getValueFromSizeableConstraint 
        |> List.collect id |> 
        List.choose (fun (v:Asn1AcnAst.BitStringValue, (id,loc)) ->
                    let valKind = BitStringValue (v.Value)
                    let recValue = {Asn1Value.kind = valKind; id=id;loc=loc}
                    match id with
                    | ReferenceToValue (typePath,(VA2 vasName)::[]) -> None
                    | ReferenceToValue(ts,vs)                       ->
                        let typeDefinitionName = defOrRef.longTypedefName l
                        Some ({AnonymousVariable.valueName = (recValue.getBackendName l); valueExpresion = (printValue curProgramUnit None recValue.kind); typeDefinitionName = typeDefinitionName; valKind = valKind}))
    let compareSingValueFunc (p:CallerScope) (v:Asn1AcnAst.BitStringValue, (id,loc)) =
        let recValue = {Asn1Value.kind = BitStringValue (v.Value ); id=id;loc=loc}
        //let vstr = VALUE (recValue.getBackendName l)
        let vstr = {CallerScope.modName = id.ModName; arg = VALUE (recValue.getBackendName l)}
//        let vstr = 
//            match p.arg.getAcces l with
//            | "->"  -> getAddres l (recValue.getBackendName l)
//            | _     -> recValue.getBackendName l
        match equalFunc.isEqualBody with
        | EqualBodyExpression eqFunc    ->
            match eqFunc p vstr  with
            | None          -> raise(BugErrorException "unexpected case")
            | Some (ret,_)      -> ret
        | EqualBodyStatementList  _     -> raise(BugErrorException "unexpected case")
    let foldSizeCon p c = foldSizableConstraint l (fun p v -> compareSingValueFunc p  v) (fun l p -> l.Length p.arg.p (p.arg.getAcces l)) p c
    createBitOrOctetStringFunction r l t allCons foldSizeCon typeDefinition []  anonymousVariables us


(*  SEQUENCE *)

let isValidSequenceChild   (l:ProgrammingLanguage) (o:Asn1AcnAst.Asn1Child) (newChild:Asn1Type) (us:State)= 
    let JoinTwoIfFirstOk                  = match l with C    -> isvalid_c.JoinTwoIfFirstOk              |Ada   -> isvalid_a.JoinTwoIfFirstOk 
    let c_name = o.getBackendName l
    let sInnerStatement = 
        match newChild.isValidFunction with
        | Some (isValidFunction)    ->
            Some((fun (p:CallerScope)  ->
                    isValidFunction.funcBody ({p with arg = p.arg.getSeqChild l c_name newChild.isIA5String})), isValidFunction)
        | None      -> None
    let sInnerStatement =
        match sInnerStatement with
        | None                  -> None
        | Some (func, isValid)  ->
            match o.Optionality with
            | Some _    -> 
                match l with
                | C     -> 
                    let newFunc = (fun (p:CallerScope)  -> isvalid_c.Sequence_OptionalChild p.arg.p (p.arg.getAcces l) c_name (func p ))
                    Some (newFunc, isValid)
                | Ada   -> 
                    let newFunc = (fun (p:CallerScope) -> isvalid_a.Sequence_OptionalChild p.arg.p (p.arg.getAcces l) c_name (func p ))
                    Some (newFunc, isValid)
            | None      -> Some (func, isValid)
    let isAlwaysPresentStatement, finalState =
        let child_always_present_or_absent = 
            match l with 
            | C -> isvalid_c.Sequence_optional_child_always_present_or_absent 
            | Ada -> isvalid_a.Sequence_optional_child_always_present_or_absent
            
        match o.Optionality with
        | Some(Asn1AcnAst.AlwaysAbsent)                     -> 
            let errCodeName = ToC ("ERR_" + ((newChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm"))) + "_IS_PRESENT"
            let errCode, ns = getNextValidErrorCode us errCodeName
            let isValidStatement = (fun (p:CallerScope) -> child_always_present_or_absent p.arg.p (p.arg.getAcces l) c_name errCode.errCodeName "0")
            Some(isValidStatement, errCode), ns
        | Some(Asn1AcnAst.AlwaysPresent)                    -> 
            let errCodeName = ToC ("ERR_" + ((newChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm"))) + "_IS_ABSENT"
            let errCode, ns = getNextValidErrorCode us errCodeName
            let isValidStatement = (fun (p:CallerScope) -> child_always_present_or_absent p.arg.p (p.arg.getAcces l) c_name errCode.errCodeName "1")
            Some(isValidStatement, errCode), ns
        | _         -> None, us

    match sInnerStatement, isAlwaysPresentStatement with
    | None, None                                       -> None , finalState
    | None, Some(isValid, errCode)                     -> 
        Some({SeqChoiceChildInfoIsValid.isValidStatement = isValid; localVars = []; alphaFuncs = []; errCode = [errCode]}), finalState
    | Some(isValid, chFunc), None                      -> 
        Some({SeqChoiceChildInfoIsValid.isValidStatement = isValid; localVars = chFunc.localVariables; alphaFuncs = chFunc.alphaFuncs; errCode = chFunc.errCodes}), finalState
    | Some(isValid1, chFunc), Some(isValid2, errCode)    -> 
        let isValid = (fun (p:CallerScope) -> JoinTwoIfFirstOk (isValid2 p )  (isValid1 p )) 
        Some({SeqChoiceChildInfoIsValid.isValidStatement = isValid; localVars = chFunc.localVariables; alphaFuncs = chFunc.alphaFuncs; errCode = errCode::chFunc.errCodes}), finalState


let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (children:SeqChildInfo list)  (us:State)  =

    let funcName     = getFuncName r l t.id (t.FT_TypeDefintion.[l])
    let asn1Children = children |> List.choose(fun c -> match c with Asn1Child x -> Some x | AcnChild _ -> None)
    let body =
        let childrenConent, finalState =   
            asn1Children |>
            Asn1Fold.foldMap (fun errCode cc -> cc.isValidBodyStats errCode) us
        let childrenConent = childrenConent |> List.choose id

        match childrenConent with
        | []    -> None
        | x::xs ->
            let alphaFuncs = childrenConent |> List.collect(fun x -> x.alphaFuncs)
            let localVars = childrenConent |> List.collect(fun x -> x.localVars)
            let ercCodes = childrenConent |> List.collect(fun x -> x.errCode)
            let funcBody  (p:CallerScope) = 
                let printChild (content:string) (sNestedContent:string option) = 
                    match sNestedContent with
                    | None  -> content
                    | Some c-> 
                        match l with
                        | C        -> equal_c.JoinItems content sNestedContent
                        | Ada      -> isvalid_a.JoinItems content sNestedContent
                let rec printChildren children : string option = 
                    match children with
                    |[]     -> None
                    |x::xs  -> 
                        match printChildren xs with
                        | None                 -> Some (printChild x  None)
                        | Some childrenCont    -> Some (printChild x  (Some childrenCont))

                let isValidStatementX = x.isValidStatement p  
                let isValidStatementXS = xs |> List.map(fun x -> x.isValidStatement  p )
                printChild isValidStatementX (printChildren isValidStatementXS)
            Some(alphaFuncs, localVars, ercCodes, funcBody, finalState)
    match body with
    | None    -> None, us
    | Some(alphaFuncs, localVars, ercCodes, funcBody, finalState) ->
        let  func  = 
            let p  = t.getParamType l Encode
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                let exp = funcBody p 
                let lvars = localVars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                match l with
                |C     -> Some(isvalid_c.EmitTypeAssignment_composite funcName  (typeDefinition.longTypedefName l) exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Ada   -> Some(isvalid_a.EmitTypeAssignment_composite funcName  (typeDefinition.longTypedefName l) exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     -> 
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_c.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_c.EmitTypeAssignment_composite_def funcName  (typeDefinition.longTypedefName l) arrsErrcodes)
                    |Ada   -> 
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_a.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_a.EmitTypeAssignment_composite_def funcName  (typeDefinition.longTypedefName l) arrsErrcodes)
        
        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = ercCodes
                func                        = func
                funcDef                     = funcDef
                funcExp                     = None
                funcBody                    = funcBody
                //funcBody2                   = funcBody
                alphaFuncs                  = alphaFuncs
                localVariables              = localVars
                anonymousVariables          = 
                    let ret = asn1Children |> List.collect(fun c -> match c.Type.isValidFunction with Some vf -> vf.anonymousVariables | None -> [])
                    ret |> Seq.distinctBy(fun x -> x.valueName) |> Seq.toList
            }    
        Some ret, finalState

(*  CHOICE *)
let isValidChoiceChild   (l:ProgrammingLanguage) (o:Asn1AcnAst.ChChildInfo) (newChild:Asn1Type) (us:State)= 
    let c_name = ToC (o.getBackendName l)
    let sInnerStatement = 
        match newChild.isValidFunction with
        | Some (isValidFunction)    ->
             Some((fun (p:CallerScope) ->isValidFunction.funcBody ({p with arg = p.arg.getChChild l c_name newChild.isIA5String})), isValidFunction)
        | None      -> None
    

    match sInnerStatement with
    | None  -> None , us
    | Some(isValid, chFunc)                      -> 
        Some({SeqChoiceChildInfoIsValid.isValidStatement = isValid; localVars = chFunc.localVariables; alphaFuncs = chFunc.alphaFuncs; errCode = chFunc.errCodes}), us

let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (children:ChChildInfo list) (baseTypeValFunc : IsValidFunction option) (us:State)  =
    let funcName            = getFuncName r l t.id (t.FT_TypeDefintion.[l])
    let choice                  = match l with C    -> isvalid_c.choice              |Ada   -> isvalid_a.choice 
    let choice_child            = match l with C -> isvalid_c.choice_child           | Ada -> isvalid_a.choice_child
    let always_true_statement   = match l with C -> isvalid_c.always_true_statement  | Ada -> isvalid_a.always_true_statement
    let always_false_statement  = match l with C -> isvalid_c.always_false_statement | Ada -> isvalid_a.always_false_statement

    let body =
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
        let funcBody  (p:CallerScope) = 
            let childrenContent =
                childrenConent |> 
                List.map(fun (cc, vc) -> 
                    let chBody =  
                        match cc.Optionality with
                        | None  
                        | Some (Asn1AcnAst.ChoiceAlwaysPresent) ->
                            match vc with
                            | Some vc -> vc.isValidStatement p
                            | None    -> always_true_statement ()
                        | Some (Asn1AcnAst.ChoiceAlwaysAbsent)  ->
                            always_false_statement errCode.errCodeName
                    choice_child (cc.presentWhenName (Some defOrRef) l) chBody )

            choice p.arg.p (p.arg.getAcces l) childrenContent errCode.errCodeName
        Some(alphaFuncs, localVars, ercCodes, funcBody, finalState)
    match body with
    | None    -> None, us
    | Some(alphaFuncs, localVars, ercCodes, funcBody, finalState) ->
        let  func  = 
            let p = t.getParamType l Encode
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                let exp = funcBody p 
                let lvars = localVars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                match l with
                |C     -> Some(isvalid_c.EmitTypeAssignment_composite funcName  (typeDefinition.longTypedefName l) exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Ada   -> Some(isvalid_a.EmitTypeAssignment_composite funcName  (typeDefinition.longTypedefName l) exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     ->  
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_c.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_c.EmitTypeAssignment_composite_def funcName  (typeDefinition.longTypedefName l) arrsErrcodes)
                    |Ada   ->  
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_a.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_a.EmitTypeAssignment_composite_def funcName  (typeDefinition.longTypedefName l) arrsErrcodes)
        
        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = ercCodes
                func                        = func
                funcDef                     = funcDef
                funcExp                     = None
                funcBody                    = funcBody 
                //funcBody2                   = funcBody
                alphaFuncs                  = alphaFuncs
                localVariables              = localVars
                anonymousVariables          = 
                    let ret =children |> List.collect(fun c -> match c.chType.isValidFunction with Some vf -> vf.anonymousVariables | None -> []) 
                    ret |> Seq.distinctBy(fun x -> x.valueName) |> Seq.toList
            }    
        Some ret, finalState


let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference) (childType:Asn1Type) (baseTypeValFunc : IsValidFunction option) (us:State)  =
    let funcName            = getFuncName r l t.id (t.FT_TypeDefintion.[l])
    let bIsFixedSize = o.minSize.uper = o.maxSize.uper
    let hasValidationFunc = 
        match bIsFixedSize with
        | false     -> true
        | true      ->
            match childType.isValidFunction with
            | Some _  -> true
            | None    -> false

    //let baseCallStatement l p baseFncName =
    //    callBaseTypeFunc l (getAddres l p) baseFncName

    (*alphaFuncs, localVars, ercCodes, funcBody, deltaErrCode*)
    let body =
        let allSizeCons = o.AllCons |> List.filter(fun x -> match x with SizeContraint al-> true | _ -> false)
        let foldSizeCon  = foldSizableConstraint l (fun p v -> v.ToString()) (fun l p -> l.Length p.arg.p (p.arg.getAcces l))
        let sizeConstrData = 
            match bIsFixedSize with
            | true  -> None
            | false ->
                match allSizeCons with
                | []    -> None
                | _     ->
                    let errCodeName         = ToC ("ERR_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
                    let errCode, ns = getNextValidErrorCode us errCodeName
                    let sIsValidSizeExpFunc (p:CallerScope) =
                        let allCons = allSizeCons |> List.map (foldSizeCon  p )
                        l.ExpAndMulti allCons
                    Some(errCode, sIsValidSizeExpFunc, ns)
        let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
        let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
        match childType.isValidFunction, sizeConstrData with
        | None, None     -> None
        | Some cvf, None ->
            let funcBody (p:CallerScope)  = 
                //let childAccesPath = p + childAccess + l.ArrName + (l.ArrayAccess i) //"[" + i + "]"
                let innerStatement = Some(cvf.funcBody ({p with arg = p.arg.getArrayItem l i childType.isIA5String}) )
                match l with
                | C   -> isvalid_c.sequenceOf p.arg.p (p.arg.getAcces l) i bIsFixedSize ( o.minSize.uper) None None innerStatement
                | Ada -> isvalid_a.sequenceOf p.arg.p (p.arg.getAcces l) i bIsFixedSize ( o.minSize.uper) None None innerStatement
            Some(cvf.alphaFuncs, lv::cvf.localVariables , cvf.errCodes, funcBody, us)
        | None, Some(errCode, sIsValidSizeExpFunc, ns) ->
            let funcBody (p:CallerScope)  = 
                r.stg.is_valid.makeExpressionToStatement (sIsValidSizeExpFunc p ) errCode.errCodeName
            Some([],[], [errCode], funcBody, ns)
        | Some cvf, Some(errCode, sIsValidSizeExpFunc, ns) ->
            let funcBody (p:CallerScope)  = 
                //let childAccesPath = p + childAccess + l.ArrName + (l.ArrayAccess i) //"[" + i + "]"
                let innerStatement = Some(cvf.funcBody ({p with arg = p.arg.getArrayItem l i childType.isIA5String}))
                match l with
                | C   -> isvalid_c.sequenceOf p.arg.p (p.arg.getAcces l) i bIsFixedSize ( o.minSize.uper) (Some (sIsValidSizeExpFunc p )) (Some errCode.errCodeName) innerStatement
                | Ada -> isvalid_a.sequenceOf p.arg.p (p.arg.getAcces l) i bIsFixedSize ( o.minSize.uper) (Some (sIsValidSizeExpFunc p )) (Some errCode.errCodeName) innerStatement
            Some(cvf.alphaFuncs, lv::cvf.localVariables , cvf.errCodes@[errCode], funcBody, ns)


    match body with
    | None -> None, us
    | Some(alphaFuncs, localVars, ercCodes, funcBody, newState) ->
        let  func  = 
            let p  = t.getParamType l Encode
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                let exp = funcBody p 
                let lvars = localVars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                match l with
                |C     -> Some(isvalid_c.EmitTypeAssignment_composite funcName  (typeDefinition.longTypedefName l) exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
                |Ada   -> Some(isvalid_a.EmitTypeAssignment_composite funcName  (typeDefinition.longTypedefName l) exp (alphaFuncs |> List.map(fun x -> x.funcBody)) lvars)
        let  funcDef  = 
                match funcName with
                | None              -> None
                | Some funcName     -> 
                    match l with
                    |C     ->  
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_c.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_c.EmitTypeAssignment_composite_def funcName  (typeDefinition.longTypedefName l) arrsErrcodes)
                    |Ada   ->  
                        let arrsErrcodes = ercCodes |> List.map(fun s -> isvalid_a.EmitTypeAssignment_composite_def_err_code s.errCodeName (BigInteger s.errCodeValue))
                        Some(isvalid_a.EmitTypeAssignment_composite_def funcName  (typeDefinition.longTypedefName l) arrsErrcodes)

        
        let ret = 
            {
                IsValidFunction.funcName    = funcName
                errCodes                    = ercCodes
                func                        = func
                funcDef                     = funcDef
                funcExp                     = None
                funcBody                    = funcBody
                //funcBody2                   = funcBody
                alphaFuncs                  = alphaFuncs
                localVariables              = localVars
                anonymousVariables          = 
                    match childType.isValidFunction with
                    | Some v  -> v.anonymousVariables
                    | None    -> []
            }    
        Some ret, newState


let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (baseType:Asn1Type)  (us:State)  =
    baseType.isValidFunction, us    
