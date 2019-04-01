module DastValidate2

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


let ValidationBlockAsStringExpr = function
    | VCBTrue        -> "true"
    | VCBFalse       -> "false"
    | VCBExpression sExp -> sExp
    | VCBStatement sStat -> sStat

//type ObjectIdConstraintOrBasicValidation =
//    | ObjectIdConstraint_basic
//    | ObjectIdConstraint_const of ObjectIdConstraint

let ValidationCodeBlock_OR (l:ProgrammingLanguage) vp1 vp2 =
    let expressionOrStament  = match l with C -> isvalid_c.ExpressionOrStament | Ada -> isvalid_a.ExpressionOrStament
    let statementOrExpression  = match l with C -> isvalid_c.StatementOrExpression | Ada -> isvalid_a.StatementOrExpression
    let statementOrStament  = match l with C -> isvalid_c.StatementOrStament | Ada -> isvalid_a.StatementOrStament
    match vp1, vp2 with
    | VCBTrue,   _                                       -> VCBTrue
    | _,VCBTrue                                          -> VCBTrue
    | VCBFalse, _                                        -> vp2
    | _, VCBFalse                                        -> vp1
    | VCBExpression sExp1, VCBExpression sExp2    -> VCBExpression (l.ExpOr sExp1 sExp2)
    | VCBExpression sExp1, VCBStatement sStat2    -> VCBStatement(expressionOrStament sExp1 sStat2)
    | VCBStatement sStat1, VCBExpression sExp2    -> VCBStatement(statementOrExpression sStat1 sExp2)
    | VCBStatement sStat1, VCBStatement sStat2    -> VCBStatement(statementOrStament sStat1 sStat2)

let ValidationCodeBlock_AND (l:ProgrammingLanguage) vp1 vp2 =
    let expressionAndStament  = match l with C -> isvalid_c.ExpressionAndStament | Ada -> isvalid_a.ExpressionAndStament
    let statementAndExpression  = match l with C -> isvalid_c.StatementAndExpression | Ada -> isvalid_a.StatementAndExpression
    let statementAndStament  = match l with C -> isvalid_c.StatementAndStament | Ada -> isvalid_a.StatementAndStament
    match vp1, vp2 with
    | VCBTrue,   _                                       -> vp2
    | _,VCBTrue                                          -> vp1
    | VCBFalse, _                                        -> VCBFalse
    | _, VCBFalse                                        -> VCBFalse
    | VCBExpression sExp1, VCBExpression sExp2    -> VCBExpression (l.ExpAnd sExp1 sExp2)
    | VCBExpression sExp1, VCBStatement sStat2    -> VCBStatement(expressionAndStament sExp1 sStat2)
    | VCBStatement sStat1, VCBExpression sExp2    -> VCBStatement(statementAndExpression sStat1 sExp2)
    | VCBStatement sStat1, VCBStatement sStat2    -> VCBStatement(statementAndStament sStat1 sStat2)

let ValidationCodeBlock_Multiple_And (l:ProgrammingLanguage) (vpList:ValidationCodeBlock list)  =
    let makeExpressionToStatement0  = match l with C -> isvalid_c.makeExpressionToStatement0 | Ada -> isvalid_a.makeExpressionToStatement0

    match vpList |> Seq.exists((=) VCBFalse) with
    | true  -> VCBFalse
    | false ->
        let vpList = vpList |> List.filter (fun z -> match z with VCBExpression _ -> true | VCBStatement _ -> true | _ -> false )
        let bAllAreExpressions = false//vpList |> Seq.forall(fun z -> match z with VCBExpression _ -> true | _ -> false )
        match bAllAreExpressions with
        | true  -> VCBExpression (l.ExpAndMulti (vpList |> List.map(fun z -> match z with VCBExpression s -> s | VCBStatement s -> s | _ -> "invalid")))
        | false -> 
            let soJoinedStatement = 
                vpList |>
                List.map(fun z -> match z with VCBExpression s -> makeExpressionToStatement0 s | VCBStatement s -> s | _ -> "invalid") |>
                DAstUtilFunctions.nestItems l  "ret"
            match soJoinedStatement with
            | Some s    -> VCBStatement s
            | None      -> VCBTrue
let ValidationCodeBlock_Not (l:ProgrammingLanguage) vp =
    let statementNot  = match l with C -> isvalid_c.StatementNot | Ada -> isvalid_a.StatementNot
    match vp with
    | VCBTrue        -> VCBFalse
    | VCBFalse       -> VCBTrue
    | VCBExpression sExp -> VCBExpression (l.ExpNot sExp)
    | VCBStatement sStat -> VCBStatement (statementNot sStat)


let convertVCBToStatementAndAssigneErrCode (l:ProgrammingLanguage) vp  sErrCode =
    let convertVCBExpressionToStatementAndUpdateErrCode  = match l with C -> isvalid_c.convertVCBExpressionToStatementAndUpdateErrCode | Ada -> isvalid_a.convertVCBExpressionToStatementAndUpdateErrCode
    let convertVCBStatementToStatementAndUpdateErrCode  = match l with C -> isvalid_c.convertVCBStatementToStatementAndUpdateErrCode | Ada -> isvalid_a.convertVCBStatementToStatementAndUpdateErrCode
    let convertVCBTRUEToStatementAndUpdateErrCode  = match l with C -> isvalid_c.convertVCBTRUEToStatementAndUpdateErrCode | Ada -> isvalid_a.convertVCBTRUEToStatementAndUpdateErrCode
    let convertVCBFalseToStatementAndUpdateErrCode  = match l with C -> isvalid_c.convertVCBFalseToStatementAndUpdateErrCode | Ada -> isvalid_a.convertVCBFalseToStatementAndUpdateErrCode
    match vp with
    | VCBTrue        -> convertVCBTRUEToStatementAndUpdateErrCode ()
    | VCBFalse       -> convertVCBFalseToStatementAndUpdateErrCode sErrCode
    | VCBExpression sExp -> convertVCBExpressionToStatementAndUpdateErrCode sExp sErrCode
    | VCBStatement sStat -> convertVCBStatementToStatementAndUpdateErrCode sStat sErrCode


let ValidationCodeBlock_Except (l:ProgrammingLanguage) vp1 vp2 =
    // vp1 and (not vp2)
    let expressionExceptStament  = match l with C -> isvalid_c.ExpressionExceptStament | Ada -> isvalid_a.ExpressionExceptStament
    let statementExceptExpression  = match l with C -> isvalid_c.StatementExceptExpression | Ada -> isvalid_a.StatementExceptExpression
    let statementExceptStament  = match l with C -> isvalid_c.StatementExceptStament | Ada -> isvalid_a.StatementExceptStament
    match vp1, vp2 with
    | _, VCBTrue                                         -> VCBFalse
    | VCBTrue, _                                         -> ValidationCodeBlock_Not l vp2
    | VCBFalse, _                                        -> VCBFalse
    | _, VCBFalse                                        -> vp1
    
    | VCBExpression sExp1, VCBExpression sExp2    -> VCBExpression (l.ExpAnd sExp1 (l.ExpNot sExp2))
    | VCBExpression sExp1, VCBStatement sStat2    -> VCBStatement(expressionExceptStament sExp1 sStat2)
    | VCBStatement sStat1, VCBExpression sExp2    -> VCBStatement(statementExceptExpression sStat1 sExp2)
    | VCBStatement sStat1, VCBStatement sStat2    -> VCBStatement(statementExceptStament sStat1 sStat2)


let Lte (l:ProgrammingLanguage) eqIsInc  e1 e2 =
    match eqIsInc with
    | true   -> l.ExpLte e1 e2        
    | false  -> l.ExpLt  e1 e2




let foldGenericCon (l:ProgrammingLanguage) valToStrFunc  (c:GenericConstraint<'v>)  st =
    foldGenericConstraint
        (fun e1 e2 b s      -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
        (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (e2 p)), s)
        (fun e s            -> (fun p -> l.ExpNot (e p)), s)
        (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (l.ExpNot (e2 p))), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
        (fun v  s         -> (fun p -> l.ExpEqual (p.arg.getValue l) (valToStrFunc v)) ,s)
        c
        st

let foldRangeCon (l:ProgrammingLanguage) valToStrFunc   (c:RangeTypeConstraint<'v1,'v1>)  st =
    foldRangeTypeConstraint        
        (fun e1 e2 b s      -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
        (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (e2 p)), s)
        (fun e s            -> (fun p -> l.ExpNot (e p)), s)
        (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (l.ExpNot (e2 p))), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
        (fun v  s         -> (fun p -> l.ExpEqual (p.arg.getValue l) (valToStrFunc  v)) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            (fun p -> l.ExpAnd (Lte l minIsIn (valToStrFunc v1) (p.arg.getValue l)) (Lte l maxIsIn (p.arg.getValue l) (valToStrFunc v2))), s)
        (fun v1 minIsIn s   -> (fun p -> Lte l minIsIn (valToStrFunc v1) (p.arg.getValue l)), s)
        (fun v2 maxIsIn s   -> (fun p -> Lte l maxIsIn (p.arg.getValue l) (valToStrFunc v2)), s)
        c
        st



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


let foldSizableConstraint (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) hasCount compareSingValueFunc  getSizeFunc  (c:SizableTypeConstraint<'v>) st =
    foldSizableTypeConstraint2
        (fun e1 e2 b s      -> (fun p -> ValidationCodeBlock_OR l (e1 p) (e2 p) ), s)
        (fun e1 e2 s        -> (fun p -> ValidationCodeBlock_AND l (e1 p) (e2 p)), s)
        (fun e s            -> (fun p -> ValidationCodeBlock_Not l (e p)), s)
        (fun e1 e2 s        -> (fun p -> ValidationCodeBlock_AND l (e1 p) (ValidationCodeBlock_Not l (e2 p))), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> (fun p -> ValidationCodeBlock_OR l (e1 p) (e2 p)), s)
        (fun v  s           -> (fun p -> VCBExpression (compareSingValueFunc p v)) ,s)
        (fun intCon s       -> 
            let fnc p = 
                match hasCount with
                | true  ->
                    let aaa = [intCon] |> List.map (fun c -> simplifytIntegerTypeConstraint (UintHandleEqual r 0u) (UintHandleRangeContraint_val_MAX r 0u) (UintHandleRangeContraint_MIN_val r 0u UInt32.MaxValue) c) |> List.choose (fun sc -> match sc with SicAlwaysTrue -> None | SciConstraint c -> Some c)
                    let bbb = aaa |> List.map (fun intCon -> foldSizeRangeTypeConstraint l getSizeFunc p intCon |> fst)
                    VCBExpression (l.ExpAndMulti bbb)
                | false -> VCBTrue
            fnc, s)
        c
        st

let foldStringCon  (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType)   (c:IA5StringConstraint) (us0:State) =
    let alphaFuncName = ToC (((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_CharsAreValid")
    let stringContainsChar = match l with C -> isvalid_c.stringContainsChar | Ada -> isvalid_a.stringContainsChar
    let foldRangeCharCon (l:ProgrammingLanguage)   (c:CharTypeConstraint)  st =
        let valToStrFunc1 v = v.ToString().ISQ
        foldRangeTypeConstraint        
            (fun e1 e2 b s      -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
            (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (e2 p)), s)
            (fun e s            -> (fun p -> l.ExpNot (e p)), s)
            (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (l.ExpNot (e2 p))), s)
            (fun e s            -> e, s)
            (fun e1 e2 s        -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
            (fun (v:string)  s  -> (fun p -> stringContainsChar v.IDQ p.arg.p) ,s)
            (fun v1 v2  minIsIn maxIsIn s   -> 
                (fun p -> l.ExpAnd (Lte l minIsIn (valToStrFunc1 v1) (p.arg.getValue l)) (Lte l maxIsIn (p.arg.getValue l) (valToStrFunc1 v2))), s)
            (fun v1 minIsIn s   -> (fun p -> Lte l minIsIn (valToStrFunc1 v1) (p.arg.getValue l)), s)
            (fun v2 maxIsIn s   -> (fun p -> Lte l maxIsIn (p.arg.getValue l) (valToStrFunc1 v2)), s)
            c
            st |> fst

    let retExpFnc, ns = 
        foldStringTypeConstraint2
            (fun e1 e2 b s      -> (fun p -> l.ExpOr (e1 p) (e2 p) ), s)
            (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (e2 p)), s)
            (fun e s            -> (fun p -> l.ExpNot (e p)), s)
            (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (l.ExpNot (e2 p))), s)
            (fun e s            -> e, s)
            (fun e1 e2 s        -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
            (fun v  s         -> (fun p -> l.ExpStringEqual p.arg.p v.IDQ)  ,s)
            (fun intCon s       -> 
                let fnc p = 
                    let aaa = [intCon] |> List.map (fun c -> simplifytIntegerTypeConstraint (UintHandleEqual r 0u) (UintHandleRangeContraint_val_MAX r 0u) (UintHandleRangeContraint_MIN_val r 0u UInt32.MaxValue) c) |> List.choose (fun sc -> match sc with SicAlwaysTrue -> None | SciConstraint c -> Some c)
                    let bbb = aaa |> List.map (fun intCon -> foldSizeRangeTypeConstraint l (fun l p -> l.StrLen p.arg.p) p intCon |> fst)
                    l.ExpAndMulti bbb
                fnc, s)
            (fun alphcon s      -> 
                let foldAlpha c = foldRangeCharCon l   c 0 
                let alphaBody p = foldAlpha (*({CallerScope.modName = p.modName; arg = VALUE (sprintf "str%s" (l.ArrayAccess "i"))})*) alphcon p
                let alphaFuncName = sprintf "%s_%d" alphaFuncName s.alphaIndex
                let funcBody p =
                    match l with
                    | C    -> isvalid_c.Print_AlphabetCheckFunc alphaFuncName [alphaBody p]
                    | Ada  -> isvalid_a.Print_AlphabetCheckFunc alphaFuncName [alphaBody p]
                let alphFunc = {AlphaFunc.funcName = alphaFuncName; funcBody = funcBody }

                let newState = {s with alphaIndex = s.alphaIndex + 1; alphaFuncs = alphFunc::s.alphaFuncs}
                let retFnc p = sprintf "%s(%s)" alphaFuncName p.arg.p
                retFnc, newState) 
            c
            us0 
    (fun p -> VCBExpression (retExpFnc p)), ns

(*
let rec foldAnyCon (l:ProgrammingLanguage) (p:CallerScope)  (ac:AnyConstraint) st =
    match ac with
    | IntegerTypeConstraint c -> foldIntegerCon l isUnsigned p c st
    | RealTypeConstraint    c -> foldRealCon l p c st
    | OctetStringConstraint c -> None, st
    | BitStringConstraint   c -> None, st
    | BoolConstraint        c -> None, st
    | EnumConstraint        c -> None, st
    | ObjectIdConstraint    c -> None, st
    | SequenceOfConstraint  c -> None, st
    | SeqConstraint         c -> None, st
    | ChoiceConstraint      c -> None, st
    | NullConstraint          -> None, st
    | IA5StringConstraint   c -> None, st

    *)



let integerConstraint2ValidationCodeBlock (l:ProgrammingLanguage) isUnsigned  (c:IntegerTypeConstraint) st =
    let valToString  (i:BigInteger) = 
        match l with
        | Ada   -> i.ToString()
        | C     ->
            match isUnsigned with
            | true   -> sprintf "%sUL" (i.ToString())
            | false  -> sprintf "%sLL" (i.ToString())

    let fnc,ns = foldRangeCon l valToString  c st
    (fun p -> VCBExpression (fnc p)), ns

let realConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (c:RealTypeConstraint) st =
    let valToString (v:double) = v.ToString("E20", NumberFormatInfo.InvariantInfo)
    let fnc, ns = foldRangeCon l valToString c st
    (fun p -> VCBExpression (fnc p)), ns

let booleanConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (c:BoolConstraint) st =
    let fnc, ns = foldGenericCon l  (fun v -> v.ToString().ToLower()) c st
    (fun p -> VCBExpression (fnc p)), ns

let objIdConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (c:ObjectIdConstraint) st =
    let objId_equal = match l with C -> isvalid_c.objId_equal | Ada -> isvalid_a.objId_equal
    let printObjectIdentifierValue = match l with C -> variables_c.PrintObjectIdentifierValueAsCompoundLiteral | Ada -> variables_a.PrintObjectIdentifierValueAsCompoundLiteral
    let fnc, ns= 
        foldGenericConstraint
            (fun e1 e2 b s      -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
            (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (e2 p)), s)
            (fun e s            -> (fun p -> l.ExpNot (e p)), s)
            (fun e1 e2 s        -> (fun p -> l.ExpAnd (e1 p) (l.ExpNot (e2 p))), s)
            (fun e s            -> e, s)
            (fun e1 e2 s        -> (fun p -> l.ExpOr (e1 p) (e2 p)), s)
            (fun (a,b)  s           -> 
                let v =  Asn1DefinedObjectIdentifierValue(a,b)
                (fun (p:CallerScope) -> 
                    let lit = printObjectIdentifierValue (v.Values |> List.map fst) (BigInteger v.Values.Length)
                    objId_equal p.arg.p lit) ,s)
            c
            st
    (fun p -> VCBExpression (fnc p)), ns

let enumeratedConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference) (c:EnumConstraint) st =
    let printNamedItem  (v:string) =
        let itm = o.items |> Seq.find (fun x -> x.Name.Value = v)
        let ret = itm.getBackendName (Some typeDefinition ) l
        ret
    let fnc, ns = foldGenericCon l  printNamedItem c st
    (fun p -> VCBExpression (fnc p)), ns

let octetStringConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (o:Asn1AcnAst.OctetString) (equalFunc:EqualFunction) (c:OctetStringConstraint) st =
    let getSizeFunc (l:ProgrammingLanguage) p = l.Length p.arg.p (p.arg.getAcces l)
    let compareSingleValueFunc (p:CallerScope) (v:Asn1AcnAst.OctetStringValue, (id,loc))  = 
        let octet_var_string_equal = match l with C -> isvalid_c.octet_var_string_equal | Ada -> isvalid_a.octet_var_string_equal
        let octet_fix_string_equal = match l with C -> isvalid_c.octet_fix_string_equal | Ada -> isvalid_a.octet_fix_string_equal
        let printOctetArrayAsCompoundLitteral = match l with C -> variables_c.PrintOctetArrayAsCompoundLitteral | Ada -> variables_a.PrintOctetArrayAsCompoundLitteral
        let octArrLiteral = printOctetArrayAsCompoundLitteral  (v |> List.map (fun b -> b.Value))
        match o.isFixedSize with
        | true   -> octet_fix_string_equal p.arg.p (p.arg.getAcces l) o.minSize.uper (v.Length.AsBigInt) octArrLiteral
        | false  -> octet_var_string_equal p.arg.p (p.arg.getAcces l)  (v.Length.AsBigInt) octArrLiteral
    let fnc, ns = foldSizableConstraint r l (not o.isFixedSize) compareSingleValueFunc getSizeFunc c st
    fnc, ns

let bitStringConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (o:Asn1AcnAst.BitString) (equalFunc:EqualFunction) (c:BitStringConstraint) st =
    let getSizeFunc (l:ProgrammingLanguage) p = l.Length p.arg.p (p.arg.getAcces l)
    let compareSingleValueFunc (p:CallerScope) (v:Asn1AcnAst.BitStringValue, (id,loc))  = 
        let bit_var_string_equal = match l with C -> isvalid_c.bit_var_string_equal | Ada -> isvalid_a.bit_var_string_equal
        let bit_fix_string_equal = match l with C -> isvalid_c.bit_fix_string_equal | Ada -> isvalid_a.bit_fix_string_equal
        let printOctetArrayAsCompoundLitteral = match l with C -> variables_c.PrintOctetArrayAsCompoundLitteral | Ada -> variables_a.PrintOctetArrayAsCompoundLitteral
        let bytes = bitStringValueToByteArray (StringLoc.ByValue v.Value)
        let octArrLiteral = printOctetArrayAsCompoundLitteral  bytes 
        let bitArrLiteral = variables_a.PrintBitArrayAsCompoundLitteral  (v.Value.ToCharArray() |> Seq.map(fun c -> if c = '0' then 0uy else 1uy)) 
        match o.isFixedSize with
        | true   -> bit_fix_string_equal p.arg.p (p.arg.getAcces l) o.minSize.uper (v.Value.Length.AsBigInt) octArrLiteral bitArrLiteral
        | false  -> bit_var_string_equal p.arg.p (p.arg.getAcces l)  (v.Value.Length.AsBigInt) octArrLiteral bitArrLiteral
    let fnc, ns = foldSizableConstraint r l (not o.isFixedSize) compareSingleValueFunc getSizeFunc c st
    fnc, ns

let rec sequenceConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (children:Asn1Child list) valToStrFunc    (c:SeqConstraint)  st =
    foldSeqConstraint
        (fun (e1 : CallerScope -> ValidationCodeBlock) e2 b s      ->(fun p -> ValidationCodeBlock_OR l (e1 p) (e2 p)), s)
        (fun e1 e2 s        ->(fun p -> ValidationCodeBlock_AND l (e1 p) (e2 p)), s)
        (fun e s            ->(fun p -> ValidationCodeBlock_Not l (e p)), s)
        (fun e1 e2 s        ->(fun p -> ValidationCodeBlock_Except l (e1 p) (e2 p)), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> (fun p -> ValidationCodeBlock_OR l (e1 p) (e2 p)), s)
        (fun v  s           -> 
            //use compound literals
            (fun p -> VCBTrue) ,s)      //currently single value constraints are not supported.
        (fun ncs  s         -> 

            let withComponentItems, ns = 
                ncs |> 
                Asn1Fold.foldMap(fun curState nc -> 
                    let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)
                    let chp p = {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String}
                    match (ch.Type.ActualType).Kind, nc.Contraint with
                    | Integer o, (Some (IntegerTypeConstraint c))   -> 
                        let fnc, ns = integerConstraint2ValidationCodeBlock l o.baseInfo.isUnsigned c curState
                        (fun p -> fnc (chp p) ), ns
                    | Real o, (Some (RealTypeConstraint    c))      -> 
                        let fnc, ns = realConstraint2ValidationCodeBlock l c st
                        (fun p -> fnc (chp p) ), ns
                    | IA5String  o, (Some (IA5StringConstraint c))  -> 
                        let alphafuncName = ToC (((ch.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_CharsAreValid")
                        let fnc, ns = foldStringCon r l ch.Type.id  c curState
                        (fun p -> fnc(chp p)) , ns
                    | OctetString o, (Some (OctetStringConstraint c))    -> 
                        let fnc, ns = octetStringConstraint2ValidationCodeBlock r l  ch.Type.id o.baseInfo  o.equalFunction c curState
                        (fun p -> fnc (chp p) ), ns
                    | BitString o, (Some (BitStringConstraint c))        -> 
                        let fnc, ns = bitStringConstraint2ValidationCodeBlock r l  ch.Type.id o.baseInfo o.equalFunction c curState
                        (fun p -> fnc (chp p) ), ns
                    | NullType o, (Some NullConstraint)                  -> (fun p -> VCBTrue), curState
                    | Boolean o, (Some (BoolConstraint c))               -> 
                        let fnc, ns = booleanConstraint2ValidationCodeBlock l c curState
                        (fun p -> fnc (chp p) ), ns
                    | Enumerated o, (Some (EnumConstraint c))            -> 
                        let fnc, ns = enumeratedConstraint2ValidationCodeBlock  l  o.baseInfo o.definitionOrRef c curState
                        (fun p -> VCBTrue), ns
                    | ObjectIdentifier o, (Some (ObjectIdConstraint c))  -> 
                        let fnc, ns = objIdConstraint2ValidationCodeBlock l c curState
                        (fun p -> VCBTrue), ns
                    | Sequence o, (Some (SeqConstraint c))               -> 
                        let fnc, ns = sequenceConstraint2ValidationCodeBlock r l (typeId.getSeqChildId ch.Name.Value) o.Asn1Children valToStrFunc  c curState
                        (fun p -> fnc (chp p) ), ns
                    | SequenceOf o, (Some (SequenceOfConstraint c))      -> (fun p -> VCBTrue), curState
                    | Choice o, (Some (ChoiceConstraint c))              -> (fun p -> VCBTrue), curState
                    | _                                               -> raise(SemanticError(nc.Name.Location, "Invalid combination of type/constraint type"))
                ) s 
            let fnc p = 
                ValidationCodeBlock_Multiple_And l (withComponentItems |> List.map (fun fnc -> fnc p))
            fnc, ns)
        c
        st


(*
let rec foldSequenceConstraint (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (children:Asn1Child list) valToStrFunc  (p:CallerScope)  (c:SeqConstraint)  st =
    foldSeqConstraint
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s           -> l.ExpEqual (p.arg.getValue l) (valToStrFunc p v) ,s)
        (fun ncs  s         -> 
            let aaa = 
                ncs |> 
                Asn1Fold.foldMap(fun curState nc -> 
                    let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)
                    let chp = {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String}
                    match (ch.Type.ActualType).Kind, nc.Contraint with
                    | Integer o, (Some (IntegerTypeConstraint c))   -> foldIntegerCon l o.baseInfo.isUnsigned chp c curState
                    | Real o, (Some (RealTypeConstraint    c))      -> foldRealCon l chp c st
                    | IA5String  o, (Some (IA5StringConstraint c))  -> 
                        let alphafuncName = ToC (((ch.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_CharsAreValid")
                        let ret, ns = foldStringCon r l alphafuncName chp  c curState
                        Some ret, ns
                    | OctetString o, (Some (OctetStringConstraint c))    -> 
                        let compareSingValueFunc (p:CallerScope) (v:Asn1AcnAst.OctetStringValue, (id,loc)) =
                            let recValue = {Asn1Value.kind = OctetStringValue (v |> List.map(fun z -> z.Value)); id=id;loc=loc}
                            let vstr = {CallerScope.modName = id.ModName; arg = VALUE (recValue.getBackendName l)}
                            match o.equalFunction.isEqualBody with
                            | EqualBodyExpression eqFunc    ->
                                match eqFunc p vstr with
                                | None          -> raise(BugErrorException "unexpected case")
                                | Some (ret,_)      -> ret
                            | EqualBodyStatementList  _     -> raise(BugErrorException "unexpected case")
                        let ret = foldSizableConstraint l (fun p v -> compareSingValueFunc p  v) (fun l p -> l.Length p.arg.p (p.arg.getAcces l)) chp c 
                        Some ret, curState
                    | BitString o, (Some (BitStringConstraint c))        -> None, curState
                    | NullType o, (Some NullConstraint)                  -> None, curState
                    | Boolean o, (Some (BoolConstraint c))               -> 
                        let ret, ns = foldGenericCon l (fun p v -> v.ToString().ToLower()) chp c curState
                        Some ret, ns
                    | Enumerated o, (Some (EnumConstraint c))            -> 
                        let printNamedItem (p:CallerScope) (v:string) =
                            let itm = o.baseInfo.items |> Seq.find (fun x -> x.Name.Value = v)
                            let ret = itm.getBackendName (Some o.definitionOrRef) l
                            ret
                        let ret, ns = foldGenericCon l  printNamedItem chp c curState 
                        None, curState
                    | ObjectIdentifier o, (Some (ObjectIdConstraint c))  -> 
                        None, curState
                    | Sequence o, (Some (SeqConstraint c))               -> 
                        let ret, ns = foldSequenceConstraint r l o.Asn1Children valToStrFunc chp c curState
                        Some ret, ns
                    | SequenceOf o, (Some (SequenceOfConstraint c))      -> None, curState
                    | Choice o, (Some (ChoiceConstraint c))              -> None, curState
                    | _                                               -> raise(SemanticError(nc.Name.Location, "Invalid combination of type/constraint type"))
                ) s

            "", s)
        c
        st
*)

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



let hasValidationFunc allCons =
    match allCons with
    | []      -> false
    | _       -> true

let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (td:FE_TypeDefinition) =
    match typeId.tasInfo with
    | None -> None
    | Some _ -> Some (td.typeName + "_IsConstraintValid")


let str_p (l:ProgrammingLanguage) (typeid:ReferenceToType) = ({CallerScope.modName = typeid.ModName; arg = VALUE (sprintf "str%s" (l.ArrayAccess "i"))})

type IsValidAux = {
    isValidStatement  : CallerScope -> string
    localVars         : LocalVariable list
    alphaFuncs        : AlphaFunc list
    childErrCodes     : ErroCode list
}


let createIsValidFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type)  (fncBodyE : ErroCode->CallerScope->string) (typeDefinition:TypeDefintionOrReference) (alphaFuncs : AlphaFunc list) (localVars : LocalVariable list) (childErrCodes : ErroCode list) (us:State)  =
    //let hasValidationFunc= hasValidationFunc allCons
    let emitTasFnc    = match l with C -> isvalid_c.EmitTypeAssignment_composite                | Ada -> isvalid_a.EmitTypeAssignment_composite
    let emitTasFncDef = match l with C -> isvalid_c.EmitTypeAssignment_composite_def            | Ada -> isvalid_a.EmitTypeAssignment_composite_def
    let defErrCode    = match l with C -> isvalid_c.EmitTypeAssignment_composite_def_err_code   | Ada -> isvalid_a.EmitTypeAssignment_composite_def_err_code

    let funcName            = getFuncName r l t.id (t.FT_TypeDefintion.[l])
    let errCodeName         = ToC ("ERR_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let funcBody = fncBodyE errCode
    let errCodes = errCode::childErrCodes
    let p  = t.getParamType l Encode
    let varName = p.arg.p
    let sStar = p.arg.getStar l
    let  func, funcDef  = 
            match funcName  with
            | None              -> None, None
            | Some funcName     -> 
                let exp = funcBody p  
                let lvars = localVars |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                let fnc = emitTasFnc varName sStar funcName  (typeDefinition.longTypedefName l) exp  (alphaFuncs |> List.map(fun x -> x.funcBody (str_p l t.id))) lvars
                let arrsErrcodes = errCodes |> List.map(fun s -> defErrCode s.errCodeName (BigInteger s.errCodeValue))
                let fncH = emitTasFncDef varName sStar funcName  (typeDefinition.longTypedefName l) arrsErrcodes
                Some fnc, Some fncH
    let ret = 
        {
            IsValidFunction.funcName    = funcName
            errCodes                    = errCodes
            func                        = func
            funcDef                     = funcDef
            funcBody                    = funcBody 
            alphaFuncs                  = alphaFuncs
            localVariables              = localVars
            anonymousVariables          = []
        }    
    Some ret, ns

let funcBody l fncs (e:ErroCode) (p:CallerScope) = 
    let combinedVcb = fncs |> List.map (fun fnc -> fnc p) |> (ValidationCodeBlock_Multiple_And l)
    convertVCBToStatementAndAssigneErrCode l combinedVcb e.errCodeName

let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let allCons = getIntSimplifiedConstraints r o.isUnsigned o.AllCons
    let fncs, ns = allCons |> Asn1Fold.foldMap (fun us c -> integerConstraint2ValidationCodeBlock l o.isUnsigned c us) us

    createIsValidFunction r l t  (funcBody l fncs) typeDefinition [] [] [] ns

let createIntegerFunctionByCons (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) isUnsigned (allCons  : IntegerTypeConstraint list) =
    let allCons = getIntSimplifiedConstraints r isUnsigned allCons
    let fncs, ns = allCons |> Asn1Fold.foldMap (fun us c -> integerConstraint2ValidationCodeBlock l isUnsigned c us) 0
    match allCons with
    | []        -> None
    | _         ->
        let funcExp (p:CallerScope) = 
            let vp = fncs |> List.map (fun fnc -> fnc p) |> (ValidationCodeBlock_Multiple_And l)        
            match vp with
            | VCBTrue        -> "true"
            | VCBFalse       -> "false"
            | VCBExpression sExp -> sExp
            | VCBStatement sStat -> sStat
        Some funcExp

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference)  (us:State)  =
    let fncs, ns = o.AllCons |> Asn1Fold.foldMap (fun us c -> realConstraint2ValidationCodeBlock l c us) us
    createIsValidFunction r l t (funcBody l fncs) typeDefinition [] [] [] us

let createBoolFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let fncs, ns = o.AllCons |> Asn1Fold.foldMap (fun us c -> booleanConstraint2ValidationCodeBlock l c us) us
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] us

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference) (equalFunc:EqualFunction) (printValue  : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let fncs, ns = o.AllCons |> Asn1Fold.foldMap (fun us c -> octetStringConstraint2ValidationCodeBlock r l  t.id o equalFunc c us) us
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] ns

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (equalFunc:EqualFunction) (printValue  : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let fncs, ns = o.AllCons |> Asn1Fold.foldMap (fun us c -> bitStringConstraint2ValidationCodeBlock r l  t.id o equalFunc c us) us
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] ns

let createStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let fncs, ns = o.AllCons |> Asn1Fold.foldMap (fun us c -> foldStringCon r  l t.id c us) {us with alphaIndex=0; alphaFuncs=[]}
    createIsValidFunction r l t (funcBody l fncs) typeDefinition ns.alphaFuncs [] [] ns

let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let conToStrFunc_basic (p:CallerScope)  = 
        match o.relativeObjectId with
        | false -> VCBExpression (sprintf "ObjectIdentifier_isValid(%s)" (p.arg.getPointer l))
        | true  -> VCBExpression (sprintf "RelativeOID_isValid(%s)" (p.arg.getPointer l))

    let fnc, ns = o.AllCons |> Asn1Fold.foldMap (fun us c -> objIdConstraint2ValidationCodeBlock l c us) us
    let fncs = conToStrFunc_basic::fnc
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] ns

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference)  (us:State)  =
    let fncs, ns = o.AllCons |> Asn1Fold.foldMap (fun us c -> enumeratedConstraint2ValidationCodeBlock  l  o typeDefinition c us) us
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] ns

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference) (childType:Asn1Type) (baseTypeValFunc : IsValidFunction option) (us:State)  =
    None, us

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (children:SeqChildInfo list)  (us:State)  =
    let child_always_present_or_absent   = match l with C -> isvalid_c.Sequence_optional_child_always_present_or_absent  | Ada -> isvalid_a.Sequence_optional_child_always_present_or_absent
    let sequence_OptionalChild           = match l with C -> isvalid_c.Sequence_OptionalChild                            | Ada -> isvalid_a.Sequence_OptionalChild
    let JoinTwoIfFirstOk                 = match l with C -> isvalid_c.JoinTwoIfFirstOk                                  | Ada -> isvalid_a.JoinTwoIfFirstOk 

    let asn1Children = children |> List.choose(fun c -> match c with Asn1Child x -> Some x | AcnChild _ -> None)
    let handleChild (child:Asn1Child) (us:State) =
        let c_name = child.getBackendName l
        let sInnerStatement = 
            match child.Type.isValidFunction with
            | Some (isValidFunction)    ->
                Some((fun (p:CallerScope)  ->
                        isValidFunction.funcBody ({p with arg = p.arg.getSeqChild l c_name child.Type.isIA5String})), isValidFunction)
            | None      -> None
        let sInnerStatement =
            match sInnerStatement with
            | None                  -> None
            | Some (func, isValid)  ->
                match child.Optionality with
                | Some _    -> 
                    let newFunc = (fun (p:CallerScope) -> sequence_OptionalChild p.arg.p (p.arg.getAcces l) c_name (func p ))
                    Some (newFunc, isValid)
                | None      -> Some (func, isValid)
        let isAlwaysPresentStatement, finalState =
            match child.Optionality with
            | Some(Asn1AcnAst.AlwaysAbsent)                     -> 
                let errCodeName = ToC ("ERR_" + ((child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm"))) + "_IS_PRESENT"
                let errCode, ns = getNextValidErrorCode us errCodeName
                let isValidStatement = (fun (p:CallerScope) -> child_always_present_or_absent p.arg.p (p.arg.getAcces l) c_name errCode.errCodeName "0")
                Some(isValidStatement, errCode), ns
            | Some(Asn1AcnAst.AlwaysPresent)                    -> 
                let errCodeName = ToC ("ERR_" + ((child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm"))) + "_IS_ABSENT"
                let errCode, ns = getNextValidErrorCode us errCodeName
                let isValidStatement = (fun (p:CallerScope) -> child_always_present_or_absent p.arg.p (p.arg.getAcces l) c_name errCode.errCodeName "1")
                Some(isValidStatement, errCode), ns
            | _         -> None, us

        match sInnerStatement, isAlwaysPresentStatement with
        | None, None                                       -> None , finalState
        | None, Some(isValid, errCode)                     -> 
            Some({IsValidAux.isValidStatement = isValid; localVars = []; alphaFuncs = []; childErrCodes = [errCode]}), finalState
        | Some(isValid, chFunc), None                      -> 
            Some({IsValidAux.isValidStatement = isValid; localVars = chFunc.localVariables; alphaFuncs = chFunc.alphaFuncs; childErrCodes = chFunc.errCodes}), finalState
        | Some(isValid1, chFunc), Some(isValid2, errCode)    -> 
            let isValid = (fun (p:CallerScope) -> JoinTwoIfFirstOk (isValid2 p )  (isValid1 p )) 
            Some({IsValidAux.isValidStatement = isValid; localVars = chFunc.localVariables; alphaFuncs = chFunc.alphaFuncs; childErrCodes = errCode::chFunc.errCodes}), finalState
        
    let childrenConent, ns1 =  asn1Children |> Asn1Fold.foldMap (fun us child -> handleChild child us) us
    let childrenConent = childrenConent |> List.choose id
    let childrenErrCodes = childrenConent |> List.collect(fun c -> c.childErrCodes)
    let alphaFuncs = childrenConent |> List.collect(fun c -> c.alphaFuncs)
    let localVars = childrenConent |> List.collect(fun c -> c.localVars)

    let vcbs, ns2 =  o.cons2 |> Asn1Fold.foldMap(fun cs c -> sequenceConstraint2ValidationCodeBlock r l t.id asn1Children ()  c cs) ns1

    let funBody (errCode: ErroCode) (p:CallerScope) = 
        let childrenChecks = childrenConent |> List.map(fun c -> VCBStatement (c.isValidStatement p)) 
        let combinedVcb = childrenChecks @ (vcbs |> List.map (fun fnc -> fnc p)) |> (ValidationCodeBlock_Multiple_And l)
        convertVCBToStatementAndAssigneErrCode l combinedVcb errCode.errCodeName
        
    createIsValidFunction r l t funBody  typeDefinition alphaFuncs localVars childrenErrCodes ns2

let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (children:ChChildInfo list) (baseTypeValFunc : IsValidFunction option) (us:State)  =
    None, us

let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (baseType:Asn1Type)  (us:State)  =
    baseType.isValidFunction, us    

