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

type ValidationCodeBlock =
    | VCBTrue                                // always true
    | VCBFalse                               // always false
    | VCBExpression of string                // single expression
    | VCBStatement  of string                // statement that updates ret or Result.Success


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
            | Some s    ->  VCBStatement s
            | None      -> raise(BugErrorException "invalid case")
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


type FoldConState = {
    alphaIndex : int
    alphaFuncs : AlphaFunc list //func name, func body
}

type ObjectIdConstraintOrBasicValidation =
    | ObjectIdConstraint_basic
    | ObjectIdConstraint_const of ObjectIdConstraint

let foldGenericCon (l:ProgrammingLanguage) valToStrFunc  (p:CallerScope)  (c:GenericConstraint<'v>)  st =
    foldGenericConstraint
        (fun e1 e2 b s      -> l.ExpOr e1 e2, s)
        (fun e1 e2 s        -> l.ExpAnd e1 e2, s)
        (fun e s            -> l.ExpNot e, s)
        (fun e1 e2 s        -> l.ExpAnd e1 (l.ExpNot e2), s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> l.ExpOr e1 e2, s)
        (fun v  s           -> l.ExpEqual (p.arg.getValue l) (valToStrFunc p v) ,s)
        c
        st

let foldRangeCon (l:ProgrammingLanguage) valToStrFunc1 valToStrFunc2 (p:CallerScope)  (c:RangeTypeConstraint<'v1,'v2>)  st =
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
        st

let integerConstraint2ValidationCodeBlock (l:ProgrammingLanguage) isUnsigned (p:CallerScope)  (c:IntegerTypeConstraint) st =
    let valToString  (i:BigInteger) = 
        match l with
        | Ada   -> i.ToString()
        | C     ->
            match isUnsigned with
            | true   -> sprintf "%sUL" (i.ToString())
            | false  -> sprintf "%sLL" (i.ToString())

    let ret, ns = foldRangeCon l valToString valToString p c st
    VCBExpression ret, ns

let realConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (p:CallerScope)  (c:RealTypeConstraint) st =
    let valToString (v:double) = v.ToString("E20", NumberFormatInfo.InvariantInfo)
    let ret, ns = foldRangeCon l valToString valToString p c st
    VCBExpression ret, ns



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


let foldStringCon  (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) alphaFuncName (p:CallerScope)  (c:IA5StringConstraint) (us0:FoldConState) =
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
            let foldAlpha p c = (foldRangeCon l (fun v -> v.ToString().ISQ) (fun v -> v.ToString().ISQ) p c 0 |> fst)
            let alphaBody = foldAlpha ({CallerScope.modName = p.modName; arg = VALUE (sprintf "str%s" (l.ArrayAccess "i"))}) alphcon
            let alphaFuncName = sprintf "%s_%d" alphaFuncName s.alphaIndex
            let funcBody =
                match l with
                | C    -> isvalid_c.Print_AlphabetCheckFunc alphaFuncName [alphaBody]
                | Ada  -> isvalid_a.Print_AlphabetCheckFunc alphaFuncName [alphaBody]
            let alphFunc = {AlphaFunc.funcName = alphaFuncName; funcBody = funcBody }

            let newState = {FoldConState.alphaIndex = s.alphaIndex + 1; alphaFuncs = alphFunc::s.alphaFuncs}
            sprintf "%s(%s)" alphaFuncName p.arg.p, newState) 
        c
        us0 


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




let rec sequenceConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (children:Asn1Child list) valToStrFunc  (p:CallerScope)  (c:SeqConstraint)  st =
    foldSeqConstraint
        (fun e1 e2 b s      -> ValidationCodeBlock_OR l e1 e2, s)
        (fun e1 e2 s        -> ValidationCodeBlock_AND l e1 e2, s)
        (fun e s            -> ValidationCodeBlock_Not l e, s)
        (fun e1 e2 s        -> ValidationCodeBlock_Except l e1 e2, s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> ValidationCodeBlock_OR l e1 e2, s)
        (fun v  s           -> VCBTrue ,s)      //currently single value constraints are not supported.
        (fun ncs  s         -> 
            let withComponentItems, ns = 
                ncs |> 
                Asn1Fold.foldMap(fun curState nc -> 
                    let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)
                    let chp = {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String}
                    match (ch.Type.ActualType).Kind, nc.Contraint with
                    | Integer o, (Some (IntegerTypeConstraint c))   -> integerConstraint2ValidationCodeBlock l o.baseInfo.isUnsigned chp c curState
                    | Real o, (Some (RealTypeConstraint    c))      -> realConstraint2ValidationCodeBlock l chp c st
                    | IA5String  o, (Some (IA5StringConstraint c))  -> VCBTrue, curState
                    | OctetString o, (Some (OctetStringConstraint c))    -> VCBTrue, curState
                    | BitString o, (Some (BitStringConstraint c))        -> VCBTrue, curState
                    | NullType o, (Some NullConstraint)                  -> VCBTrue, curState
                    | Boolean o, (Some (BoolConstraint c))               -> VCBTrue, curState
                    | Enumerated o, (Some (EnumConstraint c))            -> VCBTrue, curState
                    | ObjectIdentifier o, (Some (ObjectIdConstraint c))  -> VCBTrue, curState
                    | Sequence o, (Some (SeqConstraint c))               -> 
                        let ret, ns = sequenceConstraint2ValidationCodeBlock r l (typeId.getSeqChildId ch.Name.Value) o.Asn1Children valToStrFunc chp c curState
                        ret, ns
                    | SequenceOf o, (Some (SequenceOfConstraint c))      -> VCBTrue, curState
                    | Choice o, (Some (ChoiceConstraint c))              -> VCBTrue, curState
                    | _                                               -> raise(SemanticError(nc.Name.Location, "Invalid combination of type/constraint type"))
                ) s 
            let ret = ValidationCodeBlock_Multiple_And l withComponentItems 
            ret, ns)
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