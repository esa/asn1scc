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
open Language
open DAstUtilFunctions



let printGenericConAsAsn1  valToStrFunc    (c:Asn1AcnAst.GenericConstraint<'v>)  =
    Asn1Fold.foldGenericConstraint
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (valToStrFunc v) ,s)
        c
        0 |> fst

let printRangeConAsAsn1 valToStrFunc   (c:Asn1AcnAst.RangeTypeConstraint<'v1,'v1>)  =
    Asn1Fold.foldRangeTypeConstraint
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (valToStrFunc v) ,s)
        (fun _ v1 v2  minIsIn maxIsIn s   ->
            stg_asn1.Print_RangeConstraint (valToStrFunc v1) (valToStrFunc v2) minIsIn maxIsIn, s)
        (fun _ v1 minIsIn s   -> stg_asn1.Print_RangeConstraint_val_MAX (valToStrFunc v1) minIsIn, s)
        (fun _ v2 maxIsIn s   -> stg_asn1.Print_RangeConstraint_MIN_val (valToStrFunc v2) maxIsIn, s)
        c
        0 |> fst


let ValidationBlockAsStringExpr = function
    | VCBTrue        -> "true"
    | VCBFalse       -> "false"
    | VCBExpression sExp -> sExp
    | VCBStatement (sStat,_) -> sStat


let ValidationCodeBlock_OR (lm:LanguageMacros) vp1 vp2 =
    let expressionOrStatement  =  lm.isvalid.ExpressionOrStatement
    let statementOrExpression  = lm.isvalid.StatementOrExpression
    let statementOrStatement  = lm.isvalid.StatementOrStatement
    let expOr = lm.isvalid.ExpOr
    match vp1, vp2 with
    | VCBTrue,   _                                       -> VCBTrue
    | _,VCBTrue                                          -> VCBTrue
    | VCBFalse, _                                        -> vp2
    | _, VCBFalse                                        -> vp1
    | VCBExpression sExp1, VCBExpression sExp2    -> VCBExpression (expOr sExp1 sExp2)
    | VCBExpression sExp1, VCBStatement (sStat2, lv2)    -> VCBStatement(expressionOrStatement sExp1 sStat2, lv2)
    | VCBStatement (sStat1, lv1), VCBExpression sExp2    -> VCBStatement(statementOrExpression sStat1 sExp2, lv1)
    | VCBStatement (sStat1, lv1), VCBStatement (sStat2,lv2)    -> VCBStatement(statementOrStatement sStat1 sStat2, lv1@lv2)

let ValidationCodeBlock_AND  (lm:LanguageMacros) vp1 vp2 =
    let expressionAndStatement  = lm.isvalid.ExpressionAndStatement
    let statementAndExpression  = lm.isvalid.StatementAndExpression
    let statementAndStatement  = lm.isvalid.StatementAndStatement
    match vp1, vp2 with
    | VCBTrue,   _                                       -> vp2
    | _,VCBTrue                                          -> vp1
    | VCBFalse, _                                        -> VCBFalse
    | _, VCBFalse                                        -> VCBFalse
    | VCBExpression sExp1, VCBExpression sExp2    -> VCBExpression (lm.isvalid.ExpAnd sExp1 sExp2)
    | VCBExpression sExp1, VCBStatement (sStat2,lv2)    -> VCBStatement(expressionAndStatement sExp1 sStat2, lv2)
    | VCBStatement (sStat1, lv1), VCBExpression sExp2    -> VCBStatement(statementAndExpression sStat1 sExp2, lv1)
    | VCBStatement (sStat1, lv1), VCBStatement (sStat2,lv2)    -> VCBStatement(statementAndStatement sStat1 sStat2, lv1@lv2)

let ValidationCodeBlock_Multiple_And  (lm:LanguageMacros) (vpList:ValidationCodeBlock list)  =
    let makeExpressionToStatement0  = lm.isvalid.makeExpressionToStatement0
    let expAndMulti = lm.isvalid.ExpAndMulti
    match vpList |> Seq.exists((=) VCBFalse) with
    | true  -> VCBFalse
    | false ->
        let vpList = vpList |> List.filter (fun z -> match z with VCBExpression _ -> true | VCBStatement _ -> true | _ -> false )
        let bAllAreExpressions = false//vpList |> Seq.forall(fun z -> match z with VCBExpression _ -> true | _ -> false )
        match bAllAreExpressions with
        | true  -> VCBExpression (expAndMulti (vpList |> List.map(fun z -> match z with VCBExpression s -> s | VCBStatement (s,_) -> s | _ -> "invalid")))
        | false ->
            let soJoinedStatement, lv =
                let children, lv =
                    vpList |>
                    List.map(fun z -> match z with VCBExpression s -> makeExpressionToStatement0 s, [] | VCBStatement s -> s | _ -> "invalid", []) |>
                    List.unzip
                DAstUtilFunctions.nestItems_ret lm children, lv
            match soJoinedStatement with
            | Some s    -> VCBStatement (s, lv |> List.collect id)
            | None      -> VCBTrue

let ValidationCodeBlock_Not (lm:LanguageMacros) vp =
    let statementNot  = lm.isvalid.StatementNot
    let expNot        = lm.isvalid.ExpNot

    match vp with
    | VCBTrue        -> VCBFalse
    | VCBFalse       -> VCBTrue
    | VCBExpression sExp -> VCBExpression (expNot sExp)
    | VCBStatement (sStat, lv) -> VCBStatement ((statementNot sStat), lv)


let convertVCBToStatementAndAssignedErrCode (lm:LanguageMacros) vp  sErrCode =
    let convertVCBExpressionToStatementAndUpdateErrCode     = lm.isvalid.convertVCBExpressionToStatementAndUpdateErrCode
    let convertVCBStatementToStatementAndUpdateErrCode      = lm.isvalid.convertVCBStatementToStatementAndUpdateErrCode
    let convertVCBTRUEToStatementAndUpdateErrCode           = lm.isvalid.convertVCBTRUEToStatementAndUpdateErrCode
    let convertVCBFalseToStatementAndUpdateErrCode          = lm.isvalid.convertVCBFalseToStatementAndUpdateErrCode
    match vp with
    | VCBTrue        -> ValidationStatementTrue (convertVCBTRUEToStatementAndUpdateErrCode (), [])
    | VCBFalse       -> ValidationStatementFalse (convertVCBFalseToStatementAndUpdateErrCode sErrCode, [])
    | VCBExpression sExp -> ValidationStatement (convertVCBExpressionToStatementAndUpdateErrCode sExp sErrCode, [])
    | VCBStatement (sStat,lv) -> ValidationStatement (convertVCBStatementToStatementAndUpdateErrCode sStat sErrCode, lv)


let ValidationCodeBlock_Except  (lm:LanguageMacros) vp1 vp2 =
    // vp1 and (not vp2)
    let expressionExceptStatement     = lm.isvalid.ExpressionExceptStatement
    let statementExceptExpression   = lm.isvalid.StatementExceptExpression
    let statementExceptStatement      = lm.isvalid.StatementExceptStatement
    let expAnd                      = lm.isvalid.ExpAnd
    let expNot                      = lm.isvalid.ExpNot
    match vp1, vp2 with
    | _, VCBTrue                                         -> VCBFalse
    | VCBTrue, _                                         -> ValidationCodeBlock_Not lm vp2
    | VCBFalse, _                                        -> VCBFalse
    | _, VCBFalse                                        -> vp1
    | VCBExpression sExp1, VCBExpression sExp2    -> VCBExpression (expAnd sExp1 (expNot sExp2))
    | VCBExpression sExp1, VCBStatement (sStat2, lv2)    -> VCBStatement(expressionExceptStatement sExp1 sStat2, lv2)
    | VCBStatement (sStat1, lv1), VCBExpression sExp2    -> VCBStatement(statementExceptExpression sStat1 sExp2, lv1)
    | VCBStatement (sStat1, lv1), VCBStatement (sStat2, lv2)    -> VCBStatement(statementExceptStatement sStat1 sStat2, lv1@lv2)


let Lte (lm:LanguageMacros) eqIsInc  e1 e2 =
    match eqIsInc with
    | true   -> lm.isvalid.ExpLte e1 e2
    | false  -> lm.isvalid.ExpLt  e1 e2


let con_or  l _ (e1 : CallerScope -> ValidationCodeBlock) (e2 : CallerScope -> ValidationCodeBlock) b s =  (fun p -> ValidationCodeBlock_OR l (e1 p) (e2 p)), s
let con_and lm _ (e1 : CallerScope -> ValidationCodeBlock) (e2 : CallerScope -> ValidationCodeBlock) s =  (fun p -> ValidationCodeBlock_AND lm (e1 p) (e2 p)), s
let con_not lm _ (e : CallerScope -> ValidationCodeBlock)  s =  (fun p -> ValidationCodeBlock_Not lm (e p)), s
let con_except lm _ (e1 : CallerScope -> ValidationCodeBlock) (e2 : CallerScope -> ValidationCodeBlock) s =  (fun p -> ValidationCodeBlock_Except lm (e1 p) (e2 p)), s
let con_root _ e s = e,s
let con_root2 lm _ (e1 : CallerScope -> ValidationCodeBlock) (e2 : CallerScope -> ValidationCodeBlock)  s =  (fun p -> ValidationCodeBlock_OR lm (e1 p) (e2 p)), s

(*  e.g. INTEGER (v1..MAX)  ==>          v1 <= p.p   *)
let con_rangeConstraint_val_MAX  (lm:LanguageMacros) minint maxint v1 eqIsInc valToStrFunc pToStrFunc =
    match v1 < minint with
    | true                              -> fun (p:CallerScope) -> VCBTrue  (* e.g. for unsigned integer (-40 .. MAX) *)
    | false when v1 = minint && eqIsInc -> fun (p:CallerScope) -> VCBTrue
    | false                             -> fun (p:CallerScope) -> (VCBExpression (Lte lm eqIsInc (valToStrFunc v1) (pToStrFunc lm p (*p.arg.getValue l*) )))


(* e.g INTEGER (MIN .. v1) --> p.p <= v1 *)
let con_rangeConstraint_MIN_val  (lm:LanguageMacros) minint maxint v1 eqIsInc valToStrFunc pToStrFunc =
    match v1 > maxint with
    | true                              -> fun (p:CallerScope) -> VCBTrue  (* e.g. for unsigned integer (MIN .. value_larger_than_maxint) *)
    | false when v1 = maxint && eqIsInc -> fun (p:CallerScope) -> VCBTrue
    | false                             -> fun (p:CallerScope) -> (VCBExpression (Lte lm eqIsInc (pToStrFunc lm p) (valToStrFunc v1)))


let foldGenericCon  (lm:LanguageMacros) valToStrFunc  (c:GenericConstraint<'v>)  st =
    foldGenericConstraint (con_or lm) (con_and lm) (con_not lm) (con_except lm) con_root (con_root2 lm)
        (fun _ v  s           -> (fun p -> VCBExpression (lm.isvalid.ExpEqual (lm.lg.getValue  p.arg) (valToStrFunc v))) ,s)
        c
        st


let foldSizeRangeTypeConstraint (r:Asn1AcnAst.AstRoot)   (lm:LanguageMacros)  getSizeFunc  (c:PosIntTypeConstraint) st =
    let valToStrFunc (v:UInt32) = v.ToString()
    foldRangeTypeConstraint (con_or lm) (con_and lm) (con_not lm) (con_except lm) con_root (con_root2 lm)
        (fun _ v  (s:State)         -> (fun p -> VCBExpression (lm.isvalid.ExpEqual (getSizeFunc lm p) (valToStrFunc  v))) ,s)
        (fun _ v1 v2  minIsIn maxIsIn s   ->
            let exp1 = con_rangeConstraint_val_MAX  lm UInt32.MinValue UInt32.MaxValue v1 minIsIn valToStrFunc getSizeFunc
            let exp2 = con_rangeConstraint_MIN_val  lm UInt32.MinValue UInt32.MaxValue v2 maxIsIn valToStrFunc getSizeFunc
            con_and lm s exp1 exp2 s)
        (fun asn1 v1 minIsIn s   -> con_rangeConstraint_val_MAX  lm UInt32.MinValue UInt32.MaxValue v1 minIsIn valToStrFunc getSizeFunc, s)
        (fun asn1 v2 maxIsIn s   -> con_rangeConstraint_MIN_val  lm UInt32.MinValue UInt32.MaxValue v2 maxIsIn valToStrFunc getSizeFunc, s)
        c
        st


let foldSizableConstraint (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) hasCount compareSingValueFunc  getSizeFunc  (c:SizableTypeConstraint<'v>) st =
    foldSizableTypeConstraint2 (con_or lm) (con_and lm) (con_not lm) (con_except lm) con_root (con_root2 lm)
        (fun _ v  s           -> (fun p -> compareSingValueFunc p v) ,s)
        (fun _ intCon s       ->
            match hasCount with
            | true  -> foldSizeRangeTypeConstraint r lm getSizeFunc intCon s
            | false -> (fun p -> VCBTrue), s)
        c
        st

let ia5StringConstraint2ValidationCodeBlock  (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (typeId:ReferenceToType)   (c:IA5StringConstraint) (us0:State) =
    let print_AlphabetCheckFunc = lm.isvalid.Print_AlphabetCheckFunc
    let stringContainsChar    (v:String)  =
        let newStr =
            if v.Length>1 then
                v.IDQ
            elif v.Length = 1 then
                let c = v.ToCharArray()[0]
                if   c = CommonTypes.CharCR  then lm.vars.PrintCR ()
                elif c = CommonTypes.CharLF  then lm.vars.PrintLF ()
                elif c = CommonTypes.CharHT  then lm.vars.PrintHT ()
                elif c = CommonTypes.CharNul then lm.vars.PrintStringValueNull ()
                else v.IDQ
            else
                v.IDQ
        lm.isvalid.stringContainsChar newStr

    let alphaFuncName = ToC (((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_CharsAreValid")
    let foldRangeCharCon (lm:LanguageMacros)   (c:CharTypeConstraint)  st =
        let valToStrFunc1 v = v.ToString().ISQ
        foldRangeTypeConstraint   (con_or lm) (con_and lm) (con_not lm) (con_except lm) con_root (con_root2 lm)
            (fun _ (v:string)  s  -> (fun p -> VCBExpression (stringContainsChar v (p.arg.joined lm.lg))) ,s)
            (fun _ v1 v2  minIsIn maxIsIn s   ->
                (fun p -> VCBExpression(lm.isvalid.ExpAnd (Lte lm minIsIn (valToStrFunc1 v1) (lm.lg.getValue p.arg)) (Lte lm maxIsIn (lm.lg.getValue p.arg) (valToStrFunc1 v2)))), s)
            (fun _ v1 minIsIn s   -> (fun p -> VCBExpression( Lte lm minIsIn (valToStrFunc1 v1) (lm.lg.getValue p.arg))), s)
            (fun _ v2 maxIsIn s   -> (fun p -> VCBExpression(Lte lm maxIsIn (lm.lg.getValue p.arg) (valToStrFunc1 v2))), s)
            c
            st

    foldStringTypeConstraint2 (con_or lm) (con_and lm) (con_not lm) (con_except lm) con_root (con_root2 lm)
        (fun _ v  s         -> (fun p -> VCBExpression (lm.isvalid.ExpStringEqual (p.arg.joined lm.lg) v.IDQ))  ,s)
        (fun _ intCon s     -> foldSizeRangeTypeConstraint r lm (fun l p -> lm.isvalid.StrLen (p.arg.joined lm.lg)) intCon s)
        (fun _ alphcon (s:State)      ->
            let alphaBody p =
                let alphaFunc = foldRangeCharCon lm  alphcon 0 |> fst
                match alphaFunc p with
                | VCBExpression x   -> x
                | VCBStatement (x,_)    -> x
                | VCBTrue           -> "TRUE"
                | VCBFalse          -> "FALSE"

            let alphaFuncName = sprintf "%s_%d" alphaFuncName s.alphaIndex
            let funcBody p = print_AlphabetCheckFunc alphaFuncName [alphaBody p]
            let alphaFunc = {AlphaFunc.funcName = alphaFuncName; funcBody = funcBody }

            let newState = {s with alphaIndex = s.alphaIndex + 1; alphaFuncs = alphaFunc::s.alphaFuncs}
            let retFnc p = VCBExpression (sprintf "%s(%s)" alphaFuncName (p.arg.joined lm.lg))
            retFnc, newState)
        c
        us0

let integerConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (intClass:Asn1AcnAst.IntegerClass)  (c:IntegerTypeConstraint) st =
    let valToStrFunc  (i:BigInteger) = lm.lg.intValueToString i intClass
    let p2StrFunc l (p:CallerScope) = l.lg.getValue p.arg
    foldRangeTypeConstraint (con_or lm) (con_and lm) (con_not lm) (con_except lm) con_root (con_root2 lm)
        (fun _ v  s         -> (fun p -> VCBExpression (lm.isvalid.ExpEqual (lm.lg.getValue p.arg) (valToStrFunc  v))) ,s)
        (fun asn1 v1 v2  minIsIn maxIsIn s   ->
            let exp1 = con_rangeConstraint_val_MAX  lm intClass.Min intClass.Max v1 minIsIn valToStrFunc p2StrFunc
            let exp2 = con_rangeConstraint_MIN_val  lm intClass.Min intClass.Max v2 maxIsIn valToStrFunc p2StrFunc
            con_and  lm asn1 exp1 exp2 s)
        (fun _ v1 minIsIn s   -> con_rangeConstraint_val_MAX  lm intClass.Min intClass.Max v1 minIsIn valToStrFunc p2StrFunc, s)
        (fun _ v2 maxIsIn s   -> con_rangeConstraint_MIN_val  lm intClass.Min intClass.Max v2 maxIsIn valToStrFunc p2StrFunc, s)
        c
        st


let realConstraint2ValidationCodeBlock  (l:LanguageMacros) (c:RealTypeConstraint) st =
    let valToStrFunc (v:double) = v.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo)
    foldRangeTypeConstraint  (con_or l) (con_and l) (con_not l) (con_except l) con_root (con_root2 l)
        (fun _ v  s         -> (fun p -> VCBExpression(l.isvalid.ExpEqual (l.lg.getValue p.arg) (valToStrFunc  v))) ,s)
        (fun _ v1 v2  minIsIn maxIsIn s   ->
            (fun p -> VCBExpression (l.isvalid.ExpAnd (Lte l minIsIn (valToStrFunc v1) (l.lg.getValue p.arg)) (Lte l maxIsIn (l.lg.getValue p.arg) (valToStrFunc v2)))), s)
        (fun _ v1 minIsIn s   -> (fun p -> VCBExpression(Lte l minIsIn (valToStrFunc v1) (l.lg.getValue p.arg))), s)
        (fun _ v2 maxIsIn s   -> (fun p -> VCBExpression(Lte l maxIsIn (l.lg.getValue p.arg) (valToStrFunc v2))), s)
        c
        st


let booleanConstraint2ValidationCodeBlock  (lm:LanguageMacros) (c:BoolConstraint) st =
    foldGenericCon lm  (fun v -> v.ToString().ToLower()) c st


let objIdConstraint2ValidationCodeBlock  (l:LanguageMacros) (c:ObjectIdConstraint) st =
    let objId_equal = l.isvalid.objId_equal

    let printObjectIdentifierValue = l.vars.PrintObjectIdentifierValueAsCompoundLiteral

    foldGenericConstraint (con_or l) (con_and l) (con_not l) (con_except l) con_root (con_root2 l)
        (fun _ (a,b)  s           ->
            let v =  Asn1DefinedObjectIdentifierValue(a,b)
            (fun (p:CallerScope) ->
                let lit = printObjectIdentifierValue (v.Values |> List.map fst) (BigInteger v.Values.Length)
                VCBExpression (objId_equal (p.arg.joined l.lg) lit)) ,s)
        c
        st

let timeConstraint2ValidationCodeBlock (l:LanguageMacros) (td) (c:TimeConstraint) st =
    let print_Asn1LocalTime                  = l.vars.PrintTimeValueAsCompoundLiteral_Asn1LocalTime
    let print_Asn1UtcTime                    = l.vars.PrintTimeValueAsCompoundLiteral_Asn1UtcTime
    let print_Asn1LocalTimeWithTimeZone      = l.vars.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone
    let print_Asn1Date                       = l.vars.PrintTimeValueAsCompoundLiteral_Asn1Date
    let print_Asn1Date_LocalTime             = l.vars.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime
    let print_Asn1Date_UtcTime               = l.vars.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime
    let print_Asn1Date_LocalTimeWithTimeZone = l.vars.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone

    let printValueAsLiteral  (iv:Asn1DateTimeValue) =
        match iv with
        |Asn1LocalTimeValue                  tv        -> print_Asn1LocalTime td tv
        |Asn1UtcTimeValue                    tv        -> print_Asn1UtcTime td tv
        |Asn1LocalTimeWithTimeZoneValue      (tv,tz)   -> print_Asn1LocalTimeWithTimeZone td tv tz
        |Asn1DateValue                       dt        -> print_Asn1Date td dt
        |Asn1Date_LocalTimeValue             (dt,tv)   -> print_Asn1Date_LocalTime td dt tv
        |Asn1Date_UtcTimeValue               (dt,tv)   -> print_Asn1Date_UtcTime  td dt tv
        |Asn1Date_LocalTimeWithTimeZoneValue (dt,tv,tz)-> print_Asn1Date_LocalTimeWithTimeZone td dt tv tz

    foldGenericConstraint (con_or l) (con_and l) (con_not l) (con_except l) con_root (con_root2 l)
        (fun _ v  s           ->
            (fun (p:CallerScope) ->
                let lit = printValueAsLiteral v
                VCBExpression (l.isvalid.ExpEqual (p.arg.joined l.lg) lit)) ,s)
        c
        st


let enumeratedConstraint2ValidationCodeBlock  (l:LanguageMacros) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionOrReference) (c:EnumConstraint) st =
    let printNamedItem  (v:string) =
        let itm = o.items |> Seq.find (fun x -> x.Name.Value = v)
        let ret = l.lg.getNamedItemBackendName (Some typeDefinition ) itm
        ret
    foldGenericCon l  printNamedItem c st

let octetStringConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (typeId:ReferenceToType) (o:Asn1AcnAst.OctetString) (equalFunc:EqualFunction) (c:OctetStringConstraint) st =
    let getSizeFunc  (lm:LanguageMacros) p = l.lg.Length (p.arg.joined l.lg) (l.lg.getAccess p.arg)
    let compareSingleValueFunc (p:CallerScope) (v:Asn1AcnAst.OctetStringValue, (id,loc))  =
        let octet_var_string_equal = l.isvalid.octet_var_string_equal
        let octet_fix_string_equal = l.isvalid.octet_fix_string_equal
        let printOctetArrayAsCompoundLiteral = l.vars.PrintOctetArrayAsCompoundLiteral
        let octArrLiteral = printOctetArrayAsCompoundLiteral  (v |> List.map (fun b -> b.Value))
        match o.isFixedSize with
        | true   -> VCBExpression (octet_fix_string_equal (p.arg.joined l.lg) (l.lg.getAccess p.arg) o.minSize.uper (v.Length.AsBigInt) octArrLiteral)
        | false  -> VCBExpression (octet_var_string_equal (p.arg.joined l.lg) (l.lg.getAccess p.arg)  (v.Length.AsBigInt) octArrLiteral)
    let fnc, ns = foldSizableConstraint r l (not o.isFixedSize) compareSingleValueFunc getSizeFunc c st
    fnc, ns

let bitStringConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot)  (l:LanguageMacros) (typeId:ReferenceToType) (o:Asn1AcnAst.BitString) (equalFunc:EqualFunction) (c:BitStringConstraint) st =
    let getSizeFunc (l:LanguageMacros) p = l.lg.Length (p.arg.joined l.lg) (l.lg.getAccess p.arg)
    let compareSingleValueFunc (p:CallerScope) (v:Asn1AcnAst.BitStringValue, (id,loc))  =
        let bit_var_string_equal = l.isvalid.bit_var_string_equal
        let bit_fix_string_equal = l.isvalid.bit_fix_string_equal
        let printOctetArrayAsCompoundLiteral = l.vars.PrintOctetArrayAsCompoundLiteral
        let bytes = bitStringValueToByteArray (StringLoc.ByValue v.Value)
        let octArrLiteral = printOctetArrayAsCompoundLiteral  bytes
        let bitArrLiteral = variables_a.PrintBitArrayAsCompoundLiteral  (v.Value.ToCharArray() |> Seq.map(fun c -> if c = '0' then 0uy else 1uy))
        match o.isFixedSize with
        | true   -> VCBExpression (bit_fix_string_equal (p.arg.joined l.lg) (l.lg.getAccess p.arg) o.minSize.uper (v.Value.Length.AsBigInt) octArrLiteral bitArrLiteral)
        | false  -> VCBExpression (bit_var_string_equal (p.arg.joined l.lg) (l.lg.getAccess p.arg)  (v.Value.Length.AsBigInt) octArrLiteral bitArrLiteral)
    let fnc, ns = foldSizableConstraint r l (not o.isFixedSize) compareSingleValueFunc getSizeFunc c st
    fnc, ns


let rec anyConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot)  (l:LanguageMacros) (erLoc:SrcLoc) (t:Asn1Type) (ac:AnyConstraint) st =
    match t.ActualType.Kind, ac with
    | Integer o, IntegerTypeConstraint c        -> integerConstraint2ValidationCodeBlock r l (o.baseInfo.intClass) c st
    | Real o, RealTypeConstraint   c            -> realConstraint2ValidationCodeBlock l c st
    | IA5String  o, IA5StringConstraint c       -> ia5StringConstraint2ValidationCodeBlock r l t.id  c st
    | OctetString o, OctetStringConstraint c    -> octetStringConstraint2ValidationCodeBlock r l  t.id o.baseInfo  o.equalFunction c st
    | BitString o, BitStringConstraint c        -> bitStringConstraint2ValidationCodeBlock r l  t.id o.baseInfo o.equalFunction c st
    | NullType o, NullConstraint                -> (fun p -> VCBTrue), st
    | Boolean o, BoolConstraint c               -> booleanConstraint2ValidationCodeBlock l c st
    | Enumerated o, EnumConstraint c            -> enumeratedConstraint2ValidationCodeBlock  l  o.baseInfo o.definitionOrRef c st
    | ObjectIdentifier o, ObjectIdConstraint c  -> objIdConstraint2ValidationCodeBlock l c st
    | TimeType o, TimeConstraint c              -> timeConstraint2ValidationCodeBlock l (l.lg.typeDef o.baseInfo.typeDef) c st
    | Sequence o, SeqConstraint c               ->
        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.SeqValue) = VCBTrue //currently single value constraints are ignored.
        sequenceConstraint2ValidationCodeBlock r l t.id o.Asn1Children valToStrFunc  c st
    | SequenceOf o, SequenceOfConstraint c      -> sequenceOfConstraint2ValidationCodeBlock r l t.id o.baseInfo o.childType o.equalFunction c st
    | Choice o, ChoiceConstraint c              ->
        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.ChValue) = VCBTrue //currently single value constraints are ignored.
        choiceConstraint2ValidationCodeBlock r l t.id o.children valToStrFunc o.definitionOrRef c st
    | _                                         -> raise(SemanticError(erLoc, "Invalid combination of type/constraint type"))

and sequenceConstraint2ValidationCodeBlock (r: Asn1AcnAst.AstRoot) (l: LanguageMacros) (typeId: ReferenceToType) (children: Asn1Child list) valToStrFunc (c: SeqConstraint) st =
    let child_always_present_or_absentExp   = l.isvalid.Sequence_optional_child_always_present_or_absent_expr
    let sequence_OptionalChild              = l.isvalid.Sequence_OptionalChild
    let expressionToStatement                 = l.isvalid.ExpressionToStatement

    let handleNamedConstraint curState (nc:NamedConstraint) =
        let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)

        let childCheck, ns =
            match nc.Constraint with
            | None       -> (fun p -> VCBTrue), curState
            | Some ac    ->
                let fnc, ns = anyConstraint2ValidationCodeBlock r l nc.Name.Location ch.Type ac curState
                (fun p ->
                    let child_arg = l.lg.getSeqChild p.arg (l.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String ch.Optionality.IsSome
                    let chp = {p with arg = child_arg}
                    fnc chp), ns

        let childCheck =
            match ch.Optionality with
            | None      -> childCheck
            | Some _    ->
                let newChildCheckFnc (p:CallerScope) =
                    match childCheck p with
                    | VCBExpression  exp -> VCBStatement (sequence_OptionalChild (p.arg.joined l.lg) (l.lg.getAccess p.arg) (l.lg.getAsn1ChildBackendName ch) (expressionToStatement exp), [])
                    | VCBStatement   (stat, lv1)-> VCBStatement (sequence_OptionalChild (p.arg.joined l.lg) (l.lg.getAccess p.arg) (l.lg.getAsn1ChildBackendName ch) stat, lv1)
                    | VCBTrue            -> VCBTrue
                    | VCBFalse           -> VCBStatement (sequence_OptionalChild (p.arg.joined l.lg) (l.lg.getAccess p.arg) (l.lg.getAsn1ChildBackendName ch) (expressionToStatement "FALSE"), [])

                newChildCheckFnc

        let isAbsentFlag =
            match ST.lang with
            | ProgrammingLanguage.Scala -> l.lg.FalseLiteral
            | _ -> "0"

        let isPresentFlag =
            match ST.lang with
            | ProgrammingLanguage.Scala -> l.lg.TrueLiteral
            | _ -> "1" // leave like it was - TRUE may not be 1

        let presentAbsent =
            match nc.Mark with
            | Asn1Ast.NoMark        -> []
            | Asn1Ast.MarkOptional  -> []
            | Asn1Ast.MarkAbsent    ->
                let isExp = (fun (p:CallerScope) -> VCBExpression (child_always_present_or_absentExp (p.arg.joined l.lg) (l.lg.getAccess p.arg) (l.lg.getAsn1ChildBackendName ch)  isAbsentFlag))
                [isExp]
            | Asn1Ast.MarkPresent    ->
                let isExp = (fun (p:CallerScope) -> VCBExpression (child_always_present_or_absentExp (p.arg.joined l.lg) (l.lg.getAccess p.arg) (l.lg.getAsn1ChildBackendName ch)  isPresentFlag))
                [isExp]

        presentAbsent@[childCheck], ns

    foldSeqConstraint (con_or l) (con_and l) (con_not l) (con_except l) con_root (con_root2 l)
        (fun _ v  s           ->   (fun p -> valToStrFunc p v) ,s)
        (fun _ ncs  s         ->
            let withComponentItems, ns =  ncs |> Asn1Fold.foldMap handleNamedConstraint s
            let withComponentItems = withComponentItems |> List.collect id
            let fnc p =
                ValidationCodeBlock_Multiple_And l (withComponentItems |> List.map (fun fnc -> fnc p))
            fnc, ns)
        c
        st



and choiceConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (typeId:ReferenceToType) (children:ChChildInfo list) valToStrFunc (defOrRef:TypeDefinitionOrReference)     (c:ChoiceConstraint)  st =
    let choice_OptionalChild              = l.isvalid.Choice_OptionalChild
    let expressionToStatement               = l.isvalid.ExpressionToStatement
    let choice_child_always_present_Exp   = l.isvalid.Choice_child_always_present_Exp
    let choice_child_always_absent_Exp    = l.isvalid.Choice_child_always_absent_Exp


    let handleNamedConstraint curState (nc:NamedConstraint) =
        let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)
        let presentWhenName = l.lg.presentWhenName (Some defOrRef) ch
        let childCheck, ns =
            match nc.Constraint with
            | None       -> (fun p -> VCBTrue), curState
            | Some ac    ->
                let fnc, ns = anyConstraint2ValidationCodeBlock r l nc.Name.Location ch.chType ac curState
                (fun p ->
                    let child_arg = l.lg.getChChild p.arg (l.lg.getAsn1ChChildBackendName ch) ch.chType.isIA5String
                    let chp = {p with arg = child_arg}
                    fnc chp), ns

        let childCheck =
            let newChildCheckFnc (p:CallerScope) =
                match childCheck p with
                | VCBExpression  exp -> VCBStatement (choice_OptionalChild (p.arg.joined l.lg) "" (l.lg.getAccess p.arg) presentWhenName (expressionToStatement exp), [])
                | VCBStatement   (stat, lv1)-> VCBStatement (choice_OptionalChild (p.arg.joined l.lg) "" (l.lg.getAccess p.arg) presentWhenName stat, lv1)
                | VCBTrue            -> VCBTrue
                | VCBFalse           -> VCBStatement (choice_OptionalChild (p.arg.joined l.lg) "" (l.lg.getAccess p.arg) (presentWhenName) (expressionToStatement "FALSE"), [])

            newChildCheckFnc

        let presentAbsent =
            match nc.Mark with
            | Asn1Ast.NoMark        -> []
            | Asn1Ast.MarkOptional  -> []
            | Asn1Ast.MarkAbsent    ->
                let isExp = (fun (p:CallerScope) -> VCBExpression (choice_child_always_absent_Exp (p.arg.joined l.lg) (l.lg.getAccess p.arg) presentWhenName  ))
                [isExp]
            | Asn1Ast.MarkPresent    ->
                let isExp = (fun (p:CallerScope) -> VCBExpression (choice_child_always_present_Exp (p.arg.joined l.lg) (l.lg.getAccess p.arg) presentWhenName ))
                [isExp]

        presentAbsent@[childCheck], ns


    foldChoiceConstraint (con_or l) (con_and l) (con_not l) (con_except l) con_root (con_root2 l)
        (fun _ v  s           ->   (fun p -> valToStrFunc p v) ,s)
        (fun _ ncs  s         ->
            let withComponentItems, ns =  ncs |> Asn1Fold.foldMap handleNamedConstraint s
            let withComponentItems = withComponentItems |> List.collect id
            let fnc p =
                ValidationCodeBlock_Multiple_And l (withComponentItems |> List.map (fun fnc -> fnc p))
            fnc, ns)
        c
        st

and sequenceOfConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (typeId:ReferenceToType) (o:Asn1AcnAst.SequenceOf) (child:Asn1Type) (equalFunc:EqualFunction) (c:SequenceOfConstraint) st =

    let expressionToStatement              = l.isvalid.ExpressionToStatement
    let statementForLoop                 = l.isvalid.StatementForLoop

    let getSizeFunc (l:LanguageMacros) p = l.lg.Length (p.arg.joined l.lg) (l.lg.getAccess p.arg)
    let compareSingleValueFunc (p:CallerScope) (v:Asn1AcnAst.SeqOfValue)  =
        VCBTrue
    foldSequenceOfTypeConstraint2 (con_or l) (con_and l) (con_not l) (con_except l) con_root (con_root2 l)
        (fun _ v  s           -> (fun p -> compareSingleValueFunc p v) ,s)
        (fun _ intCon s       ->
            match o.isFixedSize with
            | false  -> foldSizeRangeTypeConstraint r l getSizeFunc intCon s
            | true  -> (fun p -> VCBTrue), s)
        (fun _ c loc s ->
            (fun (p:CallerScope) ->
                let ii = p.arg.SequenceOfLevel + 1
                let i = sprintf "i%d" ii
                let lv = SequenceOfIndex (ii, None)
                let fnc, ns = anyConstraint2ValidationCodeBlock r l loc child c s
                let ch_arg = l.lg.getArrayItem p.arg i child.isIA5String
                let childCheck p = fnc ({p with arg = ch_arg})
                let ret =
                    match childCheck p with
                    | VCBExpression  exp -> VCBStatement (statementForLoop (p.arg.joined l.lg) (l.lg.getAccess p.arg) i o.isFixedSize o.minSize.uper (expressionToStatement exp), [lv])
                    | VCBStatement   (stat, lv2)-> VCBStatement (statementForLoop (p.arg.joined l.lg) (l.lg.getAccess p.arg) i o.isFixedSize o.minSize.uper stat, lv::lv2)
                    | VCBTrue            -> VCBTrue
                    | VCBFalse           -> VCBStatement (statementForLoop (p.arg.joined l.lg) (l.lg.getAccess p.arg) i o.isFixedSize o.minSize.uper (expressionToStatement "FALSE"), [lv])
                ret), s)
        c
        st



let getIntSimplifiedConstraints (r:Asn1AcnAst.AstRoot) isUnsigned (allCons  : IntegerTypeConstraint list) =
    allCons

let hasValidationFunc allCons =
    match allCons with
    | []      -> false
    | _       -> true

let getFuncName (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (typeDefinition:TypeDefinitionOrReference) =
    getFuncNameGeneric  typeDefinition "_IsConstraintValid"


let str_p (lm:LanguageMacros) (typeid:ReferenceToType) =
    ({CallerScope.modName = typeid.ModName; arg = (Selection.emptyPath "str" FixArray).append (ArrayAccess ("i", Value))})

type IsValidAux = {
    isValidStatement  : CallerScope -> ValidationStatement
    localVars         : LocalVariable list
    alphaFuncs        : AlphaFunc list
    childErrCodes     : ErrorCode list

}

type SeqCompIsValidAux =
    | IsValidEmbedded of {|
                            isValidStatement  : CallerScope -> ValidationStatement
                            localVars         : LocalVariable list
                            alphaFuncs        : AlphaFunc list
                            childErrCodes     : ErrorCode list
                         |}
    | IsValidProcCall of {|
                            isValidStatement    : CallerScope -> ValidationStatement
                            chidIsValidFunction : IsValidFunction
                        |}


let createIsValidFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type)  (fncBodyE : ErrorCode->CallerScope->ValidationStatement) (typeDefinition:TypeDefinitionOrReference) (alphaFuncs : AlphaFunc list) (localVars : LocalVariable list) (childErrCodes : ErrorCode list) nonEmbeddedChildrenValidFuncs errorCodeComment (us:State)  =
    let emitTasFnc    = lm.isvalid.EmitTypeAssignment_composite
    let emitTasFncDef = lm.isvalid.EmitTypeAssignment_composite_def
    let defErrCode    = lm.isvalid.EmitTypeAssignment_composite_def_err_code

    let funcName            = getFuncName r lm typeDefinition
    let errCodeName         = ToC ("ERR_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName errorCodeComment

    let funcBody = fncBodyE errCode
    let errCodes = errCode::childErrCodes
    let p  = lm.lg.getParamType t Encode
    let varName = p.arg.receiverId
    let sStar = lm.lg.getStar p.arg
    let sPtrPrefix = lm.lg.getPtrPrefix p.arg
    let sPtrSuffix = lm.lg.getPtrSuffix p.arg
    let func, funcDef  =
            match funcName  with
            | None              -> None, None
            | Some funcName     ->
                let statement, stLVs, bUnreferenced =
                    match funcBody p with
                    | ValidationStatementTrue   (st,lv)
                    | ValidationStatementFalse  (st,lv) ->  st, lv, true
                    | ValidationStatement       (st,lv)  -> st, lv, false
                let lvars = (stLVs@localVars) |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct
                let fnc = emitTasFnc varName sPtrPrefix sPtrSuffix funcName (lm.lg.getLongTypedefName typeDefinition) statement (alphaFuncs |> List.map(fun x -> x.funcBody (str_p lm t.id))) lvars bUnreferenced
                let split (s:string option) =
                    match s with
                    | None -> []
                    | Some s -> s.Split('\n') |> Seq.toList
                let arrsErrcodes = errCodes |> List.map(fun s -> defErrCode s.errCodeName (BigInteger s.errCodeValue) (split s.comment))
                let fncH = emitTasFncDef varName sStar funcName  (lm.lg.getLongTypedefName typeDefinition) arrsErrcodes
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
            nonEmbeddedChildrenValidFuncs          = nonEmbeddedChildrenValidFuncs
        }
    Some ret, ns

let funcBody l fncs (e:ErrorCode) (p:CallerScope) =
    let combinedVcb = fncs |> List.map (fun fnc -> fnc p) |> (ValidationCodeBlock_Multiple_And l)
    convertVCBToStatementAndAssignedErrCode l combinedVcb e.errCodeName

let createIntegerFunction (r:Asn1AcnAst.AstRoot)  (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionOrReference) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> integerConstraint2ValidationCodeBlock r l (o.intClass) c us) us
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t  (funcBody l fncs) typeDefinition [] [] [] [] (Some errorCodeComment) ns

let createIntegerFunctionByCons (r:Asn1AcnAst.AstRoot)  (l:LanguageMacros) isUnsigned (allCons  : IntegerTypeConstraint list) =
    match allCons with
    | []        -> None
    | _         ->
        let fncs, ns = allCons |> Asn1Fold.foldMap (fun us c -> integerConstraint2ValidationCodeBlock r l isUnsigned c us) 0
        let funcExp (p:CallerScope) =
            let vp = fncs |> List.map (fun fnc -> fnc p) |> (ValidationCodeBlock_Multiple_And l)
            vp
        Some funcExp

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionOrReference)  (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> realConstraint2ValidationCodeBlock l c us) us
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t (funcBody l fncs) typeDefinition [] [] [] [] (Some errorCodeComment) ns

let createBoolFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionOrReference) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> booleanConstraint2ValidationCodeBlock l c us) us
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] (Some errorCodeComment) ns

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionOrReference) (equalFunc:EqualFunction) (printValue  : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> octetStringConstraint2ValidationCodeBlock r l  t.id o equalFunc c us) us
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] (Some errorCodeComment) ns

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionOrReference) (defOrRef:TypeDefinitionOrReference) (equalFunc:EqualFunction) (printValue  : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> bitStringConstraint2ValidationCodeBlock r l  t.id o equalFunc c us) us
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] (Some errorCodeComment) ns

let createStringFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionOrReference) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> ia5StringConstraint2ValidationCodeBlock r  l t.id c us) {us with alphaIndex=0; alphaFuncs=[]}
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t (funcBody l fncs) typeDefinition ns.alphaFuncs [] [] [] (Some errorCodeComment) ns

let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefinitionOrReference) (us:State)  =
    let conToStrFunc_basic (p:CallerScope)  =
        let namespacePrefix = l.lg.rtlModuleName
        match o.relativeObjectId with
        | false -> VCBExpression (sprintf "%sObjectIdentifier_isValid(%s)" namespacePrefix (l.lg.getPointer p.arg))
        | true  -> VCBExpression (sprintf "%sRelativeOID_isValid(%s)" namespacePrefix (l.lg.getPointer p.arg))

    let fnc, ns = o.cons |> Asn1Fold.foldMap (fun us c -> objIdConstraint2ValidationCodeBlock l c us) us
    let fncs = conToStrFunc_basic::fnc
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] (Some errorCodeComment) ns


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefinitionOrReference) (us:State)  =
    let conToStrFunc_basic (p:CallerScope)  =
        let namespacePrefix = l.lg.rtlModuleName
        VCBExpression (sprintf "%sTimeType_isValid(%s)" namespacePrefix (l.lg.getPointer p.arg))
    let fnc, ns = o.cons |> Asn1Fold.foldMap (fun us c -> timeConstraint2ValidationCodeBlock l (l.lg.typeDef o.typeDef) c us) us
    let fncs = conToStrFunc_basic::fnc
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] (Some errorCodeComment) ns


let createEfficientEnumValidation (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (o:Asn1AcnAst.Enumerated)   (us:State)  =
    let getEnumIndexByName = l.isvalid.GetEnumIndexByName
    let td = (l.lg.getEnumTypeDefinition o.typeDef)
    let bSorted =
        let sortedItems = o.validItems |> List.map(fun x -> x.definitionValue) |> List.sort
        let items = o.validItems |> List.map(fun x -> x.definitionValue)
        sortedItems = items
    let optimizedValidation (p:CallerScope) =
        let ret = getEnumIndexByName td.values_array td.values_array_count (l.lg.getValue p.arg) bSorted
        VCBExpression (ret)
    [optimizedValidation], us


let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionOrReference)  (us:State)  =
    let fncs, ns =
        match r.args.isEnumEfficientEnabled o.items.Length with
        | false -> o.cons |> Asn1Fold.foldMap (fun us c -> enumeratedConstraint2ValidationCodeBlock  l  o typeDefinition c us) us
        | true  -> createEfficientEnumValidation r l o us
    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] (Some errorCodeComment) ns

let convertMultipleVCBsToStatementAndSetErrorCode l p (errCode: ErrorCode) vcbs =
    let combinedVcb =
        vcbs |>
        List.map (fun fnc -> fnc p) |>
        List.filter(fun vcb ->
            match vcb with
            | VCBTrue        -> false
            | VCBFalse       -> true
            | VCBExpression sExp -> true
            | VCBStatement sStat -> true) |>
        ValidationCodeBlock_Multiple_And l
    let st = convertVCBToStatementAndAssignedErrCode l combinedVcb errCode.errCodeName
    match st with
    | ValidationStatementTrue _ -> []
    | ValidationStatementFalse  st
    | ValidationStatement       st -> [st]

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionOrReference) (childType:Asn1Type) (equalFunc:EqualFunction) (us:State)  =
    let sequenceOf          = l.isvalid.sequenceOf2
    let callBaseTypeFunc    = l.isvalid.call_base_type_func

    let vcbs, ns2 = o.cons |> Asn1Fold.foldMap(fun cs c -> sequenceOfConstraint2ValidationCodeBlock r l t.id o (childType:Asn1Type) equalFunc c cs) us

    let chp (p:CallerScope) = 
        let i = sprintf "i%d" (p.arg.SequenceOfLevel + 1)
        {p with arg = l.lg.getArrayItem p.arg i childType.isIA5String}

    let childFunc, childErrCodes, childAlphaFuncs, childLocalVars, nonEmbeddedChildValidFuncs =
        match childType.isValidFunction with
        | None ->   None, [], [], [], []
        | Some cvf ->
            match cvf.funcName with
            | Some fncName  ->
                let f1 (p:CallerScope)  =
                    ValidationStatement (callBaseTypeFunc (l.lg.getPointer (chp p).arg)  fncName None, [])
                Some f1, [], [], [], [cvf]
            | None          ->
                let f1 (p:CallerScope)  =  cvf.funcBody (chp p)
                Some f1, cvf.errCodes, cvf.alphaFuncs, cvf.localVariables, []

    let funBody (errCode: ErrorCode) (p:CallerScope) =
        let ii = p.arg.SequenceOfLevel + 1
        let i = sprintf "i%d" ii
        let lv = SequenceOfIndex (ii, None)
        let with_component_check, lllvs = convertMultipleVCBsToStatementAndSetErrorCode l p errCode vcbs |> List.unzip
        let childCheck =
            match childFunc with
            | None -> []
            | Some chFunc ->
                let innerStatement = chFunc p
                match innerStatement with
                | ValidationStatementTrue   (_,_)  -> []
                | ValidationStatementFalse  (st,clv)  -> [sequenceOf (p.arg.joined l.lg) (l.lg.getAccess p.arg) i o.isFixedSize o.minSize.uper st, lv::clv]
                | ValidationStatement       (st,clv)  -> [sequenceOf (p.arg.joined l.lg) (l.lg.getAccess p.arg) i o.isFixedSize o.minSize.uper st, lv::clv]
        let childCheck, lllvs2 = childCheck |> List.unzip
        match (with_component_check@childCheck) |> DAstUtilFunctions.nestItems_ret l  with
        | None   -> convertVCBToStatementAndAssignedErrCode l VCBTrue errCode.errCodeName
        | Some s ->ValidationStatement (s, (lllvs@lllvs2) |> List.collect id)

    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t funBody  typeDefinition childAlphaFuncs childLocalVars childErrCodes nonEmbeddedChildValidFuncs (Some errorCodeComment) ns2


let createSequenceFunction (r:Asn1AcnAst.AstRoot)  (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionOrReference) (children:SeqChildInfo list)  (us:State)  =
    let sequence_OptionalChild           = l.isvalid.Sequence_OptionalChild
    let callBaseTypeFunc                 = l.isvalid.call_base_type_func
    let asn1Children = children |> List.choose(fun c -> match c with Asn1Child x -> Some x | AcnChild _ -> None)
    let handleChild (child:Asn1Child) (us:State) =
        let c_name = l.lg.getAsn1ChildBackendName child
        match child.Type.isValidFunction with
        | None                      -> None, us
        | Some (isValidFunction)    ->
            let func =
                (*component's is validation statement. If the component has a separate function then make a call otherwise embed the code*)
                fun (p:CallerScope)  ->
                    let newArg = l.lg.getSeqChild p.arg c_name child.Type.isIA5String child.Optionality.IsSome
                    let newArg = if l.lg.usesWrappedOptional && newArg.isOptional then newArg.asLast else newArg
                    let chp = {p with arg = newArg}
                    match isValidFunction.funcName with
                    | Some fncName  ->
                        ValidationStatement (callBaseTypeFunc (l.lg.getPointer chp.arg)  fncName None, [])
                    | None ->
                        isValidFunction.funcBody chp
            let childFnc =
                (*handle optionality*)
                match child.Optionality with
                | Some _    ->
                    let newFunc =
                        (fun (p:CallerScope) ->
                            match func p with
                            | ValidationStatementTrue   (st,lv)  -> ValidationStatementTrue (sequence_OptionalChild (p.arg.joined l.lg) (l.lg.getAccess p.arg) c_name st, lv)
                            | ValidationStatementFalse  (st,lv)  -> ValidationStatement (sequence_OptionalChild (p.arg.joined l.lg) (l.lg.getAccess p.arg) c_name st, lv)
                            | ValidationStatement       (st,lv)  -> ValidationStatement (sequence_OptionalChild (p.arg.joined l.lg) (l.lg.getAccess p.arg) c_name st, lv) )
                    newFunc
                | None      -> func
            (*return new local variables, errorcodes or alphaFuncs*)
            match isValidFunction.funcName with
            | Some fncName ->
                Some(IsValidProcCall {|isValidStatement= childFnc; chidIsValidFunction=isValidFunction|}), us
            | None ->
                Some(IsValidEmbedded {|isValidStatement = childFnc; localVars = isValidFunction.localVariables; alphaFuncs = isValidFunction.alphaFuncs; childErrCodes = isValidFunction.errCodes|}), us
    let childrenContent, ns1 =  asn1Children |> Asn1Fold.foldMap (fun us child -> handleChild child us) us
    let childrenContent = childrenContent |> List.choose id
    let childrenErrCodes = childrenContent |> List.collect(fun s -> match s with IsValidEmbedded c -> c.childErrCodes | IsValidProcCall _ -> [])
    let alphaFuncs = childrenContent |> List.collect(fun s -> match s with IsValidEmbedded c -> c.alphaFuncs | IsValidProcCall _ -> []) //childrenContent |> List.collect(fun c -> c.alphaFuncs)
    let localVars = childrenContent |> List.collect(fun s -> match s with IsValidEmbedded c -> c.localVars | IsValidProcCall _ -> []) //childrenContent |> List.collect(fun c -> c.localVars)
    let nonEmbeddedChildrenValidFuncs = childrenContent |> List.choose(fun s -> match s with IsValidEmbedded c -> None | IsValidProcCall c -> Some c.chidIsValidFunction)
    let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.SeqValue) = VCBTrue
    let vcbs, ns2 =  o.cons |> Asn1Fold.foldMap(fun cs c -> sequenceConstraint2ValidationCodeBlock r l t.id asn1Children valToStrFunc  c cs) ns1



    let funBody (errCode: ErrorCode) (p:CallerScope) =
        let childrenChecks, lv =
            let aaa, lv =
                childrenContent |>
                List.choose(fun z ->
                    let isValidStatement = match z with IsValidEmbedded c -> c.isValidStatement | IsValidProcCall c -> c.isValidStatement
                    match isValidStatement p with
                    | ValidationStatementTrue _ -> None
                    | ValidationStatementFalse  (st,lv)
                    | ValidationStatement       (st,lv) -> Some (st, lv)) |>
                List.unzip
            aaa |> DAstUtilFunctions.nestItems_ret l  |> Option.toList, (lv |> List.collect id)
        let with_component_check, lv2 =
            convertMultipleVCBsToStatementAndSetErrorCode l p errCode vcbs |>
            List.unzip
        match (childrenChecks@with_component_check) |> DAstUtilFunctions.nestItems_ret l  with
        | None   -> convertVCBToStatementAndAssignedErrCode l VCBTrue errCode.errCodeName
        | Some s ->ValidationStatement (s, lv@(lv2 |>List.collect id))


    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t funBody  typeDefinition alphaFuncs localVars childrenErrCodes nonEmbeddedChildrenValidFuncs (Some errorCodeComment) ns2

let createChoiceFunction (r:Asn1AcnAst.AstRoot)  (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionOrReference) (defOrRef:TypeDefinitionOrReference) (children:ChChildInfo list) (baseTypeValFunc : IsValidFunction option) (us:State)  =
    let choice_OptionalChild              = l.isvalid.Choice_OptionalChild
    let callBaseTypeFunc                  = l.isvalid.call_base_type_func
    let choice_child                      = l.isvalid.choice_child
    let choice_check_children             = l.isvalid.choice
    let always_true_statement             = l.isvalid.always_true_statement
    let always_false_statement            = l.isvalid.always_false_statement


    let handleChild (child:ChChildInfo) (us:State) =
        let c_name = l.lg.getAsn1ChChildBackendName child
        let presentWhenName = l.lg.presentWhenName (Some defOrRef) child
        match child.chType.isValidFunction with
        | None                      ->
            let childFnc =
                let newFunc =
                    (fun (p:CallerScope) ->
                        ValidationStatement (choice_child presentWhenName (always_true_statement()) false c_name, []))
                newFunc
            Some(IsValidEmbedded {|isValidStatement = childFnc; localVars = []; alphaFuncs = []; childErrCodes = [] |}), us
        | Some (isValidFunction)    ->
            let func =
                (*alternative's is validation statement. If the alternative has a separate function then make a call otherwise embed the code*)
                fun (p:CallerScope)  ->
                    let chp = {p with arg = l.lg.getChChild p.arg c_name child.chType.isIA5String}
                    match isValidFunction.funcName with
                    | Some fncName ->
                        ValidationStatement (callBaseTypeFunc (l.lg.getPointer  chp.arg)  fncName None, [])
                    | None ->
                        isValidFunction.funcBody chp
            let childFnc =
                let newFunc =
                    (fun (p:CallerScope) ->
                        let localTmpVarName =
                            match ST.lang with
                            | Scala -> child._scala_name
                            | _ -> ""
                        match func p with
                        | ValidationStatementTrue   (st,lv)  -> ValidationStatementTrue (choice_child presentWhenName st true c_name, lv)
                        | ValidationStatementFalse   (st,lv)
                        | ValidationStatement   (st,lv)  -> ValidationStatement (choice_child presentWhenName st false c_name, lv) )
                        //| ValidationStatementTrue   (st,lv)  -> ValidationStatementTrue (choice_OptionalChild (p.arg.joined l.lg) localTmpVarName (l.lg.getAccess p.arg) presentWhenName st, lv)
                        //| ValidationStatementFalse  (st,lv)  -> ValidationStatement (choice_OptionalChild (p.arg.joined l.lg) localTmpVarName (l.lg.getAccess p.arg) presentWhenName st, lv)
                        //| ValidationStatement       (st,lv)  -> ValidationStatement (choice_OptionalChild (p.arg.joined l.lg) localTmpVarName (l.lg.getAccess p.arg) presentWhenName st, lv) )
                newFunc
            (*return new local variables, errorcodes or alphaFuncs*)
            match isValidFunction.funcName with
            | None ->
                Some(IsValidEmbedded {|isValidStatement = childFnc; localVars = isValidFunction.localVariables; alphaFuncs = isValidFunction.alphaFuncs; childErrCodes = isValidFunction.errCodes |}), us
            | Some fncName ->
                Some(IsValidProcCall {|isValidStatement = childFnc; chidIsValidFunction=isValidFunction|}), us

    let childrenContent, ns1 =  children |> Asn1Fold.foldMap (fun us child -> handleChild child us) us
    let childrenContent = childrenContent |> List.choose id
    let childrenErrCodes = childrenContent |> List.collect(fun s -> match s with IsValidEmbedded c -> c.childErrCodes | IsValidProcCall _ -> [])
    let alphaFuncs = childrenContent |> List.collect(fun s -> match s with IsValidEmbedded c -> c.alphaFuncs | IsValidProcCall _ -> []) //childrenContent |> List.collect(fun c -> c.alphaFuncs)
    let localVars = childrenContent |> List.collect(fun s -> match s with IsValidEmbedded c -> c.localVars | IsValidProcCall _ -> []) //childrenContent |> List.collect(fun c -> c.localVars)
    let nonEmbeddedChildrenValidFuncs = childrenContent |> List.choose(fun s -> match s with IsValidEmbedded c -> None | IsValidProcCall c -> Some c.chidIsValidFunction)
    let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.ChValue) = VCBTrue
    let vcbs, ns2 =  o.cons |> Asn1Fold.foldMap(fun cs c -> choiceConstraint2ValidationCodeBlock r l t.id children valToStrFunc defOrRef  c cs) ns1
    let funBody (errCode: ErrorCode) (p:CallerScope) =
        let choice_switch_check, lv1 =
            let childrenChecks, lv1 =
                childrenContent |>
                List.choose(fun z ->
                    let isValidStatement = match z with IsValidEmbedded c -> c.isValidStatement | IsValidProcCall c -> c.isValidStatement
                    match isValidStatement p with
                    | ValidationStatementTrue st
                    | ValidationStatementFalse  st
                    | ValidationStatement       st -> Some st) |>
                List.unzip
            //aaa |> DAstUtilFunctions.nestItems_ret l  |> Option.toList, (lv1|> List.collect id)
            let choice_switch_check =
                choice_check_children (p.arg.joined l.lg) (l.lg.getAccess p.arg) childrenChecks errCode.errCodeName

            [choice_switch_check],(lv1|> List.collect id)

        let with_component_check, lv2 =
            let a, b = convertMultipleVCBsToStatementAndSetErrorCode l p errCode vcbs |> List.unzip
            a, (b |> List.collect id)
        match (with_component_check@choice_switch_check) |> DAstUtilFunctions.nestItems_ret l  with
        | None   -> convertVCBToStatementAndAssignedErrCode l VCBTrue errCode.errCodeName
        | Some s ->ValidationStatement (s, lv1@lv2)

    let errorCodeComment = o.cons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""
    createIsValidFunction r l t funBody  typeDefinition alphaFuncs localVars childrenErrCodes nonEmbeddedChildrenValidFuncs (Some errorCodeComment) ns2


let rec createReferenceTypeFunction_this_type (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (refTypeId:ReferenceToType) (refCons:Asn1AcnAst.AnyConstraint list) (typeDefinition:TypeDefinitionOrReference) (resolvedType:Asn1Type)  (us:State)  =
    match resolvedType.Kind with
    | Sequence sq    ->
        let asn1Children = sq.children |> List.choose(fun c -> match c with Asn1Child x -> Some x | AcnChild _ -> None)
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.SeqConstraint z -> Some z | _ -> None )
        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.SeqValue) = VCBTrue
        cons |> Asn1Fold.foldMap(fun cs c -> sequenceConstraint2ValidationCodeBlock r l refTypeId asn1Children valToStrFunc  c cs) us
    | ReferenceType rt    ->
        createReferenceTypeFunction_this_type r l refTypeId refCons typeDefinition rt.resolvedType  us
    | Integer rt ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.IntegerTypeConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> integerConstraint2ValidationCodeBlock r l (rt.baseInfo.intClass) c us) us
    | Real _ ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.RealTypeConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> realConstraint2ValidationCodeBlock l c us) us
    | Boolean _ ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.BoolConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> booleanConstraint2ValidationCodeBlock l c us) us
    | OctetString oc ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.OctetStringConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> octetStringConstraint2ValidationCodeBlock r l  refTypeId oc.baseInfo resolvedType.equalFunction c us) us
    | BitString bs ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.BitStringConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> bitStringConstraint2ValidationCodeBlock r l  refTypeId bs.baseInfo resolvedType.equalFunction c us) us
    | IA5String st ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.IA5StringConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> ia5StringConstraint2ValidationCodeBlock r  l refTypeId c us) {us with alphaIndex=0; alphaFuncs=[]}
    | ObjectIdentifier _ ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.ObjectIdConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> objIdConstraint2ValidationCodeBlock l c us) us
    | TimeType tt   ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.TimeConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> timeConstraint2ValidationCodeBlock l (l.lg.typeDef tt.baseInfo.typeDef) c us) us
    | NullType _    ->
        [],us
    | Enumerated en  ->
        match r.args.isEnumEfficientEnabled en.baseInfo.items.Length with
        | false ->
            let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.EnumConstraint z -> Some z | _ -> None )
            cons |> Asn1Fold.foldMap (fun us c -> enumeratedConstraint2ValidationCodeBlock  l  en.baseInfo typeDefinition c us) us
        | true  -> createEfficientEnumValidation r l en.baseInfo us
    | Choice ch ->
        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.ChValue) = VCBTrue
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.ChoiceConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap(fun cs c -> choiceConstraint2ValidationCodeBlock r l refTypeId ch.children valToStrFunc typeDefinition  c cs) us
    | SequenceOf sqo ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.SequenceOfConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap(fun cs c -> sequenceOfConstraint2ValidationCodeBlock r l refTypeId sqo.baseInfo sqo.childType resolvedType.equalFunction c cs) us


let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionOrReference) (resolvedType:Asn1Type)  (us:State)  =
    let callBaseTypeFunc = l.isvalid.call_base_type_func
    let vcbs,us = createReferenceTypeFunction_this_type r l t.id o.refCons typeDefinition resolvedType us

    let moduleName, typeDefinitionName, baseTypeDefinitionName =
        match typeDefinition with
        | ReferenceToExistingDefinition refToExist   ->
            match refToExist.programUnit with
            | Some md -> md, refToExist.typedefName, refToExist.typedefName
            | None    -> ToC t.id.ModName, refToExist.typedefName, refToExist.typedefName
        | TypeDefinition                tdDef        ->
            match tdDef.baseType with
            | None -> ToC t.id.ModName, tdDef.typedefName, ToC2(r.args.TypePrefix + o.tasName.Value)
            | Some refToExist ->
                match refToExist.programUnit with
                | Some md -> md, refToExist.typedefName, refToExist.typedefName
                | None    -> ToC t.id.ModName, refToExist.typedefName, refToExist.typedefName
    let soTypeCasting =
        let actType = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        match t.ActualType.Kind, actType.Kind with
        | Asn1AcnAst.Integer o,  Asn1AcnAst.Integer res ->
            match o.intClass = res.intClass with
            | true -> None
            | false -> Some typeDefinitionName
        | _         -> None

    let baseFncName =
        match l.lg.hasModules with
        | false     -> baseTypeDefinitionName + "_IsConstraintValid"
        | true   ->
            match t.id.ModName = o.modName.Value with
            | true  -> baseTypeDefinitionName + "_IsConstraintValid"
            | false -> moduleName + "." + baseTypeDefinitionName + "_IsConstraintValid"


    let funBody (errCode: ErrorCode) (p:CallerScope) =
        let with_component_check, lv2 =
            convertMultipleVCBsToStatementAndSetErrorCode l p errCode vcbs |>
            List.unzip

        match resolvedType.isValidFunction with
        | Some _    ->
            let funcBodyContent = callBaseTypeFunc (l.lg.getParamValue t p.arg Encode) baseFncName soTypeCasting
            match (funcBodyContent::with_component_check) |> DAstUtilFunctions.nestItems_ret l  with
            | None   -> convertVCBToStatementAndAssignedErrCode l VCBTrue errCode.errCodeName
            | Some s ->ValidationStatement (s, (lv2 |>List.collect id))
        | None      ->
            match (with_component_check) |> DAstUtilFunctions.nestItems_ret l  with
            | None   -> convertVCBToStatementAndAssignedErrCode l VCBTrue errCode.errCodeName
            | Some s ->ValidationStatement (s, (lv2 |>List.collect id))

    let errorCodeComment = o.refCons |> List.map(fun z -> z.ASN1) |> Seq.StrJoin ""

    createIsValidFunction r l t funBody  typeDefinition [] [] [] [] (Some errorCodeComment) us


