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
    | VCBStatement (sStat,_) -> sStat


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
    | VCBExpression sExp1, VCBStatement (sStat2, lv2)    -> VCBStatement(expressionOrStament sExp1 sStat2, lv2)
    | VCBStatement (sStat1, lv1), VCBExpression sExp2    -> VCBStatement(statementOrExpression sStat1 sExp2, lv1)
    | VCBStatement (sStat1, lv1), VCBStatement (sStat2,lv2)    -> VCBStatement(statementOrStament sStat1 sStat2, lv1@lv2)

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
    | VCBExpression sExp1, VCBStatement (sStat2,lv2)    -> VCBStatement(expressionAndStament sExp1 sStat2, lv2)
    | VCBStatement (sStat1, lv1), VCBExpression sExp2    -> VCBStatement(statementAndExpression sStat1 sExp2, lv1)
    | VCBStatement (sStat1, lv1), VCBStatement (sStat2,lv2)    -> VCBStatement(statementAndStament sStat1 sStat2, lv1@lv2)

let ValidationCodeBlock_Multiple_And (l:ProgrammingLanguage) (vpList:ValidationCodeBlock list)  =
    let makeExpressionToStatement0  = match l with C -> isvalid_c.makeExpressionToStatement0 | Ada -> isvalid_a.makeExpressionToStatement0

    match vpList |> Seq.exists((=) VCBFalse) with
    | true  -> VCBFalse
    | false ->
        let vpList = vpList |> List.filter (fun z -> match z with VCBExpression _ -> true | VCBStatement _ -> true | _ -> false )
        let bAllAreExpressions = false//vpList |> Seq.forall(fun z -> match z with VCBExpression _ -> true | _ -> false )
        match bAllAreExpressions with
        | true  -> VCBExpression (l.ExpAndMulti (vpList |> List.map(fun z -> match z with VCBExpression s -> s | VCBStatement (s,_) -> s | _ -> "invalid")))
        | false -> 
            let soJoinedStatement, lv =
                let children, lv = 
                    vpList |>
                    List.map(fun z -> match z with VCBExpression s -> makeExpressionToStatement0 s, [] | VCBStatement s -> s | _ -> "invalid", []) |>
                    List.unzip
                DAstUtilFunctions.nestItems l  "ret" children, lv
            match soJoinedStatement with
            | Some s    -> VCBStatement (s, lv |> List.collect id)
            | None      -> VCBTrue

let ValidationCodeBlock_Not (l:ProgrammingLanguage) vp =
    let statementNot  = match l with C -> isvalid_c.StatementNot | Ada -> isvalid_a.StatementNot
    match vp with
    | VCBTrue        -> VCBFalse
    | VCBFalse       -> VCBTrue
    | VCBExpression sExp -> VCBExpression (l.ExpNot sExp)
    | VCBStatement (sStat, lv) -> VCBStatement ((statementNot sStat), lv)


let convertVCBToStatementAndAssigneErrCode (l:ProgrammingLanguage) vp  sErrCode =
    let convertVCBExpressionToStatementAndUpdateErrCode     = match l with C -> isvalid_c.convertVCBExpressionToStatementAndUpdateErrCode   | Ada -> isvalid_a.convertVCBExpressionToStatementAndUpdateErrCode
    let convertVCBStatementToStatementAndUpdateErrCode      = match l with C -> isvalid_c.convertVCBStatementToStatementAndUpdateErrCode    | Ada -> isvalid_a.convertVCBStatementToStatementAndUpdateErrCode
    let convertVCBTRUEToStatementAndUpdateErrCode           = match l with C -> isvalid_c.convertVCBTRUEToStatementAndUpdateErrCode         | Ada -> isvalid_a.convertVCBTRUEToStatementAndUpdateErrCode
    let convertVCBFalseToStatementAndUpdateErrCode          = match l with C -> isvalid_c.convertVCBFalseToStatementAndUpdateErrCode        | Ada -> isvalid_a.convertVCBFalseToStatementAndUpdateErrCode
    match vp with
    | VCBTrue        -> ValidationStatementTrue (convertVCBTRUEToStatementAndUpdateErrCode (), [])
    | VCBFalse       -> ValidationStatementFalse (convertVCBFalseToStatementAndUpdateErrCode sErrCode, [])
    | VCBExpression sExp -> ValidationStatement (convertVCBExpressionToStatementAndUpdateErrCode sExp sErrCode, [])
    | VCBStatement (sStat,lv) -> ValidationStatement (convertVCBStatementToStatementAndUpdateErrCode sStat sErrCode, lv)


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
    | VCBExpression sExp1, VCBStatement (sStat2, lv2)    -> VCBStatement(expressionExceptStament sExp1 sStat2, lv2)
    | VCBStatement (sStat1, lv1), VCBExpression sExp2    -> VCBStatement(statementExceptExpression sStat1 sExp2, lv1)
    | VCBStatement (sStat1, lv1), VCBStatement (sStat2, lv2)    -> VCBStatement(statementExceptStament sStat1 sStat2, lv1@lv2)


let Lte (l:ProgrammingLanguage) eqIsInc  e1 e2 =
    match eqIsInc with
    | true   -> l.ExpLte e1 e2        
    | false  -> l.ExpLt  e1 e2


let con_or l (e1 : CallerScope -> ValidationCodeBlock) (e2 : CallerScope -> ValidationCodeBlock) b s =  (fun p -> ValidationCodeBlock_OR l (e1 p) (e2 p)), s
let con_and l (e1 : CallerScope -> ValidationCodeBlock) (e2 : CallerScope -> ValidationCodeBlock) s =  (fun p -> ValidationCodeBlock_AND l (e1 p) (e2 p)), s
let con_not l (e : CallerScope -> ValidationCodeBlock)  s =  (fun p -> ValidationCodeBlock_Not l (e p)), s
let con_ecxept l (e1 : CallerScope -> ValidationCodeBlock) (e2 : CallerScope -> ValidationCodeBlock) s =  (fun p -> ValidationCodeBlock_Except l (e1 p) (e2 p)), s
let con_root e s = e,s
let con_root2 l (e1 : CallerScope -> ValidationCodeBlock) (e2 : CallerScope -> ValidationCodeBlock)  s =  (fun p -> ValidationCodeBlock_OR l (e1 p) (e2 p)), s

(*  e.g. INTEGER (v1..MAX)  ==>          v1 <= p.p   *)
let con_rangeContraint_val_MAX  (l:ProgrammingLanguage) minint maxint v1 eqIsInc valToStrFunc pToStrFunc =
    match v1 < minint with
    | true                              -> fun (p:CallerScope) -> VCBTrue  (* e.g. for unsigned integer (-40 .. MAX) *)
    | false when v1 = minint && eqIsInc -> fun (p:CallerScope) -> VCBTrue
    | false                             -> fun (p:CallerScope) -> (VCBExpression (Lte l eqIsInc (valToStrFunc v1) (pToStrFunc l p (*p.arg.getValue l*) )))


(* e.g INTEGER (MIN .. v1) --> p.p <= v1 *)
let con_angeContraint_MIN_val (l:ProgrammingLanguage) minint maxint v1 eqIsInc valToStrFunc pToStrFunc =
    match v1 > maxint with
    | true                              -> fun (p:CallerScope) -> VCBTrue  (* e.g. for unsigned integer (MIN .. value_larger_than_maxint) *)
    | false when v1 = maxint && eqIsInc -> fun (p:CallerScope) -> VCBTrue
    | false                             -> fun (p:CallerScope) -> (VCBExpression (Lte l eqIsInc (pToStrFunc l p) (valToStrFunc v1)))


let foldGenericCon (l:ProgrammingLanguage) valToStrFunc  (c:GenericConstraint<'v>)  st =
    foldGenericConstraint (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s           -> (fun p -> VCBExpression (l.ExpEqual (p.arg.getValue l) (valToStrFunc v))) ,s)
        c
        st


let foldSizeRangeTypeConstraint (r:Asn1AcnAst.AstRoot)  (l:ProgrammingLanguage)  getSizeFunc  (c:PosIntTypeConstraint) st = 
    let valToStrFunc (v:UInt32) = v.ToString()
    foldRangeTypeConstraint (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s         -> (fun p -> VCBExpression (l.ExpEqual (getSizeFunc l p) (valToStrFunc  v))) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            let exp1 = con_rangeContraint_val_MAX  l UInt32.MinValue UInt32.MaxValue v1 minIsIn valToStrFunc getSizeFunc
            let exp2 = con_angeContraint_MIN_val  l UInt32.MinValue UInt32.MaxValue v2 maxIsIn valToStrFunc getSizeFunc
            con_and l exp1 exp2 s)
        (fun v1 minIsIn s   -> con_rangeContraint_val_MAX  l UInt32.MinValue UInt32.MaxValue v1 minIsIn valToStrFunc getSizeFunc, s)
        (fun v2 maxIsIn s   -> con_angeContraint_MIN_val  l UInt32.MinValue UInt32.MaxValue v2 maxIsIn valToStrFunc getSizeFunc, s)
        c
        st


let foldSizableConstraint (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) hasCount compareSingValueFunc  getSizeFunc  (c:SizableTypeConstraint<'v>) st =
    foldSizableTypeConstraint2 (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s           -> (fun p -> compareSingValueFunc p v) ,s)
        (fun intCon s       -> 
            match hasCount with
            | true  -> foldSizeRangeTypeConstraint r l getSizeFunc intCon s
            | false -> (fun p -> VCBTrue), s)
        c
        st

let ia5StringConstraint2ValidationCodeBlock  (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType)   (c:IA5StringConstraint) (us0:State) =
    let print_AlphabetCheckFunc = match l with C -> isvalid_c.Print_AlphabetCheckFunc    | Ada -> isvalid_a.Print_AlphabetCheckFunc 
    let stringContainsChar      = match l with C -> isvalid_c.stringContainsChar         | Ada -> isvalid_a.stringContainsChar

    let alphaFuncName = ToC (((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_CharsAreValid")
    let foldRangeCharCon (l:ProgrammingLanguage)   (c:CharTypeConstraint)  st =
        let valToStrFunc1 v = v.ToString().ISQ
        foldRangeTypeConstraint   (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
            (fun (v:string)  s  -> (fun p -> VCBExpression( stringContainsChar v.IDQ p.arg.p)) ,s)
            (fun v1 v2  minIsIn maxIsIn s   -> 
                (fun p -> VCBExpression(l.ExpAnd (Lte l minIsIn (valToStrFunc1 v1) (p.arg.getValue l)) (Lte l maxIsIn (p.arg.getValue l) (valToStrFunc1 v2)))), s)
            (fun v1 minIsIn s   -> (fun p -> VCBExpression( Lte l minIsIn (valToStrFunc1 v1) (p.arg.getValue l))), s)
            (fun v2 maxIsIn s   -> (fun p -> VCBExpression(Lte l maxIsIn (p.arg.getValue l) (valToStrFunc1 v2))), s)
            c
            st 

    foldStringTypeConstraint2 (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s         -> (fun p -> VCBExpression (l.ExpStringEqual p.arg.p v.IDQ))  ,s)
        (fun intCon s     -> foldSizeRangeTypeConstraint r l (fun l p -> l.StrLen p.arg.p) intCon s)
        (fun alphcon s      -> 
            let alphaBody p = 
                let alphaFunc = foldRangeCharCon l  alphcon 0 |> fst 
                match alphaFunc p with
                | VCBExpression x   -> x
                | VCBStatement (x,_)    -> x
                | VCBTrue           -> "TRUE"
                | VCBFalse          -> "FALSE"
                                
            let alphaFuncName = sprintf "%s_%d" alphaFuncName s.alphaIndex
            let funcBody p = print_AlphabetCheckFunc alphaFuncName [alphaBody p]
            let alphFunc = {AlphaFunc.funcName = alphaFuncName; funcBody = funcBody }

            let newState = {s with alphaIndex = s.alphaIndex + 1; alphaFuncs = alphFunc::s.alphaFuncs}
            let retFnc p = VCBExpression (sprintf "%s(%s)" alphaFuncName p.arg.p)
            retFnc, newState) 
        c
        us0 




 

let integerConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) isUnsigned  (c:IntegerTypeConstraint) st =
    let valToStrFunc  (i:BigInteger) = 
        match l with
        | Ada   -> i.ToString()
        | C     ->
            match isUnsigned with
            | true   -> sprintf "%sUL" (i.ToString())
            | false  -> sprintf "%sLL" (i.ToString())
    let p2StrFunc l (p:CallerScope) = p.arg.getValue l
    foldRangeTypeConstraint (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s         -> (fun p -> VCBExpression (l.ExpEqual (p.arg.getValue l) (valToStrFunc  v))) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            let exp1 = con_rangeContraint_val_MAX  l (r.args.IntMin isUnsigned) (r.args.IntMax isUnsigned) v1 minIsIn valToStrFunc p2StrFunc
            let exp2 = con_angeContraint_MIN_val  l (r.args.IntMin isUnsigned) (r.args.IntMax isUnsigned) v2 maxIsIn valToStrFunc p2StrFunc
            con_and l exp1 exp2 s)
        (fun v1 minIsIn s   -> con_rangeContraint_val_MAX  l (r.args.IntMin isUnsigned) (r.args.IntMax isUnsigned) v1 minIsIn valToStrFunc p2StrFunc, s)
        (fun v2 maxIsIn s   -> con_angeContraint_MIN_val  l (r.args.IntMin isUnsigned) (r.args.IntMax isUnsigned) v2 maxIsIn valToStrFunc p2StrFunc, s)
        c
        st


let realConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (c:RealTypeConstraint) st =
    let valToStrFunc (v:double) = v.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo)
    foldRangeTypeConstraint  (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)      
        (fun v  s         -> (fun p -> VCBExpression(l.ExpEqual (p.arg.getValue l) (valToStrFunc  v))) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            (fun p -> VCBExpression (l.ExpAnd (Lte l minIsIn (valToStrFunc v1) (p.arg.getValue l)) (Lte l maxIsIn (p.arg.getValue l) (valToStrFunc v2)))), s)
        (fun v1 minIsIn s   -> (fun p -> VCBExpression(Lte l minIsIn (valToStrFunc v1) (p.arg.getValue l))), s)
        (fun v2 maxIsIn s   -> (fun p -> VCBExpression(Lte l maxIsIn (p.arg.getValue l) (valToStrFunc v2))), s)
        c
        st


let booleanConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (c:BoolConstraint) st =
    foldGenericCon l  (fun v -> v.ToString().ToLower()) c st
    

let objIdConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (c:ObjectIdConstraint) st =
    let objId_equal = match l with C -> isvalid_c.objId_equal | Ada -> isvalid_a.objId_equal
    let printObjectIdentifierValue = match l with C -> variables_c.PrintObjectIdentifierValueAsCompoundLiteral | Ada -> variables_a.PrintObjectIdentifierValueAsCompoundLiteral
    
    foldGenericConstraint (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun (a,b)  s           -> 
            let v =  Asn1DefinedObjectIdentifierValue(a,b)
            (fun (p:CallerScope) -> 
                let lit = printObjectIdentifierValue (v.Values |> List.map fst) (BigInteger v.Values.Length)
                VCBExpression (objId_equal p.arg.p lit)) ,s)
        c
        st
    
let timeConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (td) (c:TimeConstraint) st =
    let print_Asn1LocalTime                  = match l with C -> variables_c.PrintTimeValueAsCompoundLiteral_Asn1LocalTime                   | Ada -> variables_a.PrintTimeValueAsCompoundLiteral_Asn1LocalTime
    let print_Asn1UtcTime                    = match l with C -> variables_c.PrintTimeValueAsCompoundLiteral_Asn1UtcTime                     | Ada -> variables_a.PrintTimeValueAsCompoundLiteral_Asn1UtcTime
    let print_Asn1LocalTimeWithTimeZone      = match l with C -> variables_c.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone       | Ada -> variables_a.PrintTimeValueAsCompoundLiteral_Asn1LocalTimeWithTimeZone
    let print_Asn1Date                       = match l with C -> variables_c.PrintTimeValueAsCompoundLiteral_Asn1Date                        | Ada -> variables_a.PrintTimeValueAsCompoundLiteral_Asn1Date
    let print_Asn1Date_LocalTime             = match l with C -> variables_c.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime              | Ada -> variables_a.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTime
    let print_Asn1Date_UtcTime               = match l with C -> variables_c.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime                | Ada -> variables_a.PrintTimeValueAsCompoundLiteral_Asn1Date_UtcTime
    let print_Asn1Date_LocalTimeWithTimeZone = match l with C -> variables_c.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone  | Ada -> variables_a.PrintTimeValueAsCompoundLiteral_Asn1Date_LocalTimeWithTimeZone

    let printValueAsLiteral  (iv:Asn1DateTimeValue) = 
        match iv with
        |Asn1LocalTimeValue                  tv        -> print_Asn1LocalTime td tv
        |Asn1UtcTimeValue                    tv        -> print_Asn1UtcTime td tv
        |Asn1LocalTimeWithTimeZoneValue      (tv,tz)   -> print_Asn1LocalTimeWithTimeZone td tv tz
        |Asn1DateValue                       dt        -> print_Asn1Date td dt
        |Asn1Date_LocalTimeValue             (dt,tv)   -> print_Asn1Date_LocalTime td dt tv
        |Asn1Date_UtcTimeValue               (dt,tv)   -> print_Asn1Date_UtcTime  td dt tv
        |Asn1Date_LocalTimeWithTimeZoneValue (dt,tv,tz)-> print_Asn1Date_LocalTimeWithTimeZone td dt tv tz

    foldGenericConstraint (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s           -> 
            (fun (p:CallerScope) -> 
                let lit = printValueAsLiteral v
                VCBExpression (l.ExpEqual p.arg.p lit)) ,s)
        c
        st


let enumeratedConstraint2ValidationCodeBlock (l:ProgrammingLanguage) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference) (c:EnumConstraint) st =
    let printNamedItem  (v:string) =
        let itm = o.items |> Seq.find (fun x -> x.Name.Value = v)
        let ret = itm.getBackendName (Some typeDefinition ) l
        ret
    foldGenericCon l  printNamedItem c st

let octetStringConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (o:Asn1AcnAst.OctetString) (equalFunc:EqualFunction) (c:OctetStringConstraint) st =
    let getSizeFunc (l:ProgrammingLanguage) p = l.Length p.arg.p (p.arg.getAcces l)
    let compareSingleValueFunc (p:CallerScope) (v:Asn1AcnAst.OctetStringValue, (id,loc))  = 
        let octet_var_string_equal = match l with C -> isvalid_c.octet_var_string_equal | Ada -> isvalid_a.octet_var_string_equal
        let octet_fix_string_equal = match l with C -> isvalid_c.octet_fix_string_equal | Ada -> isvalid_a.octet_fix_string_equal
        let printOctetArrayAsCompoundLitteral = match l with C -> variables_c.PrintOctetArrayAsCompoundLitteral | Ada -> variables_a.PrintOctetArrayAsCompoundLitteral
        let octArrLiteral = printOctetArrayAsCompoundLitteral  (v |> List.map (fun b -> b.Value))
        match o.isFixedSize with
        | true   -> VCBExpression (octet_fix_string_equal p.arg.p (p.arg.getAcces l) o.minSize.uper (v.Length.AsBigInt) octArrLiteral)
        | false  -> VCBExpression (octet_var_string_equal p.arg.p (p.arg.getAcces l)  (v.Length.AsBigInt) octArrLiteral)
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
        | true   -> VCBExpression (bit_fix_string_equal p.arg.p (p.arg.getAcces l) o.minSize.uper (v.Value.Length.AsBigInt) octArrLiteral bitArrLiteral)
        | false  -> VCBExpression (bit_var_string_equal p.arg.p (p.arg.getAcces l)  (v.Value.Length.AsBigInt) octArrLiteral bitArrLiteral)
    let fnc, ns = foldSizableConstraint r l (not o.isFixedSize) compareSingleValueFunc getSizeFunc c st
    fnc, ns


let rec anyConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (erLoc:SrcLoc) (t:Asn1Type) (ac:AnyConstraint) st =
    match t.ActualType.Kind, ac with
    | Integer o, IntegerTypeConstraint c        -> integerConstraint2ValidationCodeBlock r l o.baseInfo.isUnsigned c st
    | Real o, RealTypeConstraint   c            -> realConstraint2ValidationCodeBlock l c st
    | IA5String  o, IA5StringConstraint c       -> ia5StringConstraint2ValidationCodeBlock r l t.id  c st
    | OctetString o, OctetStringConstraint c    -> octetStringConstraint2ValidationCodeBlock r l  t.id o.baseInfo  o.equalFunction c st
    | BitString o, BitStringConstraint c        -> bitStringConstraint2ValidationCodeBlock r l  t.id o.baseInfo o.equalFunction c st
    | NullType o, NullConstraint                -> (fun p -> VCBTrue), st
    | Boolean o, BoolConstraint c               -> booleanConstraint2ValidationCodeBlock l c st
    | Enumerated o, EnumConstraint c            -> enumeratedConstraint2ValidationCodeBlock  l  o.baseInfo o.definitionOrRef c st
    | ObjectIdentifier o, ObjectIdConstraint c  -> objIdConstraint2ValidationCodeBlock l c st
    | TimeType o, TimeConstraint c              -> timeConstraint2ValidationCodeBlock l (o.baseInfo.typeDef.[l]) c st
    | Sequence o, SeqConstraint c               -> 
        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.SeqValue) = VCBTrue //currently single value constraints are ignored.
        sequenceConstraint2ValidationCodeBlock r l t.id o.Asn1Children valToStrFunc  c st
    | SequenceOf o, SequenceOfConstraint c      -> sequenceOfConstraint2ValidationCodeBlock r l t.id o.baseInfo o.childType o.equalFunction c st
    | Choice o, ChoiceConstraint c              -> 
        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.ChValue) = VCBTrue //currently single value constraints are ignored.
        choiceConstraint2ValidationCodeBlock r l t.id o.children valToStrFunc o.definitionOrRef c st
    | _                                         -> raise(SemanticError(erLoc, "Invalid combination of type/constraint type"))
    
and sequenceConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (children:Asn1Child list) valToStrFunc    (c:SeqConstraint)  st =
    let child_always_present_or_absentExp   = match l with C -> isvalid_c.Sequence_optional_child_always_present_or_absent_expr     | Ada -> isvalid_a.Sequence_optional_child_always_present_or_absent_expr
    let sequence_OptionalChild              = match l with C -> isvalid_c.Sequence_OptionalChild                                    | Ada -> isvalid_a.Sequence_OptionalChild
    let expressionToStament                 = match l with C -> isvalid_c.ExpressionToStament                                       | Ada -> isvalid_a.ExpressionToStament

    let handleNamedConstraint curState (nc:NamedConstraint) = 
        let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)
                    
        let childCheck, ns =
            match nc.Contraint with
            | None       -> (fun p -> VCBTrue), curState
            | Some ac    ->
                let fnc, ns = anyConstraint2ValidationCodeBlock r l nc.Name.Location ch.Type ac curState
                (fun p -> 
                    let chp = {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String}
                    fnc chp), ns
                    
        let childCheck =
            match ch.Optionality with
            | None      -> childCheck
            | Some _    -> 
                let newChidlCheckFnc (p:CallerScope) = 
                    match childCheck p with
                    | VCBExpression  exp -> VCBStatement (sequence_OptionalChild p.arg.p (p.arg.getAcces l) (ch.getBackendName l) (expressionToStament exp), [])
                    | VCBStatement   (stat, lv1)-> VCBStatement (sequence_OptionalChild p.arg.p (p.arg.getAcces l) (ch.getBackendName l) stat, lv1)
                    | VCBTrue            -> VCBTrue
                    | VCBFalse           -> VCBStatement (sequence_OptionalChild p.arg.p (p.arg.getAcces l) (ch.getBackendName l) (expressionToStament "FALSE"), [])

                newChidlCheckFnc

        let presentAbsent =
            match nc.Mark with
            | Asn1Ast.NoMark        -> []
            | Asn1Ast.MarkOptional  -> []
            | Asn1Ast.MarkAbsent    -> 
                let isExp = (fun (p:CallerScope) -> VCBExpression (child_always_present_or_absentExp p.arg.p (p.arg.getAcces l) (ch.getBackendName l)  "0"))
                [isExp]
            | Asn1Ast.MarkPresent    -> 
                let isExp = (fun (p:CallerScope) -> VCBExpression (child_always_present_or_absentExp p.arg.p (p.arg.getAcces l) (ch.getBackendName l)  "1"))
                [isExp]

        presentAbsent@[childCheck], ns

    foldSeqConstraint (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s           ->   (fun p -> valToStrFunc p v) ,s)      
        (fun ncs  s         -> 
            let withComponentItems, ns =  ncs |> Asn1Fold.foldMap handleNamedConstraint s 
            let withComponentItems = withComponentItems |> List.collect id
            let fnc p =  
                ValidationCodeBlock_Multiple_And l (withComponentItems |> List.map (fun fnc -> fnc p))
            fnc, ns)
        c
        st



and choiceConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (children:ChChildInfo list) valToStrFunc (defOrRef:TypeDefintionOrReference)     (c:ChoiceConstraint)  st =
    let choice_OptionalChild              = match l with C -> isvalid_c.Choice_OptionalChild                                  | Ada -> isvalid_a.Choice_OptionalChild
    let expressionToStament               = match l with C -> isvalid_c.ExpressionToStament                                   | Ada -> isvalid_a.ExpressionToStament

    let choice_child_always_present_Exp   = match l with C -> isvalid_c.Choice_child_always_present_Exp                       | Ada -> isvalid_a.Choice_child_always_present_Exp
    let choice_child_always_absent_Exp    = match l with C -> isvalid_c.Choice_child_always_absent_Exp                        | Ada -> isvalid_a.Choice_child_always_absent_Exp


    let handleNamedConstraint curState (nc:NamedConstraint) = 
        let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)
        let presentWhenName = ch.presentWhenName (Some defOrRef) l
        let childCheck, ns =
            match nc.Contraint with
            | None       -> (fun p -> VCBTrue), curState
            | Some ac    ->
                let fnc, ns = anyConstraint2ValidationCodeBlock r l nc.Name.Location ch.chType ac curState
                (fun p -> 
                    let chp = {p with arg = p.arg.getChChild l (ch.getBackendName l) ch.chType.isIA5String}
                    fnc chp), ns
                    
        let childCheck =
            let newChidlCheckFnc (p:CallerScope) = 
                match childCheck p with
                | VCBExpression  exp -> VCBStatement (choice_OptionalChild p.arg.p (p.arg.getAcces l) presentWhenName (expressionToStament exp), [])
                | VCBStatement   (stat, lv1)-> VCBStatement (choice_OptionalChild p.arg.p (p.arg.getAcces l) presentWhenName stat, lv1)
                | VCBTrue            -> VCBTrue
                | VCBFalse           -> VCBStatement (choice_OptionalChild p.arg.p (p.arg.getAcces l) presentWhenName (expressionToStament "FALSE"), [])

            newChidlCheckFnc

        let presentAbsent =
            match nc.Mark with
            | Asn1Ast.NoMark        -> []
            | Asn1Ast.MarkOptional  -> []
            | Asn1Ast.MarkAbsent    -> 
                let isExp = (fun (p:CallerScope) -> VCBExpression (choice_child_always_absent_Exp p.arg.p (p.arg.getAcces l) presentWhenName  ))
                [isExp]
            | Asn1Ast.MarkPresent    -> 
                let isExp = (fun (p:CallerScope) -> VCBExpression (choice_child_always_present_Exp p.arg.p (p.arg.getAcces l) presentWhenName ))
                [isExp]

        presentAbsent@[childCheck], ns


    foldChoiceConstraint (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s           ->   (fun p -> valToStrFunc p v) ,s)      
        (fun ncs  s         -> 
            let withComponentItems, ns =  ncs |> Asn1Fold.foldMap handleNamedConstraint s 
            let withComponentItems = withComponentItems |> List.collect id
            let fnc p =  
                ValidationCodeBlock_Multiple_And l (withComponentItems |> List.map (fun fnc -> fnc p))
            fnc, ns)
        c
        st

and sequenceOfConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (o:Asn1AcnAst.SequenceOf) (child:Asn1Type) (equalFunc:EqualFunction) (c:SequenceOfConstraint) st =
    
    let ii = typeId.SeqeuenceOfLevel + 1
    let i = sprintf "i%d" ii
    let lv = SequenceOfIndex (ii, None)
    let expressionToStament              = match l with C -> isvalid_c.ExpressionToStament                            | Ada -> isvalid_a.ExpressionToStament
    let statementForLoop                 = match l with C -> isvalid_c.StatementForLoop                            | Ada -> isvalid_a.StatementForLoop

    let getSizeFunc (l:ProgrammingLanguage) p = l.Length p.arg.p (p.arg.getAcces l)
    let compareSingleValueFunc (p:CallerScope) (v:Asn1AcnAst.SeqOfValue)  = 
        VCBTrue
    foldSequenceOfTypeConstraint2 (con_or l) (con_and l) (con_not l) (con_ecxept l) con_root (con_root2 l)
        (fun v  s           -> (fun p -> compareSingleValueFunc p v) ,s)
        (fun intCon s       -> 
            match o.isFixedSize with
            | false  -> foldSizeRangeTypeConstraint r l getSizeFunc intCon s
            | true  -> (fun p -> VCBTrue), s)
        (fun c loc s -> 
            (fun p -> 
                let fnc, ns = anyConstraint2ValidationCodeBlock r l loc child c s
                let childCheck p = fnc ({p with arg = p.arg.getArrayItem l i child.isIA5String})
                let ret = 
                    match childCheck p with                    
                    | VCBExpression  exp -> VCBStatement (statementForLoop p.arg.p (p.arg.getAcces l) i o.isFixedSize o.minSize.uper (expressionToStament exp), [lv])
                    | VCBStatement   (stat, lv2)-> VCBStatement (statementForLoop p.arg.p (p.arg.getAcces l) i o.isFixedSize o.minSize.uper stat, lv::lv2)
                    | VCBTrue            -> VCBTrue
                    | VCBFalse           -> VCBStatement (statementForLoop p.arg.p (p.arg.getAcces l) i o.isFixedSize o.minSize.uper (expressionToStament "FALSE"), [lv])
                ret), s) 
        c
        st



let getIntSimplifiedConstraints (r:Asn1AcnAst.AstRoot) isUnsigned (allCons  : IntegerTypeConstraint list) =
    allCons
    
let hasValidationFunc allCons =
    match allCons with
    | []      -> false
    | _       -> true

(*
let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeId:ReferenceToType) (td:FE_TypeDefinition) =
    match typeId.tasInfo with
    | None -> None
    | Some _ -> Some (td.typeName + "_IsConstraintValid")
*)
let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (typeDefinition:TypeDefintionOrReference) =
    getFuncNameGeneric  typeDefinition "_IsConstraintValid"


let str_p (l:ProgrammingLanguage) (typeid:ReferenceToType) = ({CallerScope.modName = typeid.ModName; arg = VALUE (sprintf "str%s" (l.ArrayAccess "i"))})

type IsValidAux = {
    isValidStatement  : CallerScope -> ValidationStatement
    localVars         : LocalVariable list
    alphaFuncs        : AlphaFunc list
    childErrCodes     : ErroCode list

}

type SeqCompIsValidAux = 
    | IsValidEmbedded of {|
                            isValidStatement  : CallerScope -> ValidationStatement
                            localVars         : LocalVariable list
                            alphaFuncs        : AlphaFunc list
                            childErrCodes     : ErroCode list
                         |}
    | IsValidProcCall of {|
                            isValidStatement    : CallerScope -> ValidationStatement
                            chidIsValidFunction : IsValidFunction
                        |}


let createIsValidFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type)  (fncBodyE : ErroCode->CallerScope->ValidationStatement) (typeDefinition:TypeDefintionOrReference) (alphaFuncs : AlphaFunc list) (localVars : LocalVariable list) (childErrCodes : ErroCode list) nonEmbeddedChildrenValidFuncs (us:State)  =
    //let hasValidationFunc= hasValidationFunc allCons
    let emitTasFnc    = match l with C -> isvalid_c.EmitTypeAssignment_composite                | Ada -> isvalid_a.EmitTypeAssignment_composite
    let emitTasFncDef = match l with C -> isvalid_c.EmitTypeAssignment_composite_def            | Ada -> isvalid_a.EmitTypeAssignment_composite_def
    let defErrCode    = match l with C -> isvalid_c.EmitTypeAssignment_composite_def_err_code   | Ada -> isvalid_a.EmitTypeAssignment_composite_def_err_code

    let funcName            = getFuncName r l typeDefinition //t.id (t.FT_TypeDefintion.[l])
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
                let statement, stLVs, bUnreferenced = 
                    match funcBody p with
                    | ValidationStatementTrue   (st,lv)
                    | ValidationStatementFalse  (st,lv) ->  st, lv, true
                    | ValidationStatement       (st,lv)  -> st, lv, false
                let lvars = (stLVs@localVars) |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                let fnc = emitTasFnc varName sStar funcName  (typeDefinition.longTypedefName l) statement  (alphaFuncs |> List.map(fun x -> x.funcBody (str_p l t.id))) lvars bUnreferenced
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
            nonEmbeddedChildrenValidFuncs          = nonEmbeddedChildrenValidFuncs
        }    
    Some ret, ns

let funcBody l fncs (e:ErroCode) (p:CallerScope) = 
    let combinedVcb = fncs |> List.map (fun fnc -> fnc p) |> (ValidationCodeBlock_Multiple_And l)
    convertVCBToStatementAndAssigneErrCode l combinedVcb e.errCodeName

let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    //let allCons = getIntSimplifiedConstraints r o.isUnsigned o.AllCons
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> integerConstraint2ValidationCodeBlock r l o.isUnsigned c us) us
    createIsValidFunction r l t  (funcBody l fncs) typeDefinition [] [] [] [] ns

let createIntegerFunctionByCons (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) isUnsigned (allCons  : IntegerTypeConstraint list) =
    //let allCons = getIntSimplifiedConstraints r isUnsigned allCons
    match allCons with
    | []        -> None
    | _         ->
        let fncs, ns = allCons |> Asn1Fold.foldMap (fun us c -> integerConstraint2ValidationCodeBlock r l isUnsigned c us) 0
        let funcExp (p:CallerScope) = 
            let vp = fncs |> List.map (fun fnc -> fnc p) |> (ValidationCodeBlock_Multiple_And l)        
            vp
//            match vp with
//            | VCBTrue        -> "true"
//            | VCBFalse       -> "false"
//            | VCBExpression sExp -> sExp
//            | VCBStatement sStat -> sStat
        Some funcExp

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference)  (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> realConstraint2ValidationCodeBlock l c us) us
    createIsValidFunction r l t (funcBody l fncs) typeDefinition [] [] [] [] ns

let createBoolFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> booleanConstraint2ValidationCodeBlock l c us) us
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] ns

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference) (equalFunc:EqualFunction) (printValue  : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> octetStringConstraint2ValidationCodeBlock r l  t.id o equalFunc c us) us
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] ns

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (equalFunc:EqualFunction) (printValue  : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> bitStringConstraint2ValidationCodeBlock r l  t.id o equalFunc c us) us
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] ns

let createStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> ia5StringConstraint2ValidationCodeBlock r  l t.id c us) {us with alphaIndex=0; alphaFuncs=[]}
    createIsValidFunction r l t (funcBody l fncs) typeDefinition ns.alphaFuncs [] [] [] ns

let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let conToStrFunc_basic (p:CallerScope)  = 
        let namespacePrefix = match l with C -> "" | Ada -> "adaasn1rtl."
        match o.relativeObjectId with
        | false -> VCBExpression (sprintf "%sObjectIdentifier_isValid(%s)" namespacePrefix (p.arg.getPointer l))
        | true  -> VCBExpression (sprintf "%sRelativeOID_isValid(%s)" namespacePrefix (p.arg.getPointer l))

    let fnc, ns = o.cons |> Asn1Fold.foldMap (fun us c -> objIdConstraint2ValidationCodeBlock l c us) us
    let fncs = conToStrFunc_basic::fnc
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] ns


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefintionOrReference) (us:State)  =
    let conToStrFunc_basic (p:CallerScope)  = 
        let namespacePrefix = match l with C -> "" | Ada -> "adaasn1rtl."
        VCBExpression (sprintf "%sTimeType_isValid(%s)" namespacePrefix (p.arg.getPointer l))
    let fnc, ns = o.cons |> Asn1Fold.foldMap (fun us c -> timeConstraint2ValidationCodeBlock l (o.typeDef.[l]) c us) us
    let fncs = conToStrFunc_basic::fnc
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] ns



let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference)  (us:State)  =
    let fncs, ns = o.cons |> Asn1Fold.foldMap (fun us c -> enumeratedConstraint2ValidationCodeBlock  l  o typeDefinition c us) us
    createIsValidFunction r l t (funcBody l fncs)  typeDefinition [] [] [] [] ns

let convertMultipleVCBsToStatementAndSetErrorCode l p (errCode: ErroCode) vcbs =
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
    let st = convertVCBToStatementAndAssigneErrCode l combinedVcb errCode.errCodeName
    match st with 
    | ValidationStatementTrue _ -> [] 
    | ValidationStatementFalse  st 
    | ValidationStatement       st -> [st]

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference) (childType:Asn1Type) (equalFunc:EqualFunction) (us:State)  =
    let sequenceOf = match l with C -> isvalid_c.sequenceOf2 | Ada -> isvalid_a.sequenceOf2
    let callBaseTypeFunc                 = match l with C -> isvalid_c.call_base_type_func | Ada -> isvalid_a.call_base_type_func

    let vcbs, ns2 = o.cons |> Asn1Fold.foldMap(fun cs c -> sequenceOfConstraint2ValidationCodeBlock r l t.id o (childType:Asn1Type) equalFunc c cs) us

    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let chp p = {p with arg = p.arg.getArrayItem l i childType.isIA5String}
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)

    let childFunc, childErrCodes, childAlphaFuncs, childLocalVars, nonEmbeddedChildValidFuncs =
        match childType.isValidFunction with
        | None ->   None, [], [], [], []
        | Some cvf ->
            match cvf.funcName with
            | Some fncName  -> 
                let f1 (p:CallerScope)  =  
                    ValidationStatement (callBaseTypeFunc ((chp p).arg.getPointer l)  fncName, [])
                Some f1, [], [], [], [cvf]
            | None          -> 
                let f1 (p:CallerScope)  =  cvf.funcBody (chp p) 
                Some f1, cvf.errCodes, cvf.alphaFuncs, cvf.localVariables, []
        
    let funBody (errCode: ErroCode) (p:CallerScope) = 
        let with_component_check, lllvs = convertMultipleVCBsToStatementAndSetErrorCode l p errCode vcbs |> List.unzip
        let childCheck =
            match childFunc with
            | None -> []
            | Some chFunc ->
                let innerStatement = chFunc p
                match innerStatement with
                | ValidationStatementTrue   (_,_)  -> []
                | ValidationStatementFalse  (st,clv)  -> [sequenceOf p.arg.p (p.arg.getAcces l) i o.isFixedSize o.minSize.uper st, lv::clv]
                | ValidationStatement       (st,clv)  -> [sequenceOf p.arg.p (p.arg.getAcces l) i o.isFixedSize o.minSize.uper st, lv::clv]
        let childCheck, lllvs2 = childCheck |> List.unzip
        match (with_component_check@childCheck) |> DAstUtilFunctions.nestItems l "ret" with
        | None   -> convertVCBToStatementAndAssigneErrCode l VCBTrue errCode.errCodeName
        | Some s ->ValidationStatement (s, (lllvs@lllvs2) |> List.collect id)
    
    createIsValidFunction r l t funBody  typeDefinition childAlphaFuncs childLocalVars childErrCodes nonEmbeddedChildValidFuncs ns2


let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (children:SeqChildInfo list)  (us:State)  =
    if (t.id.AsString = "Subtypes.T3-s1") then 
        printfn "%s" t.id.AsString
    let child_always_present_or_absent   = match l with C -> isvalid_c.Sequence_optional_child_always_present_or_absent  | Ada -> isvalid_a.Sequence_optional_child_always_present_or_absent
    let sequence_OptionalChild           = match l with C -> isvalid_c.Sequence_OptionalChild                            | Ada -> isvalid_a.Sequence_OptionalChild
    let JoinTwoIfFirstOk                 = match l with C -> isvalid_c.JoinTwoIfFirstOk                                  | Ada -> isvalid_a.JoinTwoIfFirstOk 
    let callBaseTypeFunc                 = match l with C -> isvalid_c.call_base_type_func | Ada -> isvalid_a.call_base_type_func
    let asn1Children = children |> List.choose(fun c -> match c with Asn1Child x -> Some x | AcnChild _ -> None)
    let handleChild (child:Asn1Child) (us:State) =
        let c_name = child.getBackendName l
        match child.Type.isValidFunction with
        | None                      -> None, us
        | Some (isValidFunction)    ->
            let func = 
                (*component's is validation statement. If the component has a separate function then make a call otherwise embed the code*)
                fun (p:CallerScope)  ->
                    let chp = {p with arg = p.arg.getSeqChild l c_name child.Type.isIA5String}
                    match isValidFunction.funcName with
                    | Some fncName  -> 
                        ValidationStatement (callBaseTypeFunc (chp.arg.getPointer l)  fncName, [])
                    | None ->
                        isValidFunction.funcBody chp
            let childFnc = 
                (*handle optionality*)
                match child.Optionality with
                | Some _    -> 
                    let newFunc = 
                        (fun (p:CallerScope) -> 
                            match func p with
                            | ValidationStatementTrue   (st,lv)  -> ValidationStatementTrue (sequence_OptionalChild p.arg.p (p.arg.getAcces l) c_name st, lv)
                            | ValidationStatementFalse  (st,lv)  -> ValidationStatement (sequence_OptionalChild p.arg.p (p.arg.getAcces l) c_name st, lv)
                            | ValidationStatement       (st,lv)  -> ValidationStatement (sequence_OptionalChild p.arg.p (p.arg.getAcces l) c_name st, lv) )
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



    let funBody (errCode: ErroCode) (p:CallerScope) = 
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
            aaa |> DAstUtilFunctions.nestItems l "ret" |> Option.toList, (lv |> List.collect id)
        let with_component_check, lv2 = 
            convertMultipleVCBsToStatementAndSetErrorCode l p errCode vcbs |>
            List.unzip
        match (childrenChecks@with_component_check) |> DAstUtilFunctions.nestItems l "ret" with
        | None   -> convertVCBToStatementAndAssigneErrCode l VCBTrue errCode.errCodeName
        | Some s ->ValidationStatement (s, lv@(lv2 |>List.collect id))
        
        
    createIsValidFunction r l t funBody  typeDefinition alphaFuncs localVars childrenErrCodes nonEmbeddedChildrenValidFuncs ns2

let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (children:ChChildInfo list) (baseTypeValFunc : IsValidFunction option) (us:State)  =
    let choice_OptionalChild              = match l with C -> isvalid_c.Choice_OptionalChild                                  | Ada -> isvalid_a.Choice_OptionalChild
    let expressionToStament               = match l with C -> isvalid_c.ExpressionToStament                                   | Ada -> isvalid_a.ExpressionToStament
    let callBaseTypeFunc                 = match l with C -> isvalid_c.call_base_type_func | Ada -> isvalid_a.call_base_type_func
    let handleChild (child:ChChildInfo) (us:State) =
        let c_name = child.getBackendName l
        let alwaysAbsent = child.Optionality = (Some Asn1AcnAst.Asn1ChoiceOptionality.ChoiceAlwaysAbsent)
        let presentWhenName = child.presentWhenName (Some defOrRef) l
        match child.chType.isValidFunction with
        | None                      -> None, us
        | Some (isValidFunction)    ->
            let func = 
                (*alternative's is validation statement. If the alternative has a separate function then make a call otherwise embed the code*)
                fun (p:CallerScope)  ->
                    let chp = {p with arg = p.arg.getChChild l c_name child.chType.isIA5String}
                    match isValidFunction.funcName with
                    | Some fncName -> 
                        ValidationStatement (callBaseTypeFunc (chp.arg.getPointer l)  fncName, [])
                    | None ->
                        isValidFunction.funcBody chp
            let childFnc = 
                let newFunc = 
                    (fun (p:CallerScope) -> 
                        match func p with
                        | ValidationStatementTrue   (st,lv)  -> ValidationStatementTrue (choice_OptionalChild p.arg.p (p.arg.getAcces l) presentWhenName st, lv)
                        | ValidationStatementFalse  (st,lv)  -> ValidationStatement (choice_OptionalChild p.arg.p (p.arg.getAcces l) presentWhenName st, lv)
                        | ValidationStatement       (st,lv)  -> ValidationStatement (choice_OptionalChild p.arg.p (p.arg.getAcces l) presentWhenName st, lv) )
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
    let funBody (errCode: ErroCode) (p:CallerScope) = 
        let childrenChecks, lv1 = 
            let aaa, lv1 = 
                childrenContent |> 
                List.choose(fun z -> 
                    let isValidStatement = match z with IsValidEmbedded c -> c.isValidStatement | IsValidProcCall c -> c.isValidStatement
                    match isValidStatement p with 
                    | ValidationStatementTrue _ -> None 
                    | ValidationStatementFalse  st 
                    | ValidationStatement       st -> Some st) |>
                List.unzip
            aaa |> DAstUtilFunctions.nestItems l "ret" |> Option.toList, (lv1|> List.collect id)
        let with_component_check, lv2 = 
            let a, b = convertMultipleVCBsToStatementAndSetErrorCode l p errCode vcbs |> List.unzip
            a, (b |> List.collect id)
        match (with_component_check@childrenChecks) |> DAstUtilFunctions.nestItems l "ret" with
        | None   -> convertVCBToStatementAndAssigneErrCode l VCBTrue errCode.errCodeName
        | Some s ->ValidationStatement (s, lv1@lv2)
        
    createIsValidFunction r l t funBody  typeDefinition alphaFuncs localVars childrenErrCodes nonEmbeddedChildrenValidFuncs ns2


let rec createReferenceTypeFunction_this_type (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (refTypeId:ReferenceToType) (refCons:Asn1AcnAst.AnyConstraint list) (typeDefinition:TypeDefintionOrReference) (resolvedType:Asn1Type)  (us:State)  =
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
        cons |> Asn1Fold.foldMap (fun us c -> integerConstraint2ValidationCodeBlock r l rt.baseInfo.isUnsigned c us) us
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
        cons |> Asn1Fold.foldMap (fun us c -> timeConstraint2ValidationCodeBlock l (tt.baseInfo.typeDef.[l]) c us) us
    | NullType _    ->
        [],us
    | Enumerated en  ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.EnumConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap (fun us c -> enumeratedConstraint2ValidationCodeBlock  l  en.baseInfo typeDefinition c us) us        
    | Choice ch ->
        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.ChValue) = VCBTrue
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.ChoiceConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap(fun cs c -> choiceConstraint2ValidationCodeBlock r l refTypeId ch.children valToStrFunc typeDefinition  c cs) us        
    | SequenceOf sqo ->
        let cons = refCons |> List.choose(fun c -> match c with Asn1AcnAst.SequenceOfConstraint z -> Some z | _ -> None )
        cons |> Asn1Fold.foldMap(fun cs c -> sequenceOfConstraint2ValidationCodeBlock r l refTypeId sqo.baseInfo sqo.childType resolvedType.equalFunction c cs) us        


let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (resolvedType:Asn1Type)  (us:State)  =
    let callBaseTypeFunc = match l with C -> isvalid_c.call_base_type_func | Ada -> isvalid_a.call_base_type_func
    let vcbs,us = createReferenceTypeFunction_this_type r l t.id o.refCons typeDefinition resolvedType us
    let moduleName, typeDefinitionName = 
        let t1 = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        t1.FT_TypeDefintion.[l].programUnit, t1.FT_TypeDefintion.[l].typeName
        
    let typeDefinitionName0 = ToC2(r.args.TypePrefix + o.tasName.Value)
    let baseFncName = 
        match l with
        | C     -> typeDefinitionName + "_IsConstraintValid"
        | Ada   -> 
            match t.id.ModName = o.modName.Value with
            | true  -> typeDefinitionName + "_IsConstraintValid"
            | false -> moduleName + "." + typeDefinitionName + "_IsConstraintValid"


    let funBody (errCode: ErroCode) (p:CallerScope) = 
        let with_component_check, lv2 = 
            convertMultipleVCBsToStatementAndSetErrorCode l p errCode vcbs |>
            List.unzip

        match resolvedType.isValidFunction with
        | Some _    -> 
            let funcBodyContent = callBaseTypeFunc (t.getParamValue p.arg l Encode) baseFncName 
            match (funcBodyContent::with_component_check) |> DAstUtilFunctions.nestItems l "ret" with
            | None   -> convertVCBToStatementAndAssigneErrCode l VCBTrue errCode.errCodeName
            | Some s ->ValidationStatement (s, (lv2 |>List.collect id))
        | None      -> 
            match (with_component_check) |> DAstUtilFunctions.nestItems l "ret" with
            | None   -> convertVCBToStatementAndAssigneErrCode l VCBTrue errCode.errCodeName
            | Some s ->ValidationStatement (s, (lv2 |>List.collect id))

    createIsValidFunction r l t funBody  typeDefinition [] [] [] [] us

