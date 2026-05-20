module AcnExpression

open System.Globalization

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language


// Convert an ACN boolean expression (e.g. as used in `present-when` clauses)
// into the equivalent target-language expression string.
//
// `seq` is the SEQUENCE the expression belongs to (used to resolve field
// references via `getChildResult`); `pSeq` is the access path of `seq` in
// the generated code; `lm` provides the per-language operator names and
// access-path helpers.
let acnExpressionToBackendExpression (lm: LanguageMacros) (seq: Asn1AcnAst.Sequence) (pSeq: CodegenScope) (exp: AcnExpression) =
    let unaryNotOperator = lm.lg.unaryNotOperator
    let modOp            = lm.lg.modOp
    let eqOp             = lm.lg.eqOp
    let neqOp            = lm.lg.neqOp
    let andOp            = lm.lg.andOp
    let orOp             = lm.lg.orOp

    let printUnary op chExpPriority expStr minePriority =
        minePriority, if chExpPriority >= minePriority then sprintf "%s(%s)" op expStr else sprintf "%s%s" op expStr
    let printBinary op (chExpPriority1, expStr1) (chExpPriority2, expStr2) minePriority =
        minePriority, (if chExpPriority1 >= minePriority then "(" + expStr1 + ")" else expStr1 ) + " " + op + " " + (if chExpPriority2 >= minePriority then "(" + expStr2 + ")" else expStr2 )


    let rec getChildResult (seq: Asn1AcnAst.Sequence) (pSeq: CodegenScope) (RelativePath lp) =
        match lp with
        | []    -> raise(BugErrorException "empty relative path")
        | x1::xs ->
            match seq.children |> Seq.tryFind(fun c -> c.Name = x1) with
            | None ->
                raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin "."))))
            | Some ch ->
                match ch with
                | Asn1AcnAst.AcnChild ch  -> raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'. Expecting an ASN.1 child" (lp |> Seq.StrJoin "."))))
                | Asn1AcnAst.Asn1Child ch  ->
                    match ch.Type.ActualType.Kind with
                    | Asn1AcnAst.Integer        _
                    | Asn1AcnAst.Real           _
                    | Asn1AcnAst.Boolean        _  -> {pSeq with accessPath = lm.lg.getSeqChild pSeq.accessPath (lm.lg.getAsn1ChildBackendName0 ch) false ch.Optionality.IsSome}
                    | Asn1AcnAst.Sequence s when xs.Length > 1 -> getChildResult s {pSeq with accessPath = lm.lg.getSeqChild pSeq.accessPath (lm.lg.getAsn1ChildBackendName0 ch) false ch.Optionality.IsSome} (RelativePath xs)
                    | _                 -> raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin "."))))


    let ret =
        AcnGenericTypes.foldAcnExpression
            (fun i s -> ( (0, i.Value.ToString()) , 0))
            (fun i s -> ( (0,"") , 0))
            (fun i s -> ( (0, i.Value.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo)) , 0))
            (fun i s -> ( (0, i.Value.ToString().ToLower()) , 0))
            (fun lf s ->
                let plf = getChildResult seq pSeq lf
                (0, (plf.accessPath.joined lm.lg)) , 0)
            (fun loc (chExpPriority, expStr) s -> printUnary unaryNotOperator chExpPriority expStr 1, 0) //NotUnaryExpression
            (fun loc (chExpPriority, expStr) s -> printUnary "-" chExpPriority expStr 1, 0)//MinusUnaryExpression
            (fun l e1 e2  s -> printBinary "+" e1 e2 3, 0 )
            (fun l e1 e2  s -> printBinary "-" e1 e2 3, 0 )
            (fun l e1 e2  s -> printBinary "*" e1 e2 2, 0 )
            (fun l e1 e2  s -> printBinary "/" e1 e2 2, 0 )
            (fun l e1 e2  s -> printBinary modOp e1 e2 2, 0 )
            (fun l e1 e2  s -> printBinary "<=" e1 e2 4, 0 )
            (fun l e1 e2  s -> printBinary "<" e1 e2 4, 0 )
            (fun l e1 e2  s -> printBinary ">=" e1 e2 4, 0 )
            (fun l e1 e2  s -> printBinary ">" e1 e2 4, 0 )
            (fun l e1 e2  s -> printBinary eqOp e1 e2 5, 0 )
            (fun l e1 e2  s -> printBinary neqOp e1 e2 5, 0 )
            (fun lc e1 e2  s -> printBinary andOp  e1 e2 6, 0 )
            (fun lc e1 e2  s -> printBinary orOp e1 e2 6, 0 )
            exp 0 |> fst |> snd

    ret
