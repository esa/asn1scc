module validate

open System.Numerics
open FsUtils
open Ast
open System.IO
open VisitTree
open uPER
open CloneTree
//open c_utils
open BackendAst


let rec getCharComparisonBoolExp (path:list<string>) (c:Asn1Constraint) (m:Asn1Module) (r:AstRoot) : CharComparisonBoolExp option=
    let p = AccessPath "pVal[i]"
    let tasName = path.Tail.Head
    match c with
    | SingleValueConstraint(v)     ->
        let strVal = GetValueAstring v r
        Some (AlphaStringContainsChar(p, strVal))
    | RangeConstraint(a,b,minIsIn,maxIsIn)         ->
        let char1 = GetValueAstring a r
        let char2 = GetValueAstring b r
        let b1 = match minIsIn with
                 | true  -> AlphaLessThanOrEq(p, char1.Chars 0)
                 | false -> AlphaLessThan(p, char1.Chars 0)
        let b2 = match maxIsIn with
                 | true  -> AlphaGreaterThanOrEq (p, char2.Chars 0)
                 | false -> AlphaGreaterThan(p, char2.Chars 0)
        Some(AlphaAnd(b1,b2))
    | RangeConstraint_val_MAX(a,minIsIn)   ->
        let char1 = GetValueAstring a r
        match minIsIn with
        | true  -> Some (AlphaLessThanOrEq(p, char1.Chars 0))
        | false -> Some (AlphaLessThan(p, char1.Chars 0))
    | RangeConstraint_MIN_val(b,maxIsIn)   ->
        let char2 = GetValueAstring b r
        match maxIsIn with
        | true  -> Some(AlphaGreaterThanOrEq (p, char2.Chars 0))
        | false -> Some(AlphaGreaterThan(p, char2.Chars 0))
    | RangeConstraint_MIN_MAX      -> None
    | RootConstraint2(c1,c2)
    | UnionConstraint(c1, c2,_)     ->
        let k1 = getCharComparisonBoolExp  path c1  m r
        let k2 = getCharComparisonBoolExp  path c2  m r
        match k1,k2 with
        | Some k1, Some k2   -> Some(AlphaOr(k1, k2))
        | _                  -> None
    | IntersectionConstraint(c1,c2) ->
        let k1 = getCharComparisonBoolExp  path c1  m r
        let k2 = getCharComparisonBoolExp  path c2  m r
        match k1,k2 with
        | Some k1, Some k2   ->  Some(AlphaAnd(k1, k2))
        | Some k1, None     ->  Some k1
        | None, Some k2     -> Some k2
        | None, None        -> None
    | AllExceptConstraint(c1)       ->
        let k1 = getCharComparisonBoolExp  path c1  m r
        match k1 with
        | Some k1   ->    Some(AlphaNot k1)
        | None      ->    raise(BugErrorException "Always false case !!!")
    | ExceptConstraint(c1,c2)       ->
        let k1 = getCharComparisonBoolExp  path c1  m r
        let k2 = getCharComparisonBoolExp  path c2  m r
        match k1,k2 with
        | Some k1, Some k2   ->  Some(AlphaAnd(k1, AlphaNot k2))
        | Some k1, None     ->  raise(BugErrorException "Always false case !!!")
        | None, Some k2     -> Some (AlphaNot k2)
        | None, None        -> raise(BugErrorException "Always false case !!!")
    | RootConstraint(c1)            -> getCharComparisonBoolExp  path c1  m r
    | _   -> raise(BugErrorException "Unexpected Constraint")



let rec getBackendBooleanExpression (t:ConstraintType) (path:list<string>) (c:Asn1Constraint)  (m:Asn1Module) (r:AstRoot) : BackendBooleanExpression option=
    let p = AccessPath ""//(GetConstraintTypeAccessPath path  t r)
    let tasName = path.Tail.Head
    match c with
    | SingleValueConstraint(v)     ->
        let unamevar = Ast.GetValueID v
        let uvar = {UnnamedVariableDeclaration.privateName = unamevar; typereference=("",""); value="" }
        match t.Type.Kind with
        | BitString ->
            match (GetTypeUperRange t.Type.Kind t.Type.Constraints r) with
            | Concrete(min, max) when min = max -> Some (FixSizeBitStringEq (p, uvar))
            | Concrete(min, max)                -> Some (VarSizeBitStringEq (p, uvar))
            | _                                 -> raise(BugErrorException "")
        | OctetString ->
            match (GetTypeUperRange t.Type.Kind t.Type.Constraints r) with
            | Concrete(min, max) when min = max -> Some (FixSizeOctStringEq (p, uvar))
            | Concrete(min, max)                -> Some (VarSizeOctStringEq (p, uvar))
            | _                                 -> raise(BugErrorException "")
        | _             ->
            let primaryExp = variable.getBackendPrimaryExpression v t.Type m r
            Some (EqExp(p, primaryExp))
    | RangeConstraint(a,b,minIsIn,maxIsIn)         ->
        let aNum = variable.getBackendPrimaryNumericExpression a t.Type m r
        let bNum = variable.getBackendPrimaryNumericExpression b t.Type m r
        let b1 = match minIsIn with
                 | true  -> LessThanOrEqExp(p, aNum)
                 | false -> LessThanExp(p, aNum)
        let b2 = match maxIsIn with
                 | true  -> GreaterThanOrEqExp(p, bNum)
                 | false -> GreaterThanExp(p, bNum)
        Some(AndConstraintExp(b1,b2))
    | RangeConstraint_val_MAX(a,minIsIn)   ->
        let aNum = variable.getBackendPrimaryNumericExpression a t.Type m r
        match minIsIn with
        | true  -> Some(LessThanOrEqExp(p, aNum))
        | false -> Some(LessThanExp(p, aNum))
    | RangeConstraint_MIN_val(b,maxIsIn)   ->
        let bNum = variable.getBackendPrimaryNumericExpression b t.Type m r
        match maxIsIn with
        | true  -> Some(GreaterThanOrEqExp(p, bNum))
        | false -> Some(GreaterThanExp(p, bNum))
    | RangeConstraint_MIN_MAX      -> None
    | SizeConstraint(inCon)        -> getBackendBooleanExpression  (LengthOf t.Type) path inCon  m r
    | AlphabetConstraint(inCon)    ->
        let charCompExp = getCharComparisonBoolExp path inCon m r
        match charCompExp with
        | None  -> None
        | Some charCompExp  ->
            let alphaFuncName = ToC ((path |> Seq.skip 1).StrJoin("_").Replace("#","elem"))
            let alphabetCheckFunc = { AlphabetCheckFunc.funcName = alphaFuncName;  con = charCompExp}
            Some (CallAlphaFunc(p, alphabetCheckFunc))
    | RootConstraint2(c1,c2)
    | UnionConstraint(c1, c2,_)     ->
        let k1 = getBackendBooleanExpression t path c1  m r
        let k2 = getBackendBooleanExpression t path c2  m r
        match k1,k2 with
        | Some k1, Some k2   -> Some(OrConstraintExp(k1, k2))
        | _                  -> None
    | IntersectionConstraint(c1,c2) ->
        let k1 = getBackendBooleanExpression t path c1  m r
        let k2 = getBackendBooleanExpression t path c2  m r
        match k1,k2 with
        | Some k1, Some k2   ->  Some(AndConstraintExp(k1, k2))
        | Some k1, None     ->  Some k1
        | None, Some k2     -> Some k2
        | None, None        -> None
    | AllExceptConstraint(c1)       ->
        let k1 = getBackendBooleanExpression t path c1  m r
        match k1 with
        | Some k1   ->    Some(NotConstraintExp (k1))
        | None      ->    raise(BugErrorException "Always false case !!!")
    | ExceptConstraint(c1,c2)       ->
        let k1 = getBackendBooleanExpression t path c1  m r
        let k2 = getBackendBooleanExpression t path c2  m r
        match k1,k2 with
        | Some k1, Some k2   ->  Some(AndConstraintExp(k1, NotConstraintExp k2))
        | Some k1, None     ->  raise(BugErrorException "Always false case !!!")
        | None, Some k2     -> Some (NotConstraintExp k2)
        | None, None        -> raise(BugErrorException "Always false case !!!")
    | RootConstraint(c1)            -> getBackendBooleanExpression t path c1  m r
    | TypeInclusionConstraint(modName,tasName) ->
        let actualType = GetActualTypeByNameAllConsIncluded modName tasName r
        let arrCons = actualType.Constraints |> Seq.map(fun c -> getBackendBooleanExpression t path c  m r) |> Seq.choose id |> Seq.toList

        arrCons |>
        List.fold (fun s c -> match s with
                              | None -> Some c
                              | Some c0 -> Some (OrConstraintExp (c0, c))) None
    | WithComponentConstraint(_)    -> raise(BugErrorException "Unexpected Constraint")
    | WithComponentsConstraint(_)   -> raise(BugErrorException "Unexpected Constraint")
