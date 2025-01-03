(*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*)

module MapParamAstToNonParamAst

open System
open System.Globalization
open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open FsUtils



let getModuleByName = ParameterizedAsn1Ast.getModuleByName

let getTasByName  = ParameterizedAsn1Ast.getTasByName

type UserDefinedTypeScope = ScopeNode list

type UserDefinedVarScope = VarScopNode list


let visitModule (md:ParameterizedAsn1Ast.Asn1Module) : UserDefinedTypeScope=
    [MD md.Name.Value]

let visitTas (s:UserDefinedTypeScope) (ts:ParameterizedAsn1Ast.TypeAssignment) : UserDefinedTypeScope=
    s@[TA ts.Name.Value]

let visitTypeVas (s:UserDefinedTypeScope) (vas:ParameterizedAsn1Ast.ValueAssignment) : UserDefinedTypeScope=
    s@[VA vas.Name.Value]

let visitRefType (md:string) (ts:string) : UserDefinedTypeScope=
    [MD md; TA ts]

//let visitRevValue (md:string) (vs:string) =
//    {UserDefinedTypeScope.typeID=[MD md; VA vs]; asn1TypeName=None; asn1VarName=Some vs;varID=[]}

let visitSeqChild (s:UserDefinedTypeScope) (ch:ParameterizedAsn1Ast.ChildInfo) : UserDefinedTypeScope=
    let isOptional =
        match ch.Optionality with
        | None -> false
        | Some(ParameterizedAsn1Ast.AlwaysAbsent) -> true
        | Some(ParameterizedAsn1Ast.AlwaysPresent) -> false
        | Some(ParameterizedAsn1Ast.Optional) -> true
        | Some(ParameterizedAsn1Ast.Default(_)) -> true
    s@[SEQ_CHILD (ch.Name.Value, isOptional)]

let visitChoiceChild (s:UserDefinedTypeScope) (ch:ParameterizedAsn1Ast.ChildInfo) : UserDefinedTypeScope=
    s@[CH_CHILD (ch.Name.Value, ToC2 ch.Name.Value, "")]

let visitSeqOfChild (s:UserDefinedTypeScope) : UserDefinedTypeScope =
    s@[SQF]


let visitValueVas  (vs:ParameterizedAsn1Ast.ValueAssignment) : UserDefinedVarScope=
    [VA2 vs.Name.Value]


let visitDefaultValue () : UserDefinedVarScope =
    [DV]

let visitNamedItemValue  (nm:ParameterizedAsn1Ast.NamedItem) : UserDefinedVarScope=
    [NI nm.Name.Value]

let visitConstraint (s:UserDefinedVarScope) : UserDefinedVarScope=
    s@[CON 0]

let visitSiblingConstraint (s:UserDefinedVarScope) : UserDefinedVarScope =
    let idx, xs =
        match s |> List.rev with
        | (CON idx)::xs  -> idx, xs
        | _              -> raise(BugErrorException "invalid call to visitSiblingConstraint")
    xs@[CON (idx+1)]


let visitValue (s:UserDefinedVarScope) :UserDefinedVarScope =
    s @[VL 0]

let visitSiblingValue (s:UserDefinedVarScope) :UserDefinedVarScope =
    let idx, xs =
        match s |> List.rev with
        | (VL idx)::xs  -> idx, xs
        | _              -> raise(BugErrorException "invalid call to visitSiblingConstraint")
    xs@[VL (idx+1)]

let visitSeqOfValue (s:UserDefinedVarScope) idx :UserDefinedVarScope =
    s @[SQOV idx]

let visitSeqChildValue (s:UserDefinedVarScope) childName :UserDefinedVarScope =
    s @[SQCHILD childName ]



let rec getSequenceChildren (r:ParameterizedAsn1Ast.AstRoot) (input:list<ParameterizedAsn1Ast.SequenceChild>) : list<ParameterizedAsn1Ast.ChildInfo> =
    seq {
        for ch in input do
        match ch with
        | ParameterizedAsn1Ast.ChildInfo ch  -> yield ch
        | ParameterizedAsn1Ast.ComponentsOf (md,ts) ->
            let tas = ParameterizedAsn1Ast.getTypeAssignment r md ts
            match tas.Type.Kind with
            | ParameterizedAsn1Ast.Sequence(children)    ->
                 yield! getSequenceChildren r children
            | _                                          -> raise(SemanticError(ts.Location, "Expecting SEQUENCE type"))
            }|> Seq.toList

let rec getActualKind r kind =
    match kind with
    | ParameterizedAsn1Ast.ReferenceType(md, ts,_, _) ->
        let newTas = ParameterizedAsn1Ast.getTypeAssignment r md ts
        getActualKind r newTas.Type.Kind
    | _                                            -> kind


let rec MapAsn1Value (r:ParameterizedAsn1Ast.AstRoot) (t: ParameterizedAsn1Ast.Asn1Type) (typeScope : ScopeNode list) (variableScope : VarScopNode list)  (v:ParameterizedAsn1Ast.Asn1Value) :Asn1Ast.Asn1Value =
    let rec getActualKindAndModule r kind =
        let rec getActualaux r kind modName=
            match kind with
            | ParameterizedAsn1Ast.ReferenceType(md, ts,_, _) ->
                let mdl = ParameterizedAsn1Ast.getModuleByName  r md
                let newTas = ParameterizedAsn1Ast.getTypeAssignment r md ts
                getActualaux r newTas.Type.Kind (Some mdl.Name)
            | _                                            -> kind, modName
        getActualaux r kind None

    let MapAsn1ValueKind (r:ParameterizedAsn1Ast.AstRoot) (t: ParameterizedAsn1Ast.Asn1Type) (vk:ParameterizedAsn1Ast.Asn1ValueKind) :Asn1Ast.Asn1ValueKind =
        let actKind = getActualKind r t.Kind
        match vk with
        |ParameterizedAsn1Ast.IntegerValue(v)       -> Asn1Ast.IntegerValue v
        |ParameterizedAsn1Ast.RealValue(v)          -> Asn1Ast.RealValue v
        |ParameterizedAsn1Ast.StringValue(v)        ->
            let actKind, mdName = getActualKindAndModule r t.Kind
            match actKind with
            | ParameterizedAsn1Ast.TimeType tmClss  -> Asn1Ast.TimeValue (CommonTypes.createTimeValueFromString tmClss v)
            | _                                     -> Asn1Ast.StringValue ([CStringValue v.Value], v.Location)
        |ParameterizedAsn1Ast.BooleanValue(v)       -> Asn1Ast.BooleanValue v
        |ParameterizedAsn1Ast.BitStringValue(v)     -> Asn1Ast.BitStringValue v
        |ParameterizedAsn1Ast.OctetStringValue v    -> Asn1Ast.OctetStringValue v
        |ParameterizedAsn1Ast.RefValue(v1,v2)       ->
            let actKind, mdName = getActualKindAndModule r t.Kind
            match actKind with
            | ParameterizedAsn1Ast.Enumerated(items)    ->
                match mdName with
                | None  ->  Asn1Ast.RefValue(v1,v2)
                | Some(s)   -> Asn1Ast.RefValue(s,v2)
            | _                                         ->  Asn1Ast.RefValue(v1,v2)
        |ParameterizedAsn1Ast.SeqOfValue(vals)      ->
            match actKind with
            | ParameterizedAsn1Ast.SequenceOf(ch)    -> Asn1Ast.SeqOfValue(vals |> List.mapi (fun idx v -> MapAsn1Value r ch typeScope (visitSeqOfValue variableScope idx) v))
            | ParameterizedAsn1Ast.BitString namedBits      ->
                let actType = ParameterizedAsn1Ast.GetActualType t r
                let cons =  actType.Constraints |> List.map (ParameterizedAsn1Ast.getBitStringConstraint r t)
                let uperRange = ParameterizedAsn1Ast.getBitStringUperRange cons v.Location
                let bitPos =
                    vals |>
                    List.map(fun chV ->
                        match chV.Kind    with
                        | ParameterizedAsn1Ast.RefValue            (_,refVal)    ->
                            match namedBits |> Seq.tryFind(fun z -> z.Name.Value = refVal.Value) with
                            | None      -> raise (SemanticError(v.Location, (sprintf "Expecting a BIT STRING value. '%s' is not defined as a named bit" refVal.Value)))
                            | Some nb   ->
                                match nb._value with
                                | CommonTypes.IDV_IntegerValue       intVal     -> intVal.Value
                                | CommonTypes.IDV_DefinedValue   (mdVal, refVal) -> ParameterizedAsn1Ast.GetValueAsInt (ParameterizedAsn1Ast.GetBaseValue mdVal refVal r) r

                        | _                                         -> raise (SemanticError(v.Location, (sprintf "Expecting a BIT STRING value but found a SEQUENCE OF value" )))
                    ) |> Set.ofList

                //printfn "uperRange = %A" uperRange
                let maxValue =
                    match uperRange with
                    | Concrete(a, b)    ->
                        //printf "maxValue : %A, %A\n" a b

                        BigInteger (b-1u)
                    | _                 -> bitPos.MaximumElement
                //printf "maxValue : %A\n" maxValue
                let bitStrVal =
                    [0I .. maxValue] |> List.map(fun bi -> if bitPos.Contains bi then '1' else '0') |> Seq.StrJoin ""
                Asn1Ast.BitStringValue ({StringLoc.Value = bitStrVal; Location = v.Location})
            | ParameterizedAsn1Ast.IA5String  ->
                let partVals =
                    vals |>
                    List.map(fun v ->
                        match v.Kind with
                        | ParameterizedAsn1Ast.StringValue vl -> CStringValue vl.Value
                        | ParameterizedAsn1Ast.RefValue (_, vs)  ->
                            match vs.Value with
                            | "cr" -> SpecialCharacter CarriageReturn
                            | "lf" -> SpecialCharacter LineFeed
                            | "ht" -> SpecialCharacter HorizontalTab
                            | "nul" -> SpecialCharacter NullCharacter
                            | _     -> raise(SemanticError(v.Location, "Expecting a IA5String value"))
                        | _  -> raise(SemanticError(v.Location, "Expecting a IA5String value")))

                Asn1Ast.StringValue (partVals, v.Location)
            | _                                      -> raise(SemanticError(v.Location, "Expecting a SEQUENCE OF value"))
        |ParameterizedAsn1Ast.SeqValue(vals)        ->
            match actKind with
            |ParameterizedAsn1Ast.Sequence(children) ->
                let children = getSequenceChildren r children
                let mapChildVal (nm:StringLoc, chv:ParameterizedAsn1Ast.Asn1Value) =
                    match children |> Seq.tryFind(fun ch -> ch.Name=nm) with
                    | Some(child)   -> (nm, MapAsn1Value r child.Type typeScope (visitSeqChildValue variableScope nm.Value) chv)
                    | _             -> raise(SemanticError(nm.Location, sprintf "Unknown component name '%s'" nm.Value))
                Asn1Ast.SeqValue(vals |> List.map mapChildVal)
            | _                 -> raise(SemanticError(v.Location, "Expecting a SEQUENCE value"))
        |ParameterizedAsn1Ast.ChValue(n,v)          ->
            match actKind with
            |ParameterizedAsn1Ast.Choice(children) ->
                match children |> Seq.tryFind(fun x -> x.Name=n) with
                | Some(child)   -> Asn1Ast.ChValue(n, MapAsn1Value r child.Type typeScope (visitSeqChildValue variableScope n.Value) v)
                | None          -> raise(SemanticError(n.Location, sprintf "Unknown alternative name '%s'" n.Value))
            | _                 -> raise(SemanticError(v.Location, "Expecting a CHOICE value"))

        |ParameterizedAsn1Ast.NullValue             -> Asn1Ast.NullValue
        |ParameterizedAsn1Ast.ObjOrRelObjIdValue  items  -> Asn1Ast.ObjOrRelObjIdValue items

        |ParameterizedAsn1Ast.EmptyList             ->
            match actKind with
            |ParameterizedAsn1Ast.Sequence(_)       -> Asn1Ast.SeqValue []
            |ParameterizedAsn1Ast.SequenceOf(_)     -> Asn1Ast.SeqOfValue []
            | _                                     -> raise(SemanticError(v.Location, "Unexpected value"))
    {
        Asn1Ast.Asn1Value.Kind = MapAsn1ValueKind r t v.Kind
        Location = v.Location
        id = ReferenceToValue(typeScope,variableScope)
        moduleName  = v.moduleName

    }




and MapAsn1Optionality (r:ParameterizedAsn1Ast.AstRoot) (kind: ParameterizedAsn1Ast.Asn1Type) typeScope (o:ParameterizedAsn1Ast.Asn1Optionality) :Asn1Ast.Asn1Optionality =
    match o with
    | ParameterizedAsn1Ast.AlwaysAbsent     -> Asn1Ast.AlwaysAbsent
    | ParameterizedAsn1Ast.AlwaysPresent    -> Asn1Ast.AlwaysPresent
    | ParameterizedAsn1Ast.Optional         -> Asn1Ast.Optional ({Asn1Ast.Optional.defaultValue = None})
    | ParameterizedAsn1Ast.Default(v)       -> Asn1Ast.Optional ({Asn1Ast.Optional.defaultValue = Some (MapAsn1Value r kind typeScope (visitDefaultValue ()) v)})

and MapChildInfo (r:ParameterizedAsn1Ast.AstRoot)  typeScope (isSequence) (c:ParameterizedAsn1Ast.ChildInfo) :Asn1Ast.ChildInfo =
    {
        Asn1Ast.ChildInfo.Name = c.Name
        ada_name = ToC2 c.Name.Value
        c_name = ToC2 c.Name.Value
        scala_name = ToC2 c.Name.Value
        present_when_name = ToC2 c.Name.Value
        Type = MapAsn1Type r  (if isSequence then (visitSeqChild typeScope c) else (visitChoiceChild typeScope c)) c.Type
        Optionality = match c.Optionality with
                      |None -> None
                      |Some(x) -> Some (MapAsn1Optionality r c.Type typeScope x)
        Comments = c.Comments
    }

and MapNamedItem (r:ParameterizedAsn1Ast.AstRoot) moduleName typeScope (n:ParameterizedAsn1Ast.NamedItem) :Asn1Ast.NamedItem =
    {
        Asn1Ast.NamedItem.Name = n.Name
        c_name = ToC n.Name.Value
        scala_name = ToC n.Name.Value
        ada_name = ToC n.Name.Value
        _value = match n._value with
                 | None -> None
                 | Some(x)  -> Some (MapAsn1Value r {ParameterizedAsn1Ast.Asn1Type.Kind = ParameterizedAsn1Ast.Integer; Constraints = []; Location=n.Name.Location;parameterizedTypeInstance=false;acnInfo=None;unitsOfMeasure = None; moduleName=moduleName} typeScope (visitNamedItemValue n) x)
        Comments = n.Comments
    }



and MapNamedConstraint (r:ParameterizedAsn1Ast.AstRoot) (t: ParameterizedAsn1Ast.Asn1Type) typeScope cs (x:ParameterizedAsn1Ast.NamedConstraint) :Asn1Ast.NamedConstraint =
    let childType = match getActualKind r t.Kind with
                    | ParameterizedAsn1Ast.Choice(children)     ->
                        match children |> Seq.tryFind(fun ch -> ch.Name = x.Name) with
                        | None      -> raise(SemanticError(x.Name.Location, sprintf "Unknown component name '%s'" x.Name.Value))
                        | Some(child)   -> child.Type

                    | ParameterizedAsn1Ast.Sequence(children)   ->
                        let children = getSequenceChildren r children
                        match children |> Seq.tryFind(fun ch -> ch.Name = x.Name) with
                        | None      -> raise(SemanticError(x.Name.Location, sprintf "Unknown component name '%s'" x.Name.Value))
                        | Some(child)   -> child.Type
                    | _                                         -> raise(SemanticError(x.Name.Location, sprintf "Unexpected constraint type" ))
    {
        Asn1Ast.NamedConstraint.Name = x.Name;
        Mark = match x.Mark with
                | ParameterizedAsn1Ast.NoMark        -> Asn1Ast.NoMark
                | ParameterizedAsn1Ast.MarkPresent   -> Asn1Ast.MarkPresent
                | ParameterizedAsn1Ast.MarkAbsent    -> Asn1Ast.MarkAbsent
                | ParameterizedAsn1Ast.MarkOptional  -> Asn1Ast.MarkOptional
        Constraint =
            match x.Constraint with
            | None  -> None
            | Some(cc)  -> Some (MapAsn1Constraint r childType typeScope cs cc)
    }

and MapAsn1Constraint (r:ParameterizedAsn1Ast.AstRoot) (t: ParameterizedAsn1Ast.Asn1Type) typeScope cs (c:ParameterizedAsn1Ast.Asn1Constraint) :Asn1Ast.Asn1Constraint =
    match c with
    | ParameterizedAsn1Ast.Asn1Constraint.SingleValueConstraint(s, v)          -> Asn1Ast.SingleValueConstraint (s, MapAsn1Value r t typeScope (visitValue cs) v)
    | ParameterizedAsn1Ast.Asn1Constraint.RangeConstraint(s, v1,v2,b1,b2)      ->
        let vs1 = visitValue cs
        let vs2 = visitSiblingValue vs1

        Asn1Ast.RangeConstraint(s, MapAsn1Value r t typeScope vs1 v1, MapAsn1Value r t typeScope vs2 v2,b1,b2)
    | ParameterizedAsn1Ast.Asn1Constraint.RangeConstraint_val_MAX(s, v,b)        -> Asn1Ast.RangeConstraint_val_MAX (s, MapAsn1Value r t typeScope (visitValue cs) v, b)
    | ParameterizedAsn1Ast.Asn1Constraint.RangeConstraint_MIN_val(s, v,b)        -> Asn1Ast.RangeConstraint_MIN_val (s, MapAsn1Value r t typeScope (visitValue cs) v, b)
    | ParameterizedAsn1Ast.TypeInclusionConstraint(s, s1,s2)   -> Asn1Ast.TypeInclusionConstraint(s, s1,s2)
    | ParameterizedAsn1Ast.Asn1Constraint.SizeConstraint(s, c)                 -> Asn1Ast.SizeConstraint(s, MapAsn1Constraint r {ParameterizedAsn1Ast.Asn1Type.Kind = ParameterizedAsn1Ast.Integer; Constraints = []; Location=emptyLocation;parameterizedTypeInstance=false;acnInfo=None;unitsOfMeasure = None; moduleName=t.moduleName} typeScope (visitConstraint cs) c)
    | ParameterizedAsn1Ast.AlphabetConstraint(s,c)             -> Asn1Ast.AlphabetConstraint(s, MapAsn1Constraint r {ParameterizedAsn1Ast.Asn1Type.Kind = ParameterizedAsn1Ast.IA5String; Constraints = []; Location=emptyLocation;parameterizedTypeInstance=false;acnInfo=None;unitsOfMeasure = None; moduleName=t.moduleName} typeScope (visitConstraint cs) c)
    | ParameterizedAsn1Ast.UnionConstraint(s, c1,c2, b)           ->
        let cs1 = visitConstraint cs
        let cs2 = visitSiblingConstraint cs1
        Asn1Ast.UnionConstraint(s, MapAsn1Constraint r t typeScope cs1 c1, MapAsn1Constraint r t typeScope cs2 c2, b)
    | ParameterizedAsn1Ast.IntersectionConstraint(s, c1,c2)    ->
        let cs1 = visitConstraint cs
        let cs2 = visitSiblingConstraint cs1
        Asn1Ast.IntersectionConstraint(s, MapAsn1Constraint r t typeScope cs1 c1, MapAsn1Constraint r t typeScope cs2 c2)
    | ParameterizedAsn1Ast.AllExceptConstraint(s, c)           -> Asn1Ast.AllExceptConstraint(s, MapAsn1Constraint r t typeScope (visitConstraint cs) c)
    | ParameterizedAsn1Ast.ExceptConstraint(s, c1,c2)          ->
        let cs1 = visitConstraint cs
        let cs2 = visitSiblingConstraint cs1
        Asn1Ast.ExceptConstraint(s, MapAsn1Constraint r t typeScope cs1 c1, MapAsn1Constraint r t typeScope cs2 c2)
    | ParameterizedAsn1Ast.RootConstraint(s, c1)               -> Asn1Ast.RootConstraint(s, MapAsn1Constraint r t typeScope (visitConstraint cs) c1)
    | ParameterizedAsn1Ast.RootConstraint2(s, c1,c2)           ->
        let cs1 = visitConstraint cs
        let cs2 = visitSiblingConstraint cs1
        Asn1Ast.RootConstraint2(s, MapAsn1Constraint r t typeScope cs2 c1, MapAsn1Constraint r t typeScope cs2 c2)
    | ParameterizedAsn1Ast.WithComponentConstraint(s, c, loc)       ->
        let akind = getActualKind r t.Kind
        match akind with
        | ParameterizedAsn1Ast.SequenceOf(child)    ->
            Asn1Ast.WithComponentConstraint(s, (MapAsn1Constraint r child typeScope (visitConstraint cs) c), loc)
        | _                                         ->        raise(SemanticError(emptyLocation,"Unexpected constraint type"))
    | ParameterizedAsn1Ast.WithComponentsConstraint(s, ncs)    ->
        Asn1Ast.WithComponentsConstraint(s, ncs|> foldMap (fun cs c ->
                                                                let newC = MapNamedConstraint r t typeScope (visitConstraint cs) c
                                                                let newSs = visitSiblingConstraint cs
                                                                newC, newSs) cs |> fst  )

and MapAsn1Type (r:ParameterizedAsn1Ast.AstRoot) typeScope (t:ParameterizedAsn1Ast.Asn1Type) :Asn1Ast.Asn1Type =
    let aux kind : Asn1Ast.Asn1Type=
        let newCons =
            t.Constraints |>
            foldMap (fun ss c ->
                let newC = MapAsn1Constraint r t typeScope ss c
                let newSs = visitSiblingConstraint ss
                newC, newSs) (visitConstraint []) |> fst

        {
            Asn1Ast.Asn1Type.Kind = kind
            Constraints = newCons
            Location = t.Location
            parameterizedTypeInstance = t.parameterizedTypeInstance
            unitsOfMeasure = t.unitsOfMeasure
            moduleName  = t.moduleName
            acnInfo = t.acnInfo
        }
    match t.Kind with
    | ParameterizedAsn1Ast.Integer          -> aux Asn1Ast.Integer
    | ParameterizedAsn1Ast.Real             -> aux Asn1Ast.Real
    | ParameterizedAsn1Ast.IA5String        -> aux Asn1Ast.IA5String
    | ParameterizedAsn1Ast.NumericString    -> aux Asn1Ast.NumericString
    | ParameterizedAsn1Ast.OctetString      -> aux Asn1Ast.OctetString
    | ParameterizedAsn1Ast.TimeType cl        -> aux (Asn1Ast.TimeType cl)
    | ParameterizedAsn1Ast.NullType         -> aux Asn1Ast.NullType
    | ParameterizedAsn1Ast.BitString nBits       -> aux (Asn1Ast.BitString nBits)
    | ParameterizedAsn1Ast.Boolean          -> aux Asn1Ast.Boolean
    | ParameterizedAsn1Ast.ObjectIdentifier         -> aux Asn1Ast.ObjectIdentifier
    | ParameterizedAsn1Ast.RelativeObjectIdentifier -> aux Asn1Ast.RelativeObjectIdentifier
    | ParameterizedAsn1Ast.Enumerated(items)-> aux (Asn1Ast.Enumerated(items |> List.map (MapNamedItem r t.moduleName typeScope)))
    | ParameterizedAsn1Ast.SequenceOf(child)-> aux (Asn1Ast.SequenceOf(MapAsn1Type r (visitSeqOfChild typeScope) child))
    | ParameterizedAsn1Ast.Sequence(children)   ->
        let children = getSequenceChildren r children
        aux (Asn1Ast.Sequence(children |> List.map (MapChildInfo r typeScope true) ))
    | ParameterizedAsn1Ast.Choice(children)     -> aux (Asn1Ast.Choice(children |> List.map (MapChildInfo r typeScope false) ))
    | ParameterizedAsn1Ast.ReferenceType(mdName,ts,refEnc,  args)   ->
        match args with
        | []    ->  aux (Asn1Ast.ReferenceType({Asn1Ast.ReferenceType.modName = mdName; tasName = ts; tabularized = false; refEnc= refEnc}))
        | _     ->  raise(BugErrorException "")

let MapTypeAssignment (r:ParameterizedAsn1Ast.AstRoot) (m:ParameterizedAsn1Ast.Asn1Module) (tas:ParameterizedAsn1Ast.TypeAssignment) :Asn1Ast.TypeAssignment =
    {
        Asn1Ast.TypeAssignment.Name = tas.Name
        Type = MapAsn1Type r ([MD m.Name.Value; TA tas.Name.Value]) tas.Type
        c_name = ToC2 tas.Name.Value
        scala_name = ToC2 tas.Name.Value
        ada_name = ToC2 tas.Name.Value
        Comments = tas.Comments
        acnInfo = tas.acnInfo
    }

let MapValueAssignment (r:ParameterizedAsn1Ast.AstRoot) (m:ParameterizedAsn1Ast.Asn1Module) (vas:ParameterizedAsn1Ast.ValueAssignment) :Asn1Ast.ValueAssignment =
    let typeScope =
        match vas.Type.Kind with
        | ParameterizedAsn1Ast.ReferenceType(md,ts,_,_)  -> [MD md.Value; TA ts.Value]
        | _                                            -> []//raise (SemanticError (vas.Name.Location, "Unnamed types are not currently supported."))
    let varScope = [VA2 vas.Name.Value]
    {
        Asn1Ast.ValueAssignment.Name = vas.Name
        Type = MapAsn1Type r typeScope vas.Type
        Value = MapAsn1Value r  vas.Type typeScope varScope vas.Value
        //Scope =
        //    match vas.Scope with
        //    | ParameterizedAsn1Ast.GlobalScope      ->  Asn1Ast.GlobalScope
        //    | ParameterizedAsn1Ast.TypeScope(m,t)   ->  Asn1Ast.TypeScope(m,t)
        c_name = vas.c_name
        scala_name = vas.scala_name
        ada_name = vas.ada_name
    }

let MapModule (r:ParameterizedAsn1Ast.AstRoot) (m:ParameterizedAsn1Ast.Asn1Module) :Asn1Ast.Asn1Module =
    let DoImportedModule (x:ParameterizedAsn1Ast.ImportedModule) : Asn1Ast.ImportedModule =
        { Asn1Ast.ImportedModule.Name = x.Name; Types = x.Types; Values = x.Values}
    {
        Asn1Ast.Asn1Module.Name = m.Name
        TypeAssignments = m.TypeAssignments |> List.filter(fun x -> x.Parameters.Length = 0) |> List.map (MapTypeAssignment r m)
        ValueAssignments = m.ValueAssignments |> List.map (MapValueAssignment r m)
        Imports = m.Imports |> List.map DoImportedModule
        Exports  = match m.Exports with
                   | ParameterizedAsn1Ast.All               -> Asn1Ast.All
                   | ParameterizedAsn1Ast.OnlySome(lst)     -> Asn1Ast.OnlySome(lst)
        Comments = m.Comments
        position = m.position

    }

let MapFile (r:ParameterizedAsn1Ast.AstRoot) (f:ParameterizedAsn1Ast.Asn1File) :Asn1Ast.Asn1File =
    {
        Asn1Ast.Asn1File.FileName = f.FileName
        Tokens = f.Tokens
        Modules = f.Modules |> List.map (MapModule r)
    }



let DoWork (r:ParameterizedAsn1Ast.AstRoot) : Asn1Ast.AstRoot =
    let r = RemoveParameterizedTypes.DoWork r
    let files = r.Files |> List.map (MapFile r)
    {
        Asn1Ast.AstRoot.Files = files
        args = r.args
        typeAssignmentsMap = 
            seq {
                for f in files do
                    for m in f.Modules do
                        for tas in m.TypeAssignments do
                            yield ((m.Name.Value, tas.Name.Value), tas)
            } |> Map.ofSeq
    }
