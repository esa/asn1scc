module DAstAsn1

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

let printComponent (c:ObjectIdentifierValueComponent) =
    match c with
    | ObjInteger            nVal                    -> nVal.Value.ToString()
    | ObjNamedDefValue      (label,(md,ts))         -> sprintf "%s(%s.%s)" label.Value md.Value ts.Value //named form, points to an integer value
    | ObjNamedIntValue      (label, nVal)           -> sprintf "%s(%s)" label.Value (nVal.Value.ToString())                     //name form
    | ObjRegisteredKeyword  (label, nVal)           -> sprintf "%s(%s)" label.Value (nVal.ToString())                     //
    | ObjDefinedValue       (md,ts)                 -> sprintf "%s.%s" md.Value ts.Value //named form, points to an integer value                  //value assignment to Integer value or ObjectIdentifier or RelativeObject

let rec printAsn1Value (v:Asn1AcnAst.Asn1Value) =
    match v.kind with
    | Asn1AcnAst.IntegerValue        v       -> stg_asn1.Print_IntegerValue v.Value
    | Asn1AcnAst.EnumValue           v       -> v.Value
    | Asn1AcnAst.RealValue           v       -> stg_asn1.Print_RealValue v.Value
    | Asn1AcnAst.StringValue(parts,_)          ->
        match parts with
        | (CStringValue v)::[] ->        stg_asn1.Print_StringValue v
        | _     ->        stg_asn1.Print_SeqOfValue (parts |> List.map(fun p -> p.AsAsn1))
    | Asn1AcnAst.BooleanValue        v       -> stg_asn1.Print_BooleanValue v.Value
    | Asn1AcnAst.BitStringValue      v       -> stg_asn1.Print_BitStringValue v.Value
    | Asn1AcnAst.OctetStringValue    v       -> stg_asn1.Print_OctetStringValue (v |> List.map (fun b -> b.Value))
    | Asn1AcnAst.RefValue       ((md,ts),_)  -> stg_asn1.Print_RefValue  ts.Value
    | Asn1AcnAst.SeqOfValue          vals    -> stg_asn1.Print_SeqOfValue (vals |> List.map printAsn1Value)
    | Asn1AcnAst.SeqValue            vals    -> stg_asn1.Print_SeqValue (vals |> List.map (fun nmv -> stg_asn1.Print_SeqValue_Child nmv.name.Value (printAsn1Value nmv.Value)))
    | Asn1AcnAst.ChValue             nmv     -> stg_asn1.Print_ChValue nmv.name.Value (printAsn1Value nmv.Value)
    | Asn1AcnAst.NullValue           _       -> stg_asn1.Print_NullValue ()
    | Asn1AcnAst.ObjOrRelObjIdValue (_,coms) ->
        stg_asn1.Print_ObjOrRelObjIdValue (coms |> List.map printComponent)
    | Asn1AcnAst.TimeValue v                 -> stg_asn1.Print_StringValue (CommonTypes.asn1DateTimeValueToString v.Value)


let foldGenericCon valToStrFunc  (c:GenericConstraint<'v>)  =
    foldGenericConstraint
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (valToStrFunc v) ,s)
        c
        0 |> fst

let foldRangeCon valToStrFunc1 valToStrFunc2  (c:RangeTypeConstraint<'v1,'v2>)  =
    foldRangeTypeConstraint
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (valToStrFunc2 v) ,s)
        (fun _ v1 v2  minIsIn maxIsIn s   -> stg_asn1.Print_RangeConstraint (valToStrFunc1 v1) (valToStrFunc1 v2) minIsIn  maxIsIn , s)
        (fun _ v1 minIsIn s   -> stg_asn1.Print_RangeConstraint_val_MAX  (valToStrFunc1 v1) minIsIn, s)
        (fun _ v2 maxIsIn s   -> stg_asn1.Print_RangeConstraint_MIN_val (valToStrFunc1 v2) maxIsIn, s)
        c
        0 |> fst



let foldSizableConstraint printSingValueFunc   (c:SizableTypeConstraint<'v>) =
    foldSizableTypeConstraint2
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (printSingValueFunc v) ,s)
        (fun _ intCon s       -> foldRangeCon (fun i -> i.ToString()) (fun i -> i.ToString()) intCon, s)
        c
        0 |> fst

let foldSequenceOfConstraint printSingValueFunc   (c:SequenceOfConstraint) =
    foldSequenceOfTypeConstraint2
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (printSingValueFunc v) ,s)
        (fun _ intCon s       -> foldRangeCon (fun i -> i.ToString()) (fun i -> i.ToString()) intCon, s)
        (fun _ c l s          -> "", s)
        c
        0 |> fst

let foldStringCon    (c:IA5StringConstraint)  =
    foldStringTypeConstraint2
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (stg_asn1.Print_StringValue v ),s)
        (fun _ intCon s       -> foldRangeCon  (fun i -> i.ToString()) (fun i -> i.ToString()) intCon , s)
        (fun _ alphcon s      -> foldRangeCon  (fun i -> "\"" + i.ToString() + "\"") (fun i -> "\"" + i.ToString() + "\"") alphcon,s)
        c
        0 |> fst

let foldSequenceCon valToStrFunc  (c:SeqConstraint)  =
    foldSeqConstraint
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (valToStrFunc v) ,s)
        (fun _ nc s           -> "", s)
        c
        0 |> fst

let foldChoiceCon valToStrFunc  (c:ChoiceConstraint)  =
    foldChoiceConstraint
        (fun _ e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun _ e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun _ e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun _ v  s           -> stg_asn1.Print_SingleValueConstraint (valToStrFunc v) ,s)
        (fun _ nc s           -> "", s)
        c
        0 |> fst

let createAcnInteger (cons:IntegerTypeConstraint list) =
    let conToStrFunc = foldRangeCon  stg_asn1.Print_IntegerValue stg_asn1.Print_IntegerValue
    cons |> List.map conToStrFunc

let createIntegerFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer)  =
    let conToStrFunc = foldRangeCon  stg_asn1.Print_IntegerValue stg_asn1.Print_IntegerValue
    o.AllCons |> List.map conToStrFunc

let createRealFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real)  =
    let conToStrFunc = foldRangeCon  stg_asn1.Print_RealValue stg_asn1.Print_RealValue
    o.AllCons |> List.map conToStrFunc

let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier)  =
    let conToStrFunc =
        foldGenericCon (fun (_,coms) -> stg_asn1.Print_ObjOrRelObjIdValue (coms |> List.map printComponent))
    o.AllCons |> List.map conToStrFunc


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType)  =
    let conToStrFunc =
        foldGenericCon (fun (v:TimeValue) -> stg_asn1.Print_TimeValue (asn1DateTimeValueToString v))
    o.AllCons |> List.map conToStrFunc


let createStringFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  =
    o.AllCons |> List.map foldStringCon

let createBoolFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) =
    let conToStrFunc = foldGenericCon (fun b -> b.ToString().ToUpper())
    o.AllCons |> List.map conToStrFunc

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) =
    let conToStrFunc = foldGenericCon (fun b -> b)
    //remove the "virtual constraint" that is added in all ENUMERATED which constraints the type to all of each possible values
    let actualConstraints =
        o.AllCons |>
        List.filter(fun c ->
            match c with
            | Asn1AcnAst.UnionConstraint(_,_,_,virtCon)   -> not virtCon
            | _                                         -> true)
    actualConstraints |> List.map conToStrFunc

//type OctetStringConstraint  =    SizableTypeConstraint<OctetStringValue*(ReferenceToValue*SrcLoc)>

let createOctetStringFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  =
    let conToStrFunc = foldSizableConstraint (fun (bytesLoc:Asn1AcnAst.OctetStringValue, _) -> bytesLoc |> List.map (fun b-> b.Value) |> stg_asn1.Print_OctetStringValue )
    o.AllCons |> List.map conToStrFunc

let createBitStringFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)  =
    let conToStrFunc = foldSizableConstraint (fun (bits:Asn1AcnAst.BitStringValue, _) -> stg_asn1.Print_BitStringValue ("'" + bits.Value + "'H" ))
    o.AllCons |> List.map conToStrFunc


let createSequenceFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (children:SeqChildInfo list)   =
    let printSeqValue (nameItems:Asn1AcnAst.NamedValue list) =
        let arrsValues = nameItems |> List.map(fun ni -> stg_asn1.Print_SeqValue_Child ni.name.Value (printAsn1Value ni.Value))
        stg_asn1.Print_SeqValue arrsValues
    let conToStrFunc = foldSequenceCon printSeqValue
    o.AllCons |> List.map conToStrFunc


let createChoiceFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (children:ChChildInfo list)   =
    let printChValue (ni:Asn1AcnAst.NamedValue) =
        stg_asn1.Print_ChValue ni.name.Value (printAsn1Value ni.Value)
    let conToStrFunc = foldChoiceCon printChValue
    o.AllCons |> List.map conToStrFunc

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  =
    let conToStrFunc = foldSequenceOfConstraint (fun (vals:Asn1AcnAst.Asn1Value list) -> vals |> List.map printAsn1Value |> stg_asn1.Print_SeqOfValue )
    o.AllCons |> List.map conToStrFunc
