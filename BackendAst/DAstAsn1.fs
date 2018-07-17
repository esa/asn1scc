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

let rec printAsn1Value (v:Asn1AcnAst.Asn1Value) = 
    match v.kind with
    | Asn1AcnAst.IntegerValue        v       -> stg_asn1.Print_IntegerValue v.Value
    | Asn1AcnAst.EnumValue           v       -> v.Value
    | Asn1AcnAst.RealValue           v       -> stg_asn1.Print_RealValue v.Value
    | Asn1AcnAst.StringValue         v       -> stg_asn1.Print_StringValue v.Value
    | Asn1AcnAst.BooleanValue        v       -> stg_asn1.Print_BooleanValue v.Value
    | Asn1AcnAst.BitStringValue      v       -> stg_asn1.Print_BitStringValue v.Value
    | Asn1AcnAst.OctetStringValue    v       -> stg_asn1.Print_OctetStringValue (v |> List.map (fun b -> b.Value))
    | Asn1AcnAst.RefValue       ((md,ts),_)  -> stg_asn1.Print_RefValue  ts.Value
    | Asn1AcnAst.SeqOfValue          vals    -> stg_asn1.Print_SeqOfValue (vals |> List.map printAsn1Value)
    | Asn1AcnAst.SeqValue            vals    -> stg_asn1.Print_SeqValue (vals |> List.map (fun nmv -> stg_asn1.Print_SeqValue_Child nmv.name.Value (printAsn1Value nmv.Value)))
    | Asn1AcnAst.ChValue             nmv     -> stg_asn1.Print_ChValue nmv.name.Value (printAsn1Value nmv.Value)
    | Asn1AcnAst.NullValue           _       -> stg_asn1.Print_NullValue ()

let foldGenericCon valToStrFunc  (c:GenericConstraint<'v>)  =
    foldGenericConstraint
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> stg_asn1.Print_SingleValueContraint (valToStrFunc v) ,s)
        c
        0 |> fst

let foldRangeCon valToStrFunc1 valToStrFunc2  (c:RangeTypeConstraint<'v1,'v2>)  =
    foldRangeTypeConstraint        
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> stg_asn1.Print_SingleValueContraint (valToStrFunc2 v) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> stg_asn1.Print_RangeContraint (valToStrFunc1 v1) (valToStrFunc1 v2) minIsIn  maxIsIn , s)
        (fun v1 minIsIn s   -> stg_asn1.Print_RangeContraint_val_MAX  (valToStrFunc1 v1) minIsIn, s)
        (fun v2 maxIsIn s   -> stg_asn1.Print_RangeContraint_MIN_val (valToStrFunc1 v2) maxIsIn, s)
        c
        0 |> fst



let foldSizableConstraint printSingValueFunc   (c:SizableTypeConstraint<'v>) =
    foldSizableTypeConstraint2
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> stg_asn1.Print_SingleValueContraint (printSingValueFunc v) ,s)
        (fun intCon s       -> foldRangeCon (fun i -> i.ToString()) (fun i -> i.ToString()) intCon, s)
        c
        0 |> fst

let foldStringCon    (c:IA5StringConstraint)  =
    foldStringTypeConstraint2
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> stg_asn1.Print_SingleValueContraint (stg_asn1.Print_StringValue v ),s)
        (fun intCon s       -> foldRangeCon  (fun i -> i.ToString()) (fun i -> i.ToString()) intCon , s)
        (fun alphcon s      -> foldRangeCon  (fun i -> "\"" + i.ToString() + "\"") (fun i -> "\"" + i.ToString() + "\"") alphcon,s) 
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


let createStringFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  =
    o.AllCons |> List.map foldStringCon

let createBoolFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) =
    let conToStrFunc = foldGenericCon (fun b -> b.ToString().ToUpper())
    o.AllCons |> List.map conToStrFunc

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) =
    let conToStrFunc = foldGenericCon (fun b -> b)
    //remove the "virtual constraint" that is added in all ENUMERAED wich constraints the type to all of each possible values
    let actualConstraints = 
        o.AllCons |>
        List.filter(fun c ->
            match c with
            | Asn1AcnAst.UnionConstraint(_,_,virtCon)   -> not virtCon
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
    let conToStrFunc = foldGenericCon printSeqValue
    o.AllCons |> List.map conToStrFunc
    
let createChoiceFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (children:ChChildInfo list)   =
    let printChValue (ni:Asn1AcnAst.NamedValue) =
        stg_asn1.Print_ChValue ni.name.Value (printAsn1Value ni.Value)
    let conToStrFunc = foldGenericCon printChValue
    o.AllCons |> List.map conToStrFunc

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  =
    let conToStrFunc = foldSizableConstraint (fun (vals:Asn1AcnAst.Asn1Value list) -> vals |> List.map printAsn1Value |> stg_asn1.Print_SeqOfValue )
    o.AllCons |> List.map conToStrFunc
