module DAstBaseTypesEquivalence

open System
open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
//open Constraints
//open uPER2
open FsUtils
open DAst


let getInteger (t:Asn1AcnAst.Integer) (bo:Integer option) =
    let typedefBase =
        match bo with
        | None                                          -> None
        | Some bo   when t.isUnsigned = bo.baseInfo.isUnsigned   -> Some bo
        | Some _                                        -> None

    let uper =
        match bo with
        | None                 -> None
        | Some bo              ->
            match t.cons with
            | []            -> typedefBase
            | _             -> None
    
            

    {BaseTypesEquivalence.typeDefinition = typedefBase; uper = uper; acn = None}

let getReal (t:Asn1AcnAst.Real) (bo:Real option) =
    {BaseTypesEquivalence.typeDefinition = bo; uper = bo; acn = None}

let getIA5String (t:Asn1AcnAst.StringType) (bo:StringType option) =
    let typedefBase  =
        match bo with
        | None                                          -> None
        | Some bo   when t.maxSize.uper = bo.baseInfo.maxSize.uper  && t.minSize.uper = bo.baseInfo.minSize.uper    -> Some bo
        | Some _                                        -> None

    let uperBase  =
        match bo with
        | None                 -> None
        | Some bo   when t.maxSize.uper = bo.baseInfo.maxSize.uper  && t.minSize.uper = bo.baseInfo.minSize.uper  && t.uperCharSet = bo.baseInfo.uperCharSet
            -> Some bo
        | Some _                                        -> None
    {BaseTypesEquivalence.typeDefinition = typedefBase; uper = uperBase; acn = None}


let getOctetString (t:Asn1AcnAst.OctetString) (bo:OctetString option) =
    let typedefBase  =
        match bo with
        | None                                          -> None
        | Some bo   when t.maxSize.uper = bo.baseInfo.maxSize.uper  && t.minSize.uper = bo.baseInfo.minSize.uper    -> Some bo
        | Some _                                        -> None
    let uperBase  = typedefBase
    {BaseTypesEquivalence.typeDefinition = typedefBase; uper = uperBase; acn = None}



let getBitString (t:Asn1AcnAst.BitString) (bo:BitString option) =
    let typedefBase  =
        match bo with
        | None                                          -> None
        | Some bo   when t.maxSize.uper = bo.baseInfo.maxSize.uper  && t.minSize.uper = bo.baseInfo.minSize.uper    -> Some bo
        | Some _                                        -> None
    let uperBase  = typedefBase
    {BaseTypesEquivalence.typeDefinition = typedefBase; uper = uperBase; acn = None}


let getBoolean  (t: Asn1AcnAst.Boolean    ) (bo:Boolean    option) = 
    {BaseTypesEquivalence.typeDefinition = bo; uper = bo; acn = None}

let getNullType      (t:Asn1AcnAst.NullType   ) (bo:NullType   option) = 
    {BaseTypesEquivalence.typeDefinition = bo; uper = bo; acn = None}

let getEnumerated    (t:Asn1AcnAst.Enumerated ) (bo:Enumerated option) = 
    {BaseTypesEquivalence.typeDefinition = bo; uper = bo; acn = None}

let getSequenceOf    (t:Asn1AcnAst.SequenceOf ) (bo:SequenceOf option) = 
    let typedefBase  =
        match bo with
        | None                                          -> None
        | Some bo   when t.maxSize.uper = bo.baseInfo.maxSize.uper  && t.minSize.uper = bo.baseInfo.minSize.uper    -> Some bo
        | Some _                                        -> None
    let uperBase  = typedefBase
    {BaseTypesEquivalence.typeDefinition = typedefBase; uper = uperBase; acn = None}

let getSequence      (t:Asn1AcnAst.Sequence   ) (bo:Sequence   option) = 
    {BaseTypesEquivalence.typeDefinition = bo; uper = bo; acn = None}

let getChoice        (t:Asn1AcnAst.Choice     ) (bo:Choice     option) = 
    {BaseTypesEquivalence.typeDefinition = bo; uper = bo; acn = None}

