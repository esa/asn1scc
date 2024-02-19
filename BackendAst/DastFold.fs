module DastFold

open System
open System.Numerics
open Asn1AcnAst
open DAst
open DAstUtilFunctions
let foldMap = Asn1Fold.foldMap

let foldAsn1Type
    (t:Asn1Type) 
    (us:'UserState) 
    integerFunc 
    realFunc
    ia5StringFunc
    octetStringFunc
    nullTypeFunc
    bitStringFunc
    booleanFunc
    enumeratedFunc
    objectIdentifierFunc
    timeTypeFunc
    seqOfTypeFunc
    
    seqAsn1ChildFunc
    seqAcnChildFunc
    seqTypeFunc
    
    chChild
    chTypeFunc
    refTypeFunc
    typeFunc
    =
    let rec loopType (t:Asn1Type) (us:'UserState) =
        let newKind = 
            match t.Kind with
            | Integer           cnt -> integerFunc t cnt us
            | Real              cnt -> realFunc t cnt us
            | IA5String         cnt -> ia5StringFunc t cnt us
            | OctetString       cnt -> octetStringFunc t cnt us
            | NullType          cnt -> nullTypeFunc t cnt us
            | BitString         cnt -> bitStringFunc t cnt us
            | Boolean           cnt -> booleanFunc t cnt us
            | Enumerated        cnt -> enumeratedFunc t cnt us
            | ObjectIdentifier  cnt -> objectIdentifierFunc t cnt us
            | TimeType          cnt -> timeTypeFunc t cnt us
            | SequenceOf        cnt -> 
                let newChildType = loopType cnt.childType us
                seqOfTypeFunc t cnt newChildType 
            | Sequence          seqInfo -> 
                let newChildren =
                    seqInfo.children |> 
                    foldMap (fun curState ch -> 
                                    match ch with
                                    | Asn1Child asn1Chlld   -> seqAsn1ChildFunc t seqInfo asn1Chlld (loopType asn1Chlld.Type curState)
                                    | AcnChild  acnChild    -> seqAcnChildFunc  t seqInfo acnChild curState) us

                                    //seqChild ch (loopType ch.chType  curState)) us
                seqTypeFunc t seqInfo newChildren
            | Choice            chInfo -> 
                let newChildren = chInfo.children |> foldMap (fun curState ch -> chChild t chInfo ch (loopType ch.chType  curState)) us
                chTypeFunc t chInfo newChildren
            | ReferenceType ref     ->
                let baseType = loopType ref.resolvedType us
                refTypeFunc t ref baseType
        typeFunc t newKind
    loopType t us

let getValueFromSizeableConstraint (c:SizableTypeConstraint<'v>) =
    Asn1Fold.foldSizableTypeConstraint2
        (fun _ e1 e2 b s      -> e1@e2, s)
        (fun _ e1 e2 s        -> e1@e2, s)
        (fun _ e s            -> e, s)
        (fun _ e1 e2 s        -> e1@e2, s)
        (fun _ e s            -> e, s)
        (fun _ e1 e2 s        -> e1@e2, s)
        (fun _ v  s           -> [v] ,s)
        (fun _ intCon s       -> [],s)
        c
        0 |> fst

(*
let rec getOctetStringValues (t:Asn1Type) =
    seq {
        match t.Kind with
        | OctetString o -> 
            let octVals = (o.baseInfo.cons@o.baseInfo.withcons) |> List.map getValueFromSizeableConstraint |> List.collect id
            yield! octVals
        | Sequence seq  ->
            for ch in seq.Asn1Children do
                yield! getOctetStringValues ch.Type
        | Choice ch     ->
            for ch in ch.children do
                yield! getOctetStringValues ch.chType
        | SequenceOf ch  ->
            yield! getOctetStringValues ch.childType
        | _             -> ()
    } |> Seq.toList

let rec getBitStringValues (t:Asn1Type) =
    seq {
        match t.Kind with
        | BitString o -> 
            let octVals = (o.baseInfo.cons@o.baseInfo.withcons) |> List.map getValueFromSizeableConstraint |> List.collect id
            yield! octVals
        | Sequence seq  ->
            for ch in seq.Asn1Children do
                yield! getBitStringValues ch.Type
        | Choice ch     ->
            for ch in ch.children do
                yield! getBitStringValues ch.chType
        | SequenceOf ch  ->
            yield! getBitStringValues ch.childType
        | _             -> ()
    } |> Seq.toList
*)