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
    (integerFunc: Asn1Type -> Integer -> 'UserState -> 'a)
    (realFunc: Asn1Type -> Real -> 'UserState -> 'a)
    (ia5StringFunc: Asn1Type -> StringType -> 'UserState -> 'a)
    (octetStringFunc: Asn1Type -> OctetString -> 'UserState -> 'a)
    (nullTypeFunc: Asn1Type -> NullType -> 'UserState -> 'a)
    (bitStringFunc: Asn1Type -> BitString -> 'UserState -> 'a)
    (booleanFunc: Asn1Type -> Boolean -> 'UserState -> 'a)
    (enumeratedFunc: Asn1Type -> Enumerated -> 'UserState -> 'a)
    (objectIdentifierFunc: Asn1Type -> ObjectIdentifier -> 'UserState -> 'a)
    (timeTypeFunc: Asn1Type -> TimeType -> 'UserState -> 'a)
    (seqOfTypeFunc: Asn1Type -> SequenceOf -> 'b -> 'a)

    (seqAsn1ChildFunc: Asn1Type -> Sequence -> Asn1Child -> 'b -> 'c * 'UserState)
    (seqAcnChildFunc: Asn1Type -> Sequence -> AcnChild -> 'UserState -> 'c * 'UserState)
    (seqTypeFunc: Asn1Type -> Sequence -> 'c list * 'UserState -> 'a)

    (chChild: Asn1Type -> Choice -> ChChildInfo -> 'b -> 'd * 'UserState)
    (chTypeFunc: Asn1Type -> Choice -> 'd list * 'UserState -> 'a)
    (refTypeFunc: Asn1Type -> ReferenceType -> 'b -> 'a)
    (typeFunc: Asn1Type -> 'a -> 'b) : 'b
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
                                    | Asn1Child asn1Child   -> seqAsn1ChildFunc t seqInfo asn1Child (loopType asn1Child.Type curState)
                                    | AcnChild  acnChild    -> seqAcnChildFunc  t seqInfo acnChild curState) us

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
