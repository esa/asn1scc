module Asn1Fold

open Asn1AcnAst
open Asn1AcnAstUtilFunctions
(*
let rec foldMap func state lst =
    match lst with
    | []        -> [],state
    | h::tail   ->
        let procItem, newState = func state h
        let restList, finalState = tail |> foldMap func newState
        procItem::restList, finalState

*)

//let foldMap = RemoveParameterizedTypes.foldMap
(*
let foldMap func state lst =
    let rec loop acc func state lst =
        match lst with
        | []        -> acc |> List.rev , state
        | h::tail   ->
            let procItem, newState = func state h
            //let restList, finalState = tail |> loop func newState
            //procItem::restList, finalState
            loop (procItem::acc) func newState tail
    loop [] func state lst
*)
let foldMap = List.mapFold

let foldGenericConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    (c:GenericConstraint<'v>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:GenericConstraint<'v>) (s0:'UserState) =
        match c with
        | UnionConstraint(s, c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc s nc1 nc2 b s2
        | IntersectionConstraint(s, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc s nc1 nc2 s2
        | AllExceptConstraint(s, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc s nc1 s1
        | ExceptConstraint(s, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc s nc1 nc2 s2
        | RootConstraint(s, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc s nc1 s1
        | RootConstraint2(s, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 s nc1 nc2 s2
        | SingleValueConstraint (s, v)    -> singleValueFunc s v s0
    loopRecursiveConstraint c s


let foldRangeTypeConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    rangeFunc range_val_max_func range_min_val_func
    (c:RangeTypeConstraint<'v,'vr>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:RangeTypeConstraint<'v,'vr>) (s0:'UserState) =
        match c with
        | RangeUnionConstraint(s, c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc s nc1 nc2 b s2
        | RangeIntersectionConstraint(s, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc s nc1 nc2 s2
        | RangeAllExceptConstraint(s, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc s nc1 s1
        | RangeExceptConstraint(s, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc s nc1 nc2 s2
        | RangeRootConstraint(s, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc s nc1 s1
        | RangeRootConstraint2(s, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 s nc1 nc2 s2
        | RangeSingleValueConstraint (s, v)            -> singleValueFunc s v s0
        | RangeConstraint(a, (v1), (v2), b1,b2)     -> rangeFunc a v1 v2 b1 b2 s
        | RangeConstraint_val_MAX (a, (v), b)            -> range_val_max_func a v b s
        | RangeConstraint_MIN_val (a, (v), b)            -> range_min_val_func a v b s
    loopRecursiveConstraint c s

let foldSizableTypeConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc
    srangeFunc srange_val_max_func srange_min_val_func
    (c:SizableTypeConstraint<'v>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:SizableTypeConstraint<'v>) (s0:'UserState) =
        match c with
        | SizeUnionConstraint(asn1, c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc asn1 nc1 nc2 b s2
        | SizeIntersectionConstraint(asn1, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc asn1 nc1 nc2 s2
        | SizeAllExceptConstraint(asn1, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc asn1 nc1 s1
        | SizeExceptConstraint(asn1, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc asn1 nc1 nc2 s2
        | SizeRootConstraint(asn1, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc asn1 nc1 s1
        | SizeRootConstraint2(asn1, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 asn1 nc1 nc2 s2
        | SizeSingleValueConstraint (asn1, v)    -> singleValueFunc asn1 v s0
        | SizeConstraint (asn1, intCon)   -> foldRangeTypeConstraint sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc srangeFunc srange_val_max_func srange_min_val_func intCon s0
    loopRecursiveConstraint c s

let foldSizableTypeConstraint2 unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    foldRangeTypeConstraint
    (c:SizableTypeConstraint<'v>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:SizableTypeConstraint<'v>) (s0:'UserState) =
        match c with
        | SizeUnionConstraint(s, c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc s nc1 nc2 b s2
        | SizeIntersectionConstraint(s, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc s nc1 nc2 s2
        | SizeAllExceptConstraint(s, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc s nc1 s1
        | SizeExceptConstraint(s, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc s nc1 nc2 s2
        | SizeRootConstraint(s, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc s nc1 s1
        | SizeRootConstraint2(s, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 s nc1 nc2 s2
        | SizeSingleValueConstraint (s, v)    -> singleValueFunc s v s0
        | SizeConstraint (s,intCon)   -> foldRangeTypeConstraint s intCon s0
    loopRecursiveConstraint c s


let foldStringTypeConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc srangeFunc srange_val_max_func srange_min_val_func
    aunionFunc aintersectionFunc aallExceptFunc aexceptFunc arootFunc arootFunc2 asingleValueFunc arangeFunc arange_val_max_func arange_min_val_func
    (c:IA5StringConstraint)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:IA5StringConstraint) (s0:'UserState) =
        match c with
        | StrUnionConstraint(s, c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc s nc1 nc2 b s2
        | StrIntersectionConstraint(s, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc s nc1 nc2 s2
        | StrAllExceptConstraint(s, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc s nc1 s1
        | StrExceptConstraint(s, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc s nc1 nc2 s2
        | StrRootConstraint(s, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc s nc1 s1
        | StrRootConstraint2(s, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 s nc1 nc2 s2
        | StrSingleValueConstraint (s, v)    -> singleValueFunc s v s0
        | StrSizeConstraint (s,intCon)   -> foldRangeTypeConstraint sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc srangeFunc srange_val_max_func srange_min_val_func intCon s0
        | AlphabetConstraint (s,alphaCon) -> foldRangeTypeConstraint aunionFunc aintersectionFunc aallExceptFunc aexceptFunc arootFunc arootFunc2 asingleValueFunc arangeFunc arange_val_max_func arange_min_val_func alphaCon s0
    loopRecursiveConstraint c s


let foldStringTypeConstraint2 unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    foldRangeSizeConstraint foldRangeAlphaConstraint
    (c:IA5StringConstraint)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:IA5StringConstraint) (s0:'UserState) =
        match c with
        | StrUnionConstraint(s, c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc s nc1 nc2 b s2
        | StrIntersectionConstraint(s, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc s nc1 nc2 s2
        | StrAllExceptConstraint(s, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc s nc1 s1
        | StrExceptConstraint(s, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc s nc1 nc2 s2
        | StrRootConstraint(s, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc s nc1 s1
        | StrRootConstraint2(s, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 s nc1 nc2 s2
        | StrSingleValueConstraint (s, v)     -> singleValueFunc s v s0
        | StrSizeConstraint  (s,intCon)   -> foldRangeSizeConstraint  s intCon s0
        | AlphabetConstraint (s, alphaCon) -> foldRangeAlphaConstraint s alphaCon s0
    loopRecursiveConstraint c s



let foldSeqOrChConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc withComponentsFunc
    (c:SeqOrChoiceConstraint<'v>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:SeqOrChoiceConstraint<'v>) (s0:'UserState) =
        match c with
        | SeqOrChUnionConstraint(s,c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc s nc1 nc2 b s2
        | SeqOrChIntersectionConstraint(s, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc s nc1 nc2 s2
        | SeqOrChAllExceptConstraint(s, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc s nc1 s1
        | SeqOrChExceptConstraint(s, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc s nc1 nc2 s2
        | SeqOrChRootConstraint(s, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc s nc1 s1
        | SeqOrChRootConstraint2(s, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 s nc1 nc2 s2
        | SeqOrChSingleValueConstraint (s, v)    -> singleValueFunc s v s0
        | SeqOrChWithComponentsConstraint (s,nitms) -> withComponentsFunc s nitms s0
    loopRecursiveConstraint c s


let foldSeqConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc withComponentsFunc
    (c:SeqConstraint)
    (s:'UserState) =
    foldSeqOrChConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc withComponentsFunc c s

let foldChoiceConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc withComponentsFunc
    (c:ChoiceConstraint)
    (s:'UserState) =
    foldSeqOrChConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc withComponentsFunc c s

let foldSequenceOfTypeConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc
    srangeFunc srange_val_max_func srange_min_val_func
    withComponentFunc
    (c:SequenceOfConstraint)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:SequenceOfConstraint) (s0:'UserState) =
        match c with
        | SeqOfSizeUnionConstraint(s, c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc s nc1 nc2 b s2
        | SeqOfSizeIntersectionConstraint(s, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc s nc1 nc2 s2
        | SeqOfSizeAllExceptConstraint(s, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc s nc1 s1
        | SeqOfSizeExceptConstraint(s, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc s nc1 nc2 s2
        | SeqOfSizeRootConstraint(s, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc s nc1 s1
        | SeqOfSizeRootConstraint2(s, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 s nc1 nc2 s2
        | SeqOfSizeSingleValueConstraint (s, v)    -> singleValueFunc s v s0
        | SeqOfSizeConstraint (_, intCon)   -> foldRangeTypeConstraint sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc srangeFunc srange_val_max_func srange_min_val_func intCon s
        | SeqOfSeqWithComponentConstraint (s, c,l) -> withComponentFunc s c l s0
    loopRecursiveConstraint c s


let foldSequenceOfTypeConstraint2 unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    foldRangeTypeConstraint
    withComponentFunc
    (c:SequenceOfConstraint)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:SequenceOfConstraint) (s0:'UserState) =
        match c with
        | SeqOfSizeUnionConstraint(s, c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc s nc1 nc2 b s2
        | SeqOfSizeIntersectionConstraint(s, c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc s nc1 nc2 s2
        | SeqOfSizeAllExceptConstraint(s, c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc s nc1 s1
        | SeqOfSizeExceptConstraint(s, c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc s nc1 nc2 s2
        | SeqOfSizeRootConstraint(s, c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc s nc1 s1
        | SeqOfSizeRootConstraint2(s, c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 s nc1 nc2 s2
        | SeqOfSizeSingleValueConstraint (s, v)    -> singleValueFunc s v s0
        | SeqOfSizeConstraint (s,intCon)   -> foldRangeTypeConstraint s intCon s0
        | SeqOfSeqWithComponentConstraint (s, c,l) -> withComponentFunc s c l s0
    loopRecursiveConstraint c s


let foldType
    intFunc
    realFunc
    ia5StringFunc
    numStringFunc
    octStringFunc
    timeFunc
    nullTypeFunc
    bitStringFunc
    boolFunc
    enumFunc
    objectIdFunc
    seqOfFunc
    seqFunc
    seqAsn1ChildFunc
    seqAcnChildFunc
    choiceFunc
    chChildFunc
    refType
    typeFunc
    (t:Asn1Type)
    (us:'UserState)
    =
    let rec loopType (t:Asn1Type) (us:'UserState) =
        let newKind, newState =
            match t.Kind with
            | Integer        ti -> intFunc ti us
            | Real           ti -> realFunc ti us
            | IA5String      ti -> ia5StringFunc ti us
            | NumericString  ti -> numStringFunc ti us
            | OctetString    ti -> octStringFunc ti us
            | TimeType       ti -> timeFunc ti us
            | NullType       ti -> nullTypeFunc ti us
            | BitString      ti -> bitStringFunc ti us
            | Boolean        ti -> boolFunc ti us
            | Enumerated     ti -> enumFunc ti us
            | ObjectIdentifier ti -> objectIdFunc ti us
            | SequenceOf     ti ->
                let newChild, newState = loopType ti.child us
                seqOfFunc ti newChild newState
            | Sequence       ti ->
                let newChildren, newState =
                    ti.children |>
                    foldMap (fun curState ch ->
                        match ch with
                        | Asn1Child asn1Child   ->
                            let newChildType, newState = loopType asn1Child.Type curState
                            seqAsn1ChildFunc asn1Child newChildType newState
                        | AcnChild  acnChild    ->
                            seqAcnChildFunc acnChild curState) us
                seqFunc ti newChildren newState
            | Choice         ti ->
                let newChildren, newState =
                    ti.children |>
                    foldMap (fun curState ch ->
                        let newChildType, newState = loopType ch.Type curState
                        chChildFunc ch newChildType newState) us
                choiceFunc ti newChildren newState
            | ReferenceType  ti ->
               let newBaseType, newState = loopType ti.resolvedType us
               refType ti newBaseType newState
        typeFunc t newKind newState
    loopType t us

/// Provides information about the parent of one type.
type ParentInfo<'T> = {
    /// the parent ASN.1 Type
    parent : Asn1Type
    /// the name of the component or alternative this type exists
    name   : string option
    /// Information obtained by the preSeqOfFunc, preSeqFunc and preChoiceFunc
    /// which are called before visiting the children
    parentData : 'T
}

let dummy a =
    let parId = "PUS.SpacePacket"
    let c1 = "PUS.SpacePacket.header.packet-length"
    let c2 = "PUS.SpacePacket.payload"

    let aaa = c2.Substring(parId.Length + 1).Split('.').[0]
    let aaa = (c1.Substring(parId.Length + 1)).Split('.').[0]

    0

let getSeqChildrenWithPriority (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (t:Asn1Type)  =
    let id = t.id.AsString
    let getChildNameFromId (childId:string) =
        childId.Substring(id.Length + 1).Split('.').[0]
    let childrenWithPriority =
        deps.acnDependencies |>
        List.choose(fun d ->
            match d.dependencyKind with
            | AcnDepSizeDeterminant_bit_oct_str_contain _ when d.asn1Type.AsString.StartsWith (id)  &&   d.determinant.id.AsString.StartsWith (id) &&                (getChildNameFromId d.asn1Type.AsString) <> (getChildNameFromId d.determinant.id.AsString) -> Some (getChildNameFromId d.asn1Type.AsString )
            | _ -> None    )

    childrenWithPriority

let foldType2
    (intFunc: ParentInfo<'T> option -> Asn1Type -> Integer -> 'UserState -> 'a)
    (realFunc: ParentInfo<'T> option -> Asn1Type -> Real -> 'UserState -> 'a)
    (ia5StringFunc: ParentInfo<'T> option -> Asn1Type -> StringType -> 'UserState -> 'a)
    (numStringFunc: ParentInfo<'T> option -> Asn1Type -> StringType -> 'UserState -> 'a)
    (octStringFunc: ParentInfo<'T> option -> Asn1Type -> OctetString -> 'UserState -> 'a)
    (timeFunc: ParentInfo<'T> option -> Asn1Type -> TimeType -> 'UserState -> 'a)
    (nullTypeFunc: ParentInfo<'T> option -> Asn1Type -> NullType -> 'UserState -> 'a)
    (bitStringFunc: ParentInfo<'T> option -> Asn1Type -> BitString -> 'UserState -> 'a)
    (boolFunc: ParentInfo<'T> option -> Asn1Type -> Boolean -> 'UserState -> 'a)
    (enumFunc: ParentInfo<'T> option -> Asn1Type -> Enumerated -> 'UserState -> 'a)
    (objectIdFunc: ParentInfo<'T> option -> Asn1Type -> ObjectIdentifier -> 'UserState -> 'a)
    (seqOfFunc: ParentInfo<'T> option -> Asn1Type -> SequenceOf -> 'b -> 'a)
    (seqFunc: ParentInfo<'T> option -> Asn1Type -> Sequence -> 'c list * 'UserState -> 'a)
    (seqAsn1ChildFunc: Asn1Child -> 'b -> 'c * 'UserState)
    (seqAcnChildFunc: AcnChild -> 'UserState -> 'c * 'UserState)
    (choiceFunc: ParentInfo<'T> option -> Asn1Type -> Choice -> 'd list * 'UserState -> 'a)
    (chChildFunc: ChChildInfo -> 'b -> 'd * 'UserState)
    (refType: ParentInfo<'T> option -> Asn1Type -> ReferenceType -> 'b -> 'a)
    (refType2: ParentInfo<'T> option -> Asn1Type -> ReferenceType -> 'UserState -> 'a)
    (typeFunc: ParentInfo<'T> option -> Asn1Type -> 'a -> 'b)
    (preSeqOfFunc: ParentInfo<'T> option -> Asn1Type -> SequenceOf -> 'UserState -> 'T * 'UserState)
    (preSeqFunc: ParentInfo<'T> option -> Asn1Type -> Sequence -> 'UserState -> 'T * 'UserState)
    (preChoiceFunc: ParentInfo<'T> option -> Asn1Type -> Choice -> 'UserState -> 'T * 'UserState)
    (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
    (parentInfo : ParentInfo<'T> option)
    (t:Asn1Type)
    (us:'UserState) : 'b
    =
    let rec loopType (pi : ParentInfo<'T> option) (t:Asn1Type) (us:'UserState) =
        let newKind=
            match t.Kind with
            | Integer        ti -> intFunc pi t ti us
            | Real           ti -> realFunc pi t ti us
            | IA5String      ti -> ia5StringFunc pi t ti us
            | NumericString  ti -> numStringFunc pi t ti us
            | OctetString    ti -> octStringFunc pi t ti us
            | TimeType       ti -> timeFunc pi t ti us
            | NullType       ti -> nullTypeFunc pi t ti us
            | BitString      ti -> bitStringFunc pi t ti us
            | Boolean        ti -> boolFunc pi t ti us
            | Enumerated     ti -> enumFunc pi t ti us
            | ObjectIdentifier ti -> objectIdFunc pi t ti us
            | SequenceOf     ti ->
                let (parentData:'T, ns:'UserState) = preSeqOfFunc pi t ti us
                let newChild = loopType (Some {ParentInfo.parent = t ; name=None; parentData=parentData}) ti.child ns
                seqOfFunc pi t ti newChild
            | Sequence       ti ->
                let (parentData:'T, ns:'UserState) = preSeqFunc pi t ti us

                let priorities = getSeqChildrenWithPriority deps t |> Set.ofList

                //first process asn1 children and then asn.1 children.
                let initialOrder = ti.children |> List.mapi(fun i c -> match c with Asn1Child x -> (x.Name.Value, i) | AcnChild x -> (x.Name.Value, i) ) |> Map.ofList
                let newChildren, ns =
                    ti.children |>
                    List.mapi(fun i c -> match priorities.Contains c.Name.Value with true -> ((i+1), c) | false -> ((i+1)*1000000, c)) |>
                    List.sortBy fst |>
                    List.map snd |>
                    foldMap (fun curState ch ->
                        match ch with
                        | Asn1Child asn1Child   ->
                            let newChild, ns = seqAsn1ChildFunc asn1Child (loopType (Some {ParentInfo.parent = t ; name=Some asn1Child.Name.Value; parentData=parentData}) asn1Child.Type curState)
                            (asn1Child.Name.Value, newChild), ns
                        | AcnChild  acnChild    ->
                            let newChild, ns = seqAcnChildFunc  acnChild curState
                            (acnChild.Name.Value, newChild), ns) ns
                let newChildren =
                    newChildren |> List.sortBy(fun (nm, _)  -> initialOrder[nm]) |> List.map snd
                seqFunc pi t ti (newChildren, ns)
            | Choice         ti ->
                let (parentData:'T, ns:'UserState) = preChoiceFunc pi t ti us
                let newChildren = ti.children |> foldMap (fun curState ch -> chChildFunc ch (loopType (Some {ParentInfo.parent = t ; name=Some ch.Name.Value; parentData=parentData}) ch.Type curState)) ns
                choiceFunc pi t ti newChildren
            | ReferenceType  ti ->
                match ti.hasExtraConstrainsOrChildrenOrAcnArgs with
                | true ->   
                    refType pi t ti (loopType pi ti.resolvedType us)
                | false ->  
                    match ti.resolvedType.isComplexType with
                    | true->    
                        match ti.encodingOptions with
                        | None -> refType2 pi t ti us
                        | Some _ -> refType pi t ti (loopType pi ti.resolvedType us)
                    | false ->  refType pi t ti (loopType pi ti.resolvedType us)
        typeFunc pi t newKind
    loopType parentInfo t us

// EVALUATE CONSTRAINTS
let evalGenericCon (c:GenericConstraint<'v>)  eqFunc value =
    foldGenericConstraint
        (fun _ e1 e2 b s      -> e1 || e2, s)
        (fun _ e1 e2 s        -> e1 && e2, s)
        (fun _ e s            -> not e, s)
        (fun _ e1 e2 s        -> e1 && (not e2), s)
        (fun _ e s            -> e, s)
        (fun _ e1 e2 s        -> e1 || e2, s)
        (fun _ v  s           -> eqFunc v value ,s)
        c
        0 |> fst


let isValidValueGeneric allCons eqFunc value =
    allCons |> List.fold(fun cs c -> cs && (evalGenericCon c eqFunc value) ) true


let evalRangeCon  (c:RangeTypeConstraint<'v1,'v1>)  value =
    let check_v1 v1 minIsIn =
        match minIsIn with
        | true  -> v1 <= value
        | false -> v1 < value
    let check_v2 v2 maxIsIn =
        match maxIsIn with
        | true  -> value <= v2
        | false -> value < v2
    foldRangeTypeConstraint
        (fun _ e1 e2 b s      -> e1 || e2, s)    //union
        (fun _ e1 e2 s        -> e1 && e2, s)    //Intersection
        (fun _ e s            -> not e, s)       //AllExcept
        (fun _ e1 e2 s        -> e1 && (not e2), s)       //ExceptConstraint
        (fun _ e s            -> e, s)        //RootConstraint
        (fun _ e1 e2 s        -> e1 || e2, s)    //RootConstraint2
        (fun _ v  s         -> v = value ,s)        // SingleValueConstraint
        (fun _ v1 v2  minIsIn maxIsIn (s:int)   ->  //RangeConstraint
            (check_v1 v1 minIsIn) && (check_v2 v2 maxIsIn), s)
        (fun _ v1 minIsIn s   -> (check_v1 v1 minIsIn), s) //Constraint_val_MAX
        (fun _ v2 maxIsIn s   -> (check_v2 v2 maxIsIn), s) //Constraint_MIN_val
        c
        0 |> fst

let isValidValueRanged allCons value =
    allCons |> List.fold(fun cs c -> cs && (evalRangeCon c value) ) true
