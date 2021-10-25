module TypesEquivalence
open Asn1AcnAst
open Asn1AcnAstUtilFunctions

let rec uperEquivalence (t1:Asn1Type) (t2:Asn1Type) =
    match t1.Kind, t2.Kind with
    | Integer a1, Integer a2                -> a1.cons = a2.cons
    | Real _, Real _                        -> true
    | IA5String s1, IA5String s2            -> s1.cons = s2.cons
    | NumericString s1, NumericString s2    -> s1.cons = s2.cons
    | OctetString o1, OctetString o2        -> o1.cons = o2.cons
    | NullType _, NullType _                -> true
    | BitString bs1, BitString bs2          -> bs1.cons = bs2.cons
    | Boolean _, Boolean _                  -> true
    | Enumerated e1, Enumerated e2          -> e1.items = e2.items &&  e1.cons = e2.cons
    | SequenceOf sqof1, SequenceOf sqof2    -> 
        sqof1.cons = sqof2.cons && (uperEquivalence sqof1.child sqof2.child)
    | Sequence sq1, Sequence sq2    ->
        let children1 = sq1.children |> List.choose (fun c -> match c with Asn1Child a -> Some a | AcnChild _ -> None)
        let children2 = sq2.children |> List.choose (fun c -> match c with Asn1Child a -> Some a | AcnChild _ -> None)
        match children1.Length = children2.Length with
        | false -> false
        | true  ->
            let r1 = true //sq1.cons = sq2.cons 
            let zipedChildren = List.zip children1 children2
            let r2 = zipedChildren |> Seq.forall (fun (c1,c2) -> c1.Optionality = c2.Optionality &&   uperEquivalence c1.Type c2.Type)
            r1 && r2
    | Choice ch1, Choice ch2       ->
        match ch1.children.Length = ch2.children.Length with
        | false -> false
        | true  ->
            let zipedChildren = List.zip ch1.children ch2.children
            let r1 = zipedChildren |> Seq.forall (fun (c1,c2) -> uperEquivalence c1.Type c2.Type)
            let r2 = true //ch1.cons = ch2.cons
            r1 && r2
    | ReferenceType r1, _       -> uperEquivalence r1.resolvedType t2
    | _ , ReferenceType r2       -> uperEquivalence t1 r2.resolvedType 
    | _                         -> false
            
        
