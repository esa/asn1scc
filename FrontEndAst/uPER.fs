module uPER

open System
open System.Numerics
open FsUtils
open Asn1AcnAst
open Asn1Fold
open CommonTypes

let min a b = if a<b then a else b
let max a b = if a>b then a else b



let getRangeTypeConstraintUperRange (c:RangeTypeConstraint<'v1,'v1>) funcNext funcPrev (l:SrcLoc) =
    foldRangeTypeConstraint
        (fun _ r1 r2 b s      -> uperUnion r1 r2, s)
        (fun _ r1 r2 s        -> uperIntersection r1 r2 l, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> r1, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> Full, s)
        (fun _ v  s           -> Concrete (v,v),s)
        (fun _ v1 v2  minIsIn maxIsIn s  ->
            let val1 = if minIsIn then v1 else (funcNext v1)
            let val2 = if maxIsIn then v2 else (funcPrev v2)
            Concrete(val1 , val2), s)
        (fun _ v1 minIsIn  s      ->
            let val1 = if minIsIn then v1 else (funcNext v1)
            PosInf(val1) ,s )
        (fun _ v2 maxIsIn s      ->
            let val2 = if maxIsIn then v2 else (funcPrev v2)
            NegInf(val2), s)
        c
        0


let getIntTypeConstraintUperRange (cons:IntegerTypeConstraint list) (l:SrcLoc) =
    let getIntTypeConstraintUperRange (c:IntegerTypeConstraint) (l:SrcLoc) =
        getRangeTypeConstraintUperRange c (fun x -> x + 1I) (fun x -> x - 1I) l |> fst
    cons |> List.fold(fun s c -> uperIntersection s (getIntTypeConstraintUperRange c l) l) Full

let getRealTypeConstraintUperRange (cons:RealTypeConstraint list) (l:SrcLoc) =
    let getRealTypeConstraintUperRange (c:RealTypeConstraint) (l:SrcLoc) =
        getRangeTypeConstraintUperRange c id id  l |> fst
    cons |> List.fold(fun s c -> uperIntersection s (getRealTypeConstraintUperRange c l) l) Full



let getSizeableTypeConstraintUperRange (c:SizableTypeConstraint<'v>) funcGetLength (l:SrcLoc) =
    foldSizableTypeConstraint
        (fun _ r1 r2 b s      -> uperUnion r1 r2, s)
        (fun _ r1 r2 s        -> uperIntersection r1 r2 l, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> r1, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> Full, s)
        (fun _ v  s           -> Concrete (funcGetLength v,funcGetLength v),s)

        (fun _ r1 r2 b s      -> uperUnion r1 r2, s)
        (fun _ r1 r2 s        -> uperIntersection r1 r2 l, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> r1, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> Full, s)
        (fun _ v  s           -> Concrete (v,v),s)
        (fun _ v1 v2  minIsIn maxIsIn s  ->
            let val1 = if minIsIn then v1 else (v1+1u)
            let val2 = if maxIsIn then v2 else (v2-1u)
            Concrete(val1 , val2), s)
        (fun _ v1 minIsIn  s      ->
            let val1 = if minIsIn then v1 else (v1+1u)
            PosInf(val1) ,s )
        (fun _ v2 maxIsIn s      ->
            let val2 = if maxIsIn then v2 else (v2-1u)
            NegInf(val2), s)
        c
        0 |> fst

let getSizeableUperRange (cons:SizableTypeConstraint<'v> list) funcGetLength (l:SrcLoc) =
    let getConUperRange (c:SizableTypeConstraint<'v>) (l:SrcLoc) =
        getSizeableTypeConstraintUperRange c  funcGetLength l
    cons |> List.fold(fun s c -> uperIntersection s (getConUperRange c l) l) Full

let getOctetStringUperRange (cons:OctetStringConstraint list) (l:SrcLoc) =
    getSizeableUperRange cons (fun (x,_) -> uint32 x.Length) l

let getBitStringUperRange (cons:BitStringConstraint list) (l:SrcLoc) =
    getSizeableUperRange cons (fun (x,_) -> uint32 x.Value.Length) l



let getSequenceOfUperRange (cons:SequenceOfConstraint list) (l:SrcLoc) =
    let getConUperRange (c:SequenceOfConstraint)  (l:SrcLoc) =
        foldSequenceOfTypeConstraint
            (fun _ r1 r2 b s      -> uperUnion r1 r2, s)
            (fun _ r1 r2 s        -> uperIntersection r1 r2 l, s)
            (fun _ r s            -> Full, s)
            (fun _ r1 r2 s        -> r1, s)
            (fun _ r s            -> Full, s)
            (fun _ r1 r2 s        -> Full, s)
            (fun _ v  s           -> Concrete (uint32 v.Length,uint32 v.Length ),s)

            (fun _ r1 r2 b s      -> uperUnion r1 r2, s)
            (fun _ r1 r2 s        -> uperIntersection r1 r2 l, s)
            (fun _ r s            -> Full, s)
            (fun _ r1 r2 s        -> r1, s)
            (fun _ r s            -> Full, s)
            (fun _ r1 r2 s        -> Full, s)
            (fun _ v  s           -> Concrete (v,v),s)
            (fun _ v1 v2  minIsIn maxIsIn s  ->
                let val1 = if minIsIn then v1 else (v1+1u)
                let val2 = if maxIsIn then v2 else (v2-1u)
                Concrete(val1 , val2), s)
            (fun _ v1 minIsIn  s      ->
                let val1 = if minIsIn then v1 else (v1+1u)
                PosInf(val1) ,s )
            (fun _ v2 maxIsIn s      ->
                let val2 = if maxIsIn then v2 else (v2-1u)
                NegInf(val2), s)
            (fun _ c l s           -> Full, s)
            c
            0 |> fst

    cons |> List.fold(fun s c -> uperIntersection s (getConUperRange c l) l) Full


let getStringConstraintSizeUperRange (c:IA5StringConstraint) (l:SrcLoc) =
    foldStringTypeConstraint
        (fun _ r1 r2 b s      -> uperUnion r1 r2, s)
        (fun _ r1 r2 s        -> uperIntersection r1 r2 l, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> r1, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> Full, s)
        (fun _ v  s           -> Concrete (uint32 v.Length, uint32 v.Length),s)

        (fun _ r1 r2 b s      -> uperUnion r1 r2, s)
        (fun _ r1 r2 s        -> uperIntersection r1 r2 l, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> r1, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> Full, s)
        (fun _ v  s           -> Concrete (v,v),s)
        (fun _ v1 v2  minIsIn maxIsIn s  ->
            let val1 = if minIsIn then v1 else (v1+1u)
            let val2 = if maxIsIn then v2 else (v2-1u)
            Concrete(val1 , val2), s)
        (fun _ v1 minIsIn  s      ->
            let val1 = if minIsIn then v1 else (v1+1u)
            PosInf(val1) ,s )
        (fun _ v2 maxIsIn s      ->
            let val2 = if maxIsIn then v2 else (v2-1u)
            NegInf(val2), s)

        (fun _ r1 r2 b s      -> Full, s)
        (fun _ r1 r2 s        -> Full, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> Full, s)
        (fun _ r s            -> Full, s)
        (fun _ r1 r2 s        -> Full, s)
        (fun _ v    s         -> Full,s)
        (fun _ v1 v2  minIsIn maxIsIn s  ->Full, s)
        (fun _ v1 minIsIn  s  -> Full ,s )
        (fun _ v2 maxIsIn s   -> Full, s)
        c
        0 |> fst


let getStringSizeUperRange (cons:IA5StringConstraint list) (l:SrcLoc) =
    let getConUperRange (c:IA5StringConstraint) (l:SrcLoc) =
        getStringConstraintSizeUperRange c  l
    cons |> List.fold(fun s c -> uperIntersection s (getConUperRange c l) l) Full

let IntersectArrays (s1:char array) (s2:char array) (l:SrcLoc) =
    let cache = s2 |> Set.ofSeq
    let ret = s1 |> Array.filter(fun ch -> cache.Contains(ch))
    match ret.Length with
    | 0  -> raise(SemanticError(l, "The alphabet constraint defined for this type do not allow any character"))
    | _  ->ret

let getStringConstraintAlphabetUperRange (c:IA5StringConstraint) (defaultCharSet: char array) (l:SrcLoc) =


    let GetCharSetFromString (str:string) = str.ToCharArray() |> Seq.distinct |> Seq.toArray
    let CharSetUnion(s1: char array) (s2:char array) = [s1;s2] |>Seq.concat |> Seq.distinct |> Seq.toArray
    let GetCharSetFromMinMax a b minIsIn maxIsIn =

        match defaultCharSet |> Array.tryFindIndex(fun ch -> ch = a) with
        | Some a1 ->
            match defaultCharSet |> Array.tryFindIndex(fun ch -> ch = b) with
            | Some a2 ->
                let a1 = if minIsIn then a1 else a1+1
                let a2 = if maxIsIn then a2 else a2-1
                defaultCharSet.[a1..a2]
            | None  ->
                let errMsg = sprintf "Character '%c' does not belong to the base type characters set" b
                raise(SemanticError(l, errMsg))
        | None  ->
            let errMsg = sprintf "Character '%c' does not belong to the base type characters set" a
            raise(SemanticError(l, errMsg))


    let nextChar (c:System.Char) =
        System.Convert.ToChar(System.Convert.ToInt32(c)+1)
    let prevChar (c:System.Char) =
        System.Convert.ToChar(System.Convert.ToInt32(c)-1)

    foldStringTypeConstraint
        (fun _ r1 r2 b s      -> CharSetUnion r1 r2, s)
        (fun _ r1 r2 s        -> IntersectArrays r1 r2 l, s)
        (fun _ r s            -> defaultCharSet, s)
        (fun _ r1 r2 s        -> r1, s)
        (fun _ r s            -> defaultCharSet, s)
        (fun _ r1 r2 s        -> defaultCharSet, s)
        (fun _ v  s           -> defaultCharSet, s)

        (fun _ r1 r2 b s      -> defaultCharSet, s)
        (fun _ r1 r2 s        -> defaultCharSet, s)
        (fun _ r s            -> defaultCharSet, s)
        (fun _ r1 r2 s        -> defaultCharSet, s)
        (fun _ r s            -> defaultCharSet, s)
        (fun _ r1 r2 s        -> defaultCharSet, s)
        (fun _ v  s           -> defaultCharSet,s)
        (fun _ v1 v2  minIsIn maxIsIn s  ->defaultCharSet, s)
        (fun _ v1 minIsIn  s  -> defaultCharSet ,s )
        (fun _ v2 maxIsIn s   -> defaultCharSet, s)

        (fun _ r1 r2 b s      -> CharSetUnion r1 r2, s)
        (fun _ r1 r2 s        -> IntersectArrays r1 r2 l, s)
        (fun _ r s            -> defaultCharSet, s)
        (fun _ r1 r2 s        -> r1, s)
        (fun _ r s            -> defaultCharSet, s)
        (fun _ r1 r2 s        -> defaultCharSet, s)
        (fun _ v  s         -> GetCharSetFromString v, s)
        (fun _ v1 v2  minIsIn maxIsIn s  -> GetCharSetFromMinMax v1 v2 minIsIn maxIsIn, s)
        (fun _ v1 minIsIn  s      ->
            let v2 = defaultCharSet.[defaultCharSet.Length-1]
            let val1 = if minIsIn then v1 else (nextChar v1)
            GetCharSetFromMinMax v1 v2 minIsIn true ,s )
        (fun _ v2 maxIsIn s      ->
            let v1 = defaultCharSet.[0]
            GetCharSetFromMinMax v1 v2 true maxIsIn, s)

        c
        0 |> fst

let getSrtingAlphaUperRange (cons:IA5StringConstraint list) (defaultCharSet: char array) (l:SrcLoc) =
    let getConUperRange (c:IA5StringConstraint) (l:SrcLoc) =
        getStringConstraintAlphabetUperRange c defaultCharSet l
    cons |> List.fold(fun s c -> IntersectArrays s (getConUperRange c l) l) defaultCharSet



let isUnsigned uperRange =
        match uperRange with
        | Concrete (a,b) when a >= 0I   -> true
        | Concrete (a,b)                -> false
        | NegInf   _                    -> false
        | PosInf (a)     when a >= 0I   -> true
        | PosInf (a)                    -> false
        | Full                          -> false

let getSizeMinAndMaxValue loc (sizeUperRange:UInt32UperRange) =
    match sizeUperRange with
    | Concrete(a,b) -> BigInteger a, BigInteger b
    | _             -> raise(SemanticError(loc,"Declared type may have infinite size. Use size constraints to limit the upper bound"))


let getRequiredBitsForIntUperEncoding  (integerSizeInBytes:BigInteger) uperRange =
    match uperRange with
    | Concrete(a,b)                   -> (GetNumberOfBitsForNonNegativeInteger(b-a)), (GetNumberOfBitsForNonNegativeInteger(b-a))
    | Full | PosInf(_) |  NegInf(_)   -> 8I, (integerSizeInBytes+1I)*8I

let getSizeableTypeSize (a:BigInteger) (b:BigInteger) (internalSize:BigInteger) =
    let lenSize (a:BigInteger) (b:BigInteger) = GetNumberOfBitsForNonNegativeInteger(b-a)
    match a with
    | _ when a=b  && b<65536I -> a*internalSize                , b*internalSize
    | _ when a<>b && b<65536I -> a*internalSize + (lenSize a b), b*internalSize + (lenSize a b)
    | _                       -> a*internalSize + (lenSize a b), b*internalSize + (b / 65536I + 3I) * 8I
