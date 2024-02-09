module SizeableSet

open RangeSets
open ValueSets
// a set for bit strings, octet strings and Sequence Ofs

type OneOrTwo<'T> =
    | One of 'T
    | Two of 'T*'T
    | Three of 'T*'T*'T
with
    member this.toList =
        match this with
        | One a     -> [a]
        | Two (a,b) -> [a;b]
        | Three (a,b,c) -> [a;b;c]

type Range2d<'v when 'v : equality> = {
    sizeSet  : Range<uint32>
    valueSet : ValueSet<'v>
}
with
    member this.intersect (other:Range2d<'v>) =
        {sizeSet = this.sizeSet.intersect other.sizeSet; valueSet = this.valueSet.intersect other.valueSet}
    member this.isEmpty =
        this.valueSet.isEmpty || this.sizeSet = Range_Empty
    member this.isUniverse =
        this.valueSet.isUniverse &&  this.sizeSet = Range_Universe
    static member createUniverse () =
        {Range2d.sizeSet = Range_Universe; valueSet = SsUniverse}
    static member createEmptySet () =
        {Range2d.sizeSet = Range_Empty; valueSet = SsEmpty}

    member this.complement =
        match this.sizeSet = Range_Universe, this.valueSet.isUniverse with
        | true, true    -> One (Range2d<'v>.createEmptySet())
        | false, true   ->
            match this.sizeSet.complement with
            | RangeSets.One sizeComplement    -> One ({sizeSet = sizeComplement; valueSet = SsUniverse})
            | RangeSets.Two (s1Comp, s2Comp)   ->
                Two ({sizeSet = s1Comp; valueSet = SsUniverse}, {sizeSet = s2Comp; valueSet = SsUniverse})
        | true, false   -> One ({sizeSet = (Range_Universe); valueSet = this.valueSet.complement})
        | false, false  -> 

            match this.sizeSet.complement with
            | RangeSets.One sizeComplement    -> Two ({sizeSet = sizeComplement; valueSet = SsUniverse}, {sizeSet = (Range_Universe); valueSet = this.valueSet.complement})
            | RangeSets.Two (s1Comp, s2Comp)  ->
                Three (
                        {sizeSet = s1Comp; valueSet = SsUniverse}, 
                        {sizeSet = s2Comp; valueSet = SsUniverse}, 
                        {sizeSet = Range_Universe; valueSet = this.valueSet.complement})

type SizeableSet<'v when 'v : equality> =
    | Range2D of Range2d<'v>
    | Range2DCollection of (Range2d<'v>*Range2d<'v>*Range2d<'v> list)
with
    static member createFromRangeList (ranges: Range2d<'v> list)  =
        match ranges |> List.exists(fun r -> r.isUniverse) with
        | true  -> Range2D (Range2d<'v>.createUniverse ())
        | false ->
            match ranges |> List.filter (fun r -> not r.isEmpty) with
            | []     -> (Range2D (Range2d<'v>.createEmptySet ()))
            | r1::[] -> Range2D r1
            | r1::r2::rest -> Range2DCollection (r1,r2,rest)
    static member createUniverse  =
        Range2D ({sizeSet  = Range_Universe;  valueSet = SsUniverse})
    static member createFromSingleValue v =
        Range2D ({sizeSet  = Range_Universe;  valueSet = ValueSet<'v>.createFromSingleValue v})
    static member createFromSizeRange rangeSet =
        match rangeSet with
        | Range r                   -> Range2D ({sizeSet  = r;  valueSet = SsUniverse})
        | RangeCollection(r1,r2,rs) -> Range2DCollection( ({sizeSet  = r1;  valueSet = SsUniverse}, {sizeSet  = r2;  valueSet = SsUniverse}, rs |> List.map(fun r -> {sizeSet  = r;  valueSet = SsUniverse})))



    member this.intersect (other:SizeableSet<'v>) =
        match this, other with
        | Range2D r1, Range2D r2                                -> Range2D (r1.intersect r2)
        | Range2D r0, Range2DCollection (r1,r2,rest)            ->
            let ranges = r1::r2::rest |> List.map(fun r -> r.intersect r0) |> List.filter(fun r -> not r.isEmpty)
            SizeableSet<'v>.createFromRangeList ranges
        | Range2DCollection _, Range2D _                     -> other.intersect this
        | Range2DCollection (a1,a2,aRest), Range2DCollection _  ->
            let ranges = a1::a2::aRest |> List.map(fun r -> (Range2D r).intersect other) |> List.collect(fun rs -> match rs with Range2D r -> [r] | Range2DCollection (r1,r2,rRest) -> r1::r2::rRest )
            SizeableSet<'v>.createFromRangeList ranges

    member this.complement =
        match this with
        | Range2D r     ->
            match r.complement with
            | One rc        -> Range2D rc
            | Two (rc1,rc2) -> Range2DCollection(rc1,rc2, [])
            | Three (rc1,rc2, rc3) -> Range2DCollection(rc1,rc2, [rc3])
        | Range2DCollection (r1,r2,rest)                            ->
            r1::r2::rest |>
            List.map(fun r -> (Range2D r).complement) |>
            List.fold(fun newRange curR ->
                newRange.intersect curR ) (Range2D (Range2d<'v>.createUniverse ()))

    member this.difference (other:SizeableSet<'v>) =
        other.complement.intersect this

    member this.union (other:SizeableSet<'v>) =
        //not implemented yet
        this
