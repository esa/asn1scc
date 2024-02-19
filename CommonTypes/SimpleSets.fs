module SimpleSets
open System
open System.Numerics


//SIMPLE RANGE SET
type SsRange<'v> = {
    a : 'v
    b : 'v
}

type SsRangeSetDefinition<'v> = {
    universe : SsRange<'v>
    prefFunc : 'v->'v
    nextFunc : 'v->'v
}

type SsRangeSet<'v when 'v : comparison> = {
    ranges : SsRange<'v> list
    setDef : SsRangeSetDefinition<'v>
} 
with
  member this.isEmpty = this.ranges.IsEmpty

//let fullIntegerSet = {Range<'v>.a = bigint Int64.MinValue; b = bigint Int64.MaxValue}

let range_within (r:SsRange<'v>) v = 
    r.a <= v && v <= r.b


    

let range_intersect (r1:SsRange<'v>) (r2:SsRange<'v>) =
(*                      
   --- r2 ----
                   --- r1 ---
*)    
    match r2.b < r1.a with                                      
    | true      -> None
    | false     -> 
(*                      
   --- r2 ----
            --- r1 ---
*)    
        match r2.a <= r1.a && r1.a <= r2.b  && r2.b <= r1.b with
        | true  -> Some({SsRange.a = r1.a; b = r2.b})       
        | false -> 
(*                      
   ---------- r2 --------
            --- r1 ---
*)    
            match r2.a <= r1.a && r2.b > r1.b with
            | true  -> Some r1
            | false -> 
(*                      
     --- r2 ----
   ----- r1 ------
*)    
                match r1.a <= r2.a && r2.b <= r1.b with
                | true  -> Some r2
                | false ->
(*                      
         --- r2 ----
    --- r1 ---
*)    
                    match r1.a <= r2.a &&  r2.a <= r1.b && r1.b <= r2.b with
                    | true  -> Some {SsRange.a = r2.a; b = r1.b}
                    | false -> None

let range_union (r1:SsRange<'v>) (r2:SsRange<'v>) =
    match r2.b < r1.a with
    | true      -> Some r2, Some r1
    | false     -> 
        match r2.a <= r1.a && r1.a <= r2.b  && r2.b <= r1.b with
        | true  -> Some {SsRange.a = r2.a; b = r1.b}, None
        | false -> 
            match r2.a <= r1.a && r2.b > r1.b with
            | true  -> Some r2, None
            | false -> 
                match r1.a <= r2.a && r2.b <= r1.b with
                | true  -> Some r1, None
                | false ->
                    match r1.a <= r2.a &&  r2.a <= r1.b && r1.b <= r2.b with
                    | true  -> Some {SsRange.a = r1.a; b = r2.b}, None
                    | false -> Some r1, Some r2

let range_union2 (r1:SsRange<'v>) (r2:SsRange<'v>) =
    match range_union r1 r2 with
    | None, None    -> []
    | Some x, None  -> [x]
    | None, Some x  -> [x]
    | Some x, Some y -> [x;y]    

let range_complement (d:SsRangeSetDefinition<'v>) (r2:SsRange<'v>) =
    let u = d.universe
    match u = r2 with
    | true  -> None, None
    | false ->
        match u.a = r2.a && r2.b < u.b with
        | true  -> (Some {SsRange.a = d.nextFunc r2.b; b = u.b}), None
        | false ->
            match u.a < r2.a && r2.b = u.b with
            | true  -> (Some {SsRange.a = u.a; b = d.prefFunc r2.a}), None
            | false -> (Some {SsRange.a = u.a; b = d.prefFunc r2.a}), Some {SsRange.a = d.nextFunc r2.b; b = u.b}

let range_complement2 (d:SsRangeSetDefinition<'v>) (r2:SsRange<'v>) =
    match range_complement d r2 with
    | None, None    -> []
    | Some x, None  -> [x]
    | None, Some x  -> [x]
    | Some x, Some y -> [x;y]    

let range_difference (d:SsRangeSetDefinition<'v>) (r1:SsRange<'v>) (r2:SsRange<'v>) =
    // A - B = A intersection (Complement B)
    let r2Complement = range_complement d r2
    match r2Complement with
    | None, None      -> None, None
    | Some r2c, None  
    | None, Some r2c        -> range_intersect r1 r2c, None
    | Some r2c, Some r2d    ->
        let r1a = range_intersect r1 r2c
        let r1b = range_intersect r1 r2d
        match r1a, r1b with
        | None, None    -> None, None
        | Some x, None  -> Some x, None
        | None, Some x  -> Some x, None
        | Some a, Some b ->range_union a b 
    
let set_add_range (s1:SsRangeSet<'v> ) (r2: SsRange<'v>) =
    let before = s1.ranges |> List.filter(fun r1 -> r1.b < r2.a)
    let after = s1.ranges |> List.filter(fun r1 -> r2.b < r1.a)
    let middleRanges = s1.ranges |> List.filter(fun r1 -> not (r1.b < r2.a)) |> List.filter(fun r1 -> not (r2.b < r1.a))
    let midleRange =
        middleRanges |> 
        List.fold(fun st r -> (range_union2 st r).Head ) r2
    {s1 with ranges = (before@[midleRange]@after)}

let range_set_intersect (s1:SsRangeSet<'v>) (s2:SsRangeSet<'v>) =
    let ranges =
        seq {
            for r1 in s1.ranges do
                for r2 in s2.ranges do
                    yield range_intersect r1 r2
        } |> Seq.toList
    ranges |> List.choose id |> List.fold(fun set r -> set_add_range set r ) ({s1 with ranges = []})
    
let range_set_union  (s1:SsRangeSet<'v>) (s2:SsRangeSet<'v>) =
    s1.ranges |> List.fold(fun set r -> set_add_range set r) s2

let range_set_complement (s:SsRangeSet<'v>)  =
    s.ranges |> 
    List.fold(fun set r -> 
        let comp = {s with ranges = range_complement2 set.setDef r}
        range_set_intersect set comp) ({s with ranges = [s.setDef.universe]})


let range_set_difference s1 s2= 
    // A - B = A intersection (Complement B)
    range_set_intersect s1 (range_set_complement s2)    

type SsRangeSet<'v when 'v : equality> with
    member this.Complement = range_set_complement this
    member this.union  other      = range_set_union this other
    member this.intersect other   = range_set_intersect this other
    member this.difference other  = range_set_difference this other
    member this.isUniverse        = this.ranges |> List.exists (fun r -> this.setDef.universe = r)



//SIMPLE VALUE SETs
type SsValueSet<'v when 'v : comparison> =
    | SsUniverse
    | SsEmpty
    | SsValues of Set<'v>
    | SsExceptValues of (Set<'v>)
with 
    member this.isEmpty = this = SsEmpty
    member this.isUniverse = this = SsUniverse


let fixSet (s:SsValueSet<'v>)  =
    match s with
    | SsUniverse            -> SsUniverse
    | SsEmpty               -> SsEmpty
    | SsValues          s1  when s1.IsEmpty -> SsEmpty
    | SsValues          _  -> s
    | SsExceptValues    s1  when s1.IsEmpty -> SsUniverse
    | SsExceptValues    _   -> s


let value_set_complement (s:SsValueSet<'v>)  =
    match (fixSet s) with
    | SsUniverse            -> SsEmpty
    | SsEmpty               -> SsUniverse
    | SsValues          s1  -> SsExceptValues s1
    | SsExceptValues    s1  -> SsValues s1


     
     


let keepCommonElemnts s1 (s2:Set<'v>) =
    s1 |> Set.filter(fun v -> s2.Contains v)

let unionSet s1 (s2:Set<'v>) =
    s1 |> Set.fold(fun (ns:Set<'v>) v -> ns.Add v) s2


let value_set_difference s1 s2= 
    match (fixSet s1), (fixSet s2) with
    | SsEmpty            , _                     ->   SsEmpty
    | _                  , SsEmpty               ->   s1
    | _                  , SsUniverse            ->   SsEmpty
    | SsUniverse         , SsValues          s2  ->   SsExceptValues s2
    | SsUniverse         , SsExceptValues    s2  ->   SsValues s2
    
    | SsValues       a1  , SsValues          a2  ->   fixSet (SsValues (a1 |> Set.filter(fun v -> not (a2.Contains v))))
    | SsValues       b  , SsExceptValues    a  ->   
        //B-A = Intesect(Complement(A),  B)
        fixSet (SsValues (a |> Set.filter(fun v -> b.Contains v)))
    | SsExceptValues a1  , SsValues          a2  ->   fixSet (SsExceptValues (a1 |> Set.fold(fun ns v -> ns.Add v) a2))
    | SsExceptValues a1  , SsExceptValues    a2  ->   
        //B-A = Intesect(Complement(A),  B)
        fixSet(SsValues (a2 |> Set.filter(fun v -> not (a1.Contains v))))


let value_set_intersection s1 s2= 
    match (fixSet s1), (fixSet s2) with
    | SsEmpty           , _                     -> SsEmpty
    | _                 , SsEmpty               -> SsEmpty
    | SsUniverse        , _                     -> s2
    | _                 , SsUniverse            -> s1
    | SsValues       a1 , SsValues          a2  -> fixSet (SsValues (keepCommonElemnts a1 a2))
    | SsExceptValues a1 , SsExceptValues    a2  -> fixSet (SsExceptValues (unionSet a1 a2))  
    | SsValues       b  , SsExceptValues    a   -> 
        //Intesect(B, Complement(A)) = B - A
        value_set_difference s1 (SsValues       a)
    | SsExceptValues a1 , SsValues          a2  -> 
        //Intesect(Complement(A), B) = B - A
        value_set_difference s2 (SsValues       a1)

let value_set_union s1 s2= 
    match (fixSet s1), (fixSet s2) with
    | SsEmpty           , _                     -> s2
    | _                 , SsEmpty               -> s1
    | SsUniverse        , _                     -> SsUniverse
    | _                 , SsUniverse            -> SsUniverse
    | SsValues       a1 , SsValues          a2  -> fixSet (SsValues (unionSet a1 a2))
    | SsExceptValues a1 , SsExceptValues    a2  -> fixSet (SsExceptValues (keepCommonElemnts a1 a2))  
    | SsValues       a  , SsExceptValues    b   -> 
        //Union (A, Complement(B)) = Complement (B -A)
        value_set_complement (value_set_difference (SsValues b) s1)
    | SsExceptValues B , SsValues          A  -> 
        //Union (Complement(B), A ) = Complement (B -A)
        value_set_complement (value_set_difference (SsValues B) s2)

type SsValueSet<'v when 'v : comparison>  with 
    member this.Complement = value_set_complement this
    member this.union  other      = value_set_union this other
    member this.intersect other   = value_set_intersection this other
    member this.difference other  = value_set_difference this other
    

(*
let s1 = SsValues(set [1;2;3;4] )
let s2 = SsExceptValues(set [4;8] )

let s_u = value_set_union s1 s2

let s_i = value_set_intersection s1 s2

*)

(*
type SsInfSet<'v when 'v : comparison> =
    | SsValues of Set<'v>
    | SsAll
    

let SsInfSet_union (s1:SsInfSet<'v>) (s2:SsInfSet<'v>) =
    match s1,s2 with
    | SsAll, SsAll              -> SsAll
    | SsAll, SsValues _         -> SsAll
    | SsValues _, SsAll         -> SsAll
    | SsValues ss1, SsValues ss2  -> SsValues (ss1 |> Seq.fold(fun (ns:Set<'v>) v ->  ns.Add v) ss2)

let SsInfSet_intersect (s1:SsInfSet<'v>) (s2:SsInfSet<'v>) =
    match s1,s2 with
    | SsAll, SsAll              -> SsAll
    | SsAll, SsValues _         -> s2
    | SsValues _, SsAll         -> s1
    | SsValues ss1, SsValues ss2  -> SsValues (ss1 |> Set.filter(fun  v ->  ss2.Contains v) )
    


type SsValueSet<'v when 'v : comparison> = {
    values : SsInfSet<'v>
    ecxeptValues : SsInfSet<'v>
}


let value_set_intersect (s1:SsValueSet<'v>) (s2:SsValueSet<'v>) =
    let newEcxeptValues = SsInfSet_union s1.ecxeptValues s2.ecxeptValues
    let intersectionVals = SsInfSet_intersect s1.values s2.values
    match intersectionVals with
    | SsAll         ->    {SsValueSet.values = SsAll; ecxeptValues = newEcxeptValues}
    | SsValues ss1  ->
        let newSs1 =
            match newEcxeptValues with
            | SsAll         -> Set.empty
            | SsValues exc  -> ss1 |> Set.filter(fun v -> not (exc.Contains v))
        {SsValueSet.values = SsValues newSs1; ecxeptValues = newEcxeptValues}
        

let value_set_union (s1:SsValueSet<'v>) (s2:SsValueSet<'v>) =
    let newEcxeptValues = SsInfSet_union s1.ecxeptValues s2.ecxeptValues
    let unionVals = SsInfSet_union s1.values s2.values
    match unionVals with
    | SsAll         ->    {SsValueSet.values = SsAll; ecxeptValues = newEcxeptValues}
    | SsValues ss1  ->
        let newSs1 =
            match newEcxeptValues with
            | SsAll         -> Set.empty
            | SsValues exc  -> ss1 |> Set.filter(fun v -> not (exc.Contains v))
        {SsValueSet.values = SsValues newSs1; ecxeptValues = newEcxeptValues}
*)

(*

let value_set_complement (s:SsValueSet<'v>)  =
    {SsValueSet.values = s.ecxeptValues; ecxeptValues = s.values}


let value_set_difference s1 s2= 
    // A - B = A intersection (Complement B)
    value_set_intersect s1 (value_set_complement s2)    

*)

let int64RangeSetDef = { universe = {SsRange.a= bigint Int64.MinValue ;b= bigint Int64.MaxValue}; prefFunc = (fun x -> x-1I);    nextFunc = (fun x -> x+1I)}
let uint64RangeSetDef = { universe = {SsRange.a= 0I ;b= bigint UInt64.MaxValue}; prefFunc = (fun x -> x-1I);    nextFunc = (fun x -> x+1I)}

let int32RangeSetDef = { universe = {SsRange.a= bigint Int32.MinValue ;b= bigint Int32.MaxValue}; prefFunc = (fun x -> x-1I);    nextFunc = (fun x -> x+1I)}
let uint32RangeSetDef = { universe = {SsRange.a= 0I ;b= bigint UInt32.MaxValue}; prefFunc = (fun x -> x-1I);    nextFunc = (fun x -> x+1I)}

let posInt32RangeSetDef = { universe = {SsRange.a= 0u ;b= UInt32.MaxValue}; prefFunc = (fun x -> x-1u);    nextFunc = (fun x -> x+1u)}


let nextChar (c:System.Char) =
    System.Convert.ToChar(System.Convert.ToInt32(c)+1)
let prevChar (c:System.Char) =
    System.Convert.ToChar(System.Convert.ToInt32(c)-1)

let charRangeSetDef  = 
    { universe = {SsRange.a= System.Convert.ToChar(0) ;b= System.Convert.ToChar(127)}; prefFunc = prevChar;    nextFunc = nextChar}


let nextDoubleValue (d:double) =
    let rec nextDoubleValue (d:double) (e:double) =
        let ret = d+e
        if ret > d then ret else (nextDoubleValue d (e+e))
    nextDoubleValue d Double.Epsilon

let prevDoubleValue (d:double) =
    let rec prevDoubleValue (d:double) (e:double) =
        let ret = d-e
        if ret < d then ret else (prevDoubleValue d (e+e))
    prevDoubleValue d Double.Epsilon

let realRangeSetDef = { universe = {SsRange.a= Double.MinValue ;b= Double.MinValue}; prefFunc = prevDoubleValue;    nextFunc = nextDoubleValue}


type IntSet = SsRangeSet<BigInteger>
let createDefaultIntSet () = {IntSet.ranges = [int64RangeSetDef.universe]; setDef = int64RangeSetDef}

type RealSet = SsRangeSet<double>
let createDefaultRealSet () = {RealSet.ranges = [realRangeSetDef.universe]; setDef = realRangeSetDef}

type CharSet = SsRangeSet<char>
let createDefaultCharSet () = {CharSet.ranges = [charRangeSetDef.universe]; setDef = charRangeSetDef}

type SizeSet = SsRangeSet<uint32>
let createDefaultSizeSet () = {SizeSet.ranges = [posInt32RangeSetDef.universe]; setDef = posInt32RangeSetDef}
let createSizeDet minSize maxSize = 
    let newUniverse = {SsRange.a=minSize; b=maxSize}
    {SizeSet.ranges = [newUniverse]; setDef = {posInt32RangeSetDef with universe = newUniverse} }

type SingleStringSet = {
    sizeRange : SizeSet option
    charSet   : CharSet option
    values    : SsValueSet<string> option
}
with 
 member this.isEmpty = 
    match this.sizeRange, this.charSet, this.values with
    | Some s, Some c, Some v    -> s.isEmpty && c.isEmpty && v.isEmpty
    | _                         -> false

type MultiStringSet =  SingleStringSet list // union


let SingleStringSet_set_complement (ss:SingleStringSet)  = 
    {
        SingleStringSet.sizeRange = ss.sizeRange |> Option.map (fun sr -> sr.Complement)
        charSet = ss.charSet |> Option.map (fun sr -> sr.Complement)
        values = ss.values |> Option.map (fun sr -> sr.Complement)
    }

let SingleStringSet_set_intersection (s1:SingleStringSet) (s2:SingleStringSet) = 

    let newSizeRange =
        match s1.sizeRange, s2.sizeRange with
        | None, None    -> None
        | Some _, None  -> s1.sizeRange
        | None, Some _  -> s2.sizeRange
        | Some s1, Some s2 -> Some(s1.intersect s2)

    let newCharSet =
        match s1.charSet, s2.charSet with
        | None, None    -> None
        | Some _, None  -> s1.charSet
        | None, Some _  -> s2.charSet
        | Some s1, Some s2 -> Some(s1.intersect s2)

    let newValues =
        match s1.values, s2.values with
        | None, None    -> None
        | Some _, None  -> s1.values
        | None, Some _  -> s2.values
        | Some s1, Some s2 -> Some(s1.intersect s2)

    {SingleStringSet.sizeRange = newSizeRange; charSet = newCharSet; values = newValues}


let SingleStringSet_set_union (s1:SingleStringSet) (s2:SingleStringSet) : MultiStringSet= 
    match s1.sizeRange, s1.charSet, s1.values, s2.sizeRange, s2.charSet, s2.values with
    | None, None, None, _,_,_   -> [s2]
    | _, _,_, None, None, None  -> [s1]
    | Some sa, None, None, Some sb, None, None  -> [{s1 with sizeRange = Some(range_set_intersect sa sb)}]
    | None, Some c1, None, None, Some c2, None  -> [{s1 with charSet = Some(range_set_intersect c1 c2)}]
    | None, None, Some v1, None, None, Some v2  -> [{s1 with values = Some (value_set_intersection v1 v2)} ]
    | _                                         -> [s1;s2]

let MultiStringSet_set_add_SingleStringSet (ms:MultiStringSet) (ss:SingleStringSet) =
    match ms |> List.tryFindIndex(fun s0 -> List.length (SingleStringSet_set_union s0 ss) = 1) with
    | None      -> ss::ms
    | Some idx  -> 
        let arr = ms |> List.toArray
        let p1 = (arr.[0..idx-1]) |> Seq.toList
        let p2 = SingleStringSet_set_union (arr.[idx-1]) ss
        let p3 = 
            match idx+1 <= arr.Length with
            | true  ->  arr.[idx+1 .. arr.Length] |> Seq.toList
            | false ->  []
        p1@p2@p3

let MultiStringSet_set_intersect_SingleStringSet (ms:MultiStringSet) (ss:SingleStringSet) : MultiStringSet=
    ms |> List.map(fun mss0 -> SingleStringSet_set_intersection mss0 ss) |> List.filter (fun z -> not z.isEmpty)

let MultiStringSet_set_union (s1:MultiStringSet) (s2:MultiStringSet) : MultiStringSet= 
    s1 |> List.fold(fun newSet sss -> MultiStringSet_set_add_SingleStringSet newSet sss) s2
    
let MultiStringSet_set_intersect (s1:MultiStringSet) (s2:MultiStringSet) : MultiStringSet= 
    s1 |> List.fold(fun newSet sss -> MultiStringSet_set_intersect_SingleStringSet newSet sss) s2



(*
let def1 = { universe = {SiRange.a=0;b=100}; prefFunc = (fun x -> x-1);    nextFunc = (fun x -> x+1)}

let s1 = SiSet [{a=1;b=20}]
let s2 = SiSet [{a=5;b=10}]
let s3 = SiSet [{a=11;b=25}]

let s4 = set_union def1 s2 s3

let s5 = set_intersect def1 s1 s4

let s10 = set_add_range def1 (SiSet[]) {a=11;b=20}

let s11 = set_add_range def1 s10 {a=21;b=40}

let s12 = set_complement def1 s3

let s13 = set_difference def1 s1 s3

let ss20 = set_intersect def1 s1 s12

let aaa = range_complement def1 {a=11;b=25}


let c1 = range_intersect {a = 20;b = 40} {a = 5;b = 10}
let c2 = range_intersect {a = 20;b = 40} {a = 5;b = 10}

let aaa2 = range_intersect {a = 1;b = 20} {a = 26;b = 100}
*)