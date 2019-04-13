module SimpleSets
open System
open System.Numerics

type SiRange<'v> = {
    a : 'v
    b : 'v
}

type SiSet<'v> = 
    | SiSet of SiRange<'v> list

type SetDefinition<'v> = {
    universe : SiRange<'v>
    prefFunc : 'v->'v
    nextFunc : 'v->'v
}
//let fullIntegerSet = {Range<'v>.a = bigint Int64.MinValue; b = bigint Int64.MaxValue}

let range_within (r:SiRange<'v>) v = 
    r.a <= v && v <= r.b


    

let range_intersect (r1:SiRange<'v>) (r2:SiRange<'v>) =
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
        | true  -> Some({SiRange.a = r1.a; b = r2.b})       
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
                    | true  -> Some {SiRange.a = r2.a; b = r1.b}
                    | false -> None

let range_union (r1:SiRange<'v>) (r2:SiRange<'v>) =
    match r2.b < r1.a with
    | true      -> Some r2, Some r1
    | false     -> 
        match r2.a <= r1.a && r1.a <= r2.b  && r2.b <= r1.b with
        | true  -> Some {SiRange.a = r2.a; b = r1.b}, None
        | false -> 
            match r2.a <= r1.a && r2.b > r1.b with
            | true  -> Some r2, None
            | false -> 
                match r1.a <= r2.a && r2.b <= r1.b with
                | true  -> Some r1, None
                | false ->
                    match r1.a <= r2.a &&  r2.a <= r1.b && r1.b <= r2.b with
                    | true  -> Some {SiRange.a = r1.a; b = r2.b}, None
                    | false -> Some r1, Some r2

let range_union2 (r1:SiRange<'v>) (r2:SiRange<'v>) =
    match range_union r1 r2 with
    | None, None    -> []
    | Some x, None  -> [x]
    | None, Some x  -> [x]
    | Some x, Some y -> [x;y]    

let range_complement (d:SetDefinition<'v>) (r2:SiRange<'v>) =
    let u = d.universe
    match u = r2 with
    | true  -> None, None
    | false ->
        match u.a = r2.a && r2.b < u.b with
        | true  -> (Some {SiRange.a = d.nextFunc r2.b; b = u.b}), None
        | false ->
            match u.a < r2.a && r2.b = u.b with
            | true  -> (Some {SiRange.a = u.a; b = d.prefFunc r2.a}), None
            | false -> (Some {SiRange.a = u.a; b = d.prefFunc r2.a}), Some {SiRange.a = d.nextFunc r2.b; b = u.b}

let range_complement2 (d:SetDefinition<'v>) (r2:SiRange<'v>) =
    match range_complement d r2 with
    | None, None    -> []
    | Some x, None  -> [x]
    | None, Some x  -> [x]
    | Some x, Some y -> [x;y]    

let range_difference (d:SetDefinition<'v>) (r1:SiRange<'v>) (r2:SiRange<'v>) =
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
    
let set_add_range (d:SetDefinition<'v>) (SiSet s1) (r2: SiRange<'v>) =
    let before = s1 |> List.filter(fun r1 -> r1.b < r2.a)
    let after = s1 |> List.filter(fun r1 -> r2.b < r1.a)
    let middleRanges = s1 |> List.filter(fun r1 -> not (r1.b < r2.a)) |> List.filter(fun r1 -> not (r2.b < r1.a))
    let midleRange =
        middleRanges |> 
        List.fold(fun st r -> (range_union2 st r).Head ) r2
    SiSet(before@[midleRange]@after)

let set_intersect (d:SetDefinition<'v>) (SiSet s1) (SiSet s2) =
    let ranges =
        seq {
            for r1 in s1 do
                for r2 in s2 do
                    yield range_intersect r1 r2
        } |> Seq.toList
    ranges |> List.choose id |> List.fold(fun set r -> set_add_range d set r ) (SiSet [])
    
let set_union (d:SetDefinition<'v>) (SiSet s1) (SiSet s2) =
    s1 |> List.fold(fun set r -> set_add_range d set r) (SiSet s2)

let set_complement (d:SetDefinition<'v>) (SiSet ranges)  =
    ranges |> 
    List.fold(fun set r -> 
        let comp = range_complement2 d r
        set_intersect d set (SiSet comp)) (SiSet [d.universe])

let set_difference (d:SetDefinition<'v>) s1 s2= 
    // A - B = A intersection (Complement B)
    set_intersect d s1 (set_complement d s2)    

let Int64Def = { universe = {SiRange.a= bigint Int64.MinValue ;b= bigint Int64.MaxValue}; prefFunc = (fun x -> x-1I);    nextFunc = (fun x -> x+1I)}
let UInt64Def = { universe = {SiRange.a= 0I ;b= bigint UInt64.MaxValue}; prefFunc = (fun x -> x-1I);    nextFunc = (fun x -> x+1I)}

let Int32Def = { universe = {SiRange.a= bigint Int32.MinValue ;b= bigint Int32.MaxValue}; prefFunc = (fun x -> x-1I);    nextFunc = (fun x -> x+1I)}
let UInt32Def = { universe = {SiRange.a= 0I ;b= bigint UInt32.MaxValue}; prefFunc = (fun x -> x-1I);    nextFunc = (fun x -> x+1I)}

let PosInt32Def = { universe = {SiRange.a= 0u ;b= UInt32.MaxValue}; prefFunc = (fun x -> x-1u);    nextFunc = (fun x -> x+1u)}


let nextDoubleValue (d:double) =
    let rec nextDoubleValue (d:double) (e:double) =
        let ret = d+e
        if ret > d then ret else (nextDoubleValue d (e+e))
    nextDoubleValue d Double.Epsilon

let prevDoubleValue (d:double) =
    let rec prevDoubleValue (d:double) (e:double) =
        let ret = d+e
        if ret < d then ret else (prevDoubleValue d (e+e))
    prevDoubleValue d Double.Epsilon

let RealDef = { universe = {SiRange.a= Double.MinValue ;b= Double.MinValue}; prefFunc = prevDoubleValue;    nextFunc = nextDoubleValue}

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