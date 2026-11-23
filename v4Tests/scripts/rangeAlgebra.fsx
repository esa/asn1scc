// Property test of the range algebra of the PER-visible constraints (CommonTypes.uperIntersection / uperUnion), which
// every INTEGER / REAL range and every SIZE of the front end goes through (Issue 173: two arms of uperIntersection
// were wrong: n..MAX on lo..hi with n <= lo gave n..MAX, MIN..m on lo..hi gave m..hi).
// Oracle: bound arithmetic (a bound is an option, None = infinite): the intersection takes the larger lower and the
// smaller upper bound and is empty when they cross; the union is the convex hull (X.691 B.2.3.6). Checked over
// BigInteger extremes (+-2^63, +-2^64, +-10^40), a small domain with every single-value range, uint32 (sizes) and
// double (REAL, +-infinity, +-0.0, epsilon), both argument orders, and the fold order of random triples.
// Run by v4Tests/scripts/checkRangeAlgebra.sh <bin dir>, which replaces @BIN@; exit code 0 = no failure.
#r "@BIN@/Antlr3.Runtime.dll"
#r "@BIN@/CommonTypes.dll"
open System
open System.Numerics
open CommonTypes
open FsUtils

let loc = emptyLocation

let bounds (r: uperRange<'a>) : 'a option * 'a option =
    match r with
    | Full -> None, None
    | PosInf a -> Some a, None
    | NegInf b -> None, Some b
    | Concrete (a, b) -> Some a, Some b

let ofBounds (lo: 'a option, hi: 'a option) : uperRange<'a> option =
    match lo, hi with
    | None, None -> Some Full
    | Some a, None -> Some (PosInf a)
    | None, Some b -> Some (NegInf b)
    | Some a, Some b -> if a <= b then Some (Concrete (a, b)) else None

let refInter (r1: uperRange<'a>) (r2: uperRange<'a>) =
    let (l1, h1), (l2, h2) = bounds r1, bounds r2
    let lo = match l1, l2 with | None, x | x, None -> x | Some a, Some b -> Some (max a b)
    let hi = match h1, h2 with | None, x | x, None -> x | Some a, Some b -> Some (min a b)
    ofBounds (lo, hi)

let refUnion (r1: uperRange<'a>) (r2: uperRange<'a>) =
    let (l1, h1), (l2, h2) = bounds r1, bounds r2
    let lo = match l1, l2 with | None, _ | _, None -> None | Some a, Some b -> Some (min a b)
    let hi = match h1, h2 with | None, _ | _, None -> None | Some a, Some b -> Some (max a b)
    ofBounds (lo, hi)

// None = the compiler rejects the intersection as empty
let inter r1 r2 = try Some (uperIntersection r1 r2 loc) with SemanticError _ -> None

let run (name: string) (pts: 'a list) =
    let ranges : uperRange<'a> list =
        [ yield Full
          for a in pts do
              yield PosInf a
              yield NegInf a
              for b in pts do
                  if a <= b then yield Concrete (a, b) ]
    let mutable failures = 0
    let mutable shown = 0
    let fail (msg: string) =
        failures <- failures + 1
        if shown < 10 then
            shown <- shown + 1
            printfn "  %s: %s" name msg
    for r1 in ranges do
        for r2 in ranges do
            let g = inter r1 r2
            let e = refInter r1 r2
            if g <> e then fail (sprintf "%A ^ %A = %A, expected %A" r1 r2 g e)
            let u = uperUnion r1 r2
            if Some u <> refUnion r1 r2 then fail (sprintf "%A | %A = %A, expected %A" r1 r2 u (refUnion r1 r2))
    // the serial constraints of a type are folded from the left: every order of three gives the same result
    let rs = List.toArray ranges
    let rnd = Random 173
    let triples = 100000
    for _ in 1 .. triples do
        let a, b, c = rs.[rnd.Next rs.Length], rs.[rnd.Next rs.Length], rs.[rnd.Next rs.Length]
        let exp = refInter a b |> Option.bind (fun ab -> refInter ab c)
        for (x, y, z) in [ a, b, c; a, c, b; b, c, a; c, b, a; c, a, b; b, a, c ] do
            let got = inter x y |> Option.bind (fun xy -> inter xy z)
            if got <> exp then fail (sprintf "(%A ^ %A) ^ %A = %A, expected %A" x y z got exp)
    printfn "%s: %d ranges, %d pairs, %d triples x 6 orders: %d failures" name ranges.Length
        (ranges.Length * ranges.Length) triples failures
    failures

let p2 (n: int) = BigInteger.Pow (2I, n)
let big =
    [ -(BigInteger.Pow (10I, 40)); -(p2 64); -(p2 63) - 1I; -(p2 63); -(p2 31); -2I; -1I; 0I; 1I; 2I; 255I; 256I
      65535I; 65536I; p2 31 - 1I; p2 32 - 1I; p2 32; p2 63 - 1I; p2 63; p2 64 - 1I; p2 64; BigInteger.Pow (10I, 40) ]
let small = [ for i in -5 .. 5 -> BigInteger i ]
let u32 = [ 0u; 1u; 2u; 3u; 6u; 7u; 127u; 128u; 65535u; 65536u; UInt32.MaxValue - 1u; UInt32.MaxValue ]
let dbl =
    [ Double.NegativeInfinity; -Double.MaxValue; -1e300; -5.0; -Double.Epsilon; -0.0; 0.0; Double.Epsilon; 1.0; 5.0
      10.0; 1e300; Double.MaxValue; Double.PositiveInfinity ]

let total =
    run "BigInteger extremes" big + run "BigInteger -5..5 (every single value)" small + run "uint32 (sizes)" u32
    + run "double (REAL)" dbl

// the reported shapes: (0..10)(MIN..5) = 0..5, (3..10)(1..MAX) = 3..10, (0..10)(MIN..0) = 0..0, (5)(0..MAX) = 5..5
let shapes =
    [ Concrete (0I, 10I), NegInf 5I, Some (Concrete (0I, 5I))
      Concrete (3I, 10I), PosInf 1I, Some (Concrete (3I, 10I))
      Concrete (3I, 10I), PosInf 3I, Some (Concrete (3I, 10I))
      Concrete (0I, 10I), NegInf 0I, Some (Concrete (0I, 0I))
      Concrete (5I, 5I), PosInf 0I, Some (Concrete (5I, 5I))
      Concrete (0I, 10I), NegInf -1I, None ]
let shapeFailures =
    shapes |> List.sumBy (fun (r1, r2, e) ->
        if inter r1 r2 <> e || inter r2 r1 <> e then
            printfn "  shape %A ^ %A = %A / %A, expected %A" r1 r2 (inter r1 r2) (inter r2 r1) e
            1
        else 0)
printfn "reported shapes: %d failures" shapeFailures

exit (if total + shapeFailures = 0 then 0 else 1)
