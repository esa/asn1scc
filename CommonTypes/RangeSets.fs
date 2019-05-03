module RangeSets

open FsUtils
open System.Numerics


//type ISet =
//    abstract member Intersect : ISet -> ISet
//    abstract member Union : ISet -> ISet
//    abstract member Difference : ISet -> ISet
//    abstract member Complement : unit -> ISet


type V_CMP =
    | V_LT
    | V_EQ
    | V_GT




type LPoint<'v when 'v : comparison> =
    | LP of ('v*bool)
with 
    member this.not =
        match this with
        | LP(vl, b) -> LP(vl, not b)

type RPoint<'v when 'v : comparison> =
    | RP of ('v*bool)
with 
    member this.not =
        match this with
        | RP(vl, b) -> RP(vl, not b)
    member this.toLP =
        match this with
        | RP(vl, b) -> LP(vl, b)

type LPoint<'v when 'v : comparison> 
with 
    member this.toRP =
        match this with
        | LP(vl, b) -> RP(vl, b)


[<CustomEquality; CustomComparison>]
type Point<'v when 'v : comparison>  = 
    | LPoint of LPoint<'v>
    | RPoint of RPoint<'v>
with
    static member v_comp (p1: Point<'v>) (p2 :Point<'v>) =
        match p1, p2 with
        | LPoint(LP(a, a_inc)), LPoint(LP(b, b_inc)) when a = b && a_inc = b_inc -> V_EQ
        | LPoint(LP(a, a_inc)), LPoint(LP(b, b_inc)) when a = b && not a_inc && b_inc -> V_LT
        | LPoint(LP(a, a_inc)), LPoint(LP(b, b_inc)) when a = b && a_inc && not b_inc -> V_GT
        | LPoint(LP(a, a_inc)), LPoint(LP(b, b_inc))    -> if a<b then V_LT else V_GT
        | LPoint(LP(a,_)), RPoint(RP(b,_))  when a=b    -> V_EQ
        | LPoint(LP(a,_)), RPoint(RP(b,_))              -> if a<b then V_LT else V_GT
        | RPoint(RP(a,_)), RPoint(RP(b,_))  when a=b    -> V_EQ
        | RPoint(RP(a,_)), LPoint(LP(b,_))              -> if a<b then V_LT else V_GT
        | RPoint(RP(a, a_inc)), RPoint(RP(b, b_inc)) when a = b && a_inc = b_inc -> V_EQ
        | RPoint(RP(a, a_inc)), RPoint(RP(b, b_inc)) when a = b && not a_inc && b_inc -> V_LT
        | RPoint(RP(a, a_inc)), RPoint(RP(b, b_inc)) when a = b && a_inc && not b_inc -> V_GT
        | RPoint(RP(a, a_inc)), RPoint(RP(b, b_inc))    -> if a<b then V_LT else V_GT
    
    static member comp p1  p2  =
        match Point<'v>.v_comp p1 p2 with
        | V_LT  -> -1
        | V_EQ  -> 0
        | V_GT  -> 1

    override this.Equals(yobj) =
        match yobj with
        | :? Point<'v> as other -> Point<'v>.comp this other = 0
        | _ -> false
    override this.GetHashCode() = 
        match this with
        | LPoint(LP(a, _))   -> a.GetHashCode()
        | RPoint(RP(a, _))   -> a.GetHashCode()

    interface System.IComparable with 
        member this.CompareTo oth = 
            match oth with
            | :? Point<'v> as other -> Point<'v>.comp this other
            | _     -> invalidArg "oth" "cannot compare values of different types"


type OneOrTwo<'T> =
    | One of 'T
    | Two of 'T*'T
with 
    member this.toList =
        match this with
        | One a     -> [a]
        | Two (a,b) -> [a;b]


type Range<'v when 'v : comparison> =
    | Range_Empty
    | Range_Universe
    | Range_NI_A of RPoint<'v>
    | Range_B_PI of LPoint<'v>
    | Range_AB of LPoint<'v>*RPoint<'v>
with
    override this.ToString() = 
        match this with
        |Range_Empty                          -> System.Char.ConvertFromUtf32(0x2205)
        |Range_Universe                       -> "(-" + System.Char.ConvertFromUtf32(0x221E) + "..+" + System.Char.ConvertFromUtf32(0x221E) + ")"
        |Range_NI_A (RP (b, inc_b))           -> "(-" + System.Char.ConvertFromUtf32(0x221E) + ".." + (b.ToString()) + (if (inc_b) then "]" else ")")
        |Range_B_PI (LP (a, inc_a))           -> (if (inc_a) then "[" else "(") + (a.ToString()) +  "..+" + System.Char.ConvertFromUtf32(0x221E) + ")"
        |Range_AB  ((LP (a, inc_a)),(RP (b, inc_b)))   -> 
            (if (inc_a) then "[" else "(") + (a.ToString()) +  ".." + (b.ToString()) + (if (inc_b) then "]" else ")")




type RangeCompareResult<'v when 'v : comparison> =
    | NonIntersectedRanges of (Range<'v>*Range<'v>)             // low, higher
    | IntersectedRanges    of (Range<'v>*Range<'v>*Range<'v>)   // lower uncommonr part, common part,  higher part


let rec compareRanges (r1:Range<'v>) (r2:Range<'v>) =   
        match r1, r2 with
        | Range_Empty, _                                            -> NonIntersectedRanges (r1, r2)
        | _, Range_Empty                                            -> compareRanges r2 r1
        | Range_Universe, Range_Universe                            -> IntersectedRanges (Range_Empty, Range_Universe, Range_Empty)
        | Range_Universe, Range_NI_A (RP (a,inc))                   -> IntersectedRanges (Range_Empty, Range_B_PI (LP (a, not inc)), Range_Empty)
        | Range_Universe, Range_B_PI (LP (b,inc))                   -> IntersectedRanges (Range_Empty, Range_NI_A(RP (b, not inc)), Range_Empty)
        | Range_Universe, Range_AB (a,b)                            -> IntersectedRanges ((Range_NI_A a.not.toRP), r2, (Range_B_PI b.not.toLP))
        | _, Range_Universe                                         -> compareRanges r2 r1
        | Range_NI_A (RP(a1, inc1)), Range_NI_A (RP(a2, inc2))      -> 
            let mn = min a1 a2
            let mx = max a1 a2
            let mninc = if a1 < a2 then inc1 else inc2
            let mxinc = if a1 > a2 then inc1 else inc2
            let rightRange =
                match mn<mx with
                | true  -> Range_AB((LP(mn, not mninc)),(RP(mx, mxinc)))
                | false (*mn = mx*) ->
                    if not mninc && mxinc then Range_AB((LP(mn, true)),(RP(mx, true))) else Range_Empty
            IntersectedRanges (Range_Empty, Range_NI_A(RP (mn, mninc)), rightRange)
        | Range_NI_A a, (Range_B_PI b )  -> 
            // case 1 : OO ---- a   b --------- OO
            // case 2 : OO ---- a   
            //                  b --------- OO
            // case 3 : OO ---- a   
            //               b --------- OO
             match Point<'v>.v_comp (RPoint a) (LPoint b) with
             | V_LT      -> NonIntersectedRanges (r1, r2)             
             | V_EQ      -> IntersectedRanges((Range_NI_A a.not), Range_AB(b, a), Range_B_PI b.not )
             | V_GT      -> IntersectedRanges((Range_NI_A a.not), Range_AB(b, a), Range_B_PI b.not )
        | Range_NI_A x0, Range_AB (a, b)   ->
            // -oo --------x0
            //         a--------b
            match Point<'v>.v_comp (RPoint x0) (LPoint a) with
            | V_LT      -> NonIntersectedRanges (r1, r2)             
            | V_EQ      -> IntersectedRanges(Range_NI_A x0.not, Range_AB (a, x0), Range_AB (a.not, b))
            | V_GT      -> 
                match Point<'v>.v_comp (RPoint x0) (RPoint b) with
                | V_LT  -> IntersectedRanges(Range_NI_A x0.not, Range_AB (a, x0), Range_AB (a.not, b))
                | V_EQ  -> IntersectedRanges(Range_NI_A x0.not, Range_AB (a, x0), Range_AB (a.not, b))
                | V_GT  -> 
                    // -oo -----------------x0
                    //         a--------b
                    IntersectedRanges(Range_NI_A x0.not, r2, Range_AB (b.not.toLP, x0))
        | Range_B_PI _, Range_NI_A _      -> compareRanges r2 r1
        | Range_B_PI (LP (b1, inc1)), Range_B_PI(LP (b2, inc2))       -> 
            let mx = max b1 b2
            let mxi = if b1 > b2 then inc1 else inc2
            let mn = min b1 b2
            let mni = if b1 < b2 then inc1 else inc2
            let rcommon = Range_B_PI(LP (mx, mxi) )
            let leftRange =
                match mn<mx with
                | true  -> Range_AB((LP(mn, mni)),(RP(mx, not mxi)))
                | false (*mn = mx*) ->
                    if not mxi && mni then Range_AB((LP(mn, true)),(RP(mx, true))) else Range_Empty
            IntersectedRanges(leftRange, rcommon, Range_Empty)
        | Range_B_PI x0,  Range_AB (a, b)   ->
            //      x0 -------------- +oo
            //   a-------b         
            match Point<'v>.v_comp (RPoint b) (LPoint x0) with
            | V_LT      -> NonIntersectedRanges (r1, r2)
            | V_EQ      -> IntersectedRanges(Range_AB(a, b.not), Range_AB( x0, b), Range_B_PI x0.not)
            | V_GT      ->
                match Point<'v>.v_comp (LPoint a) (LPoint x0) with
                | V_GT -> 
                    //      x0 -------------- +oo
                    //          a-------b         
                    IntersectedRanges(Range_AB(x0, a.not.toRP), r2, Range_B_PI b.not.toLP)
                | V_EQ 
                | V_LT -> 
                    //               x0 -------------- +oo
                    //          a---------b         
                    IntersectedRanges(Range_AB (a, x0.not.toRP), Range_AB (x0, b), Range_B_PI b.not.toLP)
        | Range_AB _, Range_NI_A _      -> compareRanges r2 r1
        | Range_AB _, Range_B_PI _      -> compareRanges r2 r1
        | Range_AB (A1,A2), Range_AB (B1,B2) -> 
            let a1 = LPoint A1
            let b1 = LPoint B1
            let a2 = RPoint A2
            let b2 = RPoint B2
              // case 1 :    a1------a2             , condition a2 < b1,         --> empty set
              //                         b1-----b2 
              // case 2 :    a1------a2
              //                     b1------b2     , condition a2 = b1,         --> [a2,a2]
              // case 3 :    a1------a2
              //                  b1------b2        , condition a1 <= b1 && a2 <= b2,          --> [b1,a2]
              // case 4 :    a1-------------a2
              //                  b1------b2        , condition a1 <= b1 && b2 <= a2,          --> other
              // case 5 :       a1-----a2
              //              b1----------b2        , condition b1 <= a1 && a2 <= b2,          --> this
              
              // case 6 :       a1--------a2
              //              b1--------b2          , condition b1 <= a1 && b2 <= a2,          --> [a1,v2]
              
              // case 7 :               a1--------a2
              //              b1--------b2          , condition a1 = b2 ,                      --> [a1,a1]
              
              // case 8 :               a1--------a2
              //              b1-----b2             , condition b2 < a1,                       --> empty set

            if   a2 < b1 then NonIntersectedRanges (r1, r2)
            elif a2 = b1 then IntersectedRanges(Range_AB (A1,A2.not), Range_AB (B1,A2), Range_AB (B1.not,B2))
            elif a1 <= b1 && a2 <= b2 then IntersectedRanges(Range_AB (A1,B1.not.toRP), Range_AB (B1,A2), Range_AB (A2.not.toLP,B2))
            elif a1 <= b1 && b2 <= a2 then IntersectedRanges(Range_AB (A1,B1.not.toRP), r2, Range_AB (B2.not.toLP,A2))
            elif b1 <= a1 && a2 <= b2 then IntersectedRanges(Range_AB (B1,A1.not.toRP), r1, Range_AB (A2.not.toLP,B2))
            elif b1 <= a1 && b2 <= a2 then IntersectedRanges(Range_AB (B1,A1.not.toRP), Range_AB(A1,B2), Range_AB (B2.not.toLP,A2))
            elif a1 = b2 then IntersectedRanges(Range_AB (B1,A1.not.toRP), Range_AB(A1,B2), Range_AB (B2.not.toLP,A2))
            else NonIntersectedRanges(r2,r1)



type Range<'v when 'v : comparison> with
    member this.isWithin (p:Point<'v>) =
        match this with
        |Range_Empty                          -> false
        |Range_Universe                       -> true
        |Range_NI_A a                         -> p <= RPoint a 
        |Range_B_PI b                         -> LPoint b <= p
        |Range_AB  (a,b)                      -> LPoint a <= p && p <= RPoint b

    member this.complement = 
        match this with
        | Range_Empty                          -> One(Range_Universe)
        | Range_Universe                       -> One(Range_Empty)
        | Range_NI_A (RP (a,inc))              -> One(Range_B_PI(LP (a, not inc)))
        | Range_B_PI (LP (b,inc))              -> One(Range_NI_A(RP (b, not inc)))
        | Range_AB  (LP(a,inc1), RP(b,inc2))   -> Two ( (Range_NI_A (RP(a, not inc1))), (Range_B_PI(LP(b,not inc2))))

    member this.intersect (other:Range<'v>) =
        match compareRanges this other with
        | NonIntersectedRanges _                -> Range_Empty
        | IntersectedRanges    (_,ret,_)        -> ret




    #if false
        match this, other with
        | Range_Empty, _                                -> Range_Empty
        | _, Range_Empty                                -> Range_Empty
        | Range_Universe, _                             -> other
        | _, Range_Universe                             -> this
        | Range_NI_A (RP(a1, inc1)), Range_NI_A (RP(a2, inc2))  -> Range_NI_A(RP (min a1 a2, (if a1 < a2 then inc1 else inc2)))
        | (Range_NI_A a), (Range_B_PI b )  -> 
            // case 1 : OO ---- a   b --------- OO
            // case 2 : OO ---- a   
            //                  b --------- OO
            // case 3 : OO ---- a   
            //               b --------- OO
             match Point<'v>.v_comp (RPoint a) (LPoint b) with
             | V_LT      -> Range_Empty             
             | V_EQ      -> Range_AB(b, a)
             | V_GT      -> Range_AB(b, a)
        | Range_NI_A x0, Range_AB (a, b)   ->
            // -oo --------x0
            //         a--------b
            match Point<'v>.v_comp (RPoint x0) (LPoint a) with
            | V_LT      -> Range_Empty
            | V_EQ      -> Range_AB (a, x0)
            | V_GT      -> 
                match Point<'v>.v_comp (RPoint x0) (RPoint b) with
                | V_LT  -> Range_AB(a, x0)
                | V_EQ  -> Range_AB(a, x0)
                | V_GT  -> other
        | Range_B_PI _, Range_NI_A _      -> other.intersect this
        | Range_B_PI (LP (b1, inc1)), Range_B_PI(LP (b2, inc2))       -> Range_B_PI(LP (max b1 b2, (if b1 > b2 then inc1 else inc2)) )
        | Range_B_PI x0,  Range_AB (a, b)   ->
            //      x0 -------------- +oo
            //   a-------b         
            match Point<'v>.v_comp (RPoint b) (LPoint x0) with
            | V_LT      -> Range_Empty
            | V_EQ      -> Range_AB( x0, b)
            | V_GT      ->
                match Point<'v>.v_comp (LPoint a) (LPoint x0) with
                | V_GT -> other
                | V_EQ 
                | V_LT -> Range_AB (x0, b)
        | Range_AB _, Range_NI_A _      -> other.intersect this
        | Range_AB _, Range_B_PI _      -> other.intersect this
        | Range_AB (A1,A2), Range_AB (B1,B2) -> 
            let a1 = LPoint A1
            let b1 = LPoint B1
            let a2 = RPoint A2
            let b2 = RPoint B2
              // case 1 :    a1------a2             , condition a2 < b1,         --> empty set
              //                         b1-----b2 
              // case 2 :    a1------a2
              //                     b1------b2     , condition a2 = b1,         --> [a2,a2]
              // case 3 :    a1------a2
              //                  b1------b2        , condition a1 <= b1 && a2 <= b2,          --> [b1,a2]
              // case 4 :    a1-------------a2
              //                  b1------b2        , condition a1 <= b1 && b2 <= a2,          --> other
              // case 5 :       a1-----a2
              //              b1----------b2        , condition b1 <= a1 && a2 <= b2,          --> this
              
              // case 6 :       a1--------a2
              //              b1--------b2          , condition b1 <= a1 && b2 <= a2,          --> [a1,v2]
              
              // case 7 :               a1--------a2
              //              b1--------b2          , condition a1 = b2 ,                      --> [a1,a1]
              
              // case 8 :               a1--------a2
              //              b1-----b2             , condition b2 < a1,                       --> empty set

            if   a2 < b1 then Range_Empty
            elif a2 = b1 then Range_AB (B1,A2)
            elif a1 <= b1 && a2 <= b2 then Range_AB (B1,A2)
            elif a1 <= b1 && b2 <= a2 then other
            elif b1 <= a1 && a2 <= b2 then this
            elif b1 <= a1 && b2 <= a2 then Range_AB(A1,B2)
            elif a1 = b2 then Range_AB(A1,B2)
            else Range_Empty
    #endif
    member this.union (other:Range<'v>) =
        match this, other with
        | Range_Empty, _                                -> One other
        | _, Range_Empty                                -> One this
        | Range_Universe, _                             -> One Range_Universe
        | _, Range_Universe                             -> One Range_Universe
        | Range_NI_A (RP(a1, inc1)), Range_NI_A (RP(a2, inc2))  -> One (Range_NI_A(RP (max a1 a2, (if a1 > a2 then inc1 else inc2))))
        | (Range_NI_A a), (Range_B_PI b )  -> 
            // case 1 : OO ---- a   b --------- OO
            // case 2 : OO ---- a   
            //                  b --------- OO
            // case 3 : OO ---- a   
            //               b --------- OO
             match Point<'v>.v_comp (RPoint a) (LPoint b) with
             | V_LT      -> Two (this, other)
             | V_EQ      -> One Range_Universe
             | V_GT      -> One Range_Universe
        | Range_NI_A x0, Range_AB (a, b)   ->
            // -oo --------x0
            //         a--------b
            match Point<'v>.v_comp (RPoint x0) (LPoint a) with
            | V_LT      -> Two (this, other)
            | V_EQ      -> One (Range_NI_A b)
            | V_GT      -> 
                match Point<'v>.v_comp (RPoint x0) (RPoint b) with
                | V_LT  -> One (Range_NI_A b)
                | V_EQ  -> One (Range_NI_A b)
                | V_GT  -> One (this)
        | Range_B_PI _, Range_NI_A _      -> other.union this
        | Range_B_PI (LP (b1, inc1)), Range_B_PI(LP (b2, inc2))       -> One (Range_B_PI(LP (min b1 b2, (if b1 < b2 then inc1 else inc2)) ))
        | Range_B_PI x0,  Range_AB (a, b)   ->
            //      x0 -------------- +oo
            //   a-------b         
            match Point<'v>.v_comp (RPoint b) (LPoint x0) with
            | V_LT      -> Two(this, other)
            | V_EQ      -> One (Range_B_PI  a)
            | V_GT      ->
                match Point<'v>.v_comp (LPoint a) (LPoint x0) with
                | V_GT -> One (Range_B_PI  a)
                | V_EQ 
                | V_LT -> One (this)
        | Range_AB _, Range_NI_A _      -> other.union this
        | Range_AB _, Range_B_PI _      -> other.union this
        | Range_AB (A1,A2), Range_AB (B1,B2) -> 
            let a1 = LPoint A1
            let b1 = LPoint B1
            let a2 = RPoint A2
            let b2 = RPoint B2
              // case 1 :    a1------a2             , condition a2 < b1,         --> empty set
              //                         b1-----b2 
              // case 2 :    a1------a2
              //                     b1------b2     , condition a2 = b1,         --> [a2,a2]
              // case 3 :    a1------a2
              //                  b1------b2        , condition a1 <= b1 && a2 <= b2,          --> [b1,a2]
              // case 4 :    a1-------------a2
              //                  b1------b2        , condition a1 <= b1 && b2 <= a2,          --> other
              // case 5 :       a1-----a2
              //              b1----------b2        , condition b1 <= a1 && a2 <= b2,          --> this
              
              // case 6 :       a1--------a2
              //              b1--------b2          , condition b1 <= a1 && b2 <= a2,          --> [a1,v2]
              
              // case 7 :               a1--------a2
              //              b1--------b2          , condition a1 = b2 ,                      --> [a1,a1]
              
              // case 8 :               a1--------a2
              //              b1-----b2             , condition b2 < a1,                       --> empty set

            if   a2 < b1 then Two(this, other)                             // case 1
            elif a2 = b1 then One (Range_AB (A1,B2))                        // case 2
            elif a1 <= b1 && a2 <= b2 then One (Range_AB (A1,B2))           // case 3
            elif a1 <= b1 && b2 <= a2 then One (this)                       // case 4
            elif b1 <= a1 && a2 <= b2 then One (other)                      // case 5
            elif b1 <= a1 && b2 <= a2 then One (Range_AB(B1,A2))            // case 6
            elif a1 = b2 then One (Range_AB(B1,A2))                         // case 7
            else Two(other, this)                                          // case 8

        member this.isBefore other =
            match this.union other with
            | One _            -> false
            | Two (first, sec) -> first = this && sec = other

        member this.isAfter other =
            match this.union other with
            | One _            -> false
            | Two (first, sec) -> first = other && sec = this


(*

    member this.complement = 
        match this with
        | Range_Empty                          -> One(Range_Universe)
        | Range_Universe                       -> One(Range_Empty)
        | Range_NI_A (RP (a,inc))              -> One(Range_B_PI(LP (a, not inc)))
        | Range_B_PI (LP (b,inc))              -> One(Range_NI_A(RP (b, not inc)))
        | Range_AB  (LP(a,inc1), RP(b,inc2))   -> Two ( (Range_NI_A (RP(a, not inc1))), (Range_B_PI(LP(b,not inc2))))

*)



let create_range v1  v2  =
    match v1 , v2 with
    | Some v1, Some v2  when v1 <= v2 -> Range_AB ( LP (v1, true), RP(v2, true))
    | Some v1, Some v2                -> Range_Empty
    | None, Some v2    -> Range_NI_A (RP (v2, true))
    | Some v1, None    -> Range_B_PI (LP (v1, true))
    | None, None       -> Range_Universe

                                                       //  
type RangeSet<'v when 'v : comparison> =
    | Range of Range<'v>
    | RangeCollection of (Range<'v>*Range<'v>*Range<'v> list)
with
    override this.ToString() = 
        match this with
        | Range r       -> r.ToString()
        | RangeCollection (r1,r2, rRest) -> r1::r2::rRest |> List.map(fun r -> r.ToString()) |> Seq.StrJoin " U "
    static member createFromRangeList ranges =
        match ranges |> List.exists((=) Range_Universe) with
        | true  -> Range Range_Universe
        | false ->
            match ranges |> List.filter ((<>) Range_Empty) with
            | []     -> Range Range_Empty
            | r1::[] -> Range r1
            | r1::r2::rest -> RangeCollection (r1,r2,rest)
    static member createFromSingleValue v =
        Range(Range_AB (LP(v,true), RP(v,true)))

    static member createFromMultipleValues vList =
        match vList with
        | []        -> Range (Range_Empty)
        | v1::[]    -> Range(Range_AB (LP(v1,true), RP(v1,true)))
        | v1::v2::vrest ->
            let r1 = Range_AB (LP(v1,true), RP(v1,true))
            let r2 = Range_AB (LP(v2,true), RP(v2,true))
            let rest = vrest |> List.map(fun v -> Range_AB (LP(v,true), RP(v,true)) )
            RangeCollection(r1, r2,rest) 
    static member createFromValuePair v1 v2  minIsIn maxIsIn =
        Range(Range_AB (LP(v1,minIsIn), RP(v2,maxIsIn)))

    static member createPosInfinite v1 minIsIn =
        Range(Range_B_PI (LP(v1,minIsIn)))

    static member createNegInfinite v2 maxIsIn =
        Range(Range_NI_A (RP(v2,maxIsIn)))

    member this.isEmpty = 
        match this with
        | Range r                           -> r = Range_Empty
        | RangeCollection (r1,r2,rest)      -> r1 = Range_Empty && r2 = Range_Empty && rest.Length = 0

    member this.isUniverse = 
        match this with
        | Range r                           -> r = Range_Universe
        | RangeCollection (r1,r2,rest)      -> 
            let a1 = r1 = Range_Universe || r2 = Range_Universe || (rest |> List.exists(fun r -> r = Range_Universe))
            let b = 
                r1::r2::rest |> 
                List.fold(fun (ns) r -> 
                    match ns with
                    | One (cr:Range<'v>)        -> cr.union r
                    | Two _         -> ns) (One Range_Empty)
            let b2 = 
                match b with
                | One r -> r = Range_Universe
                | _     -> false
            a1 || b2


    member this.isInSet (vl:Point<'v>) : bool =
        match this with
        | Range(r )                         -> r.isWithin vl 
        | RangeCollection (r1,r2,rest)      -> r1::r2::rest |> List.exists(fun r -> r.isWithin vl )
    member this.intersect (other:RangeSet<'v>) =
        match this, other with
        | Range r1, Range r2                                -> Range (r1.intersect r2)
        | Range r0, RangeCollection (r1,r2,rest)            ->
            let ranges = r1::r2::rest |> List.map(fun r -> r.intersect r0) |> List.filter(fun r -> r <> Range_Empty)
            RangeSet<'v>.createFromRangeList ranges
        | RangeCollection _, Range _                     -> other.intersect this
        | RangeCollection (a1,a2,aRest), RangeCollection _  ->
            let ranges = a1::a2::aRest |> List.map(fun r -> (Range r).intersect other) |> List.collect(fun rs -> match rs with Range r -> [r] | RangeCollection (r1,r2,rRest) -> r1::r2::rRest )
            RangeSet<'v>.createFromRangeList ranges
    member this.complement = 
        match this with
        | Range(r )                         -> 
            match r.complement with
            | One rc -> Range rc
            | Two (r1,r2)   -> RangeCollection(r1,r2,[])
        | RangeCollection (r1,r2,rest)                            -> 
            r1::r2::rest |> 
            List.map(fun r -> (Range r).complement) |> 
            List.fold(fun newRange curR -> 
                newRange.intersect curR ) (Range Range_Universe)
    member this.difference (other:RangeSet<'v>) =
        other.complement.intersect this
    
    member this.union (other:RangeSet<'v>) =
        let addRangeToRangeCollection (r1:Range<'v>,r2,rest) r0 =
            let before = r1::r2::rest |> List.filter(fun r -> r.isBefore r0)
            let after = r1::r2::rest |> List.filter(fun r -> r.isAfter r0)
            let middleRange = 
                r1::r2::rest |> 
                List.fold(fun (nr:Range<'v>) r -> 
                    match r.isBefore r0 with
                    | true     -> nr   // ingore ranges which are before r0
                    | false    ->
                        match r.isAfter r0 with
                        | true     -> nr  // ingore ranges which are after r0
                        | false    -> 
                            match nr.union r with
                            | Two _     -> raise (BugErrorException "")
                            | One ur    -> ur ) r0

            RangeSet<'v>.createFromRangeList (before@[middleRange]@after)
        match this, other with
        | Range r1, Range r2                                -> 
            match r1.union r2 with
            | One  r -> Range r
            | Two (r1,r2) -> RangeCollection(r1, r2, [])
        | Range r0, RangeCollection (r1,r2,rest)            -> addRangeToRangeCollection (r1,r2,rest) r0
        | RangeCollection (r1,r2,rest), Range r0            -> addRangeToRangeCollection (r1,r2,rest) r0
        | RangeCollection (a1,a2,aRest), RangeCollection _  ->
            a1::a2::aRest |> 
            List.fold(fun (ret:RangeSet<'v>) r -> ret.union (Range r) ) other



//    abstract member Intersect : ISet -> ISet
//    abstract member Union : ISet -> ISet
//    abstract member Difference : ISet -> ISet
//    abstract member Complement : unit -> ISet

let create_set v1  v2  =
    Range (create_range v1 v2)


type BigIntegerSet = RangeSet<BigInteger>
type DoubleSet = RangeSet<double>


let db r =
    printfn "%s" (r.ToString())

let debug () =
    let a = 3
    let R0_100 = create_set (Some 0) (Some 100)
    let R10_200 = create_set (Some 10) (Some 200)
    let R0_200  = R0_100.union R10_200
    assert (R0_200 = create_set (Some 0) (Some 200))
    let dummy1 = R0_200.ToString()
    let C_R0_200 = R0_200.complement
    let dummy2 = C_R0_200.ToString()
    let R0_200_U_300_400 = R0_200.union(create_set (Some 300) (Some 400))
    let R0_200_U_300_400_U_500_600 = R0_200_U_300_400.union(create_set (Some 500) (Some 600))

    let C_R0_200_U_300_400_U_500_600 = R0_200_U_300_400_U_500_600.complement
    
    let R0_200_U_300_350 = R0_200_U_300_400_U_500_600.difference(create_set (Some 350) None)
    let dd = R0_200_U_300_350.ToString()

    let zzz = R0_200_U_300_400_U_500_600.difference(create_set (Some 100) (Some 100))
    let dd = zzz.ToString()

    let ddc = zzz.complement.ToString()

    let aaa = (R0_200.union(create_set (Some 300) (Some 400))).union(create_set (Some 500) (Some 600))
    let dummy3 = aaa.ToString()

    let dummy4 = aaa.complement

    System.Environment.Exit 0
    