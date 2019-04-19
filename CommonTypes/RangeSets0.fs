module RangeSets0


type V_CMP =
    | V_LT
    | V_EQ
    | V_GT


let v_comp a a_inc b b_inc =
    match a < b with
    | true  -> V_LT
    | false when a=b && a_inc && b_inc -> V_EQ
    | false when a=b                   -> V_LT
    | false -> V_GT




type Range<'v when 'v : comparison> =
    | Range_Empty
    | Range_Universe
    | Range_NI_A of 'v*bool
    | Range_B_PI of 'v*bool
    | Range_AB of ('v*bool)*('v*bool)
    | Range_NI_A_B_PI of ('v*bool)*('v*bool)
with
    member this.complement = 
        match this with
        | Range_Empty                          -> Range_Universe
        | Range_Universe                       -> Range_Empty
        | Range_NI_A (a,inc)                   -> Range_B_PI(a, not inc)
        | Range_B_PI (b,inc)                   -> Range_NI_A(b, not inc)
        | Range_AB  ((a,inc1), (b,inc2))       -> Range_NI_A_B_PI ((a, not inc1), (b,not inc2))
        | Range_NI_A_B_PI ((a,inc1), (b,inc2)) -> Range_AB ((a, not inc1), (b,not inc2))
    member this.intersect other =
        match this, other with
        | Range_Empty, _                                -> Range_Empty
        | _, Range_Empty                                -> Range_Empty
        | Range_Universe, _                             -> other
        | _, Range_Universe                             -> this
        | Range_NI_A (a1, inc1), Range_NI_A (a2, inc2)  -> Range_NI_A(min a1 a2, (if a1 < a2 then inc1 else inc2))
        | Range_NI_A (a1, inc1), Range_B_PI(b2, inc2)   -> 
             match v_comp a1 inc1 b2 inc2 with
             | V_LT      -> Range_Empty
             | V_EQ      -> Range_AB( (a1,true), (a1,true))
             | V_GT      -> Range_AB( (a1,inc1), (b2,inc2))
        | Range_NI_A (a1, inc1), Range_AB((a,inc21), (b,inc22))   ->
            match v_comp a1 inc1 a inc21 with
            | V_LT      -> Range_Empty
            | V_EQ      -> Range_AB( (a1,true), (a1,true))
            | V_GT      -> 
                match v_comp a1 inc1 b inc22 with
                | V_LT  -> Range_AB((a,inc21), (a1,inc1))
                | V_EQ  -> Range_AB((a,inc21), (a1,inc1))
                | V_GT  -> other
                
        | Range_NI_A (a1, inc1), Range_NI_A_B_PI ((a,inc21), (b,inc22)) ->
            match v_comp a1 inc1 a inc21 with
            | V_LT      -> this
            | V_EQ      -> this
            | V_GT      -> Range_NI_A(a,inc21)
        | _                                                     -> Range_Empty

