module ValueSets

//SIMPLE VALUE SETs
type ValueSet<'v when 'v : equality> =
    | SsUniverse
    | SsEmpty
    | SsValues of 'v list
    | SsExceptValues of 'v list
with 
    member this.isEmpty = this = SsEmpty
    member this.isUniverse = this = SsUniverse


let fixSet (s:ValueSet<'v>)  =
    match s with
    | SsUniverse            -> SsUniverse
    | SsEmpty               -> SsEmpty
    | SsValues          s1  when s1.IsEmpty -> SsEmpty
    | SsValues          _  -> s
    | SsExceptValues    s1  when s1.IsEmpty -> SsUniverse
    | SsExceptValues    _   -> s


let value_set_complement (s:ValueSet<'v>)  =
    match (fixSet s) with
    | SsUniverse            -> SsEmpty
    | SsEmpty               -> SsUniverse
    | SsValues          s1  -> SsExceptValues s1
    | SsExceptValues    s1  -> SsValues s1



let list_contains vl list =
    list |> List.exists( (=) vl)

let list_add vl list =
    match list_contains vl list with
    | true -> list
    | false -> vl::list

let keepCommonElemnts s1 (s2:List<'v>) =
    s1 |> List.filter(fun v -> s2 |> list_contains v)

let unionSet s1 (s2:List<'v>) =
    s1 |> List.fold(fun (ns) v -> list_add v ns) s2


let value_set_difference s1 s2= 
    match (fixSet s1), (fixSet s2) with
    | SsEmpty            , _                     ->   SsEmpty
    | _                  , SsEmpty               ->   s1
    | _                  , SsUniverse            ->   SsEmpty
    | SsUniverse         , SsValues          s2  ->   SsExceptValues s2
    | SsUniverse         , SsExceptValues    s2  ->   SsValues s2
    
    | SsValues       a1  , SsValues          a2  ->   fixSet (SsValues (a1 |> List.filter(fun v -> not (a2 |> list_contains v))))
    | SsValues       b  , SsExceptValues    a  ->   
        //B-A = Intesect(Complement(A),  B)
        fixSet (SsValues (a |> List.filter(fun v -> b |> list_contains v)))
    | SsExceptValues a1  , SsValues          a2  ->   fixSet (SsExceptValues (a1 |> List.fold(fun ns v -> ns |> list_add v) a2))
    | SsExceptValues a1  , SsExceptValues    a2  ->   
        //B-A = Intesect(Complement(A),  B)
        fixSet(SsValues (a2 |> List.filter(fun v -> not (a1 |> list_contains v))))


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

type ValueSet<'v when 'v : comparison>  with 
    member this.complement = value_set_complement this
    member this.union  other      = value_set_union this other
    member this.intersect other   = value_set_intersection this other
    member this.difference other  = value_set_difference this other
    



