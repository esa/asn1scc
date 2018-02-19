module XER

open System
open System.Numerics
open FsUtils
open Asn1Ast
open Asn1Fold



let rec XerTag (t:Asn1Type) (r:AstRoot) =
    match (GetActualType t r).Kind with
    | Enumerated(_) | Choice(_) | Boolean _  -> ""
    | _     ->
        match t.Kind with
        | ReferenceType ref    -> ref.tasName.Value
        | Integer              -> "INTEGER"
        | BitString _          -> "BIT-STRING"
        | OctetString _        -> "OCTET-STRING"
        | Boolean      _       -> ""
        | Choice(_)            -> ""
        | Enumerated(_)        -> ""
        | IA5String     _      -> "IA5String"
        | NumericString  _     -> "NumericString"
        | NullType _           -> "NULL"
        | Real      _          -> "REAL"
        | Sequence(_)          -> "SEQUENCE"
        | SequenceOf(_)        -> "SEQUENCE-OF"



let private aux (s:string) = 2 * (s.Length + 1) + 1

let getMaxSizeInBytesForXER_boolean ()  =  aux "False"
let getMaxSizeInBytesForXER_Integer ()  =  System.Int64.MinValue.ToString().Length
let getMaxSizeInBytesForXER_Enumerated (itemNames : string list) =
    let maxName = itemNames |>  Seq.max
    aux maxName
let getMaxSizeInBytesForXER_BitString   maxSize = maxSize * 8
let getMaxSizeInBytesForXER_OctetString maxSize = maxSize * 2
let getMaxSizeInBytesForXER_Real        = 50
let getMaxSizeInBytesForXER_NullType    = 0
let getMaxSizeInBytesForXER_IA5String   maxSize = maxSize
let getMaxSizeInBytesForXER_NumericString   maxSize = maxSize

let getMaxSizeInBytesForXER  (xmlTag:string) contentSize =
    match xmlTag with
    | null  -> contentSize
    | _     -> (2 * (xmlTag.Length + 2)) + 1 + contentSize

(*
let rec GetMaxSizeInBytesForXER (t:Asn1Type) (xmlTag:string) (r:AstRoot) =
    let GetSizeableMaxItems (t:Asn1Type) =
        match (uPER.GetTypeUperRange t.Kind t.Constraints r) with
        | Concrete(a,b)                        -> b
        | NegInf(_)         -> raise (BugErrorException("Negative size"))
        | PosInf(_)         -> raise (BugErrorException("All sizeable types must be constraint, otherwise max size is infinite"))
        | Full              -> raise (BugErrorException("All sizeable types must be constraint, otherwise max size is infinite"))
        | Empty             -> raise (BugErrorException("I do not known how this is handled"))      

    let rec GetMaxSizeInBytesForXER_content (t:Asn1Type) =
        match t.Kind with
        | Boolean       -> aux "False"
        | Enumerated(items) ->
            let maxName = items |> Seq.map(fun x -> x.Name.Value) |> Seq.max
            aux maxName
        | Integer           -> System.Int64.MinValue.ToString().Length
        | BitString         -> int(GetSizeableMaxItems t) * 8
        | OctetString       -> int(GetSizeableMaxItems t) * 2
        | Real              -> 50
        | NullType          -> 0
        | IA5String     
        | NumericString     -> int(GetSizeableMaxItems t)
        | ReferenceType(_)  -> GetMaxSizeInBytesForXER_content (GetActualType t r)
        | Sequence(children)-> 
            children |> Seq.map(fun c -> GetMaxSizeInBytesForXER c.Type c.Name.Value r) |> Seq.fold (+) 0
        | Choice(children)  -> 
            match children with
            | []    -> 0
            | _     -> children |> Seq.map(fun c -> GetMaxSizeInBytesForXER c.Type c.Name.Value r) |> Seq.max
        | SequenceOf(child) -> 
            let childSize = GetMaxSizeInBytesForXER child (XerTag child r) r
            childSize*(int (GetSizeableMaxItems t))

    let content = GetMaxSizeInBytesForXER_content t
    match xmlTag with
    | null  -> content
    | _     -> (2 * (xmlTag.Length + 2)) + 1 + content
    
*)