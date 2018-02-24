module XER

open System
open System.Numerics
open FsUtils
open Asn1Ast
open Asn1Fold





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