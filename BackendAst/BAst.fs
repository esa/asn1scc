module BAst

open System
open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open Constraints
open uPER2
open FsUtils


type TypeAssignmentInfo = {
    modName : string
    tasName : string
}

(*
typedefBaseType
uperBaseType
acnBaseType
*)

type Integer = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    cons                : IntegerTypeConstraint list
    withcons            : IntegerTypeConstraint list
    uperRange           : uperRange<BigInteger>
    baseType            : Integer option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons  = this.cons@this.withcons
    member this.IsUnsigned =
        match this.uperRange with
        | Concrete  (a,b) when a >= 0I -> true      //[a, b]
        | Concrete  _                  -> false
        | NegInf    _                  -> false    //(-inf, b]
        | PosInf   a when a >= 0I      -> true     //[a, +inf)
        | PosInf  _                    -> false    //[a, +inf)
        | Full    _                    -> false    // (-inf, +inf)

type Real = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    cons                : RealTypeConstraint list
    withcons            : RealTypeConstraint list
    uperRange           : uperRange<double>
    baseType            : Real option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons  = this.cons@this.withcons

type StringType = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    cons                : IA5StringConstraint list
    withcons            : IA5StringConstraint list
    minSize             : int
    maxSize             : int
    charSet             : char array
    baseType            : StringType option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons  = this.cons@this.withcons

type EnumItem = {
    name        : string
    Value       : BigInteger
    comments    : string list
    c_name:string
    ada_name:string
}
with
    member this.getBackendName l =
        match l with
        | C         -> ToC this.c_name
        | Ada       -> ToC this.ada_name

type Enumerated = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    items               : EnumItem list
    userDefinedValues   : bool      //if true, the user has associated at least one item with a value
    cons                : EnumConstraint list
    withcons            : EnumConstraint list
    baseType            : Enumerated option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons  = this.cons@this.withcons


type Boolean = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    cons                : BoolConstraint list
    withcons            : BoolConstraint list
    baseType            : Boolean option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons  = this.cons@this.withcons


type OctetString = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    cons                : OctetStringConstraint list
    withcons            : OctetStringConstraint list
    minSize             : int
    maxSize             : int
    baseType            : OctetString option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons = this.cons@this.withcons

type NullType = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    baseType            : NullType option
    Location            : SrcLoc
}

type BitString = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    cons                : BitStringConstraint list
    withcons            : BitStringConstraint list
    minSize             : int
    maxSize             : int
    baseType            : BitString option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons = this.cons@this.withcons

type Asn1Optionality =
    | AlwaysAbsent
    | AlwaysPresent
    | Optional
    | Default   of Asn1GenericValue

type SequenceOf = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    childType           : Asn1Type
    cons                : SequenceOfConstraint list
    withcons            : SequenceOfConstraint list
    minSize             : int
    maxSize             : int
    baseType            : SequenceOf option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons = this.cons@this.withcons

and ChildInfo = {
    Name                :string
    chType              :Asn1Type
    Optionality         :Asn1Optionality option
    Comments            :string list
    acnInsertedField    :bool
    Location            : SrcLoc
}

and Sequence = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    children            : ChildInfo list
    cons                : SequenceConstraint list
    withcons            : SequenceConstraint list
    baseType            : Sequence option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons = this.cons@this.withcons

and Choice = {
    id                  : ReferenceToType
    tasInfo             : TypeAssignmentInfo option
    uperMaxSizeInBits   : int
    uperMinSizeInBits   : int
    children            : ChildInfo list
    cons                : ChoiceConstraint list
    withcons            : ChoiceConstraint list
    baseType            : Choice option
    Location            : SrcLoc
}
with
    member this.Cons     = this.cons
    member this.WithCons = this.withcons
    member this.AllCons = this.cons@this.withcons



and Asn1Type =
    | Integer           of Integer
    | Real              of Real
    | IA5String         of StringType
    | OctetString       of OctetString
    | NullType          of NullType
    | BitString         of BitString
    | Boolean           of Boolean
    | Enumerated        of Enumerated
    | SequenceOf        of SequenceOf
    | Sequence          of Sequence
    | Choice            of Choice
with
    member this.id =
        match this with
        | Integer      t -> t.id
        | Real         t -> t.id
        | IA5String    t -> t.id
        | OctetString  t -> t.id
        | NullType     t -> t.id
        | BitString    t -> t.id
        | Boolean      t -> t.id
        | Enumerated   t -> t.id
        | SequenceOf   t -> t.id
        | Sequence     t -> t.id
        | Choice       t -> t.id
    member this.baseType =
        match this with
        | Integer      t -> t.baseType |> Option.map Integer
        | Real         t -> t.baseType |> Option.map Real
        | IA5String    t -> t.baseType |> Option.map IA5String
        | OctetString  t -> t.baseType |> Option.map OctetString
        | NullType     t -> t.baseType |> Option.map NullType
        | BitString    t -> t.baseType |> Option.map BitString
        | Boolean      t -> t.baseType |> Option.map Boolean
        | Enumerated   t -> t.baseType |> Option.map Enumerated
        | SequenceOf   t -> t.baseType |> Option.map SequenceOf
        | Sequence     t -> t.baseType |> Option.map Sequence
        | Choice       t -> t.baseType |> Option.map Choice
    member this.uperMaxSizeInBits =
        match this with
        | Integer      t -> t.uperMaxSizeInBits
        | Real         t -> t.uperMaxSizeInBits
        | IA5String    t -> t.uperMaxSizeInBits
        | OctetString  t -> t.uperMaxSizeInBits
        | NullType     t -> t.uperMaxSizeInBits
        | BitString    t -> t.uperMaxSizeInBits
        | Boolean      t -> t.uperMaxSizeInBits
        | Enumerated   t -> t.uperMaxSizeInBits
        | SequenceOf   t -> t.uperMaxSizeInBits
        | Sequence     t -> t.uperMaxSizeInBits
        | Choice       t -> t.uperMaxSizeInBits
    member this.uperMinSizeInBits =
        match this with
        | Integer      t -> t.uperMinSizeInBits
        | Real         t -> t.uperMinSizeInBits
        | IA5String    t -> t.uperMinSizeInBits
        | OctetString  t -> t.uperMinSizeInBits
        | NullType     t -> t.uperMinSizeInBits
        | BitString    t -> t.uperMinSizeInBits
        | Boolean      t -> t.uperMinSizeInBits
        | Enumerated   t -> t.uperMinSizeInBits
        | SequenceOf   t -> t.uperMinSizeInBits
        | Sequence     t -> t.uperMinSizeInBits
        | Choice       t -> t.uperMinSizeInBits

    member this.asn1Name =
        match this.id with
        | ReferenceToType((GenericFold2.MD _)::(GenericFold2.TA tasName)::[])   -> Some tasName
        | _                                                                     -> None



type AstRoot = AstRootTemplate<Asn1Type>



(*
Statically resolved types with Records

type Cat = { Name : string } member x.Nickname = x.Name
type Dog = { Name : string } member x.Nickname = x.Name

let inline showName (animal : ^T ) =
    let name = (^T : (member Nickname : string) (animal))
    printfn "%s" name

let cat = { Name = "Miaou" } : Cat
let dog = { Name = "Waf" } : Dog

showName cat
showName dog
*)