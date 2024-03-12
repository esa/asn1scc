module Constraints

open System
open System.Numerics
open FsUtils
open Antlr.Runtime
open CommonTypes
type ProgrammingLanguage =
    |C
    |Ada
with
    member this.SpecExtention =
        match this with
        |C      -> "h"
        |Ada    -> "ads"
    member this.BodyExtention =
        match this with
        |C      -> "c"
        |Ada    -> "adb"
    member this.ArrName =
        match this with
        |C      -> "arr"
        |Ada    -> "Data"
    member this.AssignOperator =
        match this with
        |C      -> "="
        |Ada    -> ":="
    member this.ArrayAccess idx =
        match this with
        |C      -> "[" + idx + "]"
        |Ada    -> "(" + idx + ")"
    member this.ExpOr e1 e2 =
        match this with
        |C      -> isvalid_c.ExpOr e1 e2
        |Ada    -> isvalid_a.ExpOr e1 e2
    member this.ExpAnd e1 e2 =
        match this with
        |C      -> isvalid_c.ExpAnd e1 e2
        |Ada    -> isvalid_a.ExpAnd e1 e2
    member this.ExpAndMulti expList =
        match this with
        |C      -> isvalid_c.ExpAndMulti expList
        |Ada    -> isvalid_a.ExpAndMulti expList
    member this.ExpNot e  =
        match this with
        |C      -> isvalid_c.ExpNot e
        |Ada    -> isvalid_a.ExpNot e
    member this.ExpEqual e1 e2  =
        match this with
        |C      -> isvalid_c.ExpEqual e1 e2
        |Ada    -> isvalid_a.ExpEqual e1 e2
    member this.ExpStringEqual e1 e2  =
        match this with
        |C      -> isvalid_c.ExpStringEqual e1 e2
        |Ada    -> isvalid_a.ExpStringEqual e1 e2
    member this.ExpGt e1 e2  =
        match this with
        |C      -> isvalid_c.ExpGt e1 e2
        |Ada    -> isvalid_a.ExpGt e1 e2
    member this.ExpGte e1 e2  =
        match this with
        |C      -> isvalid_c.ExpGte e1 e2
        |Ada    -> isvalid_a.ExpGte e1 e2
    member this.ExpLt e1 e2  =
        match this with
        |C      -> isvalid_c.ExpLt e1 e2
        |Ada    -> isvalid_a.ExpLt e1 e2
    member this.ExpLte e1 e2  =
        match this with
        |C      -> isvalid_c.ExpLte e1 e2
        |Ada    -> isvalid_a.ExpLte e1 e2
    member this.StrLen exp =
        match this with
        |C      -> isvalid_c.StrLen exp
        |Ada    -> isvalid_a.StrLen exp
    member this.Length exp sAcc =
        match this with
        |C      -> isvalid_c.ArrayLen exp sAcc
        |Ada    -> isvalid_a.ArrayLen exp sAcc
    member this.ArrayStartIndex =
        match this with
        |C      -> 0
        |Ada    -> 1



type FuncParamType =
  | VALUE       of string
  | POINTER     of string
  | FIXARRAY    of string
  with
    member this.toPointer (l:ProgrammingLanguage) =
        POINTER (this.getPointer l)
    member this.getPointer (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> x
        | Ada, POINTER x    -> x
        | Ada, FIXARRAY x   -> x
        | C, VALUE x        -> sprintf "(&(%s))" x
        | C, POINTER x      -> x
        | C, FIXARRAY x     -> x
    member this.getValue (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> x
        | Ada, POINTER x    -> x
        | Ada, FIXARRAY x   -> x
        | C, VALUE x        -> x
        | C, POINTER x      -> sprintf "(*(%s))" x
        | C, FIXARRAY x     -> x
    member this.p  =
        match this with
        | VALUE x      -> x
        | POINTER x    -> x
        | FIXARRAY x   -> x
    member this.getAcces (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> "."
        | Ada, POINTER x    -> "."
        | Ada, FIXARRAY x   -> "."
        | C, VALUE x        -> "."
        | C, POINTER x      -> "->"
        | C, FIXARRAY x     -> ""

    member this.getStar (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> ""
        | Ada, POINTER x    -> ""
        | Ada, FIXARRAY x   -> ""
        | C, VALUE x        -> ""
        | C, POINTER x      -> "*"
        | C, FIXARRAY x     -> ""
    member this.getArrayItem (l:ProgrammingLanguage) (idx:string) (childTypeIsString: bool) =
        match l with
        | Ada   ->
            let newPath = sprintf "%s.Data(%s)" this.p idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | C     ->
            let newPath = sprintf "%s%sarr[%s]" this.p (this.getAcces l) idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
    member this.getSeqChild (l:ProgrammingLanguage) (childName:string) (childTypeIsString: bool) =
        match l with
        | Ada   ->
            let newPath = sprintf "%s.%s" this.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | C     ->
            let newPath = sprintf "%s%s%s" this.p (this.getAcces l) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)

    member this.getChChild (l:ProgrammingLanguage) (childName:string) (childTypeIsString: bool) =
        match l with
        | Ada   ->
            let newPath = sprintf "%s.%s" this.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | C     ->
            let newPath = sprintf "%s%su.%s" this.p (this.getAcces l) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)



type Asn1TypeName = {
    moduName    : string
    tasName     : string
}
with
    member this.id =
        ReferenceToType((GenericFold2.MD this.moduName)::(GenericFold2.TA this.tasName)::[])


and Asn1ValueName = {
    moduName    : string
    vasName     : string
}



and ReferenceToType =
    | ReferenceToType of GenericFold2.ScopeNode list
    with
        override this.ToString() =
            match this with
            | ReferenceToType path -> path |> Seq.StrJoin "."
        member this.ToScopeNodeList =
            match this with
            | ReferenceToType path -> path
        member this.ModName =
            match this with
            | ReferenceToType path ->
                match path with
                | (GenericFold2.MD modName)::_    -> modName
                | _                               -> raise(BugErrorException "Did not find module at the begining of the scope path")
        member this.Asn1TypeName =
            match this with
            | ReferenceToType path ->
                match path with
                | (GenericFold2.MD mdName)::(GenericFold2.TA tasName)::[]   -> Some ({Asn1TypeName.moduName = mdName; tasName=tasName})
                | _                                                         -> None
        member this.BelongingTypeAssignment =
            match this with
            | ReferenceToType path ->
                match path with
                | (GenericFold2.MD mdName)::(GenericFold2.TA tasName)::_   -> Some ({Asn1TypeName.moduName = mdName; tasName=tasName})
                | _                                                         -> None
        member this.AcnAbsPath =
            match this with
            | ReferenceToType path -> path |> List.map (fun i -> i.StrValue)
        member this.getSeqChildId (childName:string) =
            match this with
            | ReferenceToType path -> ReferenceToType (path@[GenericFold2.SEQ_CHILD childName])
        member this.getChildId (childName:string) =
            match this with
            | ReferenceToType path -> ReferenceToType (path@[GenericFold2.CH_CHILD childName])
        member this.getParamId (paramName:string) =
            match this with
            | ReferenceToType ((GenericFold2.MD mdName)::(GenericFold2.TA tasName)::[]) -> ReferenceToType ((GenericFold2.MD mdName)::(GenericFold2.TA tasName)::[GenericFold2.PRM paramName])
            | _                                                                         -> raise(BugErrorException "Cannot add parameter here. Only within TAS scope")
        member this.getTempId (tmpTypeName:string) =
            match this with
            | ReferenceToType ((GenericFold2.MD mdName)::(GenericFold2.TA tasName)::[]) -> ReferenceToType ((GenericFold2.MD mdName)::(GenericFold2.TA tasName)::[GenericFold2.TMP tmpTypeName])
            | _                                                                         -> raise(BugErrorException "Cannot add temp type here. Only within TAS scope")

        member this.appendLongChildId (childRelativePath:string list) =
            match this with
            | ReferenceToType path ->
                let newTail =
                    childRelativePath |>
                    List.map(fun s ->GenericFold2.SEQ_CHILD s)
                ReferenceToType (path@newTail)
        member this.beginsWith (md:string) (ts:string)=
            match this with
            | ReferenceToType((GenericFold2.MD mdName)::(GenericFold2.TA tasName)::[])   -> mdName = md && tasName = ts
            | _                                                                          -> false
        member this.lastItem =
            match this with
            | ReferenceToType path ->
                match path |> List.rev |> List.head with
                | GenericFold2.SEQ_CHILD name   -> name
                | GenericFold2.CH_CHILD name    -> name
                | _                             -> raise (BugErrorException "error in lastitem")
        member this.parentTypeId =
            match this with
            | ReferenceToType path ->
                let pathPar = path |> List.rev |> List.tail |> List.rev
                match pathPar with
                | []
                | _::[]     -> None
                | _         -> Some (ReferenceToType pathPar)
        member this.SequenceOfLevel =
            match this with
            | ReferenceToType path -> path |> List.filter(fun n -> match n with GenericFold2.SQF -> true | _ -> false) |> Seq.length
        static member createFromModAndTasName (modName : string) ((tasName : string))=
            ReferenceToType((GenericFold2.MD modName)::(GenericFold2.TA tasName)::[])

        //static member createFromAcnAbsPath (absPath : AcnTypes.AbsPath) =
        //    let tas = ReferenceToType((GenericFold2.MD absPath.Head)::(GenericFold2.TA absPath.Tail.Head)::[])
        //    tas.appendLongChildId(absPath.Tail.Tail)



type ReferenceToValue =
    | ReferenceToValue of (GenericFold2.ScopeNode list)*(GenericFold2.VarScopNode list)
    with
        member this.ModName =
            match this with
            | ReferenceToValue (path,_) ->
                match path with
                | (GenericFold2.MD modName)::_    -> modName
                | _                               -> raise(BugErrorException "Did not find module at the begining of the scope path")



type Asn1Module = {
    Name : string
    Imports : list<Ast.ImportedModule>
    Exports : Ast.Exports
    Comments : string array
}

type Asn1File = {
    FileName:string;
    Tokens: IToken array
    Modules : list<Asn1Module>
}
with
    member this.FileNameWithoutExtension = System.IO.Path.GetFileNameWithoutExtension this.FileName


type LiteralOrReference =
    | Literal
    | ReferenceToAsn1NamedValue  of Asn1ValueName




type Asn1ValueTemplate<'v> = {
    id          : ReferenceToValue
    litOrRef    : LiteralOrReference
    refToType   : ReferenceToType

    //childValue is true if the value is contained within another value (one of SEQUENCE, CHOICE, SEQUENCE OF) .e.g.
    // MySeq ::= {a INTEGER, b INTEGER}
    //mySeqValue MySeq ::= {a 10, b 20}
    //In mySeqValue childValue is false, while in value a 10 is true.
    childValue  : bool
    Value       :'v
}


type IntegerValue         = Asn1ValueTemplate<BigInteger>
type RealValue            = Asn1ValueTemplate<double>
type StringValue          = Asn1ValueTemplate<string>
type BooleanValue         = Asn1ValueTemplate<bool>
type BitStringValue       = Asn1ValueTemplate<string>
type OctetStringValue     = Asn1ValueTemplate<list<byte>>
type EnumValue            = Asn1ValueTemplate<string>
type NullValue            = Asn1ValueTemplate<unit>
type SeqOfValue           = Asn1ValueTemplate<list<Asn1GenericValue>>
and SeqValue              = Asn1ValueTemplate<list<NamedValue>>
and ChValue               = Asn1ValueTemplate<NamedValue>

and NamedValue = {
    name        : string
    Value       : Asn1GenericValue
}

and Asn1GenericValue =
    | IntegerValue          of IntegerValue
    | RealValue             of RealValue
    | StringValue           of StringValue
    | BooleanValue          of BooleanValue
    | BitStringValue        of BitStringValue
    | OctetStringValue      of OctetStringValue
    | EnumValue             of EnumValue
    | SeqOfValue            of SeqOfValue
    | SeqValue              of SeqValue
    | ChValue               of ChValue
    | NullValue             of NullValue
with
    member this.id =
        match this with
        | IntegerValue     v    -> v.id
        | RealValue        v    -> v.id
        | StringValue      v    -> v.id
        | BooleanValue     v    -> v.id
        | BitStringValue   v    -> v.id
        | OctetStringValue v    -> v.id
        | EnumValue        v    -> v.id
        | SeqOfValue       v    -> v.id
        | SeqValue         v    -> v.id
        | ChValue          v    -> v.id
        | NullValue        v    -> v.id
    member this.refToType =
        match this with
        | IntegerValue     v    -> v.refToType
        | RealValue        v    -> v.refToType
        | StringValue      v    -> v.refToType
        | BooleanValue     v    -> v.refToType
        | BitStringValue   v    -> v.refToType
        | OctetStringValue v    -> v.refToType
        | EnumValue        v    -> v.refToType
        | SeqOfValue       v    -> v.refToType
        | SeqValue         v    -> v.refToType
        | ChValue          v    -> v.refToType
        | NullValue        v    -> v.refToType

    member this.childValue =
        match this with
        | IntegerValue     v    -> v.childValue
        | RealValue        v    -> v.childValue
        | StringValue      v    -> v.childValue
        | BooleanValue     v    -> v.childValue
        | BitStringValue   v    -> v.childValue
        | OctetStringValue v    -> v.childValue
        | EnumValue        v    -> v.childValue
        | SeqOfValue       v    -> v.childValue
        | SeqValue         v    -> v.childValue
        | ChValue          v    -> v.childValue
        | NullValue        v    -> v.childValue
    member this.isVAS =
        match this.id with
        | ReferenceToValue (typePath,(GenericFold2.VA2 vasName)::[]) -> true
        | _                                                          -> false
    member this.getBackendName (l:ProgrammingLanguage) =
        match this.id with
        | ReferenceToValue (typePath,(GenericFold2.VA2 vasName)::[]) -> ToC vasName
        | ReferenceToValue (typePath, vasPath)      ->
            let longName = (typePath.Tail |> List.map (fun i -> i.StrValue))@ (vasPath |> List.map (fun i -> i.StrValue))  |> Seq.StrJoin "_"
            ToC2(longName.Replace("#","elem").L1)

    member this.isLiteral (l:ProgrammingLanguage) =
        match this with
        | IntegerValue     v    -> true
        | RealValue        v    -> true
        | StringValue      v    -> true
        | BooleanValue     v    -> true
        | BitStringValue   v    -> false
        | OctetStringValue v    -> false
        | EnumValue        v    -> true
        | SeqOfValue       v    -> false
        | SeqValue         v    -> false
        | ChValue          v    -> false
        | NullValue        v    -> true

type AstRootTemplate<'ASN1TYPE> = {
    Files: list<Asn1File>
    args:CommandLineSettings
    valsMap : Map<ReferenceToValue, Asn1GenericValue>
    typesMap : Map<ReferenceToType, 'ASN1TYPE>
    TypeAssignments : list<'ASN1TYPE>
    ValueAssignments : list<Asn1GenericValue>
}


(*
These constraints are interim.
*)
type Asn1AnyConstraint =
    | AnySingleValueConstraint              of Asn1GenericValue
    | AnyRangeConstraint                    of Asn1GenericValue*Asn1GenericValue*bool*bool    //min, max, InclusiveMin(=true), InclusiveMax(=true)
    | AnyRangeConstraint_val_MAX            of Asn1GenericValue*bool         //min, InclusiveMin(=true)
    | AnyRangeConstraint_MIN_val            of Asn1GenericValue*bool         //max, InclusiveMax(=true)
    | AnySizeConstraint                     of Asn1AnyConstraint
    | AnyAlphabetConstraint                 of Asn1AnyConstraint
    | AnyUnionConstraint                   of Asn1AnyConstraint*Asn1AnyConstraint*bool //left,righ, virtual constraint
    | AnyIntersectionConstraint            of Asn1AnyConstraint*Asn1AnyConstraint
    | AnyAllExceptConstraint               of Asn1AnyConstraint
    | AnyExceptConstraint                  of Asn1AnyConstraint*Asn1AnyConstraint
    | AnyRootConstraint                    of Asn1AnyConstraint
    | AnyRootConstraint2                   of Asn1AnyConstraint*Asn1AnyConstraint


type GenericConstraint<'v> =
    | UnionConstraint                   of GenericConstraint<'v>*GenericConstraint<'v>*bool //left,righ, virtual constraint
    | IntersectionConstraint            of GenericConstraint<'v>*GenericConstraint<'v>
    | AllExceptConstraint               of GenericConstraint<'v>
    | ExceptConstraint                  of GenericConstraint<'v>*GenericConstraint<'v>
    | RootConstraint                    of GenericConstraint<'v>
    | RootConstraint2                   of GenericConstraint<'v>*GenericConstraint<'v>
    | SingleValueConstraint             of 'v*LiteralOrReference

type RangeTypeConstraint<'v1,'v2>  =
    | RangeUnionConstraint               of RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>*bool //left,righ, virtual constraint
    | RangeIntersectionConstraint        of RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeAllExceptConstraint           of RangeTypeConstraint<'v1,'v2>
    | RangeExceptConstraint              of RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeRootConstraint                of RangeTypeConstraint<'v1,'v2>
    | RangeRootConstraint2               of RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeSingleValueConstraint         of 'v2*LiteralOrReference
    | RangeConstraint                     of ('v1*LiteralOrReference) *('v1*LiteralOrReference)*bool*bool    //min, max, InclusiveMin(=true), InclusiveMax(=true)
    | RangeConstraint_val_MAX             of ('v1*LiteralOrReference) *bool            //min, InclusiveMin(=true)
    | RangeConstraint_MIN_val             of ('v1*LiteralOrReference) *bool            //max, InclusiveMax(=true)

type IntegerTypeConstraint  = RangeTypeConstraint<BigInteger, BigInteger>
type PosIntTypeConstraint   = RangeTypeConstraint<UInt32, UInt32>
type CharTypeConstraint     = RangeTypeConstraint<char, string>

type RealTypeConstraint     = RangeTypeConstraint<double, double>


type SizableTypeConstraint<'v>  =
    | SizeUnionConstraint               of SizableTypeConstraint<'v>*SizableTypeConstraint<'v>*bool //left,righ, virtual constraint
    | SizeIntersectionConstraint        of SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeAllExceptConstraint           of SizableTypeConstraint<'v>
    | SizeExceptConstraint              of SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeRootConstraint                of SizableTypeConstraint<'v>
    | SizeRootConstraint2               of SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeSingleValueConstraint         of 'v*LiteralOrReference
    | SizeConstraint                     of PosIntTypeConstraint

type IA5StringConstraint =
    | StrUnionConstraint               of IA5StringConstraint*IA5StringConstraint*bool //left,righ, virtual constraint
    | StrIntersectionConstraint        of IA5StringConstraint*IA5StringConstraint
    | StrAllExceptConstraint           of IA5StringConstraint
    | StrExceptConstraint              of IA5StringConstraint*IA5StringConstraint
    | StrRootConstraint                of IA5StringConstraint
    | StrRootConstraint2               of IA5StringConstraint*IA5StringConstraint
    | StrSingleValueConstraint         of string*LiteralOrReference
    | StrSizeConstraint                 of PosIntTypeConstraint
    | AlphabetConstraint                of CharTypeConstraint

type OctetStringConstraint  =    SizableTypeConstraint<OctetStringValue>
type BitStringConstraint    =    SizableTypeConstraint<BitStringValue>
type BoolConstraint         =    GenericConstraint<bool>
type EnumConstraint         =    GenericConstraint<string>


type SequenceOfConstraint   =     SizableTypeConstraint<SeqOfValue>
type SequenceConstraint     =     GenericConstraint<SeqValue>
type ChoiceConstraint       =     GenericConstraint<ChValue>


let foldGenericConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    (c:GenericConstraint<'v>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:GenericConstraint<'v>) (s0:'UserState) =
        match c with
        | UnionConstraint(c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc nc1 nc2 b s2
        | IntersectionConstraint(c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc nc1 nc2 s2
        | AllExceptConstraint(c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc nc1 s1
        | ExceptConstraint(c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc nc1 nc2 s2
        | RootConstraint(c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc nc1 s1
        | RootConstraint2(c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 nc1 nc2 s2
        | SingleValueConstraint (v, rv)    -> singleValueFunc v rv s0
    loopRecursiveConstraint c s


let foldRangeTypeConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    rangeFunc range_val_max_func range_min_val_func
    (c:RangeTypeConstraint<'v,'vr>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:RangeTypeConstraint<'v,'vr>) (s0:'UserState) =
        match c with
        | RangeUnionConstraint(c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc nc1 nc2 b s2
        | RangeIntersectionConstraint(c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc nc1 nc2 s2
        | RangeAllExceptConstraint(c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc nc1 s1
        | RangeExceptConstraint(c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc nc1 nc2 s2
        | RangeRootConstraint(c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc nc1 s1
        | RangeRootConstraint2(c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 nc1 nc2 s2
        | RangeSingleValueConstraint (v, rv)            -> singleValueFunc v rv s0
        | RangeConstraint((v1,rv1), (v2,rv2), b1,b2)     -> rangeFunc v1 v2 b1 b2 s
        | RangeConstraint_val_MAX ((v,rv), b)            -> range_val_max_func v b s
        | RangeConstraint_MIN_val ((v,rv), b)            -> range_min_val_func v b s
    loopRecursiveConstraint c s

let foldSizableTypeConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc
    srangeFunc srange_val_max_func srange_min_val_func
    (c:SizableTypeConstraint<'v>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:SizableTypeConstraint<'v>) (s0:'UserState) =
        match c with
        | SizeUnionConstraint(c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc nc1 nc2 b s2
        | SizeIntersectionConstraint(c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc nc1 nc2 s2
        | SizeAllExceptConstraint(c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc nc1 s1
        | SizeExceptConstraint(c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc nc1 nc2 s2
        | SizeRootConstraint(c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc nc1 s1
        | SizeRootConstraint2(c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 nc1 nc2 s2
        | SizeSingleValueConstraint (v, rv)    -> singleValueFunc v rv s0
        | SizeConstraint    intCon   -> foldRangeTypeConstraint sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc srangeFunc srange_val_max_func srange_min_val_func intCon s
    loopRecursiveConstraint c s

let foldSizableTypeConstraint2 unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    foldRangeTypeConstraint
    (c:SizableTypeConstraint<'v>)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:SizableTypeConstraint<'v>) (s0:'UserState) =
        match c with
        | SizeUnionConstraint(c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc nc1 nc2 b s2
        | SizeIntersectionConstraint(c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc nc1 nc2 s2
        | SizeAllExceptConstraint(c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc nc1 s1
        | SizeExceptConstraint(c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc nc1 nc2 s2
        | SizeRootConstraint(c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc nc1 s1
        | SizeRootConstraint2(c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 nc1 nc2 s2
        | SizeSingleValueConstraint (v, rv)    -> singleValueFunc v rv s0
        | SizeConstraint    intCon   -> foldRangeTypeConstraint intCon s0
    loopRecursiveConstraint c s


let foldStringTypeConstraint unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc srangeFunc srange_val_max_func srange_min_val_func
    aunionFunc aintersectionFunc aallExceptFunc aexceptFunc arootFunc arootFunc2 asingleValueFunc arangeFunc arange_val_max_func arange_min_val_func
    (c:IA5StringConstraint)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:IA5StringConstraint) (s0:'UserState) =
        match c with
        | StrUnionConstraint(c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc nc1 nc2 b s2
        | StrIntersectionConstraint(c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc nc1 nc2 s2
        | StrAllExceptConstraint(c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc nc1 s1
        | StrExceptConstraint(c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc nc1 nc2 s2
        | StrRootConstraint(c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc nc1 s1
        | StrRootConstraint2(c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 nc1 nc2 s2
        | StrSingleValueConstraint (v, rv)    -> singleValueFunc v rv s0
        | StrSizeConstraint        intCon   -> foldRangeTypeConstraint sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc srangeFunc srange_val_max_func srange_min_val_func intCon s
        | AlphabetConstraint       alphaCon -> foldRangeTypeConstraint aunionFunc aintersectionFunc aallExceptFunc aexceptFunc arootFunc arootFunc2 asingleValueFunc arangeFunc arange_val_max_func arange_min_val_func alphaCon s
    loopRecursiveConstraint c s


let foldStringTypeConstraint2 unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2 singleValueFunc
    foldRangeSizeConstraint foldRangeAlphaConstraint
    (c:IA5StringConstraint)
    (s:'UserState) =
    let rec loopRecursiveConstraint (c:IA5StringConstraint) (s0:'UserState) =
        match c with
        | StrUnionConstraint(c1,c2,b)         ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            unionFunc nc1 nc2 b s2
        | StrIntersectionConstraint(c1,c2)    ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            intersectionFunc nc1 nc2 s2
        | StrAllExceptConstraint(c1)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            allExceptFunc nc1 s1
        | StrExceptConstraint(c1,c2)          ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            exceptFunc nc1 nc2 s2
        | StrRootConstraint(c1)               ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            rootFunc nc1 s1
        | StrRootConstraint2(c1,c2)           ->
            let nc1, s1 = loopRecursiveConstraint c1 s0
            let nc2, s2 = loopRecursiveConstraint c2 s1
            rootFunc2 nc1 nc2 s2
        | StrSingleValueConstraint (v, rv)    -> singleValueFunc v rv s0
        | StrSizeConstraint        intCon   -> foldRangeSizeConstraint  intCon s
        | AlphabetConstraint       alphaCon -> foldRangeAlphaConstraint alphaCon s
    loopRecursiveConstraint c s



// Calculate uPER range

