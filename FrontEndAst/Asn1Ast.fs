(*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*)

module Asn1Ast
open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime

open FsUtils
open CommonTypes
open AbstractMacros

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ASN1 VALUES DEFINITION    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type Asn1Value = {
    Kind:Asn1ValueKind
    id : ReferenceToValue
    Location: SrcLoc
    moduleName  : string
}



and Asn1ValueKind =
    |   IntegerValue        of IntLoc
    |   RealValue           of DoubleLoc
    |   StringValue         of (SingleStringValue list*SrcLoc)
    |   BooleanValue        of BoolLoc
    |   BitStringValue      of StringLoc
    |   OctetStringValue    of list<ByteLoc>
    |   RefValue            of StringLoc*StringLoc
    |   SeqOfValue          of list<Asn1Value>
    |   SeqValue            of list<StringLoc*Asn1Value>
    |   ChValue             of StringLoc*Asn1Value
    |   NullValue
    |   ObjOrRelObjIdValue  of ObjectIdentifierValueComponent list
    |   TimeValue           of Asn1DateTimeValueLoc




type NamedConstraintMark =
    | NoMark
    | MarkPresent
    | MarkAbsent
    | MarkOptional


type Asn1Constraint =
    | SingleValueConstraint              of string*Asn1Value
    | RangeConstraint                    of string*Asn1Value*Asn1Value*bool*bool    //min, max, InclusiveMin(=true), InclusiveMax(=true)
    | RangeConstraint_val_MAX            of string*Asn1Value*bool         //min, InclusiveMin(=true)
    | RangeConstraint_MIN_val            of string*Asn1Value*bool         //max, InclusiveMax(=true)
    | RangeConstraint_MIN_MAX
    | TypeInclusionConstraint           of string*StringLoc*StringLoc
    | SizeConstraint                     of string*Asn1Constraint
    | AlphabetConstraint                 of string*Asn1Constraint
    | UnionConstraint                   of string*Asn1Constraint*Asn1Constraint*bool //left,righ, virtual constraint
    | IntersectionConstraint            of string*Asn1Constraint*Asn1Constraint
    | AllExceptConstraint               of string*Asn1Constraint
    | ExceptConstraint                  of string*Asn1Constraint*Asn1Constraint
    | RootConstraint                    of string*Asn1Constraint
    | RootConstraint2                   of string*Asn1Constraint*Asn1Constraint
    | WithComponentConstraint           of string*Asn1Constraint*SrcLoc
    | WithComponentsConstraint          of string*list<NamedConstraint>

and NamedConstraint = {
    Name:StringLoc;
    Constraint:Asn1Constraint option
    Mark:NamedConstraintMark
}


type uperRange<'a> =
    | Concrete      of 'a*'a    //[a, b]
    | NegInf        of 'a               //(-inf, b]
    | PosInf        of 'a               //[a, +inf)
    | Empty
    | Full                                      // (-inf, +inf)

type Asn1Size<'a> =
    | Bounded of   'a
    | Infinite


type INTTYPE =
    | UINT      // declared as unsigned integer
    | SINT      // declared as signed integer



type NamedItem = {
    Name:StringLoc
    c_name:string
    scala_name:string
    ada_name:string
    _value:Asn1Value option
    Comments: string array
}

type Optional = {
    defaultValue        : Asn1Value option
}

type Asn1Optionality =
    | AlwaysAbsent
    | AlwaysPresent
    | Optional          of Optional





type Asn1Type = {
    Kind            : Asn1TypeKind;
    Constraints     : Asn1Constraint list
    unitsOfMeasure : string option

    Location        : SrcLoc //Line no, Char pos
    parameterizedTypeInstance : bool
    acnInfo     :    AcnGenericTypes.AcnTypeEncodingSpec option
    moduleName  : string
}


and Asn1TypeKind =
    | Integer
    | Real
    | IA5String
    | NumericString
    | OctetString
    | NullType
    | TimeType         of TimeTypeClass
    | BitString        of list<NamedBit0>
    | Boolean
    | ObjectIdentifier
    | RelativeObjectIdentifier
    | Enumerated        of list<NamedItem>
    | SequenceOf        of Asn1Type
    | Sequence          of list<ChildInfo>
    | Choice            of list<ChildInfo>
    | ReferenceType     of ReferenceType

and ReferenceType = {
    modName     : StringLoc
    tasName     : StringLoc
    tabularized : bool
    refEnc      : ContainedInOctOrBitString option
}


and ChildInfo = {
    Name                        : StringLoc;
    c_name                      : string
    scala_name                  : string
    ada_name                    : string
    present_when_name           : string // used only by choices. Does not contain the "_PRESENT". Not to be used directly by backends.
    Type                        : Asn1Type
    Optionality                 : Asn1Optionality option
    Comments                    : string array
}


type TypeAssignment = {
    Name:StringLoc
    c_name:string
    scala_name:string
    ada_name:string
    Type:Asn1Type
    Comments: string array
    acnInfo   : ParameterizedAsn1Ast.AcnTypeAssignmentExtraInfo option
}

type ValueAssignment = {
    Name:StringLoc
    c_name:string
    scala_name:string
    ada_name:string
    Type:Asn1Type
    Value:Asn1Value
}

type Exports =
    | All
    | OnlySome of list<string>

type  ImportedModule = {
    Name:StringLoc
    Types:list<StringLoc>
    Values:list<StringLoc>
}

type Asn1Module = {
    Name : StringLoc
    TypeAssignments : list<TypeAssignment>
    ValueAssignments : list<ValueAssignment>
    Imports : list<ImportedModule>
    Exports : Exports
    Comments : string array
    position : SrcLoc*SrcLoc   //start pos, end pos
}

type Asn1File = {
    FileName:string;
    Tokens: IToken array
    Modules : list<Asn1Module>
}

type AstRoot = {
    Files: list<Asn1File>
    args:CommandLineSettings
    typeAssignmentsMap : Map<string*string, TypeAssignment>
}




type AstRoot with
    member r.Modules = r.Files |> List.collect(fun f -> f.Modules)
    member r.GetModuleByName(name:StringLoc)  =
        let (n,loc) = name.AsTupple
        match r.Modules |> Seq.tryFind( fun m -> m.Name = name)  with
        | Some(m) -> m
        | None    -> raise(SemanticError(loc, sprintf "No Module Defined with name: %s" n ))

type Asn1Module with
    member this.ExportedTypes =
        match this.Exports with
        | All   ->
            let importedTypes = this.Imports |> List.collect(fun imp -> imp.Types) |> List.map(fun x -> x.Value)
            (this.TypeAssignments |> List.map(fun x -> x.Name.Value))@importedTypes
        | OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> System.Char.IsUpper (x.Chars 0))
    member this.ExportedVars =
        match this.Exports with
        | All   -> this.ValueAssignments |> List.map(fun x -> x.Name.Value)
        | OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> not (System.Char.IsUpper (x.Chars 0)))
    member m.TryGetTypeAssignmentByName name (r:AstRoot) =
        match m.TypeAssignments|> Seq.tryFind(fun x -> x.Name = name) with
        | Some t   -> Some t
        | None      ->
            let othMods = m.Imports |> Seq.filter(fun imp -> imp.Types |> Seq.exists((=) name))
                                |> Seq.map(fun imp -> imp.Name) |> Seq.toList
            match othMods with
            | firstMod::_   ->
                match r.Modules |> Seq.tryFind( fun m -> m.Name = firstMod)  with
                | Some(m) -> m.TryGetTypeAssignmentByName name r
                | None    -> None
            | []            -> None

    member m.GetTypeAssignmentByName name (r:AstRoot) =
        match m.TypeAssignments|> Seq.tryFind(fun x -> x.Name = name) with
        | Some(t)   -> t
        | None      ->
            let othMods = m.Imports |> Seq.filter(fun imp -> imp.Types |> Seq.exists((=) name))
                                |> Seq.map(fun imp -> imp.Name) |> Seq.toList
            match othMods with
            | firstMod::tail   -> r.GetModuleByName(firstMod).GetTypeAssignmentByName name r
            | []               ->
                let (n,loc) = name.AsTupple
                raise(SemanticError(loc, sprintf "No Type Assignment with name: %s is defined in Module %s" n m.Name.Value))
    member m.GetValueAsigByName(name:StringLoc) (r:AstRoot) =
        let (n,loc) = name.AsTupple
        let value = m.ValueAssignments |> Seq.tryFind(fun x -> x.Name = name)
        match value with
        | Some(v)       -> v
        | None          ->
            let othMods = m.Imports
                          |> Seq.filter(fun imp -> imp.Values |> Seq.exists(fun vname -> vname = name))
                          |> Seq.map(fun imp -> imp.Name) |> Seq.toList
            match othMods with
            | firstMod::tail   -> r.GetModuleByName(firstMod).GetValueAsigByName name r
            | []               -> raise (SemanticError(loc, sprintf "No value assignment with name '%s' exists" n))



let rec GetActualType (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | ReferenceType ref ->
        let newmod = r.GetModuleByName(ref.modName)
        let tas = newmod.GetTypeAssignmentByName ref.tasName r
        GetActualType tas.Type r
    | _                         -> t


let GetBaseType (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | ReferenceType ref ->
        let newmod = r.GetModuleByName(ref.modName)
        let tas = newmod.GetTypeAssignmentByName ref.tasName r
        tas.Type
    | _                         -> t

let GetBaseTypeByName modName tasName (r:AstRoot) =
    let mdl = r.GetModuleByName(modName)
    let tas = mdl.GetTypeAssignmentByName tasName r
    tas.Type

let GetBaseTypeConsIncluded (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | ReferenceType ref ->
        let newmod = r.GetModuleByName(ref.modName)
        let tas = newmod.GetTypeAssignmentByName ref.tasName r
        let baseType = tas.Type
        {baseType with Constraints = baseType.Constraints@t.Constraints}
    | _                         -> t

let GetActualTypeByName modName tasName (r:AstRoot) =
    let mdl = r.GetModuleByName(modName)
    let tas = mdl.GetTypeAssignmentByName tasName r
    GetActualType tas.Type r

let GetBaseValue  modName vasName  (r:AstRoot) =
    let mdl = r.GetModuleByName(modName)
    let vas = mdl.GetValueAsigByName vasName r
    vas.Value

let rec GetActualValue modName vasName  (r:AstRoot) =
    let baseVal = GetBaseValue  modName vasName  r
    match baseVal.Kind with
    |RefValue(newModName, newVasName)   -> GetActualValue newModName newVasName r
    | _                                 -> baseVal


let rec GetValueAsInt (v:Asn1Value) r=
    match v.Kind with
    | IntegerValue(a)                       -> a.Value
    | RefValue(modName,valName)             -> GetValueAsInt (GetActualValue modName valName r) r
    | _                                     -> raise(SemanticError (v.Location, sprintf "Expecting Integer value"))

let GetActualTypeAllConsIncluded t (r:AstRoot) =
    let rec GetActualTypeAux (t:Asn1Type) (additionalConstraints:list<Asn1Constraint>)   =
        match t.Kind with
        | ReferenceType ref ->
            let newmod = r.GetModuleByName(ref.modName)
            let tas = newmod.GetTypeAssignmentByName ref.tasName r
            GetActualTypeAux tas.Type (t.Constraints@additionalConstraints)
        | _                         -> {t with Constraints = (t.Constraints@additionalConstraints)}
    GetActualTypeAux t []

let GetActualTypeByNameAllConsIncluded modName tasName (r:AstRoot) =
    let mdl = r.GetModuleByName(modName)
    let tas = mdl.GetTypeAssignmentByName tasName r
    GetActualTypeAllConsIncluded tas.Type r


let rec getASN1Name (r:AstRoot) (t:Asn1Type) =
    match t.Kind with
    | Integer         -> "INTEGER"
    | Real            -> "REAL"
    | IA5String       -> "IA5String"
    | NumericString   -> "NumericString"
    | OctetString     -> "OCTET STRING"
    | NullType        -> "NULL"
    | BitString _     -> "BIT STRING"
    | Boolean         -> "BOOLEAN"
    | Enumerated   _  -> "ENUMERATED"
    | SequenceOf   _  -> "SEQUENCE OF"
    | Sequence     _  -> "SEQUENCE"
    | Choice       _  -> "CHOICE"
    | ObjectIdentifier          -> "OBJECT IDENTIFIER"
    | RelativeObjectIdentifier  -> "RELATIVE-OID"
    | TimeType     _  -> "TIME"
    | ReferenceType _ -> getASN1Name r (GetActualType t r)

let rec GetMySelfAndChildren (t:Asn1Type) =
    seq {
        yield t
        match t.Kind with
        | SequenceOf(conType) ->  yield! GetMySelfAndChildren conType
        | Sequence(children) | Choice(children)->
            for ch in children do
                yield! GetMySelfAndChildren ch.Type
        |_ -> ()
    }
(*
type ChildInfo with
    member c.CName (lang:ProgrammingLanguage) =
        match lang with
        | Ada               -> c.ada_name
        | C                 -> c.c_name
        | Scala             -> c.scala_name
*)

type NamedItem with
    member c.CEnumName (r:AstRoot) (lang:ProgrammingLanguage) =
        match lang with
        |Ada    -> ToC2 (r.args.TypePrefix + c.ada_name)
        |C      -> ToC2 (r.args.TypePrefix + c.c_name)
        |Scala  -> ToC2 (r.args.TypePrefix + c.scala_name)

type Asn1Constraint with
    member this.Asn1Con =
        match this with
        | SingleValueConstraint      (s,_)           -> s
        | RangeConstraint            (s,_,_,_,_)     -> s
        | RangeConstraint_val_MAX    (s,_,_)         -> s
        | RangeConstraint_MIN_val    (s,_,_)         -> s
        | RangeConstraint_MIN_MAX                    -> "(MIN .. MAX)"
        | TypeInclusionConstraint   (s,_,_)         -> s
        | SizeConstraint             (s,_)           -> s
        | AlphabetConstraint         (s,_)           -> s
        | UnionConstraint           (s,_,_,_)       -> s
        | IntersectionConstraint    (s,_,_)         -> s
        | AllExceptConstraint       (s,_)           -> s
        | ExceptConstraint          (s,_,_)         -> s
        | RootConstraint            (s,_)           -> s
        | RootConstraint2           (s,_,_)         -> s
        | WithComponentConstraint   (s,_,_)         -> s
        | WithComponentsConstraint  (s,_)           -> s


let foldConstraint
    singleValueFunc rangeConstraintFunc rangeConstraint_val_MAX rangeConstraint_MIN_val rangeConstraint_MIN_MAX
    typeInclusionConstraint sizeConstraint alphabetConstraint
    withComponentConstraint namedItemConstraint withComponentsConstraint
    unionFunc intersectionFunc allExceptFunc exceptFunc rootFunc rootFunc2
    c =
    let rec loopRecursiveConstraint c =
        match c with
        | SingleValueConstraint (s,v)           -> singleValueFunc s v
        | RangeConstraint (s,a,b,inclusiveMin,inclusiveMax)    ->
            rangeConstraintFunc s a b inclusiveMin inclusiveMax
        | RangeConstraint_val_MAX (s,a, inclusive)  -> rangeConstraint_val_MAX s a inclusive
        | RangeConstraint_MIN_val (s,b, inclusive)  -> rangeConstraint_MIN_val s b inclusive
        | RangeConstraint_MIN_MAX                 -> rangeConstraint_MIN_MAX ()
        | TypeInclusionConstraint  (s,md,ts)       -> typeInclusionConstraint s md ts
        | SizeConstraint (s,c)                        -> sizeConstraint s (loopRecursiveConstraint c)
        | AlphabetConstraint  (s,c)                   -> alphabetConstraint s (loopRecursiveConstraint c)
        | WithComponentConstraint  (s,c,l)          -> withComponentConstraint s (loopRecursiveConstraint c) l
        | WithComponentsConstraint (s,nitems)        ->
            let newItems = nitems |> List.map(fun ni -> namedItemConstraint s ni   (ni.Constraint |> Option.map loopRecursiveConstraint))
            withComponentsConstraint s newItems
        | UnionConstraint(s,c1,c2,b)         ->
            let nc1 = loopRecursiveConstraint c1
            let nc2 = loopRecursiveConstraint c2
            unionFunc s nc1 nc2 b
        | IntersectionConstraint(s,c1,c2)    ->
            let nc1 = loopRecursiveConstraint c1
            let nc2 = loopRecursiveConstraint c2
            intersectionFunc s nc1 nc2
        | AllExceptConstraint(s,c1)          ->
            let nc1 = loopRecursiveConstraint c1
            allExceptFunc s nc1
        | ExceptConstraint(s,c1,c2)          ->
            let nc1 = loopRecursiveConstraint c1
            let nc2 = loopRecursiveConstraint c2
            exceptFunc s nc1 nc2
        | RootConstraint(s,c1)               ->
            let nc1 = loopRecursiveConstraint c1
            rootFunc s nc1
        | RootConstraint2(s,c1,c2)           ->
            let nc1 = loopRecursiveConstraint c1
            let nc2 = loopRecursiveConstraint c2
            rootFunc2 s nc1 nc2
    loopRecursiveConstraint c
