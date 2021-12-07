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

module ParameterizedAsn1Ast

open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open AbstractMacros
open AcnGenericTypes
open FsUtils
open System



type RangeTypeConstraint<'v1,'v2>  = 
    | RangeUnionConstraint               of RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>*bool //left,righ, virtual constraint
    | RangeIntersectionConstraint        of RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeAllExceptConstraint           of RangeTypeConstraint<'v1,'v2>
    | RangeExceptConstraint              of RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeRootConstraint                of RangeTypeConstraint<'v1,'v2>
    | RangeRootConstraint2               of RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeSingleValueConstraint         of 'v2
    | RangeContraint                     of ('v1) *('v1)*bool*bool    //min, max, InclusiveMin(=true), InclusiveMax(=true)
    | RangeContraint_val_MAX             of ('v1) *bool            //min, InclusiveMin(=true)
    | RangeContraint_MIN_val             of ('v1) *bool            //max, InclusiveMax(=true)

type PosIntTypeConstraint   = RangeTypeConstraint<UInt32, UInt32>

type SizableTypeConstraint<'v>  = 
    | SizeUnionConstraint               of SizableTypeConstraint<'v>*SizableTypeConstraint<'v>*bool //left,righ, virtual constraint
    | SizeIntersectionConstraint        of SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeAllExceptConstraint           of SizableTypeConstraint<'v>
    | SizeExceptConstraint              of SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeRootConstraint                of SizableTypeConstraint<'v>
    | SizeRootConstraint2               of SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeSingleValueConstraint         of 'v
    | SizeContraint                     of PosIntTypeConstraint               


type AstRoot = {
    Files: Asn1File list
    args:CommandLineSettings
}

and Asn1File = {
    FileName:string;
    Tokens: IToken array
    Modules : list<Asn1Module>
}

and Asn1Module = {
    Name:StringLoc
    TypeAssignments : list<TypeAssignment>
    ValueAssignments : list<ValueAssignment>
    Imports : list<ImportedModule>
    Exports : Exports
    Comments : string array
    postion : SrcLoc*SrcLoc   //start pos, end pos
}

and Exports =
    | All
    | OnlySome of list<string>

and  ImportedModule = {
    Name:StringLoc
    Types:list<StringLoc>
    Values:list<StringLoc>
}

and AcnTypeAssignmentExtraInfo = {
    loc : SrcLoc
    acnParameters   : AcnParameter list
    comments        : string list
}

and TypeAssignment = {
    Name:StringLoc
    Type:Asn1Type
    Parameters : TemplateParameter list
    Comments: string array
    acnInfo   : AcnTypeAssignmentExtraInfo option
}

and TemplateParameter =
    | TypeParameter of StringLoc
    | ValueParameter of Asn1Type*StringLoc

and ValueAssignment = {
    Name:StringLoc
    c_name:string
    ada_name:string
    Type:Asn1Type
    Value:Asn1Value
    Scope : ValueScope
}

and ValueScope =
    | GlobalScope
    | TypeScope  of StringLoc*StringLoc     



and Asn1Type = {
    Kind:Asn1TypeKind;
    Constraints:list<Asn1Constraint>;
    Location: SrcLoc   //Line no, Char pos
    parameterizedTypeInstance : bool
    acnInfo     : AcnTypeEncodingSpec option
}

and Asn1TypeKind =
    | Integer 
    | Real    
    | IA5String 
    | NumericString
    | OctetString 
    | ObjectIdentifier
    | RelativeObjectIdentifier
    | TimeType         of TimeTypeClass
    | NullType          
    | BitString         of list<NamedBit0>
    | Boolean 
    | Enumerated        of list<NamedItem>
    | SequenceOf        of Asn1Type    
    | Sequence          of list<SequenceChild>
    | Choice            of list<ChildInfo>
    | ReferenceType     of StringLoc*StringLoc*(ContainedInOctOrBitString option)*list<TemplateArgument>

and BitStringPosition = {
    Name:StringLoc
}

and TemplateArgument =
    | ArgType of Asn1Type
    | ArgValue of Asn1Value
    | TemplateParameter of StringLoc    //name of parameter

and NamedItem = {
    Name:StringLoc
    _value:Asn1Value option
    Comments: string array
}

and SequenceChild =
    | ChildInfo of ChildInfo
    | ComponentsOf of StringLoc*StringLoc

and ChildInfo = {
        Name:StringLoc;
        Type:Asn1Type;
        Optionality:Asn1Optionality option
        AcnInsertedField:bool
        Comments: string array
    }


and Asn1Optionality = 
    | AlwaysAbsent
    | AlwaysPresent
    | Optional  
    | Default   of Asn1Value


and Asn1Value = {
    Kind:Asn1ValueKind
    Location: SrcLoc
}



    

and Asn1ValueKind =
    |   IntegerValue        of IntLoc
    |   RealValue           of DoubleLoc
    |   StringValue         of StringLoc
    |   BooleanValue        of BoolLoc
    |   BitStringValue      of StringLoc
    |   OctetStringValue    of list<ByteLoc>
    |   RefValue            of StringLoc*StringLoc
    |   SeqOfValue          of list<Asn1Value>
    |   SeqValue            of list<StringLoc*Asn1Value>
    |   ChValue             of StringLoc*Asn1Value
    |   NullValue
    |   EmptyList
    |   ObjOrRelObjIdValue  of ObjectIdentifierValueCompoent list


and Asn1Constraint = 
    | SingleValueContraint              of Asn1Value             
    | RangeContraint                    of Asn1Value*Asn1Value*bool*bool    //min, max, InclusiveMin(=true), InclusiveMax(=true)         
    | RangeContraint_val_MAX            of Asn1Value*bool         //min, InclusiveMin(=true)
    | RangeContraint_MIN_val            of Asn1Value*bool         //max, InclusiveMax(=true)
    | TypeInclusionConstraint           of StringLoc*StringLoc     
    | SizeContraint                     of Asn1Constraint               
    | AlphabetContraint                 of Asn1Constraint           
    | UnionConstraint                   of Asn1Constraint*Asn1Constraint*bool //left,righ, virtual constraint
    | IntersectionConstraint            of Asn1Constraint*Asn1Constraint
    | AllExceptConstraint               of Asn1Constraint
    | ExceptConstraint                  of Asn1Constraint*Asn1Constraint
    | RootConstraint                    of Asn1Constraint
    | RootConstraint2                   of Asn1Constraint*Asn1Constraint
    | WithComponentConstraint           of Asn1Constraint*SrcLoc
    | WithComponentsConstraint          of list<NamedConstraint>

and NamedConstraint = {
    Name:StringLoc;
    Contraint:Asn1Constraint option
    Mark:NamedConstraintMark
}

and NamedConstraintMark =
    | NoMark
    | MarkPresent
    | MarkAbsent
    | MarkOptional



let getModuleByName (r:AstRoot) (mdName:StringLoc) =
    match r.Files|> Seq.collect(fun x -> x.Modules) |> Seq.tryFind(fun x -> x.Name = mdName) with
    | None  -> raise (SemanticError(mdName.Location, sprintf "Unknown module '%s'" mdName.Value))
    | Some (x) -> x


let getTasByName  (tsName:StringLoc) (m:Asn1Module) =
    match m.TypeAssignments |> Seq.tryFind(fun x -> x.Name = tsName) with
    | Some(x)   -> x
    | None      -> raise(SemanticError(tsName.Location, sprintf "No type assignment with name '%s' found in the module '%s'" tsName.Value m.Name.Value))


let getTypeAssignment r m t = m |> getModuleByName r |> getTasByName t

let rec TryGetActualType (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | ReferenceType(mn,tasname, _, _) ->
        let mods = r.Files |> List.collect (fun x -> x.Modules) 
        match  mods |> Seq.tryFind(fun m -> m.Name = mn) with
        | Some newmod ->
            match newmod.TypeAssignments |> Seq.tryFind(fun tas -> tas.Name.Value = tasname.Value) with
            | Some tas  -> TryGetActualType tas.Type r
            | None      -> None
        | None              -> None
    | _                         -> Some t


let rec GetActualType (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | ReferenceType(mn,tasname, _, _) ->
        match TryGetActualType t r with
        | Some t    -> t
        | None      -> raise(SemanticError(tasname.Location, sprintf "Reference type: %s.%s can not be resolved" mn.Value tasname.Value ))
    
    | _                         -> t


type Asn1Module with
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
            | firstMod::tail   -> (getModuleByName  r firstMod).GetValueAsigByName name r
            | []               -> raise (SemanticError(loc, sprintf "No value assignment with name '%s' exists" n))


let GetBaseValue  modName vasName  (r:AstRoot) =
    let mdl = getModuleByName r modName
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


let foldBConstraint 
    singleValueContraintFunc
    rangeContraintFunc
    rangeContraint_val_MAXFunc
    rangeContraint_MIN_valFunc
    sizeContraintFunc         
    alphabetContraintFunc     
    unionConstraintFunc       
    intersectionConstraintFunc
    allExceptConstraintFunc   
    exceptConstraintFunc      
    rootConstraintFunc        
    rootConstraint2Func  
    typeInclusionFnc
    withComponentConstraintFunc     
    withComponentsConstraintFunc     
    (c:Asn1Constraint) =
    match c with
    |Asn1Constraint.SingleValueContraint       rv                -> singleValueContraintFunc rv 
    |Asn1Constraint.RangeContraint             (rv1,rv2,b1,b2)   -> rangeContraintFunc rv1 rv2 b1 b2 
    |Asn1Constraint.RangeContraint_val_MAX     (rv,b)            -> rangeContraint_val_MAXFunc rv b 
    |Asn1Constraint.RangeContraint_MIN_val     (rv,b)            -> rangeContraint_MIN_valFunc rv b 
    |Asn1Constraint.SizeContraint              c                 -> sizeContraintFunc c 
    |Asn1Constraint.AlphabetContraint          c                 -> alphabetContraintFunc c 
    |Asn1Constraint.UnionConstraint            (c1,c2,b)         -> unionConstraintFunc c1 c2  b 
    |Asn1Constraint.IntersectionConstraint     (c1,c2)           -> intersectionConstraintFunc c1 c2 
    |Asn1Constraint.AllExceptConstraint        c                 -> allExceptConstraintFunc c 
    |Asn1Constraint.ExceptConstraint           (c1,c2)           -> exceptConstraintFunc c1 c2 
    |Asn1Constraint.RootConstraint             c1                -> rootConstraintFunc c1    
    |Asn1Constraint.RootConstraint2            (c1,c2)           -> rootConstraint2Func c1 c2      
    |Asn1Constraint.TypeInclusionConstraint    (md,ts)           -> typeInclusionFnc md ts
    |Asn1Constraint.WithComponentConstraint    (c,l)             -> withComponentConstraintFunc  c l //raise(BugErrorException "Unexpected constraint type")
    |Asn1Constraint.WithComponentsConstraint   ncs               -> withComponentsConstraintFunc ncs   //raise(BugErrorException "Unexpected constraint type")

type BitStringConstraint    =    SizableTypeConstraint<StringLoc>

let rec private getRangeTypeConstraint valueGetter  valueGetter2 (c:Asn1Constraint)   =
    foldBConstraint 
        (fun rv                 -> RangeTypeConstraint.RangeSingleValueConstraint (valueGetter2 rv )) 
        (fun rv1 rv2 b1 b2      -> RangeTypeConstraint.RangeContraint (valueGetter rv1 ,valueGetter rv2, b1,b2) )
        (fun rv b               -> RangeTypeConstraint.RangeContraint_val_MAX (valueGetter rv, b))
        (fun rv b               -> RangeTypeConstraint.RangeContraint_MIN_val (valueGetter rv, b))
        (fun c                  -> raise(BugErrorException "SizeContraint is not expected here"))
        (fun c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun c1 c2 b            -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeUnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeIntersectionConstraint (c1,c2))           
        (fun c                 -> 
            let c = getRangeTypeConstraint valueGetter valueGetter2 c 
            RangeAllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeExceptConstraint (c1,c2))
        (fun c                 -> 
            let c = getRangeTypeConstraint valueGetter valueGetter2 c 
            RangeRootConstraint c)
        (fun c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeRootConstraint2 (c1,c2))           
        (fun md ts  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        c


let rec private getSizeTypeConstraint (r:AstRoot) valueGetter  (c:Asn1Constraint)   =
    let rec posIntValGetter (r:AstRoot) (v:Asn1Value) =
        match v.Kind with
        | IntegerValue x when x.Value >= 0I && x.Value <= BigInteger UInt32.MaxValue  -> uint32 x.Value
        | IntegerValue x when x.Value > BigInteger UInt32.MaxValue     -> raise(SemanticError(v.Location, (sprintf "Constant value '%A' is too large" x.Value)))
        | RefValue (md, ts)     -> posIntValGetter r (GetActualValue md ts r)
        | _                                 -> raise(SemanticError(v.Location, "Value is not of expected type"))


    foldBConstraint 
        (fun rv                 -> SizeSingleValueConstraint (valueGetter rv)) 
        (fun rv1 rv2 b1 b2      -> raise(BugErrorException "Range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun c                  -> 
            let posIntCon = getRangeTypeConstraint (posIntValGetter r) (posIntValGetter r)  c
            SizableTypeConstraint.SizeContraint posIntCon)
        (fun c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun c1 c2 b            -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2 
            SizeUnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeIntersectionConstraint (c1,c2))           
        (fun c                 -> 
            let c = getSizeTypeConstraint r valueGetter c 
            SizeAllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeExceptConstraint (c1,c2))
        (fun c                 -> 
            let c = getSizeTypeConstraint r valueGetter c 
            SizeRootConstraint c)
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeRootConstraint2 (c1,c2))           
        (fun md ts  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        c

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
        | RangeSingleValueConstraint (v)            -> singleValueFunc v s0
        | RangeTypeConstraint.RangeContraint((v1), (v2), b1,b2)     -> rangeFunc v1 v2 b1 b2 s
        | RangeTypeConstraint.RangeContraint_val_MAX ((v), b)            -> range_val_max_func v b s
        | RangeTypeConstraint.RangeContraint_MIN_val ((v), b)            -> range_min_val_func v b s
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
        | SizeSingleValueConstraint (v)    -> singleValueFunc v s0
        | SizableTypeConstraint.SizeContraint    intCon   -> foldRangeTypeConstraint sunionFunc sintersectionFunc sallExceptFunc sexceptFunc srootFunc srootFunc2 ssingleValueFunc srangeFunc srange_val_max_func srange_min_val_func intCon s
    loopRecursiveConstraint c s

let getSizeableTypeConstraintUperRange (c:SizableTypeConstraint<'v>) funcGetLength (l:SrcLoc) =
    foldSizableTypeConstraint
        (fun r1 r2 b s      -> uperUnion r1 r2, s)
        (fun r1 r2 s        -> uperIntersection r1 r2 l, s)
        (fun r s            -> Full, s)       
        (fun r1 r2 s        -> r1, s)
        (fun r s            -> Full, s)       
        (fun r1 r2 s        -> Full, s)
        (fun v  s           -> Concrete (funcGetLength v,funcGetLength v),s)
        
        (fun r1 r2 b s      -> uperUnion r1 r2, s)
        (fun r1 r2 s        -> uperIntersection r1 r2 l, s)
        (fun r s            -> Full, s)       
        (fun r1 r2 s        -> r1, s)
        (fun r s            -> Full, s)       
        (fun r1 r2 s        -> Full, s)
        (fun v  s           -> Concrete (v,v),s)
        (fun v1 v2  minIsIn maxIsIn s  ->
            let val1 = if minIsIn then v1 else (v1+1u)
            let val2 = if maxIsIn then v2 else (v2-1u)
            Concrete(val1 , val2), s)
        (fun v1 minIsIn  s      -> 
            let val1 = if minIsIn then v1 else (v1+1u)
            PosInf(val1) ,s )
        (fun v2 maxIsIn s      -> 
            let val2 = if maxIsIn then v2 else (v2-1u)
            NegInf(val2), s)
        c 
        0 |> fst

let getSizeableUperRange (cons:SizableTypeConstraint<'v> list) funcGetLength (l:SrcLoc) =
    let getConUperRange (c:SizableTypeConstraint<'v>) (l:SrcLoc) =
        getSizeableTypeConstraintUperRange c  funcGetLength l 
    cons |> List.fold(fun s c -> uperIntersection s (getConUperRange c l) l) Full


let getBitStringUperRange (cons:BitStringConstraint list) (l:SrcLoc) =
    getSizeableUperRange cons (fun (x) -> uint32 x.Value.Length) l

let getBitStringConstraint   (r:AstRoot) (t:Asn1Type) = 
    let rec bitGetter (r:AstRoot)  (v:Asn1Value) =
        match v.Kind with
        | BitStringValue vl            -> (vl)
        | RefValue (md, ts)     -> bitGetter r (GetActualValue md ts r)
        | _                             -> raise(BugErrorException "Value is not of expected type")

    getSizeTypeConstraint r (bitGetter r )
