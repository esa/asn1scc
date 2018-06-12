module Asn1AcnAstUtilFunctions

open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open System
open FsUtils
open CommonTypes
open Asn1AcnAst



type Asn1Type with
    member this.uperMinSizeInBits =
        match this.Kind with
        | Integer        x -> x.uperMinSizeInBits
        | Real           x -> x.uperMinSizeInBits
        | IA5String      x -> x.uperMinSizeInBits
        | NumericString  x -> x.uperMinSizeInBits
        | OctetString    x -> x.uperMinSizeInBits
        | NullType       x -> x.uperMinSizeInBits
        | BitString      x -> x.uperMinSizeInBits
        | Boolean        x -> x.uperMinSizeInBits
        | Enumerated     x -> x.uperMinSizeInBits
        | SequenceOf     x -> x.uperMinSizeInBits
        | Sequence       x -> x.uperMinSizeInBits
        | Choice         x -> x.uperMinSizeInBits
        | ReferenceType  x -> x.resolvedType.uperMinSizeInBits

    member this.uperMaxSizeInBits =
        match this.Kind with
        | Integer        x -> x.uperMaxSizeInBits
        | Real           x -> x.uperMaxSizeInBits
        | IA5String      x -> x.uperMaxSizeInBits
        | NumericString  x -> x.uperMaxSizeInBits
        | OctetString    x -> x.uperMaxSizeInBits
        | NullType       x -> x.uperMaxSizeInBits
        | BitString      x -> x.uperMaxSizeInBits
        | Boolean        x -> x.uperMaxSizeInBits
        | Enumerated     x -> x.uperMaxSizeInBits
        | SequenceOf     x -> x.uperMaxSizeInBits
        | Sequence       x -> x.uperMaxSizeInBits
        | Choice         x -> x.uperMaxSizeInBits
        | ReferenceType  x -> x.resolvedType.uperMaxSizeInBits

    member this.acnMinSizeInBits =
        match this.Kind with
        | Integer        x -> x.acnMinSizeInBits
        | Real           x -> x.acnMinSizeInBits
        | IA5String      x -> x.acnMinSizeInBits
        | NumericString  x -> x.acnMinSizeInBits
        | OctetString    x -> x.acnMinSizeInBits
        | NullType       x -> x.acnMinSizeInBits
        | BitString      x -> x.acnMinSizeInBits
        | Boolean        x -> x.acnMinSizeInBits
        | Enumerated     x -> x.acnMinSizeInBits
        | SequenceOf     x -> x.acnMinSizeInBits
        | Sequence       x -> x.acnMinSizeInBits
        | Choice         x -> x.acnMinSizeInBits
        | ReferenceType  x -> x.resolvedType.acnMinSizeInBits

    member this.acnMaxSizeInBits =
        match this.Kind with
        | Integer        x -> x.acnMaxSizeInBits
        | Real           x -> x.acnMaxSizeInBits
        | IA5String      x -> x.acnMaxSizeInBits
        | NumericString  x -> x.acnMaxSizeInBits
        | OctetString    x -> x.acnMaxSizeInBits
        | NullType       x -> x.acnMaxSizeInBits
        | BitString      x -> x.acnMaxSizeInBits
        | Boolean        x -> x.acnMaxSizeInBits
        | Enumerated     x -> x.acnMaxSizeInBits
        | SequenceOf     x -> x.acnMaxSizeInBits
        | Sequence       x -> x.acnMaxSizeInBits
        | Choice         x -> x.acnMaxSizeInBits
        | ReferenceType  x -> x.resolvedType.acnMaxSizeInBits

    member this.ActualType =
        match this.Kind with
        | ReferenceType t-> t.resolvedType.ActualType
        | Integer      _ -> this
        | Real         _ -> this
        | IA5String    _ -> this
        | NumericString _ -> this
        | OctetString  _ -> this
        | NullType     _ -> this
        | BitString    _ -> this
        | Boolean      _ -> this
        | Enumerated   _ -> this
        | SequenceOf   _ -> this
        | Sequence     _ -> this
        | Choice       _ -> this

    member this.tasInfo =
        match this.typeAssignmentInfo with
        | Some (TypeAssignmentInfo tasInfo)  -> Some tasInfo
        | Some (ValueAssignmentInfo tasInfo)  -> None
        | None          ->
            match this.inheritInfo with
            | Some tasInfo  -> Some tasInfo.AsTasInfo
            | None          -> None

type AcnInsertedType with
    member this.acnMinSizeInBits =
        match this with
        | AcnInteger        x -> x.acnMinSizeInBits
        | AcnNullType       x -> x.acnMinSizeInBits
        | AcnBoolean        x -> x.acnMinSizeInBits
        | AcnReferenceToEnumerated x -> x.enumerated.acnMinSizeInBits
        | AcnReferenceToIA5String x -> x.str.acnMinSizeInBits
    member this.acnMaxSizeInBits =
        match this with
        | AcnInteger        x -> x.acnMaxSizeInBits
        | AcnNullType       x -> x.acnMaxSizeInBits
        | AcnBoolean        x -> x.acnMaxSizeInBits
        | AcnReferenceToEnumerated x -> x.enumerated.acnMaxSizeInBits
        | AcnReferenceToIA5String x -> x.str.acnMaxSizeInBits
    member this.acnAligment =
        match this with
        | AcnInteger        x -> x.acnAligment
        | AcnNullType       x -> x.acnAligment
        | AcnBoolean        x -> x.acnAligment
        | AcnReferenceToEnumerated x -> x.acnAligment
        | AcnReferenceToIA5String x -> x.acnAligment
    member this.Location =
        match this with
        | AcnInteger        x -> x.Location
        | AcnNullType       x -> x.Location
        | AcnBoolean        x -> x.Location
        | AcnReferenceToEnumerated x -> x.tasName.Location
        | AcnReferenceToIA5String x -> x.tasName.Location
       
type BitString with
    member this.MaxOctets = int (ceil ((double this.maxSize)/8.0))



type SeqChildInfo with
    member this.Name =
        match this with
        | Asn1Child x   -> x.Name
        | AcnChild  x   -> x.Name

    member this.acnMinSizeInBits =
        match this with
        | Asn1Child x   -> x.Type.acnMinSizeInBits
        | AcnChild  x   -> x.Type.acnMinSizeInBits
    member this.acnMaxSizeInBits =
        match this with
        | Asn1Child x   -> x.Type.acnMaxSizeInBits
        | AcnChild  x   -> x.Type.acnMaxSizeInBits
    member this.acnAligment =
        match this with
        | Asn1Child x   -> x.Type.acnAligment
        | AcnChild  x   -> x.Type.acnAligment
    member this.Optionality =
        match this with
        | Asn1Child x   -> x.Optionality
        | AcnChild  x   -> None


let rec getASN1Name  (t:Asn1Type) =
    match t.Kind with
    | Integer       _  -> "INTEGER"
    | Real          _  -> "REAL"
    | IA5String     _  -> "IA5String"
    | NumericString _  -> "NumericString"
    | OctetString   _  -> "OCTET STRING"
    | NullType      _  -> "NULL"
    | BitString     _  -> "BIT STRING"
    | Boolean       _  -> "BOOLEAN"
    | Enumerated    _  -> "ENUMERATED"
    | SequenceOf    _  -> "SEQUENCE OF"
    | Sequence      _  -> "SEQUENCE"
    | Choice        _  -> "CHOICE"
    | ReferenceType r  -> getASN1Name r.resolvedType

type Integer with
    member this.AllCons  = this.cons@this.withcons

type Real             with
    member this.AllCons  = this.cons@this.withcons

type StringType       with
    member this.AllCons  = this.cons@this.withcons


type OctetString      with
    member this.AllCons  = this.cons@this.withcons

type NullType         with
    member this.AllCons  = []

type BitString        with
    member this.AllCons  = this.cons@this.withcons

type Boolean          with
    member this.AllCons  = this.cons@this.withcons

type Enumerated       with
    member this.AllCons  = this.cons@this.withcons

type SequenceOf       with
    member this.AllCons  = this.cons@this.withcons

type Sequence         with
    member this.AllCons  = this.cons@this.withcons

type Choice           with
    member this.AllCons  = this.cons@this.withcons

//type ReferenceType    with
//    member this.AllCons  = this.cons@this.withcons


type Asn1Child with
    member this.getBackendName0 l = 
        match l with
        | CommonTypes.C         -> this._c_name
        | CommonTypes.Ada       -> this._ada_name
        | _                     -> this.Name.Value



type AcnPresentWhenConditionChoiceChild with
    member this.valueAsString = 
        match this with
        | PresenceInt   (_,v)  -> v.Value.ToString()
        | PresenceStr   (_,v)  -> v.Value
    member this.relativePath = 
        match this with
        | PresenceInt   (rp,_)
        | PresenceStr   (rp,_)  -> rp
    member this.location = 
        match this with
        | PresenceInt   (rp,intLoc) -> intLoc.Location
        | PresenceStr   (rp,strLoc) -> strLoc.Location
    member this.kind = 
        match this with
        | PresenceInt   _ -> 1
        | PresenceStr   _ -> 2



type Asn1Value with
    member this.getBackendName () =
        "unnamed_variable"



type Asn1OrAcnOrPrmType =
    | ACN_INSERTED_TYPE of AcnInsertedType
    | ASN1_TYPE         of Asn1Type
    | ACN_PARAMETER     of Asn1Type*AcnParameter


let locateTypeByRefId (r:AstRoot) (ReferenceToType nodes) =
    let origPath = ReferenceToType nodes
    let rec locateType (parent:Asn1OrAcnOrPrmType) (nodes:ScopeNode list) =
        match nodes with
        | []                        -> parent
        | (SEQ_CHILD chName)::rest  -> 
            match parent with
            | ASN1_TYPE t ->
                match t.Kind with
                | Sequence  seq ->
                    match seq.children |> Seq.tryFind(fun ch -> ch.Name.Value = chName) with
                    | Some (Asn1Child x)   -> locateType (ASN1_TYPE x.Type) rest
                    | Some (AcnChild  x)   -> locateType (ACN_INSERTED_TYPE x.Type) rest
                    | None      -> raise(UserException(sprintf "Invalid child name '%s'" chName ))
                | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
            | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
        | (CH_CHILD (chName, _))::rest  -> 
            match parent with
            | ASN1_TYPE t ->
                match t.Kind with
                | Choice  choice ->
                    match choice.children |> Seq.tryFind(fun ch -> ch.Name.Value = chName) with
                    | Some x   -> locateType (ASN1_TYPE x.Type) rest
                    | None     -> raise(UserException(sprintf "Invalid child name '%s'" chName ))
                | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
            | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
        | SQF::rest ->
            match parent with
            | ASN1_TYPE t ->
                match t.Kind with
                | SequenceOf sqof -> locateType (ASN1_TYPE sqof.child) rest
                | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
            | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
        | (PRM prmName)::[] ->
            match parent with
            | ASN1_TYPE t ->
                match t.acnParameters |> Seq.tryFind(fun p -> p.name = prmName) with
                | Some p    -> ACN_PARAMETER (t, p)
                | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
            | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
        | _     -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))

    match nodes with
    | (MD mdName)::(TA tasName)::restPath    -> 
        let md = 
            match r.Files |> List.collect(fun f -> f.Modules) |> Seq.tryFind (fun m -> m.Name.Value = mdName) with
            | Some md -> md
            | None    -> raise(UserException(sprintf "Invalid module name '%s'" mdName ))
        let tas = 
            match md.TypeAssignments |> Seq.tryFind(fun tas -> tas.Name.Value = tasName) with
            | Some tas -> tas
            | None    -> raise(UserException(sprintf "Invalid tas name '%s'" tasName ))
        locateType (ASN1_TYPE tas.Type) restPath
    | _                 -> raise(UserException(sprintf "Invalid module name " ))
        


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
        | Asn1Ast.All   -> 
            let importedTypes = this.Imports |> List.collect(fun imp -> imp.Types) |> List.map(fun x -> x.Value)
            (this.TypeAssignments |> List.map(fun x -> x.Name.Value))@importedTypes
        | Asn1Ast.OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> System.Char.IsUpper (x.Chars 0))
    member this.ExportedValues =
        match this.Exports with
        | Asn1Ast.All   -> this.ValueAssignments |> List.map(fun x -> x.Name.Value)
        | Asn1Ast.OnlySome(typesAndVars)    ->
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

let GetActualTypeByName (r:AstRoot) modName tasName  =
    let mdl = r.GetModuleByName(modName)
    let tas = mdl.GetTypeAssignmentByName tasName r
    GetActualType tas.Type r
