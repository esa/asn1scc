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

module MapParamAstToNonParamAst

open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime

open FsUtils




let getModuleByName = ParameterizedAsn1Ast.getModuleByName

let getTasByName  = ParameterizedAsn1Ast.getTasByName



let rec getSequenceChildren (r:ParameterizedAsn1Ast.AstRoot) (input:list<ParameterizedAsn1Ast.SequenceChild>) : list<ParameterizedAsn1Ast.ChildInfo> =
    seq {
        for ch in input do
        match ch with
        | ParameterizedAsn1Ast.ChildInfo ch  -> yield ch
        | ParameterizedAsn1Ast.ComponentsOf (md,ts) ->
            let tas = ParameterizedAsn1Ast.getTypeAssignment r md ts
            match tas.Type.Kind with
            | ParameterizedAsn1Ast.Sequence(children)    ->
                 yield! getSequenceChildren r children
            | _                                          -> raise(SemanticError(ts.Location, "Expecting SEQUENCE type"))    
            }|> Seq.toList

let rec getActualKind r kind =
    match kind with
    | ParameterizedAsn1Ast.ReferenceType(md, ts,_) -> 
        let newTas = ParameterizedAsn1Ast.getTypeAssignment r md ts
        getActualKind r newTas.Type.Kind
    | _                                            -> kind


let rec MapAsn1Value (r:ParameterizedAsn1Ast.AstRoot) (kind: ParameterizedAsn1Ast.Asn1TypeKind) (v:ParameterizedAsn1Ast.Asn1Value) :Ast.Asn1Value =
    let rec getActualKindAndModule r kind =
        let rec getActualaux r kind modName=
            match kind with
            | ParameterizedAsn1Ast.ReferenceType(md, ts,_) -> 
                let mdl = ParameterizedAsn1Ast.getModuleByName  r md
                let newTas = ParameterizedAsn1Ast.getTypeAssignment r md ts
                getActualaux r newTas.Type.Kind (Some mdl.Name)
            | _                                            -> kind, modName
        getActualaux r kind None

    let MapAsn1ValueKind (r:ParameterizedAsn1Ast.AstRoot) (kind: ParameterizedAsn1Ast.Asn1TypeKind) (vk:ParameterizedAsn1Ast.Asn1ValueKind) :Ast.Asn1ValueKind =
        let actKind = getActualKind r kind
        match vk with
        |ParameterizedAsn1Ast.IntegerValue(v)       -> Ast.IntegerValue v
        |ParameterizedAsn1Ast.RealValue(v)          -> Ast.RealValue v
        |ParameterizedAsn1Ast.StringValue(v)        -> Ast.StringValue v
        |ParameterizedAsn1Ast.BooleanValue(v)       -> Ast.BooleanValue v
        |ParameterizedAsn1Ast.BitStringValue(v)     -> Ast.BitStringValue v
        |ParameterizedAsn1Ast.OctetStringValue v    -> Ast.OctetStringValue v
        |ParameterizedAsn1Ast.RefValue(v1,v2)       -> 
            let actKind, mdName = getActualKindAndModule r kind
            match actKind with
            | ParameterizedAsn1Ast.Enumerated(items)    ->  
                match mdName with
                | None  ->  Ast.RefValue(v1,v2)
                | Some(s)   -> Ast.RefValue(s,v2)
            | _                                         ->  Ast.RefValue(v1,v2)
        |ParameterizedAsn1Ast.SeqOfValue(vals)      -> 
            match actKind with
            | ParameterizedAsn1Ast.SequenceOf(ch)    -> Ast.SeqOfValue(vals |> List.map (MapAsn1Value r ch.Kind))
            | _                                      -> raise(SemanticError(v.Location, "Expecting a SEQUENCE OF value"))
        |ParameterizedAsn1Ast.SeqValue(vals)        -> 
            match actKind with
            |ParameterizedAsn1Ast.Sequence(children) ->
                let children = getSequenceChildren r children
                let mapChildVal (nm:StringLoc, chv:ParameterizedAsn1Ast.Asn1Value) =
                    match children |> Seq.tryFind(fun ch -> ch.Name=nm) with
                    | Some(child)   -> (nm, MapAsn1Value r child.Type.Kind chv)
                    | _             -> raise(SemanticError(nm.Location, sprintf "Unknown component name '%s'" nm.Value))
                Ast.SeqValue(vals |> List.map mapChildVal)
            | _                 -> raise(SemanticError(v.Location, "Expecting a SEQUENCE value"))
        |ParameterizedAsn1Ast.ChValue(n,v)          -> 
            match actKind with
            |ParameterizedAsn1Ast.Choice(children) ->
                match children |> Seq.tryFind(fun x -> x.Name=n) with
                | Some(child)   -> Ast.ChValue(n, MapAsn1Value r child.Type.Kind v)
                | None          -> raise(SemanticError(n.Location, sprintf "Unknown alternative name '%s'" n.Value))
            | _                 -> raise(SemanticError(v.Location, "Expecting a CHOICE value"))
            
        |ParameterizedAsn1Ast.NullValue             -> Ast.NullValue
        |ParameterizedAsn1Ast.EmptyList             ->
            match actKind with
            |ParameterizedAsn1Ast.Sequence(_)       -> Ast.SeqValue []
            |ParameterizedAsn1Ast.SequenceOf(_)     -> Ast.SeqOfValue []
            | _                                     -> raise(SemanticError(v.Location, "Unexpected value"))
    {
        Ast.Asn1Value.Kind = MapAsn1ValueKind r kind v.Kind
        Location = v.Location
    }




and MapAsn1Optionality (r:ParameterizedAsn1Ast.AstRoot) (kind: ParameterizedAsn1Ast.Asn1TypeKind) (o:ParameterizedAsn1Ast.Asn1Optionality) :Ast.Asn1Optionality =
    match o with
    | ParameterizedAsn1Ast.AlwaysAbsent     -> Ast.AlwaysAbsent
    | ParameterizedAsn1Ast.AlwaysPresent    -> Ast.AlwaysPresent
    | ParameterizedAsn1Ast.Optional         -> Ast.Optional
    | ParameterizedAsn1Ast.Default(v)       -> Ast.Default(MapAsn1Value r kind  v)

and MapChildInfo (r:ParameterizedAsn1Ast.AstRoot)  (c:ParameterizedAsn1Ast.ChildInfo) :Ast.ChildInfo =
    {
        Ast.ChildInfo.Name = c.Name
        ada_name = ToC2 c.Name.Value
        c_name = ToC2 c.Name.Value
        present_when_name = ToC2 c.Name.Value
        Type = MapAsn1Type r c.Type
        Optionality = match c.Optionality with
                      |None -> None
                      |Some(x) -> Some (MapAsn1Optionality r c.Type.Kind x)
        AcnInsertedField = c.AcnInsertedField
        Comments = c.Comments
    }

and MapNamedItem (r:ParameterizedAsn1Ast.AstRoot)  (n:ParameterizedAsn1Ast.NamedItem) :Ast.NamedItem =
    {
        Ast.NamedItem.Name = n.Name
        c_name = n.Name.Value
        ada_name = n.Name.Value
        _value = match n._value with
                 | None -> None
                 | Some(x)  -> Some (MapAsn1Value r ParameterizedAsn1Ast.Integer x)
        Comments = n.Comments
    }



and MapNamedConstraint (r:ParameterizedAsn1Ast.AstRoot) (kind: ParameterizedAsn1Ast.Asn1TypeKind) (x:ParameterizedAsn1Ast.NamedConstraint) :Ast.NamedConstraint =
    let childKind = match getActualKind r kind with
                    | ParameterizedAsn1Ast.Choice(children)     ->
                        match children |> Seq.tryFind(fun ch -> ch.Name = x.Name) with
                        | None      -> raise(SemanticError(x.Name.Location, sprintf "Unknown component name '%s'" x.Name.Value))
                        | Some(child)   -> child.Type.Kind

                    | ParameterizedAsn1Ast.Sequence(children)   ->
                        let children = getSequenceChildren r children
                        match children |> Seq.tryFind(fun ch -> ch.Name = x.Name) with
                        | None      -> raise(SemanticError(x.Name.Location, sprintf "Unknown component name '%s'" x.Name.Value))
                        | Some(child)   -> child.Type.Kind
                    | _                                         -> raise(SemanticError(x.Name.Location, sprintf "Unexpected constraint type" ))
    {
        Ast.NamedConstraint.Name = x.Name; 
        Mark = match x.Mark with
                | ParameterizedAsn1Ast.NoMark        -> Ast.NoMark
                | ParameterizedAsn1Ast.MarkPresent   -> Ast.MarkPresent
                | ParameterizedAsn1Ast.MarkAbsent    -> Ast.MarkAbsent
                | ParameterizedAsn1Ast.MarkOptional  -> Ast.MarkOptional
        Contraint = match x.Contraint with
                    | None  -> None
                    | Some(cc)  -> Some (MapAsn1Constraint r childKind cc)
    }

and MapAsn1Constraint (r:ParameterizedAsn1Ast.AstRoot) (kind: ParameterizedAsn1Ast.Asn1TypeKind) (c:ParameterizedAsn1Ast.Asn1Constraint) :Ast.Asn1Constraint =
    match c with
    | ParameterizedAsn1Ast.SingleValueContraint(v)          -> Ast.SingleValueContraint (MapAsn1Value r kind v)
    | ParameterizedAsn1Ast.RangeContraint(v1,v2,b1,b2)      -> Ast.RangeContraint(MapAsn1Value r kind v1,MapAsn1Value r kind v2,b1,b2)
    | ParameterizedAsn1Ast.RangeContraint_val_MAX(v,b)        -> Ast.RangeContraint_val_MAX (MapAsn1Value r kind v, b)
    | ParameterizedAsn1Ast.RangeContraint_MIN_val(v,b)        -> Ast.RangeContraint_MIN_val (MapAsn1Value r kind v, b)
    | ParameterizedAsn1Ast.TypeInclusionConstraint(s1,s2)   -> Ast.TypeInclusionConstraint(s1,s2)
    | ParameterizedAsn1Ast.SizeContraint(c)                 -> Ast.SizeContraint(MapAsn1Constraint r ParameterizedAsn1Ast.Integer c)
    | ParameterizedAsn1Ast.AlphabetContraint(c)             -> Ast.AlphabetContraint(MapAsn1Constraint r ParameterizedAsn1Ast.IA5String c)
    | ParameterizedAsn1Ast.UnionConstraint(c1,c2, b)           -> Ast.UnionConstraint(MapAsn1Constraint r kind c1, MapAsn1Constraint r kind c2, b)
    | ParameterizedAsn1Ast.IntersectionConstraint(c1,c2)    -> Ast.IntersectionConstraint(MapAsn1Constraint r kind c1, MapAsn1Constraint r kind c2)
    | ParameterizedAsn1Ast.AllExceptConstraint(c)           -> Ast.AllExceptConstraint(MapAsn1Constraint r kind c)
    | ParameterizedAsn1Ast.ExceptConstraint(c1,c2)          -> Ast.ExceptConstraint(MapAsn1Constraint r kind c1, MapAsn1Constraint r kind c2)
    | ParameterizedAsn1Ast.RootConstraint(c1)               -> Ast.RootConstraint(MapAsn1Constraint r kind c1)
    | ParameterizedAsn1Ast.RootConstraint2(c1,c2)           -> Ast.RootConstraint2(MapAsn1Constraint r kind c1, MapAsn1Constraint r kind c2)
    | ParameterizedAsn1Ast.WithComponentConstraint(c)       -> 
        match getActualKind r kind with
        | ParameterizedAsn1Ast.SequenceOf(child)    ->        Ast.WithComponentConstraint(MapAsn1Constraint r child.Kind c)
        | _                                         ->        raise(SemanticError(emptyLocation,"Unexpected constraint type"))
    | ParameterizedAsn1Ast.WithComponentsConstraint(ncs)    -> 
        Ast.WithComponentsConstraint(ncs|> List.map (MapNamedConstraint r kind))


and MapAsn1Type (r:ParameterizedAsn1Ast.AstRoot) (t:ParameterizedAsn1Ast.Asn1Type) :Ast.Asn1Type =
    let aux kind : Ast.Asn1Type=
        {
            Ast.Asn1Type.Kind = kind
            Constraints = t.Constraints |> List.map (MapAsn1Constraint r t.Kind)
            AcnProperties = []
            Location = t.Location
        }        
    match t.Kind with
    | ParameterizedAsn1Ast.Integer          -> aux Ast.Integer
    | ParameterizedAsn1Ast.Real             -> aux Ast.Real
    | ParameterizedAsn1Ast.IA5String        -> aux Ast.IA5String
    | ParameterizedAsn1Ast.NumericString    -> aux Ast.NumericString
    | ParameterizedAsn1Ast.OctetString      -> aux Ast.OctetString
    | ParameterizedAsn1Ast.NullType         -> aux Ast.NullType
    | ParameterizedAsn1Ast.BitString        -> aux Ast.BitString
    | ParameterizedAsn1Ast.Boolean          -> aux Ast.Boolean
    | ParameterizedAsn1Ast.Enumerated(items)-> aux (Ast.Enumerated(items |> List.map (MapNamedItem r)))
    | ParameterizedAsn1Ast.SequenceOf(child)-> aux (Ast.SequenceOf(MapAsn1Type r child))
    | ParameterizedAsn1Ast.Sequence(children)   -> 
        let children = getSequenceChildren r children
        aux (Ast.Sequence(children |> List.map (MapChildInfo r) ))
    | ParameterizedAsn1Ast.Choice(children)     -> aux (Ast.Choice(children |> List.map (MapChildInfo r) ))
    | ParameterizedAsn1Ast.ReferenceType(mdName,ts, args)   ->
        match args with
        | []    ->  aux (Ast.ReferenceType(mdName, ts, false))
        | _     ->  raise(BugErrorException "")

    

let MapTypeAssignment (r:ParameterizedAsn1Ast.AstRoot) (tas:ParameterizedAsn1Ast.TypeAssignment) :Ast.TypeAssignment =
    {
        Ast.TypeAssignment.Name = tas.Name
        Type = MapAsn1Type r tas.Type
        c_name = ToC2 tas.Name.Value
        ada_name = ToC2 tas.Name.Value
        Comments = tas.Comments
    }

let MapValueAssignment (r:ParameterizedAsn1Ast.AstRoot) (vas:ParameterizedAsn1Ast.ValueAssignment) :Ast.ValueAssignment =
    
    {
        Ast.ValueAssignment.Name = vas.Name
        Type = MapAsn1Type r vas.Type
        Value = MapAsn1Value r  vas.Type.Kind vas.Value
        Scope = 
            match vas.Scope with
            | ParameterizedAsn1Ast.GlobalScope      ->  Ast.GlobalScope
            | ParameterizedAsn1Ast.TypeScope(m,t)   ->  Ast.TypeScope(m,t)
        c_name = vas.c_name
        ada_name = vas.ada_name
    }


let MapModule (r:ParameterizedAsn1Ast.AstRoot) (m:ParameterizedAsn1Ast.Asn1Module) :Ast.Asn1Module =
    let DoImportedModule (x:ParameterizedAsn1Ast.ImportedModule) : Ast.ImportedModule option =
        let types = x.Types |> List.choose(fun ts -> 
                                            let tas = getModuleByName r x.Name |> getTasByName ts
                                            match tas.Parameters with
                                            | []    -> Some ts
                                            | _     -> None     //Paramterized Import, so remove it
                                       )
        match types with
        | []    -> None
        | _     -> Some  { Ast.ImportedModule.Name = x.Name; Types = types; Values = x.Values}
    {
        Ast.Asn1Module.Name = m.Name
        TypeAssignments = m.TypeAssignments |> List.filter(fun x -> x.Parameters.Length = 0) |> List.map (MapTypeAssignment r)
        ValueAssignments = m.ValueAssignments |> List.map (MapValueAssignment r)
        Imports = m.Imports |> List.choose DoImportedModule
        Exports  = match m.Exports with
                   | ParameterizedAsn1Ast.All               -> Ast.All
                   | ParameterizedAsn1Ast.OnlySome(lst)     -> Ast.OnlySome(lst)
        Comments = m.Comments
    }

let MapFile (r:ParameterizedAsn1Ast.AstRoot) (f:ParameterizedAsn1Ast.Asn1File) :Ast.Asn1File =
    {
        Ast.Asn1File.FileName = f.FileName
        Tokens = f.Tokens
        Modules = f.Modules |> List.map (MapModule r)
    }



let DoWork (r:ParameterizedAsn1Ast.AstRoot) : Ast.AstRoot =
    let r = RemoveParamterizedTypes.DoWork r
    {
        Ast.AstRoot.Files = r.Files |> List.map (MapFile r)
        Encodings = r.Encodings 
        GenerateEqualFunctions = r.GenerateEqualFunctions
        TypePrefix = r.TypePrefix
        AstXmlAbsFileName = r.AstXmlAbsFileName
        IcdUperHtmlFileName = r.IcdUperHtmlFileName
        IcdAcnHtmlFileName = r.IcdAcnHtmlFileName
        CheckWithOss = r.CheckWithOss
        mappingFunctionsModule = r.mappingFunctionsModule
        integerSizeInBytes = r.integerSizeInBytes
    }
    