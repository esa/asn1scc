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

module RemoveParamterizedTypes

open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime

open FsUtils
open ParameterizedAsn1Ast


//let rec foldMap func state lst =
//    match lst with
//    | []        -> [],state
//    | h::tail   -> 
//        let procItem, newState = func state h
//        let restList, finalState = tail |> foldMap func newState
//        procItem::restList, finalState
let foldMap func state lst =
    let rec loop acc func state lst =
        match lst with
        | []        -> acc |> List.rev , state
        | h::tail   -> 
            let procItem, newState = func state h
            //let restList, finalState = tail |> loop func newState
            //procItem::restList, finalState
            loop (procItem::acc) func newState tail
    loop [] func state lst


let rec CloneType  (r:AstRoot)  (curModule:Asn1Module) (oldModName:string) (namedArgs:list<StringLoc*TemplateArgument>) (old:Asn1Type) (implicitImports : List<string*string>) : (Asn1Type * List<string*string>) =
    let CloneChild (implicitImports : List<string*string>) (ch:ChildInfo)  =
        let newType, newImports = CloneType r curModule oldModName namedArgs ch.Type implicitImports
        let newChildInfo = 
            {ch with Type = newType; Optionality = match ch.Optionality with
                                                                     | Some(Default(v))   -> Some(Default (CloneValue r curModule oldModName namedArgs (Some newType) v))
                                                                     | _                  -> ch.Optionality}
        newChildInfo, newImports


    let cloneSeqChild  (implicitImports : List<string*string>) (seqChild:SequenceChild) =
        match seqChild with
        | ComponentsOf (x,y) -> ComponentsOf (x,y), []
        | ChildInfo ch   -> 
            let newChildInfo, newImports = CloneChild implicitImports ch 
            ChildInfo newChildInfo, newImports

    let newKind, newCons, newImports = 
        let cons = old.Constraints |> List.map (CloneConstraint r curModule oldModName namedArgs (Some old))
        match old.Kind with
        | Sequence(children)  ->   
            let newChildren, newImports = children |> foldMap (fun newState ch -> cloneSeqChild newState ch) implicitImports
            Sequence(newChildren), cons, newImports
        | Choice(children)    ->   
            let newChildren, newImports = children |> foldMap (fun newState ch -> CloneChild newState ch) implicitImports
            Choice(newChildren), cons, newImports
        | SequenceOf(child)   ->   
            let newType, newImports = CloneType r curModule oldModName  namedArgs child implicitImports
            SequenceOf newType, cons, newImports
        | ReferenceType(md,ts,refEnc, args)    ->
            match args with
            | []        -> 
                match namedArgs |> Seq.tryFind (fun (nm, _) -> nm = ts) with
                | Some(_,arg)       -> 
                    match arg with
                    | ArgType(x)     -> x.Kind, (x.Constraints|> List.map (CloneConstraint r curModule oldModName namedArgs (Some x)))@cons, []
                    | _              -> raise(BugErrorException "")
                | None              -> 
                    let newImports =
                        match md.Value <> curModule.Name.Value with
                        | true  -> [(md.Value, ts.Value)]
                        | false -> []
                    old.Kind, cons, newImports

            | _         -> 
                let specRefType, newImports2  = SpecializeRefType r curModule md ts args namedArgs implicitImports
                specRefType.Kind, (specRefType.Constraints|> List.map (CloneConstraint r curModule oldModName namedArgs (Some specRefType)))@cons, newImports2

        | _                                        ->   old.Kind,cons, []

    let retType =         
        {
            Asn1Type.Kind = newKind
            Constraints = newCons
            Location = old.Location
            parameterizedTypeInstance = false
            acnInfo = old.acnInfo
            unitsOfMeasure = old.unitsOfMeasure
            moduleName = old.moduleName
        }
    retType, (implicitImports@newImports |> Seq.distinct |> Seq.toList)

and CloneConstraint (r:AstRoot) (curModule:Asn1Module) (oldModName:string) (namedArgs:list<StringLoc*TemplateArgument>) (t:Asn1Type option) (c:Asn1Constraint) :Asn1Constraint =
    match c with
    | SingleValueContraint(s, v)          -> SingleValueContraint(s, (CloneValue  r curModule oldModName namedArgs t v))
    | RangeContraint(s, v1,v2,b1,b2)            -> RangeContraint(s, CloneValue  r curModule oldModName namedArgs t v1,CloneValue r curModule oldModName namedArgs t v2,b1,b2)
    | RangeContraint_val_MAX(s,v,b)        -> RangeContraint_val_MAX (s, CloneValue r curModule oldModName namedArgs t v,b)
    | RangeContraint_MIN_val(s, v,b)        -> RangeContraint_MIN_val (s, CloneValue r curModule oldModName namedArgs t v,b)
    | TypeInclusionConstraint(s, s1,s2)   -> TypeInclusionConstraint(s, s1,s2)
    | SizeContraint(s, c)                 -> SizeContraint(s, CloneConstraint r curModule oldModName namedArgs None c)
    | AlphabetContraint(s, c)             -> AlphabetContraint(s, CloneConstraint r curModule oldModName namedArgs None c)
    | UnionConstraint(s, c1,c2,b)           -> UnionConstraint(s, CloneConstraint r curModule oldModName namedArgs t c1, CloneConstraint r curModule oldModName namedArgs t c2, b)
    | IntersectionConstraint(s, c1,c2)    -> IntersectionConstraint(s, CloneConstraint r curModule oldModName namedArgs t c1, CloneConstraint r curModule oldModName namedArgs t c2)
    | AllExceptConstraint(s, c)           -> AllExceptConstraint(s, CloneConstraint r curModule oldModName namedArgs t c)
    | ExceptConstraint(s, c1,c2)          -> ExceptConstraint(s, CloneConstraint r curModule oldModName namedArgs t c1, CloneConstraint r curModule oldModName namedArgs t c2)
    | RootConstraint(s, c1)               -> RootConstraint(s, CloneConstraint r curModule oldModName namedArgs t c)
    | RootConstraint2(s, c1,c2)           -> RootConstraint2(s, CloneConstraint r curModule oldModName namedArgs t c1, CloneConstraint r curModule oldModName namedArgs t c2)
    | WithComponentConstraint(s, c,l)     -> WithComponentConstraint(s, (CloneConstraint r curModule oldModName namedArgs None c),l)
    | WithComponentsConstraint(s, ncs)    -> WithComponentsConstraint(s, ncs|> List.map (CloneNamedConstraint r curModule oldModName namedArgs))

and CloneNamedConstraint (r:AstRoot) (curModule:Asn1Module) (oldModName:string) (namedArgs:list<StringLoc*TemplateArgument>) (x:NamedConstraint) :NamedConstraint =
    {
        NamedConstraint.Name = x.Name; 
        Mark = x.Mark 
        Contraint = match x.Contraint with
                    | None  -> None
                    | Some(cc)  -> Some (CloneConstraint r curModule oldModName namedArgs None cc)
    }
    
and CloneValue  (r:AstRoot) (curModule:Asn1Module) (oldModName:string) (namedArgs:list<StringLoc*TemplateArgument>) (t:Asn1Type option) (v:Asn1Value) :Asn1Value =
    match v.Kind with
    |RefValue(v1,v2)       -> 
        match namedArgs |> Seq.tryFind (fun (nm, _) -> nm = v2) with
        | Some(_,arg)       -> 
            match arg with
            | ArgValue(vl)     -> vl
            | _                                     -> raise(BugErrorException "")
        | None              -> 
            match t with
            | Some tt    -> 
                let actType = GetActualType tt r
                match actType.Kind with
                | Enumerated enmItems  when oldModName = v1.Value -> 
                    match enmItems |> Seq.tryFind (fun enmItem -> enmItem.Name.Value = v2.Value) with
                    | None          -> v
                    | Some _        -> {v with Kind = RefValue(StringLoc.ByValue curModule.Name.Value,v2)}
                | _                     -> v
            | None      -> v
    |SeqOfValue(vals)      -> {v with Kind = SeqOfValue(vals |> List.map (CloneValue r curModule oldModName namedArgs None))}
    |SeqValue(vals)        -> {v with Kind = SeqValue(vals |> List.map (fun (n,v) -> (n, CloneValue r curModule oldModName namedArgs None v )))}
    |ChValue(n,v)          -> {v with Kind = ChValue(n, CloneValue r curModule oldModName namedArgs None v )}
    | _                                         -> v

and SpecializeType (r:AstRoot) (curModule:Asn1Module) (implicitImports : List<string*string>) (t:Asn1Type) : (Asn1Type * List<string*string>) =
    let  mergeAcnInfo (refTypeSpec:AcnGenericTypes.AcnTypeEncodingSpec option) (genTypeSpec:AcnGenericTypes.AcnTypeEncodingSpec option) =
        let rec mergeAcnInfo_aux (refTypeSpec:AcnGenericTypes.AcnTypeEncodingSpec ) (genTypeSpec:AcnGenericTypes.AcnTypeEncodingSpec) =
            let allChildNames = 
                //if ref refTypeSpec children is a subset of genTypeSpec return genTypeSpec children
                //if genTypeSpec children is a subset of  of refTypeSpec children  return refTypeSpec children
                //else emit user error. User must provide child spec in all children
                let refTypeChildrenNames = refTypeSpec.children |> List.map(fun z -> z.name.Value) 
                let getTypeChildrenNames = genTypeSpec.children |> List.map(fun z -> z.name.Value)
                let refTypeSet = Set.ofList refTypeChildrenNames
                let genTypeSet = Set.ofList getTypeChildrenNames
                match refTypeSet.IsSubsetOf genTypeSet with
                | true  -> getTypeChildrenNames
                | false -> refTypeChildrenNames

                //(refTypeChildrenNames@getTypeChildrenNames) |> List.distinct
            let combinedChildren =
                allChildNames |> 
                List.map(fun name ->
                    let refTypeChild = refTypeSpec.children |> Seq.tryFind (fun c -> c.name.Value = name)
                    let genTypeChild = genTypeSpec.children |> Seq.tryFind (fun c -> c.name.Value = name)
                    match refTypeChild, genTypeChild with
                    | None, None        -> raise(BugErrorException(sprintf "SpecializeType no child with name :%s found" name))
                    | None, Some a      -> a
                    | Some a, None      -> a //reference type wins over generalized type
                    | Some refChild, Some genChild         -> 
                        let childEncodingSpec = mergeAcnInfo_aux refChild.childEncodingSpec genChild.childEncodingSpec
                        {refChild with childEncodingSpec = childEncodingSpec})
            //reference type properties wins over generalized type (put at before genType properties)
            {refTypeSpec with acnProperties = refTypeSpec.acnProperties@genTypeSpec.acnProperties; children = combinedChildren}

        match refTypeSpec, genTypeSpec with
        | None, None    -> None
        | Some a1, None -> Some a1
        | None, Some a2 -> Some a2
        | Some refTypeSpec, Some genTypeSpec  -> Some (mergeAcnInfo_aux refTypeSpec genTypeSpec)
    match t.Kind with
    | ReferenceType(md,ts, refEnc, args)   when args.Length>0 -> 
        let (newType, implImps) = SpecializeRefType r curModule md ts args [] implicitImports
        ({newType with Constraints = newType.Constraints@t.Constraints; acnInfo=mergeAcnInfo t.acnInfo newType.acnInfo}, implImps)
    | ReferenceType(md,ts, refEnc, args)    -> 
        let parmTas = getModuleByName r md |> getTasByName ts
        match parmTas.Parameters with
        | []    -> t, implicitImports
        | _     -> raise(SemanticError(t.Location, "Missing template arguments"))
    | _         -> t, implicitImports

and SpecializeRefType (r:AstRoot) (curModule:Asn1Module) (mdName:StringLoc) (tsName:StringLoc) (args:list<TemplateArgument>) (resolvedTeplateParams:list<StringLoc*TemplateArgument>) (implicitImports : List<string*string>) : (Asn1Type * List<string*string>) =
    let parmTas = getModuleByName r mdName |> getTasByName tsName
    let SpecializeTemplatizedArgument (implicitImports : List<string*string>) (arg:TemplateArgument) =
        match arg with
        | ArgValue(_)      -> arg, implicitImports
        | ArgType(tp)      -> 
            let newType, newImports = SpecializeType  r curModule implicitImports  tp
            ArgType newType, newImports
        | TemplateParameter prmName   -> 
            match resolvedTeplateParams |> Seq.tryFind (fun (prm,templArg) -> prm.Value = prmName.Value) with
            | Some (prm, templArg)  -> templArg, implicitImports
            | None                  -> 
                raise (SemanticError(tsName.Location, sprintf "Template argument %s cannot be resolved" (prmName.Value) ))

    let args, newImports = args |> foldMap SpecializeTemplatizedArgument implicitImports
    match parmTas.Parameters.Length = args.Length with
    | true  -> ()
    | false -> raise (SemanticError(tsName.Location, sprintf "The number of template arguments do not match the number of parameter in the type assignment"))
    
    let getNameArg (arg:TemplateArgument, prm:TemplateParameter) =
        match prm, arg with
        | TypeParameter(s), ArgType(_)      -> (s, arg)
        | TypeParameter(_), ArgValue(_)     -> raise (SemanticError(tsName.Location, sprintf "Expecting type, not value"))
        | ValueParameter(_,_), ArgType(_)   -> raise (SemanticError(tsName.Location, sprintf "Expecting value, not type"))
        | ValueParameter(t,s), ArgValue(v)  -> (s,arg)
        | _, TemplateParameter(_)   -> raise (SemanticError(tsName.Location, sprintf "Unexpected combination"))
    let namedArgs = List.zip args parmTas.Parameters |> List.map getNameArg

    let newType, implImps = CloneType  r curModule mdName.Value namedArgs parmTas.Type (implicitImports@newImports |> Seq.distinct |> Seq.toList)

    {newType with parameterizedTypeInstance=true}, implImps



and DoAsn1Type (r:AstRoot) (curModule:Asn1Module) (implicitImports : List<string*string>) (t:Asn1Type)  : (Asn1Type * List<string*string>) =
    let DoChildInfo (r:AstRoot) (implicitImports : List<string*string>) (c:ChildInfo) :ChildInfo * List<string*string> =
        let newType, newImports = DoAsn1Type r curModule implicitImports c.Type 
        {
            ChildInfo.Name = c.Name
            Type = newType
            Optionality = c.Optionality 
            AcnInsertedField = c.AcnInsertedField
            Comments = c.Comments
        }, newImports

    let DoSeqChildInof r (implicitImports : List<string*string>) ch : (SequenceChild*List<string*string>)=
        match ch with
        | ChildInfo ch  -> 
            let newType, newImports = DoChildInfo r  implicitImports ch
            ChildInfo newType, newImports
        | ComponentsOf (m,t) -> ComponentsOf (m,t), implicitImports

    let aux kind acnInfo : Asn1Type=
        {
            Asn1Type.Kind = kind
            Constraints = t.Constraints 
            Location = t.Location
            parameterizedTypeInstance = false
            acnInfo = acnInfo
            unitsOfMeasure = t.unitsOfMeasure
            moduleName = t.moduleName
        }        
    match t.Kind with
    | SequenceOf(child) -> 
        let newType, newImports = DoAsn1Type r curModule implicitImports child 
        aux (SequenceOf(newType)) t.acnInfo , newImports
    | Sequence(children)-> 
        let newChildren, newImports = children |>  foldMap (DoSeqChildInof r) implicitImports
        aux (Sequence(newChildren)) t.acnInfo , newImports
    | Choice(children)  -> 
        let newChildren, newImports = children |>  foldMap (DoChildInfo r) implicitImports
        aux (Choice(newChildren)) t.acnInfo , newImports
    | ReferenceType(_)  -> SpecializeType r curModule implicitImports t
    | _                 -> aux t.Kind t.acnInfo , implicitImports

    

let DoTypeAssignment (r:AstRoot) (curModule:Asn1Module) (implicitImports : List<string*string>) (tas:TypeAssignment) : (TypeAssignment*List<string*string>) =
    let newType, newImports = DoAsn1Type r curModule implicitImports tas.Type 
    {
        TypeAssignment.Name = tas.Name
        Type = newType
        Comments = tas.Comments
        Parameters = []
        acnInfo = tas.acnInfo
    }, newImports

let DoValueAssignment (r:AstRoot) (curModule:Asn1Module) (implicitImports : List<string*string>) (vas:ValueAssignment) :(ValueAssignment*List<string*string>) =
    let newType, newImports = DoAsn1Type r curModule implicitImports vas.Type 
    {
        ValueAssignment.Name = vas.Name
        Type = newType
        Value = vas.Value
        Scope = vas.Scope
        c_name = vas.c_name
        scala_name = vas.scala_name
        ada_name = vas.ada_name
    }, newImports


let DoModule (r:AstRoot) (m:Asn1Module) :Asn1Module =
    let DoImportedModule (x:ImportedModule) : ImportedModule option =
        let types = x.Types |> List.choose(fun ts -> 
                                            let tas = getModuleByName r x.Name |> getTasByName ts
                                            match tas.Parameters with
                                            | []    -> Some ts
                                            | _     -> None     //Paramterized Import, so remove it
                                       )
        match types, x.Values with
        | [],[]  -> None
        | _      -> Some  { ImportedModule.Name = x.Name; Types = types; Values = x.Values}
    
    let newTypeAssignments, newImports = m.TypeAssignments |> List.filter(fun x -> x.Parameters.Length = 0) |> foldMap (DoTypeAssignment r m) []
    let newValueAssignments, newImports = m.ValueAssignments |> foldMap (DoValueAssignment r m) newImports
    let addionalImports = newImports |> Seq.distinct |> Seq.toList
    let existingImports = m.Imports |> List.choose DoImportedModule
    let newImports =
        addionalImports |> 
        List.fold (fun (newCurImports:list<ImportedModule>) (impMod, impTas) -> 
            match newCurImports |> Seq.tryFind (fun imp -> imp.Name.Value = impMod) with
            | None      -> ({ImportedModule.Name = StringLoc.ByValue impMod; Types = [StringLoc.ByValue impTas];    Values = []})::newCurImports        
            | Some imp  ->
                match imp.Types |> Seq.tryFind(fun x -> x.Value = impTas) with
                | None  ->
                    let newImp = {imp with Types = (StringLoc.ByValue impTas)::imp.Types}
                    newImp::(newCurImports|>List.filter(fun imp -> imp.Name.Value <> impMod))
                | Some _    -> newCurImports
        )  existingImports
    
    {
        m with
            TypeAssignments = newTypeAssignments
            ValueAssignments = newValueAssignments
            Imports = newImports
    }

let DoFile (r:AstRoot) (f:Asn1File) :Asn1File =
    { f with Modules = f.Modules |> List.map (DoModule r) }




let DoWork (r:AstRoot) : AstRoot =
    { r with Files = r.Files |> List.map (DoFile r)  }