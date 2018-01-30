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

module CloneTree
open Asn1Ast



let rec foldMap func state lst =
    match lst with
    | []        -> [],state
    | h::tail   -> 
        let procItem, newState = func state h
        let restList, finalState = tail |> foldMap func newState
        procItem::restList, finalState



type Constructors<'state> = {
    createFile : AstRoot-> Asn1File -> Constructors<'state> -> 'state -> Asn1File*'state
    createModule : AstRoot-> Asn1Module -> Constructors<'state> -> 'state  -> Asn1Module*'state
    cloneTypeAssignment : TypeAssignment -> Asn1Module -> Constructors<'state> -> 'state -> TypeAssignment*'state
    cloneValueAssigment : ValueAssignment -> Asn1Module -> Constructors<'state> -> 'state -> ValueAssignment*'state
    cloneType: Asn1Type -> Asn1Module -> list<string> -> Constructors<'state> -> 'state  -> Asn1Type*'state
}

let rec CloneTree (old:AstRoot) cons state =
    let newFiles, newState = old.Files |> foldMap (fun s f -> cons.createFile old f cons s) state
    { old with  Files = newFiles}, newState

and CloneAsn1File (old:AstRoot) (f:Asn1File) cons state =
    let newMods, newState = f.Modules |> foldMap (fun s m-> cons.createModule old m cons s) state
    { f with Modules = newMods}, newState

let CloneModule (oldRoot:AstRoot) (old:Asn1Module) cons state  = 
    let newTas, s0 = old.TypeAssignments |> foldMap (fun s t -> cons.cloneTypeAssignment t old cons s) state
    let newVas, s1 = old.ValueAssignments |> foldMap (fun s v -> cons.cloneValueAssigment v old cons s) s0
    {
        Asn1Module.Name = old.Name;
        TypeAssignments  = newTas
        ValueAssignments = newVas
        Imports = old.Imports 
        Exports = old.Exports
        Comments = old.Comments
    }, s1


let CloneTypeAssigment (old:TypeAssignment) (m:Asn1Module) cons state =
    let newType,s = cons.cloneType old.Type m [m.Name.Value; old.Name.Value] cons state
    {
        TypeAssignment.Name = old.Name
        Type = newType
        c_name = old.c_name
        ada_name = old.ada_name
        Comments = old.Comments
    },s

let CloneValueAssignment (old:ValueAssignment)  (m:Asn1Module) cons state=
    let newType,s = cons.cloneType old.Type m [m.Name.Value; old.Name.Value] cons state
    {
        ValueAssignment.Name = old.Name
        Type = newType
        Value = old.Value 
        c_name = old.c_name
        ada_name = old.ada_name
    },s

let CloneType (old:Asn1Type) m key cons state =
    let CloneChild s (ch:ChildInfo) =
        let t,ns = cons.cloneType ch.Type m (key@[ch.Name.Value]) cons s
        {ch with Type = t},ns
    let newKind, newState = 
        match old.Kind with
        | Sequence(children)  -> 
            let newChildren, finalState = children |> foldMap CloneChild state
            Sequence(newChildren), finalState
        | Choice(children)    -> 
            let newChildren, finalState = children |> foldMap CloneChild state
            Choice(newChildren), finalState
        | SequenceOf(child)   -> 
            let nch,ns = cons.cloneType child m (key@["#"]) cons state
            SequenceOf(nch),ns
        | _                   -> old.Kind, state
        
    {
        Kind = newKind
        Constraints = old.Constraints
        Location = old.Location
        parameterizedTypeInstance = old.parameterizedTypeInstance
    }, newState

    
let defaultConstructors = {
    createFile = CloneAsn1File
    createModule = CloneModule
    cloneTypeAssignment = CloneTypeAssigment
    cloneValueAssigment = CloneValueAssignment
    cloneType = CloneType
}


let DoWork old = fst (CloneTree old defaultConstructors [])



//let CopyTree old = CloneTree old  defaultConstructors
