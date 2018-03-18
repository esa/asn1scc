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

module EnsureUniqueEnumNames

open FsUtils
open Asn1Ast
open System
open CloneTree
open CommonTypes


type private State =  string list


let rec private handleEnumChoices (r:AstRoot) (renamePolicy:EnumRenamePolicy)=
    let doubleEnumNames = seq {
        for m in r.Modules do
            for tas in m.TypeAssignments do
                for t in GetMySelfAndChildren tas.Type  do
                    match t.Kind with
                    | Choice(children)  -> 
                        let names = children |> List.map(fun x -> x.present_when_name)
                        yield! names
                    | _                 -> () } |> Seq.toList |> List.keepDuplicatesI

    match doubleEnumNames with
    | []    -> r
    | _     ->
        let CloneType (old:Asn1Type) m (key:list<string>) (cons:Constructors<State>) (state:State) =
            let CloneChild s (ch:ChildInfo) =
                let t,ns = cons.cloneType ch.Type m (key@[ch.Name.Value]) cons s
                let newUniqueName =
                    match state |> Seq.exists (ch.present_when_name.icompare) with
                    | false     -> ch.present_when_name
                    | true      ->
                        let newPrefix = key |> List.rev |> List.map ToC |> Seq.skipWhile(fun x -> ch.present_when_name.Contains x) |> Seq.head
                        newPrefix + "_" + ch.present_when_name
            
                {ch with Type = t; present_when_name = ToC2 newUniqueName},ns

            match old.Kind with
            | Choice(children)    -> 
                //let newChildren, finalState = children |> foldMap CloneChild state
                let newChildren, finalState =
                    match renamePolicy with
                    | NoRenamePolicy           -> children, state
                    | SelectiveEnumerants      -> children |> foldMap CloneChild state
                    | AllEnumerants            -> 
                        let newChildren, finalState = children |> foldMap CloneChild state
                        let newPrefix = newChildren |> List.map(fun itm -> itm.present_when_name.Replace(ToC2 itm.Name.Value,"")) |> List.maxBy(fun prf -> prf.Length)
                        let newChildren = 
                                children |> List.map(fun ch -> {ch with present_when_name = newPrefix + ch.present_when_name})
                            
                        newChildren, finalState
                {old with Kind =  Choice(newChildren)}, finalState
            | _             -> defaultConstructors.cloneType old m key cons state

        let newTree = CloneTree r {defaultConstructors with cloneType =  CloneType; } doubleEnumNames |> fst
        handleEnumChoices newTree  renamePolicy


let rec private handleSequencesAndChoices (r:AstRoot) (lang:ProgrammingLanguage) (renamePolicy:EnumRenamePolicy)=
    let doubleEnumNames = seq {
        for m in r.Modules do
            for tas in m.TypeAssignments do
                for t in GetMySelfAndChildren tas.Type  do
                    match t.Kind with
                    | Sequence(children)   
                    | Choice(children)  -> 
                        let names = 
                            children |> 
                            List.filter(fun x -> lang.keywords |> Seq.exists(lang.cmp (x.CName lang) )) |>
                            List.map(fun x -> x.CName lang)
                        yield! names
                    | _                 -> () } |> Seq.toList 


    match doubleEnumNames with
    | []    -> r
    | _     ->
        let CloneType (old:Asn1Type) m (key:list<string>) (cons:Constructors<State>) (state:State) =
            let CloneChild s (ch:ChildInfo) =
                let t,ns = cons.cloneType ch.Type m (key@[ch.Name.Value]) cons s
                let newUniqueName =
                    match state |> Seq.exists (lang.cmp (ch.CName lang)) with
                    | false     -> ch.CName lang
                    | true      ->
                        let newPrefix = key |> List.rev |> List.map ToC |> Seq.skipWhile(fun x -> (ch.CName lang).Contains x) |> Seq.head
                        newPrefix + "_" + (ch.CName lang)
            
                match lang with
                | ProgrammingLanguage.C-> {ch with Type = t; c_name = ToC2 newUniqueName},ns
                | ProgrammingLanguage.Ada | ProgrammingLanguage.Spark   -> {ch with Type = t; ada_name = ToC2 newUniqueName},ns
                | _                                                     -> ch, state

            match old.Kind with
            | Choice(children)    
            | Sequence(children)    -> 
                let newChildren, finalState =
                    match renamePolicy with
                    | NoRenamePolicy           -> children, state
                    | SelectiveEnumerants      -> children |> foldMap CloneChild state
                    | AllEnumerants            -> 
                        let newChildren, finalState = children |> foldMap CloneChild state
                        let newPrefix = newChildren |> List.map(fun itm -> (itm.CName lang).Replace(ToC2 itm.Name.Value,"")) |> List.maxBy(fun prf -> prf.Length)
                        let newChildren = 
                            match lang with
                            | ProgrammingLanguage.C-> 
                                children |> List.map(fun ch -> {ch with c_name = newPrefix + ch.c_name})
                            | ProgrammingLanguage.Ada | ProgrammingLanguage.Spark   -> 
                                children |> List.map(fun ch -> {ch with ada_name = newPrefix + ch.ada_name})
                            | _                                                     -> raise(BugErrorException "handleSequences")
                            
                        newChildren, finalState
                match old.Kind with
                | Choice(children)    -> {old with Kind =  Choice(newChildren)}, finalState
                | Sequence(children)  -> {old with Kind =  Sequence(newChildren)}, finalState
                | _                   -> raise (BugErrorException "impossible case in handleSequencesAndChoices")
            | _             -> defaultConstructors.cloneType old m key cons state

        let newTree = CloneTree r {defaultConstructors with cloneType =  CloneType; } doubleEnumNames |> fst
        handleSequencesAndChoices newTree lang renamePolicy



let rec private handleEnums (r:AstRoot) (renamePolicy:EnumRenamePolicy) (lang:ProgrammingLanguage) =
    let doubleEnumNames0 = 
        seq {
            for m in r.Modules do
                for tas in m.TypeAssignments do
                    for t in GetMySelfAndChildren tas.Type  do
                        match t.Kind with
                        | Enumerated(itesm) -> 
                            let names = itesm |> List.map(fun x -> x.EnumName lang)
                            yield! names
                        | _                 -> () 
                for vas in m.ValueAssignments do
                    yield vas.Name.Value 
        } |> Seq.toList 
    let doubleEnumNames = 
        match lang with
        | ProgrammingLanguage.C     -> doubleEnumNames0 @ CheckAsn1.c_keywords|> List.keepDuplicates
        | ProgrammingLanguage.Ada   
        | ProgrammingLanguage.Spark -> doubleEnumNames0 @ CheckAsn1.ada_keywords |> List.keepDuplicatesI
        | _                         -> doubleEnumNames0


    match doubleEnumNames with
    | []    -> r
    | _     ->
        let CloneType (old:Asn1Type) m (key:list<string>) (cons:Constructors<State>) (state:State) =
            match old.Kind with
            | Enumerated(itesm)    -> 
                let copyItem (old:NamedItem) =
                    let newUniqueName =
                        match state |> Seq.exists (lang.cmp (old.EnumName  lang)) with
                        | false     -> old.EnumName lang
                        | true      ->
                            let newPrefix = key |> List.rev |> List.map ToC |> Seq.skipWhile(fun x -> (old.EnumName lang).Contains x) |> Seq.head
                            newPrefix + "_" + (old.EnumName lang)
                    match lang with
                    | ProgrammingLanguage.C     ->      {old with c_name=newUniqueName}
                    | ProgrammingLanguage.Ada | ProgrammingLanguage.Spark   -> {old with ada_name=newUniqueName}
                    | _                                                     -> raise(BugErrorException "handleEnums")
                let newItems = 
                    match renamePolicy with
                    | NoRenamePolicy           -> itesm
                    | SelectiveEnumerants      -> itesm|> List.map copyItem
                    | AllEnumerants            -> 
                        let newPrefix = itesm|> List.map copyItem |> List.map(fun itm -> (itm.EnumName lang).Replace(ToC2 itm.Name.Value,"")) |> List.maxBy(fun prf -> prf.Length)
                        match lang with
                        | ProgrammingLanguage.C->  itesm|> List.map (fun itm -> {itm with c_name = newPrefix + itm.c_name})
                        | ProgrammingLanguage.Ada | ProgrammingLanguage.Spark   -> 
                            itesm|> List.map (fun itm -> {itm with ada_name = newPrefix + itm.ada_name})
                        | _ -> raise(BugErrorException "handleEnums")

                {old with Kind =  Enumerated(newItems)}, state
            | _             -> defaultConstructors.cloneType old m key cons state

        let newTree = CloneTree r {defaultConstructors with cloneType =  CloneType; } doubleEnumNames |> fst
        handleEnums newTree renamePolicy lang


let DoWork (ast:AstRoot)   =
    let renamePolicy = ast.args.renamePolicy
    match renamePolicy with
    | NoRenamePolicy           -> ast
    | _                                             ->
        let r1 = handleEnumChoices ast  renamePolicy
        let r2_c = handleEnums r1 renamePolicy ProgrammingLanguage.C
        let r2_ada = handleEnums r2_c renamePolicy ProgrammingLanguage.Ada
        let r3_c = handleSequencesAndChoices r2_ada ProgrammingLanguage.C renamePolicy
        let r3_ada = handleSequencesAndChoices r3_c ProgrammingLanguage.Ada renamePolicy

        r3_ada


