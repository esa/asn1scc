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
open Language


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
                    | _                 -> () } |> Seq.toList |> List.keepDuplicatesCaseInsensitive

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
                        let aaaa = key |> List.rev |> List.map ToC |> Seq.skipWhile(fun x -> ch.present_when_name.Contains x) |> Seq.toList
                        let newPrefix =
                            match aaaa with
                            | z1::_ -> z1
                            | []    ->
                                let errMsg = sprintf "Choice ch.present_when_name = '%s', key=%A, state='%A'" ch.present_when_name key state
                                raise (SemanticError(old.Location,errMsg))
                        newPrefix + "_" + ch.present_when_name

                {ch with Type = t; present_when_name = ToC2 newUniqueName},ns

            match old.Kind with
            | Choice(children)    ->
                //let newChildren, finalState = children |> foldMap CloneChild state
                let newChildren, finalState =
                    match renamePolicy with
                    | AlwaysPrefixTypeName
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


type FieldPrefixReasonToChange =
    | FieldIsKeyword
    | FieldIsAlsoType of string
    | FieldIsAlsoModule of string

type FieldPrefState = {
    curChildName : string
    reasonToChange : FieldPrefixReasonToChange
}
let rec private handleSequencesAndChoices (r:AstRoot) ((lang, lm): ProgrammingLanguage*LanguageMacros) (renamePolicy:FieldPrefix)=
    let cmp (s1:string) (s2:string) =
        match lm.lg.isCaseSensitive with
        | true -> s1 = s2
        | false -> s1.icompare s2
    match renamePolicy with
    | FieldPrefixAuto           ->
        let invalidComponentNames = seq {
            for m in r.Modules do
                for tas in m.TypeAssignments do
                    for t in GetMySelfAndChildren tas.Type  do
                        match t.Kind with
                        | Sequence(children)
                        | Choice(children)  ->
                            let names =
                                children |>
                                List.choose(fun x ->
                                    let isKeyword = lm.lg.Keywords |> Seq.exists(cmp (lm.lg.getChildInfoName x) )
                                    //In ASN.1 a type assignment name starts with a Capital letter while a field name starts with a small letter.
                                    //So, in case sensitive languages, the type assignment name will never conflict with a field name.
                                    //In case insensitive languages, however, the type assignment name may conflict with a field name.
                                    let conflictingTas =
                                        match lm.lg.isCaseSensitive with
                                        | true     -> None
                                        | false   ->
                                            m.TypeAssignments |> Seq.tryFind(fun tas -> cmp (ToC (lm.lg.getChildInfoName x)) (ToC r.args.TypePrefix + tas.Name.Value) )
                                    //if the target language supports modules and is case sensitive, then a field name may conflict with a module name.
                                    let confilectingModuleName =
                                        match lm.lg.hasModules && lm.lg.isCaseSensitive with
                                        | false     -> None
                                        | true   ->
                                            r.Modules |> Seq.tryFind(fun m -> cmp (ToC (lm.lg.getChildInfoName x)) (ToC m.Name.Value) )

                                    match isKeyword , conflictingTas, confilectingModuleName with
                                    | true, _, _       -> Some {curChildName = (lm.lg.getChildInfoName x); reasonToChange = FieldIsKeyword}
                                    | false, (Some tas), _   -> Some {curChildName = (lm.lg.getChildInfoName x); reasonToChange = (FieldIsAlsoType tas.Name.Value)}
                                    | false, None, Some m    -> Some {curChildName = (lm.lg.getChildInfoName x); reasonToChange = (FieldIsAlsoModule m.Name.Value)}
                                    | false, None, None    -> None  )
                                //List.map(fun x -> x.CName lang)
                            yield! names
                        | _                 -> () } |> Seq.toList


        match invalidComponentNames with
        | []    -> r
        | _     ->
            let CloneType (old:Asn1Type) m (key:list<string>) (cons:Constructors<List<FieldPrefState>>) (state:List<FieldPrefState>) =
                let CloneChild s (ch:ChildInfo) =
                    let parentTypeName = key |> Seq.StrJoin "."
                    let t,ns = cons.cloneType ch.Type m (key@[ch.Name.Value]) cons s
                    let newUniqueName =
                        match state |> Seq.tryFind(fun z -> (cmp (lm.lg.getChildInfoName ch) z.curChildName) )with
                        | None     -> lm.lg.getChildInfoName ch
                        | Some fps      ->
                            let newPrefix = key |> List.rev |> List.map ToC |> Seq.skipWhile(fun x -> (lm.lg.getChildInfoName ch).Contains x) |> Seq.head
                            let fieldName = newPrefix + "_" + (lm.lg.getChildInfoName ch)
                            if r.args.targetLanguages |> Seq.exists ((=) lang) then
                                match fps.reasonToChange with
                                | FieldIsKeyword            -> printfn "[INFO] Renamed field \"%s\" in type \"%s\" to \"%s\" (\"%s\" is a %A keyword)" (lm.lg.getChildInfoName ch) parentTypeName fieldName (lm.lg.getChildInfoName ch) lang
                                | FieldIsAlsoType tasName   -> printfn "[INFO] Renamed field \"%s\" in type \"%s\" to \"%s\" (Ada naming conflict with the field type \"%s\")" (lm.lg.getChildInfoName ch) parentTypeName fieldName tasName
                                | FieldIsAlsoModule modName   -> printfn "[INFO] Renamed field \"%s\" in type \"%s\" to \"%s\" (Ada naming conflict with the Module \"%s\")" (lm.lg.getChildInfoName ch) parentTypeName fieldName modName

                            fieldName
                    let newCh = lm.lg.setChildInfoName ch newUniqueName
                    newCh, ns
                    //match lang with
                    //| ProgrammingLanguage.C     -> {ch with Type = t; c_name = ToC2 newUniqueName},ns
                    //| ProgrammingLanguage.Scala -> {ch with Type = t; scala_name = ToC2 newUniqueName},ns
                    //| ProgrammingLanguage.Ada   -> {ch with Type = t; ada_name = ToC2 newUniqueName},ns

                match old.Kind with
                | Choice(children)
                | Sequence(children)    ->
                    let newChildren, finalState = children |> foldMap CloneChild state
                    match old.Kind with
                    | Choice(children)    -> {old with Kind =  Choice(newChildren)}, finalState
                    | Sequence(children)  -> {old with Kind =  Sequence(newChildren)}, finalState
                    | _                   -> raise (BugErrorException "impossible case in handleSequencesAndChoices")
                | _             -> defaultConstructors.cloneType old m key cons state

            let newTree = CloneTree r {defaultConstructors with cloneType =  CloneType; } invalidComponentNames |> fst
            handleSequencesAndChoices newTree (lang,lm) renamePolicy
    | FieldPrefixUserValue userValue    ->
        let CloneType (old:Asn1Type) m (key:list<string>) (cons:Constructors<State>) (state:State) =
            match old.Kind with
            | Choice(children)
            | Sequence(children)    ->
                let newChildren, finalState =
                    let newChildren =
                        children |> List.map(fun ch -> lm.lg.setChildInfoName ch (userValue + lm.lg.getChildInfoName ch))
                        (*
                        match lang with
                        | ProgrammingLanguage.C->
                            children |> List.map(fun ch -> {ch with c_name = ToC (userValue + ch.c_name)})
                        | ProgrammingLanguage.Scala->
                            children |> List.map(fun ch -> {ch with scala_name = ToC (userValue + ch.scala_name)})
                        | ProgrammingLanguage.Ada   ->
                            children |> List.map(fun ch -> {ch with ada_name = ToC(userValue + ch.ada_name)})
                        *)
                    newChildren, state
                match old.Kind with
                | Choice(children)    -> {old with Kind =  Choice(newChildren)}, finalState
                | Sequence(children)  -> {old with Kind =  Sequence(newChildren)}, finalState
                | _                   -> raise (BugErrorException "impossible case in handleSequencesAndChoices")
            | _             -> defaultConstructors.cloneType old m key cons state

        let newTree = CloneTree r {defaultConstructors with cloneType =  CloneType; } [] |> fst
        newTree  //no recursion is allowed in user defined value since the prefix value is constant.



let rec private handleEnums (r:AstRoot) (renamePolicy:EnumRenamePolicy) ((lang, lm): ProgrammingLanguage*LanguageMacros) =

    let doubleEnumNames0 =
        seq {
            for m in r.Modules do
                for tas in m.TypeAssignments do
                    for t in GetMySelfAndChildren tas.Type  do
                        match t.Kind with
                        | Enumerated(items) ->
                            let names = items |> List.map(fun x -> lm.lg.getNamedItemBackendName0 x)
                            yield! names
                        | _                 -> ()
                for vas in m.ValueAssignments do
                    yield vas.Name.Value
        } |> Seq.toList

    let doubleEnumNames = doubleEnumNames0@lm.lg.Keywords |> (List.keepDuplicates lm.lg.isCaseSensitive)

    match doubleEnumNames with
    | []    -> r
    | _     ->
        let CloneType (old:Asn1Type) m (key:list<string>) (cons:Constructors<State>) (state:State) =
            match old.Kind with
            | Enumerated(items)    ->
                let copyItem (old:NamedItem) =
                    let newUniqueName =
                        match state |> Seq.exists ((r.args.getBasicLang lang).cmp (lm.lg.getNamedItemBackendName0 old)) with
                        | false     -> lm.lg.getNamedItemBackendName0 old
                        | true      ->
                            let newPrefix = key |> List.rev |> List.map ToC |> Seq.skipWhile(fun x -> (lm.lg.getNamedItemBackendName0 old).Contains x) |> Seq.head
                            newPrefix + "_" + (lm.lg.getNamedItemBackendName0 old)
                    lm.lg.setNamedItemBackendName0 old newUniqueName
                let newItems =
                    match renamePolicy with
                    | AlwaysPrefixTypeName     (* to be handled later when typedefname is known*)
                    | NoRenamePolicy           -> items
                    | SelectiveEnumerants      -> items |> List.map copyItem
                    | AllEnumerants            ->
                        let newPrefix = items |> List.map copyItem |> List.map(fun itm -> (lm.lg.getNamedItemBackendName0 itm).Replace(ToC2 itm.Name.Value,"")) |> List.maxBy(fun prf -> prf.Length)
                        items|>
                            List.map(fun itm ->
                                let oldName = lm.lg.getNamedItemBackendName0 itm
                                let newName = newPrefix + oldName
                                lm.lg.setNamedItemBackendName0 itm newName)

                {old with Kind =  Enumerated(newItems)}, state
            | _             -> defaultConstructors.cloneType old m key cons state

        let newTree = CloneTree r {defaultConstructors with cloneType =  CloneType; } doubleEnumNames |> fst
        handleEnums newTree renamePolicy (lang,lm)


let DoWork (ast:AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list)  =
    let enumRenamePolicy = ast.args.renamePolicy
    let r2 =
        match enumRenamePolicy with
        | AlwaysPrefixTypeName     (* to be handled later when typedefname is known*)
        | NoRenamePolicy           -> ast
        | _                                             ->
            let r1 = handleEnumChoices ast  enumRenamePolicy

            lms |> List.fold ( fun r z -> handleEnums r enumRenamePolicy z ) r1
    match ast.args.fieldPrefix with
    | None  -> r2
    | Some fldPrefixPolicy    ->
        //let r3_c = handleSequencesAndChoices r2 ProgrammingLanguage.C fldPrefixPolicy
        //let r3_ada = handleSequencesAndChoices r3_c ProgrammingLanguage.Ada fldPrefixPolicy
        //let r3_scala = handleSequencesAndChoices r3_ada ProgrammingLanguage.Scala fldPrefixPolicy
        //r3_scala
        lms |> List.fold ( fun r z -> handleSequencesAndChoices r z fldPrefixPolicy ) r2
