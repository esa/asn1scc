module DAstProgramUnit
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes

open DAst
open DAstUtilFunctions



let private getTypeDependencies (t:Asn1Type) : (TypeAssignmentInfo list )  
    =
    let prms = t.acnParameters |> List.choose(fun z -> match z.asn1Type with Asn1AcnAst.AcnPrmRefType (mdName,tsName) -> Some ({TypeAssignmentInfo.modName = mdName.Value; tasName = tsName.Value}) | _ -> None )    
    DastFold.foldAsn1Type
        t
        ()
        (fun o newBase us -> prms)
        (fun o newBase us -> prms)
        (fun o newBase us -> prms)
        (fun o newBase us -> prms)
        (fun o newBase us -> prms)
        (fun o newBase us -> prms)
        (fun o newBase us -> prms)
        (fun o newBase us -> prms)
        (fun o sqo child ->  child@prms)
        (fun _ _ ch newChild -> newChild@prms, ())
        (fun _ _ _ _ -> prms, ())
        (fun o sq (children,_) -> (children |> List.collect id)@prms)
        (fun _ _ ch newChild -> newChild@prms, ())
        (fun o ch (children, _) -> (children|> List.collect id)@prms)
        (fun o ref baseType -> ref.AsTypeAssignmentInfo::prms@baseType)
        (fun o newKind  -> newKind@prms)


        
let rec private  getTypeDependencies2 (t:Asn1Type) : (TypeAssignmentInfo list )    =
    let prms = t.acnParameters |> List.choose(fun z -> match z.asn1Type with Asn1AcnAst.AcnPrmRefType (mdName,tsName) -> Some ({TypeAssignmentInfo.modName = mdName.Value; tasName = tsName.Value}) | _ -> None )    
    match t.Kind with
    | Integer      _             -> prms
    | Real         _             -> prms
    | IA5String    _             -> prms
    | OctetString  _             -> prms
    | NullType     _             -> prms
    | BitString    _             -> prms
    | Boolean      _             -> prms
    | Enumerated   _             -> prms
    | SequenceOf    sqof         -> prms@(getTypeDependencies2 sqof.childType) 
    | Sequence      children     -> prms@(children.Asn1Children |> List.collect (fun ch -> getTypeDependencies2 ch.Type))
    | Choice        children     -> prms@(children.children |> List.collect (fun ch -> getTypeDependencies2 ch.chType))
    | ReferenceType ref          -> ref.AsTypeAssignmentInfo::prms

let private sortTypes (typesToSort: Asn1Type list) (imports :TypeAssignmentInfo list) =
    let allNodes = 
        typesToSort |> 
        List.choose( fun tas -> 
            match tas.typeAssignmentInfo with
            | Some (TypeAssignmentInfo tasInfo)  -> Some ( (tasInfo, getTypeDependencies2 tas ))
            | Some (ValueAssignmentInfo _)  
            | None          -> raise (BugErrorException "All TypeAssignemts must have tasInfo") )
    let independentNodes = allNodes |> List.filter(fun (_,list) -> List.isEmpty list) |> List.map(fun (n,l) -> n)
    let dependentNodes = allNodes |> List.filter(fun (_,list) -> not (List.isEmpty list) )
    let sortedTypeAss = 
        DoTopologicalSort (imports @ independentNodes) dependentNodes 
            (fun cyclicTasses -> 
                match cyclicTasses with
                | []    -> BugErrorException "Impossible"
                | (m1,deps) ::_ ->
                    let printTas (md:TypeAssignmentInfo, deps: TypeAssignmentInfo list) = 
                        sprintf "Type assignment '%s.%s' depends on : %s" md.modName md.tasName (deps |> List.map(fun z -> "'" + z.modName + "." + z.tasName + "'") |> Seq.StrJoin ", ")
                    let cycTasses = cyclicTasses |> List.map printTas |> Seq.StrJoin "\n\tand\n"
                    SemanticError(emptyLocation, sprintf "Cyclic Types detected:\n%s\n"  cycTasses)                    )
    sortedTypeAss

let internal createProgramUnits (args:CommandLineSettings) (files: Asn1File list)  (l:ProgrammingLanguage) =
    match l with
    | C     -> 
        files |>
        List.map(fun f -> 
            let modulesSet = f.Modules |> List.map(fun x -> x.Name.Value) |> Set.ofList
            let fileTases = 
                seq {
                    for m in f.Modules do
                        for tas in m.TypeAssignments do
                            yield (tas.AsTypeAssignmentInfo m.Name.Value, tas)
                } |> Seq.toList
            let fileValueAssignments = f.Modules |> List.collect(fun m -> m.ValueAssignments)
            let tasSet = Map.ofList fileTases
            let fileTypes = fileTases |> List.map snd |> List.map(fun t -> t.Type)
            let valueAssignments = f.Modules |> Seq.collect(fun v -> v.ValueAssignments) 
            let thisFileModules = f.Modules |> List.map(fun x -> x.Name.Value)
            let importedModules =
                f.Modules |> 
                Seq.collect(fun m -> m.Imports) |>
                Seq.filter(fun m -> not (thisFileModules |> Seq.exists ((=) m.Name.Value) )) |>
                Seq.toList

            let importedProgramUnits =
                importedModules |>
                List.map(fun imp ->
                    let impFile = files |> Seq.find(fun f -> f.Modules |> Seq.exists (fun md -> md.Name.Value = imp.Name.Value) )
                    impFile.FileNameWithoutExtension) |> 
                Seq.distinct |> Seq.toList

            let importedTypes = 
                importedModules |>
                Seq.collect(fun imp -> imp.Types |> List.map (fun impType ->{TypeAssignmentInfo.modName = imp.Name.Value; tasName = impType.Value}  )) |> 
                Seq.distinct |> Seq.toList

            let sortedTypes = 
                sortTypes fileTypes importedTypes |> 
                List.choose(fun ref -> 
                    match tasSet.TryFind ref with
                    | Some vl -> Some vl
                    | None    -> None (*raise(SemanticError(emptyLocation, sprintf "Type assignment %s.%s cannot be resolved within progam unit %s" ref.modName ref.tasName f.FileNameWithoutExtension))*)
                )
            let specFileName = f.FileNameWithoutExtension+"."+l.SpecExtention
            let bodyFileName = f.FileNameWithoutExtension+"."+l.BodyExtention
            let tetscase_specFileName = f.FileNameWithoutExtension+"_auto_tcs."+l.SpecExtention
            let tetscase_bodyFileName = f.FileNameWithoutExtension+"_auto_tcs."+l.BodyExtention
            let tetscase_name = f.FileNameWithoutExtension+"_auto_tcs"
            {ProgramUnit.name = f.FileNameWithoutExtension; specFileName = specFileName; bodyFileName=bodyFileName; sortedTypeAssignments = sortedTypes; valueAssignments = fileValueAssignments; importedProgramUnits = importedProgramUnits; tetscase_specFileName=tetscase_specFileName; tetscase_bodyFileName=tetscase_bodyFileName; tetscase_name=tetscase_name; importedTypes= []})
    | Ada   -> 
        let typesMap = 
            files |> 
            List.collect(fun f -> f.Modules) |>
            List.collect(fun m ->
                m.TypeAssignments |> List.map(fun tas -> tas.AsTypeAssignmentInfo m.Name.Value, tas) 
            ) |> Map.ofList

        files |>
        List.collect(fun f -> f.Modules |> List.map (fun m -> f,m)) |>
        List.map(fun (f,m) ->
            let moduTypes = m.TypeAssignments |> List.map(fun x -> x.Type)
            let valueAssignments = m.ValueAssignments
            


            let importedTypes = 
                m.Imports |>
                Seq.collect(fun imp -> imp.Types |> List.map (fun impType ->{TypeAssignmentInfo.modName = imp.Name.Value; tasName = impType.Value})) |> 
                Seq.distinct |> Seq.toList        
            let sortedTypes = sortTypes moduTypes importedTypes |> List.filter(fun z -> z.modName = m.Name.Value) |> List.map(fun ref -> typesMap.[ref]) 
            let depTypesFromOtherModules =
                sortedTypes |> 
                List.collect (fun t -> getTypeDependencies t.Type) |>
                List.filter (fun t -> t.modName <> m.Name.Value) 
            let importedProgramUnitsFromVases = 
                valueAssignments |> 
                List.choose(fun z -> 
                    match z.Type.Kind with
                    |ReferenceType ref -> 
                        match ref.baseInfo.modName.Value = m.Name.Value with
                        | true -> None
                        | false -> Some (ToC ref.baseInfo.modName.Value)
                    | _                -> None)
            let importedProgramUnitsFromTasses = 
                depTypesFromOtherModules |> Seq.map(fun ti -> ToC ti.modName) |> Seq.distinct |> Seq.toList
            let importedProgramUnits = importedProgramUnitsFromTasses@importedProgramUnitsFromVases |> Seq.distinct |> Seq.toList


            let importedTypes = 
                depTypesFromOtherModules |> 
                List.collect(fun ts -> 
                    let t = typesMap.[ts]
                    let allTypes = GetMySelfAndChildren t.Type
                    
                        
                    let aaa = 
                        allTypes |> 
                        List.choose (fun t -> 
                            let rtlPrimitve =
                                match t.Kind with
                                | Integer _ 
                                | Real _ 
                                | Boolean _ 
                                | NullType _ -> true
                                | _     -> false
                            getFuncNameGeneric2 t.typeDefintionOrReference) |>
                            //getFuncNameGeneric2 args t.tasInfo t.inheritInfo rtlPrimitve t.typeDefintionOrReference) |>
                        List.map(fun td -> (ToC ts.modName) + "." + td)

                    aaa) 
                //Seq.map(fun ti -> (ToC ti.modName) + "." + (ToC (args.TypePrefix + ti.tasName)) ) |> Seq.distinct |> Seq.toList

            let specFileName = ToC (m.Name.Value.ToLower()) + "." + l.SpecExtention
            let bodyFileName = ToC (m.Name.Value.ToLower()) + "." + l.BodyExtention
            let tetscase_specFileName = ToC (m.Name.Value.ToLower()) + "_auto_tcs." + l.SpecExtention
            let tetscase_bodyFileName = ToC (m.Name.Value.ToLower()) + "_auto_tcs." + l.BodyExtention
            //let importedProgramUnits = m.Imports |> List.map (fun im -> ToC im.Name.Value)
            let tetscase_name = ToC (m.Name.Value.ToLower()+"_auto_tcs")
            {ProgramUnit.name = ToC m.Name.Value; specFileName = specFileName; bodyFileName=bodyFileName; sortedTypeAssignments = sortedTypes; valueAssignments = valueAssignments; importedProgramUnits = importedProgramUnits; tetscase_specFileName=tetscase_specFileName; tetscase_bodyFileName=tetscase_bodyFileName; tetscase_name=tetscase_name; importedTypes= importedTypes})


