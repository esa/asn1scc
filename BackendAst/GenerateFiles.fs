module GenerateFiles
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open AbstractMacros
open DAst
open DAstUtilFunctions
open Language


let getTypeDecl = DastTestCaseCreation.getTypeDecl

let rec getValidFunctions (isValidFunction:IsValidFunction) =
    seq {
        for c in isValidFunction.nonEmbeddedChildrenValidFuncs do
            yield! getValidFunctions c
        yield isValidFunction
    } |> Seq.toList

let rec getInitializationFunctions (isValidFunction:InitFunction) =
    seq {
        for c in isValidFunction.nonEmbeddedChildrenFuncs do
            yield! getInitializationFunctions c
        yield isValidFunction
    } |> Seq.toList

let printHeaderFileValueAssignment (r:DAst.AstRoot) (vasPU_name:string)  (lm:LanguageMacros) (vas:ValueAssignment) =
    let sName = vas.c_name
    let t = vas.Type
    let sTypeDecl= getTypeDecl r vasPU_name lm vas

    let sVal = DAstVariables.printValue r  lm vasPU_name vas.Type None vas.Value.kind
    lm.typeDef.PrintValueAssignment sName sTypeDecl sVal

let printSourceFileValueAssignment (r:DAst.AstRoot) (vasPU_name:string)  (lm:LanguageMacros) (vas:ValueAssignment) =
    let sName = vas.c_name
    let t = vas.Type
    let sTypeDecl: string= getTypeDecl r vasPU_name lm vas
    let sVal = DAstVariables.printValue r  lm vasPU_name vas.Type None vas.Value.kind
    lm.vars.PrintValueAssignment sName sTypeDecl sVal

let rec collectEqualFuncs (t:Asn1Type) =
    seq {
        match t.Kind with
        | Integer          _
        | Real             _
        | IA5String        _
        | OctetString      _
        | NullType         _
        | BitString        _
        | Boolean          _
        | ObjectIdentifier _
        | TimeType         _
        | Enumerated       _ -> ()
        | SequenceOf        ch ->
            yield! collectEqualFuncs ch.childType
        | Sequence        sq ->
            for ch in sq.children do
                match ch with
                | Asn1Child ch  -> yield! collectEqualFuncs ch.Type
                | AcnChild  _   -> ()
        | Choice          ch ->
            for c in ch.children do
                yield! collectEqualFuncs c.chType
        | ReferenceType     _   -> ()
        yield t.equalFunction
    } |> Seq.toList


let private printUnit (r:DAst.AstRoot)  (lm:LanguageMacros) (encodings: CommonTypes.Asn1Encoding list) outDir (pu:ProgramUnit)  =
    let tases = pu.sortedTypeAssignments
    let printChildrenIsValidFuncs (t:Asn1Type) =
        match t.Kind with
        | SequenceOf o  -> o.Cons.IsEmpty
        | Sequence o    -> o.Cons.IsEmpty
        | Choice o      -> o.Cons.IsEmpty
        | _             -> false


    let vases = pu.valueAssignments
    let arrsAnonymousValues =
        pu.sortedTypeAssignments |>
        List.choose(fun z -> z.Type.isValidFunction) |>
        List.collect (fun z -> z.anonymousVariables)  |>
        Seq.distinctBy(fun z -> z.valueName) |>
        Seq.toList

    let requiresUPER = encodings |> Seq.exists ( (=) Asn1Encoding.UPER)
    let requiresAcn = encodings |> Seq.exists ( (=) Asn1Encoding.ACN)

    //header file
    let typeDefs =
        tases |>
        List.map(fun tas ->
            let typeAssignmentInfo = tas.Type.id.tasInfo.Value
            let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}

            let type_definition =
                match tas.Type.typeDefinitionOrReference with
                | TypeDefinition td -> td.typedefBody ()
                | ReferenceToExistingDefinition _   -> raise(BugErrorException "Type Assignment with no Type Definition")
            let init_def        =
                match r.callersSet |> Set.contains (f InitFunctionType) with
                | true -> 
                    match lm.lg.initMethod with
                    | Procedure ->
                        Some(getInitializationFunctions tas.Type.initFunction |> List.choose( fun i_f -> i_f.initProcedure) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n" )
                    | Function ->
                        Some(getInitializationFunctions tas.Type.initFunction |> List.choose( fun i_f -> i_f.initFunction) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n" )
                | false -> None

            let init_globals    =
                //we generate const globals only if requested by user and the init method is procedure
                match r.args.generateConstInitGlobals && (lm.lg.initMethod  = Procedure) with
                | false -> None
                | true  -> Some (GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.initFunction.initGlobal ) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n")

            let special_init_funcs =
                tas.Type.initFunction.user_aux_functions |> List.map fst


            let equal_defs =
                match r.args.GenerateEqualFunctions && (r.callersSet |> Set.contains (f EqualFunctionType)) with
                | true  -> GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.equalFunction.isEqualFuncDef )
                | false -> []
            let isValidFuncs =
                match r.callersSet |> Set.contains (f IsValidFunctionType) with
                | false -> []
                | true  ->
                    match tas.Type.isValidFunction with
                    | None      -> []
                    | Some f    ->
                        getValidFunctions f |> List.choose(fun f -> f.funcDef)


            let uPerEncFunc = match requiresUPER && r.callersSet |> Set.contains (f UperEncDecFunctionType) with true -> tas.Type.uperEncFunction.funcDef | false -> None
            let uPerDecFunc = match requiresUPER && r.callersSet |> Set.contains (f UperEncDecFunctionType) with true -> tas.Type.uperDecFunction.funcDef | false -> None

            let xerEncFunc = match tas.Type.xerEncFunction with XerFunction z -> z.funcDef | XerFunctionDummy -> None
            let xerDecFunc = match tas.Type.xerDecFunction with XerFunction z -> z.funcDef | XerFunctionDummy -> None

            let hasAcnEncDec = r.callersSet |> Set.contains (f AcnEncDecFunctionType)
            let acnEncFunc =
                match hasAcnEncDec && requiresAcn, tas.Type.acnEncFunction with
                | true, Some x -> x.funcDef
                | _  -> None
            let acnDecFunc =
                match hasAcnEncDec && requiresAcn, tas.Type.acnDecFunction with
                | true, Some x -> x.funcDef
                | _ -> None

            let allProcs = equal_defs@isValidFuncs@special_init_funcs@([init_globals;init_def;uPerEncFunc;uPerDecFunc;acnEncFunc; acnDecFunc;xerEncFunc;xerDecFunc] |> List.choose id)
            lm.typeDef.Define_TAS type_definition allProcs
        )
    let arrsValues =
        vases |>
        List.map(fun gv -> printHeaderFileValueAssignment r pu.name lm gv)
    let arrsHeaderAnonymousValues =
        arrsAnonymousValues |>
        List.map(fun av -> lm.typeDef.PrintValueAssignment av.valueName av.typeDefinitionName "")


    let arrsPrototypes = []

    let sFileNameWithNoExtUpperCase = (ToC (System.IO.Path.GetFileNameWithoutExtension pu.specFileName))
    let bXer = r.args.encodings |> Seq.exists ((=) XER)
    let arrsUtilityDefines = []
    let puCorrName =
        match r.lang with
        | CommonTypes.ProgrammingLanguage.Scala -> ToC (pu.name)
        | _ -> pu.name

    let definitionsContntent =
        lm.typeDef.PrintSpecificationFile sFileNameWithNoExtUpperCase puCorrName pu.importedProgramUnits typeDefs (arrsValues@arrsHeaderAnonymousValues) arrsPrototypes arrsUtilityDefines (not r.args.encodings.IsEmpty) bXer

    let fileName = Path.Combine(outDir, pu.specFileName)
    File.WriteAllText(fileName, definitionsContntent.Replace("\r",""))


    // test cases header file
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  ->
        let typeDefs =
            seq {
                for tas in tases do
                    let typeAssignmentInfo = tas.Type.id.tasInfo.Value
                    let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}
                    let reqUPER = r.callersSet |> Set.contains (f UperEncDecFunctionType)
                    let reqACN = r.callersSet |> Set.contains (f AcnEncDecFunctionType)

                    if reqUPER && r.args.encodings |> Seq.exists ((=) CommonTypes.UPER) then
                        yield (tas.Type.uperEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.XER) then
                        yield (tas.Type.xerEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                    if reqACN && r.args.encodings |> Seq.exists ((=) CommonTypes.ACN) then
                        yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                } |> Seq.choose id |> Seq.toList
        let testcase_specFileName = Path.Combine(outDir, pu.testcase_specFileName)
        let tstCasesHdrContent = lm.atc.PrintAutomaticTestCasesSpecFile (ToC pu.testcase_specFileName) pu.name (pu.name::pu.importedProgramUnits) typeDefs
        File.WriteAllText(testcase_specFileName, tstCasesHdrContent.Replace("\r",""))

    //source file
    let arrsTypeAssignments =
        tases |> List.map(fun t ->
            let typeAssignmentInfo = t.Type.id.tasInfo.Value
            let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}

            let privateDefinition =
                match t.Type.typeDefinitionOrReference with
                | TypeDefinition td -> td.privateTypeDefinition
                | ReferenceToExistingDefinition _   -> None

            let initialize =
                match r.callersSet |> Set.contains (f InitFunctionType) with
                | true -> 
                    match lm.lg.initMethod with
                    | InitMethod.Procedure  ->
                        Some(getInitializationFunctions t.Type.initFunction |> List.choose( fun i_f -> i_f.initProcedure) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n" )
                    | InitMethod.Function  ->
                        Some(getInitializationFunctions t.Type.initFunction |> List.choose( fun i_f -> i_f.initFunction) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n" )
                | false -> None

            let init_globals    =
                match r.args.generateConstInitGlobals  && (lm.lg.initMethod  = Procedure) with
                | false -> None
                | true  -> Some (GetMySelfAndChildren t.Type |> List.choose(fun t -> t.initFunction.initGlobal) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n")


            let special_init_funcs =
                t.Type.initFunction.user_aux_functions |> List.map snd

            let eqFuncs =
                match r.args.GenerateEqualFunctions && (r.callersSet |> Set.contains (f EqualFunctionType)) with
                | true  -> GetMySelfAndChildren t.Type |> List.choose(fun y -> y.equalFunction.isEqualFunc)
                | false -> []

            let isValidFuncs =
                match r.callersSet |> Set.contains (f IsValidFunctionType) with
                | false -> []
                | true  ->
                    match t.Type.isValidFunction with
                    | None      -> []
                    | Some f    ->
                        getValidFunctions f |> List.choose(fun f -> f.func)

            let uperEncDec =
                if requiresUPER && r.callersSet |> Set.contains (f UperEncDecFunctionType) then
                    ((t.Type.uperEncFunction.func |> Option.toList |> List.collect (fun f -> f :: t.Type.uperEncFunction.auxiliaries))) @
                    ((t.Type.uperDecFunction.func |> Option.toList |> List.collect (fun f ->  f :: t.Type.uperDecFunction.auxiliaries)))
                else []

            let xerEncDec =
                (match t.Type.xerEncFunction with
                | XerFunction z ->  z.func |> Option.toList
                | XerFunctionDummy  -> []) @
                (match t.Type.xerDecFunction with
                | XerFunction z -> z.func |> Option.toList
                | XerFunctionDummy -> [])
            
            let hasAcnEncDec = r.callersSet |> Set.contains (f AcnEncDecFunctionType)
            let ancEncDec =
                if requiresAcn && hasAcnEncDec then
                    (t.Type.acnEncFunction |> Option.toList |> List.collect (fun x -> (x.func |> Option.toList) @ x.auxiliaries)) @
                    (t.Type.acnDecFunction |> Option.toList |> List.collect (fun x -> (x.func |> Option.toList) @ x.auxiliaries))
                else []
            let allProcs =
                (privateDefinition |> Option.toList) @
                eqFuncs @ isValidFuncs @ special_init_funcs @
                (init_globals |> Option.toList) @
                (initialize |> Option.toList) @
                uperEncDec @ ancEncDec @ xerEncDec
            lm.src.printTass allProcs)


    let arrsValueAssignments, arrsSourceAnonymousValues =
        match lm.lg.requiresValueAssignmentsInSrcFile with
        | true ->
            let arrsValueAssignments = vases |> List.map (printSourceFileValueAssignment r pu.name lm)
            let arrsSourceAnonymousValues =  arrsAnonymousValues |> List.map (fun av -> lm.vars.PrintValueAssignment av.typeDefinitionName av.valueName av.valueExpression)
            arrsValueAssignments, arrsSourceAnonymousValues
        | false ->
            [], []
    let rtlFiles = lm.lg.getRtlFiles r.args.encodings arrsTypeAssignments
    let arrsImportedFiles = rtlFiles@pu.importedUserModules@pu.importedProgramUnits |> List.distinct
    let puCorrName =
        match r.lang with
        | CommonTypes.ProgrammingLanguage.Scala -> ToC (pu.name)
        | _ -> pu.name
    let srcBody = lm.src.printSourceFile puCorrName arrsImportedFiles pu.importedTypes (arrsValueAssignments@arrsSourceAnonymousValues@arrsTypeAssignments)

    let eqContntent =
        match lm.lg.allowsSrcFilesWithNoFunctions with
        | true     ->
            Some srcBody
        | false   ->
            match arrsTypeAssignments with
            | []    -> None
            | _     -> Some srcBody

    match eqContntent with
    | Some eqContntent ->
        let fileName = Path.Combine(outDir, pu.bodyFileName)
        File.WriteAllText(fileName, eqContntent.Replace("\r",""))
    | None             -> ()

    //test cases source file
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  ->
        let encDecFuncs =
            seq {
                for tas in tases do
                    let typeAssignmentInfo = tas.Type.id.tasInfo.Value
                    let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}
                    let reqUPER = 
                        r.args.encodings |> Seq.exists ((=) CommonTypes.UPER)
                        && r.callersSet |> Set.contains (f UperEncDecFunctionType)
                    let reqACN = 
                        r.args.encodings |> Seq.exists ((=) CommonTypes.ACN)
                        && r.callersSet |> Set.contains (f AcnEncDecFunctionType)

                    if reqUPER then
                        yield (tas.Type.uperEncDecTestFunc |> Option.map (fun z -> z.func))
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.XER) then
                        yield (tas.Type.xerEncDecTestFunc |> Option.map (fun z -> z.func))
                    if reqACN  then
                        yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.func))
                } |> Seq.choose id |> Seq.toList

        let testcase_SrcFileName = Path.Combine(outDir, pu.testcase_bodyFileName)
        let bXer = r.args.encodings |> Seq.exists((=) XER)
        let tstCasesHdrContent =
            match lm.lg.allowsSrcFilesWithNoFunctions with
            | true     -> Some (lm.atc.PrintAutomaticTestCasesBodyFile pu.name pu.testcase_specFileName pu.importedProgramUnits [] encDecFuncs bXer)
            | false   ->
                match encDecFuncs with
                | []    -> None
                | _     -> Some (lm.atc.PrintAutomaticTestCasesBodyFile pu.name pu.testcase_specFileName pu.importedProgramUnits [] encDecFuncs bXer)

        tstCasesHdrContent |> Option.iter(fun tstCasesHdrContent -> File.WriteAllText(testcase_SrcFileName, tstCasesHdrContent.Replace("\r","")))

    (definitionsContntent, srcBody)



let generateAll (di:DirInfo) (r:DAst.AstRoot)  (lm:LanguageMacros) (encodings: CommonTypes.Asn1Encoding list)  =
    let generatedContent = r.programUnits |> List.map(printUnit r lm encodings di.srcDir) |> List.map snd |> Seq.StrJoin "\n"
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  ->
        lm.lg.CreateMakeFile r di
        let arrsSrcTstFiles, arrsHdrTstFiles = DastTestCaseCreation.printAllTestCasesAndTestCaseRunner r lm di.srcDir
        lm.lg.CreateAuxFiles r di (arrsSrcTstFiles, arrsHdrTstFiles)
    generatedContent


let EmitDefaultACNGrammar (r:AstRoot) outDir  =
    let printTas (tas: TypeAssignment) =
        tas.Name.Value + "[]"
    let printModule (m:Asn1Module) =
        let arrTases = m.TypeAssignments |> Seq.map printTas
        stg_acn.PrintDefaultAcnModule m.Name.Value arrTases "::="
    let printFile (f:Asn1File) =
        let fileName = f.FileNameWithoutExtension + ".acn"
        if (System.IO.File.Exists fileName) then
            System.Console.Error.WriteLine("File {0} already exists. Creation of default ASN.1 grammar abandoned", fileName);
        else
            let content = f.Modules |> Seq.map printModule |> Seq.StrJoin "\n"
            let fileName = Path.Combine(outDir, fileName)
            File.WriteAllText(fileName, content.Replace("\r",""))

    r.Files |> Seq.iter printFile