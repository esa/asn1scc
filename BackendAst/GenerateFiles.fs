module GenerateFiles
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open AbstractMacros
open DAst
open DAstUtilFunctions
open OutDirectories
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
    lm.typeDef.PrintValueAssignment sName sTypeDecl  sVal

let printSourceFileValueAssignment (r:DAst.AstRoot) (vasPU_name:string)  (lm:LanguageMacros) (vas:ValueAssignment) =
    let sName = vas.c_name
    let t = vas.Type
    let sTypeDecl= getTypeDecl r vasPU_name lm vas

    let sVal = DAstVariables.printValue r  lm vasPU_name vas.Type None vas.Value.kind
    lm.vars.PrintValueAssignment sName sTypeDecl  sVal

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
    //let requiresXER = encodings |> Seq.exists ( (=) Asn1Encoding.XER)

    //header file
    //let typeDefs = tases |> List.choose(fun t -> t.getTypeDefinition l)
    let typeDefs = 
        tases |> 
        List.map(fun tas -> 
            let type_defintion = //tas.Type.typeDefinition.completeDefinition
                match tas.Type.typeDefintionOrReference with
                | TypeDefinition td -> td.typedefBody ()      
                | ReferenceToExistingDefinition _   -> raise(BugErrorException "Type Assignment with no Type Defintion")
            let init_def        = 
                match lm.lg.initMetod with
                | Procedure ->
                    //Some (GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.initFunction.initProcedure) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n")
                    Some(getInitializationFunctions tas.Type.initFunction |> List.choose( fun i_f -> i_f.initProcedure) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n" )
                | Function ->
                    Some(getInitializationFunctions tas.Type.initFunction |> List.choose( fun i_f -> i_f.initFunction) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n" )
                    //Some (GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.initFunction.initFunction ) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n")
            let init_globals    =
                //we generate const globals only if requested by user and the init method is procedure 
                match r.args.generateConstInitGlobals && (lm.lg.initMetod  = Procedure) with
                | false -> None
                | true  -> Some (GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.initFunction.initGlobal ) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n")

            let special_init_funcs =
                tas.Type.initFunction.user_aux_functions |> List.map fst


            let equal_defs      = //collectEqualFuncs tas.Type |> List.choose(fun ef -> ef.isEqualFuncDef)
                match r.args.GenerateEqualFunctions with
                | true  -> GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.equalFunction.isEqualFuncDef ) 
                | false -> []
            let isValidFuncs        = 
                //match tas.Type.isValidFunction with
                //| None      -> []
                //| Some f    -> 
                //GetMySelfAndChildren3 printChildrenIsValidFuncs tas.Type |> List.choose(fun f -> f.isValidFunction )  |> List.choose(fun f -> f.funcDef)
                match tas.Type.isValidFunction with
                | None      -> []
                | Some f    -> 
                    getValidFunctions f |> List.choose(fun f -> f.funcDef)


            let uPerEncFunc = match requiresUPER with true -> tas.Type.uperEncFunction.funcDef | false -> None
            let uPerDecFunc = match requiresUPER with true -> tas.Type.uperDecFunction.funcDef | false -> None

            let xerEncFunc = match tas.Type.xerEncFunction with XerFunction z -> z.funcDef | XerFunctionDummy -> None
            let xerDecFunc = match tas.Type.xerDecFunction with XerFunction z -> z.funcDef | XerFunctionDummy -> None

            let acnEncFunc = 
                match requiresAcn, tas.Type.acnEncFunction with 
                | true, Some x -> x.funcDef
                | _  -> None
            let acnDecFunc = 
                match requiresAcn, tas.Type.acnDecFunction with 
                | true, Some x -> x.funcDef
                | _ -> None 

            let allProcs = equal_defs@isValidFuncs@special_init_funcs@([init_globals;init_def;uPerEncFunc;uPerDecFunc;acnEncFunc; acnDecFunc;xerEncFunc;xerDecFunc] |> List.choose id)
            lm.typeDef.Define_TAS type_defintion allProcs 
        )
    let arrsValues = 
        vases |>
        List.map(fun gv -> printHeaderFileValueAssignment r pu.name lm gv)
    let arrsHeaderAnonymousValues =
        arrsAnonymousValues |>
        List.map(fun av -> lm.typeDef.PrintValueAssignment av.valueName av.typeDefinitionName "")
    

    let arrsPrototypes = []

    //sFileNameWithNoExtUpperCase, sPackageName, arrsIncludedModules, arrsTypeAssignments, arrsValueAssignments, arrsPrototypes, arrsUtilityDefines, bHasEncodings, bXer
    let sFileNameWithNoExtUpperCase = (ToC (System.IO.Path.GetFileNameWithoutExtension pu.specFileName))
    let bXer = r.args.encodings |> Seq.exists ((=) XER) 
    let arrsUtilityDefines = []
    let defintionsContntent =
        lm.typeDef.PrintSpecificationFile sFileNameWithNoExtUpperCase pu.name pu.importedProgramUnits typeDefs (arrsValues@arrsHeaderAnonymousValues) arrsPrototypes arrsUtilityDefines (not r.args.encodings.IsEmpty) bXer
        
    let fileName = Path.Combine(outDir, pu.specFileName)
    File.WriteAllText(fileName, defintionsContntent.Replace("\r",""))


    // test cases header file
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  -> 
        let typeDefs = 
            seq {
                for tas in tases do
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.UPER) then
                        yield (tas.Type.uperEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.XER) then
                        yield (tas.Type.xerEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.ACN) then
                        yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                } |> Seq.choose id |> Seq.toList
        let tetscase_specFileName = Path.Combine(outDir, pu.tetscase_specFileName)
        let tstCasesHdrContent = lm.atc.PrintAutomaticTestCasesSpecFile (ToC pu.tetscase_specFileName) pu.name pu.importedProgramUnits typeDefs
        File.WriteAllText(tetscase_specFileName, tstCasesHdrContent.Replace("\r",""))
        
    //sourse file
    let arrsTypeAssignments = 
        tases |> List.map(fun t -> 
            let initialize        = 
                match lm.lg.initMetod with
                | InitMethod.Procedure  ->
                    //Some(GetMySelfAndChildren t.Type |> List.choose(fun y -> y.initFunction.initProcedure) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n")
                    Some(getInitializationFunctions t.Type.initFunction |> List.choose( fun i_f -> i_f.initProcedure) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n" )
                | InitMethod.Function  ->
                    //Some (GetMySelfAndChildren t.Type |> List.choose(fun t -> t.initFunction.initFunction ) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n")
                    Some(getInitializationFunctions t.Type.initFunction |> List.choose( fun i_f -> i_f.initFunction) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n" )

            let init_globals    =
                match r.args.generateConstInitGlobals  && (lm.lg.initMetod  = Procedure) with
                | false -> None
                | true  -> Some (GetMySelfAndChildren t.Type |> List.choose(fun t -> t.initFunction.initGlobal) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n")


            let special_init_funcs =
                t.Type.initFunction.user_aux_functions |> List.map snd

            //let eqFuncs = collectEqualDeffinitions t |> List.choose(fun ef -> ef.isEqualFunc)
            let eqFuncs = //collectEqualFuncs t.Type |> List.choose(fun ef -> ef.isEqualFunc)
                match r.args.GenerateEqualFunctions with
                | true  -> GetMySelfAndChildren t.Type |> List.choose(fun y -> y.equalFunction.isEqualFunc)
                | false -> []

            let isValidFuncs = //match t.Type.isValidFunction with None -> None | Some isVal -> isVal.func
                //GetMySelfAndChildren3 printChildrenIsValidFuncs t.Type |> List.choose(fun f -> f.isValidFunction )  |> List.choose(fun f -> f.func)
                match t.Type.isValidFunction with
                | None      -> []
                | Some f    -> 
                    getValidFunctions f |> List.choose(fun f -> f.func)

            let uperEncDec codec         =  
                match requiresUPER with
                | true  ->
                    match codec with
                    | CommonTypes.Encode    -> t.Type.uperEncFunction.func
                    | CommonTypes.Decode    -> t.Type.uperDecFunction.func
                | false -> None

            let xerEncDec codec         =  
                match codec with
                | CommonTypes.Encode    -> 
                    match t.Type.xerEncFunction with
                    | XerFunction z ->  z.func
                    | XerFunctionDummy  -> None
                | CommonTypes.Decode    -> 
                    match t.Type.xerDecFunction with
                    | XerFunction z -> z.func
                    | XerFunctionDummy  -> None

            let ancEncDec codec         = 
                match requiresAcn with
                | true ->
                    match codec with
                    | CommonTypes.Encode    -> match t.Type.acnEncFunction with None -> None | Some x -> x.func
                    | CommonTypes.Decode    -> match t.Type.acnDecFunction with None -> None | Some x -> x.func
                | false     -> None
            let allProcs =  eqFuncs@isValidFuncs@special_init_funcs@([init_globals;initialize; (uperEncDec CommonTypes.Encode); (uperEncDec CommonTypes.Decode);(ancEncDec CommonTypes.Encode); (ancEncDec CommonTypes.Decode);(xerEncDec CommonTypes.Encode); (xerEncDec CommonTypes.Decode)] |> List.choose id)
            lm.src.printTass allProcs )


    let arrsValueAssignments, arrsSourceAnonymousValues = 
        match lm.lg.requiresValueAssignmentsInSrcFile with
        | true -> 
            let arrsValueAssignments = vases |> List.map (printSourceFileValueAssignment r pu.name lm)
            let arrsSourceAnonymousValues =  arrsAnonymousValues |> List.map (fun av -> lm.vars.PrintValueAssignment av.typeDefinitionName av.valueName av.valueExpresion)
            arrsValueAssignments, arrsSourceAnonymousValues
        | false ->
            [], []
    let rtlFiles = lm.lg.getRtlFiles r.args.encodings arrsTypeAssignments
    let arrsImportedFiles = rtlFiles@pu.importedUserModules@pu.importedProgramUnits |> List.distinct
    let srcBody = lm.src.printSourceFile pu.name arrsImportedFiles pu.importedTypes (arrsValueAssignments@arrsSourceAnonymousValues@arrsTypeAssignments)
    
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

    //test cases sourse file
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  -> 
        let encDecFuncs = 
            seq {
                for tas in tases do
                
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.UPER) then
                        yield (tas.Type.uperEncDecTestFunc |> Option.map (fun z -> z.func))
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.XER) then
                        yield (tas.Type.xerEncDecTestFunc |> Option.map (fun z -> z.func))
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.ACN) then
                        yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.func))
                } |> Seq.choose id |> Seq.toList

        let tetscase_SrcFileName = Path.Combine(outDir, pu.tetscase_bodyFileName)
        let bXer = r.args.encodings |> Seq.exists((=) XER)
        let tstCasesHdrContent =
            match lm.lg.allowsSrcFilesWithNoFunctions with
            | true     -> Some (lm.atc.PrintAutomaticTestCasesBodyFile pu.name pu.tetscase_specFileName pu.importedProgramUnits [] encDecFuncs bXer)
            | false   -> 
                match encDecFuncs with
                | []    -> None
                | _     -> Some (lm.atc.PrintAutomaticTestCasesBodyFile pu.name pu.tetscase_specFileName pu.importedProgramUnits [] encDecFuncs bXer)
        
        tstCasesHdrContent |> Option.iter(fun tstCasesHdrContent -> File.WriteAllText(tetscase_SrcFileName, tstCasesHdrContent.Replace("\r","")))





let generateAll (di:DirInfo) (r:DAst.AstRoot)  (lm:LanguageMacros) (encodings: CommonTypes.Asn1Encoding list)  =
    r.programUnits |> Seq.iter (printUnit r lm encodings di.srcDir)
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  -> 
        lm.lg.CreateMakeFile r di
        let arrsSrcTstFiles, arrsHdrTstFiles = DastTestCaseCreation.printAllTestCasesAndTestCaseRunner r lm di.srcDir
        lm.lg.CreateAuxFiles r di (arrsSrcTstFiles, arrsHdrTstFiles)



let EmmitDefaultACNGrammar (r:AstRoot) outDir  =
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