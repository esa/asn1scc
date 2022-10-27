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


let printValueAssignment (r:DAst.AstRoot) (vasPU_name:string) (l:ProgrammingLanguage) (lm:LanguageMacros) (vas:ValueAssignment) =
    let sName = vas.c_name
    let t = vas.Type
    let sTypeDecl= getTypeDecl r vasPU_name l vas

    let sVal = DAstVariables.printValue r  l lm vasPU_name vas.Type None vas.Value.kind
    match l with
    | C     -> variables_c.PrintValueAssignment sTypeDecl sName sVal
    | Ada   -> header_a.PrintValueAssignment sName sTypeDecl sVal


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


let private printUnit (r:DAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (encodings: CommonTypes.Asn1Encoding list) outDir (pu:ProgramUnit)  =
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
            let init_def        = //tas.Type.initFunction.initFuncDef 
                Some (GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.initFunction.initFuncDef ) |> Seq.StrJoin "\n")
            let init_def2        = //tas.Type.initFunction.initFuncDef 
                Some (GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.initFunction.constantInitExpression ) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n")
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

            let allProcs = equal_defs@isValidFuncs@special_init_funcs@([init_def2;init_def;uPerEncFunc;uPerDecFunc;acnEncFunc; acnDecFunc;xerEncFunc;xerDecFunc] |> List.choose id)
            match l with
            |C     -> header_c.Define_TAS type_defintion allProcs 
            |Ada   -> header_a.Define_TAS type_defintion allProcs 
        )
    let arrsValues = 
        vases |>
        List.map(fun gv -> 
            let t = gv.Type

            match l with
            | C     -> 
                match t.Kind with
                | Integer _
                | Real _
                | Boolean _     -> 
                    let typeDefinitionName = match t.tasInfo with| Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName) | None    -> t.typeDefintionOrReference.longTypedefName l //t.typeDefinition.typeDefinitionBodyWithinSeq
                    header_c.PrintValueAssignment gv.c_name (typeDefinitionName) ""
                | ReferenceType ref ->
                    let typeDefinitionName = ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value)
//                        match l with
//                        | C     ->  
//                        | Ada   ->
//                            match ToC ref.baseInfo.modName.Value = pu.name with
//                            | true  -> ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value) 
//                            | false -> (ToC ref.baseInfo.modName.Value) + "." + ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value) 
                    header_c.PrintValueAssignment gv.c_name (typeDefinitionName) ""
                | _             -> 
                    let typeDefinitionName = match t.tasInfo with| Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName) | None    -> t.typeDefintionOrReference.longTypedefName l//t.typeDefinition.name
                    header_c.PrintValueAssignment gv.c_name (typeDefinitionName) ""
            | Ada   -> printValueAssignment r pu.name l lm gv)
    let arrsHeaderAnonymousValues =
        arrsAnonymousValues |>
        List.map(fun av -> 
            match l with
            | C     -> header_c.PrintValueAssignment av.valueName av.typeDefinitionName ""
            | Ada   -> 
                header_a.PrintValueAssignment av.valueName av.typeDefinitionName av.valueExpresion)
    

    let arrsPrototypes = []

    //sFileNameWithNoExtUpperCase, sPackageName, arrsIncludedModules, arrsTypeAssignments, arrsValueAssignments, arrsPrototypes, arrsUtilityDefines, bHasEncodings, bXer
    let sFileNameWithNoExtUpperCase = (ToC (System.IO.Path.GetFileNameWithoutExtension pu.specFileName))
    let bXer = r.args.encodings |> Seq.exists ((=) XER) 
    let arrsUtilityDefines = []
    let defintionsContntent =
        match l with
        | C     -> 
            header_c.PrintSpecificationFile sFileNameWithNoExtUpperCase pu.name pu.importedProgramUnits typeDefs (arrsValues@arrsHeaderAnonymousValues) arrsPrototypes arrsUtilityDefines (not r.args.encodings.IsEmpty) bXer
        | Ada   -> 
            header_a.PrintSpecificationFile sFileNameWithNoExtUpperCase pu.name pu.importedProgramUnits typeDefs (arrsValues@arrsHeaderAnonymousValues) arrsPrototypes arrsUtilityDefines (not r.args.encodings.IsEmpty) bXer
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
        let tstCasesHdrContent =
            match l with
            | C     -> test_cases_c.PrintAutomaticTestCasesSpecFile (ToC pu.tetscase_specFileName) pu.name pu.importedProgramUnits typeDefs
            | Ada   -> test_cases_a.PrintAutomaticTestCasesSpecFile (ToC pu.tetscase_specFileName) pu.name pu.importedProgramUnits typeDefs
        File.WriteAllText(tetscase_specFileName, tstCasesHdrContent.Replace("\r",""))
        
    //sourse file
    let arrsTypeAssignments = 
        tases |> List.map(fun t -> 
            let init_def2        = //tas.Type.initFunction.initFuncDef 
                Some (GetMySelfAndChildren t.Type |> List.choose(fun t -> t.initFunction.constantInitExpression ) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n")
            let initialize        = //t.Type.initFunction.initFunc 
                Some(GetMySelfAndChildren t.Type |> List.choose(fun y -> y.initFunction.initFunc) |> Seq.StrJoin "\n")
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
            let allProcs =  eqFuncs@isValidFuncs@special_init_funcs@([init_def2;initialize; (uperEncDec CommonTypes.Encode); (uperEncDec CommonTypes.Decode);(ancEncDec CommonTypes.Encode); (ancEncDec CommonTypes.Decode);(xerEncDec CommonTypes.Encode); (xerEncDec CommonTypes.Decode)] |> List.choose id)
            match l with
            | C     ->  body_c.printTass allProcs 
            | Ada   ->  body_a.printTass allProcs )
    let eqContntent = 
        match l with
        | C     ->
            let arrsUnnamedVariables = []
            let arrsValueAssignments = vases |> List.map (printValueAssignment r pu.name l lm)
            let arrsSourceAnonymousValues = 
                arrsAnonymousValues |>
                List.map (fun av -> variables_c.PrintValueAssignment av.typeDefinitionName av.valueName av.valueExpresion)

            let encRtl = match r.args.encodings |> Seq.exists(fun e -> e = UPER || e = ACN ) with true -> ["asn1crt_encoding"] | false -> []
            let uperRtl = match r.args.encodings |> Seq.exists(fun e -> e = UPER || e = ACN) with true -> ["asn1crt_encoding_uper"] | false -> []
            let acnRtl = match r.args.encodings |> Seq.exists(fun e -> e = ACN) with true -> ["asn1crt_encoding_acn"] | false -> []
            let xerRtl = match r.args.encodings |> Seq.exists(fun e -> e = XER) with true -> ["asn1crt_encoding_xer"] | false -> []
            let arrsImportedRtlFiles = encRtl@uperRtl@acnRtl@xerRtl@pu.importedUserModules

            Some (body_c.printSourceFile pu.name arrsImportedRtlFiles arrsUnnamedVariables (arrsValueAssignments@arrsSourceAnonymousValues) arrsTypeAssignments)
        | Ada   ->
            let arrsNegativeReals = []
            let arrsBoolPatterns = []
            let arrsChoiceValueAssignments = []
            let uperRtl = match r.args.encodings |> Seq.exists(fun e -> e = UPER || e = ACN) with true -> ["adaasn1rtl.encoding.uper"] | false -> []
            let acnRtl = 
                //match r.args.encodings |> Seq.exists(fun e -> e = ACN) with true -> ["adaasn1rtl.encoding.acn"] | false -> []
                match arrsTypeAssignments |> Seq.exists(fun s -> s.Contains "adaasn1rtl.encoding.acn") with true -> ["adaasn1rtl.encoding.acn"] | false -> []
            let xerRtl = match r.args.encodings |> Seq.exists(fun e -> e = XER) with true -> ["adaasn1rtl.encoding.xer"] | false -> []

            //adaasn1rtl.encoding is included by .uper or .acn or .xer. So, do not include it otherwise you get a warning
            let encRtl = []//match r.args.encodings |> Seq.exists(fun e -> e = UPER || e = ACN || e = XER) with true -> [] | false -> ["adaasn1rtl.encoding"]
            let rtl = (*[body_a.rtlModuleName()]@*)encRtl@uperRtl@acnRtl@xerRtl@pu.importedUserModules |> List.distinct
            match arrsTypeAssignments with
            | []    -> None
            | _     -> Some (body_a.PrintPackageBody pu.name  (rtl@pu.importedProgramUnits) arrsNegativeReals arrsBoolPatterns arrsTypeAssignments arrsChoiceValueAssignments (pu.importedTypes |> List.distinct))
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
            match l with
            | C     -> Some (test_cases_c.PrintAutomaticTestCasesBodyFile pu.name pu.tetscase_specFileName pu.importedProgramUnits [] encDecFuncs bXer)
            | Ada   -> 
                match encDecFuncs with
                | []    -> None
                | _     -> Some (test_cases_a.PrintAutomaticTestCasesBodyFile pu.name pu.tetscase_specFileName pu.importedProgramUnits [] encDecFuncs bXer)
        
        tstCasesHdrContent |> Option.iter(fun tstCasesHdrContent -> File.WriteAllText(tetscase_SrcFileName, tstCasesHdrContent.Replace("\r","")))


let CreateCMainFile (r:AstRoot)  (l:ProgrammingLanguage) outDir  =
    //Main file for test cass    
    let printMain =    test_cases_c.PrintMain //match l with C -> test_cases_c.PrintMain | Ada -> test_cases_c.PrintMain
    let content = printMain DastTestCaseCreation.TestSuiteFileName
    let outFileName = Path.Combine(outDir, "mainprogram.c")
    File.WriteAllText(outFileName, content.Replace("\r",""))


let CreateMakeFile (r:AstRoot) (l:ProgrammingLanguage) (di:DirInfo) =
    match l with
    | C ->
        let files = r.Files |> Seq.map(fun x -> x.FileNameWithoutExtension.ToLower() )
        let content = aux_c.PrintMakeFile files (r.args.integerSizeInBytes = 4I) (r.args.floatingPointSizeInBytes = 4I) r.args.streamingModeSupport
        let outFileName = Path.Combine(di.srcDir, "Makefile")
        File.WriteAllText(outFileName, content.Replace("\r",""))
    | Ada ->
        let boardNames = OutDirectories.getBoardNames l r.args.target
        let writeBoard boardName = 
            let mods = aux_a.rtlModuleName()::(r.programUnits |> List.map(fun pu -> pu.name.ToLower() ))
            let content = aux_a.PrintMakeFile boardName (sprintf "asn1_%s.gpr" boardName) mods
            let fileName = if boardNames.Length = 1 || boardName = "x86" then "Makefile" else ("Makefile." + boardName)
            let outFileName = Path.Combine(di.rootDir, fileName)
            File.WriteAllText(outFileName, content.Replace("\r",""))
        OutDirectories.getBoardNames l r.args.target |> List.iter writeBoard


let private CreateAdaIndexFile (r:AstRoot) bGenTestCases outDir boardsDirName =
    let mods = r.programUnits |> Seq.map(fun x -> (ToC x.name).ToLower()) |>Seq.toList
    //let mds = match bGenTestCases with
    //            | true  -> mods @ (modules |> Seq.filter(fun x -> ModuleHasAutoCodecs x r) |> Seq.map(fun x -> (ToC x.Name.Value+"_auto_encs_decs").ToLower() ) |>Seq.toList)
    //            | false -> mods
    let mds = mods
    let fullPath = (System.IO.Path.GetFullPath outDir) + System.String(System.IO.Path.DirectorySeparatorChar,1)
    let lines = (header_a.rtlModuleName())::mds |> List.map(fun x -> aux_a.PrintLineInIndexFile x fullPath)
    let content = match bGenTestCases with
                    | true    -> aux_a.PrintIndexFile ("mainprogram    main_program  is in MainProgram.adb"::lines)
                    | false   -> aux_a.PrintIndexFile lines
    let outFileName = Path.Combine(outDir, "spark.idx")
    File.WriteAllText(outFileName, content.Replace("\r",""))


let generateVisualStudtioProject (r:DAst.AstRoot) outDir (arrsSrcTstFilesX, arrsHdrTstFilesX) =
    //generate Visual Studio project file
//    let extrSrcFiles, extrHdrFiles = 
//        match r.args.encodings |> List.exists ((=) Asn1Encoding.XER) with
//        | false     -> [],[]
//        | true      -> ["xer.c"],["xer.h"]

    let extrSrcFiles, extrHdrFiles = 
        r.args.encodings |> 
        List.collect(fun e -> 
            match e with
            | Asn1Encoding.UPER -> ["asn1crt_encoding";"asn1crt_encoding_uper"]
            | Asn1Encoding.ACN  -> ["asn1crt_encoding";"asn1crt_encoding_uper"; "asn1crt_encoding_acn"]
            | Asn1Encoding.BER  -> ["asn1crt_encoding";"asn1crt_encoding_ber"]
            | Asn1Encoding.XER  -> ["asn1crt_encoding";"asn1crt_encoding_xer"]
        ) |> 
        List.distinct |>
        List.map(fun a -> a + ".c", a + ".h") |>
        List.unzip

    let arrsSrcTstFiles = (r.programUnits |> List.map (fun z -> z.tetscase_bodyFileName))
    let arrsHdrTstFiles = (r.programUnits |> List.map (fun z -> z.tetscase_specFileName))
    let vcprjContent = xml_outputs.emitVisualStudioProject 
                        ((r.programUnits |> List.map (fun z -> z.bodyFileName))@extrSrcFiles)
                        ((r.programUnits |> List.map (fun z -> z.specFileName))@extrHdrFiles)
                        (arrsSrcTstFiles@arrsSrcTstFilesX)
                        (arrsHdrTstFiles@arrsHdrTstFilesX)
    let vcprjFileName = Path.Combine(outDir, "VsProject.vcxproj")
    File.WriteAllText(vcprjFileName, vcprjContent)

    //generate Visual Studio Solution file
    File.WriteAllText((Path.Combine(outDir, "VsProject.sln")), (aux_c.emitVisualStudioSolution()))


let generateAll (di:DirInfo) (r:DAst.AstRoot)  (lm:LanguageMacros) (encodings: CommonTypes.Asn1Encoding list)  =
    r.programUnits |> Seq.iter (printUnit r r.lang lm encodings di.srcDir)
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  -> 
        CreateMakeFile r r.lang di
        let arrsSrcTstFiles, arrsHdrTstFiles = DastTestCaseCreation.printAllTestCases r r.lang lm di.srcDir
        match r.lang with
        | C    -> 
            CreateCMainFile r  ProgrammingLanguage.C di.srcDir

            generateVisualStudtioProject r di.srcDir (arrsSrcTstFiles, arrsHdrTstFiles)
        | Ada  -> 
            ()



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