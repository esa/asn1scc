module GenerateFiles
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions


let getTypeDecl (r:DAst.AstRoot) (vasPU_name:string) (l:ProgrammingLanguage) (vas:ValueAssignment) =
    let t = vas.Type
    match t.Kind with
    | Integer _
    | Real _
    | Boolean _     -> t.typeDefintionOrReference.longTypedefName l
//        match t.tasInfo with
//        | Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName) 
//        | None            -> t.typeDefinition.typeDefinitionBodyWithinSeq
    | ReferenceType ref ->
        let tasName = ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value) 
        match l with
        | C     -> tasName
        | Ada ->
            match vasPU_name = ref.baseInfo.modName.Value with
            | true  -> tasName
            |false  -> (ToC ref.baseInfo.modName.Value) + "." + tasName

        
    | _             -> 
        match t.tasInfo with
        | Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName) 
        | None            -> t.typeDefintionOrReference.longTypedefName l //t.typeDefinition.name

let printValueAssignment (r:DAst.AstRoot) (vasPU_name:string) (l:ProgrammingLanguage)  (vas:ValueAssignment) =
    let sName = vas.c_name
    let t = vas.Type
    let sTypeDecl= getTypeDecl r vasPU_name l vas

    let sVal = DAstVariables.printValue r  l  vas.Type None vas.Value.kind
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

let private printUnit (r:DAst.AstRoot) (l:ProgrammingLanguage) (encodings: CommonTypes.Asn1Encoding list) outDir (pu:ProgramUnit)  =
    let tases = pu.sortedTypeAssignments
    
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
    //let typeDefs = tases |> List.choose(fun t -> t.getTypeDefinition l)
    let typeDefs = 
        tases |> 
        List.map(fun tas -> 
            let type_defintion = //tas.Type.typeDefinition.completeDefinition
                match tas.Type.typeDefintionOrReference with
                | TypeDefinition td -> td.typedefBody ()      
                | ReferenceToExistingDefinition _   -> raise(BugErrorException "Type Assignment with no Type Defintion")
            let init_def        = match l with C -> tas.Type.initFunction.initFuncDef | Ada -> None
            let equal_defs      = collectEqualFuncs tas.Type |> List.choose(fun ef -> ef.isEqualFuncDef)
            let isValid        = 
                match tas.Type.isValidFunction with
                | None      -> None
                | Some f    -> f.funcDef


            let uPerEncFunc = match requiresUPER with true -> tas.Type.uperEncFunction.funcDef | false -> None
            let uPerDecFunc = match requiresUPER with true -> tas.Type.uperDecFunction.funcDef | false -> None

            let acnEncFunc = 
                match requiresAcn, tas.Type.acnEncFunction with 
                | true, Some x -> x.funcDef
                | _  -> None
            let acnDecFunc = 
                match requiresAcn, tas.Type.acnDecFunction with 
                | true, Some x -> x.funcDef
                | _ -> None 

            let allProcs = equal_defs@([init_def;isValid;uPerEncFunc;uPerDecFunc;acnEncFunc; acnDecFunc] |> List.choose id)
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
                    header_c.PrintValueAssignment (typeDefinitionName) gv.c_name
                | ReferenceType ref ->
                    let typeDefinitionName = ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value)
//                        match l with
//                        | C     ->  
//                        | Ada   ->
//                            match ToC ref.baseInfo.modName.Value = pu.name with
//                            | true  -> ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value) 
//                            | false -> (ToC ref.baseInfo.modName.Value) + "." + ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value) 
                    header_c.PrintValueAssignment (typeDefinitionName) gv.c_name
                | _             -> 
                    let typeDefinitionName = match t.tasInfo with| Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName) | None    -> t.typeDefintionOrReference.longTypedefName l//t.typeDefinition.name
                    header_c.PrintValueAssignment (typeDefinitionName) gv.c_name
            | Ada   -> printValueAssignment r pu.name l gv)
    let arrsHeaderAnonymousValues =
        arrsAnonymousValues |>
        List.map(fun av -> 
            match l with
            | C     -> header_c.PrintValueAssignment av.typeDefinitionName av.valueName
            | Ada   -> 
                header_a.PrintValueAssignment av.valueName av.typeDefinitionName av.valueExpresion)
    

    let arrsPrototypes = []
    let defintionsContntent =
        match l with
        | C     -> 
            let arrsUtilityDefines = []
            header_c.PrintHeaderFile (ToC pu.name) pu.importedProgramUnits typeDefs (arrsValues@arrsHeaderAnonymousValues) arrsPrototypes arrsUtilityDefines
        | Ada   -> 
            let arrsPrivateChoices = []
            header_a.PrintPackageSpec pu.name pu.importedProgramUnits typeDefs (arrsValues@arrsHeaderAnonymousValues) arrsPrivateChoices

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
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.ACN) then
                        yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                } |> Seq.choose id |> Seq.toList
        let tetscase_specFileName = Path.Combine(outDir, pu.tetscase_specFileName)
        let tstCasesHdrContent =
            match l with
            | C     -> test_cases_c.PrintAutomaticTestCasesHeaderFile (ToC pu.tetscase_specFileName) pu.name typeDefs
            | Ada   -> test_cases_a.PrintCodecsFile_spec pu.name pu.importedProgramUnits typeDefs
        File.WriteAllText(tetscase_specFileName, tstCasesHdrContent.Replace("\r",""))
        
    //sourse file
    let arrsTypeAssignments = 
        tases |> List.map(fun t -> 
            let initialize        = match l with C -> t.Type.initFunction.initFunc | Ada -> None
            //let eqFuncs = collectEqualDeffinitions t |> List.choose(fun ef -> ef.isEqualFunc)
            let eqFuncs = collectEqualFuncs t.Type |> List.choose(fun ef -> ef.isEqualFunc)
            let isValid = match t.Type.isValidFunction with None -> None | Some isVal -> isVal.func
            let uperEncDec codec         =  
                match requiresUPER with
                | true  ->
                    match codec with
                    | CommonTypes.Encode    -> t.Type.uperEncFunction.func
                    | CommonTypes.Decode    -> t.Type.uperDecFunction.func
                | false -> None
            let ancEncDec codec         = 
                match requiresAcn with
                | true ->
                    match codec with
                    | CommonTypes.Encode    -> match t.Type.acnEncFunction with None -> None | Some x -> x.func
                    | CommonTypes.Decode    -> match t.Type.acnDecFunction with None -> None | Some x -> x.func
                | false     -> None
            let allProcs =  eqFuncs@([initialize; isValid;(uperEncDec CommonTypes.Encode); (uperEncDec CommonTypes.Decode);(ancEncDec CommonTypes.Encode); (ancEncDec CommonTypes.Decode)] |> List.choose id)
            match l with
            | C     ->  body_c.printTass allProcs 
            | Ada   ->  body_a.printTass allProcs )
    let eqContntent = 
        match l with
        | C     ->
            let arrsUnnamedVariables = []
            let arrsValueAssignments = vases |> List.map (printValueAssignment r pu.name l )
            let arrsSourceAnonymousValues = 
                arrsAnonymousValues |>
                List.map (fun av -> variables_c.PrintValueAssignment av.typeDefinitionName av.valueName av.valueExpresion)
            body_c.printSourceFile pu.name arrsUnnamedVariables (arrsValueAssignments@arrsSourceAnonymousValues) arrsTypeAssignments
        | Ada   ->
            let arrsNegativeReals = []
            let arrsBoolPatterns = []
            let arrsChoiceValueAssignments = []
            let rtl = [body_a.rtlModuleName()]
            body_a.PrintPackageBody pu.name  (rtl@pu.importedProgramUnits) arrsNegativeReals arrsBoolPatterns arrsTypeAssignments arrsChoiceValueAssignments pu.importedTypes
    let fileName = Path.Combine(outDir, pu.bodyFileName)
    File.WriteAllText(fileName, eqContntent.Replace("\r",""))

    //test cases sourse file
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  -> 
        let encDecFuncs = 
            seq {
                for tas in tases do
                
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.UPER) then
                        yield (tas.Type.uperEncDecTestFunc |> Option.map (fun z -> z.func))
                    if r.args.encodings |> Seq.exists ((=) CommonTypes.ACN) then
                        yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.func))
                } |> Seq.choose id |> Seq.toList

        let tetscase_SrcFileName = Path.Combine(outDir, pu.tetscase_bodyFileName)
    
        let tstCasesHdrContent =
            match l with
            | C     -> test_cases_c.PrintAutomaticTestCasesSourceFile pu.tetscase_specFileName pu.importedProgramUnits encDecFuncs
            | Ada   -> test_cases_a.PrintCodecsFile_body pu.name pu.importedProgramUnits [] encDecFuncs
        File.WriteAllText(tetscase_SrcFileName, tstCasesHdrContent.Replace("\r",""))

let TestSuiteFileName = "testsuite"


let CreateCMainFile (r:AstRoot)  (l:ProgrammingLanguage) outDir  =
    //Main file for test cass    
    let printMain = match l with C -> test_cases_c.PrintMain | Ada -> test_cases_c.PrintMain
    let content = printMain TestSuiteFileName
    let outFileName = Path.Combine(outDir, "mainprogram.c")
    File.WriteAllText(outFileName, content.Replace("\r",""))




let CreateMakeFile (r:AstRoot) (l:ProgrammingLanguage) outDir  =
    match l with
    | C ->
        let files = r.Files |> Seq.map(fun x -> x.FileNameWithoutExtension.ToLower() )
        let content = aux_c.PrintMakeFile files
        let outFileName = Path.Combine(outDir, "Makefile")
        File.WriteAllText(outFileName, content.Replace("\r",""))
    | Ada ->
        let mods = aux_a.rtlModuleName()::(r.programUnits |> List.map(fun pu -> pu.name ))
        let content = aux_a.PrintMakeFile  mods
        let outFileName = Path.Combine(outDir, "Makefile")
        File.WriteAllText(outFileName, content.Replace("\r",""))


let private CreateAdaIndexFile (r:AstRoot) bGenTestCases outDir =
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

let private CreateAdaMain (r:AstRoot) bGenTestCases outDir =
    let content = aux_a.PrintMain (r.programUnits |> List.map(fun x -> (ToC x.name).ToLower()) )
    let outFileName = Path.Combine(outDir, "mainprogram.adb")
    File.WriteAllText(outFileName, content.Replace("\r",""))



let CreateTestSuiteFile (r:AstRoot) (l:ProgrammingLanguage) outDir vasName =
    let generate_dat_file  = match l with C -> test_cases_c.PrintSuite_call_codec_generate_dat_file | Ada -> test_cases_a.PrintMain_call_codec_generate_dat_file
    let call_codec =         match l with C -> test_cases_c.PrintSuite_call_codec                   | Ada -> test_cases_a.PrintMain_call_codec
    let call_codec2 =         match l with C -> test_cases_c.PrintSuite_call_codec2                   | Ada -> test_cases_a.PrintMain_call_codec2

    let GetEncodingString = function    
        | UPER  -> match l with C -> "" | Ada -> "UPER_"
        | ACN   -> "ACN_"
        | BER   -> "BER_"
        | XER   -> "XER_"
    
    let includedPackages =  
        match l with
        | C     -> r.programUnits |> Seq.map(fun x -> x.tetscase_specFileName)
        | Ada   -> r.programUnits |> Seq.collect(fun x -> [x.name; x.tetscase_name])


    let rec gAmber (t:Asn1Type) = 
        match t.Kind with
        | Integer      _ -> ""  , "&"
        | Real         _ -> ""  , "&"
        | IA5String    _ -> ""  , ""
        | OctetString  _ -> "&" , "&"
        | NullType     _ -> ""  , "&"
        | BitString    _ -> "&" , "&"
        | Boolean      _ -> ""  , "&"
        | Enumerated   _ -> ""  , "&"
        | SequenceOf   _ -> "&" , "&"
        | Sequence     _ -> "&" , "&"
        | Choice       _ -> "&" , "&"
        | ReferenceType r -> gAmber r.resolvedType

    let GetDatFile (v:ValueAssignment) modName sTasName encAmper (enc:Asn1Encoding) = 
        let bGenerateDatFile = (r.args.CheckWithOss && v.Name.Value = "testPDU")
        match bGenerateDatFile, enc with
        | false,_     -> ""
        | true, ACN   -> ""
        | true, XER   -> generate_dat_file modName sTasName encAmper (GetEncodingString enc) "Byte"
        | true, BER   -> generate_dat_file modName sTasName encAmper (GetEncodingString enc) "Byte"
        | true, uPER  -> generate_dat_file modName sTasName encAmper (GetEncodingString enc) "Bit"

    let PrintTestCase (v:ValueAssignment) (m:Asn1Module) (sTasName : string)  (idx :int) initFuncName (uperEncDecTestFunc  : EncodeDecodeTestFunc option) (acnEncDecTestFunc   : EncodeDecodeTestFunc option) =
        let modName = ToC m.Name.Value
        let encAmper, initAmper = gAmber v.Type
        let sValue = DAstVariables.printValue r l  v.Type None v.Value.kind
        let sTestCaseIndex = idx.ToString()
        let bStatic = match v.Type.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
        let GetDatFile = GetDatFile v modName sTasName encAmper
         
        r.args.encodings |> Seq.map(fun e -> 
                                        match e with
                                        | Asn1Encoding.UPER  -> 
                                            match uperEncDecTestFunc with
                                            | Some _    -> call_codec modName sTasName encAmper (GetEncodingString e) sValue sTestCaseIndex (ToC v.Name.Value) bStatic (GetDatFile e) initFuncName initAmper
                                            | None      -> ""
                                        | Asn1Encoding.ACN   -> 
                                            match acnEncDecTestFunc with
                                            | Some _    -> call_codec modName sTasName encAmper (GetEncodingString e) sValue sTestCaseIndex (ToC v.Name.Value) bStatic (GetDatFile e) initFuncName initAmper
                                            | _         -> ""
                                        | Asn1Encoding.XER   -> call_codec modName sTasName encAmper (GetEncodingString e) sValue sTestCaseIndex (ToC v.Name.Value) bStatic (GetDatFile e) initFuncName initAmper
                                        | Asn1Encoding.BER   -> call_codec modName sTasName encAmper (GetEncodingString e) sValue sTestCaseIndex (ToC v.Name.Value) bStatic (GetDatFile e) initFuncName initAmper
                                 ) |> Seq.StrJoin "\n\n"
    
    let PrintTestCase2 (initStatement:String) (m:Asn1Module) (t:Asn1Type) (sTasName : string)  (idx :int) initFuncName (uperEncDecTestFunc  : EncodeDecodeTestFunc option) (acnEncDecTestFunc   : EncodeDecodeTestFunc option) =
        let modName = ToC m.Name.Value
        let encAmper, initAmper = gAmber t
        let bStatic = match t.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
        let GetDatFile = ""
         
        r.args.encodings |> Seq.map(fun e -> 
                                        match e with
                                        | Asn1Encoding.UPER  -> 
                                            let sTestCaseIndex = idx.ToString()
                                            match uperEncDecTestFunc with
                                            | Some _    -> call_codec2 modName sTasName encAmper (GetEncodingString e) initStatement sTestCaseIndex sTestCaseIndex bStatic "" initFuncName initAmper
                                            | None      -> ""
                                        | Asn1Encoding.ACN   -> 
                                            let sTestCaseIndex = (idx+1).ToString()
                                            match acnEncDecTestFunc with
                                            | Some _    -> call_codec2 modName sTasName encAmper (GetEncodingString e) initStatement sTestCaseIndex sTestCaseIndex bStatic "" initFuncName initAmper
                                            | _         -> ""
                                        | Asn1Encoding.XER   -> 
                                            let sTestCaseIndex = idx.ToString()
                                            call_codec2 modName sTasName encAmper (GetEncodingString e) initStatement sTestCaseIndex sTestCaseIndex bStatic "" initFuncName initAmper
                                        | Asn1Encoding.BER   -> 
                                            let sTestCaseIndex = idx.ToString()
                                            call_codec2 modName sTasName encAmper (GetEncodingString e) initStatement sTestCaseIndex sTestCaseIndex bStatic "" initFuncName initAmper
                                 ) |> Seq.StrJoin "\n\n"
    let mutable idx = 0;
    let funcs, localVars = 
        seq {
            for m in r.Files |> List.collect(fun f -> f.Modules) do
                for v in m.ValueAssignments do
                        if v.Name.Value = "apid" then
                            ()
                        if vasName = "ALL" || v.Name.Value = vasName then
                            idx <- idx + 2
                            let initFuncName = v.Type.initFunction.initFuncName
                            let aaa = PrintTestCase v m (getTypeDecl r (ToC m.Name.Value) l v )  idx initFuncName v.Type.uperEncDecTestFunc v.Type.acnEncDecTestFunc
                            yield (aaa, [])
                if vasName = "ALL" then
                    for t in m.TypeAssignments do
                        let hasEncodeFunc = hasAcnEncodeFunction t.Type.acnEncFunction t.Type.acnParameters 
                        if hasEncodeFunc then
//                            for v in t.Type.automaticTestCasesValues do
//                                let vas = {ValueAssignment.Name = StringLoc.ByValue ""; c_name = ""; ada_name = ""; Type = t.Type; Value = v}
//                                idx <- idx + 1
//                                let initFuncName = t.Type.initFunction.initFuncName
//                                yield PrintTestCase vas m (ToC2(r.args.TypePrefix + t.Name.Value) ) idx initFuncName t.Type.uperEncDecTestFunc t.Type.acnEncDecTestFunc
                            for func in t.Type.initFunction.initFuncBodyTestCases  do
                                //let vas = {ValueAssignment.Name = StringLoc.ByValue ""; c_name = ""; ada_name = ""; Type = t.Type; Value = v}
                                let p = {CallerScope.modName = ToC "MainProgram"; arg = VALUE "tc_data"}
                                let initStatement = func p
                                idx <- idx + 2
                                let initFuncName = t.Type.initFunction.initFuncName
                                let ret = PrintTestCase2 initStatement.funcBody m t.Type (ToC2(r.args.TypePrefix + t.Name.Value) ) idx initFuncName t.Type.uperEncDecTestFunc t.Type.acnEncDecTestFunc
                                yield (ret, initStatement.localVariables)
                                
        }  |>  Seq.toList |> List.unzip

    let arrsVars = 
        localVars |> List.collect id |> List.map(fun lv -> lv.GetDeclaration l) |> Seq.distinct |> Seq.toList
        
    match l with
    | C ->
        let contentC = test_cases_c.PrintTestSuiteSource TestSuiteFileName includedPackages arrsVars funcs
        let outCFileName = Path.Combine(outDir, TestSuiteFileName + "." + l.BodyExtention)
        File.WriteAllText(outCFileName, contentC.Replace("\r",""))

        let contentH = test_cases_c.PrintTestSuiteHeader()
        let outHFileName = Path.Combine(outDir, TestSuiteFileName + "." + l.SpecExtention)
        File.WriteAllText(outHFileName, contentH.Replace("\r",""))
    | Ada ->
        let contentC = test_cases_a.PrintMain includedPackages arrsVars funcs [] [] false
        let outCFileName = Path.Combine(outDir, "mainprogram." + l.BodyExtention)
        File.WriteAllText(outCFileName, contentC.Replace("\r",""))
        


let generateVisualStudtioProject (r:DAst.AstRoot) outDir =
    //generate Visual Studio project file
    let vcprjContent = xml_outputs.emitVisualStudioProject 
                        (r.programUnits |> List.map (fun z -> z.bodyFileName))
                        (r.programUnits |> List.map (fun z -> z.specFileName))
                        (r.programUnits |> List.map (fun z -> z.tetscase_bodyFileName))
                        (r.programUnits |> List.map (fun z -> z.tetscase_specFileName))
    let vcprjFileName = Path.Combine(outDir, "VsProject.vcxproj")
    File.WriteAllText(vcprjFileName, vcprjContent)

    //generate Visual Studio Solution file
    File.WriteAllText((Path.Combine(outDir, "VsProject.sln")), (aux_c.emitVisualStudioSolution()))


let generateAll outDir (r:DAst.AstRoot) (encodings: CommonTypes.Asn1Encoding list)  =
    r.programUnits |> Seq.iter (printUnit r r.lang encodings outDir)
    //print extra such make files etc
    //print_debug.DoWork r outDir "debug.txt"
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  -> 
        CreateMakeFile r r.lang outDir
        match r.lang with
        | C    -> 
            CreateCMainFile r  ProgrammingLanguage.C outDir
            CreateTestSuiteFile r ProgrammingLanguage.C outDir "ALL"
            generateVisualStudtioProject r outDir
        | Ada  -> 
            //CreateAdaMain r false outDir
            CreateTestSuiteFile r ProgrammingLanguage.Ada outDir "ALL"

            CreateAdaIndexFile r false outDir



let EmmitDefaultACNGrammar (r:AstRoot) outDir  =
    let printTas (tas: TypeAssignment) =
        tas.Name.Value + "[]"
    let printModule (m:Asn1Module) =
        let arrTases = m.TypeAssignments |> Seq.map printTas
        uper_c.PrintDefaultAcnModule m.Name.Value arrTases "::="
    let printFile (f:Asn1File) =
        let fileName = f.FileNameWithoutExtension + ".acn"
        if (System.IO.File.Exists fileName) then
            System.Console.Error.WriteLine("File {0} already exists. Creation of default ASN.1 grammar abandoned", fileName);
        else
            let content = f.Modules |> Seq.map printModule |> Seq.StrJoin "\n"
            let fileName = Path.Combine(outDir, fileName)
            File.WriteAllText(fileName, content.Replace("\r",""))
    r.Files |> Seq.iter printFile