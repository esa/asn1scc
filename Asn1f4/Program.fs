open Argu
open FsUtils
open System
open System.IO
open CommonTypes
open System.Resources
open Antlr

type CliArguments =
    | [<AltCommandLine("-c")>]C_lang 
    | [<AltCommandLine("-Ada")>]Ada_Lang
    | [<AltCommandLine("-uPER")>]UPER_enc
    | [<AltCommandLine("-XER")>]XER_enc
    | [<AltCommandLine("-ACN")>]ACN_enc
    | [<AltCommandLine("-atc")>]Auto_test_cases
    | [<AltCommandLine("-o")>]Out of dir:string
    | [<AltCommandLine("-equal")>]Equal_Func 
    | [<AltCommandLine("-typePrefix")>]Type_Prefix of prefix:string
    | [<AltCommandLine("-x")>] Xml_Ast of xmlFilename:string
    | [<AltCommandLine("-renamePolicy")>] Rename_Policy of int
    | [<AltCommandLine("-gtc")>] Generate_Test_Grammar 
    | [<AltCommandLine("-customStg")>] Custom_Stg  of custom_stg_colon_outfilename:string
    | [<AltCommandLine("-customStgAstVersion")>] Custom_Stg_Ast_Version  of astver:int

    | [<AltCommandLine("-icdUper")>] IcdUper  of uper_icd_output_file:string
    | [<AltCommandLine("-customIcdUper")>] CustomIcdUper  of custom_stg_colon_out_filename:string
    | [<AltCommandLine("-icdAcn")>] IcdAcn  of acn_icd_output_file:string
    | [<AltCommandLine("-customIcdAcn")>] CustomIcdAcn  of custom_stg_colon_out_filename:string
    | [<AltCommandLine("-AdaUses")>] AdaUses 
    | [<AltCommandLine("-ACND")>] ACND  
    | [<AltCommandLine("-v")>]   Version
    | [<MainCommand; ExactlyOnce; Last>] Files of files:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version          -> "displays version information"
            | C_lang           -> "generate code for the C/C++ programming language"
            | Ada_Lang         -> "generate code for the Ada/SPARK programming language"
            | UPER_enc         -> "generates encoding and decoding functions for unaligned Packed Encoding Rules (uPER)"
            | XER_enc          -> "generates encoding and decoding functions for XML Encoding Rules (XER)"
            | ACN_enc          -> "generates encoding and decoding functions using the ASSERT ASN.1 encoding Control Notation"
            | Auto_test_cases  -> "create automatic test cases."
            | Out (_)          -> "directory where all files are produced."
            | Files (_)        -> "List of ASN.1 and ACN files to process."
            | Type_Prefix _    -> "adds 'prefix' to all generated C or Ada/SPARK data types."
            | Equal_Func       -> "generate functions for testing type equality."
            | Xml_Ast _        -> "dump internal AST in an xml file"
            | Rename_Policy _  -> "Specify rename policy for enums 0 no rename (Ada default), 1 rename only conflicting enumerants (C default), 2 rename all enumerants of an enum with at lest one conflicting enumerant"
            | Generate_Test_Grammar -> "generate a sample grammar for testing purposes"
            | Custom_Stg _   -> "custom_stg_colon_outfilename is expected as stgFile.stg:outputFile where stgFile.stg is an existing custom stg file, while outputFile is the name of the generated file. Invokes the custom stg file 'stgFile.stg' and produces the output file 'outputFile'"
            | Custom_Stg_Ast_Version _ -> "1 = original AST, 4: like version of asn1scc where inner types are replaced with referenced types"
            | IcdUper  _        -> "Produces an Interface Control Document for the input ASN.1 grammar for uPER encoding"    
            | CustomIcdUper  _  -> "Invokes the custom stg file 'stgFile.stg' using the icdUper backend and produces the output file 'outputFile'"
            | IcdAcn  _         -> "Produces an Interface Control Document for the input ASN.1 and ACN grammars for ACN encoding"
            | CustomIcdAcn  _   -> "Invokes the custom stg file 'stgFile.stg' using the icdAcn backend and produces the output file 'outputFile'"
            | AdaUses           -> "Prints in the console all type Assignments of the input ASN.1 grammar"
            | ACND              -> "creates ACN grammars for the input ASN.1 grammars using the default encoding properties"

let getCustmStgFileNames (compositeFile:string) =
    let files = compositeFile.Split ':' |> Seq.toList
    match files  with
    | stg::out::[]  -> Some(stg,out)
    | _             ->
        let files = compositeFile.Split([|"::"|], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList
        match files with
        | stg::out::[]  -> Some(stg,out)
        | _             -> None
     

let checkOutFileName (fileName:string) extension cmdoption =    
    match fileName.ToLower().EndsWith extension with
    | true  -> ()
    | false -> 
        let msg = sprintf "Invalid output filename '%s' in option %s \nGenerated file must have an .%s extension." fileName cmdoption extension
        raise (UserException msg)

let checkCompositeFile comFile cmdoption extention=
    match getCustmStgFileNames comFile with
    | Some(stgFile, outFile)  -> 
        match System.IO.File.Exists stgFile with
        | true  -> ()
        | false -> 
            let msg = sprintf "Custom stg file '%s' not found"  stgFile
            raise (UserException msg)
    | None -> 
        let msg = sprintf "Invalid argument for '%s' option. Expected format is  stgFile.stg:outputFile.\nEg -%s custom.stg:generated.%s\nUnder windows, you may user double :: to separate the stg file with output fileE.g. %s c:\\custom.stg::c:\\generated.%s" cmdoption cmdoption extention cmdoption extention
        raise (UserException msg)


let checkArguement arg =
    match arg with
    | Version          -> ()
    | C_lang           -> ()
    | Ada_Lang         -> ()
    | UPER_enc         -> ()
    | XER_enc         -> ()
    | ACN_enc          -> ()
    | Auto_test_cases  -> ()
    | Equal_Func       -> ()
    | Generate_Test_Grammar -> ()
    | Xml_Ast xmlFileName   -> checkOutFileName xmlFileName ".xml" "-x"
    | Out outDir       -> 
        match System.IO.Directory.Exists outDir with
        | true  -> ()
        | false -> raise (UserException (sprintf "directory '%s' does not exist." outDir))
    | Files files      -> 
        files |> Seq.iter(fun f -> match File.Exists f with true -> () | false -> raise (UserException (sprintf "File '%s' does not exist." f)))
    | Type_Prefix _    -> ()
    | Rename_Policy _   -> ()
    | Custom_Stg comFile  -> checkCompositeFile comFile "-customStg" "txt"
    | Custom_Stg_Ast_Version v -> 
        match v with
        | 1 -> ()
        | 4 -> ()
        | _ -> raise (UserException ("invalid value for argument -customStgAstVersion. Currently only values 1 and 4 are supported"))
    | IcdUper  outHtmlFile      -> checkOutFileName outHtmlFile ".html" "-icdUper"
    | CustomIcdUper  comFile    -> checkCompositeFile comFile "-customIcdUper" ".html"
    | IcdAcn  outHtmlFile       -> checkOutFileName outHtmlFile ".html" "-icdAcn"
    | CustomIcdAcn  comFile     -> checkCompositeFile comFile "-customIcdAcn" ".html"
    | AdaUses                   -> ()
    | ACND                      -> ()

let createInput (fileName:string) : Input = 
    {
        name = fileName
        contents = File.OpenRead(fileName)
    }

let constructCommandLineSettings args (parserResults: ParseResults<CliArguments>)=
    let files = parserResults.GetResult <@ Files @>
    {
        CommandLineSettings.asn1Files = files |> List.filter(fun a -> (a.ToLower().EndsWith(".asn1")) || (a.ToLower().EndsWith(".asn")) ) |> List.map createInput
        acnFiles  = files |> List.filter(fun a -> (a.ToLower().EndsWith(".acn")) ) |> List.map createInput
        encodings = 
            args |> 
            List.choose(fun arg ->
                match arg with
                |UPER_enc -> Some CommonTypes.Asn1Encoding.UPER
                |XER_enc  -> Some CommonTypes.Asn1Encoding.XER
                |ACN_enc  -> Some CommonTypes.Asn1Encoding.ACN
                | _       -> None )

        GenerateEqualFunctions = parserResults.Contains<@ Equal_Func @> || parserResults.Contains<@ Auto_test_cases @>
        generateAutomaticTestCases = parserResults.Contains<@ Auto_test_cases @>
        TypePrefix = parserResults.GetResult(<@ Type_Prefix@>, defaultValue = "")
        CheckWithOss = false
        AstXmlAbsFileName = parserResults.GetResult(<@Xml_Ast@>, defaultValue = "")
        IcdUperHtmlFileName = ""
        IcdAcnHtmlFileName = ""
        custom_Stg_Ast_Version = parserResults.GetResult(<@ Custom_Stg_Ast_Version @>, defaultValue = 1)
        mappingFunctionsModule = None
        integerSizeInBytes = 8
        renamePolicy = CommonTypes.EnumRenamePolicy.SelectiveEnumerants
    }    


let exportRTL outDir  (l:DAst.ProgrammingLanguage) (args:CommandLineSettings)=
    let writeTextFile fileName (content:String) =
        System.IO.File.WriteAllText(fileName, content.Replace("\r",""))
    let rm = new ResourceManager("Resource1", System.Reflection.Assembly.GetExecutingAssembly());
    match l with
    | DAst.ProgrammingLanguage.C ->
                writeTextFile (Path.Combine(outDir, "asn1crt.c")) (rm.GetString("asn1crt",null)) 
                writeTextFile (Path.Combine(outDir, "asn1crt.h")) (rm.GetString("asn1crt1",null)) 
                writeTextFile (Path.Combine(outDir, "acn.c"))     (rm.GetString("Acn",null)) 
                writeTextFile (Path.Combine(outDir, "real.c"))    (rm.GetString("real",null)) 
                match args.encodings |> Seq.exists ((=) Asn1Encoding.XER) with
                | true  -> writeTextFile (Path.Combine(outDir, "xer.c"))  (rm.GetString("xer",null)) 
                | false -> ()
    | DAst.ProgrammingLanguage.Ada ->
                writeTextFile (Path.Combine(outDir, "adaasn1rtl.adb")) (rm.GetString("adaasn1rtl_adb",null)) 
                writeTextFile (Path.Combine(outDir, "adaasn1rtl.ads")) (rm.GetString("adaasn1rtl_ads",null)) 
                writeTextFile (Path.Combine(outDir, "IgnoredExaminerWarnings.wrn"))     (rm.GetString("IgnoredExaminerWarnings",null)) 
                writeTextFile (Path.Combine(outDir, "gnat.cfg"))    (rm.GetString("gnat",null)) 
                writeTextFile (Path.Combine(outDir, "runSpark.sh"))    (rm.GetString("run",null)) 
                writeTextFile (Path.Combine(outDir, "GPS_project.gpr"))    (rm.GetString("GPS_project",null)) 
                match args.encodings |> Seq.exists ((=) Asn1Encoding.XER) with
                | true  -> 
                    writeTextFile (Path.Combine(outDir, "xer_rtl.adb")) (rm.GetString("xer_rtl_adb",null)) 
                    writeTextFile (Path.Combine(outDir, "xer_rtl.ads")) (rm.GetString("xer_rtl_ads",null)) 
                | false -> ()


let main0 argv =
    
    let parser = ArgumentParser.Create<CliArguments>(programName = "Asn1f4.exe")
    try
        let parserResults = parser.Parse argv
        let cliArgs = parserResults.GetAllResults()
        cliArgs |> Seq.iter checkArguement 
        let args = constructCommandLineSettings cliArgs parserResults
        
        // create front ent ast
        let frontEntAst, acnDeps = FrontEntMain.constructAst args
        
        // print front ent ast as xml 
        match args.AstXmlAbsFileName with
        | ""    -> ()
        | _     -> 
            ExportToXml.exportFile frontEntAst acnDeps args.AstXmlAbsFileName


        // construct backend ast
        let backends = 
            cliArgs |> 
            List.choose (fun a -> 
                match a with
                | C_lang                -> Some (DAstConstruction.DoWork frontEntAst acnDeps CommonTypes.ProgrammingLanguage.C args.encodings)
                | Ada_Lang              -> Some (DAstConstruction.DoWork frontEntAst acnDeps CommonTypes.ProgrammingLanguage.Ada args.encodings)
                | _             -> None)

        //generate code
        let outDir = parserResults.GetResult(<@Out@>, defaultValue = ".")
        backends |> 
            Seq.iter (fun r -> 
                GenerateFiles.generateAll outDir r args.encodings
                exportRTL outDir r.lang args
                match args.AstXmlAbsFileName with
                | ""    -> ()
                | _     -> DAstExportToXml.exportFile r acnDeps ("backend_" + args.AstXmlAbsFileName)
                )
        
        //custom stgs code generation
        
        // let AST for custom STGs
        // if there is an AST from Ada or C use it, otherwise create a new one

        let r = 
            match backends with
            | []    -> DAstConstruction.DoWork frontEntAst acnDeps CommonTypes.ProgrammingLanguage.C args.encodings
            | x::_  -> x
        

        cliArgs |> 
            //List.choose (fun a -> match a with | Custom_Stg compFile   -> Some compFile | _ -> None) |>
            Seq.iter(fun arg -> 
                match arg with
                | Custom_Stg comFile ->
                    match getCustmStgFileNames comFile with
                    | Some(stgFile, outFile)  -> CustomStgExport.exportFile r acnDeps stgFile outFile
                    | None  -> ()
                | IcdUper outFile       -> GenerateUperIcd.DoWork r "icdtemplate_uper.stg" outFile
                | CustomIcdUper comFile ->
                    match getCustmStgFileNames comFile with
                    | Some(stgFile, outFile)  -> GenerateUperIcd.DoWork r stgFile outFile
                    | None  -> ()
                | IcdAcn outFile       -> GenerateAcnIcd.DoWork r  acnDeps "icdtemplate_acn.stg" outFile
                | CustomIcdAcn comFile ->
                    match getCustmStgFileNames comFile with
                    | Some(stgFile, outFile)  -> 
                        GenerateAcnIcd.DoWork r  acnDeps stgFile outFile
                    | None  -> ()
                | AdaUses   -> DAstUtilFunctions.AdaUses r
                | ACND      -> GenerateFiles.EmmitDefaultACNGrammar r outDir
                | Version   -> Antlr.VersionInformation.printGitVersion()
                | _ -> ())

        cliArgs |> 
            List.filter (fun a -> match a with | Generate_Test_Grammar    -> true | _  -> false) |>
            Seq.iter(fun _ -> GrammarGenerator.generateGrammars outDir)

        0
    with
        | :? Argu.ArguParseException as ex -> 
            match argv.Length = 1 && (argv.[0] = "-v" || argv.[0] = "--version") with
            | true -> 
                Antlr.VersionInformation.printGitVersion ()
                0
            | false ->
                Console.Error.WriteLine(ex.Message)
                1    
        | UserException msg            ->
            Console.Error.WriteLine(msg)
            2
        | :? Antlr.Asn1.asn1Parser.SyntaxErrorException as ex            ->
            Console.Error.WriteLine(ex.Message)
            2
        | SemanticError (loc,msg)            ->
            Console.Error.WriteLine(FrontEntMain.formatSemanticError loc msg)
            3
        | ex            ->
            Console.Error.WriteLine(ex.Message)
            Console.Error.WriteLine(ex.StackTrace)
            4

    

[<EntryPoint>]
let main argv = 
    main0 argv
    