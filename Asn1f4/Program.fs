open Argu
open FsUtils
open System
open System.Numerics
open System.IO
open CommonTypes
open System.Resources
open Antlr

type CliArguments =
    | [<Unique; AltCommandLine("-c")>]C_lang 
    | [<Unique; AltCommandLine("-Ada")>]Ada_Lang
    | [<Unique; AltCommandLine("-uPER")>]UPER_enc
    | [<Unique; AltCommandLine("-XER")>]XER_enc
    | [<Unique; AltCommandLine("-ACN")>]ACN_enc
    | [<Unique; AltCommandLine("-atc")>]Auto_test_cases
    | [<Unique; AltCommandLine("-o")>]Out of dir:string
    | [<Unique; AltCommandLine("-equal")>]Equal_Func 
    | [<Unique; AltCommandLine("-x")>] Xml_Ast of xmlFilename:string
    | [<Unique; AltCommandLine("-typePrefix")>]Type_Prefix of prefix:string
    | [<Unique; AltCommandLine("-renamePolicy")>] Rename_Policy of int
    | [<Unique; AltCommandLine("-fp")>]  Field_Prefix of prefix:string
    | [<Unique; AltCommandLine("-gtc")>] Generate_Test_Grammar 
    | [<AltCommandLine("-customStg")>] Custom_Stg  of custom_stg_colon_outfilename:string
    | [<Unique; AltCommandLine("-customStgAstVersion")>] Custom_Stg_Ast_Version  of astver:int
    | [<Unique; AltCommandLine("-icdUper")>] IcdUper  of uper_icd_output_file:string
    | [<Unique; AltCommandLine("-customIcdUper")>] CustomIcdUper  of custom_stg_colon_out_filename:string
    | [<Unique; AltCommandLine("-icdAcn")>] IcdAcn  of acn_icd_output_file:string
    | [<Unique; AltCommandLine("-customIcdAcn")>] CustomIcdAcn  of custom_stg_colon_out_filename:string
    | [<Unique; AltCommandLine("-AdaUses")>] AdaUses 
    | [<Unique; AltCommandLine("-ACND")>] ACND  
    | [<Unique; AltCommandLine("-wordSize")>] Word_Size  of wordSize:int
    | [<Unique; AltCommandLine("-fpWordSize")>]  Fp_Word_Size of fpWordSize:int
    | [<Unique; AltCommandLine("-v")>]   Version
    | [<Unique; AltCommandLine("-asn1")>]   Debug_Asn1 of string option
    | [<Unique; AltCommandLine("-mfm")>]   Mapping_Functions_Module of string 
    | [<Unique; AltCommandLine("-debug")>]   Debug
    | [<MainCommand; ExactlyOnce; Last>] Files of files:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version          -> "displays version information"
            | Debug            -> "Option used internally for debugging"
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
            | Rename_Policy _  -> """Specify rename policy for Enumerated values. Allowed values are:
    0 perform no rename (Ada default).
    1 rename only conflicting enumerants (C default). 
      E.g. In a grammar that contains 
        RGB ::= ENUMERATED {red, green, blue} 
        FavColors = ENUMERATED {red, yellow} 
      only the red enumerant will be renamed to 
      RGB_red and FavColors_red. 
    2 rename all enumerants of an enumerated type 
      that has least one conflicting enumerant.
    3 all enumerants of all of an enumerated types
      are renamed.
"""
            | Field_Prefix _     -> """  Apply <prefix> string to any component or alternative fields present in the grammar.
  If <prefix> is AUTO (i.e. -fp AUTO) then only the conflicting component or alternative names will be prefixed with the type name.
"""

            | Generate_Test_Grammar -> "generate a sample grammar for testing purposes. Experimental ..."
            | Custom_Stg _   -> "custom_stg_colon_outfilename is expected as stgFile.stg:outputFile where stgFile.stg is an existing custom stg file, while outputFile is the name of the generated file. Invokes the custom stg file 'stgFile.stg' and produces the output file 'outputFile'"
            | Custom_Stg_Ast_Version _ -> "1 = original AST, 4: like version of asn1scc where inner types are replaced with referenced types"
            | IcdUper  _        -> "Produces an Interface Control Document for the input ASN.1 grammar for uPER encoding"    
            | CustomIcdUper  _  -> "Invokes the custom stg file 'stgFile.stg' using the icdUper backend and produces the output file 'outputFile'"
            | IcdAcn  _         -> "Produces an Interface Control Document for the input ASN.1 and ACN grammars for ACN encoding"
            | CustomIcdAcn  _   -> "Invokes the custom stg file 'stgFile.stg' using the icdAcn backend and produces the output file 'outputFile'"
            | AdaUses           -> "Prints in the console all type Assignments of the input ASN.1 grammar"
            | ACND              -> "creates ACN grammars for the input ASN.1 grammars using the default encoding properties"
            | Debug_Asn1  _     -> "Prints all input ASN.1 grammars in a single module/single file and with parameterized types removed. Used for debugging purposes"
            | Word_Size _       -> "Applicable only to C.Defines the size of asn1SccSint and asn1SccUint types. Valid values are 8 bytes (default) and 4 bytes. If you pass 4 then you should compile the C code -DWORD_SIZE=4."
            | Fp_Word_Size _     -> "Defines the size of the REAL type. Valid values are 8 bytes (default) which corresponds to double and 4 bytes which corresponds to float. If you pass 4 then you should compile the C code -DFP_WORD_SIZE=4."
            | Mapping_Functions_Module _    -> "The name of Ada module or name of C header file (without extension) containing the definitins of mapping functions"


let printVersion () =
    //let assembly = System.Reflection.Assembly.GetExecutingAssembly();
    //let fvi = System.Diagnostics.FileVersionInfo.GetVersionInfo(assembly.Location);
    //let version = fvi.FileVersion;
    let version = "4.2.1.1f"
    printfn "asn1scc version %s\n" version
    ()    

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
    | Debug            -> ()
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
        files |>        
        Seq.groupBy id |> 
        Seq.filter(fun (n,dups) -> Seq.length dups > 1) |> 
        Seq.iter(fun (file,_) -> 
            let errMsg = sprintf "Duplicate Input file. File '%s' was provided twice in the command line"  file
            raise (SemanticError (emptyLocation, errMsg))) 
    | Type_Prefix _    -> ()
    | Rename_Policy rp   -> 
        match rp with
        | 0 | 1 | 2 | 3-> ()
        | _             -> raise (UserException ("invalid value for argument -renamePolicy. Currently only values 0,1,2,3 are supported"))
    | Field_Prefix vl ->
            match vl.ToCharArray().[0] with
            | _ when (vl.ToCharArray().[0] >= 'A' && vl.ToCharArray().[0] <= 'Z') || (vl.ToCharArray().[0] >= 'a' && vl.ToCharArray().[0] <= 'z')  ||  vl.ToCharArray().[0] = '_' -> ()
            | _ -> raise (UserException ("invalid prefix value. Prefix value must start with a letter"))
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
    | Debug_Asn1  _             -> ()
    | Word_Size  ws             -> 
        match ws with
        | _ when ws = 4 -> ()
        | _ when ws = 8 -> ()
        | _  -> raise (UserException ("invalid value for argument -wordSize. Currently only values 4 and 8 are supported"))
    | Fp_Word_Size ws           ->
        match ws with
        | _ when ws = 4 -> ()
        | _ when ws = 8 -> ()
        | _  -> raise (UserException ("invalid value for argument -fpWordSize. Currently only values 4 and 8 are supported"))
    | Mapping_Functions_Module mfm  -> ()

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
        mappingFunctionsModule = parserResults.TryGetResult(<@ Mapping_Functions_Module @>)
        integerSizeInBytes = 
            let ws = parserResults.GetResult(<@Word_Size@>, defaultValue = 8)
            BigInteger ws
        floatingPointSizeInBytes =
            let fws = parserResults.GetResult(<@Fp_Word_Size@>, defaultValue = 8)
            BigInteger fws
            
        renamePolicy = 
            match args |> List.choose (fun a -> match a with Rename_Policy rp -> Some rp | _ -> None) with
            | []    ->
                match args |> List.filter(fun a -> a = C_lang || a = Ada_Lang) with
                | C_lang::[] ->  CommonTypes.EnumRenamePolicy.SelectiveEnumerants
                | Ada_Lang::[]  -> CommonTypes.EnumRenamePolicy.NoRenamePolicy
                | []            -> CommonTypes.EnumRenamePolicy.SelectiveEnumerants
                | _             -> raise (UserException ("Please select only one of target languages, not both."))
            | rp::_    ->
                match rp with
                | _ when rp = 0 -> CommonTypes.EnumRenamePolicy.NoRenamePolicy
                | _ when rp = 1 -> CommonTypes.EnumRenamePolicy.SelectiveEnumerants
                | _ when rp = 2 -> CommonTypes.EnumRenamePolicy.AllEnumerants
                | _ when rp = 3 -> CommonTypes.EnumRenamePolicy.AlwaysPrefixTypeName
                | _             -> raise (UserException ("invalid value for argument -renamePolicy. Currently only values 0,1,2,3 are supported"))
        fieldPrefix =
            match args |> List.choose(fun a -> match a with Field_Prefix vl -> Some (vl) | _ -> None) with
            | []    -> None
            | vl::_  ->
                match vl with
                | _ when vl = "AUTO"          -> Some FieldPrefixAuto
                | _                 -> Some (FieldPrefixUserValue vl)
        targetLanguages =
            args |> List.choose(fun a -> match a with C_lang -> Some (CommonTypes.ProgrammingLanguage.C) | Ada_Lang -> Some (CommonTypes.ProgrammingLanguage.Ada) | _ -> None)
    
        objectIdentifierMaxLength = 20I
    }    



let main0 argv =
    
    let parser = ArgumentParser.Create<CliArguments>(programName = "Asn1f4.exe")
    try
        let parserResults = parser.Parse argv
        let cliArgs = parserResults.GetAllResults()
        cliArgs |> Seq.iter(fun arg -> match arg with Debug -> RangeSets.debug () | _ -> ())
        cliArgs |> Seq.iter checkArguement 

        let args = constructCommandLineSettings cliArgs parserResults
        let outDir = parserResults.GetResult(<@Out@>, defaultValue = ".")
        
        // create front ent ast

        let debugFunc (r:Asn1Ast.AstRoot) (acn:AcnGenericTypes.AcnAst) = 
            match parserResults.Contains<@ Debug_Asn1 @> with
            | true  -> 
                let pdu = parserResults.GetResult(<@Debug_Asn1@>, defaultValue = None)
                let tastToPrint = PrintAsn1.printInASignleFile r outDir "SingleAsn1FileDbg.asn1" pdu
                PrintAcn.printInASignleFile acn outDir "SingleAsn1FileDbg.acn" tastToPrint
            | false -> ()


        let frontEntAst, acnDeps = FrontEntMain.constructAst args debugFunc 

        
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
                | C_lang                -> 
                    Some (DAstConstruction.DoWork frontEntAst acnDeps CommonTypes.ProgrammingLanguage.C  args.encodings)
                | Ada_Lang              -> 
                    
                    Some (DAstConstruction.DoWork {frontEntAst with stg=TargetLanguageStgMacros.a_StgMacros} acnDeps CommonTypes.ProgrammingLanguage.Ada  args.encodings)
                | _             -> None)


        //generate code
        backends |> 
            Seq.iter (fun r -> 
                GenerateFiles.generateAll outDir r args.encodings
                GenerateRTL.exportRTL outDir r.lang args
                match args.AstXmlAbsFileName with
                | ""    -> ()
                | _     -> DAstExportToXml.exportFile r acnDeps ("backend_" + args.AstXmlAbsFileName)
                )
        

        //custom stgs code generation
        
        // let AST for custom STGs
        // if there is an AST from Ada or C use it, otherwise create a new one

        let r = 
            match backends with
            | []    -> DAstConstruction.DoWork frontEntAst acnDeps CommonTypes.ProgrammingLanguage.C  args.encodings
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
                | Version   -> printVersion () //Antlr.VersionInformation.printGitVersion()
                | Debug     -> RangeSets.debug ()
                | _ -> ())

        cliArgs |> 
            List.filter (fun a -> match a with | Generate_Test_Grammar    -> true | _  -> false) |>
            Seq.iter(fun _ -> GrammarGenerator.generateGrammars outDir)

        0
    with
        | :? Argu.ArguParseException as ex -> 
            match argv.Length = 1 && (argv.[0] = "-v" || argv.[0] = "--version") with
            | true -> 
                //Antlr.VersionInformation.printGitVersion ()
                printVersion ()
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
    
