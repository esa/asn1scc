open Argu
open FsUtils
open System
open System.Numerics
open System.IO
open CommonTypes
open AbstractMacros
open System.Resources
open Antlr
open Language

type CliArguments =
    | [<Unique; AltCommandLine("-c")>]C_lang
    | [<Unique; AltCommandLine("-Ada")>]Ada_Lang
    | [<Unique; AltCommandLine("-Scala")>]Scala_Lang
    | [<Unique; AltCommandLine("-uPER")>]UPER_enc
    | [<Unique; AltCommandLine("-XER")>]XER_enc
    | [<Unique; AltCommandLine("-ACN")>]ACN_enc
    | [<Unique; AltCommandLine("-atc")>]Auto_test_cases
    | [<Unique; AltCommandLine("-o")>]Out of dir:string
    | [<Unique; AltCommandLine("-equal")>]Equal_Func
    | [<Unique; AltCommandLine("-x")>] Xml_Ast of xmlFilename:string
    | [<Unique; AltCommandLine("-typePrefix")>]Type_Prefix of prefix:string
    | [<Unique; AltCommandLine("-renamePolicy")>] Rename_Policy of int
    | [<Unique; AltCommandLine("-eee")>] Enable_Efficient_Enumerations of uint option

    | [<Unique; AltCommandLine("-fp")>]  Field_Prefix of prefix:string
    | [<Unique; AltCommandLine("-gtc")>] Generate_Test_Grammar
    | [<AltCommandLine("-customStg")>] Custom_Stg  of custom_stg_colon_outfilename:string
    | [<Unique; AltCommandLine("-customStgAstVersion")>] Custom_Stg_Ast_Version  of astver:int
    | [<Unique; AltCommandLine("-icdUper")>] IcdUper  of uper_icd_output_file:string
    | [<Unique; AltCommandLine("-customIcdUper")>] CustomIcdUper  of custom_stg_colon_out_filename:string
    | [<Unique; AltCommandLine("-icdAcn")>] IcdAcn  of acn_icd_output_file:string
    | [<Unique; AltCommandLine("-customIcdAcn")>] CustomIcdAcn  of custom_stg_colon_out_filename:string
    | [<Unique; AltCommandLine("-icdPdus")>] IcdPdus  of asn1_type_assignments_list:string

    | [<Unique; AltCommandLine("-AdaUses")>] AdaUses
    | [<Unique; AltCommandLine("-ACND")>] ACND
    | [<Unique; AltCommandLine("-wordSize")>] Word_Size  of wordSize:int
    | [<Unique; AltCommandLine("-fpWordSize")>]  Fp_Word_Size of fpWordSize:int
    | [<Unique; AltCommandLine("-slim")>] Slim
    | [<Unique; AltCommandLine("-t")>]   Target of Targets
    | [<Unique; AltCommandLine("-v")>]   Version
    | [<Unique; AltCommandLine("-asn1")>]   Debug_Asn1 of string option
    | [<Unique; AltCommandLine("-mfm")>]   Mapping_Functions_Module of string
    | [<Unique; AltCommandLine("-debug")>]   Debug
    | [<Unique; AltCommandLine("-sm")>]   Streaming_Mode
    | [<Unique; AltCommandLine("-ig")>]   Init_Globals
    | [<Unique; AltCommandLine("-es")>]   Handle_Empty_Sequences
    | [<Unique; AltCommandLine("-let")>]    Log_Execution_Time
    | [<AltCommandLine("-if")>] Include_Func of string
    | [<Unique; AltCommandLine("-invertibility")>] StainlessInvertibility
    | [<MainCommand; ExactlyOnce; Last>] Files of files:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Version          -> "displays version information"
            | Debug            -> "Option used internally for debugging"
            | C_lang           -> "generate code for the C/C++ programming language"
            | Ada_Lang         -> "generate code for the Ada/SPARK programming language"
            | Scala_Lang       -> "generate code for the Scala programming language"
            | UPER_enc         -> "generates encoding and decoding functions for unaligned Packed Encoding Rules (uPER)"
            | XER_enc          -> "generates encoding and decoding functions for XML Encoding Rules (XER)"
            | ACN_enc          -> "generates encoding and decoding functions using the ASSERT ASN.1 encoding Control Notation"
            | Auto_test_cases  -> "create automatic test cases."
            | Out (_)          -> "directory where all files are produced."
            | Files (_)        -> "List of ASN.1 and ACN files to process."
            | Type_Prefix _    -> "adds 'prefix' to all generated C or Ada/SPARK data types."
            | Equal_Func       -> "generate functions for testing type equality."
            | Init_Globals     -> "generate const globals for types initialization. Applicable only to C."
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
            | Enable_Efficient_Enumerations _ -> """Enable efficient enumerations. (Applicable only to C.)
This mode optimizes the generated C code for ASN.1 Enumerated types with multiple enumerants (e.g., 50 or more).
Instead of generating switch statements, asn1scc generates sorted arrays containing the possible values.
Lookups (e.g., for validation or index retrieval in uPER encoding) are performed using an optimized binary search.
This results in more efficient and less verbose code.
The argument is the minimum number of enumerants in an enumerated type to enable this mode.
E.g., -eee 50 will enable this mode for enumerated types with 50 or more enumerants.
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
            | IcdPdus       _   -> "A list of type assignments to be included in the generated ICD. If there are multiple type assignments, please separate them with commas and enclose them in double quotes."
            | AdaUses           -> "Prints in the console all type Assignments of the input ASN.1 grammar"
            | ACND              -> "creates ACN grammars for the input ASN.1 grammars using the default encoding properties"
            | Debug_Asn1  _     -> "Prints all input ASN.1 grammars in a single module/single file and with parameterized types removed. Used for debugging purposes"
            | Word_Size _       -> "Defines the size of asn1SccSint and asn1SccUint types. Valid values are 8 bytes (default) and 4 bytes. If you pass 4 then you should compile the C code -DWORD_SIZE=4. (Applicable only to C.)"
            | Fp_Word_Size _    -> "Defines the size of the REAL type. Valid values are 8 bytes (default) which corresponds to double and 4 bytes which corresponds to float. If you pass 4 then you should compile the C code -DFP_WORD_SIZE=4. (Applicable only to C.)"
            | Slim              -> "Generate Integer and Real types based on the ASN.1 range constraints and/or on ACN encoding properties. E.g. MyInt ::=INTEGER (0..255) becomes a uint8_t instead of asn1SccUint."
            | Target _          -> """Specify Ada configuration profile. (Applicable only to Ada.)"""
            | Mapping_Functions_Module _    -> "The name of Ada module or name of C header file (without extension) containing the definitions of mapping functions"
            | Streaming_Mode    -> "Streaming mode support"
            | Handle_Empty_Sequences -> "Adds a dummy integer member to empty ASN.1 SEQUENCE structures for compliant C code generation."
            | Include_Func _    -> "Include a function from the RTL. The function name is expected as argument. This argument can be repeated many times. This argument is supported only for C"
            | Log_Execution_Time           -> "Enables detailed logging of execution time."
            | StainlessInvertibility -> "(Scala backend only) Generate invertibility conditions and lemmas"


let printVersion () =
    //let assembly = System.Reflection.Assembly.GetExecutingAssembly();
    //let fvi = System.Diagnostics.FileVersionInfo.GetVersionInfo(assembly.Location);
    //let version = fvi.FileVersion;

    let version = "4.5.2.3"
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


let checkOutFileName (fileName:string) (extension:string) cmdoption =
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

let c_macro =
        {
            LanguageMacros.equal = new IEqual_c.IEqual_c()
            init = new Init_c.Init_c()
            typeDef = new ITypeDefinition_c.ITypeDefinition_c()
            lg = new LangGeneric_c.LangGeneric_c();
            isvalid= new IsValid_c.IsValid_c()
            vars = new IVariables_c.IVariables_c()
            uper = new iuper_c.iuper_c()
            acn = new IAcn_c.IAcn_c()
            atc = new ITestCases_c.ITestCases_c()
            xer = new IXer_c.IXer_c()
            src = new ISrcBody_c.ISrcBody_c()
        }
let scala_macro =
        {
            LanguageMacros.equal = new IEqual_scala.IEqual_scala()
            init = new IInit_scala.IInit_scala()
            typeDef = new ITypeDefinition_scala.ITypeDefinition_scala()
            lg = new LangGeneric_scala.LangGeneric_scala();
            isvalid= new IIsValid_scala.IIsValid_scala()
            vars = new IVariables_scala.IVariables_scala()
            uper = new IUper_scala.IUper_scala()
            acn = new IAcn_scala.IAcn_scala()
            atc = new ITestCases_scala.ITestCases_scala()
            xer = new IXer_scala.IXer_scala()
            src = new ISrcBody_scala.ISrcBody_scala()
        }
let ada_macro =
        {
            LanguageMacros.equal = new IEqual_a.IEqual_a();
            init = new Init_a.Init_a()
            typeDef = new ITypeDefinition_a.ITypeDefinition_a();
            lg = new LangGeneric_a.LangGeneric_a();
            isvalid= new IsValid_a.IsValid_a()
            vars = new IVariables_a.IVariables_a()
            uper = new iuper_a.iuper_a()
            acn = new IAcn_a.IAcn_a()
            atc = new ITestCases_a.ITestCases_a()
            xer = new IXer_a.IXer_a()
            src = new ISrcBody_a.ISrcBody_a()
        }
let allMacros = [ (C, c_macro); (Scala, scala_macro); (Ada, ada_macro)]
let getLanguageMacro (l:ProgrammingLanguage) =
    allMacros |> List.filter(fun (lang,_) -> lang = l) |> List.head |> snd

let checkArgument (cliArgs : CliArguments list) arg =
    match arg with
    | Version          -> ()
    | Log_Execution_Time -> ()
    | Debug            -> ()
    | C_lang           -> ()
    | Ada_Lang         -> ()
    | Scala_Lang       -> ()
    | UPER_enc         -> ()
    | XER_enc          -> ()
    | ACN_enc          -> ()
    | Auto_test_cases  -> ()
    | Equal_Func       -> ()
    | Generate_Test_Grammar -> ()
    | Init_Globals          -> ()
    | Enable_Efficient_Enumerations _ -> ()
    | Xml_Ast xmlFileName   -> checkOutFileName xmlFileName ".xml" "-x"
    | Out outDir       ->
        match System.IO.Directory.Exists outDir with
        | true  -> ()
        | false ->
            match Directory.Exists outDir with
            | true  -> ()
            | false -> Directory.CreateDirectory outDir |> ignore
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
    | Slim -> ()
    | IcdUper  outHtmlFile      -> checkOutFileName outHtmlFile ".html" "-icdUper"
    | CustomIcdUper  comFile    -> checkCompositeFile comFile "-customIcdUper" ".html"
    | IcdAcn  outHtmlFile       -> checkOutFileName outHtmlFile ".html" "-icdAcn"
    | CustomIcdAcn  comFile     -> checkCompositeFile comFile "-customIcdAcn" ".html"
    | IcdPdus _                 -> ()
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
    | Target _          ->()
    | Mapping_Functions_Module mfm  -> ()
    | Streaming_Mode    -> ()
    | Handle_Empty_Sequences -> ()
    | Include_Func  fnName    ->
        //check that the target language is C
        match cliArgs |> List.exists (fun a -> a = C_lang) with
        | true  ->
            // check if the function exists in the RTL
            match c_macro.lg.RtlFuncNames |> List.exists (fun name -> name = fnName ) with
            | true  -> ()
            | false ->
                let availableFunctions = c_macro.lg.RtlFuncNames |> String.concat "\n"
                raise (UserException (sprintf "Function '%s' does not exist in the C RTL.\nThe available functions to choose are:\n\n%s" fnName availableFunctions))
        | false -> raise (UserException ("The -if option is supported only for C."))
    | StainlessInvertibility -> ()

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
        enum_Items_To_Enable_Efficient_Enumerations =
            match parserResults.TryGetResult <@ Enable_Efficient_Enumerations @> with
            | None -> System.UInt32.MaxValue
            | Some n ->
                match n with
                    | Some n -> n
                    | None -> 0u
        GenerateEqualFunctions = (parserResults.Contains <@ Equal_Func @>) || (parserResults.Contains <@ Auto_test_cases @>)
        generateAutomaticTestCases = parserResults.Contains <@ Auto_test_cases @>
        TypePrefix = parserResults.GetResult(<@ Type_Prefix@>, defaultValue = "")
        CheckWithOss = false
        AstXmlAbsFileName = parserResults.GetResult(<@Xml_Ast@>, defaultValue = "")
        //IcdUperHtmlFileName = ""
        //IcdAcnHtmlFileName = ""
        generateAcnIcd = 
            parserResults.Contains <@ IcdAcn @> || parserResults.Contains <@ CustomIcdAcn @>
        generateConstInitGlobals = parserResults.Contains(<@Init_Globals@>)
        custom_Stg_Ast_Version = parserResults.GetResult(<@ Custom_Stg_Ast_Version @>, defaultValue = 1)
        icdPdus = 
            match parserResults.TryGetResult(<@ IcdPdus @>) with
            | None -> None
            | Some pdus -> 
                // remove double quotes and split by comma
                let actualPdus = pdus.Replace("\"", "")
                Some ((actualPdus.Split(',')) |> Seq.map(fun (z:string) -> z.Trim()) |> Seq.filter(fun z -> not (String.IsNullOrEmpty z)) |> Seq.toList)
        mappingFunctionsModule = parserResults.TryGetResult(<@ Mapping_Functions_Module @>)
        integerSizeInBytes =
            let ws = parserResults.GetResult(<@Word_Size@>, defaultValue = 8)
            BigInteger ws
        floatingPointSizeInBytes =
            let fws = parserResults.GetResult(<@Fp_Word_Size@>, defaultValue = 8)
            BigInteger fws
        slim = parserResults.Contains(<@Slim@>)
        target = parserResults.TryGetResult(<@Target@>)
        streamingModeSupport = parserResults.Contains <@ Streaming_Mode @>
        renamePolicy =
            match args |> List.choose (fun a -> match a with Rename_Policy rp -> Some rp | _ -> None) with
            | []    ->
                match args |> List.filter(fun a -> a = C_lang || a = Ada_Lang || a = Scala_Lang) with
                | [ C_lang ]    -> CommonTypes.EnumRenamePolicy.SelectiveEnumerants
                | [ Scala_Lang ]-> CommonTypes.EnumRenamePolicy.SelectiveEnumerants // TODO: Scala
                | [ Ada_Lang ]  -> CommonTypes.EnumRenamePolicy.NoRenamePolicy
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
            args |> List.choose(fun a -> match a with C_lang -> Some (CommonTypes.ProgrammingLanguage.C) | Ada_Lang -> Some (CommonTypes.ProgrammingLanguage.Ada) | Scala_Lang -> Some (CommonTypes.ProgrammingLanguage.Scala) | _ -> None)

        userRtlFunctionsToGenerate =
            args |> List.choose(fun a -> match a with Include_Func fnName -> Some fnName | _ -> None)

        objectIdentifierMaxLength = 20I
        handleEmptySequences = parserResults.Contains <@ Handle_Empty_Sequences @>

        blm = [(ProgrammingLanguage.C, new LangGeneric_c.LangBasic_c());(ProgrammingLanguage.Ada, new LangGeneric_a.LangBasic_ada());(ProgrammingLanguage.Scala, new LangGeneric_scala.LangBasic_scala()) ]
        stainlessInvertibility = args |> List.exists (fun a -> match a with StainlessInvertibility -> true | _ -> false)
    }

let setActiveLanguages args =
    let activeLangs = 
        args |> 
        List.choose(fun a -> 
            match a with 
            | C_lang -> Some (CommonTypes.ProgrammingLanguage.C) 
            | Ada_Lang -> Some (CommonTypes.ProgrammingLanguage.Ada) 
            | Scala_Lang -> Some (CommonTypes.ProgrammingLanguage.Scala) 
            | _ -> None)
    match activeLangs with
    | [] ->     ProgrammingLanguage.ActiveLanguages <- [CommonTypes.ProgrammingLanguage.C]
    | _  ->     ProgrammingLanguage.ActiveLanguages <- activeLangs
    ST.lang <- ProgrammingLanguage.ActiveLanguages.Head

let main0 argv =

    //RemoveUnusedRtlFunction.test1 ()
    //GenerateRTL.test3 (RemoveUnusedRtlFunction.C_RTL_FUNCTIONS |> List.map fst)

    let parser = ArgumentParser.Create<CliArguments>(programName = "asn1scc.exe")
    try
        let parserResults = parser.Parse argv
        let logExecutionTime = parserResults.Contains <@ Log_Execution_Time @>
        let cliArgs = parserResults.GetAllResults()
        cliArgs |> Seq.iter(fun arg -> match arg with Debug -> RangeSets.debug () | _ -> ())
        cliArgs |> Seq.iter (checkArgument cliArgs)
        setActiveLanguages cliArgs



        let args = constructCommandLineSettings cliArgs parserResults
        let outDir = parserResults.GetResult(<@Out@>, defaultValue = ".")

        // create front ent ast

        let debugFunc (r:Asn1Ast.AstRoot) (acn:AcnGenericTypes.AcnAst) =
            match parserResults.Contains <@ Debug_Asn1 @> with
            | true  ->
                let pdu = parserResults.GetResult(<@Debug_Asn1@>, defaultValue = None)
                let tastToPrint = PrintAsn1.printInASingleFile r outDir "SingleAsn1FileDbg.asn1" pdu
                PrintAcn.printInASingleFile acn outDir "SingleAsn1FileDbg.acn" tastToPrint
            | false -> ()

        // TODO frontend typo
        let activeLangs = 
            allMacros |> List.filter(fun (lang,_) -> ProgrammingLanguage.ActiveLanguages |> List.exists (fun l -> l = lang)) 
            
        let frontEntAst, acnDeps = TL "FrontEntMain.constructAst" (fun () -> FrontEntMain.constructAst args activeLangs debugFunc)


        // print front ent ast as xml
        match args.AstXmlAbsFileName with
        | ""    -> ()
        | _     ->
            TL "ExportToXml.exportFile" (fun () -> ExportToXml.exportFile frontEntAst acnDeps args.AstXmlAbsFileName)

        let icdStgFileName =
            match parserResults.TryGetResult (<@CustomIcdAcn@>) with
            | None -> "icdtemplate_acn.stg"
            | Some comFile ->
                    match getCustmStgFileNames comFile with
                    | Some(stgFile, _)  -> stgFile
                    | None  -> "icdtemplate_acn.stg"

        // construct backend ast
        let backends =
            cliArgs |>
            List.choose (fun a ->
                match a with
                | C_lang                ->
                    let lm = getLanguageMacro C
                    Some (TL "DAstConstruction.DoWork" (fun () -> DAstConstruction.DoWork frontEntAst icdStgFileName acnDeps CommonTypes.ProgrammingLanguage.C lm args.encodings))
                | Scala_Lang              ->
                    let lm = getLanguageMacro Scala
                    Some (TL "DAstConstruction.DoWork" (fun () -> DAstConstruction.DoWork frontEntAst icdStgFileName acnDeps CommonTypes.ProgrammingLanguage.Scala lm args.encodings))
                | Ada_Lang              ->
                    let lm = getLanguageMacro Ada
                    Some (TL "DAstConstruction.DoWork" (fun () -> DAstConstruction.DoWork frontEntAst icdStgFileName acnDeps CommonTypes.ProgrammingLanguage.Ada lm args.encodings))
                | _             -> None)

        let createDirectories baseDir (lm:LanguageMacros) target =
            let createDirIfNotExists outDir =
                let outDir = Path.Combine(baseDir,outDir)
                match Directory.Exists outDir with
                | true  -> ()
                | false -> Directory.CreateDirectory outDir |> ignore
            lm.lg.getTopLevelDirs target  |> Seq.iter createDirIfNotExists
            lm.lg.getBoardDirs target  |> Seq.iter createDirIfNotExists


        //generate code
        backends |>
            Seq.iter (fun r ->
                let lm = getLanguageMacro r.lang
                createDirectories outDir lm args.target
                let dirInfo = lm.lg.getDirInfo args.target outDir
                //let srcDirName = Path.Combine(outDir, OutDirectories.srcDirName r.lang)
                //let asn1rtlDirName = Path.Combine(outDir, OutDirectories.asn1rtlDirName r.lang)
                //let boardsDirName = Path.Combine(outDir, OutDirectories.boardsDirName r.lang)
                let generatedContent =  TL "GenerateFiles.generateAll" (fun () -> GenerateFiles.generateAll dirInfo r lm args.encodings)
                TL "GenerateRTL.exportRTL" (fun () -> GenerateRTL.exportRTL dirInfo r.lang args lm generatedContent)
                match args.AstXmlAbsFileName with
                | ""    -> ()
                | _     -> DAstExportToXml.exportFile r acnDeps ("backend_" + args.AstXmlAbsFileName)
                )


        //custom stgs code generation

        // let AST for custom STGs
        // if there is an AST from Ada or C use it, otherwise create a new one
        let r =
            match backends with
            | []    ->
                let lm =
                        {
                            LanguageMacros.equal = new IEqual_c.IEqual_c();
                            init = new Init_c.Init_c()
                            typeDef = new ITypeDefinition_c.ITypeDefinition_c();
                            lg = new LangGeneric_c.LangGeneric_c()
                            isvalid= new IsValid_c.IsValid_c()
                            vars = new IVariables_c.IVariables_c()
                            uper = new iuper_c.iuper_c()
                            acn = new IAcn_c.IAcn_c()
                            atc = new ITestCases_c.ITestCases_c()
                            xer = new IXer_c.IXer_c()
                            src = new ISrcBody_c.ISrcBody_c()
                        }
                TL "DAstConstruction.DoWork" (fun () -> DAstConstruction.DoWork frontEntAst icdStgFileName acnDeps CommonTypes.ProgrammingLanguage.C  lm args.encodings)
            | x::_  -> x

        let generateCustomStgCode () =
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
                    | IcdAcn outFile       -> GenerateAcnIcd.DoWork r  acnDeps "icdtemplate_acn.stg"  (Some "icdtemplate_uper.stg") outFile
                    | CustomIcdAcn comFile ->
                        match getCustmStgFileNames comFile with
                        | Some(stgFile, outFile)  ->
                            GenerateAcnIcd.DoWork r  acnDeps stgFile None outFile
                        | None  -> ()
                    | AdaUses   -> DAstUtilFunctions.AdaUses r
                    | ACND      -> GenerateFiles.EmitDefaultACNGrammar r outDir
                    | Version   -> printVersion () //Antlr.VersionInformation.printGitVersion()
                    | Debug     -> RangeSets.debug ()
                    | _ -> ())
        
        TL "GenerateCustomStgCode" generateCustomStgCode

        cliArgs |>
            List.filter (fun a -> match a with | Generate_Test_Grammar    -> true | _  -> false) |>
            Seq.iter(fun _ -> GrammarGenerator.generateGrammars outDir)
        
        if logExecutionTime then
            TL_report ()


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
            Console.Error.WriteLine(AntlrParse.formatSemanticError loc msg)
            3
        | ex            ->
            Console.Error.WriteLine(ex.Message)
            Console.Error.WriteLine( ex.StackTrace)
            4


[<EntryPoint>]
let main argv =
    main0 argv
