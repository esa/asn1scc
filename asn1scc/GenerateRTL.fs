module GenerateRTL
open FsUtils
open System
open System.Numerics
open System.IO
open CommonTypes
open AbstractMacros
open OutDirectories
open System.Resources

let writeTextFile fileName (content:String) =
    System.IO.File.WriteAllText(fileName, content.Replace("\r",""))


let getResourceAsString (rsName:string) =
    FsUtils.getResourceAsString0 "asn1scc" (System.Reflection.Assembly.GetExecutingAssembly ()) rsName


let writeResource (di:DirInfo) (rsName:string) (fn) : unit=
    let asn1rtlDirName = di.asn1rtlDir

    let resourceInitialContent = getResourceAsString rsName

    match fn with
    | None      -> 
        writeTextFile (Path.Combine(asn1rtlDirName, rsName)) resourceInitialContent
    | Some fn   -> 
        let newContent = fn resourceInitialContent 
        writeTextFile (Path.Combine(asn1rtlDirName, rsName)) newContent


let exportRTL (di:DirInfo) (l:ProgrammingLanguage) (args:CommandLineSettings)=
    let rootDir = di.rootDir
    //let asn1rtlDirName = di.asn1rtlDir
    let boardsDirName = di.boardsDir


    let rm = new ResourceManager("Resource1", System.Reflection.Assembly.GetExecutingAssembly());
    let hasUper = args.encodings |> Seq.exists(fun e -> e = UPER)
    let hasAcn = args.encodings |> Seq.exists(fun e -> e = ACN)
    let hasXer = args.encodings |> Seq.exists(fun e -> e = XER)
    let hasBer = args.encodings |> Seq.exists(fun e -> e = BER)
    match l with
    | ProgrammingLanguage.C ->
        //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt.c")) (rm.GetString("asn1crt_c",null)) 
        writeResource di "asn1crt.c" None
                
        //let asn1crt_h = rm.GetString("asn1crt_h",null)
        let intSize = sprintf "#define WORD_SIZE	%d" (int args.integerSizeInBytes)
        let fpSize = sprintf "#define FP_WORD_SIZE	%d" (int args.floatingPointSizeInBytes)
        //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt.h")) (asn1crt_h.Replace("#define WORD_SIZE	8", intSize).Replace("#define FP_WORD_SIZE	8", fpSize) )
        writeResource di "asn1crt.h" (Some (fun (s:string) -> s.Replace("#define WORD_SIZE	8", intSize).Replace("#define FP_WORD_SIZE	8", fpSize)) )
                
        match args.encodings with
        | []    -> ()
        | _     ->

            writeResource di "asn1crt_encoding.c" None
            //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding.c")) asn1crt_encoding_c


            //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding.h")) (rm.GetString("asn1crt_encoding_h",null))
            writeResource di "asn1crt_encoding.h" None

            if hasUper || hasAcn then
                writeResource di "asn1crt_encoding_uper.c" None
                writeResource di "asn1crt_encoding_uper.h" None
                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_uper.c")) (rm.GetString("asn1crt_encoding_uper_c",null))
                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_uper.h")) (rm.GetString("asn1crt_encoding_uper_h",null))

            if hasAcn  then
                writeResource di "asn1crt_encoding_acn.c" None
                writeResource di "asn1crt_encoding_acn.h" None
                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_acn.c")) (rm.GetString("asn1crt_encoding_acn_c",null))
                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_acn.h")) (rm.GetString("asn1crt_encoding_acn_h",null))

            if hasXer  then
                writeResource di "asn1crt_encoding_xer.c" None
                writeResource di "asn1crt_encoding_xer.h" None
                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_xer.c")) (rm.GetString("asn1crt_encoding_xer_c",null))
                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_xer.h")) (rm.GetString("asn1crt_encoding_xer_h",null))

            if hasBer  then
                writeResource di "asn1crt_encoding_ber.c" None
                writeResource di "asn1crt_encoding_ber.h" None
                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_ber.c")) (rm.GetString("asn1crt_encoding_ber_c",null))
                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_ber.h")) (rm.GetString("asn1crt_encoding_ber_h",null))
    | ProgrammingLanguage.Ada ->
        //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl.adb")) (rm.GetString("adaasn1rtl_adb",null))
        writeResource di "adaasn1rtl.adb" None
        //let adaasn1rtl_ads = rm.GetString("adaasn1rtl_ads",null)
        match args.floatingPointSizeInBytes  = 4I with 
        | true  -> 
            //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl.ads")) (adaasn1rtl_ads.Replace("subtype Asn1Real is Standard.Long_Float;","subtype Asn1Real is Standard.Float;"))
            writeResource di "adaasn1rtl.ads" (Some (fun (s:string) -> s.Replace("subtype Asn1Real is Standard.Long_Float;","subtype Asn1Real is Standard.Float;")))
        | false -> 
            writeResource di "adaasn1rtl.ads" None
            //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl.ads")) (adaasn1rtl_ads) 

        match args.encodings with
        | []    -> ()
        | _     ->
            let adaasn1rtl_encoding_adb_fn (s:string) = 
                match args.streamingModeSupport with
                | false -> s // rm.GetString("adaasn1rtl_encoding_adb",null)//.Replace("--  with user_code;","").Replace("--  user_code.push_data(bs, bs.pushDataPrm);","--").Replace("--  user_code.fetch_data(bs, bs.fetchDataPrm);","")
                | true  -> s.Replace("--  with user_code;","with user_code;").Replace("--  bs.Current_Bit_Pos := 0;","bs.Current_Bit_Pos := 0;").Replace("--  user_code.push_data(bs, bs.pushDataPrm);","user_code.push_data (bs, bs.pushDataPrm);").Replace("--  user_code.fetch_data(bs, bs.fetchDataPrm);","user_code.fetch_data (bs, bs.fetchDataPrm);")

            //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding.adb")) adaasn1rtl_encoding_adb
            writeResource di "adaasn1rtl-encoding.adb" (Some adaasn1rtl_encoding_adb_fn)

            //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding.ads")) (rm.GetString("adaasn1rtl_encoding_ads",null))
            writeResource di "adaasn1rtl-encoding.ads" None

            if hasUper || hasAcn then
                writeResource di "adaasn1rtl-encoding-uper.adb" None
                writeResource di "adaasn1rtl-encoding-uper.ads" None
                //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-uper.adb")) (rm.GetString("adaasn1rtl_encoding_uper_adb",null))
                //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-uper.ads")) (rm.GetString("adaasn1rtl_encoding_uper_ads",null))
                
            if hasAcn  then
                writeResource di "adaasn1rtl-encoding-acn.adb" None
                writeResource di "adaasn1rtl-encoding-acn.ads" None
                //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-acn.adb")) (rm.GetString("adaasn1rtl_encoding_acn_adb",null))
                //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-acn.ads")) (rm.GetString("adaasn1rtl_encoding_acn_ads",null))

            if hasXer  then
                writeResource di "adaasn1rtl-encoding-xer.adb" None
                writeResource di "adaasn1rtl-encoding-xer.ads" None
                //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-xer.adb")) (rm.GetString("adaasn1rtl_encoding_xer_adb",null))
                //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-xer.ads")) (rm.GetString("adaasn1rtl_encoding_xer_ads",null))

        match args.generateAutomaticTestCases with
        | true  ->
            writeResource di "adaasn1rtl-encoding-test_cases_aux.adb" None
            writeResource di "adaasn1rtl-encoding-test_cases_aux.ads" None
            //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-test_cases_aux.adb")) (rm.GetString("adaasn1rtl_encoding_test_cases_aux_adb",null))
            //writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-test_cases_aux.ads")) (rm.GetString("adaasn1rtl_encoding_test_cases_aux_ads",null))
        | false -> ()

        let writeBoard boardName = 
            let outDir = 
                match args.target with
                | Some  _       -> Path.Combine(boardsDirName, boardName)
                | None          -> boardsDirName
            writeTextFile (Path.Combine(outDir, "board_config.ads")) (getResourceAsString  (boardName+"_board_config.ads")) 

        let boardNames = OutDirectories.getBoardNames l args.target  

        boardNames |> List.iter writeBoard
        
        boardNames |> 
        List.iter(fun bn -> 
            let filename = (Path.Combine(rootDir, ("asn1_"+bn+".gpr")))
            let dirs = 
                match args.target with
                | Some _    -> ["asn1rtl"; "src"; "boards/" + bn]
                | None      -> ["."]

            let content = aux_a.PrintGpsProject bn dirs
            //let content = (rm.GetString(("asn1_"+bn+".gpr"),null))
            writeTextFile filename    content)


