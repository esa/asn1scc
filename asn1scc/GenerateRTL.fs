module GenerateRTL
open FsUtils
open System
open System.Numerics
open System.IO
open CommonTypes
open AbstractMacros
open System.Resources
open Language

let writeTextFile fileName (content:String) =
    System.IO.File.WriteAllText(fileName, content.Replace("\r",""))


let getResourceAsString (rsName:string) =
    FsUtils.getResourceAsString0 "asn1scc" (System.Reflection.Assembly.GetExecutingAssembly ()) rsName


let getResourceAsByteArray (rsName:string) =
    FsUtils.getResourceAsByteArray0 "asn1scc" (System.Reflection.Assembly.GetExecutingAssembly ()) rsName


let writeResource (di:DirInfo) (rsName:string) (fn) : unit=
    let asn1rtlDirName = di.asn1rtlDir

    let resourceInitialContent = getResourceAsString rsName

    match fn with
    | None      ->
        writeTextFile (Path.Combine(asn1rtlDirName, rsName)) resourceInitialContent
    | Some fn   ->
        let newContent = fn resourceInitialContent
        writeTextFile (Path.Combine(asn1rtlDirName, rsName)) newContent



let findUnusedRtlFunctions (args:CommandLineSettings) (lm:LanguageMacros) (rtlContent:string) (generatedContent:string) =
    let rec findAllUsedRtlFunctions (lm:LanguageMacros) (sourceCode:string)(currentlyUsedFunctionNames:string list) =
        let indirectlyUsedFunctions =
            currentlyUsedFunctionNames |> List.collect (lm.lg.detectFunctionCalls sourceCode) |> List.distinct
        let newCurrentlyUsedFunctionNames = currentlyUsedFunctionNames @ indirectlyUsedFunctions |> List.distinct
        match newCurrentlyUsedFunctionNames.Length = currentlyUsedFunctionNames.Length with
        | true -> newCurrentlyUsedFunctionNames
        | false -> findAllUsedRtlFunctions lm sourceCode newCurrentlyUsedFunctionNames

    //first detect all function names
    let directlyUsedFunctions =
        lm.lg.RtlFuncNames |> List.filter (fun fn -> generatedContent.Contains(fn))


    let allUsedFunctions = findAllUsedRtlFunctions lm rtlContent (lm.lg.AlwaysPresentRtlFuncNames@directlyUsedFunctions@args.userRtlFunctionsToGenerate) |> Set.ofList
    lm.lg.RtlFuncNames |> List.filter (fun fn -> not (allUsedFunctions.Contains(fn)))



let exportRTL (di:DirInfo) (l:ProgrammingLanguage) (args:CommandLineSettings) (lm:LanguageMacros) (generatedContent:string) =
    let rootDir = di.rootDir
    let boardsDirName = di.boardsDir


    let rm = new ResourceManager("Resource1", System.Reflection.Assembly.GetExecutingAssembly());
    let hasUper = args.encodings |> Seq.exists(fun e -> e = UPER)
    let hasAcn = args.encodings |> Seq.exists(fun e -> e = ACN)
    let hasXer = args.encodings |> Seq.exists(fun e -> e = XER)
    let hasBer = args.encodings |> Seq.exists(fun e -> e = BER)
    match l with
    | ProgrammingLanguage.C ->
        let rtlContent =
            ["asn1crt.c";"asn1crt_encoding.c";"asn1crt_encoding_uper.c";"asn1crt_encoding_acn.c"] |> List.map getResourceAsString |> Seq.StrJoin "\n"
        let unusedRtlFunctions = findUnusedRtlFunctions args lm  rtlContent generatedContent


        let removeUnusedRtlFunctionsFromHeader (sourceCode:string) =
            unusedRtlFunctions |> List.fold (fun acc fn -> lm.lg.removeFunctionFromHeader acc fn) sourceCode
        let removeUnusedRtlFunctionsFromBody (sourceCode:string) =
            let debug = unusedRtlFunctions |> List.sort |> Seq.StrJoin "\n"
            unusedRtlFunctions |> List.fold (fun acc fn -> lm.lg.removeFunctionFromBody acc fn) sourceCode

        writeResource di "asn1crt.c" (Some removeUnusedRtlFunctionsFromBody)

        //let asn1crt_h = rm.GetString("asn1crt_h",null)
        let intSize = sprintf "#define WORD_SIZE	%d" (int args.integerSizeInBytes)
        let fpSize = sprintf "#define FP_WORD_SIZE	%d" (int args.floatingPointSizeInBytes)

        let fix_asn1crt_h (s:string) =
            let ret = s.Replace("#define WORD_SIZE	8", intSize).Replace("#define FP_WORD_SIZE	8", fpSize)
            removeUnusedRtlFunctionsFromHeader ret

        writeResource di "asn1crt.h" (Some fix_asn1crt_h)

        match args.encodings with
        | []    -> ()
        | _     ->

            writeResource di "asn1crt_encoding.c" (Some removeUnusedRtlFunctionsFromBody)


            writeResource di "asn1crt_encoding.h" (Some removeUnusedRtlFunctionsFromHeader)

            if hasUper || hasAcn then
                writeResource di "asn1crt_encoding_uper.c" (Some removeUnusedRtlFunctionsFromBody)
                writeResource di "asn1crt_encoding_uper.h" (Some removeUnusedRtlFunctionsFromHeader)

            if hasAcn  then
                writeResource di "asn1crt_encoding_acn.c" (Some removeUnusedRtlFunctionsFromBody)
                writeResource di "asn1crt_encoding_acn.h" (Some removeUnusedRtlFunctionsFromHeader)

            if hasXer  then
                writeResource di "asn1crt_encoding_xer.c" None
                writeResource di "asn1crt_encoding_xer.h" None

            if hasBer  then
                writeResource di "asn1crt_encoding_ber.c" None
                writeResource di "asn1crt_encoding_ber.h" None

    // TODO: Scala
    | ProgrammingLanguage.Scala ->
        File.WriteAllBytes(
            Path.Combine(rootDir, "lib", "stainless-library_3-0.9.8.7.jar"),
            getResourceAsByteArray "stainless-library_3-0.9.8.7.jar"
        )
        writeTextFile (Path.Combine(rootDir, "build.sbt")) (getResourceAsString "build.sbt")

        writeResource di "asn1jvm.scala" None
        writeResource di "asn1jvm_Bitstream.scala" None

        //let intSize = sprintf "#define WORD_SIZE	%d" (int args.integerSizeInBytes)
        //let fpSize = sprintf "#define FP_WORD_SIZE	%d" (int args.floatingPointSizeInBytes)
        //writeResource di "asn1jvm.scala" (Some (fun (s:string) -> s.Replace("#define WORD_SIZE	8", intSize).Replace("#define FP_WORD_SIZE	8", fpSize)) )

        match args.encodings with
        | []    -> ()
        | _     ->
            writeResource di "asn1jvm_Codec.scala" None
            writeResource di "asn1jvm_Codec_PER.scala" None
            writeResource di "asn1jvm_Helper.scala" None
            writeResource di "asn1jvm_Verification.scala" None
            writeResource di "asn1jvm_Vector.scala" None

            if hasUper || hasAcn then
                writeResource di "asn1jvm_Codec_UPER.scala" None

            if hasAcn then
                writeResource di "asn1jvm_Codec_ACN.scala" None

//            if hasXer  then
//                writeResource di "asn1crt_encoding_xer.c" None
//                writeResource di "asn1crt_encoding_xer.h" None
//                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_xer.c")) (rm.GetString("asn1crt_encoding_xer_c",null))
//                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_xer.h")) (rm.GetString("asn1crt_encoding_xer_h",null))
//
//            if hasBer  then
//                writeResource di "asn1crt_encoding_ber.c" None
//                writeResource di "asn1crt_encoding_ber.h" None
//                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_ber.c")) (rm.GetString("asn1crt_encoding_ber_c",null))
//                //writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_ber.h")) (rm.GetString("asn1crt_encoding_ber_h",null))
    | ProgrammingLanguage.Ada ->
        writeResource di "adaasn1rtl.adb" None
        match args.floatingPointSizeInBytes  = 4I with
        | true  ->
            writeResource di "adaasn1rtl.ads" (Some (fun (s:string) -> s.Replace("subtype Asn1Real is Standard.Long_Float;","subtype Asn1Real is Standard.Float;")))
        | false ->
            writeResource di "adaasn1rtl.ads" None

        match args.encodings with
        | []    -> ()
        | _     ->
            let adaasn1rtl_encoding_adb_fn (s:string) =
                match args.streamingModeSupport with
                | false -> s
                | true  -> s.Replace("--  with user_code;","with user_code;").Replace("--  bs.Current_Bit_Pos := 0;","bs.Current_Bit_Pos := 0;").Replace("--  user_code.push_data(bs, bs.pushDataPrm);","user_code.push_data (bs, bs.pushDataPrm);").Replace("--  user_code.fetch_data(bs, bs.fetchDataPrm);","user_code.fetch_data (bs, bs.fetchDataPrm);")

            writeResource di "adaasn1rtl-encoding.adb" (Some adaasn1rtl_encoding_adb_fn)

            writeResource di "adaasn1rtl-encoding.ads" None

            if hasUper || hasAcn then
                writeResource di "adaasn1rtl-encoding-uper.adb" None
                writeResource di "adaasn1rtl-encoding-uper.ads" None

            if hasAcn  then
                writeResource di "adaasn1rtl-encoding-acn.adb" None
                writeResource di "adaasn1rtl-encoding-acn.ads" None

            if hasXer  then
                writeResource di "adaasn1rtl-encoding-xer.adb" None
                writeResource di "adaasn1rtl-encoding-xer.ads" None

        match args.generateAutomaticTestCases with
        | true  ->
            writeResource di "adaasn1rtl-encoding-test_cases_aux.adb" None
            writeResource di "adaasn1rtl-encoding-test_cases_aux.ads" None
        | false -> ()

        let writeBoard boardName =
            let outDir =
                match args.target with
                | Some  _       -> Path.Combine(boardsDirName, boardName)
                | None          -> boardsDirName
            writeTextFile (Path.Combine(outDir, "board_config.ads")) (getResourceAsString  (boardName+"_board_config.ads"))

        let boardNames = lm.lg.getBoardNames  args.target

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


let test2() =
    let headerFiles = [ "asn1crt.h"; "asn1crt_encoding.h"; "asn1crt_encoding_uper.h"; "asn1crt_encoding_acn.h"]
    for hd in headerFiles do
        printfn "Processing %s" hd
        let headerContents = getResourceAsString hd
        let functionNames = RemoveUnusedRtlFunction.findFunctionNames headerContents
        for fn in functionNames do
            printfn "\t%s" fn
