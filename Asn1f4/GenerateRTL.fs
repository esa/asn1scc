module GenerateRTL
open FsUtils
open System
open System.Numerics
open System.IO
open CommonTypes
open OutDirectories
open System.Resources

let exportRTL asn1rtlDirName boardsDirName (l:ProgrammingLanguage) (args:CommandLineSettings)=
    let writeTextFile fileName (content:String) =
        System.IO.File.WriteAllText(fileName, content.Replace("\r",""))
    let rm = new ResourceManager("Resource1", System.Reflection.Assembly.GetExecutingAssembly());
    let hasUper = args.encodings |> Seq.exists(fun e -> e = UPER)
    let hasAcn = args.encodings |> Seq.exists(fun e -> e = ACN)
    let hasXer = args.encodings |> Seq.exists(fun e -> e = XER)
    let hasBer = args.encodings |> Seq.exists(fun e -> e = BER)
    match l with
    | ProgrammingLanguage.C ->
        writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt.c")) (rm.GetString("asn1crt_c",null)) 
                
        let asn1crt_h = rm.GetString("asn1crt_h",null)
        let intSize = sprintf "#define WORD_SIZE	%d" (int args.integerSizeInBytes)
        let fpSize = sprintf "#define FP_WORD_SIZE	%d" (int args.floatingPointSizeInBytes)
        writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt.h")) (asn1crt_h.Replace("#define WORD_SIZE	8", intSize).Replace("#define FP_WORD_SIZE	8", fpSize) )
                
        match args.encodings with
        | []    -> ()
        | _     ->
            let asn1crt_encoding_c = rm.GetString("asn1crt_encoding_c",null)
            let asn1crt_encoding_c = 
                match args.streamingModeSupport with
                | false -> asn1crt_encoding_c
                | true  -> asn1crt_encoding_c.Replace("#define INTERNAL_FETH_DATA","")

            writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding.c")) asn1crt_encoding_c
            writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding.h")) (rm.GetString("asn1crt_encoding_h",null))

            if hasUper || hasAcn then
                writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_uper.c")) (rm.GetString("asn1crt_encoding_uper_c",null))
                writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_uper.h")) (rm.GetString("asn1crt_encoding_uper_h",null))

            if hasAcn  then
                writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_acn.c")) (rm.GetString("asn1crt_encoding_acn_c",null))
                writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_acn.h")) (rm.GetString("asn1crt_encoding_acn_h",null))

            if hasXer  then
                writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_xer.c")) (rm.GetString("asn1crt_encoding_xer_c",null))
                writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_xer.h")) (rm.GetString("asn1crt_encoding_xer_h",null))

            if hasBer  then
                writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_ber.c")) (rm.GetString("asn1crt_encoding_ber_c",null))
                writeTextFile (Path.Combine(asn1rtlDirName, "asn1crt_encoding_ber.h")) (rm.GetString("asn1crt_encoding_ber_h",null))
    | ProgrammingLanguage.Ada ->
        writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl.adb")) (rm.GetString("adaasn1rtl_adb",null))
        let adaasn1rtl_ads = rm.GetString("adaasn1rtl_ads",null)
        match args.floatingPointSizeInBytes  = 4I with 
        | true  -> writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl.ads")) (adaasn1rtl_ads.Replace("subtype Asn1Real is Standard.Long_Float;","subtype Asn1Real is Standard.Float;"))
        | false -> writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl.ads")) (adaasn1rtl_ads) 

        match args.encodings with
        | []    -> ()
        | _     ->
            let adaasn1rtl_encoding_adb = 
                match args.streamingModeSupport with
                | false -> rm.GetString("adaasn1rtl_encoding_adb",null)//.Replace("--  with user_code;","").Replace("--  user_code.push_data(bs, bs.pushDataPrm);","--").Replace("--  user_code.fetch_data(bs, bs.fetchDataPrm);","")
                | true  -> rm.GetString("adaasn1rtl_encoding_adb",null).Replace("--  with user_code;","with user_code;").Replace("--  user_code.push_data(bs, bs.pushDataPrm);","user_code.push_data(bs, bs.pushDataPrm);").Replace("--  user_code.fetch_data(bs, bs.fetchDataPrm);","user_code.fetch_data(bs, bs.fetchDataPrm);")

            writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding.adb")) adaasn1rtl_encoding_adb
            writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding.ads")) (rm.GetString("adaasn1rtl_encoding_ads",null))

            if hasUper || hasAcn then
                writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-uper.adb")) (rm.GetString("adaasn1rtl_encoding_uper_adb",null))
                writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-uper.ads")) (rm.GetString("adaasn1rtl_encoding_uper_ads",null))
                
            if hasAcn  then
                writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-acn.adb")) (rm.GetString("adaasn1rtl_encoding_acn_adb",null))
                writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-acn.ads")) (rm.GetString("adaasn1rtl_encoding_acn_ads",null))

            if hasXer  then
                writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-xer.adb")) (rm.GetString("adaasn1rtl_encoding_xer_adb",null))
                writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-xer.ads")) (rm.GetString("adaasn1rtl_encoding_xer_ads",null))

        match args.generateAutomaticTestCases with
        | true  ->
            writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-test_cases_aux.adb")) (rm.GetString("adaasn1rtl_encoding_test_cases_aux_adb",null))
            writeTextFile (Path.Combine(asn1rtlDirName, "adaasn1rtl-encoding-test_cases_aux.ads")) (rm.GetString("adaasn1rtl_encoding_test_cases_aux_ads",null))
        | false -> ()

        let writeBoard boardName = 
            let outDir = Path.Combine(boardsDirName, boardName)
            writeTextFile (Path.Combine(outDir, "IgnoredExaminerWarnings.wrn"))     (rm.GetString("IgnoredExaminerWarnings",null)) 
            writeTextFile (Path.Combine(outDir, "gnat.cfg"))    (rm.GetString("gnat",null)) 
            //writeTextFile (Path.Combine(outDir, "runSpark.sh"))    (rm.GetString("run",null)) 
            writeTextFile (Path.Combine(outDir, "GPS_project.gpr"))    (rm.GetString("GPS_project",null)) 

        OutDirectories.getBoardNames l |> List.iter writeBoard

