// For more information see https://aka.ms/fsharp-console-apps

open FSharp.Collections.ParallelSeq
open Argu
open System
open System.IO
open ShellProcess

type CliArguments =
    | [<Unique; AltCommandLine("-ac")>]     Asn1scc_Location of asn1sccdll:string
    | [<Unique; AltCommandLine("-tcd")>]    Test_Cases_Dir of dir:string
    | [<Unique; AltCommandLine("-wd")>]     Work_Dir of dir:string
    | [<Unique; AltCommandLine("-t")>]      Test_Case of test_asn1_file:string
    | [<Unique; AltCommandLine("-l")>]      Language of lang_id:string
    | [<Unique; AltCommandLine("-s")>]      Slim of enable:bool
    | [<Unique; AltCommandLine("-ws")>]     Word_Size of int
    | [<Unique; AltCommandLine("-p")>]      Parallel of int 
    | [<Unique; AltCommandLine("-ig")>]     Init_Globals
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Asn1scc_Location _ -> "specify the location of the asn1scc.dll"
            | Test_Cases_Dir _   -> "specify the directory that contains the test case files"
            | Work_Dir       _   -> "specify the working directory"
            | Test_Case      _   -> ""
            | Language       _   -> "c, Ada or Scala"
            | Slim           _   -> ""
            | Word_Size      _   -> "8 or 4"
            | Parallel       _   -> ""
            | Init_Globals       -> "generate const globals for types initialization. Applicable only to C."

let checkArguement arg =
    match arg with
    | Test_Cases_Dir _   -> ()
    | Work_Dir       _   -> ()
    | Test_Case      _   -> ()
    | Language       _   -> ()
    | Slim           _   -> ()
    | Word_Size      _   -> ()
    | Parallel       _   -> ()
    | Asn1scc_Location _ -> ()
    | Init_Globals       -> ()

let StrJoin str listItems =
    if Seq.isEmpty listItems then 
        ""
    else
        listItems |> Seq.map(fun x -> x.ToString()) |> Seq.reduce(fun agr el -> agr + str + el.ToString())

type Test_Case =
    | SuccessLine of    (string*string)
    | ErrorLine of      (string*string*string)
    | SuccessFile of    (string*string)
    | ErrorFile of      (string*string*string)
    with    
        member this.asn1 =
            match this with
            | SuccessLine (a,_)     -> a
            | ErrorLine   (a,_,_)   -> a
            | SuccessFile (a,_)     -> a
            | ErrorFile   (a,_,_)   -> a
        member this.acn =
            match this with
            | SuccessLine (_,a)     -> a
            | ErrorLine   (_,a,_)   -> a
            | SuccessFile (_,a)     -> a
            | ErrorFile   (_,a,_)   -> a
        member this.sAsn1 =
            let p = this.asn1.Split (Path.DirectorySeparatorChar)
            p[p.Length - 2 ..] |> StrJoin (Path.DirectorySeparatorChar.ToString())





let collectTestAsn1Files (test_cases_dir:string) =
    let allfiles = 
        Directory.GetFiles(test_cases_dir, "*.asn1", SearchOption.AllDirectories) |>
        Seq.map(fun s -> Path.GetFullPath s) |> 
        Seq.toList
    
    let parseAsn1File (asn1File:String) =
            File.ReadAllLines(asn1File) |> 
            Seq.toList |>
            List.choose(fun l -> 
                let folder = Path.GetDirectoryName asn1File
                if   l.StartsWith("--TCLS") then (Some (SuccessLine (asn1File, (l.Split "--TCLS")[1] ) ))
                elif   l.StartsWith("--TCFS") then (Some (SuccessFile (asn1File, Path.Combine(folder, ((l.Split "--TCFS")[1]).Trim() )) ))
                elif l.StartsWith("--TCLFC") then 
                    let rest = ((l.Split "--TCLFC")[1]).Trim()
                    let arr = rest.Split("$$$")
                    (Some (ErrorLine (asn1File, arr[0], arr[1].Trim()) ))
                elif l.StartsWith("--TCFFC") then 
                    let rest = ((l.Split "--TCFFC")[1]).Trim()
                    let arr = rest.Split("$$$")
                    (Some (ErrorFile (asn1File, Path.Combine(folder, arr[0].Trim()), arr[1].Trim()) ))
                else None)

    let test_cases =
        allfiles |> List.collect parseAsn1File
    test_cases
    //allfiles |> Seq.iter (fun z -> printfn "%s" z)
    


let emptyFolder (workDir) =
    let di = DirectoryInfo(workDir)
    di.GetFiles() |> Seq.iter (fun f -> f.Delete())
    di.GetDirectories() |> Seq.iter (fun f -> f.Delete true)

let prepareFolderAndFiles workDir  (t:Test_Case) =
    let createFileContent = 
        sprintf """-- Auto generated file
TEST-CASE DEFINITIONS ::= BEGIN

	%s
END
""" 
    let asn1FileName, acnContent =
        match t with
        | SuccessLine (a,c)     -> a, (createFileContent c)
        | ErrorLine   (a,c,_)   -> a, (createFileContent c)
        | SuccessFile (a,c)     -> a, File.ReadAllText c
        | ErrorFile   (a,c,_)   -> a, File.ReadAllText c
      
    //printfn "%s" workDir
    emptyFolder workDir
    File.Copy(asn1FileName, Path.Combine(workDir, "sample1.asn1"))
    File.WriteAllText(Path.Combine(workDir, "sample1.acn"), acnContent)
    
let executeTestCase asn1sccdll workDir  (t:Test_Case) (lang:string, ws:int, slim:bool, enableIG:bool) =
    let wsa = if ws=8 then "-fpWordSize 8 -wordSize 8" else "-fpWordSize 4 -wordSize 4"
    let slima = if slim then "-slim" else ""
    let ig = if enableIG then "-ig" else ""
    let target = 
        if lang = "Ada" && ws = 4 then 
            " -t msp430 "
        else
            ""
    
    let cmd = $"%s{asn1sccdll} -%s{lang} -x ast.xml -uPER -ACN %s{ig} -typePrefix ASN1SCC_ -renamePolicy 3 -fp AUTO -equal -atc  %s{slima} %s{wsa} %s{target} sample1.asn1 sample1.acn"
    //let cmd = sprintf "%s -%s -x ast.xml -uPER -ACN -ig -typePrefix ASN1SCC_ -renamePolicy 3 -fp AUTO -equal -atc  %s %s %s sample1.asn1 sample1.acn" asn1sccdll lang slima wsa target
    prepareFolderAndFiles workDir t
    //ShellProcess.printInfo "\nwordDir is:%s\n%s\n\n" workDir cmd
    let res = executeProcess workDir "dotnet" cmd

    let checkErrMsg expErrMsg (res:ProcessResult) =
        match res.ExitCode with
        | 0     -> 
            markError "test case %s expected to fail but passed!\n%s" t.asn1 cmd
        | _  when expErrMsg <> res.StdErr -> markError "test case %s failed.Expected error messager was\n'%s'\ngot\n'%s'\n%s" t.asn1 expErrMsg res.StdErr cmd
        | _     -> markSuccess "asn1scc failed with the expected error message!" 

    let invokeMake (t:Test_Case) : TestCaseResult=
        let asn1Lines = File.ReadAllLines t.asn1
        let bNoAtc = asn1Lines |> Seq.exists(fun l -> l.Contains "NO_AUTOMATIC_TEST_CASES")
        let bRunCodeCoverage = not (asn1Lines |> Seq.exists(fun l -> l.Contains "NOCOVERAGE"))
        //ShellProcess.printDebug "bRunCodeCoverage %b" bRunCodeCoverage
        let bRunSpark = (lang = "Ada") && asn1Lines |> Seq.exists(fun l -> l.Contains "RUN_SPARK")
        let ada_target = 
            if ws = 4 then 
                "obj_msp430"
            else
                "obj_x86"

        let coverageFile = Path.Combine(workDir, (if lang = "c" then "sample1.c.gcov" else (ada_target + "/debug/test_case.adb.gcov")))
        let covLinesToIgnore = ["}";"default:";"break;";"end"] |> Set.ofList

        let makeCommand = sprintf "make%s" (if bNoAtc || not bRunCodeCoverage then "" else " coverage")
        let res = executeBashScript workDir makeCommand
        if res.ExitCode <> 0 then    
            markError "Error code is %d\n%s\n%s" res.ExitCode res.StdErr cmd
        else
            if (bNoAtc || not bRunCodeCoverage) then
                markSuccess "Make OK" 
            else
                let restoreSrcLine (l:string) = 
                    ((l.Split(':')[2..]) |> StrJoin ":").Trim()
                let covLines = 
                    File.ReadAllLines coverageFile |>
                    Seq.filter(fun l -> l.Contains("###")) |>
                    Seq.filter(fun l -> not (l.Contains "COVERAGE_IGNORE")) |> 
                    Seq.filter(fun l -> not (covLinesToIgnore.Contains (restoreSrcLine l))) |> 
                    Seq.toList
                if not (List.isEmpty covLines) then
                    markError "Code Coverage Failed. See %s\n%s\n%s" coverageFile covLines.Head cmd
                else 
                    if bRunSpark then
                        let bWindows = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) 
                        let makeCommand = sprintf "gnatprove%s -Pasn1_x86.gpr -j0  -u test_case.adb --level=4 >sparklog.txt 2>&1" (if bWindows then ".exe" else "")
                        let res = executeBashScript workDir makeCommand
                        let sparkLogFname = Path.Combine(workDir, "sparklog.txt")
                        if res.ExitCode <> 0 then 
                            markError "SPARK Failed. See %s\n%s" sparkLogFname cmd
                        else
                            let sparkLog = File.ReadLines sparkLogFname
                            let bSparkFailed = 
                                sparkLog |> Seq.exists(fun l -> l.Contains "might fail, cannot prove")
                            if bSparkFailed then 
                                markError "SPARK Failed. See %s\n%s" sparkLogFname cmd
                            else 
                                markSuccess "Make OK, Code Coverage OK and SPARK OK" 
                    else
                        markSuccess "Make and Code Coverage OK" 


        

    match t with
    | SuccessLine (a,c)             -> invokeMake t
    | ErrorLine   (a,c,errMsg)      -> checkErrMsg errMsg res
    | SuccessFile (a,c)             -> invokeMake t
    | ErrorFile   (a,c,errMsg)      -> checkErrMsg errMsg res
let dirSafeDelete dirName =
    
    let rec dirSafeDelete_aux tries dirName =
        try
            Directory.Delete (dirName,true)
        with
            | ex  ->
                if ex.Message.Contains("because it is being used by another process.") && tries > 0 then
                    System.Threading.Thread.Sleep(2000);
                    dirSafeDelete_aux (tries - 1) dirName
                else
                    raise ex;
    dirSafeDelete_aux 5 dirName

        
let executeTestCaseSync asn1sccdll workDir (i:int) (t:Test_Case) (lang:string, ws:int, slim:bool, enableIG:bool) =
    let rndDir = Path.GetRandomFileName()
    let newWorkDir = Path.Combine(workDir, rndDir)
    Directory.CreateDirectory newWorkDir |> ignore
    let ret = executeTestCase asn1sccdll newWorkDir t (lang, ws, slim, enableIG)    
    match ret with
    | Success _     -> 
        dirSafeDelete newWorkDir
    | Failed  _     -> ()
    (i, ret, rndDir, t, lang, ws, slim)

let executeTestCaseAsync asn1sccdll workDir (i:int) (t:Test_Case) (lang:string, ws:int, slim:bool, enableIG:bool) =
    async {
    (*
        let rndDir = Path.GetRandomFileName()
        let newWorkDir = Path.Combine(workDir, rndDir)
        Directory.CreateDirectory newWorkDir |> ignore
        let ret = executeTestCase asn1sccdll newWorkDir t (lang, ws, slim)    
        match ret with
        | Success _     -> 
            dirSafeDelete newWorkDir
        | Failed  _     -> ()
        return (i, ret, rndDir, t, lang, ws, slim)
        *)
        let aa = executeTestCaseSync asn1sccdll workDir i t (lang, ws, slim, enableIG)
        return aa
    }


let rec executeBatch (batchSize:int) fnc initialList =
    let intCnt = initialList |> Seq.toList |> List.length
    let rec executeBatch_aux curResult lst =
        let actBatchSize = min batchSize (Seq.length lst)
        let witems = lst |> Seq.take actBatchSize
        let rest = lst |> Seq.skip actBatchSize
        let executeItems witems =
            witems
            |> Seq.map fnc
            |> Async.Parallel
            |> Async.RunSynchronously
        let ret1 = executeItems witems |> Seq.toList
        let newCurResult = curResult@ret1
        printInfo "Progress %d/%d\n" newCurResult.Length intCnt
        match Seq.isEmpty rest with
        | true    -> newCurResult
        | false   -> 
            let ret = executeBatch_aux newCurResult  rest
            ret
    executeBatch_aux [] initialList

let main0 argv =
    
    let parser = ArgumentParser.Create<CliArguments>(programName = "asn1scc.exe")
    try
        let parserResults = parser.Parse argv
        let cliArgs = parserResults.GetAllResults()
        cliArgs |> Seq.iter checkArguement 



        let workDir = 
            Path.GetFullPath(
                parserResults.GetResult(<@Work_Dir@>, defaultValue = "work_dir"))

        let asn1sccdll =
            Path.GetFullPath(
                parserResults.GetResult(<@Asn1scc_Location@>, defaultValue = "../asn1scc/bin/Debug/net9.0/asn1scc.dll"))

        let word_sizes =
            match parserResults.Contains <@ Word_Size @> with
            | false -> 8
            | true  -> parserResults.GetResult(<@ Word_Size @>)
            
        let test_cases_dir = 
            parserResults.GetResult(<@Test_Cases_Dir@>, defaultValue = (if word_sizes = 8 then "test-cases" else "test-cases-32"))

    

        let threadPoolSize = 
            match parserResults.Contains <@ Parallel @> with
            | true -> parserResults.GetResult(<@ Parallel @>)
            | false -> 1
        
        let all_tests = collectTestAsn1Files test_cases_dir

        let tc = 
            match parserResults.TryGetResult(<@Test_Case@>) with
            | None -> None
            | Some tc -> Some (Path.GetFullPath tc)


        let languages =
            match parserResults.Contains <@ Language @> with
            | false -> ["c"; "Ada"; "Scala"]
            | true  -> [parserResults.GetResult(<@ Language @>)]


        let slim_modes =
            match parserResults.Contains <@ Slim @> with
            | false -> [true; false]
            | true  -> [parserResults.GetResult(<@ Slim @>)]
        let enableIG = parserResults.Contains <@ Init_Globals @> 

        let asn1sccModes =
            seq {
                for l in languages do
                    for ws in [word_sizes] do
                        for sm in slim_modes do
                            yield (l,ws,sm, enableIG)
            } |> Seq.toList
        let asn1sccInvoications =
            seq {
                for tc in all_tests do
                    for m in asn1sccModes do
                        yield (tc, m)
            } |> Seq.toList

        //asn1sccInvoications |>
        let results0 = 
            asn1sccInvoications |>
            List.filter(fun (t,_) -> match tc with None -> true | Some s -> t.asn1.EndsWith s) |>
            List.mapi(fun i (t,b) -> (i,t, b)) 
        let results =
            match threadPoolSize > 1 with 
            | true ->
                results0 |>
                executeBatch threadPoolSize (fun (i,tc, m) -> executeTestCaseAsync asn1sccdll workDir i tc m) |>
                List.sortBy (fun (i, _, _, _, _, _, _) -> i)
            | false  ->
                results0 |> List.map (fun (i,tc, m) -> executeTestCaseSync asn1sccdll workDir i tc m) |>
                List.sortBy (fun (i, _, _, _, _, _, _) -> i)


        let errors = 
            results |> List.filter (fun (i, ret, _, _, _, _, _) ->  match ret with Failed _ -> true | Success _ -> false)
        let sucRes = results |> List.filter (fun (i, ret, _, _, _, _, _) ->  match ret with Failed _ -> false | Success _ -> true)

        sucRes |> 
        List.iter (fun (i, ret, rndDir, t, lang, ws, slim) -> 
            printInfo "%s, l=%s, ws=%d, slim=%b, wd=%s, acn=%s --> " t.sAsn1 lang ws slim rndDir (t.acn.Trim())
            printSuccess "OK\n"
        )
        errors |> 
        List.iter (fun (i, ret, rndDir, t, lang, ws, slim) -> 
            printInfo "%s, l=%s, ws=%d, slim=%b, wd=%s, acn=%s --> " t.sAsn1 lang ws slim rndDir (t.acn.Trim())
            match ret with
            | Failed errMsg -> 
                printError "Failed\n%s\n" errMsg
            | Success _ -> ()
        )

        (if errors.IsEmpty then 0 else 1)
    with
        | :? Argu.ArguParseException as ex -> 
            Console.Error.WriteLine(ex.Message)
            1    
        | ex            ->
            Console.Error.WriteLine(ex.Message)
            Console.Error.WriteLine( ex.StackTrace)
            4

[<EntryPoint>]
let main argv = 
    main0 argv
    