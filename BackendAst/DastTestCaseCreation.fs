module DastTestCaseCreation
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open AbstractMacros
open DAst
open DAstUtilFunctions
open Language


let GetEncodingString l = function    
    | UPER  -> match l with C -> "" | Ada -> "UPER_"
    | ACN   -> "ACN_"
    | BER   -> "BER_"
    | XER   -> "XER_"

let includedPackages r l =  
    match l with
    | C     -> r.programUnits |> Seq.map(fun x -> x.tetscase_specFileName)
    | Ada   -> r.programUnits |> Seq.collect(fun x -> [x.name; x.tetscase_name])


let rec gAmber (t:Asn1Type) = 
    match t.Kind with
    | Integer      _ -> "&"  , "&"
    | Real         _ -> "&"  , "&"
    | IA5String    _ -> ""  , ""
    | OctetString  _ -> "&" , "&"
    | NullType     _ -> "&"  , "&"
    | BitString    _ -> "&" , "&"
    | Boolean      _ -> "&"  , "&"
    | Enumerated   _ -> "&"  , "&"
    | SequenceOf   _ -> "&" , "&"
    | Sequence     _ -> "&" , "&"
    | Choice       _ -> "&" , "&"
    | ObjectIdentifier _ -> "&" , "&"
    | TimeType      _   -> "&" , "&"
    | ReferenceType r -> gAmber r.resolvedType

let emitTestCaseAsFunc l    =         match l with C -> test_cases_c.emitTestCaseAsFunc                   | Ada -> test_cases_a.emitTestCaseAsFunc
let emitTestCaseAsFunc_h  l =         match l with C -> test_cases_c.emitTestCaseAsFunc_h               | Ada -> test_cases_a.emitTestCaseAsFunc_h
let invokeTestCaseAsFunc  l =         match l with C -> test_cases_c.invokeTestCaseAsFunc               | Ada -> test_cases_a.invokeTestCaseAsFunc
let emitTestCaseAsFunc_dummy_init l = match l with C -> test_cases_c.emitTestCaseAsFunc_dummy_init | Ada -> test_cases_a.emitTestCaseAsFunc_dummy_init
let emitTestCaseAsFunc_dummy_init_function l = match l with C -> test_cases_c.emitTestCaseAsFunc_dummy_init_function | Ada -> test_cases_a.emitTestCaseAsFunc_dummy_init_function


let GetDatFile (r:DAst.AstRoot) l (v:ValueAssignment) modName sTasName encAmper (enc:Asn1Encoding) = 
    let generate_dat_file  = match l with C -> test_cases_c.PrintSuite_call_codec_generate_dat_file | Ada -> test_cases_a.PrintSuite_call_codec_generate_dat_file
    let bGenerateDatFile = (r.args.CheckWithOss && v.Name.Value = "testPDU")
    match bGenerateDatFile, enc with
    | false,_     -> ""
    | true, ACN   -> ""
    | true, XER   -> generate_dat_file modName sTasName encAmper (GetEncodingString l enc) "Byte"
    | true, BER   -> generate_dat_file modName sTasName encAmper (GetEncodingString l enc) "Byte"
    | true, uPER  -> generate_dat_file modName sTasName encAmper (GetEncodingString l enc) "Bit"

let PrintValueAssignmentAsTestCase (r:DAst.AstRoot) l lm (e:Asn1Encoding) (v:ValueAssignment) (m:Asn1Module) (typeModName:string) (sTasName : string)  (idx :int) dummyInitStatementsNeededForStatementCoverage  =
    let modName = typeModName//ToC m.Name.Value
    let sFuncName = sprintf "test_case_%A_%06d" e idx
    let encAmper, initAmper = gAmber v.Type
    let curProgramUnitName = ""  //Main program has no module
    let initStatement = DAstVariables.printValue r l lm curProgramUnitName v.Type None v.Value.kind
    let sTestCaseIndex = idx.ToString()
    let bStatic = match v.Type.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
    let GetDatFile = GetDatFile r l v modName sTasName encAmper
    let func_def = emitTestCaseAsFunc_h l sFuncName
    let func_dody = emitTestCaseAsFunc l sFuncName [] modName sTasName encAmper (GetEncodingString l e) true initStatement bStatic "" dummyInitStatementsNeededForStatementCoverage initAmper
    let func_invokation = invokeTestCaseAsFunc l sFuncName
    (func_def, func_dody, func_invokation)
    
let PrintAutomaticTestCase (r:DAst.AstRoot) l (e:Asn1Encoding) (initStatement:String) (localVars : LocalVariable list) (m:Asn1Module) (t:Asn1Type) (modName : string) (sTasName : string)  (idx :int) initFuncName  =
    let sFuncName = sprintf "test_case_%A_%06d" e idx
    //let modName = ToC m.Name.Value
    let arrsVars = localVars |> List.map(fun lv -> lv.GetDeclaration l) |> Seq.distinct |> Seq.toList

    let encAmper, initAmper = gAmber t
    let bStatic = match t.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
    let GetDatFile = ""
    let sTestCaseIndex = idx.ToString()
    let func_def = emitTestCaseAsFunc_h l sFuncName
    let func_dody = emitTestCaseAsFunc l sFuncName arrsVars modName sTasName encAmper (GetEncodingString l e) false initStatement bStatic "" initFuncName initAmper
    let func_invokation = invokeTestCaseAsFunc l sFuncName
    (func_def, func_dody, func_invokation)

let getTypeDecl (r:DAst.AstRoot) (vasPU_name:string) (l:ProgrammingLanguage) (vas:ValueAssignment) =
    let t = vas.Type
    match t.Kind with
    | Integer _
    | Real _
    | Boolean _     -> t.typeDefintionOrReference.longTypedefName l
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



let TestSuiteFileName = "testsuite"

let emitDummyInitStatementsNeededForStatementCoverage (lm:Language.LanguageMacros) l (t:Asn1Type) =
    let pdumy = {CallerScope.modName = ToC "MainProgram"; arg = VALUE "tmp0"}
    GetMySelfAndChildren2 l t pdumy |>
    List.choose(fun (t,p) -> 
        let initProc = t.initFunction.initProcedure
        let dummyVarName = 
            match t.isIA5String with
            | true  ->  p.arg.getValue l
            | false ->  p.arg.getPointer l
        let sTypeName = t.typeDefintionOrReference.longTypedefName l
        let sTypeName = 
            match l with
            | C -> sTypeName
            | Ada -> 
                match sTypeName.Contains "." with
                | true  -> sTypeName
                | false -> t.FT_TypeDefintion.[l].programUnit + "." + sTypeName
        match initProc with
        | None  -> None
        | Some initProc ->  
            match lm.lg.initMetod with
            | InitMethod.Procedure ->
                Some (emitTestCaseAsFunc_dummy_init l sTypeName initProc.funcName dummyVarName)
            | InitMethod.Function ->
                Some (emitTestCaseAsFunc_dummy_init_function l sTypeName initProc.funcName dummyVarName) )


let printAllTestCases (r:DAst.AstRoot) l lm outDir =
    let tcFunctors = 
        seq {
            for m in r.Files |> List.collect(fun f -> f.Modules) do
                for e in r.args.encodings do
                    for t in m.TypeAssignments do
                        let encDecTestFunc = t.Type.getEncDecTestFunc e
                        match encDecTestFunc with
                        | Some _    ->
                            let hasEncodeFunc = e <> Asn1Encoding.ACN ||  hasAcnEncodeFunction t.Type.acnEncFunction t.Type.acnParameters 
                            if hasEncodeFunc   then
                                let isTestCaseValid atc =
                                    match t.Type.acnEncFunction with
                                    | None  -> false
                                    |Some ancEncFnc -> ancEncFnc.isTestVaseValid atc
                                for atc in t.Type.initFunction.automaticTestCases  do
                                    let testCaseIsValid = e <> Asn1Encoding.ACN || (isTestCaseValid atc)
                                    if testCaseIsValid then
                                        let generateTcFun idx = 
                                            let p = {CallerScope.modName = ToC "MainProgram"; arg = VALUE "tc_data"}
                                            let initStatement = atc.initTestCaseFunc p
                                            let dummyInitStatementsNeededForStatementCoverage = (emitDummyInitStatementsNeededForStatementCoverage lm l t.Type)//t.Type.initFunction.initFuncName
                                            
                                            PrintAutomaticTestCase r l e initStatement.funcBody initStatement.localVariables  m t.Type (t.Type.FT_TypeDefintion.[l].programUnit) (t.Type.FT_TypeDefintion.[l].typeName) idx dummyInitStatementsNeededForStatementCoverage 
                                        yield generateTcFun
                        | None  -> () 
                    for v in m.ValueAssignments do
                        let encDecTestFunc, typeModName, tasName = 
                            //does not work for Ada (tasname is not calculated correctly, needs to be fixed)
//                            match l with
//                            | C ->
                                match v.Type.Kind with
                                | ReferenceType   ref ->
                                    ref.resolvedType.getEncDecTestFunc e, (ToC ref.baseInfo.modName.Value), (ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value) )
                                | _                  -> v.Type.getEncDecTestFunc e,  (ToC m.Name.Value), v.Type.typeDefintionOrReference.longTypedefName l
//                            | Ada ->
//                                v.Type.getEncDecTestFunc e, (getTypeDecl r (ToC m.Name.Value) l v )
                        match encDecTestFunc with
                        | Some _    ->
                            let generateTcFun idx = 
                                //let initFuncName = v.Type.initFunction.initFuncName
                                let dummyInitStatementsNeededForStatementCoverage = (emitDummyInitStatementsNeededForStatementCoverage lm l v.Type)
                                PrintValueAssignmentAsTestCase r l lm e v m typeModName tasName (*(getTypeDecl r (ToC m.Name.Value) l v )*)  idx dummyInitStatementsNeededForStatementCoverage 
                            yield generateTcFun
                        | None         -> ()
        } |> Seq.toList
    let maxTestCasesPerFile = 100.0
    let nMaxTestCasesPerFile = int maxTestCasesPerFile

    let nFiles = int (Math.Ceiling( (double tcFunctors.Length) / maxTestCasesPerFile))

    let printTestCaseFileDef       =         match l with C -> test_cases_c.printTestCaseFileDef                   | Ada -> test_cases_a.printTestCaseFileDef
    let printTestCaseFileBody      =         match l with C -> test_cases_c.printTestCaseFileBody                  | Ada -> test_cases_a.printTestCaseFileBody

    let arrsSrcTstFiles, arrsHdrTstFiles =
        [1 .. nFiles] |> 
        List.map (fun fileIndex ->
            let testCaseFileName = sprintf "test_case_%03d" fileIndex
            testCaseFileName + "." + l.BodyExtention, testCaseFileName + "." + l.SpecExtention) |>
        List.unzip



    
    [1 .. nFiles] |> 
    List.iter (fun fileIndex ->
        let arrsTestFunctionDefs, arrsTestFunctionBodies,_ = 
            tcFunctors |> 
            Seq.mapi (fun i f -> i+1,f) |> 
            Seq.filter(fun (i,_) -> i > nMaxTestCasesPerFile * (fileIndex-1) && i <= nMaxTestCasesPerFile * fileIndex) |>
            Seq.map(fun (i, fnc) -> fnc i) |>
            Seq.toList |> List.unzip3

                 
        let testCaseFileName = sprintf "test_case_%03d" fileIndex

        let contentC = printTestCaseFileBody testCaseFileName (includedPackages r l) arrsTestFunctionBodies
        let outCFileName = Path.Combine(outDir, testCaseFileName + "." + l.BodyExtention)
        File.WriteAllText(outCFileName, contentC.Replace("\r",""))

        let contentH = printTestCaseFileDef testCaseFileName (includedPackages r l) arrsTestFunctionDefs
        let outHFileName = Path.Combine(outDir, testCaseFileName + "." + l.SpecExtention)
        File.WriteAllText(outHFileName, contentH.Replace("\r",""))  )

    let _, _, func_invokations = 
        tcFunctors |>
        Seq.mapi (fun i f -> i+1,f) |> 
        Seq.map(fun (i, fnc) -> fnc i) |>
        Seq.toList |> List.unzip3

    let includedPackages =
        [1 .. nFiles] |> 
        List.map (fun fileIndex -> sprintf "test_case_%03d" fileIndex )

    match l with
    | C ->
        let contentC = test_cases_c.PrintATCRunner TestSuiteFileName includedPackages [] func_invokations [] [] false
        let outCFileName = Path.Combine(outDir, TestSuiteFileName + "." + l.BodyExtention)
        File.WriteAllText(outCFileName, contentC.Replace("\r",""))
        
        let contentH = test_cases_c.PrintATCRunnerDefinition()
        let outHFileName = Path.Combine(outDir, TestSuiteFileName + "." + l.SpecExtention)
        File.WriteAllText(outHFileName, contentH.Replace("\r",""))

    | Ada ->
        let contentC = test_cases_a.PrintATCRunner TestSuiteFileName includedPackages [] func_invokations [] [] false
        let outCFileName = Path.Combine(outDir, "mainprogram." + l.BodyExtention)
        File.WriteAllText(outCFileName, contentC.Replace("\r",""))

    arrsSrcTstFiles, arrsHdrTstFiles
    //tcFunctors |> Seq.mapi (fun i fnc -> fnc i)
        
