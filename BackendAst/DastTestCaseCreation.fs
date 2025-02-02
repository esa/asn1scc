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


let GetEncodingString (lm:LanguageMacros) = function
    | UPER  -> lm.lg.atc.uperPrefix
    | ACN   -> lm.lg.atc.acnPrefix
    | BER   -> lm.lg.atc.berPrefix
    | XER   -> lm.lg.atc.xerPrefix

let includedPackages r (lm:LanguageMacros) =
    match lm.lg.hasModules with
    | false     -> r.programUnits |> Seq.map(fun x -> x.testcase_specFileName)
    | true      -> r.programUnits |> Seq.collect(fun x -> [x.name; x.testcase_name])


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

let emitTestCaseAsFunc                      (lm:LanguageMacros) = lm.atc.emitTestCaseAsFunc
let emitTestCaseAsFunc_h                    (lm:LanguageMacros) = lm.atc.emitTestCaseAsFunc_h
let invokeTestCaseAsFunc                    (lm:LanguageMacros) = lm.atc.invokeTestCaseAsFunc
let emitTestCaseAsFunc_dummy_init           (lm:LanguageMacros) = lm.atc.emitTestCaseAsFunc_dummy_init
let emitTestCaseAsFunc_dummy_init_function  (lm:LanguageMacros) = lm.atc.emitTestCaseAsFunc_dummy_init_function


let GetDatFile (r:DAst.AstRoot) lm (v:ValueAssignment) modName sTasName encAmper (enc:Asn1Encoding) =
    let generate_dat_file  = lm.atc.PrintSuite_call_codec_generate_dat_file
    let bGenerateDatFile = (r.args.CheckWithOss && v.Name.Value = "testPDU")
    match bGenerateDatFile, enc with
    | false,_     -> ""
    | true, ACN   -> ""
    | true, XER   -> generate_dat_file modName sTasName encAmper (GetEncodingString lm enc) "Byte"
    | true, BER   -> generate_dat_file modName sTasName encAmper (GetEncodingString lm enc) "Byte"
    | true, UPER  -> generate_dat_file modName sTasName encAmper (GetEncodingString lm enc) "Bit"

let PrintValueAssignmentAsTestCase (r:DAst.AstRoot) lm (e:Asn1Encoding) (v:ValueAssignment) (m:Asn1Module) (typeModName:string) (sTasName : string)  (idx :int) dummyInitStatementsNeededForStatementCoverage  =
    let modName = typeModName//ToC m.Name.Value
    let sFuncName = sprintf "test_case_%A_%06d" e idx
    let encAmper, initAmper = gAmber v.Type
    let initAmper =
        match ProgrammingLanguage.ActiveLanguages.Head with
        | Scala ->
            match v.Type.initFunction.initProcedure with
            | Some initProc -> initProc.funcName
            | None -> ""
        | _ -> initAmper
    let curProgramUnitName = ""  //Main program has no module
    let initStatement = DAstVariables.printValue r lm curProgramUnitName v.Type None v.Value.kind
    let initStatement =
        match ProgrammingLanguage.ActiveLanguages.Head with
        | Scala ->
            match resolveReferenceType v.Type.Kind with
             | Integer v -> "val tc_data = " + initStatement
             | Real v -> initStatement
             | IA5String v -> initStatement
             | OctetString v -> initStatement
             | NullType v -> initStatement
             | BitString v -> initStatement
             | Boolean v -> initStatement
             | Enumerated v -> initStatement
             | ObjectIdentifier v -> initStatement
             | SequenceOf v -> initStatement
             | Sequence v -> initStatement
             | Choice v -> initStatement
             | TimeType v -> initStatement
             | ReferenceType _ -> raise (BugErrorException "Impossible, since we have resolvedReferenceType")
        | _ -> initStatement
    let sTestCaseIndex = idx.ToString()
    let bStatic = match v.Type.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
    let GetDatFile = GetDatFile r lm v modName sTasName encAmper
    let func_def = emitTestCaseAsFunc_h lm sFuncName
    let func_body = emitTestCaseAsFunc lm sFuncName [] modName sTasName encAmper (GetEncodingString lm e) true initStatement bStatic "" dummyInitStatementsNeededForStatementCoverage initAmper
    let func_invocation = invokeTestCaseAsFunc lm sFuncName
    (func_def, func_body, func_invocation)

let PrintAutomaticTestCase (r:DAst.AstRoot) (lm:LanguageMacros) (e:Asn1Encoding) (initStatement:String) (localVars : LocalVariable list) (m:Asn1Module) (t:Asn1Type) (modName : string) (sTasName : string)  (idx :int) initFuncName  =
    let sFuncName = sprintf "test_case_%A_%06d" e idx
    //let modName = ToC m.Name.Value
    let arrsVars = localVars |> List.map(fun lv -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct |> Seq.toList

    let encAmper, initAmper = gAmber t
    let initAmper =
        match ProgrammingLanguage.ActiveLanguages.Head with
        | Scala ->
            match t.initFunction.initProcedure with
            | Some initProc -> initProc.funcName
            | None -> ""
        | _ -> initAmper
    let bStatic = match t.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
    let GetDatFile = ""
    let sTestCaseIndex = idx.ToString()
    let func_def = emitTestCaseAsFunc_h lm sFuncName
    let func_body = emitTestCaseAsFunc lm sFuncName arrsVars modName sTasName encAmper (GetEncodingString lm e) false initStatement bStatic "" initFuncName initAmper
    let func_invocation = invokeTestCaseAsFunc lm sFuncName
    (func_def, func_body, func_invocation)

let getTypeDecl (r:DAst.AstRoot) (vasPU_name:string) (lm:LanguageMacros) (vas:ValueAssignment) =
    let t = vas.Type
    match t.Kind with
    | Integer _
    | Real _
    | Boolean _     -> lm.lg.getLongTypedefName t.typeDefinitionOrReference
    | ReferenceType ref ->
        let tasName = ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value)
        match lm.lg.hasModules with
        | false     -> tasName
        | true ->
            match ToC vasPU_name = ToC ref.baseInfo.modName.Value with
            | true  -> tasName
            |false  -> (ToC ref.baseInfo.modName.Value) + "." + tasName


    | _             ->
        match t.tasInfo with
        | Some tasInfo    -> ToC2(r.args.TypePrefix + tasInfo.tasName)
        | None            -> lm.lg.getLongTypedefName t.typeDefinitionOrReference



let TestSuiteFileName = "testsuite"

let emitDummyInitStatementsNeededForStatementCoverage (lm:Language.LanguageMacros) (t:Asn1Type) =
    let pdummy = {CallerScope.modName = ToC "MainProgram"; arg = Selection.valueEmptyPath "tmp0" }
    let rec getInitializationFunctions (n:InitFunction) =
        seq {
            for c in n.nonEmbeddedChildrenFuncs do
                yield! getInitializationFunctions c
            yield n
        } |> Seq.toList
    let actualInitFuncNames =
        getInitializationFunctions t.initFunction |>
        List.choose (fun i ->
            match i.initProcedure with
            | None  -> None
            | Some initProc ->  Some initProc.funcName) |>
        Set.ofList

    GetMySelfAndChildren2 lm t pdummy |>
    List.choose(fun (t,p) ->
        let initProc = t.initFunction.initProcedure
        let dummyVarName =
            match t.isIA5String with
            | true  ->  lm.lg.getValue p.arg
            | false ->  lm.lg.getPointer p.arg
        let sTypeName = lm.lg.getLongTypedefName t.typeDefinitionOrReference
        let sTypeName =
            match lm.lg.hasModules with
            | false -> sTypeName
            | true ->
                match sTypeName.Contains "." with
                | true  -> sTypeName
                | false ->
                    (lm.lg.getTypeDefinition t.FT_TypeDefinition).programUnit + "." + sTypeName
        match initProc with
        | None  -> None
        | Some initProc when actualInitFuncNames.Contains initProc.funcName ->
            match lm.lg.initMethod with
            | InitMethod.Procedure ->
                Some (emitTestCaseAsFunc_dummy_init lm sTypeName initProc.funcName dummyVarName)
            | InitMethod.Function ->
                Some (emitTestCaseAsFunc_dummy_init_function lm sTypeName initProc.funcName dummyVarName)
        | Some _ -> None)

let asn1EncodingMapping = function
    | UPER  -> UperEncDecFunctionType
    | ACN   -> AcnEncDecFunctionType 
    | BER   -> BerEncDecFunctionType 
    | XER   -> XerEncDecFunctionType 

let printAllTestCasesAndTestCaseRunner (r:DAst.AstRoot) (lm:LanguageMacros) outDir =
    let tcFunctors =
        seq {
            for m in r.Files |> List.collect(fun f -> f.Modules) do
                for e in r.args.encodings do
                    for t in m.TypeAssignments do
                        let encDecTestFunc = t.Type.getEncDecTestFunc e
                        match encDecTestFunc with
                        | Some _    ->
                            let hasEncodeFunc = e <> Asn1Encoding.ACN ||  hasAcnEncodeFunction t.Type.acnEncFunction t.Type.acnParameters t.Type.id.tasInfo
                            let typeAssignmentInfo = t.Type.id.tasInfo.Value
                            let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}
                            let requiresTestCaseFunc = r.callersSet |> Set.contains (f (asn1EncodingMapping e))
                            if hasEncodeFunc && requiresTestCaseFunc  then
                                let isTestCaseValid atc =
                                    match t.Type.acnEncFunction with
                                    | None  -> false
                                    |Some ancEncFnc -> ancEncFnc.isTestVaseValid atc
                                for atc in t.Type.initFunction.automaticTestCases  do
                                    let testCaseIsValid = e <> Asn1Encoding.ACN || (isTestCaseValid atc)
                                    if testCaseIsValid then
                                        let generateTcFun idx =
                                            let p = {CallerScope.modName = ToC "MainProgram"; arg = Selection.valueEmptyPath "tc_data"}
                                            let initStatement = atc.initTestCaseFunc p
                                            let dummyInitStatementsNeededForStatementCoverage = (emitDummyInitStatementsNeededForStatementCoverage lm t.Type)//t.Type.initFunction.initFuncName

                                            PrintAutomaticTestCase r lm e initStatement.funcBody initStatement.localVariables  m t.Type ((lm.lg.getTypeDefinition t.Type.FT_TypeDefinition).programUnit) ((lm.lg.getTypeDefinition t.Type.FT_TypeDefinition).typeName) idx dummyInitStatementsNeededForStatementCoverage
                                        yield generateTcFun
                        | None  -> ()
                    for v in m.ValueAssignments do

                        let encDecTestFunc, typeModName, tasName =
                            match v.Type.Kind with
                            | ReferenceType   ref ->
                                ref.resolvedType.getEncDecTestFunc e, (ToC ref.baseInfo.modName.Value), (ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value) )
                            | _                  -> v.Type.getEncDecTestFunc e,  (ToC m.Name.Value), lm.lg.getLongTypedefName v.Type.typeDefinitionOrReference
                        match encDecTestFunc with
                        | Some _    ->
                            let generateTcFun idx =
                                let dummyInitStatementsNeededForStatementCoverage = (emitDummyInitStatementsNeededForStatementCoverage lm v.Type)
                                PrintValueAssignmentAsTestCase r lm e v m typeModName tasName (*(getTypeDecl r (ToC m.Name.Value) l v )*)  idx dummyInitStatementsNeededForStatementCoverage
                            yield generateTcFun
                        | None         -> ()
        } |> Seq.toList
    let maxTestCasesPerFile = 100.0
    let nMaxTestCasesPerFile = int maxTestCasesPerFile

    let nFiles = int (Math.Ceiling( (double tcFunctors.Length) / maxTestCasesPerFile))

    let printTestCaseFileDef       =         lm.atc.printTestCaseFileDef
    let printTestCaseFileBody      =         lm.atc.printTestCaseFileBody

    let arrsSrcTstFiles, arrsHdrTstFiles =
        [1 .. nFiles] |>
        List.map (fun fileIndex ->
            let testCaseFileName = sprintf "test_case_%03d" fileIndex
            testCaseFileName + "." + lm.lg.BodyExtension, testCaseFileName + lm.lg.SpecNameSuffix + "." + lm.lg.SpecExtension) |>
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

        let contentC = printTestCaseFileBody testCaseFileName (includedPackages r lm) arrsTestFunctionBodies
        let outCFileName = Path.Combine(outDir, testCaseFileName + "." + lm.lg.BodyExtension)
        File.WriteAllText(outCFileName, contentC.Replace("\r",""))

        let contentH = printTestCaseFileDef testCaseFileName (includedPackages r lm) arrsTestFunctionDefs
        let outHFileName = Path.Combine(outDir, testCaseFileName + lm.lg.SpecNameSuffix + "." + lm.lg.SpecExtension)
        File.WriteAllText(outHFileName, contentH.Replace("\r",""))  )

    let _, _, func_invocations =
        tcFunctors |>
        Seq.mapi (fun i f -> i+1,f) |>
        Seq.map(fun (i, fnc) -> fnc i) |>
        Seq.toList |> List.unzip3

    let includedPackages =
        [1 .. nFiles] |>
        List.map (fun fileIndex -> sprintf "test_case_%03d" fileIndex )
    let contentH = lm.atc.PrintATCRunnerDefinition()
    let hasTestSuiteRunner = not (String.IsNullOrWhiteSpace contentH)

    let contentC = lm.atc.PrintATCRunner TestSuiteFileName includedPackages [] func_invocations [] [] false
    let outCFileName =
        match hasTestSuiteRunner with
        | true  -> Path.Combine(outDir, TestSuiteFileName + "." + lm.lg.BodyExtension)
        | false -> Path.Combine(outDir, "mainprogram" + "." + lm.lg.BodyExtension)
    File.WriteAllText(outCFileName, contentC.Replace("\r",""))

    if hasTestSuiteRunner then
        let outHFileName = Path.Combine(outDir, TestSuiteFileName + lm.lg.SpecNameSuffix + "." + lm.lg.SpecExtension)
        File.WriteAllText(outHFileName, contentH.Replace("\r",""))


    arrsSrcTstFiles, arrsHdrTstFiles
