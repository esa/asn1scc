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
    | false     -> r.programUnits |> Seq.map(fun x -> lm.lg.sanitizeModuleName (System.IO.Path.GetFileNameWithoutExtension(x.testcase_specFileName)))
    | true      -> r.programUnits |> Seq.collect(fun x -> [x.name; x.testcase_name])


let rec gAmber (lm:LanguageMacros) (t:Asn1Type) =
    match t.Kind with
    | Integer      _ -> "&"  , "&"
    | Real         _ -> "&"  , "&"
    | IA5String    _ -> lm.lg.getAmberForType t.Kind.baseKind
    | OctetString  _ -> "&" , "&"
    | NullType     _ -> "&"  , "&"
    | BitString    _ -> "&" , "&"
    | Boolean      _ -> "&"  , "&"
    | Enumerated   _ -> "&" , "&"
    | SequenceOf   _ -> "&" , "&"
    | Sequence     _ -> "&" , "&"
    | Choice       _ -> "&" , "&"
    | ObjectIdentifier _ -> "&" , "&"
    | TimeType      _   -> "&"  , "&"
    | ReferenceType r -> gAmber lm r.resolvedType

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
    let encAmper, initAmper = gAmber lm v.Type
    let curProgramUnitName = ""  //Main program has no module
    let valueType = match v.Type.typeDefinitionOrReference with
                    | TypeDefinition  td -> modName + "." + td.typedefName
                    | ReferenceToExistingDefinition ref -> modName + "." + ref.typedefName
    
    let initStatement = DAstVariables.printValue r lm curProgramUnitName v.Type None v.Value.kind
    let initStatement = lm.lg.formatValueAssignmentTestCase (resolveReferenceType v.Type.Kind) valueType initStatement
    // Scala prepends "val tc_data = " for Integer; Python re-types structured aliases.
    let initStatement = lm.lg.formatInitStatementForTestCase v.Type.Kind.baseKind modName sTasName initStatement
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

    let encAmper, initAmper = gAmber lm t
    let initStatement =
        match t.ActualType.Kind with
        | ObjectIdentifier _ -> lm.lg.adjustTestCaseObjectIdentifierInit modName sTasName initStatement
        | _ -> initStatement
    // Python: a structured alias TAS's automatic test value is built with the resolved (base)
    // type's constructor, but this test exercises the alias TAS (its enc_dec functions). For XER
    // the XML element tag is the value's runtime type name, so a base-typed value encodes
    // <BaseType> while the alias decode expects <AliasType>. Re-type the value to the alias so
    // the generic .encode(Encoding) dispatches to the alias's encoder and uses the alias tag
    // (idempotent when the value is already alias-typed). uPER/ACN have no element tags, so this
    // is a no-op there. Only structured kinds are handled: scalar/enum/null alias values are
    // already alias-typed (re-wrapping would corrupt them) and NULL objects reject __class__.
    let initStatement = lm.lg.formatInitStatementForTestCase t.Kind.baseKind modName sTasName initStatement
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
    let pdummy = {CodegenScope.modName = ToC "MainProgram"; accessPath = AccessPath.valueEmptyPath "tmp0" }
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
        // Skip children whose access path is not rooted at the dummy variable.
        // This happens for CHOICE children in languages where getChChild creates
        // a standalone path (e.g. Rust enum variants) — the init call would
        // reference a variable that doesn't exist in scope.
        if p.accessPath.rootId <> pdummy.accessPath.rootId then None else
        let initProc = t.initFunction.initProcedure
        let dummyVarName =
            match t.isIA5String with
            | true  ->  lm.lg.getValue p.accessPath
            | false ->  lm.lg.getPointer p.accessPath
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

/// The kinds of automatic tests with an invalid value (C).
type InvalidValueKind =
    | InvalidChoiceSelector   // CHOICE selector set to <X>_NONE
    | InvalidEnumValue        // ENUMERATED field set to a value that is not an item
    | ExcludedEnumItem        // ENUMERATED field set to a declared item that its constraints exclude

/// Nodes of a type's own tree that can hold an invalid in-memory value, as
/// (node type id, statement that sets the invalid value, kind):
/// a CHOICE selector is set to <X>_NONE (always the first enumerator, i.e. 0),
/// an ENUMERATED field to the smallest non-negative integer that is not an item
/// value. Referenced types are covered by their own type assignment's tests;
/// SEQUENCE OF elements are skipped because an automatic test value does not
/// tell which element holds a nested node.
let rec invalidValueNodes (lm:LanguageMacros) (path:AccessPath) (t:Asn1Type) : (ReferenceToType * string * InvalidValueKind) list =
    match t.Kind with
    | Choice ch ->
        let setNone = sprintf "%s%skind = 0; /* <X>_NONE */" (path.joined lm.lg) (lm.lg.getAccess path)
        let children =
            ch.children |>
            List.collect (fun c ->
                match c.Optionality with
                | Some Asn1AcnAst.ChoiceAlwaysAbsent -> []
                | Some Asn1AcnAst.ChoiceAlwaysPresent
                | None -> invalidValueNodes lm (lm.lg.getChChild path (lm.lg.getAsn1ChChildBackendName c) c.chType.isIA5String) c.chType)
        (t.id, setNone, InvalidChoiceSelector) :: children
    | Enumerated en ->
        let values = en.baseInfo.items |> List.map (fun it -> it.definitionValue) |> Set.ofList
        let invalid = Seq.initInfinite BigInteger |> Seq.find (fun v -> not (values.Contains v))
        [(t.id, sprintf "%s = %s; /* not an item value */" (path.joined lm.lg) (invalid.ToString()), InvalidEnumValue)]
    | Sequence sq ->
        sq.children |>
        List.collect (fun c ->
            match c with
            | AcnChild _ -> []
            | Asn1Child ch ->
                match ch.Optionality with
                | Some Asn1AcnAst.AlwaysAbsent -> []
                | Some Asn1AcnAst.AlwaysPresent
                | Some (Asn1AcnAst.Optional _)
                | None ->
                    let chPath = lm.lg.getSeqChild path (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String ch.Optionality.IsSome
                    invalidValueNodes lm chPath ch.Type)
    | SequenceOf _ | ReferenceType _ | Integer _ | Real _ | IA5String _ | OctetString _ | NullType _
    | BitString _ | Boolean _ | ObjectIdentifier _ | TimeType _ -> []

/// ENUMERATED nodes whose constraints permit only some of the declared items, e.g. a PUS
/// header field fixed by WITH COMPONENTS. The encoders and decoders keep an arm for every
/// declared item, which valid values never reach. Unlike invalidValueNodes this follows
/// references: such constraints are usually applied at the reference site, and the
/// constrained reference is encoded inline in the parent.
let rec excludedValueNodes (lm:LanguageMacros) (path:AccessPath) (t:Asn1Type) : (ReferenceToType * string * InvalidValueKind) list =
    match t.Kind with
    | Enumerated en ->
        // validItems applies only the type's own constraints; WITH COMPONENTS of an
        // enclosing type arrive as withcons.
        let permitted = en.baseInfo.items |> List.filter (Asn1Fold.isValidValueGeneric (en.baseInfo.cons @ en.baseInfo.withcons) (fun a b -> a = b.Name.Value))
        let valid = permitted |> List.map (fun it -> it.Name.Value) |> Set.ofList
        // one test per excluded item: each item has its own encoder and decoder arm
        en.baseInfo.items |>
        List.filter (fun it -> not (valid.Contains it.Name.Value)) |>
        List.map (fun it -> (t.id, sprintf "%s = %s; /* %s: excluded by a constraint */" (path.joined lm.lg) (it.definitionValue.ToString()) it.Name.Value, ExcludedEnumItem))
    | ReferenceType rf -> excludedValueNodes lm path rf.resolvedType
    | Choice ch ->
        ch.children |>
        List.collect (fun c ->
            match c.Optionality with
            | Some Asn1AcnAst.ChoiceAlwaysAbsent -> []
            | Some Asn1AcnAst.ChoiceAlwaysPresent
            | None -> excludedValueNodes lm (lm.lg.getChChild path (lm.lg.getAsn1ChChildBackendName c) c.chType.isIA5String) c.chType)
    | Sequence sq ->
        sq.children |>
        List.collect (fun c ->
            match c with
            | AcnChild _ -> []
            | Asn1Child ch ->
                match ch.Optionality with
                | Some Asn1AcnAst.AlwaysAbsent -> []
                | Some Asn1AcnAst.AlwaysPresent
                | Some (Asn1AcnAst.Optional _)
                | None ->
                    let chPath = lm.lg.getSeqChild path (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String ch.Optionality.IsSome
                    excludedValueNodes lm chPath ch.Type)
    | SequenceOf _ | Integer _ | Real _ | IA5String _ | OctetString _ | NullType _
    | BitString _ | Boolean _ | ObjectIdentifier _ | TimeType _ -> []

/// One automatic test per invalid-value node of a type assignment, for the
/// UPER and ACN encodings of languages that can represent such values. Each
/// starts from the first used automatic test value that contains the node.
let invalidValueTestCases (lm:LanguageMacros) (e:Asn1Encoding) (t:Asn1Type) (atcs:AutomaticTestCase list) =
    let encFuncName =
        match e with
        | Asn1Encoding.UPER -> t.uperEncFunction.funcName
        | Asn1Encoding.ACN  -> t.acnEncFunction |> Option.bind (fun f -> f.funcName)
        | Asn1Encoding.XER
        | Asn1Encoding.BER  -> None
    let decFuncName =
        match e with
        | Asn1Encoding.UPER -> t.uperDecFunction.funcName
        | Asn1Encoding.ACN  -> t.acnDecFunction |> Option.bind (fun f -> f.funcName)
        | Asn1Encoding.XER
        | Asn1Encoding.BER  -> None
    let isValidFuncName = t.isValidFunction |> Option.bind (fun f -> f.funcName)
    match lm.lg.atcEmitsInvalidValueTests, encFuncName, decFuncName, isValidFuncName with
    | true, Some sEncFunc, Some sDecFunc, Some sIsValidFunc ->
        let p = {CodegenScope.modName = ToC "MainProgram"; accessPath = AccessPath.valueEmptyPath "tc_data"}
        (invalidValueNodes lm p.accessPath t) @ (excludedValueNodes lm p.accessPath t) |>
        List.choose (fun (nodeId, sSetInvalid, kind) ->
            atcs |>
            List.tryFind (fun atc -> nodeId = t.id || atc.testCaseTypeIDsMap.ContainsKey nodeId) |>
            Option.map (fun atc ->
                fun idx ->
                    let sFuncName =
                        match kind with
                        | InvalidChoiceSelector
                        | InvalidEnumValue -> sprintf "test_case_invalid_%A_%06d" e idx
                        | ExcludedEnumItem -> sprintf "test_case_excluded_%A_%06d" e idx
                    let initStatement = atc.initTestCaseFunc p
                    let arrsVars = initStatement.localVariables |> List.map(fun lv -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct |> Seq.toList
                    let encAmper, _ = gAmber lm t
                    let bStatic = match t.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
                    let sTasName = (lm.lg.getTypeDefinition t.FT_TypeDefinition).typeName
                    let sEnc = GetEncodingString lm e
                    let func_body =
                        match kind with
                        | InvalidChoiceSelector -> lm.atc.emitInvalidValueTestCase sFuncName arrsVars sTasName encAmper sEnc initStatement.funcBody bStatic sSetInvalid sIsValidFunc sEncFunc t.equalFunction.isEqualFuncName
                        | InvalidEnumValue      -> lm.atc.emitInvalidValueTestCase sFuncName arrsVars sTasName encAmper sEnc initStatement.funcBody bStatic sSetInvalid sIsValidFunc sEncFunc None
                        | ExcludedEnumItem      -> lm.atc.emitExcludedValueTestCase sFuncName arrsVars sTasName encAmper sEnc initStatement.funcBody bStatic sSetInvalid sIsValidFunc sEncFunc sDecFunc
                    (emitTestCaseAsFunc_h lm sFuncName, func_body, invokeTestCaseAsFunc lm sFuncName)))
    | _ -> []

let asn1EncodingMapping = function
    | UPER  -> UperEncDecFunctionType
    | ACN   -> AcnEncDecFunctionType 
    | BER   -> BerEncDecFunctionType 
    | XER   -> XerEncDecFunctionType 

let printAllTestCasesAndTestCaseRunner (r:DAst.AstRoot) (lm:LanguageMacros) outDir =
    // Invalid-value tests follow all other tests, so the names of the existing ones do not change.
    let invalidValueFunctors = ResizeArray<int -> string*string*string>()
    let validFunctors =
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
                                let atcsToUse =
                                    let allAtcs = t.Type.initFunction.automaticTestCases
                                    if e = Asn1Encoding.XER && lm.lg.isObjectOriented then
                                        let sizeOf (atc:AutomaticTestCase) =
                                            atc.testCaseTypeIDsMap
                                            |> Map.toList
                                            |> List.sumBy (fun (_, tcv) -> match tcv with TcvSizeableTypeValue n -> n | _ -> 0I)
                                        match allAtcs with
                                        | [] -> []
                                        | _  ->
                                            let minSize = allAtcs |> List.map sizeOf |> List.min
                                            allAtcs |> List.filter (fun atc -> sizeOf atc = minSize)
                                    else
                                        allAtcs
                                let usedAtcs = atcsToUse |> List.filter (fun atc -> e <> Asn1Encoding.ACN || (isTestCaseValid atc))
                                invalidValueFunctors.AddRange (invalidValueTestCases lm e t.Type usedAtcs)
                                for atc in atcsToUse do
                                    let testCaseIsValid = e <> Asn1Encoding.ACN || (isTestCaseValid atc)
                                    if testCaseIsValid then
                                        let generateTcFun idx =
                                            let p = {CodegenScope.modName = ToC "MainProgram"; accessPath = AccessPath.valueEmptyPath "tc_data"}
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
    let tcFunctors = validFunctors @ List.ofSeq invalidValueFunctors
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

        let contentC = printTestCaseFileBody testCaseFileName (includedPackages r lm) arrsTestFunctionBodies (r.programUnits |> List.map (fun pu -> lm.lg.sanitizeModuleName pu.name))
        let outCFileName = Path.Combine(outDir, testCaseFileName + "." + lm.lg.BodyExtension)
        File.WriteAllText(outCFileName, contentC.Replace("\r",""))

        let contentH = printTestCaseFileDef testCaseFileName (includedPackages r lm) arrsTestFunctionDefs
        let outHFileName = Path.Combine(outDir, testCaseFileName + lm.lg.SpecNameSuffix + "." + lm.lg.SpecExtension)
        if lm.lg.shouldAppendTestCaseFile then
            File.AppendAllText(outHFileName, contentH.Replace("\r",""))
        else
            File.WriteAllText(outHFileName, contentH.Replace("\r",""))
        )

    let _, _, func_invocations =
        tcFunctors |>
        Seq.mapi (fun i f -> i+1,f) |>
        Seq.map(fun (i, fnc) -> fnc i) |>
        Seq.toList |> List.unzip3

    let autoTcsMods =
        if lm.lg.atcRunnerImportsAutoTcsUnits then
            r.programUnits |> List.map (fun pu -> lm.lg.sanitizeModuleName pu.testcase_name)
        else
            []
    let atcIncludedPackages =
        ([1 .. nFiles] |>
        List.map (fun fileIndex -> sprintf "test_case_%03d" fileIndex ))
        @ autoTcsMods
    let contentH = lm.atc.PrintATCRunnerDefinition()
    let hasTestSuiteRunner = not (String.IsNullOrWhiteSpace contentH)

    let contentC = lm.atc.PrintATCRunner TestSuiteFileName atcIncludedPackages [] func_invocations [] [] false
    let outCFileName =
        match hasTestSuiteRunner with
        | true  -> Path.Combine(outDir, TestSuiteFileName + "." + lm.lg.BodyExtension)
        | false -> Path.Combine(outDir, "mainprogram" + "." + lm.lg.BodyExtension)
    File.WriteAllText(outCFileName, contentC.Replace("\r",""))

    if hasTestSuiteRunner then
        let outHFileName = Path.Combine(outDir, TestSuiteFileName + lm.lg.SpecNameSuffix + "." + lm.lg.SpecExtension)
        if lm.lg.shouldWriteThenAppendTestSuite then
            File.WriteAllText(outHFileName, contentH.Replace("\r",""))
            File.AppendAllText(outHFileName, contentC.Replace("\r", ""))
        else
            File.WriteAllText(outHFileName, contentH.Replace("\r",""))


    arrsSrcTstFiles, arrsHdrTstFiles
