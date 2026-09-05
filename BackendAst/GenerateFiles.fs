module GenerateFiles

open System
open System.IO

open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions
open Language
open Microsoft.FSharp.Collections


let getTypeDecl = DastTestCaseCreation.getTypeDecl

let rec getValidFunctions (isValidFunction:IsValidFunction) =
    seq {
        for c in isValidFunction.nonEmbeddedChildrenValidFuncs do
            yield! getValidFunctions c
        yield isValidFunction
    } |> Seq.toList

let rec getInitializationFunctions (isValidFunction:InitFunction) =
    seq {
        for c in isValidFunction.nonEmbeddedChildrenFuncs do
            yield! getInitializationFunctions c
        yield isValidFunction
    } |> Seq.toList

let printHeaderFileValueAssignment (r:DAst.AstRoot) (vasPU_name:string)  (lm:LanguageMacros) (vas:ValueAssignment) =
    let sName = vas.c_name
    let t = vas.Type
    let sTypeDecl= getTypeDecl r vasPU_name lm vas

    let sVal = DAstVariables.printValue r  lm vasPU_name vas.Type None vas.Value.kind
    lm.typeDef.PrintValueAssignment sName sTypeDecl sVal

let printSourceFileValueAssignment (r:DAst.AstRoot) (vasPU_name:string)  (lm:LanguageMacros) (vas:ValueAssignment) =
    let sName = vas.c_name
    let t = vas.Type
    let sTypeDecl: string= getTypeDecl r vasPU_name lm vas
    let sVal = DAstVariables.printValue r  lm vasPU_name vas.Type None vas.Value.kind
    lm.vars.PrintValueAssignment sName sTypeDecl sVal

let rec collectEqualFuncs (t:Asn1Type) =
    seq {
        match t.Kind with
        | Integer          _
        | Real             _
        | IA5String        _
        | OctetString      _
        | NullType         _
        | BitString        _
        | Boolean          _
        | ObjectIdentifier _
        | TimeType         _
        | Enumerated       _ -> ()
        | SequenceOf        ch ->
            yield! collectEqualFuncs ch.childType
        | Sequence        sq ->
            for ch in sq.children do
                match ch with
                | Asn1Child ch  -> yield! collectEqualFuncs ch.Type
                | AcnChild  _   -> ()
        | Choice          ch ->
            for c in ch.children do
                yield! collectEqualFuncs c.chType
        | ReferenceType     _   -> ()
        yield t.equalFunction
    } |> Seq.toList

/// Gets the actual resolved type of any Asn1Type recursively, following references and collecting all children
/// Returns types in post-order (children before parents) so they can be defined in the correct order
let rec getResolvedTypeAndChildren (t:Asn1Type) : Asn1Type list =
    [
        // Recursively process children FIRST (post-order traversal)
        match t.Kind with
        | SequenceOf o ->
            yield! getResolvedTypeAndChildren o.childType
        | Sequence o   ->
            for ch in o.Asn1Children do
                yield! getResolvedTypeAndChildren ch.Type
        | Choice o ->
            for ch in o.children do
                yield! getResolvedTypeAndChildren ch.chType
        | ReferenceType rt ->
            // Follow the reference and get the resolved type
            // This resolved type will have the ACN context from where it's used
            yield! getResolvedTypeAndChildren rt.resolvedType
        | _ -> ()

        // Yield the current type LAST (after all children)
        // This ensures children are defined before parents
        yield t
    ]


let private combineStringOpts(a: string option) (b: string option) = (defaultArg a "") + "\n" + (defaultArg b "")

let private printUnitInternal (tas: TypeAssignment) (fullCls: Asn1Type) (encDecCls: Asn1Type) (lm: LanguageMacros) (printInit: bool) (printEquals: bool) (printIsValid: bool) (printUper: bool) (printAcn: bool) =
    let type_definition =
        match fullCls.typeDefinitionOrReference with
        | TypeDefinition td -> td.typedefBodyOnly ()
        | ReferenceToExistingDefinition _ -> raise(BugErrorException "Type Assignment with no Type Definition")

    let init_funcs        =
        match printInit with
        | true -> match fullCls.initFunction.initProcedure with | Some k -> k.body | None -> ""
        | false -> ""

    // todo: do we need special init funcs? how can they look like?
    let special_init_funcs =
        fullCls.initFunction.user_aux_functions |> List.map fst

    let equal_funcs =
        match printEquals with
        | true  -> combineStringOpts fullCls.equalFunction.isEqualFuncDef fullCls.equalFunction.isEqualFunc
        | false -> ""

    let is_valid_funcs =
        match printIsValid with
        | false -> []
        | true  ->
            match fullCls.isValidFunction with
            | None      -> []
            | Some f    -> [combineStringOpts f.funcDef f.func]

    let uPerEncDec = 
        match printUper with 
        | true -> [combineStringOpts encDecCls.uperEncFunction.funcDef encDecCls.uperEncFunction.func] @
                  encDecCls.uperEncFunction.auxiliaries @
                  [combineStringOpts encDecCls.uperDecFunction.funcDef encDecCls.uperDecFunction.func] @
                  encDecCls.uperDecFunction.auxiliaries
        | false -> []

    let xerEncFunc = match encDecCls.xerEncFunction with XerFunction z -> [combineStringOpts z.funcDef z.func] | XerFunctionDummy -> []
    let xerDecFunc = match encDecCls.xerDecFunction with XerFunction z -> [combineStringOpts z.funcDef z.func] | XerFunctionDummy -> []

    // XER error codes (funcDef) and function bodies (func) kept separate so that,
    // like uPER and ACN, the XER codes are merged into the shared EncodeConstants/DecodeConstants classes.
    let xerEncFuncDef  = match encDecCls.xerEncFunction with XerFunction z -> [z.funcDef] |> List.choose id | XerFunctionDummy -> []
    let xerDecFuncDef  = match encDecCls.xerDecFunction with XerFunction z -> [z.funcDef] |> List.choose id | XerFunctionDummy -> []
    let xerEncFuncOnly = match encDecCls.xerEncFunction with XerFunction z -> [defaultArg z.func ""] | XerFunctionDummy -> []
    let xerDecFuncOnly = match encDecCls.xerDecFunction with XerFunction z -> [defaultArg z.func ""] | XerFunctionDummy -> []

    let acnEncFunc, sEncodingSizeConstant =
        match printAcn, encDecCls.acnEncFunction with
        | true, Some x -> [combineStringOpts x.funcDef x.func] @ x.auxiliaries, [x.encodingSizeConstant]
        | _  -> [], []
    let acnDecFunc =
        match printAcn, encDecCls.acnDecFunction with
        | true, Some x -> [combineStringOpts x.funcDef x.func] @ x.auxiliaries
        | _ -> []

    let allProcs = [equal_funcs] @is_valid_funcs @special_init_funcs @[init_funcs] @uPerEncDec @sEncodingSizeConstant @acnEncFunc @acnDecFunc @xerEncFunc @xerDecFunc

    // Separated constants and funcs-only for Python's assembleAllProcs (merged EncodeConstants class)
    let allEncConstBodies =
        ([ (if printUper then encDecCls.uperEncFunction.funcDef else None)
           (if printAcn then encDecCls.acnEncFunction |> Option.bind (fun x -> x.funcDef) else None) ]
         |> List.choose id) @ xerEncFuncDef
    let allDecConstBodies =
        ([ (if printUper then encDecCls.uperDecFunction.funcDef else None)
           (if printAcn then encDecCls.acnDecFunction |> Option.bind (fun x -> x.funcDef) else None) ]
         |> List.choose id) @ xerDecFuncDef
    let uPerFuncsOnly =
        match printUper with
        | true ->
            [defaultArg encDecCls.uperEncFunction.func ""] @
            encDecCls.uperEncFunction.auxiliaries @
            [defaultArg encDecCls.uperDecFunction.func ""] @
            encDecCls.uperDecFunction.auxiliaries
        | false -> []
    let acnEncFuncOnly =
        match printAcn, encDecCls.acnEncFunction with
        | true, Some x -> [defaultArg x.func ""] @ x.auxiliaries
        | _ -> []
    let acnDecFuncOnly =
        match printAcn, encDecCls.acnDecFunction with
        | true, Some x -> [defaultArg x.func ""] @ x.auxiliaries
        | _ -> []
    let allFuncsAndOtherProcs =
        [equal_funcs] @is_valid_funcs @special_init_funcs @[init_funcs] @uPerFuncsOnly @sEncodingSizeConstant @acnEncFuncOnly @acnDecFuncOnly @xerEncFuncOnly @xerDecFuncOnly

    let finalProcs = lm.lg.assembleAllProcs allEncConstBodies allDecConstBodies allFuncsAndOtherProcs allProcs
    let generatedCode = lm.typeDef.Define_TAS type_definition finalProcs
    generatedCode

let private printUnit (r:DAst.AstRoot)  (lm:LanguageMacros) (encodings: CommonTypes.Asn1Encoding list) outDir (pu:ProgramUnit)  =
    let tases = pu.sortedTypeAssignments
    let vases = pu.valueAssignments
    let arrsAnonymousValues =
        pu.sortedTypeAssignments |>
        List.choose(fun z -> z.Type.isValidFunction) |>
        List.collect (fun z -> z.anonymousVariables)  |>
        Seq.distinctBy(fun z -> z.valueName) |>
        Seq.toList

    let requiresUPER = encodings |> Seq.exists ( (=) Asn1Encoding.UPER)
    let requiresAcn = encodings |> Seq.exists ( (=) Asn1Encoding.ACN)
      
    let (definitionsContntent, srcBody) =
        if lm.lg.isObjectOriented then
            let renderTriples = lm.lg.selectProgramUnitRenderTypes pu tases getResolvedTypeAndChildren

            let typeDefs =
                renderTriples |> List.map (fun (tasF, fullCls, encDecCls) ->
                    let f cl = {Caller.typeId = tasF.Type.id.tasInfo.Value; funcType = cl}
                    let printInit = r.callersSet |> Set.contains (f InitFunctionType)
                    let printEquals = r.args.GenerateEqualFunctions && (r.callersSet |> Set.contains (f EqualFunctionType))
                    let printIsValid = r.callersSet |> Set.contains (f IsValidFunctionType)
                    let printUper = requiresUPER && r.callersSet |> Set.contains (f UperEncDecFunctionType)
                    let printAcn = requiresAcn && r.callersSet |> Set.contains (f AcnEncDecFunctionType)
                    printUnitInternal tasF fullCls encDecCls lm printInit printEquals printIsValid printUper printAcn)

            let arrsValues =
                vases |>
                List.map(fun gv -> printHeaderFileValueAssignment r pu.name lm gv)
            let arrsHeaderAnonymousValues =
                arrsAnonymousValues |>
                List.map(fun av -> lm.typeDef.PrintValueAssignment av.valueName av.typeDefinitionName "")


            let arrsPrototypes = []

            let sFileNameWithNoExtUpperCase = (ToC (System.IO.Path.GetFileNameWithoutExtension pu.specFileName))
            let bXer = r.args.encodings |> Seq.exists ((=) XER)
            let arrsUtilityDefines = []

            let definitionsContent =
                lm.typeDef.PrintSpecificationFile sFileNameWithNoExtUpperCase pu.name pu.importedProgramUnits typeDefs (arrsValues@arrsHeaderAnonymousValues) arrsPrototypes arrsUtilityDefines (not r.args.encodings.IsEmpty) bXer

            let fileName = Path.Combine(outDir, pu.specFileName)
            File.WriteAllText(fileName, definitionsContent.Replace("\r",""))
            match lm.lg.programUnitImportStatement pu.name with
            | Some importStmt -> File.AppendAllText(Path.Combine(outDir, "__init__.py"), importStmt)
            | None -> ()

            // test cases file
            match r.args.generateAutomaticTestCases with
            | false -> ()
            | true  ->
                let typeDefs =
                    seq {
                        for tas in tases do
                            let typeAssignmentInfo = tas.Type.id.tasInfo.Value
                            let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}
                            let reqUPER = r.callersSet |> Set.contains (f UperEncDecFunctionType)
                            let reqACN = r.callersSet |> Set.contains (f AcnEncDecFunctionType)

                            if reqUPER && r.args.encodings |> Seq.exists ((=) CommonTypes.UPER) then
                                yield (tas.Type.uperEncDecTestFunc |> Option.map (fun z -> (z.funcDef + "\n" + z.func)))
                            if r.args.encodings |> Seq.exists ((=) CommonTypes.XER) then
                                yield (tas.Type.xerEncDecTestFunc |> Option.map (fun z -> z.funcDef + "\n" + z.func))
                            if reqACN && r.args.encodings |> Seq.exists ((=) CommonTypes.ACN) then
                                yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.funcDef + "\n" + z.func))
                        } |> Seq.choose id |> Seq.toList
                    
                let testcase_specFileName = Path.Combine(outDir, pu.testcase_specFileName)
                let tstCasesHdrContent = lm.atc.PrintAutomaticTestCasesBodyFile (ToC pu.testcase_specFileName) pu.name (pu.name::pu.importedProgramUnits) [""] typeDefs false
                File.WriteAllText(testcase_specFileName, tstCasesHdrContent.Replace("\r",""))
                
            definitionsContent, "BODY"
        else
            //header file
            let typeDefs =
                tases |>
                List.map(fun tas ->
                    let typeAssignmentInfo = tas.Type.id.tasInfo.Value
                    let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}

                    let type_definition =
                        match tas.Type.typeDefinitionOrReference with
                        | TypeDefinition td -> td.typedefBody ()
                        | ReferenceToExistingDefinition _   -> raise(BugErrorException "Type Assignment with no Type Definition")
                    let init_def        =
                        match r.callersSet |> Set.contains (f InitFunctionType) with
                        | true -> 
                            match lm.lg.initMethod with
                            | Procedure ->
                                Some(getInitializationFunctions tas.Type.initFunction |> List.choose( fun i_f -> i_f.initProcedure) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n" )
                            | Function ->
                                Some(getInitializationFunctions tas.Type.initFunction |> List.choose( fun i_f -> i_f.initFunction) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n" )
                        | false -> None

                    let init_globals    =
                        //we generate const globals only if requested by user and the init method is procedure
                        match r.args.generateConstInitGlobals && (lm.lg.initMethod  = Procedure) with
                        | false -> None
                        | true  -> Some (GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.initFunction.initGlobal ) |> List.map(fun c -> c.def) |> Seq.StrJoin "\n")

                    let special_init_funcs =
                        tas.Type.initFunction.user_aux_functions |> List.map fst


                    let equal_defs =
                        match r.args.GenerateEqualFunctions && (r.callersSet |> Set.contains (f EqualFunctionType)) with
                        | true  -> GetMySelfAndChildren tas.Type |> List.choose(fun t -> t.equalFunction.isEqualFuncDef )
                        | false -> []
                    let isValidFuncs =
                        match r.callersSet |> Set.contains (f IsValidFunctionType) with
                        | false -> []
                        | true  ->
                            match tas.Type.isValidFunction with
                            | None      -> []
                            | Some f    ->
                                getValidFunctions f |> List.choose(fun f -> f.funcDef)


                    let uPerEncFunc = match requiresUPER && r.callersSet |> Set.contains (f UperEncDecFunctionType) with true -> tas.Type.uperEncFunction.funcDef | false -> None
                    let uPerDecFunc = match requiresUPER && r.callersSet |> Set.contains (f UperEncDecFunctionType) with true -> tas.Type.uperDecFunction.funcDef | false -> None

                    let xerEncFunc = match tas.Type.xerEncFunction with XerFunction z -> z.funcDef | XerFunctionDummy -> None
                    let xerDecFunc = match tas.Type.xerDecFunction with XerFunction z -> z.funcDef | XerFunctionDummy -> None

                    let hasAcnEncDec = r.callersSet |> Set.contains (f AcnEncDecFunctionType)
                    let acnEncFunc, sEncodingSizeConstant =
                        match hasAcnEncDec && requiresAcn, tas.Type.acnEncFunction with
                        | true, Some x -> x.funcDef, Some x.encodingSizeConstant
                        | _  -> None, None
                    let acnDecFunc =
                        match hasAcnEncDec && requiresAcn, tas.Type.acnDecFunction with
                        | true, Some x -> x.funcDef
                        | _ -> None

                    let allProcs = equal_defs@isValidFuncs@special_init_funcs@([init_globals;init_def;uPerEncFunc;uPerDecFunc;sEncodingSizeConstant; acnEncFunc; acnDecFunc;xerEncFunc;xerDecFunc] |> List.choose id)
                    lm.typeDef.Define_TAS type_definition allProcs
                )
            let arrsValues =
                vases |>
                List.map(fun gv -> printHeaderFileValueAssignment r pu.name lm gv)
            let arrsHeaderAnonymousValues =
                arrsAnonymousValues |>
                List.map(fun av -> lm.typeDef.PrintValueAssignment av.valueName av.typeDefinitionName "")


            let arrsPrototypes = []

            let sFileNameWithNoExtUpperCase = (ToC (System.IO.Path.GetFileNameWithoutExtension pu.specFileName))
            let bXer = r.args.encodings |> Seq.exists ((=) XER)
            let arrsUtilityDefines = []
            let puCorrName =
                if lm.lg.shouldApplyToCToPackageName then ToC (pu.name) else pu.name

            let definitionsContntent =
                lm.typeDef.PrintSpecificationFile sFileNameWithNoExtUpperCase puCorrName pu.importedProgramUnits typeDefs (arrsValues@arrsHeaderAnonymousValues) arrsPrototypes arrsUtilityDefines (not r.args.encodings.IsEmpty) bXer

            let fileName = Path.Combine(outDir, pu.specFileName)
            File.WriteAllText(fileName, definitionsContntent.Replace("\r",""))


            // test cases header file
            match r.args.generateAutomaticTestCases with
            | false -> ()
            | true  ->
                let typeDefs =
                    seq {
                        for tas in tases do
                            let typeAssignmentInfo = tas.Type.id.tasInfo.Value
                            let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}
                            let reqUPER = r.callersSet |> Set.contains (f UperEncDecFunctionType)
                            let reqACN = r.callersSet |> Set.contains (f AcnEncDecFunctionType)

                            if reqUPER && r.args.encodings |> Seq.exists ((=) CommonTypes.UPER) then
                                yield (tas.Type.uperEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                            if r.args.encodings |> Seq.exists ((=) CommonTypes.XER) then
                                yield (tas.Type.xerEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                            if reqACN && r.args.encodings |> Seq.exists ((=) CommonTypes.ACN) then
                                yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.funcDef))
                        } |> Seq.choose id |> Seq.toList
                let testcase_specFileName = Path.Combine(outDir, pu.testcase_specFileName)
                let tstCasesHdrContent = lm.atc.PrintAutomaticTestCasesSpecFile (ToC pu.testcase_specFileName) pu.name (pu.name::pu.importedProgramUnits) typeDefs
                File.WriteAllText(testcase_specFileName, tstCasesHdrContent.Replace("\r",""))

            //source file
            let arrsTypeAssignments =
                tases |> List.map(fun t ->
                    let typeAssignmentInfo = t.Type.id.tasInfo.Value
                    let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}

                    let privateDefinition =
                        match t.Type.typeDefinitionOrReference with
                        | TypeDefinition td -> td.privateTypeDefinition
                        | ReferenceToExistingDefinition _   -> None

                    let initialize =
                        match r.callersSet |> Set.contains (f InitFunctionType) with
                        | true ->
                            match lm.lg.initMethod with
                            | InitMethod.Procedure  ->
                                Some(getInitializationFunctions t.Type.initFunction |> List.choose( fun i_f -> i_f.initProcedure) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n" )
                            | InitMethod.Function  ->
                                Some(getInitializationFunctions t.Type.initFunction |> List.choose( fun i_f -> i_f.initFunction) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n" )
                        | false -> None

                    let init_globals    =
                        match r.args.generateConstInitGlobals  && (lm.lg.initMethod  = Procedure) with
                        | false -> None
                        | true  -> Some (GetMySelfAndChildren t.Type |> List.choose(fun t -> t.initFunction.initGlobal) |> List.map(fun c -> c.body) |> Seq.StrJoin "\n")


                    let special_init_funcs =
                        t.Type.initFunction.user_aux_functions |> List.map snd

                    let eqFuncs =
                        match r.args.GenerateEqualFunctions && (r.callersSet |> Set.contains (f EqualFunctionType)) with
                        | true  -> GetMySelfAndChildren t.Type |> List.choose(fun y -> y.equalFunction.isEqualFunc)
                        | false -> []

                    let isValidFuncs =
                        match r.callersSet |> Set.contains (f IsValidFunctionType) with
                        | false -> []
                        | true  ->
                            match t.Type.isValidFunction with
                            | None      -> []
                            | Some f    ->
                                getValidFunctions f |> List.choose(fun f -> f.func)

                    let uperEncDec =
                        if requiresUPER && r.callersSet |> Set.contains (f UperEncDecFunctionType) then
                            ((t.Type.uperEncFunction.func |> Option.toList |> List.collect (fun f -> f :: t.Type.uperEncFunction.auxiliaries))) @
                            ((t.Type.uperDecFunction.func |> Option.toList |> List.collect (fun f ->  f :: t.Type.uperDecFunction.auxiliaries)))
                        else []

                    let xerEncDec =
                        (match t.Type.xerEncFunction with
                        | XerFunction z ->  z.func |> Option.toList
                        | XerFunctionDummy  -> []) @
                        (match t.Type.xerDecFunction with
                        | XerFunction z -> z.func |> Option.toList
                        | XerFunctionDummy -> [])

                    let hasAcnEncDec = r.callersSet |> Set.contains (f AcnEncDecFunctionType)
                    // Auxiliaries (e.g., specialized deferred-patching functions) are
                    // emitted BEFORE the main function so that forward declarations
                    // are not needed in the .c file.
                    let ancEncDec =
                        if requiresAcn && hasAcnEncDec then
                            (t.Type.acnEncFunction |> Option.toList |> List.collect (fun x -> x.auxiliaries @ (x.func |> Option.toList))) @
                            (t.Type.acnDecFunction |> Option.toList |> List.collect (fun x -> x.auxiliaries @ (x.func |> Option.toList)))
                        else []

                    let allProcs =
                        (privateDefinition |> Option.toList) @
                        eqFuncs @ isValidFuncs @ special_init_funcs @
                        (init_globals |> Option.toList) @
                        (initialize |> Option.toList) @
                        uperEncDec @ ancEncDec @ xerEncDec
                    lm.src.printTass allProcs)


            let arrsValueAssignments, arrsSourceAnonymousValues =
                match lm.lg.requiresValueAssignmentsInSrcFile with
                | true ->
                    let arrsValueAssignments = vases |> List.map (printSourceFileValueAssignment r pu.name lm)
                    let arrsSourceAnonymousValues =  arrsAnonymousValues |> List.map (fun av -> lm.vars.PrintValueAssignment av.typeDefinitionName av.valueName av.valueExpression)
                    arrsValueAssignments, arrsSourceAnonymousValues
                | false ->
                    [], []
            let rtlFiles = lm.lg.getRtlFiles r.args.encodings arrsTypeAssignments
            let arrsImportedFiles = rtlFiles@pu.importedUserModules@pu.importedProgramUnits |> List.distinct
            let arrsUserDefinedFunctions =
                tases |> List.collect(fun t ->
                        (t.Type.acnEncFunction |> Option.toList |> List.collect (fun x -> x.userDefinedFunctions )) @
                        (t.Type.acnDecFunction |> Option.toList |> List.collect (fun x -> x.userDefinedFunctions ))   ) |> 
                        List.map(fun f -> f.functionProtype.Trim()) |>
                        List.filter (fun s -> s <> "") |>
                        List.distinct
            let puCorrName =
                match r.lang with
                | CommonTypes.ProgrammingLanguage.Scala -> ToC (pu.name)
                | _ -> pu.name
            let srcBody = lm.src.printSourceFile puCorrName arrsImportedFiles pu.importedTypes arrsUserDefinedFunctions (arrsValueAssignments@arrsSourceAnonymousValues@arrsTypeAssignments)

            let eqContntent =
                match lm.lg.allowsSrcFilesWithNoFunctions with
                | true     ->
                    Some srcBody
                | false   ->
                    match arrsTypeAssignments with
                    | []    -> None
                    | _     -> Some srcBody

            match eqContntent with
            | Some eqContntent ->
                let fileName = Path.Combine(outDir, pu.bodyFileName) // todo: here
                if lm.lg.shouldAppendToBodyFile then
                    File.AppendAllText(fileName, eqContntent.Replace("\r",""))
                else
                    File.WriteAllText(fileName, eqContntent.Replace("\r",""))
            | None             -> ()

            //test cases source file
            match r.args.generateAutomaticTestCases with
            | false -> ()
            | true  ->
                let encDecFuncs =
                    seq {
                        for tas in tases do
                            let typeAssignmentInfo = tas.Type.id.tasInfo.Value
                            let f cl = {Caller.typeId = typeAssignmentInfo; funcType = cl}
                            let reqUPER = 
                                r.args.encodings |> Seq.exists ((=) CommonTypes.UPER)
                                && r.callersSet |> Set.contains (f UperEncDecFunctionType)
                            let reqACN = 
                                r.args.encodings |> Seq.exists ((=) CommonTypes.ACN)
                                && r.callersSet |> Set.contains (f AcnEncDecFunctionType)

                            if reqUPER then
                                yield (tas.Type.uperEncDecTestFunc |> Option.map (fun z -> z.func))
                            if r.args.encodings |> Seq.exists ((=) CommonTypes.XER) then
                                yield (tas.Type.xerEncDecTestFunc |> Option.map (fun z -> z.func))
                            if reqACN  then
                                yield (tas.Type.acnEncDecTestFunc |> Option.map (fun z -> z.func))
                        } |> Seq.choose id |> Seq.toList

                let testcase_SrcFileName = Path.Combine(outDir, pu.testcase_bodyFileName)
                let bXer = r.args.encodings |> Seq.exists((=) XER)
                let tstCasesHdrContent =
                    match lm.lg.allowsSrcFilesWithNoFunctions with
                    | true     -> Some (lm.atc.PrintAutomaticTestCasesBodyFile pu.name pu.testcase_specFileName pu.importedProgramUnits [] encDecFuncs bXer)
                    | false   ->
                        match encDecFuncs with
                        | []    -> None
                        | _     -> Some (lm.atc.PrintAutomaticTestCasesBodyFile pu.name pu.testcase_specFileName pu.importedProgramUnits [] encDecFuncs bXer)

                tstCasesHdrContent |> Option.iter(fun tstCasesHdrContent -> File.WriteAllText(testcase_SrcFileName, tstCasesHdrContent.Replace("\r","")))
            (definitionsContntent, srcBody)
        
    (definitionsContntent, srcBody)



let generateAll (di:DirInfo) (r:DAst.AstRoot)  (lm:LanguageMacros) (encodings: CommonTypes.Asn1Encoding list)  =
    if lm.lg.shouldGenerateInitFiles then
        // Write basic __init__.py in root & in srcDir
        File.WriteAllLines(Path.Combine(di.rootDir, "__init__.py"), ["from .asn1python import *"] @ ["from .asn1src import *"])
        File.WriteAllText(Path.Combine(di.srcDir, "__init__.py"), "")
    
    let generatedContent = r.programUnits |> List.map(printUnit r lm encodings di.srcDir) |> List.map snd |> Seq.StrJoin "\n"
    match r.args.generateAutomaticTestCases with
    | false -> ()
    | true  ->
        lm.lg.CreateMakeFile r di
        let arrsSrcTstFiles, arrsHdrTstFiles = DastTestCaseCreation.printAllTestCasesAndTestCaseRunner r lm di.srcDir
        lm.lg.CreateAuxFiles r di (arrsSrcTstFiles, arrsHdrTstFiles)
    generatedContent


let EmitDefaultACNGrammar (r:AstRoot) outDir  =
    let printTas (tas: TypeAssignment) =
        tas.Name.Value + "[]"
    let printModule (m:Asn1Module) =
        let arrTases = m.TypeAssignments |> Seq.map printTas
        stg_acn.PrintDefaultAcnModule m.Name.Value arrTases "::="
    let printFile (f:Asn1File) =
        let fileName = f.FileNameWithoutExtension + ".acn"
        if (System.IO.File.Exists fileName) then
            System.Console.Error.WriteLine("File {0} already exists. Creation of default ASN.1 grammar abandoned", fileName);
        else
            let content = f.Modules |> Seq.map printModule |> Seq.StrJoin "\n"
            let fileName = Path.Combine(outDir, fileName)
            File.WriteAllText(fileName, content.Replace("\r",""))

    r.Files |> Seq.iter printFile