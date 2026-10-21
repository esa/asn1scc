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


/// Type assignments whose name (with the type prefix, case insensitive) is defined in more than one
/// module. Ada automatic-test files USE every package, so the functions of these types (init,
/// enc_dec) have the same name in several packages and are called qualified
/// (ILangGeneric.atcQualifiedFuncName). Only these: qualifying every call made GNAT 2021 (Docker
/// image) hang on 10-SEQEUENCE/empty_seq.
let atcAmbiguousTypeNames (r:DAst.AstRoot) : string list =
    r.Files |> List.collect (fun f -> f.Modules) |>
    List.collect (fun m -> m.TypeAssignments |> List.map (fun tas -> (ToC2 (r.args.TypePrefix + tas.Name.Value)).ToLower(), m.Name.Value)) |>
    List.distinct |> List.countBy fst |> List.filter (fun (_, n) -> n > 1) |> List.map fst

/// The function belongs to one of these types: <T>_Init, <T>_<child>_Init, <T>_<ENC>_enc_dec ...
let private isAmbiguousTypeFunc (ambiguous:string list) (funcName:string) =
    let f = funcName.ToLower()
    ambiguous |> List.exists (fun t -> f.StartsWith (t + "_"))

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
    let soEncDecPrefix = if isAmbiguousTypeFunc (atcAmbiguousTypeNames r) (sTasName + "_enc_dec") then Some (lm.lg.atcQualifiedFuncName (modName + "_auto_tcs") "") else None
    let func_body = emitTestCaseAsFunc lm sFuncName [] modName sTasName encAmper (GetEncodingString lm e) true initStatement bStatic "" dummyInitStatementsNeededForStatementCoverage initAmper soEncDecPrefix
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
    let soEncDecPrefix = if isAmbiguousTypeFunc (atcAmbiguousTypeNames r) (sTasName + "_enc_dec") then Some (lm.lg.atcQualifiedFuncName (modName + "_auto_tcs") "") else None
    let func_body = emitTestCaseAsFunc lm sFuncName arrsVars modName sTasName encAmper (GetEncodingString lm e) false initStatement bStatic "" initFuncName initAmper soEncDecPrefix
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

let emitDummyInitStatementsNeededForStatementCoverage (ambiguous:string list) (lm:Language.LanguageMacros) (t:Asn1Type) =
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
        let sProgramUnit = (lm.lg.getTypeDefinition t.FT_TypeDefinition).programUnit
        let sTypeName =
            match lm.lg.hasModules with
            | false -> sTypeName
            | true ->
                match sTypeName.Contains "." with
                | true  -> sTypeName
                | false -> sProgramUnit + "." + sTypeName
        match initProc with
        | None  -> None
        | Some initProc when actualInitFuncNames.Contains initProc.funcName ->
            let sInitFuncName =
                match isAmbiguousTypeFunc ambiguous initProc.funcName with
                | true  -> lm.lg.atcQualifiedFuncName sProgramUnit initProc.funcName
                | false -> initProc.funcName
            match lm.lg.initMethod with
            | InitMethod.Procedure ->
                Some (emitTestCaseAsFunc_dummy_init lm sTypeName sInitFuncName dummyVarName)
            | InitMethod.Function ->
                Some (emitTestCaseAsFunc_dummy_init_function lm sTypeName sInitFuncName dummyVarName)
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

/// Coverage step 11c (T2): a value just outside one constraint, set on one node of a type assignment's value.
/// IsConstraintValid must return FALSE with errCode, every checked encoder too.
type ConstraintViolation = {
    nodeId          : ReferenceToType           // the start test value must contain this node
    contextNodes    : ReferenceToType list      // ... and these (the reference sites on the way to it)
    presentChildren : ReferenceToType list      // ... and have these OPTIONAL children present
    nonEmptySeqOfs  : ReferenceToType list      // ... and at least one element in these SEQUENCE OFs (element 0 is changed)
    inAlternative   : bool                      // ... and select the CHOICE alternatives on the way (via nodeId)
    setInvalid      : string                    // the statement that sets the invalid value
    errCode         : ErrorCode                 // the code IsConstraintValid must return
    errCodeModule   : string                    // the program unit that defines it
    what            : string                    // what is changed, for the test's comment
}

let rec private rangeEndpoints (c:Asn1AcnAst.RangeTypeConstraint<'v,'v>) : 'v list =
    match c with
    | Asn1AcnAst.RangeUnionConstraint (_, c1, c2, _)
    | Asn1AcnAst.RangeIntersectionConstraint (_, c1, c2)
    | Asn1AcnAst.RangeExceptConstraint (_, c1, c2)
    | Asn1AcnAst.RangeRootConstraint2 (_, c1, c2)        -> rangeEndpoints c1 @ rangeEndpoints c2
    | Asn1AcnAst.RangeAllExceptConstraint (_, c1)
    | Asn1AcnAst.RangeRootConstraint (_, c1)             -> rangeEndpoints c1
    | Asn1AcnAst.RangeSingleValueConstraint (_, v)       -> [v]
    | Asn1AcnAst.RangeConstraint (_, a, b, _, _)         -> [a; b]
    | Asn1AcnAst.RangeConstraint_val_MAX (_, a, _)       -> [a]
    | Asn1AcnAst.RangeConstraint_MIN_val (_, b, _)       -> [b]

/// The INTEGER values that the language typedef of the node can hold: with base-type typedefs (C) the whole
/// word, with ranged typedefs (Ada) only the uPER range, i.e. the gaps of a value set.
let private representableInt (r:DAst.AstRoot) (lm:LanguageMacros) (o:Asn1AcnAst.Integer) (v:BigInteger) =
    match lm.lg.typedefConstraintModel with
    | TypedefsAreBaseTypes  ->
        match r.args.slim, o.intClass with
        | true, (Asn1AcnAst.ASN1SCC_Int8 (a, b) | Asn1AcnAst.ASN1SCC_Int16 (a, b) | Asn1AcnAst.ASN1SCC_Int32 (a, b) | Asn1AcnAst.ASN1SCC_Int64 (a, b)
                | Asn1AcnAst.ASN1SCC_Int (a, b) | Asn1AcnAst.ASN1SCC_UInt8 (a, b) | Asn1AcnAst.ASN1SCC_UInt16 (a, b)
                | Asn1AcnAst.ASN1SCC_UInt32 (a, b) | Asn1AcnAst.ASN1SCC_UInt64 (a, b) | Asn1AcnAst.ASN1SCC_UInt (a, b)) ->
            // -slim: the typedef is the smallest C type of the class
            a <= v && v <= b
        | false, _ ->
            let bits = int r.args.integerSizeInBytes * 8
            match o.isUnsigned with
            | true  -> v >= 0I && v < BigInteger.Pow (2I, bits)
            | false -> v >= -(BigInteger.Pow (2I, bits - 1)) && v < BigInteger.Pow (2I, bits - 1)
    | TypedefsEnforceRanges ->
        match o.uperRange with
        | Concrete (a, b) -> a <= v && v <= b
        | NegInf b        -> v <= b
        | PosInf a        -> a <= v
        | Full            -> true

/// Values next to the constraint's endpoints that violate it (both ends of a range: one test per condition of
/// the range check, for MC/DC). baseCons: the values must satisfy them (a reference-site constraint narrows them).
let private intViolatingValues (r:DAst.AstRoot) (lm:LanguageMacros) (o:Asn1AcnAst.Integer) (baseCons:Asn1AcnAst.IntegerTypeConstraint list) (cons:Asn1AcnAst.IntegerTypeConstraint list) =
    cons |> List.collect rangeEndpoints |> List.collect (fun v -> [v - 1I; v + 1I]) |> List.distinct |> List.sort |>
    List.filter (fun v ->
        representableInt r lm o v && Asn1Fold.isValidValueRanged baseCons v && not (Asn1Fold.isValidValueRanged cons v))

let private realViolatingValues (o:Asn1AcnAst.Real) (cons:Asn1AcnAst.RealTypeConstraint list) =
    cons |> List.collect rangeEndpoints |> List.collect (fun v -> [v - 1.0; v + 1.0]) |> List.distinct |> List.sort |>
    List.filter (fun v -> not (Asn1Fold.isValidValueRanged cons v))

/// A SIZE whose value set is one interval and nothing else (no value constraints): the sizes min - 1 and max + 1.
let rec private pureSize (c:Asn1AcnAst.SizableTypeConstraint<'v>) =
    match c with
    | Asn1AcnAst.SizeConstraint _                    -> true
    | Asn1AcnAst.SizeIntersectionConstraint (_, a, b) -> pureSize a && pureSize b
    | Asn1AcnAst.SizeUnionConstraint _ | Asn1AcnAst.SizeAllExceptConstraint _ | Asn1AcnAst.SizeExceptConstraint _
    | Asn1AcnAst.SizeRootConstraint _ | Asn1AcnAst.SizeRootConstraint2 _ | Asn1AcnAst.SizeSingleValueConstraint _ -> false

/// String constraints made only of SIZE and FROM (no value sets, unions, exceptions), and whether one is a FROM.
let rec private sizeOrAlphabet (c:Asn1AcnAst.IA5StringConstraint) =
    match c with
    | Asn1AcnAst.StrSizeConstraint _
    | Asn1AcnAst.AlphabetConstraint _                   -> true
    | Asn1AcnAst.StrIntersectionConstraint (_, a, b)     -> sizeOrAlphabet a && sizeOrAlphabet b
    | Asn1AcnAst.StrUnionConstraint _ | Asn1AcnAst.StrAllExceptConstraint _ | Asn1AcnAst.StrExceptConstraint _
    | Asn1AcnAst.StrRootConstraint _ | Asn1AcnAst.StrRootConstraint2 _ | Asn1AcnAst.StrSingleValueConstraint _ -> false

let rec private hasAlphabet (c:Asn1AcnAst.IA5StringConstraint) =
    match c with
    | Asn1AcnAst.AlphabetConstraint _                   -> true
    | Asn1AcnAst.StrIntersectionConstraint (_, a, b)     -> hasAlphabet a || hasAlphabet b
    | Asn1AcnAst.StrSizeConstraint _ | Asn1AcnAst.StrUnionConstraint _ | Asn1AcnAst.StrAllExceptConstraint _ | Asn1AcnAst.StrExceptConstraint _
    | Asn1AcnAst.StrRootConstraint _ | Asn1AcnAst.StrRootConstraint2 _ | Asn1AcnAst.StrSingleValueConstraint _ -> false

let rec private pureSeqOfSize (c:Asn1AcnAst.SequenceOfConstraint) =
    match c with
    | Asn1AcnAst.SeqOfSizeConstraint _                    -> true
    | Asn1AcnAst.SeqOfSizeIntersectionConstraint (_, a, b) -> pureSeqOfSize a && pureSeqOfSize b
    | Asn1AcnAst.SeqOfSizeUnionConstraint _ | Asn1AcnAst.SeqOfSizeAllExceptConstraint _ | Asn1AcnAst.SeqOfSizeExceptConstraint _
    | Asn1AcnAst.SeqOfSizeRootConstraint _ | Asn1AcnAst.SeqOfSizeRootConstraint2 _ | Asn1AcnAst.SeqOfSizeSingleValueConstraint _
    | Asn1AcnAst.SeqOfSeqWithComponentConstraint _      -> false

/// The named constraints of a WITH COMPONENTS constraint (top level and intersections only).
let rec private withComponents (c:Asn1AcnAst.SeqConstraint) : Asn1AcnAst.NamedConstraint list =
    match c with
    | Asn1AcnAst.SeqOrChWithComponentsConstraint (_, ncs) -> ncs
    | Asn1AcnAst.SeqOrChIntersectionConstraint (_, a, b) -> withComponents a @ withComponents b
    | Asn1AcnAst.SeqOrChUnionConstraint _ | Asn1AcnAst.SeqOrChAllExceptConstraint _ | Asn1AcnAst.SeqOrChExceptConstraint _
    | Asn1AcnAst.SeqOrChRootConstraint _ | Asn1AcnAst.SeqOrChRootConstraint2 _ | Asn1AcnAst.SeqOrChSingleValueConstraint _ -> []

let private ownErrCode (t:Asn1Type) =
    match t.isValidFunction with
    | Some f ->
        match f.outcome, f.errCodes with
        | MayBeInvalid, e::_  -> Some e
        | MayBeInvalid, []
        | AlwaysValid, _      -> None
    | None   -> None

let private tryFindTypeAssignment (r:DAst.AstRoot) (modName:string) (tasName:string) =
    r.Files |> List.collect (fun f -> f.Modules) |> List.tryFind (fun m -> m.Name.Value = modName) |>
    Option.bind (fun m -> m.TypeAssignments |> List.tryFind (fun ts -> ts.Name.Value = tasName))

/// The constraint violations of a type's own tree (coverage step 11c). A reference contributes only the first
/// violation of the referenced type assignment (its own tests cover the rest), so that the parent's check of the
/// call is covered, plus the violations of the constraints added at the reference site.
let rec constraintViolations (r:DAst.AstRoot) (lm:LanguageMacros) (path:AccessPath) (t:Asn1Type) : ConstraintViolation list =
    let mk nodeId sSet errCode what = {nodeId = nodeId; contextNodes = []; presentChildren = []; nonEmptySeqOfs = []; inAlternative = false; setInvalid = sSet; errCode = errCode; errCodeModule = ToC t.id.ModName; what = what}
    let sizeViolations (minSize:BigInteger) (maxSize:BigInteger) (isPure:bool) =
        match lm.lg.typedefConstraintModel, ownErrCode t, isPure && minSize <> maxSize with
        | TypedefsAreBaseTypes, Some e, true ->
            [minSize - 1I; maxSize + 1I] |> List.filter (fun n -> n >= 0I) |>
            List.map (fun n -> mk t.id (lm.init.initVarSizeSequenceOf (path.joined lm.lg) (lm.lg.getAccess path) n []) e (sprintf "size %A, outside %A..%A" n minSize maxSize))
        | TypedefsAreBaseTypes, _, _
        | TypedefsEnforceRanges, _, _ -> []     // the Ada Length subtype holds the SIZE range
    match t.Kind with
    | Integer o ->
        match ownErrCode t with
        | None   -> []
        | Some e ->
            intViolatingValues r lm o.baseInfo [] o.baseInfo.cons |>
            List.map (fun v ->
                let sv = lm.lg.intValueToString v o.baseInfo.intClass
                mk t.id (lm.init.initInteger (lm.lg.getValue path) sv false "" "") e (sprintf "%s, outside %s" sv (o.baseInfo.cons |> List.map (fun c -> c.ASN1) |> Seq.StrJoin "")))
    | Real o ->
        match ownErrCode t with
        | None   -> []
        | Some e ->
            realViolatingValues o.baseInfo o.baseInfo.cons |>
            List.map (fun v -> mk t.id (lm.init.initReal (lm.lg.getValue path) v false "" "") e (sprintf "%A, outside %s" v (o.baseInfo.cons |> List.map (fun c -> c.ASN1) |> Seq.StrJoin "")))
    | OctetString o -> sizeViolations o.baseInfo.minSize.uper o.baseInfo.maxSize.uper (o.baseInfo.cons |> List.forall pureSize)
    | BitString o   -> sizeViolations o.baseInfo.minSize.uper o.baseInfo.maxSize.uper (o.baseInfo.cons |> List.forall pureSize)
    | SequenceOf o  ->
        let own = sizeViolations o.baseInfo.minSize.uper o.baseInfo.maxSize.uper (o.baseInfo.cons |> List.forall pureSeqOfSize)
        let elemPath = lm.lg.getArrayItem path (string lm.lg.ArrayStartIndex) o.childType.isIA5String
        let elems = constraintViolations r lm elemPath o.childType |> List.map (fun v -> {v with nonEmptySeqOfs = t.id::v.nonEmptySeqOfs})
        own @ elems
    | Sequence sq ->
        let asn1Children = sq.children |> List.choose (fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
        let childPath (ch:Asn1Child) = lm.lg.getSeqChild path (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String ch.Optionality.IsSome
        let children =
            asn1Children |>
            List.collect (fun ch ->
                match ch.Optionality with
                | Some Asn1AcnAst.AlwaysAbsent -> []
                | Some Asn1AcnAst.AlwaysPresent
                | Some (Asn1AcnAst.Optional _) ->
                    // a violation inside an OPTIONAL child needs a start value where the child is present
                    constraintViolations r lm (childPath ch) ch.Type |> List.map (fun v -> {v with presentChildren = ch.Type.id :: v.presentChildren})
                | None -> constraintViolations r lm (childPath ch) ch.Type)
        let presence = withComponentsViolations r lm path t asn1Children (sq.baseInfo.cons |> List.collect withComponents)
        children @ presence
    | Choice ch ->
        ch.children |>
        List.collect (fun c ->
            match c.Optionality with
            | Some Asn1AcnAst.ChoiceAlwaysAbsent -> []
            | Some Asn1AcnAst.ChoiceAlwaysPresent
            | None ->
                constraintViolations r lm (lm.lg.getChChild path (lm.lg.getAsn1ChChildBackendName c) c.chType.isIA5String) c.chType |>
                List.map (fun v -> {v with inAlternative = true}))
    | ReferenceType ref ->
        let referenced =
            match tryFindTypeAssignment r ref.baseInfo.modName.Value ref.baseInfo.tasName.Value with
            | None    -> []
            | Some ts ->
                // inside a reference the test values are keyed by the referenced type's node ids, which two
                // reference sites of the same type share: only a violation that needs no selection (CHOICE
                // alternative, OPTIONAL child, SEQUENCE OF element) is sure to hold at this site
                constraintViolations r lm path ts.Type |>
                List.filter (fun v -> not v.inAlternative && v.presentChildren.IsEmpty && v.nonEmptySeqOfs.IsEmpty) |>
                List.truncate 1 |>
                List.map (fun v -> {v with contextNodes = t.id :: v.contextNodes})
        let siteCons =
            match ownErrCode t, ref.resolvedType.Kind with
            | Some e, Integer o ->
                let siteIntCons = ref.baseInfo.refCons |> List.choose (fun c -> match c with Asn1AcnAst.IntegerTypeConstraint z -> Some z | _ -> None)
                intViolatingValues r lm o.baseInfo o.baseInfo.cons siteIntCons |>
                List.map (fun v ->
                    let sv = lm.lg.intValueToString v o.baseInfo.intClass
                    mk t.id (lm.init.initInteger (lm.lg.getValue path) sv false "" "") e (sprintf "%s, outside %s" sv (siteIntCons |> List.map (fun c -> c.ASN1) |> Seq.StrJoin "")))
            | Some e, Sequence sq ->
                let asn1Children = sq.children |> List.choose (fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
                let ncs = ref.baseInfo.refCons |> List.choose (fun c -> match c with Asn1AcnAst.SeqConstraint z -> Some z | _ -> None) |> List.collect withComponents
                withComponentsViolations r lm path t asn1Children ncs
            | _ -> []
        referenced @ siteCons
    | IA5String o ->
        // only SIZE and FROM constraints (a NumericString without FROM is not checked for its digits)
        match ownErrCode t, o.baseInfo.cons |> List.forall sizeOrAlphabet with
        | None, _ | Some _, false -> []
        | Some e, true ->
            let sStr = lm.lg.getValue path
            let minSize, maxSize = o.baseInfo.minSize.uper, o.baseInfo.maxSize.uper
            // the string ends one character before its minimum size (the characters before it stay valid)
            let tooShort =
                match minSize >= 1I with
                | true  -> [mk t.id (lm.atc.constraintViolationStringChar sStr (minSize - 1I) 0I) e (sprintf "size %A, outside %A..%A" (minSize - 1I) minSize maxSize)]
                | false -> []
            // the first character is outside the permitted alphabet (an empty start value gets one character)
            let badChar =
                let permitted = o.baseInfo.uperCharSet |> Set.ofArray
                [32 .. 126] @ [1 .. 31] @ [127] |> List.tryFind (fun c -> not (permitted.Contains (char c)))
            let outsideAlphabet =
                match (if o.baseInfo.cons |> List.exists hasAlphabet then badChar else None), maxSize >= 1I with
                | Some c, true -> [mk t.id (lm.atc.constraintViolationStringChar sStr 0I (BigInteger c)) e (sprintf "character code %d, outside the alphabet" c)]
                | Some _, false
                | None, _      -> []
            tooShort @ outsideAlphabet
    | Enumerated o ->
        match ownErrCode t with
        | None   -> []
        | Some e ->
            let values = o.baseInfo.items |> List.map (fun it -> it.definitionValue) |> Set.ofList
            let valid = o.baseInfo.validItems |> List.map (fun it -> it.Name.Value) |> Set.ofList
            let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
            let itemName (it:Asn1AcnAst.NamedItem) =
                lm.lg.qualifyNameWithModule tk.programUnit (ToC "MainProgram") (lm.lg.getNamedItemBackendName (Some o.definitionOrRef) it)
            let excluded = o.baseInfo.items |> List.filter (fun it -> not (valid.Contains it.Name.Value)) |> List.map (fun it -> itemName it, sprintf "%s, excluded by a constraint" it.Name.Value)
            // a value that is no item: only where the typedef can hold it (C: an int)
            let nonItem =
                match lm.lg.typedefConstraintModel with
                | TypedefsAreBaseTypes  ->
                    let v = Seq.initInfinite BigInteger |> Seq.find (fun v -> not (values.Contains v))
                    [v.ToString(), sprintf "%A: not an item value" v]
                | TypedefsEnforceRanges -> []
            nonItem @ excluded |>
            List.map (fun (sValue, what) -> mk t.id (lm.init.initEnumerated (lm.lg.getValue path) sValue "" false "") e what)
    | NullType _ | Boolean _ | ObjectIdentifier _ | TimeType _ -> []

/// WITH COMPONENTS of a SEQUENCE (they fail the SEQUENCE's own check): a child marked PRESENT is made absent; an
/// INTEGER child gets the values next to the named constraint's endpoints that its own type accepts.
and private withComponentsViolations (r:DAst.AstRoot) (lm:LanguageMacros) (path:AccessPath) (t:Asn1Type) (asn1Children:Asn1Child list) (ncs:Asn1AcnAst.NamedConstraint list) =
    match ownErrCode t with
    | None   -> []
    | Some e ->
        let absentFlag = if lm.lg.usesBooleanPresenceBits then lm.lg.FalseLiteral else "0"
        ncs |>
        List.collect (fun nc ->
            match asn1Children |> List.tryFind (fun ch -> ch.Name.Value = nc.Name.Value) with
            | None    -> []
            | Some ch ->
                let chName = lm.lg.getAsn1ChildBackendName ch
                let presentChildren = if ch.Optionality.IsSome then [ch.Type.id] else []
                let absent =
                    match nc.Mark, ch.Optionality with
                    | Asn1Ast.MarkPresent, Some _ ->
                        [{ nodeId = t.id; contextNodes = []; presentChildren = presentChildren; nonEmptySeqOfs = []; inAlternative = false
                           setInvalid = lm.init.initSequence_optionalChild (path.joined lm.lg) (lm.lg.getAccess path) chName absentFlag ""
                           errCode = e; errCodeModule = ToC t.id.ModName; what = sprintf "%s absent, WITH COMPONENTS requires it" ch.Name.Value }]
                    | (Asn1Ast.MarkPresent | Asn1Ast.NoMark | Asn1Ast.MarkOptional | Asn1Ast.MarkAbsent), _ -> []
                let values =
                    match nc.Constraint, ch.Type.ActualType.Kind with
                    | Some (Asn1AcnAst.IntegerTypeConstraint ic), Integer o ->
                        let chPath = lm.lg.getSeqChild path chName ch.Type.isIA5String ch.Optionality.IsSome
                        intViolatingValues r lm o.baseInfo o.baseInfo.cons [ic] |>
                        List.map (fun v ->
                            let sv = lm.lg.intValueToString v o.baseInfo.intClass
                            { nodeId = t.id; contextNodes = []; presentChildren = presentChildren; nonEmptySeqOfs = []; inAlternative = false
                              setInvalid = lm.init.initInteger (lm.lg.getValue chPath) sv false "" ""
                              errCode = e; errCodeModule = ToC t.id.ModName; what = sprintf "%s = %s, outside %s (WITH COMPONENTS)" ch.Name.Value sv ic.ASN1 })
                    | _ -> []
                absent @ values)

/// One automatic test per constraint violation of a type assignment (coverage step 11c), independent of the
/// encoding: it calls IsConstraintValid and every checked encoder of the type.
let constraintViolationTestCases (r:DAst.AstRoot) (lm:LanguageMacros) (t:Asn1Type) (encoders:(string*string) list) =
    let isValidFuncName = t.isValidFunction |> Option.bind (fun f -> f.funcName)
    match lm.lg.atcEmitsConstraintViolationTests, isValidFuncName with
    | true, Some sIsValidFunc ->
        let p = {CodegenScope.modName = ToC "MainProgram"; accessPath = AccessPath.valueEmptyPath "tc_data"}
        let atcs = t.initFunction.automaticTestCases
        let fits (v:ConstraintViolation) (atc:AutomaticTestCase) =
            let m = atc.testCaseTypeIDsMap
            (v.nodeId::v.contextNodes |> List.forall (fun id -> id = t.id || m.ContainsKey id)) &&
            (v.presentChildren |> List.forall (fun chId ->
                // a present child is in the map with its own value (or TcvComponentPresent)
                match m.TryFind chId with
                | Some TcvComponentAbsent -> false
                | Some _                  -> true
                | None                    -> false)) &&
            (v.nonEmptySeqOfs |> List.forall (fun id ->
                match m.TryFind id with
                | Some (TcvSizeableTypeValue n) -> n >= 1I
                | Some _ | None                 -> false))
        constraintViolations r lm p.accessPath t |>
        List.choose (fun v ->
            atcs |> List.tryFind (fits v) |>
            Option.map (fun atc ->
                fun idx ->
                    let sFuncName = sprintf "test_case_constraint_%06d" idx
                    let initStatement = atc.initTestCaseFunc p
                    let arrsVars = initStatement.localVariables |> List.map(fun lv -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct |> Seq.toList
                    let encAmper, _ = gAmber lm t
                    let bStatic = match t.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
                    let td = lm.lg.getTypeDefinition t.FT_TypeDefinition
                    let sTasName = td.typeName
                    let encodeChecks = encoders |> List.map (fun (sEncFunc, sEnc) -> lm.atc.constraintViolationEncodeCheck sEncFunc td.programUnit sTasName sEnc encAmper v.errCodeModule v.errCode.errCodeName)
                    // the text goes into a comment and a string literal
                    let sWhat = v.what.Replace("\"", "'").Replace("\\", "/").Replace("*/", "* /")
                    let func_body = lm.atc.emitConstraintViolationTestCase sFuncName arrsVars td.programUnit sTasName encAmper initStatement.funcBody bStatic v.setInvalid sWhat sIsValidFunc v.errCodeModule v.errCode.errCodeName encodeChecks
                    (emitTestCaseAsFunc_h lm sFuncName, func_body, invokeTestCaseAsFunc lm sFuncName)))
    | true, None
    | false, _ -> []

/// One automatic test per ENUMERATED type assignment whose ACN item values leave a
/// gap: the ACN decoder must reject a stream that holds the smallest unused code
/// (AcnEnum.createInvalidCodeStream), which executes its default arm.
let invalidStreamTestCases (lm:LanguageMacros) (e:Asn1Encoding) (t:Asn1Type) =
    let decFuncName = t.acnDecFunction |> Option.bind (fun f -> f.funcName)
    match lm.lg.atcEmitsInvalidStreamTests, e, t.Kind, decFuncName with
    | true, Asn1Encoding.ACN, Enumerated en, Some sDecFunc ->
        match en.acnInvalidCodeStream with
        | None    -> []
        | Some st ->
            [fun idx ->
                let sFuncName = sprintf "test_case_invalid_stream_%A_%06d" e idx
                let td = lm.lg.getTypeDefinition t.FT_TypeDefinition
                let arrsVars = st.localVariables |> List.map(fun lv -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct |> Seq.toList
                let func_body = lm.atc.emitInvalidStreamTestCase sFuncName arrsVars td.programUnit td.typeName (GetEncodingString lm e) st.codeVarName st.codeVarType st.code st.encodeStatement sDecFunc
                (emitTestCaseAsFunc_h lm sFuncName, func_body, invokeTestCaseAsFunc lm sFuncName)]
    | _ -> []

let asn1EncodingMapping = function
    | UPER  -> UperEncDecFunctionType
    | ACN   -> AcnEncDecFunctionType 
    | BER   -> BerEncDecFunctionType 
    | XER   -> XerEncDecFunctionType 

let printAllTestCasesAndTestCaseRunner (r:DAst.AstRoot) (lm:LanguageMacros) outDir =
    let ambiguousTypeNames = atcAmbiguousTypeNames r
    // Invalid-value tests follow all other tests, so the names of the existing ones do not change.
    let invalidValueFunctors = ResizeArray<int -> string*string*string>()
    // Invalid-stream tests come last, after the invalid-value tests.
    let invalidStreamFunctors = ResizeArray<int -> string*string*string>()
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
                                invalidStreamFunctors.AddRange (invalidStreamTestCases lm e t.Type)
                                for atc in atcsToUse do
                                    let testCaseIsValid = e <> Asn1Encoding.ACN || (isTestCaseValid atc)
                                    if testCaseIsValid then
                                        let generateTcFun idx =
                                            let p = {CodegenScope.modName = ToC "MainProgram"; accessPath = AccessPath.valueEmptyPath "tc_data"}
                                            let initStatement = atc.initTestCaseFunc p
                                            let dummyInitStatementsNeededForStatementCoverage = (emitDummyInitStatementsNeededForStatementCoverage ambiguousTypeNames lm t.Type)//t.Type.initFunction.initFuncName

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
                                let dummyInitStatementsNeededForStatementCoverage = (emitDummyInitStatementsNeededForStatementCoverage ambiguousTypeNames lm v.Type)
                                PrintValueAssignmentAsTestCase r lm e v m typeModName tasName (*(getTypeDecl r (ToC m.Name.Value) l v )*)  idx dummyInitStatementsNeededForStatementCoverage
                            yield generateTcFun
                        | None         -> ()
        } |> Seq.toList
    // Constraint-violation tests (coverage step 11c) come last, one set per type assignment with automatic tests.
    let constraintFunctors =
        [ for m in r.Files |> List.collect(fun f -> f.Modules) do
            for t in m.TypeAssignments do
                let tested (e:Asn1Encoding) =
                    let typeAssignmentInfo = t.Type.id.tasInfo.Value
                    (t.Type.getEncDecTestFunc e).IsSome &&
                    r.callersSet |> Set.contains {Caller.typeId = typeAssignmentInfo; funcType = asn1EncodingMapping e}
                let encoders =
                    r.args.encodings |>
                    List.choose (fun e ->
                        match e with
                        | Asn1Encoding.UPER when tested e ->
                            t.Type.uperEncFunction.funcName |> Option.map (fun f -> (f, GetEncodingString lm e))
                        | Asn1Encoding.ACN when tested e && hasAcnEncodeFunction t.Type.acnEncFunction t.Type.acnParameters t.Type.id.tasInfo ->
                            t.Type.acnEncFunction |> Option.bind (fun f -> f.funcName) |> Option.map (fun f -> (f, GetEncodingString lm e))
                        | Asn1Encoding.UPER | Asn1Encoding.ACN | Asn1Encoding.XER | Asn1Encoding.BER -> None)
                match r.args.encodings |> List.exists tested with
                | true  -> yield! constraintViolationTestCases r lm t.Type encoders
                | false -> () ]
    let tcFunctors = validFunctors @ List.ofSeq invalidValueFunctors @ List.ofSeq invalidStreamFunctors @ constraintFunctors
    let invalidStreamFunctorCount = invalidStreamFunctors.Count
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

        // Invalid-stream tests write the stream with the ACN runtime.
        let rtlUnits =
            match invalidStreamFunctorCount > 0 with
            | true  -> lm.lg.atcInvalidStreamPackages
            | false -> []
        let contentC = printTestCaseFileBody testCaseFileName (includedPackages r lm) arrsTestFunctionBodies (r.programUnits |> List.map (fun pu -> lm.lg.sanitizeModuleName pu.name)) rtlUnits
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
