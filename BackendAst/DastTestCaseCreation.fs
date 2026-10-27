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
    | true      -> r.programUnits |> Seq.collect(fun x -> [lm.lg.sanitizeModuleName x.name; lm.lg.sanitizeModuleName x.testcase_name])


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

/// Where an invalid discriminant (coverage step 12d) sits: in the type's own tree, where step 02's
/// test_case_invalid_* encode it unchecked already, or behind a reference / in a SEQUENCE OF element, where the
/// parent often encodes it inline with its own default arm.
type DiscriminantPlace =
    | OwnTree
    | Nested

/// How a constraint-violation test calls the encoders.
type ViolationEncoding =
    | CheckedOnly                           // a value outside a constraint: an unchecked encoder may write garbage or out of bounds
    | InvalidEnumItem of DiscriminantPlace  // an ENUMERATED value that is no item
    | InvalidSelector of DiscriminantPlace  // a CHOICE selector set to <X>_NONE

/// Coverage step 11c (T2): a value just outside one constraint, set on one node of a type assignment's value.
/// IsConstraintValid must return FALSE with errCode, every checked encoder too; for a nested invalid discriminant
/// (step 12d) every unchecked encoder must fail as well.
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
    encoding        : ViolationEncoding         // whether the unchecked encoders are called too
}

/// A violation of a referenced type or of a SEQUENCE OF element, seen from the enclosing type.
let private nested (v:ConstraintViolation) =
    match v.encoding with
    | CheckedOnly       -> v
    | InvalidEnumItem _ -> {v with encoding = InvalidEnumItem Nested}
    | InvalidSelector _ -> {v with encoding = InvalidSelector Nested}

/// A reference site with constraints of its own (h Inner (WITH COMPONENTS {...})) gets a merged type whose nodes, and
/// the automatic test values of the site, are keyed by the site's ids (Outer.h.k), not by the referenced type's
/// (Inner.k): the ids under the referenced type are moved under the merged type.
let private rebaseIds (fromId:ReferenceToType) (toId:ReferenceToType) (v:ConstraintViolation) =
    let prefix = fromId.ToScopeNodeList
    let rebase (id:ReferenceToType) =
        let nodes = id.ToScopeNodeList
        match nodes.Length >= prefix.Length && List.take prefix.Length nodes = prefix with
        | true  -> ReferenceToType (toId.ToScopeNodeList @ List.skip prefix.Length nodes)
        | false -> id
    {v with nodeId = rebase v.nodeId; contextNodes = List.map rebase v.contextNodes
            presentChildren = List.map rebase v.presentChildren; nonEmptySeqOfs = List.map rebase v.nonEmptySeqOfs}

/// The node with this id in a type's tree, through references (only the subtrees whose ids lead to it).
let rec private tryFindNode (id:ReferenceToType) (t:Asn1Type) : Asn1Type option =
    let leadsTo (x:ReferenceToType) =
        let p, l = x.ToScopeNodeList, id.ToScopeNodeList
        p.Length <= l.Length && List.take p.Length l = p
    match t.id = id, leadsTo t.id with
    | true, _      -> Some t
    | false, false -> None
    | false, true  ->
        match t.Kind with
        | Sequence sq   -> sq.children |> List.tryPick (fun c -> match c with Asn1Child ch -> tryFindNode id ch.Type | AcnChild _ -> None)
        | Choice ch     -> ch.children |> List.tryPick (fun c -> tryFindNode id c.chType)
        | SequenceOf o  -> tryFindNode id o.childType
        | ReferenceType rf -> tryFindNode id rf.resolvedType
        | Integer _ | Real _ | IA5String _ | OctetString _ | NullType _ | BitString _ | Boolean _
        | Enumerated _ | ObjectIdentifier _ | TimeType _ -> None

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

/// The SIZE range of a referenced sizeable type assignment and whether its typedef is the fixed-size one (no length
/// field): the typedef is the one at the end of the chain of referenced type assignments (as typedefIsFixedSize).
let rec private referencedSizeRange (r:DAst.AstRoot) (modName:string) (tasName:string) : (BigInteger * BigInteger * bool) option =
    match tryFindTypeAssignment r modName tasName with
    | None    -> None
    | Some ts ->
        let typedefFixed (isFixed:bool) =
            match ts.Type.Kind with
            | ReferenceType ref -> referencedSizeRange r ref.baseInfo.modName.Value ref.baseInfo.tasName.Value |> Option.map (fun (_, _, f) -> f)
            | _                 -> Some isFixed
        let range (minSize:BigInteger) (maxSize:BigInteger) =
            typedefFixed (minSize = maxSize) |> Option.map (fun f -> (minSize, maxSize, f))
        match ts.Type.ActualType.Kind with
        | OctetString o -> range o.baseInfo.minSize.uper o.baseInfo.maxSize.uper
        | BitString o   -> range o.baseInfo.minSize.uper o.baseInfo.maxSize.uper
        | SequenceOf o  -> range o.baseInfo.minSize.uper o.baseInfo.maxSize.uper
        | _             -> None

/// The constraint violations of a type's own tree (coverage step 11c). A reference contributes only the first
/// violation of the referenced type assignment (its own tests cover the rest), so that the parent's check of the
/// call is covered, every invalid discriminant of it (step 12d), plus the violations of the constraints added at
/// the reference site.
/// Coverage step 12f: the type assignments that have exactly one reference site in the value of a type, counted through
/// nested references (Inner once in Mid, Mid twice in Outer: two Inner sites). A site inside a SEQUENCE OF element is one
/// per element: the automatic test values give the elements different values.
let private uniqueReferenceSites (r:DAst.AstRoot) (root:Asn1Type) : Set<string * string> =
    let rec sites (many:bool) (t:Asn1Type) : ((string * string) * int) list =
        match t.Kind with
        | Sequence sq       -> sq.children |> List.collect (fun c -> match c with Asn1Child ch -> sites many ch.Type | AcnChild _ -> [])
        | Choice ch         -> ch.children |> List.collect (fun c -> sites many c.chType)
        | SequenceOf o      -> sites true o.childType
        | ReferenceType rf  ->
            let key = rf.baseInfo.modName.Value, rf.baseInfo.tasName.Value
            let inside =
                match tryFindTypeAssignment r (fst key) (snd key) with
                | Some ts -> sites many ts.Type
                | None    -> []
            (key, (if many then 2 else 1)) :: inside
        | Integer _ | Real _ | IA5String _ | OctetString _ | NullType _ | BitString _ | Boolean _
        | Enumerated _ | ObjectIdentifier _ | TimeType _ -> []
    sites false root |> List.groupBy fst |> List.filter (fun (_, xs) -> (xs |> List.sumBy snd) = 1) |> List.map fst |> Set.ofList

/// unique: the type assignments with one reference site in the tested value (uniqueReferenceSites).
let rec constraintViolations (r:DAst.AstRoot) (lm:LanguageMacros) (unique:Set<string * string>) (path:AccessPath) (t:Asn1Type) : ConstraintViolation list =
    let mk nodeId sSet errCode what = {nodeId = nodeId; contextNodes = []; presentChildren = []; nonEmptySeqOfs = []; inAlternative = false; setInvalid = sSet; errCode = errCode; errCodeModule = ToC t.id.ModName; what = what; encoding = CheckedOnly}
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
        let elems = constraintViolations r lm unique elemPath o.childType |>
                    // the elements get different test values, but the test value says only that one of them selects the
                    // alternative or has the OPTIONAL child present (element 0 is changed): no selection inside an element
                    List.filter (fun v -> not v.inAlternative && v.presentChildren.IsEmpty) |>
                    List.map (fun v -> nested {v with nonEmptySeqOfs = t.id::v.nonEmptySeqOfs})
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
                    constraintViolations r lm unique (childPath ch) ch.Type |> List.map (fun v -> {v with presentChildren = ch.Type.id :: v.presentChildren})
                | None -> constraintViolations r lm unique (childPath ch) ch.Type)
        let presence = withComponentsViolations r lm path t asn1Children (sq.baseInfo.cons |> List.collect withComponents)
        children @ presence
    | Choice ch ->
        // the selector set to <X>_NONE: only where the language can represent it (as step 02's invalidValueNodes)
        let selector =
            match lm.lg.atcEmitsInvalidValueTests, ownErrCode t with
            | true, Some e ->
                let sSet = sprintf "%s%skind = 0; /* <X>_NONE */" (path.joined lm.lg) (lm.lg.getAccess path)
                [{mk t.id sSet e "selector <X>_NONE" with encoding = InvalidSelector OwnTree}]
            | true, None
            | false, _ -> []
        let alternatives =
            ch.children |>
            List.collect (fun c ->
                match c.Optionality with
                | Some Asn1AcnAst.ChoiceAlwaysAbsent -> []
                | Some Asn1AcnAst.ChoiceAlwaysPresent
                | None ->
                    constraintViolations r lm unique (lm.lg.getChChild path (lm.lg.getAsn1ChChildBackendName c) c.chType.isIA5String) c.chType |>
                    List.map (fun v -> {v with inAlternative = true}))
        selector @ alternatives
    | ReferenceType ref ->
        let referenced =
            match tryFindTypeAssignment r ref.baseInfo.modName.Value ref.baseInfo.tasName.Value with
            | None    -> []
            | Some ts ->
                // inside a reference the test values are keyed by the referenced type's node ids, which two
                // reference sites of the same type share: a violation that needs a selection (CHOICE alternative,
                // OPTIONAL child, SEQUENCE OF element) is sure to hold at this site only when it is the type's one
                // site in the tested value (coverage step 12f)
                let needsSelection (v:ConstraintViolation) = v.inAlternative || not v.presentChildren.IsEmpty || not v.nonEmptySeqOfs.IsEmpty
                let candidates =
                    constraintViolations r lm unique path ts.Type |>
                    List.filter (fun v -> unique.Contains (ref.baseInfo.modName.Value, ref.baseInfo.tasName.Value) || not (needsSelection v)) |>
                    List.indexed
                // the first one covers the parent's check of the call (one that needs no selection if there is one, as
                // before 12f); every invalid discriminant too (step 12d): the parent often encodes the referenced type
                // inline, with a default arm of its own
                let first =
                    let callCheck (v:ConstraintViolation) =
                        match v.encoding with
                        | CheckedOnly | InvalidEnumItem _ -> true
                        | InvalidSelector _               -> false
                    candidates |> List.tryFind (fun (_, v) -> callCheck v && not (needsSelection v)) |>
                    Option.orElse (candidates |> List.tryFind (fun (_, v) -> callCheck v))
                let discriminants =
                    candidates |>
                    List.filter (fun (i, v) ->
                        match v.encoding, first with
                        | CheckedOnly, _                                       -> false
                        | (InvalidEnumItem _ | InvalidSelector _), Some (j, _) -> i <> j
                        | (InvalidEnumItem _ | InvalidSelector _), None        -> true)
                // the site's constraints may reach into references of the referenced type (WITH COMPONENTS of a
                // component): those reference sites are merged types with ids of their own as well
                let rec rebaseNestedSites i (v:ConstraintViolation) =
                    match List.tryItem i v.contextNodes with
                    | None     -> v
                    | Some ctx ->
                        let v =
                            match tryFindNode ctx ref.resolvedType |> Option.map (fun n -> n.Kind) with
                            | Some (ReferenceType rf) ->
                                match tryFindTypeAssignment r rf.baseInfo.modName.Value rf.baseInfo.tasName.Value with
                                | Some ts2 -> rebaseIds ts2.Type.id rf.resolvedType.id v
                                | None     -> v
                            | Some _ | None -> v
                        rebaseNestedSites (i + 1) v
                (Option.toList first) @ discriminants |>
                List.map (snd >> nested >> rebaseIds ts.Type.id ref.resolvedType.id >> rebaseNestedSites 0) |>
                List.map (fun v ->
                    // the referenced type's own value is the reference site's value (the test values key a
                    // primitive reference by the site only)
                    match v.nodeId = ref.resolvedType.id with
                    | true  -> {v with nodeId = t.id}
                    | false -> {v with contextNodes = t.id :: v.contextNodes})
        // A SIZE at the site (ByteFlags ::= BitFlags (SIZE(8))) keeps the typedef of the referenced type, so when
        // that typedef has a length field the site's check is a real one (q/issues.md Issue 46): the sizes next to
        // the site's range that the referenced type still accepts (else its own check fails first).
        let siteSizeViolations (e:ErrorCode) (minSize:BigInteger) (maxSize:BigInteger) (isPure:bool) =
            match isPure, referencedSizeRange r ref.baseInfo.modName.Value ref.baseInfo.tasName.Value with
            | true, Some (refMin, refMax, false) ->
                [minSize - 1I; maxSize + 1I] |> List.filter (fun n -> refMin <= n && n <= refMax) |>
                List.map (fun n -> mk t.id (lm.init.initVarSizeSequenceOf (path.joined lm.lg) (lm.lg.getAccess path) n []) e (sprintf "size %A, outside %A..%A" n minSize maxSize))
            | true, Some (_, _, true)
            | true, None
            | false, _ -> []
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
            | Some e, OctetString o ->
                siteSizeViolations e o.baseInfo.minSize.uper o.baseInfo.maxSize.uper (ref.baseInfo.refCons |> List.forall (fun c -> match c with Asn1AcnAst.OctetStringConstraint z -> pureSize z | _ -> false))
            | Some e, BitString o ->
                siteSizeViolations e o.baseInfo.minSize.uper o.baseInfo.maxSize.uper (ref.baseInfo.refCons |> List.forall (fun c -> match c with Asn1AcnAst.BitStringConstraint z -> pureSize z | _ -> false))
            | Some e, SequenceOf o ->
                siteSizeViolations e o.baseInfo.minSize.uper o.baseInfo.maxSize.uper (ref.baseInfo.refCons |> List.forall (fun c -> match c with Asn1AcnAst.SequenceOfConstraint z -> pureSeqOfSize z | _ -> false))
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
            let set (sValue, what) = mk t.id (lm.init.initEnumerated (lm.lg.getValue path) sValue "" false "") e what
            (nonItem |> List.map (fun x -> {set x with encoding = InvalidEnumItem OwnTree})) @ (excluded |> List.map set)
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
                           errCode = e; errCodeModule = ToC t.id.ModName; what = sprintf "%s absent, WITH COMPONENTS requires it" ch.Name.Value; encoding = CheckedOnly }]
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
                              errCode = e; errCodeModule = ToC t.id.ModName; what = sprintf "%s = %s, outside %s (WITH COMPONENTS)" ch.Name.Value sv ic.ASN1; encoding = CheckedOnly })
                    | _ -> []
                absent @ values)

/// One automatic test per constraint violation of a type assignment (coverage step 11c), independent of the
/// encoding: it calls IsConstraintValid and every checked encoder of the type; for a nested invalid discriminant
/// (step 12d) every unchecked encoder too.
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
        // the unchecked encoders reach the arm of an invalid discriminant only from a value they can encode
        let acnValid (atc:AutomaticTestCase) = t.acnEncFunction |> Option.forall (fun f -> f.isTestVaseValid atc)
        let start (v:ConstraintViolation) =
            let fitting = atcs |> List.filter (fits v)
            match v.encoding with
            | InvalidEnumItem Nested
            | InvalidSelector Nested -> fitting |> List.tryFind acnValid |> Option.orElse (List.tryHead fitting)
            | CheckedOnly
            | InvalidEnumItem OwnTree
            | InvalidSelector OwnTree -> List.tryHead fitting
        constraintViolations r lm (uniqueReferenceSites r t) p.accessPath t |>
        // the type's own CHOICE selectors: step 02's test_case_invalid_* cover them, checked and unchecked
        List.filter (fun v ->
            match v.encoding with
            | InvalidSelector OwnTree -> false
            | CheckedOnly | InvalidEnumItem _ | InvalidSelector Nested -> true) |>
        List.choose (fun v ->
            start v |>
            Option.map (fun atc ->
                fun idx ->
                    let sFuncName = sprintf "test_case_constraint_%06d" idx
                    let initStatement = atc.initTestCaseFunc p
                    let arrsVars = initStatement.localVariables |> List.map(fun lv -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct |> Seq.toList
                    let encAmper, _ = gAmber lm t
                    let bStatic = match t.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
                    let td = lm.lg.getTypeDefinition t.FT_TypeDefinition
                    let sTasName = td.typeName
                    let checkedEncodes = encoders |> List.map (fun (sEncFunc, sEnc) -> lm.atc.constraintViolationEncodeCheck sEncFunc td.programUnit sTasName sEnc encAmper v.errCodeModule v.errCode.errCodeName)
                    let uncheckedEncodes, what =
                        match v.encoding with
                        | InvalidEnumItem Nested
                        | InvalidSelector Nested ->
                            encoders |> List.map (fun (sEncFunc, sEnc) -> lm.atc.constraintViolationUncheckedEncodeCheck sEncFunc td.programUnit sTasName sEnc encAmper),
                            v.what + ", unchecked encoding too"
                        | CheckedOnly
                        | InvalidEnumItem OwnTree
                        | InvalidSelector OwnTree -> [], v.what
                    let encodeChecks = checkedEncodes @ uncheckedEncodes
                    // the text goes into a comment and a string literal
                    let sWhat = what.Replace("\"", "'").Replace("\\", "/").Replace("*/", "* /")
                    let func_body = lm.atc.emitConstraintViolationTestCase sFuncName arrsVars td.programUnit sTasName encAmper initStatement.funcBody bStatic v.setInvalid sWhat sIsValidFunc v.errCodeModule v.errCode.errCodeName encodeChecks
                    (emitTestCaseAsFunc_h lm sFuncName, func_body, invokeTestCaseAsFunc lm sFuncName)))
    | true, None
    | false, _ -> []

/// Coverage step 14a: T_Equal with two different values. The automatic test values carry their variants (one node of
/// the type's own tree changed, AutomaticTestCase.variants); one test per type assignment that sets, one after the
/// other, the start values whose variants an earlier one does not cover, so every variant is checked once. The first variant of a value replaces it with another test value of
/// the type (a primitive type assignment's own compare).
/// The variants of a type assignment's i-th automatic test value: the whole value replaced by the next test value,
/// then the value's own variants (AutomaticTestCase.variants).
let private startVariants (t:Asn1Type) (atcs:AutomaticTestCase array) (i:int) (atc:AutomaticTestCase) =
    let whole =
        match atcs.Length >= 2 with
        | true  ->
            let other = atcs.[(i + 1) % atcs.Length]
            [{AtcVariant.nodeId = t.id; what = ": another test value"; applyFnc = other.initTestCaseFunc; replaces = Some (t.id, other.testCaseTypeIDsMap)}]
        | false -> []
    whole @ atc.variants

/// The description of a variant with the type assignment's name.
let private variantText (t:Asn1Type) (sTypeName:string) (v:AtcVariant) =
    let what = if v.what.StartsWith ":" then sTypeName + v.what else sTypeName + "." + v.what
    // the text goes into a comment and a string literal
    what.Replace("\"", "'").Replace("\\", "/").Replace("*/", "* /")

let equalityVariantTestCases (lm:LanguageMacros) (t:Asn1Type) =
    match lm.lg.atcEmitsEqualityVariantTests, t.equalFunction.isEqualFuncName with
    | true, Some sEqualFunc ->
        let atcs = t.initFunction.automaticTestCases |> List.toArray
        let variantsOf = startVariants t atcs
        let _, starts =
            atcs |> Array.toList |> List.indexed |>
            List.fold (fun (seen:Set<string>, acc) (i, atc) ->
                let fresh = variantsOf i atc |> List.distinctBy (fun v -> v.what) |> List.filter (fun v -> not (seen.Contains v.what))
                match fresh with
                | [] -> (seen, acc)
                | _  -> (fresh |> List.fold (fun (s:Set<string>) v -> s.Add v.what) seen, acc @ [(atc, fresh)])) (Set.empty, [])
        let td = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let encAmper, _ = gAmber lm t
        let bStatic = match t.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
        let sTypeName = t.id.tasInfo |> Option.map (fun ti -> ti.tasName) |> Option.defaultValue td.typeName
        let p  = {CodegenScope.modName = ToC "MainProgram"; accessPath = AccessPath.valueEmptyPath "tc_data"}
        let p2 = {CodegenScope.modName = ToC "MainProgram"; accessPath = AccessPath.valueEmptyPath "tc_data2"}
        // one test per type assignment: the start values one after the other in the same tc_data / tc_data2 (a test
        // per start value gave PUS-C S12 > 2 GB of static test data in C)
        match starts with
        | [] -> []
        | _  ->
            [fun idx ->
                let sFuncName = sprintf "test_case_unequal_%06d" idx
                let parts =
                    starts |>
                    List.map (fun (atc, variants) ->
                        let initStatement = atc.initTestCaseFunc p
                        let applied = variants |> List.map (fun v -> v, v.applyFnc p2)
                        let checks = applied |> List.map (fun (v, a) -> lm.atc.equalityVariantCheck td.programUnit encAmper a.funcBody (variantText t sTypeName v) sEqualFunc)
                        initStatement.localVariables @ (applied |> List.collect (fun (_, a) -> a.localVariables)), initStatement.funcBody :: checks)
                let arrsVars = parts |> List.collect fst |> List.map lm.lg.getLocalVariableDeclaration |> Seq.distinct |> Seq.toList
                let blocks = parts |> List.collect snd
                let func_body = lm.atc.emitEqualityVariantTestCase sFuncName arrsVars td.programUnit td.typeName encAmper "" bStatic sEqualFunc blocks
                (emitTestCaseAsFunc_h lm sFuncName, func_body, invokeTestCaseAsFunc lm sFuncName)]
    | true, None
    | false, _ -> []

/// Coverage step 14d: values whose ACN fields disagree. A variant that writes another test value at one node
/// (AtcVariant.replaces) of an ACN-valid automatic test value, after which the value is no longer ACN-valid
/// (isTestVaseValid: the consumers of an ACN field disagree, e.g. CHOICEs sharing a determinant select different
/// alternatives, or sizes / presences differ), must be rejected by the ACN encoder with a nonzero error code. One test
/// per such variant (each description once).
let inconsistentValueTestCases (lm:LanguageMacros) (t:Asn1Type) (sEncFunc:string) (sEnc:string) =
    match lm.lg.atcEmitsConstraintViolationTests, t.acnEncFunction with
    | true, Some acnEnc ->
        let atcs = t.initFunction.automaticTestCases |> List.toArray
        let under (node:ReferenceToType) (id:ReferenceToType) =
            let a, b = node.ToScopeNodeList, id.ToScopeNodeList
            a.Length <= b.Length && List.take a.Length b = a
        // the start value's map with the node's subtree replaced by the written test value's map
        let changedMap (atc:AutomaticTestCase) (node:ReferenceToType, written:Map<ReferenceToType, TestCaseValue>) =
            written |> Map.fold (fun (m:Map<ReferenceToType, TestCaseValue>) k v -> m.Add (k, v)) (atc.testCaseTypeIDsMap |> Map.filter (fun k _ -> not (under node k)))
        let td = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let encAmper, _ = gAmber lm t
        let bStatic = match t.ActualType.Kind with Integer _ | Enumerated(_) -> false | _ -> true
        let sTypeName = t.id.tasInfo |> Option.map (fun ti -> ti.tasName) |> Option.defaultValue td.typeName
        let p = {CodegenScope.modName = ToC "MainProgram"; accessPath = AccessPath.valueEmptyPath "tc_data"}
        atcs |> Array.toList |> List.indexed |>
        List.filter (fun (_, atc) -> acnEnc.isTestVaseValid atc) |>
        List.collect (fun (i, atc) ->
            startVariants t atcs i atc |>
            List.choose (fun v ->
                v.replaces |>
                Option.bind (fun replaced ->
                    match acnEnc.isTestVaseValid {atc with testCaseTypeIDsMap = changedMap atc replaced} with
                    | true  -> None
                    | false -> Some (atc, v)))) |>
        List.distinctBy (fun (_, v) -> v.what) |>
        List.map (fun (atc, v) ->
            fun idx ->
                let sFuncName = sprintf "test_case_inconsistent_%06d" idx
                let initStatement = atc.initTestCaseFunc p
                let change = v.applyFnc p
                let arrsVars =
                    initStatement.localVariables @ change.localVariables |>
                    List.map lm.lg.getLocalVariableDeclaration |> Seq.distinct |> Seq.toList
                let func_body = lm.atc.emitInconsistentValueTestCase sFuncName arrsVars td.programUnit td.typeName encAmper initStatement.funcBody bStatic change.funcBody (variantText t sTypeName v) sEncFunc sEnc
                (emitTestCaseAsFunc_h lm sFuncName, func_body, invokeTestCaseAsFunc lm sFuncName))
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
    // Equal tests with two different values (coverage step 14a) come after them.
    let equalityFunctors =
        [ for m in r.Files |> List.collect(fun f -> f.Modules) do
            for t in m.TypeAssignments do
                let tested (e:Asn1Encoding) =
                    let typeAssignmentInfo = t.Type.id.tasInfo.Value
                    (t.Type.getEncDecTestFunc e).IsSome &&
                    r.callersSet |> Set.contains {Caller.typeId = typeAssignmentInfo; funcType = asn1EncodingMapping e}
                match r.args.encodings |> List.exists tested with
                | true  -> yield! equalityVariantTestCases lm t.Type
                | false -> () ]
    // Values whose ACN fields disagree (coverage step 14d), the ACN encoder of each type assignment tested with ACN.
    let inconsistentFunctors =
        [ for m in r.Files |> List.collect(fun f -> f.Modules) do
            for t in m.TypeAssignments do
                let typeAssignmentInfo = t.Type.id.tasInfo.Value
                let testedAcn =
                    r.args.encodings |> List.contains Asn1Encoding.ACN &&
                    (t.Type.getEncDecTestFunc Asn1Encoding.ACN).IsSome &&
                    r.callersSet |> Set.contains {Caller.typeId = typeAssignmentInfo; funcType = asn1EncodingMapping Asn1Encoding.ACN} &&
                    hasAcnEncodeFunction t.Type.acnEncFunction t.Type.acnParameters t.Type.id.tasInfo
                match testedAcn, t.Type.acnEncFunction |> Option.bind (fun f -> f.funcName) with
                | true, Some sEncFunc -> yield! inconsistentValueTestCases lm t.Type sEncFunc (GetEncodingString lm Asn1Encoding.ACN)
                | true, None
                | false, _ -> () ]
    let tcFunctors = validFunctors @ List.ofSeq invalidValueFunctors @ List.ofSeq invalidStreamFunctors @ constraintFunctors @ equalityFunctors @ inconsistentFunctors
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
