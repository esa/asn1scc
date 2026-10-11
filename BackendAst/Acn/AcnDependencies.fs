module AcnDependencies

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language

open AcnHelpers
open AcnDeterminantDef


let initExpr (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (t: Asn1AcnAst.AcnInsertedType): string =
    match t with
    | AcnInteger int -> lm.lg.asn1SccIntValueToString 0I int.isUnsigned
    | AcnNullType _ -> lm.lg.asn1SccIntValueToString 0I true
    | AcnBoolean _ -> lm.lg.FalseLiteral
    | AcnReferenceToIA5String s -> lm.lg.initializeString None (int s.str.maxSize.uper)
    | AcnReferenceToEnumerated e ->
        lm.lg.getNamedItemBackendName (Some (defOrRef r m e)) e.enumerated.items.Head

// Choose the right base scope for folding a dependency path.
// In --acn-v2, a reference type's funcBody may be inlined into a specialized
// function whose runtime pVal does not correspond to the captured type's TAS.
// To generate correct access paths, walk the enclosing scopes (current SEQUENCE
// + ancestors from nestingScope.parents) and pick the DEEPEST whose type id is
// a prefix of depPath.  Fold the remaining nodes from that scope.
//
// Intra-sequence deps resolve to the current SEQUENCE (shortest fold).
// Sibling/outer deps resolve to an ancestor whose scope matches the dep's
// outer TA, giving the correct base pVal for the fold.
//
// The returned path carries the backend names of the SEQUENCE / CHOICE
// children (withBackendChildNames), so that the access path built from it
// matches the generated type definitions.
let rec resolveDepScope (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (nestingScope: NestingScope) (pSrcRoot: CodegenScope) (depPath: ReferenceToType) : CodegenScope * ReferenceToType =
    let (ReferenceToType depNodes) = depPath
    let (ReferenceToType backendNodes) = withBackendChildNames r lm depPath
    let candidates = nestingScope.parents |> List.map (fun (p, t) -> (p, t.id))
    let matching =
        candidates |> List.tryFind (fun (_, ReferenceToType scopeNodes) ->
            let n = List.length scopeNodes
            n >= 2 && List.length depNodes >= n && List.take n depNodes = scopeNodes)
    match matching with
    | Some (scope, ReferenceToType scopeNodes) ->
        let n = List.length scopeNodes
        let remaining = ReferenceToType (List.take 2 scopeNodes @ List.skip n backendNodes)
        scope, remaining
    | None ->
        pSrcRoot, ReferenceToType backendNodes

// The scope nodes of a type path with the backend names of the SEQUENCE and
// CHOICE children instead of their ASN.1 names. The two differ when -fp AUTO
// renames a field (EnsureUniqueEnumNames: a keyword, a type name or a module
// name), e.g. Ada "type" -> "MyPDU_type", C "switch" -> "MyPDU_switch".
// A node whose child cannot be found keeps its ASN.1 name.
and withBackendChildNames (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (path: ReferenceToType) : ReferenceToType =
    let rec walk (t: Asn1AcnAst.Asn1Type option) (nodes: ScopeNode list) : ScopeNode list =
        match nodes, t |> Option.map (fun t -> t.ActualType.Kind) with
        | [], _ -> []
        | (SEQ_CHILD (name, isOpt)) :: rest, Some (Asn1AcnAst.Sequence s) ->
            match s.children |> List.tryPick (fun c -> match c with Asn1AcnAst.Asn1Child c when c.Name.Value = name -> Some c | _ -> None) with
            | Some c -> SEQ_CHILD (lm.lg.getAsn1ChildBackendName0 c, isOpt) :: walk (Some c.Type) rest
            | None   -> SEQ_CHILD (name, isOpt) :: walk None rest
        | (CH_CHILD (name, preName, chParent)) :: rest, Some (Asn1AcnAst.Choice ch) ->
            match ch.children |> List.tryFind (fun (c: Asn1AcnAst.ChChildInfo) -> c.Name.Value = name) with
            | Some c -> CH_CHILD (lm.lg.getAsn1ChChildBackendName0 c, preName, chParent) :: walk (Some c.Type) rest
            | None   -> CH_CHILD (name, preName, chParent) :: walk None rest
        | SQF :: rest, Some (Asn1AcnAst.SequenceOf sqf) -> SQF :: walk (Some sqf.child) rest
        | n :: rest, _ -> n :: walk None rest
    match path with
    | ReferenceToType ((MD md) :: (TA tas) :: xs) ->
        let tasType = r.typeAssignmentsMap.TryFind (md, tas) |> Option.map (fun ts -> ts.Type)
        ReferenceToType ((MD md) :: (TA tas) :: walk tasType xs)
    | _ -> path

/// Find which acnParameter is the CONTAINING size determinant by checking
/// the dependency list for AcnDepSizeDeterminant_bit_oct_str_contain.
/// Returns the parameter, or None if not found.
let findContainingSizeParam
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (o: Asn1AcnAst.ReferenceType) : AcnGenericTypes.AcnParameter option =
    o.resolvedType.acnParameters |> List.tryFind (fun prm ->
        deps.acnDependencies |> List.exists (fun d ->
            d.determinant.id = prm.id
            && (match d.dependencyKind with
                | Asn1AcnAst.AcnDepSizeDeterminant_bit_oct_str_contain _ -> true
                | _ -> false)))


// Context bundle threaded through every per-case handler in
// handleSingleUpdateDependency.  Five fields — well below the >12 threshold
// at which the split would be the wrong shape (per refactor plan §5).
type private DepContext = {
    r:    Asn1AcnAst.AstRoot
    deps: Asn1AcnAst.AcnInsertedFieldDependencies
    lm:   LanguageMacros
    m:    Asn1AcnAst.Asn1Module
    d:    AcnDependency
}

/// The auto-generated ICD comment(s) for a determinant, derived purely from
/// the dependency (no state / error-code side effects).  Single source of
/// truth shared by the per-case update handlers below AND by the --acn-v2
/// deferred backend (DAstACNDeferred), which computes determinant comments
/// directly from the un-dealiased dependency list because its deferred ACN
/// children carry no funcUpdateStatement to harvest them from (roadmap B8).
let icdCommentsForDependency (d: AcnDependency) : string list =
    match d.dependencyKind with
    | AcnDepRefTypeArgument acnPrm                    -> [sprintf "reference determinant for %s " (acnPrm.id.AsString)]
    | AcnDepSizeDeterminant _                         -> [sprintf "size determinant for %s " (d.asn1Type.AsString)]
    | AcnDepSizeDeterminant_bit_oct_str_contain _     -> [sprintf "size determinant for %s " (d.asn1Type.AsString)]
    | AcnDepIA5StringSizeDeterminant _                -> [sprintf "size determinant for %s " (d.asn1Type.AsString)]
    | AcnDepPresenceBool                              -> [sprintf "Used as a presence determinant for %s " (d.asn1Type.AsString)]
    | AcnDepPresence (_, chc)                         -> [sprintf "Used as a presence determinant for %s " (chc.typeDef[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)]
    | AcnDepPresenceStr _                             -> []
    | AcnDepChoiceDeterminant (_, chc, _)             -> [sprintf "Used as a presence determinant for %s " (chc.typeDef[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)]

/// The value of a determinant computed from an automatic test value, per dependency; None when it
/// cannot be computed (the test value is then not valid for ACN). The update handlers below use it,
/// and so does an --acn-v2 deferred determinant, which has no update statement (DAstConstruction);
/// it needs no state and generates no code.
let private dependencyTestCaseFnc (d:AcnDependency) (atc:AutomaticTestCase) : TestCaseValue option =
    let selectedChildren (chc:Asn1AcnAst.Choice) =
        chc.children |> List.filter(fun ch -> atc.testCaseTypeIDsMap.ContainsKey ch.Type.id)
    let presentWhen (relPath:AcnGenericTypes.RelativePath) (ch:Asn1AcnAst.ChChildInfo) =
        ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
    match d.dependencyKind with
    | AcnDepRefTypeArgument _ ->
        raise (BugErrorException "dependencyTestCaseFnc: a reference argument takes the value of the parameter's dependencies")
    | AcnDepSizeDeterminant _
    | AcnDepSizeDeterminant_bit_oct_str_contain _
    | AcnDepIA5StringSizeDeterminant _ ->
        atc.testCaseTypeIDsMap.TryFind d.asn1Type
    | AcnDepPresenceBool ->
        match atc.testCaseTypeIDsMap.TryFind(d.asn1Type) with
        | Some _    -> Some TcvComponentPresent
        | None      -> Some TcvComponentAbsent
    | AcnDepPresence (relPath, chc) ->
        let updateValues =
            selectedChildren chc |>
            List.choose(fun ch ->
                match presentWhen relPath ch with
                | PresenceInt   (_, intVal) -> Some (TcvChoiceAlternativePresentWhenInt intVal.Value)
                | PresenceStr   (_, strVal) -> None)
        match updateValues with
        | v1::[]    -> Some v1
        | _         -> None
    | AcnDepPresenceStr (relPath, chc, _) ->
        let updateValues =
            selectedChildren chc |>
            List.choose(fun ch ->
                match presentWhen relPath ch with
                | PresenceInt   (_, intVal) -> None
                | PresenceStr   (_, strVal) -> Some (TcvChoiceAlternativePresentWhenStr strVal.Value))
        match updateValues with
        | v1::[]    -> Some v1
        | _         -> None
    | AcnDepChoiceDeterminant (enm, chc, _) ->
        // The enum item of the selected alternative. Several CHOICEs can share one determinant
        // (directly or through ACN parameters); MultiAcnUpdate accepts the test value only when
        // they all yield the same value, so the alternative must be part of it (TcvAnyValue made
        // any combination look valid, PUS-C TransitionNotification). When the map does not tell
        // one alternative (type ids merged from two values of the same CHOICE type) keep the old value.
        match selectedChildren chc with
        | [ch]  ->
            match enm.enm.items |> List.tryFind (fun itm -> itm.Name.Value = ch.Name.Value) with
            | Some itm  -> Some (TcvEnumeratedValue itm.Name.Value)
            | None      -> atc.testCaseTypeIDsMap.TryFind d.asn1Type
        | _     -> atc.testCaseTypeIDsMap.TryFind d.asn1Type

/// Several dependencies of one determinant. A dependency that yields no value for this test value
/// belongs to a part of the value that is absent (e.g. another alternative of a CHOICE that receives
/// the determinant as an ACN argument). The encoder skips such dependencies at run time
/// (<name>_is_initialized), so the test value is valid when at least one dependency yields a value
/// and all yielded values agree.
let private agreedTestCaseValue (fncs:(AutomaticTestCase -> TestCaseValue option) option list) (atc:AutomaticTestCase) : TestCaseValue option =
    let updateValues = fncs |> List.map(fun z -> match z with None -> None | Some f -> f atc)
    match updateValues |> List.choose id with
    | []        -> None
    | u1::us    ->
        match us |> Seq.exists(fun z -> z <> u1) with
        | true  -> None
        | false -> Some u1

type private DeferredTestValue =
    | NoConsumerValue                   // no consumer of the determinant is in the value
    | AgreedValue of TestCaseValue
    | Conflict                          // two consumers need different values

/// The value of an --acn-v2 deferred determinant for an automatic test value, computed without
/// generating code (such a determinant has no update statement, DAstConstruction). Unlike the
/// legacy update statement, a value without any consumer is valid: the encoder then patches the
/// fallback value. Only consumers that need different values (CHOICEs sharing the determinant
/// that select different alternatives) make the test value invalid.
let deferredDeterminantTestCaseFnc (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (determinantId:ReferenceToType) (atc:AutomaticTestCase) : TestCaseValue option =
    let rec valueOf (detId:ReferenceToType) =
        let single (d:AcnDependency) =
            match d.dependencyKind with
            | AcnDepRefTypeArgument acnPrm -> valueOf acnPrm.id
            | _ ->
                match dependencyTestCaseFnc d atc with
                | Some v    -> AgreedValue v
                | None      -> NoConsumerValue
        let results = deps.acnDependencies |> List.filter(fun d -> d.determinant.id = detId) |> List.map single
        match results |> List.contains Conflict with
        | true  -> Conflict
        | false ->
            match results |> List.choose (fun r -> match r with AgreedValue v -> Some v | _ -> None) with
            | []        -> NoConsumerValue
            | v1::vs    -> if vs |> List.exists ((<>) v1) then Conflict else AgreedValue v1
    match valueOf determinantId with
    | AgreedValue v     -> Some v
    | NoConsumerValue   -> Some TcvAnyValue
    | Conflict          -> None

let rec handleSingleUpdateDependency (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (d:AcnDependency)  (us:State) =
    let ctx = { r=r; deps=deps; lm=lm; m=m; d=d }
    match d.dependencyKind with
    | AcnDepRefTypeArgument acnPrm                              -> handleRefTypeArgument          ctx acnPrm us
    | AcnDepSizeDeterminant (minSize, maxSize, _)               -> handleSizeDeterminant          ctx minSize maxSize us
    | AcnDepSizeDeterminant_bit_oct_str_contain o               -> handleSizeDeterminantContaining ctx o us
    | AcnDepIA5StringSizeDeterminant (minSize, maxSize, _)      -> handleIA5StringSizeDeterminant ctx minSize maxSize us
    | AcnDepPresenceBool                                        -> handlePresenceBool             ctx us
    | AcnDepPresence (relPath, chc)                             -> handlePresenceChoice           ctx relPath chc us
    | AcnDepPresenceStr (relPath, chc, str)                     -> handlePresenceStrChoice        ctx relPath chc str us
    | AcnDepChoiceDeterminant (enm, chc, isOptional)            -> handleChoiceDeterminant        ctx enm chc isOptional us

and private handleRefTypeArgument (ctx: DepContext) (acnPrm: AcnGenericTypes.AcnParameter) (us: State) =
    let prmUpdateStatement, ns1 = getUpdateFunctionUsedInEncoding ctx.r ctx.deps ctx.lm ctx.m acnPrm.id us
    match prmUpdateStatement with
    | None  -> None, ns1
    | Some prmUpdateStatement   ->
        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            prmUpdateStatement.updateAcnChildFnc child nestingScope vTarget pSrcRoot
        let icdComments = icdCommentsForDependency ctx.d
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=prmUpdateStatement.errCodes; testCaseFnc = prmUpdateStatement.testCaseFnc; localVariables=[]}), ns1

and private handleSizeDeterminant (ctx: DepContext) (minSize: SIZE) (maxSize: SIZE) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath = resolveDepScope r lm nestingScope pSrcRoot d.asn1Type
        let pSizeable, checkPath = getAccessFromScopeNodeList relPath false lm pBase
        let unsigned =
            match child.Type with
            | AcnInteger int -> int.isUnsigned
            | AcnNullType _ -> true
            | _ -> raise (BugErrorException "???")
        let updateStatement =
            match minSize.acn = maxSize.acn with
            | true  -> lm.acn.SizeDependencyFixedSize v minSize.acn
            | false -> lm.acn.SizeDependency v (lm.acn.getSizeableSize (pSizeable.accessPath.joined lm.lg) (lm.lg.getAccess pSizeable.accessPath) unsigned) minSize.uper maxSize.uper false false child.typeDefinitionBodyWithinSeq
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc = dependencyTestCaseFnc d
    let icdComments = icdCommentsForDependency ctx.d
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us

and private handleSizeDeterminantContaining (ctx: DepContext) (o: Asn1AcnAst.ReferenceType) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let baseTypeDefinitionName =
        match lm.lg.hasModules with
        | false  -> ToC2(r.args.TypePrefix + o.tasName.Value)
        | true   ->
            match m.Name.Value = o.modName.Value with
            | true  -> ToC2(r.args.TypePrefix + o.tasName.Value)
            | false -> (ToC o.modName.Value) + "." + ToC2(r.args.TypePrefix + o.tasName.Value)
    let baseFncName = baseTypeDefinitionName + "_ACN" + (lm.lg.codecSuffix Encode)
    let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ACN_ENCODING" baseTypeDefinitionName
    let asn1TypeD = us.newTypesMap[d.asn1Type] :?> Asn1Type
    let asn1TypeD = match asn1TypeD.Kind with ReferenceType  o -> o.resolvedType.ActualType | _  -> asn1TypeD
    let errCodes0, localVariables0, ns =
        match asn1TypeD.acnEncFunction with
        | Some f  ->
            let fncBdRes, ns = f.funcBody us [] (NestingScope.init asn1TypeD.acnMaxSizeInBits asn1TypeD.uperMaxSizeInBits []) {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
            match fncBdRes with
            | Some x -> x.errCodes, [], ns
            | None   -> [], [], us
        | None    -> [], [], us

    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let (ReferenceToType depNodes) = d.asn1Type
        let depResolvable =
            nestingScope.parents |> List.exists (fun (_, t) ->
                let (ReferenceToType scopeNodes) = t.id
                let n = List.length scopeNodes
                n >= 2 && n <= List.length depNodes && List.take n depNodes = scopeNodes)
        if not depResolvable then
            // Parent scope unreachable from the current nesting. Only Python tolerates this
            // (types generated standalone); the other backends must fail loudly rather than
            // silently drop the determinant update and emit a wrong-but-compiling encoding.
            if lm.lg.allowUnresolvedAcnDependency || nestingScope.isStandaloneRender then ""
            else raise (BugErrorException "ACN determinant parent is unreachable from the current nesting scope; the determinant update cannot be generated for this backend.")
        else
        let pBase, relPath = resolveDepScope r lm nestingScope pSrcRoot d.asn1Type
        let pSizeable, checkPath = getAccessFromScopeNodeList relPath false lm pBase
        let sInner =
            match asn1TypeD.acnEncFunction with
            | Some f  ->
                let fncBdRes, _ = f.funcBody us [] nestingScope pSizeable
                match fncBdRes with
                | None -> ""
                | Some a -> a.funcBody
            | None -> ""
        let sLocalVarType = child.typeDefinitionBodyWithinSeq
        let updateStatement = lm.acn.SizeDependency_oct_str_containing (lm.lg.getParamValue o.resolvedType pSizeable.accessPath Encode) baseFncName sReqBytesForUperEncoding v (match o.encodingOptions with Some eo -> eo.octOrBitStr = ContainedInOctString | None -> false) sInner sLocalVarType
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc = dependencyTestCaseFnc d
    let localVars = lm.lg.acn.getAcnDepSizeDeterminantLocVars sReqBytesForUperEncoding
    let icdComments = icdCommentsForDependency ctx.d
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=errCodes0; testCaseFnc=testCaseFnc; localVariables= localVariables0@localVars}), ns

and private handleIA5StringSizeDeterminant (ctx: DepContext) (minSize: SIZE) (maxSize: SIZE) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath = resolveDepScope r lm nestingScope pSrcRoot d.asn1Type
        let pSizeable, checkPath = getAccessFromScopeNodeList relPath true lm pBase
        // Range check (Ada only): SPARK needs it to prove the precondition of the
        // determinant encoder. A lower bound of 0 is left out: "len >= 0" is always True
        // for an unsigned determinant (GNAT -gnatwc, an error with -gnatwe), and SPARK
        // knows it from getStringSize returning Natural.
        let updateStatement = lm.acn.SizeDependency v (lm.acn.getStringSize (pSizeable.accessPath.joined lm.lg)) minSize.uper maxSize.uper true (minSize.uper > 0I) child.typeDefinitionBodyWithinSeq
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc = dependencyTestCaseFnc d
    let icdComments = icdCommentsForDependency ctx.d
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us

and private handlePresenceBool (ctx: DepContext) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let parDecTypeSeq =
            match d.asn1Type with
            | ReferenceToType (nodes) -> ReferenceToType (nodes |> List.rev |> List.tail |> List.rev)
        let (ReferenceToType parDecNodes) = parDecTypeSeq
        // Only generate the presence update if the parent sequence is reachable from the current nesting scope.
        // When a type is processed standalone (e.g. TM-HEADER without TM-PACKET as parent), the dep scope
        // cannot be resolved and we must skip the update to avoid accessing a non-existent field.
        let depResolvable =
            nestingScope.parents |> List.exists (fun (_, t) ->
                let (ReferenceToType scopeNodes) = t.id
                let n = List.length scopeNodes
                n >= 2 && n <= List.length parDecNodes && List.take n parDecNodes = scopeNodes)
        if not depResolvable then
            // Parent scope unreachable from the current nesting. Only Python tolerates this
            // (types generated standalone); the other backends must fail loudly rather than
            // silently drop the determinant update and emit a wrong-but-compiling encoding.
            if lm.lg.allowUnresolvedAcnDependency || nestingScope.isStandaloneRender then ""
            else raise (BugErrorException "ACN determinant parent is unreachable from the current nesting scope; the determinant update cannot be generated for this backend.")
        else
            let pBase, relPath = resolveDepScope r lm nestingScope pSrcRoot parDecTypeSeq
            let pDecParSeq, checkPath = getAccessFromScopeNodeList relPath false lm pBase
            let updateStatement = lm.acn.PresenceDependency v (pDecParSeq.accessPath.joined lm.lg) (lm.lg.getAccess pDecParSeq.accessPath) (ToC (withBackendChildNames r lm d.asn1Type).lastItem)
            match checkPath with
            | []    -> updateStatement
            | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc = dependencyTestCaseFnc d
    let icdComments = icdCommentsForDependency ctx.d
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us

and private handlePresenceChoice (ctx: DepContext) (relPath: AcnGenericTypes.RelativePath) (chc: Asn1AcnAst.Choice) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let icdComments = icdCommentsForDependency ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let (ReferenceToType depNodes) = d.asn1Type
        // Skip update when the dep target type is not reachable from the current nesting scope
        // (e.g. the parent type containing the choice field is not an ancestor of this type).
        let depResolvable =
            nestingScope.parents |> List.exists (fun (_, t) ->
                let (ReferenceToType scopeNodes) = t.id
                let n = List.length scopeNodes
                n >= 2 && n <= List.length depNodes && List.take n depNodes = scopeNodes)
        if not depResolvable then
            // Parent scope unreachable from the current nesting. Only Python tolerates this
            // (types generated standalone); the other backends must fail loudly rather than
            // silently drop the determinant update and emit a wrong-but-compiling encoding.
            if lm.lg.allowUnresolvedAcnDependency || nestingScope.isStandaloneRender then ""
            else raise (BugErrorException "ACN determinant parent is unreachable from the current nesting scope; the determinant update cannot be generated for this backend.")
        else
        let pBase, relPath1 = resolveDepScope r lm nestingScope pSrcRoot d.asn1Type
        let choicePath, checkPath = getAccessFromScopeNodeList relPath1 false lm pBase
        let sChoiceTypeName = (lm.lg.getChoiceTypeDefinition chc.typeDef).typeName
        let arrsChildUpdates =
            chc.children |>
            List.map(fun ch ->
                let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch m.Name.Value
                let unsigned =
                    match child.Type with
                    | AcnInteger int -> int.isUnsigned
                    | AcnNullType _ -> true
                    | _ -> raise (BugErrorException "???")
                match pres with
                | PresenceInt   (_, intVal) -> lm.acn.ChoiceDependencyIntPres_child v presentWhenName (lm.lg.asn1SccIntValueToString intVal.Value unsigned) sChoiceTypeName
                | PresenceStr   (_, strVal) -> raise(SemanticError(strVal.Location, "Unexpected presence condition. Expected integer, found string")))
        let updateStatement = lm.acn.ChoiceDependencyPres v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates sChoiceTypeName
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc = dependencyTestCaseFnc d
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us

and private handlePresenceStrChoice (ctx: DepContext) (relPath: AcnGenericTypes.RelativePath) (chc: Asn1AcnAst.Choice) (str: Asn1AcnAst.StringType) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath1 = resolveDepScope r lm nestingScope pSrcRoot d.asn1Type
        let choicePath, checkPath = getAccessFromScopeNodeList relPath1 false lm pBase
        let sChoiceTypeName = (lm.lg.getChoiceTypeDefinition chc.typeDef).typeName
        let sInsertedFieldTypeName =
            match child.Type with
            | AcnReferenceToIA5String s ->
                let ref : CommonTypes.ReferenceToExistingDefinition = {
                    programUnit = Some (ToC s.modName.Value)
                    typedefName = ToC (r.args.TypePrefix + s.tasName.Value)
                    definedInRtl = false
                }
                lm.lg.longTypedefName2 (ReferenceToExistingDefinition ref) lm.lg.hasModules (ToC m.Name.Value)
            | _ -> sChoiceTypeName
        let arrsChildUpdates =
            chc.children |>
            List.map(fun ch ->
                let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch m.Name.Value
                match pres with
                | PresenceInt   (_, intVal) ->
                    raise(SemanticError(intVal.Location, "Unexpected presence condition. Expected string, found integer"))
                | PresenceStr   (_, strVal) ->
                    let arrNulls = [0 .. ((int str.maxSize.acn)- strVal.Value.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
                    let bytesStr =
                        let baseBytes = System.Text.Encoding.ASCII.GetBytes strVal.Value
                        match lm.lg.nullTerminatorByte with
                        | Some nullByte -> Array.append baseBytes [| nullByte |]
                        | None -> baseBytes
                    lm.acn.ChoiceDependencyStrPres_child v presentWhenName strVal.Value bytesStr arrNulls sChoiceTypeName sInsertedFieldTypeName)
        let updateStatement = lm.acn.ChoiceDependencyPres v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates sChoiceTypeName
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc = dependencyTestCaseFnc d
    let icdComments = []
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc = testCaseFnc; localVariables=[]}), us

and private handleChoiceDeterminant (ctx: DepContext) (enm: Asn1AcnAst.ReferenceToEnumerated) (chc: Asn1AcnAst.Choice) (isOptional: bool) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath = resolveDepScope r lm nestingScope pSrcRoot d.asn1Type
        let choicePath, checkPath = getAccessFromScopeNodeList relPath false lm pBase
        let arrsChildUpdates =
            chc.children |>
            List.mapi(fun idx ch ->
                let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = ch.Name.Value)
                let choiceName = (lm.lg.getChoiceTypeDefinition chc.typeDef).typeName //chc.typeDef[Scala].typeName
                lm.acn.ChoiceDependencyEnum_Item v (lm.lg.presentWhenName0 None ch) choiceName (lm.lg.getNamedItemBackendName (Some (defOrRef2 r m enm)) enmItem) (bigint idx) isOptional)
        let updateStatement = lm.acn.ChoiceDependencyEnum v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates isOptional (initExpr r lm m child.Type)
        // TODO: To remove this, getAccessFromScopeNodeList should be accounting for languages that rely on pattern matching for
        // accessing enums fields instead of a compiler-unchecked access
        let updateStatement2 = lm.lg.formatAcnDeterminantUpdate (choicePath.accessPath.joined lm.lg) checkPath updateStatement
        match checkPath with
        | []    -> updateStatement2
        | _     -> lm.acn.checkAccessPath checkPath updateStatement2 v (initExpr r lm m child.Type)
    let testCaseFnc = dependencyTestCaseFnc d
    let icdComments = icdCommentsForDependency ctx.d
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us

and getUpdateFunctionUsedInEncoding (r: Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm: LanguageMacros) (m: Asn1AcnAst.Asn1Module) (acnChildOrAcnParameterId) (us:State) : (AcnChildUpdateResult option*State)=
    let multiAcnUpdate       = lm.acn.MultiAcnUpdate

    match deps.acnDependencies |> List.filter(fun d -> d.determinant.id = acnChildOrAcnParameterId) with
    | []  ->
        None, us
    | d1::[]    ->
        let ret, ns = handleSingleUpdateDependency r deps lm m d1 us
        ret, ns
    | d1::dds         ->
        let _errCodeName = ToC ("ERR_ACN" + ((lm.lg.codecSuffix Encode).ToUpper()) + "_UPDATE_" + ((acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errFieldPath = match acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elm")
        let errCode, us = getNextValidErrorCode us _errCodeName None errFieldPath

        let ds = d1::dds
        let c_name0 = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) 0
        let localVars (child: AcnChild) =
            ds |>
            List.mapi(fun i d1 ->
                let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                let childLv =
                    if lm.lg.decodingKind = Copy then []
                    else [AcnInsertedChild (c_name, child.typeDefinitionBodyWithinSeq, child.initExpression)]
                let initLv =
                    if lm.lg.usesWrappedOptional then []
                    else [BooleanLocalVariable (c_name+"_is_initialized", Some lm.lg.FalseLiteral)]
                childLv@initLv) |>
            List.collect(fun lvList -> lvList |> List.map (fun lv -> lm.lg.getLocalVariableDeclaration  lv))
        let localUpdateFuns,ns =
            ds |>
            List.fold(fun (updates, ns) d1 ->
                let f1, nns = handleSingleUpdateDependency r deps lm m d1 ns
                updates@[f1], nns) ([],us)
        let restErrCodes = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.errCodes)
        let restLocalVariables = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.localVariables)
        let icdComments = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.icdComments)
        let multiUpdateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let arrsLocalUpdateStatements =
                localUpdateFuns |>
                List.mapi(fun i fn ->
                    let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let lv = {CodegenScope.modName = vTarget.modName; accessPath = AccessPath.valueEmptyPath c_name}
                    match fn with
                    | None      -> None
                    | Some fn   -> Some(fn.updateAcnChildFnc child nestingScope lv pSrcRoot)) |>
                List.choose id

            let isAlwaysInit (d: AcnDependency): bool =
                match d.dependencyKind with
                | AcnDepRefTypeArgument p ->
                    // Last item is the determinant, and the second-to-last is the field referencing the determinant
                    not p.id.dropLast.lastItemIsOptional
                | AcnDepChoiceDeterminant (_, c, isOpt) -> not isOpt
                | _ -> true

            let firstAlwaysInit = ds |> List.tryFind isAlwaysInit
            let arrsGetFirstIntValue =
                let ds2 =
                    match firstAlwaysInit with
                    | Some fst when lm.lg.usesWrappedOptional -> [fst]
                    | _ -> ds
                ds2 |>
                List.mapi (fun i d ->
                    let cmp = getDeterminantTypeUpdateMacro lm d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let pBase, relPath = resolveDepScope r lm nestingScope pSrcRoot d.asn1Type
                    let choicePath, _ = getAccessFromScopeNodeList relPath d.dependencyKind.isString lm pBase
                    cmp v vi (choicePath.accessPath.joined lm.lg) (i=0) (ds2.Length = 1))
            let arrsLocalCheckEquality =
                ds |>
                List.mapi (fun i d ->
                    let cmp = getDeterminantTypeCheckEqual lm d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let pBase, relPath = resolveDepScope r lm nestingScope pSrcRoot d.asn1Type
                    let choicePath, _ = getAccessFromScopeNodeList relPath d.dependencyKind.isString lm pBase
                    cmp v vi (choicePath.accessPath.joined lm.lg) (isAlwaysInit d))
            let updateStatement = multiAcnUpdate (vTarget.accessPath.joined lm.lg) c_name0 (errCode.errCodeName) (localVars child) arrsLocalUpdateStatements arrsGetFirstIntValue firstAlwaysInit.IsSome arrsLocalCheckEquality (initExpr r lm m child.Type)
            updateStatement
        let testCaseFnc = agreedTestCaseValue (localUpdateFuns |> List.map (Option.map (fun res -> res.testCaseFnc)))

        let ret = Some(({AcnChildUpdateResult.updateAcnChildFnc = multiUpdateFunc; icdComments=icdComments; errCodes=errCode::restErrCodes ; testCaseFnc = testCaseFnc; localVariables = restLocalVariables}))
        ret, ns
