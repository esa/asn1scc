module AcnExternalField

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language


/// Name of the AcnInsertedFieldRef local variable called `name` in the caller:
/// ToC name, with the suffix "_ref" when that would clash with a keyword of the
/// target language or, in a case-insensitive language (Ada), with a module
/// (package) or type name, which the local would hide (e.g. "type", or
/// "parameterReportStructureType" hiding package ParameterReportStructureType).
let deferredDetLocalName (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (name: string) : string =
    let cName = ToC name
    let same (a: string) (b: string) =
        if lm.lg.isCaseSensitive then a = b else a.ToUpperInvariant() = b.ToUpperInvariant()
    let isKeyword = lm.lg.Keywords |> Seq.exists (same cName)
    let hidesModuleOrType =
        not lm.lg.isCaseSensitive
        && (r.modulesMap |> Map.exists (fun modName _ -> same cName (ToC modName))
            || r.typeAssignmentsMap |> Map.exists (fun (_, tasName) _ -> same cName (ToC (r.args.TypePrefix + tasName))))
    if isKeyword || hidesModuleOrType then cName + "_ref" else cName


/// The RefTypeArgument link of each ACN parameter (parameter id -> the determinant
/// passed to it), built once per dependency list.
let private paramLinksCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, Determinant>>()

let private paramLinks (deps: AcnInsertedFieldDependencies) : Map<ReferenceToType, Determinant> =
    paramLinksCache.GetValue(deps, fun deps ->
        deps.acnDependencies
        |> List.choose (fun d ->
            match d.dependencyKind with
            | AcnDepRefTypeArgument prm -> Some (prm.id, d.determinant)
            | _ -> None)
        |> List.rev
        |> Map.ofList)

/// --acn-v2: the ACN field an AcnInsertedFieldRef stands for (q/issues.md Issue 52).
/// Every AcnInsertedFieldRef, a formal parameter or a local, holds one ACN field;
/// the backend matches them by this id, never by name (two fields with the same
/// name shared one local). A parameter added by closure conversion stands for the
/// field it was created for (AstRoot.closureParamDeterminants); any other parameter
/// for what its RefTypeArgument link passes, followed up to the field. A parameter
/// without either (a parameter of the outermost function) stands for itself.
let rec private paramIdIdentity (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (prmId: ReferenceToType) : ReferenceToType =
    match r.closureParamDeterminants.TryFind prmId with
    | Some fieldId -> fieldId
    | None ->
        match (paramLinks deps).TryFind prmId with
        | Some (AcnChildDeterminant ac)      -> ac.id
        | Some (AcnParameterDeterminant prm) -> paramIdIdentity r deps prm.id
        | None                               -> prmId

let determinantIdentity (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (det: Determinant) : ReferenceToType =
    match det with
    | AcnChildDeterminant ac      -> ac.id
    | AcnParameterDeterminant prm -> paramIdIdentity r deps prm.id

/// determinantIdentity of an ACN parameter.
let paramIdentity (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (prm: AcnParameter) : ReferenceToType =
    determinantIdentity r deps (AcnParameterDeterminant prm)

/// The ACN parameters (ids) that stand for each ACN field (determinantIdentity), built once per dependency list.
let private fieldParamsCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, ReferenceToType list>>()

let private fieldParams (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) : Map<ReferenceToType, ReferenceToType list> =
    fieldParamsCache.GetValue(deps, fun deps ->
        Seq.append (paramLinks deps |> Map.keys) (r.closureParamDeterminants |> Map.keys)
        |> Seq.distinct
        |> Seq.map (fun prmId -> paramIdIdentity r deps prmId, prmId)
        |> Seq.filter (fun (identity, prmId) -> identity <> prmId)
        |> Seq.toList
        |> List.groupBy fst
        |> List.map (fun (identity, xs) -> identity, xs |> List.map snd)
        |> Map.ofList)

/// --acn-v2: whether the ACN field fieldId is held in an AcnInsertedFieldRef (InitDet / PatchDet): some ACN
/// parameter stands for it (determinantIdentity), i.e. it is passed to a referenced type or exposed by its own.
let isDeferredField (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (fieldId: ReferenceToType) : bool =
    (fieldParams r deps).ContainsKey fieldId

/// --acn-v2: the formal parameter (id) that holds the ACN field fieldId in the function that encodes it: the
/// parameter standing for the field of the innermost reference boundary enclosing it, when the field is exposed to
/// an enclosing type (closure conversion adds such a parameter at every boundary on the way out).
let fieldOwnParam (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (fieldId: ReferenceToType) : ReferenceToType option =
    let fieldPath = fieldId.ToScopeNodeList
    (fieldParams r deps).TryFind fieldId
    |> Option.defaultValue []
    |> List.choose (fun prmId ->
        let boundary = prmId.ToScopeNodeList |> List.take (prmId.ToScopeNodeList.Length - 1)
        match boundary.Length < fieldPath.Length && List.take boundary.Length fieldPath = boundary with
        | true  -> Some (boundary.Length, prmId)
        | false -> None)
    |> List.sortByDescending fst
    |> List.tryHead
    |> Option.map snd

/// The consumers of each ACN field (determinantIdentity): the dependencies that patch it (every kind but the
/// RefTypeArgument links that pass it on), built once per dependency list.
let private fieldConsumersCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, AcnDependency list>>()

let private fieldConsumers (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) : Map<ReferenceToType, AcnDependency list> =
    fieldConsumersCache.GetValue(deps, fun deps ->
        deps.acnDependencies
        |> List.filter (fun d -> match d.dependencyKind with AcnDepRefTypeArgument _ -> false | _ -> true)
        |> List.groupBy (fun d -> determinantIdentity r deps d.determinant)
        |> Map.ofList)

/// --acn-v2: the consumers of the ACN field fieldId (the dependencies that patch it).
let consumersOfField (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (fieldId: ReferenceToType) : AcnDependency list =
    (fieldConsumers r deps).TryFind fieldId |> Option.defaultValue []

/// --acn-v2, coverage step 13b: whether at most one PatchDet of the ACN field fieldId runs per InitDet, so none can
/// fail (Acn_PatchDet_* fails only on a ref already set with another value). True when the field has one consumer,
/// or consumers in different alternatives of one CHOICE (only one is encoded), and no consumer is under a SEQUENCE
/// OF that does not also hold the field (one patch per element).
let isOnlyPatchOfField (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (fieldId: ReferenceToType) : bool =
    let fieldPath = fieldId.ToScopeNodeList
    let commonPrefix (a: ScopeNode list) (b: ScopeNode list) =
        List.zip (List.truncate b.Length a) (List.truncate a.Length b) |> List.takeWhile (fun (x, y) -> x = y) |> List.length
    let belowSequenceOf (path: ScopeNode list) =
        path |> List.skip (commonPrefix fieldPath path) |> List.exists (function SQF -> true | _ -> false)
    let exclusive (a: ScopeNode list) (b: ScopeNode list) =
        let n = commonPrefix a b
        match List.tryItem n a, List.tryItem n b with
        | Some (CH_CHILD _), Some (CH_CHILD _) -> true
        | _ -> false
    match (fieldConsumers r deps).TryFind fieldId |> Option.defaultValue [] |> List.map (fun d -> d.asn1Type.ToScopeNodeList) with
    | [] -> false
    | paths ->
        not (paths |> List.exists belowSequenceOf)
        && (List.allPairs (List.indexed paths) (List.indexed paths)
            |> List.forall (fun ((i, a), (j, b)) -> i = j || exclusive a b))

/// --acn-v2, coverage step 14e (E6-C, q/issues.md Issue 79): whether every consumer of the ACN field fieldId writes one
/// and the same value, known at compile time: a size with SIZE min = max (not a CONTAINING length, which is measured),
/// a presence (present-when) or CHOICE determinant whose alternatives all write the same value (e.g. a CHOICE with one
/// alternative, an ENUMERATED field with one item). Then no PatchDet of the field can fail: Acn_PatchDet_* fails only on
/// a ref already set with another value, and the fallback patches only a ref that is not set. Any consumer whose value
/// the compiler does not know (a variable size, a presence flag, a measured length, a string length) keeps the checks.
let consumersWriteOneValue (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (fieldId: ReferenceToType) : bool =
    let presentWhenValues (relPath: RelativePath) (chc: Asn1AcnAst.Choice) =
        chc.children |> List.map (fun ch ->
            match ch.acnPresentWhenConditions |> Seq.find (fun x -> x.relativePath = relPath) with
            | PresenceInt (_, intVal) -> intVal.Value.ToString()
            | PresenceStr (_, strVal) -> "\"" + strVal.Value + "\"")
    let valuesOf (d: AcnDependency) : string list option =
        match d.dependencyKind with
        | AcnDepSizeDeterminant (minSize, maxSize, _) when minSize.acn = maxSize.acn -> Some [minSize.acn.ToString()]
        | AcnDepSizeDeterminant _                         -> None
        | AcnDepPresence (relPath, chc)                   -> Some (presentWhenValues relPath chc)
        | AcnDepPresenceStr (relPath, chc, _)             -> Some (presentWhenValues relPath chc)
        | AcnDepChoiceDeterminant (enm, chc, _) ->
            let items = AcnHelpers.determinantEnumItems deps d.determinant enm.enm
            chc.children |> List.map (fun ch ->
                items |> List.tryFind (fun itm -> itm.Name.Value = ch.Name.Value) |> Option.map (fun itm -> itm.acnEncodeValue.ToString()))
            |> fun vs -> match vs |> List.forall Option.isSome with
                         | true  -> Some (vs |> List.choose id)
                         | false -> None
        | AcnDepSizeDeterminant_bit_oct_str_contain _     -> None
        | AcnDepIA5StringSizeDeterminant _                -> None
        | AcnDepPresenceBool                              -> None
        | AcnDepRefTypeArgument _                         -> None
    match (fieldConsumers r deps).TryFind fieldId |> Option.defaultValue [] |> List.map valuesOf with
    | [] -> false
    | values ->
        match values |> List.forall Option.isSome with
        | true  -> (values |> List.choose id |> List.concat |> List.distinct).Length = 1
        | false -> false

/// The nodes of a type assignment in encoding order (preorder, ACN children included, references not entered).
let private encodingOrderCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<Asn1AcnAst.AstRoot, System.Collections.Generic.Dictionary<ReferenceToType, Map<ReferenceToType, int>>>()

let private encodingOrder (r: Asn1AcnAst.AstRoot) (tasId: ReferenceToType) : Map<ReferenceToType, int> =
    let perTas = encodingOrderCache.GetValue(r, fun _ -> System.Collections.Generic.Dictionary<ReferenceToType, Map<ReferenceToType, int>>())
    match perTas.TryGetValue tasId with
    | true, order -> order
    | false, _ ->
        let rec walk (t: Asn1AcnAst.Asn1Type) (acc: ReferenceToType list) =
            let acc = t.id :: acc
            match t.Kind with
            | Asn1AcnAst.Sequence sq ->
                sq.children |> List.fold (fun acc (c: Asn1AcnAst.SeqChildInfo) ->
                    match c with
                    | Asn1AcnAst.Asn1Child ch -> walk ch.Type acc
                    | Asn1AcnAst.AcnChild ac  -> ac.id :: acc) acc
            | Asn1AcnAst.Choice ch      -> ch.children |> List.fold (fun acc (c: Asn1AcnAst.ChChildInfo) -> walk c.Type acc) acc
            | Asn1AcnAst.SequenceOf sqf -> walk sqf.child acc
            | Asn1AcnAst.Integer _ | Asn1AcnAst.Real _ | Asn1AcnAst.IA5String _ | Asn1AcnAst.NumericString _
            | Asn1AcnAst.OctetString _ | Asn1AcnAst.NullType _ | Asn1AcnAst.BitString _ | Asn1AcnAst.Boolean _
            | Asn1AcnAst.Enumerated _ | Asn1AcnAst.ObjectIdentifier _ | Asn1AcnAst.TimeType _ | Asn1AcnAst.ReferenceType _ -> acc
        let order =
            r.Files |> List.collect (fun f -> f.Modules) |> List.collect (fun m -> m.TypeAssignments) |>
            List.tryFind (fun ts -> ts.Type.id = tasId) |>
            Option.map (fun ts -> walk ts.Type [] |> List.rev |> List.mapi (fun i id -> id, i) |> Map.ofList) |>
            Option.defaultValue Map.empty
        perTas.[tasId] <- order
        order

/// --acn-v2, coverage step 14d: whether the PatchDet at the reference site siteId (of its parameter paramId, which
/// stands for the ACN field fieldId) is the first patch of the field after its InitDet, so it cannot fail. True when
/// the site receives the field directly as an argument, its type does not pass the parameter on (no other patch of the
/// field inside the site), the site is not under a SEQUENCE OF relative to the field, and every other site that
/// receives the field comes later in encoding order. The consumers that are not reference sites patch in the epilogue
/// of a SEQUENCE that holds the site, after its children. A field exposed by a referenced child (no site with the field
/// as argument in its tree) keeps the check.
let isFirstPatchOfField (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (fieldId: ReferenceToType) (siteId: ReferenceToType) (paramId: ReferenceToType) : bool =
    let sites =
        deps.acnDependencies |> List.choose (fun d ->
            match d.dependencyKind, d.determinant with
            | AcnDepRefTypeArgument _, AcnChildDeterminant ac when ac.id = fieldId -> Some d.asn1Type
            | _ -> None)
    let passedOn =
        deps.acnDependencies |> List.exists (fun d ->
            match d.dependencyKind with
            | AcnDepRefTypeArgument _ -> d.determinant.id = paramId
            | _ -> false)
    let fieldPath = fieldId.ToScopeNodeList
    let sitePath = siteId.ToScopeNodeList
    let belowSequenceOf =
        let common = List.zip (List.truncate sitePath.Length fieldPath) (List.truncate fieldPath.Length sitePath) |> List.takeWhile (fun (x, y) -> x = y) |> List.length
        sitePath |> List.skip common |> List.exists (function SQF -> true | _ -> false)
    match sites |> List.contains siteId, passedOn, belowSequenceOf, fieldPath with
    | true, false, false, md :: ta :: _ ->
        let order = encodingOrder r (ReferenceToType [md; ta])
        match order.TryFind siteId with
        | None   -> false
        | Some k ->
            sites |> List.forall (fun s ->
                s = siteId ||
                (match order.TryFind s with
                 | Some ks -> ks > k
                 | None    -> false))
    | _ -> false

/// --acn-v2: name of the AcnInsertedFieldRef local that holds the ACN field `identity`
/// (determinantIdentity) in the function that declares it: the field's path from its
/// type assignment (`detail`, `header_detail`), unique in every function by
/// construction (q/issues.md Issue 52). A parameter of the outermost function keeps
/// its parameter name.
let deferredDetRefName (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (identity: ReferenceToType) : string =
    match identity.ToScopeNodeList |> List.tryLast with
    | Some (PRM _) -> AcnHelpers.getAcnDeterminantName identity
    | _ -> deferredDetLocalName r lm ((identity.AcnAbsPath |> List.skip 2 |> String.concat "_").Replace("#", "elm"))


let getExternalField0 (lm:LanguageMacros) (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency func1 (stopAtPrm: bool) =
    let dependency =
        match deps.acnDependencies |> List.tryFind (fun d -> d.asn1Type = asn1TypeIdWithDependency && func1 d ) with
        | Some d -> d
        | None   ->
            failwithf "getExternalField0: No dependency found for %A" asn1TypeIdWithDependency

    let rec resolveParam (prmId:ReferenceToType) =
        let nodes = match prmId with ReferenceToType nodes -> nodes
        let lastNode = nodes |> List.rev |> List.head
        match lastNode with
        | PRM prmName   ->
            if r.args.acnDeferred || stopAtPrm then
                // In deferred mode, the parameter IS the value — it arrives
                // as an AcnInsertedFieldRef* formal parameter.  Do NOT follow
                // RefTypeArgumentDependency chains; stop here.
                // When stopAtPrm=true (e.g. Python CEC_presWhen), the generated
                // code is a standalone method: use the formal parameter name, not
                // the caller's variable name.
                prmId
            else
                let newDeterminantId =
                    deps.acnDependencies |>
                    List.choose(fun d ->
                        match d.dependencyKind with
                        | AcnDepRefTypeArgument prm when prm.id = prmId -> Some d.determinant
                        | _                                             -> None)
                match newDeterminantId with
                | det1::_   -> resolveParam det1.id
                | _         -> prmId
        | _             -> prmId

    let resolvedId = resolveParam dependency.determinant.id
    // In deferred mode, a PRM determinant MUST resolve to a PRM node
    // (the parameter itself).  If it resolves to something else, it means
    // the dep rewrite is wrong or a RefTypeArgumentDependency was followed
    // when it shouldn't have been.
    if r.args.acnDeferred then
        match dependency.determinant with
        | Asn1AcnAst.AcnParameterDeterminant _ ->
            let resolvedNodes = match resolvedId with ReferenceToType nodes -> nodes
            let resolvedLastNode = resolvedNodes |> List.rev |> List.head
            match resolvedLastNode with
            | PRM _ -> ()  // correct — resolved to a parameter
            | _ -> failwithf "BUG: In deferred mode, parameter determinant %A resolved to non-PRM node %A" dependency.determinant.id resolvedId
        | _ -> ()  // AcnChildDeterminant — resolves to the ACN child, which is fine

    let baseName = AcnHelpers.getAcnDeterminantName resolvedId
    let isExposedProducer =
        r.args.acnDeferred
        && (match dependency.determinant with
            | AcnParameterDeterminant prm ->
                let (ReferenceToType paramNodes) = prm.id
                let boundaryPath = paramNodes |> List.rev |> List.tail |> List.rev
                let dependentPath = dependency.asn1Type.ToScopeNodeList
                not (boundaryPath.Length <= dependentPath.Length
                     && List.take boundaryPath.Length dependentPath = boundaryPath)
            | AcnChildDeterminant _ -> false)
    // In deferred mode, when the determinant resolved to a PRM (parameter),
    // the formal parameter is the language-specific deferred ref struct.
    // Use the abstract macros to access the value/str_value fields so the
    // syntax is correct for each backend (C: "det->value", Ada: "det.Value").
    if r.args.acnDeferred then
        let resolvedNodes = match resolvedId with ReferenceToType nodes -> nodes
        let resolvedLastNode = resolvedNodes |> List.rev |> List.head
        match resolvedLastNode, isExposedProducer with
        | _, true ->
            // the caller's local of the exposed field (DAstACNDeferred.buildCallerWrapper)
            let localName = deferredDetRefName r lm (determinantIdentity r deps dependency.determinant)
            let pointer = lm.lg.getPointer (AccessPath.valueEmptyPath localName)
            match dependency.dependencyKind with
            | Asn1AcnAst.AcnDepPresenceStr _ -> lm.acn.acn_deferred_det_access_str_ptr pointer
            | Asn1AcnAst.AcnDepPresenceBool  -> lm.acn.acn_deferred_det_access_bool_ptr pointer
            | _                              -> lm.acn.acn_deferred_det_access_ptr pointer
        | PRM _, false ->
            // Pick the access form by dependency kind.  String determinants
            // need the Str_Value field; Boolean determinants need a Boolean
            // expression (Ada is strictly typed and rejects Asn1UInt-as-Boolean
            // — see PresenceWhenBool decode in AcnSequence.fs).
            match dependency.dependencyKind with
            | Asn1AcnAst.AcnDepPresenceStr _ -> lm.acn.acn_deferred_det_access_str_ptr baseName
            | Asn1AcnAst.AcnDepPresenceBool  -> lm.acn.acn_deferred_det_access_bool_ptr baseName
            | _                              -> lm.acn.acn_deferred_det_access_ptr baseName
        | _, false -> baseName
    else
        baseName

let getExternalField0Type (r: Asn1AcnAst.AstRoot)
                          (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
                          (asn1TypeIdWithDependency: ReferenceToType)
                          (filter: AcnDependency -> bool) : AcnInsertedType option =
    let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = asn1TypeIdWithDependency && filter d)
    let nodes = match dependency.determinant.id with ReferenceToType nodes -> nodes
    let lastNode = nodes |> List.rev |> List.head
    match lastNode with
    | PRM _   ->
        let tp =
            deps.acnDependencies |>
            List.choose(fun d ->
                match d.dependencyKind with
                | AcnDepRefTypeArgument prm when prm.id = dependency.determinant.id ->
                    match d.determinant with
                    | AcnChildDeterminant child -> Some child.Type
                    | _ -> None
                | _ -> None)
        match tp with
        | tp :: _ -> Some tp
        | _ ->
            match dependency.determinant with
            | AcnChildDeterminant child -> Some child.Type
            | _ -> None
    | _ ->
        match dependency.determinant with
        | AcnChildDeterminant child -> Some child.Type
        | _ -> None

let getExternalFieldChoicePresentWhen (lm:LanguageMacros) (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency  relPath=
    let filterDependency (d:AcnDependency) =
        match d.dependencyKind with
        | AcnDepPresence (relPath0, _)   -> relPath = relPath0
        | _                              -> true
    // For Python, CHOICE present-when conditions live in standalone decode methods;
    // use the formal parameter name rather than chasing to the caller's variable.
    let stopAtPrm = lm.lg.stopAtPrmForChoicePresentWhen
    getExternalField0 lm r deps asn1TypeIdWithDependency filterDependency stopAtPrm

let getExternalFieldTypeChoicePresentWhen (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency  relPath=
    let filterDependency (d:AcnDependency) =
        match d.dependencyKind with
        | AcnDepPresence (relPath0, _)   -> relPath = relPath0
        | _                              -> true
    getExternalField0Type r deps asn1TypeIdWithDependency filterDependency

let getExternalField (lm:LanguageMacros) (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
    getExternalField0 lm r deps asn1TypeIdWithDependency (fun z -> true) false

let getExternalFieldType (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
    getExternalField0Type r deps asn1TypeIdWithDependency (fun z -> true)

// Moved to SizeDeterminantBounds.fs (coverage step 14c2b: DecodeOutcome needs them, it is compiled first)
let extFieldMayBeNegative = SizeDeterminantBounds.extFieldMayBeNegative
let extFieldSizeMin = SizeDeterminantBounds.extFieldSizeMin
let getSizeDeterminantWireMax = SizeDeterminantBounds.getSizeDeterminantWireMax
