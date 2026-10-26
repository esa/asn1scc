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

//The largest value the decoder can return for the size determinant of asn1TypeIdWithDependency,
//or None when its encoding does not bound it. The bound comes from the encoding width, not from
//the ASN.1 constraint of the determinant (uperRange): the C and Rust fixed-size integer decoders
//return every value of the width (ESACERT #74995). When the determinant is an ACN parameter,
//every argument passed to it must be bounded.
let getSizeDeterminantWireMax (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (asn1TypeIdWithDependency: ReferenceToType) : System.Numerics.BigInteger option =
    let unsignedMax (nBits:System.Numerics.BigInteger) = Some (System.Numerics.BigInteger.Pow(2I, int nBits) - 1I)
    let signedMax (nBits:System.Numerics.BigInteger) = Some (System.Numerics.BigInteger.Pow(2I, int nBits - 1) - 1I)
    let encodingMax (int: Asn1AcnAst.AcnInteger) =
        match int.acnProperties.mappingFunction with
        | Some _ -> None
        | None   ->
            match int.acnEncodingClass with
            | PositiveInteger_ConstSize_8                  -> unsignedMax 8I
            | PositiveInteger_ConstSize_big_endian_16      -> unsignedMax 16I
            | PositiveInteger_ConstSize_little_endian_16   -> unsignedMax 16I
            | PositiveInteger_ConstSize_big_endian_32      -> unsignedMax 32I
            | PositiveInteger_ConstSize_little_endian_32   -> unsignedMax 32I
            | PositiveInteger_ConstSize_big_endian_64      -> unsignedMax 64I
            | PositiveInteger_ConstSize_little_endian_64   -> unsignedMax 64I
            | PositiveInteger_ConstSize nBits              -> unsignedMax nBits
            | TwosComplement_ConstSize_8                   -> signedMax 8I
            | TwosComplement_ConstSize_big_endian_16       -> signedMax 16I
            | TwosComplement_ConstSize_little_endian_16    -> signedMax 16I
            | TwosComplement_ConstSize_big_endian_32       -> signedMax 32I
            | TwosComplement_ConstSize_little_endian_32    -> signedMax 32I
            | TwosComplement_ConstSize_big_endian_64       -> signedMax 64I
            | TwosComplement_ConstSize_little_endian_64    -> signedMax 64I
            | TwosComplement_ConstSize nBits               -> signedMax nBits
            | Integer_uPER
            | ASCII_ConstSize _
            | ASCII_VarSize_NullTerminated _
            | ASCII_UINT_ConstSize _
            | ASCII_UINT_VarSize_NullTerminated _
            | BCD_ConstSize _
            | BCD_VarSize_NullTerminated _                 -> None
    let determinantMax (det: Determinant) =
        match det with
        | AcnChildDeterminant child ->
            match child.Type with
            | AcnInsertedType.AcnInteger int              -> encodingMax int
            | AcnInsertedType.AcnNullType _
            | AcnInsertedType.AcnBoolean _
            | AcnInsertedType.AcnReferenceToEnumerated _
            | AcnInsertedType.AcnReferenceToIA5String _   -> None
        | AcnParameterDeterminant _ -> None
    let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = asn1TypeIdWithDependency)
    let nodes = match dependency.determinant.id with ReferenceToType nodes -> nodes
    let candidates =
        match nodes |> List.last with
        | PRM _ ->
            let args =
                deps.acnDependencies |>
                List.choose(fun d ->
                    match d.dependencyKind with
                    | AcnDepRefTypeArgument prm when prm.id = dependency.determinant.id -> Some d.determinant
                    | _ -> None)
            match args with
            | [] -> [dependency.determinant]
            | _  -> args
        | _ -> [dependency.determinant]
    candidates |> List.fold (fun acc det ->
        match acc, determinantMax det with
        | Some m1, Some m2 -> Some (max m1 m2)
        | _                -> None) (determinantMax candidates.Head)
