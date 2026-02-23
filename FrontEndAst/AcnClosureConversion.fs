/// Closure conversion for ACN cross-scope references.
/// Transforms implicit cross-scope ACN determinant references into explicit
/// acnParameters / acnArguments on ReferenceType boundaries.
/// Also rewrites AcnInsertedFieldDependencies so that resolveParam stops
/// at the new parameter level instead of following the chain to the original
/// ACN child.
/// After this transformation, the backend sees a single case:
/// acnParameters.Length > 0 → generate specialized function.
module AcnClosureConversion

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst


/// Check if 'prefix' is a prefix of 'full' (ScopeNode list comparison)
let private isPathPrefix (prefix: ScopeNode list) (full: ScopeNode list) : bool =
    let prefixLen = List.length prefix
    if prefixLen > List.length full then false
    else (List.take prefixLen full) = prefix


/// For a ReferenceType boundary at 'boundaryPath', find all AcnChild determinants
/// that are referenced by dependencies INSIDE the boundary but live OUTSIDE it.
/// These are the "consumer" cross-scope references: the type inside needs the
/// determinant value (e.g., for size, presence).  In the deferred model the
/// specialized function will call PatchDet on the received AcnInsertedFieldRef*.
let private findConsumerDeterminants (boundaryPath: ScopeNode list) (deps: AcnInsertedFieldDependencies) : AcnChild list =
    deps.acnDependencies
    |> List.choose (fun dep ->
        let depTypePath = dep.asn1Type.ToScopeNodeList
        let detPath = dep.determinant.id.ToScopeNodeList
        // The dependent type must be inside the boundary
        // AND the determinant must be outside the boundary
        // AND the determinant must be an ACN inserted child (not already a parameter)
        if isPathPrefix boundaryPath depTypePath
           && not (isPathPrefix boundaryPath detPath) then
            match dep.determinant with
            | AcnChildDeterminant acnChild -> Some acnChild
            | AcnParameterDeterminant _ -> None
        else
            None)
    |> List.distinctBy (fun ac -> ac.id)


/// For a ReferenceType boundary at 'boundaryPath', find all AcnChild determinants
/// that are DEFINED inside the boundary but USED by types OUTSIDE it.
/// These are the "producer" cross-scope references: the type inside contains
/// the determinant (e.g., a length field in a header) and needs to receive an
/// AcnInsertedFieldRef* so it can call InitDet instead of normal encoding.
let private findProducerDeterminants (boundaryPath: ScopeNode list) (deps: AcnInsertedFieldDependencies) : AcnChild list =
    deps.acnDependencies
    |> List.choose (fun dep ->
        let depTypePath = dep.asn1Type.ToScopeNodeList
        let detPath = dep.determinant.id.ToScopeNodeList
        // The determinant must be inside the boundary
        // AND the dependent type must be outside the boundary
        if isPathPrefix boundaryPath detPath
           && not (isPathPrefix boundaryPath depTypePath) then
            match dep.determinant with
            | AcnChildDeterminant acnChild -> Some acnChild
            | AcnParameterDeterminant _ -> None
        else
            None)
    |> List.distinctBy (fun ac -> ac.id)


/// Map AcnInsertedType to AcnParamType for parameter creation
let private mapInsertedTypeToParamType (insType: AcnInsertedType) (loc: SrcLoc) : AcnParamType =
    match insType with
    | AcnInsertedType.AcnInteger _ -> AcnPrmInteger loc
    | AcnInsertedType.AcnBoolean _ -> AcnPrmBoolean loc
    | AcnInsertedType.AcnNullType _ -> AcnPrmNullType loc
    | AcnInsertedType.AcnReferenceToEnumerated r ->
        AcnPrmRefType (r.modName, r.tasName)
    | AcnInsertedType.AcnReferenceToIA5String _ ->
        // IA5String determinants are unlikely but fallback to integer
        AcnPrmInteger loc


/// Create an AcnParameter for a cross-scope determinant
let private createParam (resolvedType: Asn1Type) (det: AcnChild) : AcnParameter =
    let (ReferenceToType rPath) = resolvedType.id
    {
        AcnParameter.name = det.Name.Value
        asn1Type = mapInsertedTypeToParamType det.Type det.Name.Location
        loc = det.Name.Location
        id = ReferenceToType (rPath @ [PRM det.Name.Value])
    }


/// A dep rewrite instruction collected during the recursive AST walk.
/// Each entry describes one consumer determinant at one boundary.
type private DepRewrite = {
    /// The ReferenceType boundary (e.g., MyModule.PDU.payload)
    boundaryTypeId : ReferenceToType
    /// The original ACN child determinant outside the boundary
    originalDet    : AcnChild
    /// The new AcnParameter created at this boundary
    newParam       : AcnParameter
    /// The boundary path (ScopeNode list) — deps whose asn1Type is inside
    /// this path and whose determinant matches originalDet will be rewritten.
    boundaryPath   : ScopeNode list
}


/// Recursively transform an Asn1Type tree, adding acnParameters and acnArguments
/// at each ReferenceType boundary that has cross-scope dependencies.
/// Processing is bottom-up: inner boundaries are transformed first, then outer ones.
/// Returns the transformed type and a list of dep rewrites to apply later.
let rec private transformType (deps: AcnInsertedFieldDependencies) (t: Asn1Type) : Asn1Type * DepRewrite list =
    // Step 1: Recurse into children (bottom-up)
    let t', childRewrites =
        match t.Kind with
        | Asn1TypeKind.Sequence sq ->
            let children', rewrites =
                sq.children |> List.map (fun c ->
                    match c with
                    | SeqChildInfo.Asn1Child ac ->
                        let t2, rw = transformType deps ac.Type
                        SeqChildInfo.Asn1Child { ac with Type = t2 }, rw
                    | SeqChildInfo.AcnChild _ -> c, [])
                |> List.unzip
            { t with Kind = Asn1TypeKind.Sequence { sq with children = children' } },
            rewrites |> List.concat

        | Asn1TypeKind.Choice ch ->
            let children', rewrites =
                ch.children |> List.map (fun c ->
                    let t2, rw = transformType deps c.Type
                    { c with Type = t2 }, rw)
                |> List.unzip
            { t with Kind = Asn1TypeKind.Choice { ch with children = children' } },
            rewrites |> List.concat

        | Asn1TypeKind.SequenceOf sqf ->
            let child', rw = transformType deps sqf.child
            { t with Kind = Asn1TypeKind.SequenceOf { sqf with child = child' } }, rw

        | Asn1TypeKind.ReferenceType rt ->
            // Recurse into the resolvedType
            let resolvedType', rw = transformType deps rt.resolvedType
            { t with Kind = Asn1TypeKind.ReferenceType { rt with resolvedType = resolvedType' } }, rw

        | _ -> t, []

    // Step 2: If this is a ReferenceType, apply boundary logic
    match t'.Kind with
    | Asn1TypeKind.ReferenceType rt ->
        let boundaryPath = t'.id.ToScopeNodeList
        // Consumer: determinant OUTSIDE, dependent type INSIDE (e.g., payload uses hdr.buffers-length)
        let consumerDets = findConsumerDeterminants boundaryPath deps
        // Producer: determinant INSIDE, dependent type OUTSIDE (e.g., hdr contains buffers-length)
        let producerDets = findProducerDeterminants boundaryPath deps
        let allCrossDets = (consumerDets @ producerDets) |> List.distinctBy (fun ac -> ac.id)

        // Filter out determinants that are already covered.
        // A determinant is covered if:
        //   (a) a parameter with the same name already exists, OR
        //   (b) a RefTypeArgumentDependency already connects this determinant
        //       to an existing parameter at this boundary (covers the case
        //       where the ACN file uses a different parameter name, e.g.,
        //       explicit "buffer-len" for determinant "buffers-length").
        let existingParamNames =
            rt.resolvedType.acnParameters
            |> List.map (fun p -> p.name)
            |> Set.ofList
        let deterministsCoveredByRefTypeArg =
            deps.acnDependencies
            |> List.choose (fun dep ->
                match dep.dependencyKind with
                | AcnDepRefTypeArgument _prm
                    when dep.asn1Type = t'.id ->
                    // This dep says: the boundary type (t'.id) receives
                    // determinant dep.determinant via a RefTypeArgument.
                    Some dep.determinant.id
                | _ -> None)
            |> Set.ofList
        let newDets =
            allCrossDets
            |> List.filter (fun d ->
                not (Set.contains d.Name.Value existingParamNames)
                && not (Set.contains d.id deterministsCoveredByRefTypeArg))

        if newDets.IsEmpty then
            t', childRewrites
        else
            let newParams = newDets |> List.map (createParam rt.resolvedType)
            let newArgs = newDets |> List.map (fun d -> RelativePath [d.Name])
            let resolvedType' =
                { rt.resolvedType with
                    acnParameters = rt.resolvedType.acnParameters @ newParams }
            let rt' =
                { rt with
                    resolvedType = resolvedType'
                    acnArguments = rt.acnArguments @ newArgs
                    hasExtraConstrainsOrChildrenOrAcnArgs = true }
            let t'' = { t' with Kind = Asn1TypeKind.ReferenceType rt' }

            // Collect dep rewrites for consumer determinants only.
            // For each consumer det that got a new parameter, we need to:
            //   (a) rewrite internal deps to point to the parameter
            //   (b) add a RefTypeArgumentDependency from boundary to original det
            // Producer dets don't need dep rewrites — the determinant is inside
            // the boundary and will be resolved locally.
            let consumerDetIds = consumerDets |> List.map (fun d -> d.id) |> Set.ofList
            let newConsumerDets = newDets |> List.filter (fun d -> Set.contains d.id consumerDetIds)
            let newRewrites =
                List.map2
                    (fun (det: AcnChild) (prm: AcnParameter) ->
                        { DepRewrite.boundaryTypeId = t'.id
                          originalDet = det
                          newParam = prm
                          boundaryPath = boundaryPath })
                    newConsumerDets
                    (newParams |> List.take newConsumerDets.Length)

            t'', childRewrites @ newRewrites

    | _ -> t', childRewrites


/// Apply collected dep rewrites to the dependency list.
/// For each consumer rewrite at a boundary:
///   1. REPLACE internal deps: deps where asn1Type is inside the boundary
///      and determinant is the original ACN child get their determinant
///      replaced with AcnParameterDeterminant(newParam).
///   2. ADD a RefTypeArgumentDependency: records that the boundary type
///      receives the original ACN child value via the new parameter.
///      This preserves the chain for the parent level and keeps the ACN
///      child "used" (avoiding validation errors).
///
/// In the backend, resolveParam stops at PRM nodes in deferred mode,
/// so the RefTypeArgumentDependency is NOT followed inside specialized
/// function bodies — it is only used at the parent level.
let private applyDepRewrites (deps: AcnInsertedFieldDependencies) (rewrites: DepRewrite list) : AcnInsertedFieldDependencies =
    if rewrites.IsEmpty then deps
    else
        // Step 1: replace internal deps
        let rewrittenDeps =
            deps.acnDependencies |> List.map (fun dep ->
                let matchingRewrite =
                    rewrites |> List.tryFind (fun rw ->
                        dep.determinant.id = rw.originalDet.id
                        && isPathPrefix rw.boundaryPath (dep.asn1Type.ToScopeNodeList))
                match matchingRewrite with
                | Some rw ->
                    { dep with determinant = AcnParameterDeterminant rw.newParam }
                | None -> dep)

        // Step 2: add RefTypeArgumentDependency for each rewrite
        let newRefTypeArgDeps =
            rewrites |> List.map (fun rw ->
                { AcnDependency.asn1Type = rw.boundaryTypeId
                  determinant = AcnChildDeterminant rw.originalDet
                  dependencyKind = AcnDepRefTypeArgument rw.newParam })

        // Step 3: rewrite the newly added RefTypeArgDeps by outer boundary
        // rewrites.  Inner boundary deps (e.g., case1 → hdr.buffers-length)
        // must be rewritten to point to the outer boundary's parameter
        // (e.g., case1 → payload.PRM.buffers-length).
        // Self-rewrites are excluded: a dep created at boundary X is not
        // rewritten by X's own rewrite (asn1Type = boundaryPath → skip).
        let rewrittenNewDeps =
            newRefTypeArgDeps |> List.map (fun dep ->
                let matchingRewrite =
                    rewrites |> List.tryFind (fun rw ->
                        dep.determinant.id = rw.originalDet.id
                        && isPathPrefix rw.boundaryPath (dep.asn1Type.ToScopeNodeList)
                        && rw.boundaryPath <> (dep.asn1Type.ToScopeNodeList))
                match matchingRewrite with
                | Some rw ->
                    { dep with determinant = AcnParameterDeterminant rw.newParam }
                | None -> dep)

        { acnDependencies = rewrittenDeps @ rewrittenNewDeps }


/// Transform a module's type assignments
let private transformModule (deps: AcnInsertedFieldDependencies) (m: Asn1Module) : Asn1Module * DepRewrite list =
    let tas', rewrites =
        m.TypeAssignments
        |> List.map (fun ta ->
            let t', rw = transformType deps ta.Type
            { ta with Type = t' }, rw)
        |> List.unzip
    { m with
        TypeAssignments = tas'
        typeAssignmentsMap = tas' |> List.map (fun ta -> ta.Name.Value, ta) |> Map.ofList },
    rewrites |> List.concat


/// Main entry point: transform the entire AST and rewrite deps.
/// Cross-scope ACN references become explicit acnParameters/acnArguments.
/// Dependencies are rewritten so that resolveParam stops at the new parameter
/// level instead of following the chain to the original ACN child.
/// Called only when args.acnDeferred = true, after CheckLongReferences.
let closureConvertAcnReferences (r: AstRoot) (deps: AcnInsertedFieldDependencies) : AstRoot * AcnInsertedFieldDependencies =
    let files', allRewrites =
        r.Files
        |> List.map (fun f ->
            let modules', rewrites =
                f.Modules |> List.map (transformModule deps) |> List.unzip
            { f with Modules = modules' }, rewrites |> List.concat)
        |> List.unzip

    let allRewrites = allRewrites |> List.concat

    // Rebuild the lookup maps to be consistent with the transformed type assignments
    let modulesMap' =
        files'
        |> List.collect (fun f -> f.Modules)
        |> List.map (fun m -> m.Name.Value, m)
        |> Map.ofList

    let typeAssignmentsMap' =
        files'
        |> List.collect (fun f ->
            f.Modules |> List.collect (fun m ->
                m.TypeAssignments |> List.map (fun ta ->
                    (m.Name.Value, ta.Name.Value), ta)))
        |> Map.ofList

    let newAst =
        { r with
            Files = files'
            modulesMap = modulesMap'
            typeAssignmentsMap = typeAssignmentsMap' }

    let newDeps = applyDepRewrites deps allRewrites

    newAst, newDeps
