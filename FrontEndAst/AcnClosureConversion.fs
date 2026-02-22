/// Closure conversion for ACN cross-scope references.
/// Transforms implicit cross-scope ACN determinant references into explicit
/// acnParameters / acnArguments on ReferenceType boundaries.
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


/// Recursively transform an Asn1Type tree, adding acnParameters and acnArguments
/// at each ReferenceType boundary that has cross-scope dependencies.
/// Processing is bottom-up: inner boundaries are transformed first, then outer ones.
let rec private transformType (deps: AcnInsertedFieldDependencies) (t: Asn1Type) : Asn1Type =
    // Step 1: Recurse into children (bottom-up)
    let t' =
        match t.Kind with
        | Asn1TypeKind.Sequence sq ->
            let children' =
                sq.children |> List.map (fun c ->
                    match c with
                    | SeqChildInfo.Asn1Child ac ->
                        SeqChildInfo.Asn1Child { ac with Type = transformType deps ac.Type }
                    | SeqChildInfo.AcnChild _ -> c)
            { t with Kind = Asn1TypeKind.Sequence { sq with children = children' } }

        | Asn1TypeKind.Choice ch ->
            let children' =
                ch.children |> List.map (fun c ->
                    { c with Type = transformType deps c.Type })
            { t with Kind = Asn1TypeKind.Choice { ch with children = children' } }

        | Asn1TypeKind.SequenceOf sqf ->
            let child' = transformType deps sqf.child
            { t with Kind = Asn1TypeKind.SequenceOf { sqf with child = child' } }

        | Asn1TypeKind.ReferenceType rt ->
            // Recurse into the resolvedType
            let resolvedType' = transformType deps rt.resolvedType
            { t with Kind = Asn1TypeKind.ReferenceType { rt with resolvedType = resolvedType' } }

        | _ -> t

    // Step 2: If this is a ReferenceType, apply boundary logic
    match t'.Kind with
    | Asn1TypeKind.ReferenceType rt ->
        let boundaryPath = t'.id.ToScopeNodeList
        // Consumer: determinant OUTSIDE, dependent type INSIDE (e.g., payload uses hdr.buffers-length)
        let consumerDets = findConsumerDeterminants boundaryPath deps
        // Producer: determinant INSIDE, dependent type OUTSIDE (e.g., hdr contains buffers-length)
        let producerDets = findProducerDeterminants boundaryPath deps
        let allCrossDets = (consumerDets @ producerDets) |> List.distinctBy (fun ac -> ac.id)

        // Filter out determinants that already have a parameter with the same name
        let existingParamNames =
            rt.resolvedType.acnParameters
            |> List.map (fun p -> p.name)
            |> Set.ofList
        let newDets =
            allCrossDets
            |> List.filter (fun d -> not (Set.contains d.Name.Value existingParamNames))

        if newDets.IsEmpty then
            t'
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
            { t' with Kind = Asn1TypeKind.ReferenceType rt' }

    | _ -> t'


/// Transform a module's type assignments
let private transformModule (deps: AcnInsertedFieldDependencies) (m: Asn1Module) : Asn1Module =
    let tas' =
        m.TypeAssignments
        |> List.map (fun ta -> { ta with Type = transformType deps ta.Type })
    { m with
        TypeAssignments = tas'
        typeAssignmentsMap = tas' |> List.map (fun ta -> ta.Name.Value, ta) |> Map.ofList }


/// Main entry point: transform the entire AST.
/// Cross-scope ACN references become explicit acnParameters/acnArguments.
/// Called only when args.acnDeferred = true, after CheckLongReferences.
let closureConvertAcnReferences (r: AstRoot) (deps: AcnInsertedFieldDependencies) : AstRoot =
    let files' =
        r.Files
        |> List.map (fun f ->
            { f with Modules = f.Modules |> List.map (transformModule deps) })

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

    { r with
        Files = files'
        modulesMap = modulesMap'
        typeAssignmentsMap = typeAssignmentsMap' }
