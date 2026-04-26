module AcnExternalField

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language


let getExternalField0 (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency func1 =
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
            if r.args.acnDeferred then
                // In deferred mode, the parameter IS the value — it arrives
                // as an AcnInsertedFieldRef* formal parameter.  Do NOT follow
                // RefTypeArgumentDependency chains; stop here.
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
    // In deferred mode, when the determinant resolved to a PRM (parameter),
    // the formal parameter is AcnInsertedFieldRef*.  Code that reads the
    // integer value must access the ->value field of the struct.
    // For IA5String determinants, use ->str_value instead of ->value.
    if r.args.acnDeferred then
        let resolvedNodes = match resolvedId with ReferenceToType nodes -> nodes
        let resolvedLastNode = resolvedNodes |> List.rev |> List.head
        match resolvedLastNode with
        | PRM _ ->
            // Check if the dependency is string-typed
            let isStringDep =
                match dependency.dependencyKind with
                | Asn1AcnAst.AcnDepPresenceStr _ -> true
                | _ -> false
            if isStringDep then baseName + "->str_value"
            else baseName + "->value"
        | _     -> baseName
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

let getExternalFieldChoicePresentWhen (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency  relPath=
    let filterDependency (d:AcnDependency) =
        match d.dependencyKind with
        | AcnDepPresence (relPath0, _)   -> relPath = relPath0
        | _                              -> true
    getExternalField0 r deps asn1TypeIdWithDependency filterDependency

let getExternalFieldTypeChoicePresentWhen (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency  relPath=
    let filterDependency (d:AcnDependency) =
        match d.dependencyKind with
        | AcnDepPresence (relPath0, _)   -> relPath = relPath0
        | _                              -> true
    getExternalField0Type r deps asn1TypeIdWithDependency filterDependency

let getExternalField (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
    getExternalField0 r deps asn1TypeIdWithDependency (fun z -> true)

let getExternalFieldType (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
    getExternalField0Type r deps asn1TypeIdWithDependency (fun z -> true)
