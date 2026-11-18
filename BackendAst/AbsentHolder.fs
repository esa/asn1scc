/// Determinant fields inside a component the decoder may find absent (q/issues.md Issue 106).
///
/// The front end accepts a determinant path through an OPTIONAL / ALWAYS ABSENT component when the dependent is not
/// always present (CheckLongReferences.checkRelativePath, Issue 53 rule): `header [present-when flg] { n ... }, data
/// [present-when flg2, size header.n]`. When the stream says `header` absent and `data` present, the field `n` is not in
/// the stream and its decode local is never set. The decoder must then fail at the dependent instead of reading the
/// local. This module decides, from the AST only, which dependencies read such a field (shared by the producers, which
/// emit the guard, and by DecodeOutcome, which must classify a guarded part as one that may fail).
module AbsentHolder

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions


/// The RefTypeArgument link of each ACN parameter (parameter id -> the determinant passed to it), built once per
/// dependency list (moved from Acn/AcnExternalField.fs: DecodeOutcome, compiled before it, needs the identity below).
let private paramLinksCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, Determinant>>()

let paramLinks (deps: AcnInsertedFieldDependencies) : Map<ReferenceToType, Determinant> =
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
let rec paramIdIdentity (r: AstRoot) (deps: AcnInsertedFieldDependencies) (prmId: ReferenceToType) : ReferenceToType =
    match r.closureParamDeterminants.TryFind prmId with
    | Some fieldId -> fieldId
    | None ->
        match (paramLinks deps).TryFind prmId with
        | Some (AcnChildDeterminant ac)      -> ac.id
        | Some (AcnParameterDeterminant prm) -> paramIdIdentity r deps prm.id
        | None                               -> prmId

let determinantIdentity (r: AstRoot) (deps: AcnInsertedFieldDependencies) (det: Determinant) : ReferenceToType =
    match det with
    | AcnChildDeterminant ac      -> ac.id
    | AcnParameterDeterminant prm -> paramIdIdentity r deps prm.id

/// --acn-v2: whether the parameter determinant of a dependency is a producer parameter of a referenced child that
/// exposes the field to its siblings (closure conversion rewrites such a sibling consumer to it,
/// AcnClosureConversion.applyDepRewrites): its boundary does not enclose the dependent. The decoder then reads the
/// caller's local AcnInsertedFieldRef of the field (AcnExternalField.getExternalField0, `isExposedProducer`).
let isExposedProducer (d: AcnDependency) (prm: AcnParameter) : bool =
    let paramNodes = prm.id.ToScopeNodeList
    let boundaryPath = paramNodes |> List.take (paramNodes.Length - 1)
    let dependentPath = d.asn1Type.ToScopeNodeList
    not (boundaryPath.Length <= dependentPath.Length && List.take boundaryPath.Length dependentPath = boundaryPath)

/// The ACN field a dependency reads in the function that decodes the dependent, and the node it is read for in the
/// field's tree; None when the field is not read in that function (an --acn-v2 formal AcnInsertedFieldRef parameter:
/// the field is decoded by a caller).
/// - legacy: a parameterised type is inlined at its reference site, so a parameter is followed through its
///   RefTypeArgument links to the field (the chain getExternalField0 follows); the reader is the outermost site, the
///   one that receives the field itself.
/// - --acn-v2: a field of this function (AcnChildDeterminant), or a field exposed by a referenced child through a
///   producer parameter (read from the caller's local ref).
let private fieldAndReader (r: AstRoot) (deps: AcnInsertedFieldDependencies) (d: AcnDependency) : (ReferenceToType * ReferenceToType) option =
    match r.args.acnDeferred, d.determinant with
    | _, AcnChildDeterminant ac -> Some (ac.id, d.asn1Type)
    | true, AcnParameterDeterminant prm ->
        match isExposedProducer d prm with
        | true  -> Some (determinantIdentity r deps d.determinant, d.asn1Type)
        | false -> None
    | false, AcnParameterDeterminant prm ->
        let rec follow (prmId: ReferenceToType) (visited: Set<ReferenceToType>) =
            match visited.Contains prmId with
            | true  -> None
            | false ->
                deps.acnDependencies
                |> List.tryPick (fun x ->
                    match x.dependencyKind with
                    | AcnDepRefTypeArgument p when p.id = prmId -> Some x
                    | _ -> None)
                |> Option.bind (fun site ->
                    match site.determinant with
                    | AcnChildDeterminant ac       -> Some (ac.id, site.asn1Type)
                    | AcnParameterDeterminant prm2 -> follow prm2.id (visited.Add prmId))
        follow prm.id Set.empty

/// The optionality of the SEQUENCE component at `path` (MD :: TA :: ... :: SEQ_CHILD name), found by walking the type
/// assignment through the components (references by their actual type, as AcnDependencies.withBackendChildNames
/// does). None when the path does not lead to an ASN.1 SEQUENCE component.
let private componentOptionality (r: AstRoot) (path: ScopeNode list) : Asn1Optionality option option =
    let rec walk (t: Asn1Type) (nodes: ScopeNode list) : Asn1Optionality option option =
        match nodes, t.ActualType.Kind with
        | [SEQ_CHILD (name, _)], Sequence s ->
            s.children |> List.tryPick (fun c ->
                match c with
                | Asn1Child c when c.Name.Value = name -> Some c.Optionality
                | Asn1Child _ | AcnChild _             -> None)
        | (SEQ_CHILD (name, _)) :: rest, Sequence s ->
            s.children
            |> List.tryPick (fun c ->
                match c with
                | Asn1Child c when c.Name.Value = name -> Some c.Type
                | Asn1Child _ | AcnChild _             -> None)
            |> Option.bind (fun ct -> walk ct rest)
        | (CH_CHILD (name, _, _)) :: rest, Choice ch ->
            ch.children |> List.tryFind (fun c -> c.Name.Value = name) |> Option.bind (fun c -> walk c.Type rest)
        | SQF :: rest, SequenceOf sqf -> walk sqf.child rest
        | _ -> None
    match path with
    | (MD md) :: (TA tas) :: rest ->
        r.typeAssignmentsMap.TryFind (md, tas) |> Option.bind (fun ts -> walk ts.Type rest)
    | _ -> None

/// ACN fields whose decode local shares its name with another ACN field of the same type assignment
/// (`getAcnDeterminantName` joins the path with `_` and maps `-` to `_`: `a.b-h.more` and `a-b.h.more` are both
/// `Top_a_b_h_more`; the declarations are merged, so one local holds both). A reset of such a local could overwrite the
/// other field's decoded value: these fields keep the code of master (no reset, no initial value), and a holder is not
/// taken as dominated by such a flag (q/issues.md Issue 106, round 3; the collision itself is pre-existing).
let private collidingFieldsCache = System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Set<ReferenceToType>>()
let private collidingFields (deps: AcnInsertedFieldDependencies) : Set<ReferenceToType> =
    collidingFieldsCache.GetValue(deps, fun deps ->
        deps.acnDependencies
        |> List.choose (fun d -> match d.determinant with AcnChildDeterminant ac -> Some ac.id | AcnParameterDeterminant _ -> None)
        |> List.distinct
        |> List.groupBy (fun id ->
            match id.ToScopeNodeList with
            | md :: ta :: _ -> Some (md, ta), AcnCreateFromAntlr.getAcnDeterminantName id
            | _             -> None, AcnCreateFromAntlr.getAcnDeterminantName id)
        |> List.filter (fun (_, ids) -> ids.Length > 1)
        |> List.collect snd
        |> Set.ofList)

/// The ACN field (id) whose value decides the presence of the SEQUENCE component at `path` (present-when <BOOLEAN
/// field>), from its AcnDepPresenceBool dependency: legacy follows ACN parameters to the field, --acn-v2 takes the
/// field a parameter stands for (determinantIdentity). None for any other optionality.
let private presenceFieldOf (r: AstRoot) (deps: AcnInsertedFieldDependencies) (path: ScopeNode list) : ReferenceToType option =
    match componentOptionality r path with
    | Some (Some (Optional {acnPresentWhen = Some (PresenceWhenBool _)})) ->
        deps.acnDependencies
        |> List.tryFind (fun d ->
            d.asn1Type = ReferenceToType path &&
                (match d.dependencyKind with
                 | AcnDepPresenceBool -> true
                 | AcnDepIA5StringSizeDeterminant _ | AcnDepSizeDeterminant _ | AcnDepSizeDeterminant_bit_oct_str_contain _
                 | AcnDepRefTypeArgument _ | AcnDepPresence _ | AcnDepPresenceStr _ | AcnDepChoiceDeterminant _ -> false))
        |> Option.map (fun d ->
            match r.args.acnDeferred, fieldAndReader r deps d with
            | false, Some (fieldId, _) -> fieldId
            | false, None | true, _    -> determinantIdentity r deps d.determinant)
    | Some (Some (Optional _)) | Some (Some AlwaysAbsent) | Some (Some AlwaysPresent) | Some None | None -> None

/// The components between the deepest common ancestor of the field a dependency reads and the dependent (its reader,
/// fieldAndReader), and the field, whose presence the decoder decides from the stream: OPTIONAL (with or without
/// present-when) and ALWAYS ABSENT; ids with their optionality, outermost first. Empty when the field's SEQUENCE
/// dominates the reader (every step mandatory or ALWAYS PRESENT, which Issue 53 treats as mandatory, or present when
/// a BOOLEAN field is TRUE that also decides the presence of the reader or of a component holding it): then the field is decoded on every path that reaches the
/// dependent. A path passes through SEQUENCE components and references only (the
/// front end rejects CHOICE and SEQUENCE OF steps), and the field precedes the dependent, so the presence of every step
/// is known when the dependent is decoded.
let private holderStepsWith (sameFlagDominates: bool) (r: AstRoot) (deps: AcnInsertedFieldDependencies) (d: AcnDependency) : (ReferenceToType * Asn1Optionality) list =
    match fieldAndReader r deps d with
    | None -> []
    | Some (fieldId, readerId) ->
        let fieldPath = fieldId.ToScopeNodeList
        let readerPath = readerId.ToScopeNodeList
        let common =
            List.zip (List.truncate readerPath.Length fieldPath) (List.truncate fieldPath.Length readerPath)
            |> List.takeWhile (fun (x, y) -> x = y) |> List.length
        // the BOOLEAN fields that decide the presence of the reader's branch below the common ancestor (the reader
        // itself included): when the reader is decoded, each of them is TRUE
        let readerFlags =
            [common + 1 .. readerPath.Length] |> List.choose (fun n -> presenceFieldOf r deps (List.take n readerPath))
        // a holder present when one of those fields says so (`header [present-when flg]`, `data [present-when flg,
        // size header.n]`, also deeper: `h1 [present-when f1] { h2 [present-when f1] { n } }`): the reader is decoded
        // only when the holder is present. The fields are compared by id (the same relative path names different
        // fields from different SEQUENCEs).
        // Also a holder that holds one of those fields (`h [present-when f] { more, n }`, `x [present-when h.more, size
        // h.n]`): the field is FALSE unless decoded in the holder (presenceFlagResets; an --acn-v2 reference starts at 0).
        let sameFlagAsReader (n: int) =
            let stepPath = List.take n fieldPath
            let holdsFlag (flag: ReferenceToType) =
                let flagPath = flag.ToScopeNodeList
                flagPath.Length > n && List.take n flagPath = stepPath && not ((collidingFields deps).Contains flag)
            sameFlagDominates &&
                ((match presenceFieldOf r deps stepPath with
                  | Some h -> readerFlags |> List.contains h
                  | None   -> false)
                 || readerFlags |> List.exists holdsFlag)
        // the components strictly between the common ancestor and the field (the field itself is the last node)
        [common + 1 .. fieldPath.Length - 1]
        |> List.choose (fun n ->
            let path = List.take n fieldPath
            match List.last path, sameFlagAsReader n with
            | SEQ_CHILD (_, true), false ->
                match componentOptionality r path with
                | Some (Some (Optional opt)) -> Some (ReferenceToType path, Optional opt)
                | Some (Some AlwaysAbsent)   -> Some (ReferenceToType path, AlwaysAbsent)
                | Some (Some AlwaysPresent)
                | Some None
                | None                       -> None
            | SEQ_CHILD (_, true), true
            | (SEQ_CHILD (_, false) | MD _ | TA _ | VA _ | PRM _ | CH_CHILD _ | SQF), _ -> None)

let private holderSteps = holderStepsWith true

/// The dependency kinds whose decoder fails when the field's holder is absent (Issue 106, fix option b): a size, a
/// CHOICE determinant, a CHOICE present-when on an INTEGER / string field. Not a present-when on a BOOLEAN field: its
/// local is set to FALSE at the start of the enclosing SEQUENCE (presenceFlagResets), an --acn-v2 AcnInsertedFieldRef starts at value 0 (written only by
/// the holder's decoder; not so in an inline SEQUENCE OF element, where the ref is declared before the loop, a shape
/// whose encoder does not compile today), so a holder that is absent makes the dependent absent, which is what the
/// encoders require, and a guard would be a condition no test can show independent (MC/DC). Not a RefTypeArgument: legacy inlines the referenced
/// type, whose consumers get the guard; --acn-v2 passes the ref (initial value 0) to a specialised function, where a
/// guard at the site would reject a stream in which the referenced type does not use the parameter.
let private failsWhenHolderAbsent (d: AcnDependency) : bool =
    match d.dependencyKind with
    | AcnDepSizeDeterminant _
    | AcnDepIA5StringSizeDeterminant _
    | AcnDepSizeDeterminant_bit_oct_str_contain _
    | AcnDepChoiceDeterminant _
    | AcnDepPresence _
    | AcnDepPresenceStr _               -> true
    | AcnDepPresenceBool
    | AcnDepRefTypeArgument _           -> false

/// What the decoder of a dependent does before it reads its determinant fields (Issue 106).
type DecodeGuard =
    /// every field is decoded on every path that reaches the dependent: no code (all existing grammars before Issue 106)
    | NoGuard
    /// test the presence of the holders (ids, outermost first) and fail when one is absent; the fields read (ids)
    | TestHolders of holders: ReferenceToType list * fields: ReferenceToType list
    /// a holder is ALWAYS ABSENT: the field is never in the stream, a present dependent always fails
    | HolderAlwaysAbsent of holders: ReferenceToType list * fields: ReferenceToType list

let private decodeGuardsCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, (AcnDependency * (ReferenceToType * Asn1Optionality) list * ReferenceToType) list>>()

/// The dependencies that read a field whose holder may be absent, with the holder steps and the field, by dependent id,
/// built once per dependency list.
let private guardedDependencies (r: AstRoot) (deps: AcnInsertedFieldDependencies) =
    decodeGuardsCache.GetValue(deps, fun deps ->
        deps.acnDependencies
        |> List.filter failsWhenHolderAbsent
        |> List.choose (fun d ->
            match holderSteps r deps d, fieldAndReader r deps d with
            | [], _ | _, None -> None
            | steps, Some (fieldId, _) -> Some (d, steps, fieldId))
        |> List.groupBy (fun (d, _, _) -> d.asn1Type)
        |> Map.ofList)

/// Whether the decoder of a type of this kind is the one that reads the determinant of the dependency: a reference
/// site with ACN properties shares its id with its resolved type (the merged type is keyed by the site), so both
/// decoders see the dependency; the read belongs to the sizeable / CHOICE type, and to the reference itself only for
/// an OCTET / BIT STRING (CONTAINING X) (whose resolved type X does not read it).
let private readsTheDeterminant (kind: Asn1TypeKind) (d: AcnDependency) : bool =
    match d.dependencyKind, kind with
    | AcnDepSizeDeterminant _, (OctetString _ | BitString _ | SequenceOf _)         -> true
    | AcnDepIA5StringSizeDeterminant _, (IA5String _ | NumericString _)             -> true
    | AcnDepSizeDeterminant_bit_oct_str_contain _, ReferenceType rf                 -> rf.encodingOptions.IsSome
    | (AcnDepChoiceDeterminant _ | AcnDepPresence _ | AcnDepPresenceStr _), Choice _ -> true
    | _ -> false

/// The DecodeGuard of the decoder of t (NoGuard for every type that does not read a determinant whose holder may be
/// absent).
let decodeGuard (r: AstRoot) (deps: AcnInsertedFieldDependencies) (t: Asn1Type) : DecodeGuard =
    match (guardedDependencies r deps).TryFind t.id |> Option.defaultValue [] |> List.filter (fun (d, _, _) -> readsTheDeterminant t.Kind d) with
    | [] -> NoGuard
    | xs ->
        let steps = xs |> List.collect (fun (_, steps, _) -> steps) |> List.distinct
        let holders = steps |> List.map fst
        let fields = xs |> List.map (fun (_, _, fieldId) -> fieldId) |> List.distinct
        let alwaysAbsent =
            steps |> List.exists (fun (_, opt) ->
                match opt with
                | AlwaysAbsent                  -> true
                | Optional _ | AlwaysPresent    -> false)
        match alwaysAbsent with
        | true  -> HolderAlwaysAbsent (holders, fields)
        | false -> TestHolders (holders, fields)

/// Whether the decoder of t has a guard (DecodeOutcome: the part may fail).
let hasDecodeGuard (r: AstRoot) (deps: AcnInsertedFieldDependencies) (t: Asn1Type) : bool =
    match decodeGuard r deps t with
    | NoGuard                                   -> false
    | TestHolders _ | HolderAlwaysAbsent _      -> true

/// A present-when on a BOOLEAN field read from a decode local (legacy; --acn-v2 a field of the same function, not an
/// AcnInsertedFieldRef) whose holder may be absent: the local is set to FALSE at the start of the SEQUENCE that
/// encloses holder and dependent (the deepest common ancestor), so a holder that is absent makes the dependent absent.
/// At the start of that SEQUENCE, not only at the declaration: a SEQUENCE OF element is decoded in a loop and the local
/// is declared once before it, so element k would see element k - 1's value (q/issues.md Issue 106, round 2).
let private presenceFlagReadsOf (r: AstRoot) (deps: AcnInsertedFieldDependencies) : (ReferenceToType * ReferenceToType * ReferenceToType list) list =
    deps.acnDependencies
    |> List.choose (fun d ->
        match d.dependencyKind, r.args.acnDeferred, d.determinant with
        | AcnDepPresenceBool, false, _
        | AcnDepPresenceBool, true, AcnChildDeterminant _ ->
            match holderStepsWith false r deps d, fieldAndReader r deps d with
            | [], _ | _, None -> None
            | _, Some (fieldId, _) when (collidingFields deps).Contains fieldId -> None
            | steps, Some (fieldId, readerId) ->
                // the deepest common ancestor of field and reader (a SEQUENCE: both are components of it or below it)
                let fieldPath, readerPath = fieldId.ToScopeNodeList, readerId.ToScopeNodeList
                let common =
                    List.zip (List.truncate readerPath.Length fieldPath) (List.truncate fieldPath.Length readerPath)
                    |> List.takeWhile (fun (x, y) -> x = y) |> List.length
                Some (ReferenceToType (List.take common fieldPath), fieldId, steps |> List.map fst)
        | AcnDepPresenceBool, true, AcnParameterDeterminant _ -> None      // an AcnInsertedFieldRef: starts at 0
        | AcnDepIA5StringSizeDeterminant _, _, _ | AcnDepSizeDeterminant _, _, _ | AcnDepSizeDeterminant_bit_oct_str_contain _, _, _
        | AcnDepRefTypeArgument _, _, _ | AcnDepPresence _, _, _ | AcnDepPresenceStr _, _, _ | AcnDepChoiceDeterminant _, _, _ -> None)

let private presenceFlagReadsCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, (ReferenceToType * ReferenceToType * ReferenceToType list) list>()
let private presenceFlagReads (r: AstRoot) (deps: AcnInsertedFieldDependencies) : (ReferenceToType * ReferenceToType * ReferenceToType list) list =
    presenceFlagReadsCache.GetValue(deps, fun deps -> presenceFlagReadsOf r deps)

/// The BOOLEAN fields (ids, with their holders) whose decode local the SEQUENCE seqId sets to FALSE at its start
/// (presenceFlagReads).
let presenceFlagResets (r: AstRoot) (deps: AcnInsertedFieldDependencies) (seqId: ReferenceToType) : (ReferenceToType * ReferenceToType list) list =
    presenceFlagReads r deps
    |> List.filter (fun (ancestor, _, _) -> ancestor = seqId)
    |> List.groupBy (fun (_, fieldId, _) -> fieldId)
    |> List.map (fun (fieldId, xs) -> fieldId, xs |> List.collect (fun (_, _, holders) -> holders) |> List.distinct)

/// The ACN fields that a size / CHOICE dependency may read while the component holding them is absent (not a
/// RefTypeArgument: the links only pass the field on; not a present-when on a BOOLEAN field: presenceFlagResets).
/// Their decode locals are declared with an initial value: gnatprove flow, which does not correlate the guard with the
/// holder's presence, sees an initialised local on every path (the guard makes the read exact, also in a loop). Also
/// when the holder and the dependent are present by the same BOOLEAN field (no guard, the read is exact): flow does
/// not correlate the two presence flags either ("might not be initialized" at the read).
let private fieldsReadCache = System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Set<ReferenceToType>>()
let fieldsReadWhenHolderAbsent (r: AstRoot) (deps: AcnInsertedFieldDependencies) : Set<ReferenceToType> =
    fieldsReadCache.GetValue(deps, fun deps ->
        deps.acnDependencies
        |> List.filter (fun d ->
            match d.dependencyKind with
            | AcnDepRefTypeArgument _ | AcnDepPresenceBool -> false
            | AcnDepIA5StringSizeDeterminant _ | AcnDepSizeDeterminant _ | AcnDepSizeDeterminant_bit_oct_str_contain _
            | AcnDepPresence _ | AcnDepPresenceStr _ | AcnDepChoiceDeterminant _ -> true)
        |> List.filter (fun d -> not (holderStepsWith false r deps d).IsEmpty)
        |> List.choose (fun d -> fieldAndReader r deps d |> Option.map fst)
        |> Set.ofList)
