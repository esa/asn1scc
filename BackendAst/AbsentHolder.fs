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

/// The ACN parameters (ids) that stand for each ACN field (determinantIdentity), built once per dependency list (moved
/// from Acn/AcnExternalField.fs: the encoder guard needs isDeferredField).
let private fieldParamsCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, ReferenceToType list>>()

let fieldParams (r: AstRoot) (deps: AcnInsertedFieldDependencies) : Map<ReferenceToType, ReferenceToType list> =
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
let isDeferredField (r: AstRoot) (deps: AcnInsertedFieldDependencies) (fieldId: ReferenceToType) : bool =
    (fieldParams r deps).ContainsKey fieldId

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

/// --acn-v2, q/issues.md Issue 107: a dependency that reads its field through an AcnInsertedFieldRef parameter of the
/// function that decodes the dependent (the cases fieldAndReader leaves out): the field (determinantIdentity), the
/// dependent, which is the reader, and the parameter. The field is decoded by a caller, which sets the ref's `in_stream`
/// where it reads the field (DAstACNDeferred); the parameter of an outermost function stands for itself (no holder).
let private refFieldAndReader (r: AstRoot) (deps: AcnInsertedFieldDependencies) (d: AcnDependency) : (ReferenceToType * ReferenceToType * AcnParameter) option =
    match r.args.acnDeferred, d.determinant with
    | true, AcnParameterDeterminant prm when not (isExposedProducer d prm) -> Some (determinantIdentity r deps d.determinant, d.asn1Type, prm)
    | true, AcnParameterDeterminant _
    | true, AcnChildDeterminant _
    | false, _ -> None

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
/// other field's decoded value, and an initial value on one of them gives two declarations of the local (C / Ada do not
/// compile): these fields keep the code of master (no reset, no initial value), and a holder is not taken as dominated
/// by such a flag (q/issues.md Issue 106, rounds 3 / 4; the collision itself is pre-existing).
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

/// Which present-when BOOLEAN fields make a holder of a field no holder of a reader (holderStepsOf).
type private FlagDominance =
    /// structural: every OPTIONAL / ALWAYS ABSENT component between the common ancestor and the field
    | NoDominance
    /// the decoder (Issue 106): the holder is present when a BOOLEAN field is TRUE that also decides the presence of
    /// the reader or of a component holding it, or the holder holds such a field (decoded FALSE when the holder is
    /// absent): the reader is then decoded only when the holder is present
    | DecoderDominance
    /// the encoder (Issue 108): the holder is present when a BOOLEAN field is TRUE that also decides the presence of
    /// the reader or of a component holding it (the field's update rejects a value in which the two disagree, before
    /// the reader is written; not an --acn-v2 deferred field, whose patches reject after the reader is written), or the holder holds a field that decides the presence of a component strictly holding
    /// the reader (that component's own guard rejects the value first). A field of the holder that decides the
    /// presence of the reader itself does not dominate: the encoder writes the reader from the value, and the
    /// present-when BOOLEAN dependency of the reader gets the guard
    | EncoderDominance

/// The components between the deepest common ancestor of the field a dependency reads and the dependent (its reader,
/// fieldAndReader), and the field, whose presence the decoder decides from the stream: OPTIONAL (with or without
/// present-when) and ALWAYS ABSENT; ids with their optionality, outermost first. Empty when the field's SEQUENCE
/// dominates the reader (every step mandatory or ALWAYS PRESENT, which Issue 53 treats as mandatory, or present when
/// a BOOLEAN field is TRUE that also decides the presence of the reader or of a component holding it): then the field is decoded on every path that reaches the
/// dependent. A path passes through SEQUENCE components and references only (the
/// front end rejects CHOICE and SEQUENCE OF steps), and the field precedes the dependent, so the presence of every step
/// is known when the dependent is decoded.
let private holderStepsOf (dominance: FlagDominance) (r: AstRoot) (deps: AcnInsertedFieldDependencies) (fieldId: ReferenceToType) (readerId: ReferenceToType) : (ReferenceToType * Asn1Optionality) list =
    let fieldPath = fieldId.ToScopeNodeList
    let readerPath = readerId.ToScopeNodeList
    let common =
        List.zip (List.truncate readerPath.Length fieldPath) (List.truncate fieldPath.Length readerPath)
        |> List.takeWhile (fun (x, y) -> x = y) |> List.length
    // the BOOLEAN fields that decide the presence of the reader's branch below the common ancestor (the reader
    // itself included): when the reader is decoded, each of them is TRUE
    let readerFlags =
        [common + 1 .. readerPath.Length] |> List.choose (fun n -> presenceFieldOf r deps (List.take n readerPath))
    // the same for the components strictly holding the reader (EncoderDominance)
    let holdingFlags =
        [common + 1 .. readerPath.Length - 1] |> List.choose (fun n -> presenceFieldOf r deps (List.take n readerPath))
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
        let sameFlag () =
            match presenceFieldOf r deps stepPath with
            | Some h -> readerFlags |> List.contains h && not ((collidingFields deps).Contains h)
            | None   -> false
        // the encoder: the flag's update rejects a disagreement before the reader is written, unless --acn-v2 defers
        // the flag (a field passed to a reference: its patches, checked after the SEQUENCE is written, reject late)
        let sameFlagRejectsFirst () =
            match presenceFieldOf r deps stepPath with
            | Some h -> not (r.args.acnDeferred && isDeferredField r deps h)
            | None   -> false
        match dominance with
        | NoDominance      -> false
        | DecoderDominance -> sameFlag () || readerFlags |> List.exists holdsFlag
        | EncoderDominance -> (sameFlag () && sameFlagRejectsFirst ()) || holdingFlags |> List.exists holdsFlag
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

let private holderStepsWith (dominance: FlagDominance) (r: AstRoot) (deps: AcnInsertedFieldDependencies) (d: AcnDependency) : (ReferenceToType * Asn1Optionality) list =
    match fieldAndReader r deps d with
    | None -> []
    | Some (fieldId, readerId) -> holderStepsOf dominance r deps fieldId readerId

/// The dependency kinds whose decoder fails when the field's holder is absent (Issue 106, fix option b): a size, a
/// CHOICE determinant, a CHOICE present-when on an INTEGER / string field. Not a present-when on a BOOLEAN field: its
/// local is set to FALSE at the start of the enclosing SEQUENCE (presenceFlagResets), an --acn-v2 AcnInsertedFieldRef starts at value 0 (written only by
/// the holder's decoder; not so in an inline SEQUENCE OF element, where the ref is declared before the loop, a shape
/// whose encoder does not compile today), so a holder that is absent makes the dependent absent, which is what the
/// encoders require, and a guard would be a condition no test can show independent (MC/DC). Not a RefTypeArgument: legacy inlines the referenced
/// type, whose consumers get the guard; --acn-v2 passes the ref (initial value 0) to a specialised function, where a
/// guard at the site would reject a stream in which the referenced type does not use the parameter.
/// The encoder (q/issues.md Issue 108) fails for the same kinds and for a present-when on a BOOLEAN field: the value
/// holds the dependent while the field, whose holder is absent, is not encoded (the stream would decode to another
/// value, the flag being FALSE).
let private failsWhenHolderAbsent (codec: Codec) (d: AcnDependency) : bool =
    match d.dependencyKind, codec with
    | AcnDepSizeDeterminant _, _
    | AcnDepIA5StringSizeDeterminant _, _
    | AcnDepSizeDeterminant_bit_oct_str_contain _, _
    | AcnDepChoiceDeterminant _, _
    | AcnDepPresence _, _
    | AcnDepPresenceStr _, _                -> true
    | AcnDepPresenceBool, Encode            -> true
    | AcnDepPresenceBool, Decode
    | AcnDepRefTypeArgument _, _            -> false

/// The flag dominance of each side (holderStepsOf).
let private dominanceOf (codec: Codec) : FlagDominance =
    match codec with
    | Decode -> DecoderDominance
    | Encode -> EncoderDominance

/// --acn-v2, q/issues.md Issue 107: the AcnInsertedFieldRef a guard tests (`in_stream`) when the dependent reads its
/// field through a ref, i.e. when the holder is not visible where the dependent is decoded.
type RefTest =
    /// the dependent is decoded in a specialised function and reads this formal parameter of it
    | FormalRef of AcnParameter
    /// the dependent is an OCTET / BIT STRING (CONTAINING X) reference site: the region wrapper of its specialised
    /// function reads this parameter, and the guard at the call tests the ref the caller passes for it
    | ArgumentRef of AcnParameter

/// What the decoder of a dependent does before it reads its determinant fields (Issue 106), and the encoder before it
/// writes the dependent (Issue 108).
type DecodeGuard =
    /// every field is decoded on every path that reaches the dependent: no code (all existing grammars before Issue 106)
    | NoGuard
    /// test the presence of the holders (ids, outermost first; fields of this function) and the `in_stream` of the refs
    /// (fields read through an AcnInsertedFieldRef, Issue 107, each with its holders for the comment), and fail when
    /// one is absent; the fields read (ids)
    | TestHolders of holders: ReferenceToType list * refs: (RefTest * ReferenceToType list) list * fields: ReferenceToType list
    /// a holder is ALWAYS ABSENT: the field is never in the stream, a present dependent always fails
    | HolderAlwaysAbsent of holders: ReferenceToType list * fields: ReferenceToType list

/// A guarded dependency: the holder steps, the field, and the parameter it is read through (Issue 107; None: a field
/// decoded in the function of the dependent, Issue 106).
type private GuardedDependency = AcnDependency * (ReferenceToType * Asn1Optionality) list * ReferenceToType * AcnParameter option

let private decodeGuardsCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, GuardedDependency list>>()
let private encodeGuardsCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, GuardedDependency list>>()

/// The dependencies that read a field whose holder may be absent, with the holder steps and the field, by dependent id,
/// built once per dependency list and codec side (language neutral). --acn-v2 (Issue 107): also a dependent that reads
/// the field through a ref parameter (refFieldAndReader); see deferrableGuarded. The encoder's list (Issue 108) adds the
/// present-when BOOLEAN dependents (failsWhenHolderAbsent) and keeps a holder whose field decides the reader's own
/// presence (EncoderDominance).
let private guardedDependencies (codec: Codec) (r: AstRoot) (deps: AcnInsertedFieldDependencies) : Map<ReferenceToType, GuardedDependency list> =
    let cache =
        match codec with
        | Decode -> decodeGuardsCache
        | Encode -> encodeGuardsCache
    cache.GetValue(deps, fun deps ->
        deps.acnDependencies
        |> List.filter (failsWhenHolderAbsent codec)
        |> List.choose (fun d ->
            let read =
                match fieldAndReader r deps d, refFieldAndReader r deps d with
                | Some (fieldId, _), _              -> Some (holderStepsWith (dominanceOf codec) r deps d, fieldId, None)
                | None, Some (fieldId, readerId, prm) -> Some (holderStepsOf (dominanceOf codec) r deps fieldId readerId, fieldId, Some prm)
                | None, None                        -> None
            match read with
            | None | Some ([], _, _) -> None
            // a local shared by two ACN fields keeps master's code (collidingFields)
            | Some (_, fieldId, _) when (collidingFields deps).Contains fieldId -> None
            | Some (steps, fieldId, prm) -> Some (d, steps, fieldId, prm))
        |> List.groupBy (fun (d, _, _, _) -> d.asn1Type)
        |> Map.ofList)

/// The type of the ACN field fieldId (the AcnChildDeterminant of some dependency), built once per dependency list.
let private fieldTypesCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, Map<ReferenceToType, AcnInsertedType>>()
let private fieldTypes (deps: AcnInsertedFieldDependencies) : Map<ReferenceToType, AcnInsertedType> =
    fieldTypesCache.GetValue(deps, fun deps ->
        deps.acnDependencies
        |> List.choose (fun d ->
            match d.determinant with
            | AcnChildDeterminant ac      -> Some (ac.id, ac.Type)
            | AcnParameterDeterminant _   -> None)
        |> Map.ofList)

/// guardedDependencies without the reads through a ref whose field is not held in the ref by the decoder of its
/// SEQUENCE: --acn-v2 defers only the field types with deferred det functions (isDeferrable =
/// `lm.lg.getDeferredDetFunctions t).IsSome`, the question DAstACNDeferred.createDeferredSequenceFunction asks); any
/// other field (e.g. an ASCII INTEGER) is decoded into a plain local and never sets the ref's `in_stream`, so a guard on
/// it would reject valid streams (cycle i107, triage 3.0). Both decodeGuard and fieldsReadThroughRef read this list:
/// a guard never tests a flag no decoder sets. A field whose type is not found keeps its dependency.
let private deferrableGuarded (codec: Codec) (r: AstRoot) (deps: AcnInsertedFieldDependencies) (isDeferrable: AcnInsertedType -> bool) : Map<ReferenceToType, GuardedDependency list> =
    guardedDependencies codec r deps
    |> Map.map (fun _ xs ->
        xs |> List.filter (fun (_, _, fieldId, prm) ->
            match prm, (fieldTypes deps).TryFind fieldId with
            | None, _               -> true
            | Some _, None          -> true
            | Some _, Some fieldType -> isDeferrable fieldType))

/// --acn-v2 (Issue 107): the fields that some dependent reads through an AcnInsertedFieldRef under a guard
/// (deferrableGuarded): the decoder sets the ref's `in_stream` where it reads such a field.
let fieldsReadThroughRef (r: AstRoot) (deps: AcnInsertedFieldDependencies) (isDeferrable: AcnInsertedType -> bool) : Set<ReferenceToType> =
    deferrableGuarded Decode r deps isDeferrable
    |> Map.toList |> List.collect snd
    |> List.choose (fun (_, _, fieldId, prm) -> prm |> Option.map (fun _ -> fieldId))
    |> Set.ofList

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

/// The guard of the dependencies xs, read where the dependent t is coded (bAtReferenceSite: t is the reference site of
/// an OCTET / BIT STRING (CONTAINING X), coded at the call).
let private guardOf (xs: GuardedDependency list) (bAtReferenceSite: bool) : DecodeGuard =
    match xs with
    | [] -> NoGuard
    | xs ->
        let steps = xs |> List.collect (fun (_, steps, _, _) -> steps) |> List.distinct
        let fields = xs |> List.map (fun (_, _, fieldId, _) -> fieldId) |> List.distinct
        let alwaysAbsent =
            steps |> List.exists (fun (_, opt) ->
                match opt with
                | AlwaysAbsent                  -> true
                | Optional _ | AlwaysPresent    -> false)
        match alwaysAbsent with
        | true  -> HolderAlwaysAbsent (steps |> List.map fst, fields)
        | false ->
            let holders =
                xs |> List.collect (fun (_, steps, _, prm) ->
                    match prm with
                    | None   -> steps |> List.map fst
                    | Some _ -> [])
                |> List.distinct
            // the dependent of an OCTET / BIT STRING (CONTAINING X) is the reference site, coded at the call
            let refs =
                xs |> List.choose (fun (_, steps, _, prm) ->
                    prm |> Option.map (fun prm ->
                        match bAtReferenceSite with
                        | true  -> ArgumentRef prm, steps |> List.map fst
                        | false -> FormalRef prm, steps |> List.map fst))
                |> List.distinctBy (fun (rt, _) -> match rt with FormalRef p | ArgumentRef p -> p.id)
            TestHolders (holders, refs, fields)

let private isReferenceKind (kind: Asn1TypeKind) : bool =
    match kind with
    | ReferenceType _ -> true
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | NullType _ | TimeType _
    | BitString _ | Boolean _ | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | Choice _ -> false

/// The type of the node at `path` (MD :: TA :: ...), walking the type assignment through SEQUENCE components, CHOICE
/// alternatives and SEQUENCE OF elements (references by their actual type); the node's own type (a reference site is
/// a ReferenceType). None when a step does not match.
let private componentTypeAt (r: AstRoot) (path: ScopeNode list) : Asn1Type option =
    let rec walk (t: Asn1Type) (nodes: ScopeNode list) : Asn1Type option =
        match nodes, t.ActualType.Kind with
        | [], _ -> Some t
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

/// The DecodeGuard of the decoder of t (NoGuard for every type that does not read a determinant whose holder may be
/// absent).
let decodeGuard (r: AstRoot) (deps: AcnInsertedFieldDependencies) (isDeferrable: AcnInsertedType -> bool) (t: Asn1Type) : DecodeGuard =
    let xs = (deferrableGuarded Decode r deps isDeferrable).TryFind t.id |> Option.defaultValue [] |> List.filter (fun (d, _, _, _) -> readsTheDeterminant t.Kind d)
    guardOf xs (isReferenceKind t.Kind)

/// q/issues.md Issue 108 (rule R4): the guard of the encoder of t, the same dependencies as its decoder's (the size,
/// CHOICE and CONTAINING length dependents), tested on the value: a holder that is absent in the value, or a ref whose
/// field the encoder did not write (`in_stream`, set by InitDet), with the dependent present fails before the dependent
/// is written. A present-when BOOLEAN dependent is guarded where its SEQUENCE encodes it (presenceEncodeGuard).
let encodeGuard (r: AstRoot) (deps: AcnInsertedFieldDependencies) (isDeferrable: AcnInsertedType -> bool) (t: Asn1Type) : DecodeGuard =
    let all = (deferrableGuarded Encode r deps isDeferrable).TryFind t.id |> Option.defaultValue []
    // the holders that the guard of t's own presence (presenceEncodeGuard, emitted around t's encoding) already tests:
    // a second test of them inside could never fail
    let presenceHolders =
        all
        |> List.filter (fun (d, _, _, _) ->
            match d.dependencyKind with
            | AcnDepPresenceBool -> true
            | AcnDepIA5StringSizeDeterminant _ | AcnDepSizeDeterminant _ | AcnDepSizeDeterminant_bit_oct_str_contain _
            | AcnDepRefTypeArgument _ | AcnDepPresence _ | AcnDepPresenceStr _ | AcnDepChoiceDeterminant _ -> false)
        |> List.collect (fun (_, steps, _, _) -> steps |> List.map fst)
        |> Set.ofList
    // --acn-v2: a reference site encoded by a call of its specialised function (the resolved type has ACN parameters)
    // whose own size / CHOICE determinant is read through a ref: the test of that ref is at the call (the ref the call
    // passes, as for a CONTAINING size), before the site's alignment, not inside the callee (whose body, the resolved
    // type with the site's id, would pad first)
    let tested ((d, _, _, prm): GuardedDependency) =
        let atCall =
            r.args.acnDeferred && prm.IsSome &&
            (match d.dependencyKind with
             | AcnDepPresenceBool | AcnDepRefTypeArgument _ -> false
             | AcnDepIA5StringSizeDeterminant _ | AcnDepSizeDeterminant _ | AcnDepSizeDeterminant_bit_oct_str_contain _
             | AcnDepPresence _ | AcnDepPresenceStr _ | AcnDepChoiceDeterminant _ -> true) &&
            (match componentTypeAt r d.asn1Type.ToScopeNodeList |> Option.map (fun ct -> ct.Kind) with
             | Some (ReferenceType rf) -> not rf.resolvedType.acnParameters.IsEmpty
             | Some _ | None           -> false)
        match isReferenceKind t.Kind, atCall with
        | true, true   -> true
        | false, true  -> false
        | _, false     -> readsTheDeterminant t.Kind d
    let xs =
        all
        |> List.filter tested
        |> List.filter (fun (_, steps, _, _) -> not (steps |> List.forall (fun (h, _) -> presenceHolders.Contains h)))
    guardOf xs (isReferenceKind t.Kind)

/// q/issues.md Issue 108: the guard of a SEQUENCE component (childId) that is present when a BOOLEAN field is TRUE whose
/// holder may be absent: the SEQUENCE encodes the component only when the value holds it, so the guard is emitted there,
/// around the component's encoding (the component's own functions are shared with other sites: a reference site's
/// resolved type, an --acn-v2 specialised function, which cannot see the holder).
let presenceEncodeGuard (r: AstRoot) (deps: AcnInsertedFieldDependencies) (isDeferrable: AcnInsertedType -> bool) (childId: ReferenceToType) : DecodeGuard =
    let xs =
        (deferrableGuarded Encode r deps isDeferrable).TryFind childId |> Option.defaultValue []
        |> List.filter (fun (d, _, _, _) ->
            match d.dependencyKind with
            | AcnDepPresenceBool -> true
            | AcnDepIA5StringSizeDeterminant _ | AcnDepSizeDeterminant _ | AcnDepSizeDeterminant_bit_oct_str_contain _
            | AcnDepRefTypeArgument _ | AcnDepPresence _ | AcnDepPresenceStr _ | AcnDepChoiceDeterminant _ -> false)
    guardOf xs false

/// q/issues.md Issue 108 (rule R4) for the automatic test values: the encoder-guarded dependencies (encodeGuard /
/// presenceEncodeGuard) that the SEQUENCE seqId can check on a test value's map: (dependent, holders). A value whose
/// map holds the dependent but not every holder (an ALWAYS ABSENT holder never) is not valid for ACN (the encoder
/// rejects it). Only where the map has a key for each of them: the dependent, the holders and every component between them
/// and seqId are inline SEQUENCE components (the map has no key for a reference site nor for a SEQUENCE OF element,
/// whose maps are merged, Issue 144; a CHOICE alternative step is left out too), and the field is read in this
/// function (not through an --acn-v2 ref). Other shapes stay valid here; their encoder still rejects the value.
let holderAbsentChecks (r: AstRoot) (deps: AcnInsertedFieldDependencies) (isDeferrable: AcnInsertedType -> bool) (seqId: ReferenceToType) : (ReferenceToType * ReferenceToType list) list =
    let seqPath = seqId.ToScopeNodeList
    // every node from seqId (exclusive) to id (inclusive) is an inline SEQUENCE component
    let keyedBelow (id: ReferenceToType) =
        let path = id.ToScopeNodeList
        path.Length > seqPath.Length && List.take seqPath.Length path = seqPath &&
            [seqPath.Length + 1 .. path.Length] |> List.forall (fun n ->
                let sub = List.take n path
                match List.last sub with
                | SEQ_CHILD _ ->
                    match componentTypeAt r sub with
                    | Some ct -> not (isReferenceKind ct.Kind)
                    | None    -> false
                | MD _ | TA _ | VA _ | PRM _ | CH_CHILD _ | SQF -> false)
    deferrableGuarded Encode r deps isDeferrable
    |> Map.toList |> List.collect snd
    |> List.choose (fun (d, steps, _, prm) ->
        match prm with
        | Some _ -> None
        | None   ->
            let holders = steps |> List.map fst
            // an OPTIONAL NULL component is never in the automatic test values (DAstInitialize skips it; C leaves its
            // flag unset, Ada's T_Init sets it): its presence cannot be read from the map
            let isNull (id: ReferenceToType) =
                match componentTypeAt r id.ToScopeNodeList |> Option.map (fun ct -> ct.ActualType.Kind) with
                | Some (NullType _) -> true
                | Some _ | None     -> false
            match keyedBelow d.asn1Type && not (isNull d.asn1Type) && holders |> List.forall keyedBelow with
            | true  -> Some (d.asn1Type, holders)
            | false -> None)
    |> List.distinct

/// Whether the decoder of t has a guard (DecodeOutcome: the part may fail).
let hasDecodeGuard (r: AstRoot) (deps: AcnInsertedFieldDependencies) (isDeferrable: AcnInsertedType -> bool) (t: Asn1Type) : bool =
    match decodeGuard r deps isDeferrable t with
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
            match holderStepsWith NoDominance r deps d, fieldAndReader r deps d with
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

/// --acn-v2 (q/issues.md Issue 107): every (deepest common ancestor of field and reader, field, holders) of a field read
/// outside the component holding it, by any dependency (an ACN argument, a sibling consumer, a consumer in a callee
/// through a ref, the consumer of a field an OPTIONAL referenced child exposes), when the ancestor is executed per
/// SEQUENCE OF element (an SQF step in its path). Structural: no same-flag dominance (holderStepsOf false).
let private refResetsCache =
    System.Runtime.CompilerServices.ConditionalWeakTable<AcnInsertedFieldDependencies, (ReferenceToType * ReferenceToType * ReferenceToType list) list>()
let private deferredRefReads (r: AstRoot) (deps: AcnInsertedFieldDependencies) =
    refResetsCache.GetValue(deps, fun deps ->
        match r.args.acnDeferred with
        | false -> []
        | true ->
            deps.acnDependencies
            |> List.choose (fun d ->
                let read =
                    match fieldAndReader r deps d, refFieldAndReader r deps d with
                    | Some (fieldId, readerId), _       -> Some (fieldId, readerId)
                    | None, Some (fieldId, readerId, _) -> Some (fieldId, readerId)
                    | None, None                        -> None
                read |> Option.bind (fun (fieldId, readerId) ->
                    match holderStepsOf NoDominance r deps fieldId readerId with
                    | [] -> None
                    | steps ->
                        let fieldPath, readerPath = fieldId.ToScopeNodeList, readerId.ToScopeNodeList
                        let common =
                            List.zip (List.truncate readerPath.Length fieldPath) (List.truncate fieldPath.Length readerPath)
                            |> List.takeWhile (fun (x, y) -> x = y) |> List.length
                        let ancestor = List.take common fieldPath
                        match ancestor |> List.exists (function SQF -> true | _ -> false) with
                        | true  -> Some (ReferenceToType ancestor, fieldId, steps |> List.map fst)
                        | false -> None)))

/// --acn-v2 (Issue 107): the fields (ids, with their holders) whose AcnInsertedFieldRef the SEQUENCE seqId resets at
/// its start (deferredRefReads): the ref is declared once before the SEQUENCE OF loop, so element k would see element
/// k - 1's `in_stream` / `is_set` / `value` when its holder is absent (the encoder patched at the old position, the
/// decoder read the old value). The caller keeps the fields held in a local ref of its function.
let deferredRefResets (r: AstRoot) (deps: AcnInsertedFieldDependencies) (seqId: ReferenceToType) : (ReferenceToType * ReferenceToType list) list =
    deferredRefReads r deps
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
        |> List.filter (fun d -> not (holderStepsWith NoDominance r deps d).IsEmpty)
        |> List.choose (fun d -> fieldAndReader r deps d |> Option.map fst)
        // a local shared by two ACN fields keeps master's single declaration (collidingFields)
        |> List.filter (fun fieldId -> not ((collidingFields deps).Contains fieldId))
        |> Set.ofList)
