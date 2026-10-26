/// ACN deferred patching backend module.
/// When --acn-deferred is active, ACN determinants reserve space (InitDet)
/// in the parent SEQUENCE and child functions patch values (PatchDet) via
/// AcnInsertedFieldRef* parameters.
/// When --acn-deferred is not active, delegates to the original inline versions.
module DAstACNDeferred

open System
open System.Numerics

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language

open AcnHelpers
open AcnDependencies


/// Name of the AcnInsertedFieldRef local that holds the ACN field `identity`
/// (AcnExternalField.determinantIdentity) in the function that declares it. Every
/// place that declares, patches or passes such a local must use this function
/// (q/issues.md Issue 52).
let deferredDetRefName (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (identity: ReferenceToType) : string =
    AcnExternalField.deferredDetRefName r lm identity


/// Compute the value expression for a PatchDet call, based on the
/// dependency kind.  Returns (preBlock option, valueExpr) where preBlock
/// is an optional block of code to prepend (e.g., a switch/case that
/// computes the value into a local variable).
///
/// E.g., C   AcnDepSizeDeterminant: (None, "pVal->buffer.nCount")
///       Ada AcnDepSizeDeterminant: (None, "val.buffer.Length")
///       AcnDepPresence:            (Some "<switch block>", "patchDetVal")
///       AcnDepChoiceDeterminant:   (Some "<switch block>", "patchDetVal")
///
/// Identifier "patchDetVal" / "patchDetStrVal" must NOT start with an
/// underscore — Ada rejects leading-underscore identifiers and the same
/// names are emitted into both C and Ada output.
///
/// bAfterCheckedBody (coverage step 12e): the block runs only after the
/// encoding of the function body succeeded.  The switch on the kind of a
/// consumer CHOICE then cannot reach its default arm (ASN1SCC-COV-004) when
/// the CHOICE is encoded unconditionally in that body: every node from the
/// boundary to it is a mandatory SEQUENCE component, so its own switch,
/// whose default arm fails, ran on the same selector.
let computePatchDetValueExpr
    (r: Asn1AcnAst.AstRoot)
    (lm: LanguageMacros)
    (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
    (dep: Asn1AcnAst.AcnDependency)
    (boundaryPath: ScopeNode list)
    (specP: CodegenScope)
    (bAfterCheckedBody: bool)
    : (string option * string * string list) =
    // backend names of the children (fields renamed by -fp AUTO)
    let backendType = withBackendChildNames r lm dep.asn1Type
    let fieldPath = backendType.ToScopeNodeList
    let relParts = fieldPath |> List.skip boundaryPath.Length
    // The access to the consumer and the checks of its path (exist of an OPTIONAL component, kind of a CHOICE
    // alternative), as the legacy update builds them (q/issues.md Issue 50): the patch runs only when the consumer
    // exists, else the fallback PatchDet of the producer writes the default.
    let relAccess (parts: ScopeNode list) (isString: bool) =
        match parts with
        | [] -> (specP.accessPath.rootId, lm.lg.getAccess specP.accessPath), []
        | _  ->
            let p, checkPath = getAccessFromScopeNodeList (ReferenceToType (MD "" :: TA "" :: parts)) isString lm specP
            (p.accessPath.joined lm.lg, lm.lg.getAccess p.accessPath), checkPath
    // coverage step 12e: the switch on the kind of a consumer CHOICE after a checked body runs under the checks of the
    // path, which hold whenever the CHOICE's own switch ran (ASN1SCC-COV-004)
    let soCovExempt =
        let reachable = relParts |> List.forall (function SEQ_CHILD _ | CH_CHILD _ -> true | _ -> false)
        match bAfterCheckedBody, reachable with
        | true, true                -> Some (DAstACN.covExemptJustification "ASN1SCC-COV-004")
        | true, false | false, _    -> None
    match dep.dependencyKind with
    | Asn1AcnAst.AcnDepSizeDeterminant (minSize, maxSize, _) ->
        if minSize.acn = maxSize.acn then
            (None, minSize.acn.ToString(), [])
        else
            let (fieldExpr, acc), checks = relAccess relParts false
            (None, lm.acn.getSizeableSize fieldExpr acc false, checks)

    | Asn1AcnAst.AcnDepIA5StringSizeDeterminant _ ->
        let (fieldExpr, _acc), checks = relAccess relParts true
        (None, lm.acn.getStringSize fieldExpr, checks)

    | Asn1AcnAst.AcnDepPresenceBool ->
        // dep.asn1Type points to the OPTIONAL child; parent is one level up
        let parentRelParts = relParts |> List.rev |> List.tail |> List.rev
        let (parentExpr, pAcc), checks = relAccess parentRelParts false
        let childName = ToC (backendType.lastItem)
        (None, lm.acn.acn_deferred_det_value_presence_bool parentExpr pAcc childName, checks)

    | Asn1AcnAst.AcnDepPresence (relPath, chc) ->
        let (choiceExpr, acc), checks = relAccess relParts false
        let kindAccess = lm.acn.acn_deferred_det_kind_access choiceExpr acc
        let varName = lm.lg.acnDeferredTempVarName "patchDetVal"
        let switchItems = chc.children |> List.map (fun ch ->
            let pres = ch.acnPresentWhenConditions |> Seq.find (fun x -> x.relativePath = relPath)
            let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch dep.asn1Type.ModName
            match pres with
            | AcnGenericTypes.PresenceInt (_, intVal) ->
                lm.acn.acn_deferred_det_switch_case_int presentWhenName varName (intVal.Value.ToString())
            | AcnGenericTypes.PresenceStr _ ->
                failwithf "BUG: PresenceStr in AcnDepPresence (should be AcnDepPresenceStr)")
        let switchBlock = lm.acn.acn_deferred_det_switch_int varName kindAccess switchItems soCovExempt
        (Some switchBlock, varName, checks)

    | Asn1AcnAst.AcnDepPresenceStr (relPath, chc, _strType) ->
        let (choiceExpr, acc), checks = relAccess relParts false
        let kindAccess = lm.acn.acn_deferred_det_kind_access choiceExpr acc
        let varName = lm.lg.acnDeferredTempVarName "patchDetStrVal"
        let switchItems = chc.children |> List.map (fun ch ->
            let pres = ch.acnPresentWhenConditions |> Seq.find (fun x -> x.relativePath = relPath)
            let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch dep.asn1Type.ModName
            match pres with
            | AcnGenericTypes.PresenceStr (_, strVal) ->
                lm.acn.acn_deferred_det_switch_case_str presentWhenName varName strVal.Value
            | AcnGenericTypes.PresenceInt _ ->
                failwithf "BUG: PresenceInt in AcnDepPresenceStr")
        let switchBlock = lm.acn.acn_deferred_det_switch_str varName kindAccess switchItems soCovExempt
        (Some switchBlock, varName, checks)

    | Asn1AcnAst.AcnDepChoiceDeterminant (enm, chc, _isOptional) ->
        let (choiceExpr, acc), checks = relAccess relParts false
        let kindAccess = lm.acn.acn_deferred_det_kind_access choiceExpr acc
        let varName = lm.lg.acnDeferredTempVarName "patchDetVal"
        // The value the determinant field itself writes (its own acnEncodeValue): the
        // CHOICE parameter type may be encoded differently (e.g. encode-values on the field).
        let fieldItems = determinantEnumItems deps dep.determinant enm.enm
        let switchItems = chc.children |> List.map (fun ch ->
            let enmItem = fieldItems |> List.find (fun itm -> itm.Name.Value = ch.Name.Value)
            let presentWhenName = ch.presentWhenName
            // Emit the ACN-encoded integer value (not the enum literal name) so
            // the assignment target — det.Value of type Asn1UInt — accepts it
            // without an enum→uint cast (which Ada disallows).
            let enumIntVal = enmItem.acnEncodeValue.ToString()
            lm.acn.acn_deferred_det_switch_case_int presentWhenName varName enumIntVal)
        let switchBlock = lm.acn.acn_deferred_det_switch_int varName kindAccess switchItems soCovExempt
        (Some switchBlock, varName, checks)

    | _ ->
        failwithf "BUG: PatchDet not yet implemented for dependency kind %A" dep.dependencyKind


/// The patch block of a consumer, under the checks of its path (computePatchDetValueExpr).
let guardPatchBlock (lm: LanguageMacros) (checks: string list) (preBlock: string option) (patchCall: string) : string =
    let block =
        match preBlock with
        | None     -> patchCall
        | Some pre -> lm.acn.acn_deferred_det_preblock_wrap pre patchCall
    match checks with
    | []    -> block
    | _     -> lm.acn.checkAccessPath checks block "" ""

/// Follow the RefTypeArgumentDependency chain from a parameter upward
/// through intermediate boundaries until reaching the original
/// AcnChildDeterminant.  Returns the (InitDet, PatchDet, nBitsOpt) function names.
let private findOriginalDeterminantForParam
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (paramId: ReferenceToType) : Asn1AcnAst.AcnChild option =
    let rec follow (pid: ReferenceToType) =
        deps.acnDependencies |> List.tryPick (fun d ->
            match d.dependencyKind with
            | AcnDepRefTypeArgument p when p.id = pid ->
                match d.determinant with
                | Asn1AcnAst.AcnChildDeterminant ac -> Some ac
                | Asn1AcnAst.AcnParameterDeterminant parentPrm ->
                    follow parentPrm.id
            | _ -> None)
    follow paramId

let findDetFunctionsForParam (lm: LanguageMacros) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (paramId: ReferenceToType) : DetFunctionNames option =
    findOriginalDeterminantForParam deps paramId
    |> Option.bind (fun determinant -> lm.lg.getDeferredDetFunctions determinant.Type)

let private getDeferredMappingFunction
        (r: Asn1AcnAst.AstRoot)
        (lm: LanguageMacros)
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (codec: CommonTypes.Codec)
        (paramId: ReferenceToType) : string option * string option * UserDefinedFunction list =
    match findOriginalDeterminantForParam deps paramId with
    | Some determinant ->
        match determinant.Type with
        | Asn1AcnAst.AcnInteger integer ->
            match integer.acnProperties.mappingFunction with
            | Some (MappingFunction (explicitModule, functionName)) ->
                let name = functionName.Value
                let mappingModule =
                    match explicitModule with
                    | Some moduleName -> Some moduleName.Value
                    | None -> AcnPrimitives.getMappingFunctionModule r lm (Some name)
                let declaration =
                    lm.acn.MappingFunctionDeclaration
                        (DAstACN.getDeterminantTypeDefinitionBodyWithinSeq
                            r lm (Asn1AcnAst.AcnChildDeterminant determinant))
                        name codec
                    |> UserMappingFunction
                Some name, mappingModule, [declaration]
            | None -> None, None, []
        | _ -> None, None, []
    | None -> None, None, []


let private isScopePrefix (prefix: ScopeNode list) (full: ScopeNode list) =
    prefix.Length <= full.Length && List.take prefix.Length full = prefix

/// The boundaries (paths of the reference types) that take the ACN field detId as an ACN argument.
let private refArgumentBoundaries (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (detId: ReferenceToType) : ScopeNode list list =
    deps.acnDependencies |> List.choose (fun dep ->
        match dep.determinant, dep.dependencyKind with
        | AcnChildDeterminant d, AcnDepRefTypeArgument _ when d.id = detId -> Some dep.asn1Type.ToScopeNodeList
        | _ -> None)

/// The deferred determinants among the ACN children of a SEQUENCE, from the original AST (before the fold): the
/// children some ACN parameter stands for (AcnExternalField.isDeferredField, matched by id, q/issues.md Issue 52),
/// wherever the reference taking it is (#421): a reference child of the SEQUENCE, a reference inside an inline child
/// (`pl SEQUENCE { choice <detail> }`), beside an inline ancestor (`header SEQUENCE {...}, choice <header.detail>`),
/// or the reference of an enclosing type the field is exposed to. Used by preSeqFunc to provide parent context to
/// createAcnChild.
let collectDeferredDetIdsFromAst (r: Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (seq: Asn1AcnAst.Sequence) : Set<ReferenceToType> =
    if not r.args.acnDeferred then Set.empty
    else
        seq.children
        |> List.choose (fun c -> match c with Asn1AcnAst.AcnChild ac when AcnExternalField.isDeferredField r deps ac.id -> Some ac.id | _ -> None)
        |> Set.ofList

/// Whether a child reference type of the SEQUENCE takes ACN arguments. A SEQUENCE
/// OF child passes the arguments of its element type (the element encoder patches
/// them, once per element), so look through SEQUENCE OF, nested ones included.
let private childrenPassArguments (children: SeqChildInfo list) : bool =
    let rec passesArguments (kind: DAst.Asn1TypeKind) =
        match kind with
        | DAst.ReferenceType rt -> not rt.baseInfo.acnArguments.IsEmpty
        | DAst.SequenceOf so -> passesArguments so.childType.Kind
        | _ -> false
    children |> List.exists (fun c ->
        match c with
        | DAst.Asn1Child ac -> passesArguments ac.Type.Kind
        | DAst.AcnChild _ -> false)


// ---------------------------------------------------------------------------
//  Deferred SEQUENCE function
// ---------------------------------------------------------------------------

/// Deferred version of createSequenceFunction.
/// Pre-processes the children list: for direct ACN children that are deferred
/// determinants (collectDeferredDetIdsFromAst), replaces their encoding with
/// InitDet into the AcnInsertedFieldRef that holds them and marks them as
/// NullType to bypass _is_initialized checks. The locals passed to child
/// references are declared by the references' callers (buildCallerWrapper).
/// Then delegates to the inline version with the modified children list.
let private createDeferredSequenceFunction
        (r:Asn1AcnAst.AstRoot)
        (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm:LanguageMacros)
        (codec:CommonTypes.Codec)
        (t:Asn1AcnAst.Asn1Type)
        (o:Asn1AcnAst.Sequence)
        (typeDefinition:TypeDefinitionOrReference)
        (isValidFunc: IsValidFunction option)
        (children:SeqChildInfo list)
        (acnPrms:DastAcnParameter list)
        (us:State) =

    let typePrefix = r.args.TypePrefix
    // The deferred determinants, matched by id (q/issues.md Issue 52): the direct
    // ACN children some ACN parameter stands for (collectDeferredDetIdsFromAst).
    // One the function exposes to an enclosing type is held in the formal
    // parameter standing for it (fieldOwnParam, also for a field of an inline
    // child); every other one in a local of this function (deferredDetRefName).
    let isDeferred (detId: ReferenceToType) = AcnExternalField.isDeferredField r deps detId
    let ownParam (detId: ReferenceToType) = AcnExternalField.fieldOwnParam r deps detId
    let hasDeferredChildren =
        children |> List.exists (function DAst.AcnChild ac -> isDeferred ac.id | DAst.Asn1Child _ -> false)

    // The AcnInsertedFieldRef local of a deferred determinant lives in the encoding
    // function of its SEQUENCE, which also holds the inline SEQUENCE children. Its
    // fallback and sibling-consumer patches must run after every reference that
    // takes it as an argument, i.e. at the end of the innermost SEQUENCE holding
    // the determinant and all of them (#421): a determinant of an inline child
    // passed beside it is patched here, not at the end of that child.
    let parentPath = t.id.ToScopeNodeList
    let patchedHere (determinantParent: ScopeNode list) (detId: ReferenceToType) =
        let boundaries = refArgumentBoundaries deps detId
        boundaries |> List.forall (isScopePrefix parentPath)
        && (determinantParent = parentPath
            || not (boundaries |> List.forall (isScopePrefix (List.take (parentPath.Length + 1) determinantParent))))
    // ACN children of the inline SEQUENCE descendants (through SEQUENCE components
    // only: a reference is its own function, which receives the field as a parameter)
    let rec inlineDescendantAcnChildren (seq: Asn1AcnAst.Sequence) : (ScopeNode list * Asn1AcnAst.AcnChild) list =
        seq.children |> List.collect (fun c ->
            match c with
            | Asn1AcnAst.Asn1Child ch ->
                match ch.Type.Kind with
                | Asn1AcnAst.Sequence inner ->
                    let innerPath = ch.Type.id.ToScopeNodeList
                    (inner.children |> List.choose (function Asn1AcnAst.AcnChild ac -> Some (innerPath, ac) | _ -> None))
                    @ inlineDescendantAcnChildren inner
                | _ -> []
            | Asn1AcnAst.AcnChild _ -> [])
    let adoptedDeterminants =
        inlineDescendantAcnChildren o
        |> List.filter (fun (determinantParent, ac) ->
            not (List.isEmpty (refArgumentBoundaries deps ac.id)) && (ownParam ac.id).IsNone && patchedHere determinantParent ac.id)
        |> List.map snd

    // If no deferred determinants, use the inline version as-is
    if not (childrenPassArguments children) && t.acnParameters.IsEmpty && not hasDeferredChildren && adoptedDeterminants.IsEmpty then
        DAstACN.createSequenceFunction_inline r deps lm codec t o typeDefinition isValidFunc children acnPrms None us
    else
        // Consumers of a determinant other than the reference arguments that
        // make it deferred: siblings in this SEQUENCE sized, selected or made
        // present by it.
        let siblingConsumers (detId: ReferenceToType) =
            deps.acnDependencies |> List.filter (fun dep ->
                match dep.determinant, dep.dependencyKind with
                | AcnChildDeterminant d, kind when d.id = detId ->
                    (match kind with AcnDepRefTypeArgument _ -> false | _ -> true)
                | _ -> false)
        let hasSiblingConsumers detId = not (List.isEmpty (siblingConsumers detId))

        // The local deferred determinants whose fallback and sibling-consumer
        // patches this SEQUENCE emits: its own (not formal parameters, not
        // passed beside it) and the adopted ones of its inline children.
        let localPatchedDets : (ReferenceToType * Asn1AcnAst.AcnInsertedType) list =
            (children |> List.choose (fun child ->
                match child with
                | DAst.AcnChild ac when isDeferred ac.id
                                     && (ownParam ac.id).IsNone
                                     && patchedHere parentPath ac.id ->
                    Some (ac.id, ac.Type)
                | _ -> None))
            @ (adoptedDeterminants |> List.map (fun ac -> (ac.id, ac.Type)))

        // Modify direct ACN children that are deferred determinants
        let modifiedChildren =
            children |> List.map (fun child ->
                match child with
                | DAst.AcnChild ac when isDeferred ac.id ->
                    let detFuncs = lm.lg.getDeferredDetFunctions ac.Type
                    match detFuncs with
                    | Some (initFuncName, _patchFuncName, nBitsOpt, _uperMinOffset) ->
                        // Replace encoding with InitDet, keep original decode:
                        // - Encode: emits Acn_InitDet_XXX call
                        // - Decode: keeps original funcBody but redirects target
                        //   to det->value (so decode writes into the struct's
                        //   value field, not into a nonexistent local variable)
                        // - funcUpdateStatement = None (value patched by consumer)
                        // - Type = AcnNullType (bypasses _is_initialized check)
                        // A field exposed to an enclosing type is held in the formal
                        // AcnInsertedFieldRef* standing for it; any other one in the
                        // local named after it (matches buildCallerWrapper).
                        let isOwnParam, detVarName =
                            match ownParam ac.id with
                            | Some prmId -> true, DAstACN.getAcnDeterminantName prmId
                            | None       -> false, deferredDetRefName r lm ac.id
                        let originalFuncBody = ac.funcBody
                        // Determinant ICD comment(s) ("size determinant for ...",
                        // "reference determinant for ...") normally reach the row via
                        // funcUpdateStatement.icdComments, but a deferred child carries
                        // no funcUpdateStatement (its value is patched later).
                        // Recompute them here from the dependency list — the same
                        // comment text the legacy inline path uses — and graft them
                        // onto the borrowed icd row so the --acn-v2 ICD matches legacy
                        // (roadmap B8).  ICD-only; no codegen effect.
                        //
                        // Closure conversion may have rewritten a CONTAINING
                        // external-field size determinant (legacy: a direct
                        // AcnDepSizeDeterminant_bit_oct_str_contain) into a
                        // RefTypeArgument pointing at a synthesized size parameter.
                        // Follow that one hop back so the comment reads "size
                        // determinant for <payload>" exactly like legacy, while
                        // leaving genuine reference determinants (arguments the user
                        // wrote for real ACN parameters, e.g. buffer-len) as
                        // "reference determinant for ...".
                        let commentFor (d: AcnDependency) =
                            match d.dependencyKind with
                            | AcnDepRefTypeArgument prm ->
                                let containingSizeDep =
                                    deps.acnDependencies |> List.tryFind (fun d2 ->
                                        d2.determinant.id = prm.id &&
                                        (match d2.dependencyKind with AcnDepSizeDeterminant_bit_oct_str_contain _ -> true | _ -> false))
                                match containingSizeDep with
                                | Some d2 -> AcnDependencies.icdCommentsForDependency d2
                                | None    -> AcnDependencies.icdCommentsForDependency d
                            | _ -> AcnDependencies.icdCommentsForDependency d
                        let detComments =
                            deps.acnDependencies
                            |> List.filter (fun d -> d.determinant.id = ac.id)
                            |> List.collect commentFor
                        let deferredFuncBody : CommonTypes.Codec -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CodegenScope -> string -> (AcnFuncBodyResult option) =
                            fun innerCodec acnArgs nestingScope p bsPos ->
                                match innerCodec with
                                | CommonTypes.Codec.Encode ->
                                    // "value" variant: det is a local variable → pass &name
                                    // "ptr" variant: det is a formal parameter → pass name as-is
                                    let initCode =
                                        match nBitsOpt with
                                        | Some nBits ->
                                            if isOwnParam then
                                                lm.acn.acn_deferred_det_init_ptr_with_size initFuncName nBits detVarName innerCodec
                                            else
                                                lm.acn.acn_deferred_det_init_value_with_size initFuncName nBits detVarName innerCodec
                                        | None ->
                                            if isOwnParam then
                                                lm.acn.acn_deferred_det_init_ptr initFuncName detVarName innerCodec
                                            else
                                                lm.acn.acn_deferred_det_init_value initFuncName detVarName innerCodec
                                    // Borrow the original encode body's icd
                                    // entry so the determinant still shows up
                                    // as a row in the parent SEQUENCE's ICD —
                                    // the wire format is unchanged (still N
                                    // bits at the same position), only the
                                    // computation timing differs (InitDet
                                    // reserves, PatchDet fills in later).
                                    // The second funcBody evaluation exists solely to
                                    // harvest the icd row; skip it entirely when the ICD
                                    // is not requested (roadmap B8 perf note).
                                    let originalIcd =
                                        match r.args.generateAcnIcd with
                                        | false -> None
                                        | true  ->
                                            originalFuncBody innerCodec acnArgs nestingScope p bsPos
                                            |> Option.bind (fun rr -> rr.icdResult)
                                            |> Option.map (fun aux ->
                                                match detComments with
                                                | [] -> aux
                                                | _  -> { aux with rowsFunc = fun n pres extra -> aux.rowsFunc n pres (extra @ detComments) })
                                    Some {
                                        AcnFuncBodyResult.funcBody = initCode
                                        errCodes = []
                                        localVariables =
                                            if isOwnParam then []
                                            else [GenericLocalVariable { name = detVarName; varType = lm.acn.acn_deferred_det_type_name(); arrSize = None; isStatic = false; initExp = Some (lm.acn.acn_deferred_det_init_expr()) }]
                                        bValIsUnReferenced = false
                                        bBsIsUnReferenced = false
                                        resultExpr = None
                                        auxiliaries = []
                                        icdResult = originalIcd
                                        userDefinedFunctions = []; encOutcome = EncodeOutcome.unknown
                                    }
                                | CommonTypes.Codec.Decode ->
                                    // Keep original decode funcBody but redirect
                                    // the target to det->value.  The original
                                    // funcBody generates e.g.
                                    //   Acn_Dec_Int_...(pBitStrm, &(<childP>))
                                    // By changing childP's accessPath to "det->value",
                                    // the decode writes into the struct's value field:
                                    //   Acn_Dec_Int_...(pBitStrm, &(det->value))
                                    //
                                    // For AcnInteger with fat types (asn1SccUint/asn1SccSint)
                                    // this works directly.  Slim types (uint8_t, uint16_t, etc.)
                                    // need temp_copy because the decode function suffix
                                    // selects a variant expecting the slim pointer type.
                                    // For AcnBoolean and AcnReferenceToEnumerated, the
                                    // funcBody generates intermediate variables with names
                                    // derived from the target path.  A path containing
                                    // "->" or "." corrupts those variable names, so we
                                    // decode to a clean temp variable and copy afterward.
                                    // A determinant that siblings of this SEQUENCE
                                    // also consume (e.g. the size of a direct
                                    // OCTET STRING child) must stay readable under
                                    // the name those siblings' decoders use,
                                    // ac.c_name: decode into that variable and copy
                                    // it to det.value (temp_copy).
                                    let redirectKind =
                                        match ac.Type with
                                        | _ when not isOwnParam && hasSiblingConsumers ac.id -> "temp_copy"
                                        | Asn1AcnAst.AcnInsertedType.AcnInteger ai ->
                                            let intClass = Asn1AcnAstUtilFunctions.getAcnIntegerClass r.args ai
                                            match intClass with
                                            | Asn1AcnAst.ASN1SCC_UInt _
                                            | Asn1AcnAst.ASN1SCC_UInt64 _ -> "direct_value"
                                            // signed: the value field is unsigned (det.value is asn1SccUint /
                                            // Asn1UInt), so decode to a signed temp and copy its bits
                                            | _ -> "temp_copy"
                                        | Asn1AcnAst.AcnInsertedType.AcnReferenceToIA5String _ -> "direct_str_value"
                                        | _ -> "temp_copy"
                                    // the AST root's arguments, for the integer class of the determinant
                                    // below (r is shadowed by the function body result there)
                                    let astArgs = r.args
                                    match redirectKind with
                                    | "direct_value" ->
                                        let valueTarget =
                                            if isOwnParam then lm.acn.acn_deferred_det_access_ptr detVarName
                                            else lm.acn.acn_deferred_det_access_value detVarName
                                        let modifiedP = {p with accessPath = AccessPath.valueEmptyPath valueTarget}
                                        let result = originalFuncBody innerCodec acnArgs nestingScope modifiedP bsPos
                                        match result with
                                        | Some r ->
                                            let extraLvars =
                                                if isOwnParam then []
                                                else [GenericLocalVariable { name = detVarName; varType = lm.acn.acn_deferred_det_type_name(); arrSize = None; isStatic = false; initExp = Some (lm.acn.acn_deferred_det_init_expr()) }]
                                            Some { r with localVariables = extraLvars @ r.localVariables }
                                        | None -> None
                                    | "direct_str_value" ->
                                        // IA5String: decode directly into det.str_value / det->str_value
                                        let strTarget =
                                            if isOwnParam then lm.acn.acn_deferred_det_access_str_ptr detVarName
                                            else lm.acn.acn_deferred_det_access_str_value detVarName
                                        let modifiedP = {p with accessPath = AccessPath.valueEmptyPath strTarget}
                                        let result = originalFuncBody innerCodec acnArgs nestingScope modifiedP bsPos
                                        match result with
                                        | Some r ->
                                            let extraLvars =
                                                if isOwnParam then []
                                                else [GenericLocalVariable { name = detVarName; varType = lm.acn.acn_deferred_det_type_name(); arrSize = None; isStatic = false; initExp = Some (lm.acn.acn_deferred_det_init_expr()) }]
                                            Some { r with localVariables = extraLvars @ r.localVariables }
                                        | None -> None
                                    | _ ->
                                        // Decode to a clean temp variable, then copy to det.value
                                        let tmpName =
                                            if not isOwnParam && hasSiblingConsumers ac.id then ac.c_name
                                            else ac.c_name + "_tmp"
                                        let tmpVarDecl =
                                            match ac.Type with
                                            | Asn1AcnAst.AcnInsertedType.AcnBoolean _ ->
                                                // Boolean tmp var: emits "flag" in C, "Boolean" in Ada.
                                                // FlagLocalVariable emits adaasn1rtl.BIT (modular type)
                                                // which is wrong here — UPER_Dec_boolean expects
                                                // Asn1Boolean.  BooleanLocalVariable maps to flag/Boolean
                                                // which both decoders accept.
                                                BooleanLocalVariable (tmpName, None)
                                            | _ ->
                                                GenericLocalVariable { name = tmpName; varType = ac.typeDefinitionBodyWithinSeq; arrSize = None; isStatic = false; initExp = None }
                                        let modifiedP = {p with accessPath = AccessPath.valueEmptyPath tmpName}
                                        let result = originalFuncBody innerCodec acnArgs nestingScope modifiedP bsPos
                                        match result with
                                        | Some r ->
                                            let detAccess =
                                                if isOwnParam then lm.acn.acn_deferred_det_access_ptr detVarName
                                                else lm.acn.acn_deferred_det_access_value detVarName
                                            let assignStmt =
                                                match ac.Type with
                                                | Asn1AcnAst.AcnInsertedType.AcnBoolean _ ->
                                                    lm.acn.acn_deferred_det_copy_bool_tmp detAccess tmpName CommonTypes.Codec.Decode
                                                | Asn1AcnAst.AcnInsertedType.AcnReferenceToEnumerated enm
                                                        when enm.enumerated.items |> List.forall (fun itm -> itm.acnEncodeValue = itm.definitionValue) ->
                                                    // For enum tmp, Ada needs <EnumType>'Enum_Rep(tmp).
                                                    // The local var declaration above uses
                                                    // ac.typeDefinitionBodyWithinSeq as the enum type
                                                    // name; reuse it here.
                                                    let enumTypeName = ac.typeDefinitionBodyWithinSeq
                                                    lm.acn.acn_deferred_det_copy_enum_tmp detAccess tmpName enumTypeName CommonTypes.Codec.Decode
                                                | Asn1AcnAst.AcnInsertedType.AcnReferenceToEnumerated enm ->
                                                    // det.value holds what the field writes (its acnEncodeValue, e.g.
                                                    // the item index), which here differs from the enum value: map it.
                                                    let defOrRef =
                                                        let programUnit = if p.modName = ToC enm.modName.Value then None else Some (ToC enm.modName.Value)
                                                        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = programUnit; typedefName = ToC (typePrefix + enm.tasName.Value); definedInRtl = false}
                                                    let cases = enm.enumerated.items |> List.map (fun itm ->
                                                        lm.acn.acn_deferred_det_enum_wire_case (lm.lg.getNamedItemBackendName (Some defOrRef) itm) detAccess (itm.acnEncodeValue.ToString()))
                                                    lm.acn.acn_deferred_det_copy_enum_wire_tmp detAccess tmpName cases CommonTypes.Codec.Decode
                                                | Asn1AcnAst.AcnInsertedType.AcnInteger ai ->
                                                    // det.value is unsigned: a signed determinant is copied with
                                                    // its two's complement bits
                                                    match Asn1AcnAstUtilFunctions.getAcnIntegerClass astArgs ai with
                                                    | Asn1AcnAst.ASN1SCC_Int8 _
                                                    | Asn1AcnAst.ASN1SCC_Int16 _
                                                    | Asn1AcnAst.ASN1SCC_Int32 _
                                                    | Asn1AcnAst.ASN1SCC_Int64 _
                                                    | Asn1AcnAst.ASN1SCC_Int _ ->
                                                        lm.acn.acn_deferred_det_copy_signed_tmp detAccess tmpName CommonTypes.Codec.Decode
                                                    | Asn1AcnAst.ASN1SCC_UInt8 _
                                                    | Asn1AcnAst.ASN1SCC_UInt16 _
                                                    | Asn1AcnAst.ASN1SCC_UInt32 _
                                                    | Asn1AcnAst.ASN1SCC_UInt64 _
                                                    | Asn1AcnAst.ASN1SCC_UInt _ ->
                                                        lm.acn.acn_deferred_det_copy_tmp detAccess tmpName CommonTypes.Codec.Decode
                                                | _ ->
                                                    lm.acn.acn_deferred_det_copy_tmp detAccess tmpName CommonTypes.Codec.Decode
                                            let extraLvars =
                                                [tmpVarDecl] @
                                                (if isOwnParam then []
                                                 else [GenericLocalVariable { name = detVarName; varType = lm.acn.acn_deferred_det_type_name(); arrSize = None; isStatic = false; initExp = Some (lm.acn.acn_deferred_det_init_expr()) }])
                                            Some { r with
                                                        funcBody = r.funcBody + "\n" + assignStmt
                                                        localVariables = extraLvars @ r.localVariables }
                                        | None -> None
                        let dummyNullType = Asn1AcnAst.AcnInsertedType.AcnNullType {
                            Asn1AcnAst.AcnNullType.acnProperties = { NullTypeAcnProperties.encodingPattern = None; savePosition = false }
                            acnAlignment = None
                            acnMaxSizeInBits = ac.Type.acnMaxSizeInBits
                            acnMinSizeInBits = ac.Type.acnMinSizeInBits
                            Location = ac.Name.Location
                            defaultValue = ""
                        }
                        let modifiedAcnChild = {
                            ac with
                                funcBody = deferredFuncBody
                                funcUpdateStatement = None
                                Type = dummyNullType
                        }
                        DAst.AcnChild modifiedAcnChild
                    | None ->
                        // Determinant type not supported for deferred patching — keep original
                        child
                | _ -> child)

        // Determinants that live inside a child reference type are declared
        // as AcnInsertedFieldRef locals by the deferred reference's caller
        // funcBody (createDeferredReferenceFunction), via its localVariables
        // result.  Nothing to do here.

        // Patch determinants exposed by a referenced child and consumed by a
        // sibling in this SEQUENCE. The child reserved their wire position;
        // this scope owns both the local AcnInsertedFieldRef and the value that
        // determines it.
        let producerPatchEpilogue =
            match codec with
            | CommonTypes.Codec.Decode -> None
            | CommonTypes.Codec.Encode ->
                let parentPath = t.id.ToScopeNodeList
                let isPrefix (prefix: ScopeNode list) (full: ScopeNode list) =
                    prefix.Length <= full.Length
                    && List.take prefix.Length full = prefix
                let producerLinks =
                    deps.acnDependencies
                    |> List.choose (fun dep ->
                        match dep.dependencyKind, dep.determinant with
                        | AcnDepRefTypeArgument parameter, AcnChildDeterminant determinant ->
                            let boundaryPath = dep.asn1Type.ToScopeNodeList
                            if boundaryPath.Length = parentPath.Length + 1
                               && isPrefix parentPath boundaryPath then
                                Some (boundaryPath, determinant, parameter)
                            else None
                        | _ -> None)
                    |> List.distinctBy (fun (_, determinant, _) -> determinant.id)
                match producerLinks with
                | [] -> None
                | _ ->
                    Some (fun root ->
                        producerLinks |> List.choose (fun (boundaryPath, determinant, parameter) ->
                            let producer =
                                children |> List.pick (function
                                    | DAst.Asn1Child child when child.Type.id.ToScopeNodeList = boundaryPath -> Some child
                                    | _ -> None)
                            let consumers =
                                deps.acnDependencies |> List.filter (fun dep ->
                                    dep.determinant.id = parameter.id
                                    && isPrefix parentPath dep.asn1Type.ToScopeNodeList
                                    && not (isPrefix boundaryPath dep.asn1Type.ToScopeNodeList)
                                    && (match dep.dependencyKind with AcnDepRefTypeArgument _ -> false | _ -> true))
                            let patches = consumers |> List.choose (fun dep ->
                                match lm.lg.getDeferredDetFunctions determinant.Type with
                                | None -> None
                                | Some (_initFn, patchFn, nBitsOpt, uperMinOffset) ->
                                    let preBlock, rawValueExpr, checks = computePatchDetValueExpr r lm deps dep parentPath root true
                                    let valueExpr =
                                        if uperMinOffset = 0I then rawValueExpr
                                        else lm.acn.acn_deferred_det_uper_offset_sub rawValueExpr (uperMinOffset.ToString())
                                    let detName = deferredDetRefName r lm determinant.id
                                    let errCode = "ERR_ACN_DET_CONSISTENCY_MISMATCH"
                                    let patchCall =
                                        if patchFn.Contains("IA5String") then
                                            match nBitsOpt with
                                            | Some nBits -> lm.acn.acn_deferred_det_patch_value_str patchFn nBits valueExpr detName errCode codec
                                            | None -> failwithf "BUG: IA5String PatchDet requires nChars (nBits) parameter"
                                        else
                                            match nBitsOpt with
                                            | Some nBits -> lm.acn.acn_deferred_det_patch_value_with_size patchFn nBits valueExpr detName errCode codec
                                            | None -> lm.acn.acn_deferred_det_patch_value patchFn valueExpr detName errCode codec
                                    Some (guardPatchBlock lm checks preBlock patchCall))
                            match patches with
                            | [] -> None
                            | _ ->
                                let patchBody = String.concat "\n" patches
                                match producer.Optionality with
                                | Some (Optional _) ->
                                    let parent = root.accessPath.joined lm.lg
                                    let access = lm.lg.getAccess root.accessPath
                                    let name = lm.lg.getAsn1ChildBackendName producer
                                    let checks =
                                        consumers
                                        |> List.choose (fun dep ->
                                            children |> List.tryPick (function
                                                | DAst.Asn1Child child when child.Type.id = dep.asn1Type ->
                                                    match child.Optionality with
                                                    | Some AlwaysAbsent -> None
                                                    | Some (Optional _) -> Some (lm.lg.getAsn1ChildBackendName child)
                                                    | _ -> Some ""
                                                | _ -> None))
                                        |> List.distinct
                                        |> List.map (fun consumer ->
                                            lm.acn.acn_deferred_det_optional_producer_check parent access name consumer "ERR_ACN_DET_CONSISTENCY_MISMATCH")
                                    let guarded =
                                        lm.acn.acn_deferred_det_optional_producer_wrap parent access name patchBody
                                    Some (String.concat "\n" (checks @ [guarded]))
                                | Some AlwaysAbsent -> None
                                | _ -> Some patchBody)
                        |> String.concat "\n")

        // Patch local deferred determinants from their sibling consumers
        // (encode only). The reference arguments' consumers patch them inside
        // the referenced types; a sibling consumer, e.g. `data [size len]`
        // beside `item <len> []`, is encoded here and must set the value too,
        // or be checked against it (ERR_ACN_DET_CONSISTENCY_MISMATCH).
        let localSiblingEpilogue =
            match codec with
            | CommonTypes.Codec.Decode -> None
            | CommonTypes.Codec.Encode ->
                let patches =
                    localPatchedDets |> List.collect (fun (acId, acType) ->
                            match lm.lg.getDeferredDetFunctions acType with
                            | None -> []
                            | Some (_initFn, patchFn, nBitsOpt, uperMinOffset) ->
                                siblingConsumers acId |> List.map (fun dep root ->
                                    let preBlock, rawValueExpr, checks = computePatchDetValueExpr r lm deps dep parentPath root true
                                    let valueExpr =
                                        if uperMinOffset = 0I then rawValueExpr
                                        else lm.acn.acn_deferred_det_uper_offset_sub rawValueExpr (uperMinOffset.ToString())
                                    let detName = deferredDetRefName r lm acId
                                    let errCode = "ERR_ACN_DET_CONSISTENCY_MISMATCH"
                                    let patchCall =
                                        if patchFn.Contains("IA5String") then
                                            match nBitsOpt with
                                            | Some nBits -> lm.acn.acn_deferred_det_patch_value_str patchFn nBits valueExpr detName errCode codec
                                            | None -> failwithf "BUG: IA5String PatchDet requires nChars (nBits) parameter"
                                        else
                                            match nBitsOpt with
                                            | Some nBits -> lm.acn.acn_deferred_det_patch_value_with_size patchFn nBits valueExpr detName errCode codec
                                            | None -> lm.acn.acn_deferred_det_patch_value patchFn valueExpr detName errCode codec
                                    guardPatchBlock lm checks preBlock patchCall))
                match patches with
                | [] -> None
                | _ -> Some (fun root -> patches |> List.map (fun patch -> patch root) |> String.concat "\n")

        // Compute fallback PatchDet code for local deferred dets (encode only).
        // When all consumers of a shared determinant are absent at runtime
        // (e.g., present-when booleans are false, or a CHOICE branch was not
        // taken), no PatchDet is called and the InitDet placeholder (zeros)
        // remains.  The fallback patches with a valid default so the decoder
        // does not reject the bitstream.  See computeFallbackDetValue for
        // detailed rationale.
        //
        // The string is passed to createSequenceFunction_inline via its
        // fallbackEpilogue parameter — appended verbatim inside the encode
        // body's nested if(ret) chain.
        let localFallbackEpilogue =
            match codec with
            | CommonTypes.Codec.Encode ->
                let fallbackDets =
                    localPatchedDets |> List.choose (fun (acId, acType) ->
                            match lm.lg.getDeferredDetFunctions acType with
                            | Some (_initFn, patchFn, nBitsOpt, uperMinOffset) ->
                                let detVarName = deferredDetRefName r lm acId
                                let defaultVal = lm.lg.computeDeferredFallbackValue acType uperMinOffset
                                Some (detVarName, patchFn, nBitsOpt, defaultVal, acType)
                            | None -> None)
                match fallbackDets with
                | [] -> None
                | _ ->
                    let fallbackCode =
                        fallbackDets |> List.map (fun (detVarName, patchFn, nBitsOpt, defaultVal, acnType) ->
                            match acnType with
                            | Asn1AcnAst.AcnInsertedType.AcnInteger _
                            | Asn1AcnAst.AcnInsertedType.AcnBoolean _
                            | Asn1AcnAst.AcnInsertedType.AcnReferenceToEnumerated _ ->
                                match nBitsOpt with
                                | None ->
                                    lm.acn.acn_deferred_det_fallback_value patchFn defaultVal detVarName codec
                                | Some nBits ->
                                    lm.acn.acn_deferred_det_fallback_value_with_size patchFn defaultVal nBits detVarName codec
                            | Asn1AcnAst.AcnInsertedType.AcnReferenceToIA5String _ ->
                                match nBitsOpt with
                                | Some nBits ->
                                    lm.acn.acn_deferred_det_fallback_value_str patchFn defaultVal nBits detVarName codec
                                | None ->
                                    failwithf "BUG: IA5String fallback PatchDet requires nChars (nBits) parameter"
                            | Asn1AcnAst.AcnInsertedType.AcnNullType _ ->
                                failwithf "BUG: AcnNullType should not appear as a deferred determinant"
                        ) |> String.concat "\n"
                    Some (fun _ -> fallbackCode)
            | CommonTypes.Codec.Decode -> None

        let epilogue =
            [producerPatchEpilogue; localSiblingEpilogue; localFallbackEpilogue]
            |> List.choose id
            |> function
               | [] -> None
               | blocks -> Some (fun p -> blocks |> List.map (fun block -> block p) |> String.concat "\n")

        // The deferred determinants of modifiedChildren carry no update statement (their value
        // is patched by the consumers), so check the automatic test values against the original
        // children: CHOICEs sharing a determinant must select the same alternative.
        let acnFunction, ns = DAstACN.createSequenceFunction_inline r deps lm codec t o typeDefinition isValidFunc modifiedChildren acnPrms epilogue us
        {acnFunction with isTestVaseValid = DAstACN.isTestCaseValidForAcnChildren children}, ns


// ---------------------------------------------------------------------------
//  Deferred REFERENCE function
// ---------------------------------------------------------------------------

/// Find which acnParameter is the CONTAINING size determinant by checking
/// the dependency list for AcnDepSizeDeterminant_bit_oct_str_contain.
/// Returns the parameter, or None if not found.
let private findContainingSizeParamInfo
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (o: Asn1AcnAst.ReferenceType) : (AcnGenericTypes.AcnParameter * Asn1AcnAst.ReferenceType) option =
    o.resolvedType.acnParameters |> List.tryPick (fun prm ->
        deps.acnDependencies |> List.tryPick (fun d ->
            if d.determinant.id <> prm.id then None
            else
                match d.dependencyKind with
                | Asn1AcnAst.AcnDepSizeDeterminant_bit_oct_str_contain containing ->
                    Some (prm, containing)
                | _ -> None))

let private findContainingSizeParam deps o =
    findContainingSizeParamInfo deps o |> Option.map fst
            
/// A local AcnInsertedFieldRef for an ACN field that a formal parameter of this
/// function holds would hide the parameter: strip it (the inner callers pass the
/// formal parameter, buildCallerWrapper).
let private stripOwnParamLocals
        (r: Asn1AcnAst.AstRoot)
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm: LanguageMacros)
        (acnParameters: AcnGenericTypes.AcnParameter list)
        (locals: LocalVariable list) : LocalVariable list =
    let ownParamNames =
        acnParameters
        |> List.map (fun prm -> deferredDetRefName r lm (AcnExternalField.paramIdentity r deps prm))
        |> Set.ofList
    let detTypeName = lm.acn.acn_deferred_det_type_name()
    locals |> List.filter (fun lv ->
        match lv with
        | GenericLocalVariable gl ->
            not (gl.varType = detTypeName && Set.contains gl.name ownParamNames)
        | _ -> true)


/// Build a placeholder body for the case where the base type's funcBody
/// returns None (no encoding produced).  Acts as a sentinel: empty statement,
/// no errors, marks both pVal and pBitStrm as unreferenced.
let private emptyBody (lm: LanguageMacros) : AcnFuncBodyResult =
    {   AcnFuncBodyResult.funcBody = lm.lg.emptyStatement
        errCodes = []
        localVariables = []
        bValIsUnReferenced = true
        bBsIsUnReferenced = true
        resultExpr = None
        auxiliaries = []
        icdResult = None
        userDefinedFunctions = []; encOutcome = EncodeOutcome.unknown }


// Constants threaded across the 7 createDeferredReferenceFunction helpers.
// Bundled into a record so each helper takes ctx + only its own variable
// inputs, instead of repeating 8 params per helper signature (per refactor
// plan §5).
type private DeferredRefCtx = {
    r:              Asn1AcnAst.AstRoot
    deps:           Asn1AcnAst.AcnInsertedFieldDependencies
    lm:             LanguageMacros
    codec:          CommonTypes.Codec
    t:              Asn1AcnAst.Asn1Type
    o:              Asn1AcnAst.ReferenceType
    typeDefinition: TypeDefinitionOrReference
    isValidFunc:    IsValidFunction option
    // ICD contribution this reference owes to its parent SEQUENCE/CHOICE.
    // Built from baseType.icdTas the same way the legacy inline path does
    // (see AcnReference.buildReferenceIcdArgAux).  Threading it through the
    // deferred path is what keeps the generated _new.html ICD complete in
    // --acn-v2 mode; without it every closure-converted reference (and
    // everything reachable only through one) disappears from the document.
    refIcd:         IcdArgAux option
}


/// CONTAINING ExternalField, base type has NO standalone function (parameterized
/// type): use funcBody closure + CONTAINING wrapper.  The CONTAINING size
/// parameter was added by closure conversion and is NOT in o.acnArguments —
/// the arguments correspond to the first N parameters; extra params from
/// closure conversion are at the end.
let private buildContainingClosureBody
        (ctx: DeferredRefCtx)
        (specP: CodegenScope)
        (errCode: ErrorCode)
        (baseAcnFunc: AcnFunction)
        (stripLocals: LocalVariable list -> LocalVariable list)
        (ns1: State) : AcnFuncBodyResult * State =
    let containingSizePrm = findContainingSizeParam ctx.deps ctx.o
    let nArgs = ctx.o.acnArguments.Length
    let paramsArgsPairs = List.zip ctx.o.acnArguments (ctx.o.resolvedType.acnParameters |> List.take nArgs)
    let bodyContent, ns2Body = baseAcnFunc.funcBody ns1 paramsArgsPairs (NestingScope.init ctx.t.acnMaxSizeInBits ctx.t.uperMaxSizeInBits []) specP
    // coverage step 12e: the checked selectors of a function body end with it
    let ns2 = {ns2Body with acnSequenceCheckedSelectors = []}
    match bodyContent with
    | None -> emptyBody ctx.lm, ns2
    | Some br ->
        let wrappedBody =
            match containingSizePrm with
            | Some sizePrm ->
                let detParamName = DAstACN.getAcnDeterminantName sizePrm.id
                let mappingFunction, mappingModule, _ =
                    getDeferredMappingFunction ctx.r ctx.lm ctx.deps ctx.codec sizePrm.id
                let patchFnName =
                    match findDetFunctionsForParam ctx.lm ctx.deps sizePrm.id with
                    | Some (_, patchFn, _, _) -> patchFn
                    | None -> ""
                match ctx.o.encodingOptions.Value.octOrBitStr with
                | CommonTypes.ContainedInOctString ->
                    ctx.lm.acn.octet_string_containing_deferred_wrapper br.funcBody detParamName patchFnName mappingFunction mappingModule errCode.errCodeName (Asn1AcnAstUtilFunctions.isLiveDeduced ctx.o.resolvedType) ctx.codec
                | CommonTypes.ContainedInBitString ->
                    ctx.lm.acn.bit_string_containing_deferred_wrapper br.funcBody detParamName patchFnName mappingFunction mappingModule errCode.errCodeName ctx.codec
            | None ->
                br.funcBody  // fallback: no wrapping if size param not found
        let mappingDeclarations =
            containingSizePrm
            |> Option.map (fun prm ->
                let _, _, declarations =
                    getDeferredMappingFunction ctx.r ctx.lm ctx.deps ctx.codec prm.id
                declarations)
            |> Option.defaultValue []
        { br with
            funcBody = wrappedBody
            errCodes = br.errCodes @ [errCode]
            userDefinedFunctions = br.userDefinedFunctions @ mappingDeclarations
            localVariables = stripLocals br.localVariables }, ns2


/// CONTAINING ExternalField, base type has standalone function (e.g. TC01b):
/// use the existing STG template approach.  PatchDet is emitted by the
/// template itself, so step 2b in the caller skips this case.
let private buildContainingStandaloneBody
        (ctx: DeferredRefCtx)
        (specP: CodegenScope)
        (errCode: ErrorCode)
        (baseFncName: string)
        (ns1: State) : AcnFuncBodyResult * State =
    let pp =
        let str = ctx.lm.lg.getParamValue ctx.t specP.accessPath ctx.codec
        match ctx.codec, ctx.lm.lg.decodingKind with
        | Decode, Copy -> ToC str
        | _ -> str
    let sizePrm = ctx.o.resolvedType.acnParameters.Head
    let detParamName = DAstACN.getAcnDeterminantName sizePrm.id
    let mappingFunction, mappingModule, mappingDeclarations =
        getDeferredMappingFunction ctx.r ctx.lm ctx.deps ctx.codec sizePrm.id
    let patchFnName =
        match findDetFunctionsForParam ctx.lm ctx.deps sizePrm.id with
        | Some (_initFn, patchFn, _, _) -> patchFn
        | None -> ""
    let fncBody =
        match ctx.o.encodingOptions.Value.octOrBitStr with
        | CommonTypes.ContainedInOctString ->
            ctx.lm.acn.octet_string_containing_deferred_func pp baseFncName detParamName patchFnName mappingFunction mappingModule errCode.errCodeName (Asn1AcnAstUtilFunctions.isLiveDeduced ctx.o.resolvedType) ctx.codec
        | CommonTypes.ContainedInBitString ->
            ctx.lm.acn.bit_string_containing_deferred_func pp baseFncName detParamName patchFnName mappingFunction mappingModule errCode.errCodeName ctx.codec
    { emptyBody ctx.lm with
        funcBody = fncBody
        errCodes = [errCode]
        userDefinedFunctions = mappingDeclarations
        bValIsUnReferenced = false
        bBsIsUnReferenced = false }, ns1


/// Normal reference (no CONTAINING): call the base type's funcBody closure
/// and apply intermediate-boundary post-processing.
let private buildNormalReferenceBody
        (ctx: DeferredRefCtx)
        (specP: CodegenScope)
        (baseAcnFunc: AcnFunction)
        (stripLocals: LocalVariable list -> LocalVariable list)
        (ns1: State) : AcnFuncBodyResult * State =
    let paramsArgsPairs = List.zip ctx.o.acnArguments ctx.o.resolvedType.acnParameters
    let bodyContent, ns2Body = baseAcnFunc.funcBody ns1 paramsArgsPairs (NestingScope.init ctx.t.acnMaxSizeInBits ctx.t.uperMaxSizeInBits []) specP
    // coverage step 12e: the checked selectors of a function body end with it
    let ns2 = {ns2Body with acnSequenceCheckedSelectors = []}
    match bodyContent with
    | None -> emptyBody ctx.lm, ns2
    | Some br ->
        { br with
            localVariables = stripLocals br.localVariables }, ns2


/// Step 2b — append PatchDet calls (encode only) to a body.  For each
/// consumer-side acnParameter (i.e. ones with a non-RefTypeArg dependency
/// pointing at this parameter), emit a PatchDet call computed from the
/// dependency.  The CONTAINING size parameter is excluded (it is handled
/// by the CONTAINING wrapper itself).
///
/// For the CONTAINING ExternalField + standalone base function path,
/// PatchDet is already produced by the STG template; this helper returns
/// the body unchanged.
let private appendPatchDetCalls
        (ctx: DeferredRefCtx)
        (specP: CodegenScope)
        (isContainingExternalField: bool)
        (baseHasStandaloneFunc: bool)
        (bodyOutcome: EncodeOutcome)
        (body: string) : string * bool =
    match isContainingExternalField, not baseHasStandaloneFunc with
    | true, false -> body, false  // old approach: PatchDet is in STG template
    | _ ->
        match ctx.codec with
        | CommonTypes.Codec.Decode -> body, false
        | CommonTypes.Codec.Encode ->
            // Identify CONTAINING size param to exclude from step 2b.  Always
            // check (not just when isContainingExternalField) because named
            // CONTAINING aliases have two ref levels — the outer ref has no
            // encodingOptions but still carries the same size parameter.
            let containingSizePrmId =
                findContainingSizeParam ctx.deps ctx.o
                |> Option.map (fun prm -> prm.id)
            let patchCalls =
                ctx.o.resolvedType.acnParameters |> List.choose (fun prm ->
                    match containingSizePrmId with
                    | Some csId when csId = prm.id -> None
                    | _ ->
                    // Find the dep inside this boundary where the determinant
                    // is this parameter (consumer-side dependency)
                    let boundaryPath = ctx.o.resolvedType.id.ToScopeNodeList
                    let consumerDep =
                        ctx.deps.acnDependencies |> List.tryFind (fun d ->
                            let dependentPath = d.asn1Type.ToScopeNodeList
                            d.determinant.id = prm.id
                            && boundaryPath.Length <= dependentPath.Length
                            && List.take boundaryPath.Length dependentPath = boundaryPath
                            && (match d.determinant with
                                | Asn1AcnAst.AcnParameterDeterminant _ -> true
                                | _ -> false)
                            // Exclude RefTypeArgDep — these are intermediate
                            // chain links (e.g., CHOICE→case boundary), not
                            // actual consumer deps with data fields.
                            && (match d.dependencyKind with
                                | AcnDepRefTypeArgument _ -> false
                                | _ -> true))
                    match consumerDep with
                    | None -> None
                    | Some dep ->
                        // Skip CONTAINING size deps — handled by CONTAINING wrapper
                        match dep.dependencyKind with
                        | Asn1AcnAst.AcnDepSizeDeterminant_bit_oct_str_contain _ -> None
                        | _ ->
                        match findDetFunctionsForParam ctx.lm ctx.deps prm.id with
                        | None -> None
                        | Some (_initFn, patchFn, nBitsOpt, uperMinOffset) ->
                            let (preBlock, rawValueExpr, checks) = computePatchDetValueExpr ctx.r ctx.lm ctx.deps dep boundaryPath specP bodyOutcome.canFail
                            // For Integer_uPER with min > 0, UPER encodes (value - min)
                            // but ConstSize encodes value directly, so subtract the offset.
                            let valueExpr =
                                if uperMinOffset = 0I then rawValueExpr
                                else ctx.lm.acn.acn_deferred_det_uper_offset_sub rawValueExpr (uperMinOffset.ToString())
                            let detParamName = DAstACN.getAcnDeterminantName prm.id
                            let errCodePatch = "ERR_ACN_DET_CONSISTENCY_MISMATCH"
                            let isIA5StringDet = patchFn.Contains("IA5String")
                            let patchCall =
                                if isIA5StringDet then
                                    match nBitsOpt with
                                    | Some nBits ->
                                        ctx.lm.acn.acn_deferred_det_patch_ptr_str patchFn nBits valueExpr detParamName errCodePatch ctx.codec
                                    | None ->
                                        failwithf "BUG: IA5String PatchDet requires nChars (nBits) parameter"
                                else
                                    match nBitsOpt with
                                    | Some nBits ->
                                        ctx.lm.acn.acn_deferred_det_patch_ptr_with_size patchFn nBits valueExpr detParamName errCodePatch ctx.codec
                                    | None ->
                                        ctx.lm.acn.acn_deferred_det_patch_ptr patchFn valueExpr detParamName errCodePatch ctx.codec
                            Some (guardPatchBlock ctx.lm checks preBlock patchCall, not checks.IsEmpty))
            match patchCalls with
            | [] -> body, false
            | calls ->
                let patchCode = calls |> List.map fst |> String.concat "\n"
                let guarded = calls |> List.exists snd
                // The PatchDet calls write the result: after a body that may fail they run only when it succeeded,
                // else a failed body would return success (Issue 48)
                match bodyOutcome.canFail with
                | true  -> ctx.lm.isvalid.JoinItems2 body patchCode, guarded
                | false -> body + "\n" + patchCode, guarded


/// Step 3 — build the specialized function's deferred-determinant formal
/// parameters.  Returns (formal-param declarations, formal-param names) in
/// the same order, both derived from o.resolvedType.acnParameters via
/// getAcnDeterminantName.
let private buildSpecializedFormalParams
        (ctx: DeferredRefCtx)
        (acnParameters: AcnGenericTypes.AcnParameter list)
        : string list * string list =
    let names = acnParameters |> List.map (fun prm -> DAstACN.getAcnDeterminantName prm.id)
    let formals = names |> List.map (fun n -> ctx.lm.acn.acn_deferred_det_formal_param n ctx.codec)
    formals, names


/// Step 4 — emit the specialized function (definition + body) and assemble
/// the auxiliaries list.  The returned list is bodyResult_auxiliaries
/// followed by the function's def + body strings.
let private emitSpecializedFunctionDecl
        (ctx: DeferredRefCtx)
        (specP: CodegenScope)
        (errCode: ErrorCode)
        (specFuncName: string)
        (deferredFormalParams: string list)
        (deferredParamNames: string list)
        (finalBody: AcnFuncBodyResult)
        : string list =
    let varName = specP.accessPath.rootId
    let sStar = ctx.lm.lg.getStar specP.accessPath
    let typeDefinitionName = ctx.typeDefinition.longTypedefName2 (Some ctx.lm.lg) ctx.lm.lg.hasModules ctx.t.moduleName
    let isValidFuncName = match ctx.isValidFunc with None -> None | Some f -> f.funcName
    let soInitFuncName = ctx.lm.lg.getFuncNameGeneric ctx.typeDefinition (ctx.lm.init.methodNameSuffix())
    // the specialised function's type is usually a reference: its init function is the referenced type's
    let soValInitFuncName =
        match soInitFuncName with
        | Some _ -> soInitFuncName
        | None   -> Some (typeDefinitionName + ctx.lm.init.methodNameSuffix())
    let nMaxBytesInACN = BigInteger (ceil ((double ctx.t.acnMaxSizeInBits)/8.0))
    let lvars = finalBody.localVariables |> List.map(fun (lv:LocalVariable) -> ctx.lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct

    // In deferred mode, pVal may be unreferenced if the resolved type has
    // only ACN-inserted children and no ASN.1 data fields (e.g., Header
    // with only version pattern + buffers-length determinant).
    let hasAsn1Children =
        match ctx.o.resolvedType.Kind with
        | Asn1AcnAst.Asn1TypeKind.Sequence sq ->
            sq.children |> List.exists (fun c -> match c with Asn1AcnAst.Asn1Child _ -> true | _ -> false)
        | _ -> true
    let bVarNameIsUnreferenced = finalBody.bValIsUnReferenced || not hasAsn1Children
    // coverage step 10c: val is `in out` when the decoder may leave it partly written (DecodeOutcome.acnValWrite)
    let bValInOut =
        match ctx.codec with
        | Decode -> DecodeOutcome.valInOut (DecodeOutcome.acnValWrite ctx.r ctx.lm ctx.o.resolvedType) soValInitFuncName && not bVarNameIsUnreferenced
        | Encode -> false

    // coverage step 12a / 12b: as AcnFunctionWrapper.createAcnFunction
    let bodyOutcome = encodeOutcomeOf ctx.codec finalBody.encOutcome
    let bValidityCannotFail = DAstUtilFunctions.encoderSkipsValidation ctx.lm ctx.codec ctx.isValidFunc ctx.t bodyOutcome
    let bValidityCalled = isValidFuncName.IsSome && not bValidityCannotFail
    // coverage step 10b: same result initialisation as AcnFunctionWrapper.createAcnFunction (DecodeOutcome.fs)
    let specBodyContent =
        match ctx.codec, ctx.lm.lg.decodeReadFailureModel with
        | Decode, ReadsCannotFail when DecodeOutcome.acnNeedsResultInit ctx.r ctx.lm ctx.o.resolvedType ->
            ctx.lm.uper.decode_result_init () + "\n" + finalBody.funcBody
        | Decode, ReadsCannotFail
        | Decode, ReadsCanFail -> finalBody.funcBody
        | Encode, _ ->
            match bValidityCalled, bodyOutcome.writes, ctx.lm.uper.decode_result_init () with
            | false, (NoWrite | SometimesWrites | ReadsFirst | WritesUnknown), sInit when sInit <> "" -> sInit + "\n" + finalBody.funcBody
            | _ -> finalBody.funcBody
    let specFuncBody =
        ctx.lm.acn.EmitTypeAssignment_primitive
            varName sStar specFuncName isValidFuncName typeDefinitionName
            lvars specBodyContent
            None null  // soSparkAnnotations, no fallback initializer for finalBody
            deferredFormalParams deferredParamNames
            (ctx.t.acnMaxSizeInBits = 0I) finalBody.bBsIsUnReferenced bVarNameIsUnreferenced
            false
            soValInitFuncName [] [] []  // funcDefAnnots, precondAnnots, postcondAnnots
            true  // bDeferredSpecialization: callers use the specialized body directly
            false // bAlwaysSucceeds: a type with deferred determinants is MayFail (DecodeOutcome.acn)
            bValInOut
            bValidityCannotFail // coverage step 12a
            ctx.codec
        |> ctx.lm.lg.wrapDeferredSpecBody

    let specErrCodStr =
        (errCode :: finalBody.errCodes)
        |> List.groupBy (fun x -> x.errCodeName)
        |> List.map (fun (k, v) -> {errCodeName = k; errCodeValue = v.Head.errCodeValue; comment = v.Head.comment; fieldPath = v.Head.fieldPath})
        |> List.map (fun x -> ctx.lm.acn.EmitTypeAssignment_def_err_code x.errCodeName (BigInteger x.errCodeValue) x.comment x.fieldPath)
        |> List.distinct

    let specFuncDef =
        ctx.lm.acn.EmitTypeAssignment_primitive_def
            varName sStar specFuncName typeDefinitionName
            specErrCodStr
            (ctx.t.acnMaxSizeInBits = 0I) nMaxBytesInACN ctx.t.acnMaxSizeInBits
            deferredFormalParams
            None  // soSparkAnnotations
            bValInOut
            true  // bDeferredSpecialization: no public wrapper (coverage step 13a), callers use the specialized body
            ctx.codec
        |> ctx.lm.lg.wrapDeferredSpecBody

    finalBody.auxiliaries @ [specFuncDef; specFuncBody]


/// Step 5 — build the caller's funcBody (a simple call to the specialized
/// function with &det actual parameters), record the cross-TAS function-call
/// dependency, and wrap the result in an AcnFunction via createAcnFunction.
let private buildCallerWrapper
        (ctx: DeferredRefCtx)
        (specFuncName: string)
        (allAuxiliaries: string list)
        (udfcs: UserDefinedFunction list)
        (ns: State)
        : AcnFunction option * State =
    let callerFuncBody (callerUs:State) (callerErrCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (_nestingScope: NestingScope) (p:CodegenScope) =
        let pp, resultExpr =
            let str = ctx.lm.lg.getParamValue ctx.t p.accessPath ctx.codec
            match ctx.codec, ctx.lm.lg.decodingKind with
            | Decode, Copy ->
                let toc = ToC str
                toc, Some toc
            | _ -> str, None

        // For each parameter of the specialized function, the ACN field it
        // stands for (AcnExternalField.paramIdentity, q/issues.md Issue 52):
        //   - a formal parameter of the enclosing function holds the field
        //     (pointer pass-through) → pass it.  No local declaration needed.
        //   - otherwise → pass &local, the local AcnInsertedFieldRef named after
        //     the field (deferredDetRefName), declared in the caller's scope:
        //     the producer and every consumer of the field in this function
        //     use the same local, and fields with the same name do not share one.
        let extraActualParams, declLevelLocalNames =
            ctx.o.resolvedType.acnParameters
            |> List.fold (fun (paramsAcc, localsAcc) resolvedParam ->
                let identity = AcnExternalField.paramIdentity ctx.r ctx.deps resolvedParam
                let parentParam =
                    acnArgs |> List.tryFind (fun (_, prm) -> AcnExternalField.paramIdentity ctx.r ctx.deps prm = identity)
                match parentParam with
                | Some (_, prm) ->
                    let pStr = DAstACN.getAcnDeterminantName prm.id
                    paramsAcc @ [pStr], localsAcc
                | None ->
                    let cName = deferredDetRefName ctx.r ctx.lm identity
                    let pStr = ctx.lm.acn.acn_deferred_det_actual_param cName ctx.codec
                    paramsAcc @ [pStr], localsAcc @ [cName]
            ) ([], [])

        let baseFuncCall = callBaseTypeFunc ctx.lm pp specFuncName ctx.codec
        let funcBodyContent = insertActualParams baseFuncCall extraActualParams

        // Declaration-level AcnInsertedFieldRef locals.  Distinct in case two
        // parameters stand for the same field; duplicates would be deduped
        // later anyway, but we save work by deduping here.
        let extraLocals =
            declLevelLocalNames
            |> List.distinct
            |> List.map (fun cName ->
                GenericLocalVariable {
                    name = cName
                    varType = ctx.lm.acn.acn_deferred_det_type_name()
                    arrSize = None
                    isStatic = false
                    initExp = Some (ctx.lm.acn.acn_deferred_det_init_expr())
                })

        Some ({AcnFuncBodyResult.funcBody = funcBodyContent
               errCodes = [callerErrCode]
               localVariables = extraLocals
               userDefinedFunctions = udfcs
               bValIsUnReferenced = false
               bBsIsUnReferenced = false
               resultExpr = resultExpr
               auxiliaries = allAuxiliaries
               icdResult = ctx.refIcd; encOutcome = EncodeOutcome.unknown}), callerUs

    // Record the function-call dependency (caller → callee)
    let ns3 =
        match ctx.t.id.topLevelTas with
        | None -> ns
        | Some tasInfo ->
            let caller = {Caller.typeId = tasInfo; funcType=AcnEncDecFunctionType}
            let callee = {Callee.typeId = {TypeAssignmentInfo.modName = ctx.o.modName.Value; tasName=ctx.o.tasName.Value} ; funcType=AcnEncDecFunctionType}
            addFunctionCallToState ns caller callee

    let soSparkAnnotations = Some(DAstACN.sparkAnnotations ctx.lm (ctx.typeDefinition.longTypedefName2 (Some ctx.lm.lg) ctx.lm.lg.hasModules ctx.t.moduleName) ctx.codec)
    let a, ns4 = DAstACN.createAcnFunction ctx.r ctx.deps ctx.lm ctx.codec ctx.t ctx.typeDefinition ctx.isValidFunc callerFuncBody (fun _atc -> true) soSparkAnnotations [] ns3
    Some a, ns4


/// Deferred version of createReferenceFunction.
/// When the resolved type has acnParameters (from closure conversion),
/// generates a specialized function per reference site with
/// AcnInsertedFieldRef* formal parameters, and returns an AcnFunction
/// whose funcBody is a simple call to the specialized function.
let private createDeferredReferenceFunction
        (r:Asn1AcnAst.AstRoot)
        (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm:LanguageMacros)
        (codec:CommonTypes.Codec)
        (t:Asn1AcnAst.Asn1Type)
        (o:Asn1AcnAst.ReferenceType)
        (typeDefinition:TypeDefinitionOrReference)
        (isValidFunc: IsValidFunction option)
        (baseType:Asn1Type)
        (us:State) =

    let _baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t "_ACN" codec

    // ICD this reference contributes to its parent.  Same construction as the
    // legacy inline path; without it the parent SEQUENCE drops the field row
    // for this reference, breaks the TypeHash row chain, and
    // GenerateAcnIcd.getMySelfAndChildren never reaches the resolved type.
    let refIcd = AcnReference.buildReferenceIcdArgAux r t o baseType

    let isContainingExternalField =
        match o.encodingOptions with
        | Some enc -> match enc.acnEncodingClass, enc.octOrBitStr with
                      | Asn1AcnAst.SZ_EC_ExternalField _, CommonTypes.ContainedInOctString -> true
                      | Asn1AcnAst.SZ_EC_ExternalField _, CommonTypes.ContainedInBitString -> true
                      | _ -> false
        | None -> false

    // Helper: create a simple funcBody from an STG template call (for CONTAINING FIXED/EMBEDDED)
    let makeContainingFuncBody (stgCall: string -> string -> ErrorCode -> (AcnFuncBodyResult option) * State) =
        let soSparkAnnotations = Some(DAstACN.sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
        let funcBody (us:State) (errCode:ErrorCode) (_acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (_nestingScope: NestingScope) (p:CodegenScope) =
            let pp = lm.lg.getParamValue t p.accessPath codec
            stgCall pp baseFncName errCode
        DAstACN.createAcnFunction r deps lm codec t typeDefinition isValidFunc
            (fun us e acnArgs nestingScope p -> funcBody us e acnArgs nestingScope p)
            (fun _atc -> true) soSparkAnnotations [] us
        |> fun (a, ns) -> Some a, ns

    // Early return for CONTAINING cases that don't need a specialized function
    let earlyReturn =
        match o.encodingOptions with
        | Some encOptions ->
            match encOptions.acnEncodingClass, encOptions.octOrBitStr with
            | Asn1AcnAst.SZ_EC_FIXED_SIZE, CommonTypes.ContainedInOctString ->
                Some (makeContainingFuncBody (fun pp fncName _ ->
                    let fncBody = lm.acn.octet_string_containing_deferred_fixed_func pp fncName codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = refIcd; encOutcome = EncodeOutcome.unknown}), us))
            | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _, CommonTypes.ContainedInOctString ->
                Some (makeContainingFuncBody (fun pp fncName errCode ->
                    let nBits = GetNumberOfBitsForNonNegativeInteger (encOptions.maxSize.acn - encOptions.minSize.acn)
                    let fncBody = lm.acn.octet_string_containing_deferred_embedded_func pp fncName encOptions.minSize.acn encOptions.maxSize.acn nBits errCode.errCodeName (Asn1AcnAstUtilFunctions.isLiveDeduced o.resolvedType) codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = refIcd; encOutcome = EncodeOutcome.unknown}), us))
            | Asn1AcnAst.SZ_EC_FIXED_SIZE, CommonTypes.ContainedInBitString ->
                Some (makeContainingFuncBody (fun pp fncName _ ->
                    let fncBody = lm.acn.bit_string_containing_deferred_fixed_func pp fncName codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = refIcd; encOutcome = EncodeOutcome.unknown}), us))
            | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _, CommonTypes.ContainedInBitString ->
                Some (makeContainingFuncBody (fun pp fncName errCode ->
                    let nBits = GetNumberOfBitsForNonNegativeInteger (encOptions.maxSize.acn - encOptions.minSize.acn)
                    let fncBody = lm.acn.bit_string_containing_deferred_embedded_func pp fncName encOptions.minSize.acn encOptions.maxSize.acn nBits errCode.errCodeName codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = refIcd; encOutcome = EncodeOutcome.unknown}), us))
            | _ when not isContainingExternalField ->
                Some (DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType us)
            | _ -> None  // ExternalField with params → specialized function below
        | None when o.resolvedType.acnParameters.Length = 0 ->
            Some (DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType us)
        | _ ->
            // Named type alias wrapping another ReferenceType with extra args:
            // the inner ref will generate the specialized function at the same path.
            // Return inner ref's AcnFunction to avoid duplicate function definition.
            match o.resolvedType.Kind with
            | Asn1AcnAst.ReferenceType innerRef when innerRef.hasExtraConstrainsOrChildrenOrAcnArgs ->
                Some (baseType.getAcnFunction codec, us)
            | _ -> None  // has params → specialized function below

    match earlyReturn with
    | Some result -> result
    | None ->

        // --- Generate specialized function per reference site ---
        // ExternalField CONTAINING (with params) OR normal reference (with params)

        let baseTypeAcnFunction = baseType.getAcnFunction codec
        match baseTypeAcnFunction with
        | None -> None, us
        | Some baseAcnFunc ->

            let specFuncName =
                let pathStr = t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin "_"
                let candidate = ToC2(r.args.TypePrefix + pathStr) + "_ACN" + (lm.lg.codecSuffix codec)
                let containingCandidate =
                    candidate.Replace("_ACN" + (lm.lg.codecSuffix codec), "_Containing_ACN" + (lm.lg.codecSuffix codec))
                // Disambiguate: if the base type's standalone function has the
                // same name, insert "_D" to avoid conflicting types in generated C.
                // This happens when a TAS name with dashes (e.g. MyPDU-a → MyPDU_a)
                // collides with a reference site path (e.g. MyPDU.a → MyPDU_a).
                // Note: baseFncName (computed at the top of createDeferredReferenceFunction
                // from getBaseFuncName) gives the TAS's standalone function name.
                // baseAcnFunc.funcName is None when the resolved type has acnParameters
                // (closure conversion), so we use baseFncName instead.
                if isContainingExternalField then
                    containingCandidate
                elif baseFncName = candidate then
                    candidate.Replace("_ACN" + (lm.lg.codecSuffix codec), "_D_ACN" + (lm.lg.codecSuffix codec))
                else candidate

            let errCodeName = ToC ("ERR_ACN" + ((lm.lg.codecSuffix codec).ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
            let errFieldPath = match t.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elm")
            let errCode, ns1 = getNextValidErrorCode us errCodeName None errFieldPath

            let specP : CodegenScope = lm.lg.getParamType t codec
            let stripLocals = stripOwnParamLocals r deps lm o.resolvedType.acnParameters

            let ctx : DeferredRefCtx = {
                r              = r
                deps           = deps
                lm             = lm
                codec          = codec
                t              = t
                o              = o
                typeDefinition = typeDefinition
                isValidFunc    = isValidFunc
                refIcd         = refIcd
            }

            // Generate body content — dispatched on the 3-way match.  Each branch
            // returns an AcnFuncBodyResult-shaped record + new State.
            let bodyResult, ns2 =
                match isContainingExternalField, baseAcnFunc.funcName.IsNone with
                | true, true  -> buildContainingClosureBody    ctx specP errCode baseAcnFunc stripLocals ns1
                | true, false -> buildContainingStandaloneBody ctx specP errCode baseFncName ns1
                | _           -> buildNormalReferenceBody      ctx specP          baseAcnFunc stripLocals ns1

            // A parameterized CHOICE may use one size parameter for each
            // alternative's CONTAINING region. Exactly one alternative is
            // encoded, so the CHOICE boundary is the containing region and
            // must measure/limit it. The selected child's referenced type has
            // no explicit ACN argument of its own, so it cannot do this work.
            let bodyResult =
                match isContainingExternalField, ctx.o.resolvedType.Kind,
                      findContainingSizeParamInfo ctx.deps ctx.o with
                | false, Asn1AcnAst.Asn1TypeKind.Choice ch, Some (sizePrm, containing) ->
                    // Some alternative's contents use 'size deduced' (Ada decodes
                    // the region from a stream limited to it).
                    let bDeducedContent =
                        ch.children |> List.exists (fun c ->
                            match c.Type.Kind with
                            | Asn1AcnAst.Asn1TypeKind.ReferenceType rf when rf.encodingOptions.IsSome ->
                                Asn1AcnAstUtilFunctions.isLiveDeduced rf.resolvedType
                            | _ -> false)
                    let detParamName = DAstACN.getAcnDeterminantName sizePrm.id
                    let mappingFunction, mappingModule, mappingDeclarations =
                        getDeferredMappingFunction ctx.r ctx.lm ctx.deps ctx.codec sizePrm.id
                    let patchFnName =
                        match findDetFunctionsForParam ctx.lm ctx.deps sizePrm.id with
                        | Some (_, patchFn, _, _) -> patchFn
                        | None -> failwithf "BUG: missing PatchDet function for CONTAINING CHOICE parameter %s" sizePrm.id.AsString
                    let wrappedBody =
                        match containing.encodingOptions.Value.octOrBitStr with
                        | CommonTypes.ContainedInOctString ->
                            ctx.lm.acn.octet_string_containing_deferred_wrapper
                                bodyResult.funcBody detParamName patchFnName mappingFunction mappingModule errCode.errCodeName bDeducedContent ctx.codec
                        | CommonTypes.ContainedInBitString ->
                            ctx.lm.acn.bit_string_containing_deferred_wrapper
                                bodyResult.funcBody detParamName patchFnName mappingFunction mappingModule errCode.errCodeName ctx.codec
                    { bodyResult with
                        funcBody = wrappedBody
                        userDefinedFunctions = bodyResult.userDefinedFunctions @ mappingDeclarations }
                | _ -> bodyResult

            // Step 2b: append PatchDet calls (encode only) onto the body.
            let finalBody =
                let body, bPatchGuarded =
                    appendPatchDetCalls ctx specP
                        isContainingExternalField (not baseAcnFunc.funcName.IsNone) bodyResult.encOutcome bodyResult.funcBody
                // coverage step 12b: the PatchDet calls write the result (they may fail); after a body that may fail
                // they are nested under its check (Issue 48), after one that cannot fail they follow it unchecked, so
                // the body's writes before them are overwritten
                let encOutcome =
                    // a patch under the checks of its consumer's path (q/issues.md Issue 50) may not run
                    let patch =
                        match bPatchGuarded with
                        | false -> ctx.lm.lg.encodeTemplateOutcome EncDeferredPatch
                        | true  -> EncodeOutcome.conditional (ctx.lm.lg.encodeTemplateOutcome EncDeferredPatch)
                    let b = bodyResult.encOutcome
                    match body = bodyResult.funcBody, b.canFail, b.writes, patch.writes with
                    | true, _, _, _                                                     -> b
                    | false, true, _, _                                                 -> EncodeOutcome.seq2 b patch
                    | false, false, (NoWrite | SometimesWrites | AlwaysWrites), AlwaysWrites -> {canFail = true; writes = AlwaysWrites; callee = None}
                    | false, false, _, _                                                -> {canFail = true; writes = (if b.writes = ReadsFirst && patch.writes = AlwaysWrites then ReadsFirst else WritesUnknown); callee = None}
                { bodyResult with funcBody = body; encOutcome = encOutcome }

            // Step 3: build the specialized function's deferred formal params.
            let deferredFormalParams, deferredParamNames =
                buildSpecializedFormalParams ctx o.resolvedType.acnParameters

            // Step 4: emit the specialized function (def + body) into auxiliaries.
            let allAuxiliaries =
                emitSpecializedFunctionDecl
                    ctx specP errCode
                    specFuncName deferredFormalParams deferredParamNames finalBody

            // Step 5: build the caller's funcBody, record the cross-TAS call,
            // and wrap into an AcnFunction.
            buildCallerWrapper ctx specFuncName allAuxiliaries finalBody.userDefinedFunctions ns2


// ---------------------------------------------------------------------------
//  Dispatch: createSequenceFunction
// ---------------------------------------------------------------------------

/// Replaces DAstACN.createSequenceFunction at the call site.
/// When acnDeferred is false, delegates to the original inline version.
/// When acnDeferred is true, calls the deferred version.
let createSequenceFunction
        (r:Asn1AcnAst.AstRoot)
        (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm:LanguageMacros)
        (codec:CommonTypes.Codec)
        (t:Asn1AcnAst.Asn1Type)
        (o:Asn1AcnAst.Sequence)
        (typeDefinition:TypeDefinitionOrReference)
        (isValidFunc: IsValidFunction option)
        (children:SeqChildInfo list)
        (acnPrms:DastAcnParameter list)
        (us:State) =
    match r.args.acnDeferred with
    | true  -> createDeferredSequenceFunction r deps lm codec t o typeDefinition isValidFunc children acnPrms us
    | false -> DAstACN.createSequenceFunction_inline r deps lm codec t o typeDefinition isValidFunc children acnPrms None us


// ---------------------------------------------------------------------------
//  Dispatch: createReferenceFunction
// ---------------------------------------------------------------------------

/// Replaces DAstACN.createReferenceFunction at the call site.
/// When acnDeferred is false, delegates to the original inline version.
/// When acnDeferred is true, calls the deferred version.
let createReferenceFunction
        (r:Asn1AcnAst.AstRoot)
        (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm:LanguageMacros)
        (codec:CommonTypes.Codec)
        (t:Asn1AcnAst.Asn1Type)
        (o:Asn1AcnAst.ReferenceType)
        (typeDefinition:TypeDefinitionOrReference)
        (isValidFunc: IsValidFunction option)
        (baseType:Asn1Type)
        (us:State) =
    match r.args.acnDeferred with
    | true  -> createDeferredReferenceFunction r deps lm codec t o typeDefinition isValidFunc baseType us
    | false -> DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType us
