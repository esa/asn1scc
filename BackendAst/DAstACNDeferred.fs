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


/// Build an access expression from relative scope nodes and the root receiver.
/// Returns (accessExpr, accessor) where the accessor is the operator that the
/// caller should put between accessExpr and the next field — "->" / "." in C
/// when the result is a pointer / struct, always "." in Ada (records).
/// The root accessor (between rootId and the first hop) comes from
/// lm.lg.getAccess on the caller's AccessPath, which carries the language's
/// pointer-vs-value convention.
/// E.g., C   []                   + pVal/ByPointer → ("pVal", "->")
///       Ada []                   + val/ByValue    → ("val",  ".")
///       both [SEQ_CHILD("buffer",_)] + …          → ("<root><rootAcc>buffer", ".")
let buildRelativeAccess (lm: LanguageMacros) (relParts: ScopeNode list) (specP: CodegenScope) : (string * string) =
    let rootId = specP.accessPath.rootId
    let rootAcc = lm.lg.getAccess specP.accessPath
    match relParts with
    | [] -> (rootId, rootAcc)
    | _ ->
        let parts = relParts |> List.map (fun node ->
            match node with
            | SEQ_CHILD (name, _) -> ToC name
            | CH_CHILD (name, _, _) -> ToC name
            | _ -> failwithf "BUG: unexpected scope node %A in relative access" node)
        // First hop after rootId uses the language-specific rootAcc; subsequent
        // hops are always "." (struct member access in both C and Ada).
        let fieldAccess = rootId + rootAcc + (parts |> String.concat ".")
        (fieldAccess, ".")


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
let computePatchDetValueExpr
    (lm: LanguageMacros)
    (dep: Asn1AcnAst.AcnDependency)
    (boundaryPath: ScopeNode list)
    (specP: CodegenScope)
    : (string option * string) =
    let fieldPath = dep.asn1Type.ToScopeNodeList
    let relParts = fieldPath |> List.skip boundaryPath.Length
    match dep.dependencyKind with
    | Asn1AcnAst.AcnDepSizeDeterminant _ ->
        let (fieldExpr, acc) = buildRelativeAccess lm relParts specP
        (None, lm.acn.getSizeableSize fieldExpr acc false)

    | Asn1AcnAst.AcnDepIA5StringSizeDeterminant _ ->
        let (fieldExpr, _acc) = buildRelativeAccess lm relParts specP
        (None, lm.acn.getStringSize fieldExpr)

    | Asn1AcnAst.AcnDepPresenceBool ->
        // dep.asn1Type points to the OPTIONAL child; parent is one level up
        let parentRelParts = relParts |> List.rev |> List.tail |> List.rev
        let (parentExpr, pAcc) = buildRelativeAccess lm parentRelParts specP
        let childName = ToC (dep.asn1Type.lastItem)
        (None, lm.acn.acn_deferred_det_value_presence_bool parentExpr pAcc childName)

    | Asn1AcnAst.AcnDepPresence (relPath, chc) ->
        let (choiceExpr, acc) = buildRelativeAccess lm relParts specP
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
        let switchBlock = lm.acn.acn_deferred_det_switch_int varName kindAccess switchItems
        (Some switchBlock, varName)

    | Asn1AcnAst.AcnDepPresenceStr (relPath, chc, _strType) ->
        let (choiceExpr, acc) = buildRelativeAccess lm relParts specP
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
        let switchBlock = lm.acn.acn_deferred_det_switch_str varName kindAccess switchItems
        (Some switchBlock, varName)

    | Asn1AcnAst.AcnDepChoiceDeterminant (enm, chc, _isOptional) ->
        let (choiceExpr, acc) = buildRelativeAccess lm relParts specP
        let kindAccess = lm.acn.acn_deferred_det_kind_access choiceExpr acc
        let varName = lm.lg.acnDeferredTempVarName "patchDetVal"
        let switchItems = chc.children |> List.map (fun ch ->
            let enmItem = enm.enm.items |> List.find (fun itm -> itm.Name.Value = ch.Name.Value)
            let presentWhenName = ch.presentWhenName
            // Emit the ACN-encoded integer value (not the enum literal name) so
            // the assignment target — det.Value of type Asn1UInt — accepts it
            // without an enum→uint cast (which Ada disallows).
            let enumIntVal = enmItem.acnEncodeValue.ToString()
            lm.acn.acn_deferred_det_switch_case_int presentWhenName varName enumIntVal)
        let switchBlock = lm.acn.acn_deferred_det_switch_int varName kindAccess switchItems
        (Some switchBlock, varName)

    | _ ->
        failwithf "BUG: PatchDet not yet implemented for dependency kind %A" dep.dependencyKind


/// Follow the RefTypeArgumentDependency chain from a parameter upward
/// through intermediate boundaries until reaching the original
/// AcnChildDeterminant.  Returns the (InitDet, PatchDet, nBitsOpt) function names.
let findDetFunctionsForParam (lm: LanguageMacros) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (paramId: ReferenceToType) : DetFunctionNames option =
    let rec follow (pid: ReferenceToType) =
        deps.acnDependencies |> List.tryPick (fun d ->
            match d.dependencyKind with
            | AcnDepRefTypeArgument p when p.id = pid ->
                match d.determinant with
                | Asn1AcnAst.AcnChildDeterminant ac ->
                    lm.lg.getDeferredDetFunctions ac.Type
                | Asn1AcnAst.AcnParameterDeterminant parentPrm ->
                    follow parentPrm.id
            | _ -> None)
    follow paramId


/// Collect deferred determinant names from the original AST (before the fold).
/// Used by preSeqFunc to provide parent context to createAcnChild.
let collectDeferredDetNamesFromAst (r: Asn1AcnAst.AstRoot) (t: Asn1AcnAst.Asn1Type) (seq: Asn1AcnAst.Sequence) : Set<string> =
    if not r.args.acnDeferred then Set.empty
    else
        let fromChildren =
            seq.children
            |> List.choose (fun c -> match c with Asn1AcnAst.Asn1Child ac -> Some ac | _ -> None)
            |> List.collect (fun ac ->
                match ac.Type.Kind with
                | Asn1AcnAst.ReferenceType rt ->
                    rt.acnArguments
                    |> List.choose (fun (AcnGenericTypes.RelativePath parts) ->
                        match parts with [] -> None | _ -> Some (parts |> List.last).Value)
                | _ -> [])
            |> Set.ofList
        let fromOwnParams = t.acnParameters |> List.map (fun p -> p.name) |> Set.ofList
        Set.union fromChildren fromOwnParams

/// Collect the names of determinants that are passed as acnArguments
/// by child reference types within a SEQUENCE.  These are the ACN children
/// that need deferred handling (InitDet instead of normal encoding).
let collectDeferredDetNames (children: SeqChildInfo list) : Set<string> =
    children
    |> List.choose (fun c ->
        match c with
        | DAst.Asn1Child ac -> Some ac
        | _ -> None)
    |> List.collect (fun ac ->
        match ac.Type.Kind with
        | DAst.ReferenceType rt ->
            rt.baseInfo.acnArguments
            |> List.choose (fun arg ->
                let (AcnGenericTypes.RelativePath parts) = arg
                // Only the last part is the determinant name.
                // Earlier parts are sibling navigation (e.g., "hdr" in
                // "hdr.buffers-length" is the sibling, not a determinant).
                match parts with
                | [] -> None
                | _  -> Some (parts |> List.last |> fun sl -> sl.Value))
        | _ -> [])
    |> Set.ofList


// ---------------------------------------------------------------------------
//  Deferred SEQUENCE function
// ---------------------------------------------------------------------------

/// Deferred version of createSequenceFunction.
/// Pre-processes the children list:
///   1. For each unique argument name referenced by child ReferenceTypes'
///      acnArguments, ensures an AcnInsertedFieldRef local variable is
///      declared at this SEQUENCE level.
///   2. For direct ACN children that match a deferred det name, replaces
///      their encoding with InitDet and marks them as NullType to bypass
///      _is_initialized checks.
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

    // Deferred determinant names come from two sources:
    //   (a) child ReferenceTypes' acnArguments — the parent SEQUENCE declares
    //       AcnInsertedFieldRef variables and passes them to children
    //   (b) the sequence's own acnParameters — the sequence receives
    //       AcnInsertedFieldRef* formal parameters (producer or consumer)
    //       and its direct ACN children matching those params need InitDet
    let deferredDetNamesFromChildren = collectDeferredDetNames children
    let deferredDetNamesFromOwnParams =
        t.acnParameters |> List.map (fun p -> p.name) |> Set.ofList
    let deferredDetNames = Set.union deferredDetNamesFromChildren deferredDetNamesFromOwnParams

    // If no deferred determinants, use the inline version as-is
    if deferredDetNames.IsEmpty then
        DAstACN.createSequenceFunction_inline r deps lm codec t o typeDefinition isValidFunc children acnPrms None us
    else
        // Collect the names of direct ACN children in this SEQUENCE
        let directAcnChildNames =
            children
            |> List.choose (fun c -> match c with DAst.AcnChild ac -> Some ac.Name.Value | _ -> None)
            |> Set.ofList

        // Modify direct ACN children that are deferred determinants
        let modifiedChildren =
            children |> List.map (fun child ->
                match child with
                | DAst.AcnChild ac when Set.contains ac.Name.Value deferredDetNames ->
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
                        let isOwnParam = Set.contains ac.Name.Value deferredDetNamesFromOwnParams
                        // For own parameters (formal AcnInsertedFieldRef*), use full
                        // path name (matches getExternalField0 resolution).
                        // For local variables, use bare name (matches callerFuncBody's
                        // ToC(argName) actual param naming).
                        let detVarName =
                            if isOwnParam then DAstACN.getAcnDeterminantName ac.id
                            else ToC ac.Name.Value
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
                                        userDefinedFunctions = []
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
                                    let redirectKind =
                                        match ac.Type with
                                        | Asn1AcnAst.AcnInsertedType.AcnInteger ai ->
                                            let intClass = Asn1AcnAstUtilFunctions.getAcnIntegerClass r.args ai
                                            match intClass with
                                            | Asn1AcnAst.ASN1SCC_UInt _
                                            | Asn1AcnAst.ASN1SCC_UInt64 _
                                            | Asn1AcnAst.ASN1SCC_Int _
                                            | Asn1AcnAst.ASN1SCC_Int64 _ -> "direct_value"
                                            | _ -> "temp_copy"
                                        | Asn1AcnAst.AcnInsertedType.AcnReferenceToIA5String _ -> "direct_str_value"
                                        | _ -> "temp_copy"
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
                                        let tmpName = ac.c_name + "_tmp"
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
                                                | Asn1AcnAst.AcnInsertedType.AcnReferenceToEnumerated _ ->
                                                    // For enum tmp, Ada needs <EnumType>'Enum_Rep(tmp).
                                                    // The local var declaration above uses
                                                    // ac.typeDefinitionBodyWithinSeq as the enum type
                                                    // name; reuse it here.
                                                    let enumTypeName = ac.typeDefinitionBodyWithinSeq
                                                    lm.acn.acn_deferred_det_copy_enum_tmp detAccess tmpName enumTypeName CommonTypes.Codec.Decode
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
        let fallbackEpilogue =
            match codec with
            | CommonTypes.Codec.Encode ->
                let fallbackDets =
                    children |> List.choose (fun child ->
                        match child with
                        | DAst.AcnChild ac when Set.contains ac.Name.Value deferredDetNames
                                             && not (Set.contains ac.Name.Value deferredDetNamesFromOwnParams) ->
                            match lm.lg.getDeferredDetFunctions ac.Type with
                            | Some (_initFn, patchFn, nBitsOpt, uperMinOffset) ->
                                let detVarName = ToC ac.Name.Value
                                let defaultVal = lm.lg.computeDeferredFallbackValue ac.Type uperMinOffset
                                Some (detVarName, patchFn, nBitsOpt, defaultVal, ac.Type)
                            | None -> None
                        | _ -> None)
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
                    Some fallbackCode
            | CommonTypes.Codec.Decode -> None

        DAstACN.createSequenceFunction_inline r deps lm codec t o typeDefinition isValidFunc modifiedChildren acnPrms fallbackEpilogue us


// ---------------------------------------------------------------------------
//  Deferred REFERENCE function
// ---------------------------------------------------------------------------

/// Find which acnParameter is the CONTAINING size determinant by checking
/// the dependency list for AcnDepSizeDeterminant_bit_oct_str_contain.
/// Returns the parameter, or None if not found.
let private findContainingSizeParam
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (o: Asn1AcnAst.ReferenceType) : AcnGenericTypes.AcnParameter option =
    o.resolvedType.acnParameters |> List.tryFind (fun prm ->
        deps.acnDependencies |> List.exists (fun d ->
            d.determinant.id = prm.id
            && (match d.dependencyKind with
                | Asn1AcnAst.AcnDepSizeDeterminant_bit_oct_str_contain _ -> true
                | _ -> false)))
            
/// After boundary post-processing rewrites &name → formal param name in the
/// body text, the matching AcnInsertedFieldRef local variable declarations
/// injected by inner callerFuncBodies become orphans.  Strip them — they
/// are now provided by this function's formal params.
let private stripOwnParamLocals
        (lm: LanguageMacros)
        (acnParameters: AcnGenericTypes.AcnParameter list)
        (locals: LocalVariable list) : LocalVariable list =
    let ownParamNames =
        acnParameters
        |> List.map (fun prm -> ToC prm.name)
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
        userDefinedFunctions = [] }


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
    let bodyContent, ns2 = baseAcnFunc.funcBody ns1 paramsArgsPairs (NestingScope.init ctx.t.acnMaxSizeInBits ctx.t.uperMaxSizeInBits []) specP
    match bodyContent with
    | None -> emptyBody ctx.lm, ns2
    | Some br ->
        let wrappedBody =
            match containingSizePrm with
            | Some sizePrm ->
                let detParamName = DAstACN.getAcnDeterminantName sizePrm.id
                let patchFnName =
                    match findDetFunctionsForParam ctx.lm ctx.deps sizePrm.id with
                    | Some (_, patchFn, _, _) -> patchFn
                    | None -> ""
                match ctx.o.encodingOptions.Value.octOrBitStr with
                | CommonTypes.ContainedInOctString ->
                    ctx.lm.acn.octet_string_containing_deferred_wrapper br.funcBody detParamName patchFnName errCode.errCodeName ctx.codec
                | CommonTypes.ContainedInBitString ->
                    ctx.lm.acn.bit_string_containing_deferred_wrapper br.funcBody detParamName patchFnName errCode.errCodeName ctx.codec
            | None ->
                br.funcBody  // fallback: no wrapping if size param not found
        { br with
            funcBody = wrappedBody
            errCodes = br.errCodes @ [errCode]
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
    let patchFnName =
        match findDetFunctionsForParam ctx.lm ctx.deps sizePrm.id with
        | Some (_initFn, patchFn, _, _) -> patchFn
        | None -> ""
    let fncBody =
        match ctx.o.encodingOptions.Value.octOrBitStr with
        | CommonTypes.ContainedInOctString ->
            ctx.lm.acn.octet_string_containing_deferred_func pp baseFncName detParamName patchFnName errCode.errCodeName ctx.codec
        | CommonTypes.ContainedInBitString ->
            ctx.lm.acn.bit_string_containing_deferred_func pp baseFncName detParamName patchFnName errCode.errCodeName ctx.codec
    { emptyBody ctx.lm with
        funcBody = fncBody
        errCodes = [errCode]
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
    let bodyContent, ns2 = baseAcnFunc.funcBody ns1 paramsArgsPairs (NestingScope.init ctx.t.acnMaxSizeInBits ctx.t.uperMaxSizeInBits []) specP
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
        (body: string) : string =
    match isContainingExternalField, not baseHasStandaloneFunc with
    | true, false -> body  // old approach: PatchDet is in STG template
    | _ ->
        match ctx.codec with
        | CommonTypes.Codec.Decode -> body
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
                    let consumerDep =
                        ctx.deps.acnDependencies |> List.tryFind (fun d ->
                            d.determinant.id = prm.id
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
                            let boundaryPath = ctx.o.resolvedType.id.ToScopeNodeList
                            let (preBlock, rawValueExpr) = computePatchDetValueExpr ctx.lm dep boundaryPath specP
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
                            let fullBlock =
                                match preBlock with
                                | None -> patchCall
                                | Some pre -> ctx.lm.acn.acn_deferred_det_preblock_wrap pre patchCall
                            Some fullBlock)
            match patchCalls with
            | [] -> body
            | calls ->
                let patchCode = calls |> String.concat "\n"
                body + "\n" + patchCode


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

    let specFuncBody =
        ctx.lm.acn.EmitTypeAssignment_primitive
            varName sStar specFuncName isValidFuncName typeDefinitionName
            lvars finalBody.funcBody
            None ""  // soSparkAnnotations, sInitialExp
            deferredFormalParams deferredParamNames
            (ctx.t.acnMaxSizeInBits = 0I) finalBody.bBsIsUnReferenced bVarNameIsUnreferenced
            false
            soInitFuncName [] [] []  // funcDefAnnots, precondAnnots, postcondAnnots
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

        // For each acnArgument, classify the call site:
        //   - Intermediate level (parent param exists) → pass parent's
        //     formal param name (pointer pass-through).  No local
        //     declaration needed; the parent already has it.
        //   - Declaration level (no parent param) → pass &localName.
        //     The local AcnInsertedFieldRef must be declared in the
        //     caller's scope, so emit a localVariable for it.
        let extraActualParams, declLevelLocalNames =
            ctx.o.acnArguments |> List.fold (fun (paramsAcc, localsAcc) arg ->
                let (AcnGenericTypes.RelativePath parts) = arg
                let argName = parts |> List.last |> fun sl -> sl.Value
                let parentParam =
                    acnArgs |> List.tryFind (fun (_, prm) -> prm.name = argName)
                match parentParam with
                | Some (_, prm) ->
                    let pStr = DAstACN.getAcnDeterminantName prm.id
                    paramsAcc @ [pStr], localsAcc
                | None ->
                    let cName = ToC argName
                    let pStr = ctx.lm.acn.acn_deferred_det_actual_param cName ctx.codec
                    paramsAcc @ [pStr], localsAcc @ [cName]
            ) ([], [])

        let baseFuncCall = callBaseTypeFunc ctx.lm pp specFuncName ctx.codec
        let funcBodyContent = insertActualParams baseFuncCall extraActualParams

        // Declaration-level AcnInsertedFieldRef locals.  Distinct by name in
        // case the same determinant is referenced via multiple acnArguments
        // paths (e.g. hdr.x and direct x); duplicates would be deduped later
        // anyway, but we save work by deduping here.
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
               icdResult = ctx.refIcd}), callerUs

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
    let makeContainingFuncBody (stgCall: string -> string -> (AcnFuncBodyResult option) * State) =
        let soSparkAnnotations = Some(DAstACN.sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
        let funcBody (us:State) (errCode:ErrorCode) (_acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (_nestingScope: NestingScope) (p:CodegenScope) =
            let pp = lm.lg.getParamValue t p.accessPath codec
            stgCall pp baseFncName
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
                Some (makeContainingFuncBody (fun pp fncName ->
                    let fncBody = lm.acn.octet_string_containing_deferred_fixed_func pp fncName codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = refIcd}), us))
            | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _, CommonTypes.ContainedInOctString ->
                Some (makeContainingFuncBody (fun pp fncName ->
                    let nBits = GetNumberOfBitsForNonNegativeInteger (encOptions.maxSize.acn - encOptions.minSize.acn)
                    let fncBody = lm.acn.octet_string_containing_deferred_embedded_func pp fncName encOptions.minSize.acn encOptions.maxSize.acn nBits codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = refIcd}), us))
            | Asn1AcnAst.SZ_EC_FIXED_SIZE, CommonTypes.ContainedInBitString ->
                Some (makeContainingFuncBody (fun pp fncName ->
                    let fncBody = lm.acn.bit_string_containing_deferred_fixed_func pp fncName codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = refIcd}), us))
            | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _, CommonTypes.ContainedInBitString ->
                Some (makeContainingFuncBody (fun pp fncName ->
                    let nBits = GetNumberOfBitsForNonNegativeInteger (encOptions.maxSize.acn - encOptions.minSize.acn)
                    let fncBody = lm.acn.bit_string_containing_deferred_embedded_func pp fncName encOptions.minSize.acn encOptions.maxSize.acn nBits codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = refIcd}), us))
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
                // Disambiguate: if the base type's standalone function has the
                // same name, insert "_D" to avoid conflicting types in generated C.
                // This happens when a TAS name with dashes (e.g. MyPDU-a → MyPDU_a)
                // collides with a reference site path (e.g. MyPDU.a → MyPDU_a).
                // Note: baseFncName (computed at the top of createDeferredReferenceFunction
                // from getBaseFuncName) gives the TAS's standalone function name.
                // baseAcnFunc.funcName is None when the resolved type has acnParameters
                // (closure conversion), so we use baseFncName instead.
                if baseFncName = candidate then
                    candidate.Replace("_ACN" + (lm.lg.codecSuffix codec), "_D_ACN" + (lm.lg.codecSuffix codec))
                else candidate

            let errCodeName = ToC ("ERR_ACN" + ((lm.lg.codecSuffix codec).ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
            let errFieldPath = match t.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elm")
            let errCode, ns1 = getNextValidErrorCode us errCodeName None errFieldPath

            let specP : CodegenScope = lm.lg.getParamType t codec
            let stripLocals = stripOwnParamLocals lm o.resolvedType.acnParameters

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

            // Step 2b: append PatchDet calls (encode only) onto the body.
            let finalBody =
                let body =
                    appendPatchDetCalls ctx specP
                        isContainingExternalField (not baseAcnFunc.funcName.IsNone) bodyResult.funcBody
                { bodyResult with funcBody = body }

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
