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


/// Build a C access expression from relative scope nodes and the root pointer.
/// Returns (accessExpr, accessor) where accessor is "->" if result is a pointer,
/// "." if result is a struct member.
/// E.g., [] + "pVal" → ("pVal", "->")
///       [SEQ_CHILD("buffer",_)] + "pVal" → ("pVal->buffer", ".")
let buildRelativeAccess (lm: LanguageMacros) (relParts: ScopeNode list) (rootId: string) : (string * string) =
    match relParts with
    | [] -> (rootId, "->")
    | _ ->
        let parts = relParts |> List.map (fun node ->
            match node with
            | SEQ_CHILD (name, _) -> ToC name
            | CH_CHILD (name, _, _) -> ToC name
            | _ -> failwithf "BUG: unexpected scope node %A in relative access" node)
        let fieldAccess = lm.acn.acn_deferred_det_relative_access rootId (parts |> String.concat ".")
        (fieldAccess, ".")


/// Compute the C value expression for a PatchDet call, based on the
/// dependency kind.  Returns (preBlock option, valueExpr) where preBlock
/// is an optional block of C code to prepend (e.g., a switch statement
/// that computes the value into a local variable).
///
/// E.g., for AcnDepSizeDeterminant:     (None, "pVal->buffer.nCount")
///       for AcnDepPresenceBool:         (None, "(pVal->exist.extra_data == 1)")
///       for AcnDepPresence:             (Some "asn1SccUint _patchDetVal;\nswitch(...){...}", "_patchDetVal")
///       for AcnDepChoiceDeterminant:    (Some "asn1SccUint _patchDetVal;\nswitch(...){...}", "_patchDetVal")
let computePatchDetValueExpr
    (lm: LanguageMacros)
    (dep: Asn1AcnAst.AcnDependency)
    (boundaryPath: ScopeNode list)
    (rootId: string)
    : (string option * string) =
    let fieldPath = dep.asn1Type.ToScopeNodeList
    let relParts = fieldPath |> List.skip boundaryPath.Length
    match dep.dependencyKind with
    | Asn1AcnAst.AcnDepSizeDeterminant _ ->
        let (fieldExpr, acc) = buildRelativeAccess lm relParts rootId
        (None, lm.acn.getSizeableSize fieldExpr acc false)

    | Asn1AcnAst.AcnDepIA5StringSizeDeterminant _ ->
        let (fieldExpr, _acc) = buildRelativeAccess lm relParts rootId
        (None, lm.acn.getStringSize fieldExpr)

    | Asn1AcnAst.AcnDepPresenceBool ->
        // dep.asn1Type points to the OPTIONAL child; parent is one level up
        let parentRelParts = relParts |> List.rev |> List.tail |> List.rev
        let (parentExpr, pAcc) = buildRelativeAccess lm parentRelParts rootId
        let childName = ToC (dep.asn1Type.lastItem)
        (None, lm.acn.acn_deferred_det_value_presence_bool parentExpr pAcc childName)

    | Asn1AcnAst.AcnDepPresence (relPath, chc) ->
        let (choiceExpr, acc) = buildRelativeAccess lm relParts rootId
        let kindAccess = lm.acn.acn_deferred_det_kind_access choiceExpr acc
        let varName = "_patchDetVal"
        let switchItems = chc.children |> List.map (fun ch ->
            let pres = ch.acnPresentWhenConditions |> Seq.find (fun x -> x.relativePath = relPath)
            let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch
            match pres with
            | AcnGenericTypes.PresenceInt (_, intVal) ->
                lm.acn.acn_deferred_det_switch_case_int presentWhenName varName (intVal.Value.ToString())
            | AcnGenericTypes.PresenceStr _ ->
                failwithf "BUG: PresenceStr in AcnDepPresence (should be AcnDepPresenceStr)")
        let switchBlock = lm.acn.acn_deferred_det_switch_int varName kindAccess switchItems
        (Some switchBlock, varName)

    | Asn1AcnAst.AcnDepPresenceStr (relPath, chc, _strType) ->
        let (choiceExpr, acc) = buildRelativeAccess lm relParts rootId
        let kindAccess = lm.acn.acn_deferred_det_kind_access choiceExpr acc
        let varName = "_patchDetStrVal"
        let switchItems = chc.children |> List.map (fun ch ->
            let pres = ch.acnPresentWhenConditions |> Seq.find (fun x -> x.relativePath = relPath)
            let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch
            match pres with
            | AcnGenericTypes.PresenceStr (_, strVal) ->
                lm.acn.acn_deferred_det_switch_case_str presentWhenName varName strVal.Value
            | AcnGenericTypes.PresenceInt _ ->
                failwithf "BUG: PresenceInt in AcnDepPresenceStr")
        let switchBlock = lm.acn.acn_deferred_det_switch_str varName kindAccess switchItems
        (Some switchBlock, varName)

    | Asn1AcnAst.AcnDepChoiceDeterminant (enm, chc, _isOptional) ->
        let (choiceExpr, acc) = buildRelativeAccess lm relParts rootId
        let kindAccess = lm.acn.acn_deferred_det_kind_access choiceExpr acc
        let varName = "_patchDetVal"
        let switchItems = chc.children |> List.map (fun ch ->
            let enmItem = enm.enm.items |> List.find (fun itm -> itm.Name.Value = ch.Name.Value)
            let presentWhenName = ch.presentWhenName
            let enumCName = lm.lg.getNamedItemBackendName None enmItem
            lm.acn.acn_deferred_det_switch_case_int presentWhenName varName enumCName)
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
                                        icdResult = None
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
                                                FlagLocalVariable (tmpName, None)
                                            | _ ->
                                                GenericLocalVariable { name = tmpName; varType = ac.typeDefinitionBodyWithinSeq; arrSize = None; isStatic = false; initExp = None }
                                        let modifiedP = {p with accessPath = AccessPath.valueEmptyPath tmpName}
                                        let result = originalFuncBody innerCodec acnArgs nestingScope modifiedP bsPos
                                        match result with
                                        | Some r ->
                                            let detAccess =
                                                if isOwnParam then lm.acn.acn_deferred_det_access_ptr detVarName
                                                else lm.acn.acn_deferred_det_access_value detVarName
                                            let assignStmt = lm.acn.acn_deferred_det_copy_tmp detAccess tmpName CommonTypes.Codec.Decode
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


/// Apply intermediate-boundary post-processing to an inner funcBody.
/// SEQUENCE bodies pass [] as acnArgs to their children, so child
/// callerFuncBodies generate "&argName" (local AcnInsertedFieldRef).  At
/// this boundary we replace those local refs with the formal parameter
/// name for pointer pass-through.
let private rewriteLocalParamRefs
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (acnParameters: AcnGenericTypes.AcnParameter list)
        (body: string) : string =
    acnParameters |> List.fold (fun (b: string) prm ->
        let localRef = lm.acn.acn_deferred_det_actual_param (ToC prm.name) codec
        let paramRef = DAstACN.getAcnDeterminantName prm.id
        b.Replace(localRef, paramRef)
    ) body


/// CONTAINING ExternalField, base type has NO standalone function (parameterized
/// type): use funcBody closure + CONTAINING wrapper.  The CONTAINING size
/// parameter was added by closure conversion and is NOT in o.acnArguments —
/// the arguments correspond to the first N parameters; extra params from
/// closure conversion are at the end.
let private buildContainingClosureBody
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (t: Asn1AcnAst.Asn1Type)
        (o: Asn1AcnAst.ReferenceType)
        (specP: CodegenScope)
        (errCode: ErrorCode)
        (baseAcnFunc: AcnFunction)
        (stripLocals: LocalVariable list -> LocalVariable list)
        (ns1: State) : AcnFuncBodyResult * State =
    let containingSizePrm = findContainingSizeParam deps o
    let nArgs = o.acnArguments.Length
    let paramsArgsPairs = List.zip o.acnArguments (o.resolvedType.acnParameters |> List.take nArgs)
    let bodyContent, ns2 = baseAcnFunc.funcBody ns1 paramsArgsPairs (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) specP
    match bodyContent with
    | None -> emptyBody lm, ns2
    | Some br ->
        let funcBody0 = rewriteLocalParamRefs lm codec o.resolvedType.acnParameters br.funcBody
        let wrappedBody =
            match containingSizePrm with
            | Some sizePrm ->
                let detParamName = DAstACN.getAcnDeterminantName sizePrm.id
                let patchFnName =
                    match findDetFunctionsForParam lm deps sizePrm.id with
                    | Some (_, patchFn, _, _) -> patchFn
                    | None -> ""
                match o.encodingOptions.Value.octOrBitStr with
                | CommonTypes.ContainedInOctString ->
                    lm.acn.octet_string_containing_deferred_wrapper funcBody0 detParamName patchFnName errCode.errCodeName codec
                | CommonTypes.ContainedInBitString ->
                    lm.acn.bit_string_containing_deferred_wrapper funcBody0 detParamName patchFnName errCode.errCodeName codec
            | None ->
                funcBody0  // fallback: no wrapping if size param not found
        { br with
            funcBody = wrappedBody
            errCodes = br.errCodes @ [errCode]
            localVariables = stripLocals br.localVariables }, ns2


/// CONTAINING ExternalField, base type has standalone function (e.g. TC01b):
/// use the existing STG template approach.  PatchDet is emitted by the
/// template itself, so step 2b in the caller skips this case.
let private buildContainingStandaloneBody
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (t: Asn1AcnAst.Asn1Type)
        (o: Asn1AcnAst.ReferenceType)
        (specP: CodegenScope)
        (errCode: ErrorCode)
        (baseFncName: string)
        (ns1: State) : AcnFuncBodyResult * State =
    let pp =
        let str = lm.lg.getParamValue t specP.accessPath codec
        match codec, lm.lg.decodingKind with
        | Decode, Copy -> ToC str
        | _ -> str
    let sizePrm = o.resolvedType.acnParameters.Head
    let detParamName = DAstACN.getAcnDeterminantName sizePrm.id
    let patchFnName =
        match findDetFunctionsForParam lm deps sizePrm.id with
        | Some (_initFn, patchFn, _, _) -> patchFn
        | None -> ""
    let fncBody =
        match o.encodingOptions.Value.octOrBitStr with
        | CommonTypes.ContainedInOctString ->
            lm.acn.octet_string_containing_deferred_func pp baseFncName detParamName patchFnName errCode.errCodeName codec
        | CommonTypes.ContainedInBitString ->
            lm.acn.bit_string_containing_deferred_func pp baseFncName detParamName patchFnName errCode.errCodeName codec
    { emptyBody lm with
        funcBody = fncBody
        errCodes = [errCode]
        bValIsUnReferenced = false
        bBsIsUnReferenced = false }, ns1


/// Normal reference (no CONTAINING): call the base type's funcBody closure
/// and apply intermediate-boundary post-processing.
let private buildNormalReferenceBody
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (t: Asn1AcnAst.Asn1Type)
        (o: Asn1AcnAst.ReferenceType)
        (specP: CodegenScope)
        (baseAcnFunc: AcnFunction)
        (stripLocals: LocalVariable list -> LocalVariable list)
        (ns1: State) : AcnFuncBodyResult * State =
    let paramsArgsPairs = List.zip o.acnArguments o.resolvedType.acnParameters
    let bodyContent, ns2 = baseAcnFunc.funcBody ns1 paramsArgsPairs (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) specP
    match bodyContent with
    | None -> emptyBody lm, ns2
    | Some br ->
        let funcBody0 = rewriteLocalParamRefs lm codec o.resolvedType.acnParameters br.funcBody
        { br with
            funcBody = funcBody0
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
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (o: Asn1AcnAst.ReferenceType)
        (specP: CodegenScope)
        (isContainingExternalField: bool)
        (baseHasStandaloneFunc: bool)
        (body: string) : string =
    match isContainingExternalField, not baseHasStandaloneFunc with
    | true, false -> body  // old approach: PatchDet is in STG template
    | _ ->
        match codec with
        | CommonTypes.Codec.Decode -> body
        | CommonTypes.Codec.Encode ->
            // Identify CONTAINING size param to exclude from step 2b.  Always
            // check (not just when isContainingExternalField) because named
            // CONTAINING aliases have two ref levels — the outer ref has no
            // encodingOptions but still carries the same size parameter.
            let containingSizePrmId =
                findContainingSizeParam deps o
                |> Option.map (fun prm -> prm.id)
            let patchCalls =
                o.resolvedType.acnParameters |> List.choose (fun prm ->
                    match containingSizePrmId with
                    | Some csId when csId = prm.id -> None
                    | _ ->
                    // Find the dep inside this boundary where the determinant
                    // is this parameter (consumer-side dependency)
                    let consumerDep =
                        deps.acnDependencies |> List.tryFind (fun d ->
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
                        match findDetFunctionsForParam lm deps prm.id with
                        | None -> None
                        | Some (_initFn, patchFn, nBitsOpt, uperMinOffset) ->
                            let boundaryPath = o.resolvedType.id.ToScopeNodeList
                            let (preBlock, rawValueExpr) = computePatchDetValueExpr lm dep boundaryPath specP.accessPath.rootId
                            // For Integer_uPER with min > 0, UPER encodes (value - min)
                            // but ConstSize encodes value directly, so subtract the offset.
                            let valueExpr =
                                if uperMinOffset = 0I then rawValueExpr
                                else lm.acn.acn_deferred_det_uper_offset_sub rawValueExpr (uperMinOffset.ToString())
                            let detParamName = DAstACN.getAcnDeterminantName prm.id
                            let errCodePatch = "ERR_ACN_DET_CONSISTENCY_MISMATCH"
                            let isIA5StringDet = patchFn.Contains("IA5String")
                            let patchCall =
                                if isIA5StringDet then
                                    match nBitsOpt with
                                    | Some nBits ->
                                        lm.acn.acn_deferred_det_patch_ptr_str patchFn nBits valueExpr detParamName errCodePatch codec
                                    | None ->
                                        failwithf "BUG: IA5String PatchDet requires nChars (nBits) parameter"
                                else
                                    match nBitsOpt with
                                    | Some nBits ->
                                        lm.acn.acn_deferred_det_patch_ptr_with_size patchFn nBits valueExpr detParamName errCodePatch codec
                                    | None ->
                                        lm.acn.acn_deferred_det_patch_ptr patchFn valueExpr detParamName errCodePatch codec
                            let fullBlock =
                                match preBlock with
                                | None -> patchCall
                                | Some pre -> lm.acn.acn_deferred_det_preblock_wrap pre patchCall
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
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (acnParameters: AcnGenericTypes.AcnParameter list)
        : string list * string list =
    let names = acnParameters |> List.map (fun prm -> DAstACN.getAcnDeterminantName prm.id)
    let formals = names |> List.map (fun n -> lm.acn.acn_deferred_det_formal_param n codec)
    formals, names


/// Step 4 — emit the specialized function (definition + body) and assemble
/// the auxiliaries list.  The returned list is bodyResult_auxiliaries
/// followed by the function's def + body strings.
let private emitSpecializedFunctionDecl
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (t: Asn1AcnAst.Asn1Type)
        (o: Asn1AcnAst.ReferenceType)
        (specP: CodegenScope)
        (typeDefinition: TypeDefinitionOrReference)
        (isValidFunc: IsValidFunction option)
        (errCode: ErrorCode)
        (specFuncName: string)
        (deferredFormalParams: string list)
        (deferredParamNames: string list)
        (finalBody: AcnFuncBodyResult)
        : string list =
    let varName = specP.accessPath.rootId
    let sStar = lm.lg.getStar specP.accessPath
    let typeDefinitionName = typeDefinition.longTypedefName2 lm.lg.hasModules
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let soInitFuncName = getFuncNameGeneric typeDefinition (lm.init.methodNameSuffix())
    let nMaxBytesInACN = BigInteger (ceil ((double t.acnMaxSizeInBits)/8.0))
    let lvars = finalBody.localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct

    // In deferred mode, pVal may be unreferenced if the resolved type has
    // only ACN-inserted children and no ASN.1 data fields (e.g., Header
    // with only version pattern + buffers-length determinant).
    let hasAsn1Children =
        match o.resolvedType.Kind with
        | Asn1AcnAst.Asn1TypeKind.Sequence sq ->
            sq.children |> List.exists (fun c -> match c with Asn1AcnAst.Asn1Child _ -> true | _ -> false)
        | _ -> true
    let bVarNameIsUnreferenced = finalBody.bValIsUnReferenced || not hasAsn1Children

    let specFuncBody =
        lm.acn.EmitTypeAssignment_primitive
            varName sStar specFuncName isValidFuncName typeDefinitionName
            lvars finalBody.funcBody
            None ""  // soSparkAnnotations, sInitialExp
            deferredFormalParams deferredParamNames
            (t.acnMaxSizeInBits = 0I) finalBody.bBsIsUnReferenced bVarNameIsUnreferenced
            soInitFuncName [] [] None  // funcDefAnnots, precondAnnots, postcondAnnots
            codec

    let specErrCodStr =
        (errCode :: finalBody.errCodes)
        |> List.groupBy (fun x -> x.errCodeName)
        |> List.map (fun (k, v) -> {errCodeName = k; errCodeValue = v.Head.errCodeValue; comment = v.Head.comment})
        |> List.map (fun x -> lm.acn.EmitTypeAssignment_def_err_code x.errCodeName (BigInteger x.errCodeValue) x.comment)
        |> List.distinct

    let specFuncDef =
        lm.acn.EmitTypeAssignment_primitive_def
            varName sStar specFuncName typeDefinitionName
            specErrCodStr
            (t.acnMaxSizeInBits = 0I) nMaxBytesInACN t.acnMaxSizeInBits
            deferredFormalParams
            None  // soSparkAnnotations
            codec

    finalBody.auxiliaries @ [specFuncDef; specFuncBody]


/// Step 5 — build the caller's funcBody (a simple call to the specialized
/// function with &det actual parameters), record the cross-TAS function-call
/// dependency, and wrap the result in an AcnFunction via createAcnFunction.
let private buildCallerWrapper
        (r: Asn1AcnAst.AstRoot)
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (t: Asn1AcnAst.Asn1Type)
        (o: Asn1AcnAst.ReferenceType)
        (typeDefinition: TypeDefinitionOrReference)
        (isValidFunc: IsValidFunction option)
        (specFuncName: string)
        (allAuxiliaries: string list)
        (udfcs: UserDefinedFunction list)
        (ns: State)
        : AcnFunction option * State =
    let callerFuncBody (callerUs:State) (callerErrCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (_nestingScope: NestingScope) (p:CodegenScope) =
        let pp, resultExpr =
            let str = lm.lg.getParamValue t p.accessPath codec
            match codec, lm.lg.decodingKind with
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
            o.acnArguments |> List.fold (fun (paramsAcc, localsAcc) arg ->
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
                    let pStr = lm.acn.acn_deferred_det_actual_param cName codec
                    paramsAcc @ [pStr], localsAcc @ [cName]
            ) ([], [])

        let baseFuncCall = callBaseTypeFunc lm pp specFuncName codec
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
                    varType = lm.acn.acn_deferred_det_type_name()
                    arrSize = None
                    isStatic = false
                    initExp = Some (lm.acn.acn_deferred_det_init_expr())
                })

        Some ({AcnFuncBodyResult.funcBody = funcBodyContent
               errCodes = [callerErrCode]
               localVariables = extraLocals
               userDefinedFunctions = udfcs
               bValIsUnReferenced = false
               bBsIsUnReferenced = false
               resultExpr = resultExpr
               auxiliaries = allAuxiliaries
               icdResult = None}), callerUs

    // Record the function-call dependency (caller → callee)
    let ns3 =
        match t.id.topLevelTas with
        | None -> ns
        | Some tasInfo ->
            let caller = {Caller.typeId = tasInfo; funcType=AcnEncDecFunctionType}
            let callee = {Callee.typeId = {TypeAssignmentInfo.modName = o.modName.Value; tasName=o.tasName.Value} ; funcType=AcnEncDecFunctionType}
            addFunctionCallToState ns caller callee

    let soSparkAnnotations = Some(DAstACN.sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    let a, ns4 = DAstACN.createAcnFunction r deps lm codec t typeDefinition isValidFunc callerFuncBody (fun _atc -> true) soSparkAnnotations [] ns3
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

    let _baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t.id "_ACN" codec

    let isContainingExternalField =
        match o.encodingOptions with
        | Some enc -> match enc.acnEncodingClass, enc.octOrBitStr with
                      | Asn1AcnAst.SZ_EC_ExternalField _, CommonTypes.ContainedInOctString -> true
                      | Asn1AcnAst.SZ_EC_ExternalField _, CommonTypes.ContainedInBitString -> true
                      | _ -> false
        | None -> false

    // Helper: create a simple funcBody from an STG template call (for CONTAINING FIXED/EMBEDDED)
    let makeContainingFuncBody (stgCall: string -> string -> (AcnFuncBodyResult option) * State) =
        let soSparkAnnotations = Some(DAstACN.sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
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
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = None}), us))
            | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _, CommonTypes.ContainedInOctString ->
                Some (makeContainingFuncBody (fun pp fncName ->
                    let fncBody = lm.acn.octet_string_containing_deferred_embedded_func pp fncName encOptions.minSize.acn encOptions.maxSize.acn codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = None}), us))
            | Asn1AcnAst.SZ_EC_FIXED_SIZE, CommonTypes.ContainedInBitString ->
                Some (makeContainingFuncBody (fun pp fncName ->
                    let fncBody = lm.acn.bit_string_containing_deferred_fixed_func pp fncName codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = None}), us))
            | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _, CommonTypes.ContainedInBitString ->
                Some (makeContainingFuncBody (fun pp fncName ->
                    let fncBody = lm.acn.bit_string_containing_deferred_embedded_func pp fncName encOptions.minSize.acn encOptions.maxSize.acn codec
                    Some ({AcnFuncBodyResult.funcBody = fncBody; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=[]; icdResult = None}), us))
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
                let candidate = ToC2(r.args.TypePrefix + pathStr) + "_ACN" + codec.suffix
                // Disambiguate: if the base type's standalone function has the
                // same name, insert "_D" to avoid conflicting types in generated C.
                // This happens when a TAS name with dashes (e.g. MyPDU-a → MyPDU_a)
                // collides with a reference site path (e.g. MyPDU.a → MyPDU_a).
                // Note: baseFncName (computed at the top of createDeferredReferenceFunction
                // from getBaseFuncName) gives the TAS's standalone function name.
                // baseAcnFunc.funcName is None when the resolved type has acnParameters
                // (closure conversion), so we use baseFncName instead.
                if baseFncName = candidate then
                    candidate.Replace("_ACN" + codec.suffix, "_D_ACN" + codec.suffix)
                else candidate

            let errCodeName = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
            let errCode, ns1 = getNextValidErrorCode us errCodeName None

            let specP : CodegenScope = lm.lg.getParamType t codec
            let stripLocals = stripOwnParamLocals lm o.resolvedType.acnParameters

            // Generate body content — dispatched on the 3-way match.  Each branch
            // returns an AcnFuncBodyResult-shaped record + new State.
            let bodyResult, ns2 =
                match isContainingExternalField, baseAcnFunc.funcName.IsNone with
                | true, true  -> buildContainingClosureBody    deps lm codec t o specP errCode baseAcnFunc stripLocals ns1
                | true, false -> buildContainingStandaloneBody deps lm codec t o specP errCode baseFncName ns1
                | _           -> buildNormalReferenceBody          lm codec t o specP          baseAcnFunc stripLocals ns1

            // Step 2b: append PatchDet calls (encode only) onto the body.
            let finalBody =
                let body =
                    appendPatchDetCalls deps lm codec o specP
                        isContainingExternalField (not baseAcnFunc.funcName.IsNone) bodyResult.funcBody
                { bodyResult with funcBody = body }

            // Step 3: build the specialized function's deferred formal params.
            let deferredFormalParams, deferredParamNames =
                buildSpecializedFormalParams lm codec o.resolvedType.acnParameters

            // Step 4: emit the specialized function (def + body) into auxiliaries.
            let allAuxiliaries =
                emitSpecializedFunctionDecl
                    lm codec t o specP typeDefinition isValidFunc errCode
                    specFuncName deferredFormalParams deferredParamNames finalBody

            // Step 5: build the caller's funcBody, record the cross-TAS call,
            // and wrap into an AcnFunction.
            buildCallerWrapper
                r deps lm codec t o typeDefinition isValidFunc
                specFuncName allAuxiliaries finalBody.userDefinedFunctions ns2


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
