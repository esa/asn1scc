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


// ---------------------------------------------------------------------------
//  Helper: map IntEncodingClass → C runtime InitDet/PatchDet function names
// ---------------------------------------------------------------------------

/// Maps an ACN integer encoding class (from AcnInteger.acnEncodingClass)
/// to the corresponding Acn_InitDet_XXX / Acn_PatchDet_XXX wrapper names
/// defined by the DEFINE_ACN_DET_ENCODERS macro in asn1crt_encoding_acn.h.
/// Returns (initFuncName, patchFuncName, nBitsOpt, uperMinOffset) where:
/// - nBitsOpt is Some nBits for generic ConstSize encodings that require an
///   extra bit-width argument, or None for fixed-size encodings (U8, U16, etc.)
/// - uperMinOffset is the UPER minimum value offset (0I for non-UPER encodings).
///   When non-zero, PatchDet must encode (value - offset) instead of value.
let mapIntEncodingClassToDetFunctions (enc: IntEncodingClass) : (string * string * BigInteger option * BigInteger) option =
    match enc with
    | PositiveInteger_ConstSize_8                    -> Some ("Acn_InitDet_U8",     "Acn_PatchDet_U8", None, 0I)
    | PositiveInteger_ConstSize_big_endian_16        -> Some ("Acn_InitDet_U16_BE", "Acn_PatchDet_U16_BE", None, 0I)
    | PositiveInteger_ConstSize_big_endian_32        -> Some ("Acn_InitDet_U32_BE", "Acn_PatchDet_U32_BE", None, 0I)
    | PositiveInteger_ConstSize_big_endian_64        -> Some ("Acn_InitDet_U64_BE", "Acn_PatchDet_U64_BE", None, 0I)
    | PositiveInteger_ConstSize_little_endian_16     -> Some ("Acn_InitDet_U16_LE", "Acn_PatchDet_U16_LE", None, 0I)
    | PositiveInteger_ConstSize_little_endian_32     -> Some ("Acn_InitDet_U32_LE", "Acn_PatchDet_U32_LE", None, 0I)
    | PositiveInteger_ConstSize_little_endian_64     -> Some ("Acn_InitDet_U64_LE", "Acn_PatchDet_U64_LE", None, 0I)
    | TwosComplement_ConstSize_8                     -> Some ("Acn_InitDet_I8",     "Acn_PatchDet_I8", None, 0I)
    | TwosComplement_ConstSize_big_endian_16         -> Some ("Acn_InitDet_I16_BE", "Acn_PatchDet_I16_BE", None, 0I)
    | TwosComplement_ConstSize_big_endian_32         -> Some ("Acn_InitDet_I32_BE", "Acn_PatchDet_I32_BE", None, 0I)
    | TwosComplement_ConstSize_big_endian_64         -> Some ("Acn_InitDet_I64_BE", "Acn_PatchDet_I64_BE", None, 0I)
    | PositiveInteger_ConstSize nBits                -> Some ("Acn_InitDet_ConstSize", "Acn_PatchDet_ConstSize", Some nBits, 0I)
    | TwosComplement_ConstSize nBits                 -> Some ("Acn_InitDet_TwosComplement_ConstSize", "Acn_PatchDet_TwosComplement_ConstSize", Some nBits, 0I)
    // Encoding classes without a deferred patching implementation
    | _ -> None


/// Map an AcnInsertedType to the corresponding InitDet/PatchDet function names.
/// Shared helper used by getDetFunctionsForAcnChild and findDetFunctionsForParam.
let getDetFunctionsForAcnInsertedType (acnType: Asn1AcnAst.AcnInsertedType) : (string * string * BigInteger option * BigInteger) option =
    match acnType with
    | Asn1AcnAst.AcnInsertedType.AcnInteger ai ->
        match ai.acnEncodingClass with
        | Integer_uPER when ai.acnMinSizeInBits = ai.acnMaxSizeInBits ->
            // Fixed-size UPER encoding → use ConstSize deferred patching.
            // UPER encodes (value - min), so PatchDet must subtract the min offset.
            let uperMinOffset =
                match ai.uperRange with
                | Concrete (minVal, _) -> minVal
                | _ -> 0I
            Some ("Acn_InitDet_ConstSize", "Acn_PatchDet_ConstSize", Some ai.acnMaxSizeInBits, uperMinOffset)
        | _ -> mapIntEncodingClassToDetFunctions ai.acnEncodingClass
    | Asn1AcnAst.AcnInsertedType.AcnBoolean bln ->
        match bln.acnProperties.encodingPattern with
        | None -> Some ("Acn_InitDet_BOOL1", "Acn_PatchDet_BOOL1", None, 0I)
        | Some _ -> None  // Custom true-value/false-value patterns: not yet supported
    | Asn1AcnAst.AcnInsertedType.AcnReferenceToEnumerated enm ->
        match enm.enumerated.acnEncodingClass with
        | Integer_uPER ->
            // For enums with UPER encoding: indices are always [0, N-1],
            // so ConstSize with ceil(log2(N)) bits is equivalent (offset = 0).
            let nItems = enm.enumerated.items.Length
            if nItems <= 1 then
                Some ("Acn_InitDet_ConstSize", "Acn_PatchDet_ConstSize", Some 0I, 0I)
            else
                let nBits = bigint (int (System.Math.Ceiling(System.Math.Log(float nItems, 2.0))))
                Some ("Acn_InitDet_ConstSize", "Acn_PatchDet_ConstSize", Some nBits, 0I)
        | _ ->
            mapIntEncodingClassToDetFunctions enm.enumerated.acnEncodingClass
    | Asn1AcnAst.AcnInsertedType.AcnReferenceToIA5String ref ->
        // IA5String determinant: fixed-size ASCII, 7 bits per character.
        // nChars is passed via the nBits slot of the _with_size calling convention.
        // maxSize.acn is already the character count (not bits).
        let nChars = ref.str.maxSize.acn
        Some ("Acn_InitDet_IA5String_FixSize", "Acn_PatchDet_IA5String_FixSize", Some nChars, 0I)
    | _ -> None

/// Get the InitDet/PatchDet function names for an ACN child determinant.
/// Returns None if the determinant type doesn't support deferred patching.
let getDetFunctionsForAcnChild (acnChild: DAst.AcnChild) : (string * string * BigInteger option * BigInteger) option =
    getDetFunctionsForAcnInsertedType acnChild.Type


/// Compute a valid default wire value for a deferred determinant that was
/// never patched at runtime — i.e., no PatchDet call was executed during
/// encoding.
///
/// This can happen whenever every code path that would call PatchDet is
/// skipped at runtime: an OPTIONAL child guarded by present-when is
/// absent, or the determinant's consumer sits inside a CHOICE branch
/// that was not taken, etc.
///
/// DEVIATION FROM INLINE MODE:
/// In the legacy (inline) encoding, the determinant value is always
/// computed in the parent function from the ASN.1 data (e.g., .nCount
/// for a size determinant, .exist.field for a presence boolean,
/// .kind for a CHOICE determinant, etc.).  Even when the consumer is
/// absent, the parent still derives a value from the struct — which
/// may be the field's default/zero initialization — and writes it to
/// the bitstream.
///
/// In deferred patching mode, PatchDet lives inside the consumer's
/// own function.  If that function is never called, no value is
/// written over the InitDet placeholder.  We must therefore supply a
/// fallback value explicitly.  The value chosen here is arbitrary but
/// must be valid for the determinant's wire encoding (otherwise the
/// decoder will reject the bitstream).  Since no consumer will
/// actually inspect this value, any valid value is semantically
/// correct — but the bitstream bytes may differ from inline mode in
/// the "no consumer executed" case.
///
/// Example (from 21-PresentWhenExpression/002):
///
///   -- ASN.1
///   MyTopMostSeq ::= SEQUENCE {
///       myChoice1 MyChoice OPTIONAL,
///       myChoice2 MyChoice OPTIONAL
///   }
///   -- ACN
///   MyTopMostSeq [] {
///       myDeterminant MyChoiceEnum [],
///       myBool1 BOOLEAN [],
///       myBool2 BOOLEAN [],
///       myChoice1 <myDeterminant> [present-when myBool1],
///       myChoice2 <myDeterminant> [present-when myBool2]
///   }
///
/// When both myChoice1 and myChoice2 are absent (myBool1=myBool2=FALSE),
/// no child calls PatchDet on myDeterminant.  Without this fallback, the
/// InitDet placeholder (all zeros) remains, but 0 is not a valid
/// MyChoiceEnum wire value (valid: 1=choice1, 2=choice2) -> decode fails.
/// The fallback patches with the first valid enum value (MyChoiceEnum_choice1).
///
/// NOTE (Ada / Scala portability):
/// This function currently uses language-specific helpers (e.g.
/// lm.lg.getNamedItemBackendName) to obtain C enum constant names.
/// When Ada / Scala backends gain deferred patching support, this
/// logic should be replaced with STG macros that emit the correct
/// default value expression per target language.
let computeFallbackDetValue (lm: LanguageMacros) (acnType: Asn1AcnAst.AcnInsertedType) (uperMinOffset: BigInteger) : string =
    match acnType with
    | Asn1AcnAst.AcnInsertedType.AcnInteger _ ->
        if uperMinOffset > 0I then uperMinOffset.ToString()
        else "0"
    | Asn1AcnAst.AcnInsertedType.AcnBoolean _ -> "0"
    | Asn1AcnAst.AcnInsertedType.AcnReferenceToEnumerated enm ->
        match enm.enumerated.items with
        | firstItem :: _ -> lm.lg.getNamedItemBackendName None firstItem
        | [] -> "0"
    | Asn1AcnAst.AcnInsertedType.AcnReferenceToIA5String _ -> "\"\""
    | _ -> "0"


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
            let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch dep.asn1Type.ModName
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
            let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch dep.asn1Type.ModName
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
let findDetFunctionsForParam (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (paramId: ReferenceToType) : (string * string * BigInteger option * BigInteger) option =
    let rec follow (pid: ReferenceToType) =
        deps.acnDependencies |> List.tryPick (fun d ->
            match d.dependencyKind with
            | AcnDepRefTypeArgument p when p.id = pid ->
                match d.determinant with
                | Asn1AcnAst.AcnChildDeterminant ac ->
                    getDetFunctionsForAcnInsertedType ac.Type
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

/// Create a **synthetic** AcnChild node that emits fallback PatchDet code.
///
/// This is NOT a real ASN.1 or ACN field.  It is a fake child node injected
/// at the END of the children list so that the fold in
/// createSequenceFunction_inline includes the fallback code inside the
/// nested if(ret) chain — and, critically, inside the TAS function text
/// that is baked by createAcnFunction (which cannot be modified after the
/// fact via closure wrapping).
///
/// For Encode, the funcBody emits the fallback PatchDet checks.
/// For Decode, it emits nothing (empty string).
///
/// See computeFallbackDetValue for the rationale behind the fallback
/// mechanism and the choice of default values.
let private makeFallbackPatchChild
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (parentType: Asn1AcnAst.Asn1Type)
        (fallbackCode: string) : SeqChildInfo =
    let syntheticName = "_fallback_patch"
    let (ReferenceToType parentPath) = parentType.id
    let syntheticId = ReferenceToType (parentPath @ [SQF])
    let dummyLoc = parentType.Location
    let dummyNullType = Asn1AcnAst.AcnInsertedType.AcnNullType {
        Asn1AcnAst.AcnNullType.acnProperties = { NullTypeAcnProperties.encodingPattern = None; savePosition = false }
        acnAlignment = None
        acnMaxSizeInBits = 0I
        acnMinSizeInBits = 0I
        Location = dummyLoc
        defaultValue = ""
    }
    let funcBody : CommonTypes.Codec -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CodegenScope -> string -> (AcnFuncBodyResult option) =
        fun innerCodec _acnArgs _nestingScope _p _bsPos ->
            let body =
                match innerCodec with
                | CommonTypes.Codec.Encode -> fallbackCode
                | CommonTypes.Codec.Decode -> ""
            Some {
                AcnFuncBodyResult.funcBody = body
                errCodes = []
                localVariables = []
                bValIsUnReferenced = false
                bBsIsUnReferenced = false
                resultExpr = None
                auxiliaries = []
                icdResult = None
                userDefinedFunctions = []
            }
    DAst.AcnChild {
        DAst.AcnChild.Name = { StringLoc.Value = syntheticName; Location = dummyLoc }
        c_name = syntheticName
        id = syntheticId
        Type = dummyNullType
        typeDefinitionBodyWithinSeq = ""
        funcBody = funcBody
        funcUpdateStatement = None
        Comments = [||]
        deps = { Asn1AcnAst.AcnInsertedFieldDependencies.acnDependencies = [] }
        initExpression = ""
    }


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
        DAstACN.createSequenceFunction_inline r deps lm codec t o typeDefinition isValidFunc children acnPrms us
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
                    let detFuncs = getDetFunctionsForAcnChild ac
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

        // Collect fallback PatchDet info for local deferred dets (encode only).
        // When all consumers of a shared determinant are absent at runtime
        // (e.g., present-when booleans are false, or a CHOICE branch was not
        // taken), no PatchDet is called and the InitDet placeholder (zeros)
        // remains.  The fallback patches with a valid default so the decoder
        // does not reject the bitstream.  See computeFallbackDetValue for
        // detailed rationale.
        //
        // We inject this as a **synthetic child** appended at the END of the
        // children list.  This is NOT a real ASN.1 or ACN field — it is a
        // fake AcnChild node whose sole purpose is to emit the fallback code
        // inside the fold's nested if(ret) chain, which is the only way to
        // get it into the TAS function text (baked inside createAcnFunction).
        let fallbackChildren =
            match codec with
            | CommonTypes.Codec.Encode ->
                let fallbackDets =
                    children |> List.choose (fun child ->
                        match child with
                        | DAst.AcnChild ac when Set.contains ac.Name.Value deferredDetNames
                                             && not (Set.contains ac.Name.Value deferredDetNamesFromOwnParams) ->
                            match getDetFunctionsForAcnChild ac with
                            | Some (_initFn, patchFn, nBitsOpt, uperMinOffset) ->
                                let detVarName = ToC ac.Name.Value
                                let defaultVal = computeFallbackDetValue lm ac.Type uperMinOffset
                                Some (detVarName, patchFn, nBitsOpt, defaultVal, ac.Type)
                            | None -> None
                        | _ -> None)
                match fallbackDets with
                | [] -> []
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
                    [makeFallbackPatchChild lm codec t fallbackCode]
            | CommonTypes.Codec.Decode -> []

        let allChildren = modifiedChildren @ fallbackChildren

        DAstACN.createSequenceFunction_inline r deps lm codec t o typeDefinition isValidFunc allChildren acnPrms us


// ---------------------------------------------------------------------------
//  Deferred REFERENCE function
// ---------------------------------------------------------------------------

let private callBaseTypeFunc (lm:LanguageMacros) = lm.uper.call_base_type_func

/// Build a function-call string and insert extra actual parameters before
/// the closing ");".  For example:
///   "ret = Fn(pVal, pBitStrm, pErrCode, FALSE);"
/// becomes:
///   "ret = Fn(pVal, pBitStrm, pErrCode, FALSE, &det1);"
let private insertActualParams (baseFuncCall: string) (extraActualParams: string list) : string =
    if extraActualParams.IsEmpty then
        baseFuncCall
    else
        let insertIdx = baseFuncCall.LastIndexOf(")")
        if insertIdx > 0 then
            baseFuncCall.[..insertIdx-1] + ", " + (extraActualParams |> String.concat ", ") + baseFuncCall.[insertIdx..]
        else
            baseFuncCall


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

    let isContainingExternalField =
        match o.encodingOptions with
        | Some enc -> match enc.acnEncodingClass, enc.octOrBitStr with
                      | Asn1AcnAst.SZ_EC_ExternalField _, CommonTypes.ContainedInOctString -> true
                      | Asn1AcnAst.SZ_EC_ExternalField _, CommonTypes.ContainedInBitString -> true
                      | _ -> false
        | None -> false

    // Helper: create a simple funcBody from an STG template call (for CONTAINING FIXED/EMBEDDED)
    let makeContainingFuncBody (stgCall: string -> string -> (AcnFuncBodyResult option) * State) =
        let soSparkAnnotations = Some(DAstACN.sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules "") codec)
        let funcBody (us:State) (errCode:ErrorCode) (_acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (_nestingScope: NestingScope) (p:CodegenScope) =
            let pp = lm.lg.getParamValue t p.accessPath codec
            stgCall pp baseFncName
        DAstACN.createAcnFunction r deps lm codec t typeDefinition isValidFunc
            (fun us e acnArgs nestingScope p -> funcBody us e acnArgs nestingScope p)
            (fun _atc -> true) soSparkAnnotations [] [] us
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
                Some (DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType [] us)
            | _ -> None  // ExternalField with params → specialized function below
        | None when o.resolvedType.acnParameters.Length = 0 ->
            Some (DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType [] us)
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
            let errFieldPath = match t.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
            let errCode, ns1 = getNextValidErrorCode us errCodeName None errFieldPath

            let specP : CodegenScope = lm.lg.getParamType t codec

            // After boundary post-processing rewrites &name → formal param name
            // in the body text, the matching AcnInsertedFieldRef local variable
            // declarations injected by inner callerFuncBodies become orphans.
            // Strip them — they are now provided by this function's formal params.
            let stripOwnParamLocals (locals: LocalVariable list) =
                let ownParamNames =
                    o.resolvedType.acnParameters
                    |> List.map (fun prm -> ToC prm.name)
                    |> Set.ofList
                let detTypeName = lm.acn.acn_deferred_det_type_name()
                locals |> List.filter (fun lv ->
                    match lv with
                    | GenericLocalVariable gl ->
                        not (gl.varType = detTypeName && Set.contains gl.name ownParamNames)
                    | _ -> true)

            // Generate body content
            let bodyResult_funcBody0, bodyResult_errCodes, bodyResult_localVariables, bBsIsUnreferenced, bVarNameIsUnreferenced, bodyResult_udfcs, bodyResult_auxiliaries, ns2 =
                match isContainingExternalField, baseAcnFunc.funcName.IsNone with
                | true, true ->
                    // CONTAINING ExternalField where base type has no standalone function
                    // (parameterized type): use funcBody closure + CONTAINING wrapper.
                    // The CONTAINING size parameter was added by closure conversion and
                    // is NOT in o.acnArguments.  The arguments correspond to the first
                    // N parameters (the original ones); extra params from closure
                    // conversion are at the end.
                    let containingSizePrm = findContainingSizeParam deps o
                    let nArgs = o.acnArguments.Length
                    let paramsArgsPairs = List.zip o.acnArguments (o.resolvedType.acnParameters |> List.take nArgs)
                    let bodyContent, ns2 = baseAcnFunc.funcBody ns1 paramsArgsPairs (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) specP
                    match bodyContent with
                    | None ->
                        lm.lg.emptyStatement, [], [], true, true, [], [], ns2
                    | Some br ->
                        // Apply intermediate boundary post-processing (same as non-CONTAINING)
                        let funcBody0 =
                            o.resolvedType.acnParameters |> List.fold (fun (body: string) prm ->
                                let localRef = lm.acn.acn_deferred_det_actual_param (ToC prm.name) codec
                                let paramRef = DAstACN.getAcnDeterminantName prm.id
                                body.Replace(localRef, paramRef)
                            ) br.funcBody

                        let wrappedBody =
                            match containingSizePrm with
                            | Some sizePrm ->
                                let detParamName = DAstACN.getAcnDeterminantName sizePrm.id
                                let patchFnName =
                                    match findDetFunctionsForParam deps sizePrm.id with
                                    | Some (_, patchFn, _, _) -> patchFn
                                    | None -> ""
                                match o.encodingOptions.Value.octOrBitStr with
                                | CommonTypes.ContainedInOctString ->
                                    lm.acn.octet_string_containing_deferred_wrapper funcBody0 detParamName patchFnName errCode.errCodeName codec
                                | CommonTypes.ContainedInBitString ->
                                    lm.acn.bit_string_containing_deferred_wrapper funcBody0 detParamName patchFnName errCode.errCodeName codec
                            | None ->
                                funcBody0  // fallback: no wrapping if size param not found

                        wrappedBody, br.errCodes @ [errCode], stripOwnParamLocals br.localVariables,
                            br.bBsIsUnReferenced, br.bValIsUnReferenced,
                            br.userDefinedFunctions, br.auxiliaries, ns2
                | true, false ->
                    // CONTAINING ExternalField with standalone base function (e.g., TC01b):
                    // use existing STG template approach
                    let pp =
                        let str = lm.lg.getParamValue t specP.accessPath codec
                        match codec, lm.lg.decodingKind with
                        | Decode, Copy -> ToC str
                        | _ -> str
                    let sizePrm = o.resolvedType.acnParameters.Head
                    let detParamName = DAstACN.getAcnDeterminantName sizePrm.id
                    let patchFnName =
                        match findDetFunctionsForParam deps sizePrm.id with
                        | Some (_initFn, patchFn, _, _) -> patchFn
                        | None -> ""
                    let fncBody =
                        match o.encodingOptions.Value.octOrBitStr with
                        | CommonTypes.ContainedInOctString ->
                            lm.acn.octet_string_containing_deferred_func pp baseFncName detParamName patchFnName errCode.errCodeName codec
                        | CommonTypes.ContainedInBitString ->
                            lm.acn.bit_string_containing_deferred_func pp baseFncName detParamName patchFnName errCode.errCodeName codec
                    fncBody, [errCode], [], false, false, [], [], ns1
                | _ ->
                    // Normal: call base type's funcBody closure
                    let paramsArgsPairs = List.zip o.acnArguments o.resolvedType.acnParameters
                    let bodyContent, ns2 = baseAcnFunc.funcBody ns1 paramsArgsPairs (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) specP
                    match bodyContent with
                    | None ->
                        lm.lg.emptyStatement, [], [], true, true, [], [], ns2
                    | Some br ->
                        // For intermediate boundaries (SEQUENCEs that receive params
                        // and forward them to children): the SEQUENCE body passes []
                        // as acnArgs to children, so child callerFuncBodies generate
                        // &argName (local variable).  Replace these with the formal
                        // parameter name for pointer pass-through.
                        let funcBody0 =
                            o.resolvedType.acnParameters |> List.fold (fun (body: string) prm ->
                                let localRef = lm.acn.acn_deferred_det_actual_param (ToC prm.name) codec
                                let paramRef = DAstACN.getAcnDeterminantName prm.id
                                body.Replace(localRef, paramRef)
                            ) br.funcBody
                        funcBody0, br.errCodes, stripOwnParamLocals br.localVariables, br.bBsIsUnReferenced, br.bValIsUnReferenced, br.userDefinedFunctions, br.auxiliaries, ns2

            // Step 2b (Encode only): Append PatchDet calls for each consumer-side
            // acnParameter.  For CONTAINING ExternalField with standalone base function
            // (old approach), skip — PatchDet is already in the STG template.
            // For CONTAINING ExternalField with funcBody closure (new approach),
            // allow PatchDet but exclude the CONTAINING size parameter (already in wrapper).
            let bodyResult_funcBody =
                match isContainingExternalField, baseAcnFunc.funcName.IsNone with
                | true, false -> bodyResult_funcBody0  // old approach: PatchDet is in STG template
                | _ ->
                    match codec with
                    | CommonTypes.Codec.Decode -> bodyResult_funcBody0
                    | CommonTypes.Codec.Encode ->
                        // Identify CONTAINING size param to exclude from step 2b
                        // Always check (not just when isContainingExternalField) because
                        // named CONTAINING aliases have two ref levels — the outer ref
                        // has no encodingOptions but still carries the same size parameter.
                        let containingSizePrmId =
                            findContainingSizeParam deps o
                            |> Option.map (fun prm -> prm.id)
                        let patchCalls =
                            o.resolvedType.acnParameters |> List.choose (fun prm ->
                                // Skip CONTAINING size param (already handled by wrapper)
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
                                    let originalDetType = findDetFunctionsForParam deps prm.id
                                    match originalDetType with
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
                        | [] -> bodyResult_funcBody0
                        | calls ->
                            let patchCode = calls |> String.concat "\n"
                            bodyResult_funcBody0 + "\n" + patchCode

            // Step 3: Build the specialized function's formal parameters.
            // Standard params come from EmitTypeAssignment_primitive; the extra
            // AcnInsertedFieldRef* params are appended via the prms list.
            // Use getAcnDeterminantName for parameter names so they match
            // what getExternalField0 / resolveParam produces inside the body.
            // E.g., prm.id = MyModule.PDU.payload.buffers-length
            //   → getAcnDeterminantName → "PDU_payload_buffers_length"
            //   → formal param: "AcnInsertedFieldRef* PDU_payload_buffers_length"
            let deferredFormalParams =
                o.resolvedType.acnParameters |> List.map (fun prm ->
                    lm.acn.acn_deferred_det_formal_param (DAstACN.getAcnDeterminantName prm.id) codec)
            let deferredParamNames =
                o.resolvedType.acnParameters |> List.map (fun prm -> DAstACN.getAcnDeterminantName prm.id)

            // Step 4: Emit the specialized function using EmitTypeAssignment_primitive.
            let varName = specP.accessPath.rootId
            let sStar = lm.lg.getStar specP.accessPath
            let typeDefinitionName = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
            let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
            let soInitFuncName = lm.lg.getFuncNameGeneric typeDefinition (lm.init.methodNameSuffix())
            let nMaxBytesInACN = BigInteger (ceil ((double t.acnMaxSizeInBits)/8.0))
            let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct

            // In deferred mode, pVal may be unreferenced if the resolved type
            // has only ACN-inserted children and no ASN.1 data fields (e.g., Header
            // with only version pattern + buffers-length determinant).
            let hasAsn1Children =
                match o.resolvedType.Kind with
                | Asn1AcnAst.Asn1TypeKind.Sequence sq ->
                    sq.children |> List.exists (fun c -> match c with Asn1AcnAst.Asn1Child _ -> true | _ -> false)
                | _ -> true
            let bVarNameIsUnreferenced = bVarNameIsUnreferenced || not hasAsn1Children

            let specFuncBody =
                lm.acn.EmitTypeAssignment_primitive
                    varName sStar specFuncName isValidFuncName typeDefinitionName
                    lvars bodyResult_funcBody
                    None ""  // soSparkAnnotations, sInitialExp
                    deferredFormalParams deferredParamNames
                    (t.acnMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced
                    false
                    soInitFuncName [] [] []  // funcDefAnnots, precondAnnots, postcondAnnots
                    codec

            let specErrCodStr =
                (errCode :: bodyResult_errCodes)
                |> List.groupBy (fun x -> x.errCodeName)
                |> List.map (fun (k, v) -> {errCodeName = k; errCodeValue = v.Head.errCodeValue; comment = v.Head.comment; fieldPath = v.Head.fieldPath})
                |> List.map (fun x -> lm.acn.EmitTypeAssignment_def_err_code x.errCodeName (BigInteger x.errCodeValue) x.comment x.fieldPath)
                |> List.distinct

            let specFuncDef =
                lm.acn.EmitTypeAssignment_primitive_def
                    varName sStar specFuncName typeDefinitionName
                    specErrCodStr
                    (t.acnMaxSizeInBits = 0I) nMaxBytesInACN t.acnMaxSizeInBits
                    deferredFormalParams
                    None  // soSparkAnnotations
                    codec

            // All auxiliary strings: the specialized function definition + body,
            // plus any auxiliaries from the base type's encoding body.
            let allAuxiliaries = bodyResult_auxiliaries @ [specFuncDef; specFuncBody]

            // Step 5: Build the caller's funcBody — a simple function call to
            // the specialized function with &det actual parameters.
            let callerFuncBody (callerUs:State) (callerErrCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
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

                // Declaration-level AcnInsertedFieldRef locals.  Distinct
                // by name in case the same determinant is referenced via
                // multiple acnArguments paths (e.g. hdr.x and direct x);
                // duplicates would be deduped later anyway, but we save
                // work by deduping here.
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

                Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [callerErrCode]; localVariables = extraLocals; userDefinedFunctions = bodyResult_udfcs; bValIsUnReferenced = false; bBsIsUnReferenced = false; resultExpr = resultExpr; auxiliaries = allAuxiliaries; icdResult = None}), callerUs

            // Record the function call dependency (caller → callee)
            let ns3 =
                match t.id.topLevelTas with
                | None -> ns2
                | Some tasInfo ->
                    let caller = {Caller.typeId = tasInfo; funcType=AcnEncDecFunctionType}
                    let callee = {Callee.typeId = {TypeAssignmentInfo.modName = o.modName.Value; tasName=o.tasName.Value} ; funcType=AcnEncDecFunctionType}
                    addFunctionCallToState ns2 caller callee

            // Wrap the caller funcBody into an AcnFunction using createAcnFunction
            let soSparkAnnotations = Some(DAstACN.sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
            let a, ns4 = DAstACN.createAcnFunction r deps lm codec t typeDefinition isValidFunc callerFuncBody (fun _atc -> true) soSparkAnnotations [] [] ns3
            Some a, ns4


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
    | false -> DAstACN.createSequenceFunction_inline r deps lm codec t o typeDefinition isValidFunc children acnPrms us


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
    | false -> DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType [] us
