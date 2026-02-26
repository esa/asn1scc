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
let mapIntEncodingClassToDetFunctions (enc: IntEncodingClass) : (string * string) option =
    match enc with
    | PositiveInteger_ConstSize_8                    -> Some ("Acn_InitDet_U8",     "Acn_PatchDet_U8")
    | PositiveInteger_ConstSize_big_endian_16        -> Some ("Acn_InitDet_U16_BE", "Acn_PatchDet_U16_BE")
    | PositiveInteger_ConstSize_big_endian_32        -> Some ("Acn_InitDet_U32_BE", "Acn_PatchDet_U32_BE")
    | PositiveInteger_ConstSize_big_endian_64        -> Some ("Acn_InitDet_U64_BE", "Acn_PatchDet_U64_BE")
    | PositiveInteger_ConstSize_little_endian_16     -> Some ("Acn_InitDet_U16_LE", "Acn_PatchDet_U16_LE")
    | PositiveInteger_ConstSize_little_endian_32     -> Some ("Acn_InitDet_U32_LE", "Acn_PatchDet_U32_LE")
    | PositiveInteger_ConstSize_little_endian_64     -> Some ("Acn_InitDet_U64_LE", "Acn_PatchDet_U64_LE")
    | TwosComplement_ConstSize_8                     -> Some ("Acn_InitDet_I8",     "Acn_PatchDet_I8")
    | TwosComplement_ConstSize_big_endian_16         -> Some ("Acn_InitDet_I16_BE", "Acn_PatchDet_I16_BE")
    | TwosComplement_ConstSize_big_endian_32         -> Some ("Acn_InitDet_I32_BE", "Acn_PatchDet_I32_BE")
    | TwosComplement_ConstSize_big_endian_64         -> Some ("Acn_InitDet_I64_BE", "Acn_PatchDet_I64_BE")
    // Encoding classes without a fixed-size macro instantiation — not supported for deferred patching
    | _ -> None


/// Map an AcnInsertedType to the corresponding InitDet/PatchDet function names.
/// Shared helper used by getDetFunctionsForAcnChild and findDetFunctionsForParam.
let getDetFunctionsForAcnInsertedType (acnType: Asn1AcnAst.AcnInsertedType) : (string * string) option =
    match acnType with
    | Asn1AcnAst.AcnInsertedType.AcnInteger ai ->
        mapIntEncodingClassToDetFunctions ai.acnEncodingClass
    | Asn1AcnAst.AcnInsertedType.AcnBoolean bln ->
        match bln.acnProperties.encodingPattern with
        | None -> Some ("Acn_InitDet_BOOL1", "Acn_PatchDet_BOOL1")
        | Some _ -> None  // Custom true-value/false-value patterns: not yet supported
    | Asn1AcnAst.AcnInsertedType.AcnReferenceToEnumerated enm ->
        mapIntEncodingClassToDetFunctions enm.enumerated.acnEncodingClass
    | _ -> None

/// Get the InitDet/PatchDet function names for an ACN child determinant.
/// Returns None if the determinant type doesn't support deferred patching.
let getDetFunctionsForAcnChild (acnChild: DAst.AcnChild) : (string * string) option =
    getDetFunctionsForAcnInsertedType acnChild.Type


/// Build a C access expression from relative scope nodes and the root pointer.
/// Returns (accessExpr, accessor) where accessor is "->" if result is a pointer,
/// "." if result is a struct member.
/// E.g., [] + "pVal" → ("pVal", "->")
///       [SEQ_CHILD("buffer",_)] + "pVal" → ("pVal->buffer", ".")
let buildRelativeAccess (relParts: ScopeNode list) (rootId: string) : (string * string) =
    match relParts with
    | [] -> (rootId, "->")
    | _ ->
        let parts = relParts |> List.map (fun node ->
            match node with
            | SEQ_CHILD (name, _) -> ToC name
            | CH_CHILD (name, _, _) -> ToC name
            | _ -> failwithf "BUG: unexpected scope node %A in relative access" node)
        let fieldAccess = sprintf "%s->%s" rootId (parts |> String.concat ".")
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
        let (fieldExpr, acc) = buildRelativeAccess relParts rootId
        (None, sprintf "%s%snCount" fieldExpr acc)

    | Asn1AcnAst.AcnDepIA5StringSizeDeterminant _ ->
        let (fieldExpr, acc) = buildRelativeAccess relParts rootId
        (None, sprintf "strlen(%s%sarr)" fieldExpr acc)

    | Asn1AcnAst.AcnDepPresenceBool ->
        // dep.asn1Type points to the OPTIONAL child; parent is one level up
        let parentRelParts = relParts |> List.rev |> List.tail |> List.rev
        let (parentExpr, pAcc) = buildRelativeAccess parentRelParts rootId
        let childName = ToC (dep.asn1Type.lastItem)
        (None, sprintf "(%s%sexist.%s == 1)" parentExpr pAcc childName)

    | Asn1AcnAst.AcnDepPresence (relPath, chc) ->
        let (choiceExpr, acc) = buildRelativeAccess relParts rootId
        let kindAccess = sprintf "%s%skind" choiceExpr acc
        let varName = "_patchDetVal"
        let switchItems = chc.children |> List.map (fun ch ->
            let pres = ch.acnPresentWhenConditions |> Seq.find (fun x -> x.relativePath = relPath)
            let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch
            match pres with
            | AcnGenericTypes.PresenceInt (_, intVal) ->
                sprintf "    case %s: %s = %s; break;" presentWhenName varName (intVal.Value.ToString())
            | AcnGenericTypes.PresenceStr _ ->
                failwithf "BUG: PresenceStr not supported for deferred patching")
        let switchBlock =
            sprintf "asn1SccUint %s;\nswitch(%s) {\n%s\n    default: ret = FALSE; break;\n}\nif (!ret) return FALSE;"
                varName kindAccess (switchItems |> String.concat "\n")
        (Some switchBlock, varName)

    | Asn1AcnAst.AcnDepChoiceDeterminant (enm, chc, _isOptional) ->
        let (choiceExpr, acc) = buildRelativeAccess relParts rootId
        let kindAccess = sprintf "%s%skind" choiceExpr acc
        let varName = "_patchDetVal"
        let switchItems = chc.children |> List.map (fun ch ->
            let enmItem = enm.enm.items |> List.find (fun itm -> itm.Name.Value = ch.Name.Value)
            let presentWhenName = ch.presentWhenName
            let enumCName = lm.lg.getNamedItemBackendName None enmItem
            sprintf "    case %s: %s = %s; break;" presentWhenName varName enumCName)
        let switchBlock =
            sprintf "asn1SccUint %s;\nswitch(%s) {\n%s\n    default: ret = FALSE; break;\n}\nif (!ret) return FALSE;"
                varName kindAccess (switchItems |> String.concat "\n")
        (Some switchBlock, varName)

    | _ ->
        failwithf "BUG: PatchDet not yet implemented for dependency kind %A" dep.dependencyKind


/// Follow the RefTypeArgumentDependency chain from a parameter upward
/// through intermediate boundaries until reaching the original
/// AcnChildDeterminant.  Returns the (InitDet, PatchDet) function names.
let findDetFunctionsForParam (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (paramId: ReferenceToType) : (string * string) option =
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

/// Create a synthetic AcnChild that only declares an AcnInsertedFieldRef
/// local variable.  It produces no encoding output — the variable is used
/// by child specialized functions that receive &name as actual parameter.
let private makeDeferredDetDeclaration
        (lm: LanguageMacros)
        (codec: CommonTypes.Codec)
        (parentType: Asn1AcnAst.Asn1Type)
        (detName: string) : SeqChildInfo =
    let c_name = ToC detName
    let (ReferenceToType parentPath) = parentType.id
    let syntheticId = ReferenceToType (parentPath @ [SQF])  // dummy scope node
    let dummyLoc = parentType.Location
    let dummyNullType = Asn1AcnAst.AcnInsertedType.AcnNullType {
        Asn1AcnAst.AcnNullType.acnProperties = { NullTypeAcnProperties.encodingPattern = None; savePosition = false }
        acnAlignment = None
        acnMaxSizeInBits = 0I
        acnMinSizeInBits = 0I
        Location = dummyLoc
        defaultValue = ""
    }
    // funcBody returns no encoding but declares the AcnInsertedFieldRef local variable
    let funcBody : CommonTypes.Codec -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CodegenScope -> string -> (AcnFuncBodyResult option) =
        fun _codec _acnArgs _nestingScope _p _bsPos ->
            Some {
                AcnFuncBodyResult.funcBody = ""
                errCodes = []
                localVariables = [GenericLocalVariable { name = c_name; varType = "AcnInsertedFieldRef"; arrSize = None; isStatic = false; initExp = Some "{0}" }]
                bValIsUnReferenced = false
                bBsIsUnReferenced = false
                resultExpr = None
                auxiliaries = []
                icdResult = None
                userDefinedFunctions = []
            }
    DAst.AcnChild {
        DAst.AcnChild.Name = { StringLoc.Value = detName; Location = dummyLoc }
        c_name = c_name
        id = syntheticId
        Type = dummyNullType
        typeDefinitionBodyWithinSeq = "AcnInsertedFieldRef"
        funcBody = funcBody
        funcUpdateStatement = None
        Comments = [||]
        deps = { Asn1AcnAst.AcnInsertedFieldDependencies.acnDependencies = [] }
        initExpression = "{0}"
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
                    | Some (initFuncName, _patchFuncName) ->
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
                                        if isOwnParam then
                                            lm.acn.acn_deferred_det_init_ptr initFuncName detVarName innerCodec
                                        else
                                            lm.acn.acn_deferred_det_init_value initFuncName detVarName innerCodec
                                    Some {
                                        AcnFuncBodyResult.funcBody = initCode
                                        errCodes = []
                                        localVariables =
                                            if isOwnParam then []
                                            else [GenericLocalVariable { name = detVarName; varType = "AcnInsertedFieldRef"; arrSize = None; isStatic = false; initExp = Some "{0}" }]
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
                                    // For AcnInteger this works directly (asn1SccUint matches).
                                    // For AcnBoolean and AcnReferenceToEnumerated, the
                                    // funcBody generates intermediate variables with names
                                    // derived from the target path.  A path containing
                                    // "->" or "." corrupts those variable names, so we
                                    // decode to a clean temp variable and copy afterward.
                                    let useDirectRedirect =
                                        match ac.Type with
                                        | Asn1AcnAst.AcnInsertedType.AcnInteger _ -> true
                                        | _ -> false
                                    if useDirectRedirect then
                                        let valueTarget =
                                            if isOwnParam then detVarName + "->value"
                                            else detVarName + ".value"
                                        let modifiedP = {p with accessPath = AccessPath.valueEmptyPath valueTarget}
                                        let result = originalFuncBody innerCodec acnArgs nestingScope modifiedP bsPos
                                        match result with
                                        | Some r ->
                                            let extraLvars =
                                                if isOwnParam then []
                                                else [GenericLocalVariable { name = detVarName; varType = "AcnInsertedFieldRef"; arrSize = None; isStatic = false; initExp = Some "{0}" }]
                                            Some { r with localVariables = extraLvars @ r.localVariables }
                                        | None -> None
                                    else
                                        // Decode to a clean temp variable, then copy to det.value
                                        let tmpName = detVarName + "_tmp"
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
                                                if isOwnParam then detVarName + "->value"
                                                else detVarName + ".value"
                                            let assignStmt = sprintf "%s = (asn1SccUint)%s;" detAccess tmpName
                                            let extraLvars =
                                                [tmpVarDecl] @
                                                (if isOwnParam then []
                                                 else [GenericLocalVariable { name = detVarName; varType = "AcnInsertedFieldRef"; arrSize = None; isStatic = false; initExp = Some "{0}" }])
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

        // For deferred det names that are NOT direct ACN children of this
        // SEQUENCE (i.e., the determinant lives inside a child reference type),
        // create synthetic AcnChild entries that only declare the
        // AcnInsertedFieldRef local variable.  These are prepended so the
        // variable is in scope when child function calls reference &name.
        // Exclude own parameters — they arrive as formal AcnInsertedFieldRef*
        // parameters and don't need a local variable declaration.
        let missingDetNames =
            deferredDetNamesFromChildren
            |> Set.filter (fun name -> not (Set.contains name directAcnChildNames))
            |> Set.filter (fun name -> not (Set.contains name deferredDetNamesFromOwnParams))
        let syntheticChildren =
            missingDetNames
            |> Set.toList
            |> List.map (makeDeferredDetDeclaration lm codec t)

        let allChildren = syntheticChildren @ modifiedChildren

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

    let _baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t.id "_ACN" codec

    let isContainingExternalField =
        match o.encodingOptions with
        | Some enc -> match enc.acnEncodingClass, enc.octOrBitStr with
                      | Asn1AcnAst.SZ_EC_ExternalField _, CommonTypes.ContainedInOctString -> true
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
            | _ when not isContainingExternalField ->
                Some (DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType us)
            | _ -> None  // ExternalField with params → specialized function below
        | None when o.resolvedType.acnParameters.Length = 0 ->
            Some (DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType us)
        | _ -> None  // has params → specialized function below

    match earlyReturn with
    | Some result -> result
    | None ->

        // --- Generate specialized function per reference site ---
        // ExternalField CONTAINING (with params) OR normal reference (with params)

        let specFuncName =
            let pathStr = t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin "_"
            ToC2(r.args.TypePrefix + pathStr) + "_ACN" + codec.suffix

        let baseTypeAcnFunction = baseType.getAcnFunction codec
        match baseTypeAcnFunction with
        | None -> None, us
        | Some baseAcnFunc ->

            let errCodeName = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
            let errCode, ns1 = getNextValidErrorCode us errCodeName None

            let specP : CodegenScope = lm.lg.getParamType t codec

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
                                    | Some (_, patchFn) -> patchFn
                                    | None -> ""
                                lm.acn.octet_string_containing_deferred_wrapper funcBody0 detParamName patchFnName errCode.errCodeName codec
                            | None ->
                                funcBody0  // fallback: no wrapping if size param not found

                        wrappedBody, br.errCodes @ [errCode], br.localVariables,
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
                        | Some (_initFn, patchFn) -> patchFn
                        | None -> ""
                    let fncBody = lm.acn.octet_string_containing_deferred_func pp baseFncName detParamName patchFnName errCode.errCodeName codec
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
                        funcBody0, br.errCodes, br.localVariables, br.bBsIsUnReferenced, br.bValIsUnReferenced, br.userDefinedFunctions, br.auxiliaries, ns2

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
                        let containingSizePrmId =
                            if isContainingExternalField then
                                findContainingSizeParam deps o
                                |> Option.map (fun prm -> prm.id)
                            else None
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
                                    let originalDetType = findDetFunctionsForParam deps prm.id
                                    match originalDetType with
                                    | None -> None
                                    | Some (_initFn, patchFn) ->
                                        let (preBlock, valueExpr) = computePatchDetValueExpr lm dep (t.id.ToScopeNodeList) specP.accessPath.rootId
                                        let detParamName = DAstACN.getAcnDeterminantName prm.id
                                        let errCodePatch = "ERR_ACN_DET_CONSISTENCY_MISMATCH"
                                        let patchCall = lm.acn.acn_deferred_det_patch_ptr patchFn valueExpr detParamName errCodePatch codec
                                        let fullBlock =
                                            match preBlock with
                                            | None -> patchCall
                                            | Some pre -> sprintf "{\n%s\n%s\n}" pre patchCall
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
            let typeDefinitionName = typeDefinition.longTypedefName2 lm.lg.hasModules
            let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
            let soInitFuncName = getFuncNameGeneric typeDefinition (lm.init.methodNameSuffix())
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
                    soInitFuncName [] [] None  // funcDefAnnots, precondAnnots, postcondAnnots
                    codec

            let specErrCodStr =
                (errCode :: bodyResult_errCodes)
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

                let extraActualParams =
                    o.acnArguments |> List.map (fun arg ->
                        let (AcnGenericTypes.RelativePath parts) = arg
                        let argName = parts |> List.last |> fun sl -> sl.Value
                        // Check if this argument maps to a parent parameter
                        // (intermediate level, e.g., inside a CHOICE specialized
                        // function).  If so, pass the parent's formal param
                        // directly (pointer pass-through, no &).
                        let parentParam =
                            acnArgs |> List.tryFind (fun (_, prm) -> prm.name = argName)
                        match parentParam with
                        | Some (_, prm) ->
                            // Intermediate level: pass parent's formal param name
                            DAstACN.getAcnDeterminantName prm.id
                        | None ->
                            // Declaration level: address of local variable
                            lm.acn.acn_deferred_det_actual_param (ToC argName) codec)

                let baseFuncCall = callBaseTypeFunc lm pp specFuncName codec
                let funcBodyContent = insertActualParams baseFuncCall extraActualParams

                Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [callerErrCode]; localVariables = []; userDefinedFunctions = bodyResult_udfcs; bValIsUnReferenced = false; bBsIsUnReferenced = false; resultExpr = resultExpr; auxiliaries = allAuxiliaries; icdResult = None}), callerUs

            // Record the function call dependency (caller → callee)
            let ns3 =
                match t.id.topLevelTas with
                | None -> ns2
                | Some tasInfo ->
                    let caller = {Caller.typeId = tasInfo; funcType=AcnEncDecFunctionType}
                    let callee = {Callee.typeId = {TypeAssignmentInfo.modName = o.modName.Value; tasName=o.tasName.Value} ; funcType=AcnEncDecFunctionType}
                    addFunctionCallToState ns2 caller callee

            // Wrap the caller funcBody into an AcnFunction using createAcnFunction
            let soSparkAnnotations = Some(DAstACN.sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
            let a, ns4 = DAstACN.createAcnFunction r deps lm codec t typeDefinition isValidFunc callerFuncBody (fun _atc -> true) soSparkAnnotations [] ns3
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
    | false -> DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType us
