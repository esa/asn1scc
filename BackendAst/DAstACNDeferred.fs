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


/// Get the InitDet/PatchDet function names for an ACN child determinant.
/// Returns None if the determinant type doesn't support deferred patching.
let getDetFunctionsForAcnChild (acnChild: DAst.AcnChild) : (string * string) option =
    match acnChild.Type with
    | Asn1AcnAst.AcnInsertedType.AcnInteger ai ->
        mapIntEncodingClassToDetFunctions ai.acnEncodingClass
    | _ -> None


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
            |> List.collect (fun arg ->
                let (AcnGenericTypes.RelativePath parts) = arg
                parts |> List.map (fun sl -> sl.Value))
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

    let deferredDetNames = collectDeferredDetNames children

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
                        // Replace encoding with InitDet:
                        // - funcBody emits Acn_InitDet_XXX call
                        // - funcUpdateStatement = None (value patched later by child functions)
                        // - Type = AcnNullType (bypasses _is_initialized check and
                        //   existsAcnChildWithNoUpdates error)
                        // The AcnInsertedFieldRef local variable is declared via
                        // funcBody's localVariables return.
                        let deferredFuncBody : CommonTypes.Codec -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CodegenScope -> string -> (AcnFuncBodyResult option) =
                            fun _codec _acnArgs _nestingScope _p _bsPos ->
                                let initCode = lm.acn.acn_deferred_det_init initFuncName ac.c_name codec
                                Some {
                                    AcnFuncBodyResult.funcBody = initCode
                                    errCodes = []
                                    localVariables = [GenericLocalVariable { name = ac.c_name; varType = "AcnInsertedFieldRef"; arrSize = None; isStatic = false; initExp = Some "{0}" }]
                                    bValIsUnReferenced = false
                                    bBsIsUnReferenced = false
                                    resultExpr = None
                                    auxiliaries = []
                                    icdResult = None
                                    userDefinedFunctions = []
                                }
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
        let missingDetNames =
            deferredDetNames
            |> Set.filter (fun name -> not (Set.contains name directAcnChildNames))
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

/// Deferred version of createReferenceFunction.
/// When the resolved type has acnParameters (from closure conversion),
/// generates a specialized function with AcnInsertedFieldRef* parameters
/// and returns an AcnFunction that calls it with &det actual arguments.
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

    match o.encodingOptions with
    | Some _ ->
        // CONTAINING pattern — Phase 7 TODO
        DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType us
    | None ->
        if o.resolvedType.acnParameters.Length = 0 then
            // No deferred params — use inline behavior
            DAstACN.createReferenceFunction_inline r deps lm codec t o typeDefinition isValidFunc baseType us
        else
            // --- Specialized function with AcnInsertedFieldRef* parameters ---
            let baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t.id "_ACN" codec

            // The specialized function is the base type's standalone ACN function.
            // Its name = baseFncName (e.g., "PayloadData_ACN_Encode").
            // The caller (parent) will call it with extra &det actual arguments.

            // Generate the caller's funcBody: a function call to baseFncName
            // with the standard params plus &det for each acnArgument.
            let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
                let pp, resultExpr =
                    let str = lm.lg.getParamValue t p.accessPath codec
                    match codec, lm.lg.decodingKind with
                    | Decode, Copy ->
                        let toc = ToC str
                        toc, Some toc
                    | _ -> str, None

                // Build extra actual parameters: &detName for each acnArgument
                let extraActualParams =
                    o.acnArguments |> List.map (fun arg ->
                        let (AcnGenericTypes.RelativePath parts) = arg
                        let detName = parts |> List.map (fun sl -> sl.Value) |> List.last |> ToC
                        lm.acn.acn_deferred_det_actual_param detName codec)

                // Generate the base function call and insert extra actual params
                let baseFuncCall = callBaseTypeFunc lm pp baseFncName codec
                let funcBodyContent =
                    if extraActualParams.IsEmpty then
                        baseFuncCall
                    else
                        // Insert extra params before the closing ");"
                        // e.g., "ret = Fn(pVal, pBitStrm, pErrCode, FALSE);"
                        // becomes "ret = Fn(pVal, pBitStrm, pErrCode, FALSE, &det1);"
                        let insertIdx = baseFuncCall.LastIndexOf(")")
                        if insertIdx > 0 then
                            baseFuncCall.[..insertIdx-1] + ", " + (extraActualParams |> String.concat ", ") + baseFuncCall.[insertIdx..]
                        else
                            baseFuncCall

                Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = None}), us

            // Record the function call dependency (caller → callee)
            let ns =
                match t.id.topLevelTas with
                | None -> us
                | Some tasInfo ->
                    let caller = {Caller.typeId = tasInfo; funcType=AcnEncDecFunctionType}
                    let callee = {Callee.typeId = {TypeAssignmentInfo.modName = o.modName.Value; tasName=o.tasName.Value} ; funcType=AcnEncDecFunctionType}
                    addFunctionCallToState us caller callee

            let soSparkAnnotations = Some(DAstACN.sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
            let a, ns2 = DAstACN.createAcnFunction r deps lm codec t typeDefinition isValidFunc funcBody (fun _atc -> true) soSparkAnnotations [] ns
            Some a, ns2


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
