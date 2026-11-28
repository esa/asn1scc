module AcnFunctionWrapper

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

// This decoder checks the deduced count before entering its fixed-width item
// loop. Other decoders still rely on their maximum-capacity preconditions.
let rec private hasBoundedFixedDeducedDecoder (t: Asn1AcnAst.Asn1Type) =
    match t.acnAlignment with
    | Some _ -> false // Alignment consumes bits before the count check.
    | None ->
        match t.Kind with
        | Asn1AcnAst.SequenceOf sq ->
            sq.acnEncodingClass = SZ_EC_Deduced &&
            sq.child.acnMinSizeInBits > 0I &&
            sq.child.acnMinSizeInBits = sq.child.acnMaxSizeInBits
        | Asn1AcnAst.ReferenceType rf ->
            match rf.encodingOptions with
            | None -> hasBoundedFixedDeducedDecoder rf.resolvedType
            | Some _ -> false // A CONTAINING wrapper reads its own framing.
        | Asn1AcnAst.Integer _
        | Asn1AcnAst.Real _
        | Asn1AcnAst.IA5String _
        | Asn1AcnAst.NumericString _
        | Asn1AcnAst.OctetString _
        | Asn1AcnAst.TimeType _
        | Asn1AcnAst.BitString _
        | Asn1AcnAst.Boolean _
        | Asn1AcnAst.Enumerated _
        | Asn1AcnAst.NullType _
        | Asn1AcnAst.Sequence _
        | Asn1AcnAst.Choice _
        | Asn1AcnAst.ObjectIdentifier _ -> false


/// q/issues.md Issue 106: the decoder of a dependent whose determinant field is in a component the stream may say
/// absent (AbsentHolder.decodeGuard) tests the component's presence first and fails with the dependent's own error
/// code when it is absent (the field's local was never decoded; C wrote past an array with a size taken from it, Ada
/// raised Constraint_Error). The guard is built where the dependent is decoded: the holder is found among the
/// enclosing scopes (AcnHelpers.tryResolveDepScope), as the encoder finds the dependent's path for its update. With an
/// ALWAYS ABSENT holder the dependent can never be decoded: only the failure is emitted (its body, and the locals only
/// the body used, are dropped). Every dependent of a field decoded on all paths keeps its body unchanged (NoGuard).
/// q/issues.md Issue 108 (rule R4): the encoder of the dependent gets the same guard, on the value (AbsentHolder.
/// encodeGuard): a value whose holder is absent while the dependent is present fails before the dependent is written
/// (legacy encoded it from fields the holder's encoding never set, e.g. the length of a CONTAINING; --acn-v2 skipped
/// the patch of a never-reserved ref): the part may fail now (EncodeOutcome).
let applyAbsentHolderGuard (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (lm: LanguageMacros) (codec: Codec)
                           (dependentId: ReferenceToType) (guard: AbsentHolder.DecodeGuard) (errCodeName: string)
                           (prms: (RelativePath * AcnParameter) list) (nestingScope: NestingScope) (b: AcnFuncBodyResult) : AcnFuncBodyResult =
    // the field / holder paths after the type assignment, for the comment (`header.n`, `header`)
    let pathText (id: ReferenceToType) = (id.AcnAbsPath |> List.skip 2 |> String.concat ".").Replace("#", "elm")
    let describe (holders: ReferenceToType list) (fields: ReferenceToType list) =
        (fields |> List.map pathText |> String.concat ", "), (holders |> List.map pathText |> String.concat " / ")
    // the arm that fails: it writes ret / result (Issue 108, encode)
    let failArm = {canFail = true; writes = AlwaysWrites; callee = None}
    // Issue 108: the automatic tests reach the failing arm of an encoder guard (14d) only where the type assignment's
    // test values can show each holder absent with the dependent present (AbsentHolder.holderAbsentChecks); elsewhere
    // (a holder or dependent behind a reference site or in a SEQUENCE OF element, a field read through a ref) the arm
    // is checked by the wire tests and is not counted by the line gate
    let bTestsCannotReach (holders: ReferenceToType list) (refs: (AbsentHolder.RefTest * ReferenceToType list) list) =
        match refs, dependentId.ToScopeNodeList with
        | [], (MD md) :: (TA ta) :: _ ->
            let checks = AbsentHolder.holderAbsentChecks r deps (fun ft -> (lm.lg.getDeferredDetFunctions ft).IsSome) (ReferenceToType [MD md; TA ta])
            not (holders |> List.forall (fun h -> checks |> List.exists (fun (d, hs) -> d = dependentId && List.contains h hs)))
        | _ -> true
    match guard with
    | AbsentHolder.NoGuard -> b
    | AbsentHolder.HolderAlwaysAbsent (holders, fields) ->
        let sFields, sHolders = describe holders fields
        match codec with
        | Decode ->
            {b with funcBody = lm.acn.acn_dec_absent_holder_fail b.funcBody errCodeName sFields sHolders
                    localVariables = []; bBsIsUnReferenced = true}
        | Encode ->
            {b with funcBody = lm.acn.acn_enc_absent_holder_fail b.funcBody errCodeName sFields sHolders (bTestsCannotReach holders [])
                    localVariables = []; bBsIsUnReferenced = true; encOutcome = failArm}
    | AbsentHolder.TestHolders (holders, refs, fields) ->
        let presence (holderId: ReferenceToType) =
            match AcnHelpers.tryResolveDepScope r lm nestingScope holderId with
            | Some (scope, ReferenceToType relNodes) ->
                let pParent, _ = getAccessFromScopeNodeList (ReferenceToType (List.take (relNodes.Length - 1) relNodes)) false lm scope
                match List.last relNodes with
                | SEQ_CHILD (name, _) -> lm.lg.getSeqChildIsPresent pParent.accessPath (ToC name)
                | MD _ | TA _ | VA _ | PRM _ | CH_CHILD _ | SQF ->
                    raise (BugErrorException (sprintf "Issue 106 guard: %s is not a SEQUENCE component" holderId.AsString))
            | None ->
                // the field's SEQUENCE must enclose the dependent in this function (AbsentHolder.fieldAndReader)
                raise (BugErrorException (sprintf "Issue 106 guard: no enclosing scope holds %s (dependent %s)" holderId.AsString dependentId.AsString))
        // q/issues.md Issue 107 (--acn-v2): a field read through an AcnInsertedFieldRef is in the stream when the ref's
        // in_stream is set (the decoder of the field sets it, DAstACNDeferred; AbsentHolder.fieldsReadThroughRef; the
        // encoder's InitDet sets it where it reserves the field)
        let inStream (refTest: AbsentHolder.RefTest) =
            match refTest with
            // the formal parameter the dependent reads (AcnExternalField.getExternalField0)
            | AbsentHolder.FormalRef prm -> lm.acn.acn_deferred_det_in_stream_ptr (getAcnDeterminantName prm.id)
            // the ref the call passes for the parameter (as DAstACNDeferred.buildCallerWrapper): the enclosing
            // function's formal parameter standing for the same field, else the local of the field
            | AbsentHolder.ArgumentRef prm ->
                let identity = AcnExternalField.paramIdentity r deps prm
                match prms |> List.tryFind (fun (_, parentPrm) -> AcnExternalField.paramIdentity r deps parentPrm = identity) with
                | Some (_, parentPrm) -> lm.acn.acn_deferred_det_in_stream_ptr (getAcnDeterminantName parentPrm.id)
                | None                -> lm.acn.acn_deferred_det_in_stream_value (AcnExternalField.deferredDetRefName r lm identity)
        let allHolders = (holders @ (refs |> List.collect snd)) |> List.distinct
        let sFields, sHolders = describe allHolders fields
        match nestingScope.isStandaloneRender with
        // the ICD renders an inline type alone (its code is not emitted): no enclosing scope holds the holder
        | true  -> b
        | false ->
            let guards = (holders |> List.map presence) @ (refs |> List.map (fst >> inStream))
            match codec with
            | Decode -> {b with funcBody = lm.acn.acn_dec_absent_holder_guard guards b.funcBody errCodeName sFields sHolders}
            | Encode ->
                {b with funcBody = lm.acn.acn_enc_absent_holder_guard guards b.funcBody errCodeName sFields sHolders (bTestsCannotReach holders refs)
                        encOutcome = EncodeOutcome.alternatives true [b.encOutcome; failArm]}

let private withAbsentHolderGuard (r: Asn1AcnAst.AstRoot) (deps: AcnInsertedFieldDependencies) (lm: LanguageMacros)
                                  (codec: Codec) (t: Asn1AcnAst.Asn1Type) (funcBody: AcnAlignment.FuncBody) : AcnAlignment.FuncBody =
    let isDeferrable (ft: AcnInsertedType) = (lm.lg.getDeferredDetFunctions ft).IsSome
    let guard =
        match codec, lm.lg.decodeFailsWhenHolderAbsent, lm.lg.encodeFailsWhenHolderAbsent with
        | Decode, true, _       -> AbsentHolder.decodeGuard r deps isDeferrable t
        | Encode, _, true       -> AbsentHolder.encodeGuard r deps isDeferrable t
        | Decode, false, _
        | Encode, _, false      -> AbsentHolder.NoGuard
    match guard with
    | AbsentHolder.NoGuard -> funcBody
    | AbsentHolder.HolderAlwaysAbsent _
    | AbsentHolder.TestHolders _ ->
        fun st errCode prms nestingScope p ->
            let res, st1 = funcBody st errCode prms nestingScope p
            res |> Option.map (applyAbsentHolderGuard r deps lm codec t.id guard errCode.errCodeName prms nestingScope), st1

// If the type assignment has acnParameters, then no function is generated.
// This function can only be inlined by the calling function (i.e. by the parent
// type encoding function).
//
// However, there are cases where the reference type is not written explicitly
// by the user in the acn grammar, but is inferred by the compiler. For example,
// when an octet-string CONTAINING a CHOICE is given an inline ACN spec inside
// the parent SEQUENCE, the compiler creates a default ACN spec for the
// referenced type assignment. In that case the file-data reference type has
// no acnArgs, which means that no ACN function must be generated for the
// referenced type assignment.
let createAcnFunction (r: Asn1AcnAst.AstRoot)
                              (deps: AcnInsertedFieldDependencies)
                              (lm: LanguageMacros)
                              (codec: Codec)
                              (t: Asn1AcnAst.Asn1Type)
                              (typeDefinition: TypeDefinitionOrReference)
                              (isValidFunc: IsValidFunction option)
                              (funcBody: AcnAlignment.FuncBody)
                              isTestVaseValid
                              (soSparkAnnotations: string option)
                              (funcDefAnnots: string list)
                              (us: State) =
    let td = lm.lg.getTypeDefinition t.FT_TypeDefinition
    // Issue 106: the dependent's own body, under the presence test of the holder of its determinant field. The decoder
    // tests it inside the alignment (as before); the encoder outside it (Issue 108: nothing is written, not even the
    // padding, before the value is rejected)
    let withGuardOf (c: Codec) (fb: AcnAlignment.FuncBody) : AcnAlignment.FuncBody =
        match codec = c with
        | true  -> withAbsentHolderGuard r deps lm codec t fb
        | false -> fb
    let funcBody = withGuardOf Decode funcBody
    // --acn-v2: a type assignment whose ACN field determines a field of an
    // enclosing type has no standalone ACN function, like a type with ACN
    // parameters (AcnClosureConversion.findTassesWithExportedDeterminants).
    let funcNameAndtasInfo   =
        match t.id.tasInfo with
        | Some tasInfo when r.tassesWithExportedDeterminants.Contains tasInfo -> None
        | Some _
        | None -> lm.lg.getACNFuncName r codec t td
    let errCodeName         = ToC ("ERR_ACN" + (lm.lg.codecSuffix codec).ToUpper() + "_" + (t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm"))
    let errFieldPath = match t.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elm")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath
    let nMaxBytesInACN = BigInteger (ceil ((double t.acnMaxSizeInBits)/8.0))
    let nMinBytesInACN = BigInteger (ceil ((double t.acnMinSizeInBits)/8.0))
    let soInitFuncName = lm.lg.getFuncNameGeneric typeDefinition (lm.init.methodNameSuffix())
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let EmitTypeAssignment_primitive     =  lm.acn.EmitTypeAssignment_primitive
    let EmitTypeAssignment_primitive_def =  lm.acn.EmitTypeAssignment_primitive_def
    let EmitTypeAssignment_def_err_code  =  lm.acn.EmitTypeAssignment_def_err_code
    let EmitEncodingSizeConstants        =  lm.acn.EmitEncodingSizeConstants

    let typeDefinitionName = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
    // coverage step 10b: a decoder that cannot fail states result.Success in its contract and its wrapper does not
    // test it (DecodeOutcome.acn)
    let decodeOutcome =
        match codec with
        | Decode -> DecodeOutcome.acn r deps lm t
        | Encode -> MayFail
    let bAlwaysSucceeds =
        match decodeOutcome with
        | AlwaysSucceeds _ -> true
        | MayFail          -> false
    // coverage step 10c: val is `in out` when the decoder may leave it partly written; the wrapper initialises it
    // (a type assignment that is a reference has no init function of its own: the referenced type's)
    let soValInitFuncName =
        match soInitFuncName with
        | Some _ -> soInitFuncName
        | None   -> Some (typeDefinitionName + lm.init.methodNameSuffix())
    let bValInOut =
        match codec with
        // cycle 15g-b1 (round 2, F3): a decoder of alignments only defines val first (sInitialExp below)
        | Decode when lm.lg.decodeReadFailureModel = ReadsCannotFail && DecodeOutcome.acnAlignmentOnlyBody t && soInitFuncName.IsSome -> false
        | Decode -> DecodeOutcome.valInOut (DecodeOutcome.acnValWrite r deps lm t) soValInitFuncName
        | Encode -> false
    let soSparkAnnotations =
        match codec, decodeOutcome with
        | Decode, _ when hasBoundedFixedDeducedDecoder t ->
            Some (lm.acn.sparkAnnotations_deducedFixed typeDefinitionName)
        | Decode, AlwaysSucceeds _ when soSparkAnnotations.IsSome ->
            Some (lm.acn.sparkAnnotationsAlwaysSucceeds typeDefinitionName)
        | Decode, AlwaysSucceeds _
        | Decode, MayFail
        | Encode, _ -> soSparkAnnotations
    let sEncodingSizeConstant = EmitEncodingSizeConstants typeDefinitionName nMaxBytesInACN t.acnMaxSizeInBits

    let funcBodyAsSeqComp (st: State)
                          (prms: (RelativePath * AcnParameter) list)
                          (nestingScope: NestingScope)
                          (p: CodegenScope)
                          (c_name: string)
                          (lvName: string): (AcnFuncBodyResult option)*State =
        // t.SaveBitStreamPosition is false for all types except NULL types where
        // the 'save-position' attribute can be used.
        let funcBody = AcnAlignment.handleSavePosition funcBody t.SaveBitStreamPosition c_name lvName t.id lm codec
        let ret = AcnAlignment.handleAlignmentForAsn1Types r lm codec t.acnAlignment funcBody
        let ret = lm.lg.adaptAcnFuncBody r deps ret isValidFuncName t codec
        let ret = withGuardOf Encode ret
        ret st errCode prms nestingScope p

    let funcBody = AcnAlignment.handleAlignmentForAsn1Types r lm codec t.acnAlignment funcBody
    let funcBody = lm.lg.adaptAcnFuncBody r deps funcBody isValidFuncName t codec
    let funcBody = withGuardOf Encode funcBody

    let sf = lm.lg.getTypeBasedSuffix FunctionType.AcnEncDecFunctionType t.Kind
    let p : CodegenScope = lm.lg.getParamTypeSuffix t sf codec
    let varName = p.accessPath.rootId
    let sStar = lm.lg.getStar p.accessPath
    // Zero-bit encodings — a NULL without an ACN encoding pattern and an empty
    // SEQUENCE — have no funcBody on Encode (content = None below), so the
    // encoders never produce an icdResult and the type would silently vanish
    // from the ICD (roadmap A1 / root cause R1): the parent CHOICE/SEQUENCE
    // drops the row and printTasses3 never emits the standalone TAS table.
    // Synthesize a 0-bit IcdArgAux for exactly those kinds; the funcBody
    // results are used unchanged, so generated code is not affected.
    let zeroBitIcdResult () : IcdArgAux option =
        match t.Kind with
        | Asn1AcnAst.NullType _ ->
            let rowsFunc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments@["NULL type without an ACN encoding pattern: occupies no bits on the wire"]; sPresent = sPresent; sType = IcdPlainType "NULL"; sConstraint = None; minLengthInBits = 0I; maxLengthInBits = 0I; sUnits = t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            Some {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = getASN1Name t; rowsFunc = rowsFunc; commentsForTas = []; scope = "type"; name = None}
        | Asn1AcnAst.Sequence _ ->
            let rowsFunc _ _ _ = [], []
            Some {IcdArgAux.canBeEmbedded = false; baseAsn1Kind = getASN1Name t; rowsFunc = rowsFunc; commentsForTas = ["empty SEQUENCE (no fields): occupies no bits on the wire"]; scope = "type"; name = None}
        | Asn1AcnAst.Integer _
        | Asn1AcnAst.Real _
        | Asn1AcnAst.IA5String _
        | Asn1AcnAst.NumericString _
        | Asn1AcnAst.OctetString _
        | Asn1AcnAst.TimeType _
        | Asn1AcnAst.BitString _
        | Asn1AcnAst.Boolean _
        | Asn1AcnAst.Enumerated _
        | Asn1AcnAst.SequenceOf _
        | Asn1AcnAst.Choice _
        | Asn1AcnAst.ObjectIdentifier _
        | Asn1AcnAst.ReferenceType _ -> None
    let func, funcDef, userDefinedFunctions, auxiliaries, icdResult, ns2  =
            match funcNameAndtasInfo  with
            | None ->
                if not lm.lg.supportsAcnIcdForUndeclaredType then
                    // Languages (e.g. Scala) that cannot evaluate funcBody for
                    // inline/parameterized/undeclared types get no icdResult here.
                    // An ICD generated from such an AST would be incomplete -
                    // Program.fs rejects -icdAcn / -customIcdAcn / -icdRaw when
                    // Scala is the first target language (roadmap B6).
                    None, None, [], [], None, ns
                else
                    match r.args.generateAcnIcd with
                    | false ->
                        None, None, [], [], None, ns
                    | true ->
                        // The call to funcBody is necessary to get the correct nesting
                        // scope; however it is expensive, so we only call it when the
                        // ICD is requested.
                        // This renders an undeclared/inline type in isolation from its
                        // real usage, so mark the scope isStandaloneRender
                        let content, ns1aBody = funcBody ns errCode [] {NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits [] with isStandaloneRender = true} p
                        // coverage step 12e: the checked selectors of a function body end with it
                        let ns1a = {ns1aBody with acnSequenceCheckedSelectors = []}
                        let icdResult, udfcs =
                            match content with
                            | None -> zeroBitIcdResult (), []
                            | Some bodyResult ->
                                match bodyResult.icdResult with
                                | Some _ -> bodyResult.icdResult, bodyResult.userDefinedFunctions
                                | None   ->
                                    // A body without icdResult is a zero-bit encoding whose
                                    // funcBody was synthesized by a wrapper (align-to-next
                                    // and/or save-position around a pattern-less NULL /
                                    // empty SEQUENCE).  Synthesize the 0-bit row here and,
                                    // when the type is aligned, prepend the padding row the
                                    // alignment emits (roadmap B3).
                                    AcnAlignment.prependAlignmentPaddingRow t.acnAlignment (zeroBitIcdResult ()), bodyResult.userDefinedFunctions
                        None, None, udfcs, [], icdResult, ns1a
            | Some funcName ->
                let precondAnnots = lm.lg.generatePrecond r ACN t codec
                let postcondAnnots = lm.lg.generatePostcond r ACN p t codec
                let content, ns1aBody = funcBody ns errCode [] (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) p
                // coverage step 12e: the checked selectors of a function body end with it
                let ns1a = {ns1aBody with acnSequenceCheckedSelectors = []}
                // coverage step 12b: what the encoder's body does (an empty body writes nothing)
                let bodyOutcome =
                    match content with
                    | Some c -> encodeOutcomeOf codec c.encOutcome
                    | None   -> encodeOutcomeOf codec EncodeOutcome.noWrite
                // coverage step 12a: an encoder does not call a validity function that cannot fail; cycle 15j: nor
                // does the decoder end with one (C)
                let bValidityCannotFail = DAstUtilFunctions.codecSkipsValidation lm codec ns.emittedValidatorOutcomes isValidFunc t bodyOutcome
                let bValidityCalled = isValidFuncName.IsSome && not bValidityCannotFail
                // coverage step 12b: an encoder that cannot fail states result.Success in its contract (Ada)
                let soSparkAnnotations =
                    match codec, bodyOutcome.canFail || bValidityCalled, soSparkAnnotations with
                    | Encode, false, Some _ -> Some (lm.acn.sparkAnnotationsAlwaysSucceeds typeDefinitionName)
                    | Encode, _, _
                    | Decode, _, _          -> soSparkAnnotations
                // coverage step 12b: without a validity call the Ada encoder initialises result before the body,
                // unless the body writes it on every path before reading it (C: the macro is empty)
                let encodeResultInit (bValidity:bool) (body:string) =
                    match codec, bValidity, bodyOutcome.writes, lm.uper.decode_result_init () with
                    | Encode, false, (NoWrite | SometimesWrites | ReadsFirst | WritesUnknown), sInit when sInit <> "" -> sInit + "\n" + body
                    | Encode, _, _, _
                    | Decode, _, _, _   -> body
                let withEncodeResultInit = encodeResultInit bValidityCalled
                // coverage step 14q (Q3 b): an encoder that validates its value is split (Ada, as in DAstUPer); not for a
                // type with ACN parameters (only parents call its _aux, which would then be called by nobody), unknown
                // writes or an empty body
                let bSplit =
                    match codec, lm.lg.encoderBodySplit && bValidityCalled && t.acnParameters.IsEmpty, content, bodyOutcome.writes with
                    | Encode, true, Some _, (NoWrite | AlwaysWrites | SometimesWrites | ReadsFirst) -> true
                    | Encode, _, _, _
                    | Decode, _, _, _ -> false
                let bodyFuncName = funcName + "_Body"
                // cycle b4a (kappa-P): a split encoder whose body cannot fail succeeds on every valid value (it validates
                // and calls the body, whose Post states result.Success); stated in its contract for the callers of other
                // packages
                let soSparkAnnotations =
                    match lm.lg.validatorPostconditions && bSplit && not bodyOutcome.canFail, isValidFuncName, soSparkAnnotations with
                    | true, Some vf, Some _ -> Some (lm.acn.sparkAnnotationsEncValidSucceeds typeDefinitionName vf varName)
                    | _, _, _               -> soSparkAnnotations
                // coverage step 12b: what a call of this encoder does, for the parents' joins
                let ns1a =
                    match codec with
                    | Encode ->
                        let callCanFail = bodyOutcome.canFail || (lm.lg.encodeCallValidates && bValidityCalled)
                        let ns1a = {ns1a with encodeCallOutcomes = ns1a.encodeCallOutcomes.Add (encodeCallKey lm t.id.ModName funcName, EncodeOutcome.call funcName callCanFail)}
                        // coverage step 14q (Q3 b): the body of a split encoder, for the callers of the same package
                        match bSplit, isValidFuncName with
                        | true, Some vf ->
                            let b = {bodyFuncName = bodyFuncName; outcome = EncodeOutcome.call bodyFuncName bodyOutcome.canFail; validatorKey = encodeCallKey lm t.id.ModName vf; modName = ToC t.id.ModName}
                            {ns1a with encodeBodies = ns1a.encodeBodies.Add (encodeCallKey lm t.id.ModName funcName, b)}
                        | true, None
                        | false, _ -> ns1a
                    | Decode -> ns1a
                // cycle 15g-b1 (Issue 112; round 2: also a SEQUENCE of aligned NULLs): a type whose decoding is only
                // alignments writes neither val nor result: the decoder defines both first, as for an empty body
                // (sInitialExp), instead of the result initialisation alone; val stays `out` (bValInOutOfDecoder)
                let bAlignedNullDecode =
                    match codec, lm.lg.decodeReadFailureModel, content with
                    | Decode, ReadsCannotFail, Some _ -> DecodeOutcome.acnAlignmentOnlyBody t && soInitFuncName.IsSome
                    | _ -> false
                let sInitialExp =
                    // A null attribute disables the optional template initializer.
                    match content with
                    | Some _ when bAlignedNullDecode -> soInitFuncName.Value
                    | Some _ -> null
                    | None ->
                        match codec, lm.lg.initMethod with
                        | Decode, InitMethod.Function -> soInitFuncName.orElse null
                        | Encode, InitMethod.Function
                        | Encode, InitMethod.Procedure
                        | Decode, InitMethod.Procedure -> null
                let bodyResult_funcBody, errCodes,  bodyResult_localVariables, bBsIsUnreferenced, bVarNameIsUnreferenced, udfcs, auxiliaries, icdResult =
                    match content with
                    | None ->
                        let emptyStatement = withEncodeResultInit lm.lg.emptyStatement
                        emptyStatement, [], [], true, isValidFuncName.IsNone || bValidityCannotFail, [], [], zeroBitIcdResult ()
                    | Some bodyResult ->
                        // Same zero-bit fallback as the inline branch above: a body
                        // without icdResult comes from the align-to-next/save-position
                        // wrappers around a zero-bit encoding.
                        let icdResult =
                            match bodyResult.icdResult with
                            | Some _ -> bodyResult.icdResult
                            | None   -> AcnAlignment.prependAlignmentPaddingRow t.acnAlignment (zeroBitIcdResult ())
                        // coverage step 10b: result is defined first when an exit or the end could read an unset
                        // error code (DecodeOutcome.acnNeedsResultInit)
                        let body =
                            match codec, lm.lg.decodeReadFailureModel with
                            | Decode, ReadsCannotFail when bAlignedNullDecode -> bodyResult.funcBody  // sInitialExp defines result
                            | Decode, ReadsCannotFail when DecodeOutcome.acnNeedsResultInit r deps lm t ->
                                lm.uper.decode_result_init () + "\n" + bodyResult.funcBody
                            | Decode, ReadsCannotFail
                            | Decode, ReadsCanFail      -> bodyResult.funcBody
                            | Encode, _                 -> withEncodeResultInit bodyResult.funcBody
                        // an encoder's val is referenced by its validity call when it makes one (coverage step 12a)
                        let bValIsUnReferenced = bodyResult.bValIsUnReferenced && not (codec = Encode && isValidFuncName.IsSome && not bValidityCannotFail) && not bAlignedNullDecode
                        // cycle 15j: a decoder without its final validity call (C) references val only in its body
                        let bValIsUnReferenced =
                            match codec, bValidityCannotFail && isValidFuncName.IsSome with
                            | Decode, true  ->
                                bValIsUnReferenced ||
                                    not (DAstUtilFunctions.decoderBodyNamesValue varName (body :: (bodyResult.localVariables |> List.map lm.lg.getLocalVariableDeclaration)))
                            | Decode, false
                            | Encode, _     -> bValIsUnReferenced
                        body, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsUnReferenced, bValIsUnReferenced, bodyResult.userDefinedFunctions, bodyResult.auxiliaries, icdResult

                let handleAcnParameter (p:AcnParameter) =
                    let intType  = lm.typeDef.Declare_Integer ()
                    let boolType = lm.typeDef.Declare_Boolean ()
                    let intZero  = lm.lg.asn1SccIntValueToString 0I false
                    let emitPrm  = lm.acn.EmitAcnParameter
                    match p.asn1Type with
                    | AcnPrmInteger    _          -> emitPrm p.c_name intType intZero
                    | AcnPrmBoolean    _          -> emitPrm p.c_name boolType lm.lg.FalseLiteral
                    | AcnPrmNullType   loc        -> raise(SemanticError (loc, "Invalid type for parameter"))
                    | AcnPrmRefType(md,ts)        ->
                        let prmTypeName =
                            match lm.lg.hasModules with
                            | false         -> ToC2(r.args.TypePrefix + ts.Value)
                            | true       ->
                                match md.Value = t.id.ModName with
                                | true  -> ToC2(r.args.TypePrefix + ts.Value)
                                | false -> lm.lg.constructCrossModuleFuncPrefix (ToC2 md.Value) + ToC2(r.args.TypePrefix + ts.Value)
                        let resolvedKind =
                            r.Modules
                            |> Seq.tryFind (fun m -> m.Name.Value = md.Value)
                            |> Option.bind (fun m -> m.TypeAssignments |> Seq.tryFind (fun ta -> ta.Name.Value = ts.Value))
                            |> Option.map (fun ta -> ta.Type.ActualType.Kind)
                        let basicType, defaultVal = lm.lg.resolveAcnPrmRefTypeEmission prmTypeName resolvedKind intZero
                        emitPrm p.c_name basicType defaultVal

                // cycle 15j round 4: a C decoder whose body cannot fail calls its validator directly (bAlwaysSucceeds of
                // the C decode templates; for C DecodeOutcome is always MayFail, so this is the only way it becomes true)
                let bAlwaysSucceedsT =
                    match codec, isValidFuncName.IsSome && not bValidityCannotFail with
                    | Decode, true  -> bAlwaysSucceeds || DAstUtilFunctions.decoderBodyCannotFail lm bodyResult_funcBody
                    | Decode, false
                    | Encode, _     -> bAlwaysSucceeds
                let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct
                let prms = t.acnParameters |> List.map handleAcnParameter
                let prmNames = t.acnParameters |> List.map (fun p -> p.c_name)
                // For languages that need it (Python): for CEC_enum CHOICE decode, inject the
                // enum determinant as an extra parameter so the standalone classmethod can access
                // the choice discriminant. Deduplicate against existing prmNames.
                let prms, prmNames =
                    if lm.lg.needsAcnChoiceDeterminantParam && codec = Decode then
                        let existing = System.Collections.Generic.HashSet<string>(prmNames)
                        let extra =
                            deps.acnDependencies
                            |> List.filter (fun d ->
                                d.asn1Type = t.id &&
                                match d.dependencyKind with
                                | AcnDepChoiceDeterminant _ -> true
                                | _ -> false)
                            |> List.choose (fun d ->
                                let detName = getAcnDeterminantName d.determinant.id
                                if existing.Add(detName) then
                                    Some (lm.acn.EmitAcnParameter detName "int" "0", detName)
                                else None)
                        let extraPrms, extraNames = extra |> List.map fst, extra |> List.map snd
                        prms @ extraPrms, prmNames @ extraNames
                    else
                        prms, prmNames
                let func =
                    match bSplit, content with
                    | true, Some bodyResult ->
                        // the body states result.Success in its contract when it cannot fail; the _aux keeps its own.
                        // Cycle b4a (kappa-P): the body requires a valid value (its callers validated it: the _aux, the
                        // bodies of the same package after their own validation, DAstUtilFunctions.encodeCallTarget)
                        let soBodyAnnotations =
                            match lm.lg.validatorPostconditions, isValidFuncName, bodyOutcome.canFail, soSparkAnnotations with
                            | true, Some vf, bCanFail, Some _ -> Some (lm.acn.sparkAnnotationsEncBody typeDefinitionName vf varName (not bCanFail))
                            | _, _, false, Some _             -> Some (lm.acn.sparkAnnotationsAlwaysSucceeds typeDefinitionName)
                            | _, _, _, _                      -> soSparkAnnotations
                        let body = lm.uper.EmitEncodeBodyProcedure varName bodyFuncName typeDefinitionName lvars (encodeResultInit false bodyResult.funcBody) soBodyAnnotations bodyResult.bBsIsUnReferenced bodyResult.bValIsUnReferenced (bodyResult.funcBody.Contains "nCurOffset")
                        let callBody = lm.uper.call_base_type_func varName bodyFuncName codec
                        let aux = EmitTypeAssignment_primitive varName sStar funcName isValidFuncName typeDefinitionName Seq.empty callBody soSparkAnnotations sInitialExp prms prmNames (t.acnMaxSizeInBits = 0I) false false false soValInitFuncName funcDefAnnots precondAnnots postcondAnnots false bAlwaysSucceeds false bValidityCannotFail codec
                        Some (body + "\n\n" + aux)
                    | _, _ ->
                        Some(EmitTypeAssignment_primitive varName sStar funcName isValidFuncName typeDefinitionName lvars bodyResult_funcBody soSparkAnnotations sInitialExp prms prmNames (t.acnMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced false soValInitFuncName funcDefAnnots precondAnnots postcondAnnots false bAlwaysSucceedsT (bValInOut && not bVarNameIsUnreferenced) bValidityCannotFail codec)

                let errCodStr =
                    errCodes |>
                    List.groupBy (fun x -> x.errCodeName) |>
                    List.map (fun (k, v) -> {errCodeName = k; errCodeValue = v.Head.errCodeValue; comment = v.Head.comment; fieldPath = v.Head.fieldPath}) |>
                    List.map(fun x -> EmitTypeAssignment_def_err_code x.errCodeName (BigInteger x.errCodeValue) x.comment x.fieldPath) |> List.distinct
                // coverage step 14c2a: a full-index CHOICE writes all of its `in out` val (DecodeOutcome.acnValInitUnused)
                let bValInitUnused = bValInOut && not bVarNameIsUnreferenced && codec = Decode && DecodeOutcome.acnValInitUnused r deps lm t
                let funcDef = Some(EmitTypeAssignment_primitive_def varName sStar funcName  typeDefinitionName errCodStr (t.acnMaxSizeInBits = 0I) nMaxBytesInACN t.acnMaxSizeInBits prms soSparkAnnotations (bValInOut && not bVarNameIsUnreferenced) false bValInitUnused codec)
                let ns2a =
                    match t.id.topLevelTas with
                    | None -> ns1a
                    | Some tasInfo ->
                        let caller = {Caller.typeId = tasInfo; funcType= UperEncDecFunctionType}
                        let callee = {Callee.typeId = tasInfo; funcType=IsValidFunctionType}
                        let ns2 = addFunctionCallToState ns1a caller callee
                        match codec, lm.lg.initMethod with
                        | Decode, InitMethod.Function when content.IsNone ->
                            // The empty Ada ACN decoder calls its initializer, also
                            // when -icdPdus retains it through a reference codec.
                            addFunctionCallToState ns2
                                {caller with funcType=AcnEncDecFunctionType}
                                {callee with funcType=InitFunctionType}
                        | Encode, InitMethod.Function
                        | Encode, InitMethod.Procedure
                        | Decode, InitMethod.Function
                        | Decode, InitMethod.Procedure -> ns2
                func, funcDef, udfcs, auxiliaries, icdResult, ns2a

    let icdAux, ns3 =
        match icdResult with
        | Some icdAux ->
            let foo () =
                // Types that resolve a reference (t.inheritInfo is set on the
                // resolved instance, never on a TAS's own type) keep the
                // referenced TAS's name in their field rows, e.g.
                // "OCTET STRING (Checksum)" (roadmap A4).
                let icdAux = AcnHelpers.icdAuxAddNamedTypeSuffix (t.inheritInfo |> Option.map (fun ii -> ii.tasName)) icdAux
                let hasAcnDefinition = t.typeAssignmentInfo.IsSome && t.acnLocation.IsSome
                // In --acn-v2, closure conversion turns the CONTAINING external-field
                // size into an explicit acnParameter on the contained type's usage
                // instance (e.g. PayloadData at PDU.payload gains a `pdu-length`
                // parameter).  Legacy mode carries no such parameter, so it would leak
                // into the table-identity hash and stop the byte-identical contained
                // instance from dedup-merging with the standalone TAS table, dropping
                // a usage site (roadmap B8).  Drop CONTAINING-size determinants from
                // the ICD parameter list so v2 matches legacy; codegen is unaffected
                // (this touches ICD content only).  No-op in legacy mode.
                let icdAcnParameters =
                    match r.args.acnDeferred with
                    | false -> t.acnParameters
                    | true  ->
                        t.acnParameters |> List.filter (fun prm ->
                            not (deps.acnDependencies |> List.exists (fun d ->
                                d.determinant.id = prm.id &&
                                (match d.dependencyKind with AcnDepSizeDeterminant_bit_oct_str_contain _ -> true | _ -> false))))
                let icdTas = AcnIcd.createIcdTas r t.id icdAux td typeDefinition nMinBytesInACN nMaxBytesInACN hasAcnDefinition icdAcnParameters
                let ns3 =
                    match ns2.icdHashes.TryFind icdTas.hash with
                    | None -> {ns2 with icdHashes = ns2.icdHashes.Add(icdTas.hash, [icdTas])}
                    | Some exList -> {ns2 with icdHashes = ns2.icdHashes.Add(icdTas.hash, icdTas::exList)}
                Some icdTas, ns3
            TL "createIcdTas" foo
        | None -> None, ns2
    let ret =
        {
            AcnFunction.funcName       = funcNameAndtasInfo
            func                       = func
            funcDef                    = funcDef
            auxiliaries                = auxiliaries
            funcBody                   = fun us acnArgs p -> funcBody us errCode acnArgs p
            funcBodyAsSeqComp          = funcBodyAsSeqComp
            isTestVaseValid            = isTestVaseValid
            icdTas                        = icdAux
            userDefinedFunctions       = userDefinedFunctions
            encodingSizeConstant       = sEncodingSizeConstant
            errCode                    = errCode
        }
    ret, ns3
