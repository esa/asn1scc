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
    let funcNameAndtasInfo   = lm.lg.getACNFuncName r codec t td
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
        ret st errCode prms nestingScope p

    let funcBody = AcnAlignment.handleAlignmentForAsn1Types r lm codec t.acnAlignment funcBody
    let funcBody = lm.lg.adaptAcnFuncBody r deps funcBody isValidFuncName t codec

    let sf = lm.lg.getTypeBasedSuffix FunctionType.AcnEncDecFunctionType t.Kind
    let p : CodegenScope = lm.lg.getParamTypeSuffix t sf codec
    let varName = p.accessPath.rootId
    let sStar = lm.lg.getStar p.accessPath
    let sInitialExp = ""
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
                        let content, ns1a = funcBody ns errCode [] {NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits [] with isStandaloneRender = true} p
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
                let content, ns1a = funcBody ns errCode [] (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) p
                let bodyResult_funcBody, errCodes,  bodyResult_localVariables, bBsIsUnreferenced, bVarNameIsUnreferenced, udfcs, auxiliaries, icdResult =
                    match content with
                    | None ->
                        let emptyStatement = lm.lg.emptyStatement
                        emptyStatement, [], [], true, isValidFuncName.IsNone, [], [], zeroBitIcdResult ()
                    | Some bodyResult ->
                        // Same zero-bit fallback as the inline branch above: a body
                        // without icdResult comes from the align-to-next/save-position
                        // wrappers around a zero-bit encoding.
                        let icdResult =
                            match bodyResult.icdResult with
                            | Some _ -> bodyResult.icdResult
                            | None   -> AcnAlignment.prependAlignmentPaddingRow t.acnAlignment (zeroBitIcdResult ())
                        bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsUnReferenced, bodyResult.bValIsUnReferenced, bodyResult.userDefinedFunctions, bodyResult.auxiliaries, icdResult

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
                                | false -> (ToC2 md.Value) + "." + ToC2(r.args.TypePrefix + ts.Value)
                        let resolvedKind =
                            r.Modules
                            |> Seq.tryFind (fun m -> m.Name.Value = md.Value)
                            |> Option.bind (fun m -> m.TypeAssignments |> Seq.tryFind (fun ta -> ta.Name.Value = ts.Value))
                            |> Option.map (fun ta -> ta.Type.ActualType.Kind)
                        let basicType, defaultVal = lm.lg.resolveAcnPrmRefTypeEmission prmTypeName resolvedKind intZero
                        emitPrm p.c_name basicType defaultVal

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
                let func = Some(EmitTypeAssignment_primitive varName sStar funcName isValidFuncName typeDefinitionName lvars bodyResult_funcBody soSparkAnnotations sInitialExp prms prmNames (t.acnMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced false soInitFuncName funcDefAnnots precondAnnots postcondAnnots codec)

                let errCodStr =
                    errCodes |>
                    List.groupBy (fun x -> x.errCodeName) |>
                    List.map (fun (k, v) -> {errCodeName = k; errCodeValue = v.Head.errCodeValue; comment = v.Head.comment; fieldPath = v.Head.fieldPath}) |>
                    List.map(fun x -> EmitTypeAssignment_def_err_code x.errCodeName (BigInteger x.errCodeValue) x.comment x.fieldPath) |> List.distinct
                let funcDef = Some(EmitTypeAssignment_primitive_def varName sStar funcName  typeDefinitionName errCodStr (t.acnMaxSizeInBits = 0I) nMaxBytesInACN t.acnMaxSizeInBits prms soSparkAnnotations codec)
                let ns2a =
                    match t.id.topLevelTas with
                    | None -> ns1a
                    | Some tasInfo ->
                        let caller = {Caller.typeId = tasInfo; funcType= UperEncDecFunctionType}
                        let callee = {Callee.typeId = tasInfo; funcType=IsValidFunctionType}
                        addFunctionCallToState ns1a caller callee
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
        }
    ret, ns3
