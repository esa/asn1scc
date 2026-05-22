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
    let errCodeName         = ToC ("ERR_ACN" + codec.suffix.ToUpper() + "_" + (t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm"))
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
    let func, funcDef, userDefinedFunctions, auxiliaries, icdResult, ns2  =
            match funcNameAndtasInfo  with
            | None ->
                if lm.lg.supportsAcnIcdForUndeclaredType && r.args.generateAcnIcd then
                    // The call to funcBody is necessary to get the correct nesting
                    // scope; however it is expensive, so we only call it when the
                    // ICD is requested.
                    let content, ns1a = funcBody ns errCode [] (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) p
                    let icdResult, udfcs =
                        match content with
                        | None -> None, []
                        | Some bodyResult -> bodyResult.icdResult, bodyResult.userDefinedFunctions
                    None, None, udfcs, [], icdResult, ns1a
                else
                    None, None, [], [], None, ns
            | Some funcName ->
                let precondAnnots = lm.lg.generatePrecond r ACN t codec
                let postcondAnnots = lm.lg.generatePostcond r ACN p t codec
                let content, ns1a = funcBody ns errCode [] (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) p
                let bodyResult_funcBody, errCodes,  bodyResult_localVariables, bBsIsUnreferenced, bVarNameIsUnreferenced, udfcs, auxiliaries, icdResult =
                    match content with
                    | None ->
                        let emptyStatement = lm.lg.emptyStatement
                        emptyStatement, [], [], true, isValidFuncName.IsNone, [], [], None
                    | Some bodyResult ->
                        bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsUnReferenced, bodyResult.bValIsUnReferenced, bodyResult.userDefinedFunctions, bodyResult.auxiliaries, bodyResult.icdResult

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
                        match lm.lg.getAcnPrmRefTypeInfo r md ts intZero with
                        | Some (basicType, defaultVal) -> emitPrm p.c_name basicType defaultVal
                        | None -> emitPrm p.c_name prmTypeName ""

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
                let hasAcnDefinition = t.typeAssignmentInfo.IsSome && t.acnLocation.IsSome
                let icdTas = AcnIcd.createIcdTas r t.id icdAux td typeDefinition nMinBytesInACN nMaxBytesInACN hasAcnDefinition
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
