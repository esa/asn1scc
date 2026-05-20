module AcnDependencies

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language

open AcnHelpers
open AcnDeterminantDef


let initExpr (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (t: Asn1AcnAst.AcnInsertedType): string =
    match t with
    | AcnInteger int -> lm.lg.asn1SccIntValueToString 0I int.isUnsigned
    | AcnNullType _ -> lm.lg.asn1SccIntValueToString 0I true
    | AcnBoolean _ -> lm.lg.FalseLiteral
    | AcnReferenceToIA5String s -> lm.lg.initializeString None (int s.str.maxSize.uper)
    | AcnReferenceToEnumerated e ->
        lm.lg.getNamedItemBackendName (Some (defOrRef r m e)) e.enumerated.items.Head

// Choose the right base scope for folding a dependency path.
// In --acn-v2, a reference type's funcBody may be inlined into a specialized
// function whose runtime pVal does not correspond to the captured type's TAS.
// To generate correct access paths, walk the enclosing scopes (current SEQUENCE
// + ancestors from nestingScope.parents) and pick the DEEPEST whose type id is
// a prefix of depPath.  Fold the remaining nodes from that scope.
//
// Intra-sequence deps resolve to the current SEQUENCE (shortest fold).
// Sibling/outer deps resolve to an ancestor whose scope matches the dep's
// outer TA, giving the correct base pVal for the fold.
let resolveDepScope (nestingScope: NestingScope) (pSrcRoot: CodegenScope) (depPath: ReferenceToType) : CodegenScope * ReferenceToType =
    let (ReferenceToType depNodes) = depPath
    let candidates = nestingScope.parents |> List.map (fun (p, t) -> (p, t.id))
    let matching =
        candidates |> List.tryFind (fun (_, ReferenceToType scopeNodes) ->
            let n = List.length scopeNodes
            n >= 2 && List.length depNodes >= n && List.take n depNodes = scopeNodes)
    match matching with
    | Some (scope, ReferenceToType scopeNodes) ->
        let n = List.length scopeNodes
        let remaining = ReferenceToType (List.take 2 scopeNodes @ List.skip n depNodes)
        scope, remaining
    | None ->
        pSrcRoot, depPath

/// Find which acnParameter is the CONTAINING size determinant by checking
/// the dependency list for AcnDepSizeDeterminant_bit_oct_str_contain.
/// Returns the parameter, or None if not found.
let findContainingSizeParam
        (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
        (o: Asn1AcnAst.ReferenceType) : AcnGenericTypes.AcnParameter option =
    o.resolvedType.acnParameters |> List.tryFind (fun prm ->
        deps.acnDependencies |> List.exists (fun d ->
            d.determinant.id = prm.id
            && (match d.dependencyKind with
                | Asn1AcnAst.AcnDepSizeDeterminant_bit_oct_str_contain _ -> true
                | _ -> false)))


// Context bundle threaded through every per-case handler in
// handleSingleUpdateDependency.  Five fields — well below the >12 threshold
// at which the split would be the wrong shape (per refactor plan §5).
type private DepContext = {
    r:    Asn1AcnAst.AstRoot
    deps: Asn1AcnAst.AcnInsertedFieldDependencies
    lm:   LanguageMacros
    m:    Asn1AcnAst.Asn1Module
    d:    AcnDependency
}

let rec handleSingleUpdateDependency (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (d:AcnDependency)  (us:State) =
    let ctx = { r=r; deps=deps; lm=lm; m=m; d=d }
    match d.dependencyKind with
    | AcnDepRefTypeArgument acnPrm                              -> handleRefTypeArgument          ctx acnPrm us
    | AcnDepSizeDeterminant (minSize, maxSize, _)               -> handleSizeDeterminant          ctx minSize maxSize us
    | AcnDepSizeDeterminant_bit_oct_str_contain o               -> handleSizeDeterminantContaining ctx o us
    | AcnDepIA5StringSizeDeterminant (minSize, maxSize, _)      -> handleIA5StringSizeDeterminant ctx minSize maxSize us
    | AcnDepPresenceBool                                        -> handlePresenceBool             ctx us
    | AcnDepPresence (relPath, chc)                             -> handlePresenceChoice           ctx relPath chc us
    | AcnDepPresenceStr (relPath, chc, str)                     -> handlePresenceStrChoice        ctx relPath chc str us
    | AcnDepChoiceDeterminant (enm, chc, isOptional)            -> handleChoiceDeterminant        ctx enm chc isOptional us

and private handleRefTypeArgument (ctx: DepContext) (acnPrm: AcnGenericTypes.AcnParameter) (us: State) =
    let prmUpdateStatement, ns1 = getUpdateFunctionUsedInEncoding ctx.r ctx.deps ctx.lm ctx.m acnPrm.id us
    match prmUpdateStatement with
    | None  -> None, ns1
    | Some prmUpdateStatement   ->
        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            prmUpdateStatement.updateAcnChildFnc child nestingScope vTarget pSrcRoot
        let icdComments = [sprintf "reference determinant for %s " (acnPrm.id.AsString)]
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=prmUpdateStatement.errCodes; testCaseFnc = prmUpdateStatement.testCaseFnc; localVariables=[]}), ns1

and private handleSizeDeterminant (ctx: DepContext) (minSize: SIZE) (maxSize: SIZE) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
        let pSizeable, checkPath = getAccessFromScopeNodeList relPath false lm pBase
        let unsigned =
            match child.Type with
            | AcnInteger int -> int.isUnsigned
            | AcnNullType _ -> true
            | _ -> raise (BugErrorException "???")
        let updateStatement =
            match minSize.acn = maxSize.acn with
            | true  -> lm.acn.SizeDependencyFixedSize v minSize.acn
            | false -> lm.acn.SizeDependency v (lm.acn.getSizeableSize (pSizeable.accessPath.joined lm.lg) (lm.lg.getAccess pSizeable.accessPath) unsigned) minSize.uper maxSize.uper false child.typeDefinitionBodyWithinSeq
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
        atc.testCaseTypeIDsMap.TryFind d.asn1Type
    let icdComments = [sprintf "size determinant for %s " (d.asn1Type.AsString)]
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us

and private handleSizeDeterminantContaining (ctx: DepContext) (o: Asn1AcnAst.ReferenceType) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let baseTypeDefinitionName =
        match lm.lg.hasModules with
        | false  -> ToC2(r.args.TypePrefix + o.tasName.Value)
        | true   ->
            match m.Name.Value = o.modName.Value with
            | true  -> ToC2(r.args.TypePrefix + o.tasName.Value)
            | false -> (ToC o.modName.Value) + "." + ToC2(r.args.TypePrefix + o.tasName.Value)
    let baseFncName = baseTypeDefinitionName + "_ACN" + Encode.suffix
    let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ACN_ENCODING" baseTypeDefinitionName
    let asn1TypeD = us.newTypesMap[d.asn1Type] :?> Asn1Type
    let asn1TypeD = match asn1TypeD.Kind with ReferenceType  o -> o.resolvedType.ActualType | _  -> asn1TypeD
    let errCodes0, localVariables0, ns =
        match asn1TypeD.acnEncFunction with
        | Some f  ->
            let fncBdRes, ns = f.funcBody us [] (NestingScope.init asn1TypeD.acnMaxSizeInBits asn1TypeD.uperMaxSizeInBits []) {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
            match fncBdRes with
            | Some x -> x.errCodes, [], ns
            | None   -> [], [], us
        | None    -> [], [], us

    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
        let pSizeable, checkPath = getAccessFromScopeNodeList relPath false lm pBase
        let sInner =
            match asn1TypeD.acnEncFunction with
            | Some f  ->
                let fncBdRes, _ = f.funcBody us [] nestingScope pSizeable
                match fncBdRes with
                | None -> ""
                | Some a -> a.funcBody
            | None -> ""
        let sLocalVarType = child.typeDefinitionBodyWithinSeq
        let updateStatement = lm.acn.SizeDependency_oct_str_containing (lm.lg.getParamValue o.resolvedType pSizeable.accessPath Encode) baseFncName sReqBytesForUperEncoding v (match o.encodingOptions with Some eo -> eo.octOrBitStr = ContainedInOctString | None -> false) sInner sLocalVarType
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
        atc.testCaseTypeIDsMap.TryFind d.asn1Type
    let localVars = lm.lg.acn.getAcnDepSizeDeterminantLocVars sReqBytesForUperEncoding
    let icdComments = [sprintf "size determinant for %s " (d.asn1Type.AsString)]
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=errCodes0; testCaseFnc=testCaseFnc; localVariables= localVariables0@localVars}), ns

and private handleIA5StringSizeDeterminant (ctx: DepContext) (minSize: SIZE) (maxSize: SIZE) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
        let pSizeable, checkPath = getAccessFromScopeNodeList relPath true lm pBase
        let updateStatement = lm.acn.SizeDependency v (lm.acn.getStringSize (pSizeable.accessPath.joined lm.lg)) minSize.uper maxSize.uper true child.typeDefinitionBodyWithinSeq
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
        atc.testCaseTypeIDsMap.TryFind d.asn1Type
    let icdComments = [sprintf "size determinant for %s " (d.asn1Type.AsString)]
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us

and private handlePresenceBool (ctx: DepContext) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let parDecTypeSeq =
            match d.asn1Type with
            | ReferenceToType (nodes) -> ReferenceToType (nodes |> List.rev |> List.tail |> List.rev)
        let pBase, relPath = resolveDepScope nestingScope pSrcRoot parDecTypeSeq
        let pDecParSeq, checkPath = getAccessFromScopeNodeList relPath false lm pBase
        let updateStatement = lm.acn.PresenceDependency v (pDecParSeq.accessPath.joined lm.lg) (lm.lg.getAccess pDecParSeq.accessPath) (ToC d.asn1Type.lastItem)
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
        match atc.testCaseTypeIDsMap.TryFind(d.asn1Type) with
        | Some _    -> Some TcvComponentPresent
        | None      -> Some TcvComponentAbsent
    let icdComments = [sprintf "Used as a presence determinant for %s " (d.asn1Type.AsString)]
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us

and private handlePresenceChoice (ctx: DepContext) (relPath: AcnGenericTypes.RelativePath) (chc: Asn1AcnAst.Choice) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let icdComments = [sprintf "Used as a presence determinant for %s " (chc.typeDef[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)]
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath1 = resolveDepScope nestingScope pSrcRoot d.asn1Type
        let choicePath, checkPath = getAccessFromScopeNodeList relPath1 false lm pBase
        let arrsChildUpdates =
            chc.children |>
            List.map(fun ch ->
                let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch m.Name.Value
                let unsigned =
                    match child.Type with
                    | AcnInteger int -> int.isUnsigned
                    | AcnNullType _ -> true
                    | _ -> raise (BugErrorException "???")
                match pres with
                | PresenceInt   (_, intVal) -> lm.acn.ChoiceDependencyIntPres_child v presentWhenName (lm.lg.asn1SccIntValueToString intVal.Value unsigned)
                | PresenceStr   (_, strVal) -> raise(SemanticError(strVal.Location, "Unexpected presence condition. Expected integer, found string")))
        let updateStatement = lm.acn.ChoiceDependencyPres v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
        let updateValues =
            chc.children |>
            List.filter(fun ch -> atc.testCaseTypeIDsMap.ContainsKey ch.Type.id) |>
            List.choose(fun ch ->
                let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                match pres with
                | PresenceInt   (_, intVal) -> Some (TcvChoiceAlternativePresentWhenInt intVal.Value)
                | PresenceStr   (_, strVal) -> None)
        match updateValues with
        | v1::[]    -> Some v1
        | _         -> None
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us

and private handlePresenceStrChoice (ctx: DepContext) (relPath: AcnGenericTypes.RelativePath) (chc: Asn1AcnAst.Choice) (str: Asn1AcnAst.StringType) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath1 = resolveDepScope nestingScope pSrcRoot d.asn1Type
        let choicePath, checkPath = getAccessFromScopeNodeList relPath1 false lm pBase
        let arrsChildUpdates =
            chc.children |>
            List.map(fun ch ->
                let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch m.Name.Value
                match pres with
                | PresenceInt   (_, intVal) ->
                    raise(SemanticError(intVal.Location, "Unexpected presence condition. Expected string, found integer"))
                | PresenceStr   (_, strVal) ->
                    let arrNulls = [0 .. ((int str.maxSize.acn)- strVal.Value.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
                    let bytesStr =
                        let baseBytes = System.Text.Encoding.ASCII.GetBytes strVal.Value
                        match lm.lg.nullTerminatorByte with
                        | Some nullByte -> Array.append baseBytes [| nullByte |]
                        | None -> baseBytes
                    let childTypeName =
                        match child.Type with
                        | AcnReferenceToIA5String t -> lm.lg.getLongTypedefName (lm.lg.definitionOrRef t.str.definitionOrRef)
                        | _ -> ""
                    lm.acn.ChoiceDependencyStrPres_child v presentWhenName strVal.Value bytesStr arrNulls childTypeName)
        let updateStatement = lm.acn.ChoiceDependencyPres v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates
        match checkPath with
        | []    -> updateStatement
        | _     -> lm.acn.checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
    let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
        let updateValues =
            chc.children |>
            List.filter(fun ch -> atc.testCaseTypeIDsMap.ContainsKey ch.Type.id) |>
            List.choose(fun ch ->
                let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                match pres with
                | PresenceInt   (_, intVal) -> None
                | PresenceStr   (_, strVal) -> Some (TcvChoiceAlternativePresentWhenStr strVal.Value))
        match updateValues with
        | v1::[]    -> Some v1
        | _         -> None
    let icdComments = []
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc = testCaseFnc; localVariables=[]}), us

and private handleChoiceDeterminant (ctx: DepContext) (enm: Asn1AcnAst.ReferenceToEnumerated) (chc: Asn1AcnAst.Choice) (isOptional: bool) (us: State) =
    let r, lm, m, d = ctx.r, ctx.lm, ctx.m, ctx.d
    let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
        let v = lm.lg.getValue vTarget.accessPath
        let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
        let choicePath, checkPath = getAccessFromScopeNodeList relPath false lm pBase
        let arrsChildUpdates =
            chc.children |>
            List.mapi(fun idx ch ->
                let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = ch.Name.Value)
                let choiceName = (lm.lg.getChoiceTypeDefinition chc.typeDef).typeName //chc.typeDef[Scala].typeName
                lm.acn.ChoiceDependencyEnum_Item v ch.presentWhenName choiceName (lm.lg.getNamedItemBackendName (Some (defOrRef2 r m enm)) enmItem) idx isOptional)
        let updateStatement = lm.acn.ChoiceDependencyEnum v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates isOptional (initExpr r lm m child.Type)
        // TODO: To remove this, getAccessFromScopeNodeList should be accounting for languages that rely on pattern matching for
        // accessing enums fields instead of a compiler-unchecked access
        let updateStatement2 =
            match ProgrammingLanguage.ActiveLanguages.Head with
            | Scala ->
                match checkPath.Length > 0 && checkPath[0].Contains("isInstanceOf") with
                | true -> (sprintf "val %s = %s.%s\n%s" (choicePath.accessPath.joined lm.lg) (checkPath[0].Replace("isInstanceOf", "asInstanceOf")) (choicePath.accessPath.joined lm.lg) updateStatement)
                | false -> updateStatement
            | _ -> updateStatement
        match checkPath with
        | []    -> updateStatement2
        | _     -> lm.acn.checkAccessPath checkPath updateStatement2 v (initExpr r lm m child.Type)
    let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
        atc.testCaseTypeIDsMap.TryFind d.asn1Type
    let icdComments = [sprintf "Used as a presence determinant for %s " (chc.typeDef[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)]
    Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us

and getUpdateFunctionUsedInEncoding (r: Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm: LanguageMacros) (m: Asn1AcnAst.Asn1Module) (acnChildOrAcnParameterId) (us:State) : (AcnChildUpdateResult option*State)=
    let multiAcnUpdate       = lm.acn.MultiAcnUpdate

    match deps.acnDependencies |> List.filter(fun d -> d.determinant.id = acnChildOrAcnParameterId) with
    | []  ->
        None, us
    | d1::[]    ->
        let ret, ns = handleSingleUpdateDependency r deps lm m d1 us
        ret, ns
    | d1::dds         ->
        let _errCodeName = ToC ("ERR_ACN" + (Encode.suffix.ToUpper()) + "_UPDATE_" + ((acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errFieldPath = match acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elm")
        let errCode, us = getNextValidErrorCode us _errCodeName None errFieldPath

        let ds = d1::dds
        let c_name0 = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) 0
        let localVars (child: AcnChild) =
            ds |>
            List.mapi(fun i d1 ->
                let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                let childLv =
                    if lm.lg.decodingKind = Copy then []
                    else [AcnInsertedChild (c_name, child.typeDefinitionBodyWithinSeq, child.initExpression)]
                let initLv =
                    if lm.lg.usesWrappedOptional then []
                    else [BooleanLocalVariable (c_name+"_is_initialized", Some lm.lg.FalseLiteral)]
                childLv@initLv) |>
            List.collect(fun lvList -> lvList |> List.map (fun lv -> lm.lg.getLocalVariableDeclaration  lv))
        let localUpdateFuns,ns =
            ds |>
            List.fold(fun (updates, ns) d1 ->
                let f1, nns = handleSingleUpdateDependency r deps lm m d1 ns
                updates@[f1], nns) ([],us)
        let restErrCodes = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.errCodes)
        let restLocalVariables = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.localVariables)
        let icdComments = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.icdComments)
        let multiUpdateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let arrsLocalUpdateStatements =
                localUpdateFuns |>
                List.mapi(fun i fn ->
                    let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let lv = {CodegenScope.modName = vTarget.modName; accessPath = AccessPath.valueEmptyPath c_name}
                    match fn with
                    | None      -> None
                    | Some fn   -> Some(fn.updateAcnChildFnc child nestingScope lv pSrcRoot)) |>
                List.choose id

            let isAlwaysInit (d: AcnDependency): bool =
                match d.dependencyKind with
                | AcnDepRefTypeArgument p ->
                    // Last item is the determinant, and the second-to-last is the field referencing the determinant
                    not p.id.dropLast.lastItemIsOptional
                | AcnDepChoiceDeterminant (_, c, isOpt) -> not isOpt
                | _ -> true

            let firstAlwaysInit = ds |> List.tryFind isAlwaysInit
            let arrsGetFirstIntValue =
                let ds2 =
                    match firstAlwaysInit with
                    | Some fst when lm.lg.usesWrappedOptional -> [fst]
                    | _ -> ds
                ds2 |>
                List.mapi (fun i d ->
                    let cmp = getDeterminantTypeUpdateMacro lm d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
                    let choicePath, _ = getAccessFromScopeNodeList relPath d.dependencyKind.isString lm pBase
                    cmp v vi (choicePath.accessPath.joined lm.lg) (i=0) (ds2.Length = 1))
            let arrsLocalCheckEquality =
                ds |>
                List.mapi (fun i d ->
                    let cmp = getDeterminantTypeCheckEqual lm d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
                    let choicePath, _ = getAccessFromScopeNodeList relPath d.dependencyKind.isString lm pBase
                    cmp v vi (choicePath.accessPath.joined lm.lg) (isAlwaysInit d))
            let updateStatement = multiAcnUpdate (vTarget.accessPath.joined lm.lg) c_name0 (errCode.errCodeName) (localVars child) arrsLocalUpdateStatements arrsGetFirstIntValue firstAlwaysInit.IsSome arrsLocalCheckEquality (initExpr r lm m child.Type)
            updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            let updateValues =
                localUpdateFuns |> List.map(fun z -> match z with None -> None | Some res -> res.testCaseFnc atc)
            match updateValues |> Seq.exists(fun z -> z.IsNone) with
            | true  -> None //at least one update is not present
            | false ->
                match updateValues |> List.choose id with
                | []        -> None
                | u1::us    ->
                    match us |> Seq.exists(fun z -> z <> u1) with
                    | true  -> None
                    | false -> Some u1

        let ret = Some(({AcnChildUpdateResult.updateAcnChildFnc = multiUpdateFunc; icdComments=icdComments; errCodes=errCode::restErrCodes ; testCaseFnc = testCaseFnc; localVariables = restLocalVariables}))
        ret, ns
