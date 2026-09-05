module AcnChoice

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
open AcnExternalField


let createChoiceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionOrReference) (defOrRef:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (acnPrms:DastAcnParameter list)  (us:State)  =
    let choice_uper                         =  lm.acn.Choice
    let choiceChildAlwaysAbsent             =  lm.acn.ChoiceChildAlwaysAbsent
    let choiceChild                         =  lm.acn.ChoiceChild
    let choice_Enum                         =  lm.acn.Choice_Enum
    let choiceChild_Enum                    =  lm.acn.ChoiceChild_Enum
    let choice_preWhen                      =  lm.acn.Choice_preWhen
    let choiceChild_preWhen                 =  lm.acn.ChoiceChild_preWhen
    let choiceChild_preWhen_int_condition   =  lm.acn.ChoiceChild_preWhen_int_condition
    let choiceChild_preWhen_str_condition   =  lm.acn.ChoiceChild_preWhen_str_condition

    let isAcnChild (ch:ChChildInfo) = match ch.Optionality with  Some (ChoiceAlwaysAbsent) -> false | _ -> true
    let acnChildren = children |> List.filter isAcnChild
    let alwaysAbsentChildren = children |> List.filter (isAcnChild >> not)
    let children =
        match lm.lg.acn.choice_handle_always_absent_child with
        | false     -> acnChildren
        | true   -> acnChildren@alwaysAbsentChildren     //in Spark, we have to cover all cases even the ones that are always absent due to SPARK strictness


    let nMax = BigInteger(Seq.length acnChildren) - 1I
    //let nBits = (GetNumberOfBitsForNonNegativeInteger (nMax-nMin))
    let nIndexSizeInBits = (GetNumberOfBitsForNonNegativeInteger (BigInteger (acnChildren.Length - 1)))
    let sChoiceIndexName = (ToC t.id.AsString) + "_index_tmp"
    let ec =
        match o.acnProperties.enumDeterminant with
        | Some _            ->
            let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = t.id)
            match dependency.dependencyKind with
            | Asn1AcnAst.AcnDepChoiceDeterminant (enm, _, _)  -> CEC_enum (enm, dependency.determinant)
            | _                                         -> raise(BugErrorException("unexpected dependency type"))
        | None              ->
            match children |> Seq.exists(fun c -> not (Seq.isEmpty c.acnPresentWhenConditions)) with
            | true           -> CEC_presWhen
            | false          -> CEC_uper

    let localVariables =
        match ec, codec with
        | _, CommonTypes.Encode             -> []
        | CEC_enum _, CommonTypes.Decode    -> []
        | CEC_presWhen, CommonTypes.Decode  -> []
        | CEC_uper, CommonTypes.Decode  -> [(Asn1SIntLocalVariable (sChoiceIndexName, None))]

    let typeDefinitionName = defOrRef.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName//getTypeDefinitionName t.id.tasInfo typeDefinition
    let uperPresenceMask, extraComment =
        match acnChildren.Length with
        | 1 -> [], []
        | _ ->
            match ec with
            | CEC_uper          ->
                let indexSize = GetChoiceUperDeterminantLengthInBits acnChildren.Length.AsBigInt
                [{IcdRow.fieldName = "ChoiceIndex"; comments = [$"Special field used by ACN to indicate which choice alternative is present."]; sPresent="always" ;sType=IcdPlainType "unsigned int"; sConstraint=None; minLengthInBits = indexSize; maxLengthInBits=indexSize;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}], []
            | CEC_enum (enm,d)  -> [],[]
            | CEC_presWhen      ->
                let extFields = acnChildren |> List.collect(fun c -> c.acnPresentWhenConditions) |> List.map(fun x -> "'" + x.relativePath.AsString + "'") |> Seq.distinct |> Seq.StrJoin ","
                let plural = if extFields.Contains "," then "s" else ""
                [],[$"Active alternative is determined by ACN using the field{plural}: %s{extFields}"]

    let icdFnc fieldName sPresent comments  =
        let chRows0, compositeChildren0 =
            acnChildren |>
            List.mapi(fun idx c ->
                    let childComments = c.Comments |> Seq.toList
                    let optionality =
                        match c.Optionality with
                        | Some(ChoiceAlwaysAbsent ) -> "never"
                        | Some(ChoiceAlwaysPresent)
                        | None ->
                            match ec with
                            | CEC_uper          ->
                                match acnChildren.Length <= 1 with
                                | true  -> "always"
                                | false -> sprintf "ChoiceIndex = %d" idx
                            | CEC_enum (enm,d)  ->
                                let refToStr id =
                                    match id with
                                    | ReferenceToType sn -> sn |> List.rev |> List.head |> (fun x -> x.AsString)
                                sprintf "%s = %s" (refToStr d.id) c.Name.Value
                            | CEC_presWhen      ->
                                let getPresenceSingle (pc:AcnGenericTypes.AcnPresentWhenConditionChoiceChild) =
                                    match pc with
                                    | AcnGenericTypes.PresenceInt   (rp, intLoc) -> sprintf "%s=%A" rp.AsString intLoc.Value
                                    | AcnGenericTypes.PresenceStr   (rp, strLoc) -> sprintf "%s=%A" rp.AsString strLoc.Value
                                c.acnPresentWhenConditions |> Seq.map getPresenceSingle |> Seq.StrJoin " AND "
                    match c.chType.icdTas with
                    | Some childIcdTas ->
                        match childIcdTas.canBeEmbedded with
                        | true -> childIcdTas.createRowsFunc c.Name.Value optionality childComments
                        | false ->
                            let sType = TypeHash childIcdTas.hash
                            let sConstraint = constraintsToIcdStr c.chType.ConstraintsAsn1Str
                            let icdRow = [{IcdRow.fieldName = c.Name.Value; comments = childComments; sPresent=optionality;sType=sType; sConstraint=sConstraint; minLengthInBits = c.chType.acnMinSizeInBits; maxLengthInBits=c.chType.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]
                            let compChild = [childIcdTas]
                            icdRow, compChild
                    |None -> [],[]) |> List.unzip
        let chRows = chRows0 |> List.collect id
        let compositeChildren = compositeChildren0 |> List.collect id
        let icdRows = renumberIcdRows (uperPresenceMask@chRows)
        icdRows, compositeChildren

    let icd = {IcdArgAux.canBeEmbedded = false; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=extraComment; scope="type"; name= None}


    let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let td = (lm.lg.getChoiceTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let acnSiblingMaxSize = children |> List.map (fun c -> c.chType.acnMaxSizeInBits) |> List.max
        let uperSiblingMaxSize = children |> List.map (fun c -> c.chType.uperMaxSizeInBits) |> List.max
        let handleChild (us:State) (idx:int) (child:ChChildInfo) =
            let chFunc = child.chType.getAcnFunction codec
            let sChildInitExpr = child.chType.initFunction.initExpressionFnc ()
            let sChildName = lm.lg.getAsn1ChChildBackendName child
            let sChildTypeDef = child.chType.typeDefinitionOrReference.longTypedefName2 (Some lm.lg) lm.lg.hasModules p.modName
            let childNestingScope =
                {nestingScope with
                    nestingLevel = nestingScope.nestingLevel + 1I
                    uperSiblingMaxSize = Some uperSiblingMaxSize
                    acnSiblingMaxSize = Some acnSiblingMaxSize
                    parents = (p, t) :: nestingScope.parents}
            let childContentResult, ns1 =
                match chFunc with
                | Some chFunc ->
                    let childP =
                        match codec, lm.lg.choiceChildDecodePath sChildTypeDef sChildName with
                        | Decode, Some customPath -> {CodegenScope.modName = p.modName; accessPath = customPath}
                        | _ ->
                            if lm.lg.acn.choice_requires_tmp_decoding && codec = Decode then
                                {CodegenScope.modName = p.modName; accessPath = AccessPath.valueEmptyPath ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")}
                            else {p with accessPath = lm.lg.getChChildForKind p.accessPath (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String child.chType.Kind codec}
                    chFunc.funcBody us acnArgs childNestingScope childP
                | None -> None, us

            let childContent_funcBody, childContent_localVariables, childContent_userDefFuncs, childContent_errCodes, auxiliaries =
                match childContentResult with
                | None              ->
                    match codec with
                    | Encode -> lm.lg.emptyStatement, [], [], [],[]
                    | Decode ->
                        let childp =
                            match lm.lg.acn.choice_requires_tmp_decoding with
                            | true ->   ({CodegenScope.modName = p.modName; accessPath = AccessPath.valueEmptyPath ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")})
                            | false ->  ({p with accessPath = lm.lg.getChChild p.accessPath (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String})
                        let decStatement =
                            match child.chType.ActualType.Kind with
                            | NullType _    -> lm.lg.decode_nullType (childp.accessPath.joined lm.lg)
                            | Sequence _    -> lm.lg.decodeEmptySeq (childp.accessPath.joined lm.lg)
                            | _             -> None
                        match decStatement with
                        | None -> lm.lg.emptyStatement,[], [], [], []
                        | Some ret ->
                            ret ,[],[],[], []

                | Some childContent -> childContent.funcBody,  childContent.localVariables, childContent.userDefinedFunctions, childContent.errCodes, childContent.auxiliaries

            let childBody =
                let sChoiceTypeName = typeDefinitionName
                match child.Optionality with
                | Some (ChoiceAlwaysAbsent) -> Some (choiceChildAlwaysAbsent (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.presentWhenName (Some defOrRef) child) (BigInteger idx) errCode.errCodeName codec)
                | Some (ChoiceAlwaysPresent)
                | None  ->
                    match ec with
                    | CEC_uper  ->
                        Some (choiceChild (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.presentWhenName (Some defOrRef) child) (BigInteger idx) nIndexSizeInBits nMax childContent_funcBody sChildName sChildTypeDef sChoiceTypeName sChildInitExpr codec)
                    | CEC_enum (enm,_) ->
                        let getDefOrRef (a:Asn1AcnAst.ReferenceToEnumerated) =
                            match p.modName = ToC a.modName with
                            | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}
                            | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName); typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}


                        let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = child.Name.Value)
                        // In deferred mode, the case discriminant is the deferred ref's
                        // .Value field (Asn1UInt) rather than the enum-typed determinant,
                        // so case branches must use the ACN-encoded integer literal.
                        // In legacy mode keep the enum literal name (preserves byte-identity).
                        let enmCaseName =
                            if r.args.acnDeferred && codec = Decode then enmItem.acnEncodeValue.ToString()
                            else lm.lg.getNamedItemBackendName (Some (getDefOrRef enm)) enmItem
                        Some (choiceChild_Enum (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) enmCaseName (lm.lg.presentWhenName (Some defOrRef) child) childContent_funcBody sChildName sChildTypeDef sChoiceTypeName sChildInitExpr codec)
                    | CEC_presWhen  ->
                        let handPresenceCond (cond:AcnGenericTypes.AcnPresentWhenConditionChoiceChild) =
                            match cond with
                            | PresenceInt  (relPath, intLoc)   ->
                                let extField = getExternalFieldChoicePresentWhen lm r deps t.id relPath
                                // Note: we always decode the external field as a asn1SccSint or asn1SccUint, therefore
                                // we do not need the exact integer class (i.e. bit width). However, some backends
                                // such as Scala requires the signedness to be passed.
                                let tp = getExternalFieldTypeChoicePresentWhen r deps t.id relPath
                                let unsigned =
                                    match tp with
                                    | Some (AcnInsertedType.AcnInteger int) -> int.isUnsigned
                                    | Some (AcnInsertedType.AcnNullType _) -> true
                                    | _ -> false
                                choiceChild_preWhen_int_condition extField (lm.lg.asn1SccIntValueToString intLoc.Value unsigned)
                            | PresenceStr  (relPath, strVal)   ->
                                let strType =
                                    deps.acnDependencies |>
                                    List.filter(fun d -> d.asn1Type = t.id) |>
                                    List.choose(fun d ->
                                        match d.dependencyKind with
                                        | AcnDepPresenceStr(relPathCond, ch, str)  when relPathCond = relPath-> Some str
                                        | _     -> None) |> Seq.head
                                let extField = getExternalFieldChoicePresentWhen lm r deps t.id relPath
                                let arrNulls = [0 .. ((int strType.maxSize.acn) - strVal.Value.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
                                let bytesStr =
                                    let baseBytes = System.Text.Encoding.ASCII.GetBytes strVal.Value
                                    match lm.lg.nullTerminatorByte with
                                    | Some nullByte -> Array.append baseBytes [| nullByte |]
                                    | None -> baseBytes
                                choiceChild_preWhen_str_condition extField strVal.Value arrNulls bytesStr
                        let conds = child.acnPresentWhenConditions |>List.map handPresenceCond
                        let pp, _ = joinedOrAsIdentifier lm codec p
                        // bIsPrimitive = true when:
                        // - no ACN function (NULL/empty-SEQUENCE with childContentResult=None)
                        // - NullType child even with ACN function (Python Copy-decoding returns Some
                        //   from createNullTypeFunction but NullType has no decode_acn method)
                        let bIsPrimitive =
                            childContentResult.IsNone ||
                            (match child.chType.ActualType.Kind with NullType _ -> true | _ -> false)
                        Some (choiceChild_preWhen pp (lm.lg.getAccess p.accessPath) (lm.lg.presentWhenName (Some defOrRef) child) childContent_funcBody conds (idx=0) sChildName sChildTypeDef sChoiceTypeName sChildInitExpr bIsPrimitive [] codec)
            [{|childBody=childBody; lvs=childContent_localVariables; userDefFuncs=childContent_userDefFuncs; errCodes=childContent_errCodes; auxiliaries=auxiliaries|}], ns1

        let childrenStatements00, ns = children |> List.mapi (fun i x -> i,x)  |> foldMap (fun us (i,x) ->  handleChild us i x) us
        let childrenStatements0 = childrenStatements00 |> List.collect id
        let childrenStatements = childrenStatements0 |> List.choose(fun (s) -> s.childBody)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (s) -> s.lvs)
        let childrenUserDefFuncs = childrenStatements0 |> List.collect(fun s -> s.userDefFuncs)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (s) -> s.errCodes)
        let childrenAuxiliaries = childrenStatements0 |> List.collect(fun (a) -> a.auxiliaries)

        let choiceContent, resultExpr =
            let pp, resultExpr = joinedOrAsIdentifier lm codec p
            let access = lm.lg.getAccess p.accessPath
            match ec with
            | CEC_uper        ->
                choice_uper pp access childrenStatements nMax sChoiceIndexName td nIndexSizeInBits errCode.errCodeName codec, resultExpr
            | CEC_enum   enm  ->
                let extField = getExternalField lm r deps t.id
                // In deferred mode the case discriminant is Asn1UInt (det.Value),
                // so Ada needs an explicit when-others to cover the full integer
                // range. In legacy mode the discriminant is the enum type and
                // when-others would be redundant — Ada flags it as a warning,
                // which becomes an error under -gnatwe.
                let bIsDeferred = r.args.acnDeferred
                choice_Enum pp access childrenStatements extField td errCode.errCodeName bIsDeferred codec, resultExpr
            | CEC_presWhen    -> choice_preWhen pp  access childrenStatements td errCode.errCodeName codec, resultExpr
        let choiceContent = lm.lg.generateChoiceProof r ACN t o choiceContent p.accessPath codec
        let aux = lm.lg.generateChoiceAuxiliaries r ACN t o nestingScope p.accessPath codec
        Some ({AcnFuncBodyResult.funcBody = choiceContent; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; userDefinedFunctions=childrenUserDefFuncs; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=childrenAuxiliaries@aux; icdResult = Some icd}), ns


    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)


    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations [] us, ec
