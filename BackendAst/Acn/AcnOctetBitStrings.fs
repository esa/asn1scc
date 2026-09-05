module AcnOctetBitStrings

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


let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let oct_external_field           = lm.acn.oct_external_field
    let oct_external_field_fix_size  = lm.acn.oct_external_field_fix_size
    let oct_sqf_null_terminated          = lm.acn.oct_sqf_null_terminated
    let fixedSize       = lm.uper.octet_FixedSize
    let varSize         = lm.uper.octet_VarSize
    let InternalItem_oct_str             = lm.uper.InternalItem_oct_str
    let nAlignSize = 0I;
    let td = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let ii = p.accessPath.SequenceOfLevel + 1
        let i = sprintf "i%d" ii
        let lv = SequenceOfIndex (ii, None)
        let pp, resultExpr = joinedOrAsIdentifier lm codec p
        let access = lm.lg.getAccess p.accessPath
        let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName
        let funcBodyContent =
            match o.acnEncodingClass with
            | SZ_EC_FIXED_SIZE ->
                let fncBody = fixedSize sType pp access o.minSize.acn codec
                Some(fncBody, [errCode],[])

            | SZ_EC_LENGTH_EMBEDDED lenSize ->
                let fncBody = varSize sType pp access (o.minSize.acn) (o.maxSize.acn) lenSize errCode.errCodeName codec
                let nStringLength =
                    match codec with
                    | Encode -> []
                    | Decode -> [lm.lg.uper.count_var]

                Some(fncBody, [errCode],nStringLength)
            | SZ_EC_ExternalField _ ->
                let extField = getExternalField lm r deps t.id
                let tp = getExternalFieldType r deps t.id
                let unsigned =
                    match tp with
                    | Some (AcnInsertedType.AcnInteger int) -> int.isUnsigned
                    | Some (AcnInsertedType.AcnNullType _) -> true
                    | _ -> false
                let detMaxOpt =
                    match tp with
                    | Some (AcnInsertedType.AcnInteger int) ->
                        match int.uperRange with
                        | Concrete (_, detMax) -> Some detMax
                        | NegInf detMax        -> Some detMax
                        | _                    -> None
                    | _ -> None
                let noSizeMin = if o.minSize.acn = 0I then None else Some o.minSize.acn
                let noSizeMax =
                    match detMaxOpt with
                    | Some detMax when detMax <= o.maxSize.acn -> None
                    | _ -> Some o.maxSize.acn
                let fncBody =
                    match o.isFixedSize with
                    | true  -> oct_external_field_fix_size sType pp access noSizeMin ( o.maxSize.acn) extField unsigned nAlignSize errCode.errCodeName codec
                    | false -> oct_external_field sType pp access noSizeMin noSizeMax extField unsigned nAlignSize errCode.errCodeName codec
                Some(fncBody, [errCode],[])
            | SZ_EC_TerminationPattern bitPattern  ->
                let mod8 = bitPattern.Value.Length % 8
                let suffix = [1 .. mod8] |> Seq.map(fun _ -> "0") |> Seq.StrJoin ""
                let bitPatten8 = bitPattern.Value + suffix
                let byteArray = bitStringValueToByteArray bitPatten8.AsLoc
                let internalItem = InternalItem_oct_str pp access i  errCode.errCodeName codec
                let noSizeMin = if o.minSize.acn=0I then None else Some ( o.minSize.acn)
                let fncBody = oct_sqf_null_terminated pp access i internalItem noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName  8I 8I sType codec
                let lv2 =
                    match codec, lm.lg.acn.checkBitPatternPresentResult with
                    | Decode, true    -> [IntegerLocalVariable ("checkBitPatternPresentResult", Some (lm.lg.intValueToString 0I (ASN1SCC_Int8 (-128I, 127I))))]
                    | _            -> []
                Some(fncBody, [errCode],lv::lv2)
            | SZ_EC_Deduced ->
                let noSizeMin = if o.minSize.acn = 0I then None else Some o.minSize.acn
                let fncBody = lm.acn.oct_deduced td pp access noSizeMin o.maxSize.acn nestingScope.deducedTrailingBits errCode.errCodeName codec
                Some(fncBody, [errCode], [])

        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) ->
            let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createOctetStringFunction r t o)
            let icd = AcnPrimitiveFactory.buildOctetOrBitStringIcdAux t.acnAlignment o.acnEncodingClass "bytes" o.maxSize.acn (getASN1Name t) (getASN1Name t) sAsn1Constraints o.acnMinSizeInBits o.acnMaxSizeInBits t.unitsOfMeasure

            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd})
    let soSparkAnnotations = Some (sparkAnnotations lm td codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] us

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let nAlignSize = 0I;
    let bitString_FixSize = lm.uper.bitString_FixSize
    let bitString_VarSize = lm.uper.bitString_VarSize

    let td = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let pp, resultExpr = joinedOrAsIdentifier lm codec p
        let access = lm.lg.getAccess p.accessPath
        let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName
        let funcBodyContent =
            match o.acnEncodingClass with
            | SZ_EC_ExternalField   _    ->
                let extField = getExternalField lm r deps t.id
                let tp = getExternalFieldType r deps t.id
                let detMaxOpt =
                    match tp with
                    | Some (AcnInsertedType.AcnInteger int) ->
                        match int.uperRange with
                        | Concrete (_, detMax) -> Some detMax
                        | NegInf detMax        -> Some detMax
                        | _                    -> None
                    | _ -> None
                let noSizeMin = if o.minSize.acn = 0I then None else Some o.minSize.acn
                let noSizeMax =
                    match detMaxOpt with
                    | Some detMax when detMax <= o.maxSize.acn -> None
                    | _ -> Some o.maxSize.acn
                let fncBody =
                    match o.minSize.uper = o.maxSize.uper with
                    | true  -> lm.acn.bit_string_external_field_fixed_size sType pp errCode.errCodeName access noSizeMin ( o.maxSize.acn) extField codec
                    | false  -> lm.acn.bit_string_external_field sType pp errCode.errCodeName access noSizeMin noSizeMax extField codec
                Some (fncBody, [errCode], [])
            | SZ_EC_TerminationPattern   bitPattern    ->
                let mod8 = bitPattern.Value.Length % 8
                let suffix = [1 .. mod8] |> Seq.map(fun _ -> "0") |> Seq.StrJoin ""
                let bitPatten8 = bitPattern.Value + suffix
                let byteArray = bitStringValueToByteArray bitPatten8.AsLoc
                let i = sprintf "i%d" (p.accessPath.SequenceOfLevel + 1)
                let lv = SequenceOfIndex (p.accessPath.SequenceOfLevel + 1, None)
                let bFixedSize = o.minSize.acn = o.maxSize.acn
                let fncBody = lm.acn.bit_string_null_terminated sType pp errCode.errCodeName access i (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) byteArray bitPattern.Value.Length.AsBigInt bFixedSize codec
                Some (fncBody, [errCode], [])
            | SZ_EC_FIXED_SIZE       ->
                let fncBody = bitString_FixSize sType pp access o.minSize.acn errCode.errCodeName codec
                Some(fncBody, [errCode],[])

            | SZ_EC_Deduced ->
                raise(BugErrorException "'size deduced' is not applicable to BIT STRING (should have been rejected by the front-end)")

            | SZ_EC_LENGTH_EMBEDDED nSizeInBits ->
                let fncBody =
                    bitString_VarSize sType pp access o.minSize.acn o.maxSize.acn errCode.errCodeName nSizeInBits codec
                let nStringLength =
                    match codec with
                    | Encode -> []
                    | Decode -> [lm.lg.uper.count_var]
                Some(fncBody, [errCode],nStringLength)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) ->
            let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createBitStringFunction r t o)
            let icd = AcnPrimitiveFactory.buildOctetOrBitStringIcdAux t.acnAlignment o.acnEncodingClass "bits" o.maxSize.acn (getASN1Name t) (getASN1Name t) sAsn1Constraints o.acnMinSizeInBits o.acnMaxSizeInBits t.unitsOfMeasure

            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd})

    let soSparkAnnotations = Some(sparkAnnotations lm td codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] us
