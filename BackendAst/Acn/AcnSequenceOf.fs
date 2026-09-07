module AcnSequenceOf

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


let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option)  (child:Asn1Type) (us:State)  =
    let oct_sqf_null_terminated = lm.acn.oct_sqf_null_terminated
    let oct_sqf_external_field_fix_size = lm.acn.sqf_external_field_fix_size
    let external_field          = lm.acn.sqf_external_field
    let fixedSize               = lm.uper.seqOf_FixedSize
    let varSize                 = lm.acn.seqOf_VarSize


    let i ii = sprintf "i%d" ii
    let lv ii =
        match o.acnEncodingClass with
        | SZ_EC_FIXED_SIZE
        | SZ_EC_LENGTH_EMBEDDED _ //-> lm.lg.uper.seqof_lv t.id o.minSize.uper o.maxSize.uper
        | SZ_EC_ExternalField       _
        | SZ_EC_TerminationPattern  _
        | SZ_EC_Deduced               -> [SequenceOfIndex (ii, None)]

    let nAlignSize = 0I;
    let nIntItemMaxSize = child.acnMaxSizeInBits
    let td = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName

    let sExtraComment =
        match o.acnEncodingClass with
        | Asn1AcnAst.SZ_EC_FIXED_SIZE                    -> $"Length is fixed to {o.maxSize.acn} elements (no length determinant is needed)."
        | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _             -> if o.maxSize.acn <2I then "The array contains a single element." else ""
        | Asn1AcnAst.SZ_EC_ExternalField relPath         ->  $"Length is determined by the external field: %s{relPath.AsString}"
        | Asn1AcnAst.SZ_EC_TerminationPattern bitPattern ->  $"Length is determined by the stop marker '%s{bitPattern.Value}'"
        | Asn1AcnAst.SZ_EC_Deduced                       ->  "Length is deduced from the size of the enclosing container/PDU (no length determinant is encoded)."

    // Rows of the SEQUENCE OF's OWN standalone table (kept unchanged): a Length
    // determinant row for the embedded-length / termination-pattern classes, then
    // the first and last element rows (either inlined for an embeddable item, or a
    // single reference row + child table for a composite item), with a ThreeDOTs
    // marker for the elided middle.
    let ownTableRows fieldName sPresent comments =
        let lengthRow, terminationPattern =
            match o.acnEncodingClass with
            | SZ_EC_LENGTH_EMBEDDED _ ->
                let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                [{IcdRow.fieldName = "Length"; comments = [$"The number of items"]; sPresent="always";sType=IcdPlainType "INTEGER"; sConstraint=None; minLengthInBits = nSizeInBits ;maxLengthInBits=nSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = Some 1}], []
            | SZ_EC_FIXED_SIZE
            | SZ_EC_ExternalField       _
            | SZ_EC_Deduced               -> [], []
            | SZ_EC_TerminationPattern  bitPattern ->
                let nSizeInBits = bitPattern.Value.Length.AsBigInt
                [], [{IcdRow.fieldName = "Length"; comments = [$"Termination pattern {bitPattern.Value}"]; sPresent="always";sType=IcdPlainType "INTEGER"; sConstraint=None; minLengthInBits = nSizeInBits ;maxLengthInBits=nSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = Some (int (o.maxSize.acn+1I))}]
        match child.icdTas with
        | Some childIcdTas ->
            match childIcdTas.canBeEmbedded with
            | true ->
                let chRows = (childIcdTas.createRowsFunc "Item #1" "always" [] |> fst) |> List.map(fun r -> {r with idxOffset = Some (lengthRow.Length + 1)})
                let lastChRows = chRows |> List.map(fun r -> {r with fieldName = $"Item #{o.maxSize.acn}"; idxOffset = Some ((int o.maxSize.acn)+lengthRow.Length)})
                lengthRow@chRows@[THREE_DOTS]@lastChRows@terminationPattern, []
            | false ->
                let sType = TypeHash childIcdTas.hash
                let sConstraint = constraintsToIcdStr child.ConstraintsAsn1Str
                let a1 = {IcdRow.fieldName = "Item #1"; comments = comments; sPresent=sPresent;sType=sType; sConstraint=sConstraint; minLengthInBits = child.acnMinSizeInBits; maxLengthInBits=child.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = Some (lengthRow.Length + 1)}
                let a2 = {a1 with fieldName = $"Item #{o.maxSize.acn}"; idxOffset = Some ((int o.maxSize.acn)+lengthRow.Length)}
                lengthRow@[a1;THREE_DOTS;a2], [childIcdTas]
        | None -> lengthRow@terminationPattern, []

    // Rows of the SEQUENCE OF when it is COLLAPSED into its parent's row group
    // (roadmap D2, the ESA "embed SEQUENCE OF into parent tables" ask): a single
    // header FieldRow that names the field, carries the SEQUENCE OF's own SIZE
    // constraint and full ACN size (so the parent's byte totals and bit-offset
    // column are unchanged), followed by the first/last element rows tagged as
    // SubItemRow (rendered as indented "field[k]" rows sub-numbered "N.k").
    let embeddedRows fieldName sPresent comments =
        match child.icdTas with
        | Some childIcdTas ->
            let headerComments = comments @ (match sExtraComment with "" -> [] | c -> [c])
            let headerRow =
                {IcdRow.fieldName = fieldName; comments = headerComments; sPresent = sPresent; sType = IcdPlainType (getASN1Name t); sConstraint = constraintsToIcdStr (DAstAsn1.createSequenceOfFunction r t o); minLengthInBits = t.acnMinSizeInBits; maxLengthInBits = t.acnMaxSizeInBits; sUnits = None; rowType = IcdRowType.FieldRow; idxOffset = None}
            let elementRows (elemIdx: bigint) =
                childIcdTas.createRowsFunc $"{fieldName}[{elemIdx}]" sPresent [] |> fst
                |> List.map(fun r -> {r with rowType = IcdRowType.SubItemRow; idxOffset = Some (int elemIdx)})
            headerRow :: (elementRows 1I @ [THREE_DOTS] @ elementRows o.maxSize.acn), []
        | None -> ownTableRows fieldName sPresent comments

    // A SEQUENCE OF collapses into its parent (D2) only when ALL hold:
    //  - its item type is embeddable (a primitive, so the element rows are
    //    plain field rows rather than a link to the item's own table);
    //  - after ThreeDots compression it contributes only a small number of rows
    //    (threshold 5) - this rejects nested embeddable SEQUENCE OFs whose
    //    element expansion would explode the parent table.
    // The "not the direct target of a CONTAINING clause" condition is enforced
    // at the reference site (AcnReference.buildReferenceIcdArgAux keeps a
    // CONTAINING carrier non-embeddable), where the usage context is known.
    let icdSeqOfEmbedThreshold = 5
    let canEmbed =
        match child.icdTas with
        | Some childIcdTas when childIcdTas.canBeEmbedded ->
            let rows, _ = embeddedRows "x" "always" []
            (rows |> List.filter(fun r -> r.rowType <> IcdRowType.ThreeDOTs) |> List.length) <= icdSeqOfEmbedThreshold
        | _ -> false

    let icdFnc fieldName sPresent comments  =
        match canEmbed, fieldName with
        // fieldName is "" only when AcnIcd.createIcdTas seeds the SEQUENCE OF's
        // own record (used for its standalone TAS table and its content hash);
        // a parent embedding it always passes a real field name.
        | true, "" -> ownTableRows fieldName sPresent comments
        | true, _  -> embeddedRows fieldName sPresent comments
        | false, _ -> ownTableRows fieldName sPresent comments
    let icd = {IcdArgAux.canBeEmbedded = canEmbed; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[sExtraComment]; scope="type"; name= None}


    let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let level = p.accessPath.SequenceOfLevel + 1
        let pp, resultExpr = joinedOrAsIdentifier lm codec p
        // `childInitExpr` is used to initialize the array of elements in which we will write their decoded values
        // It is only meaningful for "Copy" decoding kind, since InPlace will directly modify `p`'s array
        let childInitExpr = DAstInitialize.getChildExpression lm child
        let access = lm.lg.getAccess p.accessPath
        let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName
        match child.getAcnFunction codec with
        | None -> None, us
        | Some chFunc  ->
            let childNestingScope = {nestingScope with nestingLevel = nestingScope.nestingLevel + 1I; parents = (p, t) :: nestingScope.parents}
            let chp =
                if lm.lg.ArrayInitByAppend && codec = Decode then
                    let tempPath = AccessPath.emptyPath (pp + lm.lg.TempArrayItemSuffix) ByValue
                    {p with accessPath = { tempPath with phantomArrayDepth = p.accessPath.SequenceOfLevel + 1 }}
                else
                    {p with accessPath = lm.lg.getArrayItem p.accessPath (i level) child.isIA5String}
            let internalItem, ns = chFunc.funcBody us acnArgs childNestingScope chp
            let sqfProofGen = {
                SequenceOfLikeProofGen.t = Asn1TypeOrAcnRefIA5.Asn1 t
                acnOuterMaxSize = nestingScope.acnOuterMaxSize
                uperOuterMaxSize = nestingScope.uperOuterMaxSize
                nestingLevel = nestingScope.nestingLevel
                nestingIx = nestingScope.nestingIx
                acnMaxOffset = nestingScope.acnOffset
                uperMaxOffset = nestingScope.uperOffset
                nestingScope = nestingScope
                cs = p
                encDec = internalItem |> Option.map (fun ii -> ii.funcBody)
                elemDecodeFn = None
                ixVariable = (i level)
            }
            let auxiliaries, callAux = lm.lg.generateSequenceOfLikeAuxiliaries r ACN (SqOf o) sqfProofGen codec

            let wrapWithAppend (ii: AcnFuncBodyResult) =
                match codec, lm.lg.decodingKind, lm.lg.ArrayInitByAppend with
                | Decode, Copy, true when ii.resultExpr.IsSome ->
                    ii.funcBody + "\n" + (lm.uper.update_array_item pp (i level) ii.resultExpr.Value)
                | _ -> ii.funcBody

            let ret =
                match o.acnEncodingClass with
                | SZ_EC_FIXED_SIZE
                | SZ_EC_LENGTH_EMBEDDED _ ->
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (o.maxSize.acn - o.minSize.acn)
                    let nStringLength =
                        match o.minSize.uper = o.maxSize.uper,  codec with
                        | true , _    -> []
                        | false, Encode -> []
                        | false, Decode -> [lm.lg.uper.count_var]

                    let absOffset = nestingScope.acnOffset
                    let remBits = nestingScope.acnOuterMaxSize - nestingScope.acnOffset
                    let lvl = max 0I (nestingScope.nestingLevel - 1I)
                    let ix = nestingScope.nestingIx + 1I
                    let offset = nestingScope.acnRelativeOffset
                    let introSnap = nestingScope.nestingLevel = 0I

                    match internalItem with
                    | None ->
                        match o.isFixedSize with
                        | true  -> None
                        | false ->
                            let funcBody = varSize pp access td (i level) (null : string) o.minSize.acn o.maxSize.acn nSizeInBits child.acnMinSizeInBits nIntItemMaxSize 0I childInitExpr errCode.errCodeName absOffset remBits lvl ix offset introSnap callAux sType codec
                            Some ({AcnFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = (lv level)@nStringLength; userDefinedFunctions=[]; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd})

                    | Some internalItem ->
                        let childErrCodes =  internalItem.errCodes
                        let internalItemBody = wrapWithAppend internalItem
                        let ret, localVariables =
                            match o.isFixedSize with
                            | true -> fixedSize pp td (i level) internalItemBody o.minSize.acn child.acnMinSizeInBits nIntItemMaxSize 0I childInitExpr callAux codec, nStringLength
                            | false -> varSize pp access td (i level) internalItemBody o.minSize.acn o.maxSize.acn nSizeInBits child.acnMinSizeInBits nIntItemMaxSize 0I childInitExpr errCode.errCodeName absOffset remBits lvl ix offset introSnap callAux sType codec, nStringLength
                        Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = (lv level)@(internalItem.localVariables@localVariables); userDefinedFunctions=internalItem.userDefinedFunctions; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=internalItem.auxiliaries @ auxiliaries; icdResult = Some icd})

                | SZ_EC_ExternalField _ ->
                    match internalItem with
                    | None -> None
                    | Some internalItem ->
                        let localVariables = internalItem.localVariables
                        let childErrCodes = internalItem.errCodes
                        let extField = getExternalField lm r deps t.id
                        let tp = getExternalFieldType r deps t.id
                        let unsigned =
                            match tp with
                            | Some (AcnInsertedType.AcnInteger int) -> int.isUnsigned
                            | Some (AcnInsertedType.AcnNullType _) -> true
                            | _ -> false
                        let introSnap = nestingScope.nestingLevel = 0I
                        let internalItemBody = wrapWithAppend internalItem
                        let funcBodyContent =
                            match o.isFixedSize with
                            | true  -> oct_sqf_external_field_fix_size td pp access (i level) internalItemBody (if o.minSize.acn=0I then None else Some o.minSize.acn) o.maxSize.acn extField unsigned nAlignSize errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits childInitExpr introSnap callAux codec
                            | false -> external_field td pp access (i level) internalItemBody (if o.minSize.acn=0I then None else Some o.minSize.acn) o.maxSize.acn extField unsigned nAlignSize errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits childInitExpr introSnap callAux codec
                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = (lv level)@localVariables; userDefinedFunctions=internalItem.userDefinedFunctions; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=internalItem.auxiliaries @ auxiliaries; icdResult = Some icd})

                | SZ_EC_TerminationPattern bitPattern ->
                    match internalItem with
                    | None -> None
                    | Some internalItem ->
                        let mod8 = bitPattern.Value.Length % 8
                        let suffix = [1 .. mod8] |> Seq.map(fun _ -> "0") |> Seq.StrJoin ""
                        let bitPatten8 = bitPattern.Value + suffix
                        let byteArray = bitStringValueToByteArray bitPatten8.AsLoc
                        let localVariables  = internalItem.localVariables
                        let childErrCodes   = internalItem.errCodes
                        let noSizeMin = if o.minSize.acn=0I then None else Some o.minSize.acn
                        let internalItemBody = wrapWithAppend internalItem
                        let funcBodyContent = oct_sqf_null_terminated pp access (i level) internalItemBody noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits sType codec

                        let lv2 =
                            match codec, lm.lg.acn.checkBitPatternPresentResult with
                            | Decode, true -> [IntegerLocalVariable ("checkBitPatternPresentResult", Some (lm.lg.intValueToString 0I (ASN1SCC_Int8 (-128I, 127I))))]
                            | _ -> []

                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = lv2@(lv level)@localVariables; userDefinedFunctions=internalItem.userDefinedFunctions; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=internalItem.auxiliaries; icdResult = Some icd})

                | SZ_EC_Deduced ->
                    match internalItem with
                    | None -> None
                    | Some internalItem ->
                        let childErrCodes = internalItem.errCodes
                        let localVariables = internalItem.localVariables
                        let trailingBits = nestingScope.deducedTrailingBits
                        let noSizeMin = if o.minSize.acn=0I then None else Some o.minSize.acn
                        let funcBodyContent =
                            match o.child.acnMinSizeInBits = o.child.acnMaxSizeInBits with
                            | true  -> lm.acn.sqf_deduced_fix_elem td pp access (i level) (wrapWithAppend internalItem) noSizeMin o.maxSize.acn trailingBits o.child.acnMinSizeInBits errCode.errCodeName codec
                            | false -> lm.acn.sqf_deduced_var_elem td pp access (i level) (wrapWithAppend internalItem) noSizeMin o.maxSize.acn trailingBits o.child.acnMinSizeInBits errCode.errCodeName codec
                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = (lv level)@localVariables; userDefinedFunctions=internalItem.userDefinedFunctions; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=internalItem.auxiliaries; icdResult = Some icd})
            ret,ns
    let soSparkAnnotations = Some(sparkAnnotations lm td codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations [] us
