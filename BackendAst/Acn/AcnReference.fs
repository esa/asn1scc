module AcnReference

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


let emptyIcdFnc fieldName sPresent comments  = [],[]

// Construct the IcdArgAux that a reference type contributes to its parent's
// ICD.  Delegates the row generation to baseType.icdTas, optionally prefixed
// with a Length determinant row when the reference is a CONTAINING with
// LENGTH_EMBEDDED encoding.  Shared between the legacy inline path
// (createReferenceFunction_inline below) and the --acn-v2 deferred path
// (DAstACNDeferred.fs).  Without this delegation the deferred path used to
// emit None, which silently dropped every deferred reference (and all its
// transitive dependents) from the generated _new.html ICD.
let buildReferenceIcdArgAux
        (r: Asn1AcnAst.AstRoot)
        (t: Asn1AcnAst.Asn1Type)
        (o: Asn1AcnAst.ReferenceType)
        (baseType: Asn1Type) : IcdArgAux option =
    // Named-type identity of the referenced TAS ("INTEGER (I32)", roadmap A4)
    // is added centrally by AcnFunctionWrapper.createAcnFunction from the
    // resolved instance's inheritInfo - the delegation below must therefore
    // NOT suffix the rows again.
    let icdFnc, extraComment, name, refCanBeEmbedded =
        match r.args.generateAcnIcd with
        | true  ->
            match o.encodingOptions with
            | None ->
                let name =
                    match o.hasExtraConstrainsOrChildrenOrAcnArgs with
                    | false -> None
                    | true  ->
                        // In --acn-v2, closure conversion sets
                        // hasExtraConstrainsOrChildrenOrAcnArgs=true and appends
                        // acnArguments to EVERY cross-scope determinant reference,
                        // even a plain TAS reference the user wrote without extras
                        // (e.g. pdu10's `hdr Header`).  Using the usage-path RDD
                        // name (MyModule-PDU-hdr) then leaks the internal transform
                        // into the ICD; legacy mode names the table after the
                        // referenced TAS (Header).  Present the deferred reference
                        // as the referenced TAS so v2 matches legacy (roadmap B8).
                        match r.args.acnDeferred with
                        | true  -> None
                        | false -> Some t.id.AsString.RDD
                match baseType.icdTas with
                | Some baseTypeIcdTas ->
                    let icdFnc fieldName sPresent comments  =
                        baseTypeIcdTas.createRowsFunc fieldName sPresent comments
                    // A plain reference inherits the referenced type's embeddability,
                    // so a reference to a collapsible SEQUENCE OF (roadmap D2) inlines
                    // it into the parent just as an inline SEQUENCE OF child does
                    // (e.g. deduced MyPDU.items -> TLVList).
                    icdFnc, baseTypeIcdTas.comments, name, baseTypeIcdTas.canBeEmbedded
                | None -> emptyIcdFnc, [], name, false
            | Some encOptions ->
                let sContainerKind, sSizeUnit =
                    match encOptions.octOrBitStr with
                    | ContainedInOctString -> "OCTET STRING", "bytes"
                    | ContainedInBitString -> "BIT STRING", "bits"
                let lengthDetRow =
                    match encOptions.acnEncodingClass with
                    | SZ_EC_LENGTH_EMBEDDED nSizeInBits ->
                        [ {IcdRow.fieldName = "Length"; comments = [$"The number of {sSizeUnit} used in the encoding"]; sPresent="always";sType=IcdPlainType "INTEGER"; sConstraint=None; minLengthInBits = nSizeInBits ;maxLengthInBits=nSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]
                    | _ -> []
                let containingComments =
                    let sCaption = $"%s{sContainerKind} containing the encoding of %s{o.tasName.Value}"
                    match encOptions.acnEncodingClass with
                    | SZ_EC_ExternalField relPath  -> [sCaption; $"Size in %s{sSizeUnit} is given by the external field %s{relPath.AsString}"]
                    | SZ_EC_LENGTH_EMBEDDED _      -> [sCaption]    // the size source is documented by the Length row
                    | SZ_EC_FIXED_SIZE             -> [sCaption]
                    | SZ_EC_TerminationPattern _   -> [sCaption]
                    | SZ_EC_Deduced                -> [sCaption]
                match baseType.icdTas with
                | Some baseTypeIcdTas ->
                    let icdFnc fieldName sPresent comments  =
                        let rows, compChildren = baseTypeIcdTas.createRowsFunc fieldName sPresent comments
                        lengthDetRow@rows |> List.mapi(fun i rw -> {rw with idxOffset = Some (i+1)}), compChildren
                    // A CONTAINING carrier keeps its own table and is never inlined
                    // into its parent, even when the contained type is an embeddable
                    // SEQUENCE OF / primitive: the octet/bit-string framing boundary
                    // must stay visible (roadmap D2 "not the direct target of a
                    // CONTAINING clause").
                    icdFnc, (containingComments@baseTypeIcdTas.comments), Some (t.id.AsString.RDD + "_OCT_STR"), false
                | None -> emptyIcdFnc, [], None, false
        | false -> emptyIcdFnc, [], None, false
    match baseType.icdTas with
    | Some baseTypeIcdTas ->
        Some {IcdArgAux.canBeEmbedded = refCanBeEmbedded; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=extraComment; scope="REFTYPE"; name=name}
    | None -> None


let createReferenceFunction_inline (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
  let baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t "_ACN" codec

  let icd = buildReferenceIcdArgAux r t o baseType

  match o.encodingOptions with
  | None          ->
      match o.hasExtraConstrainsOrChildrenOrAcnArgs with
      | true  ->
          // TODO: this is where stuff gets inlined
          TL "ACN_REF_01" (fun () ->
          match codec with
            | Codec.Encode  -> baseType.getAcnFunction codec, us
            | Codec.Decode  ->
                let paramsArgsPairs = List.zip o.acnArguments o.resolvedType.acnParameters
                let baseTypeAcnFunction = baseType.getAcnFunction codec
                let ret =
                    match baseTypeAcnFunction with
                    | None  -> None
                    | Some baseTypeAcnFunction   ->
                        let funcBody us (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
                            baseTypeAcnFunction.funcBody us (acnArgs@paramsArgsPairs) nestingScope p
                        Some  {baseTypeAcnFunction with funcBody = funcBody}

                ret, us)
      | false ->
            let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
                TL "ACN_REF_02" (fun () ->
                //'size deduced': when fixed-size components follow this reference in its
                //decoding region, the standalone function cannot be used on decode (its
                //body assumes a zero trailing constant).  Emit the base type's body
                //inline instead, so the usage-specific trailing constant flows in
                //through the NestingScope (Docs/deduced-size-spec.md par.6.3).
                match codec = Decode && nestingScope.deducedTrailingBits <> 0I && isLiveDeduced o.resolvedType with
                | true ->
                    match baseType.getAcnFunction codec with
                    | Some baseFnc ->
                        let res, ns = baseFnc.funcBody us acnArgs nestingScope p
                        //The inlined body references the base TAS's own error-code constants
                        //(they carry the base type's names, not usage-path names).  When the
                        //base TAS lives in the same module those constants are already declared
                        //next to the base TAS's own function: re-declaring them here would be a
                        //benign duplicate #define in C but an illegal duplicate declaration in
                        //Ada.  Cross-module usages keep the re-declaration (the generated Ada
                        //bodies only 'with' sibling modules, so the local copy is what makes
                        //the unqualified name visible -- same as the C duplicate #define).
                        let res =
                            match o.modName.Value = t.id.ModName with
                            | true  -> res |> Option.map (fun rr -> {rr with errCodes = []})
                            | false -> res
                        res, ns
                    | None         -> None, us
                | false ->
                let pp, resultExpr =
                    let str = lm.lg.getParamValue t p.accessPath codec
                    match codec, lm.lg.decodingKind with
                    | Decode, Copy ->
                        let toc = ToC str
                        toc, Some toc
                    | _ -> str, None
                let funcBodyContent =
                    match p.accessPath.steps with
                    | [] ->
                        let baseContent = callSuperclassFunc lm pp baseFncName codec
                        match codec with
                        | Decode when nestingScope.nestingLevel = 0I ->
                            let rec isPrimitive (kind: Asn1AcnAst.Asn1TypeKind) =
                                match kind with
                                | Asn1AcnAst.Integer _ | Asn1AcnAst.Real _ | Asn1AcnAst.Boolean _ -> true
                                | Asn1AcnAst.ReferenceType refType -> isPrimitive refType.resolvedType.Kind
                                | _ -> false
                            match lm.lg.subtypeDecodeWrap pp (lm.lg.getLongTypedefName typeDefinition) (isPrimitive o.resolvedType.Kind) with
                            | Some wrapLine -> baseContent + "\n" + wrapLine
                            | None -> baseContent
                        | _ -> baseContent
                    | _ -> callBaseTypeFunc lm pp baseFncName codec
                Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = icd}), us)

            let ns =
                match t.id.topLevelTas with
                | None -> us
                | Some tasInfo ->
                    let caller = {Caller.typeId = tasInfo; funcType=AcnEncDecFunctionType}
                        //match List.rev t.referencedBy with
                        //| [] -> {Caller.typeId = tasInfo; funcType=AcnEncDecFunctionType}
                        //| hd::_ -> {Caller.typeId = {TypeAssignmentInfo.modName = hd.modName; tasName=hd.tasName}; funcType=AcnEncDecFunctionType}

                    let callee = {Callee.typeId = {TypeAssignmentInfo.modName = o.modName.Value; tasName=o.tasName.Value} ; funcType=AcnEncDecFunctionType}
                    addFunctionCallToState us caller callee

            let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
            let a, ns = AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc funcBody (fun atc -> true) soSparkAnnotations [] ns
            Some a, ns

    | Some encOptions ->
        //contained type i.e. MyOct ::= OCTET STRING (CONTAINING Other-Type)
        TL "ACN_REF_03" (fun () ->
        let loc = o.tasName.Location
        let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ACN_ENCODING" baseTypeDefinitionName
        let sReqBitForUperEncoding = sprintf "%s_REQUIRED_BITS_FOR_ACN_ENCODING" baseTypeDefinitionName

        let octet_string_containing_func            = lm.acn.octet_string_containing_func
        let bit_string_containing_func              = lm.acn.bit_string_containing_func
        let octet_string_containing_ext_field_func  = lm.acn.octet_string_containing_ext_field_func
        let bit_string_containing_ext_field_func    = lm.acn.bit_string_containing_ext_field_func

        let baseTypeAcnFunction = baseType.getAcnFunction codec
        //(AcnFuncBodyResult option) * State
        let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) : (AcnFuncBodyResult option)* State =
            TL "ACN_REF_04" (fun () ->
            let pp, resultExpr =
                let str = lm.lg.getParamValue t p.accessPath codec
                match codec, lm.lg.decodingKind with
                | Decode, Copy ->
                    let toc = ToC str
                    toc, Some toc
                | _ -> str, None
            let funcBodyContent, errCodes, localVariables, userDefinedFunctions, ns2 =
                match encOptions.acnEncodingClass, encOptions.octOrBitStr with
                | SZ_EC_ExternalField    relPath    , ContainedInOctString  ->
                    let filterDependency (d:AcnDependency) =
                        match d.dependencyKind with
                        | AcnDepSizeDeterminant_bit_oct_str_contain _   -> true
                        | _                              -> false
                    let extField        = getExternalField0 lm r deps t.id filterDependency false
                    let soInner, errCodes0, localVariables0, userDefinedFunctions, ns1 =
                        match baseTypeAcnFunction with
                        | None  -> None, [], [], [], us
                        | Some baseTypeAcnFunction   ->
                            //CONTAINING boundary: the contained encoding is decoded against its own
                            //region (the container's octets), so the trailing constant resets to 0
                            let acnRes, ns = baseTypeAcnFunction.funcBody us acnArgs {nestingScope with deducedTrailingBits = 0I} p
                            match acnRes with
                            | None  -> None, [], [], [], ns
                            | Some r -> Some r.funcBody, r.errCodes, r.localVariables, r.userDefinedFunctions, ns

                    let fncBody = octet_string_containing_ext_field_func pp baseFncName sReqBytesForUperEncoding extField errCode.errCodeName soInner codec

                    let lvs =
                        let localVars2 = lm.lg.acn.getAcnContainingByLocVars sReqBytesForUperEncoding
                        localVariables0@localVars2

                    fncBody, errCode::errCodes0,lvs, userDefinedFunctions, ns1
                | SZ_EC_ExternalField    relPath    , ContainedInBitString  ->
                    let extField        = getExternalField lm r deps t.id
                    let fncBody = bit_string_containing_ext_field_func pp baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding extField errCode.errCodeName codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_FIXED_SIZE        , ContainedInOctString  ->
                    let fncBody = octet_string_containing_func pp baseFncName baseTypeDefinitionName sReqBytesForUperEncoding 0I encOptions.minSize.acn encOptions.maxSize.acn true codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_LENGTH_EMBEDDED nBits , ContainedInOctString  ->
                    let fncBody = octet_string_containing_func pp baseFncName baseTypeDefinitionName sReqBytesForUperEncoding nBits encOptions.minSize.acn encOptions.maxSize.acn false codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_FIXED_SIZE                        , ContainedInBitString  ->
                    let fncBody = bit_string_containing_func pp baseFncName baseTypeDefinitionName sReqBytesForUperEncoding sReqBitForUperEncoding 0I encOptions.minSize.acn encOptions.maxSize.acn true codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_LENGTH_EMBEDDED nBits                 , ContainedInBitString  ->
                    let fncBody = bit_string_containing_func pp baseFncName baseTypeDefinitionName sReqBytesForUperEncoding sReqBitForUperEncoding nBits encOptions.minSize.acn encOptions.maxSize.acn false codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_TerminationPattern nullVal  ,  _                    ->  raise(SemanticError (loc, "Invalid type for parameter4"))
                | SZ_EC_Deduced                     ,  _                    ->  raise(SemanticError (loc, "'size deduced': backend code generation is not implemented yet"))
            let funcBodyResult = Some ({AcnFuncBodyResult.funcBody = funcBodyContent; userDefinedFunctions=userDefinedFunctions; errCodes = errCodes; localVariables = localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = icd})
            funcBodyResult, ns2)

        let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
        let a,b = AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody us e acnArgs nestingScope p) (fun atc -> true) soSparkAnnotations [] us
        Some a, b)
