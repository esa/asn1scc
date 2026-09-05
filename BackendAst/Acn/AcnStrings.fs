module AcnStrings

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


let createStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionOrReference)  (defOrRef:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Acn_String_Ascii_FixSize                            = lm.acn.Acn_String_Ascii_FixSize
    let Acn_String_Ascii_Internal_Field_Determinant         = lm.acn.Acn_String_Ascii_Internal_Field_Determinant
    let Acn_String_Ascii_Null_Terminated                    = lm.acn.Acn_String_Ascii_Null_Terminated
    let Acn_String_Ascii_External_Field_Determinant         = lm.acn.Acn_String_Ascii_External_Field_Determinant
    let Acn_String_CharIndex_External_Field_Determinant     = lm.acn.Acn_String_CharIndex_External_Field_Determinant
    let Acn_IA5String_CharIndex_External_Field_Determinant  = lm.acn.Acn_IA5String_CharIndex_External_Field_Determinant

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) (us:State) =
        let pp, resultExpr = adaptArgument lm codec p
        let td = (lm.lg.getStrTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName
        let funcBodyContent, ns =
            match o.acnEncodingClass with
            | Acn_Enc_String_uPER  _ ->
                uperFunc.funcBody_e errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.localVariables, x.auxiliaries), us
            | Acn_Enc_String_uPER_Ascii _ ->
                match o.maxSize.uper = o.minSize.uper with
                | true      ->  Some (Acn_String_Ascii_FixSize pp errCode.errCodeName ( o.maxSize.uper) codec, [errCode], [], []), us
                | false     ->
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                    Some (Acn_String_Ascii_Internal_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) ( o.minSize.acn) nSizeInBits sType codec, [errCode], [], []), us
            | Acn_Enc_String_Ascii_Null_Terminated (_,nullChars) ->
                Some (Acn_String_Ascii_Null_Terminated pp errCode.errCodeName ( o.maxSize.acn) nullChars sType codec, [errCode], [], []), us
            | Acn_Enc_String_Ascii_External_Field_Determinant _ ->
                let extField = getExternalField lm r deps t.id
                Some(Acn_String_Ascii_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) extField sType codec, [errCode], [], []), us
            | Acn_Enc_String_CharIndex_External_Field_Determinant _ ->
                let extField = getExternalField lm r deps t.id
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let encDecStatement =
                    match o.uperCharSet.Length = 128 with
                    | false ->
                        let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                        Acn_String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) arrAsciiCodes (BigInteger o.uperCharSet.Length) extField td nBits sType codec
                    | true -> Acn_IA5String_CharIndex_External_Field_Determinant pp errCode.errCodeName o.maxSize.acn extField td nBits (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) sType codec
                Some(encDecStatement, [errCode], [], []), us
            | Acn_Enc_String_Ascii_Deduced _ ->
                Some (lm.acn.str_ascii_deduced pp errCode.errCodeName o.maxSize.acn nestingScope.deducedTrailingBits sType codec, [errCode], [], []), us
        match funcBodyContent with
        | None -> None, ns
        | Some (funcBodyContent,errCodes, localVars, auxiliaries) ->
            let icd = AcnPrimitiveFactory.buildStringIcdAux t.acnAlignment o (getASN1Name t) (getASN1Name t) (constraintsToIcdStr (DAstAsn1.createStringFunction r t o)) t.unitsOfMeasure
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVars; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd} ), ns
    AcnPrimitiveFactory.createAsn1PrimitiveStateful r deps lm codec t typeDefinition isValidFunc [] us
        (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p us)


let createAcnStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToIA5String)  (us:State)  =
    let Acn_String_Ascii_FixSize                            = lm.acn.Acn_String_Ascii_FixSize
    let Acn_String_Ascii_Internal_Field_Determinant         = lm.acn.Acn_String_Ascii_Internal_Field_Determinant
    let Acn_String_Ascii_Null_Terminated                     = lm.acn.Acn_String_Ascii_Null_Terminated
    let Acn_String_Ascii_External_Field_Determinant         = lm.acn.Acn_String_Ascii_External_Field_Determinant
    let Acn_String_CharIndex_External_Field_Determinant     = lm.acn.Acn_String_CharIndex_External_Field_Determinant
    let Acn_IA5String_CharIndex_External_Field_Determinant  = lm.acn.Acn_IA5String_CharIndex_External_Field_Determinant
    let typeDefinitionName = ToC2(r.args.TypePrefix + t.tasName.Value)

    let o = t.str
    let uper_funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CodegenScope) =
        let td =
            let md = r.GetModuleByName t.modName
            let tas = md.GetTypeAssignmentByName t.tasName r
            match tas.Type.ActualType.Kind with
            | Asn1AcnAst.IA5String     z -> (lm.lg.getStrTypeDefinition z.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
            | Asn1AcnAst.NumericString z -> (lm.lg.getStrTypeDefinition z.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
            | _                           -> raise(SemanticError(t.tasName.Location, (sprintf "Type assignment %s.%s does not point to a string type" t.modName.Value t.modName.Value)))
        let ii = p.accessPath.SequenceOfLevel + 1
        let i = sprintf "i%d" ii
        let lv = SequenceOfIndex (ii, None)
        let charIndex =
            match lm.lg.uper.requires_charIndex with
            | false     -> []
            | true   -> [IntegerLocalVariable ("charIndex", None)]
        let nStringLength =
            match o.minSize.uper = o.maxSize.uper with
            | true  -> []
            | false ->[lm.lg.uper.createLv "nStringLength"]
        let InternalItem_string_no_alpha = lm.uper.InternalItem_string_no_alpha
        let InternalItem_string_with_alpha = lm.uper.InternalItem_string_with_alpha
        let str_FixedSize       = lm.uper.str_FixedSize
        let str_VarSize         = lm.uper.str_VarSize
        let initExpr =
            match codec, lm.lg.decodingKind with
            | Decode, Copy -> Some (lm.lg.initializeString None (int o.maxSize.uper))
            | _ -> None
        let pp, resultExpr = joinedOrAsIdentifier lm codec p
        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
        let internalItem =
            match o.uperCharSet.Length = 128 with
            | true  -> InternalItem_string_no_alpha pp errCode.errCodeName i codec
            | false ->
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                InternalItem_string_with_alpha pp errCode.errCodeName td i (BigInteger (o.uperCharSet.Length-1)) arrAsciiCodes (BigInteger (o.uperCharSet.Length)) nBits  codec
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (o.maxSize.uper - o.minSize.uper)
        let sqfProofGen = {
            SequenceOfLikeProofGen.t = Asn1TypeOrAcnRefIA5.AcnRefIA5 (typeId, t)
            acnOuterMaxSize = nestingScope.acnOuterMaxSize
            uperOuterMaxSize = nestingScope.uperOuterMaxSize
            nestingLevel = nestingScope.nestingLevel
            nestingIx = nestingScope.nestingIx
            acnMaxOffset = nestingScope.acnOffset
            uperMaxOffset = nestingScope.uperOffset
            nestingScope = nestingScope
            cs = p
            encDec = Some internalItem
            elemDecodeFn = None
            ixVariable = i
        }
        let introSnap = nestingScope.nestingLevel = 0I
        let auxiliaries, callAux = lm.lg.generateSequenceOfLikeAuxiliaries r ACN (StrType o) sqfProofGen codec

        let funcBodyContent, localVariables =
            match o.minSize with
            | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper  ->
                str_FixedSize pp typeDefinitionName i internalItem o.minSize.uper nBits nBits 0I initExpr introSnap callAux codec, charIndex@nStringLength
            | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper  ->
                str_VarSize pp (p.accessPath.joined lm.lg) typeDefinitionName i internalItem o.minSize.uper o.maxSize.uper nSizeInBits nBits nBits 0I initExpr callAux typeDefinitionName codec, charIndex@nStringLength
            | _                                                ->
                let funcBodyContent,localVariables = DAstUPer.handleFragmentation lm p codec errCode ii o.uperMaxSizeInBits o.minSize.uper o.maxSize.uper internalItem nBits false true
                funcBodyContent,charIndex@localVariables

        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = lv::localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries}


    AcnPrimitiveFactory.createAcnOnlyPrimitive lm codec typeId us (fun errCode ->
        fun (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) ->
            let td = (lm.lg.getStrTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
            let pp, resultExpr = adaptArgument lm codec p
            let sType = (ToC typeId.dropModule.AsString)
            let funcBodyContent =
                match t.str.acnEncodingClass with
                | Acn_Enc_String_uPER_Ascii    _                                    ->
                    match t.str.maxSize.uper = t.str.minSize.uper with
                    | true      ->  Some (Acn_String_Ascii_FixSize pp errCode.errCodeName ( t.str.maxSize.uper) codec, [], [], [])
                    | false     ->
                        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                        Some (Acn_String_Ascii_Internal_Field_Determinant pp errCode.errCodeName ( t.str.maxSize.acn) ( t.str.minSize.acn) nSizeInBits sType codec , [], [], [])
                | Acn_Enc_String_Ascii_Null_Terminated (_, nullChars)   -> Some (Acn_String_Ascii_Null_Terminated pp errCode.errCodeName ( t.str.maxSize.acn) nullChars (ToC typeId.dropModule.AsString) codec, [], [], [])
                | Acn_Enc_String_Ascii_External_Field_Determinant       _    ->
                    let extField = getExternalField lm r deps typeId
                    Some(Acn_String_Ascii_External_Field_Determinant pp errCode.errCodeName ( t.str.maxSize.acn) extField sType codec, [], [], [])
                | Acn_Enc_String_CharIndex_External_Field_Determinant   _    ->
                    let extField = getExternalField lm r deps typeId
                    let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (t.str.uperCharSet.Length-1))
                    let encDecStatement =
                        match t.str.uperCharSet.Length = 128 with
                        | false ->
                            let arrAsciiCodes = t.str.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                            Acn_String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( t.str.maxSize.acn) arrAsciiCodes (BigInteger t.str.uperCharSet.Length) extField td nBits sType codec
                        | true  -> Acn_IA5String_CharIndex_External_Field_Determinant pp errCode.errCodeName t.str.maxSize.acn extField td nBits (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) sType codec
                    Some(encDecStatement, [], [], [])
                | Acn_Enc_String_uPER    _                                         ->
                    let x = uper_funcBody errCode nestingScope p
                    Some(x.funcBody, x.errCodes, x.localVariables, x.auxiliaries)
                | Acn_Enc_String_Ascii_Deduced _ ->
                    Some (lm.acn.str_ascii_deduced pp errCode.errCodeName t.str.maxSize.acn nestingScope.deducedTrailingBits sType codec, [], [], [])
            match funcBodyContent with
            | None -> None
            | Some (funcBodyContent,errCodes, lvs, auxiliaries) ->
                // ACN-inserted children referencing an IA5String TAS keep that
                // name on the ICD row ("IA5String (DeviceName)", roadmap A4).
                let icd =
                    AcnPrimitiveFactory.buildStringIcdAux t.acnAlignment o "IA5String" "IA5String" (constraintsToIcdStr (o.AllCons |> List.map DAstAsn1.foldStringCon)) None
                    |> icdAuxAddNamedTypeSuffix (Some t.tasName.Value)
                Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::errCodes |> List.distinct ; localVariables = lvs; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd}))
