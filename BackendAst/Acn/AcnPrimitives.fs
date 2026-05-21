module AcnPrimitives

open System.Numerics
open System.Globalization

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language

open AcnHelpers


type AcnIntegerFuncBody = ErrorCode -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CodegenScope -> (AcnFuncBodyResult option)

let createAcnIntegerFunctionInternal (r:Asn1AcnAst.AstRoot)
                                     (lm:LanguageMacros)
                                     (codec:CommonTypes.Codec)
                                     (uperRange : BigIntegerUperRange)
                                     (intClass:Asn1AcnAst.IntegerClass)
                                     (acnEncodingClass: IntEncodingClass)
                                     (uperfuncBody : ErrorCode -> NestingScope -> CodegenScope -> bool -> (UPERFuncBodyResult option))
                                     (sAsn1Constraints:string option)
                                     acnMinSizeInBits
                                     acnMaxSizeInBits
                                     unitsOfMeasure
                                     (typeName:string)
                                     (soMF:string option, soMFM:string option)
                                     (sType:string): AcnIntegerFuncBody =
    let PositiveInteger_ConstSize_8                  = lm.acn.PositiveInteger_ConstSize_8
    let PositiveInteger_ConstSize_big_endian_16      = lm.acn.PositiveInteger_ConstSize_big_endian_16
    let PositiveInteger_ConstSize_little_endian_16   = lm.acn.PositiveInteger_ConstSize_little_endian_16
    let PositiveInteger_ConstSize_big_endian_32      = lm.acn.PositiveInteger_ConstSize_big_endian_32
    let PositiveInteger_ConstSize_little_endian_32   = lm.acn.PositiveInteger_ConstSize_little_endian_32
    let PositiveInteger_ConstSize_big_endian_64      = lm.acn.PositiveInteger_ConstSize_big_endian_64
    let PositiveInteger_ConstSize_little_endian_64   = lm.acn.PositiveInteger_ConstSize_little_endian_64
    let PositiveInteger_ConstSize                    = lm.acn.PositiveInteger_ConstSize
    let TwosComplement_ConstSize_8                   = lm.acn.TwosComplement_ConstSize_8
    let TwosComplement_ConstSize_big_endian_16       = lm.acn.TwosComplement_ConstSize_big_endian_16
    let TwosComplement_ConstSize_little_endian_16    = lm.acn.TwosComplement_ConstSize_little_endian_16
    let TwosComplement_ConstSize_big_endian_32       = lm.acn.TwosComplement_ConstSize_big_endian_32
    let TwosComplement_ConstSize_little_endian_32    = lm.acn.TwosComplement_ConstSize_little_endian_32
    let TwosComplement_ConstSize_big_endian_64       = lm.acn.TwosComplement_ConstSize_big_endian_64
    let TwosComplement_ConstSize_little_endian_64    = lm.acn.TwosComplement_ConstSize_little_endian_64
    let TwosComplement_ConstSize                     = lm.acn.TwosComplement_ConstSize
    let ASCII_ConstSize                              = lm.acn.ASCII_ConstSize
    let ASCII_VarSize_NullTerminated                 = lm.acn.ASCII_VarSize_NullTerminated
    //+++ todo write ada stg macros for ASCII_UINT_ConstSize, ASCII_UINT_VarSize_NullTerminated
    let ASCII_UINT_ConstSize                         = lm.acn.ASCII_UINT_ConstSize
    let ASCII_UINT_VarSize_NullTerminated            = lm.acn.ASCII_UINT_VarSize_NullTerminated
    let BCD_ConstSize                                = lm.acn.BCD_ConstSize
    let BCD_VarSize_NullTerminated                   = lm.acn.BCD_VarSize_NullTerminated
    let mappingFunctionDeclaration                   = lm.acn.MappingFunctionDeclaration

    let nUperMin, nUperMax =
        match uperRange with
        | Concrete(a,b) -> a,b
        | NegInf(b)     -> r.args.SIntMin, b
        | PosInf(a)     -> a, r.args.IntMax (a>=0I)
        | Full          -> r.args.SIntMin, r.args.SIntMax

    let userDefinedFunctions =
        match soMF with
        | None -> []
        | Some s ->
            [UserMappingFunction (mappingFunctionDeclaration typeName s codec)]

    let funcBody (errCode:ErrorCode)
                 (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list)
                 (nestingScope: NestingScope)
                 (p:CodegenScope) =
        let pp, resultExpr = adaptArgument lm codec p
        let uIntActualMax (nBits:int) =
            let a = 2I**nBits - 1I
            min a nUperMax
        let sIntActualMin (nBits:int) =
            let a = -(2I**(nBits-1))
            max a nUperMin
        let sIntActualMax (nBits:int) =
            let a = 2I**(nBits-1) - 1I
            min a nUperMax
        let sSsuffix = DAstUPer.getIntDecFuncSuffix intClass
        let castPp encFuncBits = DAstUPer.castPp r lm codec pp intClass encFuncBits
        let word_size_in_bits = (int r.args.integerSizeInBytes)*8

        let funcBodyContent  =
            match acnEncodingClass with
            | Asn1AcnAst.Integer_uPER                                  -> uperfuncBody errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.bValIsUnReferenced, x.bBsIsUnReferenced)
            | Asn1AcnAst.PositiveInteger_ConstSize_8                   -> Some(PositiveInteger_ConstSize_8 (castPp 8) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 8) sType codec, [errCode], false, false)
            | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_16       -> Some(PositiveInteger_ConstSize_big_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 16) sType codec, [errCode], false, false)
            | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_16    -> Some(PositiveInteger_ConstSize_little_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 16) sType codec, [errCode], false, false)
            | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_32       -> Some(PositiveInteger_ConstSize_big_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 32) sType codec, [errCode], false, false)
            | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_32    -> Some(PositiveInteger_ConstSize_little_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 32) sType codec, [errCode], false, false)
            | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_64       -> Some(PositiveInteger_ConstSize_big_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 64) sType codec, [errCode], false, false)
            | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_64    -> Some(PositiveInteger_ConstSize_little_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 64) sType codec, [errCode], false, false)
            | Asn1AcnAst.PositiveInteger_ConstSize bitSize             -> Some(PositiveInteger_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName bitSize soMF soMFM (max 0I nUperMin) (uIntActualMax (int bitSize)) sType codec, [errCode], false, false)

            | Asn1AcnAst.TwosComplement_ConstSize_8                    -> Some(TwosComplement_ConstSize_8 (castPp 8) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 8) (sIntActualMax 8) sType codec, [errCode], false, false)
            | Asn1AcnAst.TwosComplement_ConstSize_big_endian_16        -> Some(TwosComplement_ConstSize_big_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 16) (sIntActualMax 16) sType codec, [errCode], false, false)
            | Asn1AcnAst.TwosComplement_ConstSize_little_endian_16     -> Some(TwosComplement_ConstSize_little_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 16) (sIntActualMax 16) sType codec, [errCode], false, false)
            | Asn1AcnAst.TwosComplement_ConstSize_big_endian_32        -> Some(TwosComplement_ConstSize_big_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 32) (sIntActualMax 32) sType codec, [errCode], false, false)
            | Asn1AcnAst.TwosComplement_ConstSize_little_endian_32     -> Some(TwosComplement_ConstSize_little_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 32) (sIntActualMax 32) sType codec, [errCode], false, false)
            | Asn1AcnAst.TwosComplement_ConstSize_big_endian_64        -> Some(TwosComplement_ConstSize_big_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 64) (sIntActualMax 64) sType codec, [errCode], false, false)
            | Asn1AcnAst.TwosComplement_ConstSize_little_endian_64     -> Some(TwosComplement_ConstSize_little_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 64) (sIntActualMax 64) sType codec, [errCode], false, false)
            | Asn1AcnAst.TwosComplement_ConstSize bitSize              -> Some(TwosComplement_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM bitSize (sIntActualMin (int bitSize)) (sIntActualMax (int bitSize)) sType codec, [errCode], false, false)

            | Asn1AcnAst.ASCII_ConstSize size                          -> Some(ASCII_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax ((size)/8I) sType codec, [errCode], false, false)
            | Asn1AcnAst.ASCII_VarSize_NullTerminated nullBytes        -> Some(ASCII_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax nullBytes sType codec, [errCode], false, false)
            | Asn1AcnAst.ASCII_UINT_ConstSize size                     -> Some(ASCII_UINT_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax ((size)/8I) sType codec, [errCode], false, false)
            | Asn1AcnAst.ASCII_UINT_VarSize_NullTerminated nullBytes   -> Some(ASCII_UINT_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax nullBytes sType codec, [errCode], false, false)
            | Asn1AcnAst.BCD_ConstSize size                            -> Some(BCD_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax ((size)/4I) sType codec, [errCode], false, false)
            | Asn1AcnAst.BCD_VarSize_NullTerminated _                  -> Some(BCD_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax sType codec, [errCode], false, false)

        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, bValIsUnReferenced, bBsIsUnReferenced) ->
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType "INTEGER"); sConstraint=sAsn1Constraints; minLengthInBits = acnMinSizeInBits ;maxLengthInBits=acnMaxSizeInBits;sUnits=unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = "INTEGER"; rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsUnReferenced= bValIsUnReferenced; bBsIsUnReferenced=bBsIsUnReferenced; resultExpr = resultExpr; auxiliaries = []; userDefinedFunctions=userDefinedFunctions; icdResult=Some icd})
    funcBody

let getMappingFunctionModule (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (soMapFuncName:string option) =
    match lm.lg.hasModules with
    | false     -> None
    | true   ->
        match soMapFuncName with
        | None  -> None
        | Some sMapFuncName ->
            let knownMappingFunctions = ["milbus"]
            match knownMappingFunctions |> Seq.exists ((=) sMapFuncName) with
            | true  -> Some (acn_a.rtlModuleName() )
            | false -> r.args.mappingFunctionsModule

let createAcnIntegerFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnInteger) (typeName:string)  (us:State) (sType:string) =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elm")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath

    let uperFuncBody (errCode) (nestingScope: NestingScope) (p:CodegenScope) (fromACN: bool) =
        let acnPrimitiveTypeDef = ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = "int"; definedInRtl = false}
        DAstUPer.getIntfuncBodyByCons r lm codec t.uperRange t.Location (getAcnIntegerClass r.args t) (t.cons) (t.cons@t.withcons) typeId acnPrimitiveTypeDef errCode nestingScope p
    let soMapFunMod, soMapFunc  =
        match t.acnProperties.mappingFunction with
        | Some (MappingFunction (soMapFunMod, mapFncName))    ->
            let soMapFunMod, soMapFunc  =  soMapFunMod,  Some mapFncName.Value
            match soMapFunMod with
            | None  -> getMappingFunctionModule r lm soMapFunc, soMapFunc
            | Some soMapFunMod   -> Some soMapFunMod.Value, soMapFunc
        | None -> None, None

    let sAsn1Constraints = None
    let unitsOfMeasure = None

    let funcBody = createAcnIntegerFunctionInternal r lm codec t.uperRange t.intClass t.acnEncodingClass uperFuncBody sAsn1Constraints t.acnMinSizeInBits t.acnMaxSizeInBits unitsOfMeasure typeName (soMapFunc, soMapFunMod) sType
    (funcBody errCode), ns


let createIntegerFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let sAsn1Constraints =
        let sTmpCons = o.AllCons |> List.map (DastValidate2.printRangeConAsAsn1 (fun z -> z.ToString())) |> Seq.StrJoin ""
        match sTmpCons.Trim() with
        | "" -> None
        | _  -> Some sTmpCons

    let typeName = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
    let soMapFunMod, soMapFunc  =
        match o.acnProperties.mappingFunction with
        | Some (MappingFunction (soMapFunMod, mapFncName))    ->
            let soMapFunMod, soMapFunc  =  soMapFunMod,  Some mapFncName.Value
            match soMapFunMod with
            | None  -> getMappingFunctionModule r lm soMapFunc, soMapFunc
            | Some soMapFunMod   -> Some soMapFunMod.Value, soMapFunc
        | None -> None, None
    let funcBodyOrig = createAcnIntegerFunctionInternal r lm codec o.uperRange o.intClass o.acnEncodingClass uperFunc.funcBody_e  sAsn1Constraints t.acnMinSizeInBits t.acnMaxSizeInBits t.unitsOfMeasure typeName (soMapFunc, soMapFunMod) typeName
    let funcBody (errCode: ErrorCode)
                 (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list)
                 (nestingScope: NestingScope)
                 (p: CodegenScope) =
        let res = funcBodyOrig errCode acnArgs nestingScope p
        res |> Option.map (fun res ->
            let aux = lm.lg.generateIntegerAuxiliaries r ACN t o nestingScope p.accessPath codec
            {res with auxiliaries = res.auxiliaries @ aux})

    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] us


let createRealFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Real_32_big_endian                  = lm.acn.Real_32_big_endian
    let Real_64_big_endian                  = lm.acn.Real_64_big_endian
    let Real_32_little_endian               = lm.acn.Real_32_little_endian
    let Real_64_little_endian               = lm.acn.Real_64_little_endian

    let cls = o.getClass r.args
    let sSuffix = lm.lg.getRealEncodingSuffix r.args.floatingPointSizeInBytes cls

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let pp, resultExpr = adaptArgument lm codec p
        let castPp = DAstUPer.castRPp lm codec (o.getClass r.args) pp
        let sType = lm.lg.getLongTypedefNameBasedOnModule (lm.lg.getTypeDefinition t.FT_TypeDefinition) p.modName

        let funcBodyContent =
            match o.acnEncodingClass with
            | Real_IEEE754_32_big_endian            -> Some (Real_32_big_endian castPp sSuffix errCode.errCodeName sType codec, [errCode], [])
            | Real_IEEE754_64_big_endian            -> Some (Real_64_big_endian pp errCode.errCodeName sType codec, [errCode], [])
            | Real_IEEE754_32_little_endian         -> Some (Real_32_little_endian castPp sSuffix errCode.errCodeName sType codec, [errCode], [])
            | Real_IEEE754_64_little_endian         -> Some (Real_64_little_endian pp errCode.errCodeName sType codec, [errCode], [])
            | Real_uPER                             -> uperFunc.funcBody_e errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.auxiliaries)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, auxiliaries) ->
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}

            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult=Some icd})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    let annots =
        match ProgrammingLanguage.ActiveLanguages.Head with
        | Scala -> ["extern"]
        | _ -> []
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations annots us


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let funcBodyContent =
            uperFunc.funcBody_e errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.resultExpr, x.auxiliaries)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, resultExpr, auxiliaries) ->
            let icdFnc fieldName sPresent comments  =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] us


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let funcBodyContent =
            uperFunc.funcBody_e errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.resultExpr, x.auxiliaries)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, resultExpr, auxiliaries) ->
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None;}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] us


let nestChildItems (lm:LanguageMacros) (codec:CommonTypes.Codec) children =
    DAstUtilFunctions.nestItems lm.isvalid.JoinItems2 children


let createAcnBooleanFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnBoolean)  (us:State)  =
    AcnPrimitiveFactory.createAcnOnlyPrimitive codec typeId us (fun errCode ->
        fun (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) ->
            let pp, resultExpr = adaptArgument lm codec p
            let Boolean         = lm.uper.Boolean
            let sType = ""
            let funcBodyContent = Boolean pp errCode.errCodeName sType codec
            let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux "BOOLEAN" "BOOLEAN" None o.acnMinSizeInBits o.acnMaxSizeInBits None
            Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd})

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionOrReference) (baseTypeUperFunc : AcnFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let Boolean         = lm.uper.Boolean
        let acnBoolean      = lm.acn.Boolean
        let BooleanTrueFalse = lm.acn.BooleanTrueFalse

        let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName

        let funcBodyContent, resultExpr=
            let pvalue, ptr, resultExpr =
                match codec, lm.lg.decodingKind with
                | Decode, Copy ->
                    let resExpr = p.accessPath.asIdentifier lm.lg
                    resExpr, resExpr, Some resExpr
                | _ -> lm.lg.getValue p.accessPath, lm.lg.getPointer p.accessPath, None
            match o.acnProperties.encodingPattern with
            | None ->
                let pp, resultExpr = adaptArgument lm codec p
                Boolean pp errCode.errCodeName sType codec, resultExpr
            | Some (TrueValueEncoding pattern)  ->
                let arrBits = pattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrTrueValueAsByteArray = bitStringValueToByteArray pattern
                let arrFalseValueAsByteArray = arrTrueValueAsByteArray |> Array.map (~~~)
                let nSize = pattern.Value.Length
                acnBoolean pvalue ptr true (BigInteger nSize) arrTrueValueAsByteArray arrFalseValueAsByteArray arrBits errCode.errCodeName sType codec, resultExpr
            | Some (FalseValueEncoding pattern) ->
                let arrBits = pattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrFalseValueAsByteArray = bitStringValueToByteArray pattern
                let arrTrueValueAsByteArray = arrFalseValueAsByteArray |> Array.map (~~~)
                let nSize = pattern.Value.Length
                acnBoolean pvalue ptr false (BigInteger nSize) arrTrueValueAsByteArray arrFalseValueAsByteArray arrBits errCode.errCodeName sType codec, resultExpr
            | Some (TrueFalseValueEncoding(trPattern, fvPatten)) ->
                let arrTrueBits = trPattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrFalseBits = fvPatten.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrTrueValueAsByteArray = bitStringValueToByteArray trPattern
                let arrFalseValueAsByteArray = bitStringValueToByteArray fvPatten
                let nSize = trPattern.Value.Length
                BooleanTrueFalse pvalue ptr (BigInteger nSize) arrTrueValueAsByteArray arrFalseValueAsByteArray arrTrueBits arrFalseBits errCode.errCodeName sType codec, resultExpr
        let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux (getASN1Name t) (getASN1Name t) None o.acnMinSizeInBits o.acnMaxSizeInBits t.unitsOfMeasure
        let aux = lm.lg.generateBooleanAuxiliaries r ACN t o nestingScope p.accessPath codec
        Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=aux; icdResult = Some icd}
    AcnPrimitiveFactory.createAsn1Primitive r deps lm codec t typeDefinition isValidFunc [] us funcBody


let createAcnNullTypeFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnNullType)  (us:State)  =
    AcnPrimitiveFactory.createAcnOnlyPrimitive codec typeId us (fun errCode ->
        fun (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) ->
            let pp, resultExpr = adaptArgument lm codec p
            let nullType         = lm.acn.Null_pattern2
            match o.acnProperties.encodingPattern with
            | None      -> None
            | Some encPattern   ->
                let arrsBits, arrBytes, nBitsSize, icdDesc =
                    match encPattern with
                    | PATTERN_PROP_BITSTR_VALUE bitStringPattern ->
                        let arrsBits = bitStringPattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                        let arrBytes = bitStringValueToByteArray bitStringPattern
                        let icdDesc = sprintf "fixed pattern: '%s'B" bitStringPattern.Value
                        arrsBits, arrBytes, (BigInteger bitStringPattern.Value.Length), icdDesc
                    | PATTERN_PROP_OCTSTR_VALUE octStringBytes   ->
                        let arrBytes = octStringBytes |> Seq.map(fun z -> z.Value) |> Seq.toArray
                        let bitStringPattern = byteArrayToBitStringValue arrBytes
                        let arrsBits = bitStringPattern.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                        let icdDesc = sprintf "fixed pattern:  '%s'H" (arrBytes |> Seq.map(fun z -> z.ToString("X2")) |> Seq.StrJoin "")
                        arrsBits,arrBytes,(BigInteger bitStringPattern.Length), icdDesc
                let ret = nullType pp arrBytes nBitsSize arrsBits errCode.errCodeName o.acnProperties.savePosition codec
                let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux icdDesc "NULL" None o.acnMinSizeInBits o.acnMaxSizeInBits None
                Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd}))

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let pp, resultExpr = adaptArgument lm codec p
        let nullType         = lm.acn.Null_pattern
        let aux = lm.lg.generateNullTypeAuxiliaries r ACN t o nestingScope p.accessPath codec
        let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName

        match o.acnProperties.encodingPattern with
        | None ->
            match codec, lm.lg.decodingKind with
            | Decode, Copy ->
                // Copy-decoding backend expect all values to be declared even if they are "dummies"
                Some ({AcnFuncBodyResult.funcBody = lm.acn.Null_declare pp sType; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=Some pp; auxiliaries=aux; icdResult=None})
            | _ -> None
        | Some encPattern   ->
            let arrsBits, arrBytes, nBitsSize, icdDesc =
                match encPattern with
                | PATTERN_PROP_BITSTR_VALUE bitStringPattern ->
                    let arrsBits = bitStringPattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                    let arrBytes = bitStringValueToByteArray bitStringPattern
                    let icdDesc = sprintf "fixed pattern: '%s'B" bitStringPattern.Value
                    arrsBits, arrBytes, (BigInteger bitStringPattern.Value.Length), icdDesc
                | PATTERN_PROP_OCTSTR_VALUE octStringBytes   ->
                    let arrBytes = octStringBytes |> Seq.map(fun z -> z.Value) |> Seq.toArray
                    let bitStringPattern = byteArrayToBitStringValue arrBytes
                    let arrsBits = bitStringPattern.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                    let icdDesc = sprintf "fixed pattern:  '%s'H" (arrBytes |> Seq.map(fun z -> z.ToString("X2")) |> Seq.StrJoin "")
                    arrsBits,arrBytes,(BigInteger bitStringPattern.Length), icdDesc
            let ret = nullType pp arrBytes nBitsSize arrsBits errCode.errCodeName o.acnProperties.savePosition codec
            let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux icdDesc (getASN1Name t) None o.acnMinSizeInBits o.acnMaxSizeInBits t.unitsOfMeasure
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= lm.lg.acn.null_valIsUnReferenced; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=aux; icdResult = Some icd})
    AcnPrimitiveFactory.createAsn1Primitive r deps lm codec t typeDefinition isValidFunc [] us funcBody
