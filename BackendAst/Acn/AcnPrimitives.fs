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

/// The integer type of the RTL that the encoding of an ACN integer reads and writes (Ada: Asn1UInt / Asn1Int) and its
/// range: the type of the value the mapping-functions of the field take and return (q/issues.md Issue 58). None for a
/// uPER encoding, which applies no mapping-function.
let mappingRtlType (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (acnEncodingClass: IntEncodingClass) =
    match acnEncodingClass with
    | Asn1AcnAst.Integer_uPER                               -> None
    | Asn1AcnAst.PositiveInteger_ConstSize_8
    | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_16
    | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_16
    | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_32
    | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_32
    | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_64
    | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_64
    | Asn1AcnAst.PositiveInteger_ConstSize _
    | Asn1AcnAst.ASCII_UINT_ConstSize _
    | Asn1AcnAst.ASCII_UINT_VarSize_NullTerminated _
    | Asn1AcnAst.BCD_ConstSize _
    | Asn1AcnAst.BCD_VarSize_NullTerminated _               -> Some (lm.typeDef.Declare_PosInteger (), 0I, r.args.UIntMax)
    | Asn1AcnAst.TwosComplement_ConstSize_8
    | Asn1AcnAst.TwosComplement_ConstSize_big_endian_16
    | Asn1AcnAst.TwosComplement_ConstSize_little_endian_16
    | Asn1AcnAst.TwosComplement_ConstSize_big_endian_32
    | Asn1AcnAst.TwosComplement_ConstSize_little_endian_32
    | Asn1AcnAst.TwosComplement_ConstSize_big_endian_64
    | Asn1AcnAst.TwosComplement_ConstSize_little_endian_64
    | Asn1AcnAst.TwosComplement_ConstSize _
    | Asn1AcnAst.ASCII_ConstSize _
    | Asn1AcnAst.ASCII_VarSize_NullTerminated _             -> Some (lm.typeDef.Declare_Integer (), r.args.SIntMin, r.args.SIntMax)

let createAcnIntegerFunctionInternal (r:Asn1AcnAst.AstRoot)
                                     (lm:LanguageMacros)
                                     (codec:CommonTypes.Codec)
                                     (uperRange : BigIntegerUperRange)
                                     (intClass:Asn1AcnAst.IntegerClass)
                                     (acnEncodingClass: IntEncodingClass)
                                     (fullRangeUse: DecodeOutcome.FullRangeUse)
                                     (uperfuncBody : ErrorCode -> NestingScope -> CodegenScope -> bool -> (UPERFuncBodyResult option))
                                     (sAsn1Constraints:string option)
                                     (acnAlignment: AcnGenericTypes.AcnAlignment option)
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
        // q/issues.md Issue 58: a language that decodes a mapped integer through a temporary (Ada) reads the wire value
        // into it with the macro of the encoding without mapping and the bounds of the RTL integer type (the wire value
        // need not be a value of the field); acn_int_mapped_read then maps it and checks the field's range
        let mappedTmp =
            match codec, soMF, lm.lg.acnMappedIntDecodeViaTemporary with
            | Decode, Some sMF, true    -> mappingRtlType r lm acnEncodingClass |> Option.map (fun rtlType -> sMF, rtlType)
            | Decode, _, _
            | Encode, _, _              -> None
        // what the macro of the encoding reads: the field, or the temporary
        let rdPp, rdSuffix, rdMF, rdMFM, rdMin, rdMax =
            match mappedTmp with
            | None                                  -> pp, lm.lg.getIntDecFuncSuffix intClass, soMF, soMFM, nUperMin, nUperMax
            | Some (_, (_, typeMin, typeMax))       -> "mapFuncIntVal", "", None, None, typeMin, typeMax
        let uIntActualMax (nBits:int) =
            let a = 2I**nBits - 1I
            min a rdMax
        let sIntActualMin (nBits:int) =
            let a = -(2I**(nBits-1))
            max a rdMin
        let sIntActualMax (nBits:int) =
            let a = 2I**(nBits-1) - 1I
            min a rdMax
        let sSsuffix = rdSuffix
        let castPp encFuncBits = DAstUPer.castPp r lm codec rdPp intClass encFuncBits
        let word_size_in_bits = (int r.args.integerSizeInBytes)*8
        // coverage step 12b: the ACN integer encodings are RTL procedure calls
        let rtlWritesOnly = encodeOutcomeOf codec (lm.lg.encodeTemplateOutcome EncRtlWritesOnly)

        // Helpers that absorb the common argument shape of the integer macros.
        // The macros fall into 6 shapes; collapsing them here turns each match
        // arm into a one-line lookup.  The match itself stays exhaustive so
        // the F# compiler still catches a missing case on future DU additions.
        let unsignedFixedWidth macro width =
            Some (macro (castPp width) sSsuffix errCode.errCodeName rdMF rdMFM (max 0I rdMin) (uIntActualMax width) sType codec, [errCode], false, false, rtlWritesOnly)
        let signedFixedWidth macro width =
            Some (macro (castPp width) sSsuffix errCode.errCodeName rdMF rdMFM (sIntActualMin width) (sIntActualMax width) sType codec, [errCode], false, false, rtlWritesOnly)
        let unsignedVarWidth (bitSize: BigInteger) =
            Some (PositiveInteger_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName bitSize rdMF rdMFM (max 0I rdMin) (uIntActualMax (int bitSize)) sType codec, [errCode], false, false, rtlWritesOnly)
        let signedVarWidth (bitSize: BigInteger) =
            Some (TwosComplement_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName rdMF rdMFM bitSize (sIntActualMin (int bitSize)) (sIntActualMax (int bitSize)) sType codec, [errCode], false, false, rtlWritesOnly)
        let asciiOrBcdConst macro (size: BigInteger) bitsPerDigit =
            Some (macro (castPp word_size_in_bits) sSsuffix errCode.errCodeName rdMF rdMFM rdMin rdMax (size / bitsPerDigit) sType codec, [errCode], false, false, rtlWritesOnly)
        let asciiVarSize macro (nullBytes: byte list) =
            Some (macro (castPp word_size_in_bits) sSsuffix errCode.errCodeName rdMF rdMFM rdMin rdMax nullBytes sType codec, [errCode], false, false, rtlWritesOnly)
        let bcdVarSize () =
            Some (BCD_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName rdMF rdMFM rdMin rdMax sType codec, [errCode], false, false, rtlWritesOnly)

        // coverage step 10b: an ASN.1 INTEGER whose field accepts every bit pattern is decoded with the RTL _FullRange
        // twin (no Result; DecodeOutcome.acnIntegerFullRange); Integer_uPER decides in the uPER body
        let fullRange =
            match codec, fullRangeUse with
            | Decode, DecodeOutcome.UseFullRange    -> DecodeOutcome.acnIntegerFullRange r lm uperRange intClass acnEncodingClass soMF.IsSome
            | Decode, DecodeOutcome.KeepCheckedCall
            | Encode, _                             -> None
        let funcBodyContent  =
           match fullRange with
           | Some fr -> Some (lm.acn.AcnIntFullRange pp fr.sKind fr.sWidth sSsuffix fr.noBits, [errCode], false, false, rtlWritesOnly)
           | None ->
           match acnEncodingClass with
            | Asn1AcnAst.Integer_uPER                                  -> uperfuncBody errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.bValIsUnReferenced, x.bBsIsUnReferenced, x.encOutcome)
            | Asn1AcnAst.PositiveInteger_ConstSize_8                   -> unsignedFixedWidth PositiveInteger_ConstSize_8                  8
            | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_16       -> unsignedFixedWidth PositiveInteger_ConstSize_big_endian_16     16
            | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_16    -> unsignedFixedWidth PositiveInteger_ConstSize_little_endian_16  16
            | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_32       -> unsignedFixedWidth PositiveInteger_ConstSize_big_endian_32     32
            | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_32    -> unsignedFixedWidth PositiveInteger_ConstSize_little_endian_32  32
            | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_64       -> unsignedFixedWidth PositiveInteger_ConstSize_big_endian_64     64
            | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_64    -> unsignedFixedWidth PositiveInteger_ConstSize_little_endian_64  64
            | Asn1AcnAst.PositiveInteger_ConstSize bitSize             -> unsignedVarWidth bitSize

            | Asn1AcnAst.TwosComplement_ConstSize_8                    -> signedFixedWidth TwosComplement_ConstSize_8                     8
            | Asn1AcnAst.TwosComplement_ConstSize_big_endian_16        -> signedFixedWidth TwosComplement_ConstSize_big_endian_16        16
            | Asn1AcnAst.TwosComplement_ConstSize_little_endian_16     -> signedFixedWidth TwosComplement_ConstSize_little_endian_16     16
            | Asn1AcnAst.TwosComplement_ConstSize_big_endian_32        -> signedFixedWidth TwosComplement_ConstSize_big_endian_32        32
            | Asn1AcnAst.TwosComplement_ConstSize_little_endian_32     -> signedFixedWidth TwosComplement_ConstSize_little_endian_32     32
            | Asn1AcnAst.TwosComplement_ConstSize_big_endian_64        -> signedFixedWidth TwosComplement_ConstSize_big_endian_64        64
            | Asn1AcnAst.TwosComplement_ConstSize_little_endian_64     -> signedFixedWidth TwosComplement_ConstSize_little_endian_64     64
            | Asn1AcnAst.TwosComplement_ConstSize bitSize              -> signedVarWidth bitSize

            | Asn1AcnAst.ASCII_ConstSize size                          -> asciiOrBcdConst ASCII_ConstSize        size 8I
            | Asn1AcnAst.ASCII_VarSize_NullTerminated nullBytes        -> asciiVarSize    ASCII_VarSize_NullTerminated      nullBytes
            | Asn1AcnAst.ASCII_UINT_ConstSize size                     -> asciiOrBcdConst ASCII_UINT_ConstSize   size 8I
            | Asn1AcnAst.ASCII_UINT_VarSize_NullTerminated nullBytes   -> asciiVarSize    ASCII_UINT_VarSize_NullTerminated nullBytes
            | Asn1AcnAst.BCD_ConstSize size                            -> asciiOrBcdConst BCD_ConstSize          size 4I
            | Asn1AcnAst.BCD_VarSize_NullTerminated _                  -> bcdVarSize ()
           
        // the temporary mapped and checked against the range of the field (only the bounds narrower than its type)
        let funcBodyContent =
            match mappedTmp, funcBodyContent with
            | Some (sMF, (sTmpType, typeMin, typeMax)), Some (wireRead, errCodes, _, bBsIsUnReferenced, encOutcome) ->
                let noMin = if nUperMin > typeMin then Some nUperMin else None
                let noMax = if nUperMax < typeMax then Some nUperMax else None
                Some (lm.acn.acn_int_mapped_read pp wireRead sMF soMFM sTmpType noMin noMax (max typeMin nUperMin), errCodes, false, bBsIsUnReferenced, encOutcome)
            | Some _, None
            | None, _ -> funcBodyContent
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, bValIsUnReferenced, bBsIsUnReferenced, encOutcome) ->
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType "INTEGER"); sConstraint=sAsn1Constraints; minLengthInBits = acnMinSizeInBits ;maxLengthInBits=icdMaxSizeWithoutAlignment acnAlignment acnMaxSizeInBits;sUnits=unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = "INTEGER"; rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsUnReferenced= bValIsUnReferenced; bBsIsUnReferenced=bBsIsUnReferenced; resultExpr = resultExpr; auxiliaries = []; userDefinedFunctions=userDefinedFunctions; icdResult=Some icd; encOutcome = encodeOutcomeOf codec encOutcome})
    funcBody

/// The module of a mapping-function written without one in the ACN: -mfm. A language that calls the function through
/// its module (Ada) cannot do without one (q/issues.md Issue 58).
let getMappingFunctionModule (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (mapFuncName:StringLoc) =
    match lm.lg.hasModules with
    | false     -> None
    | true   ->
        let knownMappingFunctions = ["milbus"]
        match knownMappingFunctions |> Seq.exists ((=) mapFuncName.Value), r.args.mappingFunctionsModule, lm.lg.mappingFunctionsNeedModule with
        | true, _, _                -> Some (acn_a.rtlModuleName() )
        | false, Some mfm, _        -> Some mfm
        | false, None, false        -> None
        | false, None, true         ->
            raise (SemanticError (mapFuncName.Location,
                    sprintf "mapping-function '%s' names no module, which the Ada code needs: write 'mapping-function <Package>.%s' or give the package with -mfm <Package> (--mapping-functions-module)." mapFuncName.Value mapFuncName.Value))

/// The name and the module of a mapping-function (soMF, soMFM).
let getMappingFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (mappingFunction: MappingFunction option) =
    match mappingFunction with
    | Some (MappingFunction (Some mapFunMod, mapFncName))   -> Some mapFncName.Value, Some mapFunMod.Value
    | Some (MappingFunction (None, mapFncName))             -> Some mapFncName.Value, getMappingFunctionModule r lm mapFncName
    | None                                                  -> None, None

let createAcnIntegerFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnInteger) (typeName:string)  (us:State) (sType:string) =
    let errCodeName         = ToC ("ERR_ACN" + ((lm.lg.codecSuffix codec).ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elm")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath

    let uperFuncBody (errCode) (nestingScope: NestingScope) (p:CodegenScope) (fromACN: bool) =
        let acnPrimitiveTypeDef = ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = "int"; definedInRtl = false}
        DAstUPer.getIntfuncBodyByCons r lm codec t.uperRange t.Location (getAcnIntegerClass r.args t) (t.cons) (t.cons@t.withcons) typeId acnPrimitiveTypeDef errCode nestingScope p DecodeOutcome.KeepCheckedCall
    let soMapFunc, soMapFunMod = getMappingFunction r lm t.acnProperties.mappingFunction

    let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createAcnInteger (t.cons@t.withcons))
    let unitsOfMeasure = None

    let funcBody = createAcnIntegerFunctionInternal r lm codec t.uperRange t.intClass t.acnEncodingClass DecodeOutcome.KeepCheckedCall uperFuncBody sAsn1Constraints t.acnAlignment t.acnMinSizeInBits t.acnMaxSizeInBits unitsOfMeasure typeName (soMapFunc, soMapFunMod) sType
    // ACN-inserted children declared via a named TAS (e.g. "crc MyUInt8 []")
    // record the referenced TAS in inheritInfo - keep that name on the ICD row
    // ("INTEGER (MyUInt8)", roadmap A4).  ACN children bypass
    // AcnFunctionWrapper's central suffixing, so it is applied here.
    let soTasName = t.inheritInfo |> Option.map (fun ii -> ii.tasName)
    let funcBodyWithNamedType acnArgs nestingScope p =
        funcBody errCode acnArgs nestingScope p
        |> Option.map (fun res -> {res with icdResult = res.icdResult |> Option.map (icdAuxAddNamedTypeSuffix soTasName)})
    funcBodyWithNamedType, ns


let createIntegerFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let sAsn1Constraints =
        let sTmpCons = o.AllCons |> List.map (DastValidate2.printRangeConAsAsn1 (fun z -> z.ToString())) |> Seq.StrJoin ""
        match sTmpCons.Trim() with
        | "" -> None
        | _  -> Some sTmpCons

    let typeName = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
    let soMapFunc, soMapFunMod = getMappingFunction r lm o.acnProperties.mappingFunction
    let funcBodyOrig = createAcnIntegerFunctionInternal r lm codec o.uperRange o.intClass o.acnEncodingClass DecodeOutcome.UseFullRange uperFunc.funcBody_e  sAsn1Constraints t.acnAlignment t.acnMinSizeInBits t.acnMaxSizeInBits t.unitsOfMeasure typeName (soMapFunc, soMapFunMod) typeName
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


//the integer range of a scaled-integer encoding class: (intMin, intMax, isUnsigned)
let getScaledIntRange (c: Asn1AcnAst.IntEncodingClass) : BigInteger * BigInteger * bool =
    let unsignedRange (nBits:int) = 0I, BigInteger.Pow(2I, nBits) - 1I, true
    let signedRange (nBits:int) = -BigInteger.Pow(2I, nBits-1), BigInteger.Pow(2I, nBits-1) - 1I, false
    match c with
    | Asn1AcnAst.PositiveInteger_ConstSize_8                   -> unsignedRange 8
    | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_16       -> unsignedRange 16
    | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_16    -> unsignedRange 16
    | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_32       -> unsignedRange 32
    | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_32    -> unsignedRange 32
    | Asn1AcnAst.PositiveInteger_ConstSize_big_endian_64       -> unsignedRange 64
    | Asn1AcnAst.PositiveInteger_ConstSize_little_endian_64    -> unsignedRange 64
    | Asn1AcnAst.PositiveInteger_ConstSize bitSize             -> unsignedRange (int bitSize)
    | Asn1AcnAst.TwosComplement_ConstSize_8                    -> signedRange 8
    | Asn1AcnAst.TwosComplement_ConstSize_big_endian_16        -> signedRange 16
    | Asn1AcnAst.TwosComplement_ConstSize_little_endian_16     -> signedRange 16
    | Asn1AcnAst.TwosComplement_ConstSize_big_endian_32        -> signedRange 32
    | Asn1AcnAst.TwosComplement_ConstSize_little_endian_32     -> signedRange 32
    | Asn1AcnAst.TwosComplement_ConstSize_big_endian_64        -> signedRange 64
    | Asn1AcnAst.TwosComplement_ConstSize_little_endian_64     -> signedRange 64
    | Asn1AcnAst.TwosComplement_ConstSize bitSize              -> signedRange (int bitSize)
    | Asn1AcnAst.Integer_uPER
    | Asn1AcnAst.ASCII_ConstSize _
    | Asn1AcnAst.ASCII_VarSize_NullTerminated _
    | Asn1AcnAst.ASCII_UINT_ConstSize _
    | Asn1AcnAst.ASCII_UINT_VarSize_NullTerminated _
    | Asn1AcnAst.BCD_ConstSize _
    | Asn1AcnAst.BCD_VarSize_NullTerminated _                  -> raise(BugErrorException "getScaledIntRange: Real_ScaledInt can only contain a PositiveInteger_ConstSize* or TwosComplement_ConstSize* class")

//the linear mapping parameters of a scaled-integer encoding: (low, scale, intMin, intMax, isUnsigned)
let getScaledIntMapping (o:Asn1AcnAst.Real) (intEncClass: Asn1AcnAst.IntEncodingClass) =
    let a, b =
        match o.uperRange with
        | Concrete (a, b) -> a, b
        | NegInf _ | PosInf _ | Full -> raise(BugErrorException "Real_ScaledInt requires a REAL type with a closed range constraint (a..b)")
    let intMin, intMax, isUnsigned = getScaledIntRange intEncClass
    let scale = (b - a) / (double (intMax - intMin))
    a, scale, intMin, intMax, isUnsigned

let createRealFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Real_32_big_endian                  = lm.acn.Real_32_big_endian
    let Real_64_big_endian                  = lm.acn.Real_64_big_endian
    let Real_32_little_endian               = lm.acn.Real_32_little_endian
    let Real_64_little_endian               = lm.acn.Real_64_little_endian
    let Real_ScaledInt_uint                 = lm.acn.Real_ScaledInt_uint
    let Real_ScaledInt_sint                 = lm.acn.Real_ScaledInt_sint

    let cls = o.getClass r.args
    let sSuffix = lm.lg.getRealEncodingSuffix r.args.floatingPointSizeInBytes cls
    
    let sAsn1Constraints =
        let sTmpCons = o.AllCons |> List.map (DastValidate2.printRangeConAsAsn1 (fun z -> z.ToString())) |> Seq.StrJoin ""
        match sTmpCons.Trim() with
        | "" -> None
        | _  -> Some sTmpCons

    let typeName = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName

    //scaled-integer encoding: convert the REAL to a temporary integer via the linear
    //mapping helpers of the RTL and delegate the bitstream work to the integer codegen,
    //mirroring the ENUMERATED pattern (AcnEnum.createEnumCommon)
    let scaledIntFuncBody (intEncClass: Asn1AcnAst.IntEncodingClass) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let low, scale, intMin, intMax, isUnsigned = getScaledIntMapping o intEncClass
        let intTypeClass =
            match isUnsigned with
            | true  -> Asn1AcnAst.ASN1SCC_UInt (intMin, intMax)
            | false -> Asn1AcnAst.ASN1SCC_Int (intMin, intMax)
        let rtlIntType = (DAstTypeDefinition.getIntegerTypeByClass lm intTypeClass)()
        let intVal = $"intVal_{ToC (p.accessPath.asIdentifier lm.lg)}"
        let localVars =
            match lm.lg.decodingKind with
            | Copy    -> []
            | InPlace -> [GenericLocalVariable {GenericLocalVariable.name = intVal; varType = rtlIntType; arrSize = None; isStatic = false; initExp = None}]
        let pVal = {CodegenScope.modName = t.id.ModName; accessPath = AccessPath.valueEmptyPath intVal}
        let uperIntStub _ _ _ _ : UPERFuncBodyResult option = raise(BugErrorException "Real_ScaledInt: unexpected uPER integer encoding class")
        let intFuncBody = createAcnIntegerFunctionInternal r lm codec (Concrete (intMin, intMax)) intTypeClass intEncClass DecodeOutcome.KeepCheckedCall uperIntStub None t.acnAlignment o.acnMinSizeInBits o.acnMaxSizeInBits None typeName (None, None) typeName
        match intFuncBody errCode acnArgs nestingScope pVal with
        | None -> None
        | Some intRes ->
            let sRealVal = lm.lg.getValue p.accessPath
            let sLow     = lm.lg.doubleValueToString low
            let sScale   = lm.lg.doubleValueToString scale
            let sIntMin  = lm.lg.intValueToString intMin intTypeClass
            let sIntMax  = lm.lg.intValueToString intMax intTypeClass
            let mainBody =
                match isUnsigned with
                | true  -> Real_ScaledInt_uint sRealVal intVal intRes.funcBody sLow sScale sIntMax errCode.errCodeName codec
                | false -> Real_ScaledInt_sint sRealVal intVal intRes.funcBody sLow sScale sIntMin sIntMax errCode.errCodeName codec
            Some (mainBody, intRes.errCodes, localVars @ intRes.localVariables, intRes.auxiliaries)

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let pp, resultExpr = adaptArgument lm codec p
        let castPp = DAstUPer.castRPp lm codec (o.getClass r.args) pp
        let sType = lm.lg.getLongTypedefNameBasedOnModule (lm.lg.getTypeDefinition t.FT_TypeDefinition) p.modName

        let funcBodyContent =
            match o.acnEncodingClass with
            | Real_IEEE754_32_big_endian            -> Some (Real_32_big_endian castPp sSuffix errCode.errCodeName sType codec, [errCode], [], [])
            | Real_IEEE754_64_big_endian            -> Some (Real_64_big_endian pp errCode.errCodeName sType codec, [errCode], [], [])
            | Real_IEEE754_32_little_endian         -> Some (Real_32_little_endian castPp sSuffix errCode.errCodeName sType codec, [errCode], [], [])
            | Real_IEEE754_64_little_endian         -> Some (Real_64_little_endian pp errCode.errCodeName sType codec, [errCode], [], [])
            | Real_uPER                             -> uperFunc.funcBody_e errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, [], x.auxiliaries)
            | Real_ScaledInt intEncClass            -> scaledIntFuncBody intEncClass errCode acnArgs nestingScope p
        // coverage step 12b: RTL procedure calls (the uPER REAL too, DAstUPer.createRealFunction)
        let encOutcome = encodeOutcomeOf codec (lm.lg.encodeTemplateOutcome EncRtlWritesOnly)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables, auxiliaries) ->
            let icdComments =
                match o.acnEncodingClass with
                | Real_ScaledInt intEncClass ->
                    let low, scale, intMin, _, _ = getScaledIntMapping o intEncClass
                    [sprintf "scaled integer encoding: real value = %s + (encoded integer - (%s)) * %s"
                        (lm.lg.doubleValueToString low) (intMin.ToString()) (lm.lg.doubleValueToString scale)]
                | Real_uPER
                | Real_IEEE754_32_big_endian | Real_IEEE754_32_little_endian
                | Real_IEEE754_64_big_endian | Real_IEEE754_64_little_endian -> []
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments@icdComments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=sAsn1Constraints; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=icdMaxSizeWithoutAlignment t.acnAlignment o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}

            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult=Some icd; encOutcome = encOutcome})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    let annots = lm.lg.funcDefAnnotations AcnEncDecFunctionType
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations annots us


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let funcBodyContent =
            uperFunc.funcBody_e errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.resultExpr, x.auxiliaries, x.encOutcome)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, resultExpr, auxiliaries, encOutcome) ->
            let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createObjectIdentifierFunction r t o)
            let icdFnc fieldName sPresent comments  =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=sAsn1Constraints; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=icdMaxSizeWithoutAlignment t.acnAlignment o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd; encOutcome = encodeOutcomeOf codec encOutcome})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] us


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let funcBodyContent =
            uperFunc.funcBody_e errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.resultExpr, x.auxiliaries, x.encOutcome)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, resultExpr, auxiliaries, encOutcome) ->
            let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createTimeTypeFunction r t o)
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=sAsn1Constraints; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=icdMaxSizeWithoutAlignment t.acnAlignment o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None;}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd; encOutcome = encodeOutcomeOf codec encOutcome})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] us


let nestChildItems (lm:LanguageMacros) (codec:CommonTypes.Codec) children =
    DAstUtilFunctions.nestItems lm.isvalid.JoinItems2 children


let createAcnBooleanFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnBoolean)  (us:State)  =
    AcnPrimitiveFactory.createAcnOnlyPrimitive lm codec typeId us (fun errCode ->
        fun (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) ->
            let pp, resultExpr = adaptArgument lm codec p
            let Boolean         = lm.uper.Boolean
            let sType = ""
            let funcBodyContent = Boolean pp errCode.errCodeName sType codec
            let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux o.acnAlignment "BOOLEAN" "BOOLEAN" None o.acnMinSizeInBits o.acnMaxSizeInBits None
            Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd; encOutcome = encodeOutcomeOf codec (lm.lg.encodeTemplateOutcome EncRtlWritesOnly)})

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
                // coverage step 10b: a BOOLEAN cannot fail and leaves the error code alone (DecodeOutcome.fs)
                match codec, lm.lg.decodeReadFailureModel with
                | Decode, ReadsCannotFail -> lm.uper.BooleanNoErrCode pp, resultExpr
                | Decode, ReadsCanFail
                | Encode, _               -> Boolean pp errCode.errCodeName sType codec, resultExpr
            | Some (TrueValueEncoding pattern)  ->
                let arrBits = pattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrTrueValueAsByteArray = bitStringValueToByteArray pattern
                let arrFalseValueAsByteArray = arrTrueValueAsByteArray |> Array.map (~~~)
                let nSize = pattern.Value.Length
                // coverage step 10b: the pattern call cannot fail and sets result: no error code (DecodeOutcome.fs)
                match codec, lm.lg.decodeReadFailureModel with
                | Decode, ReadsCannotFail -> lm.acn.BooleanPatternNoErrCode pvalue true arrBits, resultExpr
                | Decode, ReadsCanFail
                | Encode, _               -> acnBoolean pvalue ptr true (BigInteger nSize) arrTrueValueAsByteArray arrFalseValueAsByteArray arrBits errCode.errCodeName sType codec, resultExpr
            | Some (FalseValueEncoding pattern) ->
                let arrBits = pattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrFalseValueAsByteArray = bitStringValueToByteArray pattern
                let arrTrueValueAsByteArray = arrFalseValueAsByteArray |> Array.map (~~~)
                let nSize = pattern.Value.Length
                // coverage step 10b: the pattern call cannot fail and sets result: no error code (DecodeOutcome.fs)
                match codec, lm.lg.decodeReadFailureModel with
                | Decode, ReadsCannotFail -> lm.acn.BooleanPatternNoErrCode pvalue false arrBits, resultExpr
                | Decode, ReadsCanFail
                | Encode, _               -> acnBoolean pvalue ptr false (BigInteger nSize) arrTrueValueAsByteArray arrFalseValueAsByteArray arrBits errCode.errCodeName sType codec, resultExpr
            | Some (TrueFalseValueEncoding(trPattern, fvPatten)) ->
                let arrTrueBits = trPattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrFalseBits = fvPatten.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrTrueValueAsByteArray = bitStringValueToByteArray trPattern
                let arrFalseValueAsByteArray = bitStringValueToByteArray fvPatten
                let nSize = trPattern.Value.Length
                BooleanTrueFalse pvalue ptr (BigInteger nSize) arrTrueValueAsByteArray arrFalseValueAsByteArray arrTrueBits arrFalseBits errCode.errCodeName sType codec, resultExpr
        let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createBoolFunction r t o)
        let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux t.acnAlignment (getASN1Name t) (getASN1Name t) sAsn1Constraints o.acnMinSizeInBits o.acnMaxSizeInBits t.unitsOfMeasure
        let aux = lm.lg.generateBooleanAuxiliaries r ACN t o nestingScope p.accessPath codec
        Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=aux; icdResult = Some icd; encOutcome = encodeOutcomeOf codec (lm.lg.encodeTemplateOutcome EncRtlWritesOnly)}
    AcnPrimitiveFactory.createAsn1Primitive r deps lm codec t typeDefinition isValidFunc [] us funcBody


let createAcnNullTypeFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnNullType)  (us:State)  =
    AcnPrimitiveFactory.createAcnOnlyPrimitive lm codec typeId us (fun errCode ->
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
                let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux o.acnAlignment icdDesc "NULL" None o.acnMinSizeInBits o.acnMaxSizeInBits None
                Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd; encOutcome = encodeOutcomeOf codec (lm.lg.encodeTemplateOutcome EncRtlWritesOnly)}))

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
                Some ({AcnFuncBodyResult.funcBody = lm.acn.Null_declare pp sType; errCodes = []; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=Some pp; auxiliaries=aux; icdResult=None; encOutcome = EncodeOutcome.unknown})
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
            let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux t.acnAlignment icdDesc (getASN1Name t) None o.acnMinSizeInBits o.acnMaxSizeInBits t.unitsOfMeasure
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= lm.lg.acn.null_valIsUnReferenced; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=aux; icdResult = Some icd; encOutcome = encodeOutcomeOf codec (lm.lg.encodeTemplateOutcome EncRtlWritesOnly)})
    AcnPrimitiveFactory.createAsn1Primitive r deps lm codec t typeDefinition isValidFunc [] us funcBody
