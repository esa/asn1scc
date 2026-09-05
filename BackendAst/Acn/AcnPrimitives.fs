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

        // Helpers that absorb the common argument shape of the integer macros.
        // The macros fall into 6 shapes; collapsing them here turns each match
        // arm into a one-line lookup.  The match itself stays exhaustive so
        // the F# compiler still catches a missing case on future DU additions.
        let unsignedFixedWidth macro width =
            Some (macro (castPp width) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax width) sType codec, [errCode], false, false)
        let signedFixedWidth macro width =
            Some (macro (castPp width) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin width) (sIntActualMax width) sType codec, [errCode], false, false)
        let unsignedVarWidth (bitSize: BigInteger) =
            Some (PositiveInteger_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName bitSize soMF soMFM (max 0I nUperMin) (uIntActualMax (int bitSize)) sType codec, [errCode], false, false)
        let signedVarWidth (bitSize: BigInteger) =
            Some (TwosComplement_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM bitSize (sIntActualMin (int bitSize)) (sIntActualMax (int bitSize)) sType codec, [errCode], false, false)
        let asciiOrBcdConst macro (size: BigInteger) bitsPerDigit =
            Some (macro (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax (size / bitsPerDigit) sType codec, [errCode], false, false)
        let asciiVarSize macro (nullBytes: byte list) =
            Some (macro (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax nullBytes sType codec, [errCode], false, false)
        let bcdVarSize () =
            Some (BCD_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax sType codec, [errCode], false, false)

        let funcBodyContent  =
           match acnEncodingClass with
            | Asn1AcnAst.Integer_uPER                                  -> uperfuncBody errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.bValIsUnReferenced, x.bBsIsUnReferenced)
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
           
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, bValIsUnReferenced, bBsIsUnReferenced) ->
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType "INTEGER"); sConstraint=sAsn1Constraints; minLengthInBits = acnMinSizeInBits ;maxLengthInBits=icdMaxSizeWithoutAlignment acnAlignment acnMaxSizeInBits;sUnits=unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
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
    let errCodeName         = ToC ("ERR_ACN" + ((lm.lg.codecSuffix codec).ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
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

    let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createAcnInteger (t.cons@t.withcons))
    let unitsOfMeasure = None

    let funcBody = createAcnIntegerFunctionInternal r lm codec t.uperRange t.intClass t.acnEncodingClass uperFuncBody sAsn1Constraints t.acnAlignment t.acnMinSizeInBits t.acnMaxSizeInBits unitsOfMeasure typeName (soMapFunc, soMapFunMod) sType
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
    let soMapFunMod, soMapFunc  =
        match o.acnProperties.mappingFunction with
        | Some (MappingFunction (soMapFunMod, mapFncName))    ->
            let soMapFunMod, soMapFunc  =  soMapFunMod,  Some mapFncName.Value
            match soMapFunMod with
            | None  -> getMappingFunctionModule r lm soMapFunc, soMapFunc
            | Some soMapFunMod   -> Some soMapFunMod.Value, soMapFunc
        | None -> None, None
    let funcBodyOrig = createAcnIntegerFunctionInternal r lm codec o.uperRange o.intClass o.acnEncodingClass uperFunc.funcBody_e  sAsn1Constraints t.acnAlignment t.acnMinSizeInBits t.acnMaxSizeInBits t.unitsOfMeasure typeName (soMapFunc, soMapFunMod) typeName
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
        let intFuncBody = createAcnIntegerFunctionInternal r lm codec (Concrete (intMin, intMax)) intTypeClass intEncClass uperIntStub None t.acnAlignment o.acnMinSizeInBits o.acnMaxSizeInBits None typeName (None, None) typeName
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

            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult=Some icd})
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
            let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createObjectIdentifierFunction r t o)
            let icdFnc fieldName sPresent comments  =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=sAsn1Constraints; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=icdMaxSizeWithoutAlignment t.acnAlignment o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
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
            let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createTimeTypeFunction r t o)
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=sAsn1Constraints; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=icdMaxSizeWithoutAlignment t.acnAlignment o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None;}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd})
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
        let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createBoolFunction r t o)
        let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux t.acnAlignment (getASN1Name t) (getASN1Name t) sAsn1Constraints o.acnMinSizeInBits o.acnMaxSizeInBits t.unitsOfMeasure
        let aux = lm.lg.generateBooleanAuxiliaries r ACN t o nestingScope p.accessPath codec
        Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=aux; icdResult = Some icd}
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
            let icd = AcnPrimitiveFactory.buildPrimitiveIcdAux t.acnAlignment icdDesc (getASN1Name t) None o.acnMinSizeInBits o.acnMaxSizeInBits t.unitsOfMeasure
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= lm.lg.acn.null_valIsUnReferenced; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=aux; icdResult = Some icd})
    AcnPrimitiveFactory.createAsn1Primitive r deps lm codec t typeDefinition isValidFunc [] us funcBody
