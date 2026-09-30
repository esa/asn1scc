module AcnEnum

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


let enumComment stgFileName (o:Asn1AcnAst.Enumerated) =
    let EmitItem (n:Asn1AcnAst.NamedItem) =
        let comment =  n.Comments |> Seq.StrJoin "\n"
        match comment.Trim() with
        | ""        ->    icd_uper.EmitEnumItem stgFileName n.Name.Value n.definitionValue
        | _         ->    icd_uper.EmitEnumItemWithComment stgFileName n.Name.Value n.definitionValue comment
    let itemsHtml =
        o.items |>
            List.filter(fun z ->
                let v = z.Name.Value
                Asn1Fold.isValidValueGeneric o.AllCons (=) v ) |>
            List.map EmitItem
    icd_uper.EmitEnumInternalContents stgFileName itemsHtml

/// The ACN codec of the integer that carries an ENUMERATED value (the item's
/// acnEncodeValue), applied to the CodegenScope it is called with.
let private enumIntFuncBody (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (o:Asn1AcnAst.Enumerated) sAsn1Constraints (acnAlignment: AcnGenericTypes.AcnAlignment option) acnMinSizeInBits acnMaxSizeInBits unitsOfMeasure (typeDefinitionName:string) =
    let IntFullyConstraintPos               = lm.uper.IntFullyConstraintPos
    let min = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.min
    let max = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.max
    let intTypeClass = getIntEncodingClassByUperRange r.args (Concrete (min,max))
    let uperInt (errCode:ErrorCode) (nestingScope: NestingScope) (p:CodegenScope) (fromACN: bool) =
        let pp, resultExpr = adaptArgument lm codec p
        let castPp  = DAstUPer.castPp r lm codec pp intTypeClass
        let sSsuffix = lm.lg.getIntDecFuncSuffix intTypeClass
        let word_size_in_bits = (int r.args.integerSizeInBytes)*8
        let nbits = GetNumberOfBitsForNonNegativeInteger (max-min)
        let rangeAssert =
            match typeId.topLevelTas with
            | Some tasInfo ->
                lm.lg.generateIntFullyConstraintRangeAssert (ToC (r.args.TypePrefix + tasInfo.tasName)) p codec
            | None -> None
        let intType = Some (lm.typeDef.Declare_Integer())
        let funcBody = IntFullyConstraintPos (castPp word_size_in_bits) min max nbits sSsuffix errCode.errCodeName rangeAssert intType codec
        Some({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables= []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]})
    AcnPrimitives.createAcnIntegerFunctionInternal r lm codec (Concrete (min,max)) intTypeClass o.acnEncodingClass uperInt sAsn1Constraints acnAlignment acnMinSizeInBits acnMaxSizeInBits unitsOfMeasure typeDefinitionName (None, None) ""

/// The values that a fixed-size ACN integer encoding can carry; None for the
/// uPER-like encoding (range-checked by the decoder) and the variable-size ones.
let private encodableRange (enc:IntEncodingClass) : (BigInteger*BigInteger) option =
    let unsigned (bits:int) = Some (0I, BigInteger.Pow(2I, bits) - 1I)
    let signed (bits:int) = Some (-BigInteger.Pow(2I, bits - 1), BigInteger.Pow(2I, bits - 1) - 1I)
    let digits (n:int) = BigInteger.Pow(10I, n) - 1I
    match enc with
    | PositiveInteger_ConstSize_8 -> unsigned 8
    | PositiveInteger_ConstSize_big_endian_16 | PositiveInteger_ConstSize_little_endian_16 -> unsigned 16
    | PositiveInteger_ConstSize_big_endian_32 | PositiveInteger_ConstSize_little_endian_32 -> unsigned 32
    | PositiveInteger_ConstSize_big_endian_64 | PositiveInteger_ConstSize_little_endian_64 -> unsigned 64
    | PositiveInteger_ConstSize bits -> unsigned (int bits)
    | TwosComplement_ConstSize_8 -> signed 8
    | TwosComplement_ConstSize_big_endian_16 | TwosComplement_ConstSize_little_endian_16 -> signed 16
    | TwosComplement_ConstSize_big_endian_32 | TwosComplement_ConstSize_little_endian_32 -> signed 32
    | TwosComplement_ConstSize_big_endian_64 | TwosComplement_ConstSize_little_endian_64 -> signed 64
    | TwosComplement_ConstSize bits -> signed (int bits)
    | ASCII_ConstSize bits -> let d = digits (int bits / 8 - 1) in Some (-d, d)        // sign + digits
    | ASCII_UINT_ConstSize bits -> Some (0I, digits (int bits / 8))
    | BCD_ConstSize bits -> Some (0I, digits (int bits / 4))
    | Integer_uPER
    | ASCII_VarSize_NullTerminated _
    | ASCII_UINT_VarSize_NullTerminated _
    | BCD_VarSize_NullTerminated _ -> None

/// An ACN stream that holds only a code of the ENUMERATED type that no item
/// uses, encoded like the type's own encoder encodes an item value, for the
/// automatic test of the ACN decoder's default arm. The code is the smallest one
/// between the smallest and the largest item value. When the items leave no gap,
/// and the language's ACN integer decoders do not check min..max
/// (atcInvalidStreamCodeOutsideItemRange, C), a fixed-size encoding still carries
/// max + 1 or min - 1 (not negative). None otherwise, and for the efficient
/// (switch-free) enumeration encoding.
let createInvalidCodeStream (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) : AcnInvalidCodeStream option =
    let values = o.items |> List.map(fun x -> x.acnEncodeValue) |> Set.ofList
    let min, max = Set.minElement values, Set.maxElement values
    let gapCode =
        Seq.unfold (fun v -> if v > max then None else Some (v, v + 1I)) min |>
        Seq.tryFind (fun v -> not (values.Contains v))
    let outsideCode () =
        match lm.lg.atcInvalidStreamCodeOutsideItemRange, encodableRange o.acnEncodingClass with
        | true, Some (encMin, encMax) ->
            [max + 1I; min - 1I] |> List.tryFind (fun v -> v >= encMin && v <= encMax && v >= 0I)
        | true, None
        | false, _ -> None
    match r.args.isEnumEfficientEnabled o.items.Length with
    | true  -> None
    | false ->
        (match gapCode with Some _ -> gapCode | None -> outsideCode ()) |>
        Option.bind (fun code ->
            let intTypeClass = getIntEncodingClassByUperRange r.args (Concrete (min,max))
            let codeVarName = "tc_invalidCode"
            let p = {CodegenScope.modName = t.id.ModName; accessPath = AccessPath.valueEmptyPath codeVarName}
            // The encoder of an in-range code does not fail; its error code is never set.
            let errCode = {ErrorCode.errCodeValue = 0; errCodeName = "0"; comment = None; fieldPath = ""}
            let intFuncBody = enumIntFuncBody r lm Codec.Encode t.id o None t.acnAlignment t.acnMinSizeInBits t.acnMaxSizeInBits None ""
            intFuncBody errCode [] (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) p |>
            Option.map (fun res ->
                {AcnInvalidCodeStream.code = code; codeVarName = codeVarName
                 codeVarType = (DAstTypeDefinition.getIntegerTypeByClass lm intTypeClass)()
                 encodeStatement = res.funcBody; localVariables = res.localVariables}))

let createEnumCommon (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefinitionOrReference ) (typeDefinitionName:string) (icdStgFileName:string) sAsn1Constraints (acnAlignment: AcnGenericTypes.AcnAlignment option) acnMinSizeInBits acnMaxSizeInBits unitsOfMeasure =
    let EnumeratedEncValues                 = lm.acn.EnumeratedEncValues
    let Enumerated_item                     = lm.acn.Enumerated_item
    let Enumerated_no_switch                = lm.acn.EnumeratedEncValues_no_switch

    let min = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.min
    let max = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.max
    let sFirstItemName = lm.lg.getNamedItemBackendName (Some defOrRef) o.items.Head
    let uperRange = (Concrete (min,max))
    let intTypeClass = getIntEncodingClassByUperRange r.args uperRange
    let rtlIntType = (DAstTypeDefinition.getIntegerTypeByClass lm intTypeClass)()
    let nLastItemIndex      = BigInteger(Seq.length o.items) - 1I

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let td = (lm.lg.getEnumTypeDefinition o.typeDef).longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
        let localVar, intVal =
            let varName = lm.lg.getEnumIntLocalVarName (ToC (p.accessPath.asIdentifier lm.lg))
            let lv =
                match lm.lg.decodingKind with
                | Copy -> []
                | InPlace -> [GenericLocalVariable {GenericLocalVariable.name = varName; varType= rtlIntType; arrSize= None; isStatic = false; initExp=None}]
            lv, varName
        let pVal = {CodegenScope.modName = typeId.ModName; accessPath = AccessPath.valueEmptyPath intVal}
        let intFuncBody = enumIntFuncBody r lm codec typeId o sAsn1Constraints acnAlignment acnMinSizeInBits acnMaxSizeInBits unitsOfMeasure typeDefinitionName
        let funcBodyContent =
            match intFuncBody errCode acnArgs nestingScope pVal with
            | None -> None
            | Some intAcnFuncBdResult ->
                let resultExpr, errCodes, auxiliaries =
                    intAcnFuncBdResult.resultExpr, intAcnFuncBdResult.errCodes, intAcnFuncBdResult.auxiliaries
                let mainContent, localVariables =
                    match r.args.isEnumEfficientEnabled o.items.Length  with
                    | false ->
                        let arrItems =
                            o.items |>
                            List.map(fun it ->
                                let enumClassName = lm.lg.extractEnumClassName "" it.scala_name it.Name.Value
                                Enumerated_item (lm.lg.getValue p.accessPath) (lm.lg.getNamedItemBackendName (Some defOrRef) it) enumClassName it.acnEncodeValue (lm.lg.intValueToString it.acnEncodeValue intTypeClass) intVal codec)
                        EnumeratedEncValues (lm.lg.getValue p.accessPath) td arrItems intAcnFuncBdResult.funcBody errCode.errCodeName sFirstItemName intVal codec, localVar@intAcnFuncBdResult.localVariables
                    | true ->
                        let sEnumIndex = "nEnumIndex"
                        let enumIndexVar = (Asn1SIntLocalVariable (sEnumIndex, None))
                        Enumerated_no_switch (lm.lg.getValue p.accessPath) td intAcnFuncBdResult.funcBody errCode.errCodeName sFirstItemName  intVal   sEnumIndex nLastItemIndex o.encodeValues   codec, enumIndexVar::localVar@intAcnFuncBdResult.localVariables
                Some (mainContent, resultExpr, errCodes, localVariables, auxiliaries)

        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent, resultExpr, errCodes, localVariables, auxiliaries) ->
            let icdFnc fieldName sPresent (comments:string list) =
                let newComments = comments@[enumComment icdStgFileName o]
                [{IcdRow.fieldName = fieldName; comments = newComments; sPresent=sPresent;sType=(IcdPlainType "ENUMERATED"); sConstraint=sAsn1Constraints; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=icdMaxSizeWithoutAlignment acnAlignment o.acnMaxSizeInBits;sUnits=unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = "ENUMERATED"; rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None;}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; userDefinedFunctions=[]; localVariables = localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult=Some icd})
    funcBody



let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (icdStgFileName:string) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefinitionOrReference) (typeDefinition:TypeDefinitionOrReference)   (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode: ErrorCode)
                 (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list)
                 (nestingScope: NestingScope)
                 (p: CodegenScope) =
        let typeDefinitionName = defOrRef.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName //getTypeDefinitionName t.id.tasInfo typeDefinition
        let sAsn1Constraints = constraintsToIcdStr (DAstAsn1.createEnumeratedFunction r t o)
        let funcBodyOrig = createEnumCommon r deps lm codec t.id o defOrRef typeDefinitionName icdStgFileName sAsn1Constraints t.acnAlignment t.acnMinSizeInBits t.acnMaxSizeInBits t.unitsOfMeasure
        let res = funcBodyOrig errCode acnArgs nestingScope p
        res |> Option.map (fun res ->
            let aux = lm.lg.generateEnumAuxiliaries r ACN t o nestingScope p.accessPath codec
            {res with auxiliaries = res.auxiliaries @ aux})

    AcnPrimitiveFactory.createAsn1Primitive r deps lm codec t typeDefinition isValidFunc [] us funcBody


let createAcnEnumeratedFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (icdStgFileName:string) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToEnumerated)  (defOrRef:TypeDefinitionOrReference) (us:State)  =
    let td = lm.lg.getTypeDefinition (t.getType r).FT_TypeDefinition
    let typeDefinitionName = td.typeName
    let funcBody = createEnumCommon r deps lm codec typeId t.enumerated defOrRef typeDefinitionName icdStgFileName None t.acnAlignment t.enumerated.acnMinSizeInBits t.enumerated.acnMaxSizeInBits None
    // ACN-inserted children referencing an ENUMERATED TAS keep that name on
    // the ICD row ("ENUMERATED (DeviceMode)", roadmap A4).  ACN children
    // bypass AcnFunctionWrapper's central suffixing, so it is applied here.
    AcnPrimitiveFactory.createAcnOnlyPrimitive lm codec typeId us (fun errCode ->
        fun acnArgs nestingScope p ->
            funcBody errCode acnArgs nestingScope p
            |> Option.map (fun res -> {res with icdResult = res.icdResult |> Option.map (icdAuxAddNamedTypeSuffix (Some t.tasName.Value))}))
