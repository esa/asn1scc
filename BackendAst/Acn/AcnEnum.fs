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

let createEnumCommon (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefinitionOrReference ) (typeDefinitionName:string) (icdStgFileName:string) sAsn1Constraints acnMinSizeInBits acnMaxSizeInBits unitsOfMeasure =
    let EnumeratedEncValues                 = lm.acn.EnumeratedEncValues
    let Enumerated_item                     = lm.acn.Enumerated_item
    let IntFullyConstraintPos               = lm.uper.IntFullyConstraintPos
    let Enumerated_no_switch                = lm.acn.EnumeratedEncValues_no_switch

    let min = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.min
    let max = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.max
    let sFirstItemName = lm.lg.getNamedItemBackendName (Some defOrRef) o.items.Head
    let uperRange = (Concrete (min,max))
    let intTypeClass = getIntEncodingClassByUperRange r.args uperRange
    let rtlIntType = (DAstTypeDefinition.getIntegerTypeByClass lm intTypeClass)()
    let nLastItemIndex      = BigInteger(Seq.length o.items) - 1I

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let td = (lm.lg.getEnumTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let localVar, intVal =
            let varName = $"intVal_{ToC p.accessPath.asIdentifier}"
            let lv =
                match lm.lg.decodingKind with
                | Copy -> []
                | InPlace -> [GenericLocalVariable {GenericLocalVariable.name = varName; varType= rtlIntType; arrSize= None; isStatic = false; initExp=None}]
            lv, varName
        let pVal = {CodegenScope.modName = typeId.ModName; accessPath = AccessPath.valueEmptyPath intVal}
        let intFuncBody =
            let uperInt (errCode:ErrorCode) (nestingScope: NestingScope) (p:CodegenScope) (fromACN: bool) =
                let pp, resultExpr = adaptArgument lm codec p
                let castPp  = DAstUPer.castPp r lm codec pp intTypeClass
                let sSsuffix = DAstUPer.getIntDecFuncSuffix intTypeClass
                let word_size_in_bits = (int r.args.integerSizeInBytes)*8
                let nbits = GetNumberOfBitsForNonNegativeInteger (max-min)
                let rangeAssert =
                    match typeId.topLevelTas with
                    | Some tasInfo ->
                        lm.lg.generateIntFullyConstraintRangeAssert (ToC (r.args.TypePrefix + tasInfo.tasName)) p codec
                    | None -> None
                let funcBody = IntFullyConstraintPos (castPp word_size_in_bits) min max nbits sSsuffix errCode.errCodeName rangeAssert codec
                Some({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables= []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]})
            AcnPrimitives.createAcnIntegerFunctionInternal r lm codec (Concrete (min,max)) intTypeClass o.acnEncodingClass uperInt sAsn1Constraints acnMinSizeInBits acnMaxSizeInBits unitsOfMeasure typeDefinitionName (None, None)
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
                [{IcdRow.fieldName = fieldName; comments = newComments; sPresent=sPresent;sType=(IcdPlainType "ENUMERATED"); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = "ENUMERATED"; rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None;}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; userDefinedFunctions=[]; localVariables = localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult=Some icd})
    funcBody



let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (icdStgFileName:string) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefinitionOrReference) (typeDefinition:TypeDefinitionOrReference)   (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode: ErrorCode)
                 (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list)
                 (nestingScope: NestingScope)
                 (p: CodegenScope) =
        let typeDefinitionName = defOrRef.longTypedefName2 lm.lg.hasModules //getTypeDefinitionName t.id.tasInfo typeDefinition
        let funcBodyOrig = createEnumCommon r deps lm codec t.id o defOrRef typeDefinitionName icdStgFileName None t.acnMinSizeInBits t.acnMaxSizeInBits t.unitsOfMeasure
        let res = funcBodyOrig errCode acnArgs nestingScope p
        res |> Option.map (fun res ->
            let aux = lm.lg.generateEnumAuxiliaries r ACN t o nestingScope p.accessPath codec
            {res with auxiliaries = res.auxiliaries @ aux})

    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations  [] us


let createAcnEnumeratedFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (icdStgFileName:string) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToEnumerated)  (defOrRef:TypeDefinitionOrReference) (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None
    let td = lm.lg.getTypeDefinition (t.getType r).FT_TypeDefinition
    let typeDefinitionName = td.typeName
    let funcBody = createEnumCommon r deps lm codec typeId t.enumerated defOrRef typeDefinitionName icdStgFileName None t.enumerated.acnMinSizeInBits t.enumerated.acnMaxSizeInBits None
    (funcBody errCode), ns
