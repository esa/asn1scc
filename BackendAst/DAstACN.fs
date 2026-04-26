module DAstACN

open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open System.Globalization
open Language


// --- Re-exports from extracted modules (see BackendAst/Acn/) ---
// The helpers below were moved to BackendAst/Acn/Acn{Helpers,DeterminantDef,Alignment,Icd}.fs.
// They are re-exported here so external callers that reference them as
// `DAstACN.foo` keep working unchanged.
let foldMap = AcnHelpers.foldMap
let callBaseTypeFunc = AcnHelpers.callBaseTypeFunc
let sparkAnnotations = AcnHelpers.sparkAnnotations
let THREE_DOTS = AcnHelpers.THREE_DOTS
let getAcnDeterminantName = AcnHelpers.getAcnDeterminantName
let adaptArgument = AcnHelpers.adaptArgument
let adaptArgumentValue = AcnHelpers.adaptArgumentValue
let joinedOrAsIdentifier = AcnHelpers.joinedOrAsIdentifier

let getDeterminantTypeDefinitionBodyWithinSeq = AcnDeterminantDef.getDeterminantTypeDefinitionBodyWithinSeq
let getDeterminant_macro = AcnDeterminantDef.getDeterminant_macro
let getDeterminantTypeUpdateMacro = AcnDeterminantDef.getDeterminantTypeUpdateMacro
let getDeterminantTypeCheckEqual = AcnDeterminantDef.getDeterminantTypeCheckEqual

type FuncBody = AcnAlignment.FuncBody
type FuncBodyStateless = AcnAlignment.FuncBodyStateless
let handleSavePosition = AcnAlignment.handleSavePosition
let handleAlignmentForAsn1Types = AcnAlignment.handleAlignmentForAsn1Types
let handleAlignmentForAcnTypes = AcnAlignment.handleAlignmentForAcnTypes

let md5 = AcnIcd.md5
let createIcdTas = AcnIcd.createIcdTas

// `createAcnFunction` is the generic dispatcher that wraps a per-type
// `funcBody` with alignment, save-position handling, error-code generation,
// ICD support, Spark annotations and test-case validation.  See
// BackendAst/Acn/AcnFunctionWrapper.fs for the implementation.
let createAcnFunction = AcnFunctionWrapper.createAcnFunction

// Primitive type encoders — moved to BackendAst/Acn/AcnPrimitives.fs.
// Re-exported here so external callers (DAstConstruction, DAstACNDeferred)
// keep working unchanged.
type AcnIntegerFuncBody = AcnPrimitives.AcnIntegerFuncBody
let createAcnIntegerFunctionInternal = AcnPrimitives.createAcnIntegerFunctionInternal
let getMappingFunctionModule = AcnPrimitives.getMappingFunctionModule
let createAcnIntegerFunction = AcnPrimitives.createAcnIntegerFunction
let createIntegerFunction = AcnPrimitives.createIntegerFunction
let createRealFunction = AcnPrimitives.createRealFunction
let createObjectIdentifierFunction = AcnPrimitives.createObjectIdentifierFunction
let createTimeTypeFunction = AcnPrimitives.createTimeTypeFunction
let nestChildItems = AcnPrimitives.nestChildItems
let createAcnBooleanFunction = AcnPrimitives.createAcnBooleanFunction
let createBooleanFunction = AcnPrimitives.createBooleanFunction
let createAcnNullTypeFunction = AcnPrimitives.createAcnNullTypeFunction
let createNullTypeFunction = AcnPrimitives.createNullTypeFunction

// ENUMERATED encoders — moved to BackendAst/Acn/AcnEnum.fs.
let enumComment = AcnEnum.enumComment
let createEnumCommon = AcnEnum.createEnumCommon
let createEnumeratedFunction = AcnEnum.createEnumeratedFunction
let createAcnEnumeratedFunction = AcnEnum.createAcnEnumeratedFunction

// IA5String / NumericString encoders — moved to BackendAst/Acn/AcnStrings.fs.
let createStringFunction = AcnStrings.createStringFunction
let createAcnStringFunction = AcnStrings.createAcnStringFunction

// OCTET STRING / BIT STRING encoders — moved to BackendAst/Acn/AcnOctetBitStrings.fs.
let createOctetStringFunction = AcnOctetBitStrings.createOctetStringFunction
let createBitStringFunction = AcnOctetBitStrings.createBitStringFunction

(*
If the type assignment has acnParameters, then no function is generated. This function can only be inlined by the calling function
(i.e. by the parent type encoding function).
Now, we have to make this rule recursive: 
A composite type (e.g SEQUENCE, choice etc ) may have references (i.e. reference types) to a type assignment that has acnParameters.
In this case, the reference must have arguments in the acn in the form <arg1,arg2, ...>
These argument can either ACN inserted fields or acnParameters.
If the reference type is written explicitly in the acn, by the user, then the arguments must be checked to be inline with the acnParameters.
If they are not, the user gets an error.

However, there are cases where the reference type is not written explicitly by the user in the acn grammar, 
but is infered by the compiler. For example, 

The following asn1 grammar define two types:
CfdpPDU ::= SEQUENCE {
   pdu-header PDUHeader,
   payload OCTET STRING (CONTAINING PayloadData)
}
PayloadData ::= CHOICE {
   file-directive FileDirectiveType,
   file-data FileDataType
}
FileDataType ::= SEQUENCE {
   file-data-pdu FileDataPDU
}

However the acn grammar provides defintions only for CfdpPDU and FileDataType, not PayloadData. In fact, the PayloadData acn spec
is provided inline in the CfdpPDU acn spec, not at the PayloadData Type Assignment Level. 
CfdpPDU [] {
   pdu-header                                [] {
      pdu-type                               PDUType [encoding pos-int, size 1],
      pdu-data-field-length                  PDUDataFieldLength [encoding pos-int, size 16]
   },
   payload                                   [size pdu-header.pdu-data-field-length] {
      file-directive                         [present-when pdu-header.pdu-type==0],
      file-data                              <pdu-header.pdu-data-field-length> [present-when pdu-header.pdu-type==1]
   }
}
FileDataType <PDUDataFieldLength:pdu-data-field-length> [] {
   file-data-pdu                             <pdu-data-field-length> []
}

Therefore, the compiler uses a defult acn specs for the PayloadData type assignment, which is not provided by the user.
In this case the file-data reference type has no acnArgs. This means that no acn function must be generated for the FileDataType type assignment.

*)




// External-field / determinant lookup helpers — moved to BackendAst/Acn/AcnExternalField.fs.
let getExternalField0 = AcnExternalField.getExternalField0
let getExternalField0Type = AcnExternalField.getExternalField0Type
let getExternalFieldChoicePresentWhen = AcnExternalField.getExternalFieldChoicePresentWhen
let getExternalFieldTypeChoicePresentWhen = AcnExternalField.getExternalFieldTypeChoicePresentWhen
let getExternalField = AcnExternalField.getExternalField
let getExternalFieldType = AcnExternalField.getExternalFieldType

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
        | SZ_EC_TerminationPattern  _ -> [SequenceOfIndex (ii, None)]

    let nAlignSize = 0I;
    let nIntItemMaxSize = child.acnMaxSizeInBits
    let td = typeDefinition.longTypedefName2 lm.lg.hasModules

    let icdFnc fieldName sPresent comments  =
        let lengthRow, terminationPattern =
            match o.acnEncodingClass with
            | SZ_EC_LENGTH_EMBEDDED _ ->
                let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                [{IcdRow.fieldName = "Length"; comments = [$"The number of items"]; sPresent="always";sType=IcdPlainType "INTEGER"; sConstraint=None; minLengthInBits = nSizeInBits ;maxLengthInBits=nSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = Some 1}], []
            | SZ_EC_FIXED_SIZE
            | SZ_EC_ExternalField       _ -> [], []
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
                let a1 = {IcdRow.fieldName = "Item #1"; comments = comments; sPresent=sPresent;sType=sType; sConstraint=None; minLengthInBits = child.acnMinSizeInBits; maxLengthInBits=child.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = Some (lengthRow.Length + 1)}
                let a2 = {a1 with fieldName = $"Item #{o.maxSize.acn}"; idxOffset = Some ((int o.maxSize.acn)+lengthRow.Length)}
                lengthRow@[a1;THREE_DOTS;a2], [childIcdTas]
        | None -> lengthRow@terminationPattern, []
    let sExtraComment =
        match o.acnEncodingClass with
        | Asn1AcnAst.SZ_EC_FIXED_SIZE                    -> $"Length is fixed to {o.maxSize.acn} elements (no length determinant is needed)."
        | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _             -> if o.maxSize.acn <2I then "The array contains a single element." else ""
        | Asn1AcnAst.SZ_EC_ExternalField relPath         ->  $"Length is determined by the external field: %s{relPath.AsString}"
        | Asn1AcnAst.SZ_EC_TerminationPattern bitPattern ->  $"Length is determined by the stop marker '%s{bitPattern.Value}'"
    let icd = {IcdArgAux.canBeEmbedded = false; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[sExtraComment]; scope="type"; name= None}


    let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let level = p.accessPath.SequenceOfLevel + 1
        let pp, resultExpr = joinedOrAsIdentifier lm codec p
        // `childInitExpr` is used to initialize the array of elements in which we will write their decoded values
        // It is only meaningful for "Copy" decoding kind, since InPlace will directly modify `p`'s array
        let childInitExpr = DAstInitialize.getChildExpression lm child
        let access = lm.lg.getAccess p.accessPath
        match child.getAcnFunction codec with
        | None -> None, us
        | Some chFunc  ->
            let childNestingScope = {nestingScope with nestingLevel = nestingScope.nestingLevel + 1I; parents = (p, t) :: nestingScope.parents}
            let internalItem, ns = chFunc.funcBody us acnArgs childNestingScope ({p with accessPath = lm.lg.getArrayItem p.accessPath (i level)  child.isIA5String})
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
                            let funcBody = varSize pp access td (i level) "" o.minSize.acn o.maxSize.acn nSizeInBits child.acnMinSizeInBits nIntItemMaxSize 0I childInitExpr errCode.errCodeName absOffset remBits lvl ix offset introSnap callAux codec
                            Some ({AcnFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = (lv level)@nStringLength; userDefinedFunctions=[]; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd})

                    | Some internalItem ->
                        let childErrCodes =  internalItem.errCodes
                        let ret, localVariables =
                            match o.isFixedSize with
                            | true -> fixedSize pp td (i level) internalItem.funcBody o.minSize.acn child.acnMinSizeInBits nIntItemMaxSize 0I childInitExpr callAux codec, nStringLength
                            | false -> varSize pp access td (i level) internalItem.funcBody o.minSize.acn o.maxSize.acn nSizeInBits child.acnMinSizeInBits nIntItemMaxSize 0I childInitExpr errCode.errCodeName absOffset remBits lvl ix offset introSnap callAux codec, nStringLength
                        Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = (lv level)@(internalItem.localVariables@localVariables); userDefinedFunctions=internalItem.userDefinedFunctions; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=internalItem.auxiliaries @ auxiliaries; icdResult = Some icd})

                | SZ_EC_ExternalField _ ->
                    match internalItem with
                    | None -> None
                    | Some internalItem ->
                        let localVariables = internalItem.localVariables
                        let childErrCodes = internalItem.errCodes
                        let extField = getExternalField r deps t.id
                        let tp = getExternalFieldType r deps t.id
                        let unsigned =
                            match tp with
                            | Some (AcnInsertedType.AcnInteger int) -> int.isUnsigned
                            | Some (AcnInsertedType.AcnNullType _) -> true
                            | _ -> false
                        let introSnap = nestingScope.nestingLevel = 0I
                        let funcBodyContent =
                            match o.isFixedSize with
                            | true  -> oct_sqf_external_field_fix_size td pp access (i level) internalItem.funcBody (if o.minSize.acn=0I then None else Some o.minSize.acn) o.maxSize.acn extField unsigned nAlignSize errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits childInitExpr introSnap callAux codec
                            | false -> external_field td pp access (i level) internalItem.funcBody (if o.minSize.acn=0I then None else Some o.minSize.acn) o.maxSize.acn extField unsigned nAlignSize errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits childInitExpr introSnap callAux codec
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
                        let funcBodyContent = oct_sqf_null_terminated pp access (i level) internalItem.funcBody noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits codec

                        let lv2 =
                            match codec, lm.lg.acn.checkBitPatternPresentResult with
                            | Decode, true -> [IntegerLocalVariable ("checkBitPatternPresentResult", Some (lm.lg.intValueToString 0I (ASN1SCC_Int8 (-128I, 127I))))]
                            | _ -> []

                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = lv2@(lv level)@localVariables; userDefinedFunctions=internalItem.userDefinedFunctions; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=internalItem.auxiliaries; icdResult = Some icd})
            ret,ns
    let soSparkAnnotations = Some(sparkAnnotations lm td codec)
    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations [] us

let initExpr (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (t: Asn1AcnAst.AcnInsertedType): string =
    match t with
    | AcnInteger int -> lm.lg.asn1SccIntValueToString 0I int.isUnsigned
    | AcnNullType _ -> lm.lg.asn1SccIntValueToString 0I true
    | AcnBoolean _ -> lm.lg.FalseLiteral
    | AcnReferenceToIA5String s -> lm.lg.initializeString None (int s.str.maxSize.uper)
    | AcnReferenceToEnumerated e ->
        lm.lg.getNamedItemBackendName (Some (defOrRef r m e)) e.enumerated.items.Head

// Choose the right base scope for folding a dependency path.
// In --acn-v2, a reference type's funcBody may be inlined into a specialized
// function whose runtime pVal does not correspond to the captured type's TAS.
// To generate correct access paths, walk the enclosing scopes (current SEQUENCE
// + ancestors from nestingScope.parents) and pick the DEEPEST whose type id is
// a prefix of depPath.  Fold the remaining nodes from that scope.
//
// Intra-sequence deps resolve to the current SEQUENCE (shortest fold).
// Sibling/outer deps resolve to an ancestor whose scope matches the dep's
// outer TA, giving the correct base pVal for the fold.
let private resolveDepScope (nestingScope: NestingScope) (pSrcRoot: CodegenScope) (depPath: ReferenceToType) : CodegenScope * ReferenceToType =
    let (ReferenceToType depNodes) = depPath
    let candidates = nestingScope.parents |> List.map (fun (p, t) -> (p, t.id))
    let matching =
        candidates |> List.tryFind (fun (_, ReferenceToType scopeNodes) ->
            let n = List.length scopeNodes
            n >= 2 && List.length depNodes >= n && List.take n depNodes = scopeNodes)
    match matching with
    | Some (scope, ReferenceToType scopeNodes) ->
        let n = List.length scopeNodes
        let remaining = ReferenceToType (List.take 2 scopeNodes @ List.skip n depNodes)
        scope, remaining
    | None ->
        pSrcRoot, depPath

let rec handleSingleUpdateDependency (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (d:AcnDependency)  (us:State) =
    let presenceDependency              = lm.acn.PresenceDependency
    let sizeDependency                  = lm.acn.SizeDependency
    let sizeDependencyFixedSize         = lm.acn.SizeDependencyFixedSize
    let sizeDep_oct_str_containing      = lm.acn.SizeDependency_oct_str_containing
    let getSizeableSize                 = lm.acn.getSizeableSize
    let getStringSize                   = lm.acn.getStringSize
    let choiceDependencyPres            = lm.acn.ChoiceDependencyPres
    let choiceDependencyIntPres_child   = lm.acn.ChoiceDependencyIntPres_child
    let choiceDependencyStrPres_child   = lm.acn.ChoiceDependencyStrPres_child
    let choiceDependencyEnum            = lm.acn.ChoiceDependencyEnum
    let choiceDependencyEnum_Item       = lm.acn.ChoiceDependencyEnum_Item
    let checkAccessPath                 = lm.acn.checkAccessPath

    match d.dependencyKind with
    | AcnDepRefTypeArgument           acnPrm   ->
        let prmUpdateStatement, ns1 = getUpdateFunctionUsedInEncoding r deps lm m acnPrm.id us
        match prmUpdateStatement with
        | None  -> None, ns1
        | Some prmUpdateStatement   ->
            let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
                prmUpdateStatement.updateAcnChildFnc child nestingScope vTarget pSrcRoot
            let icdComments =
                let aaa = sprintf "reference determinant for %s " (acnPrm.id.AsString)
                [aaa]
            Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=prmUpdateStatement.errCodes; testCaseFnc = prmUpdateStatement.testCaseFnc; localVariables=[]}), ns1
    | AcnDepSizeDeterminant (minSize, maxSize, szAcnProp)        ->
        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
            let pSizeable, checkPath = getAccessFromScopeNodeList relPath false lm pBase
            let unsigned =
                match child.Type with
                | AcnInteger int -> int.isUnsigned
                | AcnNullType _ -> true
                | _ -> raise (BugErrorException "???")
            let updateStatement =
                match minSize.acn = maxSize.acn with
                | true  -> sizeDependencyFixedSize v minSize.acn
                | false -> sizeDependency v (getSizeableSize (pSizeable.accessPath.joined lm.lg) (lm.lg.getAccess pSizeable.accessPath) unsigned) minSize.uper maxSize.uper false child.typeDefinitionBodyWithinSeq
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type

        let icdComments =
            let aaa = sprintf "size determinant for %s " (d.asn1Type.AsString)
            [aaa]
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepSizeDeterminant_bit_oct_str_contain  o       ->
        let baseTypeDefinitionName =
            match lm.lg.hasModules with
            | false     -> ToC2(r.args.TypePrefix + o.tasName.Value)
            | true   ->
                match m.Name.Value = o.modName.Value with
                | true  -> ToC2(r.args.TypePrefix + o.tasName.Value)
                | false -> (ToC o.modName.Value) + "." + ToC2(r.args.TypePrefix + o.tasName.Value)
        let baseFncName = baseTypeDefinitionName + "_ACN" + Encode.suffix
        let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ACN_ENCODING" baseTypeDefinitionName
        let asn1TypeD = us.newTypesMap[d.asn1Type] :?> Asn1Type
        let asn1TypeD = match asn1TypeD.Kind with ReferenceType  o -> o.resolvedType.ActualType | _  -> asn1TypeD
        let errCodes0, localVariables0, ns =
            match asn1TypeD.acnEncFunction with
            | Some f  ->
                let fncBdRes, ns = f.funcBody us [] (NestingScope.init asn1TypeD.acnMaxSizeInBits asn1TypeD.uperMaxSizeInBits []) {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
                match fncBdRes with
                | Some x -> x.errCodes, [], ns
                | None   -> [], [], us
            | None    -> [], [], us

        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
            let pSizeable, checkPath = getAccessFromScopeNodeList relPath false lm pBase
            let sInner =
                match asn1TypeD.acnEncFunction with
                | Some f  ->
                    let fncBdRes, _ = f.funcBody us [] nestingScope pSizeable
                    match fncBdRes with
                    | None -> ""
                    | Some a -> a.funcBody
                | None -> ""
            let sLocalVarType = child.typeDefinitionBodyWithinSeq
            let updateStatement = sizeDep_oct_str_containing (lm.lg.getParamValue o.resolvedType pSizeable.accessPath Encode) baseFncName sReqBytesForUperEncoding v (match o.encodingOptions with Some eo -> eo.octOrBitStr = ContainedInOctString | None -> false) sInner sLocalVarType
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        let localVars = lm.lg.acn.getAcnDepSizeDeterminantLocVars sReqBytesForUperEncoding
        let icdComments =
            let aaa = sprintf "size determinant for %s " (d.asn1Type.AsString)
            [aaa]
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=errCodes0; testCaseFnc=testCaseFnc; localVariables= localVariables0@localVars}), ns
    | AcnDepIA5StringSizeDeterminant (minSize, maxSize, szAcnProp)   ->

        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
            let pSizeable, checkPath = getAccessFromScopeNodeList relPath true lm pBase
            let updateStatement = sizeDependency v (getStringSize (pSizeable.accessPath.joined lm.lg))  minSize.uper maxSize.uper true child.typeDefinitionBodyWithinSeq
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        let icdComments =
            let aaa = sprintf "size determinant for %s " (d.asn1Type.AsString)
            [aaa]
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepPresenceBool              ->
        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let parDecTypeSeq =
                match d.asn1Type with
                | ReferenceToType (nodes) -> ReferenceToType (nodes |> List.rev |> List.tail |> List.rev)
            let pBase, relPath = resolveDepScope nestingScope pSrcRoot parDecTypeSeq
            let pDecParSeq, checkPath = getAccessFromScopeNodeList relPath false lm pBase
            let updateStatement = presenceDependency v (pDecParSeq.accessPath.joined lm.lg) (lm.lg.getAccess pDecParSeq.accessPath) (ToC d.asn1Type.lastItem)
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            match atc.testCaseTypeIDsMap.TryFind(d.asn1Type) with
            | Some _    -> Some TcvComponentPresent
            | None      -> Some TcvComponentAbsent
        let icdComments =
            let aaa = sprintf "Used as a presence determinant for %s " (d.asn1Type.AsString)
            [aaa]
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepPresence   (relPath, chc)               ->
        let icdComments =
            let aaa = sprintf "Used as a presence determinant for %s " (chc.typeDef[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)
            [aaa]
        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let pBase, relPath1 = resolveDepScope nestingScope pSrcRoot d.asn1Type
            let choicePath, checkPath = getAccessFromScopeNodeList relPath1 false lm pBase
            let arrsChildUpdates =
                chc.children |>
                List.map(fun ch ->
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch
                    let unsigned =
                        match child.Type with
                        | AcnInteger int -> int.isUnsigned
                        | AcnNullType _ -> true
                        | _ -> raise (BugErrorException "???")
                    match pres with
                    | PresenceInt   (_, intVal) -> choiceDependencyIntPres_child v presentWhenName (lm.lg.asn1SccIntValueToString intVal.Value unsigned)
                    | PresenceStr   (_, strVal) -> raise(SemanticError(strVal.Location, "Unexpected presence condition. Expected integer, found string")))
            let updateStatement = choiceDependencyPres v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            let updateValues =
                chc.children |>
                List.filter(fun ch -> atc.testCaseTypeIDsMap.ContainsKey ch.Type.id) |>
                List.choose(fun ch ->
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    match pres with
                    | PresenceInt   (_, intVal) -> Some (TcvChoiceAlternativePresentWhenInt intVal.Value)
                    | PresenceStr   (_, strVal) -> None)
            match updateValues with
            | v1::[]    -> Some v1
            | _         -> None
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepPresenceStr   (relPath, chc, str)               ->
        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let pBase, relPath1 = resolveDepScope nestingScope pSrcRoot d.asn1Type
            let choicePath, checkPath = getAccessFromScopeNodeList relPath1 false lm pBase
            let arrsChildUpdates =
                chc.children |>
                List.map(fun ch ->
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch
                    match pres with
                    | PresenceInt   (_, intVal) ->
                        raise(SemanticError(intVal.Location, "Unexpected presence condition. Expected string, found integer"))
                    | PresenceStr   (_, strVal) ->
                        let arrNulls = [0 .. ((int str.maxSize.acn)- strVal.Value.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
                        let bytesStr = Array.append (System.Text.Encoding.ASCII.GetBytes strVal.Value) [| 0uy |]
                        choiceDependencyStrPres_child v presentWhenName strVal.Value bytesStr arrNulls)
            let updateStatement = choiceDependencyPres v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement v (initExpr r lm m child.Type)
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            let updateValues =
                chc.children |>
                List.filter(fun ch -> atc.testCaseTypeIDsMap.ContainsKey ch.Type.id) |>
                List.choose(fun ch ->
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    match pres with
                    | PresenceInt   (_, intVal) -> None
                    | PresenceStr   (_, strVal) -> Some (TcvChoiceAlternativePresentWhenStr strVal.Value))
            match updateValues with
            | v1::[]    -> Some v1
            | _         -> None
        let icdComments = []
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[]; testCaseFnc = testCaseFnc; localVariables=[]}), us
    | AcnDepChoiceDeterminant (enm, chc, isOptional) ->
        let updateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
            let choicePath, checkPath = getAccessFromScopeNodeList relPath false lm pBase
            let arrsChildUpdates =
                chc.children |>
                List.map(fun ch ->
                    let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = ch.Name.Value)
                    let choiceName = (lm.lg.getChoiceTypeDefinition chc.typeDef).typeName //chc.typeDef[Scala].typeName
                    choiceDependencyEnum_Item v ch.presentWhenName choiceName (lm.lg.getNamedItemBackendName (Some (defOrRef2 r m enm)) enmItem) isOptional)
            let updateStatement = choiceDependencyEnum v (choicePath.accessPath.joined lm.lg) (lm.lg.getAccess choicePath.accessPath) arrsChildUpdates isOptional (initExpr r lm m child.Type)
            // TODO: To remove this, getAccessFromScopeNodeList should be accounting for languages that rely on pattern matching for
            // accessing enums fields instead of a compiler-unchecked access
            let updateStatement2 =
                match ProgrammingLanguage.ActiveLanguages.Head with
                | Scala ->
                    match checkPath.Length > 0 && checkPath[0].Contains("isInstanceOf") with
                    | true -> (sprintf "val %s = %s.%s\n%s" (choicePath.accessPath.joined lm.lg) (checkPath[0].Replace("isInstanceOf", "asInstanceOf")) (choicePath.accessPath.joined lm.lg) updateStatement)
                    | false -> updateStatement
                | _ -> updateStatement
            match checkPath with
            | []    -> updateStatement2
            | _     -> checkAccessPath checkPath updateStatement2 v (initExpr r lm m child.Type)
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        let icdComments =
            let aaa = sprintf "Used as a presence determinant for %s " (chc.typeDef[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)
            [aaa]
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; icdComments=icdComments; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us

and getUpdateFunctionUsedInEncoding (r: Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm: LanguageMacros) (m: Asn1AcnAst.Asn1Module) (acnChildOrAcnParameterId) (us:State) : (AcnChildUpdateResult option*State)=
    let multiAcnUpdate       = lm.acn.MultiAcnUpdate

    match deps.acnDependencies |> List.filter(fun d -> d.determinant.id = acnChildOrAcnParameterId) with
    | []  ->
        None, us
    | d1::[]    ->
        let ret, ns = handleSingleUpdateDependency r deps lm m d1 us
        ret, ns
    | d1::dds         ->
        let _errCodeName = ToC ("ERR_ACN" + (Encode.suffix.ToUpper()) + "_UPDATE_" + ((acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errCode, us = getNextValidErrorCode us _errCodeName None

        let ds = d1::dds
        let c_name0 = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) 0
        let localVars (child: AcnChild) =
            ds |>
            List.mapi(fun i d1 ->
                let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                let childLv =
                    if lm.lg.decodingKind = Copy then []
                    else [AcnInsertedChild (c_name, child.typeDefinitionBodyWithinSeq, child.initExpression)]
                let initLv =
                    if lm.lg.usesWrappedOptional then []
                    else [BooleanLocalVariable (c_name+"_is_initialized", Some lm.lg.FalseLiteral)]
                childLv@initLv) |>
            List.collect(fun lvList -> lvList |> List.map (fun lv -> lm.lg.getLocalVariableDeclaration  lv))
        let localUpdateFuns,ns =
            ds |>
            List.fold(fun (updates, ns) d1 ->
                let f1, nns = handleSingleUpdateDependency r deps lm m d1 ns
                updates@[f1], nns) ([],us)
        let restErrCodes = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.errCodes)
        let restLocalVariables = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.localVariables)
        let icdComments = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.icdComments)
        let multiUpdateFunc (child: AcnChild) (nestingScope: NestingScope) (vTarget : CodegenScope) (pSrcRoot : CodegenScope) =
            let v = lm.lg.getValue vTarget.accessPath
            let arrsLocalUpdateStatements =
                localUpdateFuns |>
                List.mapi(fun i fn ->
                    let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let lv = {CodegenScope.modName = vTarget.modName; accessPath = AccessPath.valueEmptyPath c_name}
                    match fn with
                    | None      -> None
                    | Some fn   -> Some(fn.updateAcnChildFnc child nestingScope lv pSrcRoot)) |>
                List.choose id

            let isAlwaysInit (d: AcnDependency): bool =
                match d.dependencyKind with
                | AcnDepRefTypeArgument p ->
                    // Last item is the determinant, and the second-to-last is the field referencing the determinant
                    not p.id.dropLast.lastItemIsOptional
                | AcnDepChoiceDeterminant (_, c, isOpt) -> not isOpt
                | _ -> true

            let firstAlwaysInit = ds |> List.tryFind isAlwaysInit
            let arrsGetFirstIntValue =
                let ds2 =
                    match firstAlwaysInit with
                    | Some fst when lm.lg.usesWrappedOptional -> [fst]
                    | _ -> ds
                ds2 |>
                List.mapi (fun i d ->
                    let cmp = getDeterminantTypeUpdateMacro lm d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
                    let choicePath, _ = getAccessFromScopeNodeList relPath d.dependencyKind.isString lm pBase
                    cmp v vi (choicePath.accessPath.joined lm.lg) (i=0) (ds2.Length = 1))
            let arrsLocalCheckEquality =
                ds |>
                List.mapi (fun i d ->
                    let cmp = getDeterminantTypeCheckEqual lm d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let pBase, relPath = resolveDepScope nestingScope pSrcRoot d.asn1Type
                    let choicePath, _ = getAccessFromScopeNodeList relPath d.dependencyKind.isString lm pBase
                    cmp v vi (choicePath.accessPath.joined lm.lg) (isAlwaysInit d))
            let updateStatement = multiAcnUpdate (vTarget.accessPath.joined lm.lg) c_name0 (errCode.errCodeName) (localVars child) arrsLocalUpdateStatements arrsGetFirstIntValue firstAlwaysInit.IsSome arrsLocalCheckEquality (initExpr r lm m child.Type)
            updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            let updateValues =
                localUpdateFuns |> List.map(fun z -> match z with None -> None | Some res -> res.testCaseFnc atc)
            match updateValues |> Seq.exists(fun z -> z.IsNone) with
            | true  -> None //at least one update is not present
            | false ->
                match updateValues |> List.choose id with
                | []        -> None
                | u1::us    ->
                    match us |> Seq.exists(fun z -> z <> u1) with
                    | true  -> None
                    | false -> Some u1

        let ret = Some(({AcnChildUpdateResult.updateAcnChildFnc = multiUpdateFunc; icdComments=icdComments; errCodes=errCode::restErrCodes ; testCaseFnc = testCaseFnc; localVariables = restLocalVariables}))
        ret, ns

type private SequenceChildStmt = {
    body: string option
    lvs: LocalVariable list
    errCodes: ErrorCode list
    userDefinedFunctions : UserDefinedFunction list
    icdComments : string list
}
type private SequenceChildState = {
    us: State
    childIx: bigint
    uperAccBits: bigint
    acnAccBits: bigint
}
type private SequenceChildResult = {
    stmts: SequenceChildStmt list
    resultExpr: string option
    existVar: string option
    props: SequenceChildProps
    auxiliaries: string list
    icdResult : ((IcdRow list) * (IcdTypeAss list))
} with
    member this.joinedBodies (lm:LanguageMacros) (codec:CommonTypes.Codec): string option =
        this.stmts |> List.choose (fun s -> s.body) |> nestChildItems lm codec

let createSequenceFunction_inline (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (acnPrms:DastAcnParameter list) (us:State)  =
    (*
        1. all Acn inserted children are declared as local variables in the encoded and decode functions (declaration step)
        2. all Acn inserted children must be initialized appropriately in the encoding phase
    *)
    // stg macros
    let sequence_presence_optChild                      = lm.acn.sequence_presence_optChild
    let sequence_presence_optChild_pres_bool            = lm.acn.sequence_presence_optChild_pres_bool
    let sequence_presence_optChild_pres_acn_expression  = lm.acn.sequence_presence_optChild_pres_acn_expression
    let sequence_mandatory_child                        = lm.acn.sequence_mandatory_child
    let sequence_optional_child                         = lm.acn.sequence_optional_child
    let sequence_always_present_child                   = lm.acn.sequence_always_present_child
    let sequence_always_absent_child                    = lm.acn.sequence_always_absent_child
    let sequence_default_child                          = lm.acn.sequence_default_child
    let sequence_acn_child                              = lm.acn.sequence_acn_child
    let sequence_call_post_encoding_function            = lm.acn.sequence_call_post_encoding_function
    let sequence_call_post_decoding_validator           = lm.acn.sequence_call_post_decoding_validator
    let sequence_save_bitStream_start                   = lm.acn.sequence_save_bitStream_start
    let bitStreamName                                   = lm.lg.bitStreamName
    let sequence_call_post_encoding_function_prototype  = lm.acn.sequence_call_post_encoding_function_prototype
    let sequence_call_post_decoding_validator_prototype = lm.acn.sequence_call_post_decoding_validator_prototype

    let acnExpressionToBackendExpression (seq:Asn1AcnAst.Sequence) (pSeq:CodegenScope) (exp:AcnExpression) =
        let unaryNotOperator    = lm.lg.unaryNotOperator
        let modOp               = lm.lg.modOp
        let eqOp                = lm.lg.eqOp
        let neqOp               = lm.lg.neqOp
        let andOp               = lm.lg.andOp
        let orOp                = lm.lg.orOp

        let printUnary op chExpPriority expStr minePriority =
            minePriority, if chExpPriority >= minePriority then sprintf "%s(%s)" op expStr else sprintf "%s%s" op expStr
        let printBinary op (chExpPriority1, expStr1) (chExpPriority2, expStr2) minePriority =
            minePriority, (if chExpPriority1 >= minePriority then "(" + expStr1 + ")" else expStr1 ) + " " + op + " " + (if chExpPriority2 >= minePriority then "(" + expStr2 + ")" else expStr2 )


        let rec getChildResult (seq:Asn1AcnAst.Sequence) (pSeq:CodegenScope) (RelativePath lp) =
            match lp with
            | []    -> raise(BugErrorException "empty relative path")
            | x1::xs ->
                match seq.children |> Seq.tryFind(fun c -> c.Name = x1) with
                | None ->
                    raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin "."))))
                | Some ch ->
                    match ch with
                    | Asn1AcnAst.AcnChild ch  -> raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'. Expecting an ASN.1 child" (lp |> Seq.StrJoin "."))))
                    | Asn1AcnAst.Asn1Child ch  ->
                        match ch.Type.ActualType.Kind with
                        | Asn1AcnAst.Integer        _
                        | Asn1AcnAst.Real           _
                        | Asn1AcnAst.Boolean        _  -> {pSeq with accessPath = lm.lg.getSeqChild pSeq.accessPath (lm.lg.getAsn1ChildBackendName0 ch) false ch.Optionality.IsSome}
                        | Asn1AcnAst.Sequence s when xs.Length > 1 -> getChildResult s {pSeq with accessPath = lm.lg.getSeqChild pSeq.accessPath (lm.lg.getAsn1ChildBackendName0 ch) false ch.Optionality.IsSome} (RelativePath xs)
                        | _                 -> raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin "."))))


        let ret =
            AcnGenericTypes.foldAcnExpression
                (fun i s -> ( (0, i.Value.ToString()) , 0))
                (fun i s -> ( (0,"") , 0))
                (fun i s -> ( (0, i.Value.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo)) , 0))
                (fun i s -> ( (0, i.Value.ToString().ToLower()) , 0))
                (fun lf s ->
                    let plf = getChildResult seq pSeq lf
                    (0, (plf.accessPath.joined lm.lg)) , 0)
                (fun loc (chExpPriority, expStr) s -> printUnary unaryNotOperator chExpPriority expStr 1, 0) //NotUnaryExpression
                (fun loc (chExpPriority, expStr) s -> printUnary "-" chExpPriority expStr 1, 0)//MinusUnaryExpression
                (fun l e1 e2  s -> printBinary "+" e1 e2 3, 0 )
                (fun l e1 e2  s -> printBinary "-" e1 e2 3, 0 )
                (fun l e1 e2  s -> printBinary "*" e1 e2 2, 0 )
                (fun l e1 e2  s -> printBinary "/" e1 e2 2, 0 )
                (fun l e1 e2  s -> printBinary modOp e1 e2 2, 0 )
                (fun l e1 e2  s -> printBinary "<=" e1 e2 4, 0 )
                (fun l e1 e2  s -> printBinary "<" e1 e2 4, 0 )
                (fun l e1 e2  s -> printBinary ">=" e1 e2 4, 0 )
                (fun l e1 e2  s -> printBinary ">" e1 e2 4, 0 )
                (fun l e1 e2  s -> printBinary eqOp e1 e2 5, 0 )
                (fun l e1 e2  s -> printBinary neqOp e1 e2 5, 0 )
                (fun lc e1 e2  s -> printBinary andOp  e1 e2 6, 0 )
                (fun lc e1 e2  s -> printBinary orOp e1 e2 6, 0 )
                exp 0 |> fst |> snd

        ret

    //let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName

    let acnChildren = children |>  List.choose(fun x -> match x with AcnChild z -> Some z | Asn1Child _ -> None)
    let asn1Children = children |>  List.choose(fun x -> match x with Asn1Child z -> Some z | AcnChild _ -> None)
    let sPresenceBitIndexMap  =
        asn1Children |>
        List.filter(fun c -> match c.Optionality with Some(Optional _) -> true | _ -> false) |>
        List.mapi (fun i c -> (c.Name.Value, i)) |>
        Map.ofList
    let uperPresenceMask =
        match sPresenceBitIndexMap.IsEmpty with
        | true -> []
        | false ->
            [{IcdRow.fieldName = "Presence Mask"; comments = [$"Presence bit mask"]; sPresent="always";sType=IcdPlainType "bit mask"; sConstraint=None; minLengthInBits = sPresenceBitIndexMap.Count.AsBigInt ;maxLengthInBits=sPresenceBitIndexMap.Count.AsBigInt;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]

    let icd_asn1_child (c:Asn1Child) (extra_comments:string list) : ((IcdRow list) * (IcdTypeAss list)) =
        let optionality =
            match c.Optionality with
            | None                -> "always"
            | Some(AlwaysAbsent ) -> "never"
            | Some(AlwaysPresent) -> "always"
            | Some(Optional  opt) ->
                match opt.acnPresentWhen with
                | None                                      -> $"when bit %d{sPresenceBitIndexMap[c.Name.Value]} is set in the uPER bit mask"
                | Some(PresenceWhenBool relPath)            -> $"when %s{relPath.AsString} is true"
                | Some(PresenceWhenBoolExpression acnExp)   ->
                    let dummyScope = {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
                    let retExp = acnExpressionToBackendExpression o dummyScope acnExp
                    $"when %s{retExp}"
        let comments = (c.Comments |> Seq.toList)@extra_comments
        let childIcdTas = c.Type.icdTas
        //let isRef = match c.Type.Kind with ReferenceType _ -> true | _ -> false
        match c.Type.icdTas with
        | Some childIcdTas ->
            match childIcdTas.canBeEmbedded   with
            | true  ->
                let chRows, _ = childIcdTas.createRowsFunc c.Name.Value optionality comments
                chRows, []
            | false ->
                let sType = TypeHash childIcdTas.hash
                [{IcdRow.fieldName = c.Name.Value; comments = comments; sPresent=optionality;sType=sType; sConstraint=None; minLengthInBits = c.Type.acnMinSizeInBits; maxLengthInBits=c.Type.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}], [childIcdTas]
        | None ->
            [], []

    let icd_acn_child (c:AcnChild) (extra_comments:string list) : ((IcdRow list) * (IcdTypeAss list))=
        let icdResult =
            let dummyNestingScope = NestingScope.init 0I 0I []
            let p : CodegenScope = {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath ""}
            let funcResult = c.funcBody Encode [] dummyNestingScope p "Dummy_body_bitstreamPositions" 
            match funcResult with
            | None -> None
            | Some bodyResult -> bodyResult.icdResult
        match icdResult with
        | None -> [], []
        | Some icdArgAux ->
            icdArgAux.rowsFunc c.Name.Value "always" extra_comments



    let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let acnlocalVariablesCh =
            acnChildren |>
            List.filter(fun x -> match x.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> true) |>
            List.collect(fun x ->
                let childLv =
                    match lm.lg.decodingKind with
                    | InPlace -> [AcnInsertedChild(x.c_name, x.typeDefinitionBodyWithinSeq, x.initExpression)]
                    | Copy -> []
                match codec with
                | Encode ->
                    let initLv =
                        if lm.lg.usesWrappedOptional then []
                        else [BooleanLocalVariable(x.c_name+"_is_initialized", Some lm.lg.FalseLiteral)]
                    childLv@initLv
                | Decode -> childLv)

        let acnlocalVariablesPrms =
            match t.id.tasInfo with
            | Some  _ -> [] // if the encoding type is a top level type (i.e. TAS) then the encoding parameters are transformed into function parameters and not local variables.
            | None    -> [] // acnPrms |>  List.map(fun x -> AcnInsertedChild(x.c_name, x.typeDefinitionBodyWithinSeq))
        let acnlocalVariables = acnlocalVariablesCh @ acnlocalVariablesPrms
        //let acnParams =  r.acnParameters |> List.filter(fun  prm -> prm.ModName )

        let printPresenceBit (child:Asn1Child) (existVar: string option)=
            match child.Optionality with
            | Some (Asn1AcnAst.Optional opt)   ->
                match opt.acnPresentWhen with
                | None ->
                    assert (codec = Encode || existVar.IsSome)
                    Some (sequence_presence_optChild (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.getAsn1ChildBackendName child) existVar errCode.errCodeName codec)
                | Some _ -> None
            | _ -> None

        let localVariables = acnlocalVariables
        let td = lm.lg.getSequenceTypeDefinition o.typeDef

        let hasOwnPostEncoding =
            o.acnProperties.postEncodingFunction.IsSome || o.acnProperties.preDecodingFunction.IsSome
        let bitStreamPositionsLocalVar =
            match hasOwnPostEncoding with
            | true  -> sprintf "bitStreamPositions_%s_%d" p.accessPath.lastIdOrArr (p.accessPath.SequenceOfLevel + 1)
            | false ->
                match nestingScope.parentSavePositionVar with
                | Some parentVar -> parentVar
                | None -> sprintf "bitStreamPositions_%s_%d" p.accessPath.lastIdOrArr (p.accessPath.SequenceOfLevel + 1)

        let localVariables, post_encoding_function, soBitStreamPositionsLocalVar, soSaveInitialBitStrmStatement =
            let bsPosStart = sprintf "bitStreamPositions_%s_start%d" p.accessPath.lastIdOrArr (p.accessPath.SequenceOfLevel + 1)
            match o.acnProperties.postEncodingFunction with
            | Some (PostEncodingFunction (modFncName, fncName)) when codec = Encode  ->
                let actualFncName =
                    match lm.lg.hasModules with
                    | false ->  (ToC fncName.Value)
                    | true ->
                        match modFncName with
                        | None -> (ToC (r.args.mappingFunctionsModule.orElse "")) + "." + (ToC fncName.Value)
                        | Some modFncName -> (ToC modFncName.Value) + "." + (ToC fncName.Value)

                let fncCall = sequence_call_post_encoding_function (lm.lg.getPointer p.accessPath) (actualFncName) bsPosStart bitStreamPositionsLocalVar
                let fncCallPrototype = sequence_call_post_encoding_function_prototype td.typeName actualFncName td.extension_function_positions

                let initialBitStrmStatement = sequence_save_bitStream_start bsPosStart codec
                [AcnInsertedChild(bitStreamPositionsLocalVar, td.extension_function_positions, ""); AcnInsertedChild(bsPosStart, bitStreamName, "")]@localVariables, Some (fncCall,fncCallPrototype), Some bitStreamPositionsLocalVar, Some initialBitStrmStatement
            | _ ->
                match o.acnProperties.preDecodingFunction with
                | Some (PreDecodingFunction (modFncName, fncName)) when codec = Decode  ->
                    let actualFncName =
                        match lm.lg.hasModules with
                        | false -> (ToC fncName.Value)
                        | true ->
                            match modFncName with
                            | None -> (ToC (r.args.mappingFunctionsModule.orElse "")) + "." + (ToC fncName.Value)
                            | Some modFncName -> (ToC modFncName.Value) + "." + (ToC fncName.Value)
                    let fncCall = sequence_call_post_decoding_validator (lm.lg.getPointer p.accessPath) (actualFncName) bsPosStart  bitStreamPositionsLocalVar
                    let fncCallPrototype = sequence_call_post_decoding_validator_prototype td.typeName actualFncName td.extension_function_positions
                    let initialBitStrmStatement = sequence_save_bitStream_start bsPosStart codec
                    [AcnInsertedChild(bitStreamPositionsLocalVar, td.extension_function_positions, ""); AcnInsertedChild(bsPosStart, bitStreamName, "")]@localVariables, Some (fncCall,fncCallPrototype), Some bitStreamPositionsLocalVar, Some initialBitStrmStatement
                | _ ->  localVariables, None, None, None

        let handleChild (s: SequenceChildState) (childInfo: SeqChildInfo): SequenceChildResult * SequenceChildState =
            // This binding is suspect, isn't it
            //let stateHash = getStateHash s.us
            //printfn "child is %s" childInfo.Name
            //printf "State hash: %s\n" stateHash
            let us = s.us
            let soSaveBitStrmPosStatement = None
            let childNestingScope =
                {nestingScope with
                    nestingLevel = nestingScope.nestingLevel + 1I
                    nestingIx = nestingScope.nestingIx + s.childIx
                    uperRelativeOffset = s.uperAccBits
                    uperOffset = nestingScope.uperOffset + s.uperAccBits
                    acnRelativeOffset = s.acnAccBits
                    acnOffset = nestingScope.acnOffset + s.acnAccBits
                    parents = (p, t) :: nestingScope.parents
                    parentSavePositionVar =
                        match hasOwnPostEncoding with
                        | true  -> Some bitStreamPositionsLocalVar
                        | false -> nestingScope.parentSavePositionVar}

            match childInfo with
            | Asn1Child child   ->
                let childTypeDef = child.Type.typeDefinitionOrReference.longTypedefName2 lm.lg.hasModules
                let childName = lm.lg.getAsn1ChildBackendName child
                let chFunc = child.Type.getAcnFunction codec
                let childSel = lm.lg.getSeqChild p.accessPath childName child.Type.isIA5String child.Optionality.IsSome
                let childP =
                    let newArg = if lm.lg.usesWrappedOptional && childSel.isOptional && codec = Encode then childSel.asLast else childSel
                    {p with accessPath = newArg}
                let childContentResult, ns1 =
                    match chFunc with
                    | Some chFunc -> chFunc.funcBodyAsSeqComp us [] childNestingScope childP childName bitStreamPositionsLocalVar
                    | None -> None, us

                //handle present-when acn property
                let presentWhenStmts, presentWhenLvs, presentWhenErrs, existVar, ns2 =
                    match child.Optionality with
                    | Some (Asn1AcnAst.Optional opt) ->
                        match opt.acnPresentWhen with
                        | None ->
                            match codec with
                            | Encode ->
                                // We do not need the `exist` variable for encoding as we use the child `exist` bit
                                None, [], [], None, ns1
                            | Decode ->
                                let existVar = ToC (child._c_name + "_exist")
                                let lv = FlagLocalVariable (existVar, None)
                                None, [lv], [], Some existVar, ns1
                        | Some (PresenceWhenBool _) ->
                            match codec with
                            | Encode -> None, [], [], None, ns1
                            | Decode ->
                                let getExternalField (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
                                    let filterDependency (d:AcnDependency) =
                                        match d.dependencyKind with
                                        | AcnDepPresenceBool   -> true
                                        | _                    -> false
                                    getExternalField0 r deps asn1TypeIdWithDependency filterDependency
                                let extField = getExternalField r deps child.Type.id
                                let body (p: CodegenScope) (existVar: string option): string =
                                    assert existVar.IsSome
                                    sequence_presence_optChild_pres_bool (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName existVar.Value codec
                                Some body, [], [], Some extField, ns1
                        | Some (PresenceWhenBoolExpression exp)    ->
                            let _errCodeName = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_PRESENT_WHEN_EXP_FAILED")
                            let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None
                            let retExp = acnExpressionToBackendExpression o p exp
                            let existVar =
                                if codec = Decode then Some (ToC (child._c_name + "_exist"))
                                else None
                            let lv = existVar |> Option.toList |> List.map (fun v -> FlagLocalVariable (v, None))
                            let body (p: CodegenScope) (existVar: string option): string =
                                sequence_presence_optChild_pres_acn_expression (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName retExp existVar errCode.errCodeName codec
                            Some body, lv, [errCode], existVar, ns1a
                    | _ -> None, [], [], None, ns1

                let childBody, childLvs, childUserDefFuncs, childErrs, childResultExpr, auxiliaries, ns3 =
                    match childContentResult with
                    | None ->
                        // Copy-decoding expects to have a result expression (even if unused), so we pick the initExpression
                        let childResultExpr =
                            match codec, lm.lg.decodingKind with
                            | Decode, Copy -> Some (child.Type.initFunction.initExpressionFnc ())
                            | _ -> None
                        match child.Optionality with
                        | Some Asn1AcnAst.AlwaysPresent     ->
                            let childBody (p: CodegenScope) (existVar: string option): string =
                                sequence_always_present_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName None childResultExpr childTypeDef soSaveBitStrmPosStatement codec
                            Some childBody, [], [], [], childResultExpr, [], ns2
                        | _ -> None, [], [], [], childResultExpr, [], ns2
                    | Some childContent ->
                        let childBody (p: CodegenScope) (existVar: string option): string =
                            match child.Optionality with
                            | None ->
                                sequence_mandatory_child childName childContent.funcBody soSaveBitStrmPosStatement codec
                            | Some Asn1AcnAst.AlwaysAbsent ->
                                sequence_always_absent_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName childContent.funcBody childTypeDef soSaveBitStrmPosStatement codec
                            | Some Asn1AcnAst.AlwaysPresent ->
                                sequence_always_present_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName (Some childContent.funcBody) childContent.resultExpr childTypeDef soSaveBitStrmPosStatement codec
                            | Some (Asn1AcnAst.Optional opt)   ->
                                assert (codec = Encode || existVar.IsSome)
                                let pp, _ = joinedOrAsIdentifier lm codec p
                                match opt.defaultValue with
                                | None ->
                                    sequence_optional_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody existVar childContent.resultExpr childTypeDef soSaveBitStrmPosStatement codec
                                | Some v ->
                                    let defInit= child.Type.initFunction.initByAsn1Value childP (mapValue v).kind
                                    sequence_default_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody defInit existVar childContent.resultExpr childTypeDef soSaveBitStrmPosStatement codec
                        let lvs =
                            match child.Optionality with
                            | Some Asn1AcnAst.AlwaysAbsent -> []
                            | _ -> childContent.localVariables
                        Some childBody, lvs, childContent.userDefinedFunctions, childContent.errCodes, childContent.resultExpr, childContent.auxiliaries, ns2

                let optAux, theCombinedBody =
                    if presentWhenStmts.IsNone && childBody.IsNone then [], None
                    else
                        let combinedBody (p: CodegenScope) (existVar: string option): string =
                            ((presentWhenStmts |> Option.toList) @ (childBody |> Option.toList) |> List.map (fun f -> f p existVar)) |> Seq.StrJoin "\n"
                        let soc = {SequenceOptionalChild.t = t; sq = o; child = child; existVar = existVar; p = {p with accessPath = childSel}; nestingScope = childNestingScope; childBody = combinedBody}
                        let optAux, theCombinedBody = lm.lg.generateOptionalAuxiliaries r ACN soc codec
                        optAux, Some theCombinedBody

                let stmts = {body = theCombinedBody; lvs = presentWhenLvs @ childLvs;  userDefinedFunctions=childUserDefFuncs; errCodes = presentWhenErrs @ childErrs; icdComments = []}
                let props = {info=childInfo.toAsn1AcnAst; sel=childSel; uperMaxOffset=s.uperAccBits; acnMaxOffset=s.acnAccBits}
                let icdResult = icd_asn1_child child stmts.icdComments
                let res = {stmts=[stmts]; resultExpr=childResultExpr; existVar=existVar; props=props; auxiliaries=auxiliaries @ optAux; icdResult=icdResult}
                let newAcc = {us=ns3; childIx=s.childIx + 1I; uperAccBits=s.uperAccBits + child.uperMaxSizeInBits; acnAccBits=s.acnAccBits + child.acnMaxSizeInBits}
                res, newAcc
            | AcnChild acnChild ->
                //handle updates
                let childP = {CodegenScope.modName = p.modName; accessPath= AccessPath.valueEmptyPath (getAcnDeterminantName acnChild.id)}

                let updateStatement, ns1 =
                    match codec with
                    | Encode ->
                        let pRoot = p
                        let updateStatement, lvs, errCodes, icdComments =
                            match acnChild.funcUpdateStatement with
                            | Some funcUpdateStatement -> Some (funcUpdateStatement.updateAcnChildFnc acnChild childNestingScope childP pRoot), funcUpdateStatement.localVariables, funcUpdateStatement.errCodes, funcUpdateStatement.icdComments
                            | None                     -> None, [], [], []
                        Some {body=updateStatement; lvs=lvs; errCodes=errCodes; userDefinedFunctions = [];  icdComments=icdComments}, us
                    | Decode -> None, us

                //acn child encode/decode
                let childEncDecStatement, auxiliaries, ns2 =
                    let chFunc = acnChild.funcBody codec
                    let childContentResult = chFunc [] childNestingScope childP bitStreamPositionsLocalVar
                    match childContentResult with
                    | None              -> None, [], ns1
                    | Some childContent ->
                        match codec with
                        | Encode   ->
                            match acnChild.Type with
                            | Asn1AcnAst.AcnNullType _   ->
                                let childBody = Some (sequence_mandatory_child acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement codec)
                                Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=childContent.errCodes;icdComments=[]}, childContent.auxiliaries, ns1

                            | _             ->
                                let _errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((acnChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_UNINITIALIZED")
                                let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None
                                let childBody = Some (sequence_acn_child acnChild.c_name childContent.funcBody errCode.errCodeName soSaveBitStrmPosStatement codec)
                                Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions;  errCodes=errCode::childContent.errCodes; icdComments=[]}, childContent.auxiliaries, ns1a
                        | Decode    ->
                            let childBody = Some (sequence_mandatory_child acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement codec)
                            Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=childContent.errCodes; icdComments=[]}, childContent.auxiliaries, ns1

                let stmts = (updateStatement |> Option.toList)@(childEncDecStatement |> Option.toList)
                let icdComments = stmts |> List.collect(fun z -> z.icdComments)
                // Note: uperMaxSizeBits and uperAccBits here do not make sense since we are in ACN
                let props = {info=childInfo.toAsn1AcnAst; sel=childP.accessPath; uperMaxOffset=s.uperAccBits; acnMaxOffset=s.acnAccBits}
                let icdResult = icd_acn_child acnChild icdComments
                let res =  {stmts=stmts; resultExpr=None; existVar=None; props=props; auxiliaries=auxiliaries; icdResult=icdResult}
                let newAcc = {us=ns2; childIx=s.childIx + 1I; uperAccBits=s.uperAccBits; acnAccBits=s.acnAccBits + acnChild.Type.acnMaxSizeInBits}
                res, newAcc




        // find acn inserted fields, which are not NULL types and which have no dependency.
        // For those fields we should generated no anc encode/decode function
        // Otherwise, the encoding function is wrong since an uninitialized value is encoded.
        let existsAcnChildWithNoUpdates =
            if r.args.acnDeferred then []
            else
                acnChildren |>
                List.filter (fun acnChild -> match acnChild.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> acnChild.funcUpdateStatement.IsNone)
        let saveInitialBitStrmStatements = soSaveInitialBitStrmStatement |> Option.toList
        let nbPresenceBits = asn1Children |> List.sumBy (fun c ->
            match c.Optionality with
            | Some (Optional opt) -> if opt.acnPresentWhen.IsNone then 1I else 0I
            | _ -> 0I
        )
        let (childrenStatements00: SequenceChildResult list), scs = children |> foldMap handleChild {us=us; childIx=nbPresenceBits; uperAccBits=nbPresenceBits; acnAccBits=nbPresenceBits}
        let ns = scs.us
        let childrenStatements0 = childrenStatements00 |> List.collect (fun xs -> xs.stmts)

        let presenceBits = ((List.zip children childrenStatements00)
            |> List.choose (fun (child, res) ->
                match child with
                | Asn1Child asn1 -> printPresenceBit asn1 res.existVar
                | AcnChild _ -> None))
        let seqProofGen =
            let children = childrenStatements00 |> List.map (fun xs -> xs.props)
            {SequenceProofGen.t = t; sq = o; sel = p.accessPath; acnOuterMaxSize = nestingScope.acnOuterMaxSize; uperOuterMaxSize = nestingScope.uperOuterMaxSize;
            nestingLevel = nestingScope.nestingLevel; nestingIx = nestingScope.nestingIx;
            uperMaxOffset = nestingScope.uperOffset; acnMaxOffset = nestingScope.acnOffset;
            acnSiblingMaxSize = nestingScope.acnSiblingMaxSize; uperSiblingMaxSize = nestingScope.uperSiblingMaxSize;
            children = children}
        let allStmts =
            let presenceBits = presenceBits |> List.map Some
            let children = childrenStatements00 |> List.map (fun s -> s.joinedBodies lm codec)
            presenceBits @ children
        let childrenStatements = lm.lg.generateSequenceChildProof r ACN allStmts seqProofGen codec

        let childrenLocalvars = childrenStatements0 |> List.collect(fun s -> s.lvs)
        let childrenUserDefFuncs = (childrenStatements0 |> List.collect(fun s -> s.userDefinedFunctions))@(post_encoding_function |> Option.map (fun (_,f) -> UserPostEncodingFunction f) |> Option.toList)
        let childrenExistVar = childrenStatements00 |> List.choose(fun res -> res.existVar)
        let childrenResultExpr = childrenStatements00 |> List.choose(fun res -> res.resultExpr)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun s -> s.errCodes)
        let childrenAuxiliaries = childrenStatements00 |> List.collect (fun s -> s.auxiliaries)

        let resultExpr, seqBuild=
            match codec, lm.lg.decodingKind with
            | Decode, Copy ->
                // If we are Decoding with Copy decoding kind, then all children `resultExpr`
                // must be defined as well (i.e. we must have the same number of `resultExpr` as children)
                // assert (childrenResultExpr.Length = asn1Children.Length)
                assert (childrenResultExpr.Length = asn1Children.Length)
                let existSeq =
                    if lm.lg.usesWrappedOptional || childrenExistVar.IsEmpty then []
                    else
                        let existTd = (lm.lg.getSequenceTypeDefinition o.typeDef).exist
                        [lm.init.initSequenceExpr existTd childrenExistVar []]
                let resultExpr = p.accessPath.asIdentifier
                Some resultExpr, [lm.uper.sequence_build resultExpr (typeDefinition.longTypedefName2 lm.lg.hasModules) p.accessPath.isOptional (existSeq@childrenResultExpr)]
            | _ -> None, []
        let proof = lm.lg.generateSequenceProof r ACN t o nestingScope p.accessPath codec
        let aux = lm.lg.generateSequenceAuxiliaries r ACN t o nestingScope p.accessPath codec
        let seqContent =  (saveInitialBitStrmStatements@childrenStatements@(post_encoding_function |> Option.map fst |> Option.toList)@seqBuild@proof) |> nestChildItems lm codec

        let icdFnc fieldName sPresent comments  =
            let chRows0, compositeChildren0 = childrenStatements00 |> List.map (fun s -> s.icdResult) |> List.unzip
            let chRows = chRows0 |> List.collect id
            let compositeChildren = compositeChildren0 |> List.collect id
            uperPresenceMask@chRows |> List.mapi(fun i r -> {r with idxOffset = Some (i+1)}), compositeChildren
        let icd = {IcdArgAux.canBeEmbedded = false; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}

        match existsAcnChildWithNoUpdates with
        | []     ->
            match seqContent with
            | None  ->
                match codec with
                | Encode -> None, ns
                | Decode ->
                    match lm.lg.decodeEmptySeq (p.accessPath.joined lm.lg) with
                    | None -> None, ns
                    | Some decodeEmptySeq ->
                        Some ({AcnFuncBodyResult.funcBody = decodeEmptySeq; errCodes = errCode::childrenErrCodes; userDefinedFunctions=childrenUserDefFuncs; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced= false; bBsIsUnReferenced=true; resultExpr=Some decodeEmptySeq; auxiliaries=childrenAuxiliaries @ aux; icdResult = Some icd}), ns
            | Some ret ->
                Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; userDefinedFunctions=childrenUserDefFuncs; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced= false; bBsIsUnReferenced=(o.acnMaxSizeInBits = 0I); resultExpr=resultExpr; auxiliaries=childrenAuxiliaries @ aux; icdResult = Some icd}), ns

        | errChild::_      ->
            let determinantUsage =
                match errChild.Type with
                | Asn1AcnAst.AcnInteger               _-> "length"
                | Asn1AcnAst.AcnNullType              _-> raise(BugErrorException "existsAcnChildWithNoUpdates")
                | Asn1AcnAst.AcnBoolean               _-> "presence"
                | Asn1AcnAst.AcnReferenceToEnumerated _-> "presence"
                | Asn1AcnAst.AcnReferenceToIA5String  _-> "presence"
            let errMessage = sprintf "Unused ACN inserted field.
                All fields inserted at ACN level (except NULL fields) must act as decoding determinants of other types.
                The field '%s' must either be removed or used as %s determinant of another ASN.1 type." errChild.Name.Value determinantUsage
            raise(SemanticError(errChild.Name.Location, errMessage))
            //let loc = errChild.Name.Location
            //Console.Out.WriteLine (FrontEntMain.formatSemanticWarning loc errMessage)
            //None, ns

    let isTestVaseValid (atc:AutomaticTestCase) =
        //an automatic test case value is valid
        //if all ach children can be update during the encoding from the value
        acnChildren |>
        List.filter (fun acnChild -> match acnChild.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> true) |>
        Seq.forall(fun acnChild ->
            match acnChild.funcUpdateStatement with
            | Some funcUpdateStatement -> (funcUpdateStatement.testCaseFnc atc).IsSome
            | None                     -> false)
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)

    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody isTestVaseValid soSparkAnnotations  [] us



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

    let typeDefinitionName = defOrRef.longTypedefName2 lm.lg.hasModules//getTypeDefinitionName t.id.tasInfo typeDefinition
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
                            let icdRow = [{IcdRow.fieldName = c.Name.Value; comments = comments; sPresent=optionality;sType=sType; sConstraint=None; minLengthInBits = c.chType.acnMinSizeInBits; maxLengthInBits=c.chType.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]
                            let compChild = [childIcdTas]
                            icdRow, compChild
                    |None -> [],[]) |> List.unzip
        let chRows = chRows0 |> List.collect id
        let compositeChildren = compositeChildren0 |> List.collect id
        let icdRows = uperPresenceMask@chRows |> List.mapi(fun i r -> {r with idxOffset = Some (i+1)})
        icdRows, compositeChildren

    let icd = {IcdArgAux.canBeEmbedded = false; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=extraComment; scope="type"; name= None}


    let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let td = (lm.lg.getChoiceTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let acnSiblingMaxSize = children |> List.map (fun c -> c.chType.acnMaxSizeInBits) |> List.max
        let uperSiblingMaxSize = children |> List.map (fun c -> c.chType.uperMaxSizeInBits) |> List.max
        let handleChild (us:State) (idx:int) (child:ChChildInfo) =
            let chFunc = child.chType.getAcnFunction codec
            let sChildInitExpr = child.chType.initFunction.initExpressionFnc ()
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
                        if lm.lg.acn.choice_requires_tmp_decoding && codec = Decode then
                            {CodegenScope.modName = p.modName; accessPath = AccessPath.valueEmptyPath ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")}
                        else {p with accessPath = lm.lg.getChChild p.accessPath (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String}
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
                let sChildName = (lm.lg.getAsn1ChChildBackendName child)
                let sChildTypeDef = child.chType.typeDefinitionOrReference.longTypedefName2 lm.lg.hasModules //child.chType.typeDefinition.typeDefinitionBodyWithinSeq

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
                            match p.modName = a.modName with
                            | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}
                            | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName); typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}


                        let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = child.Name.Value)
                        Some (choiceChild_Enum (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.getNamedItemBackendName (Some (getDefOrRef enm)) enmItem) (lm.lg.presentWhenName (Some defOrRef) child) childContent_funcBody sChildName sChildTypeDef sChoiceTypeName sChildInitExpr codec)
                    | CEC_presWhen  ->
                        let handPresenceCond (cond:AcnGenericTypes.AcnPresentWhenConditionChoiceChild) =
                            match cond with
                            | PresenceInt  (relPath, intLoc)   ->
                                let extField = getExternalFieldChoicePresentWhen r deps t.id relPath
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
                                let extField = getExternalFieldChoicePresentWhen r deps t.id relPath
                                let arrNulls = [0 .. ((int strType.maxSize.acn) - strVal.Value.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
                                let bytesStr = Array.append (System.Text.Encoding.ASCII.GetBytes strVal.Value) [| 0uy |]
                                choiceChild_preWhen_str_condition extField strVal.Value arrNulls bytesStr
                        let conds = child.acnPresentWhenConditions |>List.map handPresenceCond
                        let pp, _ = joinedOrAsIdentifier lm codec p
                        Some (choiceChild_preWhen pp (lm.lg.getAccess p.accessPath) (lm.lg.presentWhenName (Some defOrRef) child) childContent_funcBody conds (idx=0) sChildName sChildTypeDef sChoiceTypeName sChildInitExpr codec)
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
                let extField = getExternalField r deps t.id
                choice_Enum pp access childrenStatements extField errCode.errCodeName codec, resultExpr
            | CEC_presWhen    -> choice_preWhen pp  access childrenStatements errCode.errCodeName codec, resultExpr
        let choiceContent = lm.lg.generateChoiceProof r ACN t o choiceContent p.accessPath codec
        let aux = lm.lg.generateChoiceAuxiliaries r ACN t o nestingScope p.accessPath codec
        Some ({AcnFuncBodyResult.funcBody = choiceContent; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; userDefinedFunctions=childrenUserDefFuncs; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=childrenAuxiliaries@aux; icdResult = Some icd}), ns


    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)


    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations [] us, ec

let emptyIcdFnc fieldName sPresent comments  = [],[]

let createReferenceFunction_inline (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
  let baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t.id "_ACN" codec

  //let td = lm.lg.getTypeDefinition t.FT_TypeDefinition
  let getNewSType (r:IcdRow) =
    (*
    let newType =
        match r.sType with
        | TypeHash hash   -> TypeHash hash
        | IcdPlainType plainType when r.rowType = FieldRow -> IcdPlainType (td.asn1Name + "(" + plainType + ")")
        | IcdPlainType plainType -> IcdPlainType plainType
    {r with sType = newType}
    *)
    r

  let icdFnc,extraComment, name  =
    match r.args.generateAcnIcd with
    | true  ->
        match o.encodingOptions with
        | None ->
            let name =
              match o.hasExtraConstrainsOrChildrenOrAcnArgs with
              | false -> None
              | true -> Some t.id.AsString.RDD
            match baseType.icdTas with
            | Some baseTypeIcdTas ->
                let icdFnc fieldName sPresent comments  =
                    let rows, comp = baseTypeIcdTas.createRowsFunc fieldName sPresent comments
                    rows |> List.map(fun r -> getNewSType r), comp

                icdFnc, baseTypeIcdTas.comments, name
            | None -> emptyIcdFnc, [], name

        | Some encOptions ->
            let lengthDetRow =
                match encOptions.acnEncodingClass with
                | SZ_EC_LENGTH_EMBEDDED  nSizeInBits ->
                    let sCommentUnit = match encOptions.octOrBitStr with ContainedInOctString -> "bytes" | ContainedInBitString -> "bits"

                    [ {IcdRow.fieldName = "Length"; comments = [$"The number of {sCommentUnit} used in the encoding"]; sPresent="always";sType=IcdPlainType "INTEGER"; sConstraint=None; minLengthInBits = nSizeInBits ;maxLengthInBits=nSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]
                | _ -> []
            match baseType.icdTas with
            | Some baseTypeIcdTas ->
                let icdFnc fieldName sPresent comments  =
                    let rows0, compChildren = baseTypeIcdTas.createRowsFunc fieldName sPresent comments
                    let rows = rows0 |> List.map getNewSType
                    lengthDetRow@rows |> List.mapi(fun i r -> {r with idxOffset = Some (i+1)}), compChildren
                icdFnc, ("OCTET STING CONTAINING BY"::baseTypeIcdTas.comments), Some (t.id.AsString.RDD + "_OCT_STR" )
            | None -> emptyIcdFnc, [], None
    | false -> emptyIcdFnc, [], None


  let icd =
    match baseType.icdTas with
    | Some baseTypeIcdTas ->
        Some {IcdArgAux.canBeEmbedded = baseTypeIcdTas.canBeEmbedded; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=extraComment; scope="REFTYPE"; name=name}
    | None -> None

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
                let pp, resultExpr =
                    let str = lm.lg.getParamValue t p.accessPath codec
                    match codec, lm.lg.decodingKind with
                    | Decode, Copy ->
                        let toc = ToC str
                        toc, Some toc
                    | _ -> str, None
                let funcBodyContent = callBaseTypeFunc lm pp baseFncName codec
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

            let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
            let a, ns = createAcnFunction r deps lm codec t typeDefinition  isValidFunc funcBody (fun atc -> true) soSparkAnnotations [] ns
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
                    let extField        = getExternalField0 r deps t.id filterDependency
                    let soInner, errCodes0, localVariables0, userDefinedFunctions, ns1 =
                        match baseTypeAcnFunction with
                        | None  -> None, [], [], [], us
                        | Some baseTypeAcnFunction   ->
                            let acnRes, ns = baseTypeAcnFunction.funcBody us acnArgs nestingScope p
                            match acnRes with
                            | None  -> None, [], [], [], ns
                            | Some r -> Some r.funcBody, r.errCodes, r.localVariables, r.userDefinedFunctions, ns

                    let fncBody = octet_string_containing_ext_field_func pp baseFncName sReqBytesForUperEncoding extField errCode.errCodeName soInner codec

                    let lvs =
                        let localVars2 = lm.lg.acn.getAcnContainingByLocVars sReqBytesForUperEncoding
                        localVariables0@localVars2

                    fncBody, errCode::errCodes0,lvs, userDefinedFunctions, ns1
                | SZ_EC_ExternalField    relPath    , ContainedInBitString  ->
                    let extField        = getExternalField r deps t.id
                    let fncBody = bit_string_containing_ext_field_func pp baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding extField errCode.errCodeName codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_FIXED_SIZE        , ContainedInOctString  ->
                    let fncBody = octet_string_containing_func pp baseFncName sReqBytesForUperEncoding 0I encOptions.minSize.acn encOptions.maxSize.acn true codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_LENGTH_EMBEDDED nBits , ContainedInOctString  ->
                    let fncBody = octet_string_containing_func pp baseFncName sReqBytesForUperEncoding nBits encOptions.minSize.acn encOptions.maxSize.acn false codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_FIXED_SIZE                        , ContainedInBitString  ->
                    let fncBody = bit_string_containing_func pp baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding 0I encOptions.minSize.acn encOptions.maxSize.acn true codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_LENGTH_EMBEDDED nBits                 , ContainedInBitString  ->
                    let fncBody = bit_string_containing_func pp baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding nBits encOptions.minSize.acn encOptions.maxSize.acn false codec
                    fncBody, [errCode],[], [], us
                | SZ_EC_TerminationPattern nullVal  ,  _                    ->  raise(SemanticError (loc, "Invalid type for parameter4"))
            let funcBodyResult = Some ({AcnFuncBodyResult.funcBody = funcBodyContent; userDefinedFunctions=userDefinedFunctions; errCodes = errCodes; localVariables = localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = icd})
            funcBodyResult, ns2)

        let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
        let a,b = createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody us e acnArgs nestingScope p) (fun atc -> true) soSparkAnnotations [] us
        Some a, b)


