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


let foldMap = Asn1Fold.foldMap


let callBaseTypeFunc (lm:LanguageMacros) = lm.uper.call_base_type_func
let callSuperclassFunc (lm:LanguageMacros) = lm.uper.call_superclass_func

let sparkAnnotations (lm:LanguageMacros)  = lm.acn.sparkAnnotations

let THREE_DOTS = {IcdRow.fieldName = ""; comments = []; sPresent="";sType= IcdPlainType ""; sConstraint=None; minLengthInBits = 0I; maxLengthInBits=0I;sUnits=None; rowType = IcdRowType.ThreeDOTs; idxOffset = None}

let getAcnDeterminantName = AcnCreateFromAntlr.getAcnDeterminantName


let getDeterminantTypeDefinitionBodyWithinSeq (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (det:Determinant) =
    let createPrmAcnInteger (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  =
        let Declare_Integer     =  lm.typeDef.Declare_Integer
        Declare_Integer ()

    let createAcnInteger (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (a:Asn1AcnAst.AcnInteger) =
        let intClass = getAcnIntegerClass r.args a
        let stgMacro = DAstTypeDefinition.getIntegerTypeByClass lm intClass
        stgMacro ()

    let createAcnBoolean (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) =
        lm.typeDef.Declare_Boolean ()

    let createAcnNull (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) =
        lm.typeDef.Declare_Null ()

    let getTypeDefinitionName (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (id : ReferenceToType) =
        let longName = id.AcnAbsPath.Tail |> Seq.StrJoin "_"
        ToC2(r.args.TypePrefix + longName.Replace("#","elem"))

    match det with
    | AcnChildDeterminant       ch ->
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a -> createAcnInteger r lm a
        | Asn1AcnAst.AcnNullType _ -> createAcnNull r lm
        | Asn1AcnAst.AcnBoolean  _ -> createAcnBoolean r lm
        | Asn1AcnAst.AcnReferenceToEnumerated a -> ToC2(r.args.TypePrefix + a.tasName.Value)
        | Asn1AcnAst.AcnReferenceToIA5String a -> ToC2(r.args.TypePrefix + a.tasName.Value)

    | AcnParameterDeterminant   prm ->
        match prm.asn1Type with
        | AcnGenericTypes.AcnPrmInteger  _       -> createPrmAcnInteger r lm
        | AcnGenericTypes.AcnPrmBoolean  _       -> createAcnBoolean r lm
        | AcnGenericTypes.AcnPrmNullType _       -> createAcnNull r lm
        | AcnGenericTypes.AcnPrmRefType (md,ts)  ->
            getTypeDefinitionName r lm (ReferenceToType [MD md.Value; TA ts.Value])


let getDeterminant_macro (det:Determinant) pri_macro str_macro =
    match det with
    | AcnChildDeterminant ch ->
        match ch.Type with
        | Asn1AcnAst.AcnReferenceToIA5String _ -> str_macro
        | _ -> pri_macro
    | AcnParameterDeterminant prm -> pri_macro

let getDeterminantTypeUpdateMacro (lm:LanguageMacros) (det:Determinant) =
    let MultiAcnUpdate_get_first_init_value_pri     =  lm.acn.MultiAcnUpdate_get_first_init_value_pri
    let MultiAcnUpdate_get_first_init_value_str     =  lm.acn.MultiAcnUpdate_get_first_init_value_str
    getDeterminant_macro det MultiAcnUpdate_get_first_init_value_pri MultiAcnUpdate_get_first_init_value_str

let getDeterminantTypeCheckEqual (lm:LanguageMacros) (det:Determinant) =
    let multiAcnUpdate_checkEqual_pri     =  lm.acn.MultiAcnUpdate_checkEqual_pri
    let multiAcnUpdate_checkEqual_str     =  lm.acn.MultiAcnUpdate_checkEqual_str
    getDeterminant_macro det multiAcnUpdate_checkEqual_pri multiAcnUpdate_checkEqual_str


type FuncBody = State -> ErrorCode -> (AcnGenericTypes.RelativePath * AcnGenericTypes.AcnParameter) list -> NestingScope -> CodegenScope -> (AcnFuncBodyResult option) * State
type FuncBodyStateless = Codec -> (AcnGenericTypes.RelativePath * AcnGenericTypes.AcnParameter) list -> NestingScope -> CodegenScope -> string -> AcnFuncBodyResult option

let handleSavePosition (funcBody: FuncBody)
                       (savePosition: bool)
                       (c_name: string)
                       (lvName: string) (* Bitsream position local variable name *)
                       (typeId:ReferenceToType)
                       (lm:LanguageMacros)
                       (codec:CommonTypes.Codec): FuncBody =
    match savePosition with
    | false -> funcBody
    | true  ->
        let newFuncBody st errCode prms nestingScope (p:CodegenScope) =
            let content, ns1a = funcBody st errCode prms nestingScope p
            let sequence_save_bitstream                 = lm.acn.sequence_save_bitstream
            let savePositionStatement = sequence_save_bitstream lvName c_name codec
            let newContent =
                match content with
                | Some bodyResult   ->
                    let funcBodyStr = sprintf "%s\n%s" savePositionStatement bodyResult.funcBody
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = savePositionStatement
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= true; bBsIsUnReferenced=false; resultExpr = None; auxiliaries = []; icdResult = None }
            newContent, ns1a
        newFuncBody

let getAlignmentString (lm: LanguageMacros) (al: AcnAlignment) =
    match al with
    | AcnGenericTypes.NextByte ->
        lm.lg.getAlignmentByteTypeName, 8I
    | AcnGenericTypes.NextWord ->
        lm.lg.getAlignmentWordTypeName, 16I
    | AcnGenericTypes.NextDWord ->
        lm.lg.getAlignmentDWordTypeName, 32I

let handleAlignmentForAsn1Types (r:Asn1AcnAst.AstRoot)
                                (lm:LanguageMacros)
                                (codec:CommonTypes.Codec)
                                (acnAlignment: AcnAlignment option)
                                (funcBody: FuncBody): FuncBody  =
    let alignToNext =  lm.acn.alignToNext
    match acnAlignment with
    | None      -> funcBody
    | Some al   ->
        let alStr, nAlignmentVal = getAlignmentString lm al
        // printfn "[DEBUG] handleAlignmentForAsn1Types: alStr=%s nAlignmentVal=%A codec=%A" alStr nAlignmentVal codec
        let newFuncBody st errCode prms nestingScope p =
            let content, ns1a = funcBody st errCode prms nestingScope p
            let newContent =
                match content with
                | Some bodyResult   ->
                    // printfn "[DEBUG] handleAlignmentForAsn1Types: Applying alignment, bodyResult.funcBody length=%d" bodyResult.funcBody.Length
                    let funcBodyStr = alignToNext bodyResult.funcBody alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    // printfn "[DEBUG] handleAlignmentForAsn1Types: After alignToNext, funcBodyStr length=%d" funcBodyStr.Length
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = alignToNext "" alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {funcBody = funcBodyStr; errCodes =[errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= true; bBsIsUnReferenced=false; resultExpr = None; auxiliaries = []; icdResult=None}
            newContent, ns1a
        newFuncBody

let handleAlignmentForAcnTypes (r:Asn1AcnAst.AstRoot)
                               (lm:LanguageMacros)
                               (acnAlignment : AcnAlignment option)
                               (funcBody: FuncBodyStateless): FuncBodyStateless =
    let alignToNext = lm.acn.alignToNext
    match acnAlignment with
    | None      -> funcBody
    | Some al   ->
        let alStr, nAlignmentVal = getAlignmentString lm al
        let newFuncBody (codec:CommonTypes.Codec) (prms: (RelativePath * AcnParameter) list) (nestingScope: NestingScope) (p: CodegenScope) (lvName:string) =
            let content = funcBody codec prms nestingScope p lvName
            let newContent =
                match content with
                | Some bodyResult   ->
                    let funcBodyStr = alignToNext bodyResult.funcBody alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = alignToNext "" alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= true; bBsIsUnReferenced=false; resultExpr = None; auxiliaries = []; icdResult= None}
            newContent
        newFuncBody

let md5 = System.Security.Cryptography.MD5.Create()

let createIcdTas (r:Asn1AcnAst.AstRoot) (id:ReferenceToType) (icdAux:IcdArgAux) (td:FE_TypeDefinition) (typeDefinition:TypeDefinitionOrReference) nMinBytesInACN nMaxBytesInACN hasAcnDefinition =
    let icdRows, compositeChildren = icdAux.rowsFunc "" "" [];
    let icdTas =
        {
            IcdTypeAss.typeId = id
            tasInfo = id.tasInfo
            asn1Link = None;
            acnLink = None;
            name =
                match icdAux.name with
                | Some n -> n
                | None   -> td.asn1Name
            kind = icdAux.baseAsn1Kind;
            canBeEmbedded  = icdAux.canBeEmbedded
            createRowsFunc = icdAux.rowsFunc
            comments =
                let asn1Comments =
                    match id.tasInfo with
                    | None -> []
                    | Some tasInfo ->
                        (*
                        match r.Modules |> Seq.tryFind(fun m -> m.Name.Value = tasInfo.modName) with
                        | None -> []
                        | Some m ->
                            match m.TypeAssignments |> Seq.tryFind(fun ts -> ts.Name.Value = tasInfo.tasName) with
                            | None -> []
                            | Some ts -> ts.Comments |> Seq.toList
                            *)
                        match r.typeAssignmentsMap.TryFind (tasInfo.modName, tasInfo.tasName) with
                        | None -> []
                        | Some ts -> ts.Comments |> Seq.toList

                asn1Comments@icdAux.commentsForTas
            rows  = icdRows
            compositeChildren = compositeChildren
            minLengthInBytes = nMinBytesInACN;
            maxLengthInBytes = nMaxBytesInACN
            hasAcnDefinition = hasAcnDefinition
            hash = "" // will be calculated later
        }
    let icdHash = CalculateIcdHash.calcIcdTypeAssHash icdTas
    {icdTas with hash = icdHash}


let adaptArgument = DAstUPer.adaptArgument
let adaptArgumentValue = DAstUPer.adaptArgumentValue

let joinedOrAsIdentifier = DAstUPer.joinedOrAsIdentifier

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


let createAcnFunction (r: Asn1AcnAst.AstRoot)
                              (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
                              (lm: LanguageMacros)
                              (codec: CommonTypes.Codec)
                              (t: Asn1AcnAst.Asn1Type)
                              (typeDefinition: TypeDefinitionOrReference)
                              (isValidFunc: IsValidFunction option)
                              (funcBody: FuncBody)
                              isTestVaseValid
                              (soSparkAnnotations: string option)
                              (funcDefAnnots: string list)
                              (additionalAcnPrms: DastAcnParameter list)
                              (us: State) =
    let td = lm.lg.getTypeDefinition t.FT_TypeDefinition
    let funcNameAndtasInfo   = lm.lg.getACNFuncName r codec t td
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")))
    let errFieldPath = match t.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath
    //if t.id.AsString.EndsWith "ALPHA-DELETE-DIAGNOSTIC-PARAMETER-REPORT-STRUCTURES-GENERIC" then
    //    printfn "debug"
    let nMaxBytesInACN = BigInteger (ceil ((double t.acnMaxSizeInBits)/8.0))
    let nMinBytesInACN = BigInteger (ceil ((double t.acnMinSizeInBits)/8.0))
    let soInitFuncName = lm.lg.getFuncNameGeneric typeDefinition (lm.init.methodNameSuffix())
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let EmitTypeAssignment_primitive     =  lm.acn.EmitTypeAssignment_primitive
    let EmitTypeAssignment_primitive_def =  lm.acn.EmitTypeAssignment_primitive_def
    let EmitTypeAssignment_def_err_code  =  lm.acn.EmitTypeAssignment_def_err_code
    let EmitEncodingSizeConstants        =  lm.acn.EmitEncodingSizeConstants

    let typeDefinitionName = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
    let sEncodingSizeConstant = EmitEncodingSizeConstants typeDefinitionName nMaxBytesInACN t.acnMaxSizeInBits
    
    let funcBodyAsSeqComp (st: State)
                          (prms: (RelativePath * AcnParameter) list)
                          (nestingScope: NestingScope)
                          (p: CodegenScope)
                          (c_name: string)
                          (lvName: string): ((AcnFuncBodyResult option)*State) =
        //t.SaveBitStreamPosition is false for all types except NULL types where the 'save-position' attribute can be used
        let funcBody = handleSavePosition funcBody t.SaveBitStreamPosition c_name lvName t.id lm codec
        let ret = handleAlignmentForAsn1Types r lm codec t.acnAlignment funcBody
        let ret = lm.lg.adaptAcnFuncBody r deps ret isValidFuncName t codec
        ret st errCode prms nestingScope p

    let funcBody = handleAlignmentForAsn1Types r lm codec t.acnAlignment funcBody
    let funcBody = lm.lg.adaptAcnFuncBody r deps funcBody isValidFuncName t codec
    
    let sf = lm.lg.getTypeBasedSuffix FunctionType.AcnEncDecFunctionType t.Kind
    let p : CodegenScope = lm.lg.getParamTypeSuffix t sf codec
    let varName = p.accessPath.rootId
    let sStar = lm.lg.getStar p.accessPath

    let sInitialExp = ""
    let func, funcDef, userDefinedFunctions, auxiliaries, icdResult, ns2  =
            match funcNameAndtasInfo  with
            | None -> 
                match ProgrammingLanguage.ActiveLanguages.Head with
                | Scala -> 
                    None, None, [], [], None, ns
                | _ ->
                    match r.args.generateAcnIcd with
                    | false -> 
                        None, None, [], [], None, ns
                    | true ->
                        //the call to funcBody is necessary to get the correct nesting scope
                        //however, it is expensive to call so we only call it if we need to generate the ICD
                        let content, ns1a = funcBody ns errCode [] (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) p
                        let icdResult, udfcs =
                            match content with
                            | None -> None, []
                            | Some bodyResult -> bodyResult.icdResult, bodyResult.userDefinedFunctions
                        None, None, udfcs, [], icdResult, ns1a
            | Some funcName ->
                let precondAnnots = lm.lg.generatePrecond r ACN t codec
                let postcondAnnots = lm.lg.generatePostcond r ACN p t codec
                let content, ns1a = funcBody ns errCode [] (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) p
                let bodyResult_funcBody, errCodes,  bodyResult_localVariables, bBsIsUnreferenced, bVarNameIsUnreferenced, udfcs, auxiliaries, icdResult =
                    match content with
                    | None ->
                        let emptyStatement = lm.lg.emptyStatement
                        emptyStatement, [], [], true, isValidFuncName.IsNone, [], [], None
                    | Some bodyResult ->
                        bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsUnReferenced, bodyResult.bValIsUnReferenced, bodyResult.userDefinedFunctions, bodyResult.auxiliaries, bodyResult.icdResult

                // Determine if this instance needs to return any acn children
                // A type returns acn_children if EITHER:
                // 1. It has ACN child determinants that depend on it (it's a parent type), OR
                // 2. It receives ACN parameters (it's a child type in the dependency chain)
                let bHasAcnChildrenToReturn =
                    match t.Kind with
                    | Asn1AcnAst.Sequence sq ->
                        // Find ACN children that belong to this type and check if they'll be filtered out
                        let acnChildrenToReturn =
                            deps.acnDependencies
                            |> List.filter(fun dep ->
                                match dep.determinant with
                                | AcnChildDeterminant acnCh ->
                                    // This ACN child belongs to this type
                                    if acnCh.id.parentTypeId = Some t.id then
                                        // Check if it has external dependency (for object-oriented languages)
                                        if lm.lg.isObjectOriented then
                                            let determinantPath = dep.determinant.id.AsString
                                            let typePath = t.id.AsString
                                            let determinantBelongsToType = determinantPath.StartsWith(typePath + ".") || determinantPath = typePath
                                            let dependentPath = dep.asn1Type.AsString
                                            let dependentIsOutsideType = not (dependentPath.StartsWith(typePath + ".") || dependentPath = typePath)
                                            let hasExternalDependency = determinantBelongsToType && dependentIsOutsideType
                                            not hasExternalDependency  // Include only if no external dependency
                                        else
                                            true  // For other languages, include all
                                    else
                                        false
                                | _ -> false)
                        let hasAcnChildren = not acnChildrenToReturn.IsEmpty
                        let receivesParameters = t.acnParameters.Length > 0
                        hasAcnChildren || receivesParameters
                    | _ -> false

                let handleAcnParameter (p:AcnGenericTypes.AcnParameter) =
                    let intType  = lm.typeDef.Declare_Integer ()
                    let boolType = lm.typeDef.Declare_Boolean ()
                    let intZero  = lm.lg.asn1SccIntValueToString 0I false
                    let emitPrm  = lm.acn.EmitAcnParameter
                    match p.asn1Type with
                    | AcnGenericTypes.AcnPrmInteger    loc          -> emitPrm p.c_name intType intZero
                    | AcnGenericTypes.AcnPrmBoolean    loc          -> emitPrm p.c_name boolType lm.lg.FalseLiteral
                    | AcnGenericTypes.AcnPrmNullType   loc          -> raise(SemanticError (loc, "Invalid type for parameter"))
                    | AcnGenericTypes.AcnPrmRefType(md,ts)          ->
                        let prmTypeName =
                            match lm.lg.hasModules with
                            | false         -> ToC2(r.args.TypePrefix + ts.Value)
                            | true       ->
                                match md.Value = t.id.ModName with
                                | true  -> ToC2(r.args.TypePrefix + ts.Value)
                                | false -> (ToC2 md.Value) + "." + ToC2(r.args.TypePrefix + ts.Value)
                        match ProgrammingLanguage.ActiveLanguages.Head with
                        | Python ->
                            // For Python, use basic types: enumerations -> int, strings -> str
                            let basicType, defaultVal =
                                try
                                    let refModule = r.Modules |> Seq.find(fun m -> m.Name.Value = md.Value)
                                    let refTas = refModule.TypeAssignments |> Seq.tryFind(fun ta -> ta.Name.Value = ts.Value)
                                    match refTas with
                                    | Some tas ->
                                        match tas.Type.ActualType.Kind with
                                        | Asn1AcnAst.Enumerated _ -> "int", intZero
                                        | Asn1AcnAst.IA5String _
                                        | Asn1AcnAst.NumericString _ -> "str", "\"\""
                                        | _ -> "int", intZero
                                    | None -> "int", intZero
                                with
                                | _ -> "int", intZero
                            emitPrm p.c_name basicType defaultVal
                        | _ -> emitPrm p.c_name prmTypeName ""

                let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct

                // Handler for AcnChild (extracts type from AcnInsertedType and creates parameter representation)
                let handleAcnChild (ch:Asn1AcnAst.AcnChild) =
                    let intType  = lm.typeDef.Declare_Integer ()
                    let boolType = lm.typeDef.Declare_Boolean ()
                    let intZero  = lm.lg.asn1SccIntValueToString 0I false
                    let emitPrm  = lm.acn.EmitAcnParameter
                    match ch.Type with
                    | Asn1AcnAst.AcnInteger  _ -> emitPrm ch.c_name intType intZero
                    | Asn1AcnAst.AcnNullType _ -> emitPrm ch.c_name boolType lm.lg.FalseLiteral
                    | Asn1AcnAst.AcnBoolean  _ -> emitPrm ch.c_name boolType lm.lg.FalseLiteral
                    | Asn1AcnAst.AcnReferenceToEnumerated a ->
                        emitPrm ch.c_name (ToC2(r.args.TypePrefix + a.tasName.Value)) intZero
                    | Asn1AcnAst.AcnReferenceToIA5String a ->
                        emitPrm ch.c_name (ToC2(r.args.TypePrefix + a.tasName.Value)) "\"\""

                // Handler for DastAcnParameter (similar to handleAcnParameter but for DAst types)
                let handleDastAcnParameter (p:DastAcnParameter) =
                    let intType  = lm.typeDef.Declare_Integer ()
                    let boolType = lm.typeDef.Declare_Boolean ()
                    let intZero  = lm.lg.asn1SccIntValueToString 0I false
                    let emitPrm  = lm.acn.EmitAcnParameter
                    match p.asn1Type with
                    | AcnGenericTypes.AcnPrmInteger    loc          -> emitPrm p.c_name intType intZero
                    | AcnGenericTypes.AcnPrmBoolean    loc          -> emitPrm p.c_name boolType lm.lg.FalseLiteral
                    | AcnGenericTypes.AcnPrmNullType   loc          -> raise(SemanticError (loc, "Invalid type for parameter"))
                    | AcnGenericTypes.AcnPrmRefType(md,ts)          ->
                        let prmTypeName =
                            match lm.lg.hasModules with
                            | false         -> ToC2(r.args.TypePrefix + ts.Value)
                            | true       ->
                                match md.Value = t.id.ModName with
                                | true  -> ToC2(r.args.TypePrefix + ts.Value)
                                | false -> (ToC2 md.Value) + "." + ToC2(r.args.TypePrefix + ts.Value)
                        match ProgrammingLanguage.ActiveLanguages.Head with
                        | Python ->
                            // For Python, use basic types: enumerations -> int, strings -> str
                            let basicType, defaultVal =
                                try
                                    let refModule = r.Modules |> Seq.find(fun m -> m.Name.Value = md.Value)
                                    let refTas = refModule.TypeAssignments |> Seq.tryFind(fun ta -> ta.Name.Value = ts.Value)
                                    match refTas with
                                    | Some tas ->
                                        match tas.Type.ActualType.Kind with
                                        | Asn1AcnAst.Enumerated _ -> "int", intZero
                                        | Asn1AcnAst.IA5String _
                                        | Asn1AcnAst.NumericString _ -> "str", "\"\""
                                        | _ -> "int", intZero
                                    | None -> "int", intZero
                                with
                                | _ -> "int", intZero
                            emitPrm p.c_name basicType defaultVal
                        | _ -> emitPrm p.c_name prmTypeName ""
                // Combine t.acnParameters with additional ACN parameters from additionalAcnPrms
                let baseAcnParams = t.acnParameters |> List.map handleAcnParameter
                let additionalPrms = additionalAcnPrms |> List.map handleDastAcnParameter
                let basePrmNames = t.acnParameters |> List.map (fun p -> p.c_name)
                let additionalPrmNames = additionalAcnPrms |> List.map (fun p -> p.c_name)

                // Also include ACN parameters that are PASSED TO this type as determinants from parent types
                // These are parameters that other types reference when decoding this type
                // Extract both AcnParameterDeterminant and AcnChildDeterminant separately
                let matchingDeps = deps.acnDependencies |> List.filter(fun d -> d.asn1Type = t.id)

                // Extract AcnParameterDeterminant items
                let incomingAcnParams =
                    matchingDeps
                    |> List.choose(fun d ->
                        match d.determinant with
                        | AcnParameterDeterminant acnPrm -> Some acnPrm
                        | _ -> None
                    )

                // Extract AcnChildDeterminant items
                let incomingAcnChildren =
                    matchingDeps
                    |> List.choose(fun d ->
                        match d.determinant with
                        | AcnChildDeterminant acnCh -> Some acnCh
                        | _ -> None
                    )

                // DEBUG: Check all dependencies for this type
                // if matchingDeps.Length > 0 then
                //     // printfn "[DEBUG createAcnFunction] Found %d matching acnDependencies" matchingDeps.Length
                //     matchingDeps
                //     |> List.iter(fun d ->
                //         printfn "  - determinant: %s"
                //             (match d.determinant with
                //              | AcnParameterDeterminant p -> sprintf "AcnPrm(%s)" p.c_name
                //              | AcnChildDeterminant c -> sprintf "AcnChild(%s)" c.c_name)
                //     )

                // Process both parameter types through their respective handlers
                let incomingAcnParamStrings = incomingAcnParams |> List.map handleAcnParameter
                let incomingChildStrings = incomingAcnChildren |> List.map handleAcnChild
                let incomingPrmNames = (incomingAcnParams |> List.map (fun p -> p.c_name)) @ (incomingAcnChildren |> List.map (fun c -> c.c_name))

                // DEBUG: Log all parameters found
                let allParamCount = baseAcnParams.Length + additionalPrms.Length + incomingAcnParams.Length + incomingAcnChildren.Length
                // if allParamCount > 0 then
                    // printfn "DEBUG: baseAcnParams=%d, additionalPrms=%d, incomingAcnParams=%d, incomingChildren=%d, total=%d"
                        // baseAcnParams.Length additionalPrms.Length incomingAcnParams.Length incomingAcnChildren.Length allParamCount

                let prmNames = basePrmNames @ additionalPrmNames @ incomingPrmNames
                // Deduplicate by keeping first occurrence
                let prmNamesDistinct = prmNames |> Seq.distinct |> Seq.toList
                // Also deduplicate the prms list to match prmNamesDistinct - pair names with params and filter
                let allPrmsWithNames = List.zip prmNames (baseAcnParams @ additionalPrms @ incomingAcnParamStrings @ incomingChildStrings)
                let seenNames = System.Collections.Generic.HashSet<string>()
                let prms = allPrmsWithNames |> List.filter (fun (name, _) -> seenNames.Add(name)) |> List.map snd
               
                let func = Some(EmitTypeAssignment_primitive varName sStar funcName isValidFuncName typeDefinitionName lvars bodyResult_funcBody soSparkAnnotations sInitialExp prms prmNamesDistinct (t.acnMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced bHasAcnChildrenToReturn soInitFuncName funcDefAnnots precondAnnots postcondAnnots codec)
               
                
                let errCodStr = 
                    errCodes |> 
                    List.groupBy (fun x -> x.errCodeName) |>
                    List.map (fun (k, v) -> {errCodeName = k; errCodeValue = v.Head.errCodeValue; comment = v.Head.comment; fieldPath = v.Head.fieldPath}) |>
                    List.map(fun x -> EmitTypeAssignment_def_err_code x.errCodeName (BigInteger x.errCodeValue) x.comment x.fieldPath) |> List.distinct
                let funcDef = Some(EmitTypeAssignment_primitive_def varName sStar funcName  typeDefinitionName errCodStr (t.acnMaxSizeInBits = 0I) nMaxBytesInACN ( t.acnMaxSizeInBits) prms soSparkAnnotations codec)
                let ns2a =
                    match t.id.topLevelTas with
                    | None -> ns1a
                    | Some tasInfo ->
                        let caller = {Caller.typeId = tasInfo; funcType= UperEncDecFunctionType}
                        let callee = {Callee.typeId = tasInfo; funcType=IsValidFunctionType}
                        addFunctionCallToState ns1a caller callee
                func, funcDef, udfcs, auxiliaries, icdResult, ns2a

    let icdAux, ns3 =
        match icdResult with
        | Some icdAux ->
            let foo () =
                let hasAcnDefinition = t.typeAssignmentInfo.IsSome && t.acnLocation.IsSome
                let icdTas = createIcdTas r t.id icdAux td typeDefinition nMinBytesInACN nMaxBytesInACN hasAcnDefinition
                let ns3 =
                    match ns2.icdHashes.TryFind icdTas.hash with
                    | None -> {ns2 with icdHashes = ns2.icdHashes.Add(icdTas.hash, [icdTas])}
                    | Some exList -> {ns2 with icdHashes = ns2.icdHashes.Add(icdTas.hash, icdTas::exList)}
                Some icdTas, ns3
            TL "createIcdTas" foo
        | None -> None, ns2
    let ret =
        {
            AcnFunction.funcName       = funcNameAndtasInfo
            func                       = func
            funcDef                    = funcDef
            auxiliaries                = auxiliaries
            funcBody                   = fun us acnArgs p -> funcBody us errCode acnArgs p
            funcBodyAsSeqComp          = funcBodyAsSeqComp
            isTestVaseValid            = isTestVaseValid
            icdTas                        = icdAux
            userDefinedFunctions       = userDefinedFunctions
            encodingSizeConstant       = sEncodingSizeConstant
        }
    ret, ns3

type AcnIntegerFuncBody = ErrorCode -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CodegenScope -> (AcnFuncBodyResult option)

let private createAcnIntegerFunctionInternal (r:Asn1AcnAst.AstRoot)
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
                                             sType: AcnIntegerFuncBody =
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
            |Asn1AcnAst.Integer_uPER ->
                uperfuncBody errCode nestingScope p true |> Option.map(fun x -> x.funcBody, x.errCodes, x.bValIsUnReferenced, x.bBsIsUnReferenced)
            |Asn1AcnAst.PositiveInteger_ConstSize_8 ->
                Some(PositiveInteger_ConstSize_8 (castPp 8) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 8) sType codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_16 ->
                Some(PositiveInteger_ConstSize_big_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 16) sType codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_16 ->
                Some(PositiveInteger_ConstSize_little_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 16) sType codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_32 ->
                Some(PositiveInteger_ConstSize_big_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 32) sType codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_32 ->
                Some(PositiveInteger_ConstSize_little_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 32) sType codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_64 ->
                Some(PositiveInteger_ConstSize_big_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 64) sType codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_64 ->
                Some(PositiveInteger_ConstSize_little_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 64) sType codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize bitSize ->
                Some(PositiveInteger_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName ( bitSize) soMF soMFM (max 0I nUperMin) (uIntActualMax (int bitSize)) sType codec, [errCode], false, false)

            |Asn1AcnAst.TwosComplement_ConstSize_8 ->
                Some(TwosComplement_ConstSize_8 (castPp 8) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 8) (sIntActualMax 8) sType codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_16 ->
                Some(TwosComplement_ConstSize_big_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 16) (sIntActualMax 16) sType codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_16 ->
                Some(TwosComplement_ConstSize_little_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 16) (sIntActualMax 16) sType codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_32 ->
                Some(TwosComplement_ConstSize_big_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 32) (sIntActualMax 32) sType codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_32 ->
                Some(TwosComplement_ConstSize_little_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 32) (sIntActualMax 32) sType codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_64 ->
                Some(TwosComplement_ConstSize_big_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 64) (sIntActualMax 64) sType codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_64 ->
                Some(TwosComplement_ConstSize_little_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 64) (sIntActualMax 64) sType codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize bitSize ->
                Some(TwosComplement_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM ( bitSize) (sIntActualMin (int bitSize)) (sIntActualMax (int bitSize)) sType codec, [errCode], false, false)

            |Asn1AcnAst.ASCII_ConstSize size ->
                Some(ASCII_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax ((size)/8I) sType codec, [errCode], false, false)
            |Asn1AcnAst.ASCII_VarSize_NullTerminated nullBytes ->
                Some(ASCII_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax nullBytes sType codec, [errCode], false, false)
            |Asn1AcnAst.ASCII_UINT_ConstSize size ->
                Some(ASCII_UINT_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax (( size)/8I) sType codec, [errCode], false, false)
            |Asn1AcnAst.ASCII_UINT_VarSize_NullTerminated nullBytes ->
                Some(ASCII_UINT_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName  soMF soMFM nUperMin nUperMax nullBytes sType codec, [errCode], false, false)
            |Asn1AcnAst.BCD_ConstSize size ->
                Some(BCD_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax (( size)/4I) sType codec, [errCode], false, false)
            |Asn1AcnAst.BCD_VarSize_NullTerminated nullBytes ->
                Some(BCD_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax sType codec, [errCode], false, false)

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

let createAcnIntegerFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnInteger) (typeName:string) (us:State) (sType:String) =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath

    let uperFuncBody (errCode) (nestingScope: NestingScope) (p:CodegenScope) (fromACN: bool) =
        // ACN integers don't have type definitions - they're primitive parameters
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


let createIntegerFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (acnPrms:DastAcnParameter list) (us:State)  =
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
    createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] acnPrms us


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
        let td = (lm.lg.getEnumTypeDefinition o.typeDef).longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
        let localVar, intVal =
            let varName =
                let baseName = ToC (p.accessPath.asIdentifier lm.lg)
                match ProgrammingLanguage.ActiveLanguages.Head with
                | Python -> $"{baseName}_int"
                | _ -> $"intVal_{baseName}"
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
                let intType = Some (lm.typeDef.Declare_Integer())
                let funcBody = IntFullyConstraintPos (castPp word_size_in_bits) min max nbits sSsuffix errCode.errCodeName rangeAssert intType codec
                Some({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables= []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]})
            createAcnIntegerFunctionInternal r lm codec (Concrete (min,max)) intTypeClass o.acnEncodingClass uperInt sAsn1Constraints acnMinSizeInBits acnMaxSizeInBits unitsOfMeasure typeDefinitionName (None, None) ""
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



let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (icdStgFileName:string) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefinitionOrReference) (typeDefinition:TypeDefinitionOrReference)   (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (acnPrms:DastAcnParameter list) (us:State)  =
    let funcBody (errCode: ErrorCode)
                 (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list)
                 (nestingScope: NestingScope)
                 (p: CodegenScope) =
        let typeDefinitionName = (defOrRef.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) //getTypeDefinitionName t.id.tasInfo typeDefinition
        let funcBodyOrig = createEnumCommon r deps lm codec t.id o defOrRef typeDefinitionName icdStgFileName None t.acnMinSizeInBits t.acnMaxSizeInBits t.unitsOfMeasure
        let res = funcBodyOrig errCode acnArgs nestingScope p
        res |> Option.map (fun res ->
            let aux = lm.lg.generateEnumAuxiliaries r ACN t o nestingScope p.accessPath codec
            {res with auxiliaries = res.auxiliaries @ aux})

    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2  (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations  [] acnPrms us


let createAcnEnumeratedFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (icdStgFileName:string) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToEnumerated)  (defOrRef:TypeDefinitionOrReference) (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath
    let td = lm.lg.getTypeDefinition (t.getType r).FT_TypeDefinition
    let typeDefinitionName = td.typeName
    let funcBody = createEnumCommon r deps lm codec typeId t.enumerated defOrRef typeDefinitionName icdStgFileName None t.enumerated.acnMinSizeInBits t.enumerated.acnMaxSizeInBits None
    (funcBody errCode), ns

let createRealFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (acnPrms:DastAcnParameter list) (us:State)  =
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
    createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations annots acnPrms us


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (acnPrms:DastAcnParameter list) (us:State)  =
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
    createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] acnPrms us


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (acnPrms:DastAcnParameter list) (us:State)  =
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
    createAcnFunction r deps lm codec t typeDefinition isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] acnPrms us


let nestChildItems (lm:LanguageMacros) (codec:CommonTypes.Codec) children =
    DAstUtilFunctions.nestItems lm.isvalid.JoinItems2 children


let createAcnBooleanFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnBoolean)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let pp, resultExpr = adaptArgument lm codec p
        let Boolean         = lm.uper.Boolean
        let funcBodyContent =
            Boolean pp errCode.errCodeName (lm.typeDef.Declare_Boolean ()) codec
        let icdFnc fieldName sPresent comments =
            [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType "BOOLEAN"); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.FieldRow; idxOffset = None}], []
        let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = "BOOLEAN"; rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
        Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd}
    (funcBody errCode), ns

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionOrReference) (baseTypeUperFunc : AcnFunction option) (isValidFunc: IsValidFunction option) (acnPrms:DastAcnParameter list) (us:State)  =
    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let Boolean         = lm.uper.Boolean
        let acnBoolean      = lm.acn.Boolean
        let BooleanTrueFalse = lm.acn.BooleanTrueFalse

        let funcBodyContent, resultExpr=
            let pvalue, ptr, resultExpr =
                match codec, lm.lg.decodingKind with
                | Decode, Copy ->
                    let resExpr = (p.accessPath.asIdentifier lm.lg)
                    resExpr, resExpr, Some resExpr
                | _ -> lm.lg.getValue p.accessPath, lm.lg.getPointer p.accessPath, None
            let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
            let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName
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
        let icdFnc fieldName sPresent comments =
            [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
        let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
        let aux = lm.lg.generateBooleanAuxiliaries r ACN t o nestingScope p.accessPath codec
        {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=aux; icdResult = Some icd}
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> Some (funcBody e acnArgs nestingScope p), us) (fun atc -> true) soSparkAnnotations [] acnPrms us


let createAcnNullTypeFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnNullType)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
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
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType icdDesc); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = "NULL"; rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd})
    (funcBody errCode), ns

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (acnPrms:DastAcnParameter list) (us:State)  =
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
            let icdFnc fieldName sPresent comments =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType icdDesc); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= lm.lg.acn.null_valIsUnReferenced; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=aux; icdResult = Some icd})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] acnPrms us


let getExternalField0 (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency func1 =
    let dependency = 
        match deps.acnDependencies |> List.tryFind (fun d -> d.asn1Type = asn1TypeIdWithDependency && func1 d ) with
        | Some d -> d
        | None   -> 
            failwithf "getExternalField0: No dependency found for %A" asn1TypeIdWithDependency
        
    let rec resolveParam (prmId:ReferenceToType) =
        let nodes = match prmId with ReferenceToType nodes -> nodes
        let lastNode = nodes |> List.rev |> List.head
        match lastNode with
        | PRM prmName   ->
            if r.args.acnDeferred || ProgrammingLanguage.ActiveLanguages.Head = Python then
                // In deferred mode, the parameter IS the value — it arrives
                // as an AcnInsertedFieldRef* formal parameter.  Do NOT follow
                // RefTypeArgumentDependency chains; stop here.
                // For Python: parameters are formal args in generated functions, use them directly.
                prmId
            else
                let newDeterminantId =
                    deps.acnDependencies |>
                    List.choose(fun d ->
                        match d.dependencyKind with
                        | AcnDepRefTypeArgument prm when prm.id = prmId -> Some d.determinant
                        | _                                             -> None)
                match newDeterminantId with
                | det1::_   -> resolveParam det1.id
                | _         -> prmId
        | _             -> prmId

    let resolvedId = resolveParam dependency.determinant.id
    // In deferred mode, a PRM determinant MUST resolve to a PRM node
    // (the parameter itself).  If it resolves to something else, it means
    // the dep rewrite is wrong or a RefTypeArgumentDependency was followed
    // when it shouldn't have been.
    if r.args.acnDeferred then
        match dependency.determinant with
        | Asn1AcnAst.AcnParameterDeterminant _ ->
            let resolvedNodes = match resolvedId with ReferenceToType nodes -> nodes
            let resolvedLastNode = resolvedNodes |> List.rev |> List.head
            match resolvedLastNode with
            | PRM _ -> ()  // correct — resolved to a parameter
            | _ -> failwithf "BUG: In deferred mode, parameter determinant %A resolved to non-PRM node %A" dependency.determinant.id resolvedId
        | _ -> ()  // AcnChildDeterminant — resolves to the ACN child, which is fine

    let baseName = getAcnDeterminantName resolvedId
    // In deferred mode, when the determinant resolved to a PRM (parameter),
    // the formal parameter is AcnInsertedFieldRef*.  Code that reads the
    // integer value must access the ->value field of the struct.
    // For IA5String determinants, use ->str_value instead of ->value.
    if r.args.acnDeferred then
        let resolvedNodes = match resolvedId with ReferenceToType nodes -> nodes
        let resolvedLastNode = resolvedNodes |> List.rev |> List.head
        match resolvedLastNode with
        | PRM _ ->
            // Check if the dependency is string-typed
            let isStringDep =
                match dependency.dependencyKind with
                | Asn1AcnAst.AcnDepPresenceStr _ -> true
                | _ -> false
            if isStringDep then baseName + "->str_value"
            else baseName + "->value"
        | _     -> baseName
    else
        baseName

let getExternalField0Type (r: Asn1AcnAst.AstRoot)
                          (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
                          (asn1TypeIdWithDependency: ReferenceToType)
                          (filter: AcnDependency -> bool) : AcnInsertedType option =
    let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = asn1TypeIdWithDependency && filter d)
    let nodes = match dependency.determinant.id with ReferenceToType nodes -> nodes
    let lastNode = nodes |> List.rev |> List.head
    match lastNode with
    | PRM _   ->
        let tp =
            deps.acnDependencies |>
            List.choose(fun d ->
                match d.dependencyKind with
                | AcnDepRefTypeArgument prm when prm.id = dependency.determinant.id ->
                    match d.determinant with
                    | AcnChildDeterminant child -> Some child.Type
                    | _ -> None
                | _ -> None)
        match tp with
        | tp :: _ -> Some tp
        | _ ->
            match dependency.determinant with
            | AcnChildDeterminant child -> Some child.Type
            | _ -> None
    | _ ->
        match dependency.determinant with
        | AcnChildDeterminant child -> Some child.Type
        | _ -> None

let getExternalFieldChoicePresentWhen (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency  relPath=
    let filterDependency (d:AcnDependency) =
        match d.dependencyKind with
        | AcnDepPresence (relPath0, _)   -> relPath = relPath0
        | _                              -> true
    getExternalField0 r deps asn1TypeIdWithDependency filterDependency

let getExternalFieldTypeChoicePresentWhen (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency  relPath=
    let filterDependency (d:AcnDependency) =
        match d.dependencyKind with
        | AcnDepPresence (relPath0, _)   -> relPath = relPath0
        | _                              -> true
    getExternalField0Type r deps asn1TypeIdWithDependency filterDependency

let getExternalField (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
    getExternalField0 r deps asn1TypeIdWithDependency (fun z -> true)

let getExternalFieldType (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
    getExternalField0Type r deps asn1TypeIdWithDependency (fun z -> true)

let getExternalFieldChild (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency : Asn1AcnAst.AcnChild option =
    try
        let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = asn1TypeIdWithDependency)
        match dependency.determinant with
        | Asn1AcnAst.AcnChildDeterminant child -> Some child
        | _ -> None
    with
    | _ -> None

let createStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionOrReference)  (defOrRef:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (acnPrms:DastAcnParameter list) (us:State)  =
    let Acn_String_Ascii_FixSize                            = lm.acn.Acn_String_Ascii_FixSize
    let Acn_String_Ascii_Internal_Field_Determinant         = lm.acn.Acn_String_Ascii_Internal_Field_Determinant
    let Acn_String_Ascii_Null_Terminated                    = lm.acn.Acn_String_Ascii_Null_Terminated
    let Acn_String_Ascii_External_Field_Determinant         = lm.acn.Acn_String_Ascii_External_Field_Determinant
    let Acn_String_CharIndex_External_Field_Determinant     = lm.acn.Acn_String_CharIndex_External_Field_Determinant
    let Acn_IA5String_CharIndex_External_Field_Determinant  = lm.acn.Acn_IA5String_CharIndex_External_Field_Determinant

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) (us:State) =
        let pp, resultExpr = adaptArgument lm codec p
        let td = (lm.lg.getStrTypeDefinition o.typeDef).longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
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
                let extField = getExternalField r deps t.id
                Some(Acn_String_Ascii_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) extField sType codec, [errCode], [], []), us
            | Acn_Enc_String_CharIndex_External_Field_Determinant _ ->
                let extField = getExternalField r deps t.id
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let encDecStatement =
                    match o.uperCharSet.Length = 128 with
                    | false ->
                        let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                        Acn_String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) arrAsciiCodes (BigInteger o.uperCharSet.Length) extField td nBits sType codec
                    | true -> Acn_IA5String_CharIndex_External_Field_Determinant pp errCode.errCodeName o.maxSize.acn extField td nBits (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) sType codec
                Some(encDecStatement, [errCode], [], []), us
        match funcBodyContent with
        | None -> None, ns
        | Some (funcBodyContent,errCodes, localVars, auxiliaries) ->
            let icdFnc fieldName sPresent comments  =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVars; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd} ), ns
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p us) (fun atc -> true) soSparkAnnotations [] acnPrms us


let createAcnStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToIA5String)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
    let errCode, ns = getNextValidErrorCode us errCodeName None errFieldPath
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
            | Asn1AcnAst.IA5String     z -> (lm.lg.getStrTypeDefinition z.typeDef).longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
            | Asn1AcnAst.NumericString z -> (lm.lg.getStrTypeDefinition z.typeDef).longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
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


    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let td = (lm.lg.getStrTypeDefinition o.typeDef).longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
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
                let extField = getExternalField r deps typeId
                Some(Acn_String_Ascii_External_Field_Determinant pp errCode.errCodeName ( t.str.maxSize.acn) extField sType codec, [], [], [])
            | Acn_Enc_String_CharIndex_External_Field_Determinant   _    ->
                let extField = getExternalField r deps typeId
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
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, lvs, auxiliaries) ->
            let icdFnc fieldName sPresent comments  =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType "IA5String"); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = "IA5String"; rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::errCodes |> List.distinct ; localVariables = lvs; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=auxiliaries; icdResult = Some icd})

    (funcBody errCode), ns

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (acnPrms:DastAcnParameter list) (us:State)  =
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
                let rawExtField = getExternalField r deps t.id
                // For Python inline use: if rawExtField is a parameter name and we have acnArgs,
                // the function body is being inlined into a parent sequence where the parameter
                // is not in scope — look up the actual ACN child that was passed as the argument.
                let extField =
                    match ProgrammingLanguage.ActiveLanguages.Head with
                    | Python when acnArgs |> List.exists (fun (_, prm) -> prm.c_name = rawExtField) ->
                        let actualDep =
                            deps.acnDependencies
                            |> List.tryFind (fun d ->
                                d.asn1Type = t.id &&
                                match d.dependencyKind with
                                | AcnDepRefTypeArgument prm when prm.c_name = rawExtField -> true
                                | _ -> false)
                        match actualDep with
                        | Some dep -> getAcnDeterminantName dep.determinant.id
                        | None -> rawExtField
                    | _ -> rawExtField
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

        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) ->
            let icdFnc fieldName sPresent comments  =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}

            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd})
    let soSparkAnnotations = Some (sparkAnnotations lm td codec)

    // For external field size determinants, add the external field as an ACN parameter
    let additionalAcnPrms =
        match o.acnEncodingClass with
        | SZ_EC_ExternalField _ ->
            match getExternalFieldChild r deps t.id with
            | Some child ->
                let acnParamType =
                    match child.Type with
                    | AcnInsertedType.AcnInteger intType -> AcnGenericTypes.AcnPrmInteger intType.Location
                    | AcnInsertedType.AcnBoolean boolType -> AcnGenericTypes.AcnPrmBoolean boolType.Location
                    | AcnInsertedType.AcnNullType nullType -> AcnGenericTypes.AcnPrmNullType nullType.Location
                    | AcnInsertedType.AcnReferenceToEnumerated enumType -> AcnGenericTypes.AcnPrmRefType (enumType.modName, enumType.tasName)
                    | AcnInsertedType.AcnReferenceToIA5String strType -> AcnGenericTypes.AcnPrmRefType (strType.modName, strType.tasName)

                let acnParam = {
                    DastAcnParameter.asn1Type = acnParamType
                    name = child.Name.Value
                    loc = child.Name.Location
                    id = child.id
                    c_name = child.c_name
                    typeDefinitionBodyWithinSeq = getDeterminantTypeDefinitionBodyWithinSeq r lm (Asn1AcnAst.AcnChildDeterminant child)
                }
                [acnParam]
            | None -> []
        | _ -> []

    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] (acnPrms @ additionalAcnPrms) us

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (acnPrms:DastAcnParameter list) (us:State)  =
    let nAlignSize = 0I;
    let bitString_FixSize = lm.uper.bitString_FixSize
    let bitString_VarSize = lm.uper.bitString_VarSize

    let funcBody (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let pp, resultExpr = joinedOrAsIdentifier lm codec p
        let access = lm.lg.getAccess p.accessPath
        let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
        let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName
        let funcBodyContent =
            match o.acnEncodingClass with
            | SZ_EC_ExternalField   _    ->
                let rawExtField = getExternalField r deps t.id
                let extField =
                    match ProgrammingLanguage.ActiveLanguages.Head with
                    | Python when acnArgs |> List.exists (fun (_, prm) -> prm.c_name = rawExtField) ->
                        let actualDep =
                            deps.acnDependencies
                            |> List.tryFind (fun d ->
                                d.asn1Type = t.id &&
                                match d.dependencyKind with
                                | AcnDepRefTypeArgument prm when prm.c_name = rawExtField -> true
                                | _ -> false)
                        match actualDep with
                        | Some dep -> getAcnDeterminantName dep.determinant.id
                        | None -> rawExtField
                    | _ -> rawExtField
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
            let icdFnc fieldName sPresent comments  =
                [{IcdRow.fieldName = fieldName; comments = comments; sPresent=sPresent;sType=(IcdPlainType (getASN1Name t)); sConstraint=None; minLengthInBits = o.acnMinSizeInBits ;maxLengthInBits=o.acnMaxSizeInBits;sUnits=t.unitsOfMeasure; rowType = IcdRowType.FieldRow; idxOffset = None}], []
            let icd = {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}

            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; userDefinedFunctions=[]; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]; icdResult = Some icd})

    let td = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
    let soSparkAnnotations = Some(sparkAnnotations lm td codec)
    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us) (fun atc -> true) soSparkAnnotations [] acnPrms us

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option)  (child:Asn1Type) (acnPrms:DastAcnParameter list) (us:State)  =
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
    let td = typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName

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
                            // Pass null (not "") so templates can detect empty body via <if(sInternalItem)> (e.g. NULL element type)
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
                        let extField = getExternalField r deps t.id
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
                        let tk = lm.lg.getTypeDefinition t.FT_TypeDefinition
                        let sType = lm.lg.getLongTypedefNameBasedOnModule tk p.modName
                        let internalItemBody = wrapWithAppend internalItem
                        let funcBodyContent = oct_sqf_null_terminated pp access (i level) internalItemBody noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits sType codec

                        let lv2 =
                            match codec, lm.lg.acn.checkBitPatternPresentResult with
                            | Decode, true -> [IntegerLocalVariable ("checkBitPatternPresentResult", Some (lm.lg.intValueToString 0I (ASN1SCC_Int8 (-128I, 127I))))]
                            | _ -> []

                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = lv2@(lv level)@localVariables; userDefinedFunctions=internalItem.userDefinedFunctions; bValIsUnReferenced= false; bBsIsUnReferenced=false; resultExpr=None; auxiliaries=internalItem.auxiliaries; icdResult = Some icd})
            ret,ns
    let soSparkAnnotations = Some(sparkAnnotations lm td codec)
    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations [] acnPrms us

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
        // Guard: newTypesMap may be empty when called via hasAcnEncodeFunction with emptyState
        if not (us.newTypesMap.ContainsKey(d.asn1Type)) then
            None, us
        else
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
                    let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch m.Name.Value
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
                    let presentWhenName = lm.lg.getChoiceChildPresentWhenName chc ch m.Name.Value
                    match pres with
                    | PresenceInt   (_, intVal) ->
                        raise(SemanticError(intVal.Location, "Unexpected presence condition. Expected string, found integer"))
                    | PresenceStr   (_, strVal) ->
                        let arrNulls = [0 .. ((int str.maxSize.acn)- strVal.Value.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
                        // Add null terminator if the language requires it
                        let bytesStr =
                            let baseBytes = System.Text.Encoding.ASCII.GetBytes strVal.Value
                            match lm.lg.nullTerminatorByte with
                            | Some nullByte -> Array.append baseBytes [| nullByte |]
                            | None -> baseBytes
                        let childTypeName =
                            match child.Type with
                            | AcnReferenceToIA5String t -> lm.lg.getLongTypedefName (lm.lg.definitionOrRef t.str.definitionOrRef)
                            | _ -> ""  // For other types, use empty string (shouldn't happen for string presence)
                        choiceDependencyStrPres_child v presentWhenName strVal.Value bytesStr arrNulls childTypeName)
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
                List.mapi(fun idx ch ->
                    let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = ch.Name.Value)
                    let choiceName = (lm.lg.getChoiceTypeDefinition chc.typeDef).typeName //chc.typeDef[Scala].typeName
                    // Pass the choice index (0, 1, etc.) for discriminator comparison
                    choiceDependencyEnum_Item v ch.presentWhenName choiceName (lm.lg.getNamedItemBackendName (Some (defOrRef2 r m enm)) enmItem) idx isOptional)
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
        let _errCodeName = ToC ("ERR_ACN" + (Encode.suffix.ToUpper()) + "_UPDATE_" + ((acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")))
        let errFieldPath = match acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
        let errCode, us = getNextValidErrorCode us _errCodeName None errFieldPath

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
                    not p.id.dropLast.lastItemIsOptional
                | AcnDepChoiceDeterminant (_, c, isOpt) -> not isOpt
                | AcnDepPresence _ | AcnDepPresenceStr _ -> false
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

type private SequenceChildState = {
    us: State
    childIx: bigint
    uperAccBits: bigint
    acnAccBits: bigint
    acnChildrenEncoded: (string * AcnChild) list
    processedAsn1Children: (string * Asn1Child) list  // Track Asn1Children for nested ACN child lookup
    acnChildrenFromSiblings: Map<string, (string * AcnChild)>  // Key: ACN child ID string, Value: (sibling_name, acnChild) - for deep field access
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

    let m =
        let (ReferenceToType tNodes) = t.id
        match tNodes with
        | (MD mn) :: _ -> r.modulesMap.[mn]
        | _ -> failwith "createSequenceFunction_inline: no module in type id"

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

        let acnChildrenToReturn = lm.lg.getAcnChildrenForDeepFieldAccess asn1Children acnChildren deps

        let handleChild (s: SequenceChildState) (childInfo: SeqChildInfo): SequenceChildResult * SequenceChildState =
            // This binding is suspect, isn't it
            //let stateHash = getStateHash s.us
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

            // Build ACN parameters for child that has dependencies
            let acnParamsForTemplate =
                match childInfo with
                | Asn1Child child ->
                    // Find dependencies of this Asn1Child where the dependency kind is AcnDepRefTypeArgument or AcnDepSizeDeterminant
                    // This tells us which parameters the child type needs and where they come from
                    // NOTE: Presence determinants (AcnDepPresence, AcnDepPresenceBool, AcnDepPresenceStr) control
                    // whether to decode the child at all, not what to pass to the child's decode function.
                    // They should NOT be included in the parameter list.
                    // NOTE: AcnDepSizeDeterminant params should only be passed to INLINE (anonymous) types.
                    // For reference types (named global types like EntityId), the size dep uses a field-instance ID
                    // that differs from the global type's ID, so the global encode_acn doesn't declare the param.
                    let childIsReference =
                        match child.Type.Kind with
                        | DAst.Asn1TypeKind.ReferenceType _ -> true
                        | _ -> false
                    deps.acnDependencies
                        |> List.filter(fun d ->
                            d.asn1Type = child.Type.id &&
                            match d.dependencyKind with
                            | AcnDepRefTypeArgument _ -> true
                            | AcnDepSizeDeterminant _
                            | AcnDepIA5StringSizeDeterminant _ -> not childIsReference
                            | AcnDepChoiceDeterminant _ -> true
                            | _ -> false)
                        |> List.choose(fun d ->
                            match d.determinant with
                            | AcnChildDeterminant acnCh ->
                                // Check if child sequence produces this ACN child locally
                                // If so, don't pass it as a parameter (for decode only)
                                let childProducesAcnChild =
                                    match codec with
                                    | Decode ->
                                        // Check if the child sequence has this ACN child in its own children list
                                        match child.Type.Kind with
                                        | Sequence childSeq ->
                                            childSeq.children
                                            |> List.exists(fun c ->
                                                match c with
                                                | AcnChild childAcnCh -> childAcnCh.id = acnCh.id
                                                | _ -> false)
                                        | _ -> false
                                    | Encode -> false  // For encode, always pass parameters

                                if childProducesAcnChild then
                                    // Skip this parameter - child produces it itself during decode
                                    None
                                else
                                    // Extract parameter name from the dependency kind
                                    // The determinant (acnCh) tells us which value to pass, but the dependency's
                                    // AcnDepRefTypeArgument contains the parameter with the correct name to use
                                    let targetParamName =
                                        match d.dependencyKind with
                                        | AcnDepRefTypeArgument param ->
                                            // Use the parameter name from child type's ACN parameter declaration
                                            param.c_name
                                        | AcnDepChoiceDeterminant _ ->
                                            // For choice determinants, use the determinant's c_name
                                            acnCh.c_name
                                        | _ ->
                                            // For other dependency kinds (e.g., size determinants), use determinant's c_name
                                            acnCh.c_name

                                    // The parameter gets its value from an ACN child that was encoded earlier
                                    // First try to find it in the current sequence's ACN children
                                    let found = s.acnChildrenEncoded
                                                |> List.tryFind(fun (_, encodedAcnCh) -> encodedAcnCh.id = acnCh.id)
                                    match found with
                                    | Some (varName, encodedAcnCh) ->
                                        // For Python, if the ACN child is an enumeration determinant,
                                        // we need to append .val to access the underlying enum value
                                        // EXCEPT for choice determinants, which need the wire value (discriminator)
                                        let valueExpr =
                                            match ProgrammingLanguage.ActiveLanguages.Head, codec with
                                            | Python, Decode ->
                                                // Check if this is an enumeration type by pattern matching on the ACN inserted type
                                                match encodedAcnCh.Type with
                                                | AcnReferenceToEnumerated _ ->
                                                    // For enumerated determinants, check the dependency kind
                                                    match d.dependencyKind with
                                                    | AcnDepChoiceDeterminant _
                                                    | AcnDepRefTypeArgument _ ->
                                                        // Pass the wrapper (not raw discriminator) so ChoiceChild_Enum_decode
                                                        // can match via the .val.name endswith check
                                                        varName
                                                    | _ ->
                                                        // For other uses, use the enum value
                                                        $"%s{varName}.val"
                                                | AcnReferenceToIA5String _ ->
                                                    // For string types in decode, use the parent identifier prefix
                                                    let parentId = p.accessPath.asIdentifier lm.lg
                                                    $"%s{parentId}_%s{varName}"
                                                | _ -> varName
                                            | _ -> varName
                                        Some $"%s{targetParamName}=%s{valueExpr}"
                                    | None ->
                                        // If not found in current sequence, check if it was returned by a sibling SEQUENCE
                                        match s.acnChildrenFromSiblings.TryFind (acnCh.id.ToString()) with
                                        | Some (siblingName, _) ->
                                            // Generate code to access ACN child from sibling's returned dict
                                            // For decode: use siblingName_acn_children['acnChildName']
                                            // For encode: use the ACN child's c_name as the variable name
                                            match codec with
                                            | Decode -> Some $"%s{targetParamName}=%s{siblingName}_acn_children['%s{acnCh.c_name}']"
                                            | Encode -> Some $"%s{targetParamName}=%s{acnCh.c_name}"
                                        | None ->
                                            // Not found - this might be an error, but let the original behavior handle it
                                            None
                            | AcnParameterDeterminant paramDet ->
                                // For decode, pass the parameter only if it is in scope in the current function,
                                // i.e., it is one of the parent type's own ACN parameters.
                                // If not in scope, a sibling AcnChildDeterminant dep handles the mapping.
                                let paramInScope = t.acnParameters |> List.exists (fun p -> p.c_name = paramDet.c_name)
                                match codec, paramInScope with
                                | Decode, true ->
                                    let targetParamName =
                                        match d.dependencyKind with
                                        | AcnDepRefTypeArgument param -> param.c_name
                                        | AcnDepChoiceDeterminant _ -> paramDet.c_name
                                        | _ -> paramDet.c_name
                                    Some $"%s{targetParamName}=%s{paramDet.c_name}"
                                | _ -> None
                        )
                | AcnChild _ -> []
                
            match childInfo with
            | Asn1Child child   ->
                // printfn "[DEBUG] handleChild: Processing Asn1Child %s in sequence %s" (child.Name.Value) (t.id.AsString)

                let childTypeDef = child.Type.typeDefinitionOrReference.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
                let childTypeDef = if lm.lg.shouldRemoveModulePrefixFromTypedef && childTypeDef.StartsWith((ToC t.moduleName) + ".") then childTypeDef.Substring(t.moduleName.Length + 1) else childTypeDef
                let childName = lm.lg.getAsn1ChildBackendName child

                let wrapUpdateFnc (md: Asn1AcnAst.Asn1Module) (rtt:ReferenceToType) (s: State) =
                    getUpdateFunctionUsedInEncoding r deps lm md rtt s
                let wrapDeterminantTypeFunc (d: Determinant) =
                    getDeterminantTypeDefinitionBodyWithinSeq r lm d
                let wrapInitExpr (mdl: Asn1AcnAst.Asn1Module) (t: AcnInsertedType) =
                    initExpr r lm mdl t
                
                let crossSeqAcnUpdateStmts, crossSeqAcnParamsList, ns0 = lm.lg.updateStateForCrossSequenceAcnParams r us p o.children child childNestingScope deps t codec wrapUpdateFnc wrapDeterminantTypeFunc wrapInitExpr
                
                // Merge cross-sequence ACN parameters with existing ACN parameters, dedup by param name
                let acnParamsForTemplate =
                    (acnParamsForTemplate @ crossSeqAcnParamsList)
                    |> List.distinctBy (fun s -> s.Split('=').[0])
                // printfn "[DEBUG] handleChild: Total ACN parameters for template: %d [%A]" acnParamsForTemplate.Length acnParamsForTemplate

                // Detect if inlining is required: when child sequence has ACN-inserted fields whose values come from outside
                let bInlineRequired = lm.lg.isAcnInlineRequired t childName deps

                let chFunc = child.Type.getAcnFunction codec
                let childSel = lm.lg.getSeqChildDependingOnChoiceParent nestingScope.parents p.accessPath childName child.Type.isIA5String child.Optionality.IsSome
                let childP =
                    let newArg = if lm.lg.usesWrappedOptional && childSel.isOptional && codec = Encode then childSel.asLast else childSel
                    {p with accessPath = newArg}

                let acnArgsForChild =
                    deps.acnDependencies
                    |> List.filter(fun d -> d.asn1Type = child.Type.id)
                    |> List.choose(fun d ->
                        match d.dependencyKind with
                        | AcnDepRefTypeArgument acnPrm ->
                            Some (AcnGenericTypes.RelativePath [], acnPrm)
                        | _ -> None
                    )

                let childContentResult, ns1 =
                    match chFunc with
                    | Some chFunc -> chFunc.funcBodyAsSeqComp ns0 acnArgsForChild childNestingScope childP childName bitStreamPositionsLocalVar
                    | None -> None, ns0

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
                        | Some (PresenceWhenBool relPath) ->
                            match codec with
                            | Encode -> None, [], [], None, ns1
                            | Decode ->
                                let extField = lm.lg.getExternalField (getExternalField0 r deps child.Type.id) relPath o p
                                let body (p: CodegenScope) (existVar: string option): string =
                                    assert existVar.IsSome
                                    sequence_presence_optChild_pres_bool (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName existVar.Value codec
                                Some body, [], [], Some extField, ns1
                        | Some (PresenceWhenBoolExpression exp)    ->
                            let _errCodeName = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")) + "_PRESENT_WHEN_EXP_FAILED")
                            let errFieldPath = match child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
                            let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None errFieldPath
                            let retExp = acnExpressionToBackendExpression o p exp
                            let existVar =
                                if codec = Decode then Some (ToC (child._c_name + "_exist"))
                                else None
                            let lv = existVar |> Option.toList |> List.map (fun v -> FlagLocalVariable (v, None))
                            let body (p: CodegenScope) (existVar: string option): string =
                                sequence_presence_optChild_pres_acn_expression (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName retExp existVar errCode.errCodeName codec
                            Some body, lv, [errCode], existVar, ns1a
                    | _ -> None, [], [], None, ns1

                // Check if this child has ACN children to return to siblings
                let childHasAcnChildrenToReturn = acnChildrenToReturn.ContainsKey childName

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
                                sequence_always_present_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName None childResultExpr childTypeDef soSaveBitStrmPosStatement true acnParamsForTemplate childHasAcnChildrenToReturn None bInlineRequired codec
                            Some childBody, [], [], [], childResultExpr, [], ns2
                        | _ -> None, [], [], [], childResultExpr, [], ns2
                    | Some childContent ->
                        let isPrimitiveType =
                            match (lm.lg.getTypeDefinition child.Type.FT_TypeDefinition) with
                            | FE_PrimitiveTypeDefinition t -> t.kind.IsPrimitiveReference2RTL
                            | _ -> false

                        // Calculate field-level alignment code separately (only for non-primitives)
                        let alignmentCodeForTemplate =
                            if isPrimitiveType then None
                            else
                                match child.Type.acnAlignment with
                                | None -> None
                                | Some acnAlign ->
                                    let alStr, nAlignmentVal = getAlignmentString lm acnAlign
                                    let alignmentCode = lm.acn.alignToNext "" alStr nAlignmentVal childNestingScope.acnOffset (childNestingScope.acnOuterMaxSize - childNestingScope.acnOffset) (childNestingScope.nestingLevel - 1I) childNestingScope.nestingIx childNestingScope.acnRelativeOffset codec
                                    Some alignmentCode

                        let childBody (p: CodegenScope) (existVar: string option): string =
                            let pp, _ = joinedOrAsIdentifier lm codec p
                            match child.Optionality with
                            | None ->
                                sequence_mandatory_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody soSaveBitStrmPosStatement childTypeDef isPrimitiveType acnParamsForTemplate childHasAcnChildrenToReturn alignmentCodeForTemplate bInlineRequired codec
                            | Some Asn1AcnAst.AlwaysAbsent ->
                                sequence_always_absent_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody childTypeDef soSaveBitStrmPosStatement isPrimitiveType codec
                            | Some Asn1AcnAst.AlwaysPresent ->
                                sequence_always_present_child pp (lm.lg.getAccess p.accessPath) childName (Some childContent.funcBody) childContent.resultExpr childTypeDef soSaveBitStrmPosStatement isPrimitiveType acnParamsForTemplate childHasAcnChildrenToReturn alignmentCodeForTemplate bInlineRequired codec
                            | Some (Asn1AcnAst.Optional opt)   ->
                                assert (codec = Encode || existVar.IsSome)
                                match opt.defaultValue with
                                | None ->
                                    sequence_optional_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody existVar childContent.resultExpr childTypeDef soSaveBitStrmPosStatement isPrimitiveType acnParamsForTemplate childHasAcnChildrenToReturn alignmentCodeForTemplate bInlineRequired codec
                                | Some v ->
                                    let defInit= child.Type.initFunction.initByAsn1Value childP (mapValue v).kind
                                    sequence_default_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody defInit existVar childContent.resultExpr childTypeDef soSaveBitStrmPosStatement isPrimitiveType childHasAcnChildrenToReturn alignmentCodeForTemplate codec
                        let lvs =
                            match child.Optionality with
                            | Some Asn1AcnAst.AlwaysAbsent -> []
                            | _ -> childContent.localVariables
                        // For non-primitive children in decode mode for python, the template generates variables as <parentId>_<childName>
                        // So we need to override the resultExpr to match what the template generates
                        let adjustedResultExpr =
                            match codec, lm.lg.decodingKind, isPrimitiveType, ProgrammingLanguage.ActiveLanguages.Head with
                            | Decode, Copy, false, Python ->
                                let parentId = p.accessPath.asIdentifier lm.lg
                                Some $"%s{parentId}_%s{childName}"
                            | _ -> childContent.resultExpr
                        Some childBody, lvs, childContent.userDefinedFunctions, childContent.errCodes, adjustedResultExpr, childContent.auxiliaries, ns2

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

                // Include cross-sequence ACN update statements before the child encode call
                // printfn "[DEBUG] handleChild: Including %d cross-sequence ACN update statements in result" crossSeqAcnUpdateStmts.Length
                let allStmts = crossSeqAcnUpdateStmts @ [stmts]
                let res = {stmts=allStmts; resultExpr=childResultExpr; existVar=existVar; props=props; auxiliaries=auxiliaries @ optAux; icdResult=icdResult}

                // Check if this ASN.1 child needs to return ACN children to siblings
                let newAcnChildrenFromSiblings =
                    match acnChildrenToReturn.TryFind childName with
                    | Some acnChildrenList ->
                        // Add these ACN children to the map so siblings can reference them
                        acnChildrenList
                        |> List.fold (fun acc (acnCName, acnCh) ->
                            acc |> Map.add (acnCh.id.ToString()) (childName, acnCh)
                        ) s.acnChildrenFromSiblings
                    | None -> s.acnChildrenFromSiblings

                // Keep the accumulated ACN children so subsequent children can reference them
                // Also track processed Asn1Children so their nested ACN children can be referenced
                let newAcc = {us=ns3; childIx=s.childIx + 1I; uperAccBits=s.uperAccBits + child.uperMaxSizeInBits; acnAccBits=s.acnAccBits + child.acnMaxSizeInBits; acnChildrenEncoded = s.acnChildrenEncoded; processedAsn1Children = (childName, child) :: s.processedAsn1Children; acnChildrenFromSiblings = newAcnChildrenFromSiblings}
                res, newAcc
            | AcnChild acnChild ->
                // Check if this ACN child has dependencies on fields outside the current sequence
                let acnChildDeps =
                    deps.acnDependencies
                    |> List.filter (fun d -> d.determinant.id = acnChild.id)

                let hasExternalDependency =
                    let currentSeqPath = t.id.AsString
                    // Get parent sequence path if exists
                    // Try to find a parent that is actually different from current
                    let parentSeqPath =
                        match childNestingScope.parents with
                        | (_, parentType) :: rest ->
                            let parentPath = parentType.id.AsString
                            // If parent is same as current, try the next level up (grandparent)
                            if parentPath = currentSeqPath && not rest.IsEmpty then
                                let (_, grandparentType) = rest.Head
                                Some (grandparentType.id.AsString)
                            else
                                Some parentPath
                        | [] -> None

                    acnChildDeps
                    |> List.exists (fun dep ->
                        let depFieldPath = dep.asn1Type.AsString
                        // Check if the dependency is outside the current sequence
                        let isOutsideCurrent = not (depFieldPath.StartsWith(currentSeqPath + ".") || depFieldPath = currentSeqPath)

                        if not isOutsideCurrent then
                            // Dependent is in current sequence, not external
                            false
                        else
                            // Dependent is outside current sequence, check if it's in parent
                            match parentSeqPath with
                            | Some parentPath ->
                                let isInParent = depFieldPath.StartsWith(parentPath + ".") || depFieldPath = parentPath
                                // If dependent is in parent, we're being inlined, so DON'T treat as external
                                not isInParent
                            | None ->
                                // No parent, truly external
                                true
                    )

                //handle updates
                let childP = {CodegenScope.modName = p.modName; accessPath= AccessPath.valueEmptyPath (getAcnDeterminantName acnChild.id)}

                let updateStatement, ns1 =
                    match codec with
                    | Encode ->
                        // Skip update code generation if this ACN child has external dependencies
                        // (it will be provided as a parameter from the parent)
                        if hasExternalDependency && lm.lg.isObjectOriented then
                            None, us
                        else
                            let pRoot : CodegenScope = lm.lg.getParamType t codec
                            // In standalone OO context, dep paths may include this type's field prefix.
                            // Strip t.id's field nodes from dep asn1Type paths so getAccessFromScopeNodeList
                            // generates self.field instead of self.thisType.field.
                            let effectiveFnc, ns0 =
                                if nestingScope.parents.IsEmpty && lm.lg.isObjectOriented then
                                    let (ReferenceToType tNodes) = t.id
                                    let tFieldNodes = tNodes |> List.skip 2  // skip MD and TA
                                    let tFieldCount = tFieldNodes.Length
                                    if tFieldCount > 0 then
                                        let hasPathsToStrip =
                                            acnChildDeps |> List.exists (fun d ->
                                                let (ReferenceToType dNodes) = d.asn1Type
                                                match dNodes with
                                                | _ :: _ :: dFields ->
                                                    dFields.Length > tFieldCount &&
                                                    (List.take tFieldCount dFields) = tFieldNodes
                                                | _ -> false)
                                        if hasPathsToStrip then
                                            let strippedDeps =
                                                deps.acnDependencies
                                                |> List.map (fun d ->
                                                    let (ReferenceToType dNodes) = d.asn1Type
                                                    match dNodes with
                                                    | (MD dMod) :: (TA dTas) :: dFields
                                                        when d.determinant.id = acnChild.id &&
                                                             dFields.Length > tFieldCount &&
                                                             (List.take tFieldCount dFields) = tFieldNodes ->
                                                        {d with asn1Type = ReferenceToType ((MD dMod) :: (TA dTas) :: (List.skip tFieldCount dFields))}
                                                    | _ -> d)
                                            let strippedDepsRecord = {deps with acnDependencies = strippedDeps}
                                            getUpdateFunctionUsedInEncoding r strippedDepsRecord lm m acnChild.id us
                                        else
                                            acnChild.funcUpdateStatement, us
                                    else
                                        acnChild.funcUpdateStatement, us
                                else
                                    acnChild.funcUpdateStatement, us
                            let updateStatement, lvs, errCodes, icdComments =
                                match effectiveFnc with
                                | Some f -> Some (f.updateAcnChildFnc acnChild childNestingScope childP pRoot), f.localVariables, f.errCodes, f.icdComments
                                | None -> None, [], [], []
                            Some {body=updateStatement; lvs=lvs; errCodes=errCodes; userDefinedFunctions = []; icdComments=icdComments}, ns0
                    | Decode -> None, us

                //acn child encode/decode
                let childEncDecStatement, auxiliaries, ns2 =
                    // Skip encoding/decoding this ACN child if it has external dependencies (for object-oriented languages)
                    if hasExternalDependency && lm.lg.isObjectOriented then
                        None, [], ns1
                    else
                        let chFunc = acnChild.funcBody codec
                        let childContentResult = chFunc [] childNestingScope childP bitStreamPositionsLocalVar
                        match childContentResult with
                        | None              -> None, [], ns1
                        | Some childContent ->
                            let isPrimitiveType, sType =
                                match acnChild.Type with
                                | AcnInteger _
                                | AcnNullType _
                                | AcnBoolean _ -> true, ""
                                | AcnReferenceToEnumerated t -> true, lm.lg.getLongTypedefName (lm.lg.definitionOrRef t.enumerated.definitionOrRef)
                                | AcnReferenceToIA5String t -> false, lm.lg.getLongTypedefName (lm.lg.definitionOrRef t.str.definitionOrRef)

                            let pp, _ = joinedOrAsIdentifier lm codec p
                            match codec with
                            | Encode   ->
                                match acnChild.Type with
                                | Asn1AcnAst.AcnNullType _   ->
                                    let childBody = Some (sequence_mandatory_child pp (lm.lg.getAccess p.accessPath) acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement "" isPrimitiveType acnParamsForTemplate false None false codec)
                                    Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=childContent.errCodes;icdComments=[]}, childContent.auxiliaries, ns1

                                | _             ->
                                    let _errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((acnChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elem")) + "_UNINITIALIZED")
                                    let errFieldPath = match acnChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elem")
                                    let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None errFieldPath
                                    let childBody = Some (sequence_acn_child acnChild.c_name childContent.funcBody errCode.errCodeName soSaveBitStrmPosStatement isPrimitiveType codec)
                                    Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=errCode::childContent.errCodes; icdComments=[]}, childContent.auxiliaries, ns1a
                            | Decode    ->
                                let childBody = Some (sequence_mandatory_child pp (lm.lg.getAccess p.accessPath) acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement sType isPrimitiveType acnParamsForTemplate false None false codec)
                                Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=childContent.errCodes; icdComments=[]}, childContent.auxiliaries, ns1

                let stmts = (updateStatement |> Option.toList)@(childEncDecStatement |> Option.toList)
                let icdComments = stmts |> List.collect(fun z -> z.icdComments)
                // Note: uperMaxSizeBits and uperAccBits here do not make sense since we are in ACN
                let props = {info=childInfo.toAsn1AcnAst; sel=childP.accessPath; uperMaxOffset=s.uperAccBits; acnMaxOffset=s.acnAccBits}
                let icdResult = icd_acn_child acnChild icdComments
                let res =  {stmts=stmts; resultExpr=None; existVar=None; props=props; auxiliaries=auxiliaries; icdResult=icdResult}
                // Record this ACN child so subsequent Asn1 children can reference it
                // Only add to acnChildrenEncoded if it was actually encoded/decoded (not skipped due to external dependencies)
                let newAcnChildrenEncoded =
                    if hasExternalDependency && lm.lg.isObjectOriented then
                        s.acnChildrenEncoded  // Don't add - was skipped
                    else
                        (acnChild.c_name, acnChild) :: s.acnChildrenEncoded  // Add as normal
                let newAcc = {us=ns2; childIx=s.childIx + 1I; uperAccBits=s.uperAccBits; acnAccBits=s.acnAccBits + acnChild.Type.acnMaxSizeInBits; acnChildrenEncoded = newAcnChildrenEncoded; processedAsn1Children = s.processedAsn1Children; acnChildrenFromSiblings = s.acnChildrenFromSiblings}
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
        let (childrenStatements00: SequenceChildResult list), scs = children |> foldMap handleChild {us=us; childIx=nbPresenceBits; uperAccBits=nbPresenceBits; acnAccBits=nbPresenceBits; acnChildrenEncoded = []; processedAsn1Children = []; acnChildrenFromSiblings = Map.empty}
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
                    if lm.lg.usesWrappedOptional || childrenExistVar.IsEmpty || ProgrammingLanguage.ActiveLanguages.Head = Python then []
                    else
                        let existTd = (lm.lg.getSequenceTypeDefinition o.typeDef).exist
                        [lm.init.initSequenceExpr existTd childrenExistVar []]
                let resultExpr = (p.accessPath.asIdentifier lm.lg)
                Some resultExpr, [lm.uper.sequence_build resultExpr (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) p.accessPath.isOptional (existSeq@childrenResultExpr)]
            | _ -> None, []

        let acnChildrenDictStmts, tupleReturn = lm.lg.getAcnChildrenDictStatements codec scs.acnChildrenEncoded p
        // When a sequence receives ACN parameters (bHasAcnChildrenToReturn = true due to receivesParameters),
        // but has no inline ACN children to return, we still need to define the _acn_children dict.
        let acnChildrenDictStmts =
            if ProgrammingLanguage.ActiveLanguages.Head = Python && codec = Decode && t.acnParameters.Length > 0 && acnChildrenDictStmts.IsEmpty then
                [$"%s{p.accessPath.lastIdOrArr}_acn_children = {{}}"]
            else
                acnChildrenDictStmts

        let proof = lm.lg.generateSequenceProof r ACN t o nestingScope p.accessPath codec
        let aux = lm.lg.generateSequenceAuxiliaries r ACN t o nestingScope p.accessPath codec
        let seqContent =  (saveInitialBitStrmStatements@childrenStatements@(post_encoding_function |> Option.map fst |> Option.toList)@seqBuild@acnChildrenDictStmts@proof) |> nestChildItems lm codec

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
            // Only raise error for Encode - Decode reads ACN fields from bitstream
            match codec, ProgrammingLanguage.ActiveLanguages.Head with
            | Decode, Python ->
                // For decode, continue with the sequence content even if ACN children have no updates
                match seqContent with
                | None  ->
                    match lm.lg.decodeEmptySeq (p.accessPath.joined lm.lg) with
                    | None -> None, ns
                    | Some decodeEmptySeq ->
                        Some ({AcnFuncBodyResult.funcBody = decodeEmptySeq; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; userDefinedFunctions = childrenUserDefFuncs; bValIsUnReferenced= false; bBsIsUnReferenced=true; resultExpr=Some decodeEmptySeq; auxiliaries=childrenAuxiliaries @ aux; icdResult = Some icd}), ns
                | Some ret ->
                    Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; userDefinedFunctions = childrenUserDefFuncs; bValIsUnReferenced= false; bBsIsUnReferenced=(o.acnMaxSizeInBits = 0I); resultExpr=resultExpr; auxiliaries=childrenAuxiliaries @ aux; icdResult = Some icd}), ns
            | _, _ ->
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
            //Console.Out.WriteLine (FrontEndMain.formatSemanticWarning loc errMessage)
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
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)

    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody isTestVaseValid soSparkAnnotations [] acnPrms us



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

    let typeDefinitionName = defOrRef.longTypedefName2  (Some lm.lg) lm.lg.hasModules t.moduleName//getTypeDefinitionName t.id.tasInfo typeDefinition
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
        // If all alternatives are always-absent, there's nothing to encode/decode
        if children.IsEmpty then None, us
        else
        let td = (lm.lg.getChoiceTypeDefinition o.typeDef).longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
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
            let sChildName = (lm.lg.getAsn1ChChildBackendName child)
            let sChildTypeDef = child.chType.typeDefinitionOrReference.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName
            let childContentResult, ns1 =
                match chFunc with
                | Some chFunc ->
                    let childP =
                        match codec, lm.lg.choiceChildDecodePath sChildTypeDef sChildName with
                        | Decode, Some customPath -> {CodegenScope.modName = p.modName; accessPath = customPath}
                        | _ ->
                            if lm.lg.acn.choice_requires_tmp_decoding && codec = Decode then
                                {CodegenScope.modName = p.modName; accessPath = AccessPath.valueEmptyPath ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")}
                            else {p with accessPath = lm.lg.getChChildForKind p.accessPath (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String child.chType.Kind codec}
                    
                    let acnArgsForChild =
                      deps.acnDependencies
                      |> List.filter(fun d -> d.asn1Type = child.chType.id)
                      |> List.choose(fun d ->
                          match d.dependencyKind with
                          | AcnDepPresence (relPath, _) ->
                              match d.determinant with
                              | AcnParameterDeterminant acnPrm -> Some (relPath, acnPrm)
                              | _ -> None
                          | AcnDepPresenceStr (relPath, _, _) ->
                              match d.determinant with
                              | AcnParameterDeterminant acnPrm -> Some (relPath, acnPrm)
                              | _ -> None
                          | AcnDepRefTypeArgument acnPrm ->
                              // Use empty path for direct parameter references
                              Some (AcnGenericTypes.RelativePath [], acnPrm)
                          | _ -> None
                      )
                    chFunc.funcBody us acnArgsForChild childNestingScope childP
                    
                    // upstream:
                    // todo: check if that is ok
                    // chFunc.funcBody us acnArgs childNestingScope childP
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
                let childContent_funcBody = lm.lg.adaptFuncBodyChoice child.chType.Kind codec lm.uper ACN childContent_funcBody sChildTypeDef sChildName
                match child.Optionality with
                | Some (ChoiceAlwaysAbsent) -> Some (choiceChildAlwaysAbsent (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.presentWhenName (Some defOrRef) child) (BigInteger idx) errCode.errCodeName codec)
                | Some (ChoiceAlwaysPresent)
                | None  ->
                    match ec with
                    | CEC_uper  ->
                        Some (choiceChild (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.presentWhenName (Some defOrRef) child) (BigInteger idx) nIndexSizeInBits nMax childContent_funcBody sChildName sChildTypeDef typeDefinitionName sChildInitExpr codec)
                    | CEC_enum (enm,_) ->
                        let getDefOrRef (a:Asn1AcnAst.ReferenceToEnumerated) =
                            match p.modName = ToC a.modName with
                            | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}
                            | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName); typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}


                        let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = child.Name.Value)
                        Some (choiceChild_Enum (p.accessPath.joinedEnum lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.getNamedItemBackendName (Some (getDefOrRef enm)) enmItem) (lm.lg.presentWhenName (Some defOrRef) child) childContent_funcBody sChildName sChildTypeDef typeDefinitionName sChildInitExpr codec)
                    | CEC_presWhen  ->
                        let isPrimitiveType =
                            match (lm.lg.getTypeDefinition child.chType.FT_TypeDefinition) with
                            | FE_PrimitiveTypeDefinition t -> t.kind.IsPrimitiveReference2RTL
                            | _ -> false
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
                                // Add null terminator if the language requires it
                                let bytesStr =
                                    let baseBytes = System.Text.Encoding.ASCII.GetBytes strVal.Value
                                    match lm.lg.nullTerminatorByte with
                                    | Some nullByte -> Array.append baseBytes [| nullByte |]
                                    | None -> baseBytes
                                choiceChild_preWhen_str_condition extField strVal.Value arrNulls bytesStr
                        let conds = child.acnPresentWhenConditions |>List.map handPresenceCond
                        let pp, _ = joinedOrAsIdentifier lm codec p
                        // Compute ACN parameters to pass to the child's encode_acn/decode_acn call (for present-when choices whose alternatives need ACN params)
                        let acnParamsForChoiceChild =
                            deps.acnDependencies
                            |> List.filter(fun d ->
                                d.asn1Type = child.chType.id &&
                                match d.dependencyKind with
                                | AcnDepRefTypeArgument _ -> true
                                | _ -> false)
                            |> List.choose(fun d ->
                                match d.determinant with
                                | AcnParameterDeterminant paramDet ->
                                    let paramInScope = t.acnParameters |> List.exists (fun p -> p.c_name = paramDet.c_name)
                                    match paramInScope with
                                    | true ->
                                        let targetParamName =
                                            match d.dependencyKind with
                                            | AcnDepRefTypeArgument param -> param.c_name
                                            | _ -> paramDet.c_name
                                        Some $"%s{targetParamName}=%s{paramDet.c_name}"
                                    | false -> None
                                | _ -> None)
                        Some (choiceChild_preWhen pp (lm.lg.getAccess p.accessPath) (lm.lg.presentWhenName (Some defOrRef) child) childContent_funcBody conds (idx=0) sChildName sChildTypeDef typeDefinitionName sChildInitExpr isPrimitiveType acnParamsForChoiceChild codec)
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
            let lang = ProgrammingLanguage.ActiveLanguages.Head
            match ec with
            | CEC_uper        ->
                // Console.WriteLine("CEC_uper for " + sChoiceIndexName)
                choice_uper pp access childrenStatements nMax sChoiceIndexName td nIndexSizeInBits errCode.errCodeName codec, resultExpr
            | CEC_enum   enm  ->
                let extField = getExternalField r deps t.id
                // Console.WriteLine("CEC_enum for " + sChoiceIndexName + " (" + extField + ")")
                match lang with
                | Python ->
                    // Console.WriteLine("Using choice_uper in CEC_enum")
                    // let otherStuff = choice_Enum pp access childrenStatements extField td errCode.errCodeName codec
                    // let myStuff = choice_uper pp access childrenStatements nMax sChoiceIndexName td nIndexSizeInBits errCode.errCodeName codec
                    //"CHOICE CEC_ENUM ORIGINAL\n" + otherStuff + "\nCHOICE ENUM MYSTUFF\n" + myStuff + "\nCHOICE ENUM DONE", resultExpr
                    choice_Enum pp access childrenStatements extField td errCode.errCodeName codec, resultExpr
                | _ -> choice_Enum pp access childrenStatements extField td errCode.errCodeName codec, resultExpr
            | CEC_presWhen    ->
                // Console.WriteLine("CEC_presWhen for " + sChoiceIndexName)
                match lang with
                | Python ->
                    // Console.WriteLine("Using choice_uper in CEC_presWhen")
                    // let otherStuff = choice_preWhen pp  access childrenStatements td errCode.errCodeName codec
                    // let myStuff = choice_uper pp access childrenStatements nMax sChoiceIndexName td nIndexSizeInBits errCode.errCodeName codec
                    //"CHOICE CEC_PRESWHEN ORIGINAL\n" + otherStuff + "\nCHOICE PRESWHEN MYSTUFF\n" + myStuff + "\nCHOICE PRESWHEN DONE", resultExpr
                    choice_preWhen pp  access childrenStatements td errCode.errCodeName codec, resultExpr
                | _ -> choice_preWhen pp  access childrenStatements td errCode.errCodeName codec, resultExpr
        let choiceContent = lm.lg.generateChoiceProof r ACN t o choiceContent p.accessPath codec
        let aux = lm.lg.generateChoiceAuxiliaries r ACN t o nestingScope p.accessPath codec
        Some ({AcnFuncBodyResult.funcBody = choiceContent; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; userDefinedFunctions=childrenUserDefFuncs; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=childrenAuxiliaries@aux; icdResult = Some icd}), ns


    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)


    createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations [] acnPrms us, ec

let emptyIcdFnc fieldName sPresent comments  = [],[]

let createReferenceFunction_inline (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (acnPrms:DastAcnParameter list) (us:State)  =
  // printfn "[DEBUG] createReferenceFunction: t.id=%s t.acnAlignment=%A codec=%A" (t.id.AsString) t.acnAlignment codec
  let baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t "_ACN" codec

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
      // Check if we need to avoid the fast path due to alignment
      let shouldUseSlowPath = t.acnAlignment.IsSome
      match o.hasExtraConstrainsOrChildrenOrAcnArgs && not shouldUseSlowPath with
      | true  ->
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
                let funcBodyContent = funcBodyContent
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
            // printfn "[DEBUG] createReferenceFunction: calling createAcnFunction for %s with acnAlignment=%A" (t.id.AsString) t.acnAlignment
            let a, ns = createAcnFunction r deps lm codec t typeDefinition  isValidFunc funcBody (fun atc -> true) soSparkAnnotations [] acnPrms ns
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

        let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
        let a,b = createAcnFunction r deps lm codec t typeDefinition  isValidFunc  (fun us e acnArgs nestingScope p -> funcBody us e acnArgs nestingScope p) (fun atc -> true) soSparkAnnotations [] acnPrms us
        Some a, b)


