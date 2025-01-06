module DAstXer
open System
open System.Numerics
open System.IO

open Asn1AcnAstUtilFunctions
open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions
open Language



type XerTag with
    member this.p =
        match this with
        | XerLiteralConstant   tagValue      -> "\"" + tagValue + "\""
        | XerFunctionParameter (_,prmName)   -> prmName


let rec XerTagFnc (t:Asn1AcnAst.Asn1Type) (r:Asn1AcnAst.AstRoot) =
    match (Asn1AcnAstUtilFunctions.GetActualType t r).Kind with
    | Asn1AcnAst.Enumerated(_) | Asn1AcnAst.Choice(_) | Asn1AcnAst.Boolean _  -> None
    | _     ->
        match t.Kind with
        | Asn1AcnAst.ReferenceType ref    -> Some (XerLiteralConstant ref.tasName.Value)
        | Asn1AcnAst.Integer _            -> Some (XerLiteralConstant "INTEGER")
        | Asn1AcnAst.BitString _          -> Some (XerLiteralConstant "BIT-STRING")
        | Asn1AcnAst.OctetString _        -> Some (XerLiteralConstant "OCTET-STRING")
        | Asn1AcnAst.Boolean      _       -> None
        | Asn1AcnAst.Choice(_)            -> None
        | Asn1AcnAst.Enumerated(_)        -> None
        | Asn1AcnAst.IA5String     _      -> Some (XerLiteralConstant "IA5String")
        | Asn1AcnAst.NumericString  _     -> Some (XerLiteralConstant "NumericString")
        | Asn1AcnAst.NullType _           -> Some (XerLiteralConstant "NULL")
        | Asn1AcnAst.Real      _          -> Some (XerLiteralConstant "REAL")
        | Asn1AcnAst.Sequence(_)          -> Some (XerLiteralConstant "SEQUENCE")
        | Asn1AcnAst.SequenceOf(_)        -> Some (XerLiteralConstant "SEQUENCE-OF")
        | Asn1AcnAst.ObjectIdentifier _   -> Some (XerLiteralConstant "OBJECT-IDENTIFIER")
        | Asn1AcnAst.TimeType tc          -> Some (XerLiteralConstant (CommonTypes.timeTypeToAsn1Str tc.timeClass))


let private aux (s:string) = 2 * (s.Length + 1) + 1 |> BigInteger

let getMaxSizeInBytesForXER_boolean ()  =  BigInteger ("<false/>".Length)  //<>
let getMaxSizeInBytesForXER_Integer ()  =  BigInteger (System.Int64.MinValue.ToString().Length)
let getMaxSizeInBytesForXER_Enumerated (itemNames : string list) =
    let maxName = itemNames |>  Seq.max
    aux maxName
let getMaxSizeInBytesForXER_BitString   maxSize = maxSize * 8I
let getMaxSizeInBytesForXER_OctetString maxSize = maxSize * 2I
let getMaxSizeInBytesForXER_Real        = 50I
let getMaxSizeInBytesForXER_NullType    = 0I
let getMaxSizeInBytesForXER_IA5String   maxSize = maxSize
let getMaxSizeInBytesForXER_NumericString   maxSize = maxSize

let getMaxSizeInBytesForXER  (xmlTag:XerTag) (contentSize:BigInteger) : BigInteger =
    let tagValue =
        match  xmlTag with
        | XerLiteralConstant   tagValue
        | XerFunctionParameter (tagValue, _)    -> tagValue

    match tagValue with
    | null  -> contentSize
    | _     -> (2I * (BigInteger(tagValue.Length + 2))) + 1I + contentSize


let getFuncName (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros)  (codec:CommonTypes.Codec) (typeDefinition:TypeDefinitionOrReference) =
    getFuncNameGeneric  typeDefinition ("_XER" + codec.suffix)

let createXerFunction_any (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) xerFuncBody_e  soSparkAnnotations (us:State)  =
    let emitTypeAssignment      = lm.xer.EmitTypeAssignment
    let emitTypeAssignment_def  = lm.xer.EmitTypeAssignment_def
    let EmitTypeAssignment_def_err_code  = lm.uper.EmitTypeAssignment_def_err_code

    let funcName            = getFuncName r lm codec typeDefinition
    let p  =  lm.lg.getParamType t codec
    let varName = p.arg.receiverId
    let sStar = lm.lg.getStar p.arg
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let sInitialExp = ""
    let errCodeName         = ToC ("ERR_XER" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let defaultErrCode, ns = getNextValidErrorCode us errCodeName None
    let xerFuncBody = (xerFuncBody_e defaultErrCode)

    let  xerFunc, xerFuncDef, encodingSizeInBytes  =
        match funcName  with
        | None              -> None, None, 0I
        | Some funcName     ->
            let asn1TasName = typeDefinition.getAsn1Name r.args.TypePrefix
            let xerBdResultOpt : XERFuncBodyResult option= xerFuncBody_e defaultErrCode p  (Some (XerFunctionParameter (asn1TasName, "xmlTag")))

            let bodyResult_funcBody, errCodes,  bodyResult_localVariables, encodingSizeInBytes =
                match xerBdResultOpt with
                | Some bodyResult   ->
                    bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.encodingSizeInBytes
                | None              ->
                    let emptyStatement = lm.lg.emptyStatement
                    emptyStatement, [], [], 0I

            let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct
            let xerFunc = Some(emitTypeAssignment asn1TasName varName sStar funcName isValidFuncName  (lm.lg.getLongTypedefName typeDefinition) lvars  bodyResult_funcBody soSparkAnnotations sInitialExp codec)

            let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
            let xerFuncDef = Some(emitTypeAssignment_def varName sStar funcName  (lm.lg.getLongTypedefName typeDefinition) errCodStr (encodingSizeInBytes = 0I) (encodingSizeInBytes)  soSparkAnnotations codec)
            xerFunc, xerFuncDef, encodingSizeInBytes
    let ret =
        {
            XerFunctionRec.funcName       = funcName
            func                       = xerFunc
            funcDef                    = xerFuncDef
            encodingSizeInBytes        = encodingSizeInBytes
            funcBody_e                 = xerFuncBody_e
            funcBody                   = xerFuncBody
        }
    (XerFunction ret), ns

let orElse defaultValue optValue = match optValue with Some x -> x | None -> defaultValue

let createIntegerFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (us:State)  =
    let Integer         = lm.xer.Integer
    let IntegerPos      = lm.xer.IntegerPos
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "INTEGER")
        let checkExp =
            match (DastValidate2.createIntegerFunctionByCons r lm (o.intClass) (o.cons@o.withcons)) with
            | None  ->  None
            | Some expFunc ->
                let makeExpressionToStatement0  = lm.isvalid.makeExpressionToStatement0
                match expFunc p with
                | VCBTrue               -> Some (makeExpressionToStatement0 "true")
                | VCBFalse              -> Some (makeExpressionToStatement0 "false")
                | VCBExpression sExp    -> Some (makeExpressionToStatement0 sExp)
                | VCBStatement (sStat,_)    -> Some sStat

                //Some (expFunc p)
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Integer ()
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm =
            match o.isUnsigned with
            | true  -> IntegerPos pp xmlTag.p nLevel checkExp errCode.errCodeName codec
            | false -> Integer pp xmlTag.p nLevel checkExp errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let checkExp (isValidFunc: IsValidFunction option) p =
    match isValidFunc with
    | None  ->  None
    | Some fnc -> None
//        match fnc.funcExp with
//        | None  -> None
//        | Some expFunc -> (Some (expFunc p))

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Boolean =   lm.xer.Boolean
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "")
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_boolean ()
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = Boolean pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createRealFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Real = lm.xer.Real
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "REAL")
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Real
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = Real pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let ObjectIdentifier = lm.xer.ObjectIdentifier
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "ObjectIdentifier")
        let pp = match codec with CommonTypes.Encode -> lm.lg.getPointer p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Real
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = ObjectIdentifier pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let TimeType = lm.xer.TimeType
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "TIME")
        let pp = match codec with CommonTypes.Encode -> lm.lg.getPointer p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Real
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = TimeType pp (DAstUPer.getTimeSubTypeByClass o.timeClass) xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let nullFunc = lm.xer.Null
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let pp = match codec with CommonTypes.Encode -> lm.lg.getPointer p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "NULL")
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let totalSize = getMaxSizeInBytesForXER xmlTag 0I
        let bodyStm = nullFunc pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Enumerated = lm.xer.Enumerated
    let Enumerated_item = lm.xer.Enumerated_item
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "")
        let pp = lm.lg.getValue p.arg
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Enumerated (o.items |> List.map (fun itm -> itm.Name.Value))
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let arrItems =
            o.items |> List.mapi(fun i it -> Enumerated_item pp xmlTag.p nLevel (lm.lg.getNamedItemBackendName (Some typeDefinition) it) it.Name.Value errCode.errCodeName (i=0) codec)
        let bodyStm = Enumerated pp xmlTag.p nLevel arrItems (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createIA5StringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let String = lm.xer.String
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant (if o.isNumeric then "NumericString" else "IA5String"))
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_IA5String o.maxSize.uper
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = String pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let OctetString = lm.xer.OctetString
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "OCTET-STRING")
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_OctetString o.maxSize.uper
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = OctetString (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) xmlTag.p nLevel o.maxSize.uper (o.minSize.uper=o.maxSize.uper) (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createBitStringFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let BitString = lm.xer.BitString
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "BIT-STRING")
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_BitString o.maxSize.uper
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = BitString (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) xmlTag.p nLevel o.maxSize.uper (o.minSize.uper=o.maxSize.uper) (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (child:Asn1Type) (us:State)  =
    let SequenceOf = lm.xer.SequenceOf
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "SEQUENCE-OF")
        let ii = p.arg.SequenceOfLevel + 1
        let i = sprintf "i%d" ii
        let lv = SequenceOfIndex (ii + 1, None)
        let pp =  lm.lg.getPointer p.arg
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let chFunc =
            match child.getXerFunction codec with
            | XerFunction z -> z
            | XerFunctionDummy  -> raise (BugErrorException "XerFunctionDummy")

        let childTag =
            let definedInRTL =
                match child.typeDefinitionOrReference with
                | TypeDefinition  td -> false
                | ReferenceToExistingDefinition ref -> ref.definedInRtl
            match definedInRTL with
            | false ->
                let childTagName = (child.typeDefinitionOrReference).getAsn1Name r.args.TypePrefix
                (Some (XerLiteralConstant childTagName))
            | true  ->
                None
        let internalItem =  chFunc.funcBody ({p with arg = lm.lg.getArrayItem p.arg i child.isIA5String}) childTag
        let internalItem_str, chLocalVars, chErrCodes, chSize = match internalItem with Some x -> x.funcBody, x.localVariables, x.errCodes, x.encodingSizeInBytes | None -> "",[],[],0I
        let contentSize = o.maxSize.uper * chSize
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = SequenceOf (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) xmlTag.p nLevel i o.maxSize.uper internalItem_str (o.minSize.uper=o.maxSize.uper) (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= errCode::chErrCodes; localVariables=lv::chLocalVars;encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (us:State)  =
    let nestChildItems (lm:LanguageMacros) (codec:CommonTypes.Codec) children = DAstUtilFunctions.nestItems_ret lm  children
    let sequence_mandatory_child    = lm.xer.Sequence_mandatory_child
    let sequence_default_child      = lm.xer.Sequence_default_child
    let sequence_optional_child     = lm.xer.Sequence_optional_child
    let sequence_start              = lm.xer.SEQUENCE_start
    let sequence_end                = lm.xer.SEQUENCE_end

    let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)

    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "SEQUENCE")
        let nonAcnChildren = children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)

        let handleChild (child:Asn1Child) =
            let chFunc =
                match child.Type.getXerFunction codec with
                | XerFunction z -> z
                | XerFunctionDummy  -> raise (BugErrorException "XerFunctionDummy")
            let chP = {p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName child) child.Type.isIA5String child.Optionality.IsSome}
            let childContentResult = chFunc.funcBody chP (Some (XerLiteralConstant child.Name.Value))
            match childContentResult with
            | None              -> None
            | Some childContent ->
                let childBody, child_localVariables =
                    match child.Optionality with
                    | None                          ->  sequence_mandatory_child (lm.lg.getAsn1ChildBackendName child) childContent.funcBody child.Name.Value codec, childContent.localVariables
                    //| Some Asn1AcnAst.AlwaysAbsent  ->  match codec with CommonTypes.Encode -> None                        | CommonTypes.Decode -> Some (sequence_optional_child p.arg.p (lm.lg.getAccess p.arg) child.c_name childContent.funcBody codec)
                        | Some Asn1AcnAst.AlwaysAbsent     //-> "", []
                        | Some Asn1AcnAst.AlwaysPresent    -> sequence_optional_child (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody child.Name.Value codec, childContent.localVariables
                        | Some (Asn1AcnAst.Optional opt)   ->
                            match opt.defaultValue with
                            | None                   -> sequence_optional_child (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody child.Name.Value codec, childContent.localVariables
                            | Some v                 ->
                                let defInit= child.Type.initFunction.initByAsn1Value chP (mapValue v).kind
                                sequence_default_child (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody child.Name.Value defInit codec, childContent.localVariables
                Some (childBody, child_localVariables, childContent.errCodes, childContent.encodingSizeInBytes)


        let childrenStatements0 = nonAcnChildren |> List.choose handleChild
        let childrenStatements = childrenStatements0 |> List.map(fun (s,_,_,_)    -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s,_)  -> s)
        let contentSize = childrenStatements0 |> List.map(fun (_,_,_,s)    -> s) |> List.fold (+) 0I
        let startTag    = sequence_start (p.arg.joined lm.lg) xmlTag.p nLevel errCode.errCodeName (childrenStatements |> Seq.isEmpty) codec
        let endTag      = sequence_end xmlTag.p nLevel errCode.errCodeName codec
        let seqContent =
            let opt = (startTag::childrenStatements@[endTag]) |> nestChildItems lm codec
            match opt with
            | Some x -> x
            | None   -> ""

        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        Some {XERFuncBodyResult.funcBody = seqContent; errCodes= errCode::childrenErrCodes; localVariables=childrenLocalvars;encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createChoiceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (us:State)  =
    let choice_child =  lm.xer.CHOICE_child
    let choice_no_tag = lm.xer.CHOICE_no_tag
    let choice =        lm.xer.CHOICE
    let typeDefinitionName =  typeDefinition.longTypedefName2 lm.lg.hasModules
    let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
    let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
        let handleChild childIndex (child:ChChildInfo) =
            let chFunc =
                match child.chType.getXerFunction codec with
                | XerFunction z -> z
                | XerFunctionDummy  -> raise (BugErrorException "XerFunctionDummy")

            let childContentResult =
                chFunc.funcBody ({p with arg = lm.lg.getChChild p.arg (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String}) (Some (XerLiteralConstant child.Name.Value))
            match childContentResult with
            | None  -> None
            | Some childContent ->
                let sChildName = lm.lg.getAsn1ChChildBackendName child
                let sChildTypeDef = lm.lg.getLongTypedefName child.chType.typeDefinitionOrReference
                let sChoiceTypeName = typeDefinitionName
                let childBody = choice_child (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) (lm.lg.presentWhenName (Some typeDefinition) child) childContent.funcBody (childIndex = 0) child.Name.Value sChildName sChildTypeDef sChoiceTypeName codec
                Some (childBody, childContent.localVariables, childContent.errCodes, childContent.encodingSizeInBytes)
        let childrenStatements0 = children |> List.mapi handleChild |> List.choose id
        let childrenStatements = childrenStatements0 |> List.map(fun (s,_,_,_)    -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s,_)  -> s)
        let contentSize = childrenStatements0 |> List.map(fun (_,_,_,s)    -> s) |> List.fold max 0I
        let no_tag_body = choice_no_tag (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) childrenStatements errCode.errCodeName codec
        let chContent, totalSize =
            match xmlTag with
            | None          -> no_tag_body, contentSize
            | Some xmlTag   ->
                choice (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) xmlTag.p nLevel no_tag_body errCode.errCodeName codec, getMaxSizeInBytesForXER xmlTag contentSize

        Some {XERFuncBodyResult.funcBody = chContent; errCodes= errCode::childrenErrCodes; localVariables=childrenLocalvars;encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createReferenceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionOrReference)  (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
    let callBaseTypeFunc = lm.xer.call_base_type_func

    let moduleName, typeDefinitionName0 =
        let t1 = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        let typeDef = lm.lg.getTypeDefinition t1.FT_TypeDefinition
        typeDef.programUnit, typeDef.typeName

    let baseTypeDefinitionName =
        match lm.lg.hasModules with
        | false     -> typeDefinitionName0
        | true   ->
            match t.id.ModName = o.modName.Value with
            | true  -> typeDefinitionName0
            | false -> moduleName + "." + typeDefinitionName0

    let baseFncName = baseTypeDefinitionName+ "_XER"  + codec.suffix



    let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
    let t1WithExtensions = o.resolvedType;

    match TypesEquivalence.uperEquivalence t1 t1WithExtensions with
    | true  ->
        let funcBody (errCode:ErrorCode) (p:CallerScope) (xmlTag:XerTag option) =
            let baseTypeXerFunc =
                match baseType.getXerFunction codec with
                | XerFunction z -> z
                | XerFunctionDummy  -> raise (BugErrorException "XerFunctionDummy")

            match baseTypeXerFunc.funcBody p xmlTag with
            | Some baseXerFunc    ->
                let xmlTag = xmlTag |> orElse (XerLiteralConstant o.tasName.Value)
                let funcBodyContent = callBaseTypeFunc (lm.lg.getParamValue t p.arg codec) xmlTag.p baseFncName codec
                Some {XERFuncBodyResult.funcBody = funcBodyContent; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=baseXerFunc.encodingSizeInBytes}
            | None      -> None

        let soSparkAnnotations = None
        createXerFunction_any r lm codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us
    | false ->
        baseType.getXerFunction codec, us


