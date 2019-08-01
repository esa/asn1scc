module DAstXer
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions



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


let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (codec:CommonTypes.Codec) (typeDefinition:TypeDefintionOrReference) =
    getFuncNameGeneric  typeDefinition ("_XER" + codec.suffix)

let createXerFunction_any (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) xerFuncBody_e  soSparkAnnotations (us:State)  =

    let emitTypeAssignment      = match l with C -> xer_c.EmitTypeAssignment    | Ada -> xer_a.EmitTypeAssignment
    let emitTypeAssignment_def  = match l with C -> xer_c.EmitTypeAssignment_def    | Ada -> xer_a.EmitTypeAssignment_def
    let EmitTypeAssignment_def_err_code  = match l with C -> uper_c.EmitTypeAssignment_def_err_code    | Ada -> uper_a.EmitTypeAssignment_def_err_code

    let funcName            = getFuncName r l codec typeDefinition
    let p  = t.getParamType l codec
    let varName = p.arg.p
    let sStar = p.arg.getStar l
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let sInitilialExp = ""
    let errCodeName         = ToC ("ERR_XER" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let defaultErrCode, ns = getNextValidErrorCode us errCodeName
    let xerFuncBody = (xerFuncBody_e defaultErrCode)

    let  xerFunc, xerFuncDef, encodingSizeInBytes  = 
        match funcName  with
        | None              -> None, None, 0I
        | Some funcName     -> 
            let asn1TasName = typeDefinition.getAsn1Name r.args.TypePrefix
            let xerBdResultOpt : XERFuncBodyResult option= xerFuncBody_e defaultErrCode p  (Some (XerFunctionParameter (asn1TasName, "xmlTag")))

            let bodyResult_funcBody, errCodes,  bodyResult_localVariables, encodingSizeInBytes = 
                match xerBdResultOpt with
                | None              -> 
                    let emtyStatement = match l with C -> "" | Ada -> "null;"
                    emtyStatement, [], [], 0I
                | Some bodyResult   -> bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.encodingSizeInBytes

            let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
            let xerFunc = Some(emitTypeAssignment asn1TasName varName sStar funcName isValidFuncName  (typeDefinition.longTypedefName l) lvars  bodyResult_funcBody soSparkAnnotations sInitilialExp codec)

            let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
            let xerFuncDef = Some(emitTypeAssignment_def varName sStar funcName  (typeDefinition.longTypedefName l) errCodStr (encodingSizeInBytes = 0I) (encodingSizeInBytes)  soSparkAnnotations codec)
            xerFunc, xerFuncDef, encodingSizeInBytes
    let ret = 
        {
            XerFunction.funcName       = funcName
            func                       = xerFunc
            funcDef                    = xerFuncDef
            encodingSizeInBytes        = encodingSizeInBytes
            funcBody_e                 = xerFuncBody_e
            funcBody                   = xerFuncBody
        }
    ret, ns

let orElse defaultValue optValue = match optValue with Some x -> x | None -> defaultValue

let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (us:State)  =
    let Integer = match l with C -> xer_c.Integer   | Ada -> xer_a.Integer
    let IntegerPos = match l with C -> xer_c.IntegerPos   | Ada -> xer_a.IntegerPos
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "INTEGER")
        let checkExp = 
            match (DastValidate2.createIntegerFunctionByCons r l o.isUnsigned (o.cons@o.withcons)) with
            | None  ->  None
            | Some expFunc -> 
                let makeExpressionToStatement0  = match l with C -> isvalid_c.makeExpressionToStatement0 | Ada -> isvalid_a.makeExpressionToStatement0
                match expFunc p with
                | VCBTrue               -> Some (makeExpressionToStatement0 "true")
                | VCBFalse              -> Some (makeExpressionToStatement0 "false")
                | VCBExpression sExp    -> Some (makeExpressionToStatement0 sExp)
                | VCBStatement sStat    -> Some sStat

                //Some (expFunc p)
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Integer ()
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = 
            match o.isUnsigned with
            | true  -> IntegerPos pp xmlTag.p nLevel checkExp errCode.errCodeName codec
            | false -> Integer pp xmlTag.p nLevel checkExp errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let checkExp (isValidFunc: IsValidFunction option) p = 
    match isValidFunc with
    | None  ->  None
    | Some fnc -> None
//        match fnc.funcExp with
//        | None  -> None
//        | Some expFunc -> (Some (expFunc p))

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Boolean = match l with C -> xer_c.Boolean   | Ada -> xer_a.Boolean
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "")
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_boolean ()
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = Boolean pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Real = match l with C -> xer_c.Real   | Ada -> xer_a.Real
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "REAL")
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Real 
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = Real pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let ObjectIdentifier = match l with C -> xer_c.ObjectIdentifier   | Ada -> xer_a.ObjectIdentifier
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "ObjectIdentifier")
        let pp = match codec with CommonTypes.Encode -> p.arg.getPointer l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Real 
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = ObjectIdentifier pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us


let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let nullFunc = match l with C -> xer_c.Null   | Ada -> xer_a.Null
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getPointer l | CommonTypes.Decode -> p.arg.getPointer l
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "NULL")
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let totalSize = getMaxSizeInBytesForXER xmlTag 0I
        let bodyStm = nullFunc pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Enumerated = match l with C -> xer_c.Enumerated   | Ada -> xer_a.Enumerated
    let Enumerated_item = match l with C -> xer_c.Enumerated_item   | Ada -> xer_a.Enumerated_item
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "")
        let pp = p.arg.getValue l 
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_Enumerated (o.items |> List.map (fun itm -> itm.Name.Value))
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let arrItems =
            o.items |> List.mapi(fun i it -> Enumerated_item pp xmlTag.p nLevel (it.getBackendName (Some typeDefinition) l) it.Name.Value errCode.errCodeName (i=0) codec)
        let bodyStm = Enumerated pp xmlTag.p nLevel arrItems (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createIA5StringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let String = match l with C -> xer_c.String   | Ada -> xer_a.String
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant (if o.isNumeric then "NumericString" else "IA5String"))
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_IA5String o.maxSize.uper
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = String pp xmlTag.p nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let OctetString = match l with C -> xer_c.OctetString   | Ada -> xer_a.OctetString
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "OCTET-STRING")
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_OctetString o.maxSize.uper
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = OctetString p.arg.p (p.arg.getAcces l) xmlTag.p nLevel o.maxSize.uper (o.minSize.uper=o.maxSize.uper) (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let BitString = match l with C -> xer_c.BitString   | Ada -> xer_a.BitString
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "BIT-STRING")
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = getMaxSizeInBytesForXER_BitString o.maxSize.uper
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = BitString p.arg.p (p.arg.getAcces l) xmlTag.p nLevel o.maxSize.uper (o.minSize.uper=o.maxSize.uper) (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (child:Asn1Type) (us:State)  =
    let SequenceOf = match l with C -> xer_c.SequenceOf   | Ada -> xer_a.SequenceOf
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "SEQUENCE-OF")
        let ii = t.id.SeqeuenceOfLevel + 1
        let i = sprintf "i%d" ii
        let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
        let pp =  p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let chFunc = child.getXerFunction codec

        let childTag = 
            let definedInRTL = 
                match child.typeDefintionOrReference with
                | TypeDefinition  td -> false
                | ReferenceToExistingDefinition ref -> ref.definedInRtl
            match definedInRTL with
            | false ->
                let childTagName = (child.typeDefintionOrReference).getAsn1Name r.args.TypePrefix
                (Some (XerLiteralConstant childTagName))
            | true  ->
                None
        let internalItem =  chFunc.funcBody ({p with arg = p.arg.getArrayItem l i child.isIA5String}) childTag
        let internalItem_str, chLocalVars, chErrCodes, chSize = match internalItem with Some x -> x.funcBody, x.localVariables, x.errCodes, x.encodingSizeInBytes | None -> "",[],[],0I
        let contentSize = o.maxSize.uper * chSize
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = SequenceOf p.arg.p (p.arg.getAcces l) xmlTag.p nLevel i o.maxSize.uper internalItem_str (o.minSize.uper=o.maxSize.uper) (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= errCode::chErrCodes; localVariables=lv::chLocalVars;encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (us:State)  =
    let nestChildItems (l:ProgrammingLanguage) (codec:CommonTypes.Codec) children = DAstUtilFunctions.nestItems l "ret" children
    let sequence_mandatory_child  = match l with C -> xer_c.Sequence_mandatory_child  | Ada -> xer_a.Sequence_mandatory_child
    let sequence_default_child   = match l with C -> xer_c.Sequence_default_child     | Ada -> xer_a.Sequence_default_child
    let sequence_optional_child   = match l with C -> xer_c.Sequence_optional_child   | Ada -> xer_a.Sequence_optional_child
    let sequence_start            = match l with C -> xer_c.SEQUENCE_start            | Ada -> xer_a.SEQUENCE_start
    let sequence_end              = match l with C -> xer_c.SEQUENCE_end              | Ada -> xer_a.SEQUENCE_end
    //let sequence                  = match l with C -> xer_c.SEQUENCE_xer              | Ada -> xer_a.SEQUENCE_xer
    let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)

    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let xmlTag = xmlTag |> orElse (XerLiteralConstant "SEQUENCE")
        let nonAcnChildren = children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)

        let handleChild (child:Asn1Child) =
            let chFunc = child.Type.getXerFunction codec
            let childContentResult = chFunc.funcBody ({p with arg = p.arg.getSeqChild l (child.getBackendName l) child.Type.isIA5String}) (Some (XerLiteralConstant child.Name.Value))
            match childContentResult with
            | None              -> None
            | Some childContent ->
                let childBody, child_localVariables = 
                    match child.Optionality with
                    | None                          ->  sequence_mandatory_child (child.getBackendName l) childContent.funcBody child.Name.Value codec, childContent.localVariables
                    //| Some Asn1AcnAst.AlwaysAbsent  ->  match codec with CommonTypes.Encode -> None                        | CommonTypes.Decode -> Some (sequence_optional_child p.arg.p (p.arg.getAcces l) child.c_name childContent.funcBody codec) 
                        | Some Asn1AcnAst.AlwaysAbsent     //-> "", []
                        | Some Asn1AcnAst.AlwaysPresent    -> sequence_optional_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody child.Name.Value codec, childContent.localVariables
                        | Some (Asn1AcnAst.Optional opt)   -> 
                            match opt.defaultValue with
                            | None                   -> sequence_optional_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody child.Name.Value codec, childContent.localVariables
                            | Some v                 -> 
                                let defInit= child.Type.initFunction.initByAsn1Value ({p with arg = p.arg.getSeqChild l (child.getBackendName l) child.Type.isIA5String}) (mapValue v).kind
                                sequence_default_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody child.Name.Value defInit codec, childContent.localVariables
                Some (childBody, child_localVariables, childContent.errCodes, childContent.encodingSizeInBytes)
        

        let childrenStatements0 = nonAcnChildren |> List.choose handleChild
        let childrenStatements = childrenStatements0 |> List.map(fun (s,_,_,_)    -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s,_)  -> s)
        let contentSize = childrenStatements0 |> List.map(fun (_,_,_,s)    -> s) |> List.fold (+) 0I
        let startTag    = sequence_start p.arg.p xmlTag.p nLevel errCode.errCodeName (childrenStatements |> Seq.isEmpty) codec
        let endTag      = sequence_end xmlTag.p nLevel errCode.errCodeName codec
        let seqContent =  
            let opt = (startTag::childrenStatements@[endTag]) |> nestChildItems l codec 
            match opt with
            | Some x -> x
            | None   -> ""
        
        let totalSize = getMaxSizeInBytesForXER xmlTag contentSize
        Some {XERFuncBodyResult.funcBody = seqContent; errCodes= errCode::childrenErrCodes; localVariables=childrenLocalvars;encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (us:State)  =
    let choice_child =  match l with C -> xer_c.CHOICE_child    | Ada -> xer_a.CHOICE_child
    let choice_no_tag = match l with C -> xer_c.CHOICE_no_tag   | Ada -> xer_a.CHOICE_no_tag
    let choice =        match l with C -> xer_c.CHOICE          | Ada -> xer_a.CHOICE
    let typeDefinitionName = typeDefinition.longTypedefName l
    let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
        let handleChild childIndex (child:ChChildInfo) =
            let chFunc = child.chType.getXerFunction codec
            let childContentResult = 
                match l with
                | C   -> chFunc.funcBody ({p with arg = p.arg.getChChild l (child.getBackendName l) child.chType.isIA5String}) (Some (XerLiteralConstant child.Name.Value))
                | Ada when codec = CommonTypes.Decode -> chFunc.funcBody ({p with arg = VALUE ((child.getBackendName l) + "_tmp")}) (Some (XerLiteralConstant child.Name.Value))
                | Ada -> chFunc.funcBody ({p with arg = p.arg.getChChild l (child.getBackendName l) child.chType.isIA5String}) (Some (XerLiteralConstant child.Name.Value))
            match childContentResult with
            | None  -> None
            | Some childContent ->
                let sChildName = (child.getBackendName l)
                let sChildTypeDef = child.chType.typeDefintionOrReference.longTypedefName l 
                let sChoiceTypeName = typeDefinitionName
                let childBody = choice_child p.arg.p (p.arg.getAcces l) (child.presentWhenName (Some typeDefinition) l) childContent.funcBody (childIndex = 0) child.Name.Value sChildName sChildTypeDef sChoiceTypeName codec
                Some (childBody, childContent.localVariables, childContent.errCodes, childContent.encodingSizeInBytes)
        let childrenStatements0 = children |> List.mapi handleChild |> List.choose id
        let childrenStatements = childrenStatements0 |> List.map(fun (s,_,_,_)    -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s,_)  -> s)
        let contentSize = childrenStatements0 |> List.map(fun (_,_,_,s)    -> s) |> List.fold max 0I
        let no_tag_body = choice_no_tag p.arg.p (p.arg.getAcces l) childrenStatements errCode.errCodeName codec
        let chContent, totalSize =
            match xmlTag with
            | None          -> no_tag_body, contentSize
            | Some xmlTag   -> 
                choice p.arg.p (p.arg.getAcces l) xmlTag.p nLevel no_tag_body errCode.errCodeName codec, getMaxSizeInBytesForXER xmlTag contentSize
        
        Some {XERFuncBodyResult.funcBody = chContent; errCodes= errCode::childrenErrCodes; localVariables=childrenLocalvars;encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createReferenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
    let callBaseTypeFunc = match l with C -> xer_c.call_base_type_func | Ada -> xer_a.call_base_type_func
    let typeDefinitionName = ToC2(r.args.TypePrefix + o.tasName.Value)
    let baseFncName = 
        match l with
        | C     -> typeDefinitionName + "_XER" + codec.suffix
        | Ada   -> 
            match t.id.ModName = o.modName.Value with
            | true  -> typeDefinitionName + "_XER" + codec.suffix
            | false -> (ToC o.modName.Value) + "." + typeDefinitionName + "_XER" + codec.suffix

    let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
    let t1WithExtensios = o.resolvedType;
    
    match TypesEquivalence.uperEquivalence t1 t1WithExtensios with
    | true  ->
        let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:XerTag option) = 
            match (baseType.getXerFunction codec).funcBody p xmlTag with
            | Some baseXerFunc    -> 
                let xmlTag = xmlTag |> orElse (XerLiteralConstant o.tasName.Value)
                let funcBodyContent = callBaseTypeFunc (t.getParamValue p.arg l codec) xmlTag.p baseFncName codec
                Some {XERFuncBodyResult.funcBody = funcBodyContent; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=baseXerFunc.encodingSizeInBytes}
            | None      -> None

        let soSparkAnnotations = None
        createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us
    | false -> 
        baseType.getXerFunction codec, us
