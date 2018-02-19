module DAstXer
open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions


let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (codec:CommonTypes.Codec) (typeDefinition:TypeDefintionOrReference) =
    getFuncNameGeneric  typeDefinition ("_XER" + codec.suffix)

let createXerFunction_any (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) xerFuncBody  soSparkAnnotations (us:State)  =

    let emitTypeAssignment      = match l with C -> xer_c.EmitTypeAssignment    | Ada -> xer_c.EmitTypeAssignment
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

    let  xerFunc, xerFuncDef, encodingSizeInBytes  = 
        match funcName  with
        | None              -> None, None, 0
        | Some funcName     -> 
            let asn1TasName = typeDefinition.getAsn1Name r.args.TypePrefix
            let xerBdResultOpt : XERFuncBodyResult option= xerFuncBody defaultErrCode p  asn1TasName

            let bodyResult_funcBody, errCodes,  bodyResult_localVariables, encodingSizeInBytes = 
                match xerBdResultOpt with
                | None              -> 
                    let emtyStatement = match l with C -> "" | Ada -> "null;"
                    emtyStatement, [], [], 0
                | Some bodyResult   -> bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.encodingSizeInBytes

            let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
            let xerFunc = Some(emitTypeAssignment varName sStar funcName isValidFuncName  (typeDefinition.longTypedefName l) lvars  bodyResult_funcBody soSparkAnnotations sInitilialExp codec)

            let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
            let xerFuncDef = Some(emitTypeAssignment_def varName sStar funcName  (typeDefinition.longTypedefName l) errCodStr (encodingSizeInBytes = 0) (BigInteger encodingSizeInBytes)  soSparkAnnotations codec)
            xerFunc, xerFuncDef, encodingSizeInBytes
    let ret = 
        {
            XerFunction.funcName       = funcName
            func                       = xerFunc
            funcDef                    = xerFuncDef
            encodingSizeInBytes        = encodingSizeInBytes
            funcBody                   = xerFuncBody
        }
    ret, ns


let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (us:State)  =
    let Integer = match l with C -> xer_c.Integer   | Ada -> xer_a.Integer
    let IntegerPos = match l with C -> xer_c.IntegerPos   | Ada -> xer_a.IntegerPos
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = 
        let checkExp = 
            match (DAstValidate.createIntegerFunctionByCons r l o.isUnsigned (o.cons@o.withcons)) with
            | None  ->  None
            | Some expFunc -> Some (expFunc p)
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = XER.getMaxSizeInBytesForXER_Integer ()
        let totalSize = XER.getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = 
            match o.isUnsigned with
            | true  -> IntegerPos pp xmlTag nLevel checkExp errCode.errCodeName codec
            | false -> Integer pp xmlTag nLevel checkExp errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let checkExp (isValidFunc: IsValidFunction option) p = 
    match isValidFunc with
    | None  ->  None
    | Some fnc -> 
        match fnc.funcExp with
        | None  -> None
        | Some expFunc -> (Some (expFunc p))

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Boolean = match l with C -> xer_c.Boolean   | Ada -> xer_a.Boolean
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = XER.getMaxSizeInBytesForXER_boolean ()
        let totalSize = XER.getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = Boolean pp xmlTag nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Real = match l with C -> xer_c.Real   | Ada -> xer_a.Real
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = XER.getMaxSizeInBytesForXER_Real 
        let totalSize = XER.getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = Real pp xmlTag nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = None
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let Enumerated = match l with C -> xer_c.Enumerated   | Ada -> xer_a.Enumerated
    let Enumerated_item = match l with C -> xer_c.Enumerated_item   | Ada -> xer_a.Enumerated_item
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = 
        let pp = p.arg.getValue l 
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = XER.getMaxSizeInBytesForXER_Enumerated (o.items |> List.map (fun itm -> itm.Name.Value))
        let totalSize = XER.getMaxSizeInBytesForXER xmlTag contentSize
        let arrItems =
            o.items |> List.mapi(fun i it -> Enumerated_item pp xmlTag nLevel (it.getBackendName (Some typeDefinition) l) it.Name.Value errCode.errCodeName (i=0) codec)
        let bodyStm = Enumerated pp xmlTag nLevel arrItems (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createIA5StringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let String = match l with C -> xer_c.String   | Ada -> xer_a.String
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = XER.getMaxSizeInBytesForXER_IA5String o.maxSize
        let totalSize = XER.getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = String pp xmlTag nLevel (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let OctetString = match l with C -> xer_c.OctetString   | Ada -> xer_a.OctetString
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = 
        let pp =  p.arg.getPointer l
        let nLevel = BigInteger (t.id.AcnAbsPath.Length - 2)
        let contentSize = XER.getMaxSizeInBytesForXER_OctetString o.maxSize
        let totalSize = XER.getMaxSizeInBytesForXER xmlTag contentSize
        let bodyStm = OctetString pp (p.arg.getAcces l) xmlTag nLevel o.maxSize.AsBigInt (o.minSize=o.maxSize) (checkExp isValidFunc p) errCode.errCodeName codec
        Some {XERFuncBodyResult.funcBody = bodyStm; errCodes= [errCode]; localVariables=[];encodingSizeInBytes=totalSize}
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = None
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (child:Asn1Type) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = None
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = None
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = None
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us

let createReferenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) (xmlTag:string) = None
    let soSparkAnnotations = None
    createXerFunction_any r l codec t typeDefinition  isValidFunc  funcBody  soSparkAnnotations us
