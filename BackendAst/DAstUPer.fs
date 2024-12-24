module DAstUPer
open System
open System.Numerics
open System.Globalization
open System.IO

open FsUtils
open CommonTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language

let getFuncName (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId :ReferenceToType) (td:FE_TypeDefinition) =
    match typeId.tasInfo with
    | None -> None
    | Some _ -> Some (td.typeName + codec.suffix)


let callBaseTypeFunc (lm:LanguageMacros) = lm.uper.call_base_type_func

let sparkAnnotations (lm:LanguageMacros)  = lm.uper.sparkAnnotations

let nestChildItems  (lm:LanguageMacros) (codec:CommonTypes.Codec) children =
    DAstUtilFunctions.nestItems lm.isvalid.JoinItems2 children

let adaptArgument (lm: LanguageMacros) (codec: CommonTypes.Codec) (p: CallerScope): string * string option =
    // For Copy decoding kind, the return expression is the variable in which we save the decoding result
    match codec with
    | Encode -> lm.lg.getValue p.arg, None
    | Decode ->
        match lm.lg.decodingKind with
        | InPlace -> lm.lg.getPointer p.arg, None
        | Copy ->
            let res = p.arg.asIdentifier
            res, Some res

let adaptArgumentPtr (lm: LanguageMacros) (codec: CommonTypes.Codec) (p: CallerScope): string * string option =
    match codec, lm.lg.decodingKind with
    | Decode, Copy ->
        let res = p.arg.asIdentifier
        res, Some res
    | _ -> lm.lg.getPointer p.arg, None

let adaptArgumentValue (lm: LanguageMacros) (codec: CommonTypes.Codec) (p: CallerScope): string * string option =
    match codec, lm.lg.decodingKind with
    | Decode, Copy ->
        let res = p.arg.asIdentifier
        res, Some res
    | _ -> lm.lg.getValue p.arg, None

let joinedOrAsIdentifier (lm: LanguageMacros) (codec: CommonTypes.Codec) (p: CallerScope) : string * string option =
    match codec, lm.lg.decodingKind with
    | Decode, Copy ->
        let resExpr = p.arg.asIdentifier
        resExpr, Some resExpr
    | _ -> p.arg.joined lm.lg, None

//TODO
//1.Decode functions (and perhaps encode function) must check if the decode value is within the constraints (currently, implemented only for Integers and for case IntUnconstrainedMax )
//2.Fragmentation


let internal createUperFunction (r:Asn1AcnAst.AstRoot)
                                (lm:LanguageMacros)
                                (codec:CommonTypes.Codec)
                                (t:Asn1AcnAst.Asn1Type)
                                (typeDefinition:TypeDefinitionOrReference)
                                (baseTypeUperFunc : UPerFunction option)
                                (isValidFunc: IsValidFunction option)
                                (funcBody_e: ErrorCode -> NestingScope -> CallerScope -> bool -> UPERFuncBodyResult option)
                                soSparkAnnotations
                                (funcDefAnnots: string list)
                                (us:State)  =
    let typeDef = lm.lg.getTypeDefinition t.FT_TypeDefinition
    let funcName = getFuncName r lm codec t.id typeDef
    let errCodeName = ToC ("ERR_UPER" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None
    let soInitFuncName = getFuncNameGeneric typeDefinition (lm.init.methodNameSuffix())
    let EmitTypeAssignment = lm.uper.EmitTypeAssignment
    let EmitTypeAssignment_def = lm.uper.EmitTypeAssignment_def
    let EmitTypeAssignment_def_err_code = lm.uper.EmitTypeAssignment_def_err_code

    let funcBody = (funcBody_e errCode)
    let p = lm.lg.getParamType t codec
    let varName = p.arg.receiverId
    let sStar = lm.lg.getStar p.arg
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let sInitialExp = ""
    let func, funcDef, auxiliaries =
            match funcName  with
            | None              -> None, None, []
            | Some funcName     ->
                let content = funcBody (NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits []) p false
                let bodyResult_funcBody, errCodes,  bodyResult_localVariables, bBsIsUnreferenced, bVarNameIsUnreferenced, auxiliaries =
                    match content with
                    | None              ->
                        let emptyStatement = lm.lg.emptyStatement
                        emptyStatement, [], [], true, isValidFuncName.IsNone, []
                    | Some bodyResult   -> bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsUnReferenced, bodyResult.bValIsUnReferenced, bodyResult.auxiliaries
                let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct
                let precondAnnots = lm.lg.generatePrecond r UPER t codec
                let postcondAnnots = lm.lg.generatePostcond r UPER typeDef.typeName p t codec
                let func = Some(EmitTypeAssignment varName sStar funcName isValidFuncName  (lm.lg.getLongTypedefName typeDefinition) lvars  bodyResult_funcBody soSparkAnnotations sInitialExp (t.uperMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced soInitFuncName funcDefAnnots precondAnnots postcondAnnots codec)

                let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
                let funcDef = Some(EmitTypeAssignment_def varName sStar funcName  (lm.lg.getLongTypedefName typeDefinition) errCodStr (t.uperMaxSizeInBits = 0I) (BigInteger (ceil ((double t.uperMaxSizeInBits)/8.0))) ( t.uperMaxSizeInBits) soSparkAnnotations (t.uperMaxSizeInBits = 0I) codec)
                func, funcDef, auxiliaries


    let ret =
        {
            UPerFunction.funcName      = funcName
            func                       = func
            funcDef                    = funcDef
            funcBody                   = funcBody
            funcBody_e                 = funcBody_e
            auxiliaries                = auxiliaries
        }
    ret, ns

let getIntDecFuncSuffix (intClass:Asn1AcnAst.IntegerClass) =
    match intClass with
    | Asn1AcnAst.ASN1SCC_Int8      _ -> "Int8"
    | Asn1AcnAst.ASN1SCC_Int16     _ -> "Int16"
    | Asn1AcnAst.ASN1SCC_Int32     _ -> "Int32"
    | Asn1AcnAst.ASN1SCC_Int64     _ -> ""
    | Asn1AcnAst.ASN1SCC_Int       _ -> ""
    | Asn1AcnAst.ASN1SCC_UInt8     _ -> "UInt8"
    | Asn1AcnAst.ASN1SCC_UInt16    _ -> "UInt16"
    | Asn1AcnAst.ASN1SCC_UInt32    _ -> "UInt32"
    | Asn1AcnAst.ASN1SCC_UInt64    _ -> ""
    | Asn1AcnAst.ASN1SCC_UInt      _ -> ""

let castPp (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) codec pp (intClass:Asn1AcnAst.IntegerClass) encFuncBits =
    match codec with
    | CommonTypes.Encode ->
        match intClass with
        | Asn1AcnAst.ASN1SCC_Int8      _ -> (lm.lg.castExpression pp (lm.typeDef.Declare_Integer()))
        | Asn1AcnAst.ASN1SCC_Int16     _ -> (lm.lg.castExpression pp (lm.typeDef.Declare_Integer()))
//        | Asn1AcnAst.ASN1SCC_Int32     _ when r.args.integerSizeInBytes <> 4I -> if encFuncBits = 32 then pp  else (lm.lg.castExpression pp (lm.typeDef.Declare_Integer()))
        | Asn1AcnAst.ASN1SCC_Int32     _ -> if encFuncBits = 32 && r.args.integerSizeInBytes = 4I then pp  else (lm.lg.castExpression pp (lm.typeDef.Declare_Integer()))
        | Asn1AcnAst.ASN1SCC_Int64     _ -> if encFuncBits = 64 then pp  else (lm.lg.castExpression pp (lm.typeDef.Declare_Integer()))
        | Asn1AcnAst.ASN1SCC_Int       _ -> pp
        | Asn1AcnAst.ASN1SCC_UInt8     _ -> (lm.lg.castExpression pp (lm.typeDef.Declare_PosInteger()))
        | Asn1AcnAst.ASN1SCC_UInt16    _ -> (lm.lg.castExpression pp (lm.typeDef.Declare_PosInteger()))
//        | Asn1AcnAst.ASN1SCC_UInt32    _ when r.args.integerSizeInBytes <> 4I -> (lm.lg.castExpression pp (lm.typeDef.Declare_PosInteger()))
        | Asn1AcnAst.ASN1SCC_UInt32    _ -> if encFuncBits = 32 && r.args.integerSizeInBytes = 4I then pp  else (lm.lg.castExpression pp (lm.typeDef.Declare_PosInteger()))
        | Asn1AcnAst.ASN1SCC_UInt64    _ -> if encFuncBits = 64 then pp  else (lm.lg.castExpression pp (lm.typeDef.Declare_PosInteger()))
        | Asn1AcnAst.ASN1SCC_UInt      _ -> pp
    | CommonTypes.Decode -> pp


let getIntfuncBodyByCons (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (uperRange:BigIntegerUperRange) errLoc (intClass:Asn1AcnAst.IntegerClass) (cons: IntegerTypeConstraint list) (allCons: IntegerTypeConstraint list) (typeId: ReferenceToType) (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) =
    let pp, resultExpr = adaptArgument lm codec p

    let IntNoneRequired         = lm.uper.IntNoneRequired
    let IntFullyConstraintPos   = lm.uper.IntFullyConstraintPos
    let IntFullyConstraint      = lm.uper.IntFullyConstraint
    let IntSemiConstraintPos    = lm.uper.IntSemiConstraintPos
    let IntSemiConstraint       = lm.uper.IntSemiConstraint
    let IntUnconstrained        = lm.uper.IntUnconstrained
    let IntUnconstrainedMax     = lm.uper.IntUnconstrainedMax
    let IntRootExt              = lm.uper.IntRootExt
    let IntRootExt2             = lm.uper.IntRootExt2
    let rootCons = cons |> List.choose(fun x -> match x with RangeRootConstraint(_, a) |RangeRootConstraint2(_, a,_) -> Some(x) |_ -> None)

    let suffix = getIntDecFuncSuffix intClass
    let castPp encFuncBits = castPp r lm codec pp intClass encFuncBits

    let rangeAssert =
        match typeId.topLevelTas with
        | Some tasInfo ->
            lm.lg.generateIntFullyConstraintRangeAssert (ToC (r.args.TypePrefix + tasInfo.tasName)) p codec
        | None -> None
    let IntBod (uperRange: uperRange<BigInteger>) (extCon: bool) : string * bool * bool * Asn1IntegerEncodingType option =
        match uperRange with
        | Concrete (min, max) when min=max -> IntNoneRequired (lm.lg.getValue p.arg) (lm.lg.intValueToString min intClass) errCode.errCodeName codec, codec=Decode, true, None
        | Concrete (min, max) when intClass.IsPositive && (not extCon) -> IntFullyConstraintPos (castPp ((int r.args.integerSizeInBytes)*8)) min max (GetNumberOfBitsForNonNegativeInteger (max-min)) suffix errCode.errCodeName rangeAssert codec, false, false, Some (FullyConstrainedPositive (min, max))
        | Concrete (min, max) -> IntFullyConstraint (castPp ((int r.args.integerSizeInBytes)*8)) min max (GetNumberOfBitsForNonNegativeInteger (max-min)) suffix errCode.errCodeName codec, false, false, Some (FullyConstrained (min, max))
        | PosInf a  when a>=0I && (not extCon) -> IntSemiConstraintPos pp a  errCode.errCodeName codec, false, false, Some (SemiConstrainedPositive a)
        | PosInf a -> IntSemiConstraint pp a  errCode.errCodeName codec, false, false, Some (SemiConstrained a)
        | NegInf max -> IntUnconstrainedMax pp max None errCode.errCodeName codec, false, false, Some (UnconstrainedMax max)
        | Full -> IntUnconstrained pp errCode.errCodeName false codec, false, false, Some Unconstrained

    let getValueByConstraint uperRange =
        match uperRange with
        | Concrete(a, _)  -> a
        | PosInf(a)       -> a
        | NegInf(b)       -> b
        | Full            -> 0I
    let funcBodyContent, bValIsUnReferenced, bBsIsUnReferenced, intEncodingType =
        match rootCons with
        | []                            -> IntBod uperRange false
        | (RangeRootConstraint (_, a))::rest      ->
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            let cc,_ = DastValidate2.integerConstraint2ValidationCodeBlock r lm intClass a 0
            let cc = DastValidate2.ValidationBlockAsStringExpr (cc p)
            let rootBody, _,_, intEncodingType = IntBod uperR true
            IntRootExt pp (getValueByConstraint uperR) cc rootBody errCode.errCodeName codec, false, false, intEncodingType
        | (RangeRootConstraint2(_,a,_))::rest  ->
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            let cc,_ = DastValidate2.integerConstraint2ValidationCodeBlock r lm intClass a 0
            let cc = DastValidate2.ValidationBlockAsStringExpr (cc p)
            let rootBody, _,_, intEncodingType = IntBod uperR true
            IntRootExt2 pp (getValueByConstraint uperR) cc rootBody errCode.errCodeName codec, false, false, intEncodingType
        | _                             -> raise(BugErrorException "")
    Some({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=bValIsUnReferenced; bBsIsUnReferenced=bBsIsUnReferenced; resultExpr=resultExpr; auxiliaries = []})


let createIntegerFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        getIntfuncBodyByCons r lm codec o.uperRange t.Location o.intClass o.cons o.AllCons t.id errCode nestingScope p
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> funcBody e p) soSparkAnnotations [] us


let createBooleanFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let pp, resultExpr = adaptArgument lm codec p
        let Boolean         = lm.uper.Boolean
        let funcBodyContent = Boolean pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)

    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e ns p b -> Some (funcBody e ns p b)) soSparkAnnotations [] us

let castRPp  = DAstEqual.castRPp

let createRealFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let cls = o.getClass r.args
    let sSuffix =
        match cls with
        | ASN1SCC_REAL   -> ""
        | ASN1SCC_FP32   -> "_fp32"
        | ASN1SCC_FP64   -> ""

    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let pp, resultExpr = adaptArgument lm codec p
        let castPp = castRPp lm codec (o.getClass r.args) pp
        let Real         = lm.uper.Real
        let funcBodyContent = Real castPp sSuffix errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    let annots =
        match ST.lang with
        | Scala -> ["extern"]
        | _ -> []
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e ns p b -> Some (funcBody e ns p b)) soSparkAnnotations annots us

let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefinitionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let pp, resultExpr = adaptArgumentPtr lm codec p
        let ObjectIdentifier         =
            if o.relativeObjectId then
                lm.uper.RelativeOID
            else
                lm.uper.ObjectIdentifier
        let funcBodyContent = ObjectIdentifier pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e ns p b -> Some (funcBody e ns p b)) soSparkAnnotations [] us

let getTimeSubTypeByClass (tc) =
    match tc with
    |Asn1LocalTime                      _ -> "Asn1LocalTime"
    |Asn1UtcTime                        _ -> "Asn1UtcTime"
    |Asn1LocalTimeWithTimeZone          _ -> "Asn1TimeWithTimeZone"
    |Asn1Date                             -> "Asn1Date"
    |Asn1Date_LocalTime                 _ -> "Asn1DateLocalTime"
    |Asn1Date_UtcTime                   _ -> "Asn1DateUtcTime"
    |Asn1Date_LocalTimeWithTimeZone     _ -> "Asn1DateTimeWithTimeZone"


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefinitionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let pp, resultExpr = adaptArgumentPtr lm codec p
        let TimeType         =  lm.uper.Time
        let funcBodyContent = TimeType pp (getTimeSubTypeByClass o.timeClass) errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e ns p b -> Some (funcBody e ns p b)) soSparkAnnotations [] us

let createNullTypeFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefinitionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let pp, _ = adaptArgument lm codec p
        match codec, lm.lg.decodingKind with
        | Decode, Copy ->
            Some ({UPERFuncBodyResult.funcBody = lm.uper.Null_declare pp; errCodes = []; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=Some pp; auxiliaries = []})
        | _ -> None
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations [] us

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let Enumerated         = lm.uper.Enumerated
    let Enumerated_item    = lm.uper.Enumerated_item
    let Enumerated_no_switch = lm.uper.Enumerated_no_switch


    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let nMax = BigInteger(Seq.length o.items) - 1I
        let nLastItemIndex      = nMax
        let typeDef0 = lm.lg.getEnumTypeDefinition o.typeDef
        let td =  typeDef0.longTypedefName2 lm.lg.hasModules  (ToC p.modName)
        let pp, resultExpr = adaptArgumentValue lm codec p
        let sFirstItemName = lm.lg.getNamedItemBackendName (Some typeDefinition) o.items.Head
        let nMin = 0I
        match r.args.isEnumEfficientEnabled o.items.Length with
        | false ->
            let items =
                o.items |> List.mapi(fun i itm -> Enumerated_item pp (lm.lg.getNamedItemBackendName (Some typeDefinition) itm) (BigInteger i) nLastItemIndex codec)
            let nBits = (GetNumberOfBitsForNonNegativeInteger (nMax-nMin))
            let funcBodyContent = Enumerated pp td items nMin nMax nBits errCode.errCodeName nLastItemIndex sFirstItemName codec
            {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}
        | true ->
            let sEnumIndex = "nEnumIndex"
            let enumIndexVar = (Asn1SIntLocalVariable (sEnumIndex, None))
            let funcBodyContent = Enumerated_no_switch pp td errCode.errCodeName sEnumIndex nLastItemIndex  sFirstItemName codec
            {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = [enumIndexVar]; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e ns p b -> Some (funcBody e ns p b)) soSparkAnnotations [] us

let C64K = BigInteger 0x10000
let C48K = BigInteger 0xC000
let C32K = BigInteger 0x8000
let C16K = BigInteger 0x4000
let C_127 = BigInteger 0x7F

type FragmentationParts = {
    nBlocks64K :  BigInteger
    has48KBlock : bool
    has32KBlock : bool
    has16KBlock : bool
    nRemainingItemsVar : BigInteger
}

let FragmentationParts (size:BigInteger) =
    let nBlocks64K = size / C64K
    let nRemainingItemsVar1 = size % C64K
    let has48KBlock = nRemainingItemsVar1 >= C48K

    let nRemainingItemsVar2 = if has48KBlock then (nRemainingItemsVar1 - C48K) else nRemainingItemsVar1
    let has32KBlock = nRemainingItemsVar2 >= C32K

    let nRemainingItemsVar3 = if has32KBlock then (nRemainingItemsVar2 - C32K) else nRemainingItemsVar2
    let has16KBlock = nRemainingItemsVar3 >= C16K

    let nRemainingItemsVar = if has16KBlock then (nRemainingItemsVar3 - C16K) else nRemainingItemsVar3

    { nBlocks64K = nBlocks64K; has48KBlock = has48KBlock; has32KBlock = has32KBlock; has16KBlock = has16KBlock; nRemainingItemsVar = nRemainingItemsVar}


let handleFixedSizeFragmentation (lm:LanguageMacros) (p:CallerScope) (codec:CommonTypes.Codec) (errCode:ErrorCode) ii uperMaxSizeInBits (fixSize:BigInteger) internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString=
    let fixedSize_Fragmentation_sqf_64K          = lm.uper.FixedSize_Fragmentation_sqf_64K
    let fixedSize_Fragmentation_sqf_small_block  = lm.uper.FixedSize_Fragmentation_sqf_small_block
    let fixedSize_Fragmentation_sqf_remaining    = lm.uper.FixedSize_Fragmentation_sqf_remaining
    let fixedSize_Fragmentation_sqf              = lm.uper.FixedSize_Fragmentation_sqf
    let sRemainingItemsVar = sprintf "%s%d" "nRemainingItemsVar" ii
    let sCurBlockSize      = sprintf "%s%d" "nCurBlockSize" ii
    let sBlockIndex        = sprintf "%s%d" "nBlockIndex" ii
    let sCurOffset         = sprintf "%s%d" "nCurOffset" ii
    let sBLI               = sprintf "i%d" ii
    //let lv = SequenceOfIndex (ii, None)
    let r = FragmentationParts fixSize
    //let nBlocks64K = fixSize / C64K
    let parts =
        let part = fixedSize_Fragmentation_sqf_64K (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) sCurOffset sCurBlockSize sBlockIndex r.nBlocks64K internalItem_funcBody sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
        [part]
    let smallBlockParts =
        [(r.has48KBlock, lm.lg.toHex 195, C48K);(r.has32KBlock, lm.lg.toHex 194, C32K);(r.has16KBlock, lm.lg.toHex 193, C16K)] |>  //0xC3, 0xC2, 0xC1
        List.filter (fun (a,_,_) -> a) |>
        List.map (fun (_, sBlockId, nBlockSize) -> fixedSize_Fragmentation_sqf_small_block (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) internalItem_funcBody nBlockSize sBlockId sCurOffset sCurBlockSize sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec)
    let parts = parts@smallBlockParts

    let bRemainingItemsWithinByte = r.nRemainingItemsVar <= C_127
    let parts=
        match r.nRemainingItemsVar > 0I with
        | true  ->
            let part = fixedSize_Fragmentation_sqf_remaining (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) internalItem_funcBody bRemainingItemsWithinByte r.nRemainingItemsVar sCurOffset sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
            parts@[part]
        | false -> parts

    let createLv = lm.lg.uper.createLv


    let fragmentationVars = [createLv sCurBlockSize; createLv sCurOffset]
    let fragmentationVars = fragmentationVars |> List.addIf (codec = Decode) (createLv sRemainingItemsVar)
    let fragmentationVars = fragmentationVars |> List.addIf (lm.lg.uper.requires_sBlockIndex) (createLv sBlockIndex)
    //let fragmentationVars = fragmentationVars |> List.addIf (l = C) (lv)
    let singleNestedPart  = nestChildItems lm  codec parts |> Option.toList
    fixedSize_Fragmentation_sqf (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) singleNestedPart fixSize bIsAsciiString codec, fragmentationVars

let handleFragmentation (lm:LanguageMacros) (p:CallerScope) (codec:CommonTypes.Codec) (errCode:ErrorCode) ii uperMaxSizeInBits (minSize:BigInteger) (maxSize:BigInteger) internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString=
    match minSize = maxSize with
    | true ->
        handleFixedSizeFragmentation lm p codec errCode ii uperMaxSizeInBits minSize internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString
    | false ->
        let fragmentation   = lm.uper.Fragmentation_sqf
        let sRemainingItemsVar = sprintf "%s%d" "nRemainingItemsVar" ii
        let sCurBlockSize      = sprintf "%s%d" "nCurBlockSize" ii
        let sBlockIndex        = sprintf "%s%d" "nBlockIndex" ii
        let sCurOffset         = sprintf "%s%d" "nCurOffset" ii
        let sBLJ               = sprintf "%s%d" "nBLJ" ii
        let sLengthTmp         = sprintf "%s%d" "nLengthTmp" ii
        let sBLI               = sprintf "i%d" ii
        //let lv = SequenceOfIndex (ii, None)

        let createLv = lm.lg.uper.createLv

        let fragmentationVars = [createLv sRemainingItemsVar; createLv sCurBlockSize; createLv sCurOffset ]

        let fragmentationVars = fragmentationVars |> List.addIf (codec = Encode && lm.lg.uper.requires_sBLJ) (createLv sBLJ)
        let fragmentationVars = fragmentationVars |> List.addIf (codec = Encode) (createLv sBlockIndex)
        let fragmentationVars = fragmentationVars |> List.addIf (codec = Decode && minSize <> maxSize) (createLv sLengthTmp)
        fragmentation (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) internalItem_funcBody  nIntItemMaxSize ( minSize) ( maxSize) uperMaxSizeInBits (minSize <> maxSize) errCode.errCodeName sRemainingItemsVar sCurBlockSize sBlockIndex sCurOffset sBLJ sBLI sLengthTmp bIsBitStringType bIsAsciiString codec, fragmentationVars

let createIA5StringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionOrReference)   (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let ii = t.id.SequenceOfLevel + 1
    let i = sprintf "i%d" ii
    let lv = SequenceOfIndex (ii, None)
    let charIndex =
        match lm.lg.uper.requires_charIndex with
        | false     -> []
        | true   -> [IntegerLocalVariable ("charIndex", None)]
    let nStringLength =
        match o.minSize.uper = o.maxSize.uper with
        | true  -> []
        | false -> [lm.lg.uper.createLv "nStringLength"]
    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let td0 = lm.lg.getStrTypeDefinition o.typeDef
        let td = td0.longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let InternalItem_string_no_alpha   = lm.uper.InternalItem_string_no_alpha
        let InternalItem_string_with_alpha = lm.uper.InternalItem_string_with_alpha
        let str_FixedSize       = lm.uper.str_FixedSize
        let str_VarSize         = lm.uper.str_VarSize
        let typeDefinitionName = lm.lg.getLongTypedefName typeDefinition

        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
        let internalItem =
            match o.uperCharSet.Length = 128 with
            | true  -> InternalItem_string_no_alpha (p.arg.joined lm.lg) errCode.errCodeName i codec
            | false ->
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                InternalItem_string_with_alpha (p.arg.joined lm.lg) errCode.errCodeName td i (BigInteger (o.uperCharSet.Length-1)) arrAsciiCodes (BigInteger (o.uperCharSet.Length)) nBits  codec
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.uper - o.minSize.uper))
        let initExpr =
            match codec, lm.lg.decodingKind with
            | Decode, Copy -> Some (lm.lg.initializeString (int o.maxSize.uper))
            | _ -> None
        let pp, resultExpr = joinedOrAsIdentifier lm codec p

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
            encDec = Some internalItem
            elemDecodeFn = None
            ixVariable = i
        }
        let introSnap = nestingScope.nestingLevel = 0I
        let auxiliaries, callAux = lm.lg.generateSequenceOfLikeAuxiliaries r (if fromACN then ACN else UPER) (StrType o) sqfProofGen codec

        let funcBodyContent,localVariables =
            match o.minSize with
            | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper ->
                str_FixedSize pp typeDefinitionName i internalItem o.minSize.uper nBits nBits 0I initExpr introSnap callAux codec, lv::charIndex@nStringLength
            | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper ->
                str_VarSize pp (p.arg.joined lm.lg) typeDefinitionName i internalItem o.minSize.uper o.maxSize.uper nSizeInBits nBits nBits 0I initExpr callAux codec, lv::charIndex@nStringLength
            | _ ->
                let funcBodyContent,localVariables = handleFragmentation lm p codec errCode ii o.uperMaxSizeInBits o.minSize.uper o.maxSize.uper internalItem nBits false true
                let localVariables = localVariables |> List.addIf (lm.lg.uper.requires_IA5String_i || o.maxSize.uper<>o.minSize.uper) lv
                funcBodyContent, charIndex@localVariables

        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = auxiliaries}

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e ns p b -> Some (funcBody e ns p b)) soSparkAnnotations  [] us


let createOctetStringFunction_funcBody (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefinitionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (o:Asn1AcnAst.OctetString) (errCode:ErrorCode) (p:CallerScope) =
    let ii = id.SequenceOfLevel + 1;
    let i = sprintf "i%d" ii
    let lv = SequenceOfIndex (id.SequenceOfLevel + 1, None)

    let td = typeDefinition.longTypedefName2 lm.lg.hasModules
    let pp, resultExpr = joinedOrAsIdentifier lm codec p
    let access = lm.lg.getAccess p.arg

    let InternalItem_oct_str = lm.uper.InternalItem_oct_str
    let fixedSize = lm.uper.octet_FixedSize
    let varSize  = lm.uper.octet_VarSize

    let nIntItemMaxSize = 8I
    let internalItem = InternalItem_oct_str pp access i  errCode.errCodeName codec
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
    let funcBodyContent, localVariables =
        let nStringLength =
            match isFixedSize,  codec with
            | true , _    -> []
            | false, Encode -> []
            | false, Decode -> [lm.lg.uper.count_var]

        match minSize with
        | _ when maxSize < 65536I && isFixedSize -> fixedSize td pp access minSize codec, (if false then lv::nStringLength else nStringLength)
        | _ when maxSize < 65536I && (not isFixedSize) -> varSize td pp access minSize maxSize nSizeInBits  errCode.errCodeName codec, (if false  then lv::nStringLength else nStringLength)
        | _ ->
            let funcBodyContent,localVariables = handleFragmentation lm p codec errCode ii uperMaxSizeInBits minSize maxSize internalItem nIntItemMaxSize false false
            let localVariables = localVariables |> List.addIf (lm.lg.uper.requires_IA5String_i || (not isFixedSize)) (lv)
            funcBodyContent, localVariables

    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}



let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        createOctetStringFunction_funcBody r lm codec t.id  typeDefinition o.isFixedSize  o.uperMaxSizeInBits o.minSize.uper o.maxSize.uper o (errCode:ErrorCode) (p:CallerScope)

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e ns p b -> Some (funcBody e ns p b)) soSparkAnnotations  [] us



(*
let createBitStringFunction_funcBody (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefinitionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErrorCode) (p:CallerScope) =
    lm.lg.uper.createBitStringFunction (handleFragmentation lm) codec id typeDefinition isFixedSize  uperMaxSizeInBits minSize maxSize errCode p
*)

let createBitStringFunction_funcBody (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefinitionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (o:Asn1AcnAst.BitString) (errCode:ErrorCode) (p:CallerScope) =
    let bitString_FixSize = lm.uper.bitString_FixSize
    let bitString_VarSize = lm.uper.bitString_VarSize
    let ii = id.SequenceOfLevel + 1;
    let i = sprintf "i%d" (id.SequenceOfLevel + 1)
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
    let internalItem = lm.uper.InternalItem_bit_str (p.arg.joined lm.lg) i  errCode.errCodeName codec
    let iVar = SequenceOfIndex (id.SequenceOfLevel + 1, None)
    let td = typeDefinition.longTypedefName2 lm.lg.hasModules
    let pp, resultExpr = joinedOrAsIdentifier lm codec p
    let access = lm.lg.getAccess p.arg

    let funcBodyContent, localVariables =
        let nStringLength =
            match isFixedSize,  codec with
            | true , _    -> []
            | false, Encode -> []
            | false, Decode -> [lm.lg.uper.count_var]

        match minSize with
        | _ when maxSize < 65536I && isFixedSize -> bitString_FixSize td pp access minSize errCode.errCodeName codec, nStringLength
        | _ when maxSize < 65536I && (not isFixedSize) -> bitString_VarSize td pp access minSize maxSize errCode.errCodeName nSizeInBits codec, nStringLength
        | _ ->
            let funcBodyContent, fragmentationLvars = handleFragmentation lm p codec errCode ii uperMaxSizeInBits minSize maxSize internalItem 1I true false
            let fragmentationLvars = fragmentationLvars |> List.addIf ((not isFixedSize) &&  lm.lg.uper.requires_sBLJ) (iVar)
            (funcBodyContent,fragmentationLvars)

    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody  (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        createBitStringFunction_funcBody r lm codec t.id typeDefinition o.isFixedSize  o.uperMaxSizeInBits o.minSize.uper o.maxSize.uper o (errCode:ErrorCode) (p:CallerScope)

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e ns p b -> Some (funcBody e ns p b))  soSparkAnnotations  [] us


let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (child:Asn1Type) (us:State)  =
    let fixedSize       = lm.uper.seqOf_FixedSize
    let varSize         = lm.uper.seqOf_VarSize
    let td = typeDefinition.longTypedefName2 lm.lg.hasModules
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.uper - o.minSize.uper))
    let nIntItemMaxSize = ( child.uperMaxSizeInBits)

    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        match baseFuncName with
        | None ->
            let pp, resultExpr = joinedOrAsIdentifier lm codec p
            let childNestingScope = {nestingScope with nestingLevel = nestingScope.nestingLevel + 1I; parents = (p, t) :: nestingScope.parents}
            let access = lm.lg.getAccess p.arg
            // `childInitExpr` is used to initialize the array of elements in which we will write their decoded values
            // It is only meaningful for "Copy" decoding kind, since InPlace will directly modify `p`'s array
            let childInitExpr = DAstInitialize.getChildExpression lm child

            let ii = t.id.SequenceOfLevel + 1
            let i = sprintf "i%d" ii
            let lv = lm.lg.uper.seqof_lv t.id o.minSize.uper o.maxSize.uper

            let nStringLength =
                match o.minSize.uper = o.maxSize.uper,  codec with
                | true , _    -> []
                | false, Encode -> []
                | false, Decode -> [lm.lg.uper.count_var]

            let chFunc = child.getUperFunction codec
            let chp =
                let recv = if lm.lg.decodingKind = Copy then Selection.emptyPath p.arg.asIdentifier p.arg.selectionType else p.arg
                {p with arg = lm.lg.getArrayItem recv i child.isIA5String}
            let internalItem = chFunc.funcBody childNestingScope chp fromACN

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
                ixVariable = i
            }
            let auxiliaries, callAux = lm.lg.generateSequenceOfLikeAuxiliaries r (if fromACN then ACN else UPER) (SqOf o) sqfProofGen codec

            let absOffset = nestingScope.uperOffset
            let remBits = nestingScope.uperOuterMaxSize - nestingScope.uperOffset
            let lvl = max 0I (nestingScope.nestingLevel - 1I)
            let ix = nestingScope.nestingIx + 1I
            let offset = nestingScope.uperRelativeOffset
            let introSnap = nestingScope.nestingLevel = 0I

            match internalItem with
            | None  ->
                match o.minSize with
                | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper  -> None
                | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper ->
                    let funcBody = varSize pp access  td i "" o.minSize.uper o.maxSize.uper nSizeInBits child.uperMinSizeInBits nIntItemMaxSize 0I childInitExpr errCode.errCodeName absOffset remBits lvl ix offset introSnap callAux codec
                    Some ({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = lv@nStringLength; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = auxiliaries})
                | _                                                ->
                    let funcBody, localVariables = handleFragmentation lm p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper "" nIntItemMaxSize false false
                    Some ({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = auxiliaries})
            | Some internalItem ->
                let childErrCodes =  internalItem.errCodes
                let internalItemBody =
                    match codec, lm.lg.decodingKind with
                    | Decode, Copy ->
                        assert internalItem.resultExpr.IsSome
                        internalItem.funcBody + "\n" + (lm.uper.update_array_item pp i internalItem.resultExpr.Value)
                    | _ -> internalItem.funcBody
                let ret,localVariables =
                    match o.minSize with
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper -> fixedSize pp td i internalItemBody o.minSize.uper child.uperMinSizeInBits nIntItemMaxSize 0I childInitExpr callAux codec, nStringLength
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper -> varSize pp access  td i internalItemBody o.minSize.uper o.maxSize.uper nSizeInBits child.uperMinSizeInBits nIntItemMaxSize 0I childInitExpr errCode.errCodeName absOffset remBits lvl ix offset introSnap callAux codec, nStringLength
                    | _ -> handleFragmentation lm p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper internalItemBody nIntItemMaxSize false false
                Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = lv@(localVariables@internalItem.localVariables); bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=internalItem.auxiliaries@auxiliaries})
        | Some baseFuncName ->
            let pp, resultExpr = adaptArgumentPtr lm codec p
            let funcBodyContent =  callBaseTypeFunc lm pp baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []})
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  [] us

type private SequenceChildState = {
    childIx: bigint
    uperAccBits: bigint
    acnAccBits: bigint
}
type private SequenceChildStmt = {
    body: string option
    lvs: LocalVariable list
    errCodes: ErrorCode list
}
type private SequenceChildResult = {
    stmt: SequenceChildStmt option
    resultExpr: string option
    props: SequenceChildProps
    auxiliaries: string list
}

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (us:State)  =
    // stg macros
    let sequence_presence_bit       = lm.uper.sequence_presence_bit
    let sequence_presence_bit_fix   = lm.uper.sequence_presence_bit_fix
    let sequence_mandatory_child    = lm.uper.sequence_mandatory_child
    let sequence_optional_child     = lm.uper.sequence_optional_child
    let sequence_default_child      = lm.uper.sequence_default_child
    let sequence_build              = lm.uper.sequence_build

    let td = typeDefinition.longTypedefName2 lm.lg.hasModules

    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let nonAcnChildren = children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
        let localVariables =
            match nonAcnChildren |> Seq.exists(fun x -> x.Optionality.IsSome) with
            | true when codec = CommonTypes.Decode && lm.lg.uper.requires_presenceBit -> [(FlagLocalVariable ("presenceBit", None))]
            | _ -> []
        let pp, resultExpr = joinedOrAsIdentifier lm codec p
        let access = lm.lg.getAccess p.arg

        let printPresenceBit (child: Asn1Child): string option =
            let childName = lm.lg.getAsn1ChildBackendName child
            let existVar =
                match codec, lm.lg.decodingKind with
                | Decode, Copy -> Some (ToC (child._c_name + "_exist"))
                | _ -> None
            let absent, present =
                match ST.lang with
                | Scala -> "false", "true"
                | _ -> "0", "1"
            // please note that in decode, macro uper_sequence_presence_bit_fix
            // calls macro uper_sequence_presence_bit (i.e. behaves like optional)
            let seq_presence_bit_fix (value: string) =
                sequence_presence_bit_fix pp access childName existVar errCode.errCodeName value codec
            match child.Optionality with
            | None -> None
            | Some Asn1AcnAst.AlwaysAbsent -> Some (seq_presence_bit_fix absent)
            | Some Asn1AcnAst.AlwaysPresent -> Some (seq_presence_bit_fix present)
            | Some (Asn1AcnAst.Optional opt) -> Some (sequence_presence_bit pp access childName existVar errCode.errCodeName codec)

        let handleChild (s: SequenceChildState) (child:Asn1Child): SequenceChildResult * SequenceChildState =
            let childName =  TL "handleChild_01" (fun () -> lm.lg.getAsn1ChildBackendName child)
            let childTypeDef = TL "handleChild_02" (fun () -> child.Type.typeDefinitionOrReference.longTypedefName2 lm.lg.hasModules)
            let childNestingScope =
                TL "handleChild_03" (fun () -> 
                {nestingScope with
                    nestingLevel = nestingScope.nestingLevel + 1I
                    nestingIx = nestingScope.nestingIx + s.childIx
                    uperRelativeOffset = s.uperAccBits
                    uperOffset = nestingScope.uperOffset + s.uperAccBits
                    parents = (p, t) :: nestingScope.parents})
            let chFunc = TL "handleChild_04" (fun () -> child.Type.getUperFunction codec)
            let childSel = TL "handleChild_05" (fun () -> lm.lg.getSeqChild p.arg childName child.Type.isIA5String child.Optionality.IsSome)
            let childP =
                TL "handleChild_06" (fun () -> 
                let newArg = if lm.lg.usesWrappedOptional && childSel.isOptional && codec = Encode then childSel.asLast else childSel
                {p with arg = newArg})
            let log_name = "HC_" + child.Type.Kind.BaseAsn1Type
            let childContentResult = TL log_name (fun () -> chFunc.funcBody childNestingScope childP fromACN)
            let existVar =
                TL "handleChild_08" (fun () -> 
                match codec, lm.lg.decodingKind with
                | Decode, Copy -> Some (ToC (child._c_name + "_exist"))
                | _ -> None)

            let props = TL "handleChild_09" (fun () -> {info=(Asn1Child child).toAsn1AcnAst; sel=childSel; uperMaxOffset=s.uperAccBits; acnMaxOffset=s.acnAccBits})
            let newAcc = TL "handleChild_10" (fun () -> {childIx=s.childIx + 1I; uperAccBits=s.uperAccBits + child.uperMaxSizeInBits; acnAccBits=s.acnAccBits + child.acnMaxSizeInBits})

            match childContentResult with
            | None ->
                // Copy-decoding expects to have a result expression (even if unused), so we pick the initExpression
                let childResultExpr =
                    TL "handleChild_11" (fun () -> 
                    match codec, lm.lg.decodingKind with
                    | Decode, Copy -> Some child.Type.initFunction.initExpression
                    | _ -> None)
                {stmt=None; resultExpr=childResultExpr; props=props; auxiliaries = []}, newAcc
            | Some childContent ->
                let childBody, child_localVariables =
                    match child.Optionality with
                    | None -> TL "handleChild_12" (fun () -> Some (sequence_mandatory_child childName childContent.funcBody codec) , childContent.localVariables)
                    | Some Asn1AcnAst.AlwaysAbsent ->
                        TL "handleChild_13" (fun () -> 
                        match codec with
                        | CommonTypes.Encode -> None, []
                        | CommonTypes.Decode -> Some (sequence_optional_child pp access childName childContent.funcBody existVar childContent.resultExpr childTypeDef codec), childContent.localVariables)
                    | Some Asn1AcnAst.AlwaysPresent ->
                        TL "handleChild_14" (fun () -> 
                        if lm.lg.usesWrappedOptional then
                            Some (sequence_optional_child pp access childName childContent.funcBody existVar childContent.resultExpr childTypeDef codec), childContent.localVariables
                        else
                            match codec with
                            | CommonTypes.Encode -> Some childContent.funcBody, childContent.localVariables
                            | CommonTypes.Decode -> Some (sequence_optional_child pp access childName childContent.funcBody existVar childContent.resultExpr childTypeDef codec), childContent.localVariables)
                    | Some (Asn1AcnAst.Optional opt) ->
                        TL "handleChild_15" (fun () ->
                        match opt.defaultValue with
                        | None -> Some (sequence_optional_child pp access childName childContent.funcBody existVar childContent.resultExpr childTypeDef codec), childContent.localVariables
                        | Some v ->
                            let defInit= child.Type.initFunction.initByAsn1Value childP (mapValue v).kind
                            Some (sequence_default_child pp access childName childContent.funcBody existVar childContent.resultExpr childTypeDef defInit codec), childContent.localVariables)
                TL "handleChild_16" (fun () ->                 
                {
                    stmt = Some {
                        body = childBody
                        lvs = child_localVariables
                        errCodes = childContent.errCodes
                    }
                    resultExpr = childContent.resultExpr
                    props = props
                    auxiliaries = childContent.auxiliaries
                }, newAcc)

        let presenceBits = TL "SQ_UPER_presenceBits" (fun () -> nonAcnChildren |> List.map printPresenceBit)
        let nbPresenceBits = TL "SQ_UPER_nbPresenceBits" (fun () -> presenceBits |> List.sumBy (fun s -> if s.IsSome then 1I else 0I))
        let childrenStatements00, _ = TL "SQ_UPER_handleChild" (fun () -> nonAcnChildren |> foldMap handleChild {childIx=nbPresenceBits; uperAccBits=nbPresenceBits; acnAccBits=nbPresenceBits})

        let seqProofGen =
            TL "SQ_UPER_seqProofGen" (fun () -> 
                let children = childrenStatements00 |> List.map (fun xs -> xs.props)
                {SequenceProofGen.t = t; sq = o; sel = p.arg; acnOuterMaxSize = nestingScope.acnOuterMaxSize; uperOuterMaxSize = nestingScope.uperOuterMaxSize;
                nestingLevel = nestingScope.nestingLevel; nestingIx = nestingScope.nestingIx;
                uperMaxOffset = nestingScope.uperOffset; acnMaxOffset = nestingScope.acnOffset;
                acnSiblingMaxSize = nestingScope.acnSiblingMaxSize; uperSiblingMaxSize = nestingScope.uperSiblingMaxSize;
                children = children})
        let allStmts =
            TL "SQ_UPER_allStmts" (fun () -> 
                let children = childrenStatements00 |> List.map (fun s -> s.stmt |> Option.bind (fun stmt -> stmt.body))
                presenceBits @ children)
        let childrenStatements = TL "SQ_UPER_generateSequenceChildProof" (fun () -> lm.lg.generateSequenceChildProof r UPER allStmts seqProofGen codec)

        let childrenStatements0 = TL "SQ_UPER_1" (fun () -> childrenStatements00 |> List.choose(fun s -> s.stmt))
        let childrenLocalVars   = TL "SQ_UPER_2" (fun () -> childrenStatements0 |> List.collect(fun s -> s.lvs))
        let childrenErrCodes    = TL "SQ_UPER_3" (fun () -> childrenStatements0 |> List.collect(fun s -> s.errCodes))
        let childrenResultExpr  = TL "SQ_UPER_4" (fun () -> childrenStatements00 |> List.choose(fun s -> s.resultExpr))
        let childrenAuxiliaries = TL "SQ_UPER_5" (fun () -> childrenStatements00 |> List.collect(fun s -> s.auxiliaries))

        // If we are Decoding with Copy decoding kind, then all children `resultExpr` must be defined as well (i.e. we must have the same number of `resultExpr` as children)
        assert (resultExpr.IsNone || childrenResultExpr.Length = nonAcnChildren.Length)
        let seqBuild    = TL "SQ_UPER_6" (fun () ->  resultExpr |> Option.map (fun res -> sequence_build res td p.arg.isOptional childrenResultExpr) |> Option.toList)
        let seqContent  = TL "SQ_UPER_7" (fun () -> (childrenStatements@seqBuild) |> nestChildItems lm codec)
        TL "SQ_UPER_8" (fun () -> 
            match seqContent with
            | None  -> None
            | Some ret -> Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalVars; bValIsUnReferenced=false; bBsIsUnReferenced=(o.uperMaxSizeInBits = 0I); resultExpr=resultExpr; auxiliaries=childrenAuxiliaries}))

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  [] us



let createChoiceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (us:State)  =
    // stg macros
    let choice_child       = lm.uper.choice_child
    let choice             = lm.uper.choice

    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    let sChoiceIndexName = (lm.lg.getLongTypedefName typeDefinition) + "_index_tmp"
    let localVariables =
        match codec with
        | CommonTypes.Encode  -> []
        | CommonTypes.Decode  -> [(Asn1SIntLocalVariable (sChoiceIndexName, None))]

    let typeDefinitionName = typeDefinition.longTypedefName2 lm.lg.hasModules

    let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
        let td0 = lm.lg.getChoiceTypeDefinition o.typeDef
        let td = td0.longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let acnSiblingMaxSize = children |> List.map (fun c -> c.chType.acnMaxSizeInBits) |> List.max
        let uperSiblingMaxSize = children |> List.map (fun c -> c.chType.uperMaxSizeInBits) |> List.max

        let handleChild (nIndexSizeInBits: BigInteger) (i: int) (child: ChChildInfo): string * LocalVariable list * ErrorCode list * string list =
            let childNestingScope =
                {nestingScope with
                    nestingLevel = nestingScope.nestingLevel + 1I
                    uperSiblingMaxSize = Some uperSiblingMaxSize
                    acnSiblingMaxSize = Some acnSiblingMaxSize
                    parents = (p, t) :: nestingScope.parents}
            let chFunc = child.chType.getUperFunction codec
            let uperChildRes =
                match lm.lg.uper.catd with
                | false   -> chFunc.funcBody childNestingScope ({p with arg =  lm.lg.getChChild p.arg (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String}) fromACN
                | true when codec = CommonTypes.Decode -> chFunc.funcBody childNestingScope {p with arg = Selection.valueEmptyPath ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")} fromACN
                | true -> chFunc.funcBody childNestingScope ({p with arg = lm.lg.getChChild p.arg  (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String}) fromACN
            let sChildName = (lm.lg.getAsn1ChChildBackendName child)
            let sChildTypeDef = child.chType.typeDefinitionOrReference.longTypedefName2 lm.lg.hasModules
            let isSequence = match child.chType.Kind with | Sequence _ -> true | _ -> false
            let isEnum = match child.chType.Kind with | Enumerated _ -> true | _ -> false
            let sChildInitExpr = child.chType.initFunction.initExpression
            let sChoiceTypeName = typeDefinitionName

            let mk_choice_child (childContent: string): string =
                choice_child (p.arg.joined lm.lg) (lm.lg.getAccess p.arg) (lm.lg.presentWhenName (Some typeDefinition) child) (BigInteger i) nIndexSizeInBits (BigInteger (children.Length - 1)) childContent sChildName sChildTypeDef sChoiceTypeName sChildInitExpr isSequence isEnum codec

            match uperChildRes with
            | None              ->
                let childContent =
                    match lm.lg.uper.catd with
                    | false -> lm.lg.createSingleLineComment "no encoding/decoding is required"
                    | true when codec=Decode ->
                        let childp = ({p with arg = Selection.valueEmptyPath ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")})
                        match child.chType.ActualType.Kind with
                        | NullType _    -> uper_a.decode_nullType childp.arg.receiverId
                        | Sequence _    -> uper_a.decode_empty_sequence_emptySeq childp.arg.receiverId
                        | _             -> lm.lg.createSingleLineComment "no encoding/decoding is required"
                    | true   -> lm.lg.createSingleLineComment "no encoding/decoding is required"
                mk_choice_child childContent, [], [], []
            | Some childContent ->
                mk_choice_child childContent.funcBody, childContent.localVariables, childContent.errCodes, childContent.auxiliaries

        match baseFuncName with
        | None ->
            let nIndexSizeInBits = (GetNumberOfBitsForNonNegativeInteger (BigInteger (children.Length - 1)))
            let childrenContent3 = children |> List.mapi (handleChild nIndexSizeInBits)
            let childrenContent = childrenContent3 |> List.map(fun (s,_,_,_) -> s)
            let childrenLocalvars = childrenContent3 |> List.collect(fun (_,s,_,_) -> s)
            let childrenErrCodes = childrenContent3 |> List.collect(fun (_,_,s,_) -> s)
            let childrenAuxiliaries = childrenContent3 |> List.collect(fun (_,_,_,a) -> a)
            let introSnap = nestingScope.nestingLevel = 0I
            let pp, resultExpr = joinedOrAsIdentifier lm codec p
            let ret = choice pp (lm.lg.getAccess p.arg) childrenContent (BigInteger (children.Length - 1)) sChoiceIndexName errCode.errCodeName td nIndexSizeInBits introSnap codec
            let ret = lm.lg.generateChoiceProof r ACN t o ret p.arg codec
            Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=childrenAuxiliaries})
        | Some baseFuncName ->
            let pp, resultExpr = adaptArgumentPtr lm codec p
            let funcBodyContent = callBaseTypeFunc lm pp baseFuncName codec
            let ret = lm.lg.generateChoiceProof r ACN t o funcBodyContent p.arg codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries=[]})

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)

    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  [] us

let createReferenceFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
    let baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t.id "" codec

    match o.encodingOptions with
    | None          ->
        let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        let t1WithExtensions = o.resolvedType;
        let areUperTypesEquivalent = TL "UPER_REF_01" (fun () -> TypesEquivalence.uperEquivalence t1 t1WithExtensions)
        match areUperTypesEquivalent with
        | true  ->
            let soSparkAnnotations = TL "UPER_REF_02" (fun () -> Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec))
            let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
                //let funcBodyContent = TL "UPER_REF_03" (fun () -> (baseType.getUperFunction codec).funcBody nestingScope p fromACN)
                //match funcBodyContent with
                //| Some _ ->
                    let pp, resultExpr =
                        TL "UPER_REF_04" (fun () -> 
                        let str = lm.lg.getParamValue t p.arg codec
                        match codec, lm.lg.decodingKind with
                        | Decode, Copy ->
                            let toc = ToC str
                            toc, Some toc
                        | _ -> str, None)
                    let funcBodyContent = TL "UPER_REF_05" (fun () -> callBaseTypeFunc lm pp baseFncName codec)
                    Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}
                //| None -> None
            TL "UPER_REF_06" (fun () -> createUperFunction r lm codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations [] us)
        | false ->
            baseType.getUperFunction codec, us
    | Some opts  ->
        TL "UPER_REF_50" (fun () -> 
            let octet_string_containing_func  = lm.uper.octet_string_containing_func
            let bit_string_containing_func  = lm.uper.bit_string_containing_func
            let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
            let funcBody (errCode:ErrorCode) (nestingScope: NestingScope) (p:CallerScope) (fromACN: bool) =
                match (baseType.getUperFunction codec).funcBody nestingScope p fromACN with
                | Some _ ->
                    let pp, resultExpr =
                        let str = lm.lg.getParamValue t p.arg codec
                        match codec, lm.lg.decodingKind with
                        | Decode, Copy ->
                            let toc = ToC str
                            toc, Some toc
                        | _ -> str, None
                    let nBits = GetNumberOfBitsForNonNegativeInteger (opts.maxSize.uper - opts.minSize.uper)
                    let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ENCODING" baseTypeDefinitionName
                    let sReqBitForUperEncoding = sprintf "%s_REQUIRED_BITS_FOR_ENCODING" baseTypeDefinitionName
                    let funcBodyContent =
                        match opts.octOrBitStr with
                        | ContainedInOctString  -> octet_string_containing_func pp baseFncName sReqBytesForUperEncoding nBits opts.minSize.uper opts.maxSize.uper codec
                        | ContainedInBitString  -> bit_string_containing_func pp baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding nBits opts.minSize.uper opts.maxSize.uper codec
                    Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false; resultExpr=resultExpr; auxiliaries = []}
                | None -> None
            createUperFunction r lm codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  [] us)
