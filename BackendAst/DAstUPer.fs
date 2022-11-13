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


//TODO
//1.Decode functions (and perhaps encode function) muct check if the decode value is within the constraints (currently, implemented only for Integers and for case IntUnconstraintMax )
//2.Fragmentation


let internal createUperFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option)  (funcBody_e:ErroCode->CallerScope -> (UPERFuncBodyResult option)) soSparkAnnotations (us:State)  =
    let typeDef = lm.lg.getTypeDefinition t.FT_TypeDefintion
    let funcName            = getFuncName r lm codec t.id typeDef
    let errCodeName         = ToC ("ERR_UPER" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None
    let soInitFuncName = getFuncNameGeneric typeDefinition (lm.init.methodNameSuffix())
    let EmitTypeAssignment = lm.uper.EmitTypeAssignment
    let EmitTypeAssignment_def = lm.uper.EmitTypeAssignment_def
    let EmitTypeAssignment_def_err_code  = lm.uper.EmitTypeAssignment_def_err_code

    let funcBody = (funcBody_e errCode)
    let p  = lm.lg.getParamType t codec
    let topLevAcc = lm.lg.getAcces p.arg
    let varName = p.arg.p
    let sStar = lm.lg.getStar p.arg
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let sInitilialExp = ""
    let  func, funcDef  = 
            match funcName  with
            | None              -> None, None
            | Some funcName     -> 
                let content = funcBody p  
                let bodyResult_funcBody, errCodes,  bodyResult_localVariables, bBsIsUnreferenced, bVarNameIsUnreferenced = 
                    match content with 
                    | None              -> 
                        let emtyStatement = lm.lg.emtyStatement
                        emtyStatement, [], [], true, isValidFuncName.IsNone
                    | Some bodyResult   -> bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsUnReferenced, bodyResult.bValIsUnReferenced
                let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct
                let func = Some(EmitTypeAssignment varName sStar funcName isValidFuncName  (lm.lg.getLongTypedefName typeDefinition) lvars  bodyResult_funcBody soSparkAnnotations sInitilialExp (t.uperMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced soInitFuncName codec)
                
                //let errCodes = bodyResult.errCodes
                let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
                let funcDef = Some(EmitTypeAssignment_def varName sStar funcName  (lm.lg.getLongTypedefName typeDefinition) errCodStr (t.uperMaxSizeInBits = 0I) (BigInteger (ceil ((double t.uperMaxSizeInBits)/8.0))) ( t.uperMaxSizeInBits) soSparkAnnotations (t.uperMaxSizeInBits = 0I) codec)
                func, funcDef


    let ret = 
        {
            UPerFunction.funcName      = funcName
            func                       = func 
            funcDef                    = funcDef
            funcBody                   = funcBody
            funcBody_e                 = funcBody_e
        }
    ret, ns

let getIntDecFuncSuffix (r:Asn1AcnAst.AstRoot) uperRange =
    match Asn1AcnAstUtilFunctions.getIntEncodingClassByUperRange r.args uperRange with
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

let castPp (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) codec pp uperRange encFuncBits =
    match codec with 
    | CommonTypes.Encode -> 
        match Asn1AcnAstUtilFunctions.getIntEncodingClassByUperRange r.args uperRange with
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


let getIntfuncBodyByCons (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) uperRange errLoc isUnsigned (cons: IntegerTypeConstraint list) (allCons: IntegerTypeConstraint list) (errCode:ErroCode) (p:CallerScope) = 
    let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg

    let IntNoneRequired         = lm.uper.IntNoneRequired          
    let IntFullyConstraintPos   = lm.uper.IntFullyConstraintPos    
    let IntFullyConstraint      = lm.uper.IntFullyConstraint       
    let IntSemiConstraintPos    = lm.uper.IntSemiConstraintPos     
    let IntSemiConstraint       = lm.uper.IntSemiConstraint        
    let IntUnconstraint         = lm.uper.IntUnconstraint          
    let IntUnconstraintMax      = lm.uper.IntUnconstraintMax       
    let IntRootExt              = lm.uper.IntRootExt               
    let IntRootExt2             = lm.uper.IntRootExt2              
    let rootCons = cons |> List.choose(fun x -> match x with RangeRootConstraint(_, a) |RangeRootConstraint2(_, a,_) -> Some(x) |_ -> None) 

    let checkExp = 
        //match (DastValidate2.createIntegerFunctionByCons r l isUnsigned allCons) with
        //| None  ->  None
        //| Some expFunc -> Some (expFunc p)
        None
    let sSsuffix = getIntDecFuncSuffix r uperRange 
    let castPp encFuncBits = castPp r lm codec pp uperRange encFuncBits

    let IntBod uperRange extCon =
        match uperRange with
      //| Concrete(min, max) when min=max                    -> IntNoneRequired (p.arg.getValue l) min   errCode.errCodeName codec, false, (match l with C -> true | Ada -> false)
        | Concrete(min, max) when min=max                    -> IntNoneRequired (lm.lg.getValue p.arg) min   errCode.errCodeName codec, codec=Decode, true
        | Concrete(min, max) when min>=0I && (not extCon)    -> IntFullyConstraintPos (castPp ((int r.args.integerSizeInBytes)*8)) min max (GetNumberOfBitsForNonNegativeInteger (max-min))  sSsuffix errCode.errCodeName codec, false, false
        | Concrete(min, max)                                 -> IntFullyConstraint (castPp ((int r.args.integerSizeInBytes)*8)) min max (GetNumberOfBitsForNonNegativeInteger (max-min))  sSsuffix errCode.errCodeName codec, false, false
        | PosInf(a)  when a>=0I && (not extCon)  -> IntSemiConstraintPos pp a  errCode.errCodeName codec, false, false
        | PosInf(a)               -> IntSemiConstraint pp a  errCode.errCodeName codec, false, false
        | NegInf(max)             -> IntUnconstraintMax pp max checkExp errCode.errCodeName codec, false, false
        | Full                    -> IntUnconstraint pp errCode.errCodeName false codec, false, false

    let getValueByConstraint uperRange =
        match uperRange with
        | Concrete(a, _)  -> a
        | PosInf(a)       -> a
        | NegInf(b)       -> b
        | Full            -> 0I
    let funcBodyContent, bValIsUnReferenced, bBsIsUnReferenced = 
        match rootCons with
        | []                            -> IntBod uperRange false
        | (RangeRootConstraint (_, a))::rest      -> 
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            let cc,_ = DastValidate2.integerConstraint2ValidationCodeBlock r lm isUnsigned a 0
            let cc = DastValidate2.ValidationBlockAsStringExpr (cc p) 
            //let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) p a
            let rootBody, _,_ = IntBod uperR true
            IntRootExt pp (getValueByConstraint uperR) cc rootBody errCode.errCodeName codec, false, false
        | (RangeRootConstraint2(_,a,_))::rest  -> 
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            //let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) p a
            let cc,_ = DastValidate2.integerConstraint2ValidationCodeBlock r lm isUnsigned a 0
            let cc = DastValidate2.ValidationBlockAsStringExpr (cc p) 
            let rootBody, _,_ = IntBod uperR true
            IntRootExt2 pp (getValueByConstraint uperR) cc rootBody errCode.errCodeName codec, false, false 
        | _                             -> raise(BugErrorException "")
    Some({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=bValIsUnReferenced; bBsIsUnReferenced=bBsIsUnReferenced})
    




let createIntegerFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        getIntfuncBodyByCons r lm codec o.uperRange t.Location (o.getClass r.args) o.cons o.AllCons errCode p
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> funcBody e p) soSparkAnnotations us


let createBooleanFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let Boolean         = lm.uper.Boolean
        let funcBodyContent = Boolean pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
        
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let castRPp  = DAstEqual.castRPp

let createRealFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let sSuffix =
        match o.getClass r.args with
        | ASN1SCC_REAL   -> ""
        | ASN1SCC_FP32   -> "_fp32"
        | ASN1SCC_FP64   -> ""


    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let castPp = castRPp lm codec (o.getClass r.args) pp 
        let Real         = lm.uper.Real
        let funcBodyContent = Real castPp sSuffix errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = lm.lg.getPointer p.arg
        let ObjectIdentifier         = 
            if o.relativeObjectId then
                lm.uper.RelativeOID
            else
                lm.uper.ObjectIdentifier
        let funcBodyContent = ObjectIdentifier pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let getTimeSubTypeByClass (tc) =
    match tc with
    |Asn1LocalTime                      _ -> "Asn1LocalTime"
    |Asn1UtcTime                        _ -> "Asn1UtcTime"
    |Asn1LocalTimeWithTimeZone          _ -> "Asn1TimeWithTimeZone"
    |Asn1Date                             -> "Asn1Date"
    |Asn1Date_LocalTime                 _ -> "Asn1DateLocalTime"
    |Asn1Date_UtcTime                   _ -> "Asn1DateUtcTime"
    |Asn1Date_LocalTimeWithTimeZone     _ -> "Asn1DateTimeWithTimeZone"


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = lm.lg.getPointer p.arg
        let TimeType         =  lm.uper.Time
        let funcBodyContent = TimeType pp (getTimeSubTypeByClass o.timeClass) errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us



let createNullTypeFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = None
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations us


let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        //let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let Enumerated         = lm.uper.Enumerated
        let Enumerated_item    = lm.uper.Enumerated_item
        let typeDef0 = lm.lg.getEnmTypeDefintion o.typeDef
        let td =  typeDef0.longTypedefName2 lm.lg.hasModules  (ToC p.modName)
        //let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
        let nMin = 0I
        let nMax = BigInteger(Seq.length o.items) - 1I
        let nLastItemIndex      = nMax
        let items = 
            o.items |> List.mapi(fun i itm -> Enumerated_item (lm.lg.getValue p.arg) (lm.lg.getNamedItemBackendName (Some typeDefinition) itm) (BigInteger i) nLastItemIndex codec) 
        let nBits = (GetNumberOfBitsForNonNegativeInteger (nMax-nMin))
        let sFirstItemName = lm.lg.getNamedItemBackendName (Some typeDefinition) o.items.Head
            //o.items.Head.getBackendName (Some typeDefinition) l
        let funcBodyContent = Enumerated (lm.lg.getValue p.arg) td items nMin nMax nBits errCode.errCodeName nLastItemIndex sFirstItemName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us


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


let handleFixedSizeFragmentation (lm:LanguageMacros) (p:CallerScope) (codec:CommonTypes.Codec) (errCode:ErroCode) ii uperMaxSizeInBits (fixSize:BigInteger) internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString=
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
        let part = fixedSize_Fragmentation_sqf_64K p.arg.p (lm.lg.getAcces p.arg) sCurOffset sCurBlockSize sBlockIndex r.nBlocks64K internalItem_funcBody sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
        [part]
    let smallBlockParts = 
        [(r.has48KBlock, lm.lg.toHex 195, C48K);(r.has32KBlock, lm.lg.toHex 194, C32K);(r.has16KBlock, lm.lg.toHex 193, C16K)] |>  //0xC3, 0xC2, 0xC1
        List.filter (fun (a,_,_) -> a) |>
        List.map (fun (_, sBlockId, nBlockSize) -> fixedSize_Fragmentation_sqf_small_block p.arg.p (lm.lg.getAcces p.arg) internalItem_funcBody nBlockSize sBlockId sCurOffset sCurBlockSize sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec)
    let parts = parts@smallBlockParts

    let bRemainingItemsWithinByte = r.nRemainingItemsVar <= C_127
    let parts=
        match r.nRemainingItemsVar > 0I with
        | true  ->
            let part = fixedSize_Fragmentation_sqf_remaining p.arg.p (lm.lg.getAcces p.arg) internalItem_funcBody bRemainingItemsWithinByte r.nRemainingItemsVar sCurOffset sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
            parts@[part]
        | false -> parts

    let createLv = lm.lg.uper.createLv


    let fragmentationVars = [createLv sCurBlockSize; createLv sCurOffset]
    let fragmentationVars = fragmentationVars |> List.addIf (codec = Decode) (createLv sRemainingItemsVar)
    let fragmentationVars = fragmentationVars |> List.addIf (lm.lg.uper.requires_sBlockIndex) (createLv sBlockIndex)
    //let fragmentationVars = fragmentationVars |> List.addIf (l = C) (lv)
    let singleNestedPart  = nestChildItems lm  codec parts |> Option.toList
    fixedSize_Fragmentation_sqf p.arg.p (lm.lg.getAcces p.arg) singleNestedPart fixSize bIsAsciiString codec, fragmentationVars

let handleFragmentation (lm:LanguageMacros) (p:CallerScope) (codec:CommonTypes.Codec) (errCode:ErroCode) ii uperMaxSizeInBits (minSize:BigInteger) (maxSize:BigInteger) internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString=
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
        fragmentation p.arg.p (lm.lg.getAcces p.arg) internalItem_funcBody  nIntItemMaxSize ( minSize) ( maxSize) uperMaxSizeInBits (minSize <> maxSize) errCode.errCodeName sRemainingItemsVar sCurBlockSize sBlockIndex sCurOffset sBLJ sBLI sLengthTmp bIsBitStringType bIsAsciiString codec, fragmentationVars

let createIA5StringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference)   (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let ii = t.id.SeqeuenceOfLevel + 1
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
            //match l with
            //| Ada  -> [IntegerLocalVariable ("nStringLength", None)]
            //| C    -> [Asn1SIntLocalVariable ("nStringLength", None)]
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let td0 = lm.lg.getStrTypeDefinition o.typeDef
        let td = td0.longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let InternalItem_string_no_alpha   = lm.uper.InternalItem_string_no_alpha
        let InternalItem_string_with_alpha = lm.uper.InternalItem_string_with_alpha
        let str_FixedSize       = lm.uper.str_FixedSize
        let str_VarSize         = lm.uper.str_VarSize
        let typeDefinitionName = lm.lg.getLongTypedefName typeDefinition//getTypeDefinitionName t.id.tasInfo typeDefinition

        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
        let internalItem =
            match o.uperCharSet.Length = 128 with
            | true  -> InternalItem_string_no_alpha p.arg.p errCode.errCodeName i  codec 
            | false -> 
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                InternalItem_string_with_alpha p.arg.p errCode.errCodeName td i (BigInteger (o.uperCharSet.Length-1)) arrAsciiCodes (BigInteger (o.uperCharSet.Length)) nBits  codec
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.uper - o.minSize.uper))
        let funcBodyContent,localVariables = 
            match o.minSize with
            | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper  -> str_FixedSize p.arg.p typeDefinitionName i internalItem ( o.minSize.uper) nBits nBits 0I codec , lv::charIndex@nStringLength
            | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper  -> str_VarSize p.arg.p typeDefinitionName i internalItem ( o.minSize.uper) ( o.maxSize.uper) nSizeInBits nBits nBits 0I codec , lv::charIndex@nStringLength
            | _                                                -> 
                let funcBodyContent,localVariables = handleFragmentation lm p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper internalItem nBits false true
                let localVariables = localVariables |> List.addIf (lm.lg.uper.requires_IA5String_i || o.maxSize.uper<>o.minSize.uper) (lv)
                funcBodyContent, charIndex@localVariables

        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
//    let soSparkAnnotations = 
//        match l with
//        | C     -> None
//        | Ada   ->
//            Some(uper_a.annotations (lm.lg.getLongTypedefName typeDefinition) true isValidFunc.IsSome true true codec)
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations  us


let createOctetStringFunction_funcBody (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    let ii = id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" ii
    let lv = SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)

    let typeDefinitionName = typeDefinition.longTypedefName2 lm.lg.hasModules //getTypeDefinitionName t.id.tasInfo typeDefinition

    let InternalItem_oct_str = lm.uper.InternalItem_oct_str
    let fixedSize       = lm.uper.octect_FixedSize
    let varSize         = lm.uper.octect_VarSize
    //let fragmentation   = match l with C -> uper_a.Fragmentation_sqf       | Ada -> uper_a.Fragmentation_sqf

    let nIntItemMaxSize = 8I
    let internalItem = InternalItem_oct_str p.arg.p (lm.lg.getAcces p.arg) i  errCode.errCodeName codec 
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
    let funcBodyContent, localVariables = 
        let nStringLength =
            match isFixedSize,  codec with
            | true , _    -> []
            | false, Encode -> []
            | false, Decode -> [lm.lg.uper.count_var]

        match minSize with
        | _ when maxSize < 65536I && isFixedSize  ->  fixedSize p.arg.p (lm.lg.getAcces p.arg) minSize codec , (if false then lv::nStringLength else nStringLength)
        | _ when maxSize < 65536I && (not isFixedSize)  -> varSize p.arg.p (lm.lg.getAcces p.arg)  (minSize) ( maxSize) nSizeInBits  errCode.errCodeName codec , (if false  then lv::nStringLength else nStringLength)
        | _                                                -> 
            let funcBodyContent,localVariables = handleFragmentation lm p codec errCode ii ( uperMaxSizeInBits) minSize maxSize internalItem nIntItemMaxSize false false
            let localVariables = localVariables |> List.addIf (lm.lg.uper.requires_IA5String_i || (not isFixedSize)) (lv)
            funcBodyContent, localVariables

    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    



let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody (errCode:ErroCode) (p:CallerScope) =
        createOctetStringFunction_funcBody r lm codec t.id  typeDefinition o.isFixedSize  o.uperMaxSizeInBits o.minSize.uper o.maxSize.uper (errCode:ErroCode) (p:CallerScope) 

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody  e p)) soSparkAnnotations  us



(*
let createBitStringFunction_funcBody (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    lm.lg.uper.createBitStringFunction (handleFragmentation lm) codec id typeDefinition isFixedSize  uperMaxSizeInBits minSize maxSize errCode p
*)

let createBitStringFunction_funcBody (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    let bitString_FixSize = lm.uper.bitString_FixSize
    let bitString_VarSize = lm.uper.bitString_VarSize
    let ii = id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" (id.SeqeuenceOfLevel + 1)
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
    let internalItem = lm.uper.InternalItem_bit_str p.arg.p i  errCode.errCodeName codec 
    let iVar = SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)

    let funcBodyContent, localVariables = 
        let nStringLength =
            match isFixedSize,  codec with
            | true , _    -> []
            | false, Encode -> []
            | false, Decode -> [lm.lg.uper.count_var]
        
        match minSize with
        | _ when maxSize < 65536I && isFixedSize   -> bitString_FixSize p.arg.p (getAcces_c p.arg) (minSize) errCode.errCodeName codec , nStringLength
        | _ when maxSize < 65536I && (not isFixedSize)  -> bitString_VarSize p.arg.p (getAcces_c p.arg) (minSize) (maxSize) errCode.errCodeName nSizeInBits codec, nStringLength
        | _                                                -> 
            let funcBodyContent, fragmentationLvars = handleFragmentation lm p codec errCode ii uperMaxSizeInBits minSize maxSize internalItem 1I true false
            let fragmentationLvars = fragmentationLvars |> List.addIf ((not isFixedSize) &&  lm.lg.uper.requires_sBLJ) (iVar)
            (funcBodyContent,fragmentationLvars)

    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody  (errCode:ErroCode) (p:CallerScope) =
        createBitStringFunction_funcBody r lm codec t.id typeDefinition o.isFixedSize  o.uperMaxSizeInBits o.minSize.uper o.maxSize.uper (errCode:ErroCode) (p:CallerScope)

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p))  soSparkAnnotations  us


//let get

let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (child:Asn1Type) (us:State)  =
    let fixedSize       = lm.uper.seqOf_FixedSize
    let varSize         = lm.uper.seqOf_VarSize
    let typeDefinitionName = typeDefinition.longTypedefName2 lm.lg.hasModules //getTypeDefinitionName t.id.tasInfo typeDefinition
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.uper - o.minSize.uper))
    let nIntItemMaxSize = ( child.uperMaxSizeInBits)


    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        match baseFuncName with
        | None ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let lv = lm.lg.uper.seqof_lv t.id o.minSize.uper o.maxSize.uper
//                match l with 
//                | C           -> [SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)]
//                | Ada   when o.maxSize.uper >= 65536I && o.maxSize.uper=o.minSize.uper   -> []      //fixed size fragmentation does not need the i variable
//                | Ada         -> [SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)]

            let nStringLength =
                match o.minSize.uper = o.maxSize.uper,  codec with
                | true , _    -> []
                | false, Encode -> []
                | false, Decode -> [lm.lg.uper.count_var]


            let chFunc = child.getUperFunction codec
            let internalItem = 
                chFunc.funcBody ({p with arg = lm.lg.getArrayItem p.arg i child.isIA5String})



            match internalItem with
            | None  -> 
                    match o.minSize with
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper  -> None
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper -> 
                        let funcBody = varSize p.arg.p (lm.lg.getAcces p.arg)  typeDefinitionName i "" ( o.minSize.uper) ( o.maxSize.uper) nSizeInBits ( child.uperMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec
                        Some ({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = lv@nStringLength; bValIsUnReferenced=false; bBsIsUnReferenced=false})    
                    | _                                                -> 
                        let funcBody, localVariables = handleFragmentation lm p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper "" nIntItemMaxSize false false
                        Some ({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false})    
            | Some internalItem -> 
                let childErrCodes =  internalItem.errCodes
                let ret,localVariables = 
                    match o.minSize with
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper  -> fixedSize p.arg.p typeDefinitionName i internalItem.funcBody ( o.minSize.uper) ( child.uperMinSizeInBits) nIntItemMaxSize 0I codec, nStringLength 
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper  -> varSize p.arg.p (lm.lg.getAcces p.arg)  typeDefinitionName i internalItem.funcBody ( o.minSize.uper) ( o.maxSize.uper) nSizeInBits ( child.uperMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec , nStringLength
                    | _                                                -> handleFragmentation lm p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper internalItem.funcBody nIntItemMaxSize false false

                Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = lv@(localVariables@internalItem.localVariables); bValIsUnReferenced=false; bBsIsUnReferenced=false})    
        | Some baseFuncName ->
            let funcBodyContent =  callBaseTypeFunc lm (lm.lg.getPointer  p.arg) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false})
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us



let createSequenceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (us:State)  =
    // stg macros
    let sequence_presence_bit       = lm.uper.sequence_presence_bit
    let sequence_presence_bit_fix   = lm.uper.sequence_presence_bit_fix
    let sequence_mandatory_child    = lm.uper.sequence_mandatory_child
    let sequence_optional_child     = lm.uper.sequence_optional_child
    let sequence_default_child      = lm.uper.sequence_default_child
    //let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName

    let funcBody (errCode:ErroCode) (p:CallerScope) = 
//        match baseFuncName with
//        | None ->
            let nonAcnChildren = children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
            let localVariables =
                match nonAcnChildren |> Seq.exists(fun x -> x.Optionality.IsSome) with
                | true  when lm.lg.uper.requires_presenceBit  && codec = CommonTypes.Decode -> [(FlagLocalVariable ("presenceBit", None))]
                | _                                       -> []
            let printPresenceBit (child:Asn1Child) =
                match child.Optionality with
                | None                       -> None
                | Some Asn1AcnAst.AlwaysAbsent     -> Some (sequence_presence_bit_fix p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) errCode.errCodeName "0"  codec)    // please note that in decode, macro uper_sequence_presence_bit_fix
                | Some Asn1AcnAst.AlwaysPresent    -> Some (sequence_presence_bit_fix p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) errCode.errCodeName "1"  codec)    // calls macro uper_sequence_presence_bit (i.e. behaves like optional)
                | Some (Asn1AcnAst.Optional opt)   -> Some (sequence_presence_bit p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child)  errCode.errCodeName codec)
            let handleChild (child:Asn1Child) =
                let chFunc = child.Type.getUperFunction codec
                let ch_arg = lm.lg.getSeqChild p.arg
                let childContentResult = chFunc.funcBody ({p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName child) child.Type.isIA5String})
                match childContentResult with
                | None              -> None
                | Some childContent ->
                    let childBody, child_localVariables = 
                        match child.Optionality with
                        | None                       -> Some (sequence_mandatory_child (lm.lg.getAsn1ChildBackendName child) childContent.funcBody codec) , childContent.localVariables
                        | Some Asn1AcnAst.AlwaysAbsent     -> 
                            match codec with 
                            | CommonTypes.Encode -> None, []                        
                            | CommonTypes.Decode -> Some (sequence_optional_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody codec) , childContent.localVariables
                        | Some Asn1AcnAst.AlwaysPresent    -> 
                            match codec with 
                            | CommonTypes.Encode -> Some childContent.funcBody, childContent.localVariables  
                            | CommonTypes.Decode -> Some (sequence_optional_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody codec), childContent.localVariables
                        | Some (Asn1AcnAst.Optional opt)   -> 
                            match opt.defaultValue with
                            | None                   -> Some (sequence_optional_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody codec), childContent.localVariables
                            | Some v                 -> 
                                let defInit= child.Type.initFunction.initByAsn1Value ({p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName child) child.Type.isIA5String}) (mapValue v).kind
                                Some (sequence_default_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody defInit codec), childContent.localVariables 
                    Some (childBody, child_localVariables, childContent.errCodes)
            
            let presenseBits = nonAcnChildren |> List.choose printPresenceBit
            let childrenStatements0 = nonAcnChildren |> List.choose handleChild
            let childrenStatements = childrenStatements0 |> List.choose(fun (s,_,_) -> s)
            let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_) -> s)
            let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s) -> s)
            let seqContent =  (presenseBits@childrenStatements) |> nestChildItems lm codec 
            match seqContent with
            | None  -> None
            | Some ret -> Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced=false; bBsIsUnReferenced=false})    
//        | Some baseFuncName ->
//            let funcBodyContent = callBaseTypeFunc l (p.arg.getPointer l) baseFuncName codec
//            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []})
            
    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
    createUperFunction r lm codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  us



let createChoiceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (us:State)  =
    // stg macros
    let choice_child       = lm.uper.choice_child
    let choice             = lm.uper.choice

    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    let sChoiceIndexName = (lm.lg.getLongTypedefName typeDefinition) + "_index_tmp"
    let localVariables =
        match codec with
        | CommonTypes.Encode  -> []
        | CommonTypes.Decode  -> [(Asn1SIntLocalVariable (sChoiceIndexName, None))]

    let typeDefinitionName = typeDefinition.longTypedefName2 lm.lg.hasModules //getTypeDefinitionName t.id.tasInfo typeDefinition

    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let td0 = lm.lg.getChoiceTypeDefinition o.typeDef
        let td = td0.longTypedefName2 lm.lg.hasModules (ToC p.modName)
        match baseFuncName with
        | None ->
            let nIndexSizeInBits = (GetNumberOfBitsForNonNegativeInteger (BigInteger (children.Length - 1)))
            let childrenContent3 =
                children |> 
                List.mapi(fun i child ->
                    let chFunc = child.chType.getUperFunction codec
                    let uperChildRes = 
                        match lm.lg.uper.catd with
                        | false   -> chFunc.funcBody ({p with arg =  lm.lg.getChChild p.arg (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String})
                        | true when codec = CommonTypes.Decode -> chFunc.funcBody ({p with arg = VALUE ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")})
                        | true -> chFunc.funcBody ({p with arg = lm.lg.getChChild p.arg  (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String})
                    let sChildName = (lm.lg.getAsn1ChChildBackendName child)
                    let sChildTypeDef = child.chType.typeDefintionOrReference.longTypedefName2 lm.lg.hasModules //child.chType.typeDefinition.typeDefinitionBodyWithinSeq
                    let sCHildInitExpr = child.chType.initFunction.initExpression
                    let sChoiceTypeName = typeDefinitionName
                    match uperChildRes with
                    | None              -> 
                        let childContent = 
                            match lm.lg.uper.catd with 
                            | false -> lm.lg.createSingleLineComment "no encoding/decoding is required" 
                            | true when codec=Decode -> 
                                let childp = ({p with arg = VALUE ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")})
                                match child.chType.ActualType.Kind with
                                | NullType _    -> uper_a.decode_nullType childp.arg.p
                                | Sequence _    -> uper_a.decode_empty_sequence_emptySeq childp.arg.p
                                | _             -> lm.lg.createSingleLineComment "no encoding/decoding is required"
                            | true   -> lm.lg.createSingleLineComment "no encoding/decoding is required"
                        choice_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.presentWhenName (Some typeDefinition) child) (BigInteger i) nIndexSizeInBits (BigInteger (children.Length - 1)) childContent sChildName sChildTypeDef sChoiceTypeName sCHildInitExpr codec,[],[]
                    | Some childContent ->  
                        choice_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.presentWhenName (Some typeDefinition) child) (BigInteger i) nIndexSizeInBits (BigInteger (children.Length - 1)) childContent.funcBody sChildName sChildTypeDef sChoiceTypeName sCHildInitExpr codec, childContent.localVariables, childContent.errCodes )
            let childrenContent = childrenContent3 |> List.map(fun (s,_,_) -> s)
            let childrenLocalvars = childrenContent3 |> List.collect(fun (_,s,_) -> s)
            let childrenErrCodes = childrenContent3 |> List.collect(fun (_,_,s) -> s)
            
            let ret = choice p.arg.p (lm.lg.getAcces p.arg) childrenContent (BigInteger (children.Length - 1)) sChoiceIndexName errCode.errCodeName td nIndexSizeInBits  codec
            Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced=false; bBsIsUnReferenced=false})
        | Some baseFuncName ->
            let funcBodyContent = callBaseTypeFunc lm (lm.lg.getPointer p.arg) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false})

    let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)

    createUperFunction r lm codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us


    (*
let getFuncName (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId :ReferenceToType) (td:FE_TypeDefinition) =
match typeId.tasInfo with
| None -> None
| Some _ -> Some (td.typeName + codec.suffix)
    
    *)

let createReferenceFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
(*
    let moduleName, typeDefinitionName0 = 
        let t1 = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        let typeDef = lm.lg.getTypeDefinition t1.FT_TypeDefintion
        typeDef.programUnit, typeDef.typeName
    let baseTypeDefinitionName = 
        match lm.lg.hasModules with
        | false     -> typeDefinitionName0 
        | true   -> 
            match t.id.ModName = o.modName.Value with
            | true  -> typeDefinitionName0 
            | false -> moduleName + "." + typeDefinitionName0 
    let baseFncName = baseTypeDefinitionName + codec.suffix
*)

    let baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t.id "" codec

    match o.encodingOptions with 
    | None          -> 
        let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        let t1WithExtensios = o.resolvedType;
        match TypesEquivalence.uperEquivalence t1 t1WithExtensios with
        | true  ->
            let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
            let funcBody (errCode:ErroCode) (p:CallerScope) = 
                match (baseType.getUperFunction codec).funcBody p with
                | Some _    -> 
                    let funcBodyContent = callBaseTypeFunc lm (lm.lg.getParamValue t p.arg codec) baseFncName codec
                    Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
                | None      -> None
            createUperFunction r lm codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  us
        | false -> 
            baseType.getUperFunction codec, us
    | Some opts  -> 
        let octet_string_containing_func  = lm.uper.octet_string_containing_func
        let bit_string_containing_func  = lm.uper.bit_string_containing_func
        let soSparkAnnotations = Some(sparkAnnotations lm (lm.lg.getLongTypedefName typeDefinition) codec)
        let funcBody (errCode:ErroCode) (p:CallerScope) = 
            match (baseType.getUperFunction codec).funcBody p with
            | Some _    -> 
                let nBits = GetNumberOfBitsForNonNegativeInteger (opts.maxSize.uper - opts.minSize.uper)
                let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ENCODING" baseTypeDefinitionName
                let sReqBitForUperEncoding = sprintf "%s_REQUIRED_BITS_FOR_ENCODING" baseTypeDefinitionName
                let funcBodyContent = 
                    match opts.octOrBitStr with
                    | ContainedInOctString  -> octet_string_containing_func  (lm.lg.getParamValue t p.arg codec) baseFncName sReqBytesForUperEncoding nBits opts.minSize.uper opts.maxSize.uper codec
                    | ContainedInBitString  -> bit_string_containing_func  (lm.lg.getParamValue t p.arg codec) baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding nBits opts.minSize.uper opts.maxSize.uper codec
                Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
            | None      -> None
        createUperFunction r lm codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  us
        
