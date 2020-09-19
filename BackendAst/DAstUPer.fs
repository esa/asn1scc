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

let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId :ReferenceToType) (td:FE_TypeDefinition) =
    match typeId.tasInfo with
    | None -> None
    | Some _ -> Some (td.typeName + codec.suffix)


let callBaseTypeFunc l = match l with C -> uper_c.call_base_type_func | Ada -> uper_a.call_base_type_func

let sparkAnnotations l  = match l with C -> (fun _ _ -> "")    | Ada -> uper_a.sparkAnnotations

let nestChildItems (l:ProgrammingLanguage) (codec:CommonTypes.Codec) children = DAstUtilFunctions.nestItems l "result" children
//    let printChild (content:string) (sNestedContent:string option) = 
//        match sNestedContent with
//        | None  -> content
//        | Some c-> 
//            match l with
//            | C        -> equal_c.JoinItems content sNestedContent
//            | Ada      -> uper_a.JoinItems content sNestedContent
//    let rec printChildren children : Option<string> = 
//        match children with
//        |[]     -> None
//        |x::xs  -> 
//            match printChildren xs with
//            | None                 -> Some (printChild x  None)
//            | Some childrenCont    -> Some (printChild x  (Some childrenCont))
//    printChildren children

//TODO
//1.Decode functions (and perhaps encode function) muct check if the decode value is within the constraints (currently, implemented only for Integers and for case IntUnconstraintMax )
//2.Fragmentation


let internal createUperFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option)  (funcBody_e:ErroCode->CallerScope -> (UPERFuncBodyResult option)) soSparkAnnotations (us:State)  =
    let funcName            = getFuncName r l codec t.id (t.FT_TypeDefintion.[l])
    let errCodeName         = ToC ("ERR_UPER" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let EmitTypeAssignment = match l with C -> uper_c.EmitTypeAssignment    | Ada -> uper_a.EmitTypeAssignment
    let EmitTypeAssignment_def = match l with C -> uper_c.EmitTypeAssignment_def    | Ada -> uper_a.EmitTypeAssignment_def
    let EmitTypeAssignment_def_err_code  = match l with C -> uper_c.EmitTypeAssignment_def_err_code    | Ada -> uper_a.EmitTypeAssignment_def_err_code

    let funcBody = (funcBody_e errCode)
    let p  = t.getParamType l codec
    let topLevAcc = p.arg.getAcces l
    let varName = p.arg.p
    let sStar = p.arg.getStar l
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
                        let emtyStatement = match l with C -> "" | Ada -> "null;"
                        emtyStatement, [], [], true, isValidFuncName.IsNone
                    | Some bodyResult   -> bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsUnReferenced, bodyResult.bValIsUnReferenced
                let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                let func = Some(EmitTypeAssignment varName sStar funcName isValidFuncName  (typeDefinition.longTypedefName l) lvars  bodyResult_funcBody soSparkAnnotations sInitilialExp (t.uperMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced codec)
                
                //let errCodes = bodyResult.errCodes
                let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
                let funcDef = Some(EmitTypeAssignment_def varName sStar funcName  (typeDefinition.longTypedefName l) errCodStr (t.uperMaxSizeInBits = 0I) (BigInteger (ceil ((double t.uperMaxSizeInBits)/8.0))) ( t.uperMaxSizeInBits) soSparkAnnotations (t.uperMaxSizeInBits = 0I) codec)
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


let getIntfuncBodyByCons (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) uperRange errLoc isUnsigned (cons: IntegerTypeConstraint list) (allCons: IntegerTypeConstraint list) (errCode:ErroCode) (p:CallerScope) = 
    let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
    let IntNoneRequired         = match l with C -> uper_c.IntNoneRequired          | Ada -> (fun p min   errCode codec -> if min >= 0I then  (uper_a.IntFullyConstraintPos p min min 0I errCode codec) else (uper_a.IntFullyConstraint p min min 0I errCode codec))
    let IntFullyConstraintPos   = match l with C -> uper_c.IntFullyConstraintPos    | Ada -> uper_a.IntFullyConstraintPos
    let IntFullyConstraint      = match l with C -> uper_c.IntFullyConstraint       | Ada -> uper_a.IntFullyConstraint
    let IntSemiConstraintPos    = match l with C -> uper_c.IntSemiConstraintPos     | Ada -> uper_a.IntSemiConstraintPos
    let IntSemiConstraint       = match l with C -> uper_c.IntSemiConstraint        | Ada -> uper_a.IntSemiConstraint
    let IntUnconstraint         = match l with C -> uper_c.IntUnconstraint          | Ada -> uper_a.IntUnconstraint
    let IntUnconstraintMax      = match l with C -> uper_c.IntUnconstraintMax       | Ada -> uper_a.IntUnconstraintMax
    let IntRootExt              = match l with C -> uper_c.IntRootExt               | Ada -> uper_a.IntRootExt
    let IntRootExt2             = match l with C -> uper_c.IntRootExt2              | Ada -> uper_a.IntRootExt2
    let rootCons = cons |> List.choose(fun x -> match x with RangeRootConstraint(a) |RangeRootConstraint2(a,_) -> Some(x) |_ -> None) 

    let checkExp = 
        //match (DastValidate2.createIntegerFunctionByCons r l isUnsigned allCons) with
        //| None  ->  None
        //| Some expFunc -> Some (expFunc p)
        None

    let IntBod uperRange extCon =
        match uperRange with
        | Concrete(min, max) when min=max                    -> IntNoneRequired (p.arg.getValue l) min   errCode.errCodeName codec, false, (match l with C -> true | Ada -> false)
        | Concrete(min, max) when min>=0I && (not extCon)    -> IntFullyConstraintPos pp min max (GetNumberOfBitsForNonNegativeInteger (max-min))  errCode.errCodeName codec, false, false
        | Concrete(min, max)                                 -> IntFullyConstraint pp min max (GetNumberOfBitsForNonNegativeInteger (max-min))  errCode.errCodeName codec, false, false
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
        | (RangeRootConstraint a)::rest      -> 
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            let cc,_ = DastValidate2.integerConstraint2ValidationCodeBlock r l isUnsigned a 0
            let cc = DastValidate2.ValidationBlockAsStringExpr (cc p) 
            //let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) p a
            let rootBody, _,_ = IntBod uperR true
            IntRootExt pp (getValueByConstraint uperR) cc rootBody errCode.errCodeName codec, false, false
        | (RangeRootConstraint2(a,_))::rest  -> 
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            //let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) p a
            let cc,_ = DastValidate2.integerConstraint2ValidationCodeBlock r l isUnsigned a 0
            let cc = DastValidate2.ValidationBlockAsStringExpr (cc p) 
            let rootBody, _,_ = IntBod uperR true
            IntRootExt2 pp (getValueByConstraint uperR) cc rootBody errCode.errCodeName codec, false, false 
        | _                             -> raise(BugErrorException "")
    Some({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=bValIsUnReferenced; bBsIsUnReferenced=bBsIsUnReferenced})
    




let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        getIntfuncBodyByCons r l codec o.uperRange t.Location o.isUnsigned o.cons o.AllCons errCode p
    let soSparkAnnotations = Some(sparkAnnotations l (typeDefinition.longTypedefName l) codec)
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> funcBody e p) soSparkAnnotations us


let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let Boolean         = match l with C -> uper_c.Boolean          | Ada -> uper_a.Boolean
        let funcBodyContent = Boolean pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = Some(sparkAnnotations l (typeDefinition.longTypedefName l) codec)
        
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let Real         = match l with C -> uper_c.Real          | Ada -> uper_a.Real
        let funcBodyContent = Real pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = Some(sparkAnnotations l (typeDefinition.longTypedefName l) codec)
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getPointer l | CommonTypes.Decode -> p.arg.getPointer l
        let ObjectIdentifier         = 
            if o.relativeObjectId then
                match l with C -> uper_c.RelativeOID          | Ada -> uper_a.RelativeOID
            else
                match l with C -> uper_c.ObjectIdentifier          | Ada -> uper_a.ObjectIdentifier
        let funcBodyContent = ObjectIdentifier pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let getTimeSubTypeByClass (tc) =
    match tc with
    |Asn1LocalTime                      _ -> "Asn1LocalTime"
    |Asn1UtcTime                        _ -> "Asn1UtcTime"
    |Asn1LocalTimeWithTimeZone          _ -> "Asn1TimeWithTimeZone"
    |Asn1Date                             -> "Asn1Date"
    |Asn1Date_LocalTime                 _ -> "Asn1DateLocalTime"
    |Asn1Date_UtcTime                   _ -> "Asn1DateUtcTime"
    |Asn1Date_LocalTimeWithTimeZone     _ -> "Asn1DateTimeWithTimeZone"


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getPointer l | CommonTypes.Decode -> p.arg.getPointer l
        let TimeType         =  match l with C -> uper_c.Time          | Ada -> uper_a.Time
        let funcBodyContent = TimeType pp (getTimeSubTypeByClass o.timeClass) errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us



let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let soSparkAnnotations = None
    let funcBody (errCode:ErroCode) (p:CallerScope) = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations us


let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let Enumerated         = match l with C -> uper_c.Enumerated                | Ada -> uper_a.Enumerated
        let Enumerated_item    = match l with C -> uper_c.Enumerated_item           | Ada -> uper_a.Enumerated_item
        let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
        let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
        let nMin = 0I
        let nMax = BigInteger(Seq.length o.items) - 1I
        let nLastItemIndex      = nMax
        let items = 
            o.items |> List.mapi(fun i itm -> Enumerated_item (p.arg.getValue l) (itm.getBackendName (Some typeDefinition) l) (BigInteger i) nLastItemIndex codec) 
        let nBits = (GetNumberOfBitsForNonNegativeInteger (nMax-nMin))
        let sFirstItemName = o.items.Head.getBackendName (Some typeDefinition) l
        let funcBodyContent = Enumerated (p.arg.getValue l) td items nMin nMax nBits errCode.errCodeName nLastItemIndex sFirstItemName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us


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


let handleFixedSizeFragmentation (l:ProgrammingLanguage) (p:CallerScope) (codec:CommonTypes.Codec) (errCode:ErroCode) ii uperMaxSizeInBits (fixSize:BigInteger) internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString=
    let fixedSize_Fragmentation_sqf_64K          = match l with C -> uper_c.FixedSize_Fragmentation_sqf_64K       | Ada -> uper_a.FixedSize_Fragmentation_sqf_64K
    let fixedSize_Fragmentation_sqf_small_block  = match l with C -> uper_c.FixedSize_Fragmentation_sqf_small_block       | Ada -> uper_a.FixedSize_Fragmentation_sqf_small_block
    let fixedSize_Fragmentation_sqf_remaining  = match l with C -> uper_c.FixedSize_Fragmentation_sqf_remaining       | Ada -> uper_a.FixedSize_Fragmentation_sqf_remaining
    let fixedSize_Fragmentation_sqf            = match l with C -> uper_c.FixedSize_Fragmentation_sqf       | Ada -> uper_a.FixedSize_Fragmentation_sqf  
    let sRemainingItemsVar = sprintf "%s%d" "nRemainingItemsVar" ii
    let sCurBlockSize      = sprintf "%s%d" "nCurBlockSize" ii
    let sBlockIndex        = sprintf "%s%d" "nBlockIndex" ii
    let sCurOffset         = sprintf "%s%d" "nCurOffset" ii
    let sBLI               = sprintf "i%d" ii
    //let lv = SequenceOfIndex (ii, None)
    let r = FragmentationParts fixSize
    //let nBlocks64K = fixSize / C64K
    let parts =
        let part = fixedSize_Fragmentation_sqf_64K p.arg.p (p.arg.getAcces l) sCurOffset sCurBlockSize sBlockIndex r.nBlocks64K internalItem_funcBody sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
        [part]
    let smallBlockParts = 
        [(r.has48KBlock, l.toHex 195, C48K);(r.has32KBlock, l.toHex 194, C32K);(r.has16KBlock, l.toHex 193, C16K)] |>  //0xC3, 0xC2, 0xC1
        List.filter (fun (a,_,_) -> a) |>
        List.map (fun (_, sBlockId, nBlockSize) -> fixedSize_Fragmentation_sqf_small_block p.arg.p (p.arg.getAcces l) internalItem_funcBody nBlockSize sBlockId sCurOffset sCurBlockSize sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec)
    let parts = parts@smallBlockParts

    //let nRemainingItemsVar1 = fixSize % C64K
    //let has48KBlock = nRemainingItemsVar1 >= C48K
//    let parts = 
//        match r.has48KBlock with
//        | true  -> 
//            let sBlockId           = if l = C then "0xC3" else "16#C3#"
//            let part = fixedSize_Fragmentation_sqf_small_block p.arg.p (p.arg.getAcces l) internalItem_funcBody C48K sBlockId sCurOffset sCurBlockSize sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
//            parts@[part]
//        | false  -> parts
//
//    //let nRemainingItemsVar2 = if has48KBlock then (nRemainingItemsVar1 - C48K) else nRemainingItemsVar1
//    //let has32KBlock = nRemainingItemsVar2 >= C32K
//    let parts = 
//        match r.has32KBlock with
//        | true  -> 
//            let sBlockId           = if l = C then "0xC2" else "16#C2#"
//            let part = fixedSize_Fragmentation_sqf_small_block p.arg.p (p.arg.getAcces l) internalItem_funcBody C32K sBlockId sCurOffset sCurBlockSize sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
//            parts@[part]
//        | false  -> parts
//
//
//    //let nRemainingItemsVar3 = if has32KBlock then (nRemainingItemsVar2 - C32K) else nRemainingItemsVar2
//    //let has16KBlock = nRemainingItemsVar3 >= C16K
//    let parts = 
//        match r.has16KBlock with
//        | true  -> 
//            let sBlockId           = if l = C then "0xC1" else "16#C1#"
//            let part = fixedSize_Fragmentation_sqf_small_block p.arg.p (p.arg.getAcces l) internalItem_funcBody C16K sBlockId sCurOffset sCurBlockSize sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
//            parts@[part]
//        | false  -> parts
//

    //let nRemainingItemsVar = if has16KBlock then (nRemainingItemsVar3 - C16K) else nRemainingItemsVar3
    let bRemainingItemsWithinByte = r.nRemainingItemsVar <= C_127
    let parts=
        match r.nRemainingItemsVar > 0I with
        | true  ->
            let part = fixedSize_Fragmentation_sqf_remaining p.arg.p (p.arg.getAcces l) internalItem_funcBody bRemainingItemsWithinByte r.nRemainingItemsVar sCurOffset sBLI sRemainingItemsVar bIsBitStringType errCode.errCodeName codec
            parts@[part]
        | false -> parts

    let createLv name =
        match l with Ada -> IntegerLocalVariable(name ,None) | C -> Asn1SIntLocalVariable(name,None)

    let fragmentationVars = [createLv sCurBlockSize; createLv sCurOffset]
    let fragmentationVars = fragmentationVars |> List.addIf (codec = Decode) (createLv sRemainingItemsVar)
    let fragmentationVars = fragmentationVars |> List.addIf (l = C) (createLv sBlockIndex)
    //let fragmentationVars = fragmentationVars |> List.addIf (l = C) (lv)
    let singleNestedPart  = nestChildItems l codec parts |> Option.toList
    fixedSize_Fragmentation_sqf singleNestedPart  codec, fragmentationVars

let handleFragmentation (l:ProgrammingLanguage) (p:CallerScope) (codec:CommonTypes.Codec) (errCode:ErroCode) ii uperMaxSizeInBits (minSize:BigInteger) (maxSize:BigInteger) internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString=
    match minSize = maxSize with
    | true ->
        handleFixedSizeFragmentation l p codec errCode ii uperMaxSizeInBits minSize internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString
    | false ->
        let fragmentation   = match l with C -> uper_c.Fragmentation_sqf       | Ada -> uper_a.Fragmentation_sqf
        let sRemainingItemsVar = sprintf "%s%d" "nRemainingItemsVar" ii
        let sCurBlockSize      = sprintf "%s%d" "nCurBlockSize" ii
        let sBlockIndex        = sprintf "%s%d" "nBlockIndex" ii
        let sCurOffset         = sprintf "%s%d" "nCurOffset" ii
        let sBLJ               = sprintf "%s%d" "nBLJ" ii
        let sLengthTmp         = sprintf "%s%d" "nLengthTmp" ii
        let sBLI               = sprintf "i%d" ii
        //let lv = SequenceOfIndex (ii, None)

        let createLv name =
            match l with Ada -> IntegerLocalVariable(name ,None) | C -> Asn1SIntLocalVariable(name,None)

        let fragmentationVars = [createLv sRemainingItemsVar; createLv sCurBlockSize; createLv sCurOffset ]

        let fragmentationVars = fragmentationVars |> List.addIf (codec = Encode && l = Ada) (createLv sBLJ)
        let fragmentationVars = fragmentationVars |> List.addIf (codec = Encode) (createLv sBlockIndex)
        let fragmentationVars = fragmentationVars |> List.addIf (codec = Decode && minSize <> maxSize) (createLv sLengthTmp)
        fragmentation p.arg.p (p.arg.getAcces l) internalItem_funcBody  nIntItemMaxSize ( minSize) ( maxSize) uperMaxSizeInBits (minSize <> maxSize) errCode.errCodeName sRemainingItemsVar sCurBlockSize sBlockIndex sCurOffset sBLJ sBLI sLengthTmp bIsBitStringType bIsAsciiString codec, fragmentationVars

let createIA5StringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference)   (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let ii = t.id.SeqeuenceOfLevel + 1
    let i = sprintf "i%d" ii
    let lv = SequenceOfIndex (ii, None)
    let charIndex =
        match l with
        | C     -> []
        | Ada   -> [IntegerLocalVariable ("charIndex", None)]
    let nStringLength =
        match o.minSize.uper = o.maxSize.uper with
        | true  -> []
        | false ->
            match l with
            | Ada  -> [IntegerLocalVariable ("nStringLength", None)]
            | C    -> [Asn1SIntLocalVariable ("nStringLength", None)]
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
        let InternalItem_string_no_alpha = match l with C -> uper_c.InternalItem_string_no_alpha        | Ada -> uper_a.InternalItem_string_no_alpha
        let InternalItem_string_with_alpha = match l with C -> uper_c.InternalItem_string_with_alpha        | Ada -> uper_a.InternalItem_string_with_alpha
        let str_FixedSize       = match l with C -> uper_c.str_FixedSize        | Ada -> uper_a.str_FixedSize
        let str_VarSize         = match l with C -> uper_c.str_VarSize          | Ada -> uper_a.str_VarSize
        //let Fragmentation_sqf   = match l with C -> uper_c.Fragmentation_sqf    | Ada -> uper_a.Fragmentation_sqf
        let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition

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
                let funcBodyContent,localVariables = handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper internalItem nBits false true
                let localVariables = localVariables |> List.addIf (l = C || o.maxSize.uper<>o.minSize.uper) (lv)
                funcBodyContent, charIndex@localVariables

        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
//    let soSparkAnnotations = 
//        match l with
//        | C     -> None
//        | Ada   ->
//            Some(uper_a.annotations (typeDefinition.longTypedefName l) true isValidFunc.IsSome true true codec)
    let soSparkAnnotations = Some(sparkAnnotations l (typeDefinition.longTypedefName l) codec)
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations  us


let createOctetStringFunction_funcBody (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    let ii = id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" ii
    let lv = SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)

    let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition

    let InternalItem_oct_str = match l with C -> uper_c.InternalItem_oct_str        | Ada -> uper_a.InternalItem_oct_str
    let fixedSize       = match l with C -> uper_c.octect_FixedSize        | Ada -> uper_a.octect_FixedSize
    let varSize         = match l with C -> uper_c.octect_VarSize          | Ada -> uper_a.octect_VarSize
    let fragmentation   = match l with C -> uper_a.Fragmentation_sqf       | Ada -> uper_a.Fragmentation_sqf

    let nIntItemMaxSize = 8I
    let internalItem = InternalItem_oct_str p.arg.p (p.arg.getAcces l) i  errCode.errCodeName codec 
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
    let funcBodyContent, localVariables = 
        let nStringLength =
            match isFixedSize,  l, codec with
            | true , _,_    -> []
            | false, Ada, Encode -> []
            | false, Ada, Decode -> [IntegerLocalVariable ("nStringLength", None)]
            | false, C, Encode -> []
            | false, C, Decode -> [Asn1SIntLocalVariable ("nCount", None)]

        match minSize with
        | _ when maxSize < 65536I && isFixedSize  ->  fixedSize p.arg.p typeDefinitionName i internalItem ( minSize) nIntItemMaxSize nIntItemMaxSize 0I codec , lv::nStringLength
        | _ when maxSize < 65536I && (not isFixedSize)  -> varSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i internalItem ( minSize) ( maxSize) nSizeInBits nIntItemMaxSize nIntItemMaxSize 0I errCode.errCodeName codec , lv::nStringLength
        | _                                                -> 
            let funcBodyContent,localVariables = handleFragmentation l p codec errCode ii ( uperMaxSizeInBits) minSize maxSize internalItem nIntItemMaxSize false false
            let localVariables = localVariables |> List.addIf (l = C || (not isFixedSize)) (lv)
            funcBodyContent, localVariables

    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody (errCode:ErroCode) (p:CallerScope) =
        createOctetStringFunction_funcBody r l codec t.id  typeDefinition o.isFixedSize  o.uperMaxSizeInBits o.minSize.uper o.maxSize.uper (errCode:ErroCode) (p:CallerScope) 

    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   ->
            Some(uper_a.annotations (typeDefinition.longTypedefName l) true isValidFunc.IsSome true true codec)
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody  e p)) soSparkAnnotations  us


let createBitStringFunction_funcBody (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    let ii = id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" (id.SeqeuenceOfLevel + 1)
    let funcBodyContent, localVariables = 
        match l with
        | Ada ->
            let nStringLength = 
                match isFixedSize with  
                | true  -> [] 
                | false -> 
                    match codec with
                    | Encode    -> []
                    | Decode    -> [IntegerLocalVariable ("nStringLength", None)]
            let iVar = SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)

            let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
            let nBits = 1I
            let internalItem = uper_a.InternalItem_bit_str p.arg.p i  errCode.errCodeName codec 
            let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
            match minSize with
            | _ when maxSize < 65536I && isFixedSize  -> uper_a.octect_FixedSize p.arg.p typeDefinitionName i internalItem (minSize) nBits nBits 0I codec, iVar::nStringLength 
            | _ when maxSize < 65536I && (not isFixedSize) -> uper_a.octect_VarSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i internalItem ( minSize) (maxSize) nSizeInBits nBits nBits 0I errCode.errCodeName codec , iVar::nStringLength
            | _                                                -> 
                let funcBodyContent, fragmentationLvars = handleFragmentation l p codec errCode ii (uperMaxSizeInBits) minSize maxSize internalItem nBits true false
                let fragmentationLvars = fragmentationLvars |> List.addIf (not isFixedSize) (iVar)
                (funcBodyContent,fragmentationLvars)

        | C ->
            let nStringLength =
                match isFixedSize,  codec with
                | true , _    -> []
                | false, Encode -> []
                | false, Decode -> [Asn1SIntLocalVariable ("nCount", None)]

            match minSize with
            | _ when maxSize < 65536I && isFixedSize   -> uper_c.bitString_FixSize p.arg.p (p.arg.getAcces l) (minSize) errCode.errCodeName codec , nStringLength
            | _ when maxSize < 65536I && (not isFixedSize)  -> uper_c.bitString_VarSize p.arg.p (p.arg.getAcces l) (minSize) (maxSize) errCode.errCodeName codec, nStringLength
            | _                                                -> 
                handleFragmentation l p codec errCode ii (uperMaxSizeInBits) minSize maxSize "" 1I true false
    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =

    let funcBody  (errCode:ErroCode) (p:CallerScope) =
        createBitStringFunction_funcBody r l codec t.id typeDefinition o.isFixedSize  o.uperMaxSizeInBits o.minSize.uper o.maxSize.uper (errCode:ErroCode) (p:CallerScope)

    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> Some(uper_a.annotations (typeDefinition.longTypedefName l) true isValidFunc.IsSome true true codec)
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p))  soSparkAnnotations  us




let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (child:Asn1Type) (us:State)  =
    let fixedSize       = match l with C -> uper_c.octect_FixedSize        | Ada -> uper_a.octect_FixedSize
    let varSize         = match l with C -> uper_c.octect_VarSize          | Ada -> uper_a.octect_VarSize
    let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.uper - o.minSize.uper))
    let nIntItemMaxSize = ( child.uperMaxSizeInBits)


    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        match baseFuncName with
        | None ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let lv = 
                match l with 
                | C           -> [SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)]
                | Ada   when o.maxSize.uper >= 65536I && o.maxSize.uper=o.minSize.uper   -> []      //fixed size fragmentation does not need the i variable
                | Ada         -> [SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)]


            let nStringLength =
                match o.minSize.uper = o.maxSize.uper,  l, codec with
                | true , _,_    -> []
                | false, Ada, Encode -> []
                | false, Ada, Decode -> [IntegerLocalVariable ("nStringLength", None)]
                | false, C, Encode -> []
                | false, C, Decode -> [Asn1SIntLocalVariable ("nCount", None)]

            let chFunc = child.getUperFunction codec
            let internalItem = 
                chFunc.funcBody ({p with arg = p.arg.getArrayItem l i child.isIA5String})



            match internalItem with
            | None  -> 
                    match o.minSize with
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper  -> None
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper -> 
                        let funcBody = varSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i "" ( o.minSize.uper) ( o.maxSize.uper) nSizeInBits ( child.uperMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec
                        Some ({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = lv@nStringLength; bValIsUnReferenced=false; bBsIsUnReferenced=false})    
                    | _                                                -> 
                        let funcBody, localVariables = handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper "" nIntItemMaxSize false false
                        Some ({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false})    
            | Some internalItem -> 
                let childErrCodes =  internalItem.errCodes
                let ret,localVariables = 
                    match o.minSize with
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper  -> fixedSize p.arg.p typeDefinitionName i internalItem.funcBody ( o.minSize.uper) ( child.uperMinSizeInBits) nIntItemMaxSize 0I codec, nStringLength 
                    | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper  -> varSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i internalItem.funcBody ( o.minSize.uper) ( o.maxSize.uper) nSizeInBits ( child.uperMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec , nStringLength
                    | _                                                -> handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper internalItem.funcBody nIntItemMaxSize false false

                Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = lv@(localVariables@internalItem.localVariables); bValIsUnReferenced=false; bBsIsUnReferenced=false})    
        | Some baseFuncName ->
            let funcBodyContent =  callBaseTypeFunc l (p.arg.getPointer l) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false})
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us



let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (us:State)  =
    // stg macros
    let sequence_presence_bit       = match l with C -> uper_c.sequence_presence_bit        | Ada -> uper_a.sequence_presence_bit
    let sequence_presence_bit_fix   = match l with C -> uper_c.sequence_presence_bit_fix    | Ada -> uper_a.sequence_presence_bit_fix
    let sequence_mandatory_child    = match l with C -> uper_c.sequence_mandatory_child     | Ada -> uper_a.sequence_mandatory_child
    let sequence_optional_child     = match l with C -> uper_c.sequence_optional_child      | Ada -> uper_a.sequence_optional_child
    let sequence_default_child      = match l with C -> uper_c.sequence_default_child       | Ada -> uper_a.sequence_default_child
    //let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName

    let funcBody (errCode:ErroCode) (p:CallerScope) = 
//        match baseFuncName with
//        | None ->
            let nonAcnChildren = children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
            let localVariables =
                match nonAcnChildren |> Seq.exists(fun x -> x.Optionality.IsSome) with
                | true  when l = C  && codec = CommonTypes.Decode -> [(FlagLocalVariable ("presenceBit", None))]
                | _                                       -> []
            let printPresenceBit (child:Asn1Child) =
                match child.Optionality with
                | None                       -> None
                | Some Asn1AcnAst.AlwaysAbsent     -> Some (sequence_presence_bit_fix p.arg.p (p.arg.getAcces l) (child.getBackendName l) errCode.errCodeName "0"  codec)    // please note that in decode, macro uper_sequence_presence_bit_fix
                | Some Asn1AcnAst.AlwaysPresent    -> Some (sequence_presence_bit_fix p.arg.p (p.arg.getAcces l) (child.getBackendName l) errCode.errCodeName "1"  codec)    // calls macro uper_sequence_presence_bit (i.e. behaves like optional)
                | Some (Asn1AcnAst.Optional opt)   -> Some (sequence_presence_bit p.arg.p (p.arg.getAcces l) (child.getBackendName l)  errCode.errCodeName codec)
            let handleChild (child:Asn1Child) =
                let chFunc = child.Type.getUperFunction codec
                let childContentResult = chFunc.funcBody ({p with arg = p.arg.getSeqChild l (child.getBackendName l) child.Type.isIA5String})
                match childContentResult with
                | None              -> None
                | Some childContent ->
                    let childBody, child_localVariables = 
                        match child.Optionality with
                        | None                       -> Some (sequence_mandatory_child (child.getBackendName l) childContent.funcBody codec) , childContent.localVariables
                        | Some Asn1AcnAst.AlwaysAbsent     -> 
                            match codec with 
                            | CommonTypes.Encode -> None, []                        
                            | CommonTypes.Decode -> Some (sequence_optional_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody codec) , childContent.localVariables
                        | Some Asn1AcnAst.AlwaysPresent    -> 
                            match codec with 
                            | CommonTypes.Encode -> Some childContent.funcBody, childContent.localVariables  
                            | CommonTypes.Decode -> Some (sequence_optional_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody codec), childContent.localVariables
                        | Some (Asn1AcnAst.Optional opt)   -> 
                            match opt.defaultValue with
                            | None                   -> Some (sequence_optional_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody codec), childContent.localVariables
                            | Some v                 -> 
                                let defInit= child.Type.initFunction.initByAsn1Value ({p with arg = p.arg.getSeqChild l (child.getBackendName l) child.Type.isIA5String}) (mapValue v).kind
                                Some (sequence_default_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody defInit codec), childContent.localVariables 
                    Some (childBody, child_localVariables, childContent.errCodes)
            
            let presenseBits = nonAcnChildren |> List.choose printPresenceBit
            let childrenStatements0 = nonAcnChildren |> List.choose handleChild
            let childrenStatements = childrenStatements0 |> List.choose(fun (s,_,_) -> s)
            let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_) -> s)
            let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s) -> s)
            let seqContent =  (presenseBits@childrenStatements) |> nestChildItems l codec 
            match seqContent with
            | None  -> None
            | Some ret -> Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced=false; bBsIsUnReferenced=false})    
//        | Some baseFuncName ->
//            let funcBodyContent = callBaseTypeFunc l (p.arg.getPointer l) baseFuncName codec
//            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []})
            
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> None
    createUperFunction r l codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  us



let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (us:State)  =
    // stg macros
    let choice_child       = match l with C -> uper_c.choice_child | Ada -> uper_a.choice_child
    let choice             = match l with C -> uper_c.choice       | Ada -> uper_a.choice

    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    let sChoiceIndexName = (typeDefinition.longTypedefName l) + "_index_tmp"
    let localVariables =
        match codec with
        | CommonTypes.Encode  -> []
        | CommonTypes.Decode  -> [(Asn1SIntLocalVariable (sChoiceIndexName, None))]

    let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition

    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
        match baseFuncName with
        | None ->
            let nIndexSizeInBits = (GetNumberOfBitsForNonNegativeInteger (BigInteger (children.Length - 1)))
            let childrenContent3 =
                children |> 
                List.mapi(fun i child ->
                    let chFunc = child.chType.getUperFunction codec
                    let uperChildRes = 
                        match l with
                        | C   -> chFunc.funcBody ({p with arg = p.arg.getChChild l (child.getBackendName l) child.chType.isIA5String})
                        | Ada when codec = CommonTypes.Decode -> chFunc.funcBody ({p with arg = VALUE ((child.getBackendName l) + "_tmp")})
                        | Ada -> chFunc.funcBody ({p with arg = p.arg.getChChild l (child.getBackendName l) child.chType.isIA5String})
                    let sChildName = (child.getBackendName l)
                    let sChildTypeDef = child.chType.typeDefintionOrReference.longTypedefName l //child.chType.typeDefinition.typeDefinitionBodyWithinSeq
                    let sChoiceTypeName = typeDefinitionName
                    match uperChildRes with
                    | None              -> 
                        let childContent = 
                            match l with 
                            | C ->"/*no encoding/decoding is required*/" 
                            | Ada when codec=Decode -> 
                                let childp = ({p with arg = VALUE ((child.getBackendName l) + "_tmp")})
                                uper_a.null_decode childp.arg.p
                            | Ada   -> "--no encoding/decoding is required"
                        choice_child p.arg.p (p.arg.getAcces l) (child.presentWhenName (Some typeDefinition) l) (BigInteger i) nIndexSizeInBits (BigInteger (children.Length - 1)) childContent sChildName sChildTypeDef sChoiceTypeName codec,[],[]
                    | Some childContent ->  
                        choice_child p.arg.p (p.arg.getAcces l) (child.presentWhenName (Some typeDefinition) l) (BigInteger i) nIndexSizeInBits (BigInteger (children.Length - 1)) childContent.funcBody sChildName sChildTypeDef sChoiceTypeName codec, childContent.localVariables, childContent.errCodes )
            let childrenContent = childrenContent3 |> List.map(fun (s,_,_) -> s)
            let childrenLocalvars = childrenContent3 |> List.collect(fun (_,s,_) -> s)
            let childrenErrCodes = childrenContent3 |> List.collect(fun (_,_,s) -> s)
            
            let ret = choice p.arg.p (p.arg.getAcces l) childrenContent (BigInteger (children.Length - 1)) sChoiceIndexName errCode.errCodeName td nIndexSizeInBits  codec
            Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced=false; bBsIsUnReferenced=false})
        | Some baseFuncName ->
            let funcBodyContent = callBaseTypeFunc l (p.arg.getPointer l) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false})

    let soSparkAnnotations = None

    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us


let createReferenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
    let baseTypeDefinitionName = 
        match l with
        | C     -> ToC2(r.args.TypePrefix + o.tasName.Value) 
        | Ada   -> 
            match t.id.ModName = o.modName.Value with
            | true  -> ToC2(r.args.TypePrefix + o.tasName.Value) 
            | false -> (ToC o.modName.Value) + "." + ToC2(r.args.TypePrefix + o.tasName.Value) 
    let baseFncName = baseTypeDefinitionName + codec.suffix

    match o.encodingOptions with 
    | None          -> 
        let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        let t1WithExtensios = o.resolvedType;
        match TypesEquivalence.uperEquivalence t1 t1WithExtensios with
        | true  ->
            let soSparkAnnotations = None
            let funcBody (errCode:ErroCode) (p:CallerScope) = 
                match (baseType.getUperFunction codec).funcBody p with
                | Some _    -> 
                    let funcBodyContent = callBaseTypeFunc l (t.getParamValue p.arg l codec) baseFncName codec
                    Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
                | None      -> None
            createUperFunction r l codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  us
        | false -> 
            baseType.getUperFunction codec, us
    | Some opts  -> 
        let octet_string_containing_func  = match l with C -> uper_c.octet_string_containing_func | Ada -> uper_a.octet_string_containing_func
        let bit_string_containing_func  = match l with C -> uper_c.bit_string_containing_func | Ada -> uper_a.bit_string_containing_func
        let soSparkAnnotations = None
        let funcBody (errCode:ErroCode) (p:CallerScope) = 
            match (baseType.getUperFunction codec).funcBody p with
            | Some _    -> 
                let nBits = GetNumberOfBitsForNonNegativeInteger (opts.maxSize.uper - opts.minSize.uper)
                let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ENCODING" baseTypeDefinitionName
                let sReqBitForUperEncoding = sprintf "%s_REQUIRED_BITS_FOR_ENCODING" baseTypeDefinitionName
                let funcBodyContent = 
                    match opts.octOrBitStr with
                    | ContainedInOctString  -> octet_string_containing_func  (t.getParamValue p.arg l codec) baseFncName sReqBytesForUperEncoding nBits opts.minSize.uper opts.maxSize.uper codec
                    | ContainedInBitString  -> bit_string_containing_func  (t.getParamValue p.arg l codec) baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding nBits opts.minSize.uper opts.maxSize.uper codec
                Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
            | None      -> None
        createUperFunction r l codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  us
        
