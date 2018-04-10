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

let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId:ReferenceToType) =
    typeId.tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + codec.suffix))

(*
let getTypeDefinitionName (tasInfo:TypeAssignmentInfo option) (typeDefinition:TypeDefintionOrReference) =
    match tasInfo with
    | Some _                -> (typeDefinition.longTypedefName l)
    | None (*inner type*)   -> typeDefinition.typeDefinitionBodyWithinSeq
*)

let callBaseTypeFunc l = match l with C -> uper_c.call_base_type_func | Ada -> uper_a.call_base_type_func



//TODO
//1.Decode functions (and perhaps encode function) muct check if the decode value is within the constraints (currently, implemented only for Integers and for case IntUnconstraintMax )
//2.Fragmentation


let internal createUperFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option)  (funcBody_e:ErroCode->CallerScope -> (UPERFuncBodyResult option)) soSparkAnnotations (us:State)  =
    let funcName            = getFuncName r l codec t.id
    let errCodeName         = ToC ("ERR_UPER" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let EmitTypeAssignment_primitive = match l with C -> uper_c.EmitTypeAssignment_primitive    | Ada -> uper_a.EmitTypeAssignment
    let EmitTypeAssignment_primitive_def = match l with C -> uper_c.EmitTypeAssignment_primitive_def    | Ada -> uper_a.EmitTypeAssignment_def
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
                let bodyResult_funcBody, errCodes,  bodyResult_localVariables = 
                    match content with 
                    | None              -> 
                        let emtyStatement = match l with C -> "" | Ada -> "null;"
                        emtyStatement, [], []
                    | Some bodyResult   -> bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables

                let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                let func = Some(EmitTypeAssignment_primitive varName sStar funcName isValidFuncName  (typeDefinition.longTypedefName l) lvars  bodyResult_funcBody soSparkAnnotations sInitilialExp codec)
                
                //let errCodes = bodyResult.errCodes
                let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
                let funcDef = Some(EmitTypeAssignment_primitive_def varName sStar funcName  (typeDefinition.longTypedefName l) errCodStr (t.uperMaxSizeInBits = 0I) (BigInteger (ceil ((double t.uperMaxSizeInBits)/8.0))) ( t.uperMaxSizeInBits) soSparkAnnotations codec)
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
        match (DAstValidate.createIntegerFunctionByCons r l isUnsigned allCons) with
        | None  ->  None
        | Some expFunc -> Some (expFunc p)

    let IntBod uperRange extCon =
        
        match uperRange with
        | Concrete(min, max) when min=max                    -> IntNoneRequired (p.arg.getValue l) min   errCode.errCodeName codec 
        | Concrete(min, max) when min>=0I && (not extCon)    -> IntFullyConstraintPos pp min max (GetNumberOfBitsForNonNegativeInteger (max-min))  errCode.errCodeName codec
        | Concrete(min, max)                                 -> IntFullyConstraint pp min max (GetNumberOfBitsForNonNegativeInteger (max-min))  errCode.errCodeName codec
        | PosInf(a)  when a>=0I && (not extCon)  -> IntSemiConstraintPos pp a  errCode.errCodeName codec
        | PosInf(a)               -> IntSemiConstraint pp a  errCode.errCodeName codec
        | NegInf(max)             -> IntUnconstraintMax pp max checkExp errCode.errCodeName codec
        | Full                    -> IntUnconstraint pp errCode.errCodeName false codec

    let getValueByConstraint uperRange =
        match uperRange with
        | Concrete(a, _)  -> a
        | PosInf(a)       -> a
        | NegInf(b)       -> b
        | Full            -> 0I
    let funcBodyContent = 
        match rootCons with
        | []                            -> IntBod uperRange false
        | (RangeRootConstraint a)::rest      -> 
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) p a
            IntRootExt pp (getValueByConstraint uperR) cc (IntBod uperR true) errCode.errCodeName codec
        | (RangeRootConstraint2(a,_))::rest  -> 
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) p a
            IntRootExt2 pp (getValueByConstraint uperR) cc (IntBod uperR true) errCode.errCodeName codec 
        | _                             -> raise(BugErrorException "")
    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    




let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        if (t.id.AsString = "TEST-CASE.DEF-NPAL-TC-header.field-packet-header.field-packet-id.field-version-number") then
            let dummy = 0
            ()
        getIntfuncBodyByCons r l codec o.uperRange t.Location o.isUnsigned o.cons o.AllCons errCode p
        (*
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let IntNoneRequired         = match l with C -> uper_c.IntNoneRequired          | Ada -> (fun p min   errCode codec -> uper_a.IntFullyConstraint p min min 0I errCode codec)
        let IntFullyConstraintPos   = match l with C -> uper_c.IntFullyConstraintPos    | Ada -> uper_a.IntFullyConstraint
        let IntFullyConstraint      = match l with C -> uper_c.IntFullyConstraint       | Ada -> uper_a.IntFullyConstraint
        let IntSemiConstraintPos    = match l with C -> uper_c.IntSemiConstraintPos     | Ada -> uper_a.IntSemiConstraint
        let IntSemiConstraint       = match l with C -> uper_c.IntSemiConstraint        | Ada -> uper_a.IntSemiConstraint
        let IntUnconstraint         = match l with C -> uper_c.IntUnconstraint          | Ada -> uper_a.IntUnconstraint
        let IntUnconstraintMax      = match l with C -> uper_c.IntUnconstraintMax       | Ada -> uper_a.IntUnconstraintMax
        let IntRootExt              = match l with C -> uper_c.IntRootExt               | Ada -> uper_a.IntRootExt
        let IntRootExt2             = match l with C -> uper_c.IntRootExt2              | Ada -> uper_a.IntRootExt2
        let rootCons = o.cons |> List.choose(fun x -> match x with RangeRootConstraint(a) |RangeRootConstraint2(a,_) -> Some(x) |_ -> None) 
        let checkExp = 
            match isValidFunc with
            | None  ->  None
            | Some fnc -> 
                match fnc.funcExp with
                | None  -> None
                | Some expFunc -> (Some (expFunc (p.getValue l)))
        let IntBod uperRange extCon =
            match uperRange with
            | Concrete(min, max) when min=max                    -> IntNoneRequired (p.getValue l) min   errCode.errCodeName codec 
            | Concrete(min, max) when min>=0I && (not extCon)    -> IntFullyConstraintPos pp min max (GetNumberOfBitsForNonNegativeInteger (max-min))  errCode.errCodeName codec
            | Concrete(min, max)                                 -> IntFullyConstraint pp min max (GetNumberOfBitsForNonNegativeInteger (max-min))  errCode.errCodeName codec
            | PosInf(a)  when a>=0I && (not extCon)  -> IntSemiConstraintPos pp a  errCode.errCodeName codec
            | PosInf(a)               -> IntSemiConstraint pp a  errCode.errCodeName codec
            | NegInf(max)             -> IntUnconstraintMax pp max checkExp errCode.errCodeName codec
            | Full                    -> IntUnconstraint pp errCode.errCodeName false codec

        let getValueByConstraint uperRange =
            match uperRange with
            | Concrete(a, _)  -> a
            | PosInf(a)       -> a
            | NegInf(b)       -> b
            | Full            -> 0I
        let funcBodyContent = 
            match rootCons with
            | []                            -> IntBod o.uperRange false
            | (RangeRootConstraint a)::rest      -> 
                let uperR    = uPER.getIntTypeConstraintUperRange [a]  t.Location
                let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) (p.getValue l) a
                IntRootExt pp (getValueByConstraint uperR) cc (IntBod uperR true) errCode.errCodeName codec
            | (RangeRootConstraint2(a,_))::rest  -> 
                let uperR    = uPER.getIntTypeConstraintUperRange [a]  t.Location
                let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) (p.getValue l) a
                IntRootExt2 pp (getValueByConstraint uperR) cc (IntBod uperR true) errCode.errCodeName codec 
            | _                             -> raise(BugErrorException "")
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
        *)
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us


let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let Boolean         = match l with C -> uper_c.Boolean          | Ada -> uper_a.Boolean
        let funcBodyContent = Boolean pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let Real         = match l with C -> uper_c.Real          | Ada -> uper_a.Real
        let funcBodyContent = Real pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> None) soSparkAnnotations us


let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let Enumerated         = match l with C -> uper_c.Enumerated                | Ada -> uper_a.Enumerated
        let Enumerated_item    = match l with C -> uper_c.Enumerated_item           | Ada -> uper_a.Enumerated_item
        let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
        let nMin = 0I
        let nMax = BigInteger(Seq.length o.items) - 1I
        let nLastItemIndex      = nMax
        let items = 
            o.items |> List.mapi(fun i itm -> Enumerated_item (p.arg.getValue l) (itm.getBackendName (Some typeDefinition) l) (BigInteger i) nLastItemIndex codec) 
        let nBits = (GetNumberOfBitsForNonNegativeInteger (nMax-nMin))
        let sFirstItemName = o.items.Head.getBackendName (Some typeDefinition) l
        let funcBodyContent = Enumerated (p.arg.getValue l) typeDefinitionName items nMin nMax nBits errCode.errCodeName nLastItemIndex sFirstItemName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us


let handleFragmentation (l:ProgrammingLanguage) (p:CallerScope) (codec:CommonTypes.Codec) (errCode:ErroCode) ii uperMaxSizeInBits (minSize:BigInteger) (maxSize:BigInteger) internalItem_funcBody nIntItemMaxSize bIsBitStringType bIsAsciiString=
    let fragmentation   = match l with C -> uper_c.Fragmentation_sqf       | Ada -> uper_a.Fragmentation_sqf
    let sRemainingItemsVar = sprintf "%s%d" "nRemainingItemsVar" ii
    let sCurBlockSize      = sprintf "%s%d" "nCurBlockSize" ii
    let sBlockIndex        = sprintf "%s%d" "nBlockIndex" ii
    let sCurOffset         = sprintf "%s%d" "nCurOffset" ii
    let sBLJ               = sprintf "%s%d" "nBLJ" ii
    let sLengthTmp         = sprintf "%s%d" "nLengthTmp" ii
    let sBLI               = sprintf "i%d" ii

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
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
    let charIndex =
        match l with
        | C     -> []
        | Ada   -> [IntegerLocalVariable ("charIndex", None)]
    let nStringLength =
        match o.minSize = o.maxSize with
        | true  -> []
        | false ->
            match l with
            | Ada  -> [IntegerLocalVariable ("nStringLength", None)]
            | C    -> [Asn1SIntLocalVariable ("nStringLength", None)]
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let InternalItem_string_no_alpha = match l with C -> uper_c.InternalItem_string_no_alpha        | Ada -> uper_a.InternalItem_string_no_alpha
        let InternalItem_string_with_alpha = match l with C -> uper_c.InternalItem_string_with_alpha        | Ada -> uper_a.InternalItem_string_with_alpha
        let str_FixedSize       = match l with C -> uper_c.str_FixedSize        | Ada -> uper_a.str_FixedSize
        let str_VarSize         = match l with C -> uper_c.str_VarSize          | Ada -> uper_a.str_VarSize
        //let Fragmentation_sqf   = match l with C -> uper_c.Fragmentation_sqf    | Ada -> uper_a.Fragmentation_sqf
        let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition

        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
        let internalItem =
            match o.uperCharSet.Length = 128 with
            | true  -> InternalItem_string_no_alpha p.arg.p i  codec 
            | false -> 
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                InternalItem_string_with_alpha p.arg.p (typeDefinition.longTypedefName l) i (BigInteger (o.uperCharSet.Length-1)) arrAsciiCodes (BigInteger (o.uperCharSet.Length)) nBits  codec
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize - o.minSize))
        let funcBodyContent,localVariables = 
            match o.minSize with
            | _ when o.maxSize < 65536I && o.maxSize=o.minSize  -> str_FixedSize p.arg.p typeDefinitionName i internalItem ( o.minSize) nBits nBits 0I codec , charIndex@nStringLength
            | _ when o.maxSize < 65536I && o.maxSize<>o.minSize  -> str_VarSize p.arg.p typeDefinitionName i internalItem ( o.minSize) ( o.maxSize) nSizeInBits nBits nBits 0I codec , charIndex@nStringLength
            | _                                                -> 
                let funcBodyContent,localVariables = handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize o.maxSize internalItem nBits false true
                funcBodyContent,charIndex@localVariables

        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = lv::localVariables}    
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   ->
            Some(uper_a.annotations (typeDefinition.longTypedefName l) true isValidFunc.IsSome true true codec)
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations  us

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let ii = t.id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" ii
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)


    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition

        let InternalItem_oct_str = match l with C -> uper_c.InternalItem_oct_str        | Ada -> uper_a.InternalItem_oct_str
        let fixedSize       = match l with C -> uper_c.octect_FixedSize        | Ada -> uper_a.octect_FixedSize
        let varSize         = match l with C -> uper_c.octect_VarSize          | Ada -> uper_a.octect_VarSize
        let fragmentation   = match l with C -> uper_a.Fragmentation_sqf       | Ada -> uper_a.Fragmentation_sqf

        let nIntItemMaxSize = 8I
        let internalItem = InternalItem_oct_str p.arg.p (p.arg.getAcces l) i  errCode.errCodeName codec 
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize - o.minSize))
        let funcBodyContent, localVariables = 
            let nStringLength =
                match o.minSize = o.maxSize,  l, codec with
                | true , _,_    -> []
                | false, Ada, _ -> [IntegerLocalVariable ("nStringLength", None)]
                | false, C, Encode -> []
                | false, C, Decode -> [Asn1SIntLocalVariable ("nCount", None)]

            match o.minSize with
            | _ when o.maxSize < 65536I && o.maxSize=o.minSize  ->  fixedSize p.arg.p typeDefinitionName i internalItem ( o.minSize) nIntItemMaxSize nIntItemMaxSize 0I codec , nStringLength
            | _ when o.maxSize < 65536I && o.maxSize<>o.minSize  -> varSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i internalItem ( o.minSize) ( o.maxSize) nSizeInBits nIntItemMaxSize nIntItemMaxSize 0I errCode.errCodeName codec , nStringLength
            | _                                                -> 
                handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize o.maxSize internalItem nIntItemMaxSize false false

        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = lv::localVariables}    
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   ->
            Some(uper_a.annotations (typeDefinition.longTypedefName l) true isValidFunc.IsSome true true codec)
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations  us

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let ii = t.id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)

    
(*
        match o.minSize <> o.maxSize && codec = Codec.Decode with
        | false  -> []
        | true ->
            match l with
            | Ada  -> 
                let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
                lv::[IntegerLocalVariable ("nStringLength", None)]
            | C    -> [Asn1SIntLocalVariable ("nCount", None)]*)
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        let funcBodyContent, localVariables = 
            match l with
            | Ada ->
                let nStringLength = match o.minSize = o.maxSize with  true  -> [] | false -> [IntegerLocalVariable ("nStringLength", None)]
                let iVar = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)

                let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
                let nBits = 1I
                let internalItem = uper_a.InternalItem_bit_str p.arg.p i  errCode.errCodeName codec 
                let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize - o.minSize))
                match o.minSize with
                | _ when o.maxSize < 65536I && o.maxSize=o.minSize  -> uper_a.octect_FixedSize p.arg.p typeDefinitionName i internalItem ( o.minSize) nBits nBits 0I codec, iVar::nStringLength 
                | _ when o.maxSize < 65536I && o.maxSize<>o.minSize -> uper_a.octect_VarSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i internalItem ( o.minSize) ( o.maxSize) nSizeInBits nBits nBits 0I errCode.errCodeName codec , iVar::nStringLength
                | _                                                -> 
                    let funcBodyContent, fragmentationLvars = handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize o.maxSize internalItem nBits true false
                    (funcBodyContent,iVar::fragmentationLvars)

            | C ->
                let nStringLength =
                    match o.minSize = o.maxSize,  codec with
                    | true , _    -> []
                    | false, Encode -> []
                    | false, Decode -> [Asn1SIntLocalVariable ("nCount", None)]

                match o.minSize with
                | _ when o.maxSize < 65536I && o.maxSize=o.minSize   -> uper_c.bitString_FixSize p.arg.p (p.arg.getAcces l) ( o.minSize) errCode.errCodeName codec , nStringLength
                | _ when o.maxSize < 65536I && o.maxSize<>o.minSize  -> uper_c.bitString_VarSize p.arg.p (p.arg.getAcces l) ( o.minSize) ( o.maxSize) errCode.errCodeName codec, nStringLength
                | _                                                -> 
                    handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize o.maxSize "" 1I true false
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables}    
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> Some(uper_a.annotations (typeDefinition.longTypedefName l) true isValidFunc.IsSome true true codec)
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p))  soSparkAnnotations  us




let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference)  (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (child:Asn1Type) (us:State)  =
    let fixedSize       = match l with C -> uper_c.octect_FixedSize        | Ada -> uper_a.octect_FixedSize
    let varSize         = match l with C -> uper_c.octect_VarSize          | Ada -> uper_a.octect_VarSize
    let typeDefinitionName = typeDefinition.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize - o.minSize))
    let nIntItemMaxSize = ( child.uperMaxSizeInBits)


    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    let funcBody (errCode:ErroCode) (p:CallerScope) = 
        match baseFuncName with
        | None ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)


            let nStringLength =
                match o.minSize = o.maxSize,  l, codec with
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
                    | _ when o.maxSize < 65536I && o.maxSize=o.minSize  -> None
                    | _ when o.maxSize < 65536I && o.maxSize<>o.minSize -> 
                        let funcBody = varSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i "" ( o.minSize) ( o.maxSize) nSizeInBits ( child.uperMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec
                        Some ({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = lv::nStringLength})    
                    | _                                                -> 
                        let funcBody, localVariables = handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize o.maxSize "" nIntItemMaxSize false false
                        Some ({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = localVariables})    
            | Some internalItem -> 
                let childErrCodes =  internalItem.errCodes
                let ret,localVariables = 
                    match o.minSize with
                    | _ when o.maxSize < 65536I && o.maxSize=o.minSize  -> fixedSize p.arg.p typeDefinitionName i internalItem.funcBody ( o.minSize) ( child.uperMinSizeInBits) nIntItemMaxSize 0I codec, nStringLength 
                    | _ when o.maxSize < 65536I && o.maxSize<>o.minSize  -> varSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i internalItem.funcBody ( o.minSize) ( o.maxSize) nSizeInBits ( child.uperMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec , nStringLength
                    | _                                                -> handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize o.maxSize internalItem.funcBody nIntItemMaxSize false false

                Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = lv::(localVariables@internalItem.localVariables)})    
        | Some baseFuncName ->
            let funcBodyContent =  callBaseTypeFunc l (p.arg.getPointer l) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []})
    let soSparkAnnotations = None
    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us


let nestChildItems (l:ProgrammingLanguage) (codec:CommonTypes.Codec) children = 
    let printChild (content:string) (sNestedContent:string option) = 
        match sNestedContent with
        | None  -> content
        | Some c-> 
            match l with
            | C        -> equal_c.JoinItems content sNestedContent
            | Ada      -> uper_a.JoinItems content sNestedContent
    let rec printChildren children : Option<string> = 
        match children with
        |[]     -> None
        |x::xs  -> 
            match printChildren xs with
            | None                 -> Some (printChild x  None)
            | Some childrenCont    -> Some (printChild x  (Some childrenCont))
    printChildren children

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
                | Some Asn1AcnAst.AlwaysAbsent     -> Some (sequence_presence_bit_fix p.arg.p (p.arg.getAcces l) child.c_name errCode.errCodeName "0"  codec)    // please note that in decode, macro uper_sequence_presence_bit_fix
                | Some Asn1AcnAst.AlwaysPresent    -> Some (sequence_presence_bit_fix p.arg.p (p.arg.getAcces l) child.c_name errCode.errCodeName "1"  codec)    // calls macro uper_sequence_presence_bit (i.e. behaves like optional)
                | Some (Asn1AcnAst.Optional opt)   -> Some (sequence_presence_bit p.arg.p (p.arg.getAcces l) child.c_name  errCode.errCodeName codec)
            let handleChild (child:Asn1Child) =
                let chFunc = child.Type.getUperFunction codec
                let childContentResult = chFunc.funcBody ({p with arg = p.arg.getSeqChild l child.c_name child.Type.isIA5String})
                match childContentResult with
                | None              -> None
                | Some childContent ->
                    let childBody = 
                        match child.Optionality with
                        | None                       -> Some (sequence_mandatory_child child.c_name childContent.funcBody codec) 
                        | Some Asn1AcnAst.AlwaysAbsent     -> match codec with CommonTypes.Encode -> None                        | CommonTypes.Decode -> Some (sequence_optional_child p.arg.p (p.arg.getAcces l) child.c_name childContent.funcBody codec) 
                        | Some Asn1AcnAst.AlwaysPresent    -> match codec with CommonTypes.Encode -> Some childContent.funcBody  | CommonTypes.Decode -> Some (sequence_optional_child p.arg.p (p.arg.getAcces l) child.c_name childContent.funcBody codec)
                        | Some (Asn1AcnAst.Optional opt)   -> 
                            match opt.defaultValue with
                            | None                   -> Some (sequence_optional_child p.arg.p (p.arg.getAcces l) child.c_name childContent.funcBody codec)
                            | Some v                 -> 
                                let defInit= child.Type.initFunction.initByAsn1Value ({p with arg = p.arg.getSeqChild l child.c_name child.Type.isIA5String}) (mapValue v).kind
                                Some (sequence_default_child p.arg.p (p.arg.getAcces l) child.c_name childContent.funcBody defInit codec) 
                    Some (childBody, childContent.localVariables, childContent.errCodes)
            
            let presenseBits = nonAcnChildren |> List.choose printPresenceBit
            let childrenStatements0 = nonAcnChildren |> List.choose handleChild
            let childrenStatements = childrenStatements0 |> List.choose(fun (s,_,_) -> s)
            let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_) -> s)
            let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s) -> s)
            let seqContent =  (presenseBits@childrenStatements) |> nestChildItems l codec 
            match seqContent with
            | None  -> None
            | Some ret -> Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars})    
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
        match baseFuncName with
        | None ->
            let nIndexSizeInBits = (GetNumberOfBitsForNonNegativeInteger (BigInteger (children.Length - 1)))
            let childrenContent3 =
                children |> 
                List.mapi(fun i child ->
                    let chFunc = child.chType.getUperFunction codec
                    let uperChildRes = 
                        match l with
                        | C   -> chFunc.funcBody ({p with arg = p.arg.getChChild l child.c_name child.chType.isIA5String})
                        | Ada when codec = CommonTypes.Decode -> chFunc.funcBody ({p with arg = VALUE (child.c_name + "_tmp")})
                        | Ada -> chFunc.funcBody ({p with arg = p.arg.getChChild l child.c_name child.chType.isIA5String})
                    match uperChildRes with
                    | None              -> 
                        let sChildName = child.c_name
                        let sChildTypeDef = child.chType.typeDefintionOrReference.longTypedefName l //child.chType.typeDefinition.typeDefinitionBodyWithinSeq
                        let sChoiceTypeName = typeDefinitionName
                        let noEncodingComment = match l with C ->"/*no encoding/decoding is required*/" | Ada -> "--no encoding/decoding is required"
                        choice_child p.arg.p (p.arg.getAcces l) (child.presentWhenName (Some typeDefinition) l) (BigInteger i) nIndexSizeInBits (BigInteger (children.Length - 1)) noEncodingComment sChildName sChildTypeDef sChoiceTypeName codec,[],[]
                    | Some childContent ->  
                        let sChildName = child.c_name
                        let sChildTypeDef = child.chType.typeDefintionOrReference.longTypedefName l //child.chType.typeDefinition.typeDefinitionBodyWithinSeq
                        let sChoiceTypeName = typeDefinitionName
                        choice_child p.arg.p (p.arg.getAcces l) (child.presentWhenName (Some typeDefinition) l) (BigInteger i) nIndexSizeInBits (BigInteger (children.Length - 1)) childContent.funcBody sChildName sChildTypeDef sChoiceTypeName codec, childContent.localVariables, childContent.errCodes )
            let childrenContent = childrenContent3 |> List.map(fun (s,_,_) -> s)
            let childrenLocalvars = childrenContent3 |> List.collect(fun (_,s,_) -> s)
            let childrenErrCodes = childrenContent3 |> List.collect(fun (_,_,s) -> s)
            
            let ret = choice p.arg.p (p.arg.getAcces l) childrenContent (BigInteger (children.Length - 1)) sChoiceIndexName errCode.errCodeName typeDefinitionName nIndexSizeInBits  codec
            Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars})
        | Some baseFuncName ->
            let funcBodyContent = callBaseTypeFunc l (p.arg.getPointer l) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []})

    let soSparkAnnotations = None

    createUperFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us

(*
let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId:ReferenceToType) =
    typeId.tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + codec.suffix))

*)

let createReferenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
    
    if (t.id.AsString.StartsWith("Onboard-Parameters.Parameter-Value")) then
        let aaa = t.id.AsString
        ()


    let typeDefinitionName = ToC2(r.args.TypePrefix + o.tasName.Value)
    let baseFncName = 
        match l with
        | C     -> typeDefinitionName + codec.suffix
        | Ada   -> 
            match t.id.ModName = o.modName.Value with
            | true  -> typeDefinitionName + codec.suffix
            | false -> (ToC o.modName.Value) + "." + typeDefinitionName + codec.suffix


    let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
    let t1WithExtensios = o.resolvedType;
    match TypesEquivalence.uperEquivalence t1 t1WithExtensios with
    | true  ->
        let soSparkAnnotations = None
        let funcBody (errCode:ErroCode) (p:CallerScope) = 
            match (baseType.getUperFunction codec).funcBody p with
            | Some _    -> 
                let funcBodyContent = callBaseTypeFunc l (t.getParamValue p.arg l codec) baseFncName codec
                Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
            | None      -> None
        createUperFunction r l codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  us
    | false -> 
        baseType.getUperFunction codec, us
