﻿module DAstUPer
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
    match l with
    | C
    | Ada       -> typeId.tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + codec.suffix))
    | Python    -> 
        match typeId.tasInfo with
        | None      -> Some ("uper_" + codec.suffix.ToLower())
        | _         -> typeId.tasInfo |> Option.map (fun x -> ToCPy(r.args.TypePrefix + x.tasName + ".uper_" + codec.suffix.ToLower()))

let getTypeDefinitionName (tasInfo:TypeAssignmentInfo option) (typeDefinition:TypeDefinitionCommon) =
    match tasInfo with
    | Some _                -> typeDefinition.name
    | None (*inner type*)   -> typeDefinition.typeDefinitionBodyWithinSeq


let callBaseTypeFunc l = match l with C -> uper_c.call_base_type_func | Ada -> uper_a.call_base_type_func | Python -> uper_p.call_base_type_func



//TODO
//1.Decode functions (and perhaps encode function) muct check if the decode value is within the constraints (currently, implemented only for Integers and for case IntUnconstraintMax )
//2.Fragmentation


let createPrimitiveFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option)  (funcBody_e:ErroCode->FuncParamType -> (UPERFuncBodyResult option)) soSparkAnnotations (us:State)  =
    let funcName            = getFuncName r l codec t.id
    let errCodeName         = ToC ("ERR_UPER" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let EmitTypeAssignment_primitive = 
        match l with 
        | C -> uper_c.EmitTypeAssignment_primitive    
        | Ada -> uper_a.EmitTypeAssignment 
        | Python -> 
            match t.Kind with
            | Asn1AcnAst.Asn1TypeKind.ReferenceType _
            | Asn1AcnAst.Asn1TypeKind.Sequence _
            | Asn1AcnAst.Asn1TypeKind.Choice _          -> uper_p.EmitTypeAssignment_primitive_no_set
            | _                                         -> uper_p.EmitTypeAssignment_primitive
    let EmitTypeAssignment_primitive_def = match l with C -> uper_c.EmitTypeAssignment_primitive_def    | Ada -> uper_a.EmitTypeAssignment_def | Python -> uper_p.EmitTypeAssignment_primitive_def
    let EmitTypeAssignment_def_err_code  = match l with C -> uper_c.EmitTypeAssignment_def_err_code    | Ada -> uper_a.EmitTypeAssignment_def_err_code | Python -> (fun _ _ -> "")

    let funcBody = (funcBody_e errCode)
    let p : FuncParamType = t.getParamType l codec
    let topLevAcc = p.getAcces l
    let varName = p.p
    let sStar = p.getStar l
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let sInitilialExp = ""
    let  func, funcDef  = 
            match funcName  with
            | None              -> None, None
            | Some funcName     -> 
                let content = funcBody p  
                match content with 
                | None          -> None, None
                | Some bodyResult  ->
                    let lvars = bodyResult.localVariables |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                    let func = Some(EmitTypeAssignment_primitive varName sStar funcName isValidFuncName  typeDefinition.name lvars  bodyResult.funcBody soSparkAnnotations sInitilialExp codec)
                    let func =
                        match l, codec with
                        | Python, Encode    -> Some(uper_p.EmitTypeWithRequiredBytesAndBits (BigInteger (ceil ((double t.uperMaxSizeInBits)/8.0))) (BigInteger t.uperMaxSizeInBits) func.Value)
                        | _, _              -> func
                
                    let errCodes = bodyResult.errCodes
                    let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
                    let funcDef = Some(EmitTypeAssignment_primitive_def varName sStar funcName  typeDefinition.name errCodStr (t.uperMaxSizeInBits = 0) (BigInteger (ceil ((double t.uperMaxSizeInBits)/8.0))) (BigInteger t.uperMaxSizeInBits) codec)
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


let getIntfuncBodyByCons (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) uperRange errLoc isUnsigned (cons: IntegerTypeConstraint list) (allCons: IntegerTypeConstraint list) (errCode:ErroCode) (p:FuncParamType) = 
    let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
    let IntNoneRequired         = match l with C -> uper_c.IntNoneRequired          | Ada -> (fun p min   errCode codec -> if min > 0I then  (uper_a.IntFullyConstraintPos p min min 0I errCode codec) else (uper_a.IntFullyConstraint p min min 0I errCode codec)) | Python -> uper_p.IntNoneRequired
    let IntFullyConstraintPos   = match l with C -> uper_c.IntFullyConstraintPos    | Ada -> uper_a.IntFullyConstraintPos   | Python -> uper_p.IntFullyConstraintPos
    let IntFullyConstraint      = match l with C -> uper_c.IntFullyConstraint       | Ada -> uper_a.IntFullyConstraint      | Python -> uper_p.IntFullyConstraint
    let IntSemiConstraintPos    = match l with C -> uper_c.IntSemiConstraintPos     | Ada -> uper_a.IntSemiConstraintPos    | Python -> uper_p.IntSemiConstraintPos
    let IntSemiConstraint       = match l with C -> uper_c.IntSemiConstraint        | Ada -> uper_a.IntSemiConstraint       | Python -> uper_p.IntSemiConstraint
    let IntUnconstraint         = match l with C -> uper_c.IntUnconstraint          | Ada -> uper_a.IntUnconstraint         | Python -> uper_p.IntUnconstraint
    let IntUnconstraintMax      = match l with C -> uper_c.IntUnconstraintMax       | Ada -> uper_a.IntUnconstraintMax      | Python -> uper_p.IntUnconstraintMax
    let IntRootExt              = match l with C -> uper_c.IntRootExt               | Ada -> uper_a.IntRootExt              | Python -> uper_p.IntRootExt
    let IntRootExt2             = match l with C -> uper_c.IntRootExt2              | Ada -> uper_a.IntRootExt2             | Python -> uper_p.IntRootExt2
    let rootCons = cons |> List.choose(fun x -> match x with RangeRootConstraint(a) |RangeRootConstraint2(a,_) -> Some(x) |_ -> None) 

    let checkExp = 
        match (DAstValidate.createIntegerFunctionByCons r l isUnsigned allCons) with
        | None  ->  None
        | Some expFunc -> Some (expFunc (p.getValue l))

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
        | []                            -> IntBod uperRange false
        | (RangeRootConstraint a)::rest      -> 
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) (p.getValue l) a
            IntRootExt pp (getValueByConstraint uperR) cc (IntBod uperR true) errCode.errCodeName codec
        | (RangeRootConstraint2(a,_))::rest  -> 
            let uperR    = uPER.getIntTypeConstraintUperRange [a]  errLoc
            let cc = DAstValidate.foldRangeCon l (fun v -> v.ToString()) (fun v -> v.ToString()) (p.getValue l) a
            IntRootExt2 pp (getValueByConstraint uperR) cc (IntBod uperR true) errCode.errCodeName codec 
        | _                             -> raise(BugErrorException "")
    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    




let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        getIntfuncBodyByCons r l codec o.uperRange t.Location o.isUnsigned o.cons o.AllCons errCode p
        (*
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
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
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us


let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let Boolean         = match l with C -> uper_c.Boolean          | Ada -> uper_a.Boolean | Python -> uper_p.Boolean
        let funcBodyContent = Boolean pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    let soSparkAnnotations = None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let Real         = match l with C -> uper_c.Real          | Ada -> uper_a.Real | Python -> uper_p.Real
        let funcBodyContent = Real pp errCode.errCodeName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    let soSparkAnnotations = None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let soSparkAnnotations = None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> None) soSparkAnnotations us


let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let Enumerated         = match l with C -> uper_c.Enumerated        | Ada -> uper_a.Enumerated      | Python -> uper_p.Enumerated
        let Enumerated_item    = match l with C -> uper_c.Enumerated_item   | Ada -> uper_a.Enumerated_item | Python -> uper_p.Enumerated_item
        let typeDefinitionName = getTypeDefinitionName t.id.tasInfo typeDefinition
        let nMin = 0I
        let nMax = BigInteger(Seq.length o.items) - 1I
        let nLastItemIndex      = nMax
        let items = 
            o.items |> List.mapi(fun i itm -> Enumerated_item (p.getValue l) (itm.getBackendName l) (BigInteger i) nLastItemIndex codec) 
        let nBits = (GetNumberOfBitsForNonNegativeInteger (nMax-nMin))
        let sFirstItemName = o.items.Head.getBackendName l
        let funcBodyContent = Enumerated (p.getValue l) typeDefinitionName items nMin nMax nBits errCode.errCodeName nLastItemIndex sFirstItemName codec
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    let soSparkAnnotations = None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations us



let createIA5StringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
    let charIndex =
        match l with
        | C     -> []
        | Ada   -> [IntegerLocalVariable ("charIndex", None)]
        | Python     -> []
    let nStringLength =
        match o.minSize = o.maxSize with
        | true  -> []
        | false ->
            match l with
            | Ada  -> [IntegerLocalVariable ("nStringLength", None)]
            | C    -> [Asn1SIntLocalVariable ("nStringLength", None)]
            | Python -> [Asn1SIntLocalVariable ("nStringLength", None)]
    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        let InternalItem_string_no_alpha    = match l with C -> uper_c.InternalItem_string_no_alpha     | Ada -> uper_a.InternalItem_string_no_alpha    | Python -> uper_p.InternalItem_string_no_alpha
        let InternalItem_string_with_alpha  = match l with C -> uper_c.InternalItem_string_with_alpha   | Ada -> uper_a.InternalItem_string_with_alpha  | Python -> uper_p.InternalItem_string_with_alpha
        let str_FixedSize   = match l with C -> uper_c.str_FixedSize    | Ada -> uper_a.str_FixedSize   | Python -> uper_p.str_FixedSize
        let str_VarSize     = match l with C -> uper_c.str_VarSize      | Ada -> uper_a.str_VarSize     | Python -> uper_p.str_VarSize
        //let Fragmentation_sqf   = match l with C -> uper_c.Fragmentation_sqf    | Ada -> uper_a.Fragmentation_sqf
        let typeDefinitionName = getTypeDefinitionName t.id.tasInfo typeDefinition

        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
        let internalItem =
            match o.uperCharSet.Length = 128 with
            | true  -> InternalItem_string_no_alpha p.p i  codec 
            | false -> 
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                InternalItem_string_with_alpha p.p typeDefinition.name i (BigInteger (o.uperCharSet.Length-1)) arrAsciiCodes (BigInteger (o.uperCharSet.Length)) nBits  codec
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.maxSize - o.minSize))
        let funcBodyContent = 
            match o.minSize with
            | _ when o.maxSize < 65536 && o.maxSize=o.minSize  -> str_FixedSize p.p typeDefinitionName i internalItem (BigInteger o.minSize) nBits nBits 0I codec 
            | _ when o.maxSize < 65536 && o.maxSize<>o.minSize  -> str_VarSize p.p typeDefinitionName i internalItem (BigInteger o.minSize) (BigInteger o.maxSize) nSizeInBits nBits nBits 0I codec 
            | _                                                -> raise(Exception "fragmentation not implemented yet")
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = lv::charIndex@nStringLength}    
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   ->
            Some(uper_a.annotations typeDefinition.name true isValidFunc.IsSome true true codec)
        | Python-> None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations  us

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
    let nStringLength =
        (*
        match o.minSize <> o.maxSize with
        | false  -> []
        | true ->
            match l with
            | Ada  -> [IntegerLocalVariable ("nStringLength", None)]
            | C    -> 
                match codec = Codec.Decode with
                | true  -> [Asn1SIntLocalVariable ("nCount", None)]
                | false -> []
        *)
        match o.minSize = o.maxSize,  l, codec with
        | true , _,_    -> []
        | false, Ada, _ -> [IntegerLocalVariable ("nStringLength", None)]
        | false, C, Encode -> []
        | false, C, Decode -> [Asn1SIntLocalVariable ("nCount", None)]
        | false, Python, _ -> []


    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        let typeDefinitionName = getTypeDefinitionName t.id.tasInfo typeDefinition

        let InternalItem_oct_str    = match l with C -> uper_c.InternalItem_oct_str     | Ada -> uper_a.InternalItem_oct_str    | Python -> uper_p.InternalItem_oct_str
        let fixedSize               = match l with C -> uper_c.octect_FixedSize         | Ada -> uper_a.octect_FixedSize        | Python -> uper_p.octect_FixedSize
        let varSize                 = match l with C -> uper_c.octect_VarSize           | Ada -> uper_a.octect_VarSize          | Python -> uper_p.octect_VarSize

        let nBits = 8I
        let internalItem = InternalItem_oct_str p.p (p.getAcces l) i  errCode.errCodeName codec 
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.maxSize - o.minSize))
        let funcBodyContent = 
            match o.minSize with
            | _ when o.maxSize < 65536 && o.maxSize=o.minSize  ->  fixedSize p.p typeDefinitionName i internalItem (BigInteger o.minSize) nBits nBits 0I codec 
            | _ when o.maxSize < 65536 && o.maxSize<>o.minSize  -> varSize p.p (p.getAcces l)  typeDefinitionName i internalItem (BigInteger o.minSize) (BigInteger o.maxSize) nSizeInBits nBits nBits 0I errCode.errCodeName codec 
            | _                                                -> raise(Exception "fragmentation not implemented yet")
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = lv::nStringLength}    
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   ->
            Some(uper_a.annotations typeDefinition.name true isValidFunc.IsSome true true codec)
        | Python -> None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p)) soSparkAnnotations  us

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let localVariables =
        match o.minSize = o.maxSize,  l, codec with
        | true , _,_    -> []
        | false, Ada, _ -> [IntegerLocalVariable ("nStringLength", None)]
        | false, C, Encode -> []
        | false, C, Decode -> [Asn1SIntLocalVariable ("nCount", None)]
        | false, Python, _ -> []
    let localVariables =
        match l with 
        | C -> localVariables 
        | Ada -> 
            let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
            lv::localVariables
        | Python -> localVariables
(*
        match o.minSize <> o.maxSize && codec = Codec.Decode with
        | false  -> []
        | true ->
            match l with
            | Ada  -> 
                let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
                lv::[IntegerLocalVariable ("nStringLength", None)]
            | C    -> [Asn1SIntLocalVariable ("nCount", None)]*)
    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        let funcBodyContent = 
            match l with
            | Ada ->
                let typeDefinitionName = getTypeDefinitionName t.id.tasInfo typeDefinition
                let nBits = 1I
                let internalItem = uper_a.InternalItem_bit_str p.p i  errCode.errCodeName codec 
                let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.maxSize - o.minSize))
                match o.minSize with
                | _ when o.maxSize < 65536 && o.maxSize=o.minSize  -> uper_a.octect_FixedSize p.p typeDefinitionName i internalItem (BigInteger o.minSize) nBits nBits 0I codec 
                | _ when o.maxSize < 65536 && o.maxSize<>o.minSize -> uper_a.octect_VarSize p.p (p.getAcces l)  typeDefinitionName i internalItem (BigInteger o.minSize) (BigInteger o.maxSize) nSizeInBits nBits nBits 0I errCode.errCodeName codec 
                | _                                                -> raise(Exception "fragmentation not implemented yet")
            | C ->
                match o.minSize with
                | _ when o.maxSize < 65536 && o.maxSize=o.minSize   -> uper_c.bitString_FixSize p.p (p.getAcces l) (BigInteger o.minSize) errCode.errCodeName codec 
                | _ when o.maxSize < 65536 && o.maxSize<>o.minSize  -> uper_c.bitString_VarSize p.p (p.getAcces l) (BigInteger o.minSize) (BigInteger o.maxSize) errCode.errCodeName codec 
                | _                                                -> raise(Exception "fragmentation not implemented yet")
            | Python ->
                match o.minSize with
                | _ when o.maxSize < 65536 && o.maxSize=o.minSize   -> uper_p.bitString_FixSize p.p (p.getAcces l) (BigInteger o.minSize) errCode.errCodeName codec 
                | _ when o.maxSize < 65536 && o.maxSize<>o.minSize  -> uper_p.bitString_VarSize p.p (p.getAcces l) (BigInteger o.minSize) (BigInteger o.maxSize) errCode.errCodeName codec 
                | _                                                -> raise(Exception "fragmentation not implemented yet")
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables}    
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> Some(uper_a.annotations typeDefinition.name true isValidFunc.IsSome true true codec)
        | Python -> None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  (fun e p -> Some (funcBody e p))  soSparkAnnotations  us



let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (child:Asn1Type) (us:State)  =
    let nStringLength =
        match o.minSize = o.maxSize,  l, codec with
        | true , _,_    -> []
        | false, Ada, _ -> [IntegerLocalVariable ("nStringLength", None)]
        | false, C, Encode -> []
        | false, C, Decode -> [Asn1SIntLocalVariable ("nCount", None)]
        | false , Python,_    -> []
(*
        match o.minSize <> o.maxSize && codec = Codec.Decode with
        | false  -> []
        | true ->
            match l with
            | Ada  -> [IntegerLocalVariable ("nStringLength", None)]
            | C    -> [Asn1SIntLocalVariable ("nCount", None)]
*)
    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        match baseFuncName with
        | None ->
            let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
            let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)

            let fixedSize   = match l with C -> uper_c.octect_FixedSize     | Ada -> uper_a.octect_FixedSize | Python -> uper_p.sequenceOf_FixedSize
            let varSize     = match l with C -> uper_c.octect_VarSize       | Ada -> uper_a.octect_VarSize   | Python -> uper_p.sequenceOf_VarSize

            let chFunc = child.getUperFunction codec
            let internalItem = 
                chFunc.funcBody (p.getArrayItem l i child.isIA5String)
            match internalItem with
            | None  -> 
                    match o.minSize with
                    | _ when o.maxSize < 65536 && o.maxSize=o.minSize  -> None
                    | _ when o.maxSize < 65536 && o.maxSize<>o.minSize -> Some ({UPERFuncBodyResult.funcBody = "todo Encode only length"; errCodes = [errCode]; localVariables = nStringLength})    
                    | _                                                -> raise(Exception "fragmentation not implemented yet")
            | Some internalItem -> 
                let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.maxSize - o.minSize))
                let localVariables = internalItem.localVariables
                let childErrCodes =  internalItem.errCodes
                let typeDefinitionName = getTypeDefinitionName t.id.tasInfo typeDefinition

                let ret = 
                    match o.minSize with
                    | _ when o.maxSize < 65536 && o.maxSize=o.minSize   -> fixedSize p.p typeDefinitionName i internalItem.funcBody (BigInteger o.minSize) (BigInteger child.uperMinSizeInBits) (BigInteger child.uperMaxSizeInBits) 0I codec 
                    | _ when o.maxSize < 65536 && o.maxSize<>o.minSize  -> varSize p.p (p.getAcces l)  typeDefinitionName i internalItem.funcBody (BigInteger o.minSize) (BigInteger o.maxSize) nSizeInBits (BigInteger child.uperMinSizeInBits) (BigInteger child.uperMaxSizeInBits) 0I errCode.errCodeName codec 
                    | _                                                 -> raise(Exception "fragmentation not implemented yet")
                Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = lv::(nStringLength@localVariables)})    
        | Some baseFuncName ->
            let funcBodyContent =  callBaseTypeFunc l (p.getPointer l) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []})
    let soSparkAnnotations = None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us


let nestChildItems (l:ProgrammingLanguage) (codec:CommonTypes.Codec) children = 
    let printChild (content:string) (sNestedContent:string option) = 
        match sNestedContent with
        | None  -> content
        | Some c-> 
            match l with
            | C         -> equal_c.JoinItems content sNestedContent
            | Ada       -> uper_a.JoinItems content sNestedContent
            | Python    -> uper_p.JoinItems content sNestedContent
    let rec printChildren children : Option<string> = 
        match children with
        |[]     -> None
        |x::xs  -> 
            match printChildren xs with
            | None                 -> Some (printChild x  None)
            | Some childrenCont    -> Some (printChild x  (Some childrenCont))
    printChildren children

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (us:State)  =
    // stg macros
    let sequence_presence_bit       = match l with C -> uper_c.sequence_presence_bit        | Ada -> uper_a.sequence_presence_bit       | Python -> uper_p.sequence_presence_bit
    let sequence_presence_bit_fix   = match l with C -> uper_c.sequence_presence_bit_fix    | Ada -> uper_a.sequence_presence_bit_fix   | Python -> uper_p.sequence_presence_bit_fix
    let sequence_mandatory_child    = match l with C -> uper_c.sequence_mandatory_child     | Ada -> uper_a.sequence_mandatory_child    | Python -> uper_p.sequence_mandatory_child
    let sequence_optional_child     = match l with C -> uper_c.sequence_optional_child      | Ada -> uper_a.sequence_optional_child     | Python -> uper_p.sequence_optional_child
    let sequence_default_child      = match l with C -> uper_c.sequence_default_child       | Ada -> uper_a.sequence_default_child      | Python -> uper_p.sequence_default_child
    let baseFuncName                = match baseTypeUperFunc with None -> None | Some baseFunc -> baseFunc.funcName 

    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        match baseFuncName with
        | None ->
            let nonAcnChildren = children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
            let localVariables =
                match nonAcnChildren |> Seq.exists(fun x -> x.Optionality.IsSome) with
                | true  when l = C  && codec = CommonTypes.Decode -> [(FlagLocalVariable ("presenceBit", None))]
                | _                                       -> []
            let printPresenceBit (child:Asn1Child) =
                match child.Optionality with
                | None                       -> None
                | Some Asn1AcnAst.AlwaysAbsent     -> Some (sequence_presence_bit_fix p.p (p.getAcces l) child.c_name "0" errCode.errCodeName codec)    // please note that in decode, macro uper_sequence_presence_bit_fix
                | Some Asn1AcnAst.AlwaysPresent    -> Some (sequence_presence_bit_fix p.p (p.getAcces l) child.c_name "1" errCode.errCodeName codec)    // calls macro uper_sequence_presence_bit (i.e. behaves like optional)
                | Some (Asn1AcnAst.Optional opt)   -> Some (sequence_presence_bit p.p (p.getAcces l) child.c_name  errCode.errCodeName codec)
            let handleChild (child:Asn1Child) =
                let chFunc = child.Type.getUperFunction codec
                let childContentResult = chFunc.funcBody (p.getSeqChild l child.c_name child.Type.isIA5String)
                match childContentResult with
                | None              -> None
                | Some childContent ->
                    let childBody = 
                        match child.Optionality with
                        | None                       -> Some (sequence_mandatory_child child.c_name childContent.funcBody codec) 
                        | Some Asn1AcnAst.AlwaysAbsent     -> match codec with CommonTypes.Encode -> None                        | CommonTypes.Decode -> Some (sequence_optional_child p.p (p.getAcces l) child.c_name childContent.funcBody codec) 
                        | Some Asn1AcnAst.AlwaysPresent    -> match codec with CommonTypes.Encode -> Some childContent.funcBody  | CommonTypes.Decode -> Some (sequence_optional_child p.p (p.getAcces l) child.c_name childContent.funcBody codec)
                        | Some (Asn1AcnAst.Optional opt)   -> 
                            match opt.defaultValue with
                            | None                   -> Some (sequence_optional_child p.p (p.getAcces l) child.c_name childContent.funcBody codec)
                            | Some v                 -> 
                                let defInit= child.Type.initFunction.initFuncBody (p.getSeqChild l child.c_name child.Type.isIA5String) (mapValue v).kind
                                Some (sequence_default_child p.p (p.getAcces l) child.c_name childContent.funcBody defInit codec) 
                    Some (childBody, childContent.localVariables, childContent.errCodes)
            
            let presenseBits = nonAcnChildren |> List.choose printPresenceBit
            let childrenStatements0 = nonAcnChildren |> List.choose handleChild
            let childrenStatements = childrenStatements0 |> List.choose(fun (s,_,_) -> s)
            let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_) -> s)
            let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s) -> s)
            let seqContent = (presenseBits@childrenStatements) |> nestChildItems l codec
            match seqContent with
            | None  -> None
            | Some ret -> Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars})    
        | Some baseFuncName ->
            let funcBodyContent = callBaseTypeFunc l (p.getPointer l) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []})
            
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> None
        | Python-> None
    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us



let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : UPerFunction option) (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (us:State)  =
    // stg macros
    let choice_child       = match l with C -> uper_c.choice_child | Ada -> uper_a.choice_child | Python -> uper_p.choice_child
    let choice             = match l with C -> uper_c.choice       | Ada -> uper_a.choice       | Python -> uper_p.choice

    let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName
    
    let sChoiceIndexName = typeDefinition.name + "_index_tmp"
    let localVariables =
        match codec with
        | CommonTypes.Encode  -> []
        | CommonTypes.Decode  -> [(Asn1SIntLocalVariable (sChoiceIndexName, None))]

    let typeDefinitionName = getTypeDefinitionName t.id.tasInfo typeDefinition

    let funcBody (errCode:ErroCode) (p:FuncParamType) = 
        match baseFuncName with
        | None ->
            let nIndexSizeInBits = (GetNumberOfBitsForNonNegativeInteger (BigInteger (children.Length - 1)))
            let childrenContent3 =
                children |> 
                List.mapi(fun i child ->
                    let chFunc = child.chType.getUperFunction codec
                    let uperChildRes = 
                        match l with
                        | C                                     -> chFunc.funcBody (p.getChChild l child.c_name child.chType.isIA5String)
                        | Ada when codec = CommonTypes.Decode   -> chFunc.funcBody (VALUE (child.c_name + "_tmp"))
                        | Ada                                   -> chFunc.funcBody (p.getChChild l child.c_name child.chType.isIA5String)
                        | Python                                -> chFunc.funcBody (p.getChChild l child.py_name child.chType.isIA5String)
                    match uperChildRes with
                    | None              -> match l with C | Ada -> "/*no encoding/decoding is required*/",[],[] | Python -> uper_p.choice_null_child (BigInteger i) child.c_name codec,[],[]
                    | Some childContent ->  
                        let sChildName = child.c_name
                        let sChildTypeDef = child.chType.typeDefinition.typeDefinitionBodyWithinSeq
                        let sChoiceTypeName = typeDefinitionName
                        choice_child p.p (p.getAcces l) child.presentWhenName (BigInteger i) nIndexSizeInBits (BigInteger (children.Length - 1)) childContent.funcBody sChildName sChildTypeDef sChoiceTypeName codec, childContent.localVariables, childContent.errCodes )
            let childrenContent = childrenContent3 |> List.map(fun (s,_,_) -> s)
            let childrenLocalvars = childrenContent3 |> List.collect(fun (_,s,_) -> s)
            let childrenErrCodes = childrenContent3 |> List.collect(fun (_,_,s) -> s)
            
            let ret = choice p.p (p.getAcces l) childrenContent (BigInteger (children.Length - 1)) sChoiceIndexName errCode.errCodeName typeDefinitionName nIndexSizeInBits  codec
            Some ({UPERFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars})
        | Some baseFuncName ->
            let funcBodyContent = callBaseTypeFunc l (p.getPointer l) baseFuncName codec
            Some ({UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []})

    let soSparkAnnotations = None

    createPrimitiveFunction r l codec t typeDefinition baseTypeUperFunc  isValidFunc  funcBody soSparkAnnotations  us

(*
let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId:ReferenceToType) =
    typeId.tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + codec.suffix))

*)

let createReferenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionCommon) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
    let typeDefinitionName = ToC2(r.args.TypePrefix + o.tasName.Value)
    let baseFncName = typeDefinitionName + codec.suffix

    let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
    let t1WithExtensios = o.resolvedType;
    match TypesEquivalence.uperEquivalence t1 t1WithExtensios with
    | true  ->
        let soSparkAnnotations = None
        let funcBody (errCode:ErroCode) (p:FuncParamType) = 
            match (baseType.getUperFunction codec).funcBody p with
            | Some _    -> 
                let funcBodyContent = 
                    match l with 
                    | C 
                    | Ada       -> callBaseTypeFunc l (t.getParamValue p l codec) baseFncName codec 
                    | Python    -> callBaseTypeFunc l (t.getParamValue p l codec) typeDefinitionName codec 
                Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
            | None      -> None
        createPrimitiveFunction r l codec t typeDefinition None  isValidFunc  funcBody soSparkAnnotations  us
    | false -> 
        baseType.getUperFunction codec, us
