﻿module DAstACN

open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions

let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId:ReferenceToType) =
    typeId.tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + "_ACN" + codec.suffix))

let getTypeDefinitionName (tasInfo:TypeAssignmentInfo option) (typeDefinition:TypeDefinitionCommon) =
    match tasInfo with
    | Some _                -> typeDefinition.name
    | None (*inner type*)   -> typeDefinition.typeDefinitionBodyWithinSeq

let callBaseTypeFunc l = match l with C -> uper_c.call_base_type_func | Ada -> uper_a.call_base_type_func | Python -> uper_p.call_base_type_func


let getAcnDeterminantName (l:ProgrammingLanguage) (id : ReferenceToType) =
    match id with
    | ReferenceToType path ->
        match path with
        | (MD mdName)::(TA tasName)::(PRM prmName)::[]   -> ToC2 prmName
        | _ ->
            let longName = id.AcnAbsPath.Tail |> Seq.StrJoin "_"
            let determinantName = ToC2(longName.Replace("#","elem"))
            match l with C | Ada -> determinantName | Python -> determinantName.ToLower()                

let getDeterminantTypeDefinitionBodyWithinSeq (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (det:Determinant) = 
    match det with
    | AcnChildDeterminant       ch ->
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a -> DAstTypeDefinition.createAcnInteger r l a
        | Asn1AcnAst.AcnNullType _ -> DAstTypeDefinition.createAcnNull r l
        | Asn1AcnAst.AcnBoolean  _ -> DAstTypeDefinition.createAcnBoolean r l
        | Asn1AcnAst.AcnReferenceToEnumerated a -> ToC2(r.args.TypePrefix + a.tasName.Value)
        | Asn1AcnAst.AcnReferenceToIA5String a -> ToC2(r.args.TypePrefix + a.tasName.Value)

    | AcnParameterDeterminant   prm ->
        match prm.asn1Type with
        | Asn1AcnAst.AcnPrmInteger  _       -> DAstTypeDefinition.createPrmAcnInteger r l 
        | Asn1AcnAst.AcnPrmBoolean  _       -> DAstTypeDefinition.createAcnBoolean r l
        | Asn1AcnAst.AcnPrmNullType _       -> DAstTypeDefinition.createAcnNull r l
        | Asn1AcnAst.AcnPrmRefType (md,ts)  -> DAstTypeDefinition.getTypeDefinitionName r l (ReferenceToType [MD md.Value; TA ts.Value])


let getDeterminant_macro (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (det:Determinant) pri_macro str_macro = 
    match det with
    | AcnChildDeterminant       ch ->
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a  -> pri_macro
        | Asn1AcnAst.AcnNullType _  -> pri_macro
        | Asn1AcnAst.AcnBoolean  _  -> pri_macro
        | Asn1AcnAst.AcnReferenceToEnumerated a -> pri_macro
        | Asn1AcnAst.AcnReferenceToIA5String a -> str_macro

    | AcnParameterDeterminant   prm ->
        match prm.asn1Type with
        | Asn1AcnAst.AcnPrmInteger  _       -> pri_macro
        | Asn1AcnAst.AcnPrmBoolean  _       -> pri_macro
        | Asn1AcnAst.AcnPrmNullType _       -> pri_macro
        | Asn1AcnAst.AcnPrmRefType (md,ts)  -> pri_macro

let getDeterminantTypeUpdateMacro (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (det:Determinant) = 
    let MultiAcnUpdate_get_first_init_value_pri     =  match l with C -> acn_c.MultiAcnUpdate_get_first_init_value_pri        | Ada -> acn_a.MultiAcnUpdate_get_first_init_value_pri    | Python -> acn_p.MultiAcnUpdate_get_first_init_value_pri
    let MultiAcnUpdate_get_first_init_value_str     =  match l with C -> acn_c.MultiAcnUpdate_get_first_init_value_str        | Ada -> acn_a.MultiAcnUpdate_get_first_init_value_str    | Python -> acn_p.MultiAcnUpdate_get_first_init_value_str
    getDeterminant_macro r l det MultiAcnUpdate_get_first_init_value_pri MultiAcnUpdate_get_first_init_value_str

let getDeterminantTypeCheckEqual (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (det:Determinant) = 
    let multiAcnUpdate_checkEqual_pri     =  match l with C -> acn_c.MultiAcnUpdate_checkEqual_pri        | Ada -> acn_c.MultiAcnUpdate_checkEqual_pri  | Python -> acn_p.MultiAcnUpdate_checkEqual_pri
    let multiAcnUpdate_checkEqual_str     =  match l with C -> acn_c.MultiAcnUpdate_checkEqual_str        | Ada -> acn_c.MultiAcnUpdate_checkEqual_str  | Python -> acn_p.MultiAcnUpdate_checkEqual_str
    getDeterminant_macro r l det multiAcnUpdate_checkEqual_pri multiAcnUpdate_checkEqual_str
    (*
    match det with
    | AcnChildDeterminant       ch ->
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a  -> multiAcnUpdate_checkEqual_pri
        | Asn1AcnAst.AcnNullType _  -> multiAcnUpdate_checkEqual_pri
        | Asn1AcnAst.AcnBoolean  _  -> multiAcnUpdate_checkEqual_pri
        | Asn1AcnAst.AcnReferenceToEnumerated a -> multiAcnUpdate_checkEqual_pri
        | Asn1AcnAst.AcnReferenceToIA5String a -> multiAcnUpdate_checkEqual_str

    | AcnParameterDeterminant   prm ->
        match prm.asn1Type with
        | Asn1AcnAst.AcnPrmInteger  _       -> multiAcnUpdate_checkEqual_pri
        | Asn1AcnAst.AcnPrmBoolean  _       -> multiAcnUpdate_checkEqual_pri
        | Asn1AcnAst.AcnPrmNullType _       -> multiAcnUpdate_checkEqual_pri
        | Asn1AcnAst.AcnPrmRefType (md,ts)  -> multiAcnUpdate_checkEqual_pri
        *)

let private createAcnFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionCommon) (isValidFunc: IsValidFunction option)  (funcBody:ErroCode->((Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) -> FuncParamType -> (AcnFuncBodyResult option)) soSparkAnnotations (us:State)  =
    let funcNameAndtasInfo   = getFuncName r l codec t.id
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let EmitTypeAssignment_primitive = 
        match l with 
        | C -> acn_c.EmitTypeAssignment_primitive  
        | Ada -> acn_a.EmitTypeAssignment_primitive
        | Python -> 
            match t.Kind with
            | Asn1AcnAst.Asn1TypeKind.ReferenceType _
            | Asn1AcnAst.Asn1TypeKind.Sequence _
            | Asn1AcnAst.Asn1TypeKind.Choice _          -> acn_p.EmitTypeAssignment_primitive_no_set
            | Asn1AcnAst.Asn1TypeKind.SequenceOf _      -> acn_p.EmitTypeAssignment_primitive_sqf
            | _                                         -> acn_p.EmitTypeAssignment_primitive
    let EmitTypeAssignment_primitive_def =  match l with C -> acn_c.EmitTypeAssignment_primitive_def    | Ada -> acn_a.EmitTypeAssignment_primitive_def     | Python -> acn_p.EmitTypeAssignment_primitive_def
    let EmitTypeAssignment_def_err_code  =  match l with C -> acn_c.EmitTypeAssignment_def_err_code     | Ada -> acn_a.EmitTypeAssignment_def_err_code      | Python -> (fun _ _ -> "")

    let funcBody = (funcBody errCode)
    let p : FuncParamType = t.getParamType l codec
    let topLevAcc = p.getAcces l
    let varName = p.p
    let sStar = p.getStar l
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let sInitilialExp = ""
    let  func, funcDef  = 
            match funcNameAndtasInfo  with
            |  None              -> None, None
            |  Some funcName     -> 
                let content = funcBody [] p  
                match content with 
                | None          -> None, None
                | Some bodyResult  ->
                    let handleAcnParameter (p:Asn1AcnAst.AcnParameter) =
                        let intType  = match l with C -> header_c.Declare_Integer () | Ada -> header_a.Declare_Integer ()   | Python -> types_p.Declare_Integer ()
                        let boolType = match l with C -> header_c.Declare_Boolean () | Ada -> header_a.Declare_BOOLEAN ()   | Python -> types_p.Declare_Boolean ()
                        let emitPrm  = match l with C -> acn_c.EmitAcnParameter      | Ada -> acn_a.EmitAcnParameter        | Python -> acn_p.EmitAcnParameter
                        match p.asn1Type with
                        | Asn1AcnAst.AcnPrmInteger    loc          -> emitPrm p.c_name intType
                        | Asn1AcnAst.AcnPrmBoolean    loc          -> emitPrm p.c_name boolType
                        | Asn1AcnAst.AcnPrmNullType   loc          -> raise(SemanticError (loc, "Invalid type for parameter"))
                        | Asn1AcnAst.AcnPrmRefType(md,ts)          -> emitPrm p.c_name (ToC2(r.args.TypePrefix + ts.Value))

                    let lvars = bodyResult.localVariables |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                    let prms = t.acnParameters |> List.map handleAcnParameter
                    let prmNames = t.acnParameters |> List.map (fun p -> p.c_name)
                    let func = Some(EmitTypeAssignment_primitive varName sStar funcName isValidFuncName  typeDefinition.name lvars  bodyResult.funcBody soSparkAnnotations sInitilialExp prms prmNames codec)
                    let func =
                        match l, codec with
                        | Python, Encode    -> Some(acn_p.EmitTypeWithRequiredBytesAndBits (BigInteger (ceil ((double t.acnMaxSizeInBits)/8.0))) (BigInteger t.acnMaxSizeInBits) func.Value)
                        | _, _              -> func
                
                    let errCodes = bodyResult.errCodes
                    let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
                    let funcDef = Some(EmitTypeAssignment_primitive_def varName sStar funcName  typeDefinition.name errCodStr (t.acnMaxSizeInBits = 0) (BigInteger (ceil ((double t.acnMaxSizeInBits)/8.0))) (BigInteger t.acnMaxSizeInBits) prms codec)
                    func, funcDef


    let ret = 
        {
            AcnFunction.funcName       = funcNameAndtasInfo 
            func                       = func 
            funcDef                    = funcDef
            funcBody                   = funcBody
        }
    ret, ns

let private createAcnIntegerFunctionInternal (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (uperRange : uperRange<BigInteger>) acnEncodingClass (uperfuncBody : ErroCode -> FuncParamType -> (UPERFuncBodyResult option)) : (ErroCode -> ((Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) -> FuncParamType -> (AcnFuncBodyResult option))  =
    let PositiveInteger_ConstSize_8                  = match l with C -> acn_c.PositiveInteger_ConstSize_8                | Ada -> acn_a.PositiveInteger_ConstSize_8               | Python -> acn_p.PositiveInteger_ConstSize_8     
    let PositiveInteger_ConstSize_big_endian_16      = match l with C -> acn_c.PositiveInteger_ConstSize_big_endian_16    | Ada -> acn_a.PositiveInteger_ConstSize_big_endian_16   | Python -> acn_p.PositiveInteger_ConstSize_big_endian_16
    let PositiveInteger_ConstSize_little_endian_16   = match l with C -> acn_c.PositiveInteger_ConstSize_little_endian_16 | Ada -> acn_a.PositiveInteger_ConstSize_little_endian_16| Python -> acn_p.PositiveInteger_ConstSize_little_endian_16
    let PositiveInteger_ConstSize_big_endian_32      = match l with C -> acn_c.PositiveInteger_ConstSize_big_endian_32    | Ada -> acn_a.PositiveInteger_ConstSize_big_endian_32   | Python -> acn_p.PositiveInteger_ConstSize_big_endian_32
    let PositiveInteger_ConstSize_little_endian_32   = match l with C -> acn_c.PositiveInteger_ConstSize_little_endian_32 | Ada -> acn_a.PositiveInteger_ConstSize_little_endian_32| Python -> acn_p.PositiveInteger_ConstSize_little_endian_32
    let PositiveInteger_ConstSize_big_endian_64      = match l with C -> acn_c.PositiveInteger_ConstSize_big_endian_64    | Ada -> acn_a.PositiveInteger_ConstSize_big_endian_64   | Python -> acn_p.PositiveInteger_ConstSize_big_endian_64
    let PositiveInteger_ConstSize_little_endian_64   = match l with C -> acn_c.PositiveInteger_ConstSize_little_endian_64 | Ada -> acn_a.PositiveInteger_ConstSize_little_endian_64| Python -> acn_p.PositiveInteger_ConstSize_little_endian_64
    let PositiveInteger_ConstSize                    = match l with C -> acn_c.PositiveInteger_ConstSize                  | Ada -> acn_a.PositiveInteger_ConstSize                 | Python -> acn_p.PositiveInteger_ConstSize
    let TwosComplement_ConstSize_8                   = match l with C -> acn_c.TwosComplement_ConstSize_8                 | Ada -> acn_a.TwosComplement_ConstSize_8                | Python -> acn_p.TwosComplement_ConstSize_8
    let TwosComplement_ConstSize_big_endian_16       = match l with C -> acn_c.TwosComplement_ConstSize_big_endian_16     | Ada -> acn_a.TwosComplement_ConstSize_big_endian_16    | Python -> acn_p.TwosComplement_ConstSize_big_endian_16
    let TwosComplement_ConstSize_little_endian_16    = match l with C -> acn_c.TwosComplement_ConstSize_little_endian_16  | Ada -> acn_a.TwosComplement_ConstSize_little_endian_16 | Python -> acn_p.TwosComplement_ConstSize_little_endian_16
    let TwosComplement_ConstSize_big_endian_32       = match l with C -> acn_c.TwosComplement_ConstSize_big_endian_32     | Ada -> acn_a.TwosComplement_ConstSize_big_endian_32    | Python -> acn_p.TwosComplement_ConstSize_big_endian_32
    let TwosComplement_ConstSize_little_endian_32    = match l with C -> acn_c.TwosComplement_ConstSize_little_endian_32  | Ada -> acn_a.TwosComplement_ConstSize_little_endian_32 | Python -> acn_p.TwosComplement_ConstSize_little_endian_32
    let TwosComplement_ConstSize_big_endian_64       = match l with C -> acn_c.TwosComplement_ConstSize_big_endian_64     | Ada -> acn_a.TwosComplement_ConstSize_big_endian_64    | Python -> acn_p.TwosComplement_ConstSize_big_endian_64
    let TwosComplement_ConstSize_little_endian_64    = match l with C -> acn_c.TwosComplement_ConstSize_little_endian_64  | Ada -> acn_a.TwosComplement_ConstSize_little_endian_64 | Python -> acn_p.TwosComplement_ConstSize_little_endian_64
    let TwosComplement_ConstSize                     = match l with C -> acn_c.TwosComplement_ConstSize                   | Ada -> acn_a.TwosComplement_ConstSize                  | Python -> acn_p.TwosComplement_ConstSize
    let ASCII_ConstSize                              = match l with C -> acn_c.ASCII_ConstSize                            | Ada -> acn_a.ASCII_ConstSize                           | Python -> acn_p.ASCII_ConstSize
    let ASCII_VarSize_NullTerminated                 = match l with C -> acn_c.ASCII_VarSize_NullTerminated               | Ada -> acn_a.ASCII_VarSize_NullTerminated              | Python -> acn_p.ASCII_VarSize_NullTerminated
    //+++ todo write ada stg macros for ASCII_UINT_ConstSize, ASCII_UINT_VarSize_NullTerminated
    let ASCII_UINT_ConstSize                         = match l with C -> acn_c.ASCII_UINT_ConstSize                       | Ada -> acn_a.ASCII_UINT_ConstSize                      | Python -> acn_p.ASCII_UINT_ConstSize
    let ASCII_UINT_VarSize_NullTerminated            = match l with C -> acn_c.ASCII_UINT_VarSize_NullTerminated          | Ada -> acn_a.ASCII_UINT_VarSize_NullTerminated         | Python -> acn_p.ASCII_UINT_VarSize_NullTerminated
    let BCD_ConstSize                                = match l with C -> acn_c.BCD_ConstSize                              | Ada -> acn_a.BCD_ConstSize                             | Python -> acn_p.BCD_ConstSize
    let BCD_VarSize_NullTerminated                   = match l with C -> acn_c.BCD_VarSize_NullTerminated                 | Ada -> acn_a.BCD_VarSize_NullTerminated                | Python -> acn_p.BCD_VarSize_NullTerminated

    //let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    //let errCodeValue        = us.currErrCode
    //let errCode             = {ErroCode.errCodeName = errCodeName; errCodeValue = errCodeValue}
    let nUperMin, nUperMax = 
        match uperRange with
        | Concrete(a,b) -> a,b
        | NegInf(b)     -> r.args.SIntMin, b
        | PosInf(a)     -> a, r.args.IntMax (a>=0I)
        | Full          -> r.args.SIntMin, r.args.SIntMax

    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType)        = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let funcBodyContent = 
            match acnEncodingClass with
            |Asn1AcnAst.Integer_uPER                                       ->  uperfuncBody errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
            |Asn1AcnAst.PositiveInteger_ConstSize_8                        ->  Some(PositiveInteger_ConstSize_8 pp None None nUperMin nUperMax  codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_16            ->  Some(PositiveInteger_ConstSize_big_endian_16 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_16         ->  Some(PositiveInteger_ConstSize_little_endian_16 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_32            ->  Some(PositiveInteger_ConstSize_big_endian_32 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_32         ->  Some(PositiveInteger_ConstSize_little_endian_32 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_64            ->  Some(PositiveInteger_ConstSize_big_endian_64 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_64         ->  Some(PositiveInteger_ConstSize_little_endian_64 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize bitSize                  ->  Some(PositiveInteger_ConstSize pp (BigInteger bitSize) None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_8                         ->  Some(TwosComplement_ConstSize_8 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_16             ->  Some(TwosComplement_ConstSize_big_endian_16 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_16          ->  Some(TwosComplement_ConstSize_little_endian_16 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_32             ->  Some(TwosComplement_ConstSize_big_endian_32 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_32          ->  Some(TwosComplement_ConstSize_little_endian_32 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_64             ->  Some(TwosComplement_ConstSize_big_endian_64 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_64          ->  Some(TwosComplement_ConstSize_little_endian_64 pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize bitSize                   ->  Some(TwosComplement_ConstSize pp None None (BigInteger bitSize) nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.ASCII_ConstSize size                               ->  Some(ASCII_ConstSize pp None None nUperMin nUperMax ((BigInteger size)/8I) codec, [errCode])
            |Asn1AcnAst.ASCII_VarSize_NullTerminated nullByte              ->  Some(ASCII_VarSize_NullTerminated pp None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.ASCII_UINT_ConstSize size                          ->  Some(ASCII_UINT_ConstSize pp None None nUperMin nUperMax ((BigInteger size)/8I) codec, [errCode])
            |Asn1AcnAst.ASCII_UINT_VarSize_NullTerminated nullByte         ->  Some(ASCII_UINT_VarSize_NullTerminated pp  None None nUperMin nUperMax codec, [errCode])
            |Asn1AcnAst.BCD_ConstSize size                                 ->  Some(BCD_ConstSize pp None None nUperMin nUperMax ((BigInteger size)/4I) codec, [errCode])
            |Asn1AcnAst.BCD_VarSize_NullTerminated nullByte                ->  Some(BCD_VarSize_NullTerminated pp None None nUperMin nUperMax codec, [errCode])
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []})
    //let funcBody = (funcBody errCode)
    funcBody


let createAcnIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnInteger)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let uperFuncBody (errCode) (p:FuncParamType) = 
        Some (DAstUPer.getIntfuncBodyByCons r l codec t.uperRange t.Location t.isUnsigned (t.cons) (t.cons@t.withcons) errCode p)
        (*let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let IntUnconstraint = match l with C -> uper_c.IntUnconstraint          | Ada -> uper_a.IntUnconstraint
        let funcBodyContent = IntUnconstraint pp errCode.errCodeName false codec
        Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    *)

    let funcBody = createAcnIntegerFunctionInternal r l codec t.uperRange t.acnEncodingClass uperFuncBody
    (funcBody errCode), ns



let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionCommon)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody = createAcnIntegerFunctionInternal r l codec o.uperRange o.acnEncodingClass uperFunc.funcBody_e
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition isValidFunc  (fun e acnArgs p -> funcBody e acnArgs p) soSparkAnnotations  us


let createEnumComn (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (o:Asn1AcnAst.Enumerated) (typeDefinitionName:string)  =
    let EnumeratedEncValues         = match l with C -> acn_c.EnumeratedEncValues       | Ada -> acn_a.EnumeratedEncValues      | Python -> acn_p.EnumeratedEncValues
    let Enumerated_item             = match l with C -> acn_c.Enumerated_item           | Ada -> acn_a.Enumerated_item          | Python -> acn_p.Enumerated_item
    //let IntFullyConstraint                  = match l with C -> uper_c.IntFullyConstraint       | Ada -> uper_a.IntFullyConstraint
    let IntFullyConstraintPos       = match l with C -> uper_c.IntFullyConstraintPos    | Ada -> uper_a.IntFullyConstraintPos   | Python -> uper_p.IntFullyConstraint
    let min = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.min
    let max = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.max
    let intVal = match l with C | Ada -> "intVal" | Python -> "enum_index"
    let sFirstItemName = o.items.Head.getBackendName l
    let localVar =
        match min >= 0I with
        | true -> Asn1UIntLocalVariable (intVal,None)
        | false -> Asn1SIntLocalVariable (intVal,None)
    let pVal = VALUE intVal
    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType)        = 
        let intFuncBody = 
            let uperInt (errCode:ErroCode) (p:FuncParamType) = 
                let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
                let funcBody = IntFullyConstraintPos pp min max (GetNumberOfBitsForNonNegativeInteger (max-min))  errCode.errCodeName codec
                Some({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables= []})
            createAcnIntegerFunctionInternal r l codec (Concrete (min,max)) o.acnEncodingClass uperInt
        let funcBodyContent = 
            match intFuncBody errCode acnArgs pVal with
            | None      -> None
            | Some(intAcnFuncBdResult) ->
                let getName (it:NamedItem) = 
                    match l with C | Ada -> it.getBackendName l | Python -> it.definitionValue.ToString()
                let arrItems = o.items |> List.map(fun it -> Enumerated_item (p.getValue l) (getName it) it.acnEncodeValue codec)
                Some (EnumeratedEncValues p.p typeDefinitionName arrItems intAcnFuncBdResult.funcBody errCode.errCodeName sFirstItemName codec, intAcnFuncBdResult.errCodes, localVar::intAcnFuncBdResult.localVariables)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables})
    funcBody

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionCommon)  (defOrRed:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let typeDefinitionName = defOrRed.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
    let funcBody = createEnumComn r l codec t.id o typeDefinitionName
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody soSparkAnnotations  us


let createAcnEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToEnumerated)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName
    let typeDefinitionName = ToC2(r.args.TypePrefix + t.tasName.Value)
    let funcBody = createEnumComn r l codec typeId t.enumerated typeDefinitionName
    (funcBody errCode), ns



let createRealrFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionCommon)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Real_32_big_endian                  = match l with C -> acn_c.Real_32_big_endian                | Ada -> acn_a.Real_32_big_endian       | Python -> acn_p.Real_32_big_endian 
    let Real_64_big_endian                  = match l with C -> acn_c.Real_64_big_endian                | Ada -> acn_a.Real_64_big_endian       | Python -> acn_p.Real_64_big_endian 
    let Real_32_little_endian               = match l with C -> acn_c.Real_32_little_endian             | Ada -> acn_a.Real_32_little_endian    | Python -> acn_p.Real_32_little_endian 
    let Real_64_little_endian               = match l with C -> acn_c.Real_64_little_endian             | Ada -> acn_a.Real_64_little_endian    | Python -> acn_p.Real_64_little_endian 
    
    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType)        = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let funcBodyContent = 
            match o.acnEncodingClass with
            | Real_IEEE754_32_big_endian            -> Some (Real_32_big_endian pp codec, [errCode])
            | Real_IEEE754_64_big_endian            -> Some (Real_64_big_endian pp codec, [errCode])
            | Real_IEEE754_32_little_endian         -> Some (Real_32_little_endian pp codec, [errCode])
            | Real_IEEE754_64_little_endian         -> Some (Real_64_little_endian pp codec, [errCode])
            | Real_uPER                             -> uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition isValidFunc  (fun e acnArgs p -> funcBody e acnArgs p) soSparkAnnotations us


let nestChildItems (l:ProgrammingLanguage) (codec:CommonTypes.Codec) children = 
    let printChild (content:string) (sNestedContent:string option) = 
        match sNestedContent with
        | None  -> content
        | Some c-> 
            match l with
            | C        -> equal_c.JoinItems content sNestedContent
            | Ada      -> uper_a.JoinItems content sNestedContent
            | Python   -> uper_p.JoinItems content sNestedContent
    let rec printChildren children : Option<string> = 
        match children with
        |[]     -> None
        |x::xs  -> 
            match printChildren xs with
            | None                 -> Some (printChild x  None)
            | Some childrenCont    -> Some (printChild x  (Some childrenCont))
    printChildren children



let createAcnBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnBoolean)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType) = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let Boolean         = match l with C -> uper_c.Boolean      | Ada -> uper_a.Boolean     | Python -> uper_p.Boolean
        let funcBodyContent = 
            Boolean pp errCode.errCodeName codec
        Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    (funcBody errCode), ns

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionCommon) (baseTypeUperFunc : AcnFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType) = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let pvalue = p.getValue l
        let ptr = p.getPointer l
        let Boolean         = match l with C -> uper_c.Boolean          | Ada -> uper_a.Boolean | Python -> uper_p.Boolean
        let acnBoolean      = match l with C -> acn_c.Boolean           | Ada -> acn_a.Boolean  | Python -> acn_p.Boolean
        let funcBodyContent = 
            match o.acnProperties.encodingPattern with
            | None  -> Boolean pp errCode.errCodeName codec
            | Some (TrueValue  bitVal)  ->
                let arrsBits = bitVal.Value.ToCharArray() |> Seq.map(fun x -> if x='0' then "0" else "1") |> Seq.toList
                let arrBytes = bitStringValueToByteArray bitVal
                let bEncValIsTrue, arruTrueValueAsByteArray, arruFalseValueAsByteArray, nSize =
                    true, arrBytes, (arrBytes |> Array.map (~~~)), bitVal.Value.Length
                acnBoolean pvalue ptr bEncValIsTrue (BigInteger nSize) arruTrueValueAsByteArray arruFalseValueAsByteArray arrsBits errCode.errCodeName codec
            | Some (FalseValue   bitVal)    ->
                let arrsBits = bitVal.Value.ToCharArray() |> Seq.map(fun x -> if x='0' then "0" else "1") |> Seq.toList
                let arrBytes = bitStringValueToByteArray bitVal
                let bEncValIsTrue, arruTrueValueAsByteArray, arruFalseValueAsByteArray, nSize =
                    false, arrBytes, (arrBytes |> Array.map (~~~)), bitVal.Value.Length
                acnBoolean pvalue ptr bEncValIsTrue (BigInteger nSize) arruTrueValueAsByteArray arruFalseValueAsByteArray arrsBits errCode.errCodeName codec
                
        {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  (fun e acnArgs p -> Some (funcBody e acnArgs p)) soSparkAnnotations us




let createAcnNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnNullType)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType) = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let nullType         = match l with C -> acn_c.Null          | Ada -> acn_a.Null_pattern2    | Python -> acn_p.Null
        match o.acnProperties.encodingPattern with
        | None      -> None
        | Some encPattern   ->
            let arrsBits = encPattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
            let arrBytes = bitStringValueToByteArray encPattern
            let ret = nullType pp arrBytes (BigInteger encPattern.Value.Length) arrsBits errCode.errCodeName codec
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []})
    (funcBody errCode), ns

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefinitionCommon) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType) = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let nullType         = match l with C -> acn_c.Null          | Ada -> acn_a.Null_pattern2    | Python -> acn_p.Null
        match o.acnProperties.encodingPattern with
        | None      -> None
        | Some encPattern   ->
            let arrsBits = encPattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
            let arrBytes = bitStringValueToByteArray encPattern
            let ret = nullType pp arrBytes (BigInteger encPattern.Value.Length) arrsBits errCode.errCodeName codec
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody soSparkAnnotations us


let getExternaField0 (l:ProgrammingLanguage) (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency func1 =
    let dependencies = deps.acnDependencies |> List.filter(fun d -> d.asn1Type = asn1TypeIdWithDependency && func1 d )
    match dependencies with
    | []                    -> ""
    | _                     ->
        let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = asn1TypeIdWithDependency && func1 d )
        let rec resolveParam (prmId:ReferenceToType) =
            let nodes = match prmId with ReferenceToType nodes -> nodes
            let lastNode = nodes |> List.rev |> List.head
            match lastNode with
            | PRM prmName   -> 
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
        getAcnDeterminantName  l (resolveParam dependency.determinant.id)

let getExternaFieldChoizePresentWhen (l:ProgrammingLanguage) (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency  relPath=
    let filterDependency (d:AcnDependency) =
        match d.dependencyKind with
        | AcnDepPresence (relPath0, _)   -> relPath = relPath0
        | _                              -> true
    getExternaField0 l r deps asn1TypeIdWithDependency filterDependency


let getExternaField (l:ProgrammingLanguage) (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
    getExternaField0 l r deps asn1TypeIdWithDependency (fun z -> true)

let createStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionCommon)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Acn_String_Ascii_FixSize                            = match l with C -> acn_c.Acn_String_Ascii_FixSize                          | Ada -> acn_a.Acn_String_Ascii_FixSize                         | Python -> acn_p.Acn_String_Ascii_FixSize
    let Acn_String_Ascii_Internal_Field_Determinant         = match l with C -> acn_c.Acn_String_Ascii_Internal_Field_Determinant       | Ada -> acn_a.Acn_String_Ascii_Internal_Field_Determinant      | Python -> acn_p.Acn_String_Ascii_Internal_Field_Determinant
    let Acn_String_Ascii_Null_Teminated                     = match l with C -> acn_c.Acn_String_Ascii_Null_Teminated                   | Ada -> acn_a.Acn_String_Ascii_Null_Teminated                  | Python -> acn_p.Acn_String_Ascii_Null_Teminated
    let Acn_String_Ascii_External_Field_Determinant         = match l with C -> acn_c.Acn_String_Ascii_External_Field_Determinant       | Ada -> acn_a.Acn_String_Ascii_External_Field_Determinant      | Python -> acn_p.Acn_String_Ascii_External_Field_Determinant
    let Acn_String_CharIndex_External_Field_Determinant     = match l with C -> acn_c.Acn_String_CharIndex_External_Field_Determinant   | Ada -> acn_a.Acn_String_CharIndex_External_Field_Determinant  | Python -> acn_p.Acn_String_CharIndex_External_Field_Determinant
    
    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType)        = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let funcBodyContent = 
            match o.acnEncodingClass with
            | Acn_Enc_String_uPER                                              -> uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes, x.localVariables)
            | Acn_Enc_String_uPER_Ascii                                        -> 
                match o.maxSize = o.minSize with
                | true      ->  Some (Acn_String_Ascii_FixSize pp (BigInteger o.maxSize) codec, [errCode], [])
                | false     ->  
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.maxSize - o.minSize))
                    Some (Acn_String_Ascii_Internal_Field_Determinant pp (BigInteger o.maxSize) (BigInteger o.minSize) nSizeInBits codec , [errCode], [])
            | Acn_Enc_String_Ascii_Null_Teminated                   nullChar   -> Some (Acn_String_Ascii_Null_Teminated pp (BigInteger o.maxSize) (nullChar.ToString()) codec, [errCode], [])
            | Acn_Enc_String_Ascii_External_Field_Determinant       _    -> 
                let extField = getExternaField l r deps t.id
                Some(Acn_String_Ascii_External_Field_Determinant pp (BigInteger o.maxSize) extField codec, [errCode], [])
            | Acn_Enc_String_CharIndex_External_Field_Determinant   _    -> 
                let extField = getExternaField l r deps t.id
                let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                let typeDefinitionName = getTypeDefinitionName t.id.tasInfo typeDefinition
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                Some(Acn_String_CharIndex_External_Field_Determinant pp (BigInteger o.maxSize) arrAsciiCodes (BigInteger o.uperCharSet.Length) extField typeDefinitionName nBits codec, [errCode], [])
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVars) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVars})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody soSparkAnnotations us


let createAcnStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToIA5String)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName
    let Acn_String_Ascii_FixSize                            = match l with C -> acn_c.Acn_String_Ascii_FixSize                          | Ada -> acn_a.Acn_String_Ascii_FixSize                         | Python -> acn_p.Acn_String_Ascii_FixSize
    let Acn_String_Ascii_Internal_Field_Determinant         = match l with C -> acn_c.Acn_String_Ascii_Internal_Field_Determinant       | Ada -> acn_a.Acn_String_Ascii_Internal_Field_Determinant      | Python -> acn_p.Acn_String_Ascii_Internal_Field_Determinant
    let Acn_String_Ascii_Null_Teminated                     = match l with C -> acn_c.Acn_String_Ascii_Null_Teminated                   | Ada -> acn_a.Acn_String_Ascii_Null_Teminated                  | Python -> acn_p.Acn_String_Ascii_Null_Teminated
    let Acn_String_Ascii_External_Field_Determinant         = match l with C -> acn_c.Acn_String_Ascii_External_Field_Determinant       | Ada -> acn_a.Acn_String_Ascii_External_Field_Determinant      | Python -> acn_p.Acn_String_Ascii_External_Field_Determinant
    let Acn_String_CharIndex_External_Field_Determinant     = match l with C -> acn_c.Acn_String_CharIndex_External_Field_Determinant   | Ada -> acn_a.Acn_String_CharIndex_External_Field_Determinant  | Python -> acn_p.Acn_String_CharIndex_External_Field_Determinant
    let typeDefinitionName = ToC2(r.args.TypePrefix + t.tasName.Value)

    let o = t.str
    let uper_funcBody (errCode:ErroCode) (p:FuncParamType) = 
        let i = sprintf "i%d" (typeId.SeqeuenceOfLevel + 1)
        let lv = SequenceOfIndex (typeId.SeqeuenceOfLevel + 1, None)
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
                | Python-> []
        let InternalItem_string_no_alpha    = match l with C -> uper_c.InternalItem_string_no_alpha     | Ada -> uper_a.InternalItem_string_no_alpha    | Python -> uper_p.InternalItem_string_no_alpha
        let InternalItem_string_with_alpha  = match l with C -> uper_c.InternalItem_string_with_alpha   | Ada -> uper_a.InternalItem_string_with_alpha  | Python -> uper_p.InternalItem_string_with_alpha
        let str_FixedSize       = match l with C -> uper_c.str_FixedSize    | Ada -> uper_a.str_FixedSize   | Python -> uper_p.str_FixedSize
        let str_VarSize         = match l with C -> uper_c.str_VarSize      | Ada -> uper_a.str_VarSize     | Python -> uper_p.str_VarSize
        //let Fragmentation_sqf   = match l with C -> uper_c.Fragmentation_sqf    | Ada -> uper_a.Fragmentation_sqf

        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
        let internalItem =
            match o.uperCharSet.Length = 128 with
            | true  -> InternalItem_string_no_alpha p.p i  codec 
            | false -> 
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                InternalItem_string_with_alpha p.p typeDefinitionName i (BigInteger (o.uperCharSet.Length-1)) arrAsciiCodes (BigInteger (o.uperCharSet.Length)) nBits  codec
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.maxSize - o.minSize))
        let funcBodyContent = 
            match o.minSize with
            | _ when o.maxSize < 65536 && o.maxSize=o.minSize  -> str_FixedSize p.p typeDefinitionName i internalItem (BigInteger o.minSize) nBits nBits 0I codec 
            | _ when o.maxSize < 65536 && o.maxSize<>o.minSize  -> str_VarSize p.p typeDefinitionName i internalItem (BigInteger o.minSize) (BigInteger o.maxSize) nSizeInBits nBits nBits 0I codec 
            | _                                                -> raise(Exception "fragmentation not implemented yet")
        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = lv::charIndex@nStringLength}    


    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType)        = 
        let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let funcBodyContent = 
            match t.str.acnEncodingClass with
            | Acn_Enc_String_uPER_Ascii                                        -> 
                match t.str.maxSize = t.str.minSize with
                | true      ->  Some (Acn_String_Ascii_FixSize pp (BigInteger t.str.maxSize) codec, [])
                | false     ->  
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.maxSize - o.minSize))
                    Some (Acn_String_Ascii_Internal_Field_Determinant pp (BigInteger t.str.maxSize) (BigInteger t.str.minSize) nSizeInBits codec , [])
            | Acn_Enc_String_Ascii_Null_Teminated                   nullChar   -> Some (Acn_String_Ascii_Null_Teminated pp (BigInteger t.str.maxSize) (nullChar.ToString()) codec, [])
            | Acn_Enc_String_Ascii_External_Field_Determinant       _    -> 
                let extField = getExternaField l r deps typeId
                Some(Acn_String_Ascii_External_Field_Determinant pp (BigInteger t.str.maxSize) extField codec, [])
            | Acn_Enc_String_CharIndex_External_Field_Determinant   _    -> 
                let extField = getExternaField l r deps typeId
                let arrAsciiCodes = t.str.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (t.str.uperCharSet.Length-1))
                Some(Acn_String_CharIndex_External_Field_Determinant pp (BigInteger t.str.maxSize) arrAsciiCodes (BigInteger t.str.uperCharSet.Length) extField typeDefinitionName nBits codec, [])
            | Acn_Enc_String_uPER                                              -> 
                let x = (uper_funcBody errCode) p 
                Some(x.funcBody, x.errCodes)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::errCodes; localVariables = []})


    (funcBody errCode), ns


let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionCommon) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let oct_sqf_external_field          = match l with C -> acn_c.oct_sqf_external_field       | Ada -> acn_a.oct_sqf_external_field    | Python -> acn_p.oct_external_field
    let InternalItem_oct_str            = match l with C -> uper_c.InternalItem_oct_str        | Ada -> uper_a.InternalItem_oct_str     | Python -> uper_p.InternalItem_oct_str
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
    let nAlignSize = 0I;

    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType)        = 
        let funcBodyContent = 
            match o.acnEncodingClass with
            | SZ_EC_uPER                                              -> uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes, x.localVariables)
            | SZ_EC_ExternalField   _    -> 
                let extField = getExternaField l r deps t.id
                let internalItem = InternalItem_oct_str p.p (p.getAcces l) i  errCode.errCodeName codec 
                let fncBody = oct_sqf_external_field p.p (p.getAcces l) i internalItem (if o.minSize=0 then None else Some (BigInteger o.minSize)) (BigInteger o.maxSize) extField nAlignSize errCode.errCodeName 8I 8I codec
                Some(fncBody, [errCode],[])
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = lv::localVariables})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody soSparkAnnotations us

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionCommon)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let nAlignSize = 0I;

    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType)        = 
        //let pp = match codec with CommonTypes.Encode -> p.getValue l | CommonTypes.Decode -> p.getPointer l
        let funcBodyContent = 
            match o.acnEncodingClass with
            | SZ_EC_uPER                                              -> uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes, x.localVariables)
            | SZ_EC_ExternalField   _    -> 
                let extField = getExternaField l r deps t.id
                match l with
                | C     ->
                    let fncBody = acn_c.bit_string_external_field p.p (p.getAcces l) (if o.minSize=0 then None else Some (BigInteger o.minSize)) (BigInteger o.maxSize) extField codec
                    Some(fncBody, [errCode], [])
                | Ada   ->
                    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
                    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
                    let internalItem = uper_a.InternalItem_bit_str p.p i  errCode.errCodeName codec 
                    let fncBody = acn_a.oct_sqf_external_field p.p (p.getAcces l) i internalItem (if o.minSize=0 then None else Some (BigInteger o.minSize)) (BigInteger o.maxSize) extField nAlignSize errCode.errCodeName 1I 1I codec
                    Some(fncBody, [errCode], [lv])
                | Python ->
                    let fncBody = acn_p.bit_string_external_field p.p (p.getAcces l) (if o.minSize=0 then None else Some (BigInteger o.minSize)) (BigInteger o.maxSize) extField codec
                    Some(fncBody, [errCode], [])
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody soSparkAnnotations us



let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionCommon) (isValidFunc: IsValidFunction option)  (child:Asn1Type) (us:State)  =
    let external_field          = match l with C -> acn_c.oct_sqf_external_field       | Ada -> acn_a.oct_sqf_external_field    | Python -> acn_p.sequenceOf_external_field
    let fixedSize               = match l with C -> uper_c.octect_FixedSize            | Ada -> uper_a.octect_FixedSize         | Python -> acn_p.sequenceOf_FixedSize
    let varSize                 = match l with C -> uper_c.octect_VarSize              | Ada -> uper_a.octect_VarSize           | Python -> acn_p.sequenceOf_VarSize
    

    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
    let nAlignSize = 0I;

    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType)        = 
        match child.getAcnFunction codec with
        | None         -> None
        | Some chFunc  ->
            let internalItem = chFunc.funcBody acnArgs (p.getArrayItem l i child.isIA5String)
            match o.acnEncodingClass with
            | SZ_EC_uPER                                              -> 
                let nStringLength =
                    match o.minSize = o.maxSize,  l, codec with
                    | true , _,_    -> []
                    | false, Ada, _ -> [IntegerLocalVariable ("nStringLength", None)]
                    | false, C, Encode -> []
                    | false, C, Decode -> [Asn1SIntLocalVariable ("nCount", None)]
                    | _, Python, _  -> []
(*
                    match codec, o.minSize = o.maxSize with
                    | Encode, _  -> []
                    | Decode, true  -> []
                    | Decode,false ->
                        match l with
                        | Ada  -> [IntegerLocalVariable ("nStringLength", None)]
                        | C    -> [Asn1SIntLocalVariable ("nCount", None)]
*)
                match internalItem with
                | None  -> 
                        match o.minSize with
                        | _ when o.maxSize < 65536 && o.maxSize=o.minSize  -> None
                        | _ when o.maxSize < 65536 && o.maxSize<>o.minSize -> Some ({AcnFuncBodyResult.funcBody = "todo Encode only length"; errCodes = [errCode]; localVariables = nStringLength})    
                        | _                                                -> raise(Exception "fragmentation not implemented yet")
                | Some internalItem -> 
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.maxSize - o.minSize))
                    let localVariables = internalItem.localVariables
                    let childErrCodes =  internalItem.errCodes
                    let typeDefinitionName = getTypeDefinitionName t.id.tasInfo typeDefinition
                    let ret = 
                        match o.minSize with
                        | _ when o.maxSize < 65536 && o.maxSize=o.minSize  -> fixedSize p.p typeDefinitionName i internalItem.funcBody (BigInteger o.minSize) (BigInteger child.uperMinSizeInBits) (BigInteger child.uperMaxSizeInBits) 0I codec 
                        | _ when o.maxSize < 65536 && o.maxSize<>o.minSize  -> varSize p.p (p.getAcces l)  typeDefinitionName i internalItem.funcBody (BigInteger o.minSize) (BigInteger o.maxSize) nSizeInBits (BigInteger child.uperMinSizeInBits) (BigInteger child.uperMaxSizeInBits) 0I errCode.errCodeName codec 
                        | _                                                -> raise(Exception "fragmentation not implemented yet")
                    Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = lv::(nStringLength@localVariables)})    

            | SZ_EC_ExternalField   _    -> 
                match internalItem with
                | None  -> None
                | Some internalItem ->
                    let localVariables  = internalItem.localVariables
                    let childErrCodes   = internalItem.errCodes
                    let internalItem    = internalItem.funcBody
                    let extField        = getExternaField l r deps t.id
                    let funcBodyContent = external_field p.p (p.getAcces l) i internalItem (if o.minSize=0 then None else Some (BigInteger o.minSize)) (BigInteger o.maxSize) extField nAlignSize errCode.errCodeName o.child.acnMinSizeInBits.AsBigInt o.child.acnMaxSizeInBits.AsBigInt  codec
                    Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = lv::localVariables})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody soSparkAnnotations us


let rec handleSingleUpdateDependency (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (m:Asn1AcnAst.Asn1Module) (d:AcnDependency)  (us:State) =
    let presenceDependency              = match l with C -> acn_c.PresenceDependency                | Ada -> acn_a.PresenceDependency               | Python -> acn_p.PresenceDependency
    let sizeDependency                  = match l with C -> acn_c.SizeDependency                    | Ada -> acn_a.SizeDependency                   | Python -> acn_p.SizeDependency
    let getSizeableSize                 = match l with C -> uper_c.getSizeableSize                  | Ada -> acn_a.getSizeableSize                  | Python -> uper_p.getSizeableSize
    let getStringSize                   = match l with C -> uper_c.getStringSize                    | Ada -> acn_a.getStringSize                    | Python -> uper_p.getStringSize
    let choiceDependencyPres            = match l with C -> acn_c.ChoiceDependencyPres              | Ada -> acn_a.ChoiceDependencyPres             | Python -> acn_p.ChoiceDependencyPres
    let choiceDependencyIntPres_child   = match l with C -> acn_c.ChoiceDependencyIntPres_child     | Ada -> acn_a.ChoiceDependencyIntPres_child    | Python -> acn_p.ChoiceDependencyIntPres_child
    let choiceDependencyStrPres_child   = match l with C -> acn_c.ChoiceDependencyStrPres_child     | Ada -> acn_a.ChoiceDependencyStrPres_child    | Python -> acn_p.ChoiceDependencyStrPres_child
    let choiceDependencyEnum            = match l with C -> acn_c.ChoiceDependencyEnum              | Ada -> acn_a.ChoiceDependencyEnum             | Python -> acn_p.ChoiceDependencyEnum
    let choiceDependencyEnum_Item       = match l with C -> acn_c.ChoiceDependencyEnum_Item         | Ada -> acn_a.ChoiceDependencyEnum_Item        | Python -> acn_p.ChoiceDependencyEnum_Item
    let checkAccessPath                 = match l with C -> acn_c.checkAccessPath                   | Ada -> acn_a.checkAccessPath                  | Python -> acn_p.checkAccessPath
    

    match d.dependencyKind with
    | AcnDepRefTypeArgument           acnPrm   -> 
        let prmUpdateStatement, ns1 = getUpdateFunctionUsedInEncoding r deps l m acnPrm.id us
        match prmUpdateStatement with
        | None  -> None, ns1
        | Some prmUpdateStatement   -> 
            let updateFunc (vTarget : FuncParamType) (pSrcRoot : FuncParamType)  = 
                prmUpdateStatement.func vTarget pSrcRoot
            Some ({AcnChildUpdateResult.func = updateFunc; errCodes=prmUpdateStatement.errCodes}), ns1
    | AcnDepSizeDeterminant         -> 
        let updateFunc (vTarget : FuncParamType) (pSrcRoot : FuncParamType)  = 
            let pSizeable, checkPath = getAccessFromScopeNodeList d.asn1Type false l pSrcRoot
            let updateStatement = sizeDependency (vTarget.getValue l) (getSizeableSize pSizeable.p (pSizeable.getAcces l))
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        Some ({AcnChildUpdateResult.func = updateFunc; errCodes=[]}), us
    | AcnDepIA5StringSizeDeterminant    ->
        let updateFunc (vTarget : FuncParamType) (pSrcRoot : FuncParamType)  = 
            let pSizeable, checkPath = getAccessFromScopeNodeList d.asn1Type true l pSrcRoot
            let updateStatement = sizeDependency (vTarget.getValue l) (getStringSize pSizeable.p)
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        Some ({AcnChildUpdateResult.func = updateFunc; errCodes=[]}), us
    | AcnDepPresenceBool              -> 
        let updateFunc (vTarget : FuncParamType) (pSrcRoot : FuncParamType)  = 
            let v = vTarget.getValue l
            let parDecTypeSeq =
                match d.asn1Type with
                | ReferenceToType (nodes) -> ReferenceToType (nodes |> List.rev |> List.tail |> List.rev)
            let pDecParSeq, checkPath = getAccessFromScopeNodeList parDecTypeSeq false l pSrcRoot
            let updateStatement = presenceDependency v (pDecParSeq.p) (pDecParSeq.getAcces l) (ToC d.asn1Type.lastItem)
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        Some ({AcnChildUpdateResult.func = updateFunc; errCodes=[]}), us
    | AcnDepPresence   (relPath, chc)               -> 
        let updateFunc (vTarget : FuncParamType) (pSrcRoot : FuncParamType)  = 
            let v = vTarget.getValue l
            let choicePath, checkPath = getAccessFromScopeNodeList d.asn1Type false l pSrcRoot
            let arrsChildUpdates = 
                chc.children |> 
                List.map(fun ch -> 
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    let name = match l with C | Ada -> ch.presentWhenName | Python -> ToCPy(ch.Name.Value)
                    match pres with
                    | PresenceInt   (_, intVal) -> choiceDependencyIntPres_child v name intVal.Value
                    | PresenceStr   (_, strVal) -> choiceDependencyStrPres_child v name strVal.Value )
            let acc = match l with C | Ada -> (choicePath.getAcces l) | Python -> v
            let updateStatement = choiceDependencyPres choicePath.p acc arrsChildUpdates
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        Some ({AcnChildUpdateResult.func = updateFunc; errCodes=[]}), us
    | AcnDepChoiceDeteterminant       (enm,chc)      -> 
        let updateFunc (vTarget : FuncParamType) (pSrcRoot : FuncParamType)  = 
            let v = vTarget.getValue l
            let choicePath, checkPath = getAccessFromScopeNodeList d.asn1Type false l pSrcRoot
            let arrsChildUpdates = 
                chc.children |> 
                List.map(fun ch -> 
                    let enmItem = enm.items |> List.find(fun itm -> itm.Name.Value = ch.Name.Value)
                    let enumName = match l with C | Ada -> enmItem.getBackendName l | Python -> enmItem.definitionValue.ToString()
                    let childEnumName = match l with C | Ada -> ch.presentWhenName | Python -> ToCPy(ch.Name.Value)
                    choiceDependencyEnum_Item v childEnumName enumName )
            let updateStatement = choiceDependencyEnum choicePath.p (choicePath.getAcces l) arrsChildUpdates
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        Some ({AcnChildUpdateResult.func = updateFunc; errCodes=[]}), us

and getUpdateFunctionUsedInEncoding (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (m:Asn1AcnAst.Asn1Module) (acnChildOrAcnParameterId) (us:State) : (AcnChildUpdateResult option*State)=
    let multiAcnUpdate       = match l with C -> acn_c.MultiAcnUpdate          | Ada -> acn_a.MultiAcnUpdate    | Python -> acn_p.MultiAcnUpdate

    match deps.acnDependencies |> List.filter(fun d -> d.determinant.id = acnChildOrAcnParameterId) with
    | []  -> None, us
    | d1::[]    -> 
        let ret, ns = handleSingleUpdateDependency r deps l m d1 us
        ret, ns
    | d1::dds         -> 
        let _errCodeName         = ToC ("ERR_ACN" + (Encode.suffix.ToUpper()) + "_UPDATE_" + ((acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errCode, us = getNextValidErrorCode us _errCodeName


        let ds = d1::dds
        let c_name0 = sprintf "%s%02d" (getAcnDeterminantName l acnChildOrAcnParameterId) 0
        let localVars = 
            ds |> 
            List.mapi(fun i d1 -> 
                let c_name = sprintf "%s%02d" (getAcnDeterminantName l acnChildOrAcnParameterId) i
                let typegetDeterminantTypeDefinitionBodyWithinSeq = getDeterminantTypeDefinitionBodyWithinSeq r l d1.determinant
                [AcnInsertedChild (c_name, typegetDeterminantTypeDefinitionBodyWithinSeq); BooleanLocalVariable (c_name+"_is_initialized", Some false)]) |>
            List.collect(fun lvList -> lvList |> List.map (fun lv -> lv.GetDeclaration l))
        let localUpdateFuns,ns =
            ds |>
            List.fold(fun (updates, ns) d1 -> 
                let f1, nns = handleSingleUpdateDependency r deps l m d1 ns 
                updates@[f1], nns) ([],us)

        let multiUpdateFunc (vTarget : FuncParamType) (pSrcRoot : FuncParamType)  = 
            let v = vTarget.getValue l
            let arrsLocalUpdateStatements = 
                localUpdateFuns |> 
                List.mapi(fun i fn -> 
                    let c_name = sprintf "%s%02d" (getAcnDeterminantName l acnChildOrAcnParameterId) i
                    let lv = VALUE c_name
                    match fn with
                    | None      -> None
                    | Some fn   -> Some(fn.func lv pSrcRoot)) |>
                List.choose id
            let v0 = sprintf "%s%02d" (getAcnDeterminantName l acnChildOrAcnParameterId) 0
            let arrsGetFirstIntValue =
                ds |>
                List.mapi (fun i d -> 
                    let cmp = getDeterminantTypeUpdateMacro r l d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName l acnChildOrAcnParameterId) i
                    cmp v vi (i=0) )
            let arrsLocalCheckEquality = 
                ds |> 
                List.mapi (fun i d -> 
                    let cmp = getDeterminantTypeCheckEqual r l d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName l acnChildOrAcnParameterId) i
                    cmp v vi )
            let updateStatement = multiAcnUpdate vTarget.p c_name0 errCode.errCodeName localVars arrsLocalUpdateStatements arrsGetFirstIntValue arrsLocalCheckEquality
            updateStatement

        let ret = Some(({AcnChildUpdateResult.func = multiUpdateFunc; errCodes=[errCode]}))
        ret, ns

type private AcnSequenceStatement =
    | AcnPresenceStatement
    | Asn1ChildEncodeStatement
    | AcnChildUpdateStatement
    | AcnChildEncodeStatement

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionCommon) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (acnPrms:AcnParameter list) (us:State)  =
    (*
        1. all Acn inserted children are declared as local variables in the encoded and decode functions (declaration step)
        2. all Acn inserted children must be initialized appropriatelly in the encoding phase
        3. 
    *)

    // stg macros
    let sequence_presense_optChild              = match l with C -> acn_c.sequence_presense_optChild             | Ada -> acn_a.sequence_presense_optChild              | Python -> acn_p.sequence_presense_optChild
    let sequence_presense_optChild_pres_bool    = match l with C -> acn_c.sequence_presense_optChild_pres_bool   | Ada -> acn_a.sequence_presense_optChild_pres_bool    | Python -> acn_p.sequence_presense_optChild_pres_bool
    let sequence_presense_optChild_pres_int     = match l with C -> acn_c.sequence_presense_optChild_pres_int    | Ada -> acn_a.sequence_presense_optChild_pres_int     | Python -> acn_p.sequence_presense_optChild_pres_int
    let sequence_presense_optChild_pres_str     = match l with C -> acn_c.sequence_presense_optChild_pres_str    | Ada -> acn_a.sequence_presense_optChild_pres_str     | Python -> acn_p.sequence_presense_optChild_pres_str
    let sequence_mandatory_child                = match l with C -> acn_c.sequence_mandatory_child               | Ada -> acn_a.sequence_mandatory_child                | Python -> acn_p.sequence_mandatory_child
    let sequence_optional_child                 = match l with C -> acn_c.sequence_optional_child                | Ada -> acn_a.sequence_optional_child                 | Python -> acn_p.sequence_optional_child
    let sequence_optional_always_present        = match l with C -> acn_c.sequence_optional_always_present_child | Ada -> acn_a.sequence_optional_always_present_child  | Python -> acn_p.sequence_optional_always_present_child
    let sequence_default_child                  = match l with C -> acn_c.sequence_default_child                 | Ada -> acn_a.sequence_default_child                  | Python -> acn_p.sequence_default_child
    let sequence_acn_child                      = match l with C -> acn_c.sequence_acn_child                     | Ada -> acn_a.sequence_acn_child                      | Python -> acn_p.sequence_acn_child
    //let baseFuncName =  match baseTypeUperFunc  with None -> None | Some baseFunc -> baseFunc.funcName

    let acnChildren = children |>  List.choose(fun x -> match x with AcnChild z -> Some z | Asn1Child _ -> None)
    let asn1Children = children |>  List.choose(fun x -> match x with Asn1Child z -> Some z | AcnChild _ -> None)

    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType) = 
        let acnlocalVariablesCh = 
            acnChildren |>  
            List.filter(fun x -> match x.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> true) |>  
            List.collect(fun x -> 
                match codec with
                | Encode     -> [AcnInsertedChild(x.c_name, x.typeDefinitionBodyWithinSeq); BooleanLocalVariable(x.c_name+"_is_initialized", Some false)]
                | Decode    -> [AcnInsertedChild(x.c_name, x.typeDefinitionBodyWithinSeq)])
        
        let acnlocalVariablesPrms = 
            match t.id.tasInfo with
            | Some  _ -> [] // if the encoding type is a top level type (i.e. TAS) then the encodig parameters are transformed into function parameters and not local variables.
            | None    -> [] // acnPrms |>  List.map(fun x -> AcnInsertedChild(x.c_name, x.typeDefinitionBodyWithinSeq))
        let acnlocalVariables = acnlocalVariablesCh @ acnlocalVariablesPrms
        //let acnParams =  r.acnParameters |> List.filter(fun  prm -> prm.ModName ) 

        let printPresenceBit (child:Asn1Child) =
            match child.Optionality with
            | None                       -> None
            | Some Asn1AcnAst.AlwaysAbsent     -> None
            | Some Asn1AcnAst.AlwaysPresent    -> None
            | Some (Asn1AcnAst.Optional opt)   -> 
                match opt.acnPresentWhen with
                | None      -> Some (sequence_presense_optChild p.p (p.getAcces l) child.c_name  errCode.errCodeName codec)
                | Some _    -> None

        let localVariables =
            match asn1Children |> List.choose  printPresenceBit with
            | x::_  when l = C  && codec = CommonTypes.Decode -> (FlagLocalVariable ("presenceBit", None))::acnlocalVariables
            | _        -> acnlocalVariables
                    
        let handleChild (child:SeqChildInfo) =
            //let chFunc = child.chType.getAcnFunction codec
            seq {
                match child with
                | Asn1Child child   -> 
                    let chFunc = child.Type.getAcnFunction codec
                    let childContentResult = 
                        match chFunc with
                        | Some chFunc   -> chFunc.funcBody [] (p.getSeqChild l child.c_name child.Type.isIA5String)
                        | None          -> None

                    //handle present-when acn property
                    match codec with
                    | Codec.Encode  -> ()
                    | Codec.Decode  ->
                        let acnPresenceStatement = 
                            match child.Optionality with
                            | Some (Asn1AcnAst.Optional opt)   -> 
                                match opt.acnPresentWhen with
                                | None    -> None
                                | Some (PresenceWhenBool _)    -> 
                                    let extField = getExternaField l r deps child.Type.id
                                    Some(sequence_presense_optChild_pres_bool p.p (p.getAcces l) child.c_name extField codec)
                            | _                 -> None
                        yield (AcnPresenceStatement, acnPresenceStatement, [], [])


                    match childContentResult with
                    | None              -> ()
                    | Some childContent ->
                        let childBody = 
                            match child.Optionality with
                            | None                             -> Some (sequence_mandatory_child child.c_name childContent.funcBody codec)
                            | Some Asn1AcnAst.AlwaysAbsent     -> match codec with CommonTypes.Encode -> None                        | CommonTypes.Decode -> Some (sequence_optional_child p.p (p.getAcces l) child.c_name childContent.funcBody codec) 
                            | Some Asn1AcnAst.AlwaysPresent    -> match codec with CommonTypes.Encode -> Some childContent.funcBody  | CommonTypes.Decode -> Some (sequence_optional_child p.p (p.getAcces l) child.c_name childContent.funcBody codec)
                            | Some (Asn1AcnAst.Optional opt)   -> 
                                match opt.defaultValue with
                                | None                   -> Some (sequence_optional_child p.p (p.getAcces l) child.c_name childContent.funcBody codec)
                                | Some v                 -> 
                                    let defInit= child.Type.initFunction.initFuncBody (p.getSeqChild l child.c_name child.Type.isIA5String) (mapValue v).kind
                                    Some (sequence_default_child p.p (p.getAcces l) child.c_name childContent.funcBody defInit codec) 
                        yield (Asn1ChildEncodeStatement, childBody, childContent.localVariables, childContent.errCodes)

                | AcnChild  acnChild    -> 
                    //handle updates
                    //acnChild.c_name
                    let childP = VALUE (getAcnDeterminantName l acnChild.id)

                    match codec with
                    | CommonTypes.Encode -> 
                        let pRoot : FuncParamType = t.getParamType l codec  //????
                        let updateStatement, lvs, lerCodes = 
                            match acnChild.funcUpdateStatement with
                            | Some funcUpdateStatement -> Some (funcUpdateStatement.func childP pRoot), [], funcUpdateStatement.errCodes
                            | None                     -> None, [], []

                        yield (AcnChildUpdateStatement, updateStatement, lvs, lerCodes)
                    | CommonTypes.Decode -> ()

                    //acn child encode/decode
                    let chFunc = acnChild.funcBody codec
                    let childContentResult = chFunc []  childP
                    match childContentResult with
                    | None              -> ()
                    | Some childContent ->
                        match codec with
                        | Encode   ->
                            match acnChild.Type with
                            | Asn1AcnAst.AcnNullType _   ->
                                let childBody = Some (sequence_mandatory_child acnChild.c_name childContent.funcBody codec)
                                yield (AcnChildEncodeStatement, childBody, childContent.localVariables, childContent.errCodes)
                            | _             ->
                                let _errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((acnChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_UNITIALIZED")
                                let errCode, ns = getNextValidErrorCode us _errCodeName
                        
                                let childBody = Some (sequence_acn_child acnChild.c_name childContent.funcBody errCode.errCodeName codec)
                                yield (AcnChildEncodeStatement, childBody, childContent.localVariables, errCode::childContent.errCodes)
                        | Decode    ->
                            let childBody = Some (sequence_mandatory_child acnChild.c_name childContent.funcBody codec)
                            yield (AcnChildEncodeStatement, childBody, childContent.localVariables, childContent.errCodes)

            } |> Seq.toList

        // find acn inserted fields, which are not NULL types and which have no dependency.
        // For those fields we shoud generated no anc encode/decode function
        // Otherwise, the encoding function is wrong since an unitialized value is encoded.
        let existsAcnChildWithNoUpdates =
            acnChildren |>
            List.filter (fun acnChild -> match acnChild.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> true) |>
            List.exists(fun acnChild -> 
                let childP = VALUE (getAcnDeterminantName l acnChild.id)
                let pRoot : FuncParamType = t.getParamType l codec  
                let updateStatement = 
                    match acnChild.funcUpdateStatement with
                    | Some funcUpdateStatement -> Some (funcUpdateStatement.func childP pRoot)
                    | None                     -> None
                updateStatement.IsNone)

        let presenseBits = asn1Children |> List.choose printPresenceBit
        let childrenStatements0 = children |> List.collect handleChild
        let childrenStatements = childrenStatements0 |> List.choose(fun (_, s,_,_) -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_, _,s,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_, _,_,s) -> s)
        let seqContent =  (presenseBits@childrenStatements) |> nestChildItems l codec 
        match existsAcnChildWithNoUpdates with
        | true      -> None
        | false     ->
            match seqContent with
            | None  -> None
            | Some ret -> Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars})    
            
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> None
        | Python-> None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody soSparkAnnotations  us


type private AcnChoiceEncClass =
    | CEC_uper
    | CEC_enum          of Asn1AcnAst.Enumerated
    | CEC_presWhen

let createChoiceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionCommon) (defOrRef:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (acnPrms:AcnParameter list)  (us:State)  =
    let choice_uper          =  match l with C -> acn_c.Choice                | Ada -> acn_a.Choice                 | Python -> acn_p.Choice
    let choiceChild          =  match l with C -> acn_c.ChoiceChild           | Ada -> acn_a.ChoiceChild            | Python -> acn_p.ChoiceChild
    let choice_Enum          =  match l with C -> acn_c.Choice_Enum           | Ada -> acn_a.Choice_Enum            | Python -> acn_p.Choice_Enum
    let choiceChild_Enum     =  match l with C -> acn_c.ChoiceChild_Enum      | Ada -> acn_a.ChoiceChild_Enum       | Python -> acn_p.ChoiceChild_Enum
    let choice_preWhen       =  match l with C -> acn_c.Choice_preWhen        | Ada -> acn_a.Choice_preWhen         | Python -> acn_p.Choice_preWhen
    let choiceChild_preWhen  =  match l with C -> acn_c.ChoiceChild_preWhen   | Ada -> acn_a.ChoiceChild_preWhen    | Python -> acn_p.ChoiceChild_preWhen
    let choiceChild_preWhen_int_condition  =  match l with C -> acn_c.ChoiceChild_preWhen_int_condition   | Ada -> acn_a.ChoiceChild_preWhen_int_condition  | Python -> acn_p.ChoiceChild_preWhen_int_condition
    let choiceChild_preWhen_str_condition  =  match l with C -> acn_c.ChoiceChild_preWhen_str_condition   | Ada -> acn_a.ChoiceChild_preWhen_str_condition  | Python -> acn_p.ChoiceChild_preWhen_str_condition


    let nMin = 0I
    let nMax = BigInteger(Seq.length children) - 1I
    //let nBits = (GetNumberOfBitsForNonNegativeInteger (nMax-nMin))
    let nIndexSizeInBits = (GetNumberOfBitsForNonNegativeInteger (BigInteger (children.Length - 1)))
    let sChoiceIndexName = typeDefinition.name + "_index_tmp"

    let ec =  
        match o.acnProperties.enumDeterminant with
        | Some _            -> 
            let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = t.id)
            match dependency.dependencyKind with
            | Asn1AcnAst.AcnDepChoiceDeteterminant (enm,_)  -> CEC_enum enm
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

    let typeDefinitionName = defOrRef.longTypedefName l//getTypeDefinitionName t.id.tasInfo typeDefinition

    let funcBody (errCode:ErroCode) (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType) = 
        let handleChild (idx:int) (child:ChChildInfo) =
            seq {
                let chFunc = child.chType.getAcnFunction codec
                let childContentResult = 
                    match chFunc with
                    | Some chFunc   -> 
                        match l with
                        | C   ->  chFunc.funcBody [] (p.getChChild l child.c_name child.chType.isIA5String)
                        | Ada when codec = CommonTypes.Decode  ->  chFunc.funcBody [] (VALUE (child.c_name + "_tmp"))
                        | Ada   ->  chFunc.funcBody [] (p.getChChild l child.c_name child.chType.isIA5String)
                        | Python -> chFunc.funcBody [] (p.getChChild l child.c_name child.chType.isIA5String)
                    | None          -> None

                match childContentResult with
                | None              -> ()
                | Some childContent ->
                    let childBody = 
                        let sChildName = child.c_name
                        let sChildTypeDef = child.chType.typeDefintionOrReference.longTypedefName l //child.chType.typeDefinition.typeDefinitionBodyWithinSeq
                        let sChoiceTypeName = typeDefinitionName
                        match ec with
                        | CEC_uper  -> 
                            Some (choiceChild p.p (p.getAcces l) child.presentWhenName (BigInteger idx) nIndexSizeInBits nMax childContent.funcBody sChildName sChildTypeDef sChoiceTypeName codec)
                        | CEC_enum enm -> 
                            let enmItem = enm.items |> List.find(fun itm -> itm.Name.Value = child.Name.Value)
                            let enumName = match l with C | Ada -> (enmItem.getBackendName l) | Python -> enmItem.definitionValue.ToString()
                            Some (choiceChild_Enum p.p (p.getAcces l) enumName child.presentWhenName childContent.funcBody sChildName sChildTypeDef sChoiceTypeName codec)
                        | CEC_presWhen  ->
                            let handPresenseCond (cond:Asn1AcnAst.AcnPresentWhenConditionChoiceChild) =
                                match cond with
                                | PresenceInt  (relPath, intLoc)   -> 
                                    let extField = getExternaFieldChoizePresentWhen l r deps t.id relPath
                                    choiceChild_preWhen_int_condition extField intLoc.Value
                                | PresenceStr  (relPath, strVal)   -> 
                                    let extField = getExternaFieldChoizePresentWhen l r deps t.id relPath
                                    choiceChild_preWhen_str_condition extField strVal.Value
                            let conds = child.acnPresentWhenConditions |>List.map handPresenseCond
                            Some (choiceChild_preWhen p.p (p.getAcces l) child.presentWhenName childContent.funcBody conds (idx=0) sChildName sChildTypeDef sChoiceTypeName codec)
                    yield (childBody, childContent.localVariables, childContent.errCodes)
            } |> Seq.toList

        let childrenStatements0 = children |> List.mapi handleChild |> List.collect id
        let childrenStatements = childrenStatements0 |> List.choose(fun (s,_,_) -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s) -> s)

        let choiceContent =  
            match ec with
            | CEC_uper        -> 
                //let ret = choice p.p (p.getAcces l) childrenContent (BigInteger (children.Length - 1)) sChoiceIndexName errCode.errCodeName typeDefinitionName nBits  codec
                choice_uper p.p (p.getAcces l) childrenStatements nMax sChoiceIndexName typeDefinitionName nIndexSizeInBits errCode.errCodeName codec
            | CEC_enum   enm  -> 
                let extField = getExternaField l r deps t.id
                choice_Enum p.p (p.getAcces l) childrenStatements extField errCode.errCodeName codec
            | CEC_presWhen    -> choice_preWhen p.p  (p.getAcces l) childrenStatements errCode.errCodeName codec
        Some ({AcnFuncBodyResult.funcBody = choiceContent; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars})    


    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> None
        | Python   -> None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody soSparkAnnotations  us

let createReferenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionCommon) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
    match codec with
    | Codec.Encode  -> baseType.getAcnFunction codec, us
    | Codec.Decode  -> 
        let paramsArgsPairs = List.zip o.acnArguments o.resolvedType.acnParameters
        let baseTypeAcnFunction = baseType.getAcnFunction codec 
        let ret =
            match baseTypeAcnFunction with
            | None  -> None
            | Some baseTypeAcnFunction   ->
                let funcBody (acnArgs: (Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) (p:FuncParamType) = 
                    baseTypeAcnFunction.funcBody (acnArgs@paramsArgsPairs) p
                Some  {baseTypeAcnFunction with funcBody = funcBody} 

        ret, us


(*
-- 
TEST-CASE DEFINITIONS ::= BEGIN
        
    MyPDU[] {    
         tap[]        
    }
    
    
    TAP3File[]{
         headerType [],  
         sourceData <header.nrCalls> []
    }
    
    HeaderType[]{
        operatorID[],
        nrCalls CallsSize []
    }          
             
    SourceData<INTEGER:nElements2>[]
    {

         calls<nElements>[]
    }
    
    CallsArray<INTEGER:nElements>[size nElements]
    
    Call[]
END  

*)