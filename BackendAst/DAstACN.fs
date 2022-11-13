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

let sparkAnnotations (lm:LanguageMacros)  = lm.acn.sparkAnnotations


let getAcnDeterminantName (id : ReferenceToType) =
    match id with
    | ReferenceToType path ->
        match path with
        | (MD mdName)::(TA tasName)::(PRM prmName)::[]   -> ToC2 prmName
        | _ ->
            let longName = id.AcnAbsPath.Tail |> Seq.StrJoin "_"
            ToC2(longName.Replace("#","elem"))


let getDeterminantTypeDefinitionBodyWithinSeq (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (det:Determinant) = 
    let createPrmAcnInteger (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  =
        let Declare_Integer     =  lm.typeDef.Declare_Integer 
        Declare_Integer ()

    let createAcnInteger (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (a:Asn1AcnAst.AcnInteger) =
        let intClass = getAcnIntegerClass r.args a
        let stgMacro = DAstTypeDefinition.getIntererTypeByClass lm intClass
        stgMacro ()
        (*
        let Declare_Integer     =  lm.typeDef.Declare_Integer 
        let Declare_PosInteger  =  lm.typeDef.Declare_PosInteger  
        match a.isUnsigned with
        | true     -> Declare_PosInteger ()
        | false    -> Declare_Integer ()
        *)
    
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


let getDeterminant_macro (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (det:Determinant) pri_macro str_macro = 
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
        |AcnGenericTypes.AcnPrmInteger  _       -> pri_macro
        |AcnGenericTypes.AcnPrmBoolean  _       -> pri_macro
        |AcnGenericTypes.AcnPrmNullType _       -> pri_macro
        |AcnGenericTypes.AcnPrmRefType (md,ts)  -> pri_macro

let getDeterminantTypeUpdateMacro (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (det:Determinant) = 
    let MultiAcnUpdate_get_first_init_value_pri     =  lm.acn.MultiAcnUpdate_get_first_init_value_pri
    let MultiAcnUpdate_get_first_init_value_str     =  lm.acn.MultiAcnUpdate_get_first_init_value_str
    getDeterminant_macro r lm det MultiAcnUpdate_get_first_init_value_pri MultiAcnUpdate_get_first_init_value_str

let getDeterminantTypeCheckEqual (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (det:Determinant) = 
    let multiAcnUpdate_checkEqual_pri     =  lm.acn.MultiAcnUpdate_checkEqual_pri
    let multiAcnUpdate_checkEqual_str     =  lm.acn.MultiAcnUpdate_checkEqual_str
    getDeterminant_macro r lm det multiAcnUpdate_checkEqual_pri multiAcnUpdate_checkEqual_str
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

let handleSavePostion (funcBody:State-> ErroCode->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> ((AcnFuncBodyResult option)*State)) savePosition c_name (typeId:ReferenceToType) (lm:LanguageMacros) (codec:CommonTypes.Codec) prms p =
    match savePosition with
    | false -> funcBody
    | true  -> 
        let newFuncBody st errCode prms p =
            let content, ns1a = funcBody st errCode prms p  
            let sequence_save_bitstream                 = lm.acn.sequence_save_bitstream              
            let lvName = sprintf "bitStreamPositions_%d" (typeId.SeqeuenceOfLevel + 1)
            let savePositionStatement = sequence_save_bitstream lvName c_name codec
            let newContent = 
                match content with
                | Some bodyResult   -> 
                    let funcBodyStr = sprintf "%s\n%s" savePositionStatement bodyResult.funcBody
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = savePositionStatement 
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; bValIsUnReferenced= true; bBsIsUnReferenced=false}                        
            newContent, ns1a
        newFuncBody

let handleAlignemntForAsn1Types (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (acnAligment     : AcnAligment option ) (funcBody:State-> ErroCode->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> ((AcnFuncBodyResult option)*State))  =
    let alignToNext                      =  lm.acn.alignToNext
    match acnAligment with
    | None      -> funcBody
    | Some al   -> 
        let alStr, nAligmVal = 
            match al with
            | AcnGenericTypes.NextByte   -> "NextByte", 8I
            | AcnGenericTypes.NextWord   -> "NextWord", 16I
            | AcnGenericTypes.NextDWord  -> "NextDWord", 32I
        let newFuncBody st errCode prms p =
            let content, ns1a = funcBody st errCode prms p  
            let newContent = 
                match content with
                | Some bodyResult   -> 
                    let funcBodyStr = alignToNext bodyResult.funcBody alStr nAligmVal codec
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = alignToNext "" alStr nAligmVal codec
                    Some {funcBody = funcBodyStr; errCodes =[errCode]; localVariables = []; bValIsUnReferenced= true; bBsIsUnReferenced=false}                        
            newContent, ns1a
        newFuncBody

let handleAlignemntForAcnTypes (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)   (acnAligment : AcnAligment option ) (funcBody:CommonTypes.Codec -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> (AcnFuncBodyResult option))  =
    let alignToNext                      =  lm.acn.alignToNext
    match acnAligment with
    | None      -> funcBody
    | Some al   -> 
        let alStr, nAligmVal = 
            match al with
            | AcnGenericTypes.NextByte   -> "NextByte", 8I
            | AcnGenericTypes.NextWord   -> "NextWord", 16I
            | AcnGenericTypes.NextDWord  -> "NextDWord", 32I
        let newFuncBody (codec:CommonTypes.Codec) prms p =
            let content = funcBody codec prms p  
            let newContent = 
                match content with
                | Some bodyResult   -> 
                    let funcBodyStr = alignToNext bodyResult.funcBody alStr nAligmVal codec
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = alignToNext "" alStr nAligmVal codec
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; bValIsUnReferenced= true; bBsIsUnReferenced=false}                        
            newContent
        newFuncBody


let private createAcnFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option)  (funcBody:State-> ErroCode->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> ((AcnFuncBodyResult option)*State)) isTestVaseValid soSparkAnnotations (us:State)  =
    let funcNameAndtasInfo   = 
        let getFuncName (r:Asn1AcnAst.AstRoot) (codec:CommonTypes.Codec) (typeId:ReferenceToType) (td:FE_TypeDefinition) =
            match typeId.tasInfo with
            | None -> None
            | Some _ -> Some (td.typeName + "_ACN"  + codec.suffix)

        match t.acnParameters with
        | []    -> getFuncName r codec t.id (lm.lg.getTypeDefinition t.FT_TypeDefintion)
        | _     -> None
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None

    let EmitTypeAssignment_primitive     =  lm.acn.EmitTypeAssignment_primitive
    let EmitTypeAssignment_primitive_def =  lm.acn.EmitTypeAssignment_primitive_def
    let EmitTypeAssignment_def_err_code  =  lm.acn.EmitTypeAssignment_def_err_code

    


    let funcBodyAsSeqComp st prms p c_name : ((AcnFuncBodyResult option)*State) =
        let funcBody = handleSavePostion funcBody t.SaveBitStreamPosition c_name t.id lm codec prms p
        let ret = handleAlignemntForAsn1Types r lm codec t.acnAligment funcBody
        ret st  errCode prms p 
        

    let funcBody = handleAlignemntForAsn1Types r lm codec t.acnAligment funcBody

    let p : CallerScope = lm.lg.getParamType t codec
    let topLevAcc = lm.lg.getAcces p.arg
    let varName = p.arg.p
    let sStar = lm.lg.getStar p.arg
    let isValidFuncName = match isValidFunc with None -> None | Some f -> f.funcName
    let sInitilialExp = ""
    let  func, funcDef,ns2  = 
            match funcNameAndtasInfo  with
            |  None              -> None, None, ns
            |  Some funcName     -> 
                let content, ns1a = funcBody ns errCode [] p  
                let bodyResult_funcBody, errCodes,  bodyResult_localVariables, bBsIsUnreferenced, bVarNameIsUnreferenced = 
                    match content with 
                    | None              -> 
                        let emtyStatement = lm.lg.emtyStatement
                        emtyStatement, [], [], true, isValidFuncName.IsNone
                    | Some bodyResult   -> 
                        bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsUnReferenced, bodyResult.bValIsUnReferenced

                let handleAcnParameter (p:AcnGenericTypes.AcnParameter) =
                    let intType  = lm.typeDef.Declare_Integer ()
                    let boolType = lm.typeDef.Declare_Boolean ()
                    let emitPrm  = lm.acn.EmitAcnParameter
                    match p.asn1Type with
                    | AcnGenericTypes.AcnPrmInteger    loc          -> emitPrm p.c_name intType
                    | AcnGenericTypes.AcnPrmBoolean    loc          -> emitPrm p.c_name boolType
                    | AcnGenericTypes.AcnPrmNullType   loc          -> raise(SemanticError (loc, "Invalid type for parameter"))
                    | AcnGenericTypes.AcnPrmRefType(md,ts)          -> 
                        let prmTypeName =
                            match lm.lg.hasModules with
                            | false         -> ToC2(r.args.TypePrefix + ts.Value)
                            | true       -> 
                                match md.Value = t.id.ModName with
                                | true  -> ToC2(r.args.TypePrefix + ts.Value)
                                | false -> (ToC2 md.Value) + "." + ToC2(r.args.TypePrefix + ts.Value)
                        emitPrm p.c_name prmTypeName

                let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> Seq.distinct
                let prms = t.acnParameters |> List.map handleAcnParameter
                let prmNames = t.acnParameters |> List.map (fun p -> p.c_name)
                let func = Some(EmitTypeAssignment_primitive varName sStar funcName isValidFuncName  (typeDefinition.longTypedefName2 lm.lg.hasModules) lvars  bodyResult_funcBody soSparkAnnotations sInitilialExp prms prmNames (t.acnMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced codec)
                
                //let errCodes = bodyResult.errCodes
                let errCodStr = errCodes |> List.map(fun x -> EmitTypeAssignment_def_err_code x.errCodeName (BigInteger x.errCodeValue) x.comment) |> List.distinct
                let funcDef = Some(EmitTypeAssignment_primitive_def varName sStar funcName  (typeDefinition.longTypedefName2 lm.lg.hasModules) errCodStr (t.acnMaxSizeInBits = 0I) (BigInteger (ceil ((double t.acnMaxSizeInBits)/8.0))) ( t.acnMaxSizeInBits) prms soSparkAnnotations codec)
                func, funcDef,ns1a


    let ret = 
        {
            AcnFunction.funcName       = funcNameAndtasInfo 
            func                       = func 
            funcDef                    = funcDef
            funcBody                   = (fun us acnArgs p -> funcBody us errCode acnArgs p )
            funcBodyAsSeqComp          = funcBodyAsSeqComp
            isTestVaseValid            = isTestVaseValid
        }
    ret, ns2

let private createAcnIntegerFunctionInternal (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (uperRange : BigIntegerUperRange) acnEncodingClass (uperfuncBody : ErroCode -> CallerScope -> (UPERFuncBodyResult option)) (soMF:string option, soMFM:string option) : (ErroCode -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> (AcnFuncBodyResult option))  =
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

    //let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    //let errCodeValue        = us.currErrCode
    //let errCode             = {ErroCode.errCodeName = errCodeName; errCodeValue = errCodeValue}
    let nUperMin, nUperMax = 
        match uperRange with
        | Concrete(a,b) -> a,b
        | NegInf(b)     -> r.args.SIntMin, b
        | PosInf(a)     -> a, r.args.IntMax (a>=0I)
        | Full          -> r.args.SIntMin, r.args.SIntMax

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let uIntActualMax (nBits:int) =
            let a = 2I**nBits - 1I
            min a nUperMax
        let sIntActualMin (nBits:int) =
            let a = -(2I**(nBits-1))
            max a nUperMin
        let sIntActualMax (nBits:int) =
            let a = 2I**(nBits-1) - 1I
            min a nUperMax
        let sSsuffix = DAstUPer.getIntDecFuncSuffix r uperRange 
        let castPp encFuncBits = DAstUPer.castPp r lm codec pp uperRange encFuncBits
        let word_size_in_bits = (int r.args.integerSizeInBytes)*8
        //let soMF = match 
        let funcBodyContent  = 
            match acnEncodingClass with
            |Asn1AcnAst.Integer_uPER                                       ->  uperfuncBody errCode p |> Option.map(fun x -> x.funcBody, x.errCodes, x.bValIsUnReferenced, x.bBsIsUnReferenced)
            |Asn1AcnAst.PositiveInteger_ConstSize_8                        ->  Some(PositiveInteger_ConstSize_8 (castPp 8) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 8)  codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_16            ->  Some(PositiveInteger_ConstSize_big_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 16) codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_16         ->  Some(PositiveInteger_ConstSize_little_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 16) codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_32            ->  Some(PositiveInteger_ConstSize_big_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 32) codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_32         ->  Some(PositiveInteger_ConstSize_little_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 32) codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_64            ->  Some(PositiveInteger_ConstSize_big_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 64) codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_64         ->  Some(PositiveInteger_ConstSize_little_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 64) codec, [errCode], false, false)
            |Asn1AcnAst.PositiveInteger_ConstSize bitSize                  ->  Some(PositiveInteger_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName ( bitSize) soMF soMFM (max 0I nUperMin) (uIntActualMax (int bitSize)) codec, [errCode], false, false)
            
            |Asn1AcnAst.TwosComplement_ConstSize_8                         ->  Some(TwosComplement_ConstSize_8 (castPp 8) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 8) (sIntActualMax 8) codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_16             ->  Some(TwosComplement_ConstSize_big_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 16) (sIntActualMax 16) codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_16          ->  Some(TwosComplement_ConstSize_little_endian_16 (castPp 16) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 16) (sIntActualMax 16) codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_32             ->  Some(TwosComplement_ConstSize_big_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 32) (sIntActualMax 32) codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_32          ->  Some(TwosComplement_ConstSize_little_endian_32 (castPp 32) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 32) (sIntActualMax 32) codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_64             ->  Some(TwosComplement_ConstSize_big_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 64) (sIntActualMax 64) codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_64          ->  Some(TwosComplement_ConstSize_little_endian_64 (castPp 64) sSsuffix errCode.errCodeName soMF soMFM (sIntActualMin 64) (sIntActualMax 64) codec, [errCode], false, false)
            |Asn1AcnAst.TwosComplement_ConstSize bitSize                   ->  Some(TwosComplement_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM ( bitSize) (sIntActualMin (int bitSize)) (sIntActualMax (int bitSize)) codec, [errCode], false, false)

            |Asn1AcnAst.ASCII_ConstSize size                               ->  Some(ASCII_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax ((size)/8I) codec, [errCode], false, false)
            |Asn1AcnAst.ASCII_VarSize_NullTerminated nullBytes             ->  Some(ASCII_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax nullBytes codec, [errCode], false, false)
            |Asn1AcnAst.ASCII_UINT_ConstSize size                          ->  Some(ASCII_UINT_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax (( size)/8I) codec, [errCode], false, false)
            |Asn1AcnAst.ASCII_UINT_VarSize_NullTerminated nullBytes         -> Some(ASCII_UINT_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName  soMF soMFM nUperMin nUperMax nullBytes codec, [errCode], false, false)
            |Asn1AcnAst.BCD_ConstSize size                                 ->  Some(BCD_ConstSize (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax (( size)/4I) codec, [errCode], false, false)
            |Asn1AcnAst.BCD_VarSize_NullTerminated nullBytes                -> Some(BCD_VarSize_NullTerminated (castPp word_size_in_bits) sSsuffix errCode.errCodeName soMF soMFM nUperMin nUperMax codec, [errCode], false, false)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, bValIsUnReferenced, bBsIsUnReferenced ) -> 
            Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsUnReferenced= bValIsUnReferenced; bBsIsUnReferenced=bBsIsUnReferenced})
    //let funcBody = (funcBody errCode)
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

let createAcnIntegerFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnInteger)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None

    let uperFuncBody (errCode) (p:CallerScope) = 
        DAstUPer.getIntfuncBodyByCons r lm codec t.uperRange t.Location (getAcnIntegerClass r.args t) (t.cons) (t.cons@t.withcons) errCode p
        (*let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let IntUnconstraint = match l with C -> uper_c.IntUnconstraint          | Ada -> uper_a.IntUnconstraint
        let funcBodyContent = IntUnconstraint pp errCode.errCodeName false codec
        Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    *)
    let soMapFunMod, soMapFunc  = 
        match t.acnProperties.mappingFunction with 
        | Some (MappingFunction (soMapFunMod, mapFncName))    -> 
            let soMapFunMod, soMapFunc  =  soMapFunMod,  Some mapFncName.Value 
            match soMapFunMod with
            | None  -> getMappingFunctionModule r lm soMapFunc, soMapFunc
            | Some soMapFunMod   -> Some soMapFunMod.Value, soMapFunc
        | None -> None, None
    let funcBody = createAcnIntegerFunctionInternal r lm codec t.uperRange t.acnEncodingClass uperFuncBody (soMapFunc, soMapFunMod)
    (funcBody errCode), ns



let createIntegerFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let soMapFunMod, soMapFunc  = 
        match o.acnProperties.mappingFunction with 
        | Some (MappingFunction (soMapFunMod, mapFncName))    -> 
            let soMapFunMod, soMapFunc  =  soMapFunMod,  Some mapFncName.Value 
            match soMapFunMod with
            | None  -> getMappingFunctionModule r lm soMapFunc, soMapFunc
            | Some soMapFunMod   -> Some soMapFunMod.Value, soMapFunc
        | None -> None, None
    let funcBody = createAcnIntegerFunctionInternal r lm codec o.uperRange o.acnEncodingClass uperFunc.funcBody_e (soMapFunc, soMapFunMod)
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let createEnumComn (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefintionOrReference ) (typeDefinitionName:string)  =
    let EnumeratedEncValues                 = lm.acn.EnumeratedEncValues
    let Enumerated_item                     = lm.acn.Enumerated_item
    let IntFullyConstraintPos               = lm.uper.IntFullyConstraintPos
    let min = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.min
    let max = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.max
    //let intVal = "intVal"
    let sFirstItemName = lm.lg.getNamedItemBackendName (Some defOrRef)  o.items.Head 
    let uperRange = (Concrete (min,max))
    let intTypeClass = getIntEncodingClassByUperRange r.args uperRange
    let rtlIntType = (DAstTypeDefinition.getIntererTypeByClass lm intTypeClass)()
    let localVar, intVal =
        let varName = "intVal"
        GenericLocalVariable {GenericLocalVariable.name = varName; varType= rtlIntType; arrSize= None; isStatic = false; initExp=None}, varName
        //match min >= 0I with
        //| true -> Asn1UIntLocalVariable ("uIntVal",None), "uIntVal"
        //| false -> Asn1SIntLocalVariable ("intVal",None), "intVal"
    let pVal = {CallerScope.modName = typeId.ModName; arg = VALUE intVal}
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let td = (lm.lg.getEnmTypeDefintion o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let intFuncBody = 
            let uperInt (errCode:ErroCode) (p:CallerScope) = 
                let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
                let castPp  = DAstUPer.castPp r lm codec pp uperRange
                let sSsuffix = DAstUPer.getIntDecFuncSuffix r uperRange 
                let word_size_in_bits = (int r.args.integerSizeInBytes)*8
                let funcBody = IntFullyConstraintPos (castPp word_size_in_bits) min max (GetNumberOfBitsForNonNegativeInteger (max-min))  sSsuffix errCode.errCodeName codec
                Some({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables= []; bValIsUnReferenced=false; bBsIsUnReferenced=false})
            createAcnIntegerFunctionInternal r lm codec (Concrete (min,max)) o.acnEncodingClass uperInt (None, None)
        let funcBodyContent = 
            match intFuncBody errCode acnArgs pVal with
            | None      -> None
            | Some(intAcnFuncBdResult) ->
                let arrItems = o.items |> List.map(fun it -> Enumerated_item (lm.lg.getValue p.arg) (lm.lg.getNamedItemBackendName (Some defOrRef) it ) it.acnEncodeValue intVal codec)
                Some (EnumeratedEncValues (lm.lg.getValue p.arg) td arrItems intAcnFuncBdResult.funcBody errCode.errCodeName sFirstItemName intVal codec, intAcnFuncBdResult.errCodes, localVar::intAcnFuncBdResult.localVariables)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false})
    funcBody

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefintionOrReference) (typeDefinition:TypeDefintionOrReference)   (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let typeDefinitionName = defOrRef.longTypedefName2 lm.lg.hasModules //getTypeDefinitionName t.id.tasInfo typeDefinition
    let funcBody = createEnumComn r lm codec t.id o defOrRef typeDefinitionName
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations  us


let createAcnEnumeratedFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToEnumerated)  (defOrRef:TypeDefintionOrReference) (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None
    let td = lm.lg.getTypeDefinition (t.getType r).FT_TypeDefintion
    let typeDefinitionName = td.typeName
    let funcBody = createEnumComn r lm codec typeId t.enumerated defOrRef typeDefinitionName
    (funcBody errCode), ns



let createRealrFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Real_32_big_endian                  = lm.acn.Real_32_big_endian
    let Real_64_big_endian                  = lm.acn.Real_64_big_endian
    let Real_32_little_endian               = lm.acn.Real_32_little_endian
    let Real_64_little_endian               = lm.acn.Real_64_little_endian

    let sSuffix =
        match o.getClass r.args with
        | ASN1SCC_REAL   -> ""
        | ASN1SCC_FP32   -> "_fp32"
        | ASN1SCC_FP64   -> ""

    
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let castPp = DAstUPer.castRPp lm codec (o.getClass r.args) pp 

        let funcBodyContent = 
            match o.acnEncodingClass with
            | Real_IEEE754_32_big_endian            -> Some (Real_32_big_endian castPp sSuffix errCode.errCodeName codec, [errCode])
            | Real_IEEE754_64_big_endian            -> Some (Real_64_big_endian pp errCode.errCodeName codec, [errCode])
            | Real_IEEE754_32_little_endian         -> Some (Real_32_little_endian castPp sSuffix errCode.errCodeName codec, [errCode])
            | Real_IEEE754_64_little_endian         -> Some (Real_64_little_endian pp errCode.errCodeName codec, [errCode])
            | Real_uPER                             -> uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsUnReferenced= false; bBsIsUnReferenced=false})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getPointer p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let funcBodyContent = 
            uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsUnReferenced= false; bBsIsUnReferenced=false})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getPointer p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let funcBodyContent = 
            uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsUnReferenced= false; bBsIsUnReferenced=false})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let nestChildItems (lm:LanguageMacros) (codec:CommonTypes.Codec) children = 
    DAstUtilFunctions.nestItems lm.isvalid.JoinItems2 children
    
#if false
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
#endif


let createAcnBooleanFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnBoolean)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let Boolean         = lm.uper.Boolean
        let funcBodyContent = 
            Boolean pp errCode.errCodeName codec
        Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced= false; bBsIsUnReferenced=false}    
    (funcBody errCode), ns

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : AcnFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let pvalue = lm.lg.getValue p.arg
        let ptr = lm.lg.getPointer p.arg
        let Boolean         = lm.uper.Boolean
        let acnBoolean      = lm.acn.Boolean
        let funcBodyContent = 
            match o.acnProperties.encodingPattern with
            | None  -> Boolean pp errCode.errCodeName codec
            | Some (TrueValue  bitVal)  ->
                let arrsBits = bitVal.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrBytes = bitStringValueToByteArray bitVal
                let bEncValIsTrue, arruTrueValueAsByteArray, arruFalseValueAsByteArray, nSize =
                    true, arrBytes, (arrBytes |> Array.map (~~~)), bitVal.Value.Length
                acnBoolean pvalue ptr bEncValIsTrue (BigInteger nSize) arruTrueValueAsByteArray arruFalseValueAsByteArray arrsBits errCode.errCodeName codec
            | Some (FalseValue   bitVal)    ->
                let arrsBits = bitVal.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                let arrBytes = bitStringValueToByteArray bitVal
                let bEncValIsTrue, arruTrueValueAsByteArray, arruFalseValueAsByteArray, nSize =
                    false, (arrBytes |> Array.map (~~~)), arrBytes, bitVal.Value.Length
                acnBoolean pvalue ptr bEncValIsTrue (BigInteger nSize) arruTrueValueAsByteArray arruFalseValueAsByteArray arrsBits errCode.errCodeName codec
                
        {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced= false; bBsIsUnReferenced=false}    
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> Some (funcBody e acnArgs p), us) (fun atc -> true) soSparkAnnotations us




let createAcnNullTypeFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnNullType)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let nullType         = lm.acn.Null_pattern2
        match o.acnProperties.encodingPattern with
        | None      -> None
        | Some encPattern   ->
            let arrsBits, arrBytes, nBitsSize =
                match encPattern with
                | PATTERN_PROP_BITSTR_VALUE bitStringPattern ->
                    let arrsBits = bitStringPattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                    let arrBytes = bitStringValueToByteArray bitStringPattern
                    arrsBits, arrBytes, (BigInteger bitStringPattern.Value.Length)
                | PATTERN_PROP_OCTSTR_VALUE octStringBytes   ->
                    let arrBytes = octStringBytes |> Seq.map(fun z -> z.Value) |> Seq.toArray
                    let bitStringPattern = byteArrayToBitStringValue arrBytes
                    let arrsBits = bitStringPattern.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                    arrsBits,arrBytes,(BigInteger bitStringPattern.Length)
            let ret = nullType pp arrBytes nBitsSize arrsBits errCode.errCodeName o.acnProperties.savePosition codec
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; bValIsUnReferenced= false; bBsIsUnReferenced=false})
    (funcBody errCode), ns

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let nullType         = lm.acn.Null_pattern
        
        match o.acnProperties.encodingPattern with
        | None      ->  None
        | Some encPattern   ->
            let arrsBits, arrBytes, nBitsSize =
                match encPattern with
                | PATTERN_PROP_BITSTR_VALUE bitStringPattern ->
                    let arrsBits = bitStringPattern.Value.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                    let arrBytes = bitStringValueToByteArray bitStringPattern
                    arrsBits, arrBytes, (BigInteger bitStringPattern.Value.Length)
                | PATTERN_PROP_OCTSTR_VALUE octStringBytes   ->
                    let arrBytes = octStringBytes |> Seq.map(fun z -> z.Value) |> Seq.toArray
                    let bitStringPattern = byteArrayToBitStringValue arrBytes
                    let arrsBits = bitStringPattern.ToCharArray() |> Seq.mapi(fun i x -> ((i+1).ToString()) + "=>" + if x='0' then "0" else "1") |> Seq.toList
                    arrsBits,arrBytes,(BigInteger bitStringPattern.Length)
            let ret = nullType pp arrBytes nBitsSize arrsBits errCode.errCodeName o.acnProperties.savePosition codec
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; bValIsUnReferenced= lm.lg.acn.null_valIsUnReferenced; bBsIsUnReferenced=false})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let getExternaField0 (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency func1 =
    //let dependencies = deps.acnDependencies |> List.filter(fun d -> d.asn1Type = asn1TypeIdWithDependency && func1 d )
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
    getAcnDeterminantName  (resolveParam dependency.determinant.id)

let getExternaFieldChoizePresentWhen (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency  relPath=
    let filterDependency (d:AcnDependency) =
        match d.dependencyKind with
        | AcnDepPresence (relPath0, _)   -> relPath = relPath0
        | _                              -> true
    getExternaField0 r deps asn1TypeIdWithDependency filterDependency


let getExternaField (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
    getExternaField0 r deps asn1TypeIdWithDependency (fun z -> true)

let createStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference)  (defOrRef:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Acn_String_Ascii_FixSize                            = lm.acn.Acn_String_Ascii_FixSize                          
    let Acn_String_Ascii_Internal_Field_Determinant         = lm.acn.Acn_String_Ascii_Internal_Field_Determinant       
    let Acn_String_Ascii_Null_Teminated                     = lm.acn.Acn_String_Ascii_Null_Teminated                   
    let Acn_String_Ascii_External_Field_Determinant         = lm.acn.Acn_String_Ascii_External_Field_Determinant       
    let Acn_String_CharIndex_External_Field_Determinant     = lm.acn.Acn_String_CharIndex_External_Field_Determinant   
    let Acn_IA5String_CharIndex_External_Field_Determinant  = lm.acn.Acn_IA5String_CharIndex_External_Field_Determinant
    
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)  (us:State)      = 
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let td = (lm.lg.getStrTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let funcBodyContent, ns = 
            match o.acnEncodingClass with
            | Acn_Enc_String_uPER  _                                           -> uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes, x.localVariables), us
            | Acn_Enc_String_uPER_Ascii _                                      -> 
                match o.maxSize.uper = o.minSize.uper with
                | true      ->  Some (Acn_String_Ascii_FixSize pp errCode.errCodeName ( o.maxSize.uper) codec, [errCode], []), us
                | false     ->  
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                    Some (Acn_String_Ascii_Internal_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) ( o.minSize.acn) nSizeInBits codec , [errCode], []), us
            | Acn_Enc_String_Ascii_Null_Teminated                   (_,nullChars)   -> Some (Acn_String_Ascii_Null_Teminated pp errCode.errCodeName ( o.maxSize.acn) nullChars codec, [errCode], []), us
            | Acn_Enc_String_Ascii_External_Field_Determinant       _    -> 
                let extField = getExternaField r deps t.id
                Some(Acn_String_Ascii_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) extField codec, [errCode], []), us
            | Acn_Enc_String_CharIndex_External_Field_Determinant   _    -> 
                let extField = getExternaField r deps t.id
                let typeDefinitionName = defOrRef.longTypedefName2 lm.lg.hasModules//getTypeDefinitionName t.id.tasInfo typeDefinition
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let encDecStatement = 
                    match o.uperCharSet.Length = 128 with
                    | false -> 
                        let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                        Acn_String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) arrAsciiCodes (BigInteger o.uperCharSet.Length) extField td nBits codec 
                    | true  -> Acn_IA5String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn)  extField td nBits codec
                Some(encDecStatement, [errCode], []), us
        match funcBodyContent with
        | None -> None, ns
        | Some (funcBodyContent,errCodes, localVars) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVars; bValIsUnReferenced= false; bBsIsUnReferenced=false}), ns
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p us) (fun atc -> true) soSparkAnnotations us


let createAcnStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToIA5String)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName None
    let Acn_String_Ascii_FixSize                            = lm.acn.Acn_String_Ascii_FixSize                          
    let Acn_String_Ascii_Internal_Field_Determinant         = lm.acn.Acn_String_Ascii_Internal_Field_Determinant       
    let Acn_String_Ascii_Null_Teminated                     = lm.acn.Acn_String_Ascii_Null_Teminated                   
    let Acn_String_Ascii_External_Field_Determinant         = lm.acn.Acn_String_Ascii_External_Field_Determinant       
    let Acn_String_CharIndex_External_Field_Determinant     = lm.acn.Acn_String_CharIndex_External_Field_Determinant   
    let Acn_IA5String_CharIndex_External_Field_Determinant  = lm.acn.Acn_IA5String_CharIndex_External_Field_Determinant
    let typeDefinitionName = ToC2(r.args.TypePrefix + t.tasName.Value)
    let callerProgramUnit = ToC typeId.ModName
    
    //let td = o.
    let o = t.str
    let uper_funcBody (errCode:ErroCode) (p:CallerScope) = 
        let td =
            let md = r.GetModuleByName t.modName
            let tas = md.GetTypeAssignmentByName t.tasName r
            match tas.Type.ActualType.Kind with
            | Asn1AcnAst.IA5String     z -> (lm.lg.getStrTypeDefinition z.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
            | Asn1AcnAst.NumericString z -> (lm.lg.getStrTypeDefinition z.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
            | _                           -> raise(SemanticError(t.tasName.Location, (sprintf "Type assignment %s.%s does not point to a string type" t.modName.Value t.modName.Value)))
        let ii = typeId.SeqeuenceOfLevel + 1
        let i = sprintf "i%d" ii
        let lv = SequenceOfIndex (typeId.SeqeuenceOfLevel + 1, None)
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
        //let Fragmentation_sqf   = match l with C -> uper_c.Fragmentation_sqf    | Ada -> uper_a.Fragmentation_sqf

        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
        let internalItem =
            match o.uperCharSet.Length = 128 with
            | true  -> InternalItem_string_no_alpha p.arg.p errCode.errCodeName i  codec 
            | false -> 
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                InternalItem_string_with_alpha p.arg.p errCode.errCodeName td i (BigInteger (o.uperCharSet.Length-1)) arrAsciiCodes (BigInteger (o.uperCharSet.Length)) nBits  codec
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.uper - o.minSize.uper))
        let funcBodyContent, localVariables = 
            match o.minSize with
            | _ when o.maxSize.uper < 65536I && o.maxSize.uper=o.minSize.uper  -> 
                str_FixedSize p.arg.p typeDefinitionName i internalItem ( o.minSize.uper) nBits nBits 0I codec, charIndex@nStringLength
            | _ when o.maxSize.uper < 65536I && o.maxSize.uper<>o.minSize.uper  -> 
                //printfn "%A\n" nStringLength
                str_VarSize p.arg.p typeDefinitionName i internalItem ( o.minSize.uper) ( o.maxSize.uper) nSizeInBits nBits nBits 0I codec , charIndex@nStringLength
            | _                                                -> 
                let funcBodyContent,localVariables = DAstUPer.handleFragmentation lm p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper internalItem nBits false true
                funcBodyContent,charIndex@localVariables

        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = lv::localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    


    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let td = (lm.lg.getStrTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let pp = match codec with CommonTypes.Encode -> lm.lg.getValue p.arg | CommonTypes.Decode -> lm.lg.getPointer p.arg
        let funcBodyContent = 
            match t.str.acnEncodingClass with
            | Acn_Enc_String_uPER_Ascii    _                                    -> 
                match t.str.maxSize.uper = t.str.minSize.uper with
                | true      ->  Some (Acn_String_Ascii_FixSize pp errCode.errCodeName ( t.str.maxSize.uper) codec, [], [])
                | false     ->  
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                    Some (Acn_String_Ascii_Internal_Field_Determinant pp errCode.errCodeName ( t.str.maxSize.acn) ( t.str.minSize.acn) nSizeInBits codec , [], [])
            | Acn_Enc_String_Ascii_Null_Teminated                  (_, nullChars)   -> Some (Acn_String_Ascii_Null_Teminated pp errCode.errCodeName ( t.str.maxSize.acn) nullChars codec, [], [])
            | Acn_Enc_String_Ascii_External_Field_Determinant       _    -> 
                let extField = getExternaField r deps typeId
                Some(Acn_String_Ascii_External_Field_Determinant pp errCode.errCodeName ( t.str.maxSize.acn) extField codec, [], [])
            | Acn_Enc_String_CharIndex_External_Field_Determinant   _    -> 
                let extField = getExternaField r deps typeId
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (t.str.uperCharSet.Length-1))
                let encDecStatement = 
                    match t.str.uperCharSet.Length = 128 with
                    | false -> 
                        let arrAsciiCodes = t.str.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                        Acn_String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( t.str.maxSize.acn) arrAsciiCodes (BigInteger t.str.uperCharSet.Length) extField td nBits codec
                    | true  -> Acn_IA5String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( t.str.maxSize.acn) extField td nBits codec
                Some(encDecStatement, [], [])
            | Acn_Enc_String_uPER    _                                         -> 
                let x = (uper_funcBody errCode) p 
                Some(x.funcBody, x.errCodes, x.localVariables)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, lvs) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::errCodes |> List.distinct ; localVariables = lvs; bValIsUnReferenced= false; bBsIsUnReferenced=false})


    (funcBody errCode), ns


let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let oct_external_field           = lm.acn.oct_external_field
    let oct_external_field_fix_size  = lm.acn.oct_external_field_fix_size
    let oct_sqf_null_terminated          = lm.acn.oct_sqf_null_terminated
    let fixedSize       = lm.uper.octect_FixedSize
    let varSize         = lm.uper.octect_VarSize
    let InternalItem_oct_str             = lm.uper.InternalItem_oct_str
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
    let nAlignSize = 0I;

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let funcBodyContent = 
            match o.acnEncodingClass with
            | SZ_EC_FIXED_SIZE                                              -> 
                let fncBody = 
                    fixedSize p.arg.p (lm.lg.getAcces p.arg) o.minSize.acn codec 
                Some(fncBody, [errCode],[])

            | SZ_EC_LENGTH_EMBEDDED lenSize                                 ->
                let fncBody = 
                    varSize p.arg.p (lm.lg.getAcces p.arg)  (o.minSize.acn) (o.maxSize.acn) lenSize errCode.errCodeName codec 
                let nStringLength =
                    match codec with
                    | Encode -> []
                    | Decode -> [lm.lg.uper.count_var]

                Some(fncBody, [errCode],nStringLength)
            | SZ_EC_ExternalField   _    -> 
                let extField = getExternaField r deps t.id
                let fncBody = 
                    match o.isFixedSize with
                    | true  -> oct_external_field_fix_size p.arg.p (lm.lg.getAcces p.arg) (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName codec
                    | false -> oct_external_field p.arg.p (lm.lg.getAcces p.arg) (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName codec
                Some(fncBody, [errCode],[])
            | SZ_EC_TerminationPattern bitPattern   ->
                let mod8 = bitPattern.Value.Length % 8
                let suffix = [1 .. mod8] |> Seq.map(fun _ -> "0") |> Seq.StrJoin ""
                let bitPatten8 = bitPattern.Value + suffix
                let byteArray = bitStringValueToByteArray bitPatten8.AsLoc
                let internalItem = InternalItem_oct_str p.arg.p (lm.lg.getAcces p.arg) i  errCode.errCodeName codec 
                let noSizeMin = if o.minSize.acn=0I then None else Some ( o.minSize.acn)
                let fncBody = oct_sqf_null_terminated p.arg.p (lm.lg.getAcces p.arg) i internalItem noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName  8I 8I codec
                let lv2 = 
                    match codec, lm.lg.acn.checkBitPatternPresentResult with
                    | Decode, true    -> [IntegerLocalVariable ("checkBitPatternPresentResult", Some 0)]
                    | _            -> []
                Some(fncBody, [errCode],lv::lv2)
                


        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us





let createBitStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let nAlignSize = 0I;
    let bitString_FixSize = lm.uper.bitString_FixSize
    let bitString_VarSize = lm.uper.bitString_VarSize

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let funcBodyContent = 
            match o.acnEncodingClass with
            | SZ_EC_ExternalField   _    -> 
                let createBitStringFunction_extfld  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (errCode:ErroCode) (p:CallerScope) (extField:string) (codec:CommonTypes.Codec) : (string*ErroCode list*LocalVariable list) = 
                    let fncBody = 
                        match o.minSize.uper = o.maxSize.uper with
                        | true  -> lm.acn.bit_string_external_field_fixed_size p.arg.p errCode.errCodeName (getAcces_c p.arg) (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField codec
                        | false  -> lm.acn.bit_string_external_field p.arg.p errCode.errCodeName (getAcces_c p.arg) (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField codec
                    (fncBody, [errCode], [])

                
                let extField = getExternaField r deps t.id
                let ret = createBitStringFunction_extfld t o errCode p extField codec
                Some ret
            | SZ_EC_TerminationPattern   bitPattern    -> 
                let createBitStringFunction_term_pat  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (errCode:ErroCode) (p:CallerScope) (codec:CommonTypes.Codec) (bitPattern:Asn1AcnAst.BitStringValue): (string*ErroCode list*LocalVariable list) = 
                    let mod8 = bitPattern.Value.Length % 8
                    let suffix = [1 .. mod8] |> Seq.map(fun _ -> "0") |> Seq.StrJoin ""
                    let bitPatten8 = bitPattern.Value + suffix
                    let byteArray = bitStringValueToByteArray bitPatten8.AsLoc
                    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
                    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
                    let fncBody = lm.acn.bit_string_null_terminated p.arg.p errCode.errCodeName (getAcces_c p.arg) i (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) byteArray bitPattern.Value.Length.AsBigInt codec
                    (fncBody, [errCode], [])

                let ret = createBitStringFunction_term_pat t o errCode p codec bitPattern

                Some ret
            | SZ_EC_FIXED_SIZE       ->
                let fncBody = 
                    bitString_FixSize p.arg.p (getAcces_c p.arg) o.minSize.acn errCode.errCodeName codec
                Some(fncBody, [errCode],[])

            | SZ_EC_LENGTH_EMBEDDED nSizeInBits -> 
                let fncBody =
                    bitString_VarSize p.arg.p (getAcces_c p.arg) o.minSize.acn o.maxSize.acn errCode.errCodeName nSizeInBits codec
                let nStringLength =
                    match codec with
                    | Encode -> []
                    | Decode -> [lm.lg.uper.count_var]
                Some(fncBody, [errCode],nStringLength)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false})
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us



let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (isValidFunc: IsValidFunction option)  (child:Asn1Type) (us:State)  =
    let oct_sqf_null_terminated = lm.acn.oct_sqf_null_terminated
    let oct_sqf_external_field_fix_size                 = lm.acn.sqf_external_field_fix_size
    let external_field          = lm.acn.sqf_external_field
    let fixedSize               = lm.uper.seqOf_FixedSize
    let varSize                 = lm.uper.seqOf_VarSize
    
    let ii = t.id.SeqeuenceOfLevel + 1

    let i = sprintf "i%d" ii
    let lv = 
        match o.acnEncodingClass with
        | SZ_EC_FIXED_SIZE
        | SZ_EC_LENGTH_EMBEDDED _ //-> lm.lg.uper.seqof_lv t.id o.minSize.uper o.maxSize.uper
        | SZ_EC_ExternalField       _
        | SZ_EC_TerminationPattern  _ -> [SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)]

    let nAlignSize = 0I;
    let typeDefinitionName = defOrRef.longTypedefName2 lm.lg.hasModules 
    let nIntItemMaxSize = ( child.acnMaxSizeInBits)
    let funcBody (us:State) (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        match child.getAcnFunction codec with
        | None         -> None, us
        | Some chFunc  ->
            let internalItem, ns = chFunc.funcBody us acnArgs ({p with arg = lm.lg.getArrayItem p.arg i child.isIA5String})
            let ret = 
                match o.acnEncodingClass with
                | SZ_EC_FIXED_SIZE
                | SZ_EC_LENGTH_EMBEDDED _ -> 
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                    let nStringLength =
                        match o.minSize.uper = o.maxSize.uper,  codec with
                        | true , _    -> []
                        | false, Encode -> []
                        | false, Decode -> [lm.lg.uper.count_var]

                    match internalItem with
                    | None  -> 
                            match o.isFixedSize with
                            | true  -> None
                            | false -> 
                                let funcBody = varSize p.arg.p (lm.lg.getAcces p.arg)  typeDefinitionName i "" ( o.minSize.acn) ( o.maxSize.acn) nSizeInBits ( child.acnMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec
                                Some ({AcnFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = lv@nStringLength; bValIsUnReferenced= false; bBsIsUnReferenced=false})    

                    | Some internalItem -> 
                        let childErrCodes =  internalItem.errCodes
                        let ret, localVariables = 
                            match o.isFixedSize with
                            | true   -> fixedSize p.arg.p typeDefinitionName i internalItem.funcBody ( o.minSize.acn) ( child.acnMinSizeInBits) nIntItemMaxSize 0I codec , nStringLength 
                            | false  -> varSize p.arg.p (lm.lg.getAcces p.arg)  typeDefinitionName i internalItem.funcBody ( o.minSize.acn) ( o.maxSize.acn) nSizeInBits ( child.acnMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec , nStringLength 
                        Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = lv@(internalItem.localVariables@localVariables); bValIsUnReferenced= false; bBsIsUnReferenced=false})    

                | SZ_EC_ExternalField   _    -> 
                    match internalItem with
                    | None  -> None
                    | Some internalItem ->
                        let localVariables  = internalItem.localVariables
                        let childErrCodes   = internalItem.errCodes
                        let internalItem    = internalItem.funcBody
                        let extField        = getExternaField r deps t.id
                        let funcBodyContent = 
                            match o.isFixedSize with
                            | true  -> oct_sqf_external_field_fix_size p.arg.p (lm.lg.getAcces p.arg) i internalItem (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits  codec
                            | false -> external_field p.arg.p (lm.lg.getAcces p.arg) i internalItem (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits  codec
                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = lv@localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false})
                | SZ_EC_TerminationPattern   bitPattern    -> 
                    match internalItem with
                    | None  -> None
                    | Some internalItem ->
                        let mod8 = bitPattern.Value.Length % 8
                        let suffix = [1 .. mod8] |> Seq.map(fun _ -> "0") |> Seq.StrJoin ""
                        let bitPatten8 = bitPattern.Value + suffix
                        let byteArray = bitStringValueToByteArray bitPatten8.AsLoc
                        let localVariables  = internalItem.localVariables
                        let childErrCodes   = internalItem.errCodes
                        let internalItem    = internalItem.funcBody
                        let noSizeMin = if o.minSize.acn=0I then None else Some ( o.minSize.acn)
                        let funcBodyContent = oct_sqf_null_terminated p.arg.p (lm.lg.getAcces p.arg) i internalItem noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits codec

                        let lv2 = 
                            match codec, lm.lg.acn.checkBitPatternPresentResult with
                            | Decode, true    -> [IntegerLocalVariable ("checkBitPatternPresentResult", Some 0)]
                            | _            -> []

                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = lv2@lv@localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false})
            ret,ns
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations us


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
            let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
                prmUpdateStatement.updateAcnChildFnc typedefName vTarget pSrcRoot
            
            Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=prmUpdateStatement.errCodes; testCaseFnc = prmUpdateStatement.testCaseFnc; localVariables=[]}), ns1
    | AcnDepSizeDeterminant (minSize, maxSize, szAcnProp)        -> 
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let pSizeable, checkPath = getAccessFromScopeNodeList d.asn1Type false lm pSrcRoot
            let updateStatement = 
                match minSize.acn = maxSize.acn with
                | true  -> sizeDependencyFixedSize (lm.lg.getValue vTarget.arg) minSize.acn
                | false -> sizeDependency (lm.lg.getValue vTarget.arg) (getSizeableSize pSizeable.arg.p (lm.lg.getAcces pSizeable.arg)) minSize.uper maxSize.uper false typedefName
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type

        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepSizeDeterminant_bit_oct_str_containt  o       -> 
        let baseTypeDefinitionName = 
            match lm.lg.hasModules with
            | false     -> ToC2(r.args.TypePrefix + o.tasName.Value) 
            | true   -> 
                match m.Name.Value = o.modName.Value with
                | true  -> ToC2(r.args.TypePrefix + o.tasName.Value) 
                | false -> (ToC o.modName.Value) + "." + ToC2(r.args.TypePrefix + o.tasName.Value) 
        let baseFncName = baseTypeDefinitionName + "_ACN" + Encode.suffix
        let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ACN_ENCODING" baseTypeDefinitionName
        let asn1TypeD = us.newTypesMap[d.asn1Type] :?> Asn1Type
        let asn1TypeD = match asn1TypeD.Kind with ReferenceType  o -> o.resolvedType.ActualType | _  -> asn1TypeD
        let errCodes0, localVariables0, ns = 
            match asn1TypeD.acnEncFunction with
            | Some f  -> 
                let fncBdRes, ns = f.funcBody us [] {CallerScope.modName = ""; arg = VALUE "dummy"}
                match fncBdRes with
                | Some x -> x.errCodes, x.localVariables, ns
                | None   -> [], [], us
            | None    -> [], [], us

        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let pSizeable, checkPath = getAccessFromScopeNodeList d.asn1Type false lm pSrcRoot
            let sComment= 
                match asn1TypeD.acnEncFunction with
                | Some f  -> 
                    let fncBdRes, _ = f.funcBody us [] pSizeable
                    match fncBdRes with
                    | None -> ""
                    | Some a -> a.funcBody
                | None -> ""
                    
            let updateStatement = sizeDep_oct_str_containing (lm.lg.getParamValue o.resolvedType pSizeable.arg Encode) baseFncName sReqBytesForUperEncoding (lm.lg.getValue vTarget.arg) (match o.encodingOptions with Some eo -> eo.octOrBitStr = ContainedInOctString | None -> false) sComment
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        let localVars = lm.lg.acn.getAcnDepSizeDeterminantLocVars sReqBytesForUperEncoding

        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=errCodes0; testCaseFnc=testCaseFnc; localVariables= localVariables0@localVars}), ns
    | AcnDepIA5StringSizeDeterminant (minSize, maxSize, szAcnProp)   ->

        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let pSizeable, checkPath = getAccessFromScopeNodeList d.asn1Type true lm pSrcRoot
            let updateStatement = sizeDependency (lm.lg.getValue vTarget.arg) (getStringSize pSizeable.arg.p)  minSize.uper maxSize.uper true typedefName
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement 
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepPresenceBool              -> 
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let v = lm.lg.getValue vTarget.arg
            let parDecTypeSeq =
                match d.asn1Type with
                | ReferenceToType (nodes) -> ReferenceToType (nodes |> List.rev |> List.tail |> List.rev)
            let pDecParSeq, checkPath = getAccessFromScopeNodeList parDecTypeSeq false lm pSrcRoot
            let updateStatement = presenceDependency v (pDecParSeq.arg.p) (lm.lg.getAcces pDecParSeq.arg) (ToC d.asn1Type.lastItem)
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            match atc.testCaseTypeIDsMap.TryFind(d.asn1Type) with
            | Some _    -> Some TcvComponentPresent
            | None      -> Some TcvComponentAbsent
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepPresence   (relPath, chc)               -> 
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let v = lm.lg.getValue vTarget.arg
            let choicePath, checkPath = getAccessFromScopeNodeList d.asn1Type false lm pSrcRoot
            let arrsChildUpdates = 
                chc.children |> 
                List.map(fun ch -> 
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    match pres with
                    | PresenceInt   (_, intVal) -> choiceDependencyIntPres_child v ch.presentWhenName intVal.Value
                    | PresenceStr   (_, strVal) -> raise(SemanticError(strVal.Location, "Unexpected presence condition. Expected integer, found string")))
            let updateStatement = choiceDependencyPres choicePath.arg.p (lm.lg.getAcces choicePath.arg) arrsChildUpdates
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
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
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepPresenceStr   (relPath, chc, str)               -> 
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let v = lm.lg.getValue vTarget.arg
            let choicePath, checkPath = getAccessFromScopeNodeList d.asn1Type false lm pSrcRoot
            let arrsChildUpdates = 
                chc.children |> 
                List.map(fun ch -> 
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    match pres with
                    | PresenceInt   (_, intVal) -> 
                        raise(SemanticError(intVal.Location, "Unexpected presence condition. Expected string, found integer"))
                        //choiceDependencyIntPres_child v ch.presentWhenName intVal.Value
                    | PresenceStr   (_, strVal) -> 
                        let arrNuls = [0 .. ((int str.maxSize.acn)- strVal.Value.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
                        choiceDependencyStrPres_child v ch.presentWhenName strVal.Value arrNuls)
            let updateStatement = choiceDependencyPres choicePath.arg.p (lm.lg.getAcces choicePath.arg) arrsChildUpdates
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
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
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[]; testCaseFnc = testCaseFnc; localVariables=[]}), us
    | AcnDepChoiceDeteterminant       (enm,chc)      -> 
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let v = lm.lg.getValue vTarget.arg
            let choicePath, checkPath = getAccessFromScopeNodeList d.asn1Type false lm pSrcRoot
            let defOrRef (a:Asn1AcnAst.ReferenceToEnumerated) =
                match m.Name.Value = a.modName with
                | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}
                | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName); typedefName = ToC (r.args.TypePrefix + a.tasName) ; definedInRtl = false}
            
            let arrsChildUpdates = 
                chc.children |> 
                List.map(fun ch -> 
                    let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = ch.Name.Value)
                    choiceDependencyEnum_Item v ch.presentWhenName (lm.lg.getNamedItemBackendName (Some (defOrRef enm)) enmItem ) )
            let updateStatement = choiceDependencyEnum choicePath.arg.p (lm.lg.getAcces choicePath.arg) arrsChildUpdates
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us

and getUpdateFunctionUsedInEncoding (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (acnChildOrAcnParameterId) (us:State) : (AcnChildUpdateResult option*State)=
    let multiAcnUpdate       = lm.acn.MultiAcnUpdate

    match deps.acnDependencies |> List.filter(fun d -> d.determinant.id = acnChildOrAcnParameterId) with
    | []  -> 
        None, us
    | d1::[]    -> 
        let ret, ns = handleSingleUpdateDependency r deps lm m d1 us
        ret, ns
    | d1::dds         -> 
        let _errCodeName         = ToC ("ERR_ACN" + (Encode.suffix.ToUpper()) + "_UPDATE_" + ((acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errCode, us = getNextValidErrorCode us _errCodeName None


        let ds = d1::dds
        let c_name0 = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) 0
        let localVars (typedefName :string) = 
            ds |> 
            List.mapi(fun i d1 -> 
                let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                let typegetDeterminantTypeDefinitionBodyWithinSeq = typedefName // getDeterminantTypeDefinitionBodyWithinSeq r l d1.determinant
                [AcnInsertedChild (c_name, typegetDeterminantTypeDefinitionBodyWithinSeq); BooleanLocalVariable (c_name+"_is_initialized", Some false)]) |>
            List.collect(fun lvList -> lvList |> List.map (fun lv -> lm.lg.getLocalVariableDeclaration  lv))
        let localUpdateFuns,ns =
            ds |>
            List.fold(fun (updates, ns) d1 -> 
                let f1, nns = handleSingleUpdateDependency r deps lm m d1 ns 
                updates@[f1], nns) ([],us)
        let restErrCodes = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.errCodes)
        let restLocalVariables = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.localVariables)
        let multiUpdateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let v = lm.lg.getValue vTarget.arg
            let arrsLocalUpdateStatements = 
                localUpdateFuns |> 
                List.mapi(fun i fn -> 
                    let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    let lv = {CallerScope.modName = vTarget.modName; arg = VALUE c_name}
                    match fn with
                    | None      -> None
                    | Some fn   -> Some(fn.updateAcnChildFnc typedefName lv pSrcRoot)) |>
                List.choose id
            let v0 = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) 0
            let arrsGetFirstIntValue =
                ds |>
                List.mapi (fun i d -> 
                    let cmp = getDeterminantTypeUpdateMacro r lm d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    cmp v vi (i=0) )
            let arrsLocalCheckEquality = 
                ds |> 
                List.mapi (fun i d -> 
                    let cmp = getDeterminantTypeCheckEqual r lm d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    cmp v vi )
            let updateStatement = multiAcnUpdate vTarget.arg.p c_name0 (errCode.errCodeName ) (localVars typedefName) arrsLocalUpdateStatements arrsGetFirstIntValue arrsLocalCheckEquality
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
                    
        let ret = Some(({AcnChildUpdateResult.updateAcnChildFnc = multiUpdateFunc; errCodes=errCode::restErrCodes ; testCaseFnc = testCaseFnc; localVariables = restLocalVariables}))
        ret, ns

type private AcnSequenceStatement =
    | AcnPresenceStatement
    | Asn1ChildEncodeStatement
    | AcnChildUpdateStatement
    | AcnChildEncodeStatement

type private HandleChild_Aux = {
    statementKind           : AcnSequenceStatement
    acnPresenceStatement    : string option
    localVariableList       : LocalVariable list
    errCodeList             : ErroCode list
}

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (acnPrms:DastAcnParameter list) (us:State)  =
    (*
        1. all Acn inserted children are declared as local variables in the encoded and decode functions (declaration step)
        2. all Acn inserted children must be initialized appropriatelly in the encoding phase
        3. 
    *)

    // stg macros
    let sequence_presense_optChild                      = lm.acn.sequence_presense_optChild                        
    let sequence_presense_optChild_pres_bool            = lm.acn.sequence_presense_optChild_pres_bool              
    let sequence_presense_optChild_pres_acn_expression  = lm.acn.sequence_presense_optChild_pres_acn_expression    
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

    let acnExpressionToBackendExpression (seq:Asn1AcnAst.Sequence) (pSeq:CallerScope) (exp:AcnExpression) =
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


        let rec getChildResult (seq:Asn1AcnAst.Sequence) (pSeq:CallerScope) (RelativePath lp) =
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
                        | Asn1AcnAst.Boolean        _  -> {pSeq with arg = lm.lg.getSeqChild pSeq.arg (lm.lg.getAsn1ChildBackendName0 ch) false} 
                        | Asn1AcnAst.Sequence s when xs.Length > 1 -> getChildResult s {pSeq with arg = lm.lg.getSeqChild pSeq.arg (lm.lg.getAsn1ChildBackendName0 ch) false} (RelativePath xs)
                        | _                 -> raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin "."))))


        let ret =
            AcnGenericTypes.foldAcnExpression
                (fun i s -> ( (0, i.Value.ToString()) , 0))
                (fun i s -> ( (0,"") , 0))
                (fun i s -> ( (0, i.Value.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo)) , 0))
                (fun i s -> ( (0, i.Value.ToString().ToLower()) , 0))
                (fun lf s ->
                    let plf = getChildResult seq pSeq lf
                    (0, plf.arg.p) , 0)
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
    let funcBody (us:State) (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
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
                | None      -> Some (sequence_presense_optChild p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child)  errCode.errCodeName codec)
                | Some _    -> None

        let localVariables =
            match asn1Children |> List.choose  printPresenceBit with
            | x::_  when lm.lg.uper.requires_presenceBit  && codec = CommonTypes.Decode -> (FlagLocalVariable ("presenceBit", None))::acnlocalVariables
            | _        -> acnlocalVariables
        
        let td = lm.lg.getSequenceTypeDefinition o.typeDef


        let localVariables, post_encoding_function, soBitStreamPositionsLocalVar, soSaveInitialBitStrmStatement =
            let bitStreamPositionsLocalVar = sprintf "bitStreamPositions_%d" (t.id.SeqeuenceOfLevel + 1)
            let bsPosStart = sprintf "bitStreamPositions_start%d" (t.id.SeqeuenceOfLevel + 1)
            match o.acnProperties.postEncodingFunction with
            | Some (PostEncodingFunction (modFncName, fncName)) when codec = Encode  ->
                let actualFncName = 
                    match lm.lg.hasModules with 
                    | false ->  (ToC fncName.Value) 
                    | true -> 
                        match modFncName with
                        | None -> (ToC (r.args.mappingFunctionsModule.orElse "")) + "." + (ToC fncName.Value)
                        | Some modFncName -> (ToC modFncName.Value) + "." + (ToC fncName.Value)

                let fncCall = sequence_call_post_encoding_function (lm.lg.getPointer p.arg) (actualFncName) bsPosStart  bitStreamPositionsLocalVar
                let initialBitStrmStatement = sequence_save_bitStream_start bsPosStart codec
                [AcnInsertedChild(bitStreamPositionsLocalVar, td.extention_function_potisions); AcnInsertedChild(bsPosStart, bitStreamName)]@localVariables, Some fncCall, Some bitStreamPositionsLocalVar, Some initialBitStrmStatement
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
                    let fncCall = sequence_call_post_decoding_validator (lm.lg.getPointer p.arg) (actualFncName) bsPosStart  bitStreamPositionsLocalVar
                    let initialBitStrmStatement = sequence_save_bitStream_start bsPosStart codec
                    [AcnInsertedChild(bitStreamPositionsLocalVar, td.extention_function_potisions); AcnInsertedChild(bsPosStart, bitStreamName)]@localVariables, Some fncCall, Some bitStreamPositionsLocalVar, Some initialBitStrmStatement
                | _ ->  localVariables, None, None, None

            

        let handleChild (us:State) (child:SeqChildInfo) =
            let soSaveBitStrmPosStatement = None
//                match soBitStreamPositionsLocalVar with
//                | Some lvName when child.savePosition ->  Some (sequence_save_bitstream lvName (child.getBackendName l) codec)
//                | _                                    -> None
            match child with
            | Asn1Child child   -> 
                let chFunc = child.Type.getAcnFunction codec
                let childContentResult, ns1 = 
                    match chFunc with
                    | Some chFunc   -> chFunc.funcBodyAsSeqComp us [] ({p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName child) child.Type.isIA5String}) (lm.lg.getAsn1ChildBackendName child)
                    | None          -> None, us

                //handle present-when acn property
                let present_when_statements, ns2 =
                        let acnPresenceStatement, errCodes, ns1b = 
                            match child.Optionality with
                            | Some (Asn1AcnAst.Optional opt)   -> 
                                match opt.acnPresentWhen with
                                | None    -> None, [], ns1
                                | Some (PresenceWhenBool _)    -> 
                                    match codec with
                                    | Codec.Encode  -> None, [], ns1
                                    | Codec.Decode  ->
                                        let extField = getExternaField r deps child.Type.id
                                        Some(sequence_presense_optChild_pres_bool p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) extField codec), [], ns1
                                | Some (PresenceWhenBoolExpression exp)    -> 
                                    let _errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_PRESENT_WHEN_EXP_FAILED")
                                    let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None
                                    let retExp = acnExpressionToBackendExpression o p exp
                                    Some(sequence_presense_optChild_pres_acn_expression p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) retExp errCode.errCodeName codec), [errCode], ns1a
                            | _                 -> None, [], ns1
                        [(AcnPresenceStatement, acnPresenceStatement, [], errCodes)], ns1b

                let childEncDecStatement, ns3 = 
                    match childContentResult with
                    | None              -> [], ns2
                    | Some childContent ->
                        let childBody, chLocalVars = 
                            match child.Optionality with
                            | None                             -> Some (sequence_mandatory_child (lm.lg.getAsn1ChildBackendName child) childContent.funcBody soSaveBitStrmPosStatement codec), childContent.localVariables
                            | Some Asn1AcnAst.AlwaysAbsent     -> Some (sequence_always_absent_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody soSaveBitStrmPosStatement codec), []
                            | Some Asn1AcnAst.AlwaysPresent    -> Some(sequence_always_present_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody soSaveBitStrmPosStatement codec), childContent.localVariables
                            | Some (Asn1AcnAst.Optional opt)   -> 
                                match opt.defaultValue with
                                | None                   -> Some(sequence_optional_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody soSaveBitStrmPosStatement codec), childContent.localVariables
                                | Some v                 -> 
                                    let defInit= child.Type.initFunction.initByAsn1Value ({p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName child) child.Type.isIA5String}) (mapValue v).kind
                                    Some(sequence_default_child p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getAsn1ChildBackendName child) childContent.funcBody defInit soSaveBitStrmPosStatement codec), childContent.localVariables
                        [(Asn1ChildEncodeStatement, childBody, chLocalVars, childContent.errCodes)], ns2
                present_when_statements@childEncDecStatement,ns3
            | AcnChild  acnChild    -> 
                //handle updates
                //acnChild.c_name
                let childP = {CallerScope.modName = p.modName; arg= VALUE (getAcnDeterminantName acnChild.id)}

                let updtateStatement, ns1 = 
                    match codec with
                    | CommonTypes.Encode -> 
                        let pRoot : CallerScope = lm.lg.getParamType t codec  //????
                        let updateStatement, lvs, lerCodes = 
                            match acnChild.funcUpdateStatement with
                            | Some funcUpdateStatement -> Some (funcUpdateStatement.updateAcnChildFnc acnChild.typeDefinitionBodyWithinSeq childP pRoot), funcUpdateStatement.localVariables, funcUpdateStatement.errCodes
                            | None                     -> None, [], []

                        [(AcnChildUpdateStatement, updateStatement, lvs, lerCodes)], us
                    | CommonTypes.Decode -> [], us

                //acn child encode/decode
                let childEncDecStatement, ns2 = 
                    let chFunc = acnChild.funcBody codec
                    let childContentResult = chFunc []  childP
                    match childContentResult with
                    | None              -> [],ns1
                    | Some childContent ->
                        match codec with
                        | Encode   ->
                            match acnChild.Type with
                            | Asn1AcnAst.AcnNullType _   ->
                                let childBody = Some (sequence_mandatory_child acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement codec)
                                [(AcnChildEncodeStatement, childBody, childContent.localVariables, childContent.errCodes)], ns1
                            | _             ->
                                let _errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((acnChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_UNITIALIZED")
                                let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None
                        
                                let childBody = Some (sequence_acn_child acnChild.c_name childContent.funcBody errCode.errCodeName soSaveBitStrmPosStatement codec)
                                [(AcnChildEncodeStatement, childBody, childContent.localVariables, errCode::childContent.errCodes)], ns1a
                        | Decode    ->
                            let childBody = Some (sequence_mandatory_child acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement codec)
                            [(AcnChildEncodeStatement, childBody, childContent.localVariables, childContent.errCodes)], ns1
                updtateStatement@childEncDecStatement, ns2

        // find acn inserted fields, which are not NULL types and which have no dependency.
        // For those fields we shoud generated no anc encode/decode function
        // Otherwise, the encoding function is wrong since an unitialized value is encoded.
        let existsAcnChildWithNoUpdates =
            acnChildren |>
            List.filter (fun acnChild -> match acnChild.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> true) |>
            List.filter(fun acnChild -> 
                let childP = {CallerScope.modName = p.modName; arg = VALUE (getAcnDeterminantName acnChild.id)}
                let pRoot : CallerScope = lm.lg.getParamType t codec  
                let updateStatement = 
                    match acnChild.funcUpdateStatement with
                    | Some funcUpdateStatement -> Some (funcUpdateStatement.updateAcnChildFnc acnChild.typeDefinitionBodyWithinSeq childP pRoot)
                    | None                     -> None
                updateStatement.IsNone)
        let saveInitialBitStrmStatements = soSaveInitialBitStrmStatement |> Option.toList
        let presenseBits = asn1Children |> List.choose printPresenceBit
        let childrenStatements00, ns = children |> foldMap handleChild us
        let childrenStatements0 = childrenStatements00 |> List.collect id 
        let childrenStatements = childrenStatements0 |> List.choose(fun (_, s,_,_) -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_, _,s,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_, _,_,s) -> s)
        
        
        let seqContent =  (saveInitialBitStrmStatements@presenseBits@childrenStatements@(post_encoding_function |> Option.toList)) |> nestChildItems lm codec 
        match existsAcnChildWithNoUpdates with
        | []     ->
            match seqContent with
            | None  -> 
                match codec with
                | Encode -> None, ns
                | Decode ->
                    match lm.lg.decodeEmptySeq p.arg.p with
                    | None -> None, ns
                    | Some decodeEmptySeq ->
                        Some ({AcnFuncBodyResult.funcBody = decodeEmptySeq; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced= false; bBsIsUnReferenced=true}), ns
            | Some ret -> Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced= false; bBsIsUnReferenced=false}), ns
            
        | errChild::_      -> 
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
            //Console.Out.WriteLine (FrontEntMain.formatSemanticWarning loc errMessage)
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
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  funcBody isTestVaseValid soSparkAnnotations  us



let createChoiceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (acnPrms:DastAcnParameter list)  (us:State)  =
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
        | true   -> acnChildren@alwaysAbsentChildren     //in Ada, we have to cover all cases even the ones that are always absent.


    let nMin = 0I
    let nMax = BigInteger(Seq.length acnChildren) - 1I
    //let nBits = (GetNumberOfBitsForNonNegativeInteger (nMax-nMin))
    let nIndexSizeInBits = (GetNumberOfBitsForNonNegativeInteger (BigInteger (acnChildren.Length - 1)))
    let sChoiceIndexName = (ToC t.id.AsString) + "_index_tmp"
    let ec =  
        match o.acnProperties.enumDeterminant with
        | Some _            -> 
            let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = t.id)
            match dependency.dependencyKind with
            | Asn1AcnAst.AcnDepChoiceDeteterminant (enm,_)  -> CEC_enum (enm, dependency.determinant)
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

    let typeDefinitionName = defOrRef.longTypedefName2 lm.lg.hasModules//getTypeDefinitionName t.id.tasInfo typeDefinition

    let funcBody (us:State) (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let td = (lm.lg.getChoiceTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules (ToC p.modName)
        let handleChild (us:State) (idx:int) (child:ChChildInfo) =
                let chFunc = child.chType.getAcnFunction codec
                let sCHildInitExpr = child.chType.initFunction.initExpression
                let childContentResult, ns1 = 
                    //match child.Optionality with
                    //| Some (ChoiceAlwaysAbsent) -> None//Some (always_false_statement errCode.errCodeName)
                    //| Some (ChoiceAlwaysPresent)
                    //| None  ->
                        match chFunc with
                        | Some chFunc   -> 
                            match lm.lg.acn.choice_requires_tmp_decoding with
                            | false   ->  chFunc.funcBody us [] ({p with arg = lm.lg.getChChild p.arg (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String})
                            | true when codec = CommonTypes.Decode  ->  chFunc.funcBody us [] ({CallerScope.modName = p.modName; arg = VALUE ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")})
                            | true   ->  chFunc.funcBody us [] ({p with arg = lm.lg.getChChild p.arg (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String})
                        | None          -> None, us

                let childContent_funcBody, childContent_localVariables, childContent_errCodes =
                    match childContentResult with
                    | None              -> 
                        match codec with
                        | Encode -> lm.lg.emtyStatement, [], []
                        | Decode ->
                            let childp = 
                                match lm.lg.acn.choice_requires_tmp_decoding with
                                | true ->   ({CallerScope.modName = p.modName; arg = VALUE ((lm.lg.getAsn1ChChildBackendName child) + "_tmp")})
                                | false ->  ({p with arg = lm.lg.getChChild p.arg (lm.lg.getAsn1ChChildBackendName child) child.chType.isIA5String})
                            let decStatement =
                                match child.chType.ActualType.Kind with
                                | NullType _    -> lm.lg.decode_nullType childp.arg.p
                                | Sequence _    -> lm.lg.decodeEmptySeq childp.arg.p
                                | _             -> None
                            match decStatement with
                            | None -> lm.lg.emtyStatement,[], []
                            | Some ret ->
                                ret ,[],[]

                    | Some childContent -> childContent.funcBody,  childContent.localVariables, childContent.errCodes

//                match childContentResult with
//                | None              -> [], ns1
//                | Some childContent ->
                let childBody = 
                    let sChildName = (lm.lg.getAsn1ChChildBackendName child)
                    let sChildTypeDef = child.chType.typeDefintionOrReference.longTypedefName2 lm.lg.hasModules //child.chType.typeDefinition.typeDefinitionBodyWithinSeq

                    let sChoiceTypeName = typeDefinitionName
                    match child.Optionality with
                    | Some (ChoiceAlwaysAbsent) -> Some (choiceChildAlwaysAbsent p.arg.p (lm.lg.getAcces p.arg) (lm.lg.presentWhenName (Some defOrRef) child) (BigInteger idx) errCode.errCodeName codec)
                    | Some (ChoiceAlwaysPresent)
                    | None  ->
                        match ec with
                        | CEC_uper  -> 
                            Some (choiceChild p.arg.p (lm.lg.getAcces p.arg) (lm.lg.presentWhenName (Some defOrRef) child) (BigInteger idx) nIndexSizeInBits nMax childContent_funcBody sChildName sChildTypeDef sChoiceTypeName sCHildInitExpr codec)
                        | CEC_enum (enm,_) -> 
                            let getDefOrRef (a:Asn1AcnAst.ReferenceToEnumerated) =
                                match p.modName = a.modName with
                                | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}
                                | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName); typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}


                            let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = child.Name.Value)
                            Some (choiceChild_Enum p.arg.p (lm.lg.getAcces p.arg) (lm.lg.getNamedItemBackendName (Some (getDefOrRef enm)) enmItem  ) (lm.lg.presentWhenName (Some defOrRef) child) childContent_funcBody sChildName sChildTypeDef sChoiceTypeName sCHildInitExpr codec)
                        | CEC_presWhen  ->
                            let handPresenseCond (cond:AcnGenericTypes.AcnPresentWhenConditionChoiceChild) =
                                match cond with
                                | PresenceInt  (relPath, intLoc)   -> 
                                    let extField = getExternaFieldChoizePresentWhen r deps t.id relPath
                                    choiceChild_preWhen_int_condition extField intLoc.Value
                                | PresenceStr  (relPath, strVal)   -> 
                                    let strType = 
                                        deps.acnDependencies |> 
                                        List.filter(fun d -> d.asn1Type = t.id) |>
                                        List.choose(fun d -> 
                                            match d.dependencyKind with
                                            | AcnDepPresenceStr(relPathCond, ch, str)  when relPathCond = relPath-> Some str
                                            | _     -> None) |> Seq.head


                                    let extField = getExternaFieldChoizePresentWhen r deps t.id relPath
                                    let arrNuls = [0 .. ((int strType.maxSize.acn) - strVal.Value.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
                                    choiceChild_preWhen_str_condition extField strVal.Value arrNuls
                            let conds = child.acnPresentWhenConditions |>List.map handPresenseCond
                            Some (choiceChild_preWhen p.arg.p (lm.lg.getAcces p.arg) (lm.lg.presentWhenName (Some defOrRef) child) childContent_funcBody conds (idx=0) sChildName sChildTypeDef sChoiceTypeName sCHildInitExpr codec)
                [(childBody, childContent_localVariables, childContent_errCodes)], ns1
        let childrenStatements00, ns = children |> List.mapi (fun i x -> i,x)  |> foldMap (fun us (i,x) ->  handleChild us i x) us
        let childrenStatements0 = childrenStatements00 |> List.collect id
        let childrenStatements = childrenStatements0 |> List.choose(fun (s,_,_) -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s) -> s)

        let choiceContent =  
            match ec with
            | CEC_uper        -> 
                //let ret = choice p.arg.p (lm.lg.getAcces p.arg) childrenContent (BigInteger (children.Length - 1)) sChoiceIndexName errCode.errCodeName typeDefinitionName nBits  codec
                choice_uper p.arg.p (lm.lg.getAcces p.arg) childrenStatements nMax sChoiceIndexName td nIndexSizeInBits errCode.errCodeName codec
            | CEC_enum   enm  -> 
                let extField = getExternaField r deps t.id
                choice_Enum p.arg.p (lm.lg.getAcces p.arg) childrenStatements extField errCode.errCodeName codec
            | CEC_presWhen    -> choice_preWhen p.arg.p  (lm.lg.getAcces p.arg) childrenStatements errCode.errCodeName codec
        Some ({AcnFuncBodyResult.funcBody = choiceContent; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced= false; bBsIsUnReferenced=false}), ns    


    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
    createAcnFunction r lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations us, ec

let createReferenceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
  let baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t.id "_ACN" codec
  match o.encodingOptions with 
  | None          -> 
      match o.hasExtraConstrainsOrChildrenOrAcnArgs with
      | true  ->
          match codec with
            | Codec.Encode  -> baseType.getAcnFunction codec, us
            | Codec.Decode  -> 
                let paramsArgsPairs = List.zip o.acnArguments o.resolvedType.acnParameters
                let baseTypeAcnFunction = baseType.getAcnFunction codec 
                let ret =
                    match baseTypeAcnFunction with
                    | None  -> None
                    | Some baseTypeAcnFunction   ->
                        let funcBody us (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
                            baseTypeAcnFunction.funcBody us (acnArgs@paramsArgsPairs) p
                        Some  {baseTypeAcnFunction with funcBody = funcBody} 

                ret, us
      | false ->    
            (*
            let moduleName, typeDefinitionName0 = 
                let t1 = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
                let typeDef = lm.lg.getTypeDefinition t1.FT_TypeDefintion
                typeDef.programUnit, typeDef.typeName
            let moduleName, typeDefinitionName0 = 
                match typeDefinition with
                | ReferenceToExistingDefinition refToExist   ->
                    match refToExist.programUnit with
                    | Some md -> md, refToExist.typedefName
                    | None    -> t.id.ModName, refToExist.typedefName
                | TypeDefinition                tdDef        -> t.id.ModName, tdDef.typedefName

            let baseTypeDefinitionName = 
                match lm.lg.hasModules with
                | false     -> typeDefinitionName0 
                | true   -> 
                    match t.id.ModName = o.modName.Value with
                    | true  -> typeDefinitionName0 
                    | false -> moduleName + "." + typeDefinitionName0 
            let baseFncName = baseTypeDefinitionName + "_ACN" + codec.suffix
                *)

            let funcBody (us:State) (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
                let funcBodyContent = callBaseTypeFunc lm (lm.lg.getParamValue t p.arg codec) baseFncName codec
                Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsUnReferenced= false; bBsIsUnReferenced=false}), us


            let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
            let a, ns = createAcnFunction r lm codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations us
            Some a, ns

    | Some( encOptions) ->
        //contained type i.e. MyOct ::= OCTET STRING (CONTAINING Other-Type)
        let loc = o.tasName.Location
        (*
        let moduleName, typeDefinitionName0 = 
            let t1 = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
            let typeDef = lm.lg.getTypeDefinition t1.FT_TypeDefintion
            typeDef.programUnit, typeDef.typeName
        let moduleName, typeDefinitionName0 = 
            match typeDefinition with
            | ReferenceToExistingDefinition refToExist   ->
                match refToExist.programUnit with
                | Some md -> md, refToExist.typedefName
                | None    -> t.id.ModName, refToExist.typedefName
            | TypeDefinition                tdDef        -> t.id.ModName, tdDef.typedefName

        let baseTypeDefinitionName = 
            match lm.lg.hasModules with
            | false     -> typeDefinitionName0 
            | true   -> 
                match t.id.ModName = o.modName.Value with
                | true  -> typeDefinitionName0 
                | false -> moduleName + "." + typeDefinitionName0 
        let baseFncName = baseTypeDefinitionName + "_ACN" + codec.suffix
        *)
        let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ACN_ENCODING" baseTypeDefinitionName
        let sReqBitForUperEncoding = sprintf "%s_REQUIRED_BITS_FOR_ACN_ENCODING" baseTypeDefinitionName
        
        let octet_string_containing_func            = lm.acn.octet_string_containing_func
        let bit_string_containing_func              = lm.acn.bit_string_containing_func
        let octet_string_containing_ext_field_func  = lm.acn.octet_string_containing_ext_field_func
        let bit_string_containing_ext_field_func    = lm.acn.bit_string_containing_ext_field_func

        let baseTypeAcnFunction = baseType.getAcnFunction codec 



        let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
            let funcBodyContent = 
                match encOptions.acnEncodingClass, encOptions.octOrBitStr with
                | SZ_EC_ExternalField    relPath    , ContainedInOctString  ->  
                    let filterDependency (d:AcnDependency) =
                        match d.dependencyKind with
                        | AcnDepSizeDeterminant_bit_oct_str_containt _   -> true
                        | _                              -> false


                    let extField        = getExternaField0 r deps t.id filterDependency

                    let soInner, errCodes0, localVariables0 =
                        match baseTypeAcnFunction with
                        | None  -> None, [], []
                        | Some baseTypeAcnFunction   ->
                            let acnRes, ns = baseTypeAcnFunction.funcBody us (acnArgs) p
                            match acnRes with
                            | None  -> None, [], []
                            | Some r -> Some r.funcBody, r.errCodes, r.localVariables

                    let fncBody = octet_string_containing_ext_field_func (lm.lg.getParamValue t p.arg codec)  baseFncName sReqBytesForUperEncoding extField errCode.errCodeName soInner codec
                    Some(fncBody, errCode::errCodes0,localVariables0)
                | SZ_EC_ExternalField    relPath    , ContainedInBitString  ->  
                    let extField        = getExternaField r deps t.id
                    let fncBody = bit_string_containing_ext_field_func (lm.lg.getParamValue t p.arg codec)  baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding extField errCode.errCodeName codec
                    Some(fncBody, [errCode],[])


                | SZ_EC_FIXED_SIZE        , ContainedInOctString  ->
                    let fncBody = octet_string_containing_func  (lm.lg.getParamValue t p.arg codec) baseFncName sReqBytesForUperEncoding 0I encOptions.minSize.acn encOptions.maxSize.acn true codec
                    Some(fncBody, [errCode],[])
                | SZ_EC_LENGTH_EMBEDDED nBits , ContainedInOctString  -> 
                    let fncBody = octet_string_containing_func  (lm.lg.getParamValue t p.arg codec) baseFncName sReqBytesForUperEncoding nBits encOptions.minSize.acn encOptions.maxSize.acn false codec
                    Some(fncBody, [errCode],[])
                | SZ_EC_FIXED_SIZE                        , ContainedInBitString  ->
                    let fncBody = bit_string_containing_func  (lm.lg.getParamValue t p.arg codec) baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding 0I encOptions.minSize.acn encOptions.maxSize.acn true codec
                    Some(fncBody, [errCode],[])
                | SZ_EC_LENGTH_EMBEDDED nBits                 , ContainedInBitString  ->  
                    let fncBody = bit_string_containing_func  (lm.lg.getParamValue t p.arg codec) baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding nBits encOptions.minSize.acn encOptions.maxSize.acn false codec
                    Some(fncBody, [errCode],[])
                | SZ_EC_TerminationPattern nullVal  ,  _                    ->  raise(SemanticError (loc, "Invalid type for parameter4"))

            match funcBodyContent with
            | None -> None
            | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; bValIsUnReferenced= false; bBsIsUnReferenced=false})

        let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)
        let a,b = createAcnFunction r lm codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us
        Some a, b











      
        
