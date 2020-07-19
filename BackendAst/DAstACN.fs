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

let foldMap = Asn1Fold.foldMap


let callBaseTypeFunc l = match l with C -> uper_c.call_base_type_func | Ada -> uper_a.call_base_type_func


let getAcnDeterminantName (id : ReferenceToType) =
    match id with
    | ReferenceToType path ->
        match path with
        | (MD mdName)::(TA tasName)::(PRM prmName)::[]   -> ToC2 prmName
        | _ ->
            let longName = id.AcnAbsPath.Tail |> Seq.StrJoin "_"
            ToC2(longName.Replace("#","elem"))


let getDeterminantTypeDefinitionBodyWithinSeq (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (det:Determinant) = 
    let createPrmAcnInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  =
        let Declare_Integer     =  match l with  C  -> header_c.Declare_Integer  | Ada   -> header_a.Declare_Integer 
        Declare_Integer ()

    let createAcnInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (a:Asn1AcnAst.AcnInteger) =
        let Declare_Integer     =  match l with  C  -> header_c.Declare_Integer  | Ada   -> header_a.Declare_Integer 
        let Declare_PosInteger  =  match l with  C  -> header_c.Declare_PosInteger  | Ada   -> header_a.Declare_PosInteger  
        match a.isUnsigned with
        | true     -> Declare_PosInteger ()
        | false    -> Declare_Integer ()
        
    
    let createAcnBoolean (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) =
        match l with
        | C                      -> header_c.Declare_Boolean ()
        | Ada                    -> header_a.Declare_BOOLEAN ()    

    let createAcnNull (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) =
        match l with
        | C                      -> header_c.Declare_NullType ()
        | Ada                    -> header_a.Declare_NULL ()
    
    let getTypeDefinitionName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (id : ReferenceToType) =
        let longName = id.AcnAbsPath.Tail |> Seq.StrJoin "_"
        ToC2(r.args.TypePrefix + longName.Replace("#","elem"))

    match det with
    | AcnChildDeterminant       ch ->
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a -> createAcnInteger r l a
        | Asn1AcnAst.AcnNullType _ -> createAcnNull r l
        | Asn1AcnAst.AcnBoolean  _ -> createAcnBoolean r l
        | Asn1AcnAst.AcnReferenceToEnumerated a -> ToC2(r.args.TypePrefix + a.tasName.Value)
        | Asn1AcnAst.AcnReferenceToIA5String a -> ToC2(r.args.TypePrefix + a.tasName.Value)

    | AcnParameterDeterminant   prm ->
        match prm.asn1Type with
        | AcnGenericTypes.AcnPrmInteger  _       -> createPrmAcnInteger r l 
        | AcnGenericTypes.AcnPrmBoolean  _       -> createAcnBoolean r l
        | AcnGenericTypes.AcnPrmNullType _       -> createAcnNull r l
        | AcnGenericTypes.AcnPrmRefType (md,ts)  -> getTypeDefinitionName r l (ReferenceToType [MD md.Value; TA ts.Value])


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
        |AcnGenericTypes.AcnPrmInteger  _       -> pri_macro
        |AcnGenericTypes.AcnPrmBoolean  _       -> pri_macro
        |AcnGenericTypes.AcnPrmNullType _       -> pri_macro
        |AcnGenericTypes.AcnPrmRefType (md,ts)  -> pri_macro

let getDeterminantTypeUpdateMacro (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (det:Determinant) = 
    let MultiAcnUpdate_get_first_init_value_pri     =  match l with C -> acn_c.MultiAcnUpdate_get_first_init_value_pri        | Ada -> acn_a.MultiAcnUpdate_get_first_init_value_pri
    let MultiAcnUpdate_get_first_init_value_str     =  match l with C -> acn_c.MultiAcnUpdate_get_first_init_value_str        | Ada -> acn_a.MultiAcnUpdate_get_first_init_value_str
    getDeterminant_macro r l det MultiAcnUpdate_get_first_init_value_pri MultiAcnUpdate_get_first_init_value_str

let getDeterminantTypeCheckEqual (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (det:Determinant) = 
    let multiAcnUpdate_checkEqual_pri     =  match l with C -> acn_c.MultiAcnUpdate_checkEqual_pri        | Ada -> acn_a.MultiAcnUpdate_checkEqual_pri
    let multiAcnUpdate_checkEqual_str     =  match l with C -> acn_c.MultiAcnUpdate_checkEqual_str        | Ada -> acn_a.MultiAcnUpdate_checkEqual_str
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

let handleSavePostion (funcBody:State-> ErroCode->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> ((AcnFuncBodyResult option)*State)) savePosition c_name (typeId:ReferenceToType) l (codec:CommonTypes.Codec) prms p =
    match savePosition with
    | false -> funcBody
    | true  -> 
        let newFuncBody st errCode prms p =
            let content, ns1a = funcBody st errCode prms p  
            let sequence_save_bitstream                 = match l with C -> acn_c.sequence_save_bitstream                     | Ada -> acn_a.sequence_save_bitstream              
            let lvName = sprintf "bitStreamPositions_%d" (typeId.SeqeuenceOfLevel + 1)
            let savePositionStatement = sequence_save_bitstream lvName c_name codec
            let newContent = 
                match content with
                | Some bodyResult   -> 
                    let funcBodyStr = sprintf "%s\n%s" savePositionStatement bodyResult.funcBody
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = savePositionStatement 
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; bValIsReferenced= true; bBsIsReferenced=false}                        
            newContent, ns1a
        newFuncBody

let handleAlignemntForAsn1Types (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (acnAligment     : AcnAligment option ) (funcBody:State-> ErroCode->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> ((AcnFuncBodyResult option)*State))  =
    let alignToNext                      =  match l with C -> acn_c.alignToNext                         | Ada -> acn_a.alignToNext
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
                    Some {funcBody = funcBodyStr; errCodes =[errCode]; localVariables = []; bValIsReferenced= true; bBsIsReferenced=false}                        
            newContent, ns1a
        newFuncBody

let handleAlignemntForAcnTypes (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (acnAligment : AcnAligment option ) (funcBody:CommonTypes.Codec -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> (AcnFuncBodyResult option))  =
    let alignToNext                      =  match l with C -> acn_c.alignToNext                         | Ada -> acn_a.alignToNext
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
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; bValIsReferenced= true; bBsIsReferenced=false}                        
            newContent
        newFuncBody


let private createAcnFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option)  (funcBody:State-> ErroCode->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> ((AcnFuncBodyResult option)*State)) isTestVaseValid soSparkAnnotations (us:State)  =
    let funcNameAndtasInfo   = 
        let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId:ReferenceToType) (td:FE_TypeDefinition) =
            match typeId.tasInfo with
            | None -> None
            | Some _ -> Some (td.typeName + "_ACN"  + codec.suffix)

        match t.acnParameters with
        | []    -> getFuncName r l codec t.id (t.FT_TypeDefintion.[l])
        | _     -> None
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((t.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let EmitTypeAssignment_primitive     =  match l with C -> acn_c.EmitTypeAssignment_primitive        | Ada -> acn_a.EmitTypeAssignment_primitive
    let EmitTypeAssignment_primitive_def =  match l with C -> acn_c.EmitTypeAssignment_primitive_def    | Ada -> acn_a.EmitTypeAssignment_primitive_def
    let EmitTypeAssignment_def_err_code  =  match l with C -> acn_c.EmitTypeAssignment_def_err_code     | Ada -> acn_a.EmitTypeAssignment_def_err_code

    


    let funcBodyAsSeqComp st prms p c_name : ((AcnFuncBodyResult option)*State) =
        let funcBody = handleSavePostion funcBody t.SaveBitStreamPosition c_name t.id l codec prms p
        let ret = handleAlignemntForAsn1Types r l codec t.acnAligment funcBody
        ret st  errCode prms p 
        

    let funcBody = handleAlignemntForAsn1Types r l codec t.acnAligment funcBody

    let p : CallerScope = t.getParamType l codec
    let topLevAcc = p.arg.getAcces l
    let varName = p.arg.p
    let sStar = p.arg.getStar l
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
                        let emtyStatement = match l with C -> "" | Ada -> "null;"
                        emtyStatement, [], [], true, isValidFuncName.IsNone
                    | Some bodyResult   -> 
                        let bVarNameIsUnreferenced =
                            match codec with
                            | Encode -> isValidFuncName.IsNone && bodyResult.bValIsReferenced
                            | Decode -> isValidFuncName.IsNone && bodyResult.bValIsReferenced

                        bodyResult.funcBody, bodyResult.errCodes, bodyResult.localVariables, bodyResult.bBsIsReferenced, (bVarNameIsUnreferenced || bodyResult.bValIsReferenced)

                let handleAcnParameter (p:AcnGenericTypes.AcnParameter) =
                    let intType  = match l with C -> header_c.Declare_Integer () | Ada -> header_a.Declare_Integer ()
                    let boolType = match l with C -> header_c.Declare_Boolean () | Ada -> header_a.Declare_BOOLEAN ()
                    let emitPrm  = match l with C -> acn_c.EmitAcnParameter      | Ada -> acn_a.EmitAcnParameter
                    match p.asn1Type with
                    | AcnGenericTypes.AcnPrmInteger    loc          -> emitPrm p.c_name intType
                    | AcnGenericTypes.AcnPrmBoolean    loc          -> emitPrm p.c_name boolType
                    | AcnGenericTypes.AcnPrmNullType   loc          -> raise(SemanticError (loc, "Invalid type for parameter"))
                    | AcnGenericTypes.AcnPrmRefType(md,ts)          -> 
                        let prmTypeName =
                            match l with
                            | C         -> ToC2(r.args.TypePrefix + ts.Value)
                            | Ada       -> 
                                match md.Value = t.id.ModName with
                                | true  -> ToC2(r.args.TypePrefix + ts.Value)
                                | false -> (ToC2 md.Value) + "." + ToC2(r.args.TypePrefix + ts.Value)
                        emitPrm p.c_name prmTypeName

                let lvars = bodyResult_localVariables |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                let prms = t.acnParameters |> List.map handleAcnParameter
                let prmNames = t.acnParameters |> List.map (fun p -> p.c_name)
                let func = Some(EmitTypeAssignment_primitive varName sStar funcName isValidFuncName  (typeDefinition.longTypedefName l) lvars  bodyResult_funcBody soSparkAnnotations sInitilialExp prms prmNames (t.acnMaxSizeInBits = 0I) bBsIsUnreferenced bVarNameIsUnreferenced codec)
                
                //let errCodes = bodyResult.errCodes
                let errCodStr = errCodes |> List.map(fun x -> (EmitTypeAssignment_def_err_code x.errCodeName) (BigInteger x.errCodeValue))
                let funcDef = Some(EmitTypeAssignment_primitive_def varName sStar funcName  (typeDefinition.longTypedefName l) errCodStr (t.acnMaxSizeInBits = 0I) (BigInteger (ceil ((double t.acnMaxSizeInBits)/8.0))) ( t.acnMaxSizeInBits) prms codec)
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

let private createAcnIntegerFunctionInternal (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (uperRange : BigIntegerUperRange) acnEncodingClass (uperfuncBody : ErroCode -> CallerScope -> (UPERFuncBodyResult option)) (soMF:string option, soMFM:string option) : (ErroCode -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> (AcnFuncBodyResult option))  =
    let PositiveInteger_ConstSize_8                  = match l with C -> acn_c.PositiveInteger_ConstSize_8                | Ada -> acn_a.PositiveInteger_ConstSize_8               
    let PositiveInteger_ConstSize_big_endian_16      = match l with C -> acn_c.PositiveInteger_ConstSize_big_endian_16    | Ada -> acn_a.PositiveInteger_ConstSize_big_endian_16   
    let PositiveInteger_ConstSize_little_endian_16   = match l with C -> acn_c.PositiveInteger_ConstSize_little_endian_16 | Ada -> acn_a.PositiveInteger_ConstSize_little_endian_16
    let PositiveInteger_ConstSize_big_endian_32      = match l with C -> acn_c.PositiveInteger_ConstSize_big_endian_32    | Ada -> acn_a.PositiveInteger_ConstSize_big_endian_32   
    let PositiveInteger_ConstSize_little_endian_32   = match l with C -> acn_c.PositiveInteger_ConstSize_little_endian_32 | Ada -> acn_a.PositiveInteger_ConstSize_little_endian_32
    let PositiveInteger_ConstSize_big_endian_64      = match l with C -> acn_c.PositiveInteger_ConstSize_big_endian_64    | Ada -> acn_a.PositiveInteger_ConstSize_big_endian_64   
    let PositiveInteger_ConstSize_little_endian_64   = match l with C -> acn_c.PositiveInteger_ConstSize_little_endian_64 | Ada -> acn_a.PositiveInteger_ConstSize_little_endian_64
    let PositiveInteger_ConstSize                    = match l with C -> acn_c.PositiveInteger_ConstSize                  | Ada -> acn_a.PositiveInteger_ConstSize                 
    let TwosComplement_ConstSize_8                   = match l with C -> acn_c.TwosComplement_ConstSize_8                 | Ada -> acn_a.TwosComplement_ConstSize_8                
    let TwosComplement_ConstSize_big_endian_16       = match l with C -> acn_c.TwosComplement_ConstSize_big_endian_16     | Ada -> acn_a.TwosComplement_ConstSize_big_endian_16    
    let TwosComplement_ConstSize_little_endian_16    = match l with C -> acn_c.TwosComplement_ConstSize_little_endian_16  | Ada -> acn_a.TwosComplement_ConstSize_little_endian_16 
    let TwosComplement_ConstSize_big_endian_32       = match l with C -> acn_c.TwosComplement_ConstSize_big_endian_32     | Ada -> acn_a.TwosComplement_ConstSize_big_endian_32    
    let TwosComplement_ConstSize_little_endian_32    = match l with C -> acn_c.TwosComplement_ConstSize_little_endian_32  | Ada -> acn_a.TwosComplement_ConstSize_little_endian_32 
    let TwosComplement_ConstSize_big_endian_64       = match l with C -> acn_c.TwosComplement_ConstSize_big_endian_64     | Ada -> acn_a.TwosComplement_ConstSize_big_endian_64    
    let TwosComplement_ConstSize_little_endian_64    = match l with C -> acn_c.TwosComplement_ConstSize_little_endian_64  | Ada -> acn_a.TwosComplement_ConstSize_little_endian_64 
    let TwosComplement_ConstSize                     = match l with C -> acn_c.TwosComplement_ConstSize                   | Ada -> acn_a.TwosComplement_ConstSize                  
    let ASCII_ConstSize                              = match l with C -> acn_c.ASCII_ConstSize                            | Ada -> acn_a.ASCII_ConstSize                           
    let ASCII_VarSize_NullTerminated                 = match l with C -> acn_c.ASCII_VarSize_NullTerminated               | Ada -> acn_a.ASCII_VarSize_NullTerminated              
    //+++ todo write ada stg macros for ASCII_UINT_ConstSize, ASCII_UINT_VarSize_NullTerminated
    let ASCII_UINT_ConstSize                         = match l with C -> acn_c.ASCII_UINT_ConstSize                       | Ada -> acn_a.ASCII_UINT_ConstSize                      
    let ASCII_UINT_VarSize_NullTerminated            = match l with C -> acn_c.ASCII_UINT_VarSize_NullTerminated          | Ada -> acn_a.ASCII_UINT_VarSize_NullTerminated         
    let BCD_ConstSize                                = match l with C -> acn_c.BCD_ConstSize                              | Ada -> acn_a.BCD_ConstSize                             
    let BCD_VarSize_NullTerminated                   = match l with C -> acn_c.BCD_VarSize_NullTerminated                 | Ada -> acn_a.BCD_VarSize_NullTerminated                

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
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let uIntActualMax (nBits:int) =
            let a = 2I**nBits - 1I
            min a nUperMax
        let sIntActualMin (nBits:int) =
            let a = -(2I**(nBits-1))
            max a nUperMin
        let sIntActualMax (nBits:int) =
            let a = 2I**(nBits-1) - 1I
            min a nUperMax
        
        //let soMF = match 
        let funcBodyContent = 
            match acnEncodingClass with
            |Asn1AcnAst.Integer_uPER                                       ->  uperfuncBody errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
            |Asn1AcnAst.PositiveInteger_ConstSize_8                        ->  Some(PositiveInteger_ConstSize_8 pp errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 8)  codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_16            ->  Some(PositiveInteger_ConstSize_big_endian_16 pp errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 16) codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_16         ->  Some(PositiveInteger_ConstSize_little_endian_16 pp errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 16) codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_32            ->  Some(PositiveInteger_ConstSize_big_endian_32 pp errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 32) codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_32         ->  Some(PositiveInteger_ConstSize_little_endian_32 pp errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 32) codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_big_endian_64            ->  Some(PositiveInteger_ConstSize_big_endian_64 pp errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 64) codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize_little_endian_64         ->  Some(PositiveInteger_ConstSize_little_endian_64 pp errCode.errCodeName soMF soMFM (max 0I nUperMin) (uIntActualMax 64) codec, [errCode])
            |Asn1AcnAst.PositiveInteger_ConstSize bitSize                  ->  Some(PositiveInteger_ConstSize pp errCode.errCodeName ( bitSize) soMF soMFM (max 0I nUperMin) (uIntActualMax (int bitSize)) codec, [errCode])
            
            |Asn1AcnAst.TwosComplement_ConstSize_8                         ->  Some(TwosComplement_ConstSize_8 pp errCode.errCodeName soMF soMFM (sIntActualMin 8) (sIntActualMax 8) codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_16             ->  Some(TwosComplement_ConstSize_big_endian_16 pp errCode.errCodeName soMF soMFM (sIntActualMin 16) (sIntActualMax 16) codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_16          ->  Some(TwosComplement_ConstSize_little_endian_16 pp errCode.errCodeName soMF soMFM (sIntActualMin 16) (sIntActualMax 16) codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_32             ->  Some(TwosComplement_ConstSize_big_endian_32 pp errCode.errCodeName soMF soMFM (sIntActualMin 32) (sIntActualMax 32) codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_32          ->  Some(TwosComplement_ConstSize_little_endian_32 pp errCode.errCodeName soMF soMFM (sIntActualMin 32) (sIntActualMax 32) codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_big_endian_64             ->  Some(TwosComplement_ConstSize_big_endian_64 pp errCode.errCodeName soMF soMFM (sIntActualMin 64) (sIntActualMax 64) codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize_little_endian_64          ->  Some(TwosComplement_ConstSize_little_endian_64 pp errCode.errCodeName soMF soMFM (sIntActualMin 64) (sIntActualMax 64) codec, [errCode])
            |Asn1AcnAst.TwosComplement_ConstSize bitSize                   ->  Some(TwosComplement_ConstSize pp errCode.errCodeName soMF soMFM ( bitSize) (sIntActualMin (int bitSize)) (sIntActualMax (int bitSize)) codec, [errCode])

            |Asn1AcnAst.ASCII_ConstSize size                               ->  Some(ASCII_ConstSize pp errCode.errCodeName soMF soMFM nUperMin nUperMax (( size)/8I) codec, [errCode])
            |Asn1AcnAst.ASCII_VarSize_NullTerminated nullBytes             ->  Some(ASCII_VarSize_NullTerminated pp errCode.errCodeName soMF soMFM nUperMin nUperMax nullBytes codec, [errCode])
            |Asn1AcnAst.ASCII_UINT_ConstSize size                          ->  Some(ASCII_UINT_ConstSize pp errCode.errCodeName soMF soMFM nUperMin nUperMax (( size)/8I) codec, [errCode])
            |Asn1AcnAst.ASCII_UINT_VarSize_NullTerminated nullBytes         ->  Some(ASCII_UINT_VarSize_NullTerminated pp errCode.errCodeName  soMF soMFM nUperMin nUperMax nullBytes codec, [errCode])
            |Asn1AcnAst.BCD_ConstSize size                                 ->  Some(BCD_ConstSize pp errCode.errCodeName soMF soMFM nUperMin nUperMax (( size)/4I) codec, [errCode])
            |Asn1AcnAst.BCD_VarSize_NullTerminated nullBytes                ->  Some(BCD_VarSize_NullTerminated pp errCode.errCodeName soMF soMFM nUperMin nUperMax codec, [errCode])
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsReferenced= false; bBsIsReferenced=false})
    //let funcBody = (funcBody errCode)
    funcBody

let getMappingFunctionModule (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (soMapFuncName:string option) = 
    match l with
    | C     -> None
    | Ada   ->
        match soMapFuncName with
        | None  -> None
        | Some sMapFuncName ->
            let knownMappingFunctions = ["milbus"]
            match knownMappingFunctions |> Seq.exists ((=) sMapFuncName) with
            | true  -> Some (acn_a.rtlModuleName() )
            | false -> r.args.mappingFunctionsModule
let createAcnIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnInteger)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let uperFuncBody (errCode) (p:CallerScope) = 
        DAstUPer.getIntfuncBodyByCons r l codec t.uperRange t.Location t.isUnsigned (t.cons) (t.cons@t.withcons) errCode p
        (*let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let IntUnconstraint = match l with C -> uper_c.IntUnconstraint          | Ada -> uper_a.IntUnconstraint
        let funcBodyContent = IntUnconstraint pp errCode.errCodeName false codec
        Some {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []}    *)
    let soMapFunc = match t.acnProperties.mappingFunction with Some (MappingFunction mapFncName)    -> Some mapFncName.Value | None -> None
    let soMapFunMod = getMappingFunctionModule r l soMapFunc
    let funcBody = createAcnIntegerFunctionInternal r l codec t.uperRange t.acnEncodingClass uperFuncBody (soMapFunc, soMapFunMod)
    (funcBody errCode), ns



let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let soMapFunc = match o.acnProperties.mappingFunction with Some (MappingFunction mapFncName)    -> Some mapFncName.Value | None -> None
    let soMapFunMod = getMappingFunctionModule r l soMapFunc
    let funcBody = createAcnIntegerFunctionInternal r l codec o.uperRange o.acnEncodingClass uperFunc.funcBody_e (soMapFunc, soMapFunMod)
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let createEnumComn (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefintionOrReference ) (typeDefinitionName:string)  =
    let EnumeratedEncValues                 = match l with C -> acn_c.EnumeratedEncValues             | Ada -> acn_a.EnumeratedEncValues
    let Enumerated_item                     = match l with C -> acn_c.Enumerated_item                 | Ada -> acn_a.Enumerated_item
    //let IntFullyConstraint                  = match l with C -> uper_c.IntFullyConstraint       | Ada -> uper_a.IntFullyConstraint
    let IntFullyConstraintPos   = match l with C -> uper_c.IntFullyConstraintPos    | Ada -> uper_a.IntFullyConstraintPos
    let min = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.min
    let max = o.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.max
    let intVal = "intVal"
    let sFirstItemName = o.items.Head.getBackendName (Some defOrRef) l
    let localVar =
        match min >= 0I with
        | true -> Asn1UIntLocalVariable (intVal,None)
        | false -> Asn1SIntLocalVariable (intVal,None)
    let pVal = {CallerScope.modName = typeId.ModName; arg = VALUE intVal}
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
        let intFuncBody = 
            let uperInt (errCode:ErroCode) (p:CallerScope) = 
                let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
                let funcBody = IntFullyConstraintPos pp min max (GetNumberOfBitsForNonNegativeInteger (max-min))  errCode.errCodeName codec
                Some({UPERFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables= []})
            createAcnIntegerFunctionInternal r l codec (Concrete (min,max)) o.acnEncodingClass uperInt (None, None)
        let funcBodyContent = 
            match intFuncBody errCode acnArgs pVal with
            | None      -> None
            | Some(intAcnFuncBdResult) ->
                let arrItems = o.items |> List.map(fun it -> Enumerated_item (p.arg.getValue l) (it.getBackendName (Some defOrRef) l) it.acnEncodeValue codec)
                Some (EnumeratedEncValues (p.arg.getValue l) td arrItems intAcnFuncBdResult.funcBody errCode.errCodeName sFirstItemName codec, intAcnFuncBdResult.errCodes, localVar::intAcnFuncBdResult.localVariables)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; bValIsReferenced= false; bBsIsReferenced=false})
    funcBody

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefintionOrReference) (typeDefinition:TypeDefintionOrReference)   (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let typeDefinitionName = defOrRef.longTypedefName l //getTypeDefinitionName t.id.tasInfo typeDefinition
    let funcBody = createEnumComn r l codec t.id o defOrRef typeDefinitionName
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations  us


let createAcnEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToEnumerated)  (defOrRef:TypeDefintionOrReference) (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName
    let typeDefinitionName = (t.getType r).FT_TypeDefintion.[l].typeName
    let funcBody = createEnumComn r l codec typeId t.enumerated defOrRef typeDefinitionName
    (funcBody errCode), ns



let createRealrFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Real_32_big_endian                  = match l with C -> acn_c.Real_32_big_endian                | Ada -> acn_a.Real_32_big_endian
    let Real_64_big_endian                  = match l with C -> acn_c.Real_64_big_endian                | Ada -> acn_a.Real_64_big_endian
    let Real_32_little_endian               = match l with C -> acn_c.Real_32_little_endian             | Ada -> acn_a.Real_32_little_endian
    let Real_64_little_endian               = match l with C -> acn_c.Real_64_little_endian             | Ada -> acn_a.Real_64_little_endian
    
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let funcBodyContent = 
            match o.acnEncodingClass with
            | Real_IEEE754_32_big_endian            -> Some (Real_32_big_endian pp errCode.errCodeName codec, [errCode])
            | Real_IEEE754_64_big_endian            -> Some (Real_64_big_endian pp errCode.errCodeName codec, [errCode])
            | Real_IEEE754_32_little_endian         -> Some (Real_32_little_endian pp errCode.errCodeName codec, [errCode])
            | Real_IEEE754_64_little_endian         -> Some (Real_64_little_endian pp errCode.errCodeName codec, [errCode])
            | Real_uPER                             -> uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsReferenced= false; bBsIsReferenced=false})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getPointer l | CommonTypes.Decode -> p.arg.getPointer l
        let funcBodyContent = 
            uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsReferenced= false; bBsIsReferenced=false})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getPointer l | CommonTypes.Decode -> p.arg.getPointer l
        let funcBodyContent = 
            uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes)
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = []; bValIsReferenced= false; bBsIsReferenced=false})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


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



let createAcnBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnBoolean)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let Boolean         = match l with C -> uper_c.Boolean          | Ada -> uper_a.Boolean
        let funcBodyContent = 
            Boolean pp errCode.errCodeName codec
        Some {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsReferenced= false; bBsIsReferenced=false}    
    (funcBody errCode), ns

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefintionOrReference) (baseTypeUperFunc : AcnFunction option) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let pvalue = p.arg.getValue l
        let ptr = p.arg.getPointer l
        let Boolean         = match l with C -> uper_c.Boolean          | Ada -> uper_a.Boolean
        let acnBoolean      = match l with C -> acn_c.Boolean          | Ada -> acn_a.Boolean
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
                
        {AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = []; bValIsReferenced= false; bBsIsReferenced=false}    
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> Some (funcBody e acnArgs p), us) (fun atc -> true) soSparkAnnotations us




let createAcnNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec)  (typeId : ReferenceToType) (o:Asn1AcnAst.AcnNullType)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nullType         = match l with C -> acn_c.Null          | Ada -> acn_a.Null_pattern2
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
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; bValIsReferenced= false; bBsIsReferenced=false})
    (funcBody errCode), ns

let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (us:State)  =
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let nullType         = match l with C -> acn_c.Null          | Ada -> acn_a.Null_pattern
        
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
            Some ({AcnFuncBodyResult.funcBody = ret; errCodes = [errCode]; localVariables = []; bValIsReferenced= false; bBsIsReferenced=false})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


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

let createStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefintionOrReference)  (defOrRef:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let Acn_String_Ascii_FixSize                            = match l with C -> acn_c.Acn_String_Ascii_FixSize                          | Ada -> acn_a.Acn_String_Ascii_FixSize
    let Acn_String_Ascii_Internal_Field_Determinant         = match l with C -> acn_c.Acn_String_Ascii_Internal_Field_Determinant       | Ada -> acn_a.Acn_String_Ascii_Internal_Field_Determinant
    let Acn_String_Ascii_Null_Teminated                     = match l with C -> acn_c.Acn_String_Ascii_Null_Teminated                   | Ada -> acn_a.Acn_String_Ascii_Null_Teminated
    let Acn_String_Ascii_External_Field_Determinant         = match l with C -> acn_c.Acn_String_Ascii_External_Field_Determinant       | Ada -> acn_a.Acn_String_Ascii_External_Field_Determinant
    let Acn_String_CharIndex_External_Field_Determinant     = match l with C -> acn_c.Acn_String_CharIndex_External_Field_Determinant   | Ada -> acn_a.Acn_String_CharIndex_External_Field_Determinant
    let Acn_IA5String_CharIndex_External_Field_Determinant  = match l with C -> acn_c.Acn_IA5String_CharIndex_External_Field_Determinant   | Ada -> acn_a.Acn_IA5String_CharIndex_External_Field_Determinant
    
    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
        let funcBodyContent = 
            match o.acnEncodingClass with
            | Acn_Enc_String_uPER  _                                           -> uperFunc.funcBody_e errCode p |> Option.map(fun x -> x.funcBody, x.errCodes, x.localVariables)
            | Acn_Enc_String_uPER_Ascii _                                      -> 
                match o.maxSize.uper = o.minSize.uper with
                | true      ->  Some (Acn_String_Ascii_FixSize pp errCode.errCodeName ( o.maxSize.uper) codec, [errCode], [])
                | false     ->  
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                    Some (Acn_String_Ascii_Internal_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) ( o.minSize.acn) nSizeInBits codec , [errCode], [])
            | Acn_Enc_String_Ascii_Null_Teminated                   (_,nullChars)   -> Some (Acn_String_Ascii_Null_Teminated pp errCode.errCodeName ( o.maxSize.acn) nullChars codec, [errCode], [])
            | Acn_Enc_String_Ascii_External_Field_Determinant       _    -> 
                let extField = getExternaField r deps t.id
                Some(Acn_String_Ascii_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) extField codec, [errCode], [])
            | Acn_Enc_String_CharIndex_External_Field_Determinant   _    -> 
                let extField = getExternaField r deps t.id
                let typeDefinitionName = defOrRef.longTypedefName l//getTypeDefinitionName t.id.tasInfo typeDefinition
                let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (o.uperCharSet.Length-1))
                let encDecStatement = 
                    match o.uperCharSet.Length = 128 with
                    | false -> 
                        let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
                        Acn_String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn) arrAsciiCodes (BigInteger o.uperCharSet.Length) extField td nBits codec 
                    | true  -> Acn_IA5String_CharIndex_External_Field_Determinant pp errCode.errCodeName ( o.maxSize.acn)  extField td nBits codec
                Some(encDecStatement, [errCode], [])
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVars) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVars; bValIsReferenced= false; bBsIsReferenced=false})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us


let createAcnStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (typeId : ReferenceToType) (t:Asn1AcnAst.AcnReferenceToIA5String)  (us:State)  =
    let errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errCode, ns = getNextValidErrorCode us errCodeName
    let Acn_String_Ascii_FixSize                            = match l with C -> acn_c.Acn_String_Ascii_FixSize                          | Ada -> acn_a.Acn_String_Ascii_FixSize
    let Acn_String_Ascii_Internal_Field_Determinant         = match l with C -> acn_c.Acn_String_Ascii_Internal_Field_Determinant       | Ada -> acn_a.Acn_String_Ascii_Internal_Field_Determinant
    let Acn_String_Ascii_Null_Teminated                     = match l with C -> acn_c.Acn_String_Ascii_Null_Teminated                   | Ada -> acn_a.Acn_String_Ascii_Null_Teminated
    let Acn_String_Ascii_External_Field_Determinant         = match l with C -> acn_c.Acn_String_Ascii_External_Field_Determinant       | Ada -> acn_a.Acn_String_Ascii_External_Field_Determinant
    let Acn_String_CharIndex_External_Field_Determinant     = match l with C -> acn_c.Acn_String_CharIndex_External_Field_Determinant   | Ada -> acn_a.Acn_String_CharIndex_External_Field_Determinant
    let Acn_IA5String_CharIndex_External_Field_Determinant  = match l with C -> acn_c.Acn_IA5String_CharIndex_External_Field_Determinant   | Ada -> acn_c.Acn_IA5String_CharIndex_External_Field_Determinant
    let typeDefinitionName = ToC2(r.args.TypePrefix + t.tasName.Value)
    let callerProgramUnit = ToC typeId.ModName
    
    //let td = o.
    let o = t.str
    let uper_funcBody (errCode:ErroCode) (p:CallerScope) = 
        let td =
            let md = r.GetModuleByName t.modName
            let tas = md.GetTypeAssignmentByName t.tasName r
            match tas.Type.ActualType.Kind with
            | Asn1AcnAst.IA5String     z -> z.typeDef.[l].longTypedefName l (ToC p.modName)
            | Asn1AcnAst.NumericString z -> z.typeDef.[l].longTypedefName l (ToC p.modName)
            | _                           -> raise(SemanticError(t.tasName.Location, (sprintf "Type assignment %s.%s does not point to a string type" t.modName.Value t.modName.Value)))
        let ii = typeId.SeqeuenceOfLevel + 1
        let i = sprintf "i%d" ii
        let lv = SequenceOfIndex (typeId.SeqeuenceOfLevel + 1, None)
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
        let InternalItem_string_no_alpha = match l with C -> uper_c.InternalItem_string_no_alpha        | Ada -> uper_a.InternalItem_string_no_alpha
        let InternalItem_string_with_alpha = match l with C -> uper_c.InternalItem_string_with_alpha        | Ada -> uper_a.InternalItem_string_with_alpha
        let str_FixedSize       = match l with C -> uper_c.str_FixedSize        | Ada -> uper_a.str_FixedSize
        let str_VarSize         = match l with C -> uper_c.str_VarSize          | Ada -> uper_a.str_VarSize
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
                let funcBodyContent,localVariables = DAstUPer.handleFragmentation l p codec errCode ii ( o.uperMaxSizeInBits) o.minSize.uper o.maxSize.uper internalItem nBits false true
                funcBodyContent,charIndex@localVariables

        {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = lv::localVariables}    


    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
        let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
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
        | Some (funcBodyContent,errCodes, lvs) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::errCodes |> List.distinct ; localVariables = lvs; bValIsReferenced= false; bBsIsReferenced=false})


    (funcBody errCode), ns


let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let oct_sqf_external_field                          = match l with C -> acn_c.oct_sqf_external_field            | Ada -> acn_a.oct_sqf_external_field
    let oct_sqf_external_field_fix_size                 = match l with C -> acn_c.oct_sqf_external_field_fix_size   | Ada -> acn_a.oct_sqf_external_field_fix_size
    let oct_sqf_null_terminated = match l with C -> acn_c.oct_sqf_null_terminated       | Ada -> acn_a.oct_sqf_null_terminated
    let InternalItem_oct_str                            = match l with C -> uper_c.InternalItem_oct_str        | Ada -> uper_a.InternalItem_oct_str
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
    let nAlignSize = 0I;

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        let funcBodyContent = 
            match o.acnEncodingClass with
            | SZ_EC_uPER                                              -> 
                let funcBody (errCode:ErroCode) (p:CallerScope) =
                    Some (DAstUPer.createOctetStringFunction_funcBody r l codec t.id  typeDefinition o.isFixedSize  o.uperMaxSizeInBits o.minSize.acn o.maxSize.acn (errCode:ErroCode) (p:CallerScope) )
            
                funcBody errCode p |> Option.map(fun x -> x.funcBody, x.errCodes, x.localVariables)
            | SZ_EC_ExternalField   _    -> 
                let extField = getExternaField r deps t.id
                let internalItem = InternalItem_oct_str p.arg.p (p.arg.getAcces l) i  errCode.errCodeName codec 
                let fncBody = 
                    match o.isFixedSize with
                    | true  -> oct_sqf_external_field_fix_size p.arg.p (p.arg.getAcces l) i internalItem (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName 8I 8I codec
                    | false -> oct_sqf_external_field p.arg.p (p.arg.getAcces l) i internalItem (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName 8I 8I codec
                Some(fncBody, [errCode],[lv])
            | SZ_EC_TerminationPattern bitPattern   ->
                let mod8 = bitPattern.Value.Length % 8
                let suffix = [1 .. mod8] |> Seq.map(fun _ -> "0") |> Seq.StrJoin ""
                let bitPatten8 = bitPattern.Value + suffix
                let byteArray = bitStringValueToByteArray bitPatten8.AsLoc
                let internalItem = InternalItem_oct_str p.arg.p (p.arg.getAcces l) i  errCode.errCodeName codec 
                let noSizeMin = if o.minSize.acn=0I then None else Some ( o.minSize.acn)
                let fncBody = oct_sqf_null_terminated p.arg.p (p.arg.getAcces l) i internalItem noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName  codec
                let lv2 = 
                    match codec, l with
                    | Decode, C    -> [IntegerLocalVariable ("checkBitPatternPresentResult", Some 0)]
                    | _            -> []
                Some(fncBody, [errCode],lv::lv2)
                


        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; bValIsReferenced= false; bBsIsReferenced=false})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us

let createBitStringFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefintionOrReference)  (isValidFunc: IsValidFunction option) (uperFunc: UPerFunction) (us:State)  =
    let nAlignSize = 0I;

    let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        //let pp = match codec with CommonTypes.Encode -> p.arg.getValue l | CommonTypes.Decode -> p.arg.getPointer l
        //let bit_string_null_terminated = match l with C -> acn_c.bit_string_null_terminated | Ada -> acn_a.bit_string_null_terminated
        let funcBodyContent = 
            match o.acnEncodingClass with
            | SZ_EC_uPER                                              -> 
                let funcBody  (errCode:ErroCode) (p:CallerScope) =
                    Some (DAstUPer.createBitStringFunction_funcBody r l codec t.id typeDefinition o.isFixedSize  o.uperMaxSizeInBits o.minSize.acn o.maxSize.acn (errCode:ErroCode) (p:CallerScope))
                funcBody errCode p |> Option.map(fun x -> x.funcBody, x.errCodes, x.localVariables)
            | SZ_EC_ExternalField   _    -> 
                let extField = getExternaField r deps t.id
                match l with
                | C     ->
                    let fncBody = 
                        match o.isFixedSize with
                        | true  -> acn_c.bit_string_external_field_fixed_size p.arg.p errCode.errCodeName (p.arg.getAcces l) (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField codec
                        | false  -> acn_c.bit_string_external_field p.arg.p errCode.errCodeName (p.arg.getAcces l) (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField codec
                    Some(fncBody, [errCode], [])
                | Ada   ->
                    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
                    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
                    let internalItem = uper_a.InternalItem_bit_str p.arg.p i  errCode.errCodeName codec 
                    let fncBody = 
                        match o.isFixedSize with
                        | true  -> acn_a.oct_sqf_external_field_fix_size p.arg.p (p.arg.getAcces l) i internalItem (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName 1I 1I codec
                        | false  -> acn_a.oct_sqf_external_field p.arg.p (p.arg.getAcces l) i internalItem (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName 1I 1I codec
                    Some(fncBody, [errCode], [lv])
            | SZ_EC_TerminationPattern   bitPattern    -> 
                let mod8 = bitPattern.Value.Length % 8
                let suffix = [1 .. mod8] |> Seq.map(fun _ -> "0") |> Seq.StrJoin ""
                let bitPatten8 = bitPattern.Value + suffix
                let byteArray = bitStringValueToByteArray bitPatten8.AsLoc
                match l with
                | C     ->
                    let fncBody = acn_c.bit_string_null_terminated p.arg.p errCode.errCodeName (p.arg.getAcces l) (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) byteArray bitPattern.Value.Length.AsBigInt codec
                    Some(fncBody, [errCode], [])
                | Ada   ->
                    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
                    let lv = SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)
                    let internalItem = uper_a.InternalItem_bit_str p.arg.p i  errCode.errCodeName codec 
                    let noSizeMin = if o.minSize.acn=0I then None else Some ( o.minSize.acn)
                    let fncBody = acn_a.oct_sqf_null_terminated p.arg.p (p.arg.getAcces l) i internalItem noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName  codec
                    Some(fncBody, [errCode], [lv])
                    
        match funcBodyContent with
        | None -> None
        | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; bValIsReferenced= false; bBsIsReferenced=false})
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us



let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (isValidFunc: IsValidFunction option)  (child:Asn1Type) (us:State)  =
    let oct_sqf_null_terminated = match l with C -> acn_c.oct_sqf_null_terminated       | Ada -> acn_a.oct_sqf_null_terminated
    let oct_sqf_external_field_fix_size                 = match l with C -> acn_c.oct_sqf_external_field_fix_size   | Ada -> acn_a.oct_sqf_external_field_fix_size
    let external_field          = match l with C -> acn_c.oct_sqf_external_field       | Ada -> acn_a.oct_sqf_external_field
    let fixedSize               = match l with C -> uper_c.octect_FixedSize            | Ada -> uper_a.octect_FixedSize
    let varSize                 = match l with C -> uper_c.octect_VarSize              | Ada -> uper_a.octect_VarSize
    
    let ii = t.id.SeqeuenceOfLevel + 1

    let i = sprintf "i%d" ii
    let lv = 
        match l with 
        | C           -> [SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)]
        | Ada   when o.acnEncodingClass = SZ_EC_uPER && o.maxSize.uper >= 65536I && o.maxSize.uper=o.minSize.uper   -> []      //fixed size fragmentation does not need the i variable
        | Ada         -> [SequenceOfIndex (t.id.SeqeuenceOfLevel + 1, None)]
    let nAlignSize = 0I;
    let typeDefinitionName = defOrRef.longTypedefName l 
    let nIntItemMaxSize = ( child.acnMaxSizeInBits)
    let funcBody (us:State) (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
        match child.getAcnFunction codec with
        | None         -> None, us
        | Some chFunc  ->
            let internalItem, ns = chFunc.funcBody us acnArgs ({p with arg = p.arg.getArrayItem l i child.isIA5String})
            let ret = 
                match o.acnEncodingClass with
                | SZ_EC_uPER                                              -> 
                    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                    let nStringLength =
                        match o.minSize.uper = o.maxSize.uper,  l, codec with
                        | true , _,_    -> []
                        | false, Ada, Encode -> []
                        | false, Ada, Decode -> [IntegerLocalVariable ("nStringLength", None)]
                        | false, C, Encode -> []
                        | false, C, Decode -> [Asn1SIntLocalVariable ("nCount", None)]

                    match internalItem with
                    | None  -> 
                            match o.minSize with
                            | _ when o.maxSize.acn < 65536I && o.isFixedSize  -> None
                            | _ when o.maxSize.acn < 65536I && o.isVariableSize -> 
                                let funcBody = varSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i "" ( o.minSize.acn) ( o.maxSize.acn) nSizeInBits ( child.acnMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec
                                Some ({AcnFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = lv@nStringLength; bValIsReferenced= false; bBsIsReferenced=false})    
                            | _                                                -> 
                                let funcBody, localVariables = DAstUPer.handleFragmentation l p codec errCode ii ( o.acnMaxSizeInBits) o.minSize.acn o.maxSize.acn "" nIntItemMaxSize false false
                                Some ({AcnFuncBodyResult.funcBody = funcBody; errCodes = [errCode]; localVariables = localVariables; bValIsReferenced= false; bBsIsReferenced=false})    

                    | Some internalItem -> 
                        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (o.maxSize.acn - o.minSize.acn))
                        let childErrCodes =  internalItem.errCodes
                        let ret, localVariables = 
                            match o.minSize with
                            | _ when o.maxSize.acn < 65536I && o.isFixedSize  -> fixedSize p.arg.p typeDefinitionName i internalItem.funcBody ( o.minSize.acn) ( child.acnMinSizeInBits) nIntItemMaxSize 0I codec , nStringLength 
                            | _ when o.maxSize.acn < 65536I && o.isVariableSize  -> varSize p.arg.p (p.arg.getAcces l)  typeDefinitionName i internalItem.funcBody ( o.minSize.acn) ( o.maxSize.acn) nSizeInBits ( child.acnMinSizeInBits) nIntItemMaxSize 0I errCode.errCodeName codec , nStringLength 
                            | _                                                -> 
                                DAstUPer.handleFragmentation l p codec errCode ii ( o.acnMaxSizeInBits) o.minSize.acn o.maxSize.acn internalItem.funcBody nIntItemMaxSize false false
                        Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childErrCodes; localVariables = lv@(internalItem.localVariables@localVariables); bValIsReferenced= false; bBsIsReferenced=false})    

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
                            | true  -> oct_sqf_external_field_fix_size p.arg.p (p.arg.getAcces l) i internalItem (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits  codec
                            | false -> external_field p.arg.p (p.arg.getAcces l) i internalItem (if o.minSize.acn=0I then None else Some ( o.minSize.acn)) ( o.maxSize.acn) extField nAlignSize errCode.errCodeName o.child.acnMinSizeInBits o.child.acnMaxSizeInBits  codec
                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = lv@localVariables; bValIsReferenced= false; bBsIsReferenced=false})
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
                        let funcBodyContent = oct_sqf_null_terminated p.arg.p (p.arg.getAcces l) i internalItem noSizeMin o.maxSize.acn byteArray bitPattern.Value.Length.AsBigInt errCode.errCodeName  codec
                        let lv2 = 
                            match codec, l with
                            | Decode, C    -> [IntegerLocalVariable ("checkBitPatternPresentResult", Some 0)]
                            | _            -> []
                        Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCode::childErrCodes; localVariables = lv2@lv@localVariables; bValIsReferenced= false; bBsIsReferenced=false})
            ret,ns
    let soSparkAnnotations = None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations us


let rec handleSingleUpdateDependency (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (m:Asn1AcnAst.Asn1Module) (d:AcnDependency)  (us:State) =
    let presenceDependency              = match l with C -> acn_c.PresenceDependency                | Ada -> acn_a.PresenceDependency          
    let sizeDependency                  = match l with C -> acn_c.SizeDependency                    | Ada -> acn_a.SizeDependency      
    let sizeDependencyFixedSize         = match l with C -> acn_c.SizeDependencyFixedSize           | Ada -> acn_a.SizeDependencyFixedSize      
    let sizeDep_oct_str_containing      = match l with C -> acn_c.SizeDependency_oct_str_containing | Ada -> acn_a.SizeDependency_oct_str_containing
    let getSizeableSize                 = match l with C -> uper_c.getSizeableSize                  | Ada -> acn_a.getSizeableSize          
    let getStringSize                   = match l with C -> uper_c.getStringSize                    | Ada -> acn_a.getStringSize          
    let choiceDependencyPres            = match l with C -> acn_c.ChoiceDependencyPres              | Ada -> acn_a.ChoiceDependencyPres
    let choiceDependencyIntPres_child   = match l with C -> acn_c.ChoiceDependencyIntPres_child     | Ada -> acn_a.ChoiceDependencyIntPres_child
    let choiceDependencyStrPres_child   = match l with C -> acn_c.ChoiceDependencyStrPres_child     | Ada -> acn_a.ChoiceDependencyStrPres_child
    let choiceDependencyEnum            = match l with C -> acn_c.ChoiceDependencyEnum              | Ada -> acn_a.ChoiceDependencyEnum
    let choiceDependencyEnum_Item       = match l with C -> acn_c.ChoiceDependencyEnum_Item         | Ada -> acn_a.ChoiceDependencyEnum_Item
    let checkAccessPath                 = match l with C -> acn_c.checkAccessPath                   | Ada -> acn_a.checkAccessPath
    

    match d.dependencyKind with
    | AcnDepRefTypeArgument           acnPrm   -> 
        let prmUpdateStatement, ns1 = getUpdateFunctionUsedInEncoding r deps l m acnPrm.id us
        match prmUpdateStatement with
        | None  -> None, ns1
        | Some prmUpdateStatement   -> 
            let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
                prmUpdateStatement.updateAcnChildFnc typedefName vTarget pSrcRoot
            
            Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=prmUpdateStatement.errCodes; testCaseFnc = prmUpdateStatement.testCaseFnc; localVariables=[]}), ns1
    | AcnDepSizeDeterminant (minSize, maxSize, szAcnProp)        -> 
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let pSizeable, checkPath = getAccessFromScopeNodeList d.asn1Type false l pSrcRoot
            let updateStatement = 
                match minSize.acn = maxSize.acn with
                | true  -> sizeDependencyFixedSize (vTarget.arg.getValue l) minSize.acn
                | false -> sizeDependency (vTarget.arg.getValue l) (getSizeableSize pSizeable.arg.p (pSizeable.arg.getAcces l))
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type

        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepSizeDeterminant_bit_oct_str_containt  o       -> 
        let baseTypeDefinitionName = 
            match l with
            | C     -> ToC2(r.args.TypePrefix + o.tasName.Value) 
            | Ada   -> 
                match m.Name.Value = o.modName.Value with
                | true  -> ToC2(r.args.TypePrefix + o.tasName.Value) 
                | false -> (ToC o.modName.Value) + "." + ToC2(r.args.TypePrefix + o.tasName.Value) 
        let baseFncName = baseTypeDefinitionName + "_ACN" + Encode.suffix
        let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ACN_ENCODING" baseTypeDefinitionName
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let pSizeable, checkPath = getAccessFromScopeNodeList d.asn1Type false l pSrcRoot
            
            let updateStatement = sizeDep_oct_str_containing (o.resolvedType.getParamValue pSizeable.arg l Encode) baseFncName sReqBytesForUperEncoding (vTarget.arg.getValue l) (match o.encodingOptions with Some eo -> eo.octOrBitStr = ContainedInOctString | None -> false)
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        let localVars = 
            match l with 
            | C     -> 
                [
                    GenericLocalVariable {GenericLocalVariable.name = "arr"; varType = "byte"; arrSize = Some sReqBytesForUperEncoding; isStatic = true; initExp = None}
                    GenericLocalVariable {GenericLocalVariable.name = "bitStrm"; varType = "BitStream"; arrSize = None; isStatic = false; initExp = None}
                ]
            | Ada   -> 
                [
                    GenericLocalVariable {GenericLocalVariable.name = "tmpBs"; varType = "adaasn1rtl.encoding.BitStream"; arrSize = None; isStatic = false;initExp = Some (sprintf "adaasn1rtl.encoding.BitStream_init(%s)" sReqBytesForUperEncoding)}
                ]

        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[]; testCaseFnc=testCaseFnc; localVariables= localVars}), us
    | AcnDepIA5StringSizeDeterminant    ->
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let pSizeable, checkPath = getAccessFromScopeNodeList d.asn1Type true l pSrcRoot
            let updateStatement = sizeDependency (vTarget.arg.getValue l) (getStringSize pSizeable.arg.p)
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[]; testCaseFnc=testCaseFnc; localVariables=[]}), us
    | AcnDepPresenceBool              -> 
        let updateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let v = vTarget.arg.getValue l
            let parDecTypeSeq =
                match d.asn1Type with
                | ReferenceToType (nodes) -> ReferenceToType (nodes |> List.rev |> List.tail |> List.rev)
            let pDecParSeq, checkPath = getAccessFromScopeNodeList parDecTypeSeq false l pSrcRoot
            let updateStatement = presenceDependency v (pDecParSeq.arg.p) (pDecParSeq.arg.getAcces l) (ToC d.asn1Type.lastItem)
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
            let v = vTarget.arg.getValue l
            let choicePath, checkPath = getAccessFromScopeNodeList d.asn1Type false l pSrcRoot
            let arrsChildUpdates = 
                chc.children |> 
                List.map(fun ch -> 
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    match pres with
                    | PresenceInt   (_, intVal) -> choiceDependencyIntPres_child v ch.presentWhenName intVal.Value
                    | PresenceStr   (_, strVal) -> raise(SemanticError(strVal.Location, "Unexpected presence condition. Expected integer, found string")))
            let updateStatement = choiceDependencyPres choicePath.arg.p (choicePath.arg.getAcces l) arrsChildUpdates
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
            let v = vTarget.arg.getValue l
            let choicePath, checkPath = getAccessFromScopeNodeList d.asn1Type false l pSrcRoot
            let arrsChildUpdates = 
                chc.children |> 
                List.map(fun ch -> 
                    let pres = ch.acnPresentWhenConditions |> Seq.find(fun x -> x.relativePath = relPath)
                    match pres with
                    | PresenceInt   (_, intVal) -> 
                        raise(SemanticError(intVal.Location, "Unexpected presence condition. Expected string, found integer"))
                        //choiceDependencyIntPres_child v ch.presentWhenName intVal.Value
                    | PresenceStr   (_, strVal) -> 
                        let arrNuls = [0 .. ((int str.maxSize.acn)- strVal.Value.Length)]|>Seq.map(fun x -> variables_a.PrintStringValueNull())
                        choiceDependencyStrPres_child v ch.presentWhenName strVal.Value arrNuls)
            let updateStatement = choiceDependencyPres choicePath.arg.p (choicePath.arg.getAcces l) arrsChildUpdates
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
            let v = vTarget.arg.getValue l
            let choicePath, checkPath = getAccessFromScopeNodeList d.asn1Type false l pSrcRoot
            let defOrRef (a:Asn1AcnAst.ReferenceToEnumerated) =
                match m.Name.Value = a.modName with
                | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}
                | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName); typedefName = ToC (r.args.TypePrefix + a.tasName) ; definedInRtl = false}
            
            let arrsChildUpdates = 
                chc.children |> 
                List.map(fun ch -> 
                    let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = ch.Name.Value)
                    choiceDependencyEnum_Item v ch.presentWhenName (enmItem.getBackendName (Some (defOrRef enm))  l) )
            let updateStatement = choiceDependencyEnum choicePath.arg.p (choicePath.arg.getAcces l) arrsChildUpdates
            match checkPath with
            | []    -> updateStatement
            | _     -> checkAccessPath checkPath updateStatement
        let testCaseFnc (atc:AutomaticTestCase) : TestCaseValue option =
            atc.testCaseTypeIDsMap.TryFind d.asn1Type
        Some ({AcnChildUpdateResult.updateAcnChildFnc = updateFunc; errCodes=[] ; testCaseFnc=testCaseFnc; localVariables=[]}), us

and getUpdateFunctionUsedInEncoding (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (m:Asn1AcnAst.Asn1Module) (acnChildOrAcnParameterId) (us:State) : (AcnChildUpdateResult option*State)=
    let multiAcnUpdate       = match l with C -> acn_c.MultiAcnUpdate          | Ada -> acn_a.MultiAcnUpdate

    match deps.acnDependencies |> List.filter(fun d -> d.determinant.id = acnChildOrAcnParameterId) with
    | []  -> 
        None, us
    | d1::[]    -> 
        let ret, ns = handleSingleUpdateDependency r deps l m d1 us
        ret, ns
    | d1::dds         -> 
        let _errCodeName         = ToC ("ERR_ACN" + (Encode.suffix.ToUpper()) + "_UPDATE_" + ((acnChildOrAcnParameterId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
        let errCode, us = getNextValidErrorCode us _errCodeName


        let ds = d1::dds
        let c_name0 = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) 0
        let localVars (typedefName :string) = 
            ds |> 
            List.mapi(fun i d1 -> 
                let c_name = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                let typegetDeterminantTypeDefinitionBodyWithinSeq = typedefName // getDeterminantTypeDefinitionBodyWithinSeq r l d1.determinant
                [AcnInsertedChild (c_name, typegetDeterminantTypeDefinitionBodyWithinSeq); BooleanLocalVariable (c_name+"_is_initialized", Some false)]) |>
            List.collect(fun lvList -> lvList |> List.map (fun lv -> lv.GetDeclaration l))
        let localUpdateFuns,ns =
            ds |>
            List.fold(fun (updates, ns) d1 -> 
                let f1, nns = handleSingleUpdateDependency r deps l m d1 ns 
                updates@[f1], nns) ([],us)
        let restErrCodes = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.errCodes)
        let restLocalVariables = localUpdateFuns |> List.choose id |> List.collect(fun z -> z.localVariables)
        let multiUpdateFunc (typedefName :string) (vTarget : CallerScope) (pSrcRoot : CallerScope)  = 
            let v = vTarget.arg.getValue l
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
                    let cmp = getDeterminantTypeUpdateMacro r l d.determinant
                    let vi = sprintf "%s%02d" (getAcnDeterminantName acnChildOrAcnParameterId) i
                    cmp v vi (i=0) )
            let arrsLocalCheckEquality = 
                ds |> 
                List.mapi (fun i d -> 
                    let cmp = getDeterminantTypeCheckEqual r l d.determinant
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

let createSequenceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (acnPrms:DastAcnParameter list) (us:State)  =
    (*
        1. all Acn inserted children are declared as local variables in the encoded and decode functions (declaration step)
        2. all Acn inserted children must be initialized appropriatelly in the encoding phase
        3. 
    *)

    // stg macros
    let sequence_presense_optChild              = match l with C -> acn_c.sequence_presense_optChild             | Ada -> acn_a.sequence_presense_optChild          
    let sequence_presense_optChild_pres_bool    = match l with C -> acn_c.sequence_presense_optChild_pres_bool   | Ada -> acn_a.sequence_presense_optChild_pres_bool
    let sequence_presense_optChild_pres_acn_expression  = match l with C -> acn_c.sequence_presense_optChild_pres_acn_expression   | Ada -> acn_a.sequence_presense_optChild_pres_acn_expression
    let sequence_presense_optChild_pres_int     = match l with C -> acn_c.sequence_presense_optChild_pres_int    | Ada -> acn_a.sequence_presense_optChild_pres_int 
    let sequence_presense_optChild_pres_str     = match l with C -> acn_c.sequence_presense_optChild_pres_str    | Ada -> acn_a.sequence_presense_optChild_pres_str 
    let sequence_mandatory_child                = match l with C -> acn_c.sequence_mandatory_child               | Ada -> acn_a.sequence_mandatory_child            
    let sequence_optional_child                 = match l with C -> acn_c.sequence_optional_child                | Ada -> acn_a.sequence_optional_child             
    let sequence_always_present_child           = match l with C -> acn_c.sequence_always_present_child          | Ada -> acn_a.sequence_always_present_child             
    let sequence_always_absent_child           = match l with C -> acn_c.sequence_always_absent_child          | Ada -> acn_a.sequence_always_absent_child             
    let sequence_optional_always_present        = match l with C -> acn_c.sequence_optional_always_present_child | Ada -> acn_a.sequence_optional_always_present_child
    let sequence_default_child                  = match l with C -> acn_c.sequence_default_child                 | Ada -> acn_a.sequence_default_child              
    let sequence_acn_child                      = match l with C -> acn_c.sequence_acn_child                     | Ada -> acn_a.sequence_acn_child              
    let sequence_call_post_encoding_function    = match l with C -> acn_c.sequence_call_post_encoding_function                     | Ada -> acn_a.sequence_call_post_encoding_function              
    let sequence_call_post_decoding_validator   = match l with C -> acn_c.sequence_call_post_decoding_validator                     | Ada -> acn_a.sequence_call_post_decoding_validator              
    let sequence_save_bitstream                 = match l with C -> acn_c.sequence_save_bitstream                     | Ada -> acn_a.sequence_save_bitstream              
    let sequence_save_bitStream_start                 = match l with C -> acn_c.sequence_save_bitStream_start                     | Ada -> acn_a.sequence_save_bitStream_start              
    
    let bitStreamName                           = match l with C -> "BitStream"                                  | Ada -> "adaasn1rtl.encoding.BitStreamPtr"

    let acnExpressionToBackendExpression (seq:Asn1AcnAst.Sequence) (pSeq:CallerScope) (exp:AcnExpression) =
        let unaryNotOperator = match l with C -> "!" | Ada -> "not"
        let modOp = match l with C -> "%" | Ada -> "mod"
        let eqOp = match l with C -> "==" | Ada -> "="
        let neqOp = match l with C -> "!=" | Ada -> "<>"
        let andOp = match l with C -> "&&" | Ada -> "and"
        let orOp = match l with C -> "||" | Ada -> "or"

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
                        | Asn1AcnAst.Boolean        _  -> {pSeq with arg = pSeq.arg.getSeqChild l (ch.getBackendName l) false} 
                        | Asn1AcnAst.Sequence s when xs.Length > 1 -> getChildResult s {pSeq with arg = pSeq.arg.getSeqChild l (ch.getBackendName l) false} (RelativePath xs)
                        | _                 -> raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin "."))))


        let ret =
            AcnGenericTypes.foldAcnExpression
                (fun i s -> ( (0, i.Value.ToString()) , 0))
                (fun i s -> ( (0,"") , 0))
                (fun i s -> ( (0, i.Value.ToString("E20", NumberFormatInfo.InvariantInfo)) , 0))
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
                | None      -> Some (sequence_presense_optChild p.arg.p (p.arg.getAcces l) (child.getBackendName l)  errCode.errCodeName codec)
                | Some _    -> None

        let localVariables =
            match asn1Children |> List.choose  printPresenceBit with
            | x::_  when l = C  && codec = CommonTypes.Decode -> (FlagLocalVariable ("presenceBit", None))::acnlocalVariables
            | _        -> acnlocalVariables
        
        let td = o.typeDef.[l]


        let localVariables, post_encoding_function, soBitStreamPositionsLocalVar, soSaveInitialBitStrmStatement =
            let bitStreamPositionsLocalVar = sprintf "bitStreamPositions_%d" (t.id.SeqeuenceOfLevel + 1)
            let bsPosStart = sprintf "bitStreamPositions_start%d" (t.id.SeqeuenceOfLevel + 1)
            match o.acnProperties.postEncodingFunction with
            | Some (PostEncodingFunction fncName) when codec = Encode  ->
                let actualFncName = match l with C -> (ToC fncName.Value) | Ada -> (ToC (r.args.mappingFunctionsModule.orElse "")) + "." + (ToC fncName.Value)
                let fncCall = sequence_call_post_encoding_function (p.arg.getPointer l) (actualFncName) bsPosStart  bitStreamPositionsLocalVar
                let initialBitStrmStatement = sequence_save_bitStream_start bsPosStart codec
                [AcnInsertedChild(bitStreamPositionsLocalVar, td.extention_function_potisions); AcnInsertedChild(bsPosStart, bitStreamName)]@localVariables, Some fncCall, Some bitStreamPositionsLocalVar, Some initialBitStrmStatement
            | _ ->
                match o.acnProperties.preDecodingFunction with
                | Some (PreDecodingFunction fncName) when codec = Decode  ->
                    let actualFncName = match l with C -> (ToC fncName.Value) | Ada -> (ToC (r.args.mappingFunctionsModule.orElse "")) + "." + (ToC fncName.Value)
                    let fncCall = sequence_call_post_decoding_validator (p.arg.getPointer l) (actualFncName) bsPosStart  bitStreamPositionsLocalVar
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
                    | Some chFunc   -> chFunc.funcBodyAsSeqComp us [] ({p with arg = p.arg.getSeqChild l (child.getBackendName l) child.Type.isIA5String}) (child.getBackendName l)
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
                                        Some(sequence_presense_optChild_pres_bool p.arg.p (p.arg.getAcces l) (child.getBackendName l) extField codec), [], ns1
                                | Some (PresenceWhenBoolExpression exp)    -> 
                                    let _errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_PRESENT_WHEN_EXP_FAILED")
                                    let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName
                                    let retExp = acnExpressionToBackendExpression o p exp
                                    Some(sequence_presense_optChild_pres_acn_expression p.arg.p (p.arg.getAcces l) (child.getBackendName l) retExp errCode.errCodeName codec), [errCode], ns1a
                            | _                 -> None, [], ns1
                        [(AcnPresenceStatement, acnPresenceStatement, [], errCodes)], ns1b

                let childEncDecStatement, ns3 = 
                    match childContentResult with
                    | None              -> [], ns2
                    | Some childContent ->
                        let childBody, chLocalVars = 
                            match child.Optionality with
                            | None                             -> Some (sequence_mandatory_child (child.getBackendName l) childContent.funcBody soSaveBitStrmPosStatement codec), childContent.localVariables
                            | Some Asn1AcnAst.AlwaysAbsent     -> Some (sequence_always_absent_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody soSaveBitStrmPosStatement codec), []
                            | Some Asn1AcnAst.AlwaysPresent    -> Some(sequence_always_present_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody soSaveBitStrmPosStatement codec), childContent.localVariables
                            | Some (Asn1AcnAst.Optional opt)   -> 
                                match opt.defaultValue with
                                | None                   -> Some(sequence_optional_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody soSaveBitStrmPosStatement codec), childContent.localVariables
                                | Some v                 -> 
                                    let defInit= child.Type.initFunction.initByAsn1Value ({p with arg = p.arg.getSeqChild l (child.getBackendName l) child.Type.isIA5String}) (mapValue v).kind
                                    Some(sequence_default_child p.arg.p (p.arg.getAcces l) (child.getBackendName l) childContent.funcBody defInit soSaveBitStrmPosStatement codec), childContent.localVariables
                        [(Asn1ChildEncodeStatement, childBody, chLocalVars, childContent.errCodes)], ns2
                present_when_statements@childEncDecStatement,ns3
            | AcnChild  acnChild    -> 
                //handle updates
                //acnChild.c_name
                let childP = {CallerScope.modName = p.modName; arg= VALUE (getAcnDeterminantName acnChild.id)}

                let updtateStatement, ns1 = 
                    match codec with
                    | CommonTypes.Encode -> 
                        let pRoot : CallerScope = t.getParamType l codec  //????
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
                                let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName
                        
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
                let pRoot : CallerScope = t.getParamType l codec  
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
        
        
        let seqContent =  (saveInitialBitStrmStatements@presenseBits@childrenStatements@(post_encoding_function |> Option.toList)) |> nestChildItems l codec 
        match existsAcnChildWithNoUpdates with
        | []     ->
            match seqContent with
            | None  -> None, ns
            | Some ret -> Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsReferenced= false; bBsIsReferenced=false}), ns    
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
    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody isTestVaseValid soSparkAnnotations  us



let createChoiceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (defOrRef:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (children:ChChildInfo list) (acnPrms:DastAcnParameter list)  (us:State)  =
    let choice_uper          =  match l with C -> acn_c.Choice                | Ada -> acn_a.Choice  
    let choiceChildAlwaysAbsent          =  match l with C -> acn_c.ChoiceChildAlwaysAbsent                | Ada -> acn_a.ChoiceChildAlwaysAbsent
    let choiceChild          =  match l with C -> acn_c.ChoiceChild           | Ada -> acn_a.ChoiceChild
    let choice_Enum          =  match l with C -> acn_c.Choice_Enum           | Ada -> acn_a.Choice_Enum
    let choiceChild_Enum     =  match l with C -> acn_c.ChoiceChild_Enum      | Ada -> acn_a.ChoiceChild_Enum
    let choice_preWhen       =  match l with C -> acn_c.Choice_preWhen        | Ada -> acn_a.Choice_preWhen
    let choiceChild_preWhen  =  match l with C -> acn_c.ChoiceChild_preWhen   | Ada -> acn_a.ChoiceChild_preWhen
    let choiceChild_preWhen_int_condition  =  match l with C -> acn_c.ChoiceChild_preWhen_int_condition   | Ada -> acn_a.ChoiceChild_preWhen_int_condition
    let choiceChild_preWhen_str_condition  =  match l with C -> acn_c.ChoiceChild_preWhen_str_condition   | Ada -> acn_a.ChoiceChild_preWhen_str_condition
    let always_false_statement  = match l with C -> isvalid_c.always_false_statement | Ada -> isvalid_a.always_false_statement

    let isAcnChild (ch:ChChildInfo) = match ch.Optionality with  Some (ChoiceAlwaysAbsent) -> false | _ -> true
    let acnChildren = children |> List.filter isAcnChild
    let alwaysAbsentChildren = children |> List.filter (isAcnChild >> not)
    let children = 
        match l with
        | C     -> acnChildren
        | Ada   -> acnChildren@alwaysAbsentChildren     //in Ada, we have to cover all cases even the ones that are always absent.


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

    let typeDefinitionName = defOrRef.longTypedefName l//getTypeDefinitionName t.id.tasInfo typeDefinition

    let funcBody (us:State) (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope) = 
        let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
        let handleChild (us:State) (idx:int) (child:ChChildInfo) =
                let chFunc = child.chType.getAcnFunction codec
                let childContentResult, ns1 = 
                    //match child.Optionality with
                    //| Some (ChoiceAlwaysAbsent) -> None//Some (always_false_statement errCode.errCodeName)
                    //| Some (ChoiceAlwaysPresent)
                    //| None  ->
                        match chFunc with
                        | Some chFunc   -> 
                            match l with
                            | C   ->  chFunc.funcBody us [] ({p with arg = p.arg.getChChild l (child.getBackendName l) child.chType.isIA5String})
                            | Ada when codec = CommonTypes.Decode  ->  chFunc.funcBody us [] ({CallerScope.modName = p.modName; arg = VALUE ((child.getBackendName l) + "_tmp")})
                            | Ada   ->  chFunc.funcBody us [] ({p with arg = p.arg.getChChild l (child.getBackendName l) child.chType.isIA5String})
                        | None          -> None, us

                let childContent_funcBody, childContent_localVariables, childContent_errCodes =
                    match childContentResult with
                    | None              -> 
                        match l with 
                        | C -> "",[],[] 
                        | Ada when codec = CommonTypes.Decode -> 
                            let childp = ({CallerScope.modName = p.modName; arg = VALUE ((child.getBackendName l) + "_tmp")})
                            let ret = uper_a.null_decode childp.arg.p
                            ret ,[],[]
                        | Ada  -> "null;",[],[]
                    | Some childContent -> childContent.funcBody,  childContent.localVariables, childContent.errCodes

//                match childContentResult with
//                | None              -> [], ns1
//                | Some childContent ->
                let childBody = 
                    let sChildName = (child.getBackendName l)
                    let sChildTypeDef = child.chType.typeDefintionOrReference.longTypedefName l //child.chType.typeDefinition.typeDefinitionBodyWithinSeq

                    let sChoiceTypeName = typeDefinitionName
                    match child.Optionality with
                    | Some (ChoiceAlwaysAbsent) -> Some (choiceChildAlwaysAbsent p.arg.p (p.arg.getAcces l) (child.presentWhenName (Some defOrRef) l) (BigInteger idx) errCode.errCodeName codec)
                    | Some (ChoiceAlwaysPresent)
                    | None  ->
                        match ec with
                        | CEC_uper  -> 
                            Some (choiceChild p.arg.p (p.arg.getAcces l) (child.presentWhenName (Some defOrRef) l) (BigInteger idx) nIndexSizeInBits nMax childContent_funcBody sChildName sChildTypeDef sChoiceTypeName codec)
                        | CEC_enum (enm,_) -> 
                            let getDefOrRef (a:Asn1AcnAst.ReferenceToEnumerated) =
                                match p.modName = a.modName with
                                | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}
                                | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName); typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}


                            let enmItem = enm.enm.items |> List.find(fun itm -> itm.Name.Value = child.Name.Value)
                            Some (choiceChild_Enum p.arg.p (p.arg.getAcces l) (enmItem.getBackendName (Some (getDefOrRef enm)) l) (child.presentWhenName (Some defOrRef) l) childContent_funcBody sChildName sChildTypeDef sChoiceTypeName codec)
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
                                    let arrNuls = [0 .. ((int strType.maxSize.acn) - strVal.Value.Length)]|>Seq.map(fun x -> variables_a.PrintStringValueNull())
                                    choiceChild_preWhen_str_condition extField strVal.Value arrNuls
                            let conds = child.acnPresentWhenConditions |>List.map handPresenseCond
                            Some (choiceChild_preWhen p.arg.p (p.arg.getAcces l) (child.presentWhenName (Some defOrRef) l) childContent_funcBody conds (idx=0) sChildName sChildTypeDef sChoiceTypeName codec)
                [(childBody, childContent_localVariables, childContent_errCodes)], ns1
        let childrenStatements00, ns = children |> List.mapi (fun i x -> i,x)  |> foldMap (fun us (i,x) ->  handleChild us i x) us
        let childrenStatements0 = childrenStatements00 |> List.collect id
        let childrenStatements = childrenStatements0 |> List.choose(fun (s,_,_) -> s)
        let childrenLocalvars = childrenStatements0 |> List.collect(fun (_,s,_) -> s)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun (_,_,s) -> s)

        let choiceContent =  
            match ec with
            | CEC_uper        -> 
                //let ret = choice p.arg.p (p.arg.getAcces l) childrenContent (BigInteger (children.Length - 1)) sChoiceIndexName errCode.errCodeName typeDefinitionName nBits  codec
                choice_uper p.arg.p (p.arg.getAcces l) childrenStatements nMax sChoiceIndexName td nIndexSizeInBits errCode.errCodeName codec
            | CEC_enum   enm  -> 
                let extField = getExternaField r deps t.id
                choice_Enum p.arg.p (p.arg.getAcces l) childrenStatements extField errCode.errCodeName codec
            | CEC_presWhen    -> choice_preWhen p.arg.p  (p.arg.getAcces l) childrenStatements errCode.errCodeName codec
        Some ({AcnFuncBodyResult.funcBody = choiceContent; errCodes = errCode::childrenErrCodes; localVariables = localVariables@childrenLocalvars; bValIsReferenced= false; bBsIsReferenced=false}), ns    


    let soSparkAnnotations = 
        match l with
        | C     -> None
        | Ada   -> None
    createAcnFunction r l codec t typeDefinition  isValidFunc  funcBody (fun atc -> true) soSparkAnnotations us, ec

let createReferenceFunction (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (l:ProgrammingLanguage) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (isValidFunc: IsValidFunction option) (baseType:Asn1Type) (us:State)  =
  match o.encodingOptions with 
  | None          -> 
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
    | Some( encOptions) ->
        //contained type i.e. MyOct ::= OCTET STRING (CONTAINING Other-Type)
        let loc = o.tasName.Location
        let baseTypeDefinitionName = 
            match l with
            | C     -> ToC2(r.args.TypePrefix + o.tasName.Value) 
            | Ada   -> 
                match t.id.ModName = o.modName.Value with
                | true  -> ToC2(r.args.TypePrefix + o.tasName.Value) 
                | false -> (ToC o.modName.Value) + "." + ToC2(r.args.TypePrefix + o.tasName.Value) 
        let baseFncName = baseTypeDefinitionName + "_ACN" + codec.suffix
        let sReqBytesForUperEncoding = sprintf "%s_REQUIRED_BYTES_FOR_ACN_ENCODING" baseTypeDefinitionName
        let sReqBitForUperEncoding = sprintf "%s_REQUIRED_BITS_FOR_ACN_ENCODING" baseTypeDefinitionName
        let octet_string_containing_func  = match l with C -> uper_c.octet_string_containing_func | Ada -> uper_a.octet_string_containing_func
        let bit_string_containing_func  = match l with C -> uper_c.bit_string_containing_func | Ada -> uper_a.bit_string_containing_func
        let octet_string_containing_ext_field_func = match l with C -> acn_c.octet_string_containing_ext_field_func | Ada -> acn_a.octet_string_containing_ext_field_func
        let bit_string_containing_ext_field_func = match l with C -> acn_c.bit_string_containing_ext_field_func | Ada -> acn_a.bit_string_containing_ext_field_func

        let funcBody (errCode:ErroCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (p:CallerScope)        = 
            let funcBodyContent = 

                match encOptions.acnEncodingClass, encOptions.octOrBitStr with
                | SZ_EC_ExternalField    relPath    , ContainedInOctString  ->  
                    let extField        = getExternaField r deps t.id
                    let fncBody = octet_string_containing_ext_field_func (t.getParamValue p.arg l codec)  baseFncName sReqBytesForUperEncoding extField errCode.errCodeName codec
                    Some(fncBody, [errCode],[])
                | SZ_EC_ExternalField    relPath    , ContainedInBitString  ->  
                    let extField        = getExternaField r deps t.id
                    let fncBody = bit_string_containing_ext_field_func (t.getParamValue p.arg l codec)  baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding extField errCode.errCodeName codec
                    Some(fncBody, [errCode],[])

                | SZ_EC_uPER                        , ContainedInOctString  ->  
                    let nBits = GetNumberOfBitsForNonNegativeInteger (encOptions.maxSize.acn - encOptions.minSize.acn)
                    let fncBody = octet_string_containing_func  (t.getParamValue p.arg l codec) baseFncName sReqBytesForUperEncoding nBits encOptions.minSize.acn encOptions.maxSize.acn codec
                    Some(fncBody, [errCode],[])
                | SZ_EC_uPER                        , ContainedInBitString  ->  
                    let nBits = GetNumberOfBitsForNonNegativeInteger (encOptions.maxSize.acn - encOptions.minSize.acn)
                    let fncBody = bit_string_containing_func  (t.getParamValue p.arg l codec) baseFncName sReqBytesForUperEncoding sReqBitForUperEncoding nBits encOptions.minSize.acn encOptions.maxSize.acn codec
                    Some(fncBody, [errCode],[])
                | SZ_EC_TerminationPattern nullVal  ,  _                    ->  raise(SemanticError (loc, "Invalid type for parameter4"))

            match funcBodyContent with
            | None -> None
            | Some (funcBodyContent,errCodes, localVariables) -> Some ({AcnFuncBodyResult.funcBody = funcBodyContent; errCodes = errCodes; localVariables = localVariables; bValIsReferenced= false; bBsIsReferenced=false})
        let soSparkAnnotations = None
        let a,b = createAcnFunction r l codec t typeDefinition  isValidFunc  (fun us e acnArgs p -> funcBody e acnArgs p, us) (fun atc -> true) soSparkAnnotations us
        Some a, b














      
        
