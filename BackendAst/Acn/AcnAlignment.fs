module AcnAlignment

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language


type FuncBody = State -> ErrorCode -> (AcnGenericTypes.RelativePath * AcnGenericTypes.AcnParameter) list -> NestingScope -> CodegenScope -> (AcnFuncBodyResult option) * State
type FuncBodyStateless = Codec -> (AcnGenericTypes.RelativePath * AcnGenericTypes.AcnParameter) list -> NestingScope -> CodegenScope -> string -> AcnFuncBodyResult option

let handleSavePosition (funcBody: FuncBody)
                       (savePosition: bool)
                       (c_name: string)
                       (lvName: string) (* Bitsream position local variable name *)
                       (typeId:ReferenceToType)
                       (lm:LanguageMacros)
                       (codec:CommonTypes.Codec): FuncBody =
    match savePosition with
    | false -> funcBody
    | true  ->
        let newFuncBody st errCode prms nestingScope (p:CodegenScope) =
            let content, ns1a = funcBody st errCode prms nestingScope p
            let sequence_save_bitstream                 = lm.acn.sequence_save_bitstream
            let savePositionStatement = sequence_save_bitstream lvName c_name codec
            let newContent =
                match content with
                | Some bodyResult   ->
                    let funcBodyStr = sprintf "%s\n%s" savePositionStatement bodyResult.funcBody
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = savePositionStatement
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= true; bBsIsUnReferenced=false; resultExpr = None; auxiliaries = []; icdResult = None }
            newContent, ns1a
        newFuncBody

let handleAlignmentForAsn1Types (r:Asn1AcnAst.AstRoot)
                                (lm:LanguageMacros)
                                (codec:CommonTypes.Codec)
                                (acnAlignment: AcnAlignment option)
                                (funcBody: FuncBody): FuncBody  =
    let alignToNext =  lm.acn.alignToNext
    match acnAlignment with
    | None      -> funcBody
    | Some al   ->
        let alStr, nAlignmentVal =
            match al with
            | AcnGenericTypes.NextByte ->
                match ProgrammingLanguage.ActiveLanguages.Head with
                | Scala -> "Byte", 8I
                | _ -> "NextByte", 8I
            | AcnGenericTypes.NextWord ->
                match ProgrammingLanguage.ActiveLanguages.Head with
                | Scala -> "Short", 16I
                | _ -> "NextWord", 16I
            | AcnGenericTypes.NextDWord ->
                match ProgrammingLanguage.ActiveLanguages.Head with
                | Scala -> "Int", 32I
                | _ -> "NextDWord", 32I
        let newFuncBody st errCode prms nestingScope p =
            let content, ns1a = funcBody st errCode prms nestingScope p
            let newContent =
                match content with
                | Some bodyResult   ->
                    let funcBodyStr = alignToNext bodyResult.funcBody alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = alignToNext "" alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {funcBody = funcBodyStr; errCodes =[errCode]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= true; bBsIsUnReferenced=false; resultExpr = None; auxiliaries = []; icdResult=None}
            newContent, ns1a
        newFuncBody

let handleAlignmentForAcnTypes (r:Asn1AcnAst.AstRoot)
                               (lm:LanguageMacros)
                               (acnAlignment : AcnAlignment option)
                               (funcBody: FuncBodyStateless): FuncBodyStateless =
    let alignToNext = lm.acn.alignToNext
    match acnAlignment with
    | None      -> funcBody
    | Some al   ->
        let alStr, nAlignmentVal =
            match al with
            | AcnGenericTypes.NextByte   -> "NextByte", 8I
            | AcnGenericTypes.NextWord   -> "NextWord", 16I
            | AcnGenericTypes.NextDWord  -> "NextDWord", 32I
        let newFuncBody (codec:CommonTypes.Codec) (prms: (RelativePath * AcnParameter) list) (nestingScope: NestingScope) (p: CodegenScope) (lvName:string) =
            let content = funcBody codec prms nestingScope p lvName
            let newContent =
                match content with
                | Some bodyResult   ->
                    let funcBodyStr = alignToNext bodyResult.funcBody alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {bodyResult with funcBody  = funcBodyStr}
                | None              ->
                    let funcBodyStr = alignToNext "" alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= true; bBsIsUnReferenced=false; resultExpr = None; auxiliaries = []; icdResult= None}
            newContent
        newFuncBody
