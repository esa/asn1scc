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

// ---------------------------------------------------------------------------
// ICD presentation of align-to-next (roadmap B3).
// The wire carries 0 .. alignment-1 padding bits in front of an aligned
// encoding and AcnEncodingClasses folds that amount into the type's
// acnMaxSizeInBits.  Make the padding explicit: the wrappers below prepend a
// dedicated PaddingRow to the wrapped icdResult, while the leaf row builders
// subtract the same amount from their field-row max
// (AcnHelpers.icdMaxSizeWithoutAlignment), so the table rows still sum up to
// the type's acnMin/MaxSizeInBits totals.
// The exact padding constant cannot be computed here: ICD rows are produced
// by executing funcBody with dummy nesting scopes (offset 0), so the
// preceding wire offset is unknown at row-construction time.  The per-row
// offset column planned by roadmap D1 (computed in the renderer, where the
// cumulative offset is known) is the place to refine 0..alignment-1 into an
// exact constant when all preceding rows are fixed-size.
// ---------------------------------------------------------------------------
let private alignmentUnitName (al: AcnGenericTypes.AcnAlignment) =
    match al with
    | AcnGenericTypes.NextByte  -> "byte"
    | AcnGenericTypes.NextWord  -> "word"
    | AcnGenericTypes.NextDWord -> "dword"

let icdPaddingRow (al: AcnGenericTypes.AcnAlignment) (sPresent: string) : IcdRow =
    let unit = alignmentUnitName al
    let nMaxPaddingBits = AcnEncodingClasses.getAlignmentSize (Some al)
    {IcdRow.idxOffset = Some 1
     fieldName = "Padding"
     comments = [sprintf "Padding inserted by the ACN 'align-to-next %s' property (0 bits when the encoding is already %s-aligned at this point)." unit unit]
     sPresent = sPresent
     sType = IcdPlainType "padding bits"
     sConstraint = None
     minLengthInBits = 0I
     maxLengthInBits = nMaxPaddingBits
     sUnits = None
     rowType = IcdRowType.PaddingRow}

/// Prepend the padding row of an align-to-next property to a wrapped type's
/// icdResult.  Row numbering: the padding row takes index 1; rows that carry
/// an explicit index are shifted by one (preserving schemes like the
/// SEQUENCE OF "Item #N" numbering); a lone unnumbered leaf row becomes
/// index 2; ThreeDOTs rows stay unnumbered.  Parent SEQUENCE/CHOICE tables
/// renumber embedded rows anyway - the indices here only matter for the
/// standalone table of an aligned type assignment.
let prependAlignmentPaddingRow (acnAlignment: AcnGenericTypes.AcnAlignment option) (icdResult: IcdArgAux option) : IcdArgAux option =
    match acnAlignment, icdResult with
    | None, _ -> icdResult
    | Some _, None -> None
    | Some al, Some aux ->
        let newRowsFunc fieldName sPresent comments =
            let rows, compChildren = aux.rowsFunc fieldName sPresent comments
            let shiftedRows =
                rows |> List.map (fun rw ->
                    match rw.rowType, rw.idxOffset with
                    // ThreeDOTs stay unnumbered; SubItemRow rows keep their own
                    // element index (their "N.k" sub-number, roadmap D2).
                    | IcdRowType.ThreeDOTs, _ -> rw
                    | IcdRowType.SubItemRow, _ -> rw
                    | _, Some i -> {rw with idxOffset = Some (i+1)}
                    | _, None   -> {rw with idxOffset = Some 2})
            (icdPaddingRow al sPresent) :: shiftedRows, compChildren
        Some {aux with rowsFunc = newRowsFunc}

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
                | _ -> lm.lg.getAlignmentByteTypeName, 8I
            | AcnGenericTypes.NextWord ->
                match ProgrammingLanguage.ActiveLanguages.Head with
                | Scala -> "Short", 16I
                | _ -> lm.lg.getAlignmentWordTypeName, 16I
            | AcnGenericTypes.NextDWord ->
                match ProgrammingLanguage.ActiveLanguages.Head with
                | Scala -> "Int", 32I
                | _ -> lm.lg.getAlignmentDWordTypeName, 32I
        let newFuncBody st errCode prms nestingScope p =
            let content, ns1a = funcBody st errCode prms nestingScope p
            let newContent =
                match content with
                | Some bodyResult   ->
                    let funcBodyStr = alignToNext bodyResult.funcBody alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {bodyResult with funcBody  = funcBodyStr; icdResult = prependAlignmentPaddingRow (Some al) bodyResult.icdResult}
                | None              ->
                    let funcBodyStr = alignToNext "" alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    // Aligned zero-bit type (pattern-less NULL / empty SEQUENCE):
                    // icdResult stays None here and AcnFunctionWrapper synthesizes
                    // the 0-bit row (zeroBitIcdResult) and prepends the padding row
                    // via prependAlignmentPaddingRow, so the type keeps its ICD row.
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
            | AcnGenericTypes.NextByte   -> lm.lg.getAlignmentByteTypeName, 8I
            | AcnGenericTypes.NextWord   -> lm.lg.getAlignmentWordTypeName, 16I
            | AcnGenericTypes.NextDWord  -> lm.lg.getAlignmentDWordTypeName, 32I
        let newFuncBody (codec:CommonTypes.Codec) (prms: (RelativePath * AcnParameter) list) (nestingScope: NestingScope) (p: CodegenScope) (lvName:string) =
            let content = funcBody codec prms nestingScope p lvName
            let newContent =
                match content with
                | Some bodyResult   ->
                    let funcBodyStr = alignToNext bodyResult.funcBody alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    Some {bodyResult with funcBody  = funcBodyStr; icdResult = prependAlignmentPaddingRow (Some al) bodyResult.icdResult}
                | None              ->
                    let funcBodyStr = alignToNext "" alStr nAlignmentVal nestingScope.acnOffset (nestingScope.acnOuterMaxSize - nestingScope.acnOffset) (nestingScope.nestingLevel - 1I) nestingScope.nestingIx nestingScope.acnRelativeOffset codec
                    // The wrapped ACN child encodes 0 bits (pattern-less ACN NULL)
                    // but the alignment itself still emits up to alignment-1 padding
                    // bits - give the parent table a padding-only row so its rows
                    // account for them.  The 0-bit child itself stays invisible,
                    // exactly as it is without alignment.
                    let paddingOnlyIcd =
                        let rowsFunc _ sPresent _ = [icdPaddingRow al sPresent], []
                        Some {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = "NULL"; rowsFunc = rowsFunc; commentsForTas = []; scope = "type"; name = None}
                    Some {funcBody = funcBodyStr; errCodes =[]; localVariables = []; userDefinedFunctions=[]; bValIsUnReferenced= true; bBsIsUnReferenced=false; resultExpr = None; auxiliaries = []; icdResult= paddingOnlyIcd}
            newContent
        newFuncBody
