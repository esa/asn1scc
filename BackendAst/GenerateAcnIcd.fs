module GenerateAcnIcd
open System.Globalization
open System
open System.Numerics
open System.IO
open FsUtils
open CommonTypes
open DAst
open DastFold
open DAstUtilFunctions
open Antlr.Asn1
open Antlr.Acn
open Antlr.Runtime



let acnTokens = [
        "endianness"; "big"; "little"; "encoding"; "pos-int"; "twos-complement"; "BCD"; "ASCII";
        "IEEE754-1985-32"; "IEEE754-1985-64"; "size"; "null-terminated"; "align-to-next"; "byte";
        "word"; "dword"; "encode-values"; "true-value"; "false-value"; "pattern"; "present-when";
        "determinant"; "DEFINITIONS"; "BEGIN"; "END"; "CONSTANT"; "NOT"; "INTEGER"; "BOOLEAN"; "NULL"] |> Set.ofList

let Kind2Name  = GenerateUperIcd.Kind2Name


let makeEmptyNull (s:string) =
    match s with
    | null  -> null
    | _     -> match s.Trim() with "" -> null | _ -> s

// Generate a formatted version of the ACN grammar given as input,
// using the stringtemplate layouts.

let PrintAcnAsHTML stgFileName (r:AstRoot)  =
    let colorize (t: IToken, tasses: string array) =
            let lt = icd_acn.LeftDiple stgFileName ()
            let gt = icd_acn.RightDiple stgFileName ()
            let containedIn = Array.exists (fun elem -> elem = t.Text)
            let isAcnKeyword = acnTokens.Contains t.Text
            let isType = containedIn tasses
            let safeText = t.Text.Replace("<",lt).Replace(">",gt)
            let uid =
                match isType with
                |true -> icd_acn.TasName stgFileName safeText (ToC safeText)
                |false -> safeText
            let colored =
                match t.Type with
                |acnLexer.StringLiteral
                |acnLexer.BitStringLiteral -> icd_acn.StringLiteral stgFileName safeText
                |acnLexer.UID -> uid
                |acnLexer.COMMENT
                |acnLexer.COMMENT2 -> icd_acn.Comment stgFileName safeText
                |_ -> safeText
            if isAcnKeyword then icd_acn.AcnKeyword stgFileName safeText else colored

    let tasNames = r.Files |> Seq.collect(fun f -> f.Modules) |> Seq.collect(fun x -> x.TypeAssignments) |> Seq.map(fun x -> x.Name.Value) |> Seq.toArray

    r.acnParseResults |>
    Seq.map(fun pr -> pr.fileName, pr.tokens) |>
    Seq.map(fun (fName, tokens) ->
            //let f = r.Files |> Seq.find(fun x -> Path.GetFileNameWithoutExtension(x.FileName) = Path.GetFileNameWithoutExtension(fName))
            //let tasNames = f.Modules |> Seq.collect(fun x -> x.TypeAssignments) |> Seq.map(fun x -> x.Name.Value) |> Seq.toArray
            let content = tokens |> Seq.map(fun token -> colorize(token,tasNames))
            icd_acn.EmitFilePart2  stgFileName (Path.GetFileName fName) (content )
    )

let PrintAcnAsHTML2 stgFileName (r:AstRoot)  (icdHashesToPrint:string list) =
    let icdHashesToPrintSet = icdHashesToPrint |> Set.ofList
    let fileTypeAssignments =
        r.icdHashes.Values |>
        Seq.collect id |>
        Seq.choose(fun z ->
            match z.tasInfo with
            | Some ts when icdHashesToPrintSet.Contains z.hash  -> Some (ts.tasName, z.hash)
            | _ -> None) |>
        Map.ofSeq

    let colorize (t: IToken) =
            let lt = icd_acn.LeftDiple stgFileName ()
            let gt = icd_acn.RightDiple stgFileName ()
            let isAcnKeyword = acnTokens.Contains t.Text
            let safeText = t.Text.Replace("<",lt).Replace(">",gt)
            let uid =
                match fileTypeAssignments.TryFind t.Text with
                |Some hash (*when icdHashesToPrintSet.Contains hash*) -> icd_acn.TasName stgFileName safeText hash
                |None -> safeText
            let colored =
                match t.Type with
                |acnLexer.StringLiteral
                |acnLexer.BitStringLiteral -> icd_acn.StringLiteral stgFileName safeText
                |acnLexer.UID -> uid
                |acnLexer.COMMENT
                |acnLexer.COMMENT2 -> icd_acn.Comment stgFileName safeText
                |_ -> safeText
            if isAcnKeyword then icd_acn.AcnKeyword stgFileName safeText else colored

    r.acnParseResults |>
    Seq.map(fun pr -> pr.fileName, pr.tokens) |>
    Seq.map(fun (fName, tokens) ->
            //let f = r.Files |> Seq.find(fun x -> Path.GetFileNameWithoutExtension(x.FileName) = Path.GetFileNameWithoutExtension(fName))
            //let tasNames = f.Modules |> Seq.collect(fun x -> x.TypeAssignments) |> Seq.map(fun x -> x.Name.Value) |> Seq.toArray
            let content = tokens |> Seq.map(fun token -> colorize(token))
            icd_acn.EmitFilePart2  stgFileName (Path.GetFileName fName) (content)
    ) |> Seq.toList

let foldGenericCon = GenerateUperIcd.foldGenericCon
let foldRangeCon   = GenerateUperIcd.foldRangeCon

type SequenceRow = {
    sClass              : string
    nIndex              : BigInteger
    chName              : string
    sComment            : string
    sPresentWhen        : string
    sType               : string
    sAsn1Constraints    : string
    sMinBits            : string
    sMaxBits            : string
    noAlignToNextSize   : string
    soUnit              : string option
}
let enumStg : GenerateUperIcd.StgCommentLineMacros =
    {
        NewLine                  = icd_acn.NewLine
        EmitEnumItem             = icd_acn.EmitEnumItem
        EmitEnumItemWithComment  = icd_acn.EmitEnumItemWithComment
        EmitEnumInternalContents = icd_acn.EmitEnumInternalContents
    }

let handleNullType stgFileName (encodingPattern : AcnGenericTypes.PATTERN_PROP_VALUE option) defaultRetValue =
    match encodingPattern with
    | Some(AcnGenericTypes.PATTERN_PROP_BITSTR_VALUE bitStr)       -> icd_acn.NullTypeWithBitPattern  stgFileName  bitStr.Value
    | Some(AcnGenericTypes.PATTERN_PROP_OCTSTR_VALUE byteList     )-> icd_acn.NullTypeWithBytePattern stgFileName  (byteList |> List.map (fun z -> z.Value))
    | None                                                        -> defaultRetValue



let emitSequenceComponent (r:AstRoot) stgFileName (optionalLikeUperChildren:Asn1Child list) (i:int) (ch:SeqChildInfo) =
    let GetCommentLine = GenerateUperIcd.GetCommentLineFactory stgFileName enumStg
    let sClass = if i % 2 = 0 then (icd_acn.EvenRow stgFileName ()) else (icd_acn.OddRow stgFileName ())
    let nIndex = BigInteger i
    let sPresentWhen, bAlwaysAbsent =
        match ch with
        | AcnChild  _ -> "always", false
        | Asn1Child ch  ->
            let aux1 = function
                | 1 -> "st"
                | 2 -> "nd"
                | 3 -> "rd"
                | _ -> "th"
            match ch.Optionality with
            | None                      -> "always", false
            | Some(Asn1AcnAst.AlwaysPresent)   -> "always", false
            | Some(Asn1AcnAst.AlwaysAbsent)    -> "never", true
            | Some(Asn1AcnAst.Optional opt)    ->
                match opt.acnPresentWhen with
                | None  ->
                    let nBit =  optionalLikeUperChildren |> Seq.findIndex(fun x -> x.Name.Value = ch.Name.Value) |> (+) 1
                    sprintf "when the %d%s bit of the bit mask is set" nBit (aux1 nBit), false
                | Some (AcnGenericTypes.PresenceWhenBool presWhen)     ->
                    let dependency = r.deps.acnDependencies |> List.find(fun d -> d.asn1Type = ch.Type.id )
                    //match dependency.dependencyKind with
                    sprintf "when %s is true" presWhen.AsString , false
                | Some (AcnGenericTypes.PresenceWhenBoolExpression acnExp)  ->
                    let _, debugStr = AcnGenericCreateFromAntlr.printDebug acnExp
                    sprintf "when %s is true" debugStr, false

    let sType, sComment, sAsn1Constraints, nAlignToNextSize, soUnit  =
        match ch with
        | Asn1Child ch  ->
            let sType =
                let defaultRetValue =
                    match ch.Type.Kind with
                    | ReferenceType o when not o.baseInfo.hasExtraConstrainsOrChildrenOrAcnArgs ->
                        icd_acn.EmitSeqChild_RefType stgFileName ch.Type.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name (ToC ch.Type.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)
                    | OctetString o -> "OCTET STRING"
                    | _ ->
                        match ch.Type.ActualType.Kind with
                        | Sequence _
                        | Choice _
                        | SequenceOf _ ->
                            icd_acn.EmitSeqChild_RefType stgFileName ch.Type.id.AsString.RDD (ToC ch.Type.id.AsString.RDD)
                        | _            ->
                            icd_acn.EmitSeqChild_RefType stgFileName ch.Type.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name (ToC ch.Type.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)
                match ch.Type.ActualType.Kind with
                | DAst.NullType       o  when o.baseInfo.acnProperties.encodingPattern.IsSome  ->
                                                handleNullType stgFileName o.baseInfo.acnProperties.encodingPattern defaultRetValue
                | _                       -> defaultRetValue
            let soUnit =  GenerateUperIcd.getUnits ch.Type
            sType, GetCommentLine ch.Comments ch.Type, ch.Type.ConstraintsAsn1Str |> Seq.StrJoin "", AcnEncodingClasses.getAlignmentSize ch.Type.acnAlignment, soUnit
        | AcnChild ch   ->
            let commentLine =
                ch.Comments |> Seq.StrJoin (enumStg.NewLine stgFileName ())


            let sType,consAsStt =
                match ch.Type with
                | Asn1AcnAst.AcnInteger                o ->
                    let constAsStr = DAstAsn1.createAcnInteger (o.cons@o.withcons) |> Seq.StrJoin ""
                    match o.inheritInfo with
                    | None                              ->  icd_acn.Integer           stgFileName ()    , constAsStr
                    | Some inhInfo                      ->  icd_acn.EmitSeqChild_RefType stgFileName inhInfo.tasName (ToC inhInfo.tasName) , constAsStr
                | Asn1AcnAst.AcnNullType               o ->
                    let sType = handleNullType stgFileName o.acnProperties.encodingPattern (icd_acn.NullType           stgFileName ())
                    sType, ""
                | Asn1AcnAst.AcnBoolean                o -> icd_acn.Boolean           stgFileName (), ""
                | Asn1AcnAst.AcnReferenceToEnumerated  o -> icd_acn.EmitSeqChild_RefType stgFileName o.tasName.Value (ToC o.tasName.Value), ""
                | Asn1AcnAst.AcnReferenceToIA5String   o -> icd_acn.EmitSeqChild_RefType stgFileName o.tasName.Value (ToC o.tasName.Value), ""
            sType, commentLine, consAsStt, AcnEncodingClasses.getAlignmentSize ch.Type.acnAlignment, None
    let name = ch.Name
    let noAlignToNextSize = if nAlignToNextSize = 0I then None else (Some nAlignToNextSize)
    let acnMaxSizeInBits = ch.acnMaxSizeInBits - nAlignToNextSize
    let acnMinSizeInBits = (if bAlwaysAbsent then 0I else ch.acnMinSizeInBits) - nAlignToNextSize
    let sMaxBits, sMaxBytes = acnMaxSizeInBits.ToString(), BigInteger(System.Math.Ceiling(double(acnMaxSizeInBits)/8.0)).ToString()
    let sMinBits, sMinBytes = acnMinSizeInBits.ToString(), BigInteger(System.Math.Ceiling(double(acnMinSizeInBits)/8.0)).ToString()
    icd_acn.EmitSeqOrChoiceRow stgFileName sClass nIndex ch.Name sComment  sPresentWhen  sType sAsn1Constraints sMinBits sMaxBits noAlignToNextSize soUnit


let rec printType stgFileName (tas:GenerateUperIcd.IcdTypeAssignment) (t:Asn1Type) (m:Asn1Module) (r:AstRoot)  isAnonymousType : string list=
    let GetCommentLine = GenerateUperIcd.GetCommentLineFactory stgFileName enumStg

    let hasAcnDef = r.acnParseResults |> Seq.collect (fun p -> p.tokens) |> Seq.exists(fun x -> x.Text = tas.name)

    let sTasName = tas.name
    let sKind = Kind2Name stgFileName t
    let sMaxBits, sMaxBytes = t.acnMaxSizeInBits.ToString(), BigInteger(System.Math.Ceiling(double(t.acnMaxSizeInBits)/8.0)).ToString()
    let sMinBits, sMinBytes = t.acnMinSizeInBits.ToString(), BigInteger(System.Math.Ceiling(double(t.acnMinSizeInBits)/8.0)).ToString()
    let sMaxBitsExplained =  ""
    let sCommentLine = GetCommentLine tas.comments t
    let myParams colSpan=
        t.acnParameters |>
        List.mapi(fun i x ->
            let sType = match x.asn1Type with
                            | AcnGenericTypes.AcnParamType.AcnPrmInteger    _         -> "INTEGER"
                            | AcnGenericTypes.AcnParamType.AcnPrmBoolean    _         -> "BOOLEAN"
                            | AcnGenericTypes.AcnParamType.AcnPrmNullType   _         -> "NULL"
                            | AcnGenericTypes.AcnParamType.AcnPrmRefType(_,ts)     -> icd_acn.EmitSeqChild_RefType stgFileName ts.Value (ToC ts.Value)

            icd_acn.PrintParam stgFileName (i+1).AsBigInt x.name sType colSpan)



    let handlePrimitive (sAsn1Constraints:string)   =
        let ret = icd_acn.EmitPrimitiveType stgFileName isAnonymousType sTasName (ToC sTasName) hasAcnDef sKind sMinBytes sMaxBytes sMaxBitsExplained sCommentLine ( if sAsn1Constraints.Trim() ="" then "N.A." else sAsn1Constraints) sMinBits sMaxBits (myParams 2I) (sCommentLine.Split [|'\n'|]) t.unitsOfMeasure
        [ret]
    match t.Kind with
    | Integer    o   ->
        let sAsn1Constraints = o.AllCons |> List.map (foldRangeCon (fun z -> z.ToString())) |> Seq.StrJoin ""
        handlePrimitive sAsn1Constraints
    | Real    o   ->
        let sAsn1Constraints = o.AllCons |> List.map (foldRangeCon (fun z -> z.ToString())) |> Seq.StrJoin ""
        handlePrimitive sAsn1Constraints
    | Boolean   o   ->
        let sAsn1Constraints = o.AllCons |> List.map (foldGenericCon (fun z -> z.ToString().ToUpper() )) |> Seq.StrJoin ""
        handlePrimitive sAsn1Constraints
    | NullType  o   ->
        let sAsn1Constraints = ""
        handlePrimitive sAsn1Constraints
    | Enumerated  o   ->
        let sAsn1Constraints = ""
        handlePrimitive sAsn1Constraints
    | ObjectIdentifier  o   ->
        let sAsn1Constraints = ""
        handlePrimitive sAsn1Constraints
    |ReferenceType o when  o.baseInfo.encodingOptions.IsNone ->
        printType stgFileName tas o.resolvedType m r isAnonymousType
    |Sequence seq   ->
        let optionalLikeUperChildren =
            seq.Asn1Children
            |> Seq.filter (fun x ->
                match x.Optionality with
                | None  -> false
                | Some (Asn1AcnAst.Optional opt)  ->
                    match opt.acnPresentWhen with
                    | Some (AcnGenericTypes.PresenceWhenBool _) -> false
                    | Some (AcnGenericTypes.PresenceWhenBoolExpression _) -> false
                    | None                      -> true
                | _                               -> false)
            |> Seq.toList
        let SeqPreamble =
            match optionalLikeUperChildren with
            | []    -> None
            | _     ->
                let arrsOptWihtNoPresentWhenChildren =
                    optionalLikeUperChildren |> Seq.mapi(fun i c -> icd_acn.EmitSequencePreambleSingleComment stgFileName (BigInteger (i+1)) c.Name.Value)

                let nLen = optionalLikeUperChildren |> Seq.length
                let ret = icd_acn.EmitSeqOrChoiceRow stgFileName (icd_acn.OddRow stgFileName ()) 1I "Preamble" (icd_acn.EmitSequencePreambleComment stgFileName arrsOptWihtNoPresentWhenChildren)  "always"  "Bit mask" "N.A." (nLen.ToString()) (nLen.ToString()) None None
                Some ret
        let emitSequenceRow (i:int, curResult:string list) (ch:SeqChildInfo) =
            let di, newLines =
                match ch with
                | AcnChild  _ -> 1, [emitSequenceComponent r stgFileName optionalLikeUperChildren i ch]
                | Asn1Child ach  ->
                                1, [emitSequenceComponent r stgFileName optionalLikeUperChildren i ch]
                (*
                    match ach.Type.Kind with
                    | OctetString o when o.baseInfo.acnEncodingClass = Asn1AcnAst.SizeableAcnEncodingClass.SZ_EC_uPER ->
                        let lengthLine =
                            let sClass = if i % 2 = 0 then (icd_acn.EvenRow stgFileName ()) else (icd_acn.OddRow stgFileName ())

                            icd_acn.EmitSeqOrChoiceRow stgFileName sClass (BigInteger i) "length" "uper length determinant"  "???"  "OCTET STRING" "N/A" sMinBits sMaxBits None None

                        1, [emitSequenceComponent r stgFileName optionalLikeUperChildren i ch]
                    | _ -> 1, [emitSequenceComponent r stgFileName optionalLikeUperChildren i ch]
                *)
            (i+di, curResult@newLines)
        let arChildren idx =
            seq.children |>
            Seq.fold emitSequenceRow (idx, []) |>
            snd
//        let arChildren idx =
//            seq.children |>
//            Seq.mapi(fun i ch -> emitSequenceComponent r stgFileName optionalLikeUperChildren (idx + i) ch )
//            |> Seq.toList
        let childTasses =
            seq.children |>
            Seq.map(fun ch ->
                    match ch with
                    | Asn1Child ch  ->
                        match ch.Type.Kind with
                        | ReferenceType o when not o.baseInfo.hasExtraConstrainsOrChildrenOrAcnArgs -> []
                        | _     ->
                            match ch.Type.ActualType.Kind with
                            | Sequence _
                            | Choice _
                            | SequenceOf _ ->
                                let chTas = {tas with name=ch.Type.id.AsString.RDD; t=ch.Type; comments = Array.concat [ tas.comments; [|sprintf "Acn inline encoding in the context of %s type and %s component" tas.name ch.Name.Value|]]; isBlue = true }
                                printType stgFileName chTas ch.Type m r isAnonymousType
                            | _            -> []
                    | AcnChild _       -> [])|>
            Seq.collect id |> Seq.toList
        let arRows =
            match SeqPreamble with
            | None          -> arChildren 1
            | Some(prm)     -> prm::(arChildren 2)
        let seqRet = icd_acn.EmitSequenceOrChoice stgFileName isAnonymousType sTasName (ToC sTasName) hasAcnDef "SEQUENCE" sMinBytes sMaxBytes sMaxBitsExplained sCommentLine arRows (myParams 4I) (sCommentLine.Split [|'\n'|])
        [seqRet] @ childTasses
    |Choice chInfo   ->
        let EmitSeqOrChoiceChild (i:int) (ch:ChChildInfo)  getPresence =
            let sClass = if i % 2 = 0 then (icd_acn.EvenRow stgFileName ()) else (icd_acn.OddRow stgFileName ())
            let nIndex = BigInteger i
            let sComment = GetCommentLine ch.Comments ch.chType

            let sPresentWhen = getPresence i ch

            let sType =
                let defaultRetValue = icd_acn.EmitSeqChild_RefType stgFileName ch.chType.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name (ToC ch.chType.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)
                match ch.chType.Kind with
                | ReferenceType o when not o.baseInfo.hasExtraConstrainsOrChildrenOrAcnArgs -> defaultRetValue
                | _  ->
                    match ch.chType.ActualType.Kind with
                    | Sequence _
                    | Choice _
                    | SequenceOf _ ->
                        icd_acn.EmitSeqChild_RefType stgFileName ch.chType.id.AsString.RDD (ToC ch.chType.id.AsString.RDD)
                    | DAst.NullType       o  when o.baseInfo.acnProperties.encodingPattern.IsSome  ->
                                                    handleNullType stgFileName o.baseInfo.acnProperties.encodingPattern defaultRetValue
                    | _                       -> defaultRetValue

            let sAsn1Constraints = ch.chType.ConstraintsAsn1Str |> Seq.StrJoin ""
            let sMaxBits, sMaxBytes = ch.chType.acnMaxSizeInBits.ToString(), BigInteger(System.Math.Ceiling(double(ch.chType.acnMaxSizeInBits)/8.0)).ToString()
            let sMinBits, sMinBytes = ch.chType.acnMinSizeInBits.ToString(), BigInteger(System.Math.Ceiling(double(ch.chType.acnMinSizeInBits)/8.0)).ToString()
            let soUnit = GenerateUperIcd.getUnits ch.chType
            icd_acn.EmitSeqOrChoiceRow stgFileName sClass nIndex ch.Name.Value sComment  sPresentWhen  sType sAsn1Constraints sMinBits sMaxBits None soUnit
        let children = chInfo.children
        let children = children |> List.filter(fun ch -> match ch.Optionality with  Some (Asn1AcnAst.ChoiceAlwaysAbsent) -> false | _ -> true)
        let arrRows =
            match chInfo.ancEncClass with
            | CEC_uper          ->
                let ChIndex, curI =
                    //let optChild = chInfo.children |> Seq.mapi(fun i c -> icd_uper.EmitChoiceIndexSingleComment stgFileName (BigInteger (i+1)) c.Name.Value)
                    match children.Length <= 1 with
                    | true    -> [], 1
                    | false     ->
                        let sComment = icd_acn.EmitChoiceIndexComment stgFileName ()
                        let indexSize = (GetChoiceUperDeterminantLengthInBits(BigInteger(Seq.length children))).ToString()
                        let ret = icd_acn.EmitSeqOrChoiceRow stgFileName (icd_acn.OddRow stgFileName ()) (BigInteger 1) "ChoiceIndex" sComment  "always"  "unsigned int" "N.A." indexSize indexSize None None
                        [ret], 2
                    //icd_acn.EmitSeqOrChoiceRow stgFileName sClass nIndex ch.Name.Value sComment  sPresentWhen  sType sAsn1Constraints sMinBits sMaxBits
                let getPresenceWhenNone_uper (i:int) (ch:ChChildInfo) =
                    match children.Length <= 1 with
                    | true  -> "always"
                    | false ->
                        let index = children |> Seq.findIndex (fun z -> z.Name.Value = ch.Name.Value)
                        sprintf "ChoiceIndex = %d" index
                    //sprintf "ChoiceIndex = %d" i
                let EmitChild (i:int) (ch:ChChildInfo) = EmitSeqOrChoiceChild i ch  getPresenceWhenNone_uper
                let arChildren = children |> Seq.mapi(fun i ch -> EmitChild (curI + i) ch) |> Seq.toList
                ChIndex@arChildren
            | CEC_enum    (r,d)     ->
                let getPresence (i:int) (ch:ChChildInfo) =
                    let refToStr id =
                        match id with
                        | ReferenceToType sn -> sn |> List.rev |> List.head |> (fun x -> x.AsString)

                    sprintf "%s = %s" (refToStr d.id) ch.Name.Value
                let EmitChild (i:int) (ch:ChChildInfo) = EmitSeqOrChoiceChild i ch  getPresence
                children |> Seq.mapi(fun i ch -> EmitChild (1 + i) ch) |> Seq.toList
            | CEC_presWhen      ->
                let getPresence (i:int) (ch:ChChildInfo) =
                    let getPresenceSingle (pc:AcnGenericTypes.AcnPresentWhenConditionChoiceChild) =
                        match pc with
                        | AcnGenericTypes.PresenceInt   (rp, intLoc) -> sprintf "%s=%A" rp.AsString intLoc.Value
                        | AcnGenericTypes.PresenceStr   (rp, strLoc) -> sprintf "%s=%A" rp.AsString strLoc.Value
                    ch.acnPresentWhenConditions |> Seq.map getPresenceSingle |> Seq.StrJoin " AND "
                let EmitChild (i:int) (ch:ChChildInfo) = EmitSeqOrChoiceChild i ch  getPresence
                children |> Seq.mapi(fun i ch -> EmitChild (1 + i) ch) |> Seq.toList
        let chRet = icd_acn.EmitSequenceOrChoice stgFileName isAnonymousType sTasName (ToC sTasName) hasAcnDef "CHOICE" sMinBytes sMaxBytes sMaxBitsExplained sCommentLine arrRows (myParams 4I) (sCommentLine.Split [|'\n'|])
        let childTasses =
            chInfo.children |>
            Seq.map(fun ch ->
                    match ch.chType.Kind with
                    | ReferenceType o when not o.baseInfo.hasExtraConstrainsOrChildrenOrAcnArgs -> []
                    | _  ->
                        match ch.chType.ActualType.Kind with
                        | Sequence _
                        | Choice _
                        | SequenceOf _ ->
                            let chTas = {tas with name=ch.chType.id.AsString.RDD; t=ch.chType; comments = Array.concat [ tas.comments; [|sprintf "Acn inline encoding in the context of %s type and %s component" tas.name ch.Name.Value|]]; isBlue = true }
                            printType stgFileName chTas ch.chType m r isAnonymousType
                        | _            -> [] )|>
            Seq.collect id |> Seq.toList
        [chRet]@childTasses
    | IA5String  o  ->
        let nMin, nMax, encClass = o.baseInfo.minSize.acn, o.baseInfo.maxSize.acn, o.baseInfo.acnEncodingClass
        let sType, characterSizeInBits =
            match encClass with
            | Asn1AcnAst.Acn_Enc_String_uPER                                   characterSizeInBits             -> "NUMERIC CHARACTER" , characterSizeInBits.ToString()
            | Asn1AcnAst.Acn_Enc_String_uPER_Ascii                             characterSizeInBits             -> "ASCII CHARACTER"   , characterSizeInBits.ToString()
            | Asn1AcnAst.Acn_Enc_String_Ascii_Null_Terminated                  (characterSizeInBits, nullChars)  -> "ASCII CHARACTER"  , characterSizeInBits.ToString()
            | Asn1AcnAst.Acn_Enc_String_Ascii_External_Field_Determinant      (characterSizeInBits, rp)        -> "ASCII CHARACTER"   , characterSizeInBits.ToString()
            | Asn1AcnAst.Acn_Enc_String_CharIndex_External_Field_Determinant  (characterSizeInBits, rp)        -> "NUMERIC CHARACTER" , characterSizeInBits.ToString()
        let ChildRow (lineFrom:BigInteger) (i:BigInteger) =
            let sClass = if i % 2I = 0I then icd_acn.EvenRow stgFileName () else icd_acn.OddRow stgFileName ()
            let nIndex = lineFrom + i
            let sFieldName = icd_acn.ItemNumber stgFileName i
            let sComment = ""
            icd_acn.EmitChoiceChild stgFileName sClass nIndex sFieldName sComment  sType "" characterSizeInBits characterSizeInBits
        let NullRow (lineFrom:BigInteger) (i:BigInteger) =
            let sClass = if i % 2I = 0I then icd_acn.EvenRow stgFileName () else icd_acn.OddRow stgFileName ()
            let nIndex = lineFrom + i
            let sFieldName = icd_acn.ItemNumber stgFileName i
            let sComment = "NULL Character"
            icd_acn.EmitChoiceChild stgFileName sClass nIndex sFieldName sComment  sType "" characterSizeInBits characterSizeInBits

        let comment = "Special field used by ACN indicating the number of items."
        let sCon = t.ConstraintsAsn1Str |> Seq.StrJoin ""
        let sCon =  if sCon.Trim() ="" then "N.A." else sCon
        let lenDetSize = GetNumberOfBitsForNonNegativeInteger ( (o.baseInfo.maxSize.acn - o.baseInfo.minSize.acn))
        let arRows, sExtraComment =
            match encClass with
            | Asn1AcnAst.Acn_Enc_String_uPER                                  nSizeInBits              ->
                let lengthLine = icd_acn.EmitChoiceChild stgFileName (icd_acn.OddRow stgFileName ()) 1I "Length" comment    "unsigned int" sCon (lenDetSize.ToString()) (lenDetSize.ToString())
                lengthLine::(ChildRow 1I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 1I ( nMax))::[], ""
            | Asn1AcnAst.Acn_Enc_String_uPER_Ascii                            nSizeInBits              ->
                let lengthLine = icd_acn.EmitChoiceChild stgFileName (icd_acn.OddRow stgFileName ()) 1I "Length" comment    "unsigned int" sCon (lenDetSize.ToString()) (lenDetSize.ToString())
                lengthLine::(ChildRow 1I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 1I ( nMax))::[], ""
            | Asn1AcnAst.Acn_Enc_String_Ascii_Null_Terminated                  (nSizeInBits, nullChars)  ->
                (ChildRow 0I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 0I ( nMax))::(NullRow 0I ( (nMax+1I)))::[],""
            | Asn1AcnAst.Acn_Enc_String_Ascii_External_Field_Determinant      (nSizeInBits, rp)        ->
                (ChildRow 0I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 0I ( nMax))::[], sprintf "Length determined by external field %s" (rp.AsString)
            | Asn1AcnAst.Acn_Enc_String_CharIndex_External_Field_Determinant  (nSizeInBits, rp)        ->
                (ChildRow 0I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 0I ( nMax))::[], sprintf "Length determined by external field %s" (rp.AsString)
        let sCommentLine = match sCommentLine with
                           | null | ""  -> sExtraComment
                           | _          -> sprintf "%s%s%s" sCommentLine (icd_acn.NewLine stgFileName ()) sExtraComment

        let strRet = icd_acn.EmitSizeable stgFileName isAnonymousType sTasName  (ToC sTasName) hasAcnDef (Kind2Name stgFileName t) sMinBytes sMaxBytes sMaxBitsExplained (makeEmptyNull sCommentLine) arRows (myParams 2I) (sCommentLine.Split [|'\n'|])
        [strRet]
    | ReferenceType _
    | OctetString _
    | BitString  _
    | SequenceOf _   ->
        let nMin, nMax, encClass =
            match t.Kind with
            | OctetString o   ->
               o.baseInfo.minSize.acn, o.baseInfo.maxSize.acn, o.baseInfo.acnEncodingClass
            | BitString   o   ->
                o.baseInfo.minSize.acn, o.baseInfo.maxSize.acn, o.baseInfo.acnEncodingClass
            | SequenceOf  o   ->
                o.baseInfo.minSize.acn, o.baseInfo.maxSize.acn, o.baseInfo.acnEncodingClass
            | ReferenceType o   ->
                match o.baseInfo.encodingOptions with
                | None      -> raise(BugErrorException "")
                | Some eo   ->
                    eo.minSize.acn, eo.maxSize.acn, eo.acnEncodingClass
            | _                            -> raise(BugErrorException "")
        let ChildRow (lineFrom:BigInteger) (i:BigInteger) =
            let sClass = if i % 2I = 0I then icd_acn.EvenRow stgFileName () else icd_acn.OddRow stgFileName ()
            let nIndex = lineFrom + i
            let sFieldName = icd_acn.ItemNumber stgFileName i
            let sComment = ""
            let sType, sAsn1Constraints, sMinBits, sMaxBits =
                match t.Kind with
                | SequenceOf(seqOf) ->
                    let child = seqOf.childType
                    let sAsn1Constraints = child.ConstraintsAsn1Str |> Seq.StrJoin ""
                    let ret = ( if sAsn1Constraints.Trim() ="" then "N.A." else sAsn1Constraints)
                    let sMaxBits, sMaxBytes = child.acnMaxSizeInBits.ToString(), BigInteger(System.Math.Ceiling(double(child.acnMaxSizeInBits)/8.0)).ToString()
                    let sMinBits, sMinBytes = child.acnMinSizeInBits.ToString(), BigInteger(System.Math.Ceiling(double(child.acnMinSizeInBits)/8.0)).ToString()
                    let sType =
                        icd_acn.EmitSeqChild_RefType stgFileName child.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name (ToC child.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].asn1Name)
//                        match child.Kind with
//                        | ReferenceType ref -> icd_acn.EmitSeqChild_RefType stgFileName ref.baseInfo.tasName.Value (ToC ref.baseInfo.tasName.Value)
//                        | _                       -> Kind2Name stgFileName child
                    sType, ret, sMinBits, sMaxBits
                | OctetString        _         -> "OCTET", "", "8", "8"
                | BitString          _         -> "BIT", "", "1","1"
                | ReferenceType o   ->
                    match o.baseInfo.encodingOptions with
                    | None      -> raise(BugErrorException "")
                    | Some eo   ->
                        match eo.octOrBitStr with
                        | ContainedInOctString  -> "OCTET", "", "8", "8"
                        | ContainedInBitString  -> "BIT", "", "1","1"
                | _                            -> raise(BugErrorException "")
            icd_acn.EmitChoiceChild stgFileName sClass nIndex sFieldName sComment  sType sAsn1Constraints sMinBits sMaxBits
        let sFixedLengthComment = sprintf "Length is Fixed equal to %A, so no length determinant is encoded." nMax
        let arRows, sExtraComment =
            match encClass, nMax >= 2I with
            | Asn1AcnAst.SZ_EC_FIXED_SIZE, _
            | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _, _                     ->
                let sizeUperRange =  CommonTypes.Concrete(nMin, nMax)
                let sFixedLengthComment (nMax: BigInteger) =
                    sprintf "Length is fixed to %A elements (no length determinant is needed)." nMax
                let LengthRow =
                    let nMin, nLengthSize =
                        match sizeUperRange with
                        | CommonTypes.Concrete(a,b)  when a=b       -> 0I, 0I
                        | CommonTypes.Concrete(a,b)                 -> (GetNumberOfBitsForNonNegativeInteger(b - a)), (GetNumberOfBitsForNonNegativeInteger(b - a))
                        | CommonTypes.NegInf(_)                     -> raise(BugErrorException "")
                        | CommonTypes.PosInf(b)                     ->  8I, 16I
                        | CommonTypes.Full                          -> 8I, 16I
                    let comment = "Special field used by ACN to indicate the number of items present in the array."
                    let ret = t.ConstraintsAsn1Str |> Seq.StrJoin "" //+++ t.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin ""
                    let sCon = ( if ret.Trim() ="" then "N.A." else ret)

                    icd_acn.EmitChoiceChild stgFileName (icd_acn.OddRow stgFileName ()) (BigInteger 1) "Length" comment    "unsigned int" sCon (nMin.ToString()) (nLengthSize.ToString())

                match sizeUperRange with
                | CommonTypes.Concrete(a,b)  when a=b && b<2I     -> [ChildRow 0I 1I], "The array contains a single element."
                | CommonTypes.Concrete(a,b)  when a=b && b=2I     -> (ChildRow 0I 1I)::(ChildRow 0I 2I)::[], (sFixedLengthComment b)
                | CommonTypes.Concrete(a,b)  when a=b && b>2I     -> (ChildRow 0I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 0I b)::[], (sFixedLengthComment b)
                | CommonTypes.Concrete(a,b)  when a<>b && b<2I    -> LengthRow::(ChildRow 1I 1I)::[],""
                | CommonTypes.Concrete(a,b)                       -> LengthRow::(ChildRow 1I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 1I b)::[], ""
                | CommonTypes.PosInf(_)
                | CommonTypes.Full                                -> LengthRow::(ChildRow 1I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 1I 65535I)::[], ""
                | CommonTypes.NegInf(_)                           -> raise(BugErrorException "")

            | Asn1AcnAst.SZ_EC_ExternalField relPath,false    ->
                (ChildRow 0I 1I)::[], sprintf "Length is determined by the external field: %s" relPath.AsString
            | Asn1AcnAst.SZ_EC_ExternalField relPath,true     ->
                (ChildRow 0I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 0I (nMax))::[], sprintf "Length determined by external field %s" relPath.AsString
            | Asn1AcnAst.SZ_EC_TerminationPattern bitPattern, false ->
                (ChildRow 0I 1I)::[], sprintf "Length is determined by the stop marker '%s'" bitPattern.Value
            | Asn1AcnAst.SZ_EC_TerminationPattern bitPattern, true ->
                (ChildRow 0I 1I)::(icd_acn.EmitRowWith3Dots stgFileName ())::(ChildRow 0I (nMax))::[], sprintf "Length is determined by the stop marker '%s'" bitPattern.Value



        let sCommentLine = match sCommentLine with
                           | null | ""  -> sExtraComment
                           | _          -> sprintf "%s%s%s" sCommentLine (icd_acn.NewLine stgFileName ()) sExtraComment

        let sizeRet = icd_acn.EmitSizeable stgFileName false (*isAnonymousType*) sTasName  (ToC sTasName) hasAcnDef (Kind2Name stgFileName t) sMinBytes sMaxBytes sMaxBitsExplained (makeEmptyNull sCommentLine) arRows (myParams 2I) (sCommentLine.Split [|'\n'|])
        [sizeRet]
    | other -> raise (BugErrorException $"Unsupported kind for printType: {other}")
let PrintTas stgFileName (tas:GenerateUperIcd.IcdTypeAssignment) (m:Asn1Module) (r:AstRoot)   =
    //let isAnonymousType = blueTasses |> Seq.exists (fun x -> x = tas.Name.Value)
    let tasses = printType stgFileName tas tas.t  m r  tas.isBlue
    tasses |> List.map (icd_acn.EmitTass stgFileName ) |> Seq.StrJoin "\n"


let PrintModule stgFileName (m:Asn1Module) (f:Asn1File) (r:AstRoot)   =
    //let blueTasses = GenerateUperIcd.getModuleBlueTasses m |> Seq.map snd
    //let sortedTas = m.TypeAssignments //spark_spec.SortTypeAssignments m r acn |> List.rev
    let icdTasses = GenerateUperIcd.getModuleIcdTasses m

    let tases = icdTasses |> Seq.map (fun x -> PrintTas stgFileName x m r )
    let comments = m.Comments |> Array.map (fun x -> x.Trim().Replace("--", "").Replace("/*", "").Replace("*/",""))
    let moduleName = m.Name.Value
    let title = if comments.Length > 0 then moduleName + " - " + comments.[0] else moduleName
    let commentsTail = if comments.Length > 1 then comments.[1..] else [||]
    let acnFileName =
        match r.acnParseResults |> Seq.tryFind(fun (rp) -> rp.tokens |> Seq.exists (fun (token:IToken) -> token.Text = moduleName)) with
        | Some (rp) -> (Some (Path.GetFileName(rp.fileName)))
        | None                  -> None

    icd_acn.EmitModule stgFileName title (Path.GetFileName(f.FileName)) acnFileName commentsTail tases

let PrintNavLink stgFileName sTitle sTarget   =
    icd_acn.EmitNavLink stgFileName sTitle sTarget

let PrintTasses stgFileName (f:Asn1File)  (r:AstRoot)   =
    f.Modules |> Seq.map (fun  m -> PrintModule stgFileName m f r ) |> String.concat "\n"

let PrintNavLinks stgFileName (f:Asn1File)  (r:AstRoot)   =
    f.Modules |> Seq.map (fun  m -> PrintNavLink stgFileName m.Name.Value m.Name.Value ) |> String.concat "\n"

let emitCss (r:AstRoot) stgFileName   outFileName =
    let cssContent = icd_acn.RootCss stgFileName ()
    File.WriteAllText(outFileName, cssContent.Replace("\r", ""))

let selectTypeWithSameHash (lst:IcdTypeAss list) =
    match lst with
    | x1::[] -> x1
    | _      ->
        lst |> List.minBy(
            fun z ->
                let (ReferenceToType nodes) = z.typeId;
                nodes.Length)


let emitTypeCol stgFileName (r:AstRoot) (sType : IcdTypeCol) =
    match sType with
    | IcdPlainType label -> label
    | TypeHash hash ->
        let label = (selectTypeWithSameHash (r.icdHashes[hash])).name
        icd_acn.EmitSeqChild_RefType stgFileName label hash


let emitIcdRow stgFileName (r:AstRoot) _i (rw:IcdRow) =
    let i = match rw.idxOffset with Some z -> z | None -> 1
    let sComment = rw.comments |> Seq.StrJoin (icd_uper.NewLine stgFileName ())
    let sConstraint = match rw.sConstraint with None -> "N.A." | Some x -> x
    let sClass = if i % 2 = 0 then (icd_acn.EvenRow stgFileName ()) else (icd_acn.OddRow stgFileName ())
    match rw.rowType with
    |ThreeDOTs -> icd_acn.EmitRowWith3Dots stgFileName ()
    | _        -> icd_acn.EmitSeqOrChoiceRow stgFileName sClass (BigInteger i) rw.fieldName sComment  rw.sPresent  (emitTypeCol stgFileName r rw.sType) sConstraint (rw.minLengthInBits.ToString()) (rw.maxLengthInBits.ToString()) None rw.sUnits

let emitTas2 stgFileName (r:AstRoot) myParams (icdTas:IcdTypeAss)  =
    let sCommentLine = icdTas.comments |> Seq.StrJoin (icd_uper.NewLine stgFileName ())
    let arRows =
        icdTas.rows |> List.mapi (fun i rw -> emitIcdRow stgFileName r (i+1) rw)
    let bHasAcnDef = icdTas.hasAcnDefinition
    let sMaxBitsExplained = ""
    icd_acn.EmitSequenceOrChoice stgFileName false icdTas.name icdTas.hash bHasAcnDef (icdTas.kind) (icdTas.minLengthInBytes.ToString()) (icdTas.maxLengthInBytes.ToString()) sMaxBitsExplained sCommentLine arRows (myParams 4I) (sCommentLine.Split [|'\n'|])

(*
let rec PrintType2 stgFileName (r:AstRoot)  acnParams (icdTas:IcdTypeAss): string list =
    seq {
        let htmlContent = emitTas2 stgFileName acnParams icdTas
        yield htmlContent
        for c in icdTas.rows do
            match c.sType with
            | IcdPlainType _ -> ()
            | IcdRefType (_, IcdTypeAssHas hash) ->
                match r.icdHashes.TryFind hash with
                | Some chIcdTas ->
                    yield! PrintType2 stgFileName r (fun _ -> []) chIcdTas
                | None -> ()
    } |> Seq.toList
let printTas2 stgFileName (r:AstRoot) (ts:TypeAssignment) : string list =
    let myParams colSpan=
        ts.Type.acnParameters |>
        List.mapi(fun i x ->
            let sType =
                match x.asn1Type with
                | AcnGenericTypes.AcnParamType.AcnPrmInteger    _         -> "INTEGER"
                | AcnGenericTypes.AcnParamType.AcnPrmBoolean    _         -> "BOOLEAN"
                | AcnGenericTypes.AcnParamType.AcnPrmNullType   _         -> "NULL"
                | AcnGenericTypes.AcnParamType.AcnPrmRefType(_,ts)     -> icd_acn.EmitSeqChild_RefType stgFileName ts.Value (ToC ts.Value)
            icd_acn.PrintParam stgFileName (i+1).AsBigInt x.name sType colSpan)

    PrintType2 stgFileName r myParams ts.Type.icdFunction.typeAss
*)

let rec getMySelfAndChildren (r:AstRoot) (icdTas:IcdTypeAss) =
    seq {
        yield icdTas.hash
        for c in icdTas.rows do
            match c.sType with
            | IcdPlainType _ -> ()
            | TypeHash( hash) ->
                match r.icdHashes.TryFind hash with
                | Some chIcdTas ->
                    yield! getMySelfAndChildren r (selectTypeWithSameHash chIcdTas)
                | None -> ()

    } |> Seq.toList
(*
let PrintTasses2 stgFileName (r:AstRoot) : string list =
    let pdus = r.args.icdPdus |> Option.map Set.ofList
    r.icdHashes.Values |>
    Seq.collect id |>
    Seq.choose(fun z ->
        match z.tasInfo with
        | None -> None
        | Some ts when pdus.IsNone || pdus.Value.Contains ts.tasName -> Some z
        | Some _ -> None ) |>
    Seq.collect(fun icdTas -> getMySelfAndChildren r icdTas) |>
    Seq.distinct |>
    Seq.choose(fun hash ->
        let acnParams colSpan = []
        match r.icdHashes.TryFind hash with
        | Some chIcdTas -> Some (emitTas2 stgFileName r (fun _ -> []) (selectTypeWithSameHash chIcdTas))
        | None -> None) |>
    Seq.toList
*)
let printTasses3 stgFileName (r:DAst.AstRoot) : (string list)*(string list)*(IcdTypeAss list) =
    let pdus = r.args.icdPdus |> Option.map Set.ofList
    let icdHashesToPrint =
        seq {
            for f in r.Files do
                for m in f.Modules do
                    for tas in m.TypeAssignments do
                        match pdus.IsNone || pdus.Value.Contains tas.Name.Value with
                        | true  -> 
                            match tas.Type.icdTas with
                            | Some icdTas -> 
                                let icdTassesHash = getMySelfAndChildren r icdTas
                                yield! icdTassesHash
                            | None -> ()
                        | false -> ()
        } |> Seq.distinct |> Seq.toList
    //print in a file the icds that are going to be printed
    let content = icdHashesToPrint |> Seq.StrJoin "\n"
    let fileName = sprintf "%s_icdHashesToPrint.txt" stgFileName
    File.WriteAllText(fileName, content)
    let files, navLinks =
        icdHashesToPrint
        |> Seq.choose(fun hash ->
            match r.icdHashes.TryFind hash with
            | Some chIcdTas -> 
                //let PrintNavLink stgFileName sTitle sTarget   =
                let tas = selectTypeWithSameHash chIcdTas
                let tasContent = emitTas2 stgFileName r (fun _ -> []) tas
                //let tasLink = icd_acn.EmitNavLink stgFileName tas.name tas.hash
                Some (tasContent, tas)
            | None -> None) |> Seq.toList |> List.unzip
    (files, icdHashesToPrint, navLinks)

let PrintAsn1FileInColorizedHtml (stgFileName:string) (r:AstRoot) (icdHashesToPrint:string list) (f:Asn1File) =
    let debug (tsName:string) =
        r.icdHashes.Values |> 
        Seq.collect id |> 
        Seq.filter(fun ts -> ts.name = tsName) |>
        Seq.iter(fun ts ->
            let content = DAstUtilFunctions.serializeIcdTasToText ts
            let fileName = sprintf "%s_%s.txt" tsName ts.hash
            File.WriteAllText(fileName, content))
    //debug("ALPHA-TC-SECONDARY-HEADER")
    //let tryCreateRefType = CreateAsn1AstFromAntlrTree.CreateRefTypeContent
    let icdHashesToPrintSet = icdHashesToPrint |> Set.ofList
    let fileModules = f.Modules |> List.map(fun m -> m.Name.Value) |> Set.ofList
    let fileTypeAssignments =
        r.icdHashes.Values |>
        Seq.collect id |>
        Seq.choose(fun z ->
            //match z.tasInfo with
            //| None -> None
            //| Some ts when icdHashesToPrintSet.Contains z.hash &&  fileModules.Contains ts.modName -> Some (ts.tasName, z.hash)
            //| Some _ -> None ) |> 
            match icdHashesToPrintSet.Contains z.hash with
            | true  -> Some (z.name, z.hash)
            | false -> None ) |>
        Seq.distinct |>
        Seq.groupBy fst |>
        Seq.map(fun (tasName, tasHashes) -> (tasName, tasHashes |> Seq.map snd |> List.ofSeq)) |> Map.ofSeq


    //let blueTasses = f.Modules |> Seq.collect(fun m -> getModuleBlueTasses m)
    let blueTassesWithLoc =
              f.TypeAssignments |>
              Seq.map(fun x -> x.Type) |>
              Seq.collect(fun x -> GetMySelfAndChildren x) |>
              Seq.choose(fun x -> match x.Kind with
                                  |ReferenceType ref    ->
                                    match f.TypeAssignments |> Seq.tryFind(fun y -> y.Name.Value = ref.baseInfo.tasName.Value) with
                                    | Some tas  -> Some(ref.baseInfo.tasName.Value, tas.Type.Location.srcLine, tas.Type.Location.charPos)
                                    | None      -> None
                                  | _                           -> None ) |> Seq.toArray
    let colorize (t: IToken, idx: int, blueTassesWithLoc: (string*int*int) array) =

            let blueTas = blueTassesWithLoc |> Array.tryFind(fun (_,l,c) -> l=t.Line && c=t.CharPositionInLine)
            let lt = icd_uper.LeftDiple stgFileName ()
            let gt = icd_uper.RightDiple stgFileName ()
            let isAsn1Token = GenerateUperIcd.asn1Tokens.Contains t.Text
            //let isType = containedIn tasses
            let safeText = t.Text.Replace("<",lt).Replace(">",gt)
            let uid () =
                let checkWsCmt (tok: IToken) =
                    match tok.Type with
                    |asn1Lexer.WS
                    |asn1Lexer.COMMENT
                    |asn1Lexer.COMMENT2 -> false
                    |_ -> true
                let findToken = Array.tryFind(fun tok -> checkWsCmt tok)
                let findNextToken = f.Tokens.[idx+1..] |> findToken
                let findPrevToken = Array.rev f.Tokens.[0..idx-1] |> findToken
                
                //let findNextToken = f.Tokens.[idx+1..] |> Array.tryFind(fun tok -> checkWsCmt tok)
                //let findPrevToken = f.Tokens.[0..idx-1] |> Array.tryFindBack(fun tok -> checkWsCmt tok)

                let nextToken =
                    let size = Seq.length(f.Tokens) - 1
                    match findNextToken with
                    |Some(tok) -> tok
                    |None -> if idx = size then t else f.Tokens.[idx+1]
                let prevToken =
                    match findPrevToken with
                    |Some(tok) -> tok
                    |None -> if idx = 0 then t else f.Tokens.[idx-1]
                //let tasfileTypeAssignments = fileTypeAssignments |> List.filter(fun (tasName,_) -> tasName = t.Text)
                //match tasfileTypeAssignments with
                match fileTypeAssignments.TryFind t.Text with
                | None -> safeText
                | Some ([]) -> safeText
                //| (_,tasHash)::[] ->
                | Some (tasHash::[]) ->
                    if nextToken.Type = asn1Lexer.ASSIG_OP && prevToken.Type <> asn1Lexer.LID then
                        icd_uper.TasName stgFileName safeText tasHash
                    else
                        icd_uper.TasName2 stgFileName safeText tasHash
                | Some tasHashList ->
                    let tasName = t.Text
                    let debug () =
                        printfn "Warning: %s is not unique. %d" t.Text tasHashList.Length

                        printfn "Warning: %A" tasHashList
                        //print to jso the type assignments that are not unique
                        tasHashList |> 
                        Seq.iter (fun (tasHash) -> 
                            let icdTas = r.icdHashes.[tasHash] 
                            icdTas |> 
                            Seq.iter(fun icdTas ->
                                let content = DAstUtilFunctions.serializeIcdTasToText icdTas
                                let fileName = sprintf "%s_%s.txt" tasName icdTas.hash
                                File.WriteAllText(fileName, content)))
                        
                    //debug ()
                    safeText
                //|None -> safeText
            let colored () =
                match t.Type with
                |asn1Lexer.StringLiteral
                |asn1Lexer.OctectStringLiteral
                |asn1Lexer.BitStringLiteral -> icd_uper.StringLiteral stgFileName safeText
                |asn1Lexer.UID -> uid ()
                |asn1Lexer.COMMENT
                |asn1Lexer.COMMENT2 -> icd_uper.Comment stgFileName safeText
                |_ -> safeText
            match blueTas with
            |Some (s,_,_) -> icd_uper.BlueTas stgFileName (ToC s) safeText
            |None -> if isAsn1Token then icd_uper.Asn1Token stgFileName safeText else (colored ())
    let asn1Content = f.Tokens |> Seq.mapi(fun i token -> colorize(token,i,blueTassesWithLoc)) |> Seq.toList
    icd_uper.EmitFilePart2  stgFileName (Path.GetFileName f.FileName ) (asn1Content )


let DoWork (r:AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (stgFileName:string) (asn1HtmlStgFileMacros:string option)   outFileName =
    let files1 =  TL "GenerateAcnIcd_PrintTasses" (fun () -> r.Files |> List.map (fun f -> PrintTasses stgFileName f r ))
    let (files1b, icdHashesToPrint, icdTasList ) = TL "GenerateAcnIcd_printTasses3" (fun () -> printTasses3 stgFileName r)
    let bAcnParamsMustBeExplained = true
    let asn1HtmlMacros =
        match asn1HtmlStgFileMacros with
        | None  -> stgFileName
        | Some x -> x
    let files2 = TL "GenerateAcnIcd_PrintAsn1FileInColorizedHtml" (fun () -> r.Files |> List.map (PrintAsn1FileInColorizedHtml asn1HtmlMacros r icdHashesToPrint))
    let files3 = TL "GenerateAcnIcd_PrintAcnAsHTML2" (fun () -> PrintAcnAsHTML2 stgFileName r icdHashesToPrint)

    let navLinks = 
        icdTasList |> List.map(fun tas -> PrintNavLink stgFileName tas.name tas.hash) |> String.concat "\n"
     (*
        seq {
            for f in r.Files do
                for m in f.Modules do
                    let moduleLink = PrintNavLink stgFileName m.Name.Value m.Name.Value
                    yield moduleLink
                    let thisModuleTasses = 
                        icdTasList |>
                        List.choose(fun tas ->
                            match tas.tasInfo with
                            | None -> None
                            | Some tsInfo when tsInfo.modName = m.Name.Value -> Some tas
                            | Some _ -> None) |>
                        List.sortBy (fun tas -> tas.name)
                    for tas in thisModuleTasses do
                        let tasLink = PrintNavLink stgFileName tas.name tas.hash
                        yield tasLink
        } |> Seq.distinct |> Seq.toList |> String.concat "\n"*)

    let cssFileName = Path.ChangeExtension(outFileName, ".css")
    let htmlContent = TL "GenerateAcnIcd_RootHtml" (fun () -> icd_acn.RootHtml stgFileName files1 files2 bAcnParamsMustBeExplained files3 (Path.GetFileName(cssFileName)) navLinks)
    let htmlContentb = TL "GenerateAcnIcd_RootHtml_b" (fun () -> icd_acn.RootHtml stgFileName files1b files2 bAcnParamsMustBeExplained files3 (Path.GetFileName(cssFileName)) navLinks)

    File.WriteAllText(outFileName, htmlContent.Replace("\r",""))
    File.WriteAllText(outFileName.Replace(".html", "_new.html"), htmlContentb.Replace("\r",""))
    let cssFileName = Path.ChangeExtension(outFileName, ".css");
    TL "GenerateAcnIcd_emitCss" (fun () -> emitCss r stgFileName cssFileName)


