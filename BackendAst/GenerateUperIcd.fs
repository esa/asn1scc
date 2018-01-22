module GenerateUperIcd

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
open Antlr.Runtime

let rec GetMySelfAndChildren (t:Asn1Type) = 
    seq {
        yield t
        match t.Kind with
        | SequenceOf o ->  yield! GetMySelfAndChildren o.childType
        | Sequence o   ->
            for ch in o.Asn1Children do 
                yield! GetMySelfAndChildren ch.Type
        | Choice o-> 
            for ch in o.children do 
                yield! GetMySelfAndChildren ch.chType
        |_ -> ()    
    }

let foldGenericCon  valToStrFunc    (c:Asn1AcnAst.GenericConstraint<'v>)  =
    Asn1Fold.foldGenericConstraint
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> stg_asn1.Print_SingleValueContraint (valToStrFunc v) ,s)
        c
        0 |> fst

let foldRangeCon valToStrFunc   (c:Asn1AcnAst.RangeTypeConstraint<'v1,'v1>)  =
    Asn1Fold.foldRangeTypeConstraint        
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> stg_asn1.Print_SingleValueContraint (valToStrFunc v) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> 
            stg_asn1.Print_RangeContraint (valToStrFunc v1) (valToStrFunc v2) minIsIn maxIsIn, s)
        (fun v1 minIsIn s   -> stg_asn1.Print_RangeContraint_val_MAX (valToStrFunc v1) minIsIn, s)
        (fun v2 maxIsIn s   -> stg_asn1.Print_RangeContraint_MIN_val (valToStrFunc v2) maxIsIn, s)
        c
        0 |> fst


let Kind2Name (stgFileName:string) (t:Asn1Type) =
    match t.Kind with
    | ReferenceType r               -> r.baseInfo.tasName.Value
    | Integer           _           -> icd_uper.Integer           stgFileName ()
    | BitString         _           -> icd_uper.BitString         stgFileName ()
    | OctetString       _           -> icd_uper.OctetString       stgFileName ()
    | Boolean           _           -> icd_uper.Boolean           stgFileName ()
    | Choice            _           -> icd_uper.Choice            stgFileName ()
    | Enumerated        _           -> icd_uper.Enumerated        stgFileName ()
    | IA5String         s           -> 
        match s.baseInfo.isNumeric with
        | true  -> icd_uper.NumericString     stgFileName ()
        | false -> icd_uper.IA5String         stgFileName ()
    | NullType          _           -> icd_uper.NullType          stgFileName ()
    | Real              _           -> icd_uper.Real              stgFileName ()
    | Sequence          _           -> icd_uper.Sequence          stgFileName ()
    | SequenceOf        _           -> icd_uper.SequenceOf        stgFileName ()

let getTypeName (stgFileName:string) (t:Asn1Type) =
    match t.Kind with
    | ReferenceType ref  -> icd_uper.EmmitSeqChild_RefType stgFileName ref.baseInfo.tasName.Value (ToC ref.baseInfo.tasName.Value)
    | _                  -> Kind2Name stgFileName t


let GetWhyExplanation (stgFileName:string) (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | Real  r    -> icd_uper.RealSizeExplained stgFileName ()
    | Integer i  ->
        match i.baseInfo.uperRange with
        | Asn1AcnAst.Concrete(a,b)  when a=b       -> icd_uper.ZeroSizeExplained stgFileName ()
        | Asn1AcnAst.Full                          -> icd_uper.IntSizeExplained stgFileName ()
        | _                             -> ""
    | _         -> ""


let rec printType (stgFileName:string) (m:Asn1Module) (tas:TypeAssignment) (t:Asn1Type) (r:AstRoot)  color =
    let GetCommentLine (comments:string array) (t:Asn1Type) =
        let singleComment = comments |> Seq.StrJoin (icd_uper.NewLine stgFileName ()) 
        let ret = 
            match (t.ActualType).Kind with
            | Enumerated  enum ->
                let EmitItem (n:Asn1AcnAst.NamedItem) =
                    let comment =  n.Comments |> Seq.StrJoin "\n"
                    match comment.Trim() with
                    | ""        ->    icd_uper.EmitEnumItem stgFileName n.Name.Value n.definitionValue
                    | _         ->    icd_uper.EmitEnumItemWithComment stgFileName n.Name.Value n.definitionValue comment
                let itemsHtml = 
                    enum.baseInfo.items |> 
                        List.filter(fun z -> 
                            let v = z.Name.Value
                            Asn1Fold.isValidValueGeneric enum.AllCons (=) v ) |>
                        List.map EmitItem
                let extraComment = icd_uper.EmitEnumInternalContents stgFileName itemsHtml
                match singleComment.Trim() with
                | ""    -> extraComment
                | _     -> singleComment + (icd_uper.NewLine stgFileName ()) + extraComment
            | _                 -> singleComment
        let ret = ret.Replace("/*","").Replace("*/","").Replace("--","")
        ret.Trim()
    let bitsToBytes nBits = BigInteger(System.Math.Ceiling(double(nBits)/8.0)).ToString()
    let getMinMaxBitsAndBytes nMinBits nMaxBits =
        let nMinBytes = bitsToBytes nMinBits
        let nMaxBytes = bitsToBytes nMaxBits
        (nMinBits.ToString(), nMinBytes.ToString(), nMaxBits.ToString(), nMaxBytes.ToString())
    let sMinBits, sMinBytes, sMaxBits, sMaxBytes = getMinMaxBitsAndBytes t.uperMinSizeInBits t.uperMaxSizeInBits
    let handlePrimitive (sAsn1Constraints:string) =
        let sTasName = tas.Name.Value
        let sKind = Kind2Name  stgFileName t
        let sMaxBitsExplained =  GetWhyExplanation stgFileName t r
        let sCommentLine = GetCommentLine tas.Comments t
        icd_uper.EmitPrimitiveType stgFileName color sTasName (ToC sTasName) sKind sMinBytes sMaxBytes sMaxBitsExplained sCommentLine ( if sAsn1Constraints.Trim() ="" then "N.A." else sAsn1Constraints) sMinBits sMaxBits (sCommentLine.Split [|'\n'|])
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
    |ReferenceType o ->
        printType stgFileName m tas t.ActualType r color
    |Sequence seq -> 
        let EmitChild (i:int) (ch:Asn1Child) =
            let sClass = if i % 2 = 0 then (icd_uper.EvenRow stgFileName ())  else (icd_uper.OddRow stgFileName ())
            let nIndex = BigInteger i
            let sComment = GetCommentLine ch.Comments ch.Type
            let sOptionality = match ch.Optionality with
                               | None                           -> "No"
                               | Some Asn1AcnAst.AlwaysAbsent   -> "Yes (always absent)"
                               | Some Asn1AcnAst.AlwaysPresent  -> "Yes (always present)"
                               | Some (Asn1AcnAst.Optional opt )-> "Def"
            let sType = getTypeName stgFileName ch.Type
            let sAsn1Constraints = ""
                //+++
                //let ret = ch.Type.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin ""
                //( if ret.Trim() ="" then "N.A." else ret)
    
            let sMinBits, sMinBytes, sMaxBits, sMaxBytes = getMinMaxBitsAndBytes ch.Type.uperMinSizeInBits ch.Type.uperMaxSizeInBits

            let sMaxBitsExplained =  GetWhyExplanation stgFileName ch.Type r
            icd_uper.EmmitSequenceChild stgFileName sClass nIndex ch.Name.Value sComment  sOptionality  sType sAsn1Constraints sMinBits (sMaxBits+sMaxBitsExplained)
        let SeqPreamble =
            let optChild = seq.Asn1Children |> Seq.filter (fun x -> x.Optionality.IsSome) |> Seq.mapi(fun i c -> icd_uper.EmmitSequencePreambleSingleComment stgFileName (BigInteger (i+1)) c.Name.Value)
            let nLen = optChild |> Seq.length
            if  nLen > 0 then
                let sComment = icd_uper.EmmitSequencePreambleComment stgFileName optChild
                let ret = icd_uper.EmmitSequenceChild stgFileName (icd_uper.OddRow stgFileName ()) (BigInteger 1) "Preamble" sComment  "No"  "Bit mask" "N.A." (nLen.ToString()) (nLen.ToString())
                Some ret
            else
                None
        let sTasName = tas.Name.Value
        let sMaxBitsExplained = ""
        let sCommentLine = GetCommentLine tas.Comments t
        
        let arChildren idx = seq.Asn1Children |> Seq.mapi(fun i ch -> EmitChild (idx + i) ch) |> Seq.toList
        let arRows =
            match SeqPreamble with 
            | None          -> arChildren 1
            | Some(prm)     -> prm::(arChildren 2)

        icd_uper.EmitSequence stgFileName color sTasName (ToC sTasName) sMinBytes sMaxBytes sMaxBitsExplained sCommentLine arRows (sCommentLine.Split [|'\n'|])
    |Choice chInfo   ->
        let EmitChild (i:int) (ch:ChChildInfo) =
            let sClass = if i % 2 = 0 then (icd_uper.EvenRow stgFileName ()) else (icd_uper.OddRow stgFileName () )
            let nIndex = BigInteger 2
            let sComment = GetCommentLine ch.Comments ch.chType
            let sType = getTypeName stgFileName ch.chType
            let sAsn1Constraints = ""
                //+++
                //let ret = ch.Type.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin ""
                //( if ret.Trim() ="" then "N.A." else ret)
            let sMinBits, sMinBytes, sMaxBits, sMaxBytes = getMinMaxBitsAndBytes ch.chType.uperMinSizeInBits ch.chType.uperMaxSizeInBits
            let sMaxBitsExplained =  GetWhyExplanation stgFileName ch.chType r
            icd_uper.EmmitChoiceChild stgFileName sClass nIndex ch.Name.Value sComment  sType sAsn1Constraints sMinBits (sMaxBits+sMaxBitsExplained)
        let ChIndex =
            let optChild = chInfo.children |> Seq.mapi(fun i c -> icd_uper.EmmitChoiceIndexSingleComment stgFileName (BigInteger (i+1)) c.Name.Value)
            let sComment = icd_uper.EmmitChoiceIndexComment stgFileName optChild
            let indexSize = (GetNumberOfBitsForNonNegativeInteger(BigInteger(Seq.length chInfo.children))).ToString()
            icd_uper.EmmitChoiceChild stgFileName (icd_uper.OddRow stgFileName ()) (BigInteger 1) "ChoiceIndex" sComment    "unsigned int" "N.A." indexSize indexSize
        let sTasName = tas.Name.Value
        let sMaxBitsExplained = ""
        let sCommentLine = GetCommentLine tas.Comments t
        let arChildren = chInfo.children |> Seq.mapi(fun i ch -> EmitChild (2 + i) ch) |> Seq.toList
        let arRows = ChIndex::arChildren
        icd_uper.EmitChoice stgFileName color sTasName (ToC sTasName) sMinBytes sMaxBytes sMaxBitsExplained sCommentLine arRows (sCommentLine.Split [|'\n'|])
    | OctetString _
    | IA5String  _
    | BitString  _
    | SequenceOf _   ->
        let getCharSize charSetLength =
            let charSize = GetNumberOfBitsForNonNegativeInteger (BigInteger (charSetLength-1))
            charSize.ToString()
        let sType, sAsn1Constraints, sMinChildBits, sMaxChildBits, nMinLength, nMaxLength = 
            match t.Kind with
            | SequenceOf o ->
                let child = o.childType
                let sAsn1Constraints = ""//+++ child.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin "" 
                let ret = ( if sAsn1Constraints.Trim() ="" then "N.A." else sAsn1Constraints)
                let sMinBits, _, sMaxBits, _ = getMinMaxBitsAndBytes child.uperMinSizeInBits child.uperMaxSizeInBits
                let sMaxBitsExplained =  GetWhyExplanation stgFileName child r
                let sChType = getTypeName stgFileName child
                sChType, ret, sMinBits, (sMaxBits+sMaxBitsExplained), o.baseInfo.minSize, o.baseInfo.maxSize
            | IA5String           o        -> "ASCII CHARACTER", "", getCharSize o.baseInfo.uperCharSet.Length, getCharSize o.baseInfo.uperCharSet.Length, o.baseInfo.minSize, o.baseInfo.maxSize
            | OctetString         o        -> "OCTET", "", "8", "8", o.baseInfo.minSize, o.baseInfo.maxSize
            | BitString           o        -> "BIT", "", "1","1", o.baseInfo.minSize, o.baseInfo.maxSize
            | _                            -> raise(BugErrorException "")
        let sizeUperRange =  Asn1AcnAst.Concrete(nMinLength, nMaxLength)
        let ChildRow (lineFrom:BigInteger) (i:BigInteger) =
            let sClass = if i % 2I = 0I then (icd_uper.EvenRow stgFileName ()) else (icd_uper.OddRow stgFileName ())
            let nIndex = lineFrom + i
            let sFieldName = icd_uper.ItemNumber stgFileName i
            let sComment = ""
            icd_uper.EmmitChoiceChild stgFileName sClass nIndex sFieldName sComment  sType sAsn1Constraints sMinChildBits sMaxChildBits
        
        let LengthRow =
            let nMin, nLengthSize = 
                match sizeUperRange with
                | Asn1AcnAst.Concrete(a,b)  when a=b       -> 0I, 0I
                | Asn1AcnAst.Concrete(a,b)                 -> (GetNumberOfBitsForNonNegativeInteger(b.AsBigInt-a.AsBigInt)), (GetNumberOfBitsForNonNegativeInteger(b.AsBigInt-a.AsBigInt))
                | Asn1AcnAst.NegInf(_)                     -> raise(BugErrorException "")
                | Asn1AcnAst.PosInf(b)                     ->  8I, 16I
                | Asn1AcnAst.Full                          -> 8I, 16I
            let comment = "Special field used by PER to indicate the number of items present in the array."
            let ret = "" //+++ t.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin "" 
            let sCon = ( if ret.Trim() ="" then "N.A." else ret)

            icd_uper.EmmitChoiceChild stgFileName (icd_uper.OddRow stgFileName ()) (BigInteger 1) "Length" comment    "unsigned int" sCon (nMin.ToString()) (nLengthSize.ToString())

        let sTasName = tas.Name.Value
        let sMaxBitsExplained = ""

        let sFixedLengthComment (nMax: BigInteger) =
            sprintf "Length is fixed to %A elements (no length determinant is needed)." nMax

        let arRows, sExtraComment = 
            match sizeUperRange with
            | Asn1AcnAst.Concrete(a,b)  when a=b && b<2     -> [ChildRow 0I 1I], "The array contains a single element."
            | Asn1AcnAst.Concrete(a,b)  when a=b && b=2     -> (ChildRow 0I 1I)::(ChildRow 0I 2I)::[], (sFixedLengthComment b.AsBigInt)
            | Asn1AcnAst.Concrete(a,b)  when a=b && b>2     -> (ChildRow 0I 1I)::(icd_uper.EmitRowWith3Dots stgFileName ())::(ChildRow 0I b.AsBigInt)::[], (sFixedLengthComment b.AsBigInt)
            | Asn1AcnAst.Concrete(a,b)  when a<>b && b<2    -> LengthRow::(ChildRow 1I 1I)::[],""
            | Asn1AcnAst.Concrete(a,b)                       -> LengthRow::(ChildRow 1I 1I)::(icd_uper.EmitRowWith3Dots stgFileName ())::(ChildRow 1I b.AsBigInt)::[], ""
            | Asn1AcnAst.PosInf(_)
            | Asn1AcnAst.Full                                -> LengthRow::(ChildRow 1I 1I)::(icd_uper.EmitRowWith3Dots stgFileName ())::(ChildRow 1I 65535I)::[], ""
            | Asn1AcnAst.NegInf(_)                           -> raise(BugErrorException "")

        let sCommentLine = match GetCommentLine tas.Comments t with
                           | null | ""  -> sExtraComment
                           | _          -> sprintf "%s%s%s" (GetCommentLine tas.Comments t) (icd_uper.NewLine stgFileName ()) sExtraComment


        icd_uper.EmitSizeable stgFileName color sTasName  (ToC sTasName) (Kind2Name stgFileName t) sMinBytes sMaxBytes sMaxBitsExplained sCommentLine arRows (sCommentLine.Split [|'\n'|])

let PrintTas (stgFileName:string) (m:Asn1Module) (tas:TypeAssignment) (r:AstRoot)  blueTasses =
    let tasColor =
        match blueTasses |> Seq.exists (fun x -> x = tas.Name.Value) with
        |true   -> icd_uper.Blue stgFileName ()
        |false  -> icd_uper.Orange stgFileName ()
    icd_uper.EmmitTass stgFileName (printType stgFileName m tas tas.Type r tasColor) 

let getModuleBlueTasses (m:Asn1Module) =

    m.TypeAssignments |> 
    Seq.collect(fun x -> GetMySelfAndChildren x.Type) |>
    Seq.choose(fun x -> match x.Kind with ReferenceType ref -> Some (ref.baseInfo.modName.Value,ref.baseInfo.tasName.Value) |_ -> None) |> Seq.toList

let PrintModule (stgFileName:string) (m:Asn1Module) (f:Asn1File) (r:AstRoot) =
    let blueTasses = getModuleBlueTasses m |> Seq.map snd
    let sortedTas = m.TypeAssignments //+++ spark_spec.SortTypeAssigments m r acn |> List.rev
    let tases = sortedTas  |> Seq.map (fun x -> PrintTas stgFileName m x r blueTasses) 
    let comments = []
    icd_uper.EmmitModule stgFileName m.Name.Value comments tases


let PrintFile1 (stgFileName:string) (f:Asn1File)  (r:AstRoot) =
    let modules = f.Modules |> Seq.map (fun  m -> PrintModule stgFileName m f r )  
    icd_uper.EmmitFile stgFileName (Path.GetFileName f.FileName) modules 

let PrintFile2 (stgFileName:string) (f:Asn1File) = 
    let tasNames = f.Modules |> Seq.collect(fun x -> x.TypeAssignments) |> Seq.map(fun x -> x.Name.Value) |> Seq.toArray
    let blueTasses = f.Modules |> Seq.collect(fun m -> getModuleBlueTasses m)
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
    let colorize (t: IToken, idx: int, tasses: string array, blueTassesWithLoc: (string*int*int) array) =
            let asn1Tokens = [| "PLUS-INFINITY";"MINUS-INFINITY";"GeneralizedTime";"UTCTime";"mantissa";"base";"exponent";"UNION";"INTERSECTION";
                "DEFINITIONS";"EXPLICIT";"TAGS";"IMPLICIT";"AUTOMATIC";"EXTENSIBILITY";"IMPLIED";"BEGIN";"END";"EXPORTS";"ALL";
                "IMPORTS";"FROM";"UNIVERSAL";"APPLICATION";"PRIVATE";"BIT";"STRING";"BOOLEAN";"ENUMERATED";"INTEGER";"REAL";
                "OPTIONAL";"SIZE";"OCTET";"MIN";"MAX";"TRUE";"FALSE";"ABSENT";"PRESENT";"WITH";
                "COMPONENT";"DEFAULT";"NULL";"PATTERN";"OBJECT";"IDENTIFIER";"RELATIVE-OID";"NumericString";
                "PrintableString";"VisibleString";"IA5String";"TeletexString";"VideotexString";"GraphicString";"GeneralString";
                "UniversalString";"BMPString";"UTF8String";"INCLUDES";"EXCEPT";"SET";"SEQUENCE";"CHOICE";"OF";"COMPONENTS"|]

            let blueTas = blueTassesWithLoc |> Array.tryFind(fun (_,l,c) -> l=t.Line && c=t.CharPositionInLine)
            let lt = icd_uper.LeftDiple stgFileName ()
            let gt = icd_uper.RightDiple stgFileName ()
            let containedIn = Array.exists (fun elem -> elem = t.Text)
            let isAsn1Token = containedIn asn1Tokens
            let isType = containedIn tasses
            let safeText = t.Text.Replace("<",lt).Replace(">",gt)
            let checkWsCmt (tok: IToken) =
                match tok.Type with
                |asn1Lexer.WS
                |asn1Lexer.COMMENT
                |asn1Lexer.COMMENT2 -> false
                |_ -> true
            let findToken = Array.tryFind(fun tok -> checkWsCmt tok)
            let findNextToken = f.Tokens.[idx+1..] |> findToken
            let findPrevToken = Array.rev f.Tokens.[0..idx-1] |> findToken
            let nextToken =
                let size = Seq.length(f.Tokens) - 1
                match findNextToken with
                |Some(tok) -> tok
                |None -> if idx = size then t else f.Tokens.[idx+1]
            let prevToken =
                match findPrevToken with
                |Some(tok) -> tok
                |None -> if idx = 0 then t else f.Tokens.[idx-1]
            let uid =
                match isType with
                |true -> if nextToken.Type = asn1Lexer.ASSIG_OP && prevToken.Type <> asn1Lexer.LID then icd_uper.TasName stgFileName safeText (ToC safeText) else icd_uper.TasName2 stgFileName safeText (ToC safeText)
                |false -> safeText
            let colored =
                match t.Type with
                |asn1Lexer.StringLiteral
                |asn1Lexer.OctectStringLiteral
                |asn1Lexer.BitStringLiteral -> icd_uper.StringLiteral stgFileName safeText
                |asn1Lexer.UID -> uid
                |asn1Lexer.COMMENT
                |asn1Lexer.COMMENT2 -> icd_uper.Comment stgFileName safeText
                |_ -> safeText
            match blueTas with
            |Some (s,_,_) -> icd_uper.BlueTas stgFileName (ToC s) safeText
            |None -> if isAsn1Token then icd_uper.Asn1Token stgFileName safeText else colored
    let asn1Content = f.Tokens |> Seq.mapi(fun i token -> colorize(token,i,tasNames,blueTassesWithLoc))
    icd_uper.EmmitFilePart2  stgFileName (Path.GetFileName f.FileName ) (asn1Content |> Seq.StrJoin "")

let DoWork (r:AstRoot) (stgFileName:string)   outFileName =
    let files1 = r.Files |> Seq.map (fun f -> PrintFile1 stgFileName f r ) 
    let files2 = r.Files |> Seq.map (PrintFile2 stgFileName)
    let allTypes = r.Files |> List.collect(fun x -> x.TypeAssignments) |> List.map(fun x -> x.Type) |> Seq.collect(fun x -> GetMySelfAndChildren x )
    let bIntegerSizeMustBeExplained = allTypes |> Seq.exists(fun x -> match x.Kind with 
                                                                      | Integer o-> 
                                                                        match o.baseInfo.uperRange with 
                                                                        | Asn1AcnAst.Full | Asn1AcnAst.PosInf(_) |  Asn1AcnAst.NegInf(_)  -> true 
                                                                        |_                               ->false 
                                                                      | _ -> false)
    let bRealSizeMustBeExplained = allTypes |> Seq.exists(fun x -> match x.Kind with Real _ ->true | _ -> false)
    let bLengthSizeMustBeExplained = false
    let bWithComponentMustBeExplained = false
    let bZeroBitsMustBeExplained = 
        allTypes |> 
        Seq.exists(fun x -> 
            match x.ActualType.Kind with 
            | Integer o -> o.baseInfo.uperMinSizeInBits = 0
            | _ -> false)
    let content = icd_uper.RootHtml stgFileName files1 files2 bIntegerSizeMustBeExplained bRealSizeMustBeExplained bLengthSizeMustBeExplained bWithComponentMustBeExplained bZeroBitsMustBeExplained
    File.WriteAllText(outFileName, content.Replace("\r",""))
