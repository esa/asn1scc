(*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*)

module icdUper

open System.Numerics
open FsUtils
open Ast
open System.IO
open VisitTree
open uPER
open CloneTree
open spark_utils
open Antlr.Asn1
open Antlr.Runtime


let Kind2Name (stgFileName:string) (t:Asn1Type) =
    match t.Kind with
    | ReferenceType(md,ts, _)   -> ts.Value
    | Integer                       -> icd_uper.Integer           stgFileName ()
    | BitString                     -> icd_uper.BitString         stgFileName ()
    | OctetString                   -> icd_uper.OctetString       stgFileName ()
    | Boolean                       -> icd_uper.Boolean           stgFileName ()
    | Choice(_)                     -> icd_uper.Choice            stgFileName ()
    | Enumerated(_)                 -> icd_uper.Enumerated        stgFileName ()
    | IA5String                     -> icd_uper.IA5String         stgFileName ()
    | NumericString                 -> icd_uper.NumericString     stgFileName ()
    | NullType                      -> icd_uper.NullType          stgFileName ()
    | Real                          -> icd_uper.Real              stgFileName ()
    | Sequence(_)                   -> icd_uper.Sequence          stgFileName ()
    | SequenceOf(_)                 -> icd_uper.SequenceOf        stgFileName ()


let GetWhyExplanation (stgFileName:string) (t:Ast.Asn1Type) (r:AstRoot) =
    match t.Kind with
    | Real      -> icd_uper.RealSizeExplained stgFileName ()
    | Integer   ->
        match (uPER.GetTypeUperRange t.Kind t.Constraints r) with
        | Concrete(a,b)  when a=b       -> icd_uper.ZeroSizeExplained stgFileName ()
        | Full                          -> icd_uper.IntSizeExplained stgFileName ()
        | _                             -> ""
    | _         -> ""

let rec printType (stgFileName:string) (m:Ast.Asn1Module) (tas:Ast.TypeAssignment) (t:Ast.Asn1Type) (r:AstRoot) (acn:AcnTypes.AcnAstResolved)  color =
    let uperSizeInBitsAsInt func (kind:Asn1TypeKind) (cons:list<Asn1Constraint>)  (ast:AstRoot) =
        match (func kind cons ast) with
        | Bounded(maxBits)        ->  
            let maxBytes = BigInteger(System.Math.Ceiling(double(maxBits)/8.0))
            maxBits.ToString(), maxBytes.ToString()
        | Infinite          -> icd_uper.Infinity stgFileName (), icd_uper.Infinity stgFileName ()


    let GetCommentLine (comments:string array) (t:Asn1Type) =
        let singleComment = comments |> Seq.StrJoin (icd_uper.NewLine stgFileName ()) 
        let ret = 
            match (Ast.GetActualType t r).Kind with
            | Enumerated(items) ->
                let EmitItem (n:Ast.NamedItem) =
                    let comment =  n.Comments |> Seq.StrJoin "\n"
                    match comment.Trim() with
                    | ""        ->    icd_uper.EmitEnumItem stgFileName n.Name.Value (GetItemValue items n r)
                    | _         ->    icd_uper.EmitEnumItemWithComment stgFileName n.Name.Value (GetItemValue items n r) comment
                let itemsHtml = 
                    CheckAsn1.getEnumeratedAllowedEnumerations r m t |>
                    Seq.map EmitItem
                let extraComment = icd_uper.EmitEnumInternalContents stgFileName itemsHtml
                match singleComment.Trim() with
                | ""    -> extraComment
                | _     -> singleComment + (icd_uper.NewLine stgFileName ()) + extraComment
            | _                 -> singleComment
        let ret = ret.Replace("/*","").Replace("*/","").Replace("--","")
        ret.Trim()
    match t.Kind with
    | Integer    
    | Real    
    | Boolean   
    | NullType
    | Enumerated(_) ->
        let sTasName = tas.Name.Value
        let sKind = Kind2Name  stgFileName t
        let sMaxBits, sMaxBytes = uperSizeInBitsAsInt uperGetMaxSizeInBits t.Kind t.Constraints r
        let sMinBits, sMinBytes = uperSizeInBitsAsInt uperGetMinSizeInBits t.Kind t.Constraints r
        let sMaxBitsExplained =  GetWhyExplanation stgFileName t r
        let sCommentLine = GetCommentLine tas.Comments t
        let sAsn1Constraints = t.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin ""

        icd_uper.EmitPrimitiveType stgFileName color sTasName (ToC sTasName) sKind sMinBytes sMaxBytes sMaxBitsExplained sCommentLine ( if sAsn1Constraints.Trim() ="" then "N.A." else sAsn1Constraints) sMinBits sMaxBits (sCommentLine.Split [|'\n'|])

    |ReferenceType(_) ->
        let baseTypeWithCons = Ast.GetActualTypeAllConsIncluded t r
        printType stgFileName m tas baseTypeWithCons r acn color
    |Sequence(children) -> 
        let EmitChild (i:int) (ch:ChildInfo) =
            let sClass = if i % 2 = 0 then (icd_uper.EvenRow stgFileName ())  else (icd_uper.OddRow stgFileName ())
            let nIndex = BigInteger i
            let sComment = GetCommentLine ch.Comments ch.Type
            let sOptionality = match ch.Optionality with
                               | None       -> "No"
                               | Some(Default(_))   -> "Def"
                               | Some(_)            -> "Yes"
            let sType = match ch.Type.Kind with
                        | ReferenceType(md,ts,_)  -> icd_uper.EmmitSeqChild_RefType stgFileName ts.Value (ToC ts.Value)
                        | _                           -> Kind2Name stgFileName ch.Type
            let sAsn1Constraints = 
                let ret = ch.Type.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin ""
                ( if ret.Trim() ="" then "N.A." else ret)
            let sMaxBits, sMaxBytes = uperSizeInBitsAsInt uperGetMaxSizeInBits  ch.Type.Kind ch.Type.Constraints r
            let sMinBits, sMinBytes = uperSizeInBitsAsInt uperGetMinSizeInBits  ch.Type.Kind ch.Type.Constraints r
            let sMaxBitsExplained =  GetWhyExplanation stgFileName ch.Type r
            icd_uper.EmmitSequenceChild stgFileName sClass nIndex ch.Name.Value sComment  sOptionality  sType sAsn1Constraints sMinBits (sMaxBits+sMaxBitsExplained)

        let SeqPreamble =
            let optChild = children |> Seq.filter (fun x -> x.Optionality.IsSome) |> Seq.mapi(fun i c -> icd_uper.EmmitSequencePreambleSingleComment stgFileName (BigInteger (i+1)) c.Name.Value)
            let nLen = optChild |> Seq.length
            if  nLen > 0 then
                let sComment = icd_uper.EmmitSequencePreambleComment stgFileName optChild
                let ret = icd_uper.EmmitSequenceChild stgFileName (icd_uper.OddRow stgFileName ()) (BigInteger 1) "Preamble" sComment  "No"  "Bit mask" "N.A." (nLen.ToString()) (nLen.ToString())
                Some ret
            else
                None

        let sTasName = tas.Name.Value
        let sMaxBits, sMaxBytes = uperSizeInBitsAsInt uperGetMaxSizeInBits t.Kind t.Constraints r
        let sMinBits, sMinBytes = uperSizeInBitsAsInt uperGetMinSizeInBits t.Kind t.Constraints r
        let sMaxBitsExplained = ""
        let sCommentLine = GetCommentLine tas.Comments t

        let arChildren idx = children |> Seq.mapi(fun i ch -> EmitChild (idx + i) ch) |> Seq.toList
        let arRows =
            match SeqPreamble with 
            | None          -> arChildren 1
            | Some(prm)     -> prm::(arChildren 2)

        icd_uper.EmitSequence stgFileName color sTasName (ToC sTasName) sMinBytes sMaxBytes sMaxBitsExplained sCommentLine arRows (sCommentLine.Split [|'\n'|])

    |Choice(children)   ->
        let EmitChild (i:int) (ch:ChildInfo) =
            let sClass = if i % 2 = 0 then (icd_uper.EvenRow stgFileName ()) else (icd_uper.OddRow stgFileName () )
            let nIndex = BigInteger 2
            let sComment = GetCommentLine ch.Comments ch.Type
            let sType = match ch.Type.Kind with
                        | ReferenceType(md,ts,_)   -> icd_uper.EmmitSeqChild_RefType stgFileName ts.Value (ToC ts.Value)
                        | _                        -> Kind2Name stgFileName ch.Type
            let sAsn1Constraints = 
                let ret = ch.Type.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin ""
                ( if ret.Trim() ="" then "N.A." else ret)
            let sMaxBits, sMaxBytes = uperSizeInBitsAsInt uperGetMaxSizeInBits ch.Type.Kind ch.Type.Constraints r
            let sMinBits, sMinBytes = uperSizeInBitsAsInt uperGetMinSizeInBits ch.Type.Kind ch.Type.Constraints r
            let sMaxBitsExplained =  GetWhyExplanation stgFileName ch.Type r
            icd_uper.EmmitChoiceChild stgFileName sClass nIndex ch.Name.Value sComment  sType sAsn1Constraints sMinBits (sMaxBits+sMaxBitsExplained)
        let ChIndex =
            let optChild = children |> Seq.mapi(fun i c -> icd_uper.EmmitChoiceIndexSingleComment stgFileName (BigInteger (i+1)) c.Name.Value)
            let sComment = icd_uper.EmmitChoiceIndexComment stgFileName optChild
            let indexSize = (GetNumberOfBitsForNonNegativeInteger(BigInteger(Seq.length children))).ToString()
            icd_uper.EmmitChoiceChild stgFileName (icd_uper.OddRow stgFileName ()) (BigInteger 1) "ChoiceIndex" sComment    "unsigned int" "N.A." indexSize indexSize

        let sTasName = tas.Name.Value
        let sMaxBits, sMaxBytes = uperSizeInBitsAsInt uperGetMaxSizeInBits t.Kind t.Constraints r
        let sMinBits, sMinBytes = uperSizeInBitsAsInt uperGetMinSizeInBits t.Kind t.Constraints r
        let sMaxBitsExplained = ""
        let sCommentLine = GetCommentLine tas.Comments t

        let arChildren = children |> Seq.mapi(fun i ch -> EmitChild (2 + i) ch) |> Seq.toList
        let arRows = ChIndex::arChildren

        icd_uper.EmitChoice stgFileName color sTasName (ToC sTasName) sMinBytes sMaxBytes sMaxBitsExplained sCommentLine arRows (sCommentLine.Split [|'\n'|])

    | OctetString
    | NumericString
    | IA5String
    | BitString
    | SequenceOf(_)  ->
        let getCharSize () =
            let charSet = GetTypeUperRangeFrom(t.Kind, t.Constraints, r)
            let charSize = GetNumberOfBitsForNonNegativeInteger (BigInteger (charSet.Length-1))
            charSize.ToString()
        let ChildRow (lineFrom:BigInteger) (i:BigInteger) =
            let sClass = if i % 2I = 0I then (icd_uper.EvenRow stgFileName ()) else (icd_uper.OddRow stgFileName ())
            let nIndex = lineFrom + i
            let sFieldName = icd_uper.ItemNumber stgFileName i
            let sComment = ""
            let sType, sAsn1Constraints, sMinBits, sMaxBits = 
                match t.Kind with
                | SequenceOf(child) ->
                    let ret = child.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin "" 
                    let ret = ( if ret.Trim() ="" then "N.A." else ret)
                    let sMaxBits, _ = uperSizeInBitsAsInt uperGetMaxSizeInBits child.Kind child.Constraints r
                    let sMinBits, _ = uperSizeInBitsAsInt uperGetMinSizeInBits child.Kind child.Constraints r
                    let sMaxBitsExplained =  GetWhyExplanation stgFileName child r
                    match child.Kind with
                    | ReferenceType(md,ts,_)   -> icd_uper.EmmitSeqChild_RefType stgFileName ts.Value (ToC ts.Value), ret, sMinBits, (sMaxBits+sMaxBitsExplained)
                    | _                        -> Kind2Name stgFileName child, ret, sMinBits, (sMaxBits+sMaxBitsExplained)
                | IA5String                    -> "ASCII CHARACTER", "", getCharSize (), getCharSize ()
                | NumericString                -> "NUMERIC CHARACTER", "", getCharSize (), getCharSize ()
                | OctetString                  -> "OCTET", "", "8", "8"
                | BitString                    -> "BIT", "", "1","1"
                | _                            -> raise(BugErrorException "")
            icd_uper.EmmitChoiceChild stgFileName sClass nIndex sFieldName sComment  sType sAsn1Constraints sMinBits sMaxBits

        let LengthRow =
            let nMin, nLengthSize = 
                match (GetTypeUperRange t.Kind t.Constraints  r) with
                | Concrete(a,b)  when a=b       -> 0I, 0I
                | Concrete(a,b)                 -> (GetNumberOfBitsForNonNegativeInteger(b-a)), (GetNumberOfBitsForNonNegativeInteger(b-a))
                | NegInf(_)                     -> raise(BugErrorException "")
                | PosInf(b)                     ->  8I, 16I
                | Empty                         -> raise(BugErrorException "")
                | Full                          -> 8I, 16I
            let comment = "Special field used by PER to indicate the number of items present in the array."
            let ret = t.Constraints |> Seq.map PrintAsn1.PrintConstraint |> Seq.StrJoin "" 
            let sCon = ( if ret.Trim() ="" then "N.A." else ret)

            icd_uper.EmmitChoiceChild stgFileName (icd_uper.OddRow stgFileName ()) (BigInteger 1) "Length" comment    "unsigned int" sCon (nMin.ToString()) (nLengthSize.ToString())

        let sTasName = tas.Name.Value
        let sMaxBits, sMaxBytes = uperSizeInBitsAsInt uperGetMaxSizeInBits t.Kind t.Constraints r
        let sMinBits, sMinBytes = uperSizeInBitsAsInt uperGetMinSizeInBits t.Kind t.Constraints r
        let sMaxBitsExplained = ""

        let sFixedLengthComment (nMax: BigInteger) =
            sprintf "Length is fixed to %A elements (no length determinant is needed)." nMax

        let arRows, sExtraComment = 
            match (GetTypeUperRange t.Kind t.Constraints  r) with
            | Concrete(a,b)  when a=b && b<2I     -> [ChildRow 0I 1I], "The array contains a single element."
            | Concrete(a,b)  when a=b && b=2I     -> (ChildRow 0I 1I)::(ChildRow 0I 2I)::[], (sFixedLengthComment b)
            | Concrete(a,b)  when a=b && b>2I     -> (ChildRow 0I 1I)::(icd_uper.EmitRowWith3Dots stgFileName ())::(ChildRow 0I b)::[], (sFixedLengthComment b)
            | Concrete(a,b)  when a<>b && b<2I    -> LengthRow::(ChildRow 1I 1I)::[],""
            | Concrete(a,b)                       -> LengthRow::(ChildRow 1I 1I)::(icd_uper.EmitRowWith3Dots stgFileName ())::(ChildRow 1I b)::[], ""
            | PosInf(_)
            | Full                                -> LengthRow::(ChildRow 1I 1I)::(icd_uper.EmitRowWith3Dots stgFileName ())::(ChildRow 1I 65535I)::[], ""
            | NegInf(_)                           -> raise(BugErrorException "")
            | Empty                               -> [], ""

        let sCommentLine = match GetCommentLine tas.Comments t with
                           | null | ""  -> sExtraComment
                           | _          -> sprintf "%s%s%s" (GetCommentLine tas.Comments t) (icd_uper.NewLine stgFileName ()) sExtraComment


        icd_uper.EmitSizeable stgFileName color sTasName  (ToC sTasName) (Kind2Name stgFileName t) sMinBytes sMaxBytes sMaxBitsExplained sCommentLine arRows (sCommentLine.Split [|'\n'|])


let PrintTas (stgFileName:string) (m:Ast.Asn1Module) (tas:Ast.TypeAssignment) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) blueTasses =
    let tasColor =
        match blueTasses |> Seq.exists (fun x -> x = tas.Name.Value) with
        |true   -> icd_uper.Blue stgFileName ()
        |false  -> icd_uper.Orange stgFileName ()
    icd_uper.EmmitTass stgFileName (printType stgFileName m tas tas.Type r acn tasColor) 


let getModuleBlueTasses (m:Asn1Module) =
        m.TypeAssignments |> 
        Seq.collect(fun x -> Ast.GetMySelfAndChildren x.Type) |>
        Seq.choose(fun x -> match x.Kind with ReferenceType(md,nm,true) -> Some (md.Value,nm.Value) |_ -> None) |> Seq.toList


let PrintModule (stgFileName:string) (m:Asn1Module) (f:Asn1File) (r:AstRoot) (acn:AcnTypes.AcnAstResolved)  =
    let blueTasses = getModuleBlueTasses m |> Seq.map snd
    let sortedTas = spark_spec.SortTypeAssignments m r acn |> List.rev
    let tases = sortedTas  |> Seq.map (fun x -> PrintTas stgFileName m x r acn blueTasses) 
    let comments = []
    icd_uper.EmmitModule stgFileName m.Name.Value comments tases

let PrintFile1 (stgFileName:string) (f:Asn1File)  (r:AstRoot) (acn:AcnTypes.AcnAstResolved)  =
    let modules = f.Modules |> Seq.map (fun  m -> PrintModule stgFileName m f r acn )  
    icd_uper.EmmitFile stgFileName (Path.GetFileName f.FileName) modules 

let PrintFile2 (stgFileName:string) (f:Asn1File) = 
    let tasNames = f.Modules |> Seq.collect(fun x -> x.TypeAssignments) |> Seq.map(fun x -> x.Name.Value) |> Seq.toArray
    let blueTasses = f.Modules |> Seq.collect(fun m -> getModuleBlueTasses m)
    let blueTassesWithLoc = 
              f.TypeAssignments |> 
              Seq.map(fun x -> x.Type) |> 
              Seq.collect(fun x -> Ast.GetMySelfAndChildren x) |>
              Seq.choose(fun x -> match x.Kind with
                                  |ReferenceType(md,ts,true)    -> 
                                    let tas = f.TypeAssignments |> Seq.find(fun y -> y.Name.Value = ts.Value)
                                    Some(ts.Value, tas.Type.Location.srcLine, tas.Type.Location.charPos)
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

let DoWork (stgFileName:string) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) outFileName =
    let files1 = r.Files |> Seq.map (fun f -> PrintFile1 stgFileName f r acn) 
    let files2 = r.Files |> Seq.map (PrintFile2 stgFileName)
    let allTypes = r.Files |> List.collect(fun x -> x.TypeAssignments) |> List.map(fun x -> x.Type) |> Seq.collect(fun x -> Ast.GetMySelfAndChildren x )
    let bIntegerSizeMustBeExplained = allTypes |> Seq.exists(fun x -> match x.Kind with 
                                                                      | Integer -> match GetTypeUperRange x.Kind x.Constraints r with 
                                                                                   | Full | PosInf(_) |  NegInf(_)  -> true 
                                                                                   |_                               ->false 
                                                                      | _ -> false)
    let bRealSizeMustBeExplained = allTypes |> Seq.exists(fun x -> match x.Kind with Real ->true | _ -> false)
    let bLengthSizeMustBeExplained = false
    let bWithComponentMustBeExplained = false
    let bZeroBitsMustBeExplained = 
        allTypes |> 
        Seq.exists(fun x -> 
            match x.Kind with 
            | Integer 
            | ReferenceType(_) -> 
                match uperGetMaxSizeInBits x.Kind x.Constraints r with Bounded(a) when a = 0I -> true |_->false 
            | _ -> false)
    let content = icd_uper.RootHtml stgFileName files1 files2 bIntegerSizeMustBeExplained bRealSizeMustBeExplained bLengthSizeMustBeExplained bWithComponentMustBeExplained bZeroBitsMustBeExplained
    File.WriteAllText(outFileName, content.Replace("\r",""))

