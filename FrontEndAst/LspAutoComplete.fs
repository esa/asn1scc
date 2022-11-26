module LspAutoComplete
#nowarn "3536"

open Antlr.Runtime
open Antlr.Runtime.Tree
open System.IO
open Antlr.Asn1
open Antlr.Acn
open System.Collections.Generic
open System.Linq
open CommonTypes
open FsUtils
open Antlr
open System

open System.IO

open System
open System.Numerics
open FsUtils
open Antlr.Asn1
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open CommonTypes
open AcnGenericTypes
open AntlrParse
open LspAst
open FsUtils




                
let getAsn1TypeChildren (asn1Type: ITree) =
    match asn1Type.Type with
    | asn1Parser.SEQUENCE_TYPE        -> 
        match getOptionChildByType(asn1Type, asn1Parser.SEQUENCE_BODY) with
        | Some(sequenceBody)    -> 
            getChildrenByType(sequenceBody, asn1Parser.SEQUENCE_ITEM) |> 
            List.map(fun x ->
                let lid = getChildByType(x, asn1Parser.LID)
                let typeDef = getChildByType(x, asn1Parser.TYPE_DEF)
                (lid.Text, Asn1TypeDef typeDef)
            )
        | None                  -> []
    | asn1Parser.CHOICE_TYPE        -> 
        getChildrenByType(asn1Type, asn1Parser.CHOICE_ITEM) |> 
        List.map(fun x ->
            let lid = getChildByType(x, asn1Parser.LID)
            let typeDef = getChildByType(x, asn1Parser.TYPE_DEF)
            (lid.Text, Asn1TypeDef typeDef)
        )
    | _     -> []




let tryFindErrPartInAncProperties (typeKind:int) (propText : string) (char0:int) = 
    let tryParceEncodingSpec (src : string) =
        let parser ts = acnTreeGeneric (fun p -> p.encodingSpec().Tree :?> ITree) ts
        let stm = new MemoryStream(System.Text.Encoding.UTF8.GetBytes(src))
        antlrParse acnLexer parser ("dummy", [{Input.name="dummy"; contents=stm}])

    let tryParcePropertyList (src : string) =
        let parser ts = acnTreeGeneric (fun p -> p.propertyList().Tree :?> ITree) ts
        let stm = new MemoryStream(System.Text.Encoding.UTF8.GetBytes(src))
        antlrParse acnLexer parser ("dummy", [{Input.name="dummy"; contents=stm}])
    
    let tryParceProperty (src : string) =
        let parser ts = acnTreeGeneric (fun p -> p.property().Tree :?> ITree) ts
        let stm = new MemoryStream(System.Text.Encoding.UTF8.GetBytes(src))
        antlrParse acnLexer parser ("dummy", [{Input.name="dummy"; contents=stm}])

    let allPossibleEncodingProps = getTypeAcnProps typeKind

    let propText, char0 =
        match propText.StartsWith "[" with
        | false  -> propText.Replace("]", ""), char0
        | true   -> propText.Replace("]", "").Replace("[",""), char0 - 1
    let char0 = if propText.Length = char0 && char0 > 0 then char0 - 1 else char0

    let split_string_into_parts (propText:string) =
        propText.Split(',') |>
        Seq.fold(fun (cs, rt) e -> (cs+e.Length+1, rt@[(cs, cs+e.Length - 1 ,e)] ) ) (0,[]) |> snd

    let parts = split_string_into_parts (propText)

    let cursorPart, otherParts = 
        parts |>
        Seq.map(fun (a, b , str) -> 
            match a <= char0 && char0 <= b with
            | true   -> (0, str) 
            | false  -> (1, str)) |>
        Seq.toList |>
        List.split (fun (x,_) -> x=0)
    let existingProperties = 
        otherParts |> 
        List.map snd |> 
        List.map tryParceProperty |>
        List.filter(fun (_,e) -> e.IsEmpty) |>
        List.map (fun (pr,_) -> pr.rootItem.Type)
    let possibleProperties = allPossibleEncodingProps |> List.filter(fun (a,_) -> not (existingProperties |> Seq.contains a) )
    let possibleAnwers = possibleProperties |> List.map snd |> List.collect id
    
    match propText.Trim() = "" with
    | true -> possibleAnwers
    | false ->
        match cursorPart with
        | []           -> []   //return no suggestion
        | (_, str)::_  -> 
            let actStr = str.Trim()
            possibleAnwers |> List.filter (fun a -> a.StartsWith (actStr)) 


let autoCompleteAcnProperties (fn:LspFile) (lspPos:LspPos) (asn1Type:ITree) (antlrNodes: ITree list)  =
    match antlrNodes |> Seq.tryFind (fun n -> n.Type = acnParser.ENCODING_SPEC ) with
    | Some es -> 
        let child_enc_spec = es.Children |> Seq.tryFind(fun c -> c.Type = acnParser.CHILDREN_ENC_SPEC)
        let esText = 
            match child_enc_spec with
            | None  -> es.getCompositeText fn.tokens
            | Some ces ->
                let ret = 

                    [es.TokenStartIndex .. es.TokenStopIndex] |> 
                    List.filter(fun i -> i < ces.TokenStartIndex || i > ces.TokenStopIndex) |>
                    List.map(fun i -> (fn.tokens.[i]).Text) |>
                    Seq.StrJoin "" 
                ret.Trim()
        Some (tryFindErrPartInAncProperties asn1Type.Type esText (lspPos.charPos - (es.getRange fn.tokens).start.charPos) )
    | None  -> None


let autoCompleteChildName (fn:LspFile) (lspPos:LspPos) (asn1Type:ITree) (antlrNodes: ITree list) =
    match antlrNodes |> Seq.tryFind (fun n -> n.Type = acnParser.CHILDREN_ENC_SPEC ) with
    | Some ces -> 
        let acnChildren = ces.GetChildrenByType(acnParser.CHILD) |> List.map(fun c -> (c.GetChild 0).Text) |> Set.ofList
        let asn1ChildNames = getAsn1TypeChildren asn1Type |> List.map fst |> List.filter(fun asc -> not (acnChildren.Contains asc)) |> List.map(fun s -> s + " []")
        match asn1ChildNames with
        | x::_  -> Some ([x])
        | []    -> Some []
    | None     -> None
    

let acnAutoComplete (fn:LspFile) (lspPos:LspPos) (asn1Type:ITree) (antlrNodes: ITree list) =
    match antlrNodes with
    | []    -> None
    | x1::_ ->
        //first, try acn properties. This will work if the curent position is within brackets []
        match antlrNodes |> Seq.tryFind (fun n -> n.Type = acnParser.CHILDREN_ENC_SPEC || n.Type = acnParser.ENCODING_SPEC ) with
        | Some n   ->
            if n.Type = acnParser.ENCODING_SPEC then
                autoCompleteAcnProperties fn lspPos asn1Type antlrNodes 
            elif n.Type = acnParser.CHILDREN_ENC_SPEC then
                autoCompleteChildName fn lspPos asn1Type antlrNodes
            else
                None
        | _    ->  None
            
            


let lspAutoComplete (ws:LspWorkSpace) filename (line0:int) (charPos:int) =
    let asn1Keywords = ["INTEGER"; "REAL"; "ENUMERATED"; "CHOICE"; "SEQUENCE"; "SEQUENCE OF"; "OCTET STRING"; "BIT STRING"; "IA5String"]
    let tasList = ws.files |> List.collect(fun x -> x.tasList) |> List.map(fun x -> x.name)


    let lspAutoCompleteAcn () = 
        match ws.files |> Seq.tryFind(fun fn -> fn.fileName = filename) with
        | None  -> None
        | Some fn ->
            let lspPos = {LspPos.line=line0+1; charPos=charPos}
            
            let antlrNodes = getTreeNodesByPosition fn lspPos 
            let acnTypePath= getAcnTypePathByITree antlrNodes

            let asn1TypeDefinition = getAsn1TypeDefinitionByPath  (ws.files |> List.filter(fun f -> f.lspFileType = LspFileType.LspAsn1) |> List.map(fun f -> f.antlrResult) ) true acnTypePath
            match asn1TypeDefinition with
            | None  -> None
            | Some asn1TpDef ->
                let asn1Type = getAsn1Type asn1TpDef
                //getProposeAcnProperties ws fn lspPos pathFromChildToRoot
                acnAutoComplete fn lspPos asn1Type antlrNodes


    match tryGetFileType filename with
    | Some LspAsn1  -> 
        tasList@asn1Keywords
    | Some LspAcn   -> 
        let acnProps =  lspAutoCompleteAcn ()
        match acnProps with
        | None    -> tasList
        | Some acnProps    -> acnProps
    | None          -> []


