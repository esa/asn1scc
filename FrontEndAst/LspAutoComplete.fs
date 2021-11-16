module LspAutoComplete
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



let rec getAcnTypePathByITree (nodes:ITree list)  =
    let handleTypeEncoding (typeEnc : ITree) =
        if (typeEnc.Type <> acnParser.TYPE_ENCODING) then
            raise (BugErrorException "handleTypeEncoding")
        let tasName = typeEnc.GetChildByType(acnParser.UID).Text
        let modDef = typeEnc.Parent
        let modName = modDef.GetChildByType(acnParser.UID).Text
        modName::[tasName]

    let handleChild (child: ITree) =
        if (child.Type <> acnParser.CHILD) then
            raise (BugErrorException "handleChild")
        match child.Children |> Seq.tryFind (fun z -> z.Type = acnParser.LID) with
        | Some lid -> [lid.Text]
        | None     -> ["#"] // Sequence Of case

    nodes |> 
    List.rev |> //node are in reverse order, so reverse them
    List.choose(fun z ->
        match z.Type with
        | acnParser.TYPE_ENCODING  -> Some (handleTypeEncoding z)
        | acnParser.CHILD          -> Some (handleChild z)
        | _                        -> None) |>
    List.collect id

    
type Asn1TypeDef = Asn1TypeDef of ITree

let getAsn1Type (Asn1TypeDef typeDef:Asn1TypeDef) = 
    let children = getTreeChildren(typeDef)
    let typeNodes = children |> List.filter(fun x -> (not (CreateAsn1AstFromAntlrTree.ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
    let typeNode = List.head(typeNodes)
    typeNode


let rec getAsn1TypeDefinitionByPath (asn1Trees:AntlrParserResult list) (path:string list)  =
    let getModuleByName modName =
        asn1Trees |> Seq.collect (fun r -> r.rootItem.Children) |> Seq.tryFind(fun m -> m.GetChild(0).Text = modName)
    let getTasByName mdTree tasName =
        getChildrenByType(mdTree, asn1Parser.TYPE_ASSIG) |> Seq.tryFind (fun ts -> ts.GetChild(0).Text = tasName)
    let rec resolveReferenceType (typeDef:Asn1TypeDef) : Asn1TypeDef option =
        let typeNode = getAsn1Type typeDef
        match typeNode.Type with
        |asn1Parser.REFERENCED_TYPE ->
            let refType = 
                match getTreeChildren(typeNode) |> List.filter(fun x -> x.Type<> asn1Parser.ACTUAL_PARAM_LIST) with
                | refTypeName::[]           ->  
                    let mdTree = typeNode.GetAncestor(asn1Parser.MODULE_DEF)
                    let mdName = mdTree.GetChild(0).Text
                    let imports = mdTree.GetChildrenByType(asn1Parser.IMPORTS_FROM_MODULE)
                    let importedFromModule = imports |> List.tryFind(fun imp-> imp.GetChildrenByType(asn1Parser.UID) |> Seq.exists(fun impTypeName -> impTypeName.Text = refTypeName.Text ))
                    match importedFromModule with
                    |Some(imp)  -> Some ( imp.GetChild(0).Text,  refTypeName.Text)
                    |None       -> Some ( typeNode.GetAncestor(asn1Parser.MODULE_DEF).GetChild(0).Text,  refTypeName.Text)
                | modName::refTypeName::[]  ->  Some ( modName.Text, refTypeName.Text)
                | _   -> None
            match refType with
            | None  -> None
            | Some (x1, x2) -> 
                match getModuleByName x1 with
                | None      -> None
                | Some md   -> 
                    let tas = getTasByName md x2
                    match tas with
                    | None  -> None
                    | Some ts ->
                        let typeDef = ts.GetChild 1

                        resolveReferenceType (Asn1TypeDef typeDef)
        | _     -> Some typeDef

    let rec getTypeByPath (Asn1TypeDef typeDef:Asn1TypeDef) (path:string list) =
        if (typeDef.Type <> asn1Parser.TYPE_DEF) then
            raise (BugErrorException "No a type")

        let children = getTreeChildren(typeDef)
        let typeNodes = children |> List.filter(fun x -> (not (CreateAsn1AstFromAntlrTree.ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
        let typeNode = List.head(typeNodes)

        match path with
        | []        -> 
            resolveReferenceType (Asn1TypeDef typeDef)
        | x1::xs    ->
            match typeNode.Type with
            | asn1Parser.CHOICE_TYPE        -> 
                let childItems = getChildrenByType(typeNode, asn1Parser.CHOICE_ITEM)
                match childItems |> Seq.tryFind(fun ch -> x1 = ch.GetChild(0).Text) with
                | Some ch -> 
                    let typeDef = getChildByType(ch, asn1Parser.TYPE_DEF)
                    getTypeByPath (Asn1TypeDef typeDef) xs
                | None    -> None
            | asn1Parser.SEQUENCE_TYPE        -> 
                let childItems =
                    match getOptionChildByType(typeNode, asn1Parser.SEQUENCE_BODY) with
                    | Some(sequenceBody)    -> getChildrenByType(sequenceBody, asn1Parser.SEQUENCE_ITEM)
                    | None                  -> []
                match childItems |> Seq.tryFind(fun ch -> x1 = ch.GetChild(0).Text) with
                | Some ch -> 
                    let typeDef = getChildByType(ch, asn1Parser.TYPE_DEF)
                    getTypeByPath (Asn1TypeDef typeDef) xs
                | None    -> None
            | asn1Parser.SEQUENCE_OF_TYPE   when x1="#"     -> 
                let typeDef = getChildByType(typeNode, asn1Parser.TYPE_DEF)
                getTypeByPath (Asn1TypeDef typeDef) xs
            |asn1Parser.REFERENCED_TYPE ->
                let actType = resolveReferenceType (Asn1TypeDef typeDef)
                match actType with
                | Some actType  -> getTypeByPath actType path
                | None          -> None
            | _         -> None

    match path with
    | []
    | _::[] -> None
    | x1::x2::xs    ->
        match getModuleByName x1 with
        | None      -> None
        | Some md   -> 
            let tas = getTasByName md x2
            match tas with
            | None  -> None
            | Some ts -> 
                getTypeByPath (Asn1TypeDef (ts.GetChild 1)) xs
                


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

    //let t, e = tryParceEncodingSpec propText
    //match e with
    //| []    -> []
    //| _     -> 
    let propText, char0 =
        match propText.StartsWith "[" with
        | false  -> propText.Replace("]", ""), char0
        | true   -> propText.Replace("]", "").Replace("[",""), char0 - 1
    let char0 = if propText.Length = char0 && char0 > 0 then char0 - 1 else char0
    //let t, e = tryParcePropertyList propText
    //match e with
    //| []    -> []
    //| _     ->
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


let autoCompleteAcnProperties (fn:LspFile) (lspPos:LspPos) (typeKind:int) (antlrNodes: ITree list) =

    match antlrNodes with
    | []    -> None
    | x1::_ ->
        //exportFile fn.tokens (antlrNodes |> List.rev).Head """C:\prj\GitHub\asn1scc\lsp\workdir\2\antlr.xml"""

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
            Some (tryFindErrPartInAncProperties typeKind esText (lspPos.charPos - (es.getRange fn.tokens).start.charPos) )
        | _  -> None


let lspAutoComplete (ws:LspWorkSpace) filename (line0:int) (charPos:int) =
    let asn1Keywords = ["INTEGER"; "REAL"; "ENUMERATED"; "CHOICE"; "SEQUENCE"; "SEQUENCE OF"; "OCTET STRING"; "BIT STRING"; "IA5String"]
    let tasList = ws.files |> List.collect(fun x -> x.tasList) |> List.map(fun x -> x.name)

    (*The following must be rewritten to utilize the getTreeNodesByPosition.
      The followin function requires an astRoot which will not be the case*)

    let lspAutoCompleteAcn () = 
        match ws.files |> Seq.tryFind(fun fn -> fn.fileName = filename) with
        | None  -> None
        | Some fn ->
            let lspPos = {LspPos.line=line0+1; charPos=charPos}
            
            let antlrNodes = getTreeNodesByPosition fn lspPos 
            let acnTypePath= getAcnTypePathByITree antlrNodes

            let asn1TypeDefinition = getAsn1TypeDefinitionByPath  (ws.files |> List.filter(fun f -> f.lspFileType = LspFileType.LspAsn1) |> List.map(fun f -> f.antlrResult) ) acnTypePath
            match asn1TypeDefinition with
            | None  -> None
            | Some asn1TpDef ->
                let asn1Type = getAsn1Type asn1TpDef
                //getProposeAcnProperties ws fn lspPos pathFromChildToRoot
                autoCompleteAcnProperties fn lspPos asn1Type.Type antlrNodes


    match tryGetFileType filename with
    | Some LspAsn1  -> 
        tasList@asn1Keywords
    | Some LspAcn   -> 
        let acnProps =  lspAutoCompleteAcn ()
        match acnProps with
        | None    -> tasList
        | Some acnProps    -> acnProps
    | None          -> []


