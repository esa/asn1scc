module Lsp

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

type LspFileType =
    | LspAsn1
    | LspAcn








let rec getAcnPathByITree (n:ITree) curResult =
    let encSpec =
        if    n.Type = acnParser.ENCODING_PROPERTIES then Some (n.Parent)
        elif  n.Type = acnParser.ENCODING_SPEC then Some n
        else None

    match encSpec with
    | None  -> curResult
    | Some ens ->
        if ens.Parent.Type = acnParser.TYPE_ENCODING then  
            let tasName = ens.Parent.GetChildByType(acnParser.UID).Text
            let modDef = ens.Parent.Parent
            let modName = modDef.GetChildByType(acnParser.UID).Text
            modName::tasName::curResult
        elif ens.Parent.Type = acnParser.CHILD then  
            let name  = 
                match (ens.Parent).GetOptChild(acnParser.LID) with
                | None          -> "#"
                | Some(lid)     -> lid.Text
            let result = name::curResult
            getAcnPathByITree ens.Parent.Parent result
        else    
            curResult

type Asn1TypeDef = Asn1TypeDef of ITree

let getAsn1Type (Asn1TypeDef typeDef:Asn1TypeDef) = 
    let children = getTreeChildren(typeDef)
    let typeNodes = children |> List.filter(fun x -> (not (CreateAsn1AstFromAntlrTree.ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
    let typeNode = List.head(typeNodes)
    typeNode

let rec getAsn1TypeByPath (asn1Trees:AntlrParserResult list) (path:string list)  =
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
                



let lspParseAsn1File (fileName:string) (fileContent:string) =
    let stm = new MemoryStream(System.Text.Encoding.UTF8.GetBytes(fileContent))
    let asn1ParseTree, antlerErrors = antlrParse asn1Lexer asn1treeParser (fileName, [{Input.name=fileName; contents=stm}])
    let typeAssI = asn1ParseTree.rootItem.AllChildren |> List.filter(fun z -> z.Type = asn1Parser.TYPE_ASSIG) |> List.map(fun z -> (z.GetChild(0))) 
    let typeAss = typeAssI |> List.map(fun x -> x.Text) |> Seq.toArray
    let tokens = asn1ParseTree.tokens
    let parseErrors = 
        antlerErrors |> List.map(fun ae -> {LspError.line0 = ae.line - 1; charPosInline = ae.charPosInline; msg = ae.msg})
    let tasList = 
        typeAssI |> 
        List.map(fun x -> 
            let tk = tokens.[x.TokenStartIndex]
            {LspTypeAssignment.name = x.Text; line0 = tk.Line - 1; charPos = tk.CharPositionInLine}) 
    {LspFile.fileName = fileName; content = fileContent; tokens=tokens; antlrResult=asn1ParseTree; parseErrors = parseErrors; semanticErrors = []; tasList=tasList}


let debuggetAsn1TypeByPath (a_path:string array)  =
    let fileName = @"C:\prj\GitHub\asn1scc\v4Tests\test-cases\acn\14-RealCases\NPAL.asn1"
    let path = a_path |> Seq.toList
    let a = lspParseAsn1File fileName (System.IO.File.ReadAllText fileName)
    let b = getAsn1TypeByPath [a.antlrResult] path
    match b with
    | None  -> printfn "%s not found!" (Seq.StrJoin "." path)
    | Some typedef -> 
        let t= getAsn1Type typedef
        printfn "It is %s, children" (asn1Parser.tokenNames.[t.Type]) 

let printAntlrNode (tokens : IToken array) (r:ITree) =
    let s = tokens.[r.TokenStartIndex]
    let e = tokens.[r.TokenStopIndex]
    sprintf "type =%d, text=%s, start(%d,%d) end (%d,%d) " r.Type r.Text s.Line s.CharPositionInLine e.Line (e.CharPositionInLine + e.Text.Length)


let rec printAntlrTree (tokens : IToken array) (r:ITree) n =
    printf "%s" (String(' ', 2*n))
    printfn "%s toString : %s"  (printAntlrNode tokens r) (r.ToString())
    r.Children |> Seq.iter(fun ch -> printAntlrTree tokens ch (n+1) )



type LspPos = {
    line   : int
    charPos : int
}

type LspRange = {
    start  : LspPos
    end_   : LspPos
}

let getTreeRange (tokens : IToken array) (r:ITree)  =
    let s = tokens.[r.TokenStartIndex]
    let e = tokens.[r.TokenStopIndex]
    {LspRange.start = {LspPos.line = s.Line; charPos=s.CharPositionInLine}; end_ = {LspPos.line = e.Line; charPos=e.CharPositionInLine + e.Text.Length}}

let isInside (range : LspRange) (pos: LspPos) =
    if range.start.line < pos.line && pos.line < range.end_.line then true
    elif range.start.line < pos.line && pos.line = range.end_.line then pos.charPos <= range.end_.charPos
    elif range.start.line <= pos.line && pos.line < range.end_.line then range.start.charPos <= pos.charPos
    elif range.start.line = pos.line && pos.line = range.end_.line then range.start.charPos <= pos.charPos && pos.charPos <= range.end_.charPos
    else false
    

let rec getTreeNodesByPosition (tokens : IToken array) (pos:LspPos) (r:ITree)  =
    seq {
        let range = getTreeRange tokens  r
        match isInside range pos with
        | false -> ()
        | true  -> 
            yield r
            for c in r.Children do  
                yield! getTreeNodesByPosition tokens pos c
    } |> Seq.toList


let getProposeAcnProperties (pathToRoot:ITree list) =
    match pathToRoot with
    | []        -> []
    | x1::xs    ->
        match x1::xs |> Seq.tryFind (fun z -> z.Type = acnParser.ENCODING_PROPERTIES) with
        | None  -> []
        | Some ep   ->
            let existingProperties = ep.Children |> List.map(fun z -> z.Type)
            //get Type, let's suppose it INTEGER
            let encodingProps = 
                [
                    (acnParser.ENCODING, ["encoding pos-int"; "encoding twos-complement"; "encoding BCD"; "encoding ASCII"])
                    (acnParser.SIZE, ["size"])
                    (acnParser.ENDIANNES, ["endianness big"; "endianness little"])
                    (acnParser.MAPPING_FUNCTION, ["mapping-function myFunction"])
                ]
            let encodingPropCons = encodingProps |> List.map fst


            match encodingPropCons |> Seq.tryFind(fun a -> a = x1.Type) with
            | Some _   ->
                //Am I within a completed ACN property (i.e. ENCODING, SIZE etc) then there is nothing to suggest to the user
                []
            | None     ->
                let possibleProperties = encodingProps |> List.filter(fun (a,_) -> not (existingProperties |> Seq.contains a) )
                let possibleAnwers = possibleProperties |> List.map snd |> List.collect id
                match x1.Type with
                | acnParser.ENCODING_PROPERTIES -> possibleAnwers
                | 0     (**)                -> 
                    //I am within possibly within an incomplete acn Property
                    possibleAnwers |> List.filter (fun a -> a.StartsWith (x1.Text)) 
                | _                     -> []

        


let lspParseAcnFile (fileName:string) (fileContent:string) =
    let stm = new MemoryStream(System.Text.Encoding.UTF8.GetBytes(fileContent))
    let acnParseTree, antlerErrors = antlrParse acnLexer acnTreeParser (fileName, [{Input.name=fileName; contents=stm}])
    let typeAssI = acnParseTree.rootItem.AllChildren |> List.filter(fun z -> z.Type = acnParser.TYPE_ENCODING) |> List.map(fun z -> (z.GetChild(0)).Text, z ) 

    //typeAssI |> Seq.iter(fun (nm, tr) -> printfn"%s --> %s" nm (tr.ToStringTree()) ) 
    //printfn"----"
    //typeAssI |> Seq.iter(fun (nm, tr) -> printAntlrTree acnParseTree.tokens tr 0) 
    //printfn"---- end ----"


    let test = getTreeNodesByPosition acnParseTree.tokens {LspPos.line=5; charPos=13} acnParseTree.rootItem 
    printfn"---- whereami start ----"
    test |> Seq.iter(fun z -> printfn "%s" (printAntlrNode acnParseTree.tokens z))
    printfn"---- whereami end ----"

    let tokens = acnParseTree.tokens
    let parseErrors = 
        antlerErrors |> List.map(fun ae -> {LspError.line0 = ae.line - 1; charPosInline = ae.charPosInline; msg = ae.msg})
    
    {LspFile.fileName = fileName; content = fileContent; tokens=tokens; antlrResult=acnParseTree; parseErrors = parseErrors; semanticErrors = []; tasList=[]}


let isAsn1File (fn:string) = fn.ToLower().EndsWith("asn1") || fn.ToLower().EndsWith("asn")
let isAcnFile  (fn:string) = fn.ToLower().EndsWith("acn") 


let tryGetFileType filename = 
    match isAsn1File filename with
    | true -> Some LspAsn1
    | false ->
        match isAcnFile filename with
        | true  -> Some LspAcn
        | false -> None

let lspParceFile (fileName:string) (fileContent:string) =
    if (isAsn1File fileName) then
        (Some (lspParseAsn1File fileName fileContent))
    elif (isAcnFile fileName)  then
        (Some (lspParseAcnFile fileName fileContent))
    else
        None 

let lspPerformSemanticAnalysis (ws:LspWorkSpace) =
    let asn1ParseTrees, asn1ParseErrors = 
        ws.files |> List.filter(fun z -> isAsn1File z.fileName) |> List.map(fun z -> z.antlrResult, z.parseErrors) |> Seq.toList |> List.unzip

    let acnParseTrees, acnParseErrors = 
        ws.files |> List.filter(fun z -> isAcnFile z.fileName) |> List.map(fun z -> z.antlrResult, z.parseErrors) |> Seq.toList |> List.unzip
    let args = defaultCommandLineSettings
    match asn1ParseErrors@acnParseErrors |> List.collect id with
    | []     -> 
        try
            let acnAst = AcnGenericCreateFromAntlr.CreateAcnAst args.integerSizeInBytes acnParseTrees
            //let acnAst = AcnGenericCreateFromAntlr.CreateAcnAst_no_exc  args.integerSizeInBytes acnParseTrees
            let parameterized_ast = CreateAsn1AstFromAntlrTree.CreateAstRoot asn1ParseTrees acnAst args
            let asn1Ast0 = MapParamAstToNonParamAst.DoWork parameterized_ast
            CheckAsn1.CheckFiles asn1Ast0 0
            let uniqueEnumNamesAst = EnsureUniqueEnumNames.DoWork asn1Ast0 
            let acnAst,acn0 = AcnCreateFromAntlr.mergeAsn1WithAcnAst uniqueEnumNamesAst (acnAst, acnParseTrees) 
            let acnDeps = CheckLongReferences.checkAst acnAst
            {ws with astRoot = Some (acnAst)}
        with
        | SemanticError (loc,msg)            ->
            let se = {LspError.line0 = loc.srcLine - 1; charPosInline = loc.charPos; msg = msg}          
            let newFiles =
                ws.files |> 
                List.map(fun z -> 
                    match z.fileName = loc.srcFilename with
                    | true  -> {z with parseErrors = se::z.parseErrors}
                    | false -> z)
            {ws with files = newFiles }
    | xx1::xs ->  
        ws


let lspGoToDefinition (ws:LspWorkSpace) filename line0 charPos =
    match ws.files |> Seq.tryFind (fun f -> f.fileName = filename) with
    | None      -> []
    | Some f    ->
        match f.tokens |> Seq.tryFind(fun a -> a.Line = line0 + 1 && a.CharPositionInLine <= charPos && charPos <= a.CharPositionInLine + a.Text.Length) with
        | None -> []
        | Some t -> 
            ws.files |> 
            List.choose(fun f -> 
                match f.tasList |> Seq.tryFind(fun ts -> ts.name = t.Text) with
                | Some ts -> Some(f.fileName, ts)
                | None    -> None)
        

let quickParseAcnEncSpecTree (subTree :ITree) =
    //match subTree.
    []

let lspAutoComplete (ws:LspWorkSpace) filename (line0:int) (charPos:int) =
    let asn1Keywords = ["INTEGER"; "REAL"; "ENUMERATED"; "CHOICE"; "SEQUENCE"; "SEQUENCE OF"; "OCTET STRING"; "BIT STRING"; "IA5String"]
    let tasList = ws.files |> List.collect(fun x -> x.tasList) |> List.map(fun x -> x.name)

    let getAcnproperties (dummy:int) =
        match ws.astRoot with
        | None -> []
        | Some a ->
            let tasList0 = 
                a.Files |> 
                List.collect(fun z -> z.Modules) |> 
                List.collect(fun z -> z.TypeAssignments) 

            tasList0 |>
            List.choose(fun tas -> tas.Type.acnEncSpecAntlrSubTree) |>
            List.map(fun t -> t.ToStringTree()) |>
            Seq.iter(fun s -> printfn "%s" s)

            let tasList = 
                tasList0 |>
                List.choose(fun tas ->
                    let  uiLoc = {SrcLoc.srcFilename=""; srcLine = line0+1;charPos=charPos}
                    match tas.Type.acnEncSpecPostion with
                    | None -> None
                    | Some (l1,l2) when FsUtils.srcLocContaints l1 l2 uiLoc -> Some tas 
                    | Some (_,_)    -> None)
            match tasList with
            | []    -> []
            | x1::_ -> 
                match x1.Type.Kind with
                | Asn1AcnAst.Asn1TypeKind.Integer o -> 
                    ["encoding pos-int"; "size"; "align-to-next"]
                | _ -> []
            

    match tryGetFileType filename with
    | Some LspAsn1  -> 
        tasList@asn1Keywords
    | Some LspAcn   -> 
        tasList
    (*
        match getAcnproperties 0 with
        | []    ->        tasList
        | xs    -> xs
        *)
    | None          -> []

let lspOnFileOpened (ws:LspWorkSpace) (filename:string) (filecontent:string) =
    let parseAnalysis = 
        match ws.files |> Seq.tryFind(fun z -> z.fileName = filename) with
        | Some f -> ws //nothing todo. File is already parsed in the WS
        | None   -> 
            //new File. It must be opened and also open any silbing ASN.1 or ACN files
            let dir = Path.GetDirectoryName(filename)
            let files = Directory.GetFiles(dir)
            let filesToOpen = 
                files |> 
                Seq.filter(fun f -> 
                    match ws.files |> Seq.tryFind (fun wf -> wf.fileName = f) with
                    | Some _ -> false //file already open. No need to open it again
                    | None   -> true  //file is not open in the WorkSpace. We need to open it
                ) |>
                Seq.map(fun f -> 
                    match f = filename with
                    | true  -> f, filecontent
                    | false -> f, File.ReadAllText f) |> 
                Seq.choose (fun (fn, fc) -> lspParceFile fn fc) |>
                Seq.toList

            {ws with files = ws.files@filesToOpen}
    lspPerformSemanticAnalysis parseAnalysis

let lspOnFileChanged (ws:LspWorkSpace) (filename:string) (filecontent:string) =
    let restFiles =
        ws.files |> List.filter (fun z -> z.fileName <> filename) 
    let newFiles =
        [(filename, filecontent)] |> List.choose (fun (fn, fc) -> lspParceFile fn fc)
    let parseAnalysis = 
        {ws with files = restFiles @ newFiles}
    lspPerformSemanticAnalysis parseAnalysis

let lspEmptyWs = {LspWorkSpace.files = []; astRoot = None}








