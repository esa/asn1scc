module Lsp

#nowarn "3536"
open Antlr.Runtime
open Antlr.Runtime.Tree
open System.IO
open Antlr.Asn1
open Antlr.Acn
open System.Collections.Generic
open System.Linq
open CommonTypes
open AbstractMacros
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







(*
let rec getAcnPathByITree (n:ITree) curResult =
    
    let handleTypeEncoding (typeEnc : ITree) =
        let tasName = typeEnc.GetChildByType(acnParser.UID).Text
        let modDef = typeEnc.Parent
        let modName = modDef.GetChildByType(acnParser.UID).Text
        modName::tasName::curResult
    let encSpec =
        if    n.Type = acnParser.ENCODING_PROPERTIES then Some (n.Parent)
        elif  n.Type = acnParser.ENCODING_SPEC then Some n
        else None
    match encSpec with
    | None  -> 
        if n.Type = acnParser.TYPE_ENCODING then
            handleTypeEncoding n
        else
            curResult
    | Some ens ->
        if ens.Parent.Type = acnParser.TYPE_ENCODING then  
            handleTypeEncoding ens.Parent
        elif ens.Parent.Type = acnParser.CHILD then  
            let name  = 
                match (ens.Parent).GetOptChild(acnParser.LID) with
                | None          -> "#"
                | Some(lid)     -> lid.Text
            let result = name::curResult
            getAcnPathByITree ens.Parent.Parent result
        else    
            curResult


*)


type FileContent =
    | AsLines of string list
    | AsString of string



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
    {LspFile.fileName = fileName; content = fileContent; tokens=tokens; antlrResult=asn1ParseTree; parseErrors = parseErrors; semanticErrors = []; tasList=tasList; lspFileType = LspFileType.LspAsn1}


(*
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
*)

let printAntlrNode (tokens : IToken array) (r:ITree) =
    let s = tokens.[r.TokenStartIndex]
    let e = tokens.[r.TokenStopIndex]
    sprintf "type =%d, text=%s, start(%d,%d) end (%d,%d) " r.Type r.Text s.Line s.CharPositionInLine e.Line (e.CharPositionInLine + e.Text.Length)


let rec printAntlrTree (tokens : IToken array) (r:ITree) n =
    printf "%s" (String(' ', 2*n))
    printfn "%s toString : %s"  (printAntlrNode tokens r) (r.ToString())
    r.Children |> Seq.iter(fun ch -> printAntlrTree tokens ch (n+1) )








let getCursonString (filedate:string) (lspPos:LspPos) =
    let lines = filedate.Split('\r', '\n')
    let curLine = lines.[lspPos.line - 1]
    printfn "\n%s" curLine
    printfn "'%s'" (curLine.Substring(0, lspPos.charPos-1))
    printfn "'%s'\n" (curLine.Substring(lspPos.charPos-1))






let tc str char0 =
    let ret1 = LspAutoComplete.tryFindErrPartInAncProperties asn1Parser.INTEGER_TYPE str char0
    printfn "%s" str
    printfn "%A" ret1
    printfn "----------"

let debug2 () =
    tc "[enc]" 3
    tc "[encoding pos-int, ]" 18
    tc "[encoding pos-int, s ]" 20
    



    
(*
let getProposeAcnProperties (ws:LspWorkSpace) (f:LspFile) (lspPos:LspPos) (pathToRoot:ITree list) =
    let findFirstEncProp (pathToRoot:ITree list) =
        match pathToRoot with
        | []    -> None
        | x1::_ ->
            if x1.Type = acnParser.ENCODING_PROPERTIES then 
                Some x1
            elif x1.Type = acnParser.ENCODING_SPEC then 
                x1.Children |> Seq.tryFind (fun c -> c.Type = acnParser.ENCODING_PROPERTIES)
            else pathToRoot |> Seq.tryFind (fun z -> z.Type = acnParser.ENCODING_PROPERTIES)

    match pathToRoot with
    | []        -> None
    | x1::xs    ->
        match findFirstEncProp pathToRoot with
        | None  -> 
            match x1.Type = acnParser.TYPE_ENCODING with
            | true ->  
                //TYPE ENCODING
                // DO I HAVE an acnParser.ENCODING_SPEC ?, if yes then return None i.e. the cursor is out of brackets
                // Do I have an error node child? if yes and cursor is inside the error text and error text is inside []
                // then I have to guess ...
                match x1.Children |> Seq.tryFind(fun c -> c.Type = acnParser.ENCODING_SPEC) with
                | Some _  -> None
                | None    ->
                    let getTextJustBeforeCursor (bigText:string) (textRange:LspRange) (lspPos:LspPos) =
                        let curLine = textRange.start.line - lspPos.line
                        let charPos =
                            match textRange.start.line = lspPos.line with
                            | true -> textRange.start.charPos - lspPos.charPos
                            | false -> lspPos.charPos - 1 
                        let lines = bigText.Split('\r', '\n')
                        match curLine >= 0 && curLine < lines.Length with
                        | false -> None
                        | true  ->
                            let curLine = lines.[curLine]
                            match charPos >= 0 && charPos < curLine.Length with
                            | false -> None
                            | true  -> 
                                let subStr = curLine.Substring(0, charPos)
                                let lstIndexOf = subStr.LastIndexOfAny([|' ';'[';|])
                                if lstIndexOf >= 0 then 
                                    Some (subStr.Substring lstIndexOf)
                                else None



                    let errNodes =
                        x1.Children |> 
                        Seq.choose(fun c ->
                            match c with
                            | :? CommonErrorNode as e    -> Some e
                            | _  -> None) 
                    match errNodes |> Seq.tryFind(fun c -> c.Type = 0 && (isInside (c.getRange f.tokens) lspPos) && c.Text.StartsWith("[") && c.Text.EndsWith("]")) with
                    | None -> None
                    | Some c ->
                        let errText = c.Text
                        let textJustBefore = getTextJustBeforeCursor errText (c.getRange f.tokens) lspPos
                        match textJustBefore with
                        | None  -> None
                        | Some str  ->
                            let typePath = getAcnPathByITree x1 []
                            let typeKind = getAsn1TypeByPath  (ws.files |> List.filter(fun f -> f.lspFileType = LspFileType.LspAsn1) |> List.map(fun f -> f.antlrResult) ) typePath
                            let allPossibleEncodingProps = 
                                match typeKind with
                                | Some typeDef   -> 
                                    let asn1Type = getAsn1Type typeDef
                                    getTypeAcnProps asn1Type.Type
                                | None                          -> []

                

                            None
            | false -> None
        | Some ep   ->
            let typePath = getAcnPathByITree ep []
            let typeKind = getAsn1TypeByPath  (ws.files |> List.filter(fun f -> f.lspFileType = LspFileType.LspAsn1) |> List.map(fun f -> f.antlrResult) ) typePath


            let existingProperties = ep.Children |> List.map(fun z -> z.Type)
            let allPossibleEncodingProps = 
                match typeKind with
                | Some typeDef   -> 
                    let asn1Type = getAsn1Type typeDef
                    getTypeAcnProps asn1Type.Type
                | None                          -> []

            let encodingPropCons = allPossibleEncodingProps |> List.map fst


            match encodingPropCons |> Seq.tryFind(fun a -> a = x1.Type) with
            | Some _   ->
                //Am I within a completed ACN property (i.e. ENCODING, SIZE etc) then there is nothing to suggest to the user
                Some []
            | None     ->
                let possibleProperties = allPossibleEncodingProps |> List.filter(fun (a,_) -> not (existingProperties |> Seq.contains a) )
                let possibleAnwers = possibleProperties |> List.map snd |> List.collect id
                match x1.Type with
                | acnParser.ENCODING_PROPERTIES
                | acnParser.ENCODING_SPEC -> Some possibleAnwers
                | 0     (**)                -> 
                    //I am within possibly within an incomplete acn Property
                    Some (possibleAnwers |> List.filter (fun a -> a.StartsWith (x1.Text)) )
                | _                     -> Some []

        

*)


















let lspParseAcnFile (fileName:string) (fileContent:string) =
    let stm = new MemoryStream(System.Text.Encoding.UTF8.GetBytes(fileContent))
    let acnParseTree, antlerErrors = antlrParse acnLexer acnTreeParser (fileName, [{Input.name=fileName; contents=stm}])
    let typeAssI = acnParseTree.rootItem.AllChildren |> List.filter(fun z -> z.Type = acnParser.TYPE_ENCODING) |> List.map(fun z -> (z.GetChild(0)).Text, z ) 

    //typeAssI |> Seq.iter(fun (nm, tr) -> printfn"%s --> %s" nm (tr.ToStringTree()) ) 
    //printfn"----"
    //typeAssI |> Seq.iter(fun (nm, tr) -> printAntlrTree acnParseTree.tokens tr 0) 
    //printfn"---- end ----"

    (*
    let test = getTreeNodesByPosition acnParseTree.tokens {LspPos.line=5; charPos=13} acnParseTree.rootItem 
    printfn"---- whereami start ----"
    test |> Seq.iter(fun z -> printfn "%s" (printAntlrNode acnParseTree.tokens z))
    printfn"---- whereami end ----"
    *)

    let tokens = acnParseTree.tokens
    let parseErrors = 
        antlerErrors |> List.map(fun ae -> {LspError.line0 = ae.line - 1; charPosInline = ae.charPosInline; msg = ae.msg})
    
    {LspFile.fileName = fileName; content = fileContent; tokens=tokens; antlrResult=acnParseTree; parseErrors = parseErrors; semanticErrors = []; tasList=[]; lspFileType = LspFileType.LspAcn}





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
            
            //the following line is commented out because it is not needed for the LSP
            //the LSP is about front end parsing and not about code generation
            let uniqueEnumNamesAst = asn1Ast0  // EnsureUniqueEnumNames.DoWork asn1Ast0 
            let acnAst,acn0 = AcnCreateFromAntlr.mergeAsn1WithAcnAst uniqueEnumNamesAst [] (acnAst, acnParseTrees) 
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
            let ret1 = 
                ws.files |> 
                List.filter(fun f -> isAsn1File f.fileName) |>
                List.choose(fun f -> 
                    match f.tasList |> Seq.tryFind(fun ts -> ts.name = t.Text) with
                    | Some ts -> Some(f.fileName, ts)
                    | None    -> None)
            match ret1 with
            | []    ->
                match isAcnFile f.fileName with
                | true  ->
                    let lspPos = {LspPos.line=line0+1; charPos=charPos}

                    let antlrNodes = getTreeNodesByPosition f lspPos 
                    let acnTypePath= getAcnTypePathByITree antlrNodes

                    let asn1TypeDefinition = getAsn1TypeDefinitionByPath  (ws.files |> List.filter(fun f -> f.lspFileType = LspFileType.LspAsn1) |> List.map(fun f -> f.antlrResult) ) false acnTypePath
                    match asn1TypeDefinition with
                    | None  -> []
                    | Some asn1TpDef ->
                        let asn1Type = getAsn1Type asn1TpDef
                        let fname = asn1Type.TextL.Location.srcFilename
                        [(fname, {LspTypeAssignment.name = asn1Type.Text; line0 = asn1Type.Line - 1; charPos = asn1Type.CharPositionInLine})]

                | false -> []
            | _ -> ret1
        

let quickParseAcnEncSpecTree (subTree :ITree) =
    //match subTree.
    []


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

let lspOnFileSaved (ws:LspWorkSpace) (filename:string)  =
    let filecontent = File.ReadAllText filename
    let restFiles =
        ws.files |> List.filter (fun z -> z.fileName <> filename) 
    let newFiles =
        [(filename, filecontent)] |> List.choose (fun (fn, fc) -> lspParceFile fn fc)
    let parseAnalysis = 
        {ws with files = restFiles @ newFiles}
    lspPerformSemanticAnalysis parseAnalysis
    


let getDocumentLines (ws:LspWorkSpace) (filename:string) =
    match ws.files |> Seq.tryFind(fun f -> f.fileName = filename) with
    |Some (f)  -> f.AsLines
    | None     -> [||]

let lspEmptyWs logger = {LspWorkSpace.files = []; astRoot = None; logger = logger}








