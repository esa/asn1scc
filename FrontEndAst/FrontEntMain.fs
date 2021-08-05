module FrontEntMain

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
open ParameterizedAsn1Ast
open Antlr.Asn1
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open CommonTypes
open AcnGenericTypes

//let CreateAstRoot (list:(ITree*string*array<IToken>) seq) (encodings:array<Asn1Encoding>) generateEqualFunctions typePrefix checkWithOss astXmlFileName icdUperHtmlFileName icdAcnHtmlFileName (mappingFunctionsModule:string) integerSizeInBytes =  

type LspFileType =
    | LspAsn1
    | LspAcn

type LspTypeAssignment = {
    name : string
    line0 : int
    charPos : int 
}
with 
    override this.ToString() =
        sprintf "%A" this

type LspError = {
    line0           : int      //zero based line index
    charPosInline   : int
    msg             : string
}
with 
    override this.ToString() = sprintf "%A" this

type LspFile = {
    fileName        : string
    content         : string
    tokens          : IToken array
    antlrResult     : AntlrParserResult
    parseErrors     : LspError list
    semanticErrors  : LspError list
    tasList         : LspTypeAssignment list
}
with 
    override this.ToString() = sprintf "%A" this


let defaultCommandLineSettings  =
    {
        CommandLineSettings.asn1Files = []
        acnFiles  = []
        encodings = []
        GenerateEqualFunctions = false
        generateAutomaticTestCases = false
        TypePrefix = ""
        CheckWithOss = false
        AstXmlAbsFileName = ""
        IcdUperHtmlFileName = ""
        IcdAcnHtmlFileName = ""
        custom_Stg_Ast_Version = 1
        mappingFunctionsModule = None
        integerSizeInBytes = 8I
        floatingPointSizeInBytes = 8I
        target = None
        streamingModeSupport = false
        renamePolicy = CommonTypes.EnumRenamePolicy.NoRenamePolicy
        fieldPrefix = None
        targetLanguages = []
        objectIdentifierMaxLength = 20I
    }    

type LspWorkSpace = {
    files : LspFile list
}
with 
    override this.ToString() = sprintf "%A" this




    


let antlrParse (lexer: ICharStream -> (#ITokenSource* asn1Parser.AntlrError list) ) treeParser (name: string, inputs : Input seq) = 
    let concatenateStreams (streams : Stream seq) =
        let spaceAscii = (byte 32)
        let memStream = new MemoryStream()
        streams |> Seq.iter (fun s -> s.CopyTo(memStream); memStream.WriteByte(spaceAscii))
        memStream.Position <- (int64 0)
        memStream :> Stream

    let inputStream =
        match Seq.length inputs > 1 with
        | true -> inputs |> Seq.map (fun i -> i.contents) |> concatenateStreams
        | false -> inputs |> Seq.head |> (fun i -> i.contents)
    
    let antlrStream = new ANTLRInputStream(inputStream)
    antlrStream.SourceName <- name

    let lexerTokens, lexerErrors  = lexer(antlrStream)
    let tokenStream = new CommonTokenStream(lexerTokens)
    let tokens = tokenStream.GetTokens().Cast<IToken>() |> Seq.toArray
    let tree, parcerErrors = treeParser(tokenStream);
    let allErrors = parcerErrors@lexerErrors
    {CommonTypes.AntlrParserResult.fileName = name; CommonTypes.AntlrParserResult.rootItem=tree; CommonTypes.AntlrParserResult.tokens=tokens}, allErrors




let rec visitAntlrTree (r:ITree)  =
    r.AllChildren |> List.filter(fun z -> z.Type = asn1Parser.TYPE_ASSIG) |> List.map(fun z -> z.Text) |> Seq.toArray

let asn1treeParser (tokenStream:CommonTokenStream)=
    let p = new asn1Parser(tokenStream)
    let a1 = p.moduleDefinitions().Tree :?> ITree
    let a2 = p.errorList |> Seq.toList
    a1, a2

let asn1Lexer (f:ICharStream) =
    let lexer = new asn1Lexer(f)
    lexer, lexer.errorList |> Seq.toList

let acnTreeParser (tokenStream:CommonTokenStream)=
    let p = new acnParser(tokenStream)
    let a1 = p.moduleDefinitions().Tree :?> ITree
    let a2 = p.errorList |> Seq.toList
    a1, a2

let acnLexer (f:ICharStream) =
    let lexer = new acnLexer(f)
    lexer, lexer.errorList |> Seq.toList

let constructCommandLineSettings (fileName:string) strm =
    {
        CommandLineSettings.asn1Files = [{Input.name = fileName; contents = strm }]
        acnFiles  = []
        encodings = []
        GenerateEqualFunctions = false
        generateAutomaticTestCases = false
        TypePrefix = ""
        CheckWithOss = false
        AstXmlAbsFileName = ""
        IcdUperHtmlFileName = ""
        IcdAcnHtmlFileName = ""
        custom_Stg_Ast_Version = 1
        mappingFunctionsModule = None
        integerSizeInBytes = 8I
        floatingPointSizeInBytes = 8I
        target = None
        streamingModeSupport = false
        renamePolicy = CommonTypes.EnumRenamePolicy.NoRenamePolicy
        fieldPrefix = None
        targetLanguages = []
        objectIdentifierMaxLength = 20I
    }    


type TypeAssignmentLSP = {
    name : string
    line : int
    charPos : int 
} 



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


let lspParseAcnFile (fileName:string) (fileContent:string) =
    let stm = new MemoryStream(System.Text.Encoding.UTF8.GetBytes(fileContent))
    let acnParseTree, antlerErrors = antlrParse acnLexer acnTreeParser (fileName, [{Input.name=fileName; contents=stm}])
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
            let parameterized_ast = CreateAsn1AstFromAntlrTree.CreateAstRoot asn1ParseTrees acnAst args
            let asn1Ast0 = MapParamAstToNonParamAst.DoWork parameterized_ast
            CheckAsn1.CheckFiles asn1Ast0 0
            let uniqueEnumNamesAst = EnsureUniqueEnumNames.DoWork asn1Ast0 
            let acnAst,acn0 = AcnCreateFromAntlr.mergeAsn1WithAcnAst uniqueEnumNamesAst (acnAst, acnParseTrees) 
            let acnDeps = CheckLongReferences.checkAst acnAst
            ws
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
        
let lspAutoComplete (ws:LspWorkSpace) filename (line0:int) (charPos:int) =
    let asn1Keywords = ["INTEGER"; "REAL"; "ENUMERATED"; "CHOICE"; "SEQUENCE"; "SEQUENCE OF"; "OCTET STRING"; "BIT STRING"; "IA5String"]
    let tasList = ws.files |> List.collect(fun x -> x.tasList) |> List.map(fun x -> x.name)
    match tryGetFileType filename with
    | Some LspAsn1  -> 
        tasList@asn1Keywords
    | Some LspAcn   -> tasList
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

let lspEmptyWs = {LspWorkSpace.files = []}












    

let constructAst_int (args:CommandLineSettings) (op_mode:Asn1SccOperationMode) (debugFnc : Asn1Ast.AstRoot -> AcnGenericTypes.AcnAst-> unit) : (Result<Asn1AcnAst.AstRoot*Asn1AcnAst.AcnInsertedFieldDependencies, (UserError *UserError list)>) =

    let asn1ParseTrees, asn1ParseErrors = 
        args.asn1Files |> 
        Seq.groupBy(fun f -> f.name) |> 
        Seq.map (antlrParse asn1Lexer asn1treeParser  ) |> 
        Seq.toList |> List.unzip

    let acnParseTrees, acnParseErrors = 
        args.acnFiles |> Seq.groupBy(fun f -> f.name) |> Seq.map (antlrParse acnLexer acnTreeParser  ) |> Seq.toList |> List.unzip

    match asn1ParseErrors@acnParseErrors |> List.collect id with
    | []     -> 
        let acnAst = AcnGenericCreateFromAntlr.CreateAcnAst args.integerSizeInBytes acnParseTrees

        (*
            * constructs a parameterized (templatized) ASN.1 AST by antlr trees.
            * A parameterized ASN.1 AST is the one that contains generic types. E.g.
            * 	
            * FrequenciesTemplate{INTEGER:someLength, SomeType } ::= SEQUENCE (SIZE(someLength)) OF SomeType
            * 
		        * MyTestInt ::= FrequenciesTemplate{5, INTEGER(1..20)}
		        * MyTestReal ::= FrequenciesTemplate{5, REAL}
            * 
            * 
        *)

        let parameterized_ast = CreateAsn1AstFromAntlrTree.CreateAstRoot asn1ParseTrees acnAst args

        (*
            *  Removes parameterized types by resolving them. In the example above
            *  FrequenciesTemplate will no longer exist
            *  and MyTestInt and MyTestReal will defined as follows
            *  MyTestInt ::= SEQUENCE (SIZE(5)) OF INTEGER(1..20)
            *  MyTestReal ::= SEQUENCE (SIZE(5)) OF REAL
            *)
        let asn1Ast0 = MapParamAstToNonParamAst.DoWork parameterized_ast


        (*
            * Performs semantic validations which cannot be handled during ANTLR parsing.
            * For example the following definition
            * MySeq ::= {a INTEGER, a REAL}
            * is OK for ANTLR but of course not OK for ASN.1
            *)
        //todo : check for commented code with uPER.
        CheckAsn1.CheckFiles asn1Ast0 0

        (*
            Ensure unique enum names
        *)
        let uniqueEnumNamesAst = EnsureUniqueEnumNames.DoWork asn1Ast0 


        (*
            - Updates ASN.1 AST with ACN information
            - Creates the expanded tree (i.e reference types are now resolved)
        *)
        let acnAst,acn0 = AcnCreateFromAntlr.mergeAsn1WithAcnAst uniqueEnumNamesAst (acnAst, acnParseTrees) 
        debugFnc uniqueEnumNamesAst acn0

        (*
            check acn references
        *)
        let acnDeps = CheckLongReferences.checkAst acnAst

        Ok (acnAst, acnDeps)
    | xx1::xs ->  
        let mp (e:asn1Parser.AntlrError) = {UserError.filePath = e.filePath; line = e.line; charPosInline = e.charPosInline; msg = e.msg; fullMessage = e.FullMessage; severity = UserErrorSeverity.ERROR}
        let e1 = mp xx1
        let errs = xs |> List.map mp
        Error (e1,errs)
        


let constructAst (args:CommandLineSettings)  (debugFnc : Asn1Ast.AstRoot -> AcnGenericTypes.AcnAst-> unit) : (Asn1AcnAst.AstRoot*Asn1AcnAst.AcnInsertedFieldDependencies) =

    match constructAst_int args Asn1SccOperationMode.Asn1Compiler debugFnc with
    | Ok res    -> res
    | Error (xx1, _)  -> raise (asn1Parser.SyntaxErrorException xx1.fullMessage)
        


let formatSemanticError (loc:SrcLoc) (msg:string) =
    if loc.Equals(FsUtils.emptyLocation)
        then "error: " + msg
        else ErrorFormatter.FormatError(loc.srcFilename, loc.srcLine, loc.charPos, msg)

let formatSemanticWarning (loc:SrcLoc) (msg:string) =
    if loc.Equals(FsUtils.emptyLocation)
        then "warning: " + msg
        else ErrorFormatter.FormatWarning(loc.srcFilename, loc.srcLine, loc.charPos, msg)