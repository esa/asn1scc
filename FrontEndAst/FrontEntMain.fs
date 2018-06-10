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

//let CreateAstRoot (list:(ITree*string*array<IToken>) seq) (encodings:array<Asn1Encoding>) generateEqualFunctions typePrefix checkWithOss astXmlFileName icdUperHtmlFileName icdAcnHtmlFileName (mappingFunctionsModule:string) integerSizeInBytes =  


let antlrParse (lexer: ICharStream -> #ITokenSource ) parser treeParser (name: string, inputs : Input seq) = 

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
    let tokenStream = new CommonTokenStream(lexer(antlrStream))
    let tokens = tokenStream.GetTokens().Cast<IToken>() |> Seq.toArray
    let tree = treeParser(parser(tokenStream));
    {ParameterizedAsn1Ast.AntlrParserResult.fileName = name; ParameterizedAsn1Ast.AntlrParserResult.rootItem=tree; ParameterizedAsn1Ast.AntlrParserResult.tokens=tokens}

let constructAst (args:CommandLineSettings) (debugFnc : Asn1Ast.AstRoot -> AcnCreateFromAntlr.AcnAst-> unit)  =
    let asn1ParseTrees = args.asn1Files |> Seq.groupBy(fun f -> f.name) |> Seq.map (antlrParse (fun f -> new asn1Lexer(f)) (fun ts -> new asn1Parser(ts))  (fun p -> p.moduleDefinitions().Tree :?> ITree)  ) |> Seq.toList
    let acnParseTrees = args.acnFiles |> Seq.groupBy(fun f -> f.name) |> Seq.map (antlrParse (fun f -> new acnLexer(f)) (fun ts -> new acnParser(ts))  (fun p -> p.moduleDefinitions().Tree :?> ITree)  ) |> Seq.toList

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

    let parameterized_ast = CreateAsn1AstFromAntlrTree.CreateAstRoot asn1ParseTrees args

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
    let acnAst,acn0 = AcnCreateFromAntlr.mergeAsn1WithAcnAst uniqueEnumNamesAst acnParseTrees TargetLanguageStgMacros.c_StgMacros
    debugFnc uniqueEnumNamesAst acn0

    (*
        check acn references
    *)
    let acnDeps = CheckLongReferences.checkAst acnAst

    (acnAst, acnDeps)


let formatSemanticError (loc:SrcLoc) (msg:string) =
    if loc.Equals(FsUtils.emptyLocation)
        then "error: " + msg
        else ErrorFormatter.FormatError(loc.srcFilename, loc.srcLine, loc.charPos, msg)