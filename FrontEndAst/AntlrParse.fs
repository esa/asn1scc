module AntlrParse
#nowarn "3536"

open Antlr.Runtime
open Antlr.Runtime.Tree
open System.IO
open Antlr.Asn1
open Antlr.Acn
open FsUtils
open CommonTypes
open System.Collections.Generic
open System.Linq

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
    let b:ITree = tree
    
    let allErrors = parcerErrors@lexerErrors
    {CommonTypes.AntlrParserResult.fileName = name; CommonTypes.AntlrParserResult.rootItem=tree; CommonTypes.AntlrParserResult.tokens=tokens}, allErrors





let asn1treeParser (tokenStream:CommonTokenStream)=
    let p = new asn1Parser(tokenStream)
    let a1 = p.moduleDefinitions().Tree :?> ITree
    let a2 = p.errorList |> Seq.toList
    a1, a2

let asn1Lexer (f:ICharStream) =
    let lexer = new asn1Lexer(f)
    lexer, lexer.errorList |> Seq.toList

let acnTreeGeneric func (tokenStream:CommonTokenStream)  =
    let p = new acnParser(tokenStream)
    let a1 : ITree = func(p)
    let a2 = p.errorList |> Seq.toList
    a1, a2

//let acnTreeParserMod (tokenStream:CommonTokenStream)=
//    acnTreeGeneric tokenStream (fun p -> p.moduleDefinitions().Tree :?> ITree)

let acnTreeParser (tokenStream:CommonTokenStream)=
    let p = new acnParser(tokenStream)
    let a1 = p.moduleDefinitions().Tree :?> ITree
    let a2 = p.errorList |> Seq.toList
    a1, a2

let acnLexer (f:ICharStream) =
    let lexer = new acnLexer(f)
    lexer, lexer.errorList |> Seq.toList


let formatSemanticError (loc:SrcLoc) (msg:string) =
    if loc.Equals(emptyLocation)
        then "error: " + msg
        else Antlr.ErrorFormatter.FormatError(loc.srcFilename, loc.srcLine, loc.charPos, msg)

let formatSemanticWarning (loc:SrcLoc) (msg:string) =
    if loc.Equals(emptyLocation)
        then "warning: " + msg
        else Antlr.ErrorFormatter.FormatWarning(loc.srcFilename, loc.srcLine, loc.charPos, msg)