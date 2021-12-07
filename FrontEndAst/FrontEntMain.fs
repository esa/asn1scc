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
open AbstractMacros

open AntlrParse
//let CreateAstRoot (list:(ITree*string*array<IToken>) seq) (encodings:array<Asn1Encoding>) generateEqualFunctions typePrefix checkWithOss astXmlFileName icdUperHtmlFileName icdAcnHtmlFileName (mappingFunctionsModule:string) integerSizeInBytes =  





    

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