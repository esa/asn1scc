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
open Language
//let CreateAstRoot (list:(ITree*string*array<IToken>) seq) (encodings:array<Asn1Encoding>) generateEqualFunctions typePrefix checkWithOss astXmlFileName icdUperHtmlFileName icdAcnHtmlFileName (mappingFunctionsModule:string) integerSizeInBytes =  





    

let constructAst_int (args:CommandLineSettings) (lms:(ProgrammingLanguage*LanguageMacros) list) (op_mode:Asn1SccOperationMode) (debugFnc : Asn1Ast.AstRoot -> AcnGenericTypes.AcnAst-> unit) : (Result<Asn1AcnAst.AstRoot*Asn1AcnAst.AcnInsertedFieldDependencies, (UserError *UserError list)>) =

    let asn1ParseTrees, asn1ParseErrors = 
        TL "FrontEntMain.antlrParse_asn1" (fun () ->
            args.asn1Files |> 
            Seq.groupBy(fun f -> f.name) |> 
            Seq.map (antlrParse asn1Lexer asn1treeParser  ) |> 
            Seq.toList |> List.unzip)

    let acnParseTrees, acnParseErrors = 
        TL "FrontEntMain.antlrParse_acn" (fun () ->
            args.acnFiles |> Seq.groupBy(fun f -> f.name) |> Seq.map (antlrParse acnLexer acnTreeParser  ) |> Seq.toList |> List.unzip)

    match asn1ParseErrors@acnParseErrors |> List.collect id with
    | []     -> 
        let acnAst = 
            TL "AcnGenericCreateFromAntlr.CreateAcnAst" (fun () ->
                AcnGenericCreateFromAntlr.CreateAcnAst args.integerSizeInBytes acnParseTrees)

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

        let parameterized_ast = 
            TL "CreateAsn1AstFromAntlrTree.CreateAstRoot" (fun () ->
                CreateAsn1AstFromAntlrTree.CreateAstRoot asn1ParseTrees acnAst args)

        (*
            *  Removes parameterized types by resolving them. In the example above
            *  FrequenciesTemplate will no longer exist
            *  and MyTestInt and MyTestReal will defined as follows
            *  MyTestInt ::= SEQUENCE (SIZE(5)) OF INTEGER(1..20)
            *  MyTestReal ::= SEQUENCE (SIZE(5)) OF REAL
            *)
        let asn1Ast0 = 
            TL "MapParamAstToNonParamAst.DoWork" (fun () ->
                MapParamAstToNonParamAst.DoWork parameterized_ast)


        (*
            * Performs semantic validations which cannot be handled during ANTLR parsing.
            * For example the following definition
            * MySeq ::= {a INTEGER, a REAL}
            * is OK for ANTLR but of course not OK for ASN.1
            *)
        //todo : check for commented code with uPER.
        TL "CheckAsn1.CheckFiles" (fun () ->
            CheckAsn1.CheckFiles asn1Ast0 0)

        (*
            Ensure unique enum names
        *)

        let uniqueEnumNamesAst = 
            TL "EnsureUniqueEnumNames.DoWork" (fun () ->            
                EnsureUniqueEnumNames.DoWork asn1Ast0 lms)


        (*
            - Updates ASN.1 AST with ACN information
            - Creates the expanded tree (i.e reference types are now resolved)
        *)
        let acnAst,acn0 = 
            TL "AcnCreateFromAntlr.mergeAsn1WithAcnAst" (fun () ->
                AcnCreateFromAntlr.mergeAsn1WithAcnAst uniqueEnumNamesAst lms (acnAst, acnParseTrees))

        TL "debugFnc uniqueEnumNamesAst" (fun () ->
            debugFnc uniqueEnumNamesAst acn0)

        (*
            check acn references
        *)
        let acnDeps = 
            TL "CheckLongReferences.checkAst" (fun () ->
                CheckLongReferences.checkAst acnAst)

        Ok (acnAst, acnDeps)
    | xx1::xs ->  
        let mp (e:asn1Parser.AntlrError) = {UserError.filePath = e.filePath; line = e.line; charPosInline = e.charPosInline; msg = e.msg; fullMessage = e.FullMessage; severity = UserErrorSeverity.ERROR}
        let e1 = mp xx1
        let errs = xs |> List.map mp
        Error (e1,errs)
        


let constructAst (args:CommandLineSettings) (lms:(ProgrammingLanguage*LanguageMacros) list) (debugFnc : Asn1Ast.AstRoot -> AcnGenericTypes.AcnAst-> unit) : (Asn1AcnAst.AstRoot*Asn1AcnAst.AcnInsertedFieldDependencies) =

    match constructAst_int args lms Asn1SccOperationMode.Asn1Compiler debugFnc with
    | Ok res    -> res
    | Error (xx1, _)  -> raise (asn1Parser.SyntaxErrorException xx1.fullMessage)
        


