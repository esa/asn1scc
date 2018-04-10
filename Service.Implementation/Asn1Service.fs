namespace Service.Implementation

open Service
open System
open Service.Implementation.Utils
open CommonTypes
open System.Collections.Generic
open FsUtils
open Antlr
open System.Text
open System.IO

type Asn1Service() =

    let asn1f2AccessLock = new Object();
    
    interface IAsn1Service with
        
        member this.BuildAst(files: Dto.InputFiles): Dto.Output = 
  

            try
                lock asn1f2AccessLock (fun() -> this.CallProgram files)
            with
                | :? Antlr.Asn1.asn1Parser.SyntaxErrorException as ex ->
                    {
                        ErrorCode = 1
                        Files = null
                        Messages = Seq.singleton ex.Message
                    }
                | SemanticError (loc,msg)  ->
                    {
                        ErrorCode = 2
                        Files = null
                        Messages = [ FrontEntMain.formatSemanticError loc msg]
                    }

                | UserException msg ->
                    {
                        ErrorCode = 2
                        Files = null
                        Messages = Seq.singleton(msg)
                    }
                | ex ->
                    {
                        ErrorCode = 4
                        Files = null
                        Messages = [ex.Message; ex.StackTrace]
                    }

        member this.Version = "TODO" /// TODO, where version should be stored? FrontEnd? it can't be in Asnf4 Asn1f2.Program.GetVersionString()
    
    member private this.BuildCommandLineSettings (input:Dto.InputFiles) outfile =
        {
            CommandLineSettings.asn1Files = input.AsnFiles |> Seq.map this.ConvertInput |> Seq.toList
            acnFiles = input.AcnFiles |> Seq.map this.ConvertInput |> Seq.toList
            encodings = [ CommonTypes.Asn1Encoding.ACN ]// TODO does this influence generated XML?
            GenerateEqualFunctions = false
            generateAutomaticTestCases = false
            TypePrefix = ""
            CheckWithOss = false
            AstXmlAbsFileName = outfile
            IcdUperHtmlFileName = ""
            IcdAcnHtmlFileName = ""
            mappingFunctionsModule = None
            integerSizeInBytes = 8I
            renamePolicy = CommonTypes.EnumRenamePolicy.NoRenamePolicy
            custom_Stg_Ast_Version = 1
        }
    
    member private this.ConvertInput (input:Dto.FileData) : CommonTypes.Input =
        let bytes = Encoding.UTF8.GetBytes input.Contents
        {
            name = input.Name
            contents = new MemoryStream(bytes)
        }
    member private this.ReadAstXmlFromTempDir (file:string) (dir:TemporaryDirectory) : Dto.FileData = 
                {
                    Name = file.Replace(dir.Path, "")
                    Contents = System.IO.File.ReadAllLines file |> String.concat "\n"
                }
    member private this.CallProgram (files: Dto.InputFiles) : Dto.Output =
        use dir = new TemporaryDirectory()
        let xmlFileName = dir.FullPath "AST.xml"

        let args = this.BuildCommandLineSettings files xmlFileName
        let frontEntAst, acnDeps = FrontEntMain.constructAst args
        ExportToXml.exportFile frontEntAst acnDeps args.AstXmlAbsFileName

        {
            ErrorCode = 0
            Files = this.ReadAstXmlFromTempDir xmlFileName dir |> Seq.singleton
            Messages = null
        }
