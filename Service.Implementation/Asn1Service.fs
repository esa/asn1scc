namespace Service.Implementation

open Service
open System
open Service.Implementation.Utils
open CommonTypes
open System.Collections.Generic
open FsUtils
open Antlr

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
    
    member private this.BuildCommandLineSettings (dir:TemporaryDirectory) (input:Dto.InputFiles) outfile =
        {
            CommandLineSettings.asn1Files = this.StoreFilesInDirectory dir input.AsnFiles
            acnFiles = this.StoreFilesInDirectory dir input.AcnFiles
            encodings = [ CommonTypes.Asn1Encoding.ACN ]// TODO does this influence generated XML?
            GenerateEqualFunctions = false
            generateAutomaticTestCases = false
            TypePrefix = ""
            CheckWithOss = false
            AstXmlAbsFileName = outfile
            IcdUperHtmlFileName = ""
            IcdAcnHtmlFileName = ""
            mappingFunctionsModule = None
            integerSizeInBytes = 8
            renamePolicy = CommonTypes.EnumRenamePolicy.NoRenamePolicy
            custom_Stg_Ast_Version = 1
        }

    member private this.StoreFilesInDirectory (dir:TemporaryDirectory) (files:IEnumerable<Dto.FileData>) = files |> Seq.map (fun f -> dir.Store f) |> Seq.toList
    member private this.ReadAstXmlFromTempDir file (dir:TemporaryDirectory) : Dto.FileData = 
                {
                    Name = file
                    Contents = dir.FullPath(file) 
                                |> System.IO.File.ReadAllLines 
                                |> Seq.map (fun l -> l.Replace(dir.Path, ""))
                                |> String.concat "\n"
                }
    member private this.CallProgram (files: Dto.InputFiles) : Dto.Output =
        use dir = new TemporaryDirectory()
        let xmlFileName = dir.FullPath "AST.xml"

        let args = this.BuildCommandLineSettings dir files xmlFileName
        let frontEntAst, acnDeps = FrontEntMain.constructAst args
        ExportToXml.exportFile frontEntAst acnDeps args.AstXmlAbsFileName

        {
            ErrorCode = 0
            Files = this.ReadAstXmlFromTempDir xmlFileName dir |> Seq.singleton
            Messages = null
        }
