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

    let defaultOptions = {
        Dto.GenerationOptions.Encoding = CommonTypes.Asn1Encoding.ACN 
        Dto.GenerationOptions.TargetLanguage = CommonTypes.ProgrammingLanguage.C
        Dto.GenerationOptions.IntegerSizeInBytes = 8
        Dto.GenerationOptions.FloatingPointSizeInBytes = 8
        Dto.GenerationOptions.TypePrefix = ""
        Dto.GenerationOptions.FieldPrefix = CommonTypes.FieldPrefixAuto
        Dto.GenerationOptions.RenamePolicy = CommonTypes.EnumRenamePolicy.SelectiveEnumerants
        Dto.GenerationOptions.ObjectIdentifierMaxLength = 8
    }
    
    interface IAsn1Service with
        
        member this.BuildAst(input: Dto.Input): Dto.Output = 
  

            try
                lock asn1f2AccessLock (fun() -> this.CallProgram input)
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
    
    member private this.BuildCommandLineSettings asnFiles acnFiles (options:Dto.GenerationOptions) outfile =
        {
            CommandLineSettings.asn1Files = asnFiles |> Seq.map this.ConvertInput |> Seq.toList
            acnFiles = acnFiles |> Seq.map this.ConvertInput |> Seq.toList
            encodings = match box options.Encoding with | null -> [defaultOptions.Encoding] | _ -> [options.Encoding]
            GenerateEqualFunctions = false
            generateAutomaticTestCases = false
            TypePrefix = match options.TypePrefix with | null -> defaultOptions.TypePrefix | _ -> options.TypePrefix
            CheckWithOss = false
            AstXmlAbsFileName = outfile
            IcdUperHtmlFileName = ""
            IcdAcnHtmlFileName = ""
            mappingFunctionsModule = None
            integerSizeInBytes = bigint(match options.IntegerSizeInBytes with | 0 -> defaultOptions.IntegerSizeInBytes | _ -> options.IntegerSizeInBytes)
            floatingPointSizeInBytes = bigint(match options.FloatingPointSizeInBytes with | 0 -> defaultOptions.FloatingPointSizeInBytes | _ -> options.FloatingPointSizeInBytes)
            renamePolicy = match box options.RenamePolicy with | null -> defaultOptions.RenamePolicy | _ -> options.RenamePolicy
            custom_Stg_Ast_Version = 1
            fieldPrefix = match box options.FieldPrefix with | null -> None | _ -> Some(options.FieldPrefix)
            targetLanguages = match box options.TargetLanguage with | null -> [defaultOptions.TargetLanguage] | _ -> [options.TargetLanguage]
            objectIdentifierMaxLength = bigint(match options.ObjectIdentifierMaxLength with | 0 -> defaultOptions.ObjectIdentifierMaxLength | _ -> options.ObjectIdentifierMaxLength)
            streamingModeSupport = false
            target = None
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
    member private this.CallProgram (input: Dto.Input) : Dto.Output =
        use dir = new TemporaryDirectory()
        let xmlFileName = dir.FullPath "AST.xml"

        let options = match box input.Options with | null -> defaultOptions | _ -> input.Options
        let args = this.BuildCommandLineSettings input.AsnFiles input.AcnFiles options xmlFileName
        let frontEntAst, acnDeps = FrontEntMain.constructAst args (fun a b -> ())
        ExportToXml.exportFile frontEntAst acnDeps args.AstXmlAbsFileName

        {
            ErrorCode = 0
            Files = this.ReadAstXmlFromTempDir xmlFileName dir |> Seq.singleton
            Messages = null
        }
