namespace Service.Implementation

open Service
open System
open Service.Implementation.Utils

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
                | :? FsUtils.SemanticError as ex ->
                    {
                        ErrorCode = 2
                        Files = null
                        Messages = Seq.singleton(Asn1f2.Program.FormatError(ex))
                    }


        member this.Version = Asn1f2.Program.GetVersionString()

    member private this.BuildAstArguments outFile inputPaths = Seq.append [ "-ast"; outFile ] inputPaths
    member private this.StoreFilesInDirectory (dir:TemporaryDirectory) (input:Dto.InputFiles) = input.AsnFiles |> Seq.map (fun f -> dir.Store f)
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
        let xmlFileName = "AST.xml"

        let tempFiles = this.StoreFilesInDirectory dir files
        let args = this.BuildAstArguments (dir.FullPath xmlFileName) tempFiles
        
        match Asn1f2.Program.CheckSuccess(args) with
        | 0 -> 
            {
                ErrorCode = 0
                Files = this.ReadAstXmlFromTempDir xmlFileName dir |> Seq.singleton
                Messages = null
            }
        | err -> 
            {
                ErrorCode = err
                Files = null
                Messages = Seq.singleton "Internal compiler error"
            }
                                                                
