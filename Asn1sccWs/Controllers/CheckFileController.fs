namespace MyWebApi.Controllers

//module CheckFileController

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open CommonTypes
open System.IO
open FsUtils

type Asn1File = {
    fileName : string
    fileContent : string
}

type ErrorMsg = {
    fileName : string
    line    : int
    charPos : int
    errMsg  : string
}

//curl -k -d '{"fileName":"a.asn1", "fileContent":"a ::= SEQUENCE"}' -H "Content-Type: application/json" -X POST https://localhost:5001/checkfile


//curl -k -d '{"fileName":"C:/prj/GitHub/asn1scc/tmp/21/a.asn", "fileContent":"a ::= SEQUENCE"}' -H "Content-Type: application/json" -X POST https://localhost:5001/checkfile


//curl -k -d '{"fileName":"C:/prj/GitHub/asn1scc/tmp/21/a.asn", "fileContent":"T DEFINITIONS ::= \nBEGIN \nMySeq ::= SEQUENCE { \na OCTET STRING (SIZE (1 .. r))\n} \nEND"}' -H "Content-Type: application/json" -X POST https://localhost:5001/checkfile


[<ApiController>]
[<Route("[controller]")>]
type CheckFileController (logger : ILogger<CheckFileController>) =
    inherit ControllerBase()

    [<HttpPost>]
    member _.Post(f:Asn1File) =

        let debugFunc (r:Asn1Ast.AstRoot) (acn:AcnGenericTypes.AcnAst) = ()
        let constructCommandLineSettings (f:Asn1File) =
            let createInput (f:Asn1File) : Input = 
                {
                    name = f.fileName; 
                    contents = 
                        match f.fileContent with
                        | null
                        | ""        -> File.OpenRead(f.fileName) :> Stream 
                        | _ as fc   ->
                            let stream = new MemoryStream()
                            let writer = new StreamWriter(stream)
                            writer.Write(fc)
                            writer.Flush()
                            stream.Position <- 0L
                            stream :> Stream 
                }

            {
                CommandLineSettings.asn1Files = [f] |> List.map createInput
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

        
        let args = constructCommandLineSettings f

        try 
            let frontEntAst, acnDeps = FrontEntMain.constructAst args debugFunc 
            [| |]
        with
            | UserException msg            ->
                [|
                    {ErrorMsg.fileName = f.fileName; line=0; charPos=0; errMsg=msg}
                |]
            | :? Antlr.Asn1.asn1Parser.SyntaxErrorException as ex            ->
                [|
                    {ErrorMsg.fileName = f.fileName; line=0; charPos=0; errMsg=ex.Message}
                |]
            | SemanticError (loc,msg)            ->
                [|
                    {ErrorMsg.fileName = loc.srcFilename; line=loc.srcLine; charPos=loc.charPos; errMsg=msg}
                |]


