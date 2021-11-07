module LspAst

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


type LspType =
    | LspInteger 
    | LspReal    
    | LspIA5String 
    | LspNumericString
    | LspOctetString 
    | LspObjectIdentifier
    | LspRelativeObjectIdentifier
    | LspTimeType         
    | LspNullType          
    | LspBitString         
    | LspBoolean 
    | LspEnumerated        of list<string>
    | LspSequenceOf        of LspType    
    | LspSequence          of list<string*LspType>
    | LspChoice            of list<StringLoc*LspType>
    | LspReferenceType     of StringLoc*StringLoc


type LspTypeAssignment = {
    name : string
    line0 : int
    charPos : int 
    //lspType : LspType
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

type LspModule = {
    typeAssignments : LspTypeAssignment list
}

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
    astRoot : Asn1AcnAst.AstRoot option
}
with 
    override this.ToString() = sprintf "%A" this


let private acnEncPropsPerType = 

    [
     (asn1Parser.INTEGER,   (acnParser.ENCODING, ["encoding pos-int"; "encoding twos-complement"; "encoding BCD"; "encoding ASCII"]))
     (asn1Parser.INTEGER,   (acnParser.SIZE, ["size"]))
     (asn1Parser.INTEGER,   (acnParser.ENDIANNES, ["endianness big"; "endianness little"]))
     (asn1Parser.INTEGER,   (acnParser.MAPPING_FUNCTION, ["mapping-function myFunction"]))
    ] 

let getTypeAcnProps typeKind =
    acnEncPropsPerType |> List.choose (fun (a,b) -> if typeKind = a then Some b else None)
