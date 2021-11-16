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
open System.Xml
open System.Xml.Linq

type LspFileType =
    | LspAsn1
    | LspAcn


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
    lspFileType     : LspFileType
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
    logger : (string->int)
    files : LspFile list
    astRoot : Asn1AcnAst.AstRoot option
}
with 
    override this.ToString() = sprintf "%A" this


let private acnEncPropsPerType = 

    [
        (asn1Parser.INTEGER_TYPE,   (acnParser.ENCODING, ["encoding pos-int"; "encoding twos-complement"; "encoding BCD"; "encoding ASCII"]))
        (asn1Parser.INTEGER_TYPE,   (acnParser.SIZE, ["size"]))
        (asn1Parser.INTEGER_TYPE,   (acnParser.ENDIANNES, ["endianness big"; "endianness little"]))
        (asn1Parser.INTEGER_TYPE,   (acnParser.MAPPING_FUNCTION, ["mapping-function myFunction"]))

        (asn1Parser.REAL,           (acnParser.ENCODING, ["encoding IEEE754-1985-32"; "encoding IEEE754-1985-32"]))
        (asn1Parser.REAL,           (acnParser.ENDIANNES, ["endianness big"; "endianness little"]))

        (asn1Parser.IA5String, (acnParser.ENCODING, ["encoding ASCII"]))
        (asn1Parser.IA5String, (acnParser.SIZE, ["size other-fld"; "size null-terminated"]))
        (asn1Parser.IA5String, (acnParser.TERMINATION_PATTERN, ["termination-pattern '00'H" ]))

        (asn1Parser.OCTECT_STING, (acnParser.SIZE, ["size other-fld"; "size null-terminated"]))
        (asn1Parser.OCTECT_STING, (acnParser.TERMINATION_PATTERN, ["termination-pattern '0000'H" ]))

        (asn1Parser.BIT_STRING_TYPE, (acnParser.SIZE, ["size other-fld"; "size null-terminated"]))
        (asn1Parser.BIT_STRING_TYPE, (acnParser.TERMINATION_PATTERN, ["termination-pattern '0000'H" ]))

        (asn1Parser.SEQUENCE_OF_TYPE, (acnParser.SIZE, ["size other-fld"; "size null-terminated"]))
        (asn1Parser.SEQUENCE_OF_TYPE, (acnParser.TERMINATION_PATTERN, ["termination-pattern '0000'H" ]))


        (asn1Parser.ENUMERATED_TYPE,   (acnParser.ENCODING, ["encoding pos-int"; "encoding twos-complement"; "encoding BCD"; "encoding ASCII"]))
        (asn1Parser.ENUMERATED_TYPE,   (acnParser.SIZE, ["size"]))
        (asn1Parser.ENUMERATED_TYPE,   (acnParser.ENDIANNES, ["endianness big"; "endianness little"]))
        (asn1Parser.ENUMERATED_TYPE,   (acnParser.MAPPING_FUNCTION, ["mapping-function myFunction"]))
        (asn1Parser.ENUMERATED_TYPE,   (acnParser.ENCODE_VALUES, ["encode-values"]))

        (asn1Parser.BOOLEAN,      (acnParser.TRUE_VALUE, ["true-value '1'B"]))
        (asn1Parser.BOOLEAN,      (acnParser.FALSE_VALUE, ["false-value '1'B"]))
        
        (asn1Parser.NULL,         (acnParser.PATTERN, ["pattern '1'B"]))

        (asn1Parser.CHOICE_TYPE,       (acnParser.DETERMINANT, ["determinant other-fld"]))
    ] 

let getTypeAcnProps typeKind =
    let actType =
        match typeKind with
        | asn1Parser.SET_TYPE       -> asn1Parser.SEQUENCE_TYPE
        | asn1Parser.SET_OF_TYPE    -> asn1Parser.SET_OF_TYPE
        | asn1Parser.NumericString  -> asn1Parser.IA5String
        | _                         -> typeKind
    acnEncPropsPerType |> List.choose (fun (a,b) -> if actType = a then Some b else None)


let isAsn1File (fn:string) = fn.ToLower().EndsWith("asn1") || fn.ToLower().EndsWith("asn")
let isAcnFile  (fn:string) = fn.ToLower().EndsWith("acn") 

let tryGetFileType filename = 
    match isAsn1File filename with
    | true -> Some LspAsn1
    | false ->
        match isAcnFile filename with
        | true  -> Some LspAcn
        | false -> None

let isInside (range : LspRange) (pos: LspPos) =
    if range.start.line < pos.line && pos.line < range.end_.line then true
    elif range.start.line < pos.line && pos.line = range.end_.line then pos.charPos <= range.end_.charPos
    elif range.start.line <= pos.line && pos.line < range.end_.line then range.start.charPos <= pos.charPos
    elif range.start.line = pos.line && pos.line = range.end_.line then range.start.charPos <= pos.charPos && pos.charPos <= range.end_.charPos
    else false
    

(*
Returns the antlr tree nodes based from the leaf to the root based on the current position (line, charpos)
The position is provided by the UI
*)
let getTreeNodesByPosition (fn:LspFile) (lspPos:LspPos) =

    let rec getTreeNodesByPosition_aux (tokens : IToken array)  (r:ITree)  =
        seq {
            let range = r.getRange tokens
            match isInside range lspPos with
            | false -> ()
            | true  ->
                    yield r
                    for c in r.Children do  
                        yield! getTreeNodesByPosition_aux tokens  c
        } |> Seq.toList 

    let ret = getTreeNodesByPosition_aux fn.tokens fn.antlrResult.rootItem 
    List.rev ret


let private xname s = System.Xml.Linq.XName.Get(s)
let private xnameNs str ns = System.Xml.Linq.XName.Get(str, ns)

let private xsiUrl = "http://www.w3.org/2001/XMLSchema-instance"
let private xsi = XNamespace.Get xsiUrl


let exportFile (tokens : IToken array) (r:ITree)  (fileName:string) =
    let writeTextFile fileName (content:String) =
        System.IO.File.WriteAllText(fileName, content.Replace("\r",""))

    let rec exportNode (r:ITree) =
        let rg = r.getRange tokens
        let f (p:LspPos) = sprintf "(%d,%d)" p.line p.charPos

        XElement(xname "Node",
            XAttribute(xname "type", acnParser.tokenNames.[r.Type]),
            XAttribute(xname "text", r.Text),
            XAttribute(xname "start", (f rg.start)),
            XAttribute(xname "end", (f rg.end_)),
            XElement(xname "Content", (r.getCompositeText tokens)),
            (r.Children |> List.map exportNode) )
    let wsRoot =
        XElement(xname "Antl",
            XAttribute(XNamespace.Xmlns + "xsi", xsi),
            (exportNode r))

    let dec = new XDeclaration("1.0", "utf-8", "true")
    let doc =new XDocument(dec)
    doc.AddFirst wsRoot
    doc.Save(fileName)



