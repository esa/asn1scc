module LspAst
#nowarn "3536"

open Antlr.Runtime
open Antlr.Runtime.Tree
open System.IO
open Antlr.Asn1
open Antlr.Acn
open System.Collections.Generic
open System.Linq
open CommonTypes
open AbstractMacros
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
    //lines           : System.Collections.Generic.List<String>
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
    member this.AsLines =
        this.content.Split([|"\r\n";"\n"|], StringSplitOptions.None)

    //member this.content = String.Join(Environment.NewLine, this.lines)

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
        //IcdUperHtmlFileName = ""
        //IcdAcnHtmlFileName = ""
        generateAcnIcd = false
        custom_Stg_Ast_Version = 1
        mappingFunctionsModule = None
        integerSizeInBytes = 8I
        floatingPointSizeInBytes = 8I
        slim = false
        target = None
        streamingModeSupport = false
        renamePolicy = CommonTypes.EnumRenamePolicy.NoRenamePolicy
        fieldPrefix = None
        targetLanguages = []
        objectIdentifierMaxLength = 20I
        generateConstInitGlobals = false
        icdPdus = None
        handleEmptySequences = false
        blm = []
        userRtlFunctionsToGenerate= []
        enum_Items_To_Enable_Efficient_Enumerations = System.UInt32.MaxValue
        stainlessInvertibility = false
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
        (asn1Parser.INTEGER_TYPE,   (acnParser.ENDIANNESS, ["endianness big"; "endianness little"]))
        (asn1Parser.INTEGER_TYPE,   (acnParser.MAPPING_FUNCTION, ["mapping-function myFunction"]))

        (asn1Parser.REAL,           (acnParser.ENCODING, ["encoding IEEE754-1985-32"; "encoding IEEE754-1985-32"]))
        (asn1Parser.REAL,           (acnParser.ENDIANNESS, ["endianness big"; "endianness little"]))

        (asn1Parser.IA5String, (acnParser.ENCODING, ["encoding ASCII"]))
        (asn1Parser.IA5String, (acnParser.SIZE, ["size other-fld"; "size null-terminated"]))
        (asn1Parser.IA5String, (acnParser.TERMINATION_PATTERN, ["termination-pattern '00'H" ]))

        (asn1Parser.OCTET_STRING, (acnParser.SIZE, ["size other-fld"; "size null-terminated"]))
        (asn1Parser.OCTET_STRING, (acnParser.TERMINATION_PATTERN, ["termination-pattern '0000'H" ]))

        (asn1Parser.BIT_STRING_TYPE, (acnParser.SIZE, ["size other-fld"; "size null-terminated"]))
        (asn1Parser.BIT_STRING_TYPE, (acnParser.TERMINATION_PATTERN, ["termination-pattern '0000'H" ]))

        (asn1Parser.SEQUENCE_OF_TYPE, (acnParser.SIZE, ["size other-fld"; "size null-terminated"]))
        (asn1Parser.SEQUENCE_OF_TYPE, (acnParser.TERMINATION_PATTERN, ["termination-pattern '0000'H" ]))


        (asn1Parser.ENUMERATED_TYPE,   (acnParser.ENCODING, ["encoding pos-int"; "encoding twos-complement"; "encoding BCD"; "encoding ASCII"]))
        (asn1Parser.ENUMERATED_TYPE,   (acnParser.SIZE, ["size"]))
        (asn1Parser.ENUMERATED_TYPE,   (acnParser.ENDIANNESS, ["endianness big"; "endianness little"]))
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



let rec getAcnTypePathByITree (nodes:ITree list)  =
    let handleTypeEncoding (typeEnc : ITree) =
        if (typeEnc.Type <> acnParser.TYPE_ENCODING) then
            raise (BugErrorException "handleTypeEncoding")
        let tasName = typeEnc.GetChildByType(acnParser.UID).Text
        let modDef = typeEnc.Parent
        let modName = modDef.GetChildByType(acnParser.UID).Text
        modName::[tasName]

    let handleChild (child: ITree) =
        if (child.Type <> acnParser.CHILD) then
            raise (BugErrorException "handleChild")
        match child.Children |> Seq.tryFind (fun z -> z.Type = acnParser.LID) with
        | Some lid -> [lid.Text]
        | None     -> ["#"] // Sequence Of case

    nodes |>
    List.rev |> //node are in reverse order, so reverse them
    List.choose(fun z ->
        match z.Type with
        | acnParser.TYPE_ENCODING  -> Some (handleTypeEncoding z)
        | acnParser.CHILD          -> Some (handleChild z)
        | _                        -> None) |>
    List.collect id


type Asn1TypeDef = Asn1TypeDef of ITree


let getAsn1Type (Asn1TypeDef typeDef:Asn1TypeDef) =
    let children = getTreeChildren(typeDef)
    let typeNodes = children |> List.filter(fun x -> (not (CreateAsn1AstFromAntlrTree.ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
    let typeNode = List.head(typeNodes)
    typeNode


let rec getAsn1TypeDefinitionByPath (asn1Trees:AntlrParserResult list) (bResolveRefType:bool) (path:string list)  =
    let getModuleByName modName =
        asn1Trees |> Seq.collect (fun r -> r.rootItem.Children) |> Seq.tryFind(fun m -> m.GetChild(0).Text = modName)
    let getTasByName mdTree tasName =
        getChildrenByType(mdTree, asn1Parser.TYPE_ASSIG) |> Seq.tryFind (fun ts -> ts.GetChild(0).Text = tasName)
    let rec resolveReferenceType (typeDef:Asn1TypeDef) : Asn1TypeDef option =
        let typeNode = getAsn1Type typeDef
        match typeNode.Type with
        |asn1Parser.REFERENCED_TYPE ->
            let refType =
                match getTreeChildren(typeNode) |> List.filter(fun x -> x.Type<> asn1Parser.ACTUAL_PARAM_LIST) with
                | refTypeName::[]           ->
                    let mdTree = typeNode.GetAncestor(asn1Parser.MODULE_DEF)
                    let mdName = mdTree.GetChild(0).Text
                    let imports = mdTree.GetChildrenByType(asn1Parser.IMPORTS_FROM_MODULE)
                    let importedFromModule = imports |> List.tryFind(fun imp-> imp.GetChildrenByType(asn1Parser.UID) |> Seq.exists(fun impTypeName -> impTypeName.Text = refTypeName.Text ))
                    match importedFromModule with
                    |Some(imp)  -> Some ( imp.GetChild(0).Text,  refTypeName.Text)
                    |None       -> Some ( typeNode.GetAncestor(asn1Parser.MODULE_DEF).GetChild(0).Text,  refTypeName.Text)
                | modName::refTypeName::[]  ->  Some ( modName.Text, refTypeName.Text)
                | _   -> None
            match refType with
            | None  -> None
            | Some (x1, x2) ->
                match getModuleByName x1 with
                | None      -> None
                | Some md   ->
                    let tas = getTasByName md x2
                    match tas with
                    | None  -> None
                    | Some ts ->
                        let typeDef = ts.GetChild 1

                        resolveReferenceType (Asn1TypeDef typeDef)
        | _     -> Some typeDef

    let rec getTypeByPath (Asn1TypeDef typeDef:Asn1TypeDef) (path:string list) =
        if (typeDef.Type <> asn1Parser.TYPE_DEF) then
            raise (BugErrorException "No a type")

        let children = getTreeChildren(typeDef)
        let typeNodes = children |> List.filter(fun x -> (not (CreateAsn1AstFromAntlrTree.ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
        let typeNode = List.head(typeNodes)

        match path with
        | []        ->
            match bResolveRefType with
            | true  -> resolveReferenceType (Asn1TypeDef typeDef)
            | false -> Some (Asn1TypeDef typeDef)
        | x1::xs    ->
            match typeNode.Type with
            | asn1Parser.CHOICE_TYPE        ->
                let childItems = getChildrenByType(typeNode, asn1Parser.CHOICE_ITEM)
                match childItems |> Seq.tryFind(fun ch -> x1 = ch.GetChild(0).Text) with
                | Some ch ->
                    let typeDef = getChildByType(ch, asn1Parser.TYPE_DEF)
                    getTypeByPath (Asn1TypeDef typeDef) xs
                | None    -> None
            | asn1Parser.SEQUENCE_TYPE        ->
                let childItems =
                    match getOptionChildByType(typeNode, asn1Parser.SEQUENCE_BODY) with
                    | Some(sequenceBody)    -> getChildrenByType(sequenceBody, asn1Parser.SEQUENCE_ITEM)
                    | None                  -> []
                match childItems |> Seq.tryFind(fun ch -> x1 = ch.GetChild(0).Text) with
                | Some ch ->
                    let typeDef = getChildByType(ch, asn1Parser.TYPE_DEF)
                    getTypeByPath (Asn1TypeDef typeDef) xs
                | None    -> None
            | asn1Parser.SEQUENCE_OF_TYPE   when x1="#"     ->
                let typeDef = getChildByType(typeNode, asn1Parser.TYPE_DEF)
                getTypeByPath (Asn1TypeDef typeDef) xs
            |asn1Parser.REFERENCED_TYPE ->
                let actType = resolveReferenceType (Asn1TypeDef typeDef)
                match actType with
                | Some actType  -> getTypeByPath actType path
                | None          -> None
            | _         -> None

    match path with
    | []
    | _::[] -> None
    | x1::x2::xs    ->
        match getModuleByName x1 with
        | None      -> None
        | Some md   ->
            let tas = getTasByName md x2
            match tas with
            | None  -> None
            | Some ts ->
                getTypeByPath (Asn1TypeDef (ts.GetChild 1)) xs
