module ExportToXml

open System
open System.Numerics
open System.IO
open System.Xml.Linq
open FsUtils
open Asn1AcnAst
open Asn1Fold
open Asn1AcnAstUtilFunctions
let private xname s = System.Xml.Linq.XName.Get(s)
let private xnameNs str ns = System.Xml.Linq.XName.Get(str, ns)

let private xsiUrl = "http://www.w3.org/2001/XMLSchema-instance"
let private xsi = XNamespace.Get xsiUrl
let private customWsSchemaLocation = "asn1sccAst.xsd"


let constraintsTag = "Constraints"
let withCompConstraintsTag = "WithComponentConstraints"

let private printUInt (v:UInt32) = XElement(xname "IntegerValue", v)
let private printIntVal (v:BigInteger) = XElement(xname "IntegerValue", v)
let private printRealVal (v:double) = XElement(xname "RealValue", v)
let private printStringVal (v:String) = XElement(xname "StringValue", v)
let private printCharVal (v:char) = XElement(xname "StringValue", v)
let private printEnumVal (v:String) = XElement(xname "EnumValue", v)
let private printBoolVal (v:bool) = XElement(xname "BooleanValue", v)

let private printBitStringVal (v:BitStringValue,_) = XElement(xname "BitStringValue", v.Value)
let private printOctetStringVal (v:OctetStringValue,_) = XElement(xname "OctetStringValue", (v |> List.map(fun b -> b.Value)))

let rec private printSeqOfValue (v:SeqOfValue) = 
    XElement(xname "SequenceOfValue", (v |> List.map PrintAsn1GenericValue ))
and private printSeqValue(v:SeqValue) =
    XElement(xname "SequenceOfValue", 
        (v |> List.map(fun nv -> 
            XElement(xname "NamedValue", 
                XAttribute(xname "Name", nv.name.Value),
                PrintAsn1GenericValue nv.Value                ))))
and private printChoiceValue (v:ChValue) =
    XElement(xname "ChoiceValue", 
                ([v] |> List.map(fun nv -> 
                    XElement(xname "NamedValue", 
                        XAttribute(xname "Name", nv.name.Value),
                        PrintAsn1GenericValue nv.Value                ))))    
and private printRefValue ((md:StringLoc,ts:StringLoc), v:Asn1Value) =    
        XElement(xname "ReferenceValue", 
            XAttribute(xname "Module", md.Value),
            XAttribute(xname "Name", ts.Value),
            XAttribute(xname "Line", ts.Location.srcLine),
            XAttribute(xname "CharPositionInLine", ts.Location.charPos),
            (PrintAsn1GenericValue v))
and private PrintAsn1GenericValue (v:Asn1Value) = 
    match v.kind with
    |IntegerValue(v)         -> printIntVal v.Value
    |RealValue(v)            -> printRealVal v.Value
    |StringValue(v)          -> printStringVal v.Value
    |EnumValue(v)            -> printEnumVal v.Value
    |BooleanValue(v)         -> printBoolVal v.Value
    |BitStringValue(v)       -> printBitStringVal (v, ())
    |OctetStringValue(v)     -> printOctetStringVal (v, ())
    |SeqOfValue(v)           -> printSeqOfValue v
    |SeqValue(v)             -> printSeqValue v
    |ChValue(nv)             -> printChoiceValue nv
    |NullValue   _           -> XElement(xname "NullValue")
    |RefValue ((md,ts), v)   -> printRefValue ((md,ts), v)

let private printGenericConstraint printValue (c:GenericConstraint<'v>)  = 
    foldGenericConstraint
        (fun r1 r2 b s      -> XElement(xname "OR", r1, r2) , s)
        (fun r1 r2 s        -> XElement(xname "AND", r1, r2) , s)
        (fun r s            -> XElement(xname "ALL_EXCEPT", r), s)       
        (fun r1 r2 s        -> XElement(xname "EXCEPT", r1, r2), s)
        (fun r s            -> XElement(xname "ROOTC_CONSTRAINT", r), s)       
        (fun r1 r2 s        -> XElement(xname "ROOTC_CONSTRAINT_WITH_EXTENSION", r1, r2), s)
        (fun v  s           -> printValue v, s)
        c 
        0 |> fst

let private printRangeConstraint0 (printValue:'v1->XElement) printValue2  (c:RangeTypeConstraint<'v1,'v2>) = 
    foldRangeTypeConstraint
        (fun r1 r2 b s      -> XElement(xname "OR", r1, r2), s)
        (fun r1 r2 s        -> XElement(xname "AND", r1, r2) , s)
        (fun r s            -> XElement(xname "ALL_EXCEPT", r), s)       
        (fun r1 r2 s        -> XElement(xname "EXCEPT", r1, r2), s)
        (fun r s            -> XElement(xname "ROOTC_CONSTRAINT", r), s)       
        (fun r1 r2 s        -> XElement(xname "ROOTC_CONSTRAINT_WITH_EXTENSION", r1, r2), s)
        (fun v  s           -> printValue2 v,s)
        (fun v1 v2  b1 b2 s -> XElement(xname "Range", (XElement(xname "a", (printValue v1))), (XElement(xname "b", (printValue v2)))), s)
        (fun v1 b s         -> XElement(xname "GT", (XElement(xname "a", (printValue v1))) ),s )
        (fun v2 b s         -> XElement(xname "LT", (XElement(xname "b", (printValue v2))) ), s)
        c 
        0 |> fst

let private printRangeConstraint printValue (c:RangeTypeConstraint<'v1,'v1>)  = 
    printRangeConstraint0 printValue printValue c 


let private printSizableConstraint printValue (c:SizableTypeConstraint<'v>)  = 
    foldSizableTypeConstraint2
        (fun r1 r2 b s      -> XElement(xname "OR", r1, r2), s)
        (fun r1 r2 s        -> XElement(xname "AND", r1, r2) , s)
        (fun r s            -> XElement(xname "ALL_EXCEPT", r), s)       
        (fun r1 r2 s        -> XElement(xname "EXCEPT", r1, r2), s)
        (fun r s            -> XElement(xname "ROOTC_CONSTRAINT", r), s)       
        (fun r1 r2 s        -> XElement(xname "ROOTC_CONSTRAINT_WITH_EXTENSION", r1, r2), s)
        (fun v  s           -> printValue v,s)
        (fun sc s           -> 
            let sizeCon = printRangeConstraint0 printUInt printUInt sc 
            XElement(xname "SIZE", sizeCon), s)
        c 
        0 |> fst

let private printAlphaConstraint printValue (c:IA5StringConstraint)  = 
    foldStringTypeConstraint2
        (fun r1 r2 b s      -> XElement(xname "OR", r1, r2), s)
        (fun r1 r2 s        -> XElement(xname "AND", r1, r2) , s)
        (fun r s            -> XElement(xname "ALL_EXCEPT", r), s)       
        (fun r1 r2 s        -> XElement(xname "EXCEPT", r1, r2), s)
        (fun r s            -> XElement(xname "ROOTC_CONSTRAINT", r), s)       
        (fun r1 r2 s        -> XElement(xname "ROOTC_CONSTRAINT_WITH_EXTENSION", r1, r2), s)
        (fun v  s         -> (printValue v),s)
        (fun sc s           -> 
            let sizeCon = printRangeConstraint0 printUInt printUInt sc 
            XElement(xname "SIZE", sizeCon), s)
        (fun sc s           -> 
            let alphaCon = printRangeConstraint0 printCharVal printStringVal  sc 
            XElement(xname "ALPHA", alphaCon), s)
        c 
        0 |> fst

let exportChoiceOptionality (opt:Asn1ChoiceOptionality option) =
    match opt with
    | None  -> []
    | Some ChoiceAlwaysAbsent  -> [XAttribute(xname "ALWAYS-ABSENT", "TRUE" ) :> Object]
    | Some ChoiceAlwaysPresent -> [XAttribute(xname "ALWAYS-PRESENT", "TRUE" )]

let exportOptionality (opt:Asn1Optionality option) =
    match opt with
    | None  -> []
    | Some AlwaysAbsent  -> [XAttribute(xname "ALWAYS-ABSENT", "TRUE" ) :> Object]
    | Some AlwaysPresent -> [XAttribute(xname "ALWAYS-PRESENT", "TRUE" )]
    | Some (Optional opt) ->
        match opt.acnPresentWhen, opt.defaultValue with
        | Some (PresenceWhenBool( RelativePath  rp)), Some v -> [XAttribute(xname "Optional", "TRUE" ); XAttribute(xname "present-when", (rp |> Seq.StrJoin ".") ); XElement(xname "Default",(PrintAsn1GenericValue v))]
        | Some (PresenceWhenBool( RelativePath  rp)), None  ->  [XAttribute(xname "Optional", "TRUE" ); XAttribute(xname "present-when", (rp |> Seq.StrJoin ".") )]
        | None, Some v      -> [XElement(xname "Default",(PrintAsn1GenericValue v))]
        | None, None        -> [XAttribute(xname "Optional", "TRUE" );]

let exportChoiceChildPresentWhenCondition (presentConditions:AcnPresentWhenConditionChoiceChild list) =
    let attrValue (aa:AcnPresentWhenConditionChoiceChild) = 
        match aa with
        | PresenceInt (RelativePath path, intVal)   -> (sprintf "%s = %A" (path |> Seq.StrJoin ".") intVal.Value) 
        | PresenceStr (RelativePath path, strVal)   -> (sprintf "%s = %A" (path |> Seq.StrJoin ".") strVal.Value) 
    match presentConditions with
    | []    -> null
    | _     ->
        let attrValue = presentConditions |> List.map attrValue |> Seq.StrJoin " "
        XAttribute(xname "present-when", attrValue)
    

let exportAcnEndianness (a:AcnEndianness option) =
    match a with
    | (Some LittleEndianness)  -> XAttribute(xname "endianness", "little" )
    | (Some BigEndianness)     -> XAttribute(xname "endianness", "big" )       
    | None                     -> null

let exportAcnAligment (a:AcnAligment option) = 
    match a with
    | Some NextByte      -> XAttribute(xname "align-to-next", "byte" )
    | Some NextWord      -> XAttribute(xname "align-to-next", "word" )
    | Some NextDWord     -> XAttribute(xname "align-to-next", "dword" )
    | None               -> null

let exportAcnIntSizeProperty (a:AcnIntSizeProperty option) = 
    match a with
    | None  -> []
    | Some (Fixed             s)   -> [XAttribute(xname "size", s )]
    | Some (IntNullTerminated b)   -> [XAttribute(xname "size", "null-terminated" ); XAttribute(xname "termination-pattern", b )]

let exportAcnIntEncoding (a:AcnIntEncoding option) =
    match a with            
    | Some PosInt           -> [XAttribute(xname "encoding", "pos-int" )]
    | Some TwosComplement   -> [XAttribute(xname "encoding", "twos-complement" )]
    | Some IntAscii         -> [XAttribute(xname "encoding", "ASCII" )]
    | Some BCD              -> [XAttribute(xname "encoding", "BCD" )]
    | None                  -> []

let exportAcnRealEncoding (a:AcnRealEncoding option) =
    match a with            
    | Some IEEE754_32       -> [XAttribute(xname "encoding", "IEEE754-1985-32" )]
    | Some IEEE754_64       -> [XAttribute(xname "encoding", "IEEE754-1985-64" )]
    | None                  -> []

let exportAcnStringEncoding (a:AcnStringEncoding option) = 
    match a with
    | Some StrAscii     -> [XAttribute(xname "encoding", "ASCII" )]
    | None              -> []
let exportAcnStringSizeProperty (a:AcnStringSizeProperty option) =
    match a with
    | Some (StrExternalField  (RelativePath path)) -> [XAttribute(xname "size", (path |> Seq.StrJoin ".") )]
    | Some (StrNullTerminated  b)                  -> [XAttribute(xname "size", "null-terminated" ); XAttribute(xname "termination-pattern", b )]
    | None                                         -> []

let exportSizeableSizeProp (a:RelativePath option) = 
    match a with
    | Some (RelativePath path) -> [XAttribute(xname "size", (path |> Seq.StrJoin ".") )]
    | None                     -> []

let exportChoiceDeterminant (a:RelativePath option) = 
    match a with
    | Some (RelativePath path) -> [XAttribute(xname "determinant", (path |> Seq.StrJoin ".") )]
    | None                     -> []

let exprtRefTypeArgument ((RelativePath path): RelativePath) =
     [XElement(xname "argument", (path |> Seq.StrJoin ".") )]

let exportAcnBooleanEncoding (a:AcnBooleanEncoding option) =
    match a with
    | None                     -> []
    | Some (TrueValue  pat)    -> [XAttribute(xname "true-value", pat.Value )]
    | Some (FalseValue pat)    -> [XAttribute(xname "false-value", pat.Value )]

let exportAcnNullType (a:StringLoc option) =
    match a with
    | None        -> []
    | Some pat    -> [XAttribute(xname "pattern", pat.Value )]
                         
let exportAcnParameter (a:AcnParameter) =
    XElement(xname "AcnParameter", 
        XAttribute(xname "Id", a.id.AsString),
        XAttribute(xname "Name", a.name),
        XAttribute(xname "Type", (a.asn1Type.ToString())))    

let private exportType (t:Asn1Type) = 
    Asn1Fold.foldType
        (fun ti us -> 
                    XElement(xname "INTEGER",
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportAcnEndianness ti.acnProperties.endiannessProp),
                        (exportAcnIntSizeProperty ti.acnProperties.sizeProp),
                        (exportAcnIntEncoding ti.acnProperties.encodingProp),
                        XElement(xname constraintsTag, ti.cons |> List.map (printRangeConstraint printIntVal) ),
                        XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printRangeConstraint printIntVal ))
                        ), us )
        (fun ti us -> XElement(xname "REAL",
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportAcnEndianness ti.acnProperties.endiannessProp),
                        (exportAcnRealEncoding ti.acnProperties.encodingProp),
                        XElement(xname constraintsTag, ti.cons |> List.map(printRangeConstraint printRealVal)),
                        XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printRangeConstraint printRealVal ))
                        ), us )
        (fun ti us -> XElement(xname "IA5String",
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportAcnStringSizeProperty ti.acnProperties.sizeProp),
                        (exportAcnStringEncoding ti.acnProperties.encodingProp),
                        XElement(xname constraintsTag, ti.cons |> List.map(printAlphaConstraint printStringVal )),
                        XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printAlphaConstraint printStringVal ))
                        ), us )
        (fun ti us -> XElement(xname "NumericString",
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportAcnStringSizeProperty ti.acnProperties.sizeProp),
                        (exportAcnStringEncoding ti.acnProperties.encodingProp),
                        XElement(xname constraintsTag, ti.cons |> List.map(printAlphaConstraint printStringVal )),
                        XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printAlphaConstraint printStringVal ))
                        ), us )
        (fun ti us -> XElement(xname "OCTET_STRING",
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportSizeableSizeProp ti.acnProperties.sizeProp),
                        XElement(xname constraintsTag, ti.cons |> List.map(printSizableConstraint printOctetStringVal )),
                        XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printSizableConstraint printOctetStringVal ))
                        ), us )
        (fun ti us -> XElement(xname "NULL", 
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportAcnNullType ti.acnProperties.encodingPattern)),us )
        (fun ti us -> XElement(xname "BIT_STRING",
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportSizeableSizeProp ti.acnProperties.sizeProp),
                        XElement(xname constraintsTag, ti.cons |> List.map(printSizableConstraint printBitStringVal )),
                        XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printSizableConstraint printBitStringVal ))
                        ), us )
        (fun ti us -> XElement(xname "BOOLEAN",
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportAcnBooleanEncoding ti.acnProperties.encodingPattern),
                        XElement(xname constraintsTag, ti.cons |> List.map(printGenericConstraint printBoolVal )),
                        XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printGenericConstraint printBoolVal ))
                        ), us )
        (fun ti us -> XElement(xname "Enumerated",
                        (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                        (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                        (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                        (exportAcnEndianness ti.acnProperties.endiannessProp),
                        (exportAcnIntSizeProperty ti.acnProperties.sizeProp),
                        (exportAcnIntEncoding ti.acnProperties.encodingProp),
                        (XAttribute(xname "encode-values", ti.encodeValues)),
                        XElement(xname "Items", ti.items |> List.map(fun c ->  
                                                                XElement(xname "Item", 
                                                                    XAttribute(xname "Name", c.Name.Value), 
                                                                    XAttribute(xname "Value", c.definitionValue),   
                                                                    XAttribute(xname "Line", c.Name.Location.srcLine),
                                                                    XAttribute(xname "acnEncodeValue", c.acnEncodeValue),
                                                                    XAttribute(xname "CharPositionInLine", c.Name.Location.charPos)
                                                                ))),
                        XElement(xname constraintsTag, ti.cons |> List.map(printGenericConstraint printEnumVal )),
                        XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printGenericConstraint printEnumVal ))
                        ), us )
        (fun ti nc us -> XElement(xname "SEQUENCE_OF",
                            (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                            (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                            (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                            (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                            (exportSizeableSizeProp ti.acnProperties.sizeProp),
                            XElement(xname constraintsTag, ti.cons |> List.map(printSizableConstraint printSeqOfValue )),
                            XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printSizableConstraint printSeqOfValue )),
                            nc), us )
        (fun ti children us -> XElement(xname "SEQUENCE",
                                (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                                (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                                (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                                (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                                children,
                                XElement(xname constraintsTag, ti.cons |> List.map(printGenericConstraint printSeqValue )),
                                XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printGenericConstraint printSeqValue ))
                                ), us )
        (fun ch nt us -> XElement(xname "SEQUENCE_COMPONENT",
                            XAttribute(xname "Name", ch.Name.Value),
                            XAttribute(xname "Line", ch.Name.Location.srcLine),
                            XAttribute(xname "CharPositionInLine", ch.Name.Location.charPos),
                            (exportOptionality ch.Optionality ),
                            nt), us )
        (fun ch us -> 
            match ch.Type with
            | AcnInteger  (a)        -> 
                XElement(xname "ACN_COMPONENT", 
                    XAttribute(xname "Id", ch.id.AsString),
                    XAttribute(xname "Name", ch.Name.Value),
                    XAttribute(xname "Type", "INTEGER"), 
                    (exportAcnEndianness a.acnProperties.endiannessProp),
                    (exportAcnIntSizeProperty a.acnProperties.sizeProp),
                    (exportAcnIntEncoding a.acnProperties.encodingProp),
                    (exportAcnAligment a.acnAligment)), us 
            | AcnNullType (a)        -> 
                XElement(xname "ACN_COMPONENT", 
                    XAttribute(xname "Id", ch.id.AsString),
                    XAttribute(xname "Name", ch.Name.Value),
                    XAttribute(xname "Type", "NULL"),    
                    (exportAcnNullType a.acnProperties.encodingPattern),
                    (exportAcnAligment a.acnAligment)), us 

            | AcnReferenceToEnumerated (a)        -> 
                XElement(xname "ACN_COMPONENT", 
                    XAttribute(xname "Id", ch.id.AsString),
                    XAttribute(xname "Name", ch.Name.Value),
                    XAttribute(xname "Type", "Enumerated"),    
                    (exportAcnAligment a.acnAligment),
                    (exportAcnEndianness a.enumerated.acnProperties.endiannessProp),
                    (exportAcnIntSizeProperty a.enumerated.acnProperties.sizeProp),
                    (exportAcnIntEncoding a.enumerated.acnProperties.encodingProp),
                    XElement(xname "Items", a.enumerated.items |> List.map(fun c ->  XElement(xname "Item", XAttribute(xname "Name", c.Name.Value), XAttribute(xname "Value", c.definitionValue))   )),
                    XElement(xname constraintsTag, a.enumerated.cons |> List.map(printGenericConstraint printEnumVal )),
                    XElement(xname withCompConstraintsTag, a.enumerated.withcons |> List.map(printGenericConstraint printEnumVal )) ), us 

            | AcnReferenceToIA5String (a)        -> 
                XElement(xname "ACN_COMPONENT", 
                    XAttribute(xname "Id", ch.id.AsString),
                    XAttribute(xname "Name", ch.Name.Value),
                    XAttribute(xname "Type", "IA5String"),    
                    (exportAcnAligment a.acnAligment) ), us 


            | AcnBoolean  (a)       -> 
                XElement(xname "ACN_COMPONENT", 
                    XAttribute(xname "Id", ch.id.AsString),
                    XAttribute(xname "Name", ch.Name.Value),
                    XAttribute(xname "Type", "BOOLEAN"), 
                    (exportAcnBooleanEncoding a.acnProperties.encodingPattern),
                    (exportAcnAligment a.acnAligment)), us )
        (fun ti children us -> XElement(xname "CHOICE",
                                (XAttribute(xname "acnMaxSizeInBits", ti.acnMaxSizeInBits )),
                                (XAttribute(xname "acnMinSizeInBits", ti.acnMinSizeInBits )),
                                (XAttribute(xname "uperMaxSizeInBits", ti.uperMaxSizeInBits )),
                                (XAttribute(xname "uperMinSizeInBits", ti.uperMinSizeInBits )),
                                (exportChoiceDeterminant ti.acnProperties.enumDeterminant),
                                children,
                                XElement(xname constraintsTag, ti.cons |> List.map(printGenericConstraint printChoiceValue )),
                                XElement(xname withCompConstraintsTag, ti.withcons |> List.map(printGenericConstraint printChoiceValue ))
                                ), us )
        (fun ch nt us -> XElement(xname "CHOICE_ALTERNATIVE",
                            XAttribute(xname "Name", ch.Name.Value),
                            XAttribute(xname "Line", ch.Name.Location.srcLine),
                            XAttribute(xname "CharPositionInLine", ch.Name.Location.charPos),
                            XAttribute(xname "PresentWhenName", ch.present_when_name),
                            XAttribute(xname "AdaName", ch.ada_name),
                            XAttribute(xname "CName", ch.c_name),
                            (exportChoiceOptionality ch.Optionality ),
                            (exportChoiceChildPresentWhenCondition ch.acnPresentWhenConditions),
                            nt), us )
        (fun ref nt us -> XElement(xname "REFERENCE_TYPE",
                            XAttribute(xname "Module", ref.modName.Value),
                            XAttribute(xname "TypeAssignment", ref.tasName.Value),
                            (match ref.acnArguments with
                             | []   -> []
                             | args -> [XElement(xname "AcnArguments", (args |> List.map exprtRefTypeArgument) )]),
                            nt), us )
        (fun t nk us -> XElement(xname "Asn1Type",
                            XAttribute(xname "id", t.id.AsString),
                            XAttribute(xname "Line", t.Location.srcLine),
                            XAttribute(xname "CharPositionInLine", t.Location.charPos),
                            XAttribute(xname "ParameterizedTypeInstance", t.parameterizedTypeInstance),
                            (match t.inheritInfo with
                             |Some ts -> XAttribute(xname "tasInfoModule",ts.modName)
                             |None     -> null
                            ),
                            (match t.inheritInfo with
                             |Some ts -> XAttribute(xname "tasInfoName",ts.tasName)
                             |None     -> null
                            ),
                            (exportAcnAligment t.acnAligment),
                            (match t.acnParameters with
                            | []    -> []
                            | prms  -> [XElement(xname "AcnParameters", (prms|> List.map exportAcnParameter) )]),
                            nk), us )
        t
        0 |> fst



let private exportTas (tas:TypeAssignment) =
    XElement(xname "TypeAssignment",
        XAttribute(xname "Name", tas.Name.Value),
        XAttribute(xname "Line", tas.Name.Location.srcLine),
        XAttribute(xname "CharPositionInLine", tas.Name.Location.charPos),
        (exportType tas.Type)
    )

let private exportVas (vas:ValueAssignment) =
    XElement(xname "ValueAssignment",
        XAttribute(xname "Name", vas.Name.Value),
        XAttribute(xname "Line", vas.Name.Location.srcLine),
        XAttribute(xname "CharPositionInLine", vas.Name.Location.charPos),
        (exportType vas.Type),
        (PrintAsn1GenericValue vas.Value)
    )



let private exportModule (m:Asn1Module) =
    let handleImpotModule (im:Asn1Ast.ImportedModule) =
        XElement(xname "ImportedModule",
            XAttribute(xname "ID", im.Name.Value),
            (XElement(xname "ImportedTypes",
                im.Types |> List.map(fun et -> XElement(xname "ImportedType", XAttribute(xname "Name", et.Value))))),
            (XElement(xname "ImportedValues",
                im.Values |> List.map(fun et -> XElement(xname "ImportedValue", XAttribute(xname "Name", et.Value))))))

    XElement(xname "Module",
        XAttribute(xname "Name", m.Name.Value),
        XAttribute(xname "Line", m.Name.Location.srcLine),
        XAttribute(xname "CharPositionInLine", m.Name.Location.charPos),
        (XElement(xname "ExportedTypes",
            m.ExportedTypes |> List.map(fun et -> XElement(xname "ExportedType", XAttribute(xname "Name", et))))),
        (XElement(xname "ExportedValues",
            m.ExportedValues |> List.map(fun et -> XElement(xname "ExportedValue", XAttribute(xname "Name", et))))),
        (XElement(xname "ImportedModules", m.Imports |> List.map handleImpotModule)),
        XElement(xname "TypeAssignments", m.TypeAssignments |> List.map  exportTas),
        XElement(xname "ValueAssignments",m.ValueAssignments |> List.map  exportVas)
    )

let private exportAcnDependencyKind (d:AcnDependencyKind) =
    match d with                   
    | AcnDepIA5StringSizeDeterminant        -> XElement(xname "SizeDependency")
    | AcnDepSizeDeterminant        -> XElement(xname "SizeDependency")
    | AcnDepRefTypeArgument prm    -> XElement(xname "RefTypeArgumentDependency", XAttribute(xname "prmId", prm.id.AsString))
    | AcnDepPresenceBool           -> XElement(xname "PresenseBoolDependency")
    | AcnDepPresence   _           -> XElement(xname "ChoicePresenseDependency")
    | AcnDepPresenceStr   _           -> XElement(xname "ChoicePresenseStrDependency")
    | AcnDepChoiceDeteterminant _  -> XElement(xname "ChoiceEnumDependency")
                                   
let private exportAcnDependency (d:AcnDependency) =
    XElement(xname "AcnDependency",
        XAttribute(xname "Asn1TypeID", d.asn1Type.AsString),
        XAttribute(xname "DeterminantId", d.determinant.id.AsString),
        (exportAcnDependencyKind d.dependencyKind)
    )

let  exportAcnDependencies (deps:AcnInsertedFieldDependencies) =
    XElement(xname "AcnDependencies",
        (deps.acnDependencies |> List.map exportAcnDependency)
    )


let exportFile (r:AstRoot) (deps:AcnInsertedFieldDependencies) (fileName:string) =
    let wsRoot =
        XElement(xname "AstRoot",
            XAttribute(XNamespace.Xmlns + "xsi", xsi),
            XAttribute(xnameNs "noNamespaceSchemaLocation" xsiUrl, customWsSchemaLocation),
            XAttribute(xname "rename_policy", (sprintf "%A" r.args.renamePolicy)),
            r.Files |>
            List.map(fun f ->
                    XElement(xname "Asn1File",
                        XAttribute(xname "FileName", f.FileName),
                        XElement(xname "Modules",
                            f.Modules |> List.map  exportModule)
                    )),
            (exportAcnDependencies deps)
            )


    let dec = new XDeclaration("1.0", "utf-8", "true")
    let doc =new XDocument(dec)
    doc.AddFirst wsRoot
    doc.Save(fileName)
