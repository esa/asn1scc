module LoadAcnInfo

open System.Linq
open System.Numerics
open Asn1Ast
open Antlr.Acn
open Antlr.Runtime.Tree
open Antlr.Runtime

open FsUtils



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN CONSTANTS DEFINITION /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type AcnIntExpr =
    | IntegerExpr       of AcnIntegerConstant
    | SumExpr           of AcnIntExpr*AcnIntExpr
    | MinExpr           of AcnIntExpr*AcnIntExpr
    | MulExpr           of AcnIntExpr*AcnIntExpr
    | DivExpr           of AcnIntExpr*AcnIntExpr
    | ModExpr           of AcnIntExpr*AcnIntExpr
    | PowExpr           of AcnIntExpr*AcnIntExpr
    | UnMinExp          of AcnIntExpr

and AcnIntegerConstant =
    | IntConst of IntLoc
    | RefConst of StringLoc       //reference to other constant

type AcnConstant = {
    Name  : StringLoc
    Value : AcnIntExpr
}

let rec EvaluateConstant (constants:AcnConstant list) intConstant =
    match intConstant with
    | IntConst(a)   -> a.Value
    | RefConst(consLookUp)  ->
        match constants |> Seq.tryFind(fun c-> c.Name.Value = consLookUp.Value) with
        |None       -> raise(SemanticError(consLookUp.Location, (sprintf "Unknown symbol '%s'" consLookUp.Value)))
        |Some(cn)   -> EvaluateAcnIntExpression constants cn.Value

and  EvaluateAcnIntExpression (constants:AcnConstant list) acnExpr = 
    match  acnExpr with
    | IntegerExpr(consta)   -> EvaluateConstant constants consta
    | SumExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) + (EvaluateAcnIntExpression constants exp2)
    | MinExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) - (EvaluateAcnIntExpression constants exp2)
    | MulExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) * (EvaluateAcnIntExpression constants exp2)
    | DivExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) / (EvaluateAcnIntExpression constants exp2)
    | ModExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) % (EvaluateAcnIntExpression constants exp2)
    | PowExpr(exp1,exp2)    -> 
        System.Numerics.BigInteger.Pow(EvaluateAcnIntExpression constants exp1, int (EvaluateAcnIntExpression constants exp2))
    | UnMinExp(exp1)        -> -(EvaluateAcnIntExpression constants exp1) 


type AntlrParserResult = {
    rootItem    : ITree
    fileName    : string
    tokens      : IToken array
}


let CreateLongField(t:ITree) = t.Children |> List.map(fun x -> x.Text) |> RelativePath



//Search in the antlr acn AST for a specific module, typeassigment and
//return the ACN parameters as a string list
let GetParams (files:AntlrParserResult list) modName tasName  =
    let GetParamsAux (asn1File:ITree) =
        match  asn1File.Children |> Seq.tryFind(fun x -> x.GetChild(0).Text = modName)   with
        | None      -> []
        | Some(md)  ->
            match md.GetChildrenByType(acnParser.TYPE_ENCODING) |> Seq.tryFind(fun x -> x.GetChild(0).Text = tasName)   with
            | None  -> []
            | Some(tas) ->
                match tas.GetOptChild(acnParser.PARAM_LIST) with
                | None          -> []
                | Some(prmLst)  -> prmLst.Children |> List.map(fun p -> p.GetChild(1).Text)
    files |>  List.map (fun pr -> GetParamsAux pr.rootItem) |> List.collect(fun x -> x)


let CheckModuleName (r:AstRoot) (modNameL:StringLoc)  =
    let extToThrow = SemanticError(modNameL.Location, (sprintf "No ASN.1 module is defined with name '%s'" modNameL.Value))
    CheckExists modNameL (r.Modules |> Seq.map(fun x -> x.Name)) extToThrow
    r.GetModuleByName modNameL

let gatTasByName (r:AstRoot) (m:Asn1Module) (tasNameL:StringLoc) =
    match m.TypeAssignments |> Seq.tryFind(fun t -> t.Name.Value = tasNameL.Value) with
    | None      -> raise(SemanticError(tasNameL.Location, (sprintf "No ASN.1 Type Assignment is defined with name '%s'" tasNameL.Value)))
    | Some tas  -> tas


let CreateAcnAsn1Type (t:ITree) (r:AstRoot) =
    match t.Type with
    | acnParser.INTEGER         -> AcnAsn1Type.AcnInteger t.Location
    | acnParser.BOOLEAN         -> AcnAsn1Type.AcnBoolean t.Location
    | acnParser.NULL            -> AcnAsn1Type.AcnNullType t.Location
    | acnParser.REFERENCED_TYPE -> 
        let mdName, tsName = 
            match t.Children with
            | first::[]             -> first.GetAncestor(acnParser.MODULE_DEF).GetChild(0).TextL,first.TextL
            | first::sec::[]        -> first.TextL,sec.TextL
            | _                     -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnAsn1Type 1"))
        let m = CheckModuleName r mdName 
        gatTasByName r m tsName |> ignore
        AcnAsn1Type.AcnRefType(mdName, tsName)
    | _                         -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnAsn1Type 2"))



let BalladerProperties = [acnParser.PRESENT_WHEN; acnParser.ALIGNTONEXT;]

let rec AllowedPropertiesPerType (r:AstRoot) = function
    | Integer           -> [acnParser.ENCODING; acnParser.SIZE; acnParser.ENDIANNES; acnParser.MAPPING_FUNCTION]
    | Real              -> [acnParser.ENCODING; acnParser.ENDIANNES]
    | IA5String         -> [acnParser.ENCODING; acnParser.SIZE]
    | NumericString     -> [acnParser.ENCODING; acnParser.SIZE]
    | OctetString       -> [acnParser.SIZE]
    | NullType          -> [acnParser.PATTERN]
    | BitString         -> [acnParser.SIZE]
    | Boolean           -> [acnParser.TRUE_VALUE; acnParser.FALSE_VALUE]
    | Enumerated(_)     -> [acnParser.ENCODING; acnParser.SIZE; acnParser.ENCODE_VALUES; acnParser.ENDIANNES]
    | SequenceOf(_)     -> [acnParser.SIZE]
    | Sequence(_)       -> []
    | Choice(_)         -> [acnParser.DETERMINANT]
    | ReferenceType rf  -> 
        let baseType = GetBaseTypeByName rf.modName rf.tasName r
        AllowedPropertiesPerType r baseType.Kind


let MandatoryAcnPropertiesPerType asn1Kind : List<int> =
    match asn1Kind with
    | Integer           -> [acnParser.ENCODING; acnParser.SIZE; ]   //ADJUST = 0, ENDIANNES=big
    | Real              -> [acnParser.ENCODING; ]   //ENDIANNES=big
    | IA5String         -> []     // pointing to a field
    | NumericString     -> []     // pointing to a field
    | OctetString       -> [acnParser.SIZE]     // pointing to a field
    | NullType          -> []                   // pattern = ''
    | BitString         -> [acnParser.SIZE]     // pointing to a field
    | Boolean           -> []                   // true-value = '1'B
    | Enumerated(_)     -> [acnParser.ENCODING; acnParser.SIZE; ]
    | SequenceOf(_)     -> [acnParser.SIZE]     // pointing to a field
    | Sequence(_)       -> []
    | Choice(_)         -> []
    | ReferenceType(_)  -> []
    
let PropID_to_Text = function
    | acnParser.PRESENT_WHEN    -> "present-when"
    | acnParser.ALIGNTONEXT     -> "align-to-next"
    | acnParser.ENCODING        -> "encoding"
    | acnParser.SIZE            -> "size"
    | acnParser.ENDIANNES       -> "endianness"
    | acnParser.PATTERN         -> "pattern"
    | acnParser.TRUE_VALUE      -> "true-value"
    | acnParser.FALSE_VALUE     -> "false-value"
    | acnParser.ENCODE_VALUES   -> "encode-values"
    | acnParser.DETERMINANT     -> "determinant"
    | acnParser.MAPPING_FUNCTION   -> "mapping-function"  
    | _                         -> raise(BugErrorException "")


let CheckConsistencyOfAsn1TypeWithAcnProperty (r:AstRoot) asn1Kind (t:ITree) =
    match BalladerProperties |> Seq.exists(fun x -> x = t.Type) with
    | true  -> ()   //ballader propery, so it can be applied to any type
    | false ->
        //1. Is it allowed
        match (AllowedPropertiesPerType r asn1Kind) |> Seq.exists(fun x -> x = t.Type) with
        | false     -> raise(SemanticError(t.Location, sprintf "Acn property '%s' cannot be applied here" t.Text))
        | true      -> ()



let getAcnAligmentProperty (t:ITree) =
    match t.Type with
    | acnParser.ALIGNTONEXT ->
        match t.GetChild(0).Type with 
        | acnParser.BYTE                -> Some  NextByte
        | acnParser.WORD                -> Some  NextWord
        | acnParser.DWORD               -> Some  NextDWord
        | _                             -> raise(BugErrorException("getAcnAligmentProperty"))
    | _                                 -> None

let getAcnIntEncoding (t:ITree) =
    match t.Type with
    | acnParser.ALIGNTONEXT ->
        match t.GetChild(0).Type with
        | acnParser.POS_INT             -> Some PosInt
        | acnParser.TWOSCOMPLEMENT      -> Some TwosComplement
        | acnParser.BCD                 -> Some BCD
        | acnParser.ASCII               -> Some IntAscii
        | acnParser.IEEE754_1985_32     -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for integer type") )
        | acnParser.IEEE754_1985_64     -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for integer type") )
        | _                             -> raise(BugErrorException("getAcnIntEncoding"))
    | _                                 -> None

let getAcnRealEncoding (t:ITree) =
    match t.Type with
    | acnParser.ALIGNTONEXT ->
        match t.GetChild(0).Type with
        | acnParser.POS_INT             -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for integer type") )
        | acnParser.TWOSCOMPLEMENT      -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for integer type") )
        | acnParser.BCD                 -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for integer type") )
        | acnParser.ASCII               -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for integer type") )
        | acnParser.IEEE754_1985_32     -> Some IEEE754_32
        | acnParser.IEEE754_1985_64     -> Some IEEE754_64
        | _                             -> raise(BugErrorException("getAcnIntEncoding"))
    | _                                 -> None

let getAcnStringEncoding (t:ITree) =
    match t.Type with
    | acnParser.ALIGNTONEXT ->
        match t.GetChild(0).Type with
        | acnParser.POS_INT             -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for string type") )
        | acnParser.TWOSCOMPLEMENT      -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for string type") )
        | acnParser.BCD                 -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for string type") )
        | acnParser.ASCII               -> Some StrAscii
        | acnParser.IEEE754_1985_32     -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for string type") )
        | acnParser.IEEE754_1985_64     -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for string type") )
        | _                             -> raise(BugErrorException("getAcnStringEncoding"))
    | _                                 -> None

let GetActualString (str:string) = 
    let strVal = str.Substring(1)
    strVal.Remove(strVal.Length-2).Replace("\r", "").Replace("\n", "").Replace("\t", "").Replace(" ", "")

let handlNullTerminate (t:ITree) =
    match t.Children |> Seq.tryFind(fun x -> x.Type = acnParser.TERMINATION_PATTERN) with
    | None  -> byte 0
    | Some tp   ->
        let bitPattern = GetActualString (tp.GetChild(0).Text)
        match tp.GetChild(0).Type with
        | acnParser.BitStringLiteral    ->
            match bitPattern.Length <> 8 with
            | true  -> raise(SemanticError(tp.Location, sprintf "ternination-patern value must be a byte"  ))
            | false ->
                bitPattern.ToCharArray() |> 
                Seq.fold(fun (p,cs) c -> if c='0' then (p/2,cs) else (p/2,p+cs) ) (128, 0) 
                |> snd |> byte
        | acnParser.OctectStringLiteral ->
            match bitPattern.Length <> 2 with
            | true  -> raise(SemanticError(tp.Location, sprintf "ternination-patern value must be a byte"  ))
            | false ->
                System.Byte.Parse(bitPattern, System.Globalization.NumberStyles.AllowHexSpecifier)
        | _     ->  raise(BugErrorException("HandleAcnProperty: Neither BitStringLiteral or OctectStringLiteral"))

let getAcnIntSizeProperty(r:AstRoot) (t:ITree) =
    match t.Type with
    | acnParser.SIZE        ->
        match t.GetChild(0).Type with
        | acnParser.NULL_TERMINATED     -> 
            let nullByte = handlNullTerminate t
            Some (IntNullTerminated nullByte)
        | acnParser.INT                 -> Some (Fixed (t.GetChild(0)).BigInt)
        | acnParser.UID                 -> Some (Fixed r.acnConstants.[(t.GetChild(0)).Text])
        | _                             -> raise(BugErrorException("getAcnIntSizeProperty"))
    | _                                 -> None

let getAcnStringSizeProperty  (r:AstRoot) (t:ITree) =
    let GetActualString (str:string) = 
        let strVal = str.Substring(1)
        strVal.Remove(strVal.Length-2).Replace("\r", "").Replace("\n", "").Replace("\t", "").Replace(" ", "")
    match t.Type with
    | acnParser.SIZE        ->
        match t.GetChild(0).Type with
        | acnParser.NULL_TERMINATED     -> 
            let nullByte = handlNullTerminate t
            Some (StrNullTerminated nullByte)
        | acnParser.LONG_FIELD          -> Some (StrExternalField (CreateLongField (t.GetChild(0))))
        | acnParser.INT                 -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for string type") )
        | acnParser.UID                 -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for string type") )
        | _                             -> raise(BugErrorException("getAcnStringSizeProperty"))
    | _                                 -> None

let getAcnSizeableSizeProperty  (r:AstRoot) (t:ITree) =
    match t.Type with
    | acnParser.SIZE        ->
        match t.GetChild(0).Type with
        | acnParser.NULL_TERMINATED     -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for octet/bit/sequence of type") )
        | acnParser.LONG_FIELD          -> Some (CreateLongField (t.GetChild(0)))
        | acnParser.INT                 -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for octet/bit/sequence of type") )
        | acnParser.UID                 -> raise(SemanticError (t.GetChild(0).Location, "Invalid value for octet/bit/sequence of type") )
        | _                             -> raise(BugErrorException("getAcnStringSizeProperty"))
    | _                                 -> None



let getAcnEndianness (t:ITree) =
    match t.Type with
    | acnParser.ENDIANNES               -> 
        match t.GetChild(0).Type with 
        | acnParser.BIG                 -> Some BigEndianness
        | acnParser.LITTLE              -> Some LittleEndianness
        | _                             -> raise(BugErrorException("getAcnEndianness"))
    | _                                 -> None

let getAcnEncodeValues (t:ITree) =
    match t.Type with
    | acnParser.ENCODE_VALUES           -> Some true
    | _                                 -> None

let GetEnumValuesResetInAcn (enumItems:NamedItem list) (t:ITree)   =
    match t.GetOptChild(acnParser.CHILDREN_ENC_SPEC) with
    | None                  -> None
    | Some(childrenList)    -> 
        let errLoc = t.Location
        let enmChildren = 
            childrenList.Children |> 
            List.map(fun x -> match x.Type with
                                |acnParser.CHILD -> 
                                let name = x.GetChild(0).TextL
                                let errLoc = x.GetChild(0).Location
                                let chEncSpec = x.GetChildByType(acnParser.ENCODING_SPEC) 
                                let encValue = match chEncSpec.GetOptChild acnParser.ENCODING_PROPERTIES with
                                                | None -> raise(SemanticError(errLoc, "Expecting integer value"))
                                                | Some(propLst)    ->
                                                    match propLst.Children with
                                                    | []        -> raise(SemanticError(errLoc, "Expecting integer value"))
                                                    | prop::_   -> match prop.Type with
                                                                    | acnParser.INT -> prop.BigIntL
                                                                    | _             -> raise(SemanticError(errLoc, "Expecting integer value"))
                                ( name, encValue)
                                | _  -> raise(SemanticError(x.Location, "Expecting an enumerated name")) )

        // 1 make sure that the two lists match
        let enmList1 = enumItems |> List.map(fun x -> x.Name) 
        let enmList2 = enmChildren |> List.map fst 
        CompareLists enmList1 enmList2 (fun x -> SemanticError(x.Location, sprintf "Unexpected value '%s' " x.Value))
        CompareLists enmList2 enmList1 (fun x -> SemanticError(errLoc, sprintf "Missing value '%s' " x.Value))
        //2 make sure that there is no duplicate value
        enmChildren |> List.map snd |> CheckForDuplicates
        let redefVals = enmChildren |> List.map(fun (nm, vl) ->  {EnumeratedRedefinedValue.enumName = nm.Value; enumValue = vl.Value}) 
        Some redefVals

let getNullEncodingPattern (t:ITree) =
    match t.Type with
    | acnParser.PATTERN                 -> 
        let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
        Some v
    | _                                 -> None

let getBoolEncodingPattern (t:ITree) =
    match t.Type with
    | acnParser.TRUE_VALUE              -> 
        let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
        Some (TrueValue(v))
    | acnParser.FALSE_VALUE             -> 
        let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
        Some (FalseValue(v))
    | _                                 -> None


let getPresentWhenCondtition (r:AstRoot) (t:ITree) =
    match t.Type with
    | acnParser.PRESENT_WHEN            -> 
        match t.Type with
        | acnParser.LONG_FIELD  -> Some(PresenceBool  (CreateLongField(t)))
        | acnParser.EQUAL       -> 
            Some (PresenceInt(CreateLongField(t.GetChild(0)), r.acnConstants.[(t.GetChild(1)).Text]))
        | acnParser.PRESENT_WHEN_STR_EQUAL ->
            Some (PresenceStr(CreateLongField(t.GetChild(0)), (t.GetChild(1).Text.Replace("\"",""))))
        | _                     -> raise(BugErrorException("getPresentWhenCondtition"))
    | _                                 -> None



let GetArgumentList (allAcnFiles: AntlrParserResult list) (t:ITree) childTypeKind =
    match t.GetOptChild(acnParser.ARGUMENTS) with
    | None            -> []
    | Some(argList)   -> 
        //I have arguments ==> I have to be a ref type.
        let prmNames = match childTypeKind with
                        | ReferenceType rf  ->  GetParams allAcnFiles rf.modName.Value rf.tasName.Value
                        | _                        -> raise(SemanticError(t.Location, "Only reference types can have arguments" ))
        if argList.Children.Length <> prmNames.Length then
            raise(SemanticError(t.Location, sprintf "Expecting %d arguments" prmNames.Length ))
        let zipped = List.zip argList.Children prmNames
        zipped |> List.map(fun (a, prmName) -> CreateLongField a)


let rec CreateType (allAcnFiles: AntlrParserResult list) (thisAcnFile: AntlrParserResult) (r:AstRoot) (m:Asn1Module) (parentType : Asn1Type option)  (t:Asn1Type) (tTree:ITree) : Asn1Type =
    let mergeTerminationPatternWithNullTerminated (props:ITree list) : ITree list =
        let ret = props |> List.filter(fun x -> x.Type <> acnParser.TERMINATION_PATTERN)
        match props |> Seq.tryFind(fun p -> p.Type = acnParser.TERMINATION_PATTERN) with
        | None      -> ret
        | Some tp   ->
            match props |> Seq.tryFind(fun p -> p.Type = acnParser.SIZE) with
            | None  -> raise(SemanticError(tp.Location, sprintf "termination-pattern can appear only if acn size NULL-TERMINATED is present" ))
            | Some sz -> 
                match sz.Children |> Seq.tryFind(fun p -> p.Type = acnParser.NULL_TERMINATED) with
                | None      -> raise(SemanticError(tp.Location, sprintf "termination-pattern can appear only if acn size NULL-TERMINATED is present" ))
                | Some _    -> 
                    sz.AddChild(tp)
                    ret


    let props = 
        match tTree.GetOptChild(acnParser.ENCODING_PROPERTIES) with
        | None              -> []
        | Some(propList)    -> propList.Children


    //check each property against the asn1 type
    props |> Seq.iter(fun x -> CheckConsistencyOfAsn1TypeWithAcnProperty r t.Kind x)
    //check for duplicate ACN properties
    props |> Seq.map(fun x -> x.TextL) |> CheckForDuplicates 

    let props = mergeTerminationPatternWithNullTerminated props


    //check than no mandatory properties are missing
    let propsNoBallader = props |> List.filter(fun x -> not(BalladerProperties |> List.contains x.Type ) )
    match propsNoBallader with
    | []    -> ()   // uper mode
    | x::xs -> 
        let errLoc = x.Location
        let mandProps = MandatoryAcnPropertiesPerType t.Kind 
        let missing = mandProps |> List.filter(fun x -> not(propsNoBallader |> List.exists(fun y -> y.Type=x) ))
        match missing with
        | []    -> ()
        | _     -> raise(SemanticError(errLoc, sprintf "missing properties : %s" (missing |> Seq.map PropID_to_Text |>Seq.StrJoin ", ") ))

    let getFirstOrNone  func =
        match props |> List.choose func with
        | xx::_     -> Some xx
        | []        -> None

    let acnAligment = getFirstOrNone getAcnAligmentProperty
    let presenceConditions = props |> List.choose (getPresentWhenCondtition r)
    
    match presenceConditions with
    | []    -> ()
    | _     ->
        match parentType with
        | Some parentType   ->
            match parentType.Kind with
            | Sequence _    
            | Choice  _     -> ()
            | _             -> raise (SemanticError(tTree.Location, "present-when' attribute can be applied to optional components only"))
        | None              -> raise (SemanticError(tTree.Location, "present-when' attribute can be applied to optional components only"))
    let newT = 
        match t.Kind with
        | Integer   -> 
            let props = 
                {
                    IntegerAcnProperties.encodingProp       = getFirstOrNone getAcnIntEncoding
                    sizeProp                                = getFirstOrNone (getAcnIntSizeProperty r)
                    endiannessProp                          = getFirstOrNone getAcnEndianness
                    acnAligment                             = acnAligment
                    presentWhen                             = presenceConditions
                }
            
            {t with acnProperties = Some (IntegerAcnProperties props)}
        | Enumerated enmItems   ->
            let props   =
                {
                    EnumeratedAcnProperties.encodingProp    = getFirstOrNone getAcnIntEncoding
                    sizeProp                                = getFirstOrNone (getAcnIntSizeProperty r)
                    endiannessProp                          = getFirstOrNone getAcnEndianness
                    acnAligment                             = acnAligment
                    redefinedValues                         = GetEnumValuesResetInAcn enmItems tTree
                    encodeValues                            = getFirstOrNone getAcnEncodeValues
                    presentWhen                             = presenceConditions
                }
            {t with acnProperties = Some (EnumeratedAcnProperties props)}
        | Real                  -> 
            let props = 
                {
                    RealAcnProperties.encodingProp          = getFirstOrNone getAcnRealEncoding
                    endiannessProp                          = getFirstOrNone getAcnEndianness
                    acnAligment                             = acnAligment
                    presentWhen                             = presenceConditions
                }
            {t with acnProperties = Some (RealAcnProperties props)}
        | IA5String
        | NumericString ->
            let props = 
                {
                    StringAcnProperties.encodingProp        = getFirstOrNone getAcnStringEncoding
                    sizeProp                                = getFirstOrNone (getAcnStringSizeProperty r)
                    acnAligment                             = acnAligment
                    presentWhen                             = presenceConditions
                }
            {t with acnProperties = Some (StringAcnProperties props)}
        | OctetString   ->
            let props = 
                {
                    SizeableAcnProperties.sizeProp          = getFirstOrNone (getAcnSizeableSizeProperty r)
                    acnAligment                             = acnAligment
                    presentWhen                             = presenceConditions
                }
            {t with acnProperties = Some (OctetStringAcnProperties props)}
        | BitString     -> 
            let props = 
                {
                    SizeableAcnProperties.sizeProp          = getFirstOrNone (getAcnSizeableSizeProperty r)
                    acnAligment                             = acnAligment
                    presentWhen                             = presenceConditions
                }
            {t with acnProperties = Some (BitStringAcnProperties props)}
        | NullType              -> 
            let props = 
                {
                    NullTypeAcnProperties.encodingPattern   = getFirstOrNone (getNullEncodingPattern)
                    acnAligment                             = acnAligment
                    presentWhen                             = presenceConditions
                }
            {t with acnProperties = Some (NullTypeAcnProperties props)}
        | Boolean               -> 
            let props = 
                {
                    BooleanAcnProperties.encodingPattern   = getFirstOrNone (getBoolEncodingPattern)
                    acnAligment                             = acnAligment
                    presentWhen                             = presenceConditions
                }
            {t with acnProperties = Some (BooleanAcnProperties props)}
        | SequenceOf   chType   -> 
            let props = 
                {
                    SizeableAcnProperties.sizeProp          = getFirstOrNone (getAcnSizeableSizeProperty r)
                    acnAligment                             = acnAligment
                    presentWhen                             = presenceConditions
                }
            //let rec CreateType (allAcnFiles: AntlrParserResult list) (thisAcnFile: AntlrParserResult) (r:AstRoot) (m:Asn1Module)   (t:Asn1Type) (tTree:ITree) : Asn1Type =
            let childrenEncSpec = tTree.GetOptChild(acnParser.CHILDREN_ENC_SPEC)

            let newChild = 
                match childrenEncSpec with
                | None    -> chType
                | Some childrenList ->
                    match childrenList.Children with
                    | []    -> chType
                    | chTree::[]  ->
                        match chTree.Type with
                        | acnParser.CHILD       ->
                            //Check that no name is provided
                            match chTree.GetOptChild(acnParser.LID) with
                            | None          -> ()
                            | Some(lid)     -> raise(SemanticError(t.Location, sprintf "%s Unexpected field name" lid.Text))
                            match chType.Kind with
                            | ReferenceType rf ->
                                let args= GetArgumentList allAcnFiles chTree chType.Kind
                                let newChild = {chType with Kind = ReferenceType ({rf with acnArguments = args})}
                                CreateType allAcnFiles thisAcnFile r m (Some t) newChild (chTree.GetChildByType acnParser.ENCODING_SPEC)    
                            | _                 ->
                                CreateType allAcnFiles thisAcnFile r m (Some t) chType (chTree.GetChildByType acnParser.ENCODING_SPEC)    
                        | acnParser.CHILD_NEW           -> raise(SemanticError(chTree.Location, "New fields can be inserted within sequences, not sequence ofs."))
                        | _                             -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnChild"))
                    | x1::x2::_  -> raise(SemanticError(t.Location, sprintf "SEQUENCE OFs have only one child"))

            {t with acnProperties = Some (SequenceOfAcnProperties props); Kind = SequenceOf newChild}
        | Sequence     children -> t
        | Choice       children -> t
        | ReferenceType rf      -> 
            let baseType = GetBaseType  t r
            let virtualNewT = CreateType allAcnFiles thisAcnFile r m parentType baseType tTree
            {t with acnProperties = virtualNewT.acnProperties}


    newT

let CreateTypeAssignment (allAcnFiles: AntlrParserResult list) (thisAcnFile: AntlrParserResult) (r:AstRoot) (m:Asn1Module)  (tas:TypeAssignment) (tasTree:ITree) : TypeAssignment =
    let encSpec = tasTree.GetChildByType(acnParser.ENCODING_SPEC)
    let prms = 
        match tasTree.GetOptChild(acnParser.PARAM_LIST) with
        | None -> []
        | Some(paramList) -> 
            let CreateParam (x:ITree) =
                let prmName = x.GetChild(1).Text
                let loc = x.GetChild(1).Location
                let refs = encSpec.AllChildren |> List.filter(fun x -> x.Type = acnParser.LONG_FIELD && x.ChildCount=1) |> List.map(fun x -> x.GetChild(0).Text)
                match refs |> Seq.tryFind(fun x -> x = prmName) with
                | Some(_)   -> {AcnParameter.name = prmName; asn1Type=CreateAcnAsn1Type (x.GetChild(0)) r; loc = loc}
                | None      -> raise(SemanticError(loc, sprintf "unreferenced parameter '%s'" prmName))
            paramList.Children |> List.map CreateParam

    let newType = CreateType allAcnFiles thisAcnFile r m None tas.Type encSpec
    {tas with Type = newType; acnParameters = prms}

let CreateModule (allAcnFiles: AntlrParserResult list) (thisAcnFile: AntlrParserResult) (r:AstRoot)  (modTree : ITree) : Asn1Module =
    let modNameL = modTree.GetChildByType(acnParser.UID).TextL
    let asn1Mod = CheckModuleName r modNameL

    let tasITreeList = modTree.GetChildrenByType(acnParser.TYPE_ENCODING)
    
    //check for duplicate type assignments in the ACN module
    tasITreeList |> List.map(fun x -> x.GetChildByType(acnParser.UID).TextL) |> CheckForDuplicates

    let newTassesMap =
        tasITreeList |> 
        List.map(fun tasTree ->
            let tasNameL = tasTree.GetChildByType(acnParser.UID).TextL
            let tas = gatTasByName r asn1Mod tasNameL
            CreateTypeAssignment allAcnFiles thisAcnFile r asn1Mod tas tasTree) |>
        List.map(fun tas -> tas.Name.Value, tas) |>
        Map.ofList
    
    {asn1Mod with TypeAssignments = asn1Mod.TypeAssignments |> List.map(fun tas -> match newTassesMap.TryFind tas.Name.Value with Some x -> x | None -> tas)}

let LoadAcnFile (allAcnFiles: AntlrParserResult list) (r:AstRoot) (thisAcnFile: AntlrParserResult)   : Asn1Module list = 
    thisAcnFile.rootItem.Children |> List.map (CreateModule allAcnFiles thisAcnFile r)


let CreateAcnIntegerConstant constantsSet (t:ITree) = 
    match t.Type with
    | acnParser.INT                 -> IntConst(t.BigIntL)
    | acnParser.UID                 -> 
        let extToThrow = SemanticError(t.Location, (sprintf "No ACN constant is defined with name '%s'" t.Text))
        CheckExists t.TextL constantsSet extToThrow
        RefConst(t.TextL)
    | _                             -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnIntegerConstant"))

let rec CreateExpression (r:AstRoot, st:List<StringLoc>)  (t:ITree) = 
    match t.Type with
    | acnParser.INT | acnParser.UID -> IntegerExpr(CreateAcnIntegerConstant st t)
    | acnParser.PLUS                -> SumExpr(CreateExpression (r,st) (t.GetChild(0)), CreateExpression (r,st) (t.GetChild(1)))
    | acnParser.MINUS               -> MinExpr(CreateExpression (r,st) (t.GetChild(0)), CreateExpression (r,st) (t.GetChild(1)))
    | acnParser.MULTIPLICATION      -> MulExpr(CreateExpression (r,st) (t.GetChild(0)), CreateExpression (r,st) (t.GetChild(1)))
    | acnParser.DIVISION            -> DivExpr(CreateExpression (r,st) (t.GetChild(0)), CreateExpression (r,st) (t.GetChild(1)))
    | acnParser.MODULO              -> ModExpr(CreateExpression (r,st) (t.GetChild(0)), CreateExpression (r,st) (t.GetChild(1)))
    | acnParser.POWER_SYMBOL        -> PowExpr(CreateExpression (r,st) (t.GetChild(0)), CreateExpression (r,st) (t.GetChild(1)))
    | acnParser.UNARY_MINUS         -> UnMinExp(CreateExpression (r,st) (t.GetChild(0)))
    | _                             -> raise( BugErrorException("AcnCreateFromAntlr::CreateExpression Unsupported operator"))


let CreateNamedExpression (r:AstRoot, st:List<StringLoc>) (t:ITree) : AcnConstant= 
    let name = t.GetChild(0).TextL
    // Constanst have global scope (i.e. within all modules) and should not conflict with any type or value assigment names
    for m in r.Modules do
        let names = m.TypeAssignments |> Seq.map(fun x -> x.Name)
        CheckUniqueName name names (fun n -> SemanticError(name.Location, (sprintf "ACN Constant '%s' conflicts with the Type Assignment defined in %s " name.Value (PrintLocation n.Location)) ))
        let names = m.ValueAssignments |> Seq.map(fun x -> x.Name)
        CheckUniqueName name names (fun n -> SemanticError(name.Location, (sprintf "ACN Constant '%s' conflicts with the Value Assignment defined in %s " name.Value (PrintLocation n.Location)) ))
    {AcnConstant.Name = t.GetChild(0).TextL;  Value = CreateExpression (r,st) (t.GetChild(1)) }

let CheckCircularDependenciesInAcnConstants (constants : List<ITree>) =
    let HandleConstant (t:ITree) =
        let rec GetNamesFromExpr (t:ITree) =
            seq {
                match t.Type with
                | acnParser.INT                 -> ()
                | acnParser.UID                 -> yield t.TextL
                | acnParser.PLUS |acnParser.MINUS | acnParser.MULTIPLICATION | acnParser.DIVISION | acnParser.MODULO | acnParser.POWER_SYMBOL        -> 
                    yield! GetNamesFromExpr (t.GetChild(0))
                    yield! GetNamesFromExpr (t.GetChild(1))
                | acnParser.UNARY_MINUS         -> yield! GetNamesFromExpr (t.GetChild(0)) 
                | _                             -> raise(BugErrorException("CheckCircularDependenciesInAcnConstants.HandleConstant.GetNamesFromExpr Unsupported operator"))
                } |> Seq.toList
        (t.GetChild(0).TextL, GetNamesFromExpr (t.GetChild(1)))
    let constantsExpanded = constants |> List.map HandleConstant
    let independentConstants = constantsExpanded |> List.filter(fun (nm, lst) -> lst.IsEmpty ) |> List.map fst
    let dependentConstansts = constantsExpanded |> List.filter(fun (nm, lst) -> not (lst.IsEmpty) )
    let comparer (s1:StringLoc) (s2:StringLoc) = s1.Value = s2.Value
    let ExToThrow (cyclicDepds:List<StringLoc*List<StringLoc>>) = 
        match cyclicDepds with
        | []        -> raise(BugErrorException(""))
        | (x,_)::xs -> 
            let names = cyclicDepds |> Seq.map (fun (n,_) -> n.Value) |> Seq.StrJoin ", "
            SemanticError(x.Location, sprintf "Cyclic dependencies in ACN constants: %s" names)
    DoTopologicalSort2 independentConstants dependentConstansts comparer ExToThrow |> ignore


let CreateAcnAst (allAcnFiles: AntlrParserResult list) (r0:AstRoot) : AstRoot =  
    ITree.RegisterFiles(allAcnFiles|> Seq.map(fun pr -> (pr.rootItem, pr.fileName)))
    let constants = seq {
        for acnFile in allAcnFiles do
            for m in acnFile.rootItem.Children do
                for c in m.GetChildrenByType(acnParser.CONSTANT) do
                    yield c } |> Seq.toList

    let constantNames = constants |> List.map(fun c -> c.GetChild(0).TextL)

    // check that all constant names are unique
    constantNames |> CheckForDuplicates 

    CheckCircularDependenciesInAcnConstants constants

    let constantValues = constants |> List.map (CreateNamedExpression (r0, constantNames)) 
    let acnConstantsMap = constantValues |> List.map(fun c -> c.Name.Value, EvaluateAcnIntExpression constantValues c.Value) |> Map.ofList


    // first update acnConstants so we can use them
    let r = {r0 with acnConstants = acnConstantsMap}

    let allModules =  allAcnFiles |> List.collect (LoadAcnFile allAcnFiles r)
    let modulesSet = allModules |> List.map(fun  m -> m.Name.Value, m) |> Map.ofList
    let handleFile (f:Asn1File) : Asn1File =
        {f with Modules = f.Modules |> List.map(fun m -> match modulesSet.TryFind m.Name.Value with Some acnMod -> acnMod | None -> m)}

    {r with Files = r.Files |> List.map handleFile; acnConstants = acnConstantsMap}



