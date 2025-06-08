module AcnGenericTypes

#nowarn "3536"
open System.Numerics
open System
open FsUtils
open CommonTypes
open Antlr.Runtime.Tree
open Antlr.Runtime

type RelativePath =
    | RelativePath of StringLoc list
with
    member this.AsString =
        match this with  RelativePath p -> p |> Seq.StrJoin "."
    member this.location =
        match this with  RelativePath p -> p |> List.map(fun z -> z.Location) |> List.head
    member this.isPrefixOf (other: RelativePath) =
        match this, other with
            RelativePath this, RelativePath other ->
                List.isPrefixOf this other

    member this.isPrefixOf2 (other: string list) =
        match this with
            RelativePath this ->
                List.isPrefixOf (this |> List.map (fun s -> s.Value)) other
    member this.asStringList =
        match this with
        | RelativePath path -> path |> List.map (fun s -> s.Value)

    member this.concat (other: RelativePath): RelativePath =
        match this, other with
            RelativePath this, RelativePath other ->
                RelativePath (this @ other)

    override this.ToString() = this.AsString

type AcnEndianness =
    | LittleEndianness
    | BigEndianness

type AcnAlignment =
    | NextByte
    | NextWord
    | NextDWord
with
    member this.nbBits: bigint =
        match this with
        | NextByte -> 8I
        | NextWord -> 16I
        | NextDWord -> 32I



(*  PRESENT WHEN EXPRESSIONS *)


type AcnExpression =
    | IntegerConstantExp            of IntLoc
    | AcnIntegerConstExp            of StringLoc
    | RealConstantExp               of DoubleLoc
    | BooleanConstantExp            of BoolLoc
    | Asn1LongField                 of RelativePath
    | NotUnaryExpression            of SrcLoc*AcnExpression
    | MinusUnaryExpression          of SrcLoc*AcnExpression
    | AdditionExpression            of SrcLoc*AcnExpression*AcnExpression
    | SubtractionExpression         of SrcLoc*AcnExpression*AcnExpression
    | MultiplicationExpression      of SrcLoc*AcnExpression*AcnExpression
    | DivisionExpression            of SrcLoc*AcnExpression*AcnExpression
    | ModuloExpression              of SrcLoc*AcnExpression*AcnExpression
    | LessThanEqualExpression       of SrcLoc*AcnExpression*AcnExpression
    | LessThanExpression            of SrcLoc*AcnExpression*AcnExpression
    | GreaterThanEqualExpression    of SrcLoc*AcnExpression*AcnExpression
    | GreaterThanExpression         of SrcLoc*AcnExpression*AcnExpression
    | EqualExpression               of SrcLoc*AcnExpression*AcnExpression
    | NotEqualExpression            of SrcLoc*AcnExpression*AcnExpression
    | AndExpression                 of SrcLoc*AcnExpression*AcnExpression
    | OrExpression                  of SrcLoc*AcnExpression*AcnExpression


let foldAcnExpression intConstFnc acnIntConstFnc realConstFnc boolConstFnc asn1LongFldFnc notUnExpFnc mnUnExpFnc
        addFnc subFnc mulFnc divFnc modFnc lteFnc ltFnc gteFnc gtFnc eqFnc neqFnc andFnc orFnc (exp:AcnExpression) (s:'UserState) =
    let rec loopExpression (exp:AcnExpression) (s:'UserState) =
        match exp with
        | IntegerConstantExp            x      -> intConstFnc x s
        | AcnIntegerConstExp            x      -> acnIntConstFnc x s
        | RealConstantExp               x      -> realConstFnc x s
        | BooleanConstantExp            x      -> boolConstFnc x s
        | Asn1LongField                 x      -> asn1LongFldFnc x s
        | NotUnaryExpression            (l,e1)     ->
            let re1, s1 = loopExpression e1 s
            notUnExpFnc l re1 s1
        | MinusUnaryExpression          (l,e1)     ->
            let re1, s1 = loopExpression e1 s
            mnUnExpFnc l re1 s1
        | AdditionExpression            (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            addFnc l re1 re2 s2
        | SubtractionExpression         (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            subFnc l re1 re2 s2
        | MultiplicationExpression       (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            mulFnc l re1 re2 s2
        | DivisionExpression            (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            divFnc l re1 re2 s2
        | ModuloExpression              (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            modFnc l re1 re2 s2
        | LessThanEqualExpression       (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            lteFnc l re1 re2 s2
        | LessThanExpression            (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            ltFnc l re1 re2 s2
        | GreaterThanEqualExpression    (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            gteFnc l re1 re2 s2
        | GreaterThanExpression         (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            gtFnc l re1 re2 s2
        | EqualExpression               (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            eqFnc l re1 re2 s2
        | NotEqualExpression            (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            neqFnc l re1 re2 s2
        | AndExpression                 (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            andFnc l re1 re2 s2
        | OrExpression                  (l,e1, e2) ->
            let re1, s1 = loopExpression e1 s
            let re2, s2 = loopExpression e2 s1
            orFnc l re1 re2 s2

    loopExpression exp s

type NonBooleanExpressionType =
    | IntExpType
    | RealExpType
with
    override this.ToString() =
        match this with
        | IntExpType    -> "integer expression"
        | RealExpType   -> "real expression"

type ExpressionType =
    | BoolExpression
    | NonBooleanExpression of NonBooleanExpressionType
with
    override this.ToString() =
        match this with
        | BoolExpression    -> "boolean expression"
        | NonBooleanExpression z -> z.ToString()

type ValidationResult =
    | ValResultOK   of ExpressionType
    | ValResultError of (SrcLoc*String)


let validateAcnExpression handleLongField (exp:AcnExpression) =
    let numericBinaryOperator l e1 e2  s =
        match e1 with
        | ValResultError _  -> (e1,s)
        | ValResultOK   expType    ->
            match expType with
            | BoolExpression    -> (ValResultError (l, "Expecting numeric expression"), 0)
            | NonBooleanExpression net1->
                match e2 with
                | ValResultError _  -> (e2,s)
                | ValResultOK   expType    ->
                    match expType with
                    | BoolExpression    -> (ValResultError (l, "Expecting numeric expression"), 0)
                    | NonBooleanExpression net2 ->
                        match net1 = net2 with
                        | true  -> (ValResultOK (NonBooleanExpression net1), 0)
                        | false -> (ValResultError (l, (sprintf "Expecting %s expression" (net1.ToString()))), 0)

    let numericComparativeBinaryOperator l e1 e2  s =
        match e1 with
        | ValResultError _  -> (e1,s)
        | ValResultOK   expType    ->
            match expType with
            | BoolExpression    -> (ValResultError (l, "Expecting numeric expression"), 0)
            | NonBooleanExpression net1->
                match e2 with
                | ValResultError _  -> (e2,s)
                | ValResultOK   expType    ->
                    match expType with
                    | BoolExpression    -> (ValResultError (l, "Expecting numeric expression"), 0)
                    | NonBooleanExpression net2 -> (ValResultOK BoolExpression, 0)
    let eqNeqOperator l e1 e2  s =
        match e1 with
        | ValResultError _  -> (e1,s)
        | ValResultOK   expType1    ->
            match e2 with
            | ValResultError _  -> (e2,s)
            | ValResultOK   expType2    ->
                match expType1 = expType2 with
                | true  -> (ValResultOK BoolExpression , 0)
                | false -> (ValResultError (l, (sprintf "Expecting %s expression" (expType1.ToString()))), 0)

    let andOrBinaryOperator l e1 e2  s =
        match e1 with
        | ValResultError _  -> (e1,s)
        | ValResultOK   expType    ->
            match expType with
            | NonBooleanExpression _    -> (ValResultError (l, "Expecting boolean expression"), 0)
            | BoolExpression ->
                match e2 with
                | ValResultError _  -> (e2,s)
                | ValResultOK   expType    ->
                    match expType with
                    | NonBooleanExpression _    -> (ValResultError (l, "Expecting boolean expression"), 0)
                    | BoolExpression -> (ValResultOK BoolExpression, 0)

    foldAcnExpression
        (fun i s -> (ValResultOK (NonBooleanExpression IntExpType)) , 0)
        (fun i s -> (ValResultOK (NonBooleanExpression IntExpType)) , 0)
        (fun i s -> (ValResultOK (NonBooleanExpression RealExpType)) , 0)
        (fun i s -> (ValResultOK BoolExpression) , 0)
        (fun lf s -> (handleLongField lf) , 0)     //we have to resolve the lf path and decide if its numeric or boolean expression.
        (fun l (e1) s ->        //NotUnaryExpression
            match e1 with
            | ValResultError _  -> (e1,s)
            | ValResultOK   expType    ->
                match expType with
                | BoolExpression    -> (ValResultOK BoolExpression, 0)
                | NonBooleanExpression _ -> (ValResultError (l, "Expecting boolean expression"), 0) )
        (fun l (e1) s ->        //MinusUnaryExpression
            match e1 with
            | ValResultError _  -> (e1,s)
            | ValResultOK   expType    ->
                match expType with
                | BoolExpression    -> (ValResultError (l, "Expecting numeric expression"), 0)
                | NonBooleanExpression z-> (ValResultOK (NonBooleanExpression z), 0) )
        numericBinaryOperator   (*add*)
        numericBinaryOperator   (*sub*)
        numericBinaryOperator   (*mul*)
        numericBinaryOperator   (*div*)
        numericBinaryOperator   (*mod*)
        numericComparativeBinaryOperator (*lte*)
        numericComparativeBinaryOperator (*lt*)
        numericComparativeBinaryOperator (*gte*)
        numericComparativeBinaryOperator (*gt*)
        eqNeqOperator (*equal*)
        eqNeqOperator (*not equal*)
        andOrBinaryOperator (*and*)
        andOrBinaryOperator (*or*)
        exp 0 |> fst

// present when property definition
// this property is not part of the ACN type itself but part of the AcnChildInfo
type PresenceWhenBool  =
    | PresenceWhenBool of RelativePath
    | PresenceWhenBoolExpression of AcnExpression


type AcnPresentWhenConditionChoiceChild =
    | PresenceInt   of RelativePath*IntLoc
    | PresenceStr   of RelativePath*StringLoc
with
    member this.valueAsString =
        match this with
        | PresenceInt   (_,v)  -> v.Value.ToString()
        | PresenceStr   (_,v)  -> v.Value
    member this.relativePath =
        match this with
        | PresenceInt   (rp,_)
        | PresenceStr   (rp,_)  -> rp
    member this.location =
        match this with
        | PresenceInt   (rp,intLoc) -> intLoc.Location
        | PresenceStr   (rp,strLoc) -> strLoc.Location
    member this.kind =
        match this with
        | PresenceInt   _ -> 1
        | PresenceStr   _ -> 2


// Integer acn properties
type AcnIntSizeProperty =
    | Fixed                 of BigInteger
    | IntNullTerminated     of byte list      //termination character when encoding is ASCII

type AcnIntEncoding =
    | PosInt
    | TwosComplement
    | IntAscii
    | BCD

type MappingFunction  =
    | MappingFunction of (StringLoc option)*StringLoc

type PostEncodingFunction  =
    | PostEncodingFunction of (StringLoc option)*StringLoc

type PreDecodingFunction  =
    | PreDecodingFunction of (StringLoc option)*StringLoc


type IntegerAcnProperties = {
    encodingProp    : AcnIntEncoding        option
    sizeProp        : AcnIntSizeProperty    option
    endiannessProp  : AcnEndianness         option
    mappingFunction : MappingFunction       option
}




// Real acn properties
type AcnRealEncoding =
    | IEEE754_32
    | IEEE754_64

type RealAcnProperties = {
    encodingProp    : AcnRealEncoding       option
    endiannessProp  : AcnEndianness         option
}

// String acn properties
type AcnStringSizeProperty =
    | StrExternalField   of RelativePath
    | StrNullTerminated  of byte list     //termination character when encoding is ASCII

type AcnStringEncoding =
    | StrAscii

type StringAcnProperties = {
    encodingProp    : AcnStringEncoding     option
    sizeProp        : AcnStringSizeProperty option
}

type AcnSizeableSizeProperty =
    | SzExternalField   of RelativePath
    | SzNullTerminated  of StringLoc     //termination pattern


type SizeableAcnProperties = {
    sizeProp        : AcnSizeableSizeProperty          option
}

type PATTERN_PROP_VALUE =
    | PATTERN_PROP_BITSTR_VALUE of StringLoc
    | PATTERN_PROP_OCTSTR_VALUE of ByteLoc list


type NullTypeAcnProperties = {
    encodingPattern     : PATTERN_PROP_VALUE            option
    savePosition        : bool
}

type ObjectIdTypeAcnProperties = {
    sizeProperties     : SizeableAcnProperties          option
    itemProperties     : IntegerAcnProperties           option
}


type AcnBooleanEncoding = 
    | TrueValueEncoding of StringLoc //the user specifies only the true value. All other values are considered false
    | FalseValueEncoding of StringLoc //the user specifies only the false value. All other values are considered true
    | TrueFalseValueEncoding of StringLoc*StringLoc //the user specifies both true and false values

with
    member this.bitValLength = 
        match this with
        | TrueValueEncoding tv -> tv.Value.Length
        | FalseValueEncoding fv -> fv.Value.Length
        | TrueFalseValueEncoding (tv,_) -> tv.Value.Length

type BooleanAcnProperties = {
    encodingPattern: AcnBooleanEncoding option
}

type ChoiceAcnProperties = {
    enumDeterminant: RelativePath option
}

type SequenceAcnProperties = {
    postEncodingFunction    : PostEncodingFunction option
    preDecodingFunction     : PreDecodingFunction option
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN PARAMETERS DEFINITION ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

[<CustomEquality; NoComparison>]
type AcnParamType =
    | AcnPrmInteger    of SrcLoc
    | AcnPrmBoolean    of SrcLoc
    | AcnPrmNullType   of SrcLoc
    | AcnPrmRefType    of StringLoc*StringLoc
with
    override this.ToString() =
        match this with
        | AcnPrmInteger   _         -> "INTEGER"
        | AcnPrmBoolean   _         -> "BOOLEAN"
        | AcnPrmNullType  _         -> "NULL"
        | AcnPrmRefType    (md,ts)  -> sprintf "%s.%s" md.Value ts.Value
    override x.Equals(yobj) =
        match yobj with
        | :? AcnParamType as other ->
            match x, other with
            | AcnPrmInteger    _       , AcnPrmInteger    _         -> true
            | AcnPrmBoolean    _       , AcnPrmBoolean    _         -> true
            | AcnPrmNullType   _       , AcnPrmNullType   _         -> true
            | AcnPrmRefType    (md,ts) , AcnPrmRefType    (md2,ts2) -> md=md2 && ts=ts2
            | _                                                     -> false
        | _ -> false
    override x.GetHashCode() =
        match x with
            | AcnPrmInteger    _       -> 1
            | AcnPrmBoolean    _       -> 2
            | AcnPrmNullType   _       -> 3
            | AcnPrmRefType    (md,ts) -> md.GetHashCode() ^^^ ts.GetHashCode()



type AcnParameter = {
    name        : string
    asn1Type    : AcnParamType
    loc         : SrcLoc
    id          : ReferenceToType
}
with
    member this.c_name = ToC this.name


type GenericAcnPresentWhenCondition =
    | GP_PresenceBool  of RelativePath
    | GP_PresenceInt   of RelativePath*IntLoc
    | GP_PresenceStr   of RelativePath*StringLoc

type GenAcnEncodingProp =
    | GP_PosInt
    | GP_TwosComplement
    | GP_Ascii
    | GP_BCD
    | GP_IEEE754_32
    | GP_IEEE754_64

type GenSizeProperty =
    | GP_Fixed                 of IntLoc
    | GP_NullTerminated
    | GP_SizeDeterminant       of RelativePath



type GenericAcnProperty =
    | ENCODING          of GenAcnEncodingProp
    | SIZE              of GenSizeProperty
    | ALIGNTONEXT       of AcnAlignment
    | ENCODE_VALUES
    | SAVE_POSITION
    | PRESENT_WHEN      of GenericAcnPresentWhenCondition list
    | PRESENT_WHEN_EXP  of AcnExpression
    | TRUE_VALUE        of StringLoc
    | FALSE_VALUE       of StringLoc
    | PATTERN           of PATTERN_PROP_VALUE
    | CHOICE_DETERMINANT of RelativePath
    | ENDIANNESS        of AcnEndianness
    | ENUM_SET_VALUE    of IntLoc
    | TERMINATION_PATTERN of StringLoc //bit pattern
    | MAPPING_FUNCTION  of (StringLoc option)*StringLoc
    | POST_ENCODING_FUNCTION of (StringLoc option)*StringLoc
    | PRE_DECODING_FUNCTION of (StringLoc option)*StringLoc
    




type AcnTypeEncodingSpec = {
    acnProperties   : GenericAcnProperty list
    children        : ChildSpec list
    loc             : SrcLoc
    comments        : string list
    position        : SrcLoc*SrcLoc   //start pos, end pos
    antlrSubTree    :ITree option
}

and ChildSpec = {
    name            : StringLoc
    childEncodingSpec : AcnTypeEncodingSpec
    asn1Type        : AcnParamType option    // if present then it indicates an ACN inserted type
    argumentList    : RelativePath list
    comments        : string list
}

type AcnTypeAssignment = {
    name            : StringLoc
    acnParameters   : AcnParameter list
    typeEncodingSpec: AcnTypeEncodingSpec
    comments        : string list
    position        : RangeWithinFile
}

type AcnModule = {
    name            : StringLoc
    typeAssignments : AcnTypeAssignment list
}


type AcnFile = {
    antlrResult : CommonTypes.AntlrParserResult
    modules     : AcnModule list
}

type AcnAst = {
    files : AcnFile list
    acnConstants : Map<string, BigInteger>
}