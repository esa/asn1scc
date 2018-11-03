module AcnGenericTypes

open System.Numerics
open System
open FsUtils
open CommonTypes

type RelativePath = 
    | RelativePath of StringLoc list
with
    member this.AsString = 
        match this with  RelativePath p -> p |> Seq.StrJoin "."
    member this.location = 
        match this with  RelativePath p -> p |> List.map(fun z -> z.Location) |> List.head
    override this.ToString() = this.AsString

type AcnEndianness =
    | LittleEndianness
    | BigEndianness            

type AcnAligment = 
    | NextByte
    | NextWord
    | NextDWord

// present when property defintion
// this property is not part of the ACN type itself but part of the AcnChildInfo
type PresenceWhenBool  = 
    | PresenceWhenBool of RelativePath                         

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
    | IntNullTerminated     of byte      //termination character when encoding is ASCII

type AcnIntEncoding =
    | PosInt
    | TwosComplement
    | IntAscii
    | BCD

type MappingFunction  = 
    | MappingFunction of StringLoc

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
    | StrNullTerminated  of byte      //termination character when encoding is ASCII

type AcnStringEncoding =
    | StrAscii

type StringAcnProperties = {
    encodingProp    : AcnStringEncoding     option
    sizeProp        : AcnStringSizeProperty option
}


type SizeableAcnProperties = {
    sizeProp        : RelativePath          option
}

type PATERN_PROP_VALUE =
    | PATERN_PROP_BITSTR_VALUE of StringLoc
    | PATERN_PROP_OCTSTR_VALUE of ByteLoc list


type NullTypeAcnProperties = {
    encodingPattern     : PATERN_PROP_VALUE             option
}

type ObjectIdTypeAcnProperties = {
    sizeProperties     : SizeableAcnProperties          option
    itemProperties     : IntegerAcnProperties           option
}


type AcnBooleanEncoding =
    | TrueValue    of StringLoc    
    | FalseValue   of StringLoc

type BooleanAcnProperties = {
    encodingPattern     : AcnBooleanEncoding    option
}

type ChoiceAcnProperties = {
    enumDeterminant     : RelativePath              option
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


