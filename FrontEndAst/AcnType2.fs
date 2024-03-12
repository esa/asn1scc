module AcnType2
open FsUtils
open System.Numerics

type RelativePath =
    | RelativePath of string list



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


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN PROPERTIES DEFINITION ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type AcnBooleanEncoding =
    | TrueValue    of StringLoc    //Default '1'
    | FalseValue   of StringLoc

type AcnEncoding =
    | PosInt
    | TwosComplement
    | Ascii
    | BCD
    | IEEE754_32
    | IEEE754_64

type AcnEndianness =
    | LittleEndianness
    | BigEndianness            // Default

type AcnAlignment =
    | NextByte
    | NextWord
    | NextDWord

type AcnSizeProperty =
    | Fixed             of AcnIntegerConstant
    | NullTerminated    of byte      //termination character
    | SizeDeterminant   of (RelativePath*SrcLoc)



type AcnProperty =
    | Encoding          of AcnEncoding                     // used by int, real, enum
    | SizeProperty      of AcnSizeProperty                 // used by int, real, and all sizeable types
    | Alignment          of AcnAlignment                     // *
    | EncodeValues                                          // used by enums => values will be encoded and not indexes
    | BooleanEncoding   of AcnBooleanEncoding              // bool
    | NullValue         of StringLoc                    // null
    | Endianness        of AcnEndianness                   // used by int, real, enum
    | EnumeratorResetValue of string*BigInteger        // used by enum children to redefine values
    | MappingFunction   of StringLoc                    // used by int
with
    override this.ToString() =
        match this with
        | Encoding          enc                       -> sprintf "encoding %A" enc
        | SizeProperty      sz                        -> sprintf "size %A" sz
        | Alignment          al                        -> sprintf "alignment %A" al
        | EncodeValues                                -> "encode-values"
        | BooleanEncoding   ben                       -> sprintf "pattern '%A'" ben
        | NullValue         pattern                   -> sprintf "pattern '%s'" pattern.Value
        | Endianness        endi                -> sprintf "endianness %A" endi
        | EnumeratorResetValue (enChildName,vl)       -> enChildName + vl.ToString()
        | MappingFunction   funcName                  -> funcName.Value

// present when property definition
// this property is not part of the ACN type itself but part of the AcnChildInfo
type PresentWhenCondition =
    | PresenceBool  of RelativePath                         // applied to SEQUENCE or Choice child
    | PresenceInt   of RelativePath*AcnIntegerConstant      // applied to SEQUENCE or Choice child
    | PresenceStr   of RelativePath*string


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN TYPES DEFINITION      ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type AcnAsn1Type =
    | AcnInteger    of SrcLoc
    | AcnBoolean    of SrcLoc
    | AcnNullType   of SrcLoc
    | AcnRefType    of StringLoc*StringLoc


type AcnType = {
    properties  : AcnProperty list
    children    : AcnChildInfo list
    Location    : SrcLoc
    Comments    : string array
}

and AcnChildInfo = {
    name            : string                //For sequence of contains the string '#'
    acnArguments    : RelativePath list
    presentWhen     : PresentWhenCondition list
    child           : AcnType
}



type AcnParameter = {
    name        : string
    asn1Type    : AcnAsn1Type
    loc         : SrcLoc
}

type AcnTypeAssignment = {
    name            : string
    acnParameters   : AcnParameter
    acnType         : AcnType
    loc             : SrcLoc
}


type AcnConstant = {
    Name  : StringLoc
    Value : AcnIntExpr
}


type AcnModule = {
    name : string
    constants : AcnConstant list
}

type AcnFile = {
    acnModules : AcnModule list
}
