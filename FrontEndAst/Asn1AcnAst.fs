module Asn1AcnAst

#nowarn "3536"
open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open System
open FsUtils
open CommonTypes
open AcnGenericTypes
open AbstractMacros






/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN PROPERTIES DEFINITION ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ASN1 VALUES DEFINITION    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type IntegerValue         = IntLoc
type RealValue            = DoubleLoc
type StringValue          = StringLoc
type TimeValue            = Asn1DateTimeValueLoc
type BooleanValue         = BoolLoc
type BitStringValue       = StringLoc
type OctetStringValue     = list<ByteLoc>
type EnumValue            = StringLoc
type NullValue            = unit
type SeqOfValue           = list<Asn1Value>
and SeqValue              = list<NamedValue>
and ChValue               = NamedValue
and RefValue              = ((StringLoc*StringLoc)*Asn1Value)
and ObjectIdentifierValue   = ((ResolvedObjectIdentifierValueComponent list)*(ObjectIdentifierValueComponent list))

and NamedValue = {
    name        : StringLoc
    Value       : Asn1Value
}
and Asn1Value = {
    kind : Asn1ValueKind
    loc  : SrcLoc
    moduleName      : string
    id   : ReferenceToValue
}

and Asn1ValueKind =
    | IntegerValue          of IntegerValue
    | RealValue             of RealValue
    | StringValue           of (SingleStringValue list*SrcLoc)
    | BooleanValue          of BooleanValue
    | BitStringValue        of BitStringValue
    | TimeValue             of TimeValue
    | OctetStringValue      of OctetStringValue
    | EnumValue             of EnumValue
    | SeqOfValue            of SeqOfValue
    | SeqValue              of SeqValue
    | ChValue               of ChValue
    | NullValue             of NullValue
    | RefValue              of RefValue
    | ObjOrRelObjIdValue    of ObjectIdentifierValue


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ASN1 CONSTRAINTS DEFINITION    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type GenericConstraint<'v> =
    | UnionConstraint                   of string*GenericConstraint<'v>*GenericConstraint<'v>*bool //left,right, virtual constraint
    | IntersectionConstraint            of string*GenericConstraint<'v>*GenericConstraint<'v>
    | AllExceptConstraint               of string*GenericConstraint<'v>
    | ExceptConstraint                  of string*GenericConstraint<'v>*GenericConstraint<'v>
    | RootConstraint                    of string*GenericConstraint<'v>
    | RootConstraint2                   of string*GenericConstraint<'v>*GenericConstraint<'v>
    | SingleValueConstraint             of string*'v
    with
        member
            this.ASN1 =
                match this with
                | UnionConstraint (s,_,_,_) -> s
                | IntersectionConstraint (s,_,_) -> s
                | AllExceptConstraint    (s,_) -> s
                | ExceptConstraint       (s,_,_) -> s
                | RootConstraint         (s,_) -> s
                | RootConstraint2        (s,_,_) -> s
                | SingleValueConstraint  (s,_) -> s


type RangeTypeConstraint<'v1,'v2>  =
    | RangeUnionConstraint               of string*RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>*bool //left,right, virtual constraint
    | RangeIntersectionConstraint        of string*RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeAllExceptConstraint           of string*RangeTypeConstraint<'v1,'v2>
    | RangeExceptConstraint              of string*RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeRootConstraint                of string*RangeTypeConstraint<'v1,'v2>
    | RangeRootConstraint2               of string*RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeSingleValueConstraint         of string*'v2
    | RangeConstraint                     of string*('v1) *('v1)*bool*bool    //min, max, InclusiveMin(=true), InclusiveMax(=true)
    | RangeConstraint_val_MAX             of string*('v1) *bool            //min, InclusiveMin(=true)
    | RangeConstraint_MIN_val             of string*('v1) *bool            //max, InclusiveMax(=true)
    with
        member this.ASN1 =
            match this with
            | RangeUnionConstraint               (s,_,_,_) -> s
            | RangeIntersectionConstraint        (s,_,_) -> s
            | RangeAllExceptConstraint           (s,_) -> s
            | RangeExceptConstraint              (s,_,_) -> s
            | RangeRootConstraint                (s,_) -> s
            | RangeRootConstraint2               (s,_,_) -> s
            | RangeSingleValueConstraint         (s,_) -> s
            | RangeConstraint                     (s,_,_,_,_) -> s
            | RangeConstraint_val_MAX             (s,_,_) -> s
            | RangeConstraint_MIN_val             (s,_,_) -> s


type IntegerTypeConstraint  = RangeTypeConstraint<BigInteger, BigInteger>
type PosIntTypeConstraint   = RangeTypeConstraint<UInt32, UInt32>
type CharTypeConstraint     = RangeTypeConstraint<char, string>

type RealTypeConstraint     = RangeTypeConstraint<double, double>


type SizableTypeConstraint<'v>  =
    | SizeUnionConstraint               of string*SizableTypeConstraint<'v>*SizableTypeConstraint<'v>*bool //left,right, virtual constraint
    | SizeIntersectionConstraint        of string*SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeAllExceptConstraint           of string*SizableTypeConstraint<'v>
    | SizeExceptConstraint              of string*SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeRootConstraint                of string*SizableTypeConstraint<'v>
    | SizeRootConstraint2               of string*SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeSingleValueConstraint         of string*'v
    | SizeConstraint                     of string*PosIntTypeConstraint
    with
        member
            this.ASN1 =
                match this with
                | SizeUnionConstraint       (s,_,_,_) -> s
                | SizeIntersectionConstraint(s,_,_) -> s
                | SizeAllExceptConstraint   (s,_) -> s
                | SizeExceptConstraint      (s,_,_) -> s
                | SizeRootConstraint        (s,_) -> s
                | SizeRootConstraint2       (s,_,_) -> s
                | SizeSingleValueConstraint (s,_) -> s
                | SizeConstraint             (s,_) -> s

type IA5StringConstraint =
    | StrUnionConstraint               of string*IA5StringConstraint*IA5StringConstraint*bool //left,right, virtual constraint
    | StrIntersectionConstraint        of string*IA5StringConstraint*IA5StringConstraint
    | StrAllExceptConstraint           of string*IA5StringConstraint
    | StrExceptConstraint              of string*IA5StringConstraint*IA5StringConstraint
    | StrRootConstraint                of string*IA5StringConstraint
    | StrRootConstraint2               of string*IA5StringConstraint*IA5StringConstraint
    | StrSingleValueConstraint         of string*string
    | StrSizeConstraint                 of string*PosIntTypeConstraint
    | AlphabetConstraint                of string*CharTypeConstraint
    with
        member
            this.ASN1 =
                match this with
                | StrUnionConstraint        (s,_,_,_) -> s
                | StrIntersectionConstraint (s,_,_) -> s
                | StrAllExceptConstraint    (s,_) -> s
                | StrExceptConstraint       (s,_,_) -> s
                | StrRootConstraint         (s,_) -> s
                | StrRootConstraint2        (s,_,_) -> s
                | StrSingleValueConstraint  (s,_) -> s
                | StrSizeConstraint          (s,_) -> s
                | AlphabetConstraint         (s,_) -> s









type OctetStringConstraint  =    SizableTypeConstraint<OctetStringValue*(ReferenceToValue*SrcLoc)>
type BitStringConstraint    =    SizableTypeConstraint<BitStringValue*(ReferenceToValue*SrcLoc)>
type BoolConstraint         =    GenericConstraint<bool>
type EnumConstraint         =    GenericConstraint<string>
type ObjectIdConstraint     =    GenericConstraint<ObjectIdentifierValue>
type TimeConstraint         =    GenericConstraint<Asn1DateTimeValue>


//type SequenceOfConstraint   =     SizableTypeConstraint<SeqOfValue>
//type SequenceConstraint     =     GenericConstraint<SeqValue>

type SeqOrChoiceConstraint<'v> =
    | SeqOrChUnionConstraint                   of string*SeqOrChoiceConstraint<'v>*SeqOrChoiceConstraint<'v>*bool //left,right, virtual constraint
    | SeqOrChIntersectionConstraint            of string*SeqOrChoiceConstraint<'v>*SeqOrChoiceConstraint<'v>
    | SeqOrChAllExceptConstraint               of string*SeqOrChoiceConstraint<'v>
    | SeqOrChExceptConstraint                  of string*SeqOrChoiceConstraint<'v>*SeqOrChoiceConstraint<'v>
    | SeqOrChRootConstraint                    of string*SeqOrChoiceConstraint<'v>
    | SeqOrChRootConstraint2                   of string*SeqOrChoiceConstraint<'v>*SeqOrChoiceConstraint<'v>
    | SeqOrChSingleValueConstraint             of string*'v
    | SeqOrChWithComponentsConstraint          of string*NamedConstraint list
    with
        member
            this.ASN1 =
                match this with
                | SeqOrChUnionConstraint              (s,_,_,_) -> s
                | SeqOrChIntersectionConstraint       (s,_,_) -> s
                | SeqOrChAllExceptConstraint          (s,_) -> s
                | SeqOrChExceptConstraint             (s,_,_) -> s
                | SeqOrChRootConstraint               (s,_) -> s
                | SeqOrChRootConstraint2              (s,_,_) -> s
                | SeqOrChSingleValueConstraint        (s,_) -> s
                | SeqOrChWithComponentsConstraint     (s,_) -> s


and SeqConstraint = SeqOrChoiceConstraint<SeqValue>

and ChoiceConstraint       =     SeqOrChoiceConstraint<ChValue>

and SequenceOfConstraint   =
    | SeqOfSizeUnionConstraint               of string*SequenceOfConstraint*SequenceOfConstraint*bool //left,right, virtual constraint
    | SeqOfSizeIntersectionConstraint        of string*SequenceOfConstraint*SequenceOfConstraint
    | SeqOfSizeAllExceptConstraint           of string*SequenceOfConstraint
    | SeqOfSizeExceptConstraint              of string*SequenceOfConstraint*SequenceOfConstraint
    | SeqOfSizeRootConstraint                of string*SequenceOfConstraint
    | SeqOfSizeRootConstraint2               of string*SequenceOfConstraint*SequenceOfConstraint
    | SeqOfSizeSingleValueConstraint         of string*SeqOfValue
    | SeqOfSizeConstraint                     of string*PosIntTypeConstraint
    | SeqOfSeqWithComponentConstraint        of string*AnyConstraint*SrcLoc
    with
        member
            this.ASN1 =
                match this with
                | SeqOfSizeUnionConstraint         (s,_,_,_) -> s
                | SeqOfSizeIntersectionConstraint  (s,_,_) -> s
                | SeqOfSizeAllExceptConstraint     (s,_) -> s
                | SeqOfSizeExceptConstraint        (s,_,_) -> s
                | SeqOfSizeRootConstraint          (s,_) -> s
                | SeqOfSizeRootConstraint2         (s,_,_) -> s
                | SeqOfSizeSingleValueConstraint   (s,_) -> s
                | SeqOfSizeConstraint               (s,_) -> s
                | SeqOfSeqWithComponentConstraint  (s,_,_) -> s


and AnyConstraint =
    | IntegerTypeConstraint of IntegerTypeConstraint
    | IA5StringConstraint   of IA5StringConstraint
    | RealTypeConstraint    of RealTypeConstraint
    | OctetStringConstraint of OctetStringConstraint
    | BitStringConstraint   of BitStringConstraint
    | BoolConstraint        of BoolConstraint
    | EnumConstraint        of EnumConstraint
    | ObjectIdConstraint    of ObjectIdConstraint
    | SequenceOfConstraint  of SequenceOfConstraint
    | SeqConstraint         of SeqConstraint
    | ChoiceConstraint      of ChoiceConstraint
    | NullConstraint
    | TimeConstraint        of TimeConstraint
    with
        member
            this.ASN1 =
                match this with
                | IntegerTypeConstraint (x) -> x.ASN1
                | IA5StringConstraint   (x) -> x.ASN1
                | RealTypeConstraint    (x) -> x.ASN1
                | OctetStringConstraint (x) -> x.ASN1
                | BitStringConstraint   (x) -> x.ASN1
                | BoolConstraint        (x) -> x.ASN1
                | EnumConstraint        (x) -> x.ASN1
                | ObjectIdConstraint    (x) -> x.ASN1
                | SequenceOfConstraint  (x) -> x.ASN1
                | SeqConstraint         (x) -> x.ASN1
                | ChoiceConstraint      (x) -> x.ASN1
                | NullConstraint            -> ""
                | TimeConstraint        (x) -> x.ASN1


and NamedConstraint = {
    Name: StringLoc
    Constraint:AnyConstraint option
    Mark:Asn1Ast.NamedConstraintMark
}


type NamedItem = {
    Name:StringLoc
    c_name:string
    scala_name:string
    ada_name:string
    definitionValue : BigInteger          // the value in the header file

    // the value encoded by ACN. It can (a) the named item index (i.e. like uper), (b) The definition value, (c) The redefined value from acn properties
    acnEncodeValue  : BigInteger
    Comments: string array
}

type Optional = {
    defaultValue        : Asn1Value option
    acnPresentWhen      : PresenceWhenBool option
}

type Asn1Optionality =
    | AlwaysAbsent
    | AlwaysPresent
    | Optional          of Optional

type Asn1ChoiceOptionality =
    | ChoiceAlwaysAbsent
    | ChoiceAlwaysPresent

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN ENCODING CLASSES    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type IntEncodingClass =
    |Integer_uPER
    |PositiveInteger_ConstSize_8
    |PositiveInteger_ConstSize_big_endian_16
    |PositiveInteger_ConstSize_little_endian_16
    |PositiveInteger_ConstSize_big_endian_32
    |PositiveInteger_ConstSize_little_endian_32
    |PositiveInteger_ConstSize_big_endian_64
    |PositiveInteger_ConstSize_little_endian_64
    |PositiveInteger_ConstSize of BigInteger
    |TwosComplement_ConstSize_8
    |TwosComplement_ConstSize_big_endian_16
    |TwosComplement_ConstSize_little_endian_16
    |TwosComplement_ConstSize_big_endian_32
    |TwosComplement_ConstSize_little_endian_32
    |TwosComplement_ConstSize_big_endian_64
    |TwosComplement_ConstSize_little_endian_64
    |TwosComplement_ConstSize of BigInteger
    |ASCII_ConstSize of BigInteger
    |ASCII_VarSize_NullTerminated of byte list
    |ASCII_UINT_ConstSize of BigInteger
    |ASCII_UINT_VarSize_NullTerminated of byte  list
    |BCD_ConstSize of BigInteger
    |BCD_VarSize_NullTerminated of byte  list


type RealEncodingClass =
    | Real_uPER
    | Real_IEEE754_32_big_endian
    | Real_IEEE754_64_big_endian
    | Real_IEEE754_32_little_endian
    | Real_IEEE754_64_little_endian

type StringAcnEncodingClass =
    | Acn_Enc_String_uPER                                   of BigInteger                          //char size in bits, as in uper
    | Acn_Enc_String_uPER_Ascii                             of BigInteger                          //char size in bits, as in uper but with charset (0..255)
    | Acn_Enc_String_Ascii_Null_Terminated                  of BigInteger*(byte  list)             //char size in bits, byte = the null character
    | Acn_Enc_String_Ascii_External_Field_Determinant       of BigInteger*RelativePath             //char size in bits, encode ascii, size is provided by an external length determinant
    | Acn_Enc_String_CharIndex_External_Field_Determinant   of BigInteger*RelativePath             //char size in bits, encode char index, size is provided by an external length determinant
with
    member this.charSizeInBits: bigint =
        match this with
        | Acn_Enc_String_uPER c
        | Acn_Enc_String_uPER_Ascii c
        | Acn_Enc_String_Ascii_Null_Terminated (c, _)
        | Acn_Enc_String_Ascii_External_Field_Determinant (c, _)
        | Acn_Enc_String_CharIndex_External_Field_Determinant (c, _) -> c

type SizeableAcnEncodingClass =
    //| SZ_EC_uPER
    | SZ_EC_FIXED_SIZE
    | SZ_EC_LENGTH_EMBEDDED     of BigInteger //embedded length determinant size in bits
    | SZ_EC_ExternalField       of RelativePath
    | SZ_EC_TerminationPattern  of BitStringValue

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// FRONT END TYPE DEFINITIONS   /////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ASN1 WITH ACN INFORMATION  DEFINITION    /////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////




type BigIntegerUperRange = uperRange<BigInteger>
type DoubleUperRange = uperRange<Double>
type UInt32UperRange = uperRange<uint32>

type IntegerClass =
    | ASN1SCC_Int8      of BigInteger*BigInteger
    | ASN1SCC_Int16     of BigInteger*BigInteger
    | ASN1SCC_Int32     of BigInteger*BigInteger
    | ASN1SCC_Int64     of BigInteger*BigInteger
    | ASN1SCC_Int       of BigInteger*BigInteger
    | ASN1SCC_UInt8     of BigInteger*BigInteger
    | ASN1SCC_UInt16    of BigInteger*BigInteger
    | ASN1SCC_UInt32    of BigInteger*BigInteger
    | ASN1SCC_UInt64    of BigInteger*BigInteger
    | ASN1SCC_UInt      of BigInteger*BigInteger

type RealClass =
    | ASN1SCC_REAL
    | ASN1SCC_FP32
    | ASN1SCC_FP64


type Integer = {
    acnProperties       : IntegerAcnProperties
    cons                : IntegerTypeConstraint list
    withcons            : IntegerTypeConstraint list
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger
    uperRange           : BigIntegerUperRange
    intClass            : IntegerClass
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : IntEncodingClass
    isUnsigned          : bool
    typeDef             : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    defaultInitVal      : String
}

type Real = {
    acnProperties       : RealAcnProperties
    cons                : RealTypeConstraint list
    withcons            : RealTypeConstraint list
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger
    uperRange           : DoubleUperRange

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : RealEncodingClass

    typeDef             : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    defaultInitVal      : String
}

type StringType = {
    acnProperties       : StringAcnProperties
    cons                : IA5StringConstraint list
    withcons            : IA5StringConstraint list

    minSize             : SIZE
    maxSize             : SIZE
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger
    uperCharSet         : char array

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : StringAcnEncodingClass
    isNumeric           : bool
    typeDef             : Map<ProgrammingLanguage, FE_StringTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
}


type OctetString = {
    acnProperties       : SizeableAcnProperties
    cons                : OctetStringConstraint list
    withcons            : OctetStringConstraint list
    minSize             : SIZE
    maxSize             : SIZE
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : SizeableAcnEncodingClass
    acnArgs             : RelativePath list
    typeDef             : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>

}

type BitString = {
    acnProperties   : SizeableAcnProperties
    cons                : BitStringConstraint list
    withcons            : BitStringConstraint list
    minSize             : SIZE
    maxSize             : SIZE
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : SizeableAcnEncodingClass
    acnArgs             : RelativePath list
    typeDef             : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    namedBitList        : NamedBit1 list
}

type TimeType = {
    timeClass       :   TimeTypeClass
    cons                : TimeConstraint list
    withcons            : TimeConstraint list
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    typeDef             : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
}

type NullType = {
    acnProperties       : NullTypeAcnProperties
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    typeDef             : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    defaultInitVal      : String
}

type Boolean = {
    acnProperties       : BooleanAcnProperties
    cons                : BoolConstraint list
    withcons            : BoolConstraint list
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    typeDef             : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    defaultInitVal      : String
}

type ObjectIdentifier = {
    acnProperties       : ObjectIdTypeAcnProperties
    cons                : ObjectIdConstraint list
    withcons            : ObjectIdConstraint list
    relativeObjectId    : bool
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    typeDef             : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
}


type Enumerated = {
    items               : NamedItem list
    validItems          : NamedItem list            //this list contains only the items that are valid (i.e.returned true) after the constraints are applied
    acnProperties       : IntegerAcnProperties
    cons                : EnumConstraint list
    withcons            : EnumConstraint list
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : IntEncodingClass
    encodeValues        : bool
    userDefinedValues   : bool      //if true, the user has associated at least one item with a value
    typeDef             : Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
}

type AcnReferenceToEnumerated = {
    modName             : StringLoc
    tasName             : StringLoc
    enumerated          : Enumerated
    acnAlignment         : AcnAlignment option
}


type AcnReferenceToIA5String = {
    modName             : StringLoc
    tasName             : StringLoc
    str                 : StringType
    acnAlignment        : AcnAlignment option
}

type AcnInteger = {
    acnProperties       : IntegerAcnProperties
    cons                : IntegerTypeConstraint list
    withcons            : IntegerTypeConstraint list
    acnAlignment        : AcnAlignment option
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : IntEncodingClass
    Location            : SrcLoc //Line no, Char pos
    uperRange           : BigIntegerUperRange
    intClass            : IntegerClass
    isUnsigned          : bool
    checkIntHasEnoughSpace  : BigInteger -> BigInteger -> unit
    inheritInfo          : InheritanceInfo option
    defaultValue        : string
}

type AcnBoolean = {
    acnProperties       : BooleanAcnProperties
    acnAlignment        : AcnAlignment option
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    Location            : SrcLoc //Line no, Char pos
    defaultValue        : string
}

type AcnNullType = {
    acnProperties       : NullTypeAcnProperties
    acnAlignment        : AcnAlignment option
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    Location            : SrcLoc //Line no, Char pos
    defaultValue        : string
}

type AcnInsertedType =
    | AcnInteger                of AcnInteger
    | AcnNullType               of AcnNullType
    | AcnBoolean                of AcnBoolean
    | AcnReferenceToEnumerated  of AcnReferenceToEnumerated
    | AcnReferenceToIA5String   of AcnReferenceToIA5String
with
    member this.AsString =
        match this with
        | AcnInteger  _                 -> "INTEGER"
        | AcnNullType _                 -> "NULL"
        | AcnBoolean  _                 -> "BOOLEAN"
        | AcnReferenceToEnumerated o    -> sprintf "%s.%s" o.modName.Value o.tasName.Value
        | AcnReferenceToIA5String  o    -> sprintf "%s.%s" o.modName.Value o.tasName.Value
    member this.acnAlignment =
        match this with
        | AcnInteger  o                 -> o.acnAlignment
        | AcnNullType o                 -> o.acnAlignment
        | AcnBoolean  o                 -> o.acnAlignment
        | AcnReferenceToEnumerated o    -> o.acnAlignment
        | AcnReferenceToIA5String  o    -> o.acnAlignment
    member this.savePosition  =
        match this with
        | AcnInteger  a                 -> false
        | AcnBoolean  a                 -> false
        | AcnNullType a                 -> a.acnProperties.savePosition
        | AcnReferenceToEnumerated a    -> false
        | AcnReferenceToIA5String a     -> false
    member this.acnMaxSizeInBits  =
        match this with
        | AcnInteger  a -> a.acnMaxSizeInBits
        | AcnBoolean  a -> a.acnMaxSizeInBits
        | AcnNullType a -> a.acnMaxSizeInBits
        | AcnReferenceToEnumerated a -> a.enumerated.acnMaxSizeInBits
        | AcnReferenceToIA5String a -> a.str.acnMaxSizeInBits


type Asn1Type = {
    id              : ReferenceToType
    parameterizedTypeInstance : bool
    Kind            : Asn1TypeKind
    acnAlignment    : AcnAlignment option
    acnParameters   : AcnParameter list
    Location        : SrcLoc //Line no, Char pos
    moduleName      : string
    acnLocation     : SrcLoc option

    /// Indicates that this type
    /// is a subclass (or inherits) from referenceType
    /// (i.e. this type resolves the reference type)
    inheritInfo     : InheritanceInfo option

    /// it indicates that this type is directly under a type assignment.
    typeAssignmentInfo  : AssignmentInfo option

    acnEncSpecPosition          : (SrcLoc*SrcLoc) option   //start pos, end pos
    acnEncSpecAntlrSubTree      : ITree option
    unitsOfMeasure : string option

    referencedBy    : TypeAssignmentInfo list
}
 with
    member this.externalDependencies: RelativePath list =
        match this.Kind with
        | ReferenceType tp -> tp.resolvedType.externalDependencies
        | Sequence sq ->
            let prefixes = sq.children |> List.map (fun c ->
                match c with
                | Asn1Child c -> c.Name.Value
                | AcnChild c -> c.id.lastItem
            )
            this.allDependencies |> List.filter (fun dep ->
                prefixes |> List.forall (fun prefix -> not (List.isPrefixOf [prefix] dep.asStringList)))
        | _ -> this.allDependencies

    member this.allDependencies: RelativePath list =
        match this.Kind with
        | ReferenceType tp -> tp.resolvedType.allDependencies
        | Sequence sq ->
            sq.acnArgs @ (sq.children |> List.collect (fun c ->
                match c with
                | Asn1Child c -> c.Type.allDependencies
                | AcnChild _ -> []
            ))
        | Choice ch -> ch.acnArgs
        | SequenceOf sqf -> sqf.acnArgs
        | OctetString os -> os.acnArgs
        | BitString bs -> bs.acnArgs
        | _ -> []
    
    member this.maxAlignment =
        match this.Kind with
        | Sequence sq -> sq.maxAlignment
        | SequenceOf sqf -> sqf.maxAlignment
        | Choice ch -> ch.maxAlignment
        | ReferenceType tp -> tp.maxAlignment
        | _ -> this.acnAlignment


and Asn1TypeKind =
    | Integer           of Integer
    | Real              of Real
    | IA5String         of StringType
    | NumericString     of StringType
    | OctetString       of OctetString
    | NullType          of NullType
    | TimeType          of TimeType
    | BitString         of BitString
    | Boolean           of Boolean
    | Enumerated        of Enumerated
    | SequenceOf        of SequenceOf
    | Sequence          of Sequence
    | Choice            of Choice
    | ObjectIdentifier  of ObjectIdentifier
    | ReferenceType     of ReferenceType

and SequenceOf = {
    child           : Asn1Type
    acnProperties   : SizeableAcnProperties
    cons                : SequenceOfConstraint list
    withcons            : SequenceOfConstraint list
    minSize             : SIZE
    maxSize             : SIZE
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : SizeableAcnEncodingClass
    acnArgs             : RelativePath list
    typeDef             : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    maxAlignment        : AcnAlignment option

}

and Sequence = {
    children                : SeqChildInfo list
    acnProperties           : SequenceAcnProperties
    cons                    : SeqConstraint list
    withcons                : SeqConstraint list
    uperMaxSizeInBits       : BigInteger
    uperMinSizeInBits       : BigInteger
    acnMaxSizeInBits        : BigInteger
    acnMinSizeInBits        : BigInteger
    acnArgs                 : RelativePath list
    typeDef                 : Map<ProgrammingLanguage, FE_SequenceTypeDefinition>
    definitionOrRef         : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    maxAlignment        : AcnAlignment option
}

and AcnChild = {
    Name                        : StringLoc
    c_name                      : string
    id                          : ReferenceToType
    Type                        : AcnInsertedType
    Comments                    : string array
}

and SeqChildInfo =
    | Asn1Child of Asn1Child
    | AcnChild  of AcnChild


and Asn1Child = {
    Name                        : StringLoc
    _c_name                     : string
    _scala_name                 : string
    _ada_name                   : string
    Type                        : Asn1Type
    Optionality                 : Asn1Optionality option
    // acnArgs                     : RelativePath list // TODO: RM?
    asn1Comments                : string list
    acnComments                 : string list
}
with
    member this.Comments = this.asn1Comments@this.acnComments




and Choice = {
    children            : ChChildInfo list
    acnProperties       : ChoiceAcnProperties
    cons                : ChoiceConstraint list
    withcons            : ChoiceConstraint list
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnParameters       : AcnParameter list
    // detArg              : RelativePath option
    acnArgs             : RelativePath list
    acnLoc              : SrcLoc option
    typeDef             : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    maxAlignment        : AcnAlignment option
}

and ChChildInfo = {
    Name                        : StringLoc
    _c_name                     : string
    _scala_name                 : string
    _ada_name                   : string
    present_when_name           : string // Does not contain the "_PRESENT". Not to be used directly by backends.
    Type                        : Asn1Type
    acnPresentWhenConditions    : AcnPresentWhenConditionChoiceChild list
    asn1Comments                : string list
    acnComments                 : string list
    Optionality                 : Asn1ChoiceOptionality option
}
with
    member this.Comments = this.asn1Comments@this.acnComments


and EncodeWithinOctetOrBitStringProperties = {
    minSize             : SIZE
    maxSize             : SIZE
    acnEncodingClass    : SizeableAcnEncodingClass
    octOrBitStr         : ContainedInOctOrBitString
}

and ReferenceType = {
    modName     : StringLoc
    tasName     : StringLoc
    tabularized : bool
    acnArguments: RelativePath list
    resolvedType: Asn1Type
    hasConstraints : bool
    hasExtraConstrainsOrChildrenOrAcnArgs : bool
    typeDef             : Map<ProgrammingLanguage, FE_TypeDefinition>
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    encodingOptions        : EncodeWithinOctetOrBitStringProperties option
    refCons             : AnyConstraint list
    definitionOrRef     : Map<ProgrammingLanguage, TypeDefinitionOrReference>
    maxAlignment        : AcnAlignment option
}

type Asn1AcnType =
    | Acn of AcnInsertedType
    | Asn1 of Asn1Type

type TypeAssignment = {
    Name:StringLoc
    c_name:string
    scala_name:string
    ada_name:string
    Type:Asn1Type
    asn1Comments: string list
    acnComments : string list
}
with
    member this.Comments = this.asn1Comments@this.acnComments

type ValueAssignment = {
    Name:StringLoc
    c_name:string
    scala_name:string
    ada_name:string
    Type:Asn1Type
    Value:Asn1Value
}


type Asn1Module = {
    Name : StringLoc
    TypeAssignments : list<TypeAssignment>
    typeAssignmentsMap : Map<string, TypeAssignment>
    ValueAssignments : list<ValueAssignment>
    valueAssignmentsMap : Map<string, ValueAssignment>
    Imports : list<Asn1Ast.ImportedModule>
    typeImportMap : Map<string, Asn1Ast.ImportedModule> //key is the imported type name, value is the imported module
    Exports : Asn1Ast.Exports
    Comments : string array
    position : SrcLoc*SrcLoc   //start pos, end pos
}

type Asn1File = {
    FileName:string;
    Tokens: IToken array
    Modules : list<Asn1Module>
}

type AstRoot = {
    Files: list<Asn1File>
    modulesMap : Map<string, Asn1Module>
    typeAssignmentsMap : Map<string*string, TypeAssignment>
    acnConstants : Map<string, BigInteger>
    args:CommandLineSettings
    acnParseResults:CommonTypes.AntlrParserResult list //used in ICDs to regenerate with colors the initial ACN input
    allDependencies : (TypeAssignmentInfo*TypeAssignmentInfo) list      //caller, callee
}



type ReferenceToEnumerated = {
    modName : string
    tasName : string
    enm     : Enumerated
}

type AcnDependencyKind =
    | AcnDepIA5StringSizeDeterminant of SIZE * SIZE * StringAcnProperties   // The asn1Type has a size dependency in IA5String etc
    | AcnDepSizeDeterminant of SIZE * SIZE * SizeableAcnProperties          // The asn1Type has a size dependency a SEQUENCE OF, BIT STRING, OCTET STRING etc
    | AcnDepSizeDeterminant_bit_oct_str_contain of ReferenceType            // The asn1Type has a size dependency a BIT STRING, OCTET STRING containing another type
    | AcnDepRefTypeArgument of AcnParameter                                 // string is the param name
    | AcnDepPresenceBool                                                    // points to a SEQUENCE or Choice child
    | AcnDepPresence of RelativePath * Choice
    | AcnDepPresenceStr of RelativePath * Choice * StringType
    | AcnDepChoiceDeterminant of ReferenceToEnumerated * Choice * bool      // points to Enumerated type acting as CHOICE determinant; is optional
    with
        member this.isString =
            match this with
            | AcnDepIA5StringSizeDeterminant _ -> true
            | _ -> false

type Determinant =
    | AcnChildDeterminant       of AcnChild
    | AcnParameterDeterminant   of AcnParameter
    with
        member this.id =
            match this with
            | AcnChildDeterminant       c  -> c.id
            | AcnParameterDeterminant   p  -> p.id

//The following type expresses the dependencies that exists between ASN.1 types and ACNs types and parameters
type AcnDependency = {
    asn1Type        : ReferenceToType      // an ASN.1 type that its decoding depends on the determinant
    determinant     : Determinant          // an ACN inserted type or an ACN parameter that acts as determinant
    dependencyKind  : AcnDependencyKind
}

type AcnInsertedFieldDependencies = {
    acnDependencies: AcnDependency list
}



type Asn1AcnMergeState = {
    args:CommandLineSettings

    //the folowin map contains the currently allocated type names for each language
    //if value of the map is a list of the module names where the type is defined (obviously we have different types with the same name but in different modules)
    allocatedTypeNames          : Map<(ProgrammingLanguage*string), string list>      //language, type definition name, program unit lists 
    allocatedFE_TypeDefinition  : Map<(ProgrammingLanguage*ReferenceToType), FE_TypeDefinition>
    temporaryTypesAllocation    : Map<(ProgrammingLanguage*ReferenceToType), string>
    allDependencies             : (TypeAssignmentInfo*TypeAssignmentInfo) list      //caller, callee
}
