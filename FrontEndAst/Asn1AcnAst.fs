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
and ObjectIdenfierValue   = ((ResolvedObjectIdentifierValueCompoent list)*(ObjectIdentifierValueCompoent list))

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
    | ObjOrRelObjIdValue    of ObjectIdenfierValue


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ASN1 CONSTRAINTS DEFINITION    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type GenericConstraint<'v> =
    | UnionConstraint                   of string*GenericConstraint<'v>*GenericConstraint<'v>*bool //left,righ, virtual constraint
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
    | RangeUnionConstraint               of string*RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>*bool //left,righ, virtual constraint
    | RangeIntersectionConstraint        of string*RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeAllExceptConstraint           of string*RangeTypeConstraint<'v1,'v2>
    | RangeExceptConstraint              of string*RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeRootConstraint                of string*RangeTypeConstraint<'v1,'v2>
    | RangeRootConstraint2               of string*RangeTypeConstraint<'v1,'v2>*RangeTypeConstraint<'v1,'v2>
    | RangeSingleValueConstraint         of string*'v2
    | RangeContraint                     of string*('v1) *('v1)*bool*bool    //min, max, InclusiveMin(=true), InclusiveMax(=true)
    | RangeContraint_val_MAX             of string*('v1) *bool            //min, InclusiveMin(=true)
    | RangeContraint_MIN_val             of string*('v1) *bool            //max, InclusiveMax(=true)
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
            | RangeContraint                     (s,_,_,_,_) -> s
            | RangeContraint_val_MAX             (s,_,_) -> s
            | RangeContraint_MIN_val             (s,_,_) -> s


type IntegerTypeConstraint  = RangeTypeConstraint<BigInteger, BigInteger>
type PosIntTypeConstraint   = RangeTypeConstraint<UInt32, UInt32>
type CharTypeConstraint     = RangeTypeConstraint<char, string>
    
type RealTypeConstraint     = RangeTypeConstraint<double, double>


type SizableTypeConstraint<'v>  = 
    | SizeUnionConstraint               of string*SizableTypeConstraint<'v>*SizableTypeConstraint<'v>*bool //left,righ, virtual constraint
    | SizeIntersectionConstraint        of string*SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeAllExceptConstraint           of string*SizableTypeConstraint<'v>
    | SizeExceptConstraint              of string*SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeRootConstraint                of string*SizableTypeConstraint<'v>
    | SizeRootConstraint2               of string*SizableTypeConstraint<'v>*SizableTypeConstraint<'v>
    | SizeSingleValueConstraint         of string*'v
    | SizeContraint                     of string*PosIntTypeConstraint               
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
                | SizeContraint             (s,_) -> s

type IA5StringConstraint = 
    | StrUnionConstraint               of string*IA5StringConstraint*IA5StringConstraint*bool //left,righ, virtual constraint
    | StrIntersectionConstraint        of string*IA5StringConstraint*IA5StringConstraint
    | StrAllExceptConstraint           of string*IA5StringConstraint
    | StrExceptConstraint              of string*IA5StringConstraint*IA5StringConstraint
    | StrRootConstraint                of string*IA5StringConstraint
    | StrRootConstraint2               of string*IA5StringConstraint*IA5StringConstraint
    | StrSingleValueConstraint         of string*string
    | StrSizeContraint                 of string*PosIntTypeConstraint               
    | AlphabetContraint                of string*CharTypeConstraint           
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
                | StrSizeContraint          (s,_) -> s
                | AlphabetContraint         (s,_) -> s









type OctetStringConstraint  =    SizableTypeConstraint<OctetStringValue*(ReferenceToValue*SrcLoc)>
type BitStringConstraint    =    SizableTypeConstraint<BitStringValue*(ReferenceToValue*SrcLoc)>
type BoolConstraint         =    GenericConstraint<bool>
type EnumConstraint         =    GenericConstraint<string>
type ObjectIdConstraint     =    GenericConstraint<ObjectIdenfierValue>
type TimeConstraint         =    GenericConstraint<Asn1DateTimeValue>


//type SequenceOfConstraint   =     SizableTypeConstraint<SeqOfValue>
//type SequenceConstraint     =     GenericConstraint<SeqValue>

type SeqOrChoiceConstraint<'v> =
    | SeqOrChUnionConstraint                   of string*SeqOrChoiceConstraint<'v>*SeqOrChoiceConstraint<'v>*bool //left,righ, virtual constraint
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
    | SeqOfSizeUnionConstraint               of string*SequenceOfConstraint*SequenceOfConstraint*bool //left,righ, virtual constraint
    | SeqOfSizeIntersectionConstraint        of string*SequenceOfConstraint*SequenceOfConstraint
    | SeqOfSizeAllExceptConstraint           of string*SequenceOfConstraint
    | SeqOfSizeExceptConstraint              of string*SequenceOfConstraint*SequenceOfConstraint
    | SeqOfSizeRootConstraint                of string*SequenceOfConstraint
    | SeqOfSizeRootConstraint2               of string*SequenceOfConstraint*SequenceOfConstraint
    | SeqOfSizeSingleValueConstraint         of string*SeqOfValue
    | SeqOfSizeContraint                     of string*PosIntTypeConstraint               
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
                | SeqOfSizeContraint               (s,_) -> s
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
    Contraint:AnyConstraint option
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
    | Acn_Enc_String_Ascii_Null_Teminated                   of BigInteger*(byte  list)             //char size in bits, byte = the null character
    | Acn_Enc_String_Ascii_External_Field_Determinant       of BigInteger*RelativePath             //char size in bits, encode ascii, size is provided by an external length determinant
    | Acn_Enc_String_CharIndex_External_Field_Determinant   of BigInteger*RelativePath             //char size in bits, encode char index, size is provided by an external length determinant

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
    defaultInitVal      : String
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
    typeDef             : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>

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
    typeDef             : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>
    namedBitList        : NamedBit1 list
    defaultInitVal      : String
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
}

type NullType = {
    acnProperties       : NullTypeAcnProperties
    uperMaxSizeInBits   : BigInteger
    uperMinSizeInBits   : BigInteger

    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    typeDef             : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>
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
}


type Enumerated = {
    items               : NamedItem list
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
    defaultInitVal      : String
}

type AcnReferenceToEnumerated = {
    modName             : StringLoc
    tasName             : StringLoc
    enumerated          : Enumerated
    acnAligment         : AcnAligment option
}


type AcnReferenceToIA5String = {
    modName             : StringLoc
    tasName             : StringLoc
    str                 : StringType
    acnAligment         : AcnAligment option
}

type AcnInteger = {
    acnProperties       : IntegerAcnProperties
    cons                : IntegerTypeConstraint list
    withcons            : IntegerTypeConstraint list
    acnAligment         : AcnAligment option
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    acnEncodingClass    : IntEncodingClass
    Location            : SrcLoc //Line no, Char pos
    uperRange           : BigIntegerUperRange
    intClass            : IntegerClass
    isUnsigned          : bool
    checkIntHasEnoughSpace  : BigInteger -> BigInteger -> unit
    inheritInfo          : InheritanceInfo option
}

type AcnBoolean = {
    acnProperties       : BooleanAcnProperties
    acnAligment         : AcnAligment option
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    Location            : SrcLoc //Line no, Char pos
}

type AcnNullType = {
    acnProperties       : NullTypeAcnProperties
    acnAligment         : AcnAligment option
    acnMaxSizeInBits    : BigInteger
    acnMinSizeInBits    : BigInteger
    Location            : SrcLoc //Line no, Char pos
}

type  AcnInsertedType = 
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
    member this.acnAligment =
        match this with
        | AcnInteger  o                 -> o.acnAligment
        | AcnNullType o                 -> o.acnAligment
        | AcnBoolean  o                 -> o.acnAligment
        | AcnReferenceToEnumerated o    -> o.acnAligment
        | AcnReferenceToIA5String  o    -> o.acnAligment
    member this.savePosition  =
        match this with            
        | AcnInteger  a                 -> false
        | AcnBoolean  a                 -> false
        | AcnNullType a                 -> a.acnProperties.savePosition 
        | AcnReferenceToEnumerated a    -> false
        | AcnReferenceToIA5String a     -> false




type Asn1Type = {
    id              : ReferenceToType
    parameterizedTypeInstance : bool
    Kind            : Asn1TypeKind
    acnAligment     : AcnAligment option
    acnParameters   : AcnParameter list
    Location        : SrcLoc //Line no, Char pos
    moduleName      : string
    acnLocation     : SrcLoc option

    /// Indicates that this type
    /// is a subclass (or inherits) from referencType
    /// (i.e. this type resolves the reference type)
    inheritInfo     : InheritanceInfo option

    /// it indicates that this type is directly under a type assignment.
    typeAssignmentInfo  : AssignmentInfo option

    acnEncSpecPostion           : (SrcLoc*SrcLoc) option   //start pos, end pos
    acnEncSpecAntlrSubTree      :ITree option
    unitsOfMeasure : string option
}


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
    typeDef             : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>

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
    typeDef                 : Map<ProgrammingLanguage, FE_SequenceTypeDefinition>
}

and AcnChild = {
    Name                        : StringLoc
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
    acnLoc              : SrcLoc option
    typeDef             : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>
    defaultInitVal      : String
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
}


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
    ValueAssignments : list<ValueAssignment>
    Imports : list<Asn1Ast.ImportedModule>
    Exports : Asn1Ast.Exports
    Comments : string array
    postion : SrcLoc*SrcLoc   //start pos, end pos
}

type Asn1File = {
    FileName:string;
    Tokens: IToken array
    Modules : list<Asn1Module>
}

type AstRoot = {
    Files: list<Asn1File>
    acnConstants : Map<string, BigInteger>
    args:CommandLineSettings
    acnParseResults:CommonTypes.AntlrParserResult list //used in ICDs to regenerate with collors the initial ACN input
}



type ReferenceToEnumerated = {
    modName : string
    tasName : string
    enm     : Enumerated
}

type AcnDependencyKind = 
    | AcnDepIA5StringSizeDeterminant  of (SIZE*SIZE*StringAcnProperties)                // The asn1Type has a size dependency in IA5String etc
    | AcnDepSizeDeterminant     of (SIZE*SIZE*SizeableAcnProperties)               // The asn1Type has a size dependency a SEQUENCE OF, BIT STRINT, OCTET STRING etc
    | AcnDepSizeDeterminant_bit_oct_str_containt     of ReferenceType             // The asn1Type has a size dependency a BIT STRINT, OCTET STRING containing another type
    | AcnDepRefTypeArgument       of AcnParameter        // string is the param name
    | AcnDepPresenceBool                     // points to a SEQEUNCE or Choice child
    | AcnDepPresence              of (RelativePath*Choice)
    | AcnDepPresenceStr           of (RelativePath*Choice*StringType)
    | AcnDepChoiceDeteterminant   of (ReferenceToEnumerated*Choice)           // points to Enumerated type acting as CHOICE determinant.

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
    acnDependencies                         : AcnDependency list
}



type Asn1AcnMergeState = {
    args:CommandLineSettings    
    allocatedTypeNames          : (ProgrammingLanguage*string*string)  list     //language, program unit, type definition name
    allocatedFE_TypeDefinition  : Map<(ProgrammingLanguage*ReferenceToType), FE_TypeDefinition>
    temporaryTypesAllocation    : Map<(ProgrammingLanguage*ReferenceToType), string>
}
