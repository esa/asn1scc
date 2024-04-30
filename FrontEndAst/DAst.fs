module DAst

open Antlr.Runtime.Tree
open Antlr.Runtime
open System
open System.Numerics
open FsUtils
open CommonTypes
open AcnGenericTypes
open AbstractMacros
open System.Collections.Generic

//open Constraints





type CallerScope = {
    modName : string
    arg     : Selection
}

type AlphaFunc   = {
    funcName            : string
    funcBody            : CallerScope -> string
}


type IcdTypeCol =
    | TypeHash of string
    | IcdPlainType of string

(*
let getIcdTypeCol_label  l =
    match l with
    | IcdRefType (l,_)
    | IcdPlainType l -> l
*)

type IcdRowType =
    | FieldRow
    | ReferenceToCompositeTypeRow
    | LengthDeterminantRow
    | PresentDeterminantRow
    | ThreeDOTs

type IcdRow = {
    idxOffset :int option
    fieldName : string
    comments  : string list
    sPresent  : string
    sType     : IcdTypeCol
    sConstraint : string option
    minLengthInBits : BigInteger
    maxLengthInBits : BigInteger
    sUnits      : string option
    rowType     : IcdRowType
}

type IcdTypeAss = {
    linkId  : ReferenceToType
    tasInfo : TypeAssignmentInfo option
    asn1Link : string option
    acnLink : string option
    name : string
    kind : string
    comments : string list
    rows : IcdRow list
    //compositeChildren : IcdTypeAss list
    minLengthInBytes : BigInteger
    maxLengthInBytes : BigInteger
    hash            : string
}
type State = {
    currErrorCode   : int
    curErrCodeNames : Set<String>
    //allocatedTypeDefNames : (string*string) list        // program unit, typedef name
    //allocatedTypeDefNameInTas : Map<TypeAssignmentInfo, (string*string)>
    alphaIndex : int
    alphaFuncs : AlphaFunc list //func name, func body
    typeIdsSet : Map<String,int>
    newTypesMap : Dictionary<ReferenceToType, System.Object>
    icdHashes   : Map<String, IcdTypeAss list>
}


let emptyState = {currErrorCode=0; curErrCodeNames=Set.empty; (*allocatedTypeDefNames = []; allocatedTypeDefNameInTas = Map.empty;*) alphaIndex=0; alphaFuncs=[]; typeIdsSet=Map.empty; newTypesMap = new Dictionary<ReferenceToType, System.Object>(); icdHashes = Map.empty}




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ASN1 VALUES DEFINITION    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type IntegerValue         = BigInteger
type RealValue            = double
type StringValue          = (SingleStringValue list*SrcLoc)
type BooleanValue         = bool
type BitStringValue       = string
type OctetStringValue     = list<byte>
type EnumValue            = string
type NullValue            = unit
type SeqOfValue           = list<Asn1Value>
and SeqValue              = list<NamedValue>
and ChValue               = NamedValue
and RefValue              = ((string*string)*Asn1Value)
and TimeValue             = Asn1DateTimeValue
and ObjectIdentifierValue   =
    | Asn1DefinedObjectIdentifierValue of ((ResolvedObjectIdentifierValueComponent list)*(ObjectIdentifierValueComponent list))
    | InternalObjectIdentifierValue of BigInteger list

and NamedValue = {
    name        : string
    Value       : Asn1Value
}

and Asn1Value = {
    kind : Asn1ValueKind
    loc  : SrcLoc
    id   : ReferenceToValue
    moduleName      : string
}

and Asn1ValueKind =
    | IntegerValue          of IntegerValue
    | RealValue             of RealValue
    | StringValue           of StringValue
    | TimeValue             of TimeValue
    | BooleanValue          of BooleanValue
    | BitStringValue        of BitStringValue
    | OctetStringValue      of OctetStringValue
    | EnumValue             of EnumValue
    | SeqOfValue            of SeqOfValue
    | SeqValue              of SeqValue
    | ChValue               of ChValue
    | NullValue             of NullValue
    | RefValue              of RefValue
    | ObjOrRelObjIdValue    of ObjectIdentifierValue

//type Asn1GenericValue = Asn1Value




type ExpOrStatement =
    | Expression
    | Statement

type GenericLocalVariable =
    {
        name     : string
        varType  : string
        arrSize  : string option     //if none then it is not an array
        isStatic : bool
        initExp  : string option
    }

type LocalVariable =
    | SequenceOfIndex       of int*string option     //i index, initialValue
    | IntegerLocalVariable  of string*string option  //variable name, initialValue
    | Asn1SIntLocalVariable of string*string option  //variable name, initialValue
    | Asn1UIntLocalVariable of string*string option  //variable name, initialValue
    | FlagLocalVariable     of string*string option  //variable name, initialValue
    | BooleanLocalVariable  of string*string option  //variable name, initialValue
    | AcnInsertedChild      of string*string*string  //variable name, type, initialValue
    | GenericLocalVariable  of GenericLocalVariable



(*an expression or statement that checks if a constraint is met or not. IT DOES NOT ASSIGN the error code field*)
type ValidationCodeBlock =
    | VCBTrue                                // always true
    | VCBFalse                               // always false
    | VCBExpression of string                // single expression
    | VCBStatement  of (string * LocalVariable list)                // statement that updates ret or Result.Success

(*a statement that checks if a constraint is met or not and which assigns the error code*)
type ValidationStatement =
    | ValidationStatementTrue   of (string * LocalVariable list)
    | ValidationStatementFalse  of (string * LocalVariable list)
    | ValidationStatement       of (string * LocalVariable list)


type ReferenceToExistingDefinition = {
    /// the module where this type is defined
    /// if the value is not present then is the same as the "caller"
    programUnit     : string option
    /// The name of the defined type.
    typedefName     : string
    definedInRtl    : bool
}

type TypeDefinition = {
    // The program unit where this type is defined.
    // In C this is None
    //programUnitName : string option
    /// The name of the defined type. If type is a type assignment then is the name of the type assignment.
    /// if the type is an inner type (i.e. within a SEQUENCE/SEQUENCE OF/CHOICE) then name is created as
    /// parentType.typedefName + "_" + component_name
    typedefName : string

    /// the complete definition of the type
    /// e.g. C : typedef asn1SccSint MyInt4;
    /// and Ada: SUBTYPE MyInt4 IS adaasn1rtl.Asn1Int range 0..25;
    /// For composite types, typedefBody contains also the definition of any
    /// inner children
    typedefBody : unit -> string
    baseType    : ReferenceToExistingDefinition option

    /// extra information that is needed for the type definition
    /// E.g. the name of the array that is used to store the enumerated values, used in enum efficient encoding
    /// This information is not exposed to other types, it is used only in the implementation of the type
    /// and is not part of the public interface. That is why it should be defined in the implementation file, not in the header file
    privateTypeDefinition : string option
}

type TypeDefinitionOrReference =
    /// indicates that no extra type definition is required (e.g. INTEGER without constraints or type reference type without new constraints)
    | ReferenceToExistingDefinition    of ReferenceToExistingDefinition
    /// indicates that a new type is
    | TypeDefinition                of TypeDefinition

type ErrorCode = {
    errCodeValue    : int
    errCodeName     : string
    comment         : string option
}

type BaseTypesEquivalence<'T> = {
    typeDefinition  : 'T option
    uper            : 'T option
    acn             : 'T option
}

(*
Generates initialization statement(s) that initialize the type with the given Asn1GeneticValue.
*)
type InitFunctionResult = {
    funcBody            : string
    localVariables      : LocalVariable list
}

type TestCaseValue =
    | TcvComponentPresent
    | TcvComponentAbsent
    | TcvAnyValue
    | TcvEnumeratedValue of String
    | TcvSizeableTypeValue of BigInteger       //length
    | TcvChoiceAlternativePresentWhenInt of BigInteger
    | TcvChoiceAlternativePresentWhenStr of String

(*
In general, an automatic test involves many types (e.g. in sequences, choices etc). It consists of function (initTestCaseFunc) that returns
a string with the statements than initialize all involved types plus the local variables needed.
The id of the types that are involved in this automatic test case are stored within a map with name testCaseTypeIDsMap. The need for this map
is in order to generate valid ACN test cases. I.e. the ACN checks that test case provides values for all ACN inserted fields. Otherwise is invalid and not
generated.
*)
type AutomaticTestCase = {
    initTestCaseFunc        : CallerScope  -> InitFunctionResult //returns a list of set the statement(s) that initialize this type accordingly
    testCaseTypeIDsMap      : Map<ReferenceToType, TestCaseValue>    //used by ACN to produce valid test cases
}

type InitProcedure0 = {
    funcName:string;
    def:string;
    body:string
}

type InitFunction = {
    initExpression          : string               // an expression that provides the default initialization.
    initExpressionGlobal    : string               // an expression that provides the default initialization.
                                                          //It is usually present except of some rare cases such as an empty sequence (for C only) etc
    initProcedure           : InitProcedure0 option
    initFunction            : InitProcedure0 option                      // an expression that initializes the given type to a default value.
    initGlobal              : {|globalName:string; def:string; body:string |} option                      // an expression that initializes the given type to a default value.

    initTas                 : (CallerScope  -> InitFunctionResult)              // returns the statement(s) that defaults initialize this type (used in the init function)
    initByAsn1Value         : CallerScope  -> Asn1ValueKind -> string           // returns the statement(s) that initialize according to the asn1value
    //initFuncBodyTestCases   : (CallerScope  -> InitFunctionResult) list         // returns a list of set the statement(s). Each set that initialize this type according to a specific test case
    automaticTestCases      : AutomaticTestCase list
    user_aux_functions      : (string*string) list
    nonEmbeddedChildrenFuncs  : InitFunction list      //a list with the first level child funcs which are not embedded by this
                                                       //InitFunction but the the function is called

}


type IsEqualBody =
    | EqualBodyExpression       of (CallerScope -> CallerScope -> (string*(LocalVariable list)) option)
    | EqualBodyStatementList    of (CallerScope -> CallerScope -> (string*(LocalVariable list)) option)

type EqualFunction = {
    isEqualFuncName     : string option               // the name of the equal function.
    isEqualFunc         : string option               // the body of the equal function
    isEqualFuncDef      : string option
    isEqualBody         : IsEqualBody                 // a function that  returns an expression or a statement list
    //isEqualBody2        : IsEqualBody2
}


type AnonymousVariable = {
    valueName           : string
    valueExpression      : string
    typeDefinitionName  : string
    valKind             : Asn1ValueKind        // the value
}

type IsValidFunction = {
    errCodes            : ErrorCode list
    funcName            : string option               // the name of the function. Valid only for TASes)
    func                : string option               // the body of the function
    funcDef             : string option               // function definition in header file
    funcBody            : CallerScope -> ValidationStatement            //returns a list of validations statements

    alphaFuncs          : AlphaFunc list
    localVariables      : LocalVariable list
    anonymousVariables  : AnonymousVariable  list      //list with the anonymous asn1 values used in constraints and which must be declared.
                                                       //these are the bit and octet string values which cannot be expressed as single primitives in C/Ada
    nonEmbeddedChildrenValidFuncs  : IsValidFunction list         //a list with the first level child funcs which are not embedded by this
                                                       //IsValidFunction but the the function is called
}


/////////////////////////////////////////////////////////////////////


type IntegerSignedness =
    | Positive
    | TwosComplement

type IntegerEndiannessSize =
    | S16
    | S32
    | S64
with
    member this.bitSize =
        match this with
        | S16 -> 16
        | S32 -> 32
        | S64 -> 64

type IntegerEndianness =
    | Byte
    | Unbounded
    | LittleEndian of IntegerEndiannessSize
    | BigEndian of IntegerEndiannessSize

type AcnIntegerEncodingType = {
    signedness: IntegerSignedness
    endianness: IntegerEndianness
}

type AcnRealEncodingType =
    | BigEndian32
    | BigEndian64
    | LittleEndian32
    | LittleEndian64

type Asn1IntegerEncodingType =
    | FullyConstrainedPositive of bigint * bigint
    | FullyConstrained of bigint * bigint
    | SemiConstrainedPositive of bigint
    | SemiConstrained of bigint
    | UnconstrainedMax of bigint
    | Unconstrained

type TypeEncodingKind = // TODO: Alignment???
    | Asn1IntegerEncodingType of Asn1IntegerEncodingType option // None if range min = max
    | Asn1RealEncodingType of Asn1AcnAst.RealClass
    | AcnIntegerEncodingType of AcnIntegerEncodingType
    | AcnRealEncodingType of AcnRealEncodingType
    | AcnBooleanEncodingType of AcnBooleanEncoding option
    | AcnNullEncodingType of PATTERN_PROP_VALUE option
    | AcnStringEncodingType of Asn1AcnAst.StringAcnEncodingClass
    | AcnOctetStringEncodingType of Asn1AcnAst.SizeableAcnEncodingClass
    | AcnBitStringEncodingType of Asn1AcnAst.SizeableAcnEncodingClass
    | SequenceOfEncodingType of TypeEncodingKind * Asn1AcnAst.SizeableAcnEncodingClass
    | SequenceEncodingType of TypeEncodingKind option list
    | ChoiceEncodingType of TypeEncodingKind option list
    | ReferenceEncodingType of string
    | OptionEncodingType of TypeEncodingKind
    | Placeholder

/////////////////////////////////////////////////////////////////////


type NestingScope = {
    acnOuterMaxSize: bigint
    uperOuterMaxSize: bigint
    nestingLevel: bigint
    nestingIx: bigint
    acnOffset: bigint
    uperOffset: bigint
    acnRelativeOffset: bigint
    uperRelativeOffset: bigint
    acnSiblingMaxSize: bigint option
    uperSiblingMaxSize: bigint option
} with
    static member init (acnOuterMaxSize: bigint) (uperOuterMaxSize: bigint): NestingScope =
        {acnOuterMaxSize = acnOuterMaxSize; uperOuterMaxSize = uperOuterMaxSize; nestingLevel = 0I; nestingIx = 0I; acnRelativeOffset = 0I; uperRelativeOffset = 0I; acnOffset = 0I; uperOffset = 0I; acnSiblingMaxSize = None; uperSiblingMaxSize = None}


type UPERFuncBodyResult = {
    funcBody            : string
    errCodes            : ErrorCode list
    localVariables      : LocalVariable list
    bValIsUnReferenced  : bool
    bBsIsUnReferenced   : bool
    resultExpr          : string option
    typeEncodingKind    : TypeEncodingKind option
}
type UPerFunction = {
    funcName            : string option               // the name of the function
    func                : string option               // the body of the function
    funcDef             : string option               // function definition in header file
    funcBody            : NestingScope -> CallerScope -> (UPERFuncBodyResult option)            // returns a list of validations statements
    funcBody_e          : ErrorCode -> NestingScope -> CallerScope -> (UPERFuncBodyResult option)
}

type AcnFuncBodyResult = {
    funcBody            : string
    errCodes            : ErrorCode list
    localVariables      : LocalVariable list
    bValIsUnReferenced  : bool
    bBsIsUnReferenced   : bool
    resultExpr          : string option
    typeEncodingKind    : TypeEncodingKind option
}

type XERFuncBodyResult = {
    funcBody            : string
    errCodes            : ErrorCode list
    localVariables      : LocalVariable list
    encodingSizeInBytes : BigInteger
}

type XerTag =
    | XerLiteralConstant   of string        //tagValue
    | XerFunctionParameter of string*string //tagValue, prmName holding the tag value

type XerFunctionRec = {
    funcName            : string option               // the name of the function
    func                : string option               // the body of the function
    funcDef             : string option               // function definition in header file
    encodingSizeInBytes : BigInteger
    funcBody            : CallerScope -> (XerTag option) -> (XERFuncBodyResult option)
    funcBody_e          : ErrorCode -> CallerScope -> (XerTag option) -> (XERFuncBodyResult option)            //p, XmlTag,   returns a list of encoding/decoding statements
}

type XerFunction =
    | XerFunction of XerFunctionRec
    | XerFunctionDummy

type IcdAux = {
    canBeEmbedded  : bool
    createRowsFunc : string->string->string list ->IcdRow list
    typeAss        : IcdTypeAss
}

type AcnFunction = {
    funcName            : string option               // the name of the function. Valid only for TASes)
    func                : string option               // the body of the function
    funcDef             : string option               // function definition

    // takes as input (a) any acn arguments and (b) the field where the encoding/decoding takes place
    // returns a list of acn encoding statements
    funcBody            : State->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CallerScope -> ((AcnFuncBodyResult option)*State)
    funcBodyAsSeqComp   : State->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CallerScope -> string -> ((AcnFuncBodyResult option)*State)
    isTestVaseValid     : AutomaticTestCase -> bool
    icd                 : IcdAux option (* always present in Encode, always None in Decode *)
}

type EncodeDecodeTestFunc = {
    funcName            : string               // the name of the function
    func                : string               // the body of the function
    funcDef             : string               // function definition in header file
}

type TestCaseFunction = {
    funcName            : string               // the name of the function
    func                : string               // the body of the function
    funcDef             : string               // function definition in header file
}

type Integer = {
    //bast inherited properties
    baseInfo             : Asn1AcnAst.Integer

    //DAst properties
    //baseTypeEquivalence: BaseTypesEquivalence<Integer>
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints

    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        : IntegerValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction

    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    ////automaticTestCasesValues     : Asn1Value list

}

type Enumerated = {
    baseInfo             : Asn1AcnAst.Enumerated

    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //DAst properties
    //baseTypeEquivalence: BaseTypesEquivalence<Enumerated>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        : EnumValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    ////automaticTestCasesValues     : Asn1Value list
}

type ObjectIdentifier = {
    baseInfo             : Asn1AcnAst.ObjectIdentifier

    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        : ObjectIdentifierValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    //automaticTestCasesValues     : Asn1Value list
}

type TimeType = {
    baseInfo             : Asn1AcnAst.TimeType

    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        : TimeValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    //automaticTestCasesValues     : Asn1Value list
}

type Real = {
    baseInfo             : Asn1AcnAst.Real

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints


    //baseTypeEquivalence: BaseTypesEquivalence<Real>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        : RealValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    //automaticTestCasesValues     : Asn1Value list
}


type Boolean = {
    baseInfo             : Asn1AcnAst.Boolean

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<Boolean>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        : BooleanValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    //automaticTestCasesValues     : Asn1Value list
}


type NullType = {
    baseInfo             : Asn1AcnAst.NullType

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<NullType>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initFunction        : InitFunction
    //initialValue        : NullValue
    equalFunction       : EqualFunction
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option
}


type StringType = {
    baseInfo             : Asn1AcnAst.StringType

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<StringType>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        :  StringValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    //automaticTestCasesValues     : Asn1Value list
}


type OctetString = {
    baseInfo             : Asn1AcnAst.OctetString


    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<OctetString>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        : OctetStringValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    //automaticTestCasesValues     : Asn1Value list
}



type BitString = {
    baseInfo             : Asn1AcnAst.BitString

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<BitString>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    //initialValue        : BitStringValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    //automaticTestCasesValues     : Asn1Value list
}


type SequenceOf = {
    baseInfo            : Asn1AcnAst.SequenceOf
    childType           : Asn1Type

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option
}


and AcnChild = {
    Name                        : StringLoc
    c_name                      : string
    id                          : ReferenceToType
    Type                        : Asn1AcnAst.AcnInsertedType
    typeDefinitionBodyWithinSeq : string
    funcBody                    : CommonTypes.Codec -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> NestingScope -> CallerScope -> (AcnFuncBodyResult option)            // returns a list of validations statements
    funcUpdateStatement         : AcnChildUpdateResult option                                    // vTarget,  pSrcRoot, return the update statement
    Comments                    : string array
    deps                        : Asn1AcnAst.AcnInsertedFieldDependencies
    initExpression              : string
}

and SeqChildInfo =
    | Asn1Child of Asn1Child
    | AcnChild  of AcnChild


and Asn1Child = {
    Name                        : StringLoc
    _c_name                     : string
    _scala_name                 : string
    _ada_name                   : string
    isEqualBodyStats            : CallerScope -> CallerScope -> (string*(LocalVariable list)) option
    Type                        : Asn1Type
    Optionality                 : Asn1AcnAst.Asn1Optionality option
    Comments                    : string array
}



and Sequence = {
    baseInfo            : Asn1AcnAst.Sequence
    children            : SeqChildInfo list


    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstrained integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    //
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option
}




and ChChildInfo = {
    Name                        : StringLoc
    _c_name                     : string
    _scala_name                 : string
    _ada_name                   : string
    _present_when_name_private  : string // Does not contain the "_PRESENT". Not to be used directly by backends. Backends should use presentWhenName
    acnPresentWhenConditions    : AcnGenericTypes.AcnPresentWhenConditionChoiceChild list
    Comments                    : string array

    chType                      : Asn1Type
    Optionality                 : Asn1AcnAst.Asn1ChoiceOptionality option

    //DAst properties
    isEqualBodyStats    : CallerScope -> CallerScope  -> string*(LocalVariable list)
}

and AcnChoiceEncClass =
    | CEC_uper
    | CEC_enum          of (Asn1AcnAst.ReferenceToEnumerated * Asn1AcnAst.Determinant)
    | CEC_presWhen
    with
        override this.ToString () =
            match this with
            | CEC_uper           -> "CEC_uper"
            | CEC_enum  (a,b)    -> sprintf "CEC_enum(%s)" b.id.AsString
            | CEC_presWhen       -> "CEC_presWhen"



and Choice = {
    baseInfo            : Asn1AcnAst.Choice
    children            : ChChildInfo list
    ancEncClass         : AcnChoiceEncClass
    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option
}


and ReferenceType = {
    baseInfo            : Asn1AcnAst.ReferenceType
    resolvedType        : Asn1Type

    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefinitionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option
}

and AcnChildUpdateResult = {
    updateAcnChildFnc        : AcnChild -> NestingScope -> CallerScope -> CallerScope -> string
    //Given an automatic test case (which includes a map with the IDs of the involved types), this function
    //checks if the automatic test case contains a type which depends on this acn Child. If this is true
    // it returns the value of the dependency, otherwise none
    //if the acn child depends on multiple ASN.1 types then this function (multi ACN update) checks that all ASN.1 types
    //have value and the value is the same. In this case the value is returned. Otherwise none
    testCaseFnc : AutomaticTestCase -> TestCaseValue option
    errCodes    : ErrorCode list
    localVariables      : LocalVariable list
}

and DastAcnParameter = {
    name        : string
    c_name      : string
    asn1Type    : AcnGenericTypes.AcnParamType
    loc         : SrcLoc
    id          : ReferenceToType
    typeDefinitionBodyWithinSeq : string
}



and Asn1Type = {
    id              : ReferenceToType
    acnAlignment     : AcnGenericTypes.AcnAlignment option
    acnParameters   : DastAcnParameter list
    Location        : SrcLoc //Line no, Char pos
    moduleName      : string
    //when inheritInfo has a value it indicates that this type is
    //a specialization of a reference type.
    inheritInfo   : InheritanceInfo option

    //it simply indicates that this type is under a type assignment
    typeAssignmentInfo  : AssignmentInfo option

    Kind            : Asn1TypeKind
    unitsOfMeasure  : string option
}

and Asn1TypeKind =
    | Integer           of Integer
    | Real              of Real
    | IA5String         of StringType
    | OctetString       of OctetString
    | NullType          of NullType
    | BitString         of BitString
    | Boolean           of Boolean
    | Enumerated        of Enumerated
    | ObjectIdentifier  of ObjectIdentifier
    | SequenceOf        of SequenceOf
    | Sequence          of Sequence
    | Choice            of Choice
    | ReferenceType     of ReferenceType
    | TimeType          of TimeType
with
    member this.baseKind: Asn1AcnAst.Asn1TypeKind =
        match this with
        | Integer k -> Asn1AcnAst.Integer k.baseInfo
        | Real k -> Asn1AcnAst.Real k.baseInfo
        | IA5String k -> Asn1AcnAst.IA5String k.baseInfo
        | OctetString k -> Asn1AcnAst.OctetString k.baseInfo
        | NullType k -> Asn1AcnAst.NullType k.baseInfo
        | BitString k -> Asn1AcnAst.BitString k.baseInfo
        | Boolean k -> Asn1AcnAst.Boolean k.baseInfo
        | Enumerated k -> Asn1AcnAst.Enumerated k.baseInfo
        | ObjectIdentifier k -> Asn1AcnAst.ObjectIdentifier k.baseInfo
        | SequenceOf k -> Asn1AcnAst.SequenceOf k.baseInfo
        | Sequence k -> Asn1AcnAst.Sequence k.baseInfo
        | Choice k -> Asn1AcnAst.Choice k.baseInfo
        | ReferenceType k -> Asn1AcnAst.ReferenceType k.baseInfo
        | TimeType k -> Asn1AcnAst.TimeType k.baseInfo


let getNextValidErrorCode (cur:State) (errCodeName:string) (comment:string option) =
    let rec getErrorCode (errCodeName:string) =
        match cur.curErrCodeNames.Contains errCodeName with
        | false -> {ErrorCode.errCodeName = errCodeName; errCodeValue = cur.currErrorCode; comment=comment}
        | true  ->
            getErrorCode (errCodeName + "_2")

    let errCode = getErrorCode (errCodeName.ToUpper())
    errCode, {cur with currErrorCode = cur.currErrorCode + 1; curErrCodeNames = cur.curErrCodeNames.Add errCode.errCodeName}

type TypeAssignment = {
    Name:StringLoc
    c_name:string
    scala_name:string
    ada_name:string
    Type:Asn1Type
    Comments: string array
}

type ValueAssignment = {
    Name    :StringLoc
    c_name  :string
    scala_name:string
    ada_name:string
    Type    :Asn1Type
    Value   :Asn1Value
}




type Asn1Module = {
    Name : StringLoc
    TypeAssignments : list<TypeAssignment>
    ValueAssignments : list<ValueAssignment>
    Imports : list<Asn1Ast.ImportedModule>
    Exports : Asn1Ast.Exports
    Comments : string array
}

type Asn1File = {
    FileName:string;
    Tokens: IToken array
    Modules : list<Asn1Module>
}

type ProgramUnit = {
    name                    : string
    testcase_name           : string
    specFileName            : string
    bodyFileName            : string
    testcase_specFileName   : string
    testcase_bodyFileName   : string
    sortedTypeAssignments   : TypeAssignment list
    valueAssignments        : ValueAssignment list
    importedProgramUnits    : string list
    importedTypes           : string list
    importedUserModules     : string list
}


type AstRoot = {
    Files        : Asn1File list
    deps         :Asn1AcnAst.AcnInsertedFieldDependencies
    acnConstants : Map<string, BigInteger>
    args         : CommandLineSettings
    programUnits : ProgramUnit list
    lang         : ProgrammingLanguage
    acnParseResults:CommonTypes.AntlrParserResult list //used in ICDs to regenerate with collors the initial ACN input
    icdHashes   : Map<String, IcdTypeAss list>
}


type TC_Param_Direction = TC_IN | TC_OUT | TC_INOUT
type TC_Param = {
    name : string
    direction : TC_Param_Direction
    typeName : string
}

type TC_ExpressionType =
    | TC_INTEGER
    | TC_REAL
    | TC_STRING
    | TC_BOOL
    | TC_COMPLEX

type TC_Statement =
    | ReturnStatement       of      TC_Expression
    | CompositeStatement    of      TC_Statement list
    | AssignmentStatement   of      TC_Expression*TC_Expression
    | ForStatement          of      {|initExp:TC_Expression; termination : TC_Expression; incrementExpression : TC_Expression; innerStatement:TC_Statement|}
    | WhileStatement        of      {|whileTrueExp : TC_Expression; innerStatement:TC_Statement|}
    | IfStatement           of      {| ifelsif : {|whileTrueExp : TC_Expression; innerStatement:TC_Statement|} list; elseStatement: TC_Statement option |}


and TC_Expression =
    | TC_EqExpression              of TC_Expression*TC_Expression
    | TC_GtExpression              of TC_Expression*TC_Expression
    | TC_GteExpression             of TC_Expression*TC_Expression
    | TC_ReferenceToVariable       of TC_ExpressionType*string
    | TC_Literal                   of TC_ExpressionType*string
    | TC_AccessToField             of TC_Expression * string
    | TC_AccessToArrayElement      of TC_Expression * TC_Expression


type TC_Function = {
    name            : string
    returnTypeName  : string
    parameters      : TC_Param list
    body            : TC_Statement list
}
