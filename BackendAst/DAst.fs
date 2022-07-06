module DAst

open Antlr.Runtime.Tree
open Antlr.Runtime
open System
open System.Numerics
open FsUtils
open CommonTypes
open AbstractMacros
open System.Collections.Generic

//open Constraints





type CallerScope = {
    modName : string
    arg     : FuncParamType
}

type AlphaFunc   = {
    funcName            : string
    funcBody            : CallerScope -> string
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
}


let emptyState = {currErrorCode=0; curErrCodeNames=Set.empty; (*allocatedTypeDefNames = []; allocatedTypeDefNameInTas = Map.empty;*) alphaIndex=0; alphaFuncs=[]; typeIdsSet=Map.empty; newTypesMap = new Dictionary<ReferenceToType, System.Object>()}




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ASN1 VALUES DEFINITION    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type IntegerValue         = BigInteger
type RealValue            = double
type StringValue          = string
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
and ObjectIdenfierValue   = 
    | Asn1DefinedObjectIdentifierValue of ((ResolvedObjectIdentifierValueCompoent list)*(ObjectIdentifierValueCompoent list))
    | InternalObjectIdentifierValue of BigInteger list

and NamedValue = {
    name        : string
    Value       : Asn1Value
}

and Asn1Value = {
    kind : Asn1ValueKind
    loc  : SrcLoc
    id   : ReferenceToValue
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
    | ObjOrRelObjIdValue    of ObjectIdenfierValue

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
    | SequenceOfIndex       of int*int option        //i index, initialValue
    | IntegerLocalVariable  of string*int option     //variable name, initialValue
    | Asn1SIntLocalVariable of string*int option     //variable name, initialValue
    | Asn1UIntLocalVariable of string*int option     //variable name, initialValue
    | FlagLocalVariable     of string*int option     //variable name, initialValue
    | BooleanLocalVariable  of string*bool option    //variable name, initialValue
    | AcnInsertedChild      of string*string         //variable name, type initialValue
    | GenericLocalVariable  of GenericLocalVariable



(*an expression or statement that checks if a constraint is met or not. IT DOES NOT ASSIGNG the error code field*)
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


         //Emit_local_variable_SQF_Index(nI, bHasInitalValue)::="I<nI>:Integer<if(bHasInitalValue)>:=1<endif>;"


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
}


type TypeDefintionOrReference =
    /// indicates that no extra type definition is required (e.g. INTEGER without constraints or type reference type without new constraints)
    | ReferenceToExistingDefinition    of ReferenceToExistingDefinition                
    /// indicates that a new type is defined
    | TypeDefinition                of TypeDefinition       



type ErroCode = {
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
Generates initialization statement(s) that inititalize the type with the given Asn1GeneticValue.
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
a string with the statements than initialize all involved types plus the local variavles needed.
The id of the types that are involved in this automatic test case are stored within a map with name testCaseTypeIDsMap. The need for this map
is in order to generate valid ACN test cases. I.e. the ACN checks that test case provides values for all ACN inserted fields. Otherwise is invalid and not 
generated.
*)
type AutomaticTestCase = {
    initTestCaseFunc : CallerScope  -> InitFunctionResult //returns a list of set the statement(s) that initialize this type accordingly
    testCaseTypeIDsMap         : Map<ReferenceToType, TestCaseValue>    //used by ACN to produce valid test cases
}

type InitFunction = {
    initFuncName            : string option               // the name of the function
    initFunc                : string option               // the body of the function
    initFuncDef             : string option               // function definition in header file
    initTas                 : (CallerScope  -> InitFunctionResult)              // returns the statement(s) that defaults initialize this type (used in the init function)
    initByAsn1Value         : CallerScope  -> Asn1ValueKind -> string           // returns the statement(s) that initialize according to the asn1value
    //initFuncBodyTestCases   : (CallerScope  -> InitFunctionResult) list         // returns a list of set the statement(s). Each set that initialize this type according to a specific test case
    automaticTestCases      : AutomaticTestCase list
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
    valueExpresion      : string
    typeDefinitionName  : string
    valKind             : Asn1ValueKind        // the value
}

type IsValidFunction = {
    errCodes            : ErroCode list
    funcName            : string option               // the name of the function. Valid only for TASes)
    func                : string option               // the body of the function
    funcDef             : string option               // function definition in header file
    //funcExp             : (CallerScope -> ValidationCodeBlock)    // return a single boolean expression
    funcBody            : CallerScope -> ValidationStatement            //returns a list of validations statements
    //funcBody2           : string -> string -> string  //like funBody but with two arguement p and accessOper ( i.e. '->' or '.')
    
    alphaFuncs          : AlphaFunc list  
    localVariables      : LocalVariable list
    anonymousVariables  : AnonymousVariable  list      //list with the anonymous asn1 values used in constraints and which must be declared.
                                                       //these are the bit and octet string values which cannot be expressed as single primitives in C/Ada
    nonEmbeddedChildrenValidFuncs  : IsValidFunction list         //a list with the first level child funcs which are not embedded by this
                                                       //IsValidFunction but the the function is called
}

type UPERFuncBodyResult = {
    funcBody            : string
    errCodes            : ErroCode list
    localVariables      : LocalVariable list
    bValIsUnReferenced    : bool
    bBsIsUnReferenced     : bool
}
type UPerFunction = {
    funcName            : string option               // the name of the function
    func                : string option               // the body of the function
    funcDef             : string option               // function definition in header file
    funcBody            : CallerScope -> (UPERFuncBodyResult option)            // returns a list of validations statements
    funcBody_e          : ErroCode -> CallerScope -> (UPERFuncBodyResult option)
}

type AcnFuncBodyResult = {
    funcBody            : string
    errCodes            : ErroCode list
    localVariables      : LocalVariable list
    bValIsUnReferenced    : bool
    bBsIsUnReferenced     : bool

}

type XERFuncBodyResult = {
    funcBody            : string
    errCodes            : ErroCode list
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
    funcBody_e          : ErroCode -> CallerScope -> (XerTag option) -> (XERFuncBodyResult option)            //p, XmlTag,   returns a list of encoding/decoding statements
}

type XerFunction =
    | XerFunction of XerFunctionRec
    | XerFunctionDummy


type AcnFunction = {
    funcName            : string option               // the name of the function. Valid only for TASes)
    func                : string option               // the body of the function
    funcDef             : string option               // function definition

    // takes as input (a) any acn arguments and (b) the field where the encoding/decoding takes place
    // returns a list of acn encoding statements
    funcBody            : State->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> ((AcnFuncBodyResult option)*State)            
    funcBodyAsSeqComp   : State->((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> string -> ((AcnFuncBodyResult option)*State)            
    isTestVaseValid     : AutomaticTestCase -> bool
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
    //bast inherrited properties
    baseInfo             : Asn1AcnAst.Integer

    //DAst properties
    //baseTypeEquivalence: BaseTypesEquivalence<Integer>
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints

    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : IntegerValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction

    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list

}

type Enumerated = {
    baseInfo             : Asn1AcnAst.Enumerated

    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //DAst properties
    //baseTypeEquivalence: BaseTypesEquivalence<Enumerated>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : EnumValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list
}

type ObjectIdentifier = {
    baseInfo             : Asn1AcnAst.ObjectIdentifier

    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : ObjectIdenfierValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list
}

type TimeType = {
    baseInfo             : Asn1AcnAst.TimeType

    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : TimeValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list
}

type Real = {
    baseInfo             : Asn1AcnAst.Real

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints


    //baseTypeEquivalence: BaseTypesEquivalence<Real>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : RealValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list
}


type Boolean = {
    baseInfo             : Asn1AcnAst.Boolean

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<Boolean>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : BooleanValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list
}


type NullType = {
    baseInfo             : Asn1AcnAst.NullType

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<NullType>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initFunction        : InitFunction
    initialValue        : NullValue
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
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        :  StringValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list
}


type OctetString = {
    baseInfo             : Asn1AcnAst.OctetString


    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<OctetString>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : OctetStringValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list
}



type BitString = {
    baseInfo             : Asn1AcnAst.BitString

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<BitString>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : BitStringValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
    uperEncFunction     : UPerFunction
    uperDecFunction     : UPerFunction
    acnEncFunction      : AcnFunction
    acnDecFunction      : AcnFunction
    xerEncFunction      : XerFunction
    xerDecFunction      : XerFunction
    uperEncDecTestFunc  : EncodeDecodeTestFunc option
    acnEncDecTestFunc   : EncodeDecodeTestFunc option
    xerEncDecTestFunc   : EncodeDecodeTestFunc option

    automaticTestCasesValues     : Asn1Value list
}


type SequenceOf = {
    baseInfo            : Asn1AcnAst.SequenceOf
    childType           : Asn1Type

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<SequenceOf>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : SeqOfValue
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

    automaticTestCasesValues     : Asn1Value list
}



(*
and SeqChildInfo = {
    baseInfo            : Asn1AcnAst.SeqChildInfo
    chType              : Asn1Type
    

    //DAst properties
    c_name              : string
    isEqualBodyStats    : string -> string  -> string -> (string*(LocalVariable list)) option  // 
    isValidBodyStats    : int -> (SeqChoiceChildInfoIsValid option * int)
}

*)


and AcnChild = {
    Name                        : StringLoc
    c_name                      : string
    id                          : ReferenceToType
    Type                        : Asn1AcnAst.AcnInsertedType
    typeDefinitionBodyWithinSeq : string
    funcBody                    : CommonTypes.Codec -> ((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) -> CallerScope -> (AcnFuncBodyResult option)            // returns a list of validations statements
    funcUpdateStatement         : AcnChildUpdateResult option                                    // vTarget,  pSrcRoot, return the update statement 
    Comments                    : string array
}

and SeqChildInfo = 
    | Asn1Child of Asn1Child
    | AcnChild  of AcnChild


and Asn1Child = {
    Name                        : StringLoc
    _c_name                     : string
    _ada_name                   : string                     
    isEqualBodyStats            : CallerScope -> CallerScope -> (string*(LocalVariable list)) option  // 
    //isValidBodyStats            : State -> (SeqChoiceChildInfoIsValid option * State)
    Type                        : Asn1Type
    Optionality                 : Asn1AcnAst.Asn1Optionality option
    Comments                    : string array
}



and Sequence = {
    baseInfo            : Asn1AcnAst.Sequence
    children            : SeqChildInfo list


    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<Sequence>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : SeqValue
    initFunction        : InitFunction
    equalFunction       : EqualFunction
    isValidFunction     : IsValidFunction option      // it is optional because some types do not require an IsValid function (e.g. an unconstraint integer)
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

    automaticTestCasesValues     : Asn1Value list
}




and ChChildInfo = {
    Name                        : StringLoc
    _c_name                      : string
    _ada_name                    : string                     
    _present_when_name_private  : string // Does not contain the "_PRESENT". Not to be used directly by backends. Backends should use presentWhenName
    acnPresentWhenConditions    : AcnGenericTypes.AcnPresentWhenConditionChoiceChild list
    Comments                    : string array

    chType              :Asn1Type
    Optionality                 : Asn1AcnAst.Asn1ChoiceOptionality option
    
    //DAst properties
    isEqualBodyStats    : CallerScope -> CallerScope  -> string*(LocalVariable list) // 
    //isValidBodyStats    : State -> (SeqChoiceChildInfoIsValid option * State)
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
    //baseTypeEquivalence: BaseTypesEquivalence<Choice>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : ChValue
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

    automaticTestCasesValues     : Asn1Value list
}


and ReferenceType = {
    baseInfo            : Asn1AcnAst.ReferenceType
    resolvedType        : Asn1Type

    //typeDefinition      : TypeDefinitionCommon
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    definitionOrRef     : TypeDefintionOrReference
    printValue          : string -> (Asn1ValueKind option) -> (Asn1ValueKind) -> string
    initialValue        : Asn1Value
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

    automaticTestCasesValues     : Asn1Value list
}

and AcnChildUpdateResult = {
    updateAcnChildFnc        : (*typedef name*)string -> CallerScope -> CallerScope -> string
    //Given an automatic test case (which includes a map with the IDs of the involved types), this function
    //checks if the automatic test case contains a type which depends on this acn Child. If this is true
    // it returns the value of the depenendency, otherwise none
    //if the acn child depends on multiple ASN.1 types then this function (multi ACN update) checks that all ASN.1 types
    //have value and the value is the same. In this case the value is returned. Otherwise none
    testCaseFnc : AutomaticTestCase -> TestCaseValue option 
    errCodes    : ErroCode list
    localVariables      : LocalVariable list
}

and DastAcnParameter = {
    name        : string
    c_name      : string
    asn1Type    : AcnGenericTypes.AcnParamType
    loc         : SrcLoc
    id          : ReferenceToType
    typeDefinitionBodyWithinSeq : string
    //funcUpdateStatement00         : AcnChildUpdateResult option                                    // vTarget,  pSrcRoot, return the update statement 
}
    


and Asn1Type = {
    id              : ReferenceToType
    acnAligment     : AcnGenericTypes.AcnAligment option
    acnParameters   : DastAcnParameter list
    Location        : SrcLoc //Line no, Char pos

    //when inheritInfo has a value it indicates that this type is
    //a specialization of a reference type.
    inheritInfo   : InheritanceInfo option

    //it simply indicates that this type is under a type assignment
    typeAssignmentInfo  : AssignmentInfo option

    Kind            : Asn1TypeKind
    unitsOfMeasure : string option

    //parInfoData : Asn1Fold.ParentInfo<ParentInfoData> option
    //newTypeDefName  : string
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



let getNextValidErrorCode (cur:State) (errCodeName:string) (comment:string option) =
    
    //if (errCodeName.ToUpper() = "ERR_ACN_ENCODE_TC_DATA_HEADER_TCPACKETPUSVERSIONNUMBER") then
    //    let aaa = cur.curErrCodeNames |> Set.toArray
    //    printfn "???"
        
    let rec getErroCode (errCodeName:string) = 
        match cur.curErrCodeNames.Contains errCodeName with
        | false -> {ErroCode.errCodeName = errCodeName; errCodeValue = cur.currErrorCode; comment=comment}
        | true  -> 
            getErroCode (errCodeName + "_2")

    let errCode = getErroCode (errCodeName.ToUpper())
    errCode, {cur with currErrorCode = cur.currErrorCode + 1; curErrCodeNames = cur.curErrCodeNames.Add errCode.errCodeName}
(*
let getUniqueValidTypeDefName (cur:State) (l:ProgrammingLanguage) (tasInfo:TypeAssignmentInfo option) (programUnit:string) (proposedTypeDefName:string)  =
    let getNextCount (oldName:string) =
        match oldName.Split('_') |> Seq.toList |> List.rev with
        | []    
        | _::[]     -> oldName + "_1"
        | curN::oldPart   ->
            match Int32.TryParse curN with
            | true, num ->  (oldPart |> List.rev |> Seq.StrJoin "_") + "_" + ((num+1).ToString())
            | _         -> oldName + "_1"
    let rec getValidTypeDefname (proposedTypeDefName:string) = 
        match l with
        | C     ->  
            match cur.allocatedTypeDefNames |> Seq.exists(fun (_,z) -> z = proposedTypeDefName) with
            | false -> proposedTypeDefName
            | true  -> 
                match cur.allocatedTypeDefNames |> Seq.exists(fun (pu,td) -> pu = programUnit && td = proposedTypeDefName) with
                | false -> getValidTypeDefname (programUnit + "_" + proposedTypeDefName ) 
                | true  -> getValidTypeDefname (getNextCount proposedTypeDefName ) 
        | Ada   ->  
            match cur.allocatedTypeDefNames |> Seq.exists(fun (pu,td) -> pu.ToUpper() = programUnit.ToUpper() && td.ToUpper() = proposedTypeDefName.ToUpper()) with
            | false -> proposedTypeDefName
            | true  -> getValidTypeDefname (getNextCount proposedTypeDefName  ) 

    
    match tasInfo with
    | None  ->
        let validTypeDefname = getValidTypeDefname proposedTypeDefName 
        validTypeDefname, {cur with allocatedTypeDefNames = (programUnit, validTypeDefname)::cur.allocatedTypeDefNames}
    | Some tasInfo  ->
        match cur.allocatedTypeDefNameInTas.TryFind tasInfo with
        | Some (programUnit, validTypeDefname)  -> validTypeDefname, cur
        | None  ->
            let validTypeDefname = getValidTypeDefname proposedTypeDefName 
            validTypeDefname, {cur with allocatedTypeDefNames = (programUnit, validTypeDefname)::cur.allocatedTypeDefNames; allocatedTypeDefNameInTas = cur.allocatedTypeDefNameInTas.Add(tasInfo, (programUnit,validTypeDefname))}
            *)

type TypeAssignment = {
    Name:StringLoc
    c_name:string
    ada_name:string
    Type:Asn1Type
    Comments: string array
}

type ValueAssignment = {
    Name    :StringLoc
    c_name  :string
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
    tetscase_name           : string
    specFileName            : string
    bodyFileName            : string
    tetscase_specFileName   : string
    tetscase_bodyFileName   : string
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
}


type TC_Param_Direction = TC_IN | TC_OUT | TC_INOUT
type TC_Param = {
    name : string
    direction : TC_Param_Direction
    typeName : string
}

type TC_EpressionType =
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
    | TC_ReferenceToVariable       of TC_EpressionType*string
    | TC_Literal                   of TC_EpressionType*string
    | TC_AccessToField             of TC_Expression * string
    | TC_AccessToArrayElement      of TC_Expression * TC_Expression


type TC_Function = {
    name            : string
    returnTypeName  : string
    parameters      : TC_Param list
    body            : TC_Statement list
}

