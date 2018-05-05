module DAst

open Antlr.Runtime.Tree
open Antlr.Runtime
open System
open System.Numerics
open FsUtils
open CommonTypes
//open Constraints


type State = {
    curSeqOfLevel : int
    currErrorCode   : int
    curErrCodeNames : Set<String>
}

let emptyState = {curSeqOfLevel=0; currErrorCode=0; curErrCodeNames=Set.empty}

type ParentInfoData = {
    program_unit_name : string
    typedefName       : string
    isRefType         : bool
}



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
    | BooleanValue          of BooleanValue    
    | BitStringValue        of BitStringValue  
    | OctetStringValue      of OctetStringValue
    | EnumValue             of EnumValue       
    | SeqOfValue            of SeqOfValue      
    | SeqValue              of SeqValue        
    | ChValue               of ChValue         
    | NullValue             of NullValue
    | RefValue              of RefValue   

//type Asn1GenericValue = Asn1Value

type ProgrammingLanguage =
    |C
    |Ada

    
type FuncParamType =
  | VALUE       of string
  | POINTER     of string
  | FIXARRAY    of string

type CallerScope = {
    modName : string
    arg     : FuncParamType
}


type ExpOrStatement =
    | Expression 
    | Statement  

type LocalVariable =
    | SequenceOfIndex       of int*int option        //i index, initialValue
    | IntegerLocalVariable  of string*int option     //variable name, initialValue
    | Asn1SIntLocalVariable of string*int option     //variable name, initialValue
    | Asn1UIntLocalVariable of string*int option     //variable name, initialValue
    | FlagLocalVariable     of string*int option     //variable name, initialValue
    | BooleanLocalVariable  of string*bool option    //variable name, initialValue
    | AcnInsertedChild      of string*string         //variable name, type initialValue



         //Emit_local_variable_SQF_Index(nI, bHasInitalValue)::="I<nI>:Integer<if(bHasInitalValue)>:=1<endif>;"

type TypeOrSubsType =
    | TYPE
    | SUBTYPE


type TypeDefinitionCommon = {
    // the name of the type C or Ada type. Defives from ASN.1 Type Assignment name.
    // Eg. for MyInt4 ::= INTEGER(0..15|20|25)
    // name will be MyInt4 
    // In inner types (i.e. types within compound types such as sequence, sequence of, choice) the name
    // is the name of the parent plus the component name.
    // For example, MySeq { a INTEGER} the name in the 'a' component will be MySeq_a
    name                : string            

    // used only in Ada backend. 
    typeOrSubsType      : TypeOrSubsType    

    // Used only for Strings and is the size of the string (plus one for the null terminated character)
    // It is usefull only in C due to the fact that the size of the array is not part of the type definition body but follows the the type name
    // i.e. for the ASN.1 type   STRCAPS   ::= IA5String (SIZE(1..100)) (FROM ("A".."Z"))	
    // the C definition is : typedef char STRCAPS[101];
    // If the definition was typedef char[101] STRCAPS; (which is the case for all other languages Ada, C#, Java etc) then we wouldn't need it
    arraySize           : int option      
    
    // the complete definition of the type
    // e.g. C : typedef asn1SccSint MyInt4;
    // and Ada: SUBTYPE MyInt4 IS adaasn1rtl.Asn1Int range 0..25;    
    completeDefinition  : string  


    // Ada does not allow nested type definitions.
    // Therefore The following type MySeq { a INTEGER, innerSeq SEQUENCE {b REAL}}
    // must be declared as if it was defined MySeq_innerSeq SEQUENCE {b REAL} MySeq { a INTEGER, innerSeq MySeq_innerSeq}
    // in this case it will be : MySeq_innerSeq 
    typeDefinitionBodyWithinSeq : string

    //the complete deffinition of inner complex types that must be declared
    //before this type
    completeDefinitionWithinSeq : string option
}

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

type AutomaticTestCase = {
    initTestCaseFunc : CallerScope  -> InitFunctionResult //returns a list of set the statement(s) that initialize this type accordingly
    testCase         : Map<ReferenceToType, TestCaseValue>
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

type AlphaFunc   = {
    funcName            : string
    funcBody            : string
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
    funcExp             : (CallerScope -> string) option   // return a single boolean expression
    funcBody            : CallerScope -> string            //returns a list of validations statements
    //funcBody2           : string -> string -> string  //like funBody but with two arguement p and accessOper ( i.e. '->' or '.')
    
    alphaFuncs          : AlphaFunc list  
    localVariables      : LocalVariable list
    anonymousVariables  : AnonymousVariable  list      //list with the anonymous asn1 values used in constraints and which must be declared.
                                                       //these are the bit and octet string values which cannot be expressed as single primitives in C/Ada
}

type UPERFuncBodyResult = {
    funcBody            : string
    errCodes            : ErroCode list
    localVariables      : LocalVariable list
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

type XerFunction = {
    funcName            : string option               // the name of the function
    func            : string option               // the body of the function
    funcDef             : string option               // function definition in header file
    encodingSizeInBytes : BigInteger
    funcBody            : CallerScope -> (XerTag option) -> (XERFuncBodyResult option)
    funcBody_e          : ErroCode -> CallerScope -> (XerTag option) -> (XERFuncBodyResult option)            //p, XmlTag,   returns a list of encoding/decoding statements
}


type AcnFunction = {
    funcName            : string option               // the name of the function. Valid only for TASes)
    func                : string option               // the body of the function
    funcDef             : string option               // function definition

    // takes as input (a) any acn arguments and (b) the field where the encoding/decoding takes place
    // returns a list of acn encoding statements
    funcBody            : State->((Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) -> CallerScope -> ((AcnFuncBodyResult option)*State)            
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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

type Real = {
    baseInfo             : Asn1AcnAst.Real

    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints


    //baseTypeEquivalence: BaseTypesEquivalence<Real>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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


and SeqChoiceChildInfoIsValid = {
    isValidStatement  : CallerScope -> string
    localVars         : LocalVariable list
    alphaFuncs        : AlphaFunc list
    errCode           : ErroCode list
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
    funcBody                    : CommonTypes.Codec -> ((Asn1AcnAst.RelativePath*Asn1AcnAst.AcnParameter) list) -> CallerScope -> (AcnFuncBodyResult option)            // returns a list of validations statements
    funcUpdateStatement         : AcnChildUpdateResult option                                    // vTarget,  pSrcRoot, return the update statement 
}

and SeqChildInfo = 
    | Asn1Child of Asn1Child
    | AcnChild  of AcnChild


and Asn1Child = {
    Name                        : StringLoc
    c_name                      : string
    ada_name                    : string                     
    isEqualBodyStats            : CallerScope -> CallerScope -> (string*(LocalVariable list)) option  // 
    isValidBodyStats            : State -> (SeqChoiceChildInfoIsValid option * State)
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    c_name                      : string
    ada_name                    : string                     
    _present_when_name_private  : string // Does not contain the "_PRESENT". Not to be used directly by backends. Backends should use presentWhenName
    acnPresentWhenConditions    : Asn1AcnAst.AcnPresentWhenConditionChoiceChild list
    Comments                    : string array

    chType              :Asn1Type
    Optionality                 : Asn1AcnAst.Asn1ChoiceOptionality option
    
    //DAst properties
    isEqualBodyStats    : CallerScope -> CallerScope  -> string*(LocalVariable list) // 
    isValidBodyStats    : State -> (SeqChoiceChildInfoIsValid option * State)
}

and AcnChoiceEncClass =
    | CEC_uper
    | CEC_enum          of (Asn1AcnAst.ReferenceToEnumerated * Asn1AcnAst.Determinant)
    | CEC_presWhen


and Choice = {
    baseInfo            : Asn1AcnAst.Choice
    children            : ChChildInfo list
    ancEncClass         : AcnChoiceEncClass
    //DAst properties
    constraintsAsn1Str  : string list   //an ASN.1 representation of the constraints
    //baseTypeEquivalence: BaseTypesEquivalence<Choice>
    //typeDefinition      : TypeDefinitionCommon
    definitionOrRef     : TypeDefintionOrReference
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    printValue          : (Asn1ValueKind option) -> (Asn1ValueKind) -> string
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
    updateAcnChildFnc        : CallerScope -> CallerScope -> string
    testCaseFnc : AutomaticTestCase -> TestCaseValue option 
    errCodes    : ErroCode list
}

and AcnParameter = {
    name        : string
    c_name      : string
    asn1Type    : Asn1AcnAst.AcnParamType
    loc         : SrcLoc
    id          : ReferenceToType
    typeDefinitionBodyWithinSeq : string
    funcUpdateStatement         : AcnChildUpdateResult option                                    // vTarget,  pSrcRoot, return the update statement 
}
    


and Asn1Type = {
    id              : ReferenceToType
    acnAligment     : Asn1AcnAst.AcnAligment option
    acnParameters   : AcnParameter list
    Location        : SrcLoc //Line no, Char pos

    //when inheritInfo has a value it indicates that this type is
    //a specialization of a reference type.
    inheritInfo   : InheritanceInfo option

    //it simply indicates that this type is under a type assignment
    typeAssignmentInfo  : AssignmentInfo option

    Kind            : Asn1TypeKind
    parInfoData : Asn1Fold.ParentInfo<ParentInfoData> option
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
    | SequenceOf        of SequenceOf
    | Sequence          of Sequence
    | Choice            of Choice
    | ReferenceType     of ReferenceType



let getNextValidErrorCode (cur:State) (errCodeName:string) =
    
    //if (errCodeName.ToUpper() = "ERR_ACN_ENCODE_TC_DATA_HEADER_TCPACKETPUSVERSIONNUMBER") then
    //    let aaa = cur.curErrCodeNames |> Set.toArray
    //    printfn "???"
        
    let rec getErroCode (errCodeName:string) = 
        match cur.curErrCodeNames.Contains errCodeName with
        | false -> {ErroCode.errCodeName = errCodeName; errCodeValue = cur.currErrorCode}
        | true  -> 
            getErroCode (errCodeName + "_2")

    let errCode = getErroCode (errCodeName.ToUpper())
    errCode, {cur with currErrorCode = cur.currErrorCode + 1; curErrCodeNames = cur.curErrCodeNames.Add errCode.errCodeName}

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
}


type AstRoot = {
    Files        : Asn1File list
    deps         :Asn1AcnAst.AcnInsertedFieldDependencies
    acnConstants : Map<string, BigInteger>
    args         : CommandLineSettings
    programUnits : ProgramUnit list
    lang         : ProgrammingLanguage
    acnParseResults:ParameterizedAsn1Ast.AntlrParserResult list //used in ICDs to regenerate with collors the initial ACN input
}

