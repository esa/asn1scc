module Language
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open AbstractMacros

type Uper_parts = {
    createLv : string -> LocalVariable
    requires_sBlockIndex : bool
    requires_sBLJ        : bool
    requires_charIndex   : bool
    requires_IA5String_i : bool
    count_var            : LocalVariable
    requires_presenceBit : bool
    catd                 : bool //if true then Choice Alternatives are Temporarily Decoded (i.e. in _tmp variables in curent scope)
    //createBitStringFunction  : (CallerScope -> CommonTypes.Codec -> ErroCode -> int -> BigInteger -> BigInteger -> BigInteger -> string -> BigInteger -> bool -> bool -> (string * LocalVariable list)) -> CommonTypes.Codec -> ReferenceToType -> TypeDefintionOrReference -> bool -> BigInteger -> BigInteger -> BigInteger -> ErroCode ->  CallerScope -> UPERFuncBodyResult
    seqof_lv             : ReferenceToType -> BigInteger -> BigInteger -> LocalVariable list

}

type Acn_parts = {
    null_valIsUnReferenced              : bool
    checkBitPatternPresentResult        : bool
    getAcnDepSizeDeterminantLocVars     : string -> LocalVariable list
    choice_handle_always_absent_child   : bool
    choice_requires_tmp_decoding        : bool
}
type Initialize_parts = {
    zeroIA5String_localVars             : int -> LocalVariable list
    choiceComponentTempInit             : bool
}

type Atc_parts = {
    uperPrefix : string
    acnPrefix : string
    xerPrefix : string
    berPrefix : string
}


type InitMethod = 
    | Procedure
    | Function

[<AbstractClass>]
type ILangGeneric () =
    abstract member ArrayStartIndex : int
    abstract member getPointer      : FuncParamType -> string;
    abstract member getValue        : FuncParamType -> string;
    abstract member getAccess       : FuncParamType -> string;
    abstract member getStar         : FuncParamType -> string;
    abstract member getPtrPrefix    : FuncParamType -> string;
    abstract member getPtrSuffix    : FuncParamType -> string;
    abstract member getAmber        : FuncParamType -> string;
    abstract member toPointer       : FuncParamType -> FuncParamType;
    abstract member getArrayItem    : FuncParamType -> (string) -> (bool) -> FuncParamType;
    abstract member intValueToString : BigInteger -> Asn1AcnAst.IntegerClass -> string;
    abstract member doubleValueToString : double -> string
    abstract member initializeString : int -> string
    abstract member supportsInitExpressions : bool
    abstract member getNamedItemBackendName  : TypeDefintionOrReference option -> Asn1AcnAst.NamedItem -> string
    abstract member getNamedItemBackendName2  : string -> string -> Asn1AcnAst.NamedItem -> string
    abstract member decodeEmptySeq  : string -> string option
    abstract member decode_nullType : string -> string option
    abstract member castExpression  : string -> string -> string
    abstract member createSingleLineComment : string -> string
    abstract member SpecExtention : string
    abstract member BodyExtention : string

    abstract member getRtlFiles : Asn1Encoding list -> string list -> string list

    abstract member getAsn1ChildBackendName0  : Asn1AcnAst.Asn1Child -> string
    abstract member getAsn1ChChildBackendName0: Asn1AcnAst.ChChildInfo -> string

    abstract member getAsn1ChildBackendName  : Asn1Child -> string
    abstract member getAsn1ChChildBackendName: ChChildInfo -> string

    abstract member choiceIDForNone : Map<string,int> -> ReferenceToType -> string

    abstract member Length          : string -> string -> string
    abstract member typeDef         : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition> -> FE_PrimitiveTypeDefinition
    abstract member getTypeDefinition : Map<ProgrammingLanguage, FE_TypeDefinition> -> FE_TypeDefinition
    abstract member getEnmTypeDefintion : Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>  -> FE_EnumeratedTypeDefinition
    abstract member getStrTypeDefinition : Map<ProgrammingLanguage, FE_StringTypeDefinition> -> FE_StringTypeDefinition
    abstract member getChoiceTypeDefinition : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition> -> FE_ChoiceTypeDefinition
    abstract member getSequenceTypeDefinition :Map<ProgrammingLanguage, FE_SequenceTypeDefinition> -> FE_SequenceTypeDefinition
    abstract member getSizeableTypeDefinition : Map<ProgrammingLanguage, FE_SizeableTypeDefinition> -> FE_SizeableTypeDefinition

    abstract member getSeqChild     : FuncParamType -> string -> bool -> FuncParamType;
    abstract member getChChild      : FuncParamType -> string -> bool -> FuncParamType;
    abstract member getLocalVariableDeclaration : LocalVariable -> string;
    abstract member getLongTypedefName : TypeDefintionOrReference -> string;
    abstract member getEmptySequenceInitExpression : unit -> string
    abstract member callFuncWithNoArgs : unit -> string

    //abstract member getEnmLongTypedefName : FE_EnumeratedTypeDefinition -> string -> FE_EnumeratedTypeDefinition;


    abstract member ArrayAccess     : string -> string;

    abstract member presentWhenName : TypeDefintionOrReference option -> ChChildInfo -> string;
    abstract member getParamTypeSuffix : Asn1AcnAst.Asn1Type -> string -> Codec -> CallerScope;
    abstract member getParamValue   : Asn1AcnAst.Asn1Type -> FuncParamType -> Codec -> string

    abstract member getParamType    : Asn1AcnAst.Asn1Type -> Codec -> CallerScope;
    abstract member rtlModuleName   : string
    abstract member hasModules      : bool
    abstract member allowsSrcFilesWithNoFunctions : bool
    abstract member requiresValueAssignmentsInSrcFile      : bool

    abstract member supportsStaticVerification      : bool
    abstract member AssignOperator   :string
    abstract member TrueLiteral      :string
    abstract member FalseLiteral     :string
    abstract member emtyStatement    :string
    abstract member bitStreamName    :string
    abstract member unaryNotOperator :string
    abstract member modOp            :string
    abstract member eqOp             :string
    abstract member neqOp            :string
    abstract member andOp            :string
    abstract member orOp             :string
    abstract member initMetod        :InitMethod
    abstract member bitStringValueToByteArray:  BitStringValue -> byte[]

    abstract member toHex : int -> string
    abstract member uper  : Uper_parts;
    abstract member acn   : Acn_parts
    abstract member init  : Initialize_parts
    abstract member atc   : Atc_parts
    abstract member getValueAssignmentName : ValueAssignment -> string

    abstract member CreateMakeFile : AstRoot -> OutDirectories.DirInfo -> unit
    abstract member CreateAuxFiles : AstRoot -> OutDirectories.DirInfo -> string list*string list -> unit

//    abstract member createLocalVariable_frag : string -> LocalVariable

    default this.getAmber (fpt:FuncParamType) =
        if this.getStar fpt = "*" then "&" else ""        
    default this.toPointer  (fpt:FuncParamType) =
        POINTER (this.getPointer fpt)
    default this.getParamType    (t:Asn1AcnAst.Asn1Type) (c:Codec) : CallerScope =
        this.getParamTypeSuffix t "" c


type LanguageMacros = {
    lg      : ILangGeneric;
    init    : IInit;
    equal   : IEqual;
    typeDef : ITypeDefinition;
    isvalid : IIsValid
    vars    : IVariables
    uper    : IUper
    acn     : IAcn
    atc     : ITestCases
    xer     : IXer
    src     : ISrcBody
}



