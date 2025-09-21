module Language
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open AbstractMacros
open Asn1AcnAstUtilFunctions

type Uper_parts = {
    createLv : string -> LocalVariable
    requires_sBlockIndex : bool
    requires_sBLJ        : bool
    requires_charIndex   : bool
    requires_IA5String_i : bool
    count_var            : LocalVariable
    requires_presenceBit : bool
    catd                 : bool //if true then Choice Alternatives are Temporarily Decoded (i.e. in _tmp variables in current scope)
    //createBitStringFunction  : (CallerScope -> CommonTypes.Codec -> ErrorCode -> int -> BigInteger -> BigInteger -> BigInteger -> string -> BigInteger -> bool -> bool -> (string * LocalVariable list)) -> CommonTypes.Codec -> ReferenceToType -> TypeDefinitionOrReference -> bool -> BigInteger -> BigInteger -> BigInteger -> ErrorCode ->  CallerScope -> UPERFuncBodyResult
    seqof_lv             : ReferenceToType -> BigInteger -> BigInteger -> LocalVariable list
    exprMethodCall       : Asn1TypeKind -> string -> string

}

type Acn_parts = {
    null_valIsUnReferenced              : bool
    checkBitPatternPresentResult        : bool
    getAcnDepSizeDeterminantLocVars     : string -> LocalVariable list
    getAcnContainingByLocVars           : string -> LocalVariable list
    createLocalVariableEnum             : string -> LocalVariable       //create a local integer variable that is used to store the value of an enumerated type. The input is the RTL integer type
    choice_handle_always_absent_child   : bool
    choice_requires_tmp_decoding        : bool
}
type Initialize_parts = {
    zeroIA5String_localVars             : int -> LocalVariable list
    choiceComponentTempInit             : bool
    initMethSuffix                      : Asn1TypeKind -> string // TODO REMOVE?
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

type DecodingKind =
    | InPlace
    | Copy

type UncheckedAccessKind =
    | FullAccess // unwrap all selection, including the last one
    | PartialAccess // unwrap all but the last selection

type SequenceChildProps = {
    info: Asn1AcnAst.SeqChildInfo
    sel: AccessPath
    uperMaxOffset: bigint
    acnMaxOffset: bigint
} with
    member this.maxOffset (enc: Asn1Encoding): bigint =
        match enc with
        | ACN -> this.acnMaxOffset
        | UPER -> this.uperMaxOffset
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

type SequenceProofGen = {
    t: Asn1AcnAst.Asn1Type
    sq: Asn1AcnAst.Sequence
    sel: AccessPath
    acnOuterMaxSize: bigint
    uperOuterMaxSize: bigint
    nestingLevel: bigint
    nestingIx: bigint
    uperMaxOffset: bigint
    acnMaxOffset: bigint
    acnSiblingMaxSize: bigint option
    uperSiblingMaxSize: bigint option
    children: SequenceChildProps list
} with

    member this.siblingMaxSize (enc: Asn1Encoding): bigint option =
        match enc with
        | ACN -> this.acnSiblingMaxSize
        | UPER -> this.uperSiblingMaxSize
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

    member this.outerMaxSize (enc: Asn1Encoding): bigint =
        match enc with
        | ACN -> this.acnOuterMaxSize
        | UPER -> this.uperOuterMaxSize
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")
    member this.maxOffset (enc: Asn1Encoding): bigint =
        match enc with
        | ACN -> this.acnMaxOffset
        | UPER -> this.uperMaxOffset
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

type SequenceOfLike =
    | SqOf of Asn1AcnAst.SequenceOf
    | StrType of Asn1AcnAst.StringType
with
    member this.nbElems (enc: Asn1Encoding): bigint * bigint =
        let nbElemsMin, nbElemsMax =
            match this with
            | SqOf sqf -> sqf.minSize, sqf.maxSize
            | StrType st -> st.minSize, st.maxSize
        match enc with
        | ACN -> nbElemsMin.acn, nbElemsMax.acn
        | UPER -> nbElemsMin.uper, nbElemsMax.uper
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

    member this.minNbElems (enc: Asn1Encoding): bigint =
        fst (this.nbElems enc)

    member this.maxNbElems (enc: Asn1Encoding): bigint =
        snd (this.nbElems enc)

    member this.sizeInBits (enc: Asn1Encoding): bigint * bigint =
        match enc, this with
        | ACN, SqOf sqf -> sqf.acnMinSizeInBits, sqf.acnMaxSizeInBits
        | UPER, SqOf sqf -> sqf.uperMinSizeInBits, sqf.uperMaxSizeInBits
        | ACN, StrType st -> st.acnMinSizeInBits, st.acnMaxSizeInBits
        | UPER, StrType st -> st.uperMinSizeInBits, st.uperMaxSizeInBits
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

    member this.minSizeInBits (enc: Asn1Encoding): bigint =
        fst (this.sizeInBits enc)

    member this.maxSizeInBits (enc: Asn1Encoding): bigint =
        snd (this.sizeInBits enc)


    member this.elemSizeInBits (enc: Asn1Encoding): bigint * bigint =
        match enc, this with
        | ACN, SqOf sqf -> sqf.child.acnMinSizeInBits, sqf.child.acnMaxSizeInBits
        | UPER, SqOf sqf -> sqf.child.uperMinSizeInBits, sqf.child.uperMaxSizeInBits
        | ACN, StrType st -> st.acnEncodingClass.charSizeInBits, st.acnEncodingClass.charSizeInBits
        | UPER, StrType st ->
            let sz = GetNumberOfBitsForNonNegativeInteger (bigint (st.uperCharSet.Length - 1))
            sz, sz
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

    member this.minElemSizeInBits (enc: Asn1Encoding): bigint =
        fst (this.elemSizeInBits enc)

    member this.maxElemSizeInBits (enc: Asn1Encoding): bigint =
        snd (this.elemSizeInBits enc)

    member this.isFixedSize: bool =
        match this with
        | SqOf sqf -> sqf.isFixedSize
        | StrType st -> st.isFixedSize

type Asn1TypeOrAcnRefIA5 =
| Asn1 of Asn1AcnAst.Asn1Type
| AcnRefIA5 of ReferenceToType * Asn1AcnAst.AcnReferenceToIA5String

// TODO: rename
type SequenceOfLikeProofGen = {
    t: Asn1TypeOrAcnRefIA5
    acnOuterMaxSize: bigint
    uperOuterMaxSize: bigint
    nestingLevel: bigint
    nestingIx: bigint
    acnMaxOffset: bigint
    uperMaxOffset: bigint
    nestingScope: NestingScope
    cs: CodegenScope
    encDec: string option
    elemDecodeFn: string option
    ixVariable: string
} with
    member this.outerMaxSize (enc: Asn1Encoding): bigint =
        match enc with
        | ACN -> this.acnOuterMaxSize
        | UPER -> this.uperOuterMaxSize
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

    member this.maxOffset (enc: Asn1Encoding): bigint =
        match enc with
        | ACN -> this.acnMaxOffset
        | UPER -> this.uperMaxOffset
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

type SequenceOfLikeProofGenResult = {
    preSerde: string
    postSerde: string
    postInc: string
    invariant: string
}

type SequenceOptionalChild = {
    t: Asn1AcnAst.Asn1Type
    sq: Asn1AcnAst.Sequence
    child: Asn1Child
    existVar: string option
    p: CodegenScope
    nestingScope: NestingScope
    childBody: CodegenScope -> string option -> string
}

type AcnFuncBody = State -> ErrorCode -> (AcnGenericTypes.RelativePath * AcnGenericTypes.AcnParameter) list -> NestingScope -> CodegenScope -> (AcnFuncBodyResult option) * State

[<AbstractClass>]
type ILangGeneric () =
    abstract member ArrayStartIndex : int
    abstract member getPointer      : AccessPath -> string;
    abstract member getPointerUnchecked: AccessPath -> UncheckedAccessKind -> string;
    abstract member getValue        : AccessPath -> string;
    abstract member getValueUnchecked: AccessPath -> UncheckedAccessKind -> string;
    abstract member joinSelectionUnchecked: AccessPath -> UncheckedAccessKind -> string;
    abstract member getAccess       : AccessPath -> string;
    abstract member getAccess2      : AccessStep  -> string;
    abstract member getStar         : AccessPath -> string;
    abstract member getPtrPrefix    : AccessPath -> string;
    abstract member getPtrSuffix    : AccessPath -> string;

    abstract member getArrayItem    : sel: AccessPath -> idx: string -> childTypeIsString: bool -> AccessPath;
    abstract member asn1SccIntValueToString : BigInteger -> unsigned: bool -> string;
    abstract member intValueToString : BigInteger -> Asn1AcnAst.IntegerClass -> string;
    abstract member doubleValueToString : double -> string
    abstract member initializeString : int -> string
    abstract member supportsInitExpressions : bool
    abstract member setNamedItemBackendName0 : Asn1Ast.NamedItem -> string -> Asn1Ast.NamedItem
    abstract member getNamedItemBackendName0 : Asn1Ast.NamedItem -> string
    abstract member getNamedItemBackendName  : TypeDefinitionOrReference option -> Asn1AcnAst.NamedItem -> string
    abstract member getNamedItemBackendName2  : string -> string -> Asn1AcnAst.NamedItem -> string
    abstract member decodeEmptySeq  : string -> string option
    abstract member decode_nullType : string -> string option
    abstract member castExpression  : string -> string -> string
    abstract member createSingleLineComment : string -> string
    abstract member SpecNameSuffix: string
    abstract member SpecExtension : string
    abstract member BodyExtension : string
    abstract member Keywords : string Set
    abstract member isCaseSensitive : bool

    abstract member RtlFuncNames : string list
    abstract member AlwaysPresentRtlFuncNames : string list

    abstract member detectFunctionCalls : string -> string -> string list
    abstract member removeFunctionFromHeader : string -> string -> string
    abstract member removeFunctionFromBody : string -> string -> string


    abstract member getRtlFiles : Asn1Encoding list -> string list -> string list

    abstract member getChildInfoName : Asn1Ast.ChildInfo -> string
    abstract member setChildInfoName : Asn1Ast.ChildInfo -> string -> Asn1Ast.ChildInfo

    abstract member getAsn1ChildBackendName0  : Asn1AcnAst.Asn1Child -> string
    abstract member getAsn1ChChildBackendName0: Asn1AcnAst.ChChildInfo -> string
    abstract member getChoiceChildPresentWhenName : Asn1AcnAst.Choice -> Asn1AcnAst.ChChildInfo -> string

    abstract member getAsn1ChildBackendName  : Asn1Child -> string
    abstract member getAsn1ChChildBackendName: ChChildInfo -> string


    abstract member choiceIDForNone : Map<string,int> -> ReferenceToType -> string

    abstract member Length          : string -> string -> string
    abstract member typeDef         : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition> -> FE_PrimitiveTypeDefinition
    abstract member definitionOrRef : Map<ProgrammingLanguage, TypeDefinitionOrReference> -> TypeDefinitionOrReference
    abstract member getTypeDefinition : Map<ProgrammingLanguage, FE_TypeDefinition> -> FE_TypeDefinition
    abstract member getEnumTypeDefinition : Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>  -> FE_EnumeratedTypeDefinition
    abstract member getStrTypeDefinition : Map<ProgrammingLanguage, FE_StringTypeDefinition> -> FE_StringTypeDefinition
    abstract member getChoiceTypeDefinition : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition> -> FE_ChoiceTypeDefinition
    abstract member getSequenceTypeDefinition :Map<ProgrammingLanguage, FE_SequenceTypeDefinition> -> FE_SequenceTypeDefinition
    abstract member getSizeableTypeDefinition : Map<ProgrammingLanguage, FE_SizeableTypeDefinition> -> FE_SizeableTypeDefinition

    abstract member getSeqChild: sel: AccessPath -> childName: string -> childTypeIsString: bool -> childIsOptional: bool -> AccessPath;
    //return a string that contains code with a boolean expression that is true if the child is present
    abstract member getSeqChildIsPresent   : AccessPath -> string -> string
    abstract member getChChildIsPresent   : AccessPath -> string -> string-> string
    abstract member getChChild      : AccessPath -> string -> bool -> AccessPath;
    abstract member getLocalVariableDeclaration : LocalVariable -> string;
    abstract member getLongTypedefName : TypeDefinitionOrReference -> string;
    abstract member getEmptySequenceInitExpression : string -> string
    abstract member callFuncWithNoArgs : unit -> string
    abstract member extractEnumClassName : string -> string -> string -> string
    abstract member presentWhenName : TypeDefinitionOrReference option -> ChChildInfo -> string;
    abstract member presentWhenName0 : TypeDefinitionOrReference option -> Asn1AcnAst.ChChildInfo -> string;
    abstract member getParamTypeSuffix : Asn1AcnAst.Asn1Type -> string -> Codec -> CodegenScope;
    abstract member getParamValue   : Asn1AcnAst.Asn1Type -> AccessPath -> Codec -> string

    abstract member getParamType    : Asn1AcnAst.Asn1Type -> Codec -> CodegenScope;
    abstract member rtlModuleName   : string
    abstract member hasModules      : bool
    abstract member allowsSrcFilesWithNoFunctions : bool
    abstract member requiresValueAssignmentsInSrcFile      : bool
    abstract member requiresHandlingOfEmptySequences : bool
    abstract member requiresHandlingOfZeroArrays : bool

    abstract member supportsStaticVerification      : bool
    abstract member AssignOperator   :string
    abstract member TrueLiteral      :string
    abstract member FalseLiteral     :string
    abstract member emptyStatement   :string
    abstract member bitStreamName    :string
    abstract member unaryNotOperator :string
    abstract member modOp            :string
    abstract member eqOp             :string
    abstract member neqOp            :string
    abstract member andOp            :string
    abstract member orOp             :string
    abstract member initMethod       :InitMethod
    abstract member decodingKind     :DecodingKind
    abstract member usesWrappedOptional: bool
    abstract member bitStringValueToByteArray:  BitStringValue -> byte[]

    abstract member toHex : int -> string
    abstract member uper  : Uper_parts;
    abstract member acn   : Acn_parts
    abstract member init  : Initialize_parts
    abstract member atc   : Atc_parts
    abstract member getValueAssignmentName : ValueAssignment -> string

    abstract member CreateMakeFile : AstRoot -> DirInfo -> unit
    abstract member CreateAuxFiles : AstRoot -> DirInfo -> string list*string list -> unit

    abstract member getDirInfo : Targets option -> string -> DirInfo
    abstract member getTopLevelDirs : Targets option -> string list
    abstract member getBoardNames : Targets option -> string list
    abstract member getBoardDirs : Targets option -> string list

    abstract member adaptAcnFuncBody: Asn1AcnAst.AstRoot -> Asn1AcnAst.AcnInsertedFieldDependencies -> AcnFuncBody -> isValidFuncName: string option -> Asn1AcnAst.Asn1Type -> Codec -> AcnFuncBody
    abstract member generateSequenceAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Sequence -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateIntegerAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Integer -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateBooleanAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Boolean -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateSequenceOfLikeAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> SequenceOfLike -> SequenceOfLikeProofGen -> Codec -> string list * string option
    abstract member generateOptionalAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> SequenceOptionalChild -> Codec -> string list * string
    abstract member generateChoiceAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Choice -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateNullTypeAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.NullType -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateEnumAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Enumerated -> NestingScope -> AccessPath -> Codec -> string list

    abstract member generatePrecond: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Codec -> string list
    abstract member generatePostcond: Asn1AcnAst.AstRoot -> Asn1Encoding -> funcNameBase: string -> p: CodegenScope -> t: Asn1AcnAst.Asn1Type -> Codec -> string option
    abstract member generateSequenceChildProof: Asn1AcnAst.AstRoot -> Asn1Encoding -> stmts: string option list -> SequenceProofGen -> Codec -> string list
    abstract member generateSequenceProof: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Sequence -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateChoiceProof: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Choice -> stmt: string -> AccessPath -> Codec -> string
    abstract member generateSequenceOfLikeProof: Asn1AcnAst.AstRoot -> Asn1Encoding -> SequenceOfLike -> SequenceOfLikeProofGen -> Codec -> SequenceOfLikeProofGenResult option
    abstract member generateIntFullyConstraintRangeAssert: topLevelTd: string -> CodegenScope -> Codec -> string option

    abstract member generateOctetStringInvariants: SIZE -> SIZE -> string list
    abstract member generateBitStringInvariants:  SIZE -> SIZE -> string list
    abstract member generateSequenceInvariants: Asn1AcnAst.Asn1Child list-> string list
    abstract member generateSequenceOfInvariants: SIZE -> SIZE -> string list

    abstract member generateSequenceSizeDefinitions: (AcnGenericTypes.AcnAlignment option)-> (AcnGenericTypes.AcnAlignment option)-> (BigInteger)->(BigInteger)-> (Asn1AcnAst.SeqChildInfo list) -> string list
    abstract member generateChoiceSizeDefinitions: AcnGenericTypes.AcnAlignment option ->AcnGenericTypes.AcnAlignment option-> BigInteger->BigInteger->Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>->Asn1AcnAst.ChChildInfo list-> string list
    //(typeDef : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (maxSize : SIZE) (acnEncodingClass : SizeableAcnEncodingClass) (acnAlignment : AcnAlignment option) (child : Asn1AcnAst.Asn1Type)
    abstract member generateSequenceOfSizeDefinitions: Map<ProgrammingLanguage, FE_SizeableTypeDefinition> -> BigInteger -> BigInteger-> SIZE -> Asn1AcnAst.SizeableAcnEncodingClass -> AcnGenericTypes.AcnAlignment option -> AcnGenericTypes.AcnAlignment option -> Asn1AcnAst.Asn1Type -> string list * string list
    abstract member generateSequenceSubtypeDefinitions: dealiased: string -> Map<ProgrammingLanguage, FE_SequenceTypeDefinition> -> Asn1AcnAst.Asn1Child list -> string list
    abstract member real_annotations : string list

    default this.getParamType (t:Asn1AcnAst.Asn1Type) (c:Codec) : CodegenScope =
        this.getParamTypeSuffix t "" c
    default this.requiresHandlingOfEmptySequences = false
    default this.requiresHandlingOfZeroArrays = false
    default this.RtlFuncNames = []
    default this.AlwaysPresentRtlFuncNames = []
    default this.detectFunctionCalls (sourceCode: string) (functionName: string) = []
    default this.removeFunctionFromHeader (sourceCode: string) (functionName: string) : string =
        sourceCode
    default this.removeFunctionFromBody (sourceCode: string) (functionName: string) : string =
        sourceCode
    default this.real_annotations = []

    default this.extractEnumClassName (prefix: string) (varName: string) (internalName: string): string = ""
        

    default this.adaptAcnFuncBody _ _ f _ _ _ = f
    default this.generateSequenceAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateIntegerAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateBooleanAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateSequenceOfLikeAuxiliaries _ _ _ _ _ = [], None
    default this.generateOptionalAuxiliaries _ _ soc _ =
        // By default, languages do not have wrapped optional and have an `exist` field: they "attach" the child field themselves
        [], soc.childBody {soc.p with accessPath = soc.p.accessPath.dropLast} soc.existVar
    default this.generateChoiceAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateNullTypeAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateEnumAuxiliaries _ _ _ _ _ _ _ = []

    default this.generatePrecond _ _ _ _ = []
    default this.generatePostcond _ _ _ _ _ _ = None
    default this.generateSequenceChildProof _ _ stmts _ _ = stmts |> List.choose id
    default this.generateSequenceProof _ _ _ _ _ _ _ = []
    default this.generateChoiceProof _ _ _ _ stmt _ _ = stmt
    default this.generateSequenceOfLikeProof _ _ _ _ _ = None
    default this.generateIntFullyConstraintRangeAssert _ _ _ = None

    default this.generateOctetStringInvariants _ _ = []
    default this.generateBitStringInvariants _ _ = []
    default this.generateSequenceInvariants  _ = []
    default this.generateSequenceOfInvariants _ _ = []

    default this.generateSequenceSizeDefinitions _ _ _ _ _ = []
    default this.generateChoiceSizeDefinitions _ _ _ _ _ _ = []
    default this.generateSequenceOfSizeDefinitions _ _ _ _ _ _ _ _ = [], []
    default this.generateSequenceSubtypeDefinitions _ _ _ = []

    //most programming languages are case sensitive
    default _.isCaseSensitive = true
    default _.getBoardNames _ = []
    default _.getBoardDirs  _ = []


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

type AccessPath with
    member this.joined (lg: ILangGeneric): string =
        List.fold (fun str accessor -> $"{str}{lg.getAccess2 accessor}") this.rootId this.steps
    member this.joinedUnchecked (lg: ILangGeneric) (kind: UncheckedAccessKind): string =
        lg.joinSelectionUnchecked this kind
    member this.asIdentifier: string =
        List.fold (fun str accessor ->
            let acc =
                match accessor with
                | ValueAccess (id, _, _) -> ToC id
                | PointerAccess (id, _, _) -> ToC id
                | ArrayAccess _ -> "arr"
            $"{str}_{acc}") this.rootId this.steps
