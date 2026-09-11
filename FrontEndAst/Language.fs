module Language

open AcnGenericTypes
open Asn1AcnAst
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
    zeroOctetString_localVars           : int -> LocalVariable list
    zeroBitString_localVars             : int -> LocalVariable list
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

type public SequenceChildStmt = {
    body: string option
    lvs: LocalVariable list
    errCodes: ErrorCode list
    userDefinedFunctions : UserDefinedFunction list
    icdComments : string list
}

type AcnFuncBody = State -> ErrorCode -> (AcnGenericTypes.RelativePath * AcnGenericTypes.AcnParameter) list -> NestingScope -> CodegenScope -> (AcnFuncBodyResult option) * State

/// Quadruple για deferred patching:
///   (initFuncName, patchFuncName, nBitsOpt, uperMinOffset)
/// nBitsOpt = Some n για ConstSize encoders που δέχονται bit-width· None για fixed-size (U8, U16, ...).
/// uperMinOffset = UPER minimum offset (≠0 μόνο σε Integer_uPER fixed-size). Όταν ≠0, ο PatchDet γράφει (value-offset).
type DetFunctionNames = string * string * BigInteger option * BigInteger

[<AbstractClass>]
type ILangGeneric () =
    abstract member ArrayStartIndex : int
    abstract member getPointer      : AccessPath -> string;
    abstract member getPointerUnchecked: AccessPath -> UncheckedAccessKind -> string;
    abstract member getValue        : AccessPath -> string;
    abstract member getValueUnchecked: AccessPath -> UncheckedAccessKind -> string
    abstract member joinSelection: AccessPath -> string;
    abstract member joinSelectionEnum: AccessPath -> string;
    abstract member joinSelectionUnchecked: AccessPath -> UncheckedAccessKind -> string;
    abstract member asSelectionIdentifier: AccessPath -> string;
    abstract member getAccess       : AccessPath -> string;
    abstract member getAccess2      : AccessStep  -> string;
    abstract member getAccess3      : AccessStep  -> string;
    abstract member getStar         : AccessPath -> string;
    abstract member getPtrPrefix    : AccessPath -> string;
    abstract member getPtrSuffix    : AccessPath -> string;

    abstract member getArrayItem    : sel: AccessPath -> idx: string -> childTypeIsString: bool -> AccessPath;
    abstract member asn1SccIntValueToString : BigInteger -> unsigned: bool -> string;
    abstract member intValueToString : BigInteger -> Asn1AcnAst.IntegerClass -> string;
    abstract member doubleValueToString : double -> string
    abstract member initializeString :BigInteger option -> int -> string    //the ascii code to use for initialization, and the length of the string
    abstract member supportsInitExpressions : bool
    abstract member setNamedItemBackendName0 : Asn1Ast.NamedItem -> string -> Asn1Ast.NamedItem
    abstract member getNamedItemBackendName0 : Asn1Ast.NamedItem -> string
    abstract member getNamedItemBackendName  : TypeDefinitionOrReference option -> Asn1AcnAst.NamedItem -> string
    abstract member getNamedItemBackendName2  : string -> string -> Asn1AcnAst.NamedItem -> string
    abstract member decodeEmptySeq  : string -> string option
    abstract member decode_nullType : string -> string option
    abstract member castExpression  : string -> string -> string
    abstract member castRealForEquality : floatingPointSizeInBytes:BigInteger -> realClass:RealClass -> pp:string -> realTypeName:string -> real32TypeName:string -> string
    default this.castRealForEquality _ realClass pp realTypeName _ =
        match realClass with
        | ASN1SCC_FP32 -> this.castExpression pp realTypeName
        | _            -> pp

    // Program-unit code-generation hooks used by the generic GenerateFiles driver.
    // selectProgramUnitRenderTypes picks and orders the (tasForFlags, fullType, encDecType)
    // triples to render for a program unit. The default is one triple per type assignment;
    // object-oriented backends (Python) override it to perform deep-field-access resolution
    // and deduplication (see ACN User Manual 4.2). The resolver argument yields a type and its
    // resolved children (post-order); it is injected because it lives in the backend layer.
    abstract member selectProgramUnitRenderTypes : DAst.ProgramUnit -> DAst.TypeAssignment list -> (DAst.Asn1Type -> DAst.Asn1Type list) -> (DAst.TypeAssignment * DAst.Asn1Type * DAst.Asn1Type) list
    default this.selectProgramUnitRenderTypes _ (tases: DAst.TypeAssignment list) _ = tases |> List.map (fun t -> (t, t.Type, t.Type))

    // Statement to append to the package aggregator file (Python's __init__.py) when a program
    // unit is emitted. None for languages that have no such file.
    abstract member programUnitImportStatement : puName:string -> string option
    default this.programUnitImportStatement _ = None

    abstract member createSingleLineComment : string -> string
    abstract member SpecNameSuffix: string
    abstract member SpecExtension : string
    abstract member BodyExtension : string
    abstract member Keywords : string Set
    abstract member isCaseSensitive : bool
    abstract member isFilenameCaseSensitive : bool

    abstract member constructFuncName           : string -> string -> string -> string
    abstract member constructReferenceFuncName  : string -> string -> string -> string
    abstract member getFuncNameGeneric          : TypeDefinitionOrReference -> string -> string option
    abstract member getFuncNameGeneric2         : TypeDefinitionOrReference -> string option
    abstract member getUPerFuncName             : Asn1AcnAst.AstRoot -> CommonTypes.Codec -> Asn1AcnAst.Asn1Type -> FE_TypeDefinition -> string option
    abstract member getXerFuncName              : CommonTypes.Codec -> TypeDefinitionOrReference -> string option
    abstract member getACNFuncName              : Asn1AcnAst.AstRoot -> CommonTypes.Codec -> Asn1AcnAst.Asn1Type -> FE_TypeDefinition -> string option
    /// The encode/decode suffix used when building generated function and error-code names.
    /// C/Ada/Scala use "_Encode"/"_Decode"; Python uses "encode"/"decode". Replaces the old
    /// Codec.suffix, which decided this by reading the global ActiveLanguages.Head.
    abstract member codecSuffix                 : CommonTypes.Codec -> string
    /// Whether the backend tolerates an ACN dependency whose parent scope is not reachable from
    /// the current nesting (e.g. a type generated standalone). Python needs this; the others must
    /// fail loudly rather than silently drop a determinant update. See handle* in AcnDependencies.
    abstract member allowUnresolvedAcnDependency : bool
    
    abstract member RtlFuncNames : string list
    abstract member getAlwaysPresentRtlFuncNames : CommandLineSettings -> string list

    abstract member detectFunctionCalls : string -> string -> string list
    abstract member removeFunctionFromHeader : string -> string -> string
    abstract member removeFunctionFromBody : string -> string -> string

    /// ACN deferred-patching runtime names per language.
    /// Returns (InitDet, PatchDet, nBitsOpt, uperMinOffset) for the given inserted-field type.
    /// Returns None when the language/type combo doesn't support deferred patching.
    /// Default: None (Ada/Scala don't yet implement --acn-v2).
    abstract member getDeferredDetFunctions : Asn1AcnAst.AcnInsertedType -> DetFunctionNames option
    /// Compute a target-language source-code expression for the wire fallback value
    /// of a deferred determinant that was never patched at runtime.
    /// Default: "0" (Ada/Scala don't yet implement --acn-v2).
    abstract member computeDeferredFallbackValue : Asn1AcnAst.AcnInsertedType -> BigInteger -> string

    /// Wrap the body of a closure-converted (--acn-v2) specialized function
    /// with language-specific suppression of "dead code / unused symbol"
    /// diagnostics.  The public TAS-style wrapper that some backends emit
    /// alongside the working `_aux` body is never called by the deferred
    /// path (callers invoke `_aux` directly), so warnings about it being
    /// unreferenced must be silenced under -Werror-equivalent flags.
    /// Default: identity (C and Scala don't need this).
    abstract member wrapDeferredSpecBody : body:string -> string

    /// Mangle a synthetic local-variable name used by the --acn-v2 deferred
    /// patching backend (e.g., "patchDetVal", "patchDetStrVal") to a form
    /// the target language accepts.  C prepends `_` for compiler-generated
    /// locals (preserves byte-identity with master output); Ada cannot use
    /// leading underscore identifiers.  Default: identity.
    abstract member acnDeferredTempVarName : baseName:string -> string

    abstract member getRtlFiles : Asn1Encoding list -> string list -> string list

    abstract member getChildInfoName : Asn1Ast.ChildInfo -> string
    abstract member setChildInfoName : Asn1Ast.ChildInfo -> string -> Asn1Ast.ChildInfo

    abstract member getAsn1ChildBackendName0  : Asn1AcnAst.Asn1Child -> string
    abstract member getAsn1ChChildBackendName0: Asn1AcnAst.ChChildInfo -> string
    abstract member getChoiceChildPresentWhenName : Asn1AcnAst.Choice -> Asn1AcnAst.ChChildInfo -> string -> string

    abstract member getAsn1ChildBackendName  : Asn1Child -> string
    abstract member getAsn1ChChildBackendName: ChChildInfo -> string


    abstract member choiceIDForNone : Map<string,int> -> ReferenceToType -> string

    abstract member Length          : string -> string -> string
    abstract member FixedSizeSizableHasCount : bool
    default _.FixedSizeSizableHasCount = false
    abstract member typeDef         : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition> -> FE_PrimitiveTypeDefinition
    abstract member definitionOrRef : Map<ProgrammingLanguage, TypeDefinitionOrReference> -> TypeDefinitionOrReference
    abstract member getTypeDefinition : Map<ProgrammingLanguage, FE_TypeDefinition> -> FE_TypeDefinition
    abstract member getEnumTypeDefinition : Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>  -> FE_EnumeratedTypeDefinition
    abstract member getStrTypeDefinition : Map<ProgrammingLanguage, FE_StringTypeDefinition> -> FE_StringTypeDefinition
    abstract member getChoiceTypeDefinition : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition> -> FE_ChoiceTypeDefinition
    abstract member getSequenceTypeDefinition :Map<ProgrammingLanguage, FE_SequenceTypeDefinition> -> FE_SequenceTypeDefinition
    abstract member getSizeableTypeDefinition : Map<ProgrammingLanguage, FE_SizeableTypeDefinition> -> FE_SizeableTypeDefinition

    abstract member getSeqChild: sel: AccessPath -> childName: string -> childTypeIsString: bool -> childIsOptional: bool -> AccessPath
    abstract member getSeqChildDependingOnChoiceParent: parents: (CodegenScope * Asn1AcnAst.Asn1Type) list -> sel: AccessPath -> childName: string -> childTypeIsString: bool -> childIsOptional: bool -> AccessPath
    //return a string that contains code with a boolean expression that is true if the child is present
    abstract member getSeqChildIsPresent   : AccessPath -> string -> string
    abstract member getChChildIsPresent   : AccessPath -> string -> string-> string
    abstract member getChChild      : AccessPath -> string -> bool -> AccessPath;
    // Like getChChild, but may further adjust the access path based on the child's type and codec.
    // E.g., Python enum encode needs .val appended because the enum wrapper stores its value in a .val field.
    abstract member getChChildForKind : AccessPath -> string -> bool -> Asn1TypeKind -> Codec -> AccessPath
    abstract member getLocalVariableDeclaration : LocalVariable -> string;
    abstract member getLongTypedefName : TypeDefinitionOrReference -> string
    abstract member getQualifiedTypeName : TypeDefinitionOrReference -> string -> string
    abstract member getLongTypedefNameBasedOnModule : FE_TypeDefinition -> string -> string
    abstract member getLongTypedefNameFromReferenceToTypeAndCodegenScope : ReferenceToType -> TypeDefinitionOrReference -> CodegenScope -> string option
    abstract member longTypedefName2 : TypeDefinitionOrReference -> bool -> string -> string
    abstract member adjustTypedefWithFullPath : string -> string -> string;
    abstract member getEmptySequenceInitExpression : string -> string
    abstract member callFuncWithNoArgs : unit -> string
    abstract member extractEnumClassName : string -> string -> string -> string
    abstract member presentWhenName : TypeDefinitionOrReference option -> ChChildInfo -> string;
    abstract member presentWhenName0 : TypeDefinitionOrReference option -> Asn1AcnAst.ChChildInfo -> string;
    abstract member getParamTypeSuffix : Asn1AcnAst.Asn1Type -> string -> Codec -> CodegenScope;
    abstract member getParamTypeSuffixForEquals : Asn1AcnAst.Asn1Type -> string -> Codec -> CodegenScope;
    abstract member getParamValue   : Asn1AcnAst.Asn1Type -> AccessPath -> Codec -> string

    abstract member getParamType    : Asn1AcnAst.Asn1Type -> Codec -> CodegenScope
    abstract member getParamTypeAtc : Asn1AcnAst.Asn1Type -> Codec -> CodegenScope
    
    // Additional Methods for ACN Deep Field Access for Object Oriented Languages
    abstract member getAcnChildrenForDeepFieldAccess : Asn1Child list -> AcnChild list -> AcnInsertedFieldDependencies -> Map<string, (string * AcnChild) list>
    default this.getAcnChildrenForDeepFieldAccess _ _ _ = Map.empty
    abstract member isAcnInlineRequired : Asn1AcnAst.Asn1Type -> string -> AcnInsertedFieldDependencies -> bool
    default this.isAcnInlineRequired _ _ _ = false
    abstract member getExternalField : ((AcnDependency -> bool) -> string) -> RelativePath -> Asn1AcnAst.Sequence -> CodegenScope -> string
    default this.getExternalField (getExternalField0: ((AcnDependency -> bool) -> string)) _ _ _ =
        let filterDependency (d:AcnDependency) =
            match d.dependencyKind with
            | AcnDepPresenceBool   -> true
            | _                    -> false
        getExternalField0 filterDependency
    abstract member getAcnChildrenDictStatements : Codec -> (string * AcnChild) list -> CodegenScope -> (string list * string option)
    default this.getAcnChildrenDictStatements _ _ _= [], None
    abstract member updateStateForCrossSequenceAcnParams : Asn1AcnAst.AstRoot -> State -> CodegenScope -> Asn1AcnAst.SeqChildInfo list -> Asn1Child -> NestingScope -> AcnInsertedFieldDependencies -> Asn1AcnAst.Asn1Type -> Codec -> (Asn1AcnAst.Asn1Module -> ReferenceToType -> State -> (AcnChildUpdateResult option*State)) -> (Determinant -> string) -> (Asn1AcnAst.Asn1Module -> Asn1AcnAst.AcnInsertedType -> string) -> (SequenceChildStmt list * string list * State)
    default this.updateStateForCrossSequenceAcnParams _ s _ _ _ _ _ _ _ _ _ _ = [], [], s
        
    abstract member getObjectIdentifierIsValidExpr : CodegenScope -> bool -> string
    default this.getObjectIdentifierIsValidExpr (p: CodegenScope) (isRelative: bool) : string =
        let namespacePrefix = this.rtlModuleName
        let ptr = this.getPointer p.accessPath
        if isRelative then sprintf "%sRelativeOID_isValid(%s)" namespacePrefix ptr
        else sprintf "%sObjectIdentifier_isValid(%s)" namespacePrefix ptr

    // End of additional methods

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
    abstract member ArrayInitByAppend : bool
    default _.ArrayInitByAppend = false
    abstract member TempArrayItemSuffix: string
    default _.TempArrayItemSuffix = "_Temp"
    abstract member usesWrappedOptional: bool
    abstract member needsExistSequence: bool
    default _.needsExistSequence = true
    abstract member integerIsAlwaysSigned: bool
    default _.integerIsAlwaysSigned = false
    abstract member stopAtPrmForChoicePresentWhen: bool
    default _.stopAtPrmForChoicePresentWhen = false
    abstract member usesBooleanPresenceBits: bool
    default _.usesBooleanPresenceBits = false
    abstract member usesChoiceTempVarPath: bool
    default _.usesChoiceTempVarPath = false
    abstract member supportsAcnIcdForUndeclaredType: bool
    default _.supportsAcnIcdForUndeclaredType = true
    abstract member resolveAcnPrmRefTypeEmission: prmTypeName:string -> resolvedKind:Asn1AcnAst.Asn1TypeKind option -> intZero:string -> string * string
    default _.resolveAcnPrmRefTypeEmission prmTypeName _resolvedKind _intZero = prmTypeName, ""
    abstract member needsAcnChoiceDeterminantParam: bool
    default _.needsAcnChoiceDeterminantParam = false
    abstract member nullValueForAbsentOptional: string option
    default _.nullValueForAbsentOptional = None
    abstract member getEnumIntLocalVarName: baseName:string -> string
    default _.getEnumIntLocalVarName baseName = $"intVal_{baseName}"
    abstract member adjustChildDecodeResultExpr: codec:Codec -> isPrimitive:bool -> parentId:string -> childName:string -> defaultResult:string option -> string option
    default _.adjustChildDecodeResultExpr _codec _isPrimitive _parentId _childName defaultResult = defaultResult
    abstract member getInitAssignmentLhs: p:CodegenScope -> string
    default this.getInitAssignmentLhs p = this.getValue p.accessPath
    abstract member adjustEnumAccessForValidation: AccessPath -> AccessPath
    default _.adjustEnumAccessForValidation path = path
    abstract member maybeWrapValueInConstructor: typeRef:TypeDefinitionOrReference -> typeKind:Asn1TypeKind -> modName:string -> value:string -> string
    default _.maybeWrapValueInConstructor _typeRef _typeKind _modName value = value
    abstract member prefixWithModule: modName:string -> name:string -> string
    default _.prefixWithModule _modName name = name
    abstract member getObjectIdentifierAccessPair: p:CodegenScope -> string * string
    default this.getObjectIdentifierAccessPair p = (this.joinSelection p.accessPath, this.getAccess p.accessPath)
    abstract member qualifyNameWithModule: targetMod:string -> curMod:string -> name:string -> string
    default _.qualifyNameWithModule _targetMod _curMod name = name
    abstract member wrapIA5StringValue: typeRef:TypeDefinitionOrReference -> modName:string -> literal:string -> string
    default _.wrapIA5StringValue _typeRef _modName literal = literal
    abstract member formatEnumValueInit: enumTd:FE_EnumeratedTypeDefinition -> itemCName:string -> defaultValue:string -> string
    default _.formatEnumValueInit _enumTd _itemCName defaultValue = defaultValue
    abstract member formatValueAssignmentTestCase: typeKind:Asn1TypeKind -> valueType:string -> initStmt:string -> string
    default _.formatValueAssignmentTestCase _typeKind _valueType initStmt = initStmt
    abstract member adjustTestCaseObjectIdentifierInit: modName:string -> tasName:string -> initStmt:string -> string
    default _.adjustTestCaseObjectIdentifierInit _modName _tasName initStmt = initStmt
    abstract member isObjectOriented: bool
    abstract member nullTerminatorByte: byte option
    abstract member charToNumericValueExpression : string -> string
    default this.charToNumericValueExpression charValue = charValue

    /// Converts an ASCII code (given as a decimal string) to a character literal
    /// in the target language (e.g. `'A'` for C, `b'A'` for Rust).
    abstract member charLiteralFromAsciiCode : string -> string
    default this.charLiteralFromAsciiCode asciiCode =
        sprintf "'%c'" (char (int asciiCode))
    abstract member validationStringPrefix : string
    default this.validationStringPrefix = "str"
    abstract member shouldRemoveModulePrefixFromTypedef : bool
    default this.shouldRemoveModulePrefixFromTypedef = false
    abstract member scopeErrorCodeNamesPerTypeAssignment : bool
    default this.scopeErrorCodeNamesPerTypeAssignment = false
    abstract member subtypeDecodeWrap : pp:string -> currentTypeName:string -> isPrimitive:bool -> string option
    default _.subtypeDecodeWrap _pp _currentTypeName _isPrimitive = None
    abstract member getEnumSelectionJoin : AccessPath -> string
    // XER enum encode switches on the enum VALUE, so the default must dereference to the value
    // (e.g. C "(*pVal)"), not the bare selection path (which for a ByPointer param is "pVal" and
    // yields invalid `switch(pVal)`). Python overrides this to append ".val" for child paths.
    default this.getEnumSelectionJoin path = this.getValue path
    abstract member getAlignmentByteTypeName : string
    default this.getAlignmentByteTypeName = "NextByte"
    abstract member getAlignmentWordTypeName : string
    default this.getAlignmentWordTypeName = "NextWord"
    abstract member getAlignmentDWordTypeName : string
    default this.getAlignmentDWordTypeName = "NextDWord"
    abstract member shouldApplyToCToPackageName : bool
    default this.shouldApplyToCToPackageName = false
    abstract member shouldAppendToBodyFile : bool
    default this.shouldAppendToBodyFile = false
    abstract member shouldGenerateInitFiles : bool
    default this.shouldGenerateInitFiles = false
    abstract member shouldAppendTestCaseFile : bool
    default this.shouldAppendTestCaseFile = false
    abstract member shouldWriteThenAppendTestSuite : bool
    default this.shouldWriteThenAppendTestSuite = false
    abstract member bitStringValueToByteArray:  BitStringValue -> byte[]

    abstract member padArraysWithDefaultValues : bool
    abstract member amberDecodePrefix : string

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
    abstract member adaptFuncBodyChoice: Asn1TypeKind -> Codec -> IUper -> Asn1Encoding -> string -> string -> string -> string
    abstract member choiceChildDecodePath: sChildTypeDef:string -> sChildName:string -> AccessPath option
    // Merges encode/decode constant bodies into single classes (Python) or returns legacy procs (others)
    abstract member assembleAllProcs: arrsEncConstBodies:string list -> arrsDecConstBodies:string list -> arrsFuncsAndOtherProcs:string list -> arrsLegacyAllProcs:string list -> string list
    abstract member generateSequenceAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Sequence -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateIntegerAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Integer -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateBooleanAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Boolean -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateOctetStringAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.OctetString -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateBitStringAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.BitString -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateSequenceOfLikeAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> SequenceOfLike -> SequenceOfLikeProofGen -> Codec -> string list * string option
    abstract member generateOptionalAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> SequenceOptionalChild -> Codec -> string list * string
    abstract member generateChoiceAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Choice -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateNullTypeAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.NullType -> NestingScope -> AccessPath -> Codec -> string list
    abstract member generateEnumAuxiliaries: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Asn1AcnAst.Enumerated -> NestingScope -> AccessPath -> Codec -> string list

    abstract member generatePrecond: Asn1AcnAst.AstRoot -> Asn1Encoding -> Asn1AcnAst.Asn1Type -> Codec -> string list
    abstract member generatePostcond: Asn1AcnAst.AstRoot -> Asn1Encoding -> p: CodegenScope -> t: Asn1AcnAst.Asn1Type -> Codec -> string list
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
    abstract member getTypeBasedSuffix: FunctionType -> Asn1AcnAst.Asn1TypeKind -> string

    // ─────────────────────────────────────────────────────────────────────
    // Language-specific behaviour members added to eliminate hard-coded
    // `match ProgrammingLanguage.ActiveLanguages.Head with | Rust -> ... | _ -> ...`
    // in the F# backend.  Each has a sensible default (the C/Ada behaviour)
    // and is overridden only by the backend that needs different output.
    // ─────────────────────────────────────────────────────────────────────

    /// Init expression for a NullType constant.
    /// Default "0" (C/Ada/Scala/Python); Rust overrides to "()".
    /// Replaces DAstConstruction.fs:97, DAstInitialize.fs:497.
    abstract member nullTypeInitExpression : string
    default _.nullTypeInitExpression = "0"

    /// Default init value for complex types (the fall-through arm of
    /// extractDefaultInitValue).
    /// Default "null" (C/Ada/Scala/Python); Rust overrides to "Default::default()".
    /// Replaces DAstUtilFunctions.fs:57.
    abstract member complexTypeDefaultInit : string
    default _.complexTypeDefaultInit = "null"

    /// Annotations (e.g. `"extern"`, `"pure"`) attached to a generated
    /// function definition.  The `FunctionType` argument lets backends
    /// choose different annotations per function kind.
    /// Default: `[]` for all function types.
    /// Scala overrides: `["extern"]` for UPER/ACN/XER, `["extern"; "pure"]` for init.
    /// Replaces DAstInitialize.fs:315, FE_TypeDefinition.fs:622,
    /// DAstUPer.fs:267, AcnPrimitives.fs:343.
    abstract member funcDefAnnotations : FunctionType -> string list
    default _.funcDefAnnotations _ = []

    /// Wrap an IA5String constant-initialisation expression in a language-
    /// specific struct literal when needed.
    /// Default: returns `initStr` unchanged (identity).
    /// Rust overrides: `if tdName = "" then initStr else sprintf "%s { arr: %s }" tdName initStr`.
    /// Replaces DAstInitialize.fs:391-393.
    abstract member wrapIA5StringConstantInit : tdName:string -> initStr:string -> string
    default _.wrapIA5StringConstantInit _tdName initStr = initStr

    /// Whether to use the inline init expression (`initExpressionFnc()`)
    /// instead of a function-call (`funcName + initMethSuffix`) for complex
    /// child types.
    /// Default: `false` (use function-call form — C/Ada/Scala/Python).
    /// Rust overrides: `true`.
    /// Replaces DAstInitialize.fs:761-764, 1374-1378.
    abstract member useInlineInitExpression : bool
    default _.useInlineInitExpression = false

    /// Whether the choice-child temp default-init uses the Scala form
    /// `sChildTypeDef + methodNameSuffix + "()"` rather than
    /// `extractDefaultInitValue chType.Kind`.
    /// Default: `false` (use extractDefaultInitValue — C/Ada/Rust/Python).
    /// Scala overrides: `true`.
    /// Replaces DAstInitialize.fs:1253-1257.
    abstract member scalaChoiceInitSuffix : bool
    default _.scalaChoiceInitSuffix = false

    /// Whether byte arrays and similar fixed-size collections must be padded
    /// to their maximum size in generated value literals.
    /// Default: `false` (C/Ada/Scala/Python).
    /// Rust overrides: `true`.
    /// Replaces the `match Rust -> padBytes ... | _ -> ...` blocks in DAstVariables.fs.
    abstract member padByteArraysToMaxSize : bool
    default _.padByteArraysToMaxSize = false

    /// Wrap an optional child value in the language's `Some(...)` constructor.
    /// Default: identity (the value is used as-is).
    /// Rust overrides: `sprintf "Some(%s)" childValue`.
    /// Replaces DAstVariables.fs:417-435.
    abstract member wrapOptionalValueInSome : childValue:string -> string
    default _.wrapOptionalValueInSome childValue = childValue

    /// The expression used for an *absent* optional child in a value literal.
    /// Empty string means "fall back to supportsInitExpressions logic".
    /// Default: `""` (C/Ada/Scala — use initExpressionFnc or None).
    /// Rust overrides: `"None"`.
    /// Replaces DAstVariables.fs:433-438.
    abstract member absentOptionalExpression : string
    default _.absentOptionalExpression = ""

    /// Amber (pointer-prefix) pair for test-case generation, per type kind.
    /// Returns (encAmber, initAmber).
    /// Default: `("", "")` for IA5String (all others use the caller's default).
    /// Rust overrides: `("&", "&")` for IA5String.
    /// Replaces DastTestCaseCreation.fs:31.
    abstract member getAmberForType : Asn1AcnAst.Asn1TypeKind -> string * string
    default _.getAmberForType _ = ("", "")

    /// Format the init statement for a value-assignment or automatic test
    /// case.  The type-kind, module name, and TAS name are provided so
    /// backends can prepend/append language-specific boilerplate.
    /// Default: identity (return initStatement unchanged).
    /// Scala overrides: prepend `"val tc_data = "` for Integer.
    /// Python overrides: append `"tc_data.__class__ = %s" qualifiedAlias` for structured ReferenceType.
    /// Replaces DastTestCaseCreation.fs:85-111, 145-155.
    abstract member formatInitStatementForTestCase : Asn1AcnAst.Asn1TypeKind -> modName:string -> tasName:string -> initStatement:string -> string
    default _.formatInitStatementForTestCase _typeKind _modName _tasName initStatement = initStatement

    /// Convert a character-set string to the target language's validation
    /// literal (e.g. Rust `b"..."` byte-slice vs C `"..."` double-quoted).
    /// The `lm` (LanguageMacros) is NOT available here; the default delegates
    /// to the existing `v.IDQ` and `lm.vars.Print*` functions — but since
    /// those require `lm`, the default returns the raw string and the F# call
    /// site handles the non-Rust path.  Rust overrides to produce `b"..."`.
    /// Replaces DastValidate2.fs:206-231.
    abstract member charSetToValidationLiteral : string -> string
    default _.charSetToValidationLiteral v = v

    /// Produce the (v1_name, v2_name) pair used for choice-child equality
    /// comparison temp variables.
    /// Default: `(childName, childName)` (same name for both sides — C/Ada/Python).
    /// Scala overrides: `(sprintf "%s_%s_tmp" path1 childName, sprintf "%s_%s_tmp" path2 childName)`.
    /// Rust overrides: `(childName + "1", childName + "2")`.
    /// Replaces DAstEqual.fs:92-101.
    abstract member getChoiceChildComparisonNames : Asn1AcnAst.ChChildInfo -> path1:string -> path2:string -> childName:string -> string * string
    default _.getChoiceChildComparisonNames _ _ _ childName = (childName, childName)

    /// Format the default-init expression for a choice child temp variable
    /// in a test-case init function.
    /// Default: `""` — the F# call site uses `extractDefaultInitValue` when
    /// this returns empty (C/Ada/Rust/Python).
    /// Scala overrides: returns `sChildTypeDef + suffix + "()"`.
    /// Replaces DAstInitialize.fs:1253-1257.
    abstract member formatChoiceTestCaseInit : sChildTypeDef:string -> string
    default _.formatChoiceTestCaseInit _ = ""

    /// Format an ACN determinant update statement.
    /// Default: returns `updateStatement` unchanged.
    /// Scala overrides: wraps with `sprintf "val %s = %s.%s\n%s" choicePath checkPath[0] choicePath updateStatement`.
    /// Replaces AcnDependencies.fs:358-364.
    abstract member formatAcnDeterminantUpdate : choicePath:string -> checkPath:string list -> updateStatement:string -> string
    default _.formatAcnDeterminantUpdate _choicePath _checkPath updateStatement = updateStatement

    /// Generate a `Default` impl for an enumerated type.
    /// Default: `""` (no impl — C/Ada/Scala/Python).
    /// Rust overrides: `sprintf "impl Default for %s { fn default() -> Self { %s::%s } }" typeName typeName firstEnumName`.
    /// Replaces DAstTypeDefinition.fs:411-416.
    abstract member generateEnumDefaultImpl : typeName:string -> firstEnumName:string -> string
    default _.generateEnumDefaultImpl _typeName _firstEnumName = ""

    abstract member getRealEncodingSuffix: floatingPointSizeInBytes:BigInteger -> RealClass -> string
    default _.getRealEncodingSuffix _ cls =
        match cls with
        | ASN1SCC_REAL | ASN1SCC_FP64 -> ""
        | ASN1SCC_FP32                -> "_fp32"

    default this.getParamType (t:Asn1AcnAst.Asn1Type) (c:Codec) : CodegenScope =
        this.getParamTypeSuffix t "" c
    
    default this.getParamTypeSuffixForEquals (t:Asn1AcnAst.Asn1Type) (s: string) (c:Codec) =
        this.getParamTypeSuffix t s c
    
    default this.getParamTypeAtc (t:Asn1AcnAst.Asn1Type) (c:Codec) : CodegenScope =
        this.getParamType t c
    default this.requiresHandlingOfEmptySequences = false
    default this.requiresHandlingOfZeroArrays = false
    default this.RtlFuncNames = []
    default this.getQualifiedTypeName (tdr: TypeDefinitionOrReference) (_modName: string) : string =
        this.getLongTypedefName tdr
    default this.getLongTypedefNameBasedOnModule (fe:FE_TypeDefinition) (currentModule: string) = fe.typeName
    default this.getLongTypedefNameFromReferenceToTypeAndCodegenScope (rf: ReferenceToType) (typeDefinition: TypeDefinitionOrReference) (p: CodegenScope) = Some rf.AsString
    default this.longTypedefName2 (td: TypeDefinitionOrReference) (hasModules: bool) (moduleName: string) : string =
        match td with
        | TypeDefinition  td ->
            td.typedefName
        | ReferenceToExistingDefinition ref ->
            match ref.programUnit with
            | Some pu ->
                match hasModules with
                | true   ->
                    match pu with
                    | "" -> ref.typedefName
                    | _ -> pu + "." + ref.typedefName
                | false     -> ref.typedefName
            | None    -> ref.typedefName
    default this.getAlwaysPresentRtlFuncNames args = []
    default this.detectFunctionCalls (sourceCode: string) (functionName: string) = []
    default this.removeFunctionFromHeader (sourceCode: string) (functionName: string) : string =
        sourceCode
    default this.removeFunctionFromBody (sourceCode: string) (functionName: string) : string =
        sourceCode
    default _.getDeferredDetFunctions _ = None
    default _.computeDeferredFallbackValue _ _ = "0"
    default _.wrapDeferredSpecBody body = body
    default _.acnDeferredTempVarName baseName = baseName
    default this.real_annotations = []

    default this.extractEnumClassName (prefix: string) (varName: string) (internalName: string): string = ""
        
    default this.constructFuncName (baseTypeDefinitionName: string) (codecName: string) (methodSuffix: string): string =
        baseTypeDefinitionName + codecName + methodSuffix

    default this.constructReferenceFuncName (baseTypeDefinitionName: string) (codecName: string) (methodSuffix: string): string =
        this.constructFuncName baseTypeDefinitionName codecName methodSuffix

    default this.getFuncNameGeneric (typeDefinition: TypeDefinitionOrReference) (nameSuffix: string): string option  =
        match typeDefinition with
        | ReferenceToExistingDefinition  refEx  -> None
        | TypeDefinition   td                   -> Some (td.typedefName + nameSuffix)

    default this.getFuncNameGeneric2 (typeDefinition: TypeDefinitionOrReference): string option=
        match typeDefinition with
        | ReferenceToExistingDefinition  refEx  -> None
        | TypeDefinition   td                   -> Some td.typedefName

    default this.codecSuffix (codec: CommonTypes.Codec): string =
        match codec with
        | CommonTypes.Encode -> "_Encode"
        | CommonTypes.Decode -> "_Decode"

    default this.allowUnresolvedAcnDependency = false

    default this.getUPerFuncName (r: Asn1AcnAst.AstRoot) (codec: CommonTypes.Codec) (t: Asn1AcnAst.Asn1Type) (td: FE_TypeDefinition): string option =
        match t.id.tasInfo with
        | None -> None
        | Some _ -> Some (td.typeName + this.codecSuffix codec)

    default this.getXerFuncName (codec: CommonTypes.Codec) (typeDefinition: TypeDefinitionOrReference): string option =
        this.getFuncNameGeneric typeDefinition ("_XER" + this.codecSuffix codec)

    default this.getACNFuncName (r: Asn1AcnAst.AstRoot) (codec: CommonTypes.Codec) (t: Asn1AcnAst.Asn1Type) (td: FE_TypeDefinition): string option =
        match t.acnParameters with
        | []    ->
            match t.id.tasInfo with
            | None -> None
            | Some _ -> Some (td.typeName + "_ACN"  + this.codecSuffix codec)
        | _     -> None
     
    default this.adjustTypedefWithFullPath (typeName: string) (moduleName: string) = typeName

    default this.getSeqChildDependingOnChoiceParent (parents: (CodegenScope * Asn1AcnAst.Asn1Type) list) (p: AccessPath) (childName: string) (childTypeIsString: bool) (childIsOptional: bool) =
        this.getSeqChild p childName childTypeIsString childIsOptional
    default this.getChChildForKind (accessPath: AccessPath) (childName: string) (isString: bool) (kind: Asn1TypeKind) (codec: Codec) =
        this.getChChild accessPath childName isString
    default this.adaptAcnFuncBody _ _ f _ _ _ = f
    default this.adaptFuncBodyChoice _ _ _ _ f _ _ = f
    default this.assembleAllProcs _ _ _ arrsLegacyAllProcs = arrsLegacyAllProcs
    default this.choiceChildDecodePath _ _ = None
    default this.generateSequenceAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateIntegerAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateBooleanAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateOctetStringAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateBitStringAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateSequenceOfLikeAuxiliaries _ _ _ _ _ = [], None
    default this.generateOptionalAuxiliaries _ _ soc _ =
        // By default, languages do not have wrapped optional and have an `exist` field: they "attach" the child field themselves
        [], soc.childBody {soc.p with accessPath = soc.p.accessPath.dropLast} soc.existVar
    default this.generateChoiceAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateNullTypeAuxiliaries _ _ _ _ _ _ _ = []
    default this.generateEnumAuxiliaries _ _ _ _ _ _ _ = []

    default this.generatePrecond _ _ _ _ = []
    default this.generatePostcond _ _ _ _ _ = []
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
    default this.joinSelection sel = List.fold (fun str accessor -> $"{str}{this.getAccess2 accessor}") sel.rootId sel.steps
    
    default this.joinSelectionEnum sel = List.fold (fun str accessor -> $"{str}{this.getAccess3 accessor}") sel.rootId sel.steps

    default this.getAccess3(acc: AccessStep) = ""
    
    //most programming languages are case sensitive
    default _.isCaseSensitive = true
    default _.isFilenameCaseSensitive = false
    default _.getBoardNames _ = []
    default _.getBoardDirs  _ = []
    default _.getTypeBasedSuffix _ _ = ""
       
    default _.asSelectionIdentifier sel: string = 
        List.fold (fun str accessor ->
            let acc =
                match accessor with
                | ValueAccess (id, _, _) -> ToC id
                | PointerAccess (id, _, _) -> ToC id
                | ArrayAccess (id, _) -> "arr"
            $"{str}_{acc}") sel.rootId sel.steps
    

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
    encodings: Asn1Encoding list
}

type AccessPath with
    member this.joined (lg: ILangGeneric): string =
        lg.joinSelection this
        
    member this.joinedEnum (lg: ILangGeneric): string =
        lg.getEnumSelectionJoin this
        
    member this.joinedUnchecked (lg: ILangGeneric) (kind: UncheckedAccessKind): string =
        lg.joinSelectionUnchecked this kind
    member this.asIdentifier (lg: ILangGeneric): string =
        lg.asSelectionIdentifier this
