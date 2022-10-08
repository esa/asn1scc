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
}


[<AbstractClass>]
type ILangGeneric () =
    abstract member ArrayStartIndex : int
    abstract member getPointer      : FuncParamType -> string;
    abstract member getValue        : FuncParamType -> string;
    abstract member getAcces        : FuncParamType -> string;
    abstract member getStar         : FuncParamType -> string;
    abstract member getAmber        : FuncParamType -> string;
    abstract member toPointer       : FuncParamType -> FuncParamType;
    abstract member getArrayItem    : FuncParamType -> (string) -> (bool) -> FuncParamType;
    abstract member intValueToSting : BigInteger -> Asn1AcnAst.IntegerClass -> string;
    abstract member getNamedItemBackendName  :TypeDefintionOrReference option -> Asn1AcnAst.NamedItem -> string
    abstract member decodeEmptySeq  : string -> string option
    abstract member decode_nullType : string -> string option
    abstract member castExpression  : string -> string -> string

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
    //abstract member getEnmLongTypedefName : FE_EnumeratedTypeDefinition -> string -> FE_EnumeratedTypeDefinition;

    abstract member ArrayAccess     : string -> string;

    abstract member presentWhenName : TypeDefintionOrReference option -> ChChildInfo -> string;
    abstract member getParamTypeSuffix : Asn1AcnAst.Asn1Type -> string -> Codec -> CallerScope;
    abstract member getParamValue   : Asn1AcnAst.Asn1Type -> FuncParamType -> Codec -> string

    abstract member getParamType    : Asn1AcnAst.Asn1Type -> Codec -> CallerScope;
    abstract member rtlModuleName   : string
    abstract member hasModules      : bool
    abstract member supportsStaticVerification      : bool
    abstract member AssignOperator  : string
    abstract member TrueLiteral     : string
    abstract member emtyStatement   : string
    abstract member bitStreamName   : string
    abstract member  unaryNotOperator :string
    abstract member  modOp            :string
    abstract member  eqOp             :string
    abstract member  neqOp            :string
    abstract member  andOp            :string
    abstract member  orOp             :string

    abstract member  bitStringValueToByteArray:  BitStringValue -> byte[]

    abstract member toHex : int -> string
    abstract member uper : Uper_parts;
    abstract member acn  : Acn_parts
    abstract member init : Initialize_parts
    abstract member atc  : Atc_parts
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
}


let getAcces_c  (fpt:FuncParamType) =
    match fpt with
    | VALUE x        -> "."
    | POINTER x      -> "->"
    | FIXARRAY x     -> ""

#if false
let createBitStringFunction_funcBody_c handleFragmentation (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    let ii = id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" (id.SeqeuenceOfLevel + 1)
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))

    let funcBodyContent, localVariables = 
        let nStringLength =
            match isFixedSize,  codec with
            | true , _    -> []
            | false, Encode -> []
            | false, Decode -> [Asn1SIntLocalVariable ("nCount", None)]

        match minSize with
        | _ when maxSize < 65536I && isFixedSize   -> uper_c.bitString_FixSize p.arg.p (getAcces_c p.arg) (minSize) errCode.errCodeName codec , nStringLength
        | _ when maxSize < 65536I && (not isFixedSize)  -> uper_c.bitString_VarSize p.arg.p (getAcces_c p.arg) (minSize) (maxSize) errCode.errCodeName nSizeInBits codec, nStringLength
        | _                                                -> 
            handleFragmentation p codec errCode ii (uperMaxSizeInBits) minSize maxSize "" 1I true false
    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
#endif


type LangGeneric_c() =
    inherit ILangGeneric()
        override _.ArrayStartIndex = 0

        override _.intValueToSting (i:BigInteger) (intClass:Asn1AcnAst.IntegerClass) =
            match intClass with
            | Asn1AcnAst.ASN1SCC_Int8     _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int16    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int32    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int64    _ ->  sprintf "%sL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int      _ ->  sprintf "%sL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt8    _ ->  sprintf "%sU" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt16   _ ->  sprintf "%sU" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt32   _ ->  sprintf "%sU" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt64   _ ->  sprintf "%sUL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt     _ ->  sprintf "%sUL" (i.ToString())

        override _.getPointer  (fpt:FuncParamType) =
            match fpt with
            |VALUE x        -> sprintf "(&(%s))" x
            |POINTER x      -> x
            |FIXARRAY x     -> x

        override this.getValue  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> x
            | POINTER x      -> sprintf "(*(%s))" x
            | FIXARRAY x     -> x

        override this.getAcces  (fpt:FuncParamType) = getAcces_c fpt

        override this.ArrayAccess idx = "[" + idx + "]"


        override this.getStar  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> ""
            | POINTER x      -> "*"
            | FIXARRAY x     -> ""
        override this.getArrayItem (fpt:FuncParamType) (idx:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s%sarr[%s]" fpt.p (this.getAcces fpt) idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.getNamedItemBackendName (defOrRef:TypeDefintionOrReference option) (nm:Asn1AcnAst.NamedItem) = 
            ToC nm.c_name

        override this.decodeEmptySeq _ = None
        override this.decode_nullType _ = None

        override this.Length exp sAcc =
            isvalid_c.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[C]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[C]
        override this.getEnmTypeDefintion (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[C]
        override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[C]
        override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[C]
        override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[C]
        override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[C]

        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._c_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._c_name
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._c_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._c_name


        override this.rtlModuleName  = ""
        override this.AssignOperator = "="
        override this.TrueLiteral = "TRUE"
        override this.emtyStatement = ""
        override this.bitStreamName = "BitStream"
        override this.unaryNotOperator    = "!"  
        override this.modOp               = "%"  
        override this.eqOp                = "==" 
        override this.neqOp               = "!=" 
        override this.andOp               = "&&" 
        override this.orOp                = "||" 

        override this.castExpression (sExp:string) (sCastType:string) = sprintf "(%s)(%s)" sCastType sExp
            


        override this.hasModules = false
        override this.supportsStaticVerification = false
        
        override this.getSeqChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s%s%s" fpt.p (this.getAcces fpt) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.getChChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) : FuncParamType =
            let newPath = sprintf "%s%su.%s" fpt.p (this.getAcces fpt) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
            
        override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) =  
            let prefix = ToC (id.AcnAbsPath.Tail.StrJoin("_").Replace("#","elem"))
            match typeIdsSet.TryFind prefix with
            | None  -> prefix + "_NONE" 
            | Some a when a = 1 -> prefix + "_NONE" 
            | Some a            -> ToC (id.AcnAbsPath.StrJoin("_").Replace("#","elem")) + "_NONE" 

        override this.presentWhenName (defOrRef:TypeDefintionOrReference option) (ch:ChChildInfo) : string =
            (ToC ch._present_when_name_private) + "_PRESENT"
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CallerScope =
            match c with
            | Encode  ->
                match t.Kind with
                | Asn1AcnAst.Integer         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.Real            _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.IA5String       _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.NumericString   _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.OctetString     _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.NullType        _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.BitString       _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Boolean         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.Enumerated      _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.SequenceOf      _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Sequence        _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Choice          _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ObjectIdentifier _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.TimeType _         -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ReferenceType r -> 
                    this.getParamTypeSuffix r.resolvedType suf c
            | Decode  ->
                match t.Kind with
                | Asn1AcnAst.Integer            _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Real               _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.IA5String          _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.NumericString      _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.OctetString        _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.NullType           _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.BitString          _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Boolean            _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Enumerated         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.SequenceOf         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Sequence           _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Choice             _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ObjectIdentifier _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.TimeType _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ReferenceType r -> this.getParamTypeSuffix r.resolvedType suf c
        
        override this.getParamValue  (t:Asn1AcnAst.Asn1Type) (p:FuncParamType)  (c:Codec) =
            match c with
            | Encode  ->
                match t.Kind with
                | Asn1AcnAst.Integer      _ -> this.getPointer p
                | Asn1AcnAst.Real         _ -> this.getPointer p
                | Asn1AcnAst.IA5String    _ -> this.getValue   p //FIXARRAY "val"
                | Asn1AcnAst.NumericString _-> this.getValue   p// FIXARRAY "val"
                | Asn1AcnAst.OctetString  _ -> this.getPointer p
                | Asn1AcnAst.NullType     _ -> this.getPointer p
                | Asn1AcnAst.BitString    _ -> this.getPointer p
                | Asn1AcnAst.Boolean      _ -> this.getPointer p
                | Asn1AcnAst.Enumerated   _ -> this.getPointer p
                | Asn1AcnAst.SequenceOf   _ -> this.getPointer p
                | Asn1AcnAst.Sequence     _ -> this.getPointer p
                | Asn1AcnAst.Choice       _ -> this.getPointer p
                | Asn1AcnAst.ObjectIdentifier _ -> this.getPointer p
                | Asn1AcnAst.TimeType _ -> this.getPointer p
                | Asn1AcnAst.ReferenceType r -> this.getParamValue r.resolvedType p  c
            | Decode  ->
                match t.Kind with
                | Asn1AcnAst.IA5String    _  -> this.getValue p //FIXARRAY "val"
                | Asn1AcnAst.NumericString _ -> this.getValue p// FIXARRAY "val"
                | Asn1AcnAst.ReferenceType r -> this.getParamValue r.resolvedType p  c
                | _                          -> this.getPointer p

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "int i%d;" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "int i%d=%d;" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "int %s;" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "int %s=%d;" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "asn1SccSint %s;" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "asn1SccSint %s=%d;" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "asn1SccUint %s;" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "asn1SccUint %s=%d;" name iv
            | FlagLocalVariable (name,None)             -> sprintf "flag %s;" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "flag %s=%d;" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "flag %s;" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "flag %s=%s;" name (if iv then "TRUE" else "FALSE")
            | AcnInsertedChild(name, vartype)           -> sprintf "%s %s;" vartype name
            | GenericLocalVariable lv                   ->
                sprintf "%s%s %s%s;" (if lv.isStatic then "static " else "") lv.varType lv.name (if lv.arrSize.IsNone then "" else "["+lv.arrSize.Value+"]")

            
        override this.getLongTypedefName (tdr:TypeDefintionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref -> ref.typedefName
                    
        //override this.getEnmLongTypedefName (td:FE_EnumeratedTypeDefinition) _ = td;

        override this.toHex n = sprintf "0x%x" n

        override this.bitStringValueToByteArray (v : BitStringValue) = FsUtils.bitStringValueToByteArray (StringLoc.ByValue v)

        override this.uper =
            {
                Uper_parts.createLv = (fun name -> Asn1SIntLocalVariable(name,None))
                requires_sBlockIndex  = true
                requires_sBLJ = false
                requires_charIndex = false
                requires_IA5String_i = true
                count_var            = Asn1SIntLocalVariable ("nCount", None)
                requires_presenceBit = true
                catd                 = false
                //createBitStringFunction = createBitStringFunction_funcBody_c
                seqof_lv              =
                  (fun id minSize maxSize -> [SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)])
            }
        override this.acn = 
            {
                Acn_parts.null_valIsUnReferenced = true
                checkBitPatternPresentResult = true
                getAcnDepSizeDeterminantLocVars = 
                    fun  sReqBytesForUperEncoding ->
                        [
                            GenericLocalVariable {GenericLocalVariable.name = "arr"; varType = "byte"; arrSize = Some sReqBytesForUperEncoding; isStatic = true; initExp = None}
                            GenericLocalVariable {GenericLocalVariable.name = "bitStrm"; varType = "BitStream"; arrSize = None; isStatic = false; initExp = None}
                        ]
                choice_handle_always_absent_child = false
                choice_requires_tmp_decoding = false
            }
        override this.init = 
            {
                Initialize_parts.zeroIA5String_localVars    = fun _ -> []
                choiceComponentTempInit                     = false
            }
        override this.atc =
            {
                Atc_parts.uperPrefix = ""
                acnPrefix            = "ACN_"
                xerPrefix            = "XER_"
            }


(****** Ada Implementation ******)

let getAcces_a  (_:FuncParamType) = "."
#if false
let createBitStringFunction_funcBody_Ada handleFragmentation (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    let ii = id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" (id.SeqeuenceOfLevel + 1)

    let typeDefinitionName =
        match typeDefinition with
        | TypeDefinition  td ->
            td.typedefName
        | ReferenceToExistingDefinition ref ->
            match ref.programUnit with
            | Some pu -> pu + "." + ref.typedefName
            | None    -> ref.typedefName

    let funcBodyContent, localVariables = 
        let nStringLength = 
            match isFixedSize with  
            | true  -> [] 
            | false -> 
                match codec with
                | Encode    -> []
                | Decode    -> [IntegerLocalVariable ("nStringLength", None)]
        let iVar = SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)

        let nBits = 1I
        let internalItem = uper_a.InternalItem_bit_str p.arg.p i  errCode.errCodeName codec 
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
        match minSize with
        | _ when maxSize < 65536I && isFixedSize  -> uper_a.octect_FixedSize p.arg.p typeDefinitionName i internalItem (minSize) nBits nBits 0I codec, iVar::nStringLength 
        | _ when maxSize < 65536I && (not isFixedSize) -> uper_a.octect_VarSize p.arg.p "."  typeDefinitionName i internalItem ( minSize) (maxSize) nSizeInBits nBits nBits 0I errCode.errCodeName codec , iVar::nStringLength
        | _                                                -> 
            let funcBodyContent, fragmentationLvars = handleFragmentation p codec errCode ii (uperMaxSizeInBits) minSize maxSize internalItem nBits true false
            let fragmentationLvars = fragmentationLvars |> List.addIf (not isFixedSize) (iVar)
            (funcBodyContent,fragmentationLvars)

    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
#endif







type LangGeneric_a() =
    inherit ILangGeneric()
        override _.ArrayStartIndex = 1
        override this.rtlModuleName  = "adaasn1rtl."
        override this.AssignOperator = ":="
        override this.TrueLiteral = "True"
        override this.hasModules = true
        override this.supportsStaticVerification = true 
        override this.emtyStatement = "null;"
        override this.bitStreamName = "adaasn1rtl.encoding.BitStreamPtr"
        override this.unaryNotOperator    = "not"
        override this.modOp               = "mod"
        override this.eqOp                = "="
        override this.neqOp               = "<>"
        override this.andOp               = "and"
        override this.orOp                = "or"

        override this.castExpression (sExp:string) (sCastType:string) = sprintf "%s(%s)" sCastType sExp


        override _.intValueToSting (i:BigInteger) _ = i.ToString()

        override _.getPointer  (fpt:FuncParamType) =
            match fpt with
            | VALUE x      -> x
            | POINTER x    -> x
            | FIXARRAY x   -> x

        override this.getValue  (fpt:FuncParamType) =
            match fpt with
            | VALUE x      -> x
            | POINTER x    -> x
            | FIXARRAY x   -> x
        override this.getAcces  (fpt:FuncParamType) = getAcces_a fpt

        override this.getStar  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> ""
            | POINTER x      -> ""
            | FIXARRAY x     -> ""
        override this.getArrayItem (fpt:FuncParamType) (idx:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s.Data(%s)" fpt.p idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.ArrayAccess idx = "(" + idx + ")"

        override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) =  
            let prefix = ToC (id.AcnAbsPath.Tail.StrJoin("_").Replace("#","elem"))
            prefix + "_NONE"


        override this.getNamedItemBackendName (defOrRef:TypeDefintionOrReference option) (nm:Asn1AcnAst.NamedItem) = 
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + nm.ada_name
            | Some (TypeDefinition td) when td.baseType.IsSome && td.baseType.Value.programUnit.IsSome  -> td.baseType.Value.programUnit.Value + "." + nm.ada_name
            | _       -> ToC nm.ada_name
        override this.Length exp sAcc =
            isvalid_a.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[Ada]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[Ada]
        override this.getEnmTypeDefintion (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[Ada]
        override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[Ada]
        override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[Ada]
        override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[Ada]
        override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[Ada]


        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._ada_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._ada_name
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._ada_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._ada_name

        override this.getSeqChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s.%s" fpt.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.getChChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) : FuncParamType =
            let newPath = sprintf "%s.%s" fpt.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)


        override this.presentWhenName (defOrRef:TypeDefintionOrReference option) (ch:ChChildInfo) : string =
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + ((ToC ch._present_when_name_private) + "_PRESENT")
            | _       -> (ToC ch._present_when_name_private) + "_PRESENT"
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CallerScope =
            {CallerScope.modName = t.id.ModName; arg= VALUE ("val" + suf) }

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "i%d:Integer;" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "i%d:Integer:=%d;" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "%s:Integer;" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "%s:Integer:=%d;" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1Int;" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1Int:=%d;" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1UInt;" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1UInt:=%d;" name iv
            | FlagLocalVariable (name,None)             -> sprintf "%s:adaasn1rtl.BIT;" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "%s:adaasn1rtl.BIT:=%d;" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "%s:Boolean;" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "%s:Boolean:=%s;" name (if iv then "True" else "False")
            | AcnInsertedChild(name, vartype)         -> sprintf "%s:%s;" name vartype
            | GenericLocalVariable lv                   ->
                match lv.initExp with
                | Some initExp  -> sprintf "%s : %s := %s;" lv.name lv.varType  initExp
                | None          -> sprintf "%s : %s;" lv.name lv.varType  

        override this.getLongTypedefName (tdr:TypeDefintionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref ->
                match ref.programUnit with
                | Some pu -> pu + "." + ref.typedefName
                | None    -> ref.typedefName

        override this.decodeEmptySeq p = Some (uper_a.decode_empty_sequence_emptySeq p)
        override this.decode_nullType p = Some (uper_a.decode_nullType p)



        override this.getParamValue  (t:Asn1AcnAst.Asn1Type) (p:FuncParamType)  (c:Codec) =
            p.p

        override this.toHex n = sprintf "16#%x#" n

        override this.bitStringValueToByteArray (v : BitStringValue) = 
            v.ToCharArray() |> Array.map(fun c -> if c = '0' then 0uy else 1uy)

        override this.uper =
            {
                Uper_parts.createLv = (fun name -> IntegerLocalVariable(name,None))
                requires_sBlockIndex  = false
                requires_sBLJ = true
                requires_charIndex = true
                requires_IA5String_i = false
                count_var            = IntegerLocalVariable ("nStringLength", None)
                requires_presenceBit = false
                catd                 = true
                //createBitStringFunction = createBitStringFunction_funcBody_Ada
                seqof_lv              =
                  (fun id minSize maxSize -> 
                    if maxSize >= 65536I && maxSize = minSize then 
                        []
                    else
                        [SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)])
            }
        override this.acn = 
            {
                Acn_parts.null_valIsUnReferenced = false
                checkBitPatternPresentResult = false
                getAcnDepSizeDeterminantLocVars = 
                    fun  sReqBytesForUperEncoding ->
                        [
                            GenericLocalVariable {GenericLocalVariable.name = "tmpBs"; varType = "adaasn1rtl.encoding.BitStream"; arrSize = None; isStatic = false;initExp = Some (sprintf "adaasn1rtl.encoding.BitStream_init(%s)" sReqBytesForUperEncoding)}
                        ]
                choice_handle_always_absent_child = true
                choice_requires_tmp_decoding = true
          }
        override this.init = 
            {
                Initialize_parts.zeroIA5String_localVars    = fun ii -> [SequenceOfIndex (ii, None)]
                choiceComponentTempInit                     = true
            }

        override this.atc =
            {
                Atc_parts.uperPrefix = "UPER_"
                acnPrefix            = "ACN_"
                xerPrefix            = "XER_"
            }
