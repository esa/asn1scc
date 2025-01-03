module DAstTypeDefinition

open System
open System.Numerics
open FsUtils
open Asn1AcnAstUtilFunctions
open Asn1AcnAst
open CommonTypes
open DAst
open DAstUtilFunctions
open Asn1Fold
open Language


let getIntegerTypeByClass (lm:LanguageMacros) intClass =
    match intClass with
    | ASN1SCC_Int8   (_)   -> lm.typeDef.Declare_Int8
    | ASN1SCC_Int16  (_)   -> lm.typeDef.Declare_Int16
    | ASN1SCC_Int32  (_)   -> lm.typeDef.Declare_Int32
    | ASN1SCC_Int64  (_)   -> lm.typeDef.Declare_Int64
    | ASN1SCC_Int    (_)   -> lm.typeDef.Declare_Integer
    | ASN1SCC_UInt8  (_)   -> lm.typeDef.Declare_UInt8
    | ASN1SCC_UInt16 (_)   -> lm.typeDef.Declare_UInt16
    | ASN1SCC_UInt32 (_)   -> lm.typeDef.Declare_UInt32
    | ASN1SCC_UInt64 (_)   -> lm.typeDef.Declare_UInt64
    | ASN1SCC_UInt   (_)   -> lm.typeDef.Declare_PosInteger

let getRealTypeByClass (lm:LanguageMacros) realClass =
    match realClass with
    | ASN1SCC_REAL   -> lm.typeDef.Declare_Real
    | ASN1SCC_FP32   -> lm.typeDef.Declare_Real32
    | ASN1SCC_FP64   -> lm.typeDef.Declare_Real64

let createInteger  (lm:LanguageMacros)  (typeDef : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) (intClass : IntegerClass) (uperRange : BigIntegerUperRange)    =
    let declare_Integer = getIntegerTypeByClass lm intClass

    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let defineSubType                   = lm.typeDef.Define_SubType
    let define_SubType_int_range        = lm.typeDef.Define_SubType_int_range

    let getNewRange soInheritParentTypePackage sInheritParentType =
        match uperRange with
        | Concrete(a,b)               ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) (Some b))
        | NegInf (b)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType None (Some b))
        | PosInf (a)  when a=0I       ->  None
        | PosInf (a)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) None)
        | Full                        ->  None

    let td = lm.lg.typeDef typeDef
    match td.kind with
    | PrimitiveNewTypeDefinition              -> //TypeDefinition {TypeDefinition.typedefName=td.typeName; (*programUnitName = Some programUnit;*) typedefBody = (fun () -> typedefBody); baseType= None}
        let baseType = declare_Integer()
        let typedefBody = defineSubType  td.typeName None baseType (getNewRange None baseType) None []
        Some typedefBody
    | PrimitiveNewSubTypeDefinition subDef     ->
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let typedefBody = defineSubType td.typeName otherProgramUnit subDef.typeName (getNewRange otherProgramUnit subDef.typeName) None []
        Some typedefBody
    | PrimitiveReference2RTL                  -> None
    | PrimitiveReference2OtherType            -> None














let internal getChildDefinition (childDefinition:TypeDefinitionOrReference) =
    match childDefinition with
    | TypeDefinition  td -> Some (td.typedefBody ())
    | ReferenceToExistingDefinition ref -> None











////////////////////////////////

let createInteger_u  (lm:LanguageMacros) (id : ReferenceToType) (typeDef : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) (intClass : IntegerClass) (uperRange : BigIntegerUperRange) =
    let aaa = createInteger lm typeDef intClass uperRange 
    let programUnit = ToC id.ModName
    let td = lm.lg.typeDef typeDef
    match td.kind with
    | PrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createReal_u (args:CommandLineSettings) (lm:LanguageMacros) (id: ReferenceToType) (typeDef : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) (acnEncodingClass:RealEncodingClass)   =
    let createReal ()    =
        let getRtlTypeName  = getRealTypeByClass lm (Asn1AcnAstUtilFunctions.getRealEncodingClass args.slim acnEncodingClass)
        let defineSubType = lm.typeDef.Define_SubType

        let td = lm.lg.typeDef typeDef
        let annots = lm.lg.real_annotations
        match td.kind with
        | PrimitiveNewTypeDefinition              ->
            let baseType = getRtlTypeName()
            let typedefBody = defineSubType td.typeName None baseType None None annots
            Some typedefBody
        | PrimitiveNewSubTypeDefinition subDef     ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let typedefBody = defineSubType td.typeName otherProgramUnit subDef.typeName None None annots
            Some typedefBody
        | PrimitiveReference2RTL                  -> None
        | PrimitiveReference2OtherType            -> None
    let aaa = createReal ()
    let programUnit = ToC id.ModName
    let td = lm.lg.typeDef typeDef
    match td.kind with
    | PrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createObjectIdentifier_u (lm:LanguageMacros)   (id:ReferenceToType) (typeDef: Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>)   =
    let createObjectIdentifier ()    =
        let getRtlTypeName  = lm.typeDef.Declare_ObjectIdentifierNoRTL
        let defineSubType = lm.typeDef.Define_SubType
        let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

        let td = lm.lg.typeDef typeDef
        match td.kind with
        | PrimitiveNewTypeDefinition              ->
            let baseType = getRtlTypeName()
            let typedefBody = defineSubType td.typeName rtlModuleName baseType None None []
            Some typedefBody
        | PrimitiveNewSubTypeDefinition subDef     ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let typedefBody = defineSubType td.typeName otherProgramUnit subDef.typeName None None []
            Some typedefBody
        | PrimitiveReference2RTL                  -> None
        | PrimitiveReference2OtherType            -> None
    let aaa = createObjectIdentifier ()
    let programUnit = ToC id.ModName
    let td = lm.lg.typeDef  typeDef
    match td.kind with
    | PrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createTimeType_u (lm:LanguageMacros)   (id:ReferenceToType) (typeDef: Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) (timeClass : TimeTypeClass)   =
    let createTimeType () =

        let getRtlTypeName  =
            match timeClass with
            |Asn1LocalTime                      _ -> lm.typeDef.Declare_Asn1LocalTimeNoRTL
            |Asn1UtcTime                        _ -> lm.typeDef.Declare_Asn1UtcTimeNoRTL
            |Asn1LocalTimeWithTimeZone          _ -> lm.typeDef.Declare_Asn1LocalTimeWithTimeZoneNoRTL
            |Asn1Date                             -> lm.typeDef.Declare_Asn1DateNoRTL
            |Asn1Date_LocalTime                 _ -> lm.typeDef.Declare_Asn1Date_LocalTimeNoRTL
            |Asn1Date_UtcTime                   _ -> lm.typeDef.Declare_Asn1Date_UtcTimeNoRTL
            |Asn1Date_LocalTimeWithTimeZone     _ -> lm.typeDef.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL


        let defineSubType = lm.typeDef.Define_SubType
        let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

        let td = lm.lg.typeDef typeDef
        match td.kind with
        | PrimitiveNewTypeDefinition              ->
            let baseType = getRtlTypeName()
            let typedefBody = defineSubType td.typeName rtlModuleName baseType None None []
            Some typedefBody
        | PrimitiveNewSubTypeDefinition subDef     ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let typedefBody = defineSubType td.typeName otherProgramUnit subDef.typeName None None []
            Some typedefBody
        | PrimitiveReference2RTL                  -> None
        | PrimitiveReference2OtherType            -> None
    let aaa = createTimeType ()
    let programUnit = ToC id.ModName
    let td = lm.lg.typeDef  typeDef
    match td.kind with
    | PrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createBoolean_u (lm:LanguageMacros)   (id:ReferenceToType) (typeDef: Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) =
    let createBoolean () =
        let getRtlTypeName  = lm.typeDef.Declare_BooleanNoRTL
        let defineSubType = lm.typeDef.Define_SubType
        let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))
        let td = lm.lg.typeDef typeDef
        match td.kind with
        | PrimitiveNewTypeDefinition              ->
            let baseType = getRtlTypeName()
            let typedefBody = defineSubType td.typeName rtlModuleName baseType None None []
            Some typedefBody
        | PrimitiveNewSubTypeDefinition subDef     ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let typedefBody = defineSubType td.typeName otherProgramUnit subDef.typeName None None []
            Some typedefBody
        | PrimitiveReference2RTL                  -> None
        | PrimitiveReference2OtherType            -> None
    let aaa = createBoolean ()
    let programUnit = ToC id.ModName
    let td = lm.lg.typeDef  typeDef
    match td.kind with
    | PrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createNull_u  (lm:LanguageMacros)   (id:ReferenceToType) (typeDef: Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>)  =
    let createNull () =
        let getRtlTypeName  = lm.typeDef.Declare_NullNoRTL
        let defineSubType = lm.typeDef.Define_SubType
        let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

        let td = lm.lg.typeDef typeDef
        match td.kind with
        | PrimitiveNewTypeDefinition              ->
            let baseType = getRtlTypeName()
            let typedefBody = defineSubType td.typeName rtlModuleName baseType None None []
            Some typedefBody
        | PrimitiveNewSubTypeDefinition subDef     ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let typedefBody = defineSubType td.typeName otherProgramUnit subDef.typeName None None []
            Some typedefBody
        | PrimitiveReference2RTL                  -> None
        | PrimitiveReference2OtherType            -> None

    let aaa = createNull ()
    let programUnit = id.ModName
    let td = lm.lg.typeDef  typeDef
    match td.kind with
    | PrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | PrimitiveReference2RTL                ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createOctetString_u (lm:LanguageMacros)   (id:ReferenceToType) (typeDef : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) (minSize : SIZE) (maxSize : SIZE)  =
    let createOctetString () =
        let td = lm.lg.getSizeableTypeDefinition typeDef
        let define_new_octet_string        = lm.typeDef.Define_new_octet_string
        let define_subType_octet_string    = lm.typeDef.Define_subType_octet_string
        match td.kind with
        | NonPrimitiveNewTypeDefinition ->
            let invariants = lm.lg.generateOctetStringInvariants minSize maxSize
            let completeDefinition = define_new_octet_string td minSize.uper maxSize.uper (minSize.uper = maxSize.uper) invariants
            Some completeDefinition
        | NonPrimitiveNewSubTypeDefinition subDef ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let completeDefinition = define_subType_octet_string td subDef otherProgramUnit (minSize.uper = maxSize.uper)
            Some completeDefinition
        | NonPrimitiveReference2OtherType            -> None
    let aaa = createOctetString ()
    let programUnit = ToC id.ModName
    let td = lm.lg.getSizeableTypeDefinition  typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createBitString_u (lm:LanguageMacros)   (id:ReferenceToType) (typeDef : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) (minSize : SIZE) (maxSize : SIZE) (namedBitList: NamedBit1 list)  =
    let createBitString ()  =
        let td = lm.lg.getSizeableTypeDefinition typeDef
        let define_new_bit_string   = lm.typeDef.Define_new_bit_string
        let define_named_bit        = lm.typeDef.Define_new_bit_string_named_bit

        let define_subType_bit_string    = lm.typeDef.Define_subType_bit_string
        match td.kind with
        | NonPrimitiveNewTypeDefinition              ->
            let nblist =
                namedBitList |>
                List.filter(fun nb -> nb.resolvedValue < 64I) |>
                List.map(fun nb  ->
                    let hexValue =
                        let aa = int nb.resolvedValue
                        let hexVal = ((uint64 1) <<< aa)
                        hexVal.ToString("X")
                    let sComment = sprintf "(1 << %A)" nb.resolvedValue
                    define_named_bit td (ToC (nb.Name.Value.ToUpper())) hexValue sComment
                )
            let invariants = lm.lg.generateBitStringInvariants minSize maxSize 
            let completeDefinition = define_new_bit_string td minSize.uper maxSize.uper (minSize.uper = maxSize.uper) (BigInteger (getBitStringMaxOctets maxSize)) nblist invariants
            Some completeDefinition
        | NonPrimitiveNewSubTypeDefinition subDef     ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let completeDefinition = define_subType_bit_string td subDef otherProgramUnit (minSize.uper = maxSize.uper)
            Some completeDefinition
        | NonPrimitiveReference2OtherType            -> None
    let aaa = createBitString ()
    let programUnit = ToC id.ModName
    let td = lm.lg.getSizeableTypeDefinition  typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createString_u (lm:LanguageMacros)  (id:ReferenceToType) (typeDef: Map<ProgrammingLanguage, FE_StringTypeDefinition>) (uperCharSet : char array) (minSize : SIZE) (maxSize : SIZE)   =
    let createString ()  =
        let arrnAlphaChars = (uperCharSet |> Array.map(fun c -> (BigInteger (int c))))
        let define_new_ia5string        = lm.typeDef.Define_new_ia5string
        let define_subType_ia5string    = lm.typeDef.Define_subType_ia5string

        let td = lm.lg.getStrTypeDefinition typeDef
        match td.kind with
        | NonPrimitiveNewTypeDefinition              ->
            let completeDefinition = define_new_ia5string td minSize.uper maxSize.uper (maxSize.uper + 1I) arrnAlphaChars
            Some completeDefinition
        | NonPrimitiveNewSubTypeDefinition subDef     ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let completeDefinition = define_subType_ia5string td subDef otherProgramUnit
            Some completeDefinition
        | NonPrimitiveReference2OtherType            -> None
    let aaa = createString ()
    let programUnit = ToC id.ModName
    let td = lm.lg.getStrTypeDefinition typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createEnumerated_u (args:CommandLineSettings) (lm:LanguageMacros)  (id:ReferenceToType) (typeDef : Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) (items : NamedItem list) (validItems : NamedItem list)   =
    let createEnumerated () =
        let td = lm.lg.getEnumTypeDefinition typeDef
        let define_new_enumerated_item        = lm.typeDef.Define_new_enumerated_item
        let define_new_enumerated_item_macro  = lm.typeDef.Define_new_enumerated_item_macro
        let define_new_enumerated        = lm.typeDef.Define_new_enumerated
        let define_subType_enumerated    = lm.typeDef.Define_subType_enumerated
        let define_new_enumerated_private = lm.typeDef.Define_new_enumerated_private
        let define_subType_enumerated_private = lm.typeDef.Define_subType_enumerated_private
        let orderedItems            = items |> List.sortBy(fun i -> i.definitionValue)
        let arrsEnumNames           = orderedItems |> List.map( fun i -> lm.lg.getNamedItemBackendName None i)
        let arrsEnumNamesAndValues  = orderedItems |> List.map(fun i -> define_new_enumerated_item td (lm.lg.getNamedItemBackendName None i) i.definitionValue)
        let macros                  = orderedItems |> List.map( fun i -> define_new_enumerated_item_macro td (ToC i.Name.Value) (lm.lg.getNamedItemBackendName None i) )
        let nIndexMax               = BigInteger ((Seq.length items)-1)
        let arrsValidEnumNames      = validItems |> List.sortBy(fun i -> i.definitionValue) |> List.map( fun i -> lm.lg.getNamedItemBackendName None i)

        match td.kind with
        | NonPrimitiveNewTypeDefinition              ->
            let completeDefinition = define_new_enumerated td arrsEnumNames arrsEnumNamesAndValues nIndexMax macros
            let privateDefinition =
                match args.isEnumEfficientEnabled items.Length with
                | false -> None
                | true  ->
                    let ret = define_new_enumerated_private td arrsValidEnumNames arrsEnumNames
                    match System.String.IsNullOrWhiteSpace ret with
                    | true  -> None
                    | false -> Some ret
            Some (completeDefinition, privateDefinition)
        | NonPrimitiveNewSubTypeDefinition subDef     ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let completeDefinition = define_subType_enumerated td subDef otherProgramUnit
            let privateDefinition =
                match args.isEnumEfficientEnabled items.Length with
                | false -> None
                | true  ->
                    let ret = define_subType_enumerated_private td subDef arrsValidEnumNames arrsEnumNames
                    match System.String.IsNullOrWhiteSpace ret with
                    | true  -> None
                    | false -> Some ret
            Some (completeDefinition, privateDefinition)
        | NonPrimitiveReference2OtherType            -> None


    let programUnit = ToC id.ModName
    let td = lm.lg.getEnumTypeDefinition   typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        let (aaa, priv) =
            match createEnumerated ()  with
            | Some (a, b) -> Some a, b
            | None -> None, None
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=priv; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let (aaa, priv) =
            match createEnumerated ()  with
            | Some (a, b) -> Some a, b
            | None -> None, None
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=priv; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createSequenceOf_u (lm:LanguageMacros)  (id:ReferenceToType) (typeDef : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (minSize : SIZE) (maxSize : SIZE) (acnEncodingClass : Asn1AcnAst.SizeableAcnEncodingClass) (acnAlignment : AcnGenericTypes.AcnAlignment option) (child : Asn1AcnAst.Asn1Type)   =
    let childTypeDefinitionOrReference = lm.lg.definitionOrRef child.typeDefinitionOrReference
    let createSequenceOf ()  =
        let define_new_sequence_of        = lm.typeDef.Define_new_sequence_of
        let define_subType_sequence_of    = lm.typeDef.Define_subType_sequence_of
        let td = lm.lg.getSizeableTypeDefinition typeDef

        match td.kind with
        | NonPrimitiveNewTypeDefinition ->
            let invariants = [] //lm.lg.generateSequenceOfInvariants minSize maxSize 
            let sizeClsDefinitions, sizeObjDefinitions = [],[]//lm.lg.generateSequenceOfSizeDefinitions typeDef  acnMinSizeInBits  acnMaxSizeInBits  maxSize  acnEncodingClass  acnAlignment  child
            let completeDefinition = define_new_sequence_of td minSize.uper maxSize.uper (minSize.uper = maxSize.uper) (childTypeDefinitionOrReference.longTypedefName2 lm.lg.hasModules) (getChildDefinition childTypeDefinitionOrReference) sizeClsDefinitions sizeObjDefinitions invariants
            let privateDefinition =
                match childTypeDefinitionOrReference with
                | TypeDefinition  td -> td.privateTypeDefinition
                | ReferenceToExistingDefinition ref -> None
            Some (completeDefinition, privateDefinition)
        | NonPrimitiveNewSubTypeDefinition subDef ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let completeDefinition = define_subType_sequence_of td subDef otherProgramUnit (minSize.uper = maxSize.uper) (getChildDefinition childTypeDefinitionOrReference)
            let privateDefinition =
                match childTypeDefinitionOrReference with
                | TypeDefinition  td -> td.privateTypeDefinition
                | ReferenceToExistingDefinition ref -> None
            Some (completeDefinition, privateDefinition)
        | NonPrimitiveReference2OtherType -> None
    
    let aaa, privateDef =
        match createSequenceOf ()  with
        | Some (a, b) -> Some a, b
        | None -> None, None
    let programUnit = ToC id.ModName
    let td = lm.lg.getSizeableTypeDefinition typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition = privateDef; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition = privateDef; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createSequence_u (args:CommandLineSettings) (lm:LanguageMacros) (typeDef:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>)  (id: ReferenceToType) (acnProperties : AcnGenericTypes.SequenceAcnProperties) (acnMaxSizeInBits : BigInteger)   (children:Asn1AcnAst.SeqChildInfo list)  =
    let createSequence  (allchildren: Asn1AcnAst.SeqChildInfo list)  =
        let define_new_sequence             = lm.typeDef.Define_new_sequence
        let define_new_sequence_child       = lm.typeDef.Define_new_sequence_child
        let define_new_sequence_child_bit   = lm.typeDef.Define_new_sequence_child_bit
        let define_subType_sequence         = lm.typeDef.Define_subType_sequence

        let define_new_sequence_save_pos_child         = lm.typeDef.Define_new_sequence_save_pos_child

        let children = allchildren |> List.choose (fun c -> match c with Asn1AcnAst.Asn1Child z -> Some z | _ -> None)
        let optionalChildren = children |> List.choose(fun c -> match c.Optionality with Some _ -> Some c | None -> None)
        let childrenCompleteDefinitions = children |> List.choose (fun c -> getChildDefinition (lm.lg.definitionOrRef c.Type.typeDefinitionOrReference))
        let td = lm.lg.getSequenceTypeDefinition typeDef
        let arrsNullFieldsSavePos =
            let getBackendName ci =
                match ci with
                | Asn1AcnAst.AcnChild z    -> z.c_name
                | Asn1AcnAst.Asn1Child z   -> lm.lg.getAsn1ChildBackendName0 z

            match acnProperties.postEncodingFunction.IsNone && acnProperties.preDecodingFunction.IsNone with
            | true -> []
            | false  ->
                allchildren |>
                List.choose (fun c -> if c.savePosition then Some (getBackendName c) else None ) |>
                List.map(fun childName -> define_new_sequence_save_pos_child td childName acnMaxSizeInBits)


        let arrsChildren =
            match args.handleEmptySequences && lm.lg.requiresHandlingOfEmptySequences && children.IsEmpty with
            | true  -> [define_new_sequence_child "dummy" "int" false] //define a dummy child for empty sequences
            | false -> children |> List.map (fun o -> define_new_sequence_child (lm.lg.getAsn1ChildBackendName0 o) ( (lm.lg.definitionOrRef o.Type.typeDefinitionOrReference).longTypedefName2 lm.lg.hasModules) o.Optionality.IsSome)

        let childrenPrivatePart =
            children |>
            List.choose (fun o ->
                match lm.lg.definitionOrRef o.Type.typeDefinitionOrReference with
                | TypeDefinition td -> td.privateTypeDefinition
                | ReferenceToExistingDefinition ref -> None)

        let arrsOptionalChildren  = optionalChildren |> List.map(fun c -> define_new_sequence_child_bit (lm.lg.getAsn1ChildBackendName0 c))

        match td.kind with
        | NonPrimitiveNewTypeDefinition ->
            //SOS: invariants and size definitions are current not generated for sequences
            //we need to implement this in the future
            let invariants = [] //lm.lg.generateSequenceInvariants t o allchildren
            let sizeDefinitions = [] //lm.lg.generateSequenceSizeDefinitions t o allchildren
            let completeDefinition = define_new_sequence td arrsChildren arrsOptionalChildren childrenCompleteDefinitions arrsNullFieldsSavePos sizeDefinitions invariants
            let privateDef =
                match childrenPrivatePart with
                | [] -> None
                | _  -> Some (childrenPrivatePart |> Seq.StrJoin "\n")
            Some (completeDefinition, privateDef)
        | NonPrimitiveNewSubTypeDefinition subDef ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            //SOS: extraDefs are not currently generated for sequences
            //we need to implement this in the future
            let extraDefs = []//lm.lg.generateSequenceSubtypeDefinitions subDef.typeName t o children
            let completeDefinition = define_subType_sequence td subDef otherProgramUnit arrsOptionalChildren extraDefs
            Some (completeDefinition, None)
        | NonPrimitiveReference2OtherType -> None
    let aaa, private_part =
        match createSequence children  with
        | Some (a, b) -> Some a, b
        | None -> None, None
    let programUnit = ToC id.ModName
    let td = lm.lg.getSequenceTypeDefinition   typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=private_part;  baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=private_part; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createChoice_u (args:CommandLineSettings) (typeIdsSet : Map<String,int>) (lm:LanguageMacros) (typeDef:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>)  (id: ReferenceToType) (acnProperties : AcnGenericTypes.ChoiceAcnProperties) (acnMaxSizeInBits : BigInteger)   (children:Asn1AcnAst.ChChildInfo list)  =
    let createChoice (children:Asn1AcnAst.ChChildInfo list)  =
        let define_new_choice             = lm.typeDef.Define_new_choice
        let define_new_choice_child       = lm.typeDef.Define_new_choice_child
        let define_subType_choice         = lm.typeDef.Define_subType_choice


        let td = lm.lg.getChoiceTypeDefinition typeDef
        let childrenCompleteDefinitions = children |> List.choose (fun c -> getChildDefinition (lm.lg.definitionOrRef c.Type.typeDefinitionOrReference))
        let arrsPresent = children |> List.map(fun c -> lm.lg.presentWhenName0 None c)
        let arrsChildren = children |> List.map (fun o -> define_new_choice_child (lm.lg.getAsn1ChChildBackendName0 o) ((lm.lg.definitionOrRef o.Type.typeDefinitionOrReference).longTypedefName2 lm.lg.hasModules) (lm.lg.presentWhenName0 None o))
        let arrsCombined = List.map2 (fun x y -> x + "(" + y + ")") arrsPresent arrsChildren
        let nIndexMax = BigInteger ((Seq.length children)-1)

        let privatePart =
            let childPrivateParts = children |>
                                    List.choose(fun o ->
                                        match lm.lg.definitionOrRef o.Type.typeDefinitionOrReference with
                                        | TypeDefinition td -> td.privateTypeDefinition
                                        | ReferenceToExistingDefinition ref -> None)
            match childPrivateParts with
            | [] -> None
            | _  -> Some (childPrivateParts |> Seq.StrJoin "\n")


        match td.kind with
        | NonPrimitiveNewTypeDefinition ->
            //SOS: invariants and size definitions are currently not generated for choices
            let sizeDefinitions = []//lm.lg.generateChoiceSizeDefinitions t o children
            let completeDefinition = define_new_choice td (lm.lg.choiceIDForNone typeIdsSet id) (lm.lg.presentWhenName0 None children.Head) arrsChildren arrsPresent arrsCombined nIndexMax childrenCompleteDefinitions sizeDefinitions
            Some (completeDefinition, privatePart)
        | NonPrimitiveNewSubTypeDefinition subDef ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let completeDefinition = define_subType_choice td subDef otherProgramUnit
            Some (completeDefinition, None)
        | NonPrimitiveReference2OtherType -> None
    let aaa, private_part =
        match createChoice children   with
        | Some (a, b) -> Some a, b
        | None -> None, None
    let programUnit = ToC id.ModName
    let td = lm.lg.getChoiceTypeDefinition  typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=private_part; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=private_part; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


