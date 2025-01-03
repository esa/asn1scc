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


let createInteger (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Integer)   (us:State) =
    let declare_Integer = getIntegerTypeByClass lm o.intClass

    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let defineSubType                   = lm.typeDef.Define_SubType
    let define_SubType_int_range        = lm.typeDef.Define_SubType_int_range

    let getNewRange soInheritParentTypePackage sInheritParentType =
        match o.uperRange with
        | Concrete(a,b)               ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) (Some b))
        | NegInf (b)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType None (Some b))
        | PosInf (a)  when a=0I       ->  None
        | PosInf (a)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) None)
        | Full                        ->  None

    let td = lm.lg.typeDef o.typeDef
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


let createBoolean (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Boolean)   (us:State) =
    let getRtlTypeName  = lm.typeDef.Declare_BooleanNoRTL
    let defineSubType = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))
    let td = lm.lg.typeDef o.typeDef
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

let createReal (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Real)   (us:State) =
    //let getRtlTypeName  = lm.typeDef.Declare_RealNoRTL
    let getRtlTypeName  = getRealTypeByClass lm (o.getClass r.args)
    let defineSubType = lm.typeDef.Define_SubType
    let rtlModuleName = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let td = lm.lg.typeDef o.typeDef
    let annots =
        match ST.lang with
        | Scala -> ["extern"]
        | _ -> []
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

let createObjectIdentifier (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.ObjectIdentifier)   (us:State) =
    let getRtlTypeName  = lm.typeDef.Declare_ObjectIdentifierNoRTL
    let defineSubType = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let td = lm.lg.typeDef o.typeDef
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


let createTimeType (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.TimeType)   (us:State) =

    let getRtlTypeName  =
        match o.timeClass with
        |Asn1LocalTime                      _ -> lm.typeDef.Declare_Asn1LocalTimeNoRTL
        |Asn1UtcTime                        _ -> lm.typeDef.Declare_Asn1UtcTimeNoRTL
        |Asn1LocalTimeWithTimeZone          _ -> lm.typeDef.Declare_Asn1LocalTimeWithTimeZoneNoRTL
        |Asn1Date                             -> lm.typeDef.Declare_Asn1DateNoRTL
        |Asn1Date_LocalTime                 _ -> lm.typeDef.Declare_Asn1Date_LocalTimeNoRTL
        |Asn1Date_UtcTime                   _ -> lm.typeDef.Declare_Asn1Date_UtcTimeNoRTL
        |Asn1Date_LocalTimeWithTimeZone     _ -> lm.typeDef.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL


    let defineSubType = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let td = lm.lg.typeDef o.typeDef
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


let createNull (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.NullType)   (us:State) =
    let getRtlTypeName  = lm.typeDef.Declare_NullNoRTL
    let defineSubType = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let td = lm.lg.typeDef o.typeDef
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

let createString (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let arrnAlphaChars = (o.uperCharSet |> Array.map(fun c -> (BigInteger (int c))))
    let define_new_ia5string        = lm.typeDef.Define_new_ia5string
    let define_subType_ia5string    = lm.typeDef.Define_subType_ia5string

    let td = lm.lg.getStrTypeDefinition o.typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        let completeDefinition = define_new_ia5string td o.minSize.uper o.maxSize.uper (o.maxSize.uper + 1I) arrnAlphaChars
        Some completeDefinition
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefinition = define_subType_ia5string td subDef otherProgramUnit
        Some completeDefinition
    | NonPrimitiveReference2OtherType            -> None

let createOctetString (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  (us:State) =
    let td = lm.lg.getSizeableTypeDefinition o.typeDef
    let define_new_octet_string        = lm.typeDef.Define_new_octet_string
    let define_subType_octet_string    = lm.typeDef.Define_subType_octet_string
    match td.kind with
    | NonPrimitiveNewTypeDefinition ->
        let invariants = lm.lg.generateOctetStringInvariants t o
        let completeDefinition = define_new_octet_string td o.minSize.uper o.maxSize.uper (o.minSize.uper = o.maxSize.uper) invariants
        Some completeDefinition
    | NonPrimitiveNewSubTypeDefinition subDef ->
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefinition = define_subType_octet_string td subDef otherProgramUnit (o.minSize.uper = o.maxSize.uper)
        Some completeDefinition
    | NonPrimitiveReference2OtherType            -> None



let createBitString (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)  (us:State) =
    let td = lm.lg.getSizeableTypeDefinition o.typeDef
    let define_new_bit_string   = lm.typeDef.Define_new_bit_string
    let define_named_bit        = lm.typeDef.Define_new_bit_string_named_bit

    let define_subType_bit_string    = lm.typeDef.Define_subType_bit_string
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        let nblist =
            o.namedBitList |>
            List.filter(fun nb -> nb.resolvedValue < 64I) |>
            List.map(fun nb  ->
                let hexValue =
                    let aa = int nb.resolvedValue
                    let hexVal = ((uint64 1) <<< aa)
                    hexVal.ToString("X")
                let sComment = sprintf "(1 << %A)" nb.resolvedValue
                define_named_bit td (ToC (nb.Name.Value.ToUpper())) hexValue sComment
            )
        let invariants = lm.lg.generateBitStringInvariants t o
        let completeDefinition = define_new_bit_string td o.minSize.uper o.maxSize.uper (o.minSize.uper = o.maxSize.uper) (BigInteger o.MaxOctets) nblist invariants
        Some completeDefinition
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefinition = define_subType_bit_string td subDef otherProgramUnit (o.minSize.uper = o.maxSize.uper)
        Some completeDefinition
    | NonPrimitiveReference2OtherType            -> None



let createEnumerated (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated)  (us:State) =
    let td = lm.lg.getEnumTypeDefinition o.typeDef
    let define_new_enumerated_item        = lm.typeDef.Define_new_enumerated_item
    let define_new_enumerated_item_macro  = lm.typeDef.Define_new_enumerated_item_macro
    let define_new_enumerated        = lm.typeDef.Define_new_enumerated
    let define_subType_enumerated    = lm.typeDef.Define_subType_enumerated
    let define_new_enumerated_private = lm.typeDef.Define_new_enumerated_private
    let define_subType_enumerated_private = lm.typeDef.Define_subType_enumerated_private
    let orderedItems            = TL "CR_EN_01" (fun () -> o.items |> List.sortBy(fun i -> i.definitionValue))
    let arrsEnumNames           = TL "CR_EN_02" (fun () -> orderedItems |> List.map( fun i -> lm.lg.getNamedItemBackendName None i))
    let arrsEnumNamesAndValues  = TL "CR_EN_03" (fun () -> orderedItems |> List.map(fun i -> define_new_enumerated_item td (lm.lg.getNamedItemBackendName None i) i.definitionValue))
    let macros                  = TL "CR_EN_04" (fun () -> orderedItems |> List.map( fun i -> define_new_enumerated_item_macro td (ToC i.Name.Value) (lm.lg.getNamedItemBackendName None i) ))
    let nIndexMax               = TL "CR_EN_05" (fun () -> BigInteger ((Seq.length o.items)-1))
    let arrsValidEnumNames      = TL "CR_EN_06" (fun () -> o.validItems |> List.sortBy(fun i -> i.definitionValue) |> List.map( fun i -> lm.lg.getNamedItemBackendName None i))

    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TL "CR_EN_07" (fun () -> 
        let completeDefinition = define_new_enumerated td arrsEnumNames arrsEnumNamesAndValues nIndexMax macros
        let privateDefinition =
            match r.args.isEnumEfficientEnabled o.items.Length with
            | false -> None
            | true  ->
                let ret = define_new_enumerated_private td arrsValidEnumNames arrsEnumNames
                match System.String.IsNullOrWhiteSpace ret with
                | true  -> None
                | false -> Some ret
        Some (completeDefinition, privateDefinition))
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        TL "CR_EN_08" (fun () -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefinition = define_subType_enumerated td subDef otherProgramUnit
        let privateDefinition =
            match r.args.isEnumEfficientEnabled o.items.Length with
            | false -> None
            | true  ->
                let ret = define_subType_enumerated_private td subDef arrsValidEnumNames arrsEnumNames
                match System.String.IsNullOrWhiteSpace ret with
                | true  -> None
                | false -> Some ret
        Some (completeDefinition, privateDefinition))
    | NonPrimitiveReference2OtherType            -> None

let internal getChildDefinition (childDefinition:TypeDefinitionOrReference) =
    match childDefinition with
    | TypeDefinition  td -> Some (td.typedefBody ())
    | ReferenceToExistingDefinition ref -> None


let createSequenceOf (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (t: Asn1AcnAst.Asn1Type) (o: Asn1AcnAst.SequenceOf) (childType: DAst.Asn1Type) (us: State) =
    let define_new_sequence_of        = lm.typeDef.Define_new_sequence_of
    let define_subType_sequence_of    = lm.typeDef.Define_subType_sequence_of
    let td = lm.lg.getSizeableTypeDefinition o.typeDef

    match td.kind with
    | NonPrimitiveNewTypeDefinition ->
        let invariants = lm.lg.generateSequenceOfInvariants t o childType.Kind
        let sizeClsDefinitions, sizeObjDefinitions = lm.lg.generateSequenceOfSizeDefinitions t o childType
        let completeDefinition = define_new_sequence_of td o.minSize.uper o.maxSize.uper (o.minSize.uper = o.maxSize.uper) (childType.typeDefinitionOrReference.longTypedefName2 lm.lg.hasModules) (getChildDefinition childType.typeDefinitionOrReference) sizeClsDefinitions sizeObjDefinitions invariants
        let privateDefinition =
            match childType.typeDefinitionOrReference with
            | TypeDefinition  td -> td.privateTypeDefinition
            | ReferenceToExistingDefinition ref -> None
        Some (completeDefinition, privateDefinition)
    | NonPrimitiveNewSubTypeDefinition subDef ->
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefinition = define_subType_sequence_of td subDef otherProgramUnit (o.minSize.uper = o.maxSize.uper) (getChildDefinition childType.typeDefinitionOrReference)
        let privateDefinition =
            match childType.typeDefinitionOrReference with
            | TypeDefinition  td -> td.privateTypeDefinition
            | ReferenceToExistingDefinition ref -> None
        Some (completeDefinition, privateDefinition)
    | NonPrimitiveReference2OtherType -> None



let createSequence (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (allchildren: SeqChildInfo list) (us:State) =
    let define_new_sequence             = lm.typeDef.Define_new_sequence
    let define_new_sequence_child       = lm.typeDef.Define_new_sequence_child
    let define_new_sequence_child_bit   = lm.typeDef.Define_new_sequence_child_bit
    let define_subType_sequence         = lm.typeDef.Define_subType_sequence

    let define_new_sequence_save_pos_child         = lm.typeDef.Define_new_sequence_save_pos_child

    let children = allchildren |> List.choose (fun c -> match c with Asn1Child z -> Some z | _ -> None)
    let optionalChildren = children |> List.choose(fun c -> match c.Optionality with Some _ -> Some c | None -> None)
    let optChildNames  = optionalChildren |> List.map(fun c -> lm.lg.getAsn1ChildBackendName c)
    let childrenCompleteDefinitions = children |> List.choose (fun c -> getChildDefinition c.Type.typeDefinitionOrReference)
    let td = lm.lg.getSequenceTypeDefinition o.typeDef
    let arrsNullFieldsSavePos =
        let getBackendName ci =
            match ci with
            | AcnChild z    -> z.c_name
            | Asn1Child z   -> lm.lg.getAsn1ChildBackendName z

        match o.acnProperties.postEncodingFunction.IsNone && o.acnProperties.preDecodingFunction.IsNone with
        | true -> []
        | false  ->
            allchildren |>
            List.choose (fun c -> if c.savePosition then Some (getBackendName c) else None ) |>
            List.map(fun childName -> define_new_sequence_save_pos_child td childName o.acnMaxSizeInBits)


    let arrsChildren =
        match r.args.handleEmptySequences && lm.lg.requiresHandlingOfEmptySequences && children.IsEmpty with
        | true  -> [define_new_sequence_child "dummy" "int" false] //define a dummy child for empty sequences
        | false -> children |> List.map (fun o -> define_new_sequence_child (lm.lg.getAsn1ChildBackendName o) (o.Type.typeDefinitionOrReference.longTypedefName2 lm.lg.hasModules) o.Optionality.IsSome)

    let childrenPrivatePart =
        children |>
        List.choose (fun o ->
            match o.Type.typeDefinitionOrReference with
            | TypeDefinition td -> td.privateTypeDefinition
            | ReferenceToExistingDefinition ref -> None)

    let arrsOptionalChildren  = optionalChildren |> List.map(fun c -> define_new_sequence_child_bit (lm.lg.getAsn1ChildBackendName c))

    match td.kind with
    | NonPrimitiveNewTypeDefinition ->
        let invariants = lm.lg.generateSequenceInvariants t o allchildren
        let sizeDefinitions = lm.lg.generateSequenceSizeDefinitions t o allchildren
        let completeDefinition = define_new_sequence td arrsChildren arrsOptionalChildren childrenCompleteDefinitions arrsNullFieldsSavePos sizeDefinitions invariants
        let privateDef =
            match childrenPrivatePart with
            | [] -> None
            | _  -> Some (childrenPrivatePart |> Seq.StrJoin "\n")
        Some (completeDefinition, privateDef)
    | NonPrimitiveNewSubTypeDefinition subDef ->
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let extraDefs = lm.lg.generateSequenceSubtypeDefinitions subDef.typeName t o children
        let completeDefinition = define_subType_sequence td subDef otherProgramUnit arrsOptionalChildren extraDefs
        Some (completeDefinition, None)
    | NonPrimitiveReference2OtherType -> None




let createChoice (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (children:ChChildInfo list) (us:State) =
    let define_new_choice             = lm.typeDef.Define_new_choice
    let define_new_choice_child       = lm.typeDef.Define_new_choice_child
    let define_subType_choice         = lm.typeDef.Define_subType_choice


    let td = lm.lg.getChoiceTypeDefinition o.typeDef
    let childrenCompleteDefinitions = children |> List.choose (fun c -> getChildDefinition c.chType.typeDefinitionOrReference)
    let arrsPresent = children |> List.map(fun c -> lm.lg.presentWhenName None c)
    let arrsChildren = children |> List.map (fun o -> define_new_choice_child (lm.lg.getAsn1ChChildBackendName o) (o.chType.typeDefinitionOrReference.longTypedefName2 lm.lg.hasModules) (lm.lg.presentWhenName None o))
    let arrsCombined = List.map2 (fun x y -> x + "(" + y + ")") arrsPresent arrsChildren
    let nIndexMax = BigInteger ((Seq.length children)-1)

    let privatePart =
        let childPrivateParts = children |>
                                List.choose(fun o ->
                                    match o.chType.typeDefinitionOrReference with
                                    | TypeDefinition td -> td.privateTypeDefinition
                                    | ReferenceToExistingDefinition ref -> None)
        match childPrivateParts with
        | [] -> None
        | _  -> Some (childPrivateParts |> Seq.StrJoin "\n")


    match td.kind with
    | NonPrimitiveNewTypeDefinition ->
        let sizeDefinitions = lm.lg.generateChoiceSizeDefinitions t o children
        let completeDefinition = define_new_choice td (lm.lg.choiceIDForNone us.typeIdsSet t.id) (lm.lg.presentWhenName None children.Head) arrsChildren arrsPresent arrsCombined nIndexMax childrenCompleteDefinitions sizeDefinitions
        Some (completeDefinition, privatePart)
    | NonPrimitiveNewSubTypeDefinition subDef ->
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefinition = define_subType_choice td subDef otherProgramUnit
        Some (completeDefinition, None)
    | NonPrimitiveReference2OtherType -> None


////////////////////////////////


let createInteger_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (us:State) =
    let aaa = createInteger r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.typeDef o.typeDef
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

let createReal_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real)  (us:State) =
    let aaa = createReal r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.typeDef o.typeDef
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

let createObjectIdentifier_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier)  (us:State) =
    let aaa = createObjectIdentifier r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.typeDef  o.typeDef
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


let createTimeType_u (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType)  (us:State) =
    let aaa = createTimeType r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.typeDef  o.typeDef
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


let createBoolean_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean)  (us:State) =
    let aaa = createBoolean r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.typeDef  o.typeDef
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

let createNull_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType)  (us:State) =
    let aaa = createNull r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.typeDef  o.typeDef
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

let createOctetString_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  (us:State) =
    let aaa = createOctetString r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.getSizeableTypeDefinition  o.typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createBitString_u (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)  (us:State) =
    let aaa = createBitString r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.getSizeableTypeDefinition  o.typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createString_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let aaa = createString r lm t o us
    let programUnit = ToC t.id.ModName
    let td = lm.lg.getStrTypeDefinition o.typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=None; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createEnumerated_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated)  (us:State) =
    let programUnit = ToC t.id.ModName
    let td = lm.lg.getEnumTypeDefinition   o.typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TL "CR_EN_U_01" (fun () ->
        let (aaa, priv) =
            match createEnumerated r lm t o us with
            | Some (a, b) -> Some a, b
            | None -> None, None
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=priv; baseType=None})
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        TL "CR_EN_U_02" (fun () ->
        let (aaa, priv) =
            match createEnumerated r lm t o us with
            | Some (a, b) -> Some a, b
            | None -> None, None
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=priv; baseType=Some baseType})
    | NonPrimitiveReference2OtherType            ->
        TL "CR_EN_U_03" (fun () ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false})


let createSequenceOf_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (childType: DAst.Asn1Type) (us:State) =
    let aaa, privateDef =
        match createSequenceOf r lm t o childType us with
        | Some (a, b) -> Some a, b
        | None -> None, None
    let programUnit = ToC t.id.ModName
    let td = lm.lg.getSizeableTypeDefinition o.typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition = privateDef; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition = privateDef; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createSequence_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence)  (children:SeqChildInfo list) (us:State) =
    let aaa, private_part =
        match createSequence r lm t o  children us with
        | Some (a, b) -> Some a, b
        | None -> None, None
    let programUnit = ToC t.id.ModName
    let td = lm.lg.getSequenceTypeDefinition   o.typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=private_part;  baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=private_part; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createChoice_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice)  (children:ChChildInfo list) (us:State) =
    let aaa, private_part =
        match createChoice r lm t o  children us with
        | Some (a, b) -> Some a, b
        | None -> None, None
    let programUnit = ToC t.id.ModName
    let td = lm.lg.getChoiceTypeDefinition  o.typeDef
    match td.kind with
    | NonPrimitiveNewTypeDefinition              ->
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=private_part; baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     ->
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); privateTypeDefinition=private_part; baseType=Some baseType}
    | NonPrimitiveReference2OtherType            ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createReferenceType_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType)  (baseType:Asn1Type ) (us:State) =
    match o.encodingOptions with
    | None    -> baseType.typeDefinitionOrReference
    | Some _  -> baseType.typeDefinitionOrReference
