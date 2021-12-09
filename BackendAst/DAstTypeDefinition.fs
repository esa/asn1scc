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
open OutDirectories



    
let createInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Integer)   (us:State) =
    let declare_IntegerNoRTL            = lm.typeDef.Declare_IntegerNoRTL
    let declare_PosIntegerNoRTL         = lm.typeDef.Declare_PosIntegerNoRTL
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let defineSubType l                 = lm.typeDef.Define_SubType 
    let define_SubType_int_range        = lm.typeDef.Define_SubType_int_range

    let getNewRange soInheritParentTypePackage sInheritParentType = 
        match o.uperRange with
        | Concrete(a,b)               ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) (Some b))
        | NegInf (b)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType None (Some b))
        | PosInf (a)  when a=0I       ->  None
        | PosInf (a)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) None)
        | Full                        ->  None

    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> //TypeDefinition {TypeDefinition.typedefName=td.typeName; (*programUnitName = Some programUnit;*) typedefBody = (fun () -> typedefBody); baseType= None}
        let baseType = if o.isUnsigned then declare_PosIntegerNoRTL() else declare_IntegerNoRTL()
        let typedefBody = defineSubType l td.typeName rtlModuleName baseType (getNewRange rtlModuleName baseType) None
        Some typedefBody
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let rec hasSameSignWithBase (t:Asn1AcnAst.Asn1Type) =
            match t.inheritInfo with
            | None  -> false
            | Some inhInf ->
                let baseMod = r.GetModuleByName inhInf.modName.AsLoc
                let baseTas = baseMod.GetTypeAssignmentByName inhInf.tasName.AsLoc r
                match  baseTas.Type.Kind with
                | Asn1AcnAst.Integer bo -> bo.isUnsigned = o.isUnsigned
                | Asn1AcnAst.ReferenceType br -> hasSameSignWithBase br.resolvedType
                | _                           -> false
        match hasSameSignWithBase t with
        | true  ->
            let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
            let typedefBody = defineSubType l td.typeName otherProgramUnit subDef.typeName (getNewRange otherProgramUnit subDef.typeName) None
            Some typedefBody
        | false     ->
            let baseType = if o.isUnsigned then declare_PosIntegerNoRTL() else declare_IntegerNoRTL()
            let typedefBody = defineSubType l td.typeName rtlModuleName baseType (getNewRange rtlModuleName baseType) None
            Some typedefBody
    | PrimitiveReference2RTL                  -> None
    | PrimitiveReference2OtherType            -> None


let createBoolean (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Boolean)   (us:State) =
    let getRtlTypeName  = lm.typeDef.Declare_BooleanNoRTL 
    let defineSubType l = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))
    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        let baseType = getRtlTypeName()
        let typedefBody = defineSubType l td.typeName rtlModuleName baseType None None
        Some typedefBody
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let typedefBody = defineSubType l td.typeName otherProgramUnit subDef.typeName None None
        Some typedefBody
    | PrimitiveReference2RTL                  -> None
    | PrimitiveReference2OtherType            -> None

let createReal (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Real)   (us:State) =
    let getRtlTypeName  = lm.typeDef.Declare_RealNoRTL 
    let defineSubType l = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        let baseType = getRtlTypeName()
        let typedefBody = defineSubType l td.typeName rtlModuleName baseType None None
        Some typedefBody
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let typedefBody = defineSubType l td.typeName otherProgramUnit subDef.typeName None None
        Some typedefBody
    | PrimitiveReference2RTL                  -> None
    | PrimitiveReference2OtherType            -> None

let createObjectIdentifier (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.ObjectIdentifier)   (us:State) =
    let getRtlTypeName  = lm.typeDef.Declare_ObjectIdentifierNoRTL 
    let defineSubType l = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        let baseType = getRtlTypeName()
        let typedefBody = defineSubType l td.typeName rtlModuleName baseType None None
        Some typedefBody
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let typedefBody = defineSubType l td.typeName otherProgramUnit subDef.typeName None None
        Some typedefBody
    | PrimitiveReference2RTL                  -> None
    | PrimitiveReference2OtherType            -> None


let createTimeType (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.TimeType)   (us:State) =
    
    let getRtlTypeName  = 
        match o.timeClass with
        |Asn1LocalTime                      _ -> lm.typeDef.Declare_Asn1LocalTimeNoRTL                  
        |Asn1UtcTime                        _ -> lm.typeDef.Declare_Asn1UtcTimeNoRTL                    
        |Asn1LocalTimeWithTimeZone          _ -> lm.typeDef.Declare_Asn1LocalTimeWithTimeZoneNoRTL      
        |Asn1Date                             -> lm.typeDef.Declare_Asn1DateNoRTL                     
        |Asn1Date_LocalTime                 _ -> lm.typeDef.Declare_Asn1Date_LocalTimeNoRTL             
        |Asn1Date_UtcTime                   _ -> lm.typeDef.Declare_Asn1Date_UtcTimeNoRTL               
        |Asn1Date_LocalTimeWithTimeZone     _ -> lm.typeDef.Declare_Asn1Date_LocalTimeWithTimeZoneNoRTL 
        

    let defineSubType l = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        let baseType = getRtlTypeName()
        let typedefBody = defineSubType l td.typeName rtlModuleName baseType None None
        Some typedefBody
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let typedefBody = defineSubType l td.typeName otherProgramUnit subDef.typeName None None
        Some typedefBody
    | PrimitiveReference2RTL                  -> None
    | PrimitiveReference2OtherType            -> None


let createNull (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.NullType)   (us:State) =
    let getRtlTypeName  = lm.typeDef.Declare_NullNoRTL 
    let defineSubType l = lm.typeDef.Define_SubType
    let rtlModuleName                   = if lm.typeDef.rtlModuleName().IsEmptyOrNull then None else (Some (lm.typeDef.rtlModuleName ()))

    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        let baseType = getRtlTypeName()
        let typedefBody = defineSubType l td.typeName rtlModuleName baseType None None
        Some typedefBody
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let typedefBody = defineSubType l td.typeName otherProgramUnit subDef.typeName None None
        Some typedefBody
    | PrimitiveReference2RTL                  -> None
    | PrimitiveReference2OtherType            -> None

let createString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let arrnAlphaChars = (o.uperCharSet |> Array.map(fun c -> (BigInteger (int c))))
    let define_new_ia5string        = lm.typeDef.Define_new_ia5string
    let define_subType_ia5string    = lm.typeDef.Define_subType_ia5string

    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_ia5string td (o.minSize.uper) (o.maxSize.uper) ((o.maxSize.uper + 1I)) arrnAlphaChars
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_ia5string td subDef otherProgramUnit
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None

let createOctetString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  (us:State) =
    let td = o.typeDef.[l]
    let define_new_octet_string        = lm.typeDef.Define_new_octet_string
    let define_subType_octet_string    = lm.typeDef.Define_subType_octet_string
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_octet_string td (o.minSize.uper) (o.maxSize.uper) (o.minSize.uper = o.maxSize.uper)
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_octet_string td subDef otherProgramUnit (o.minSize.uper = o.maxSize.uper)
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None



let createBitString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)  (us:State) =
    let td = o.typeDef.[l]
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
        let completeDefintion = define_new_bit_string td (o.minSize.uper) (o.maxSize.uper) (o.minSize.uper = o.maxSize.uper) (BigInteger o.MaxOctets) nblist
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_bit_string td subDef otherProgramUnit (o.minSize.uper = o.maxSize.uper)
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None



let createEnumerated (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated)  (us:State) =
    let td = o.typeDef.[l]
    let define_new_enumerated_item        = lm.typeDef.Define_new_enumerated_item
    let define_new_enumerated_item_macro  = lm.typeDef.Define_new_enumerated_item_macro
    let define_new_enumerated        = lm.typeDef.Define_new_enumerated
    let define_subType_enumerated    = lm.typeDef.Define_subType_enumerated
    let orderedItems = o.items |> List.sortBy(fun i -> i.definitionValue)
    let arrsEnumNames = orderedItems |> List.map( fun i -> i.getBackendName None l)
    let arrsEnumNamesAndValues = orderedItems |> List.map( fun i -> define_new_enumerated_item (i.getBackendName None l) i.definitionValue)
    let macros = orderedItems |> List.map( fun i -> define_new_enumerated_item_macro td (ToC i.Name.Value) (i.getBackendName None l) )
    let nIndexMax = BigInteger ((Seq.length o.items)-1)

    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_enumerated td arrsEnumNames arrsEnumNamesAndValues nIndexMax macros
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_enumerated td subDef otherProgramUnit 
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None

let internal getChildDefinition (childDefinition:TypeDefintionOrReference) =
    match childDefinition with
    | TypeDefinition  td -> Some (td.typedefBody ())
    | ReferenceToExistingDefinition ref -> None


let createSequenceOf (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  (childDefinition:TypeDefintionOrReference) (us:State) =
    let define_new_sequence_of        = lm.typeDef.Define_new_sequence_of
    let define_subType_sequence_of    = lm.typeDef.Define_subType_sequence_of
    let td = o.typeDef.[l]

    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_sequence_of td (o.minSize.uper) (o.maxSize.uper) (o.minSize.uper = o.maxSize.uper) (childDefinition.longTypedefName l) (getChildDefinition childDefinition)
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_sequence_of td subDef otherProgramUnit (o.minSize.uper = o.maxSize.uper)
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None



let createSequence (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence)  (allchildren:SeqChildInfo list) (us:State) =
    let define_new_sequence             = lm.typeDef.Define_new_sequence
    let define_new_sequence_child       = lm.typeDef.Define_new_sequence_child
    let define_new_sequence_child_bit   = lm.typeDef.Define_new_sequence_child_bit
    let define_subType_sequence         = lm.typeDef.Define_subType_sequence

    let define_new_sequence_save_pos_child         = lm.typeDef.Define_new_sequence_save_pos_child
    
    let children = allchildren |> List.choose (fun c -> match c with Asn1Child z -> Some z | _ -> None)
    let optionalChildren = children |> List.choose(fun c -> match c.Optionality with Some _ -> Some c | None -> None)
    let optChildNames  = optionalChildren |> List.map(fun c -> c.getBackendName l)
    let childldrenCompleteDefintions = children |> List.choose (fun c -> getChildDefinition c.Type.typeDefintionOrReference)
    let td = o.typeDef.[l]
    let arrsNullFieldsSavePos =
        match o.acnProperties.postEncodingFunction.IsNone && o.acnProperties.preDecodingFunction.IsNone with
        | true -> []
        | false  -> 
            allchildren |> 
            List.choose (fun c -> if c.savePosition then Some (c.getBackendName l) else None ) |>
            List.map(fun childName -> define_new_sequence_save_pos_child td childName o.acnMaxSizeInBits)


    let arrsChildren = children |> List.map (fun o -> define_new_sequence_child (o.getBackendName l) (o.Type.typeDefintionOrReference.longTypedefName l))
    let arrsOptionalChildren  = optionalChildren |> List.map(fun c -> define_new_sequence_child_bit (c.getBackendName l))


    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_sequence td arrsChildren arrsOptionalChildren childldrenCompleteDefintions arrsNullFieldsSavePos
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_sequence td subDef otherProgramUnit  arrsOptionalChildren
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None




let createChoice (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice)  (children:ChChildInfo list) (us:State) =
    let define_new_choice             = lm.typeDef.Define_new_choice
    let define_new_choice_child       = lm.typeDef.Define_new_choice_child
    let define_subType_choice         = lm.typeDef.Define_subType_choice


    let td = o.typeDef.[l]
    let childldrenCompleteDefintions = children |> List.choose (fun c -> getChildDefinition c.chType.typeDefintionOrReference)
    let arrsPresent = children |> List.map(fun c -> c.presentWhenName None l)
    let arrsChildren = children |> List.map (fun o -> define_new_choice_child (o.getBackendName l)  (o.chType.typeDefintionOrReference.longTypedefName l) (o.presentWhenName None l))
    let nIndexMax = BigInteger ((Seq.length children)-1)

    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_choice td (choiceIDForNone us.typeIdsSet l t.id) (children.Head.presentWhenName None l) arrsChildren arrsPresent nIndexMax childldrenCompleteDefintions
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_choice td subDef otherProgramUnit  
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None


////////////////////////////////


let createInteger_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer)  (us:State) =
    let aaa = createInteger r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createReal_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real)  (us:State) =
    let aaa = createReal r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createObjectIdentifier_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier)  (us:State) =
    let aaa = createObjectIdentifier r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createTimeType_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType)  (us:State) =
    let aaa = createTimeType r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createBoolean_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean)  (us:State) =
    let aaa = createBoolean r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | PrimitiveReference2RTL                  ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createNull_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType)  (us:State) =
    let aaa = createNull r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | PrimitiveReference2RTL                ->     
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createOctetString_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  (us:State) =
    let aaa = createOctetString r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | NonPrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createBitString_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)  (us:State) =
    let aaa = createBitString r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | NonPrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}
    

let createString_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let aaa = createString r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | NonPrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createEnumerated_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated)  (us:State) =
    let aaa = createEnumerated r l lm t o us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | NonPrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createSequenceOf_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  (childDefinition:TypeDefintionOrReference) (us:State) =
    let aaa = createSequenceOf r l lm t o  childDefinition us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | NonPrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createSequence_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence)  (children:SeqChildInfo list) (us:State) =
    let aaa = createSequence r l lm t o  children us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | NonPrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}

let createChoice_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice)  (children:ChChildInfo list) (us:State) =
    let aaa = createChoice r l lm t o  children us
    let programUnit = ToC t.id.ModName
    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=None}
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let baseType = {ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = (fun () -> aaa.Value); baseType=Some baseType}
    | NonPrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}


let createReferenceType_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType)  (baseType:Asn1Type ) (us:State) =
    match o.encodingOptions with
    | None    -> baseType.typeDefintionOrReference
    | Some _  -> baseType.typeDefintionOrReference

    
