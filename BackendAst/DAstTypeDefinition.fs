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



    
let createInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Integer)   (us:State) =
    let declare_IntegerNoRTL            = match l with C -> header_c.Declare_Integer                    | Ada -> header_a.Declare_IntegerNoRTL
    let declare_PosIntegerNoRTL         = match l with C -> header_c.Declare_PosInteger                 | Ada -> header_a.Declare_PosIntegerNoRTL
    let rtlModuleName  = match l with C -> None                                          | Ada -> Some (header_a.rtlModuleName())

    let defineSubType l = match l with C -> header_c.Define_SubType | Ada -> header_a.Define_SubType
    let define_SubType_int_range        = match l with C -> (fun _ _ _ _  -> "")                        | Ada -> header_a.Define_SubType_int_range

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
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let typedefBody = defineSubType l td.typeName otherProgramUnit subDef.typeName (getNewRange otherProgramUnit subDef.typeName) None
        Some typedefBody
    | PrimitiveReference2RTL                  -> None
    | PrimitiveReference2OtherType            -> None


let createBoolean (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Boolean)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_Boolean  | Ada -> header_a.Declare_BOOLEANNoRTL 
    let defineSubType l = match l with C -> header_c.Define_SubType | Ada -> header_a.Define_SubType
    let rtlModuleName  = match l with C -> None                                          | Ada -> Some (header_a.rtlModuleName())
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

let createReal (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Real)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_Real  | Ada -> header_a.Declare_REALNoRTL 
    let defineSubType l = match l with C -> header_c.Define_SubType | Ada -> header_a.Define_SubType
    let rtlModuleName  = match l with C -> None                                          | Ada -> Some (header_a.rtlModuleName())
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


let createNull (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.NullType)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_NullType  | Ada -> header_a.Declare_NULLNoRTL 
    let defineSubType l = match l with C -> header_c.Define_SubType | Ada -> header_a.Define_SubType
    let rtlModuleName  = match l with C -> None                                          | Ada -> Some (header_a.rtlModuleName())
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

let createString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let arrnAlphaChars = (o.uperCharSet |> Array.map(fun c -> (BigInteger (int c))))
    let define_new_ia5string        = match l with C -> header_c.Define_new_ia5string | Ada -> header_a.Define_new_ia5string
    let define_subType_ia5string    = match l with C -> header_c.Define_subType_ia5string | Ada -> header_a.Define_subType_ia5string

    let td = o.typeDef.[l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_ia5string td (o.minSize) (o.maxSize) ((o.maxSize + 1I)) arrnAlphaChars
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_ia5string td subDef otherProgramUnit
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None

let createOctetString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  (us:State) =
    let td = o.typeDef.[l]
    let define_new_octet_string        = match l with C -> header_c.Define_new_octet_string | Ada -> header_a.Define_new_octet_string
    let define_subType_octet_string    = match l with C -> header_c.Define_subType_octet_string | Ada -> header_a.Define_subType_octet_string
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_octet_string td (o.minSize) (o.maxSize) (o.minSize = o.maxSize)
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_octet_string td subDef otherProgramUnit (o.minSize = o.maxSize)
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None



let createBitString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)  (us:State) =
    let td = o.typeDef.[l]
    let define_new_bit_string        = match l with C -> header_c.Define_new_bit_string | Ada -> header_a.Define_new_bit_string
    let define_subType_bit_string    = match l with C -> header_c.Define_subType_bit_string | Ada -> header_a.Define_subType_bit_string
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_bit_string td (o.minSize) (o.maxSize) (o.minSize = o.maxSize) (BigInteger o.MaxOctets)
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_bit_string td subDef otherProgramUnit (o.minSize = o.maxSize)
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None



let createEnumerated (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated)  (us:State) =
    let td = o.typeDef.[l]
    let define_new_enumerated_item        = match l with C -> header_c.Define_new_enumerated_item | Ada -> header_a.Define_new_enumerated_item
    let define_new_enumerated        = match l with C -> header_c.Define_new_enumerated | Ada -> header_a.Define_new_enumerated
    let define_subType_enumerated    = match l with C -> header_c.Define_subType_enumerated | Ada -> header_a.Define_subType_enumerated
    let orderedItems = o.items |> List.sortBy(fun i -> i.definitionValue)
    let arrsEnumNames = orderedItems |> List.map( fun i -> i.getBackendName None l)
    let arrsEnumNamesAndValues = orderedItems |> List.map( fun i -> define_new_enumerated_item (i.getBackendName None l) i.definitionValue)
    let nIndexMax = BigInteger ((Seq.length o.items)-1)

    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_enumerated td arrsEnumNames arrsEnumNamesAndValues nIndexMax
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


let createSequenceOf (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  (childDefinition:TypeDefintionOrReference) (us:State) =
    let define_new_sequence_of        = match l with C -> header_c.Define_new_sequence_of | Ada -> header_a.Define_new_sequence_of
    let define_subType_sequence_of    = match l with C -> header_c.Define_subType_sequence_of | Ada -> header_a.Define_subType_sequence_of
    let td = o.typeDef.[l]

    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_sequence_of td (o.minSize) (o.maxSize) (o.minSize = o.maxSize) (childDefinition.longTypedefName l) (getChildDefinition childDefinition)
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_sequence_of td subDef otherProgramUnit (o.minSize = o.maxSize)
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None



let createSequence (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence)  (children:SeqChildInfo list) (us:State) =
    let define_new_sequence             = match l with C -> header_c.Define_new_sequence            | Ada -> header_a.Define_new_sequence
    let define_new_sequence_child       = match l with C -> header_c.Define_new_sequence_child      | Ada -> header_a.Define_new_sequence_child
    let define_new_sequence_child_bit   = match l with C -> header_c.Define_new_sequence_child_bit  | Ada -> header_a.Define_new_sequence_child_bit
    let define_subType_sequence         = match l with C -> header_c.Define_subType_sequence        | Ada -> header_a.Define_subType_sequence

    let children = children |> List.choose (fun c -> match c with Asn1Child z -> Some z | _ -> None)
    let optionalChildren = children |> List.choose(fun c -> match c.Optionality with Some _ -> Some c | None -> None)
    let optChildNames  = optionalChildren |> List.map(fun c -> c.getBackendName l)
    let childldrenCompleteDefintions = children |> List.choose (fun c -> getChildDefinition c.Type.typeDefintionOrReference)
    let td = o.typeDef.[l]

    let arrsChildren = children |> List.map (fun o -> define_new_sequence_child (o.getBackendName l) (o.Type.typeDefintionOrReference.longTypedefName l))
    let arrsOptionalChildren  = optionalChildren |> List.map(fun c -> define_new_sequence_child_bit (c.getBackendName l))


    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_sequence td arrsChildren arrsOptionalChildren childldrenCompleteDefintions
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_sequence td subDef otherProgramUnit  arrsOptionalChildren
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None




let createChoice (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice)  (children:ChChildInfo list) (us:State) =
    let define_new_choice             = match l with C -> header_c.Define_new_choice            | Ada -> header_a.Define_new_choice
    let define_new_choice_child       = match l with C -> header_c.Define_new_choice_child      | Ada -> header_a.Define_new_choice_child
    let define_subType_choice         = match l with C -> header_c.Define_subType_choice        | Ada -> header_a.Define_subType_choice


    let td = o.typeDef.[l]
    let childldrenCompleteDefintions = children |> List.choose (fun c -> getChildDefinition c.chType.typeDefintionOrReference)
    let arrsPresent = children |> List.map(fun c -> c.presentWhenName None l)
    let arrsChildren = children |> List.map (fun o -> define_new_choice_child (o.getBackendName l)  (o.chType.typeDefintionOrReference.longTypedefName l) (o.presentWhenName None l))
    let nIndexMax = BigInteger ((Seq.length children)-1)

    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_choice td (choiceIDForNone t.id) (children.Head.presentWhenName None l) arrsChildren arrsPresent nIndexMax childldrenCompleteDefintions
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_choice td subDef otherProgramUnit  
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None


////////////////////////////////


let createInteger_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer)  (us:State) =
    let aaa = createInteger r l t o us
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

let createReal_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real)  (us:State) =
    let aaa = createReal r l t o us
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

let createBoolean_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean)  (us:State) =
    let aaa = createBoolean r l t o us
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

let createNull_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType)  (us:State) =
    let aaa = createNull r l t o us
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

let createOctetString_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  (us:State) =
    let aaa = createOctetString r l t o us
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


let createBitString_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)  (us:State) =
    let aaa = createBitString r l t o us
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
    

let createString_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let aaa = createString r l t o us
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


let createEnumerated_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated)  (us:State) =
    let aaa = createEnumerated r l t o us
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


let createSequenceOf_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  (childDefinition:TypeDefintionOrReference) (us:State) =
    let aaa = createSequenceOf r l t o  childDefinition us
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


let createSequence_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence)  (children:SeqChildInfo list) (us:State) =
    let aaa = createSequence r l t o  children us
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

let createChoice_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice)  (children:ChChildInfo list) (us:State) =
    let aaa = createChoice r l t o  children us
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


let createReferenceType_u (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType)  (baseType:Asn1Type ) (us:State) =
    baseType.typeDefintionOrReference
