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


let ll l =
    match l with
    |C      -> CommonTypes.ProgrammingLanguage.C
    |Ada    -> CommonTypes.ProgrammingLanguage.Ada

    
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

    let td = o.typeDef.[ll l]
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
    let td = o.typeDef.[ll l]
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
    let td = o.typeDef.[ll l]
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
    let td = o.typeDef.[ll l]
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

let createString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let arrnAlphaChars = (o.uperCharSet |> Array.map(fun c -> (BigInteger (int c))))
    let define_new_ia5string        = match l with C -> header_c.Define_new_ia5string | Ada -> header_a.Define_new_ia5string
    let define_subType_ia5string    = match l with C -> header_c.Define_subType_ia5string | Ada -> header_a.Define_subType_ia5string

    let td = o.typeDef.[ll l]
    match td.kind with
    | NonPrimitiveNewTypeDefinition              -> 
        let completeDefintion = define_new_ia5string td (o.minSize) (o.maxSize) ((o.maxSize + 1I)) arrnAlphaChars
        Some completeDefintion
    | NonPrimitiveNewSubTypeDefinition subDef     -> 
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let completeDefintion = define_subType_ia5string td subDef otherProgramUnit
        Some completeDefintion
    | NonPrimitiveReference2OtherType            -> None
