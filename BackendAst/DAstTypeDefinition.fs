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


/// https://drive.google.com/file/d/1qWX2fUM2e-f4uGcgnHxVZ8Dt3eIx_9d-/view?usp=sharing


type PO_ReferenceToExistingDefinition = {
    /// the module where this type is defined
    programUnit     : string
    /// The name of the defined type. 
    typedefName     : string
}

type PO_TypeDefinition = {
    programUnit: string
    /// The name of the defined type. If type is a type assignment then is the name of the type assignment.
    /// if the type is an inner type (i.e. within a SEQUENCE/SEQUENCE OF/CHOICE) then name is created as 
    /// parentType.typedefName + "_" + component_name
    typedefName : string
}


type PO_TypeDefintionOrReference =
    /// indicates that no extra type definition is required (e.g. INTEGER without constraints or type reference type without new constraints)
    | PO_ReferenceToExistingDefinition      of PO_ReferenceToExistingDefinition                
    | PO_ReferenceToRTLDefinition           
    /// indicates that a new type is defined
    | PO_TypeDefinition                     of PO_TypeDefinition       
    | PO_SubTypeDefinition                   of PO_TypeDefinition*PO_ReferenceToExistingDefinition       

let PreOrder_TypeDefinitionCalculation (r:Asn1AcnAst.AstRoot)  (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<PO_TypeDefintionOrReference> option) (t:Asn1AcnAst.Asn1Type) = 
    let rtlDefinedType =
        match t.Kind with
        | Asn1AcnAst.Boolean _
        | Asn1AcnAst.Real  _
        | Asn1AcnAst.NullType _ -> true
        | Asn1AcnAst.Integer o  ->
            match o.uperRange with
            | Full                        -> true
            | PosInf (a)  when a=0I       -> true
            | _                           -> false
        |_                      -> false
        
    match t.typeAssignmentInfo with
    | Some (TypeAssignmentInfo tasInfo)      ->  (*I am a type assignmet ==> Always define a new type*)
        let typedefName = (ToC2(r.args.TypePrefix + tasInfo.tasName))
        let programUnit = ToC t.id.ModName
        let po_typeDefintion =     {PO_TypeDefinition.typedefName=typedefName; programUnit= programUnit}
        match t.inheritInfo with
        | None              ->            PO_TypeDefinition po_typeDefintion
        | Some inheritInfo  ->  
            (*E.g. MyNewType ::= ExistingType*)
            let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
            PO_SubTypeDefinition(po_typeDefintion, po_referenceToExistingDefinition)
            
    | Some (ValueAssignmentInfo vasInfo)      ->  (*I am a value assignmet ==> Reference an existing or throw a user exception*)
        match t.inheritInfo with
        | Some inheritInfo ->  
            let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
            PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
        | None   -> PO_ReferenceToRTLDefinition
    | None              -> (*I am a SEQUENCE or SEQUENCE OF or CHOICE child.*)
        match pi with
        | None              -> raise(BugErrorException "type has no typeAssignmentInfo and No parent!!!")
        | Some parentInfo   -> 
            match parentInfo.parentData with
            | PO_ReferenceToRTLDefinition           -> raise(BugErrorException "Impossible. Only primitive, not composite, types defined in RTL!!!")
            | PO_TypeDefinition parentDefinition    ->
                (* My parent type is a sequence or choice or sequence of - not a referenced type*)
                match t.inheritInfo with
                | None             ->
                    (*I am not not  referenced type*)
                    match rtlDefinedType with
                    | true  -> PO_ReferenceToRTLDefinition
                    | false -> 
                        let typedefName =
                            match parentInfo.name with
                            | Some nm -> ToC2(parentDefinition.typedefName + "_" + nm)
                            | None    -> ToC2(parentDefinition.typedefName + "_" + "elem")
                        let po_typeDefintion =     {PO_TypeDefinition.typedefName=typedefName; programUnit= parentDefinition.programUnit}
                        PO_TypeDefinition po_typeDefintion
                | Some inheritInfo  when inheritInfo.hasAdditionalConstraints   ->     
                    let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
                    let typedefName =
                        match parentInfo.name with
                        | Some nm -> ToC2(parentDefinition.typedefName + "_" + nm)
                        | None    -> ToC2(parentDefinition.typedefName + "_" + "elem")
                    let poDef = {PO_TypeDefinition.typedefName = typedefName; programUnit = ToC t.id.ModName}
                    PO_SubTypeDefinition (poDef, po_referenceToExistingDefinition)
                | Some inheritInfo  ->     
                    let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
                    PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
            | PO_ReferenceToExistingDefinition  parentReferenceToExistingDefinition                   
            | PO_SubTypeDefinition (_,parentReferenceToExistingDefinition)                 -> 
                (*My parent is a referenced type, so in most cases I am also a reference type*)
                match t.inheritInfo with
                | None             ->
                    match rtlDefinedType with
                    | true  -> PO_ReferenceToRTLDefinition
                    | false -> 
                        let typedefName =
                            match parentInfo.name with
                            | Some nm -> ToC2(parentReferenceToExistingDefinition.typedefName + "_" + nm)
                            | None    -> ToC2(parentReferenceToExistingDefinition.typedefName + "_" + "elem")
                        let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = typedefName; programUnit = parentReferenceToExistingDefinition.programUnit}
                        PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
                | Some inheritInfo  when inheritInfo.hasAdditionalConstraints   ->     
                        let typedefName =
                            match parentInfo.name with
                            | Some nm -> ToC2(parentReferenceToExistingDefinition.typedefName + "_" + nm)
                            | None    -> ToC2(parentReferenceToExistingDefinition.typedefName + "_" + "elem")
                        let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = typedefName; programUnit = parentReferenceToExistingDefinition.programUnit}
                        PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
                | Some inheritInfo  ->     
                    let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
                    PO_ReferenceToExistingDefinition po_referenceToExistingDefinition



//            match t.inheritInfo with
//            | Some inheritInfo ->     
//                    (*I am referenced type. In most cases just return a reference to my TAS. 
//                    However, if I am a reference to a primitive type with additional constraints
//                    then a new sub type must be defined*)       
//                let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
//                match parentInfo.parentData with
//                | PO_ReferenceToRTLDefinition           -> raise(BugErrorException "Impossible. Only primitive, not composite types defined in RTL!!!")
//                | PO_ReferenceToExistingDefinition  _   -> PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
//                | PO_SubTypeDefinition _                 -> PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
//                | PO_TypeDefinition parentDefinition  when (not inheritInfo.hasAdditionalConstraints)  ->
//                    PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
//                | PO_TypeDefinition parentDefinition    ->
//                    let typedefName =
//                        match parentInfo.name with
//                        | Some nm -> ToC2(parentDefinition.typedefName + "_" + nm)
//                        | None    -> ToC2(parentDefinition.typedefName + "_" + "elem")
//                    let poDef = {PO_TypeDefinition.typedefName = typedefName; programUnit = ToC t.id.ModName}
//                    PO_SubTypeDefinition (poDef, po_referenceToExistingDefinition)
//            | None  ->
//                            (*I am not a referenced type. If my parent is a referenced type then I am also a referenced type.*)
//                            (*Otherwise, define a new type or subtype*)
//                match parentInfo.parentData with
//                | PO_ReferenceToRTLDefinition           -> raise(BugErrorException "Impossible. Only primitive, not composite, types defined in RTL!!!")
//                | PO_ReferenceToExistingDefinition  parentReferenceToExistingDefinition                   
//                | PO_SubTypeDefinition (_,parentReferenceToExistingDefinition)                 -> 
//                    let typedefName =
//                        match parentInfo.name with
//                        | Some nm -> ToC2(parentReferenceToExistingDefinition.typedefName + "_" + nm)
//                        | None    -> ToC2(parentReferenceToExistingDefinition.typedefName + "_" + "elem")
//                    
//                    let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = typedefName; programUnit = parentReferenceToExistingDefinition.programUnit}
//                    PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
//                | PO_TypeDefinition parentDefinition    -> 
//                    (*I am a non referenced child and my parent is not a referenced type*)
//                    (*IF iam BOOLEAN, REAL or NULLTYPE then PO_ReferenceToRTLDefinition*)
//                    match rtlDefinedType with
//                    | true  -> PO_ReferenceToRTLDefinition
//                    | false -> 
//                        let typedefName =
//                            match parentInfo.name with
//                            | Some nm -> ToC2(parentDefinition.typedefName + "_" + nm)
//                            | None    -> ToC2(parentDefinition.typedefName + "_" + "elem")
//                        let po_typeDefintion =     {PO_TypeDefinition.typedefName=typedefName; programUnit= parentDefinition.programUnit}
//                        PO_TypeDefinition po_typeDefintion
//                

let createInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (po : PO_TypeDefintionOrReference) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Integer)   (us:State) =
    let declare_IntegerNoRTL            = match l with C -> header_c.Declare_Integer                    | Ada -> header_a.Declare_IntegerNoRTL
    match po with
    | PO_ReferenceToExistingDefinition      z                ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.typedefName = z.typedefName;programUnit= Some z.programUnit; definedInRtl=false}
    | PO_ReferenceToRTLDefinition               ->
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.typedefName = (declare_IntegerNoRTL ());programUnit= Some ""; definedInRtl=true}
    | PO_TypeDefinition                     z       ->
        TypeDefinition {TypeDefinition.typedefName = z.typedefName; baseType=None; typedefBody= (fun () -> "")}   
    | PO_SubTypeDefinition                   (z, h)  ->
        TypeDefinition {TypeDefinition.typedefName = z.typedefName; baseType=Some {ReferenceToExistingDefinition.typedefName = h.typedefName;programUnit= Some h.programUnit; definedInRtl=false}; typedefBody= (fun () -> "")}   
