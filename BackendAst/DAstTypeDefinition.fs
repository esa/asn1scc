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
    | PO_TypeReDefinition                   of PO_TypeDefinition*PO_ReferenceToExistingDefinition       

let PreOrder_TypeDefinitionCalculation (r:Asn1AcnAst.AstRoot)  (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<PO_TypeDefintionOrReference> option) (t:Asn1AcnAst.Asn1Type) = 
    match t.typeAssignmentInfo with
    | Some (TypeAssignmentInfo tasInfo)      ->  (*I am a type assignmet ==> Always define a new type*)
        let typedefName = (ToC2(r.args.TypePrefix + tasInfo.tasName))
        let programUnit = ToC t.id.ModName
        let po_typeDefintion =     {PO_TypeDefinition.typedefName=typedefName; programUnit= programUnit}
        match t.inheritInfo with
        | Some inheritInfo  ->  
            (*E.g. MyNewType ::= ExistingType*)
            let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
            PO_TypeReDefinition(po_typeDefintion, po_referenceToExistingDefinition)
        | None              ->
            PO_TypeDefinition po_typeDefintion
            
    | Some (ValueAssignmentInfo vasInfo)      ->  (*I am a value assignmet ==> Reference an existing or throw a user exception*)
        match t.inheritInfo with
        | Some inheritInfo   ->  
            let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
            PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
        | None   -> PO_ReferenceToRTLDefinition
    | None              -> (*I am a SEQUENCE or SEQUENCE OF or CHOICE child.*)
        match pi with
        | None              -> raise(BugErrorException "type has no typeAssignmentInfo and No parent!!!")
        | Some parentInfo   -> 
            match t.inheritInfo with
            | Some inheritInfo ->            
                let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName); programUnit = (ToC inheritInfo.modName)}
                match parentInfo.parentData with
                | PO_ReferenceToRTLDefinition           -> raise(BugErrorException "Impossible. Only primitive, not composite types defined in RTL!!!")
                | PO_ReferenceToExistingDefinition  _   -> PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
                | PO_TypeReDefinition _                 -> PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
                | PO_TypeDefinition parentDefinition  when (not inheritInfo.hasAdditionalConstraints)  ->
                    PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
                | PO_TypeDefinition parentDefinition    ->
                    let typedefName =
                        match parentInfo.name with
                        | Some nm -> ToC2(parentDefinition.typedefName + "_" + nm)
                        | None    -> ToC2(parentDefinition.typedefName + "_" + "elem")
                    PO_TypeDefinition {PO_TypeDefinition.typedefName = typedefName; programUnit = ToC t.id.ModName}
            | None  ->
                            (*I am not a referenced type. If my parent is a referenced type then I am also a referenced type.*)
                            (*Otherwise, define a new type or subtype*)
                match parentInfo.parentData with
                | PO_ReferenceToRTLDefinition           -> raise(BugErrorException "Impossible. Only primitive, not composite, types defined in RTL!!!")
                | PO_ReferenceToExistingDefinition  parentReferenceToExistingDefinition                   
                | PO_TypeReDefinition (_,parentReferenceToExistingDefinition)                 -> 
                    let typedefName =
                        match parentInfo.name with
                        | Some nm -> ToC2(parentReferenceToExistingDefinition.typedefName + "_" + nm)
                        | None    -> ToC2(parentReferenceToExistingDefinition.typedefName + "_" + "elem")
                    
                    let po_referenceToExistingDefinition = {PO_ReferenceToExistingDefinition.typedefName = typedefName; programUnit = parentReferenceToExistingDefinition.programUnit}
                    PO_ReferenceToExistingDefinition po_referenceToExistingDefinition
                | PO_TypeDefinition parentDefinition    ->
                    PO_ReferenceToRTLDefinition

                