module DAstTypeDefinition2

open System
open System.Numerics
open FsUtils
open Asn1AcnAstUtilFunctions
open Asn1AcnAst
open CommonTypes
open DAst
open DAstUtilFunctions

(*
type TypeDefintionOrReference =
    /// indicates that no extra type definition is required (e.g. INTEGER without constraints or type reference type without new constraints)
    | ReferenceToExistingDefinition    of ReferenceToExistingDefinition                
    /// indicates that a new type is defined
    | TypeDefinition                of TypeDefinition       


	examples
	  A ::= INTEGER				-> {TypeDefinition.typedefName = "A"; typedefBody="typedef A Asn1SInt"}
	  
	  C ::= SEQUENCE  {			-> {TypeDefinition.typedefName = "C"; typedefBody="typedef struct{ ... }"}
			a1   INTEGER		-> {ReferenceToExistingDefinition.programUnit="adaasn1rtl";  typedefName="Asn1Int"} 
			a2	 A				-> {ReferenceToExistingDefinition.programUnit=None;  typedefName="A"} //program unit is none since the type being referenced is in the same module as the referenced type
			a3	 A(1..20)		-> {TypeDefinition.typedefName = "C_a3"; typedefBody = "SUBTYPE C_a3 is A range 0..15"}
		}
		
		

                            Type Assignment ?
                            /               \
                          No                Yes --> New type definition with typedefname = Type Assignement Name
                           |                        
                 (=> Composite Type Child)
                           |
                           |
                                 (Is Primitive types with base definition in RTL  ?)
                               /                                                     \
                              Yes (Int,Real,Bool, Null)                               No (Octet, bit, IA5String, Enumerated, Sequence, Sequence Of, choice)
                               |                                                      |
                        (has extra range?)                                          (Is reference Type with no extra constraint)
                  /                             \                                    /                  \ 
                 /                               \                                  /                    \
                Yes                              No                                 Yes                   No
                 |                                |                                  |                     |
             New Subtype                      Reference to                          Reference to             New Type
            Definition with                   Existing                               Existing              Definition with 
           typedef name =                     Definition                             Definition                name = 
parent type typedefName + child Name         (The existing definition                                       parent type typedefName + child Name
and with base type the referneced type       may be the base
or  base type in RTL                         definition in RTL or
                                             the base type if this is
                                             a referenced type)  

*)



//let getPotentialTypedefName (r:AstRoot) (t:Asn1Type)  (potentialTypedefName:string)   =
//    t.newTypeDefName        
    

let createPrmAcnInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  =
    let Declare_Integer     =  match l with  C  -> header_c.Declare_Integer  | Ada   -> header_a.Declare_Integer 
    Declare_Integer ()

let createAcnInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (a:Asn1AcnAst.AcnInteger) =
    let Declare_Integer     =  match l with  C  -> header_c.Declare_Integer  | Ada   -> header_a.Declare_Integer 
    let Declare_PosInteger  =  match l with  C  -> header_c.Declare_PosInteger  | Ada   -> header_a.Declare_PosInteger  
    match a.isUnsigned with
    | true     -> Declare_PosInteger ()
    | false    -> Declare_Integer ()
        
    
let createAcnBoolean (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) =
    match l with
    | C                      -> header_c.Declare_Boolean ()
    | Ada                    -> header_a.Declare_BOOLEAN ()    

let createAcnNull (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) =
    match l with
    | C                      -> header_c.Declare_NullType ()
    | Ada                    -> header_a.Declare_NULL ()



type private DefineSubTypeAux = {
    getRtlTypeName : unit-> string
    getNewRange    : (*inheritParentTypePackage:*)string option -> (*inheritParentType:*)string -> string option
}

type private DefineNewTypeAux = {
    getCompleteDefintion   : ((*programUnit:*)string) -> ((*typedefName:*)string) -> string
}

type private DefineTypeAux = 
    | DefineSubTypeAux  of DefineSubTypeAux
    | DefineNewTypeAux  of DefineNewTypeAux

type typeDefitionKindFunc =
    | GetSubTypeRangeFnc of  (unit-> string )*(string option -> string -> string)     

/// Called before visiting a choice or sequence or sequence of children
let getParentInfoData (r:Asn1AcnAst.AstRoot) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)   =
    match t.typeAssignmentInfo with
    | Some (TypeAssignmentInfo tasInfo)       ->  
        match t.inheritInfo with
        | Some inhInfo      -> (*I am a reference type*) {ParentInfoData.program_unit_name = ToC inhInfo.modName ;typedefName = ToC2(r.args.TypePrefix + inhInfo.tasName); isRefType= true}
        | None              -> 
            {ParentInfoData.program_unit_name = ToC t.id.ModName;  typedefName = ToC2(r.args.TypePrefix + tasInfo.tasName); isRefType= false} // I am a type assignment
    | Some (ValueAssignmentInfo vasInfo)      ->  
            {ParentInfoData.program_unit_name = ToC t.id.ModName;  typedefName = ToC2(r.args.TypePrefix + vasInfo.vasName); isRefType= true} // I am a type assignment
    | None              ->  // I am an inner type
        match t.inheritInfo with
        | Some inhInfo      -> (*I am a reference type*) {ParentInfoData.program_unit_name = ToC inhInfo.modName ;typedefName = ToC2(r.args.TypePrefix + inhInfo.tasName); isRefType= true}
        | None              -> 
            match pi with
            | Some parentInfo   ->
                match parentInfo.name with
                | Some nm -> {ParentInfoData.program_unit_name = parentInfo.parentData.program_unit_name;  typedefName = ToC2(parentInfo.parentData.typedefName + "_" + nm); isRefType= parentInfo.parentData.isRefType}
                | None    -> {ParentInfoData.program_unit_name = parentInfo.parentData.program_unit_name;  typedefName = ToC2(parentInfo.parentData.typedefName + "_" + "elem"); isRefType= parentInfo.parentData.isRefType}
            | None              ->
                raise(BugErrorException "type has no typeAssignmentInfo and No parent!!!")
                


let private createTypeGeneric (r:Asn1AcnAst.AstRoot)  l (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) getExtraSubtypes (defineNewTypeFnc:DefineTypeAux)   =
    let programUnit = ToC t.id.ModName
    let rtlModuleName  = match l with C -> None                                          | Ada -> Some (header_a.rtlModuleName())
    let defineSubType l = match l with C -> header_c.Define_SubType | Ada -> header_a.Define_SubType
    if t.id.AsString = "TEST-CASE.T-META.longitude" then
        let dummy = 0
        ()
        (*
    let defineSubTypeAux (parent_pu_name:string option) (programUnit:string) (typedefName:string) (inheritInfo : InheritanceInfo option) (subAux:DefineSubTypeAux) (innerType:bool) =
        let soInheritParentTypePackage, sInheritParentType = 
            match inheritInfo with
            | None      -> rtlModuleName, subAux.getRtlTypeName()
            | Some inhInfo  ->
                let aaa = inhInfo.hasAdditionalConstraints
                match l with
                | C     ->  None, (ToC2(r.args.TypePrefix + inhInfo.tasName))
                | Ada   ->
                    match ToC(inhInfo.modName) = programUnit with
                    | true  -> None, (ToC2(r.args.TypePrefix + inhInfo.tasName))
                    | false -> Some (ToC inhInfo.modName), (ToC2(r.args.TypePrefix + inhInfo.tasName))
        
        let soNewRange = subAux.getNewRange soInheritParentTypePackage sInheritParentType
        match soNewRange with
        | None when  innerType    -> (*If there is no new range and is an inner type, then just make a reference to existing type*)
            ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = soInheritParentTypePackage; typedefName=sInheritParentType}         
        | _     -> (*Otherwise, create a new type which is a subtype of the existing type*)
            match parent_pu_name with 
            | None  ->
                let completeDefintion = defineSubType l typedefName soInheritParentTypePackage sInheritParentType soNewRange None
                TypeDefinition {TypeDefinition.typedefName = typedefName; typedefBody = (fun () -> completeDefintion)}
            | Some parent_pu_name  when programUnit =   parent_pu_name -> 
                let completeDefintion = defineSubType l typedefName soInheritParentTypePackage sInheritParentType soNewRange None
                TypeDefinition {TypeDefinition.typedefName = typedefName; typedefBody = (fun () -> completeDefintion)}
            | Some parent_pu_name   ->
                ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some parent_pu_name; typedefName= typedefName}
    //if t.id.AsString = "Onboard-Monitoring.Transition-Report.value.type-1-0" then
    //    printfn "%s" t.id.AsString
    *)
    match t.typeAssignmentInfo with
    | Some (TypeAssignmentInfo tasInfo)      ->  (*I am a type assignmet ==> Always define a new type*)
        let typedefName = (ToC2(r.args.TypePrefix + tasInfo.tasName))
        match t.inheritInfo with
        | Some inheritInfo  ->  (*E.g. MyNewType ::= ExistingType*)
            let otherProgramUnit = if  inheritInfo.modName = tasInfo.modName then None else Some (ToC inheritInfo.modName)
            let baseTypeTypedefName = ToC2(r.args.TypePrefix + inheritInfo.tasName)
            let soNewRange = 
                match defineNewTypeFnc with
                | DefineSubTypeAux subAux -> subAux.getNewRange otherProgramUnit baseTypeTypedefName
                | DefineNewTypeAux _      -> None
            let extraDefs = getExtraSubtypes typedefName otherProgramUnit baseTypeTypedefName
            let typedefBody = defineSubType l typedefName otherProgramUnit baseTypeTypedefName soNewRange extraDefs
            
            let baseTypeProgramUnit = if programUnit = ToC inheritInfo.modName then None else Some (ToC inheritInfo.modName)
            let referenceToExistingDefinition = {ReferenceToExistingDefinition.programUnit = baseTypeProgramUnit; typedefName=ToC2(r.args.TypePrefix + inheritInfo.tasName) ; definedInRtl = false}
            
            TypeDefinition {TypeDefinition.typedefName=typedefName; (*programUnitName = Some programUnit;*) typedefBody = (fun () -> typedefBody); baseType= Some referenceToExistingDefinition}
        | None             -> 
            match defineNewTypeFnc with
            | DefineSubTypeAux subAux -> 
                //defineSubTypeAux None programUnit typedefName t.inheritInfo subAux false
                let soNewRange = subAux.getNewRange rtlModuleName (subAux.getRtlTypeName())
                let completeDefintion = defineSubType l typedefName rtlModuleName (subAux.getRtlTypeName()) soNewRange None
                TypeDefinition {TypeDefinition.typedefName = typedefName; typedefBody = (fun () -> completeDefintion); baseType=None}
            | DefineNewTypeAux ntAux  ->
                let completeDefintion = ntAux.getCompleteDefintion  programUnit typedefName 
                TypeDefinition {TypeDefinition.typedefName = typedefName; (*programUnitName = Some programUnit;*) typedefBody = (fun () -> completeDefintion); baseType=None}
    | Some (ValueAssignmentInfo vasInfo)      ->  (*I am a value assignmet ==> Reference an existing or throw a user exception*)
        match t.inheritInfo with
        | Some inheritInfo   ->  
            let baseTypeProgramUnit = if programUnit = ToC inheritInfo.modName then None else Some (ToC inheritInfo.modName)
            ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = baseTypeProgramUnit; typedefName=ToC2(r.args.TypePrefix + inheritInfo.tasName); definedInRtl = false}
        | None   -> 
            match defineNewTypeFnc with
            | DefineSubTypeAux  subAux -> 
                let soInheritParentTypePackage, sInheritParentType =  rtlModuleName, subAux.getRtlTypeName()
                ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = soInheritParentTypePackage; typedefName=sInheritParentType; definedInRtl = true}         
            | DefineNewTypeAux ntAux  ->
                raise(SemanticError(t.Location, "Anonymous types are not supported. Please define a new type and then use it in the value assignment"))
    | None              -> (*I am a SEQUENCE or SEQUENCE OF or CHOICE child.*)
        let parentInfo =
            match pi with
            | Some parentInfo   -> parentInfo
            | None              -> raise(BugErrorException "type has no typeAssignmentInfo and No parent!!!")
        let typedefName =
            match parentInfo.name with
            | Some nm -> ToC2(parentInfo.parentData.typedefName + "_" + nm)
            | None    -> ToC2(parentInfo.parentData.typedefName + "_" + "elem")
        
        match t.inheritInfo with
        | Some inheritInfo -> (*I am referenced type. In most cases just return a reference to my TAS. However, if I a reference to a primitive type with additional constraints*)
                              (*then a new sub type must be defined*)
            let baseTypeProgramUnit = if programUnit = ToC inheritInfo.modName then None else Some (ToC inheritInfo.modName)
            let referenceToExistingDefinition = ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = baseTypeProgramUnit; typedefName=ToC2(r.args.TypePrefix + inheritInfo.tasName) ; definedInRtl = false}
            match defineNewTypeFnc with
            | DefineSubTypeAux subAux -> 
                match inheritInfo.hasAdditionalConstraints with
                | false -> referenceToExistingDefinition 
                | true  ->
                    match parentInfo.parentData.isRefType with
                    | true  -> referenceToExistingDefinition 
                    | false -> (*I am a referenced type of a primitive type (e.g. INTEGER) with additional constraints and my parent type is not referenced type*)
                               (*In this case a new type sub type must be defined*)
                    let soInheritParentTypePackage, sInheritParentType = 
                        match l with
                        | C     ->  None, (ToC2(r.args.TypePrefix + inheritInfo.tasName))
                        | Ada   ->
                            match ToC(inheritInfo.modName) = programUnit with
                            | true  -> None, (ToC2(r.args.TypePrefix + inheritInfo.tasName))
                            | false -> Some (ToC inheritInfo.modName), (ToC2(r.args.TypePrefix + inheritInfo.tasName))
                    let soNewRange = subAux.getNewRange soInheritParentTypePackage sInheritParentType
                    let completeDefintion = defineSubType l typedefName soInheritParentTypePackage sInheritParentType soNewRange None
                    TypeDefinition {TypeDefinition.typedefName = typedefName; typedefBody = (fun () -> completeDefintion); baseType=None}
            | DefineNewTypeAux  _  -> referenceToExistingDefinition 
        | None           -> (*I am not a referenced type. If my parent is a referenced type then I am also a referenced type.*)
                            (*Otherwise, define a new type or subtype*)
            match parentInfo.parentData.isRefType with
            | true  -> (*I am a child of a type which is a referenced type ==> ReferenceToExistingDefinition*)
                let progUnit =
                    match parentInfo.parentData.program_unit_name = programUnit with
                    | true  -> None
                    | false -> Some parentInfo.parentData.program_unit_name
                match defineNewTypeFnc with
                | DefineSubTypeAux subAux -> 
                    let soNewRange = subAux.getNewRange rtlModuleName (subAux.getRtlTypeName())
                    match soNewRange with
                    | None  ->  (*If there is no new range and since I am an inner primitive type, then just make a reference to existing type in RTL*)
                        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = rtlModuleName; typedefName=(subAux.getRtlTypeName()) ; definedInRtl = true }         
                    | Some _ -> (*I have a new range so make reference to this type*)
                        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  progUnit; typedefName= typedefName; definedInRtl = false}
                | DefineNewTypeAux ntAux  ->
                        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  progUnit; typedefName= typedefName; definedInRtl = false}
            | false -> (*I am a non referenced child and my parent is not a referenced type*)
                match defineNewTypeFnc with
                | DefineSubTypeAux subAux -> 
                    let soNewRange = subAux.getNewRange rtlModuleName (subAux.getRtlTypeName())
                    match soNewRange with
                    | None  ->  (*If there is no new range and since I am an inner primitive type, then just make a reference to existing type in RTL*)
                        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = rtlModuleName; typedefName=(subAux.getRtlTypeName()); definedInRtl = true }         
                    | Some _ ->
                        let completeDefintion = defineSubType l typedefName rtlModuleName (subAux.getRtlTypeName()) soNewRange None
                        TypeDefinition {TypeDefinition.typedefName = typedefName; typedefBody = (fun () -> completeDefintion); baseType=None}
                | DefineNewTypeAux ntAux  ->
                    let completeDefintion = ntAux.getCompleteDefintion  programUnit typedefName 
                    TypeDefinition {TypeDefinition.typedefName = typedefName; (*programUnitName = Some programUnit;*) typedefBody = (fun () -> completeDefintion); baseType=None}
        (*
        let parent_pu_name, typedefName = 
            match pi with
            | Some parentInfo   ->
                match parentInfo.name with
                | Some nm -> Some parentInfo.parentData.program_unit_name, ToC2(parentInfo.parentData.typedefName + "_" + nm)
                | None    -> Some parentInfo.parentData.program_unit_name, ToC2(parentInfo.parentData.typedefName + "_" + "elem")
            | None              ->
                raise(BugErrorException "type has no typeAssignmentInfo and No parent!!!")

        let aux () =
            match defineNewTypeFnc with
            | DefineSubTypeAux subAux -> 
                defineSubTypeAux parent_pu_name programUnit typedefName t.inheritInfo subAux true
            | DefineNewTypeAux ntAux  ->
                match t.inheritInfo with
                | Some inheritInfo   ->  
                
                    let baseTypeProgramUnit = if programUnit = ToC inheritInfo.modName then None else Some (ToC inheritInfo.modName)
                    ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = baseTypeProgramUnit; typedefName=ToC2(r.args.TypePrefix + inheritInfo.tasName)}
                | _   -> 
                    let completeDefintion = ntAux.getCompleteDefintion  programUnit typedefName 
                    TypeDefinition {TypeDefinition.typedefName = typedefName; (*programUnitName = Some programUnit;*) typedefBody = (fun () -> completeDefintion)}
        //aux ()
        match parent_pu_name with
        | None          -> aux ()
        | Some parent_pu_name  when programUnit =   parent_pu_name -> aux ()
        | Some parent_pu_name        -> //aux()
            ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some parent_pu_name; typedefName= typedefName}

            *)
(*
Primitive types with base definition in RTL which can be used as is generated code (Integer, Real, Boolean, NULL).

These types are defined as sub types.
*)

    
let createInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Integer)   (us:State) =
    let declare_IntegerNoRTL            = match l with C -> header_c.Declare_Integer                    | Ada -> header_a.Declare_IntegerNoRTL
    let declare_PosIntegerNoRTL         = match l with C -> header_c.Declare_PosInteger                 | Ada -> header_a.Declare_PosIntegerNoRTL
    let define_SubType_int_range        = match l with C -> (fun _ _ _ _  -> "")                        | Ada -> header_a.Define_SubType_int_range
//    if t.id.AsString = "TEST-CASE.T-POS.anInt" then
//        printfn "%s" t.id.AsString

    let getNewRange soInheritParentTypePackage sInheritParentType = 
        match o.uperRange with
        | Concrete(a,b)               ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) (Some b))
        | NegInf (b)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType None (Some b))
        | PosInf (a)  when a=0I       ->  None
        | PosInf (a)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) None)
        | Full                        ->  None
    let getRtlTypeName () = if o.isUnsigned then declare_PosIntegerNoRTL() else declare_IntegerNoRTL()
    let ret = createTypeGeneric r l pi t (fun _ _ _ -> None) (DefineSubTypeAux {DefineSubTypeAux.getNewRange = getNewRange; getRtlTypeName = getRtlTypeName})
    ret

let createBoolean (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Boolean)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_Boolean  | Ada -> header_a.Declare_BOOLEANNoRTL 
    createTypeGeneric r l pi t (fun _ _ _ -> None) (DefineSubTypeAux {DefineSubTypeAux.getNewRange = (fun _ _ -> None); getRtlTypeName = getRtlTypeName})

let createReal (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Real)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_Real  | Ada -> header_a.Declare_REALNoRTL 
    createTypeGeneric r l pi t (fun _ _ _ -> None) (DefineSubTypeAux {DefineSubTypeAux.getNewRange = (fun _ _ -> None); getRtlTypeName = getRtlTypeName})

let createNull (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.NullType)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_NullType  | Ada -> header_a.Declare_NULLNoRTL 
    createTypeGeneric r l pi t (fun _ _ _ -> None) (DefineSubTypeAux {DefineSubTypeAux.getNewRange = (fun _ _ -> None); getRtlTypeName = getRtlTypeName})

(*
Primitive types with NO base definition in RTL (IA5String, OCTET STRING, BIT STRING, ENUMERATED)
*)



let createString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let arrnAlphaChars = (o.uperCharSet |> Array.map(fun c -> (BigInteger (int c))))
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let typeDefinitionBody =header_c.Declare_IA5String ()
            header_c.Define_Type typeDefinitionBody typeDefinitionName (Some (o.maxSize+1I)) []
        | Ada                    -> 
            header_a.IA5STRING_OF_tas_decl typeDefinitionName (o.minSize) (o.maxSize) ((o.maxSize + 1I)) arrnAlphaChars
    let getExtraSubTypes sTypeDefinitionName soParentTypePackage sParentType =
        match l with
        | C     -> None
        | Ada   -> Some (header_a.Define_SubType_ia5string sTypeDefinitionName soParentTypePackage sParentType arrnAlphaChars)
    createTypeGeneric r l pi t getExtraSubTypes (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createOctet (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  (us:State) =
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let typeDefinitionBody = header_c.Declare_OctetString (o.minSize=o.maxSize) ( o.maxSize)
            header_c.Define_Type typeDefinitionBody typeDefinitionName None []
        | Ada                    -> 
            header_a.OCTET_STRING_tas_decl typeDefinitionName ( o.minSize) ( o.maxSize) (o.maxSize=o.minSize)
    let getExtraSubTypes sTypeDefinitionName soParentTypePackage sParentType =
        match l with
        | C     -> None
        | Ada   -> Some (header_a.Define_SubType_sizeable sTypeDefinitionName soParentTypePackage sParentType)
    createTypeGeneric r l pi t getExtraSubTypes (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createBitString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)   (us:State) =
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let typeDefinitionBody = header_c.Declare_BitString (o.minSize=o.maxSize) (BigInteger o.MaxOctets) ( o.maxSize)
            header_c.Define_Type typeDefinitionBody typeDefinitionName None []
        | Ada                    -> 
            header_a.BIT_STRING_tas_decl typeDefinitionName ( o.minSize) ( o.maxSize) (o.maxSize=o.minSize)
    let getExtraSubTypes sTypeDefinitionName soParentTypePackage sParentType =
        match l with
        | C     -> None
        | Ada   -> Some (header_a.Define_SubType_sizeable sTypeDefinitionName soParentTypePackage sParentType)
    createTypeGeneric r l pi t getExtraSubTypes (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createEnumerated (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated)  (us:State) =
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let items = o.items |> List.map( fun i -> header_c.PrintNamedItem (i.getBackendName None l) i.definitionValue)
                //match o.userDefinedValues with
                //| true  -> o.items |> List.map( fun i -> header_c.PrintNamedItem (i.getBackendName None l) i.definitionValue)
                //| false -> o.items |> List.map( fun i -> i.getBackendName None l)
            let typeDefinitionBody = header_c.Declare_Enumerated items
            header_c.Define_Type typeDefinitionBody typeDefinitionName None []
        | Ada                    -> 
            let orderedItems = o.items |> List.sortBy(fun i -> i.definitionValue)
            let arrsEnumNames = orderedItems |> List.map( fun i -> i.getBackendName None l)
            let arrsEnumNamesAndValues = orderedItems |> List.map( fun i -> header_a.ENUMERATED_tas_decl_item (i.getBackendName None l) i.definitionValue)
            let nIndexMax = BigInteger ((Seq.length o.items)-1)
            header_a.ENUMERATED_tas_decl typeDefinitionName arrsEnumNames arrsEnumNamesAndValues nIndexMax
    let getExtraSubTypes sTypeDefinitionName soParentTypePackage sParentType =
        match l with
        | C     -> None
        | Ada   -> Some (header_a.Define_SubType_enumerated sTypeDefinitionName soParentTypePackage sParentType)
    createTypeGeneric r l pi t getExtraSubTypes (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

(*
COMPOSITE TYPES (SEQUENCE OF, SEQUENCE, CHOICE)
*)

let internal getChildDefinition (childDefinition:TypeDefintionOrReference) =
    match childDefinition with
    | TypeDefinition  td -> [td.typedefBody ()]
    | ReferenceToExistingDefinition ref -> []


let createSequenceOf (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf)  (childDefinition:TypeDefintionOrReference) (us:State) =
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let typeDefinitionBody = header_c.Declare_SequenceOf (o.minSize = o.maxSize) (childDefinition.longTypedefName l) ( o.maxSize) ""
            header_c.Define_Type typeDefinitionBody typeDefinitionName None (getChildDefinition childDefinition)
        | Ada                    -> 
            header_a.SEQUENCE_OF_tas_decl typeDefinitionName ( o.minSize) ( o.maxSize) (o.minSize = o.maxSize) (childDefinition.longTypedefName l) (getChildDefinition childDefinition)
    let getExtraSubTypes sTypeDefinitionName soParentTypePackage sParentType =
        match l with
        | C     -> None
        | Ada   -> Some (header_a.Define_SubType_sizeable sTypeDefinitionName soParentTypePackage sParentType)
    createTypeGeneric r l pi t getExtraSubTypes (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})


let createSequence (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence)  (children:SeqChildInfo list) (us:State) =
    let children = children |> List.choose (fun c -> match c with Asn1Child z -> Some z | _ -> None)
    let optionalChildren = children |> List.choose(fun c -> match c.Optionality with Some _ -> Some c | None -> None)
    let optChildNames  = optionalChildren |> List.map(fun c -> c.getBackendName l)
    let childldrenCompleteDefintions = children |> List.collect (fun c -> getChildDefinition c.Type.typeDefintionOrReference)
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      ->
            let handleChild (o:Asn1Child) = header_c.PrintSeq_ChoiceChild (o.Type.typeDefintionOrReference.longTypedefName l ) (o.getBackendName l) ""
            let childrenBodies = children |> List.map handleChild
            let typeDefinitionBody = header_c.Declare_Sequence childrenBodies optChildNames
            header_c.Define_Type typeDefinitionBody typeDefinitionName None childldrenCompleteDefintions
        | Ada                    -> 
            let handleChild (o:Asn1Child) = header_a.SEQUENCE_tas_decl_child (o.getBackendName l) (o.Type.typeDefintionOrReference.longTypedefName l)
            let childrenBodies = children |> List.map handleChild
            let optChildren  = optionalChildren |> List.map(fun c -> header_a.SEQUENCE_tas_decl_child_bit (c.getBackendName l))
            header_a.SEQUENCE_tas_decl typeDefinitionName childrenBodies optChildren childldrenCompleteDefintions
    let getExtraSubTypes sTypeDefinitionName soParentTypePackage sParentType =
        match l with
        | C     -> None
        | Ada   -> Some (header_a.Define_SubType_sequence sTypeDefinitionName soParentTypePackage sParentType optChildNames)
    createTypeGeneric r l pi t getExtraSubTypes (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createChoice (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice)  (children:ChChildInfo list) (us:State) =
//    if t.id.AsString = "TEST-CASE.T-POS" then
//        printfn "%s" t.id.AsString
    let childldrenCompleteDefintions = children |> List.collect (fun c -> getChildDefinition c.chType.typeDefintionOrReference)
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      ->
            let handleChild (o:ChChildInfo) = header_c.PrintSeq_ChoiceChild (o.chType.typeDefintionOrReference.longTypedefName l) (o.getBackendName l) ""
            let chEnms = children |> List.map(fun c -> c.presentWhenName None l)
            let childrenBodies = children |> List.map handleChild
            let typeDefinitionBody = header_c.Declare_Choice (choiceIDForNone t.id) chEnms childrenBodies 
            header_c.Define_Type typeDefinitionBody typeDefinitionName None childldrenCompleteDefintions
        | Ada                    -> 
            let handleChild (o:ChChildInfo) = header_a.CHOICE_tas_decl_child (o.getBackendName l)  (o.chType.typeDefintionOrReference.longTypedefName l) (o.presentWhenName None l)
            let chEnms = children |> List.map(fun c -> c.presentWhenName None l)
            let childrenBodies = children |> List.map handleChild
            let nIndexMax = BigInteger ((Seq.length children)-1)
            header_a.CHOICE_tas_decl typeDefinitionName (children.Head.presentWhenName None l) childrenBodies chEnms nIndexMax childldrenCompleteDefintions
    let getExtraSubTypes sTypeDefinitionName soParentTypePackage sParentType =
        match l with
        | C     -> None
        | Ada   -> Some (header_a.Define_SubType_choice sTypeDefinitionName soParentTypePackage sParentType )
    createTypeGeneric r l pi t getExtraSubTypes (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createReferenceType (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType)  (baseType:Asn1Type ) (us:State) =
    let programUnit = ToC t.id.ModName
    match t.typeAssignmentInfo with
    | Some (TypeAssignmentInfo _)    
    | None                           -> 
        //
        match baseType.typeDefintionOrReference with
        | ReferenceToExistingDefinition  retEx  -> 
            let programUnit =
                match o.modName.Value = t.id.ModName with
                | true  -> None
                | false -> Some (ToC o.modName.Value)
            ReferenceToExistingDefinition  {ReferenceToExistingDefinition.programUnit = programUnit; typedefName=ToC2(r.args.TypePrefix + o.tasName.Value); definedInRtl = false}
        | TypeDefinition                   _    -> baseType.typeDefintionOrReference

    | Some (ValueAssignmentInfo _)   ->
        let soInheritParentTypePackage, sInheritParentType = 
            match l with
            | C     ->  None, (ToC2(r.args.TypePrefix + o.tasName.Value))
            | Ada   ->
                match ToC(o.modName.Value) = programUnit with
                | true  -> None, (ToC2(r.args.TypePrefix + o.tasName.Value))
                | false -> Some (ToC o.modName.Value), (ToC2(r.args.TypePrefix + o.tasName.Value))
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = soInheritParentTypePackage; typedefName=sInheritParentType; definedInRtl = false}
