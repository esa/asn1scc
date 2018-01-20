﻿module DAstTypeDefinition2

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



let getPotentialTypedefName (r:AstRoot) (t:Asn1Type)  (potentialTypedefName:string)   =
    t.newTypeDefName        
    

let createPrmAcnInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  =
    let Declare_Integer     =  match l with  C  -> header_c.Declare_Integer  | Ada   -> header_a.Declare_Integer    | Python -> types_p.Declare_Integer
    Declare_Integer ()

let createAcnInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (a:Asn1AcnAst.AcnInteger) =
    let Declare_Integer     =  match l with  C  -> header_c.Declare_Integer  | Ada   -> header_a.Declare_Integer        | Python -> types_p.Declare_Integer
    let Declare_PosInteger  =  match l with  C  -> header_c.Declare_PosInteger  | Ada   -> header_a.Declare_PosInteger  | Python -> types_p.Declare_PosInteger 
    match a.isUnsigned with
    | true     -> Declare_PosInteger ()
    | false    -> Declare_Integer ()
        
    
let createAcnBoolean (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) =
    match l with
    | C                      -> header_c.Declare_Boolean ()
    | Ada                    -> header_a.Declare_BOOLEAN ()
    | Python                 -> types_p.Declare_Boolean ()    

let createAcnNull (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) =
    match l with
    | C                      -> header_c.Declare_NullType ()
    | Ada                    -> header_a.Declare_NULL ()
    | Python                 -> types_p.Declare_NullType ()



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

let getTypedefName (r:Asn1AcnAst.AstRoot) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)   =
    match t.typeAssignmentInfo with
    | Some (TypeAssignmentInfo tasInfo)       ->  ToC2(r.args.TypePrefix + tasInfo.tasName) // I am a type assignment
    | Some (ValueAssignmentInfo vasInfo)      ->  ToC2(r.args.TypePrefix + vasInfo.vasName) // I am a type assignment
    | None              ->  // I am an inner type
        match t.inheritInfo with
        | Some inhInfo      -> (*I am a reference type*) ToC2(r.args.TypePrefix + inhInfo.tasName)
        | None              -> 
            match pi with
            | Some parentInfo   ->
                match parentInfo.name with
                | Some nm -> ToC2(parentInfo.parentData.typedefName + "_" + nm)
                | None    -> ToC2(parentInfo.parentData.typedefName + "_" + "elem")
            | None              ->
                raise(BugErrorException "type has no typeAssignmentInfo and No parent!!!")
                


let private createTypeGeneric (r:Asn1AcnAst.AstRoot)  l (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (defineNewType:DefineTypeAux)   =
    let programUnit = ToC t.id.ModName
    let rtlModuleName  = match l with C -> None                     | Ada -> Some (header_a.rtlModuleName())    | Python -> None
    let defineSubType l = match l with C -> header_c.Define_SubType | Ada -> header_a.Define_SubType            | Python -> types_p.Define_SubType
    
    let defineSubTypeAux (programUnit:string) (typedefName:string) (inheritInfo : InheritanceInfo option) (subAux:DefineSubTypeAux) (innerType:bool) =
        let soInheritParentTypePackage, sInheritParentType = 
            match inheritInfo with
            | None      -> rtlModuleName, subAux.getRtlTypeName()
            | Some inhInfo  ->
                match l with
                | C     ->  None, (ToC2(r.args.TypePrefix + inhInfo.tasName))
                | Ada   ->
                    match ToC(inhInfo.modName) = programUnit with
                    | true  -> None, (ToC2(r.args.TypePrefix + inhInfo.tasName))
                    | false -> Some (ToC inhInfo.modName), (ToC2(r.args.TypePrefix + inhInfo.tasName))
                | Python->  None, (ToC2(r.args.TypePrefix + inhInfo.tasName))
        let soNewRange = subAux.getNewRange soInheritParentTypePackage sInheritParentType
        match soNewRange with
        | None when  innerType    -> (*If there is no new range and is an inner type, then just make a reference to existing type*)
            ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = soInheritParentTypePackage; typedefName=sInheritParentType}         
        | _     -> (*Otherwise, create a new type which is a subtype of the existing type*)
            let completeDefintion = defineSubType l typedefName soInheritParentTypePackage sInheritParentType soNewRange
            TypeDefinition {TypeDefinition.typedefName = typedefName; typedefBody = (fun () -> completeDefintion)}
    
    match t.typeAssignmentInfo with
    | Some (TypeAssignmentInfo tasInfo)      ->  (*I am a type assignmet ==> Always define a new type*)
        let typedefName = (ToC2(r.args.TypePrefix + tasInfo.tasName))
        match t.inheritInfo with
        | Some inheritInfo  when not inheritInfo.hasAdditionalConstraints -> 
            let otherOptherProgrmaUnit = if  inheritInfo.modName = tasInfo.modName then None else Some (ToC inheritInfo.modName)
            let typedefBody = defineSubType l typedefName otherOptherProgrmaUnit (ToC2(r.args.TypePrefix + inheritInfo.tasName)) None
            TypeDefinition {TypeDefinition.typedefName=typedefName; typedefBody = (fun () -> typedefBody)}
        | _             -> 
            match defineNewType with
            | DefineSubTypeAux subAux -> 
                defineSubTypeAux programUnit typedefName t.inheritInfo subAux false
            | DefineNewTypeAux ntAux  ->
                let completeDefintion = ntAux.getCompleteDefintion  programUnit typedefName 
                TypeDefinition {TypeDefinition.typedefName = typedefName; typedefBody = (fun () -> completeDefintion)}
    | Some (ValueAssignmentInfo vasInfo)      ->  (*I am a value assignmet ==> Reference an existing or throw a user exception*)
        match t.inheritInfo with
        | Some inheritInfo   ->  
            let baseTypeProgramUnit = if programUnit = ToC inheritInfo.modName then None else Some (ToC inheritInfo.modName)
            ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = baseTypeProgramUnit; typedefName=ToC2(r.args.TypePrefix + inheritInfo.tasName)}
        | None   -> 
            match defineNewType with
            | DefineSubTypeAux subAux -> 
                let soInheritParentTypePackage, sInheritParentType =  rtlModuleName, subAux.getRtlTypeName()
                ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = soInheritParentTypePackage; typedefName=sInheritParentType}         
            | DefineNewTypeAux ntAux  ->
                raise(SemanticError(t.Location, "Anonymous types are not supported. Please define a new type and then use it in the value assignment"))
    | None              -> (*I am a SEQUENCE or SEQUENCE OF or CHOICE child.*)
        let typedefName = 
            match pi with
            | Some parentInfo   ->
                match parentInfo.name with
                | Some nm -> ToC2(parentInfo.parentData.typedefName + "_" + nm)
                | None    -> ToC2(parentInfo.parentData.typedefName + "_" + "elem")
            | None              ->
                raise(BugErrorException "type has no typeAssignmentInfo and No parent!!!")

        match defineNewType with
        | DefineSubTypeAux subAux -> 
            defineSubTypeAux programUnit typedefName t.inheritInfo subAux true
        | DefineNewTypeAux ntAux  ->
            match t.inheritInfo with
            //| Some inheritInfo  when not inheritInfo.hasAdditionalConstraints ->  //when enabled test cases 14-RealCases/NPAL.asn1 fails
            | Some inheritInfo   ->  
                
                let baseTypeProgramUnit = if programUnit = ToC inheritInfo.modName then None else Some (ToC inheritInfo.modName)
                ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = baseTypeProgramUnit; typedefName=ToC2(r.args.TypePrefix + inheritInfo.tasName)}
            | _   -> 
                let completeDefintion = ntAux.getCompleteDefintion  programUnit typedefName 
                TypeDefinition {TypeDefinition.typedefName = typedefName; typedefBody = (fun () -> completeDefintion)}


(*
Primitive types with base definition in RTL which can be used as is generated code (Integer, Real, Boolean, NULL).

These types are defined as sub types.
*)
    
let createInteger (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Integer)   (us:State) =
    let declare_IntegerNoRTL            = match l with C -> header_c.Declare_Integer                    | Ada -> header_a.Declare_IntegerNoRTL      | Python -> types_p.Declare_Integer
    let declare_PosIntegerNoRTL         = match l with C -> header_c.Declare_PosInteger                 | Ada -> header_a.Declare_PosIntegerNoRTL   | Python -> types_p.Declare_PosInteger
    let define_SubType_int_range        = match l with C -> (fun _ _ _ _  -> "")                        | Ada -> header_a.Define_SubType_int_range  | Python -> (fun _ _ _ _  -> "")

    let getNewRange soInheritParentTypePackage sInheritParentType = 
        match o.uperRange with
        | Concrete(a,b)               ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) (Some b))
        | NegInf (b)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType None (Some b))
        | PosInf (a)  when a=0I       ->  None
        | PosInf (a)                  ->  Some (define_SubType_int_range soInheritParentTypePackage sInheritParentType (Some a) None)
        | Full                        ->  None
    let getRtlTypeName () = if o.isUnsigned then declare_PosIntegerNoRTL() else declare_IntegerNoRTL()
    createTypeGeneric r l pi t (DefineSubTypeAux {DefineSubTypeAux.getNewRange = getNewRange; getRtlTypeName = getRtlTypeName})

let createBoolean (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Boolean)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_Boolean  | Ada -> header_a.Declare_BOOLEANNoRTL  | Python -> types_p.Declare_Boolean
    createTypeGeneric r l pi t (DefineSubTypeAux {DefineSubTypeAux.getNewRange = (fun _ _ -> None); getRtlTypeName = getRtlTypeName})

let createReal (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Real)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_Real  | Ada -> header_a.Declare_REALNoRTL  | Python -> types_p.Declare_Real
    createTypeGeneric r l pi t (DefineSubTypeAux {DefineSubTypeAux.getNewRange = (fun _ _ -> None); getRtlTypeName = getRtlTypeName})

let createNull (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.NullType)   (us:State) =
    let getRtlTypeName  = match l with C -> header_c.Declare_NullType  | Ada -> header_a.Declare_NULLNoRTL  | Python -> types_p.Declare_NullType
    createTypeGeneric r l pi t (DefineSubTypeAux {DefineSubTypeAux.getNewRange = (fun _ _ -> None); getRtlTypeName = getRtlTypeName})

(*
Primitive types with NO base definition in RTL (IA5String, OCTET STRING, BIT STRING, ENUMERATED)
*)



let createString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  (us:State) =
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let typeDefinitionBody =header_c.Declare_IA5String ()
            header_c.Define_Type typeDefinitionBody typeDefinitionName (Some (BigInteger (o.maxSize+1))) []
        | Ada                    -> 
            header_a.IA5STRING_OF_tas_decl typeDefinitionName (BigInteger o.minSize) (BigInteger o.maxSize) (BigInteger (o.maxSize + 1)) (o.uperCharSet |> Array.map(fun c -> (BigInteger (int c))))
        | Python                 -> 
            let typeDefinitionBody                       = 
                match t.Kind with
                | Asn1AcnAst.IA5String _        -> types_p.Declare_IA5String ()
                | Asn1AcnAst.NumericString _    -> types_p.Declare_NumericString ()
                | _                             -> ""
            types_p.Define_Type typeDefinitionBody typeDefinitionName (Some (BigInteger (o.maxSize+1))) []
    createTypeGeneric r l pi t (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createOctet (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString)  (us:State) =
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let typeDefinitionBody = header_c.Declare_OctetString (o.minSize=o.maxSize) (BigInteger o.maxSize)
            header_c.Define_Type typeDefinitionBody typeDefinitionName None []
        | Ada                    -> 
            header_a.OCTET_STRING_tas_decl typeDefinitionName (BigInteger o.minSize) (BigInteger o.maxSize) (o.maxSize=o.minSize)
        | Python                 -> 
            let typeDefinitionBody = types_p.Declare_OctetString (o.minSize=o.maxSize) (BigInteger o.maxSize)
            types_p.Define_Type typeDefinitionBody typeDefinitionName None []
    createTypeGeneric r l pi t (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createBitString (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString)   (us:State) =
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let typeDefinitionBody = header_c.Declare_BitString (o.minSize=o.maxSize) (BigInteger o.MaxOctets) (BigInteger o.maxSize)
            header_c.Define_Type typeDefinitionBody typeDefinitionName None []
        | Ada                    -> 
            header_a.BIT_STRING_tas_decl typeDefinitionName (BigInteger o.minSize) (BigInteger o.maxSize) (o.maxSize=o.minSize)
        | Python                 -> 
            let typeDefinitionBody = types_p.Declare_BitString ()
            types_p.Define_Type typeDefinitionBody typeDefinitionName None []
    createTypeGeneric r l pi t (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createEnumerated (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated)  (us:State) =
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      -> 
            let items = 
                match o.userDefinedValues with
                | true  -> o.items |> List.map( fun i -> header_c.PrintNamedItem (i.getBackendName l) i.definitionValue)
                | false -> o.items |> List.map( fun i -> i.getBackendName l)
            let typeDefinitionBody = header_c.Declare_Enumerated items
            header_c.Define_Type typeDefinitionBody typeDefinitionName None []
        | Ada                    -> 
            let orderedItems = o.items |> List.sortBy(fun i -> i.definitionValue)
            let arrsEnumNames = orderedItems |> List.map( fun i -> i.getBackendName l)
            let arrsEnumNamesAndValues = orderedItems |> List.map( fun i -> header_a.ENUMERATED_tas_decl_item (i.getBackendName l) i.definitionValue)
            let nIndexMax = BigInteger ((Seq.length o.items)-1)
            header_a.ENUMERATED_tas_decl typeDefinitionName arrsEnumNames arrsEnumNamesAndValues nIndexMax
        | Python                 -> 
            let items = o.items |> List.map( fun i -> types_p.PrintEnumItem (i.getBackendName l) i.definitionValue)
            let itemNames = o.items |> List.map( fun i -> (i.getBackendName l))
            let typeDefinitionBody = types_p.Declare_Enumerated items itemNames
            types_p.Define_Type typeDefinitionBody typeDefinitionName None []
    createTypeGeneric r l pi t (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

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
            let typeDefinitionBody = header_c.Declare_SequenceOf (o.minSize = o.maxSize) (childDefinition.longTypedefName l) (BigInteger o.maxSize) ""
            header_c.Define_Type typeDefinitionBody typeDefinitionName None (getChildDefinition childDefinition)
        | Ada                    -> 
            header_a.SEQUENCE_OF_tas_decl typeDefinitionName (BigInteger o.minSize) (BigInteger o.maxSize) (o.minSize = o.maxSize) (childDefinition.longTypedefName l) (getChildDefinition childDefinition)
        | Python                 -> 
            let typeDefinitionBody = types_p.Declare_SequenceOf (childDefinition.longTypedefName l) ""
            types_p.Define_Type typeDefinitionBody typeDefinitionName None (getChildDefinition childDefinition)
    createTypeGeneric r l pi t (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})


let createSequence (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence)  (children:SeqChildInfo list) (us:State) =
    let children = children |> List.choose (fun c -> match c with Asn1Child z -> Some z | _ -> None)
    let childldrenCompleteDefintions = children |> List.collect (fun c -> getChildDefinition c.Type.typeDefintionOrReference)
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      ->
            let handleChild (o:Asn1Child) = header_c.PrintSeq_ChoiceChild (o.Type.typeDefintionOrReference.longTypedefName l ) o.c_name ""
            let childrenBodies = children |> List.map handleChild
            let optChildNames  = children |> List.choose(fun c -> match c.Optionality with Some _ -> Some c.Name.Value | None -> None)
            let typeDefinitionBody = header_c.Declare_Sequence childrenBodies optChildNames
            header_c.Define_Type typeDefinitionBody typeDefinitionName None childldrenCompleteDefintions
        | Ada                    -> 
            let handleChild (o:Asn1Child) = header_a.SEQUENCE_tas_decl_child o.c_name (o.Type.typeDefintionOrReference.longTypedefName l)
            let childrenBodies = children |> List.map handleChild
            let optChildren  = children |> List.choose(fun c -> match c.Optionality with Some _ -> Some(header_a.SEQUENCE_tas_decl_child_bit c.Name.Value) | None -> None)
            header_a.SEQUENCE_tas_decl typeDefinitionName childrenBodies optChildren childldrenCompleteDefintions
        | Python                 ->
            let handleChild (o:Asn1Child) = types_p.PrintChoiceSeq_Child (o.Type.typeDefintionOrReference.longTypedefName l ) o.c_name
            let childrenBodies = children |> List.map handleChild
            let childrenNames = children |> List.map(fun c -> ToCPy (c.Name.Value))
            let optChildNames  = children |> List.choose(fun c -> match c.Optionality with Some _ -> Some c.Name.Value | None -> None)
            let typeDefinitionBody = types_p.Declare_Sequence childrenBodies optChildNames childrenNames
            types_p.Define_Type typeDefinitionBody typeDefinitionName None childldrenCompleteDefintions
    createTypeGeneric r l pi t (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createChoice (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice)  (children:ChChildInfo list) (us:State) =
    let childldrenCompleteDefintions = children |> List.collect (fun c -> getChildDefinition c.chType.typeDefintionOrReference)
    let getCompleteDefinition (programUnit:string) (typeDefinitionName:string) =
        match l with
        | C                      ->
            let handleChild (o:ChChildInfo) = header_c.PrintSeq_ChoiceChild (o.chType.typeDefintionOrReference.longTypedefName l) o.c_name ""
            let chEnms = children |> List.map(fun c -> c.presentWhenName)
            let childrenBodies = children |> List.map handleChild
            let typeDefinitionBody = header_c.Declare_Choice (choiceIDForNone t.id) chEnms childrenBodies 
            header_c.Define_Type typeDefinitionBody typeDefinitionName None childldrenCompleteDefintions
        | Ada                    -> 
            let handleChild (o:ChChildInfo) = header_a.CHOICE_tas_decl_child o.c_name  (o.chType.typeDefintionOrReference.longTypedefName l) o.presentWhenName
            let chEnms = children |> List.map(fun c -> c.presentWhenName)
            let childrenBodies = children |> List.map handleChild
            let nIndexMax = BigInteger ((Seq.length children)-1)
            header_a.CHOICE_tas_decl typeDefinitionName children.Head.presentWhenName childrenBodies chEnms nIndexMax childldrenCompleteDefintions
        | Python                 ->
            let handleChild (o:ChChildInfo) = types_p.PrintChoiceSeq_Child (o.chType.typeDefintionOrReference.longTypedefName l) o.c_name
            let childrenBodies = children |> List.map handleChild
            let childrenNames = children |> List.map(fun c -> ToCPy (c.Name.Value))
            let typeDefinitionBody = types_p.Declare_Choice childrenBodies childrenNames 
            types_p.Define_Type typeDefinitionBody typeDefinitionName None childldrenCompleteDefintions
    createTypeGeneric r l pi t (DefineNewTypeAux {DefineNewTypeAux.getCompleteDefintion = getCompleteDefinition})

let createReferenceType (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType)  (baseType:Asn1Type ) (us:State) =
    baseType.typeDefintionOrReference