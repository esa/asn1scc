module FE_TypeDefinition

open System
open System.Linq
open System.Numerics
open Antlr.Acn
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open AbstractMacros
open FsUtils

open Asn1AcnAst


let private reserveTypeDefinitionName  (typePrefix:string) (allocatedTypeNames : (ProgrammingLanguage*string*string)  list) (l:ProgrammingLanguage)  (programUnit:string) (proposedTypeDefName:string)  =
    let getNextCount (oldName:string) =
        match oldName.Split('_') |> Seq.toList |> List.rev with
        | []    
        | _::[]     -> oldName + "_1"
        | curN::oldPart   ->
            match Int32.TryParse curN with
            | true, num ->  (oldPart |> List.rev |> Seq.StrJoin "_") + "_" + ((num+1).ToString())
            | _         -> oldName + "_1"
    let rec getValidTypeDefname (proposedTypeDefName:string) = 
        let keywords =  l.keywords
        let cmp (l:ProgrammingLanguage) (s1:String) (s2:String) = l.cmp s1 s2
        let proposedTypeDefName =
            match keywords |> Seq.tryFind(fun kw -> cmp l proposedTypeDefName kw) with
            | None      -> proposedTypeDefName
            | Some _    -> getNextCount proposedTypeDefName

        match l.OnTypeNameConflictTryAppendModName with
        | true     ->  
            match allocatedTypeNames |> Seq.exists(fun (cl, _, ct) -> cl = l && ct = proposedTypeDefName) with
            | false -> proposedTypeDefName
            | true  -> 
                match allocatedTypeNames |> Seq.exists(fun (cl, cp, ct) -> cl = l && cp = programUnit && ct = proposedTypeDefName) with
                | false -> 
                    match proposedTypeDefName.StartsWith typePrefix with
                    | true  -> getValidTypeDefname (typePrefix + programUnit + "_" + proposedTypeDefName.Substring(typePrefix.Length) ) 
                    | false -> getValidTypeDefname (programUnit + "_" + proposedTypeDefName ) 
                | true  -> getValidTypeDefname (getNextCount proposedTypeDefName ) 
        | false   ->  
            match allocatedTypeNames  |> Seq.exists(fun (cl, cp, ct) -> cl = l && cp.ToUpper() = programUnit.ToUpper() && ct.ToUpper() = proposedTypeDefName.ToUpper()) with
            | false -> proposedTypeDefName
            | true  -> getValidTypeDefname (getNextCount proposedTypeDefName  ) 
    
    let validTypeDefname = getValidTypeDefname proposedTypeDefName 
    

    validTypeDefname, (l, programUnit, validTypeDefname)::allocatedTypeNames


let private reserveMasterTypeDefinitionName (us:Asn1AcnMergeState) (id:ReferenceToType) (l:ProgrammingLanguage)  (programUnit:string) (proposedTypeDefName:string)  =
    match us.temporaryTypesAllocation.TryFind(l,id) with
    | None  -> reserveTypeDefinitionName  us.args.TypePrefix us.allocatedTypeNames l programUnit proposedTypeDefName
    | Some typeName -> typeName, us.allocatedTypeNames

//returns the proposed type definition name, does not change the current state
let getProposedTypeDefName (us:Asn1AcnMergeState) l (id:ReferenceToType) =
    let lastNodeName, asn1LastName = 
        match id with
        | ReferenceToType path -> 
            match path |> List.rev |> List.head with
            | SEQ_CHILD name       -> name, name
            | CH_CHILD (name,_,_)    -> name, name
            | TA name              -> us.args.TypePrefix + name, name
            | SQF                   -> "elem", "elem"
            | _                             -> raise (BugErrorException "error in lastitem")
    let parentDef =
        match id with
        | ReferenceToType refIdNodes -> 
            match refIdNodes with
            | (MD modName)::(TA tasName)::[]    -> None
            | (MD modName)::(TA tasName)::_     ->
                let parentId = ReferenceToType (refIdNodes |> List.rev |> List.tail |> List.rev)
                us.allocatedFE_TypeDefinition.TryFind((l, parentId))
            | _                             -> raise (BugErrorException (sprintf "invalid reference to type %s"  id.AsString))

    match parentDef with
    | None              -> ToC lastNodeName, asn1LastName
    | Some  parentDef   -> ToC (parentDef.typeName + "_" + lastNodeName), parentDef.asn1Name + "-" + asn1LastName


let temporaryRegisterTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType)  programUnit proposedTypeDefName : (string*Asn1AcnMergeState)=
    let typeName, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix us.allocatedTypeNames l programUnit proposedTypeDefName
    typeName, {us with allocatedTypeNames = newAllocatedTypeNames; temporaryTypesAllocation = us.temporaryTypesAllocation.Add((l,id), typeName)}
    

/// Register the typeId 
let rec registerPrimitiveTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) getRtlDefinitionFunc : (FE_PrimitiveTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_PrimitiveTypeDefinition v)    -> 
        match kind with
        | FEI_NewSubTypeDefinition  subId   -> 
            match v.kind with
            | PrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerPrimitiveTypeDefinition us l subId FEI_NewTypeDefinition getRtlDefinitionFunc
                let newMap = ns1.allocatedFE_TypeDefinition.Remove (l,id)

                let itm = {v with kind = (PrimitiveNewSubTypeDefinition subType)}
                itm, {ns1 with allocatedFE_TypeDefinition = newMap.Add((l,id),(FE_PrimitiveTypeDefinition itm))}
        | _                             -> v, us
    | Some (_)    -> raise (BugErrorException "bug in registerPrimitiveTypeDefinition")
    | None      -> 
        let ret, ns =
            match kind with
            | FEI_Reference2RTL          -> 
                match getRtlDefinitionFunc with
                | None      -> raise(BugErrorException "kind is FE_Reference2RTL but no getRtlDefinitionFunc was provided")
                | Some fnc  -> 
                    let programUnit, typeName, asn1Name = fnc l
                    {FE_PrimitiveTypeDefinition.programUnit = programUnit; typeName = typeName; kind=PrimitiveReference2RTL; asn1Name = asn1Name; asn1Module =  None} , us
            | FEI_NewTypeDefinition      ->
                let proposedTypeDefName, asn1Name = getProposedTypeDefName us l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id l programUnit proposedTypeDefName
                let itm = {FE_PrimitiveTypeDefinition.programUnit = programUnit; typeName = typeName; kind=PrimitiveNewTypeDefinition; asn1Name = asn1Name; asn1Module =  Some id.ModName}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_PrimitiveTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerPrimitiveTypeDefinition us l subId FEI_NewTypeDefinition getRtlDefinitionFunc
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id l programUnit proposedTypeDefName
                let itm = {FE_PrimitiveTypeDefinition.programUnit = programUnit; typeName = typeName; kind=(PrimitiveNewSubTypeDefinition subType); asn1Name = asn1Name; asn1Module =  Some id.ModName }
                itm, {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_PrimitiveTypeDefinition itm))}
            | FEI_Reference2OtherType refId  -> 
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerPrimitiveTypeDefinition us l refId FEI_NewTypeDefinition getRtlDefinitionFunc
                let itm = {actDef with kind = PrimitiveReference2OtherType}
                itm, ns
                //itm, {ns with allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_PrimitiveTypeDefinition itm))}
                
        ret, ns


(************ STRING ***********************************)
let rec registerStringTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_StringTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_StringTypeDefinition v)    -> 
        match kind with
        | FEI_NewSubTypeDefinition  subId   -> 
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerStringTypeDefinition us l subId FEI_NewTypeDefinition 

                let newMap = ns1.allocatedFE_TypeDefinition.Remove (l,id)
                let itm = {v with kind = (NonPrimitiveNewSubTypeDefinition subType)}
                itm, {ns1 with allocatedFE_TypeDefinition = newMap.Add((l,id),(FE_StringTypeDefinition itm))}
        | _                             -> v, us
    | Some (_)    -> raise (BugErrorException "bug in registerPrimitiveTypeDefinition")
    | None      -> 
        let ret, ns =
            match kind with
            | FEI_Reference2RTL          -> raise(BugErrorException "String types are not defined in RTL")
            | FEI_NewTypeDefinition      ->
                let proposedTypeDefName, asn1Name = getProposedTypeDefName us l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id l programUnit proposedTypeDefName
                let encoding_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_alpha_index")
                let index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_index")
                let alpha, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_alpha")
                let alpha_set, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_alpha_set")
                let alpha_index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_alpha_index")
                let itm = {FE_StringTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; encoding_range=encoding_range; index=index; alpha_set=alpha_set; alpha=alpha; alpha_index=alpha_index}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_StringTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerStringTypeDefinition us l subId FEI_NewTypeDefinition 
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id l programUnit proposedTypeDefName
                let encoding_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_alpha_index")
                let index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_index")
                let alpha, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_alpha")
                let alpha_set, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_alpha_set")
                let alpha_index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_alpha_index")
                let itm = {FE_StringTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); encoding_range=encoding_range; index=index; alpha_set=alpha_set; alpha=alpha; alpha_index=alpha_index}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_StringTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  -> 
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerStringTypeDefinition us l refId FEI_NewTypeDefinition 
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns


(* Sizeable (OCTET STRING, BIT STRING, SEQUENCE OF)*)
let rec registerSizeableTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_SizeableTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_SizeableTypeDefinition v)    -> 
        match kind with
        | FEI_NewSubTypeDefinition  subId   -> 
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerSizeableTypeDefinition us l subId FEI_NewTypeDefinition 
                let newMap = ns1.allocatedFE_TypeDefinition.Remove (l,id)
                let itm = {v with kind = (NonPrimitiveNewSubTypeDefinition subType)}
                itm, {ns1 with allocatedFE_TypeDefinition = newMap.Add((l,id),(FE_SizeableTypeDefinition itm))}
        | _                             -> v, us
    | Some (_)    -> raise (BugErrorException "bug in registerPrimitiveTypeDefinition")
    | None      -> 
        let ret, ns =
            match kind with
            | FEI_Reference2RTL          -> raise(BugErrorException "String types are not defined in RTL")
            | FEI_NewTypeDefinition      ->
                let proposedTypeDefName, asn1Name = getProposedTypeDefName us l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id l programUnit proposedTypeDefName
                let index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_index")
                let array, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_array")
                let length_index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_length_index")
                let itm = {FE_SizeableTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; index=index; array=array; length_index=length_index}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_SizeableTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerSizeableTypeDefinition us l subId FEI_NewTypeDefinition 
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id l programUnit proposedTypeDefName
                let index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_index")
                let array, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_array")
                let length_index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_length_index")
                let itm = {FE_SizeableTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); index=index; array=array; length_index=length_index}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_SizeableTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  -> 
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerSizeableTypeDefinition us l refId FEI_NewTypeDefinition 
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns



let rec registerSequenceTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_SequenceTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_SequenceTypeDefinition v)    -> 
        match kind with
        | FEI_NewSubTypeDefinition  subId   -> 
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerSequenceTypeDefinition us l subId FEI_NewTypeDefinition 
                let newMap = ns1.allocatedFE_TypeDefinition.Remove (l,id)
                let itm = {v with kind = (NonPrimitiveNewSubTypeDefinition subType)}
                itm, {ns1 with allocatedFE_TypeDefinition = newMap.Add((l,id),(FE_SequenceTypeDefinition itm))}
        | _                             -> v, us
    | Some (_)    -> raise (BugErrorException "bug in registerPrimitiveTypeDefinition")
    | None      -> 
        let ret, ns =
            match kind with
            | FEI_Reference2RTL          -> raise(BugErrorException "String types are not defined in RTL")
            | FEI_NewTypeDefinition      ->
                let proposedTypeDefName, asn1Name = getProposedTypeDefName us l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id l programUnit proposedTypeDefName
                let exist, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_exist")
                let extention_function_potisions, newAllocatedTypeNames =
                    reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_extension_function_positions")
                let itm = {FE_SequenceTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; exist=exist; extention_function_potisions=extention_function_potisions}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_SequenceTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerSequenceTypeDefinition us l subId FEI_NewTypeDefinition 
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id l programUnit proposedTypeDefName
                let exist, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_exist")
                let extention_function_potisions, newAllocatedTypeNames =
                    reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_extension_function_positions")
                let itm = {FE_SequenceTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); exist=exist; extention_function_potisions=extention_function_potisions}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_SequenceTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  -> 
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerSequenceTypeDefinition us l refId FEI_NewTypeDefinition 
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns


let rec registerChoiceTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_ChoiceTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_ChoiceTypeDefinition v)    -> 
        match kind with
        | FEI_NewSubTypeDefinition  subId   -> 
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerChoiceTypeDefinition us l subId FEI_NewTypeDefinition 
                let newMap = ns1.allocatedFE_TypeDefinition.Remove (l,id)
                let itm = {v with kind = (NonPrimitiveNewSubTypeDefinition subType)}
                itm, {ns1 with allocatedFE_TypeDefinition = newMap.Add((l,id),(FE_ChoiceTypeDefinition itm))}
        | _                             -> v, us
    | Some (_)    -> raise (BugErrorException "bug in registerPrimitiveTypeDefinition")
    | None      -> 
        let ret, ns =
            match kind with
            | FEI_Reference2RTL          -> raise(BugErrorException "String types are not defined in RTL")
            | FEI_NewTypeDefinition      ->
                let proposedTypeDefName, asn1Name = getProposedTypeDefName us l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id l programUnit proposedTypeDefName
                let index_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_index_range")
                let selection, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_selection")
                let union_name, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_unchecked_union")
                let itm = {FE_ChoiceTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; index_range=index_range; selection=selection; union_name=union_name}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_ChoiceTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerChoiceTypeDefinition us l subId FEI_NewTypeDefinition 
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id l programUnit proposedTypeDefName
                let index_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_index_range")
                let selection, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_selection")
                let union_name, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_unchecked_union")
                let itm = {FE_ChoiceTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); index_range=index_range; selection=selection; union_name=union_name}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_ChoiceTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  -> 
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerChoiceTypeDefinition us l refId FEI_NewTypeDefinition 
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns


let rec registerEnumeratedTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_EnumeratedTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_EnumeratedTypeDefinition v)    -> 
        match kind with
        | FEI_NewSubTypeDefinition  subId   -> 
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerEnumeratedTypeDefinition us l subId FEI_NewTypeDefinition 
                let newMap = ns1.allocatedFE_TypeDefinition.Remove (l,id)
                let itm = {v with kind = (NonPrimitiveNewSubTypeDefinition subType)}
                itm, {ns1 with allocatedFE_TypeDefinition = newMap.Add((l,id),(FE_EnumeratedTypeDefinition itm))}
        | _                             -> v, us
    | Some (_)    -> raise (BugErrorException "bug in registerPrimitiveTypeDefinition")
    | None      -> 
        let ret, ns =
            match kind with
            | FEI_Reference2RTL          -> raise(BugErrorException "String types are not defined in RTL")
            | FEI_NewTypeDefinition      ->
                let proposedTypeDefName, asn1Name = getProposedTypeDefName us l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id l programUnit proposedTypeDefName
                let index_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_index_range")
                let itm = {FE_EnumeratedTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; index_range=index_range}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_EnumeratedTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerEnumeratedTypeDefinition us l subId FEI_NewTypeDefinition 
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id l programUnit proposedTypeDefName
                let index_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames l programUnit (proposedTypeDefName + "_index_range")
                let itm = {FE_EnumeratedTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); index_range=index_range}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_EnumeratedTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  -> 
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerEnumeratedTypeDefinition us l refId FEI_NewTypeDefinition 
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns


(*
let rec registerAnyTypeDefinition (asn1:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_TypeDefinition*Asn1AcnMergeState)=
    match (Asn1Ast.GetActualType t asn1).Kind with
    | Asn1Ast.Integer                  -> registerPrimitiveTypeDefinition us l id kind None  |> (fun (a,b) -> FE_PrimitiveTypeDefinition a, b)
    | Asn1Ast.Real                     -> registerPrimitiveTypeDefinition us l id kind None  |> (fun (a,b) -> FE_PrimitiveTypeDefinition a, b)
    | Asn1Ast.NullType                 -> registerPrimitiveTypeDefinition us l id kind None  |> (fun (a,b) -> FE_PrimitiveTypeDefinition a, b)
    | Asn1Ast.Boolean                  -> registerPrimitiveTypeDefinition us l id kind None  |> (fun (a,b) -> FE_PrimitiveTypeDefinition a, b)
    | Asn1Ast.Enumerated        _      -> registerEnumeratedTypeDefinition us l id kind      |> (fun (a,b) -> FE_EnumeratedTypeDefinition a, b)
    | Asn1Ast.OctetString              -> registerSizeableTypeDefinition us l id kind        |> (fun (a,b) -> FE_SizeableTypeDefinition a, b)
    | Asn1Ast.BitString                -> registerSizeableTypeDefinition us l id kind        |> (fun (a,b) -> FE_SizeableTypeDefinition a, b)
    | Asn1Ast.SequenceOf        _      -> registerSizeableTypeDefinition us l id kind        |> (fun (a,b) -> FE_SizeableTypeDefinition a, b)
    | Asn1Ast.NumericString            -> registerStringTypeDefinition us l id kind          |> (fun (a,b) -> FE_StringTypeDefinition a, b)
    | Asn1Ast.IA5String                -> registerStringTypeDefinition us l id kind          |> (fun (a,b) -> FE_StringTypeDefinition a, b)
    | Asn1Ast.Sequence          _      -> registerSequenceTypeDefinition us l id kind        |> (fun (a,b) -> FE_SequenceTypeDefinition a, b)
    | Asn1Ast.Choice            _      -> registerChoiceTypeDefinition us l id kind          |> (fun (a,b) -> FE_ChoiceTypeDefinition a, b)
    | Asn1Ast.ReferenceType     _      -> raise(BugErrorException "registerAnyTypeDefinition")
    *)

type GetTypeDifition_arg = {
    asn1TypeKind : Asn1Ast.Asn1TypeKind
    loc:SrcLoc 
    curPath : ScopeNode list 
    typeDefPath : ScopeNode list
    enmItemTypeDefPath : ScopeNode list
    inferitInfo : InheritanceInfo option
    typeAssignmentInfo : AssignmentInfo option
    rtlFnc : (ProgrammingLanguage -> (string*string*string)) option

    
}

let getTypedefKind (arg:GetTypeDifition_arg) =
    // first check if the type id is under a value assignment or type assignment
    match arg.curPath with
    | (MD _)::(VA _)::_ -> 
        //value assignment: The possible results are either reference to RTL or reference to other type (i.e. new types cannot be defined here)
        //There is a case where the typeDefPath is not correct (top level value assignments) to primitive types (see the mergeValueAssignment)
        match arg.typeDefPath   with
        | (MD _)::(VA _)::_ -> 
            match arg.asn1TypeKind with
            | Asn1Ast.Integer | Asn1Ast.Real | Asn1Ast.NullType | Asn1Ast.Boolean   -> FEI_Reference2RTL
            | _         -> raise(SemanticError(arg.loc, "Unnamed types are not supported in value assignments" ))
        | _     -> 
                match arg.curPath.Length > 2 && arg.rtlFnc.IsSome && arg.inferitInfo.IsNone with
                | true  -> FEI_Reference2RTL
                | false -> FEI_Reference2OtherType (ReferenceToType arg.typeDefPath)
    | _                 ->
        // type under a type assignment
        // when curPath  = typeDefPath then in most case it means a new type definition (or new subtype definition).
        // however if curpath is greater than 2 (i.e. child type) and type has rtlFnc then it a reference to RTL 
        match arg.curPath = arg.typeDefPath with
        | true  ->
            match arg.inferitInfo with
            | None  -> 
                match arg.curPath.Length > 2 && arg.rtlFnc.IsSome with
                | true  -> FEI_Reference2RTL
                | false -> FEI_NewTypeDefinition
            | Some inh -> FEI_NewSubTypeDefinition (ReferenceToType [MD inh.modName; TA inh.tasName])
        | false ->
            //In this case the curPath is different to typedefPath.
            //Normally this is a reference to another type. However, there are two exceptions
            //  (a) if type is child type and rtlFnc is some and inferitInfo.IsNone then is reference to RTL (instead of reference to other type)
            //  (b) if this type is type assignment (the case is A::=B) then we must define a new type (A) which has a subtype (B)
            match arg.typeAssignmentInfo with
            | Some (ValueAssignmentInfo   _)   
            | None  -> 
                match arg.curPath.Length > 2 && arg.rtlFnc.IsSome && arg.inferitInfo.IsNone with
                | true  -> FEI_Reference2RTL
                | false -> FEI_Reference2OtherType (ReferenceToType arg.typeDefPath)
                    //match arg.inferitInfo with
                    //| None -> FEI_Reference2OtherType (ReferenceToType arg.typeDefPath)
                    //| Some inh -> FEI_NewSubTypeDefinition (ReferenceToType [MD inh.modName; TA inh.tasName])
            | Some (TypeAssignmentInfo    tsInfo)   -> FEI_NewSubTypeDefinition (ReferenceToType arg.typeDefPath)



let getPrimitiveTypeDifition (arg:GetTypeDifition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, referece to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.AllLanguages |> foldMap (fun us l -> 
            let itm, ns = registerPrimitiveTypeDefinition us l (ReferenceToType arg.curPath) typedefKind arg.rtlFnc 
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getStringTypeDifition (arg:GetTypeDifition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, referece to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.AllLanguages |> foldMap (fun us l -> 
            let itm, ns = registerStringTypeDefinition us l (ReferenceToType arg.curPath) typedefKind 
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getSizeableTypeDifition (arg:GetTypeDifition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, referece to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.AllLanguages |> foldMap (fun us l -> 
            let itm, ns = registerSizeableTypeDefinition us l (ReferenceToType arg.curPath) typedefKind 
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1


let getSequenceTypeDifition (arg:GetTypeDifition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, referece to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.AllLanguages |> foldMap (fun us l -> 
            let itm, ns = registerSequenceTypeDefinition us l (ReferenceToType arg.curPath) typedefKind 
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getChoiceTypeDifition (arg:GetTypeDifition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, referece to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.AllLanguages |> foldMap (fun us l -> 
            let itm, ns = registerChoiceTypeDefinition us l (ReferenceToType arg.curPath) typedefKind 
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getEnumeratedTypeDifition (arg:GetTypeDifition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, referece to other type etc)
    let typedefKind = getTypedefKind arg
    //let typedefKindEmnItem = getTypedefKind {arg with typeDefPath=arg.enmItemTypeDefPath}
    let lanDefs, us1 =
        ProgrammingLanguage.AllLanguages |> foldMap (fun us l -> 
            let itm, ns = registerEnumeratedTypeDefinition us l (ReferenceToType arg.curPath) typedefKind 
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getRefereceTypeDefinition (asn1:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (arg:GetTypeDifition_arg) (us:Asn1AcnMergeState) =
    
    match (Asn1Ast.GetActualType t asn1).Kind with
    | Asn1Ast.Integer                  -> getPrimitiveTypeDifition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.ObjectIdentifier         -> getPrimitiveTypeDifition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.RelativeObjectIdentifier -> getPrimitiveTypeDifition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Real                     -> getPrimitiveTypeDifition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.NullType                 -> getPrimitiveTypeDifition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.TimeType _               -> getPrimitiveTypeDifition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Boolean                  -> getPrimitiveTypeDifition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Enumerated        _      -> getEnumeratedTypeDifition arg us  |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_EnumeratedTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.OctetString              -> getSizeableTypeDifition arg us    |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.BitString   _            -> getSizeableTypeDifition arg us    |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.SequenceOf        _      -> getSizeableTypeDifition arg us    |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.NumericString            -> getStringTypeDifition arg us      |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_StringTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.IA5String                -> getStringTypeDifition arg us      |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_StringTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Sequence          _      -> getSequenceTypeDifition arg us    |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_SequenceTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Choice            _      -> getChoiceTypeDifition arg us      |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_ChoiceTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.ReferenceType     _      -> raise(BugErrorException "getRefereceTypeDefinition")
