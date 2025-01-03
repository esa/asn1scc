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
open Language
open Asn1AcnAstUtilFunctions

let private reserveTypeDefinitionName  (typePrefix:string) (allocatedTypeNames : Map<(ProgrammingLanguage*string), string list>) (l:ProgrammingLanguage, ib:ILangBasic)  (programUnit:string) (proposedTypeDefName:string) : (string*Map<(ProgrammingLanguage*string), string list>)  =
    let getNextCount (oldName:string) =
        match oldName.Split('_') |> Seq.toList |> List.rev with
        | []
        | _::[]     -> oldName + "_1"
        | curN::oldPart   ->
            match Int32.TryParse curN with
            | true, num ->  (oldPart |> List.rev |> Seq.StrJoin "_") + "_" + ((num+1).ToString())
            | _         -> oldName + "_1"
    let programUnit_for_map = match ib.isCaseSensitive with true -> programUnit | false -> programUnit.ToUpper()
    let addAllocatedTypeNames (allocatedTypeNames : Map<(ProgrammingLanguage*string), string list>) (l:ProgrammingLanguage) (programUnit:string) (proposedTypeDefName:string) =
        let key= 
            match ib.isCaseSensitive with
            | true  -> (l, proposedTypeDefName)
            | false -> (l, proposedTypeDefName.ToUpper())
        
            
        match allocatedTypeNames.TryFind key with
        | None -> allocatedTypeNames.Add (key, [programUnit_for_map])
        | Some (allocatedModules) -> allocatedTypeNames.Add (key, programUnit_for_map::allocatedModules)

    let rec getValidTypeDefname (proposedTypeDefName: string) =
        let keywords =  ib.keywords
        //let cmp (ib:ILangBasic) (s1:String) (s2:String) = ib.cmp s1 s2
        let proposedTypeDefName =
            //match keywords |> Seq.tryFind(fun kw -> cmp ib proposedTypeDefName kw) with
            match ib.isKeyword proposedTypeDefName with
            | false      -> proposedTypeDefName
            | true    -> getNextCount proposedTypeDefName
        let key  =
            match ib.isCaseSensitive with
            | true  -> (l, proposedTypeDefName)
            | false -> (l, proposedTypeDefName.ToUpper())
        
        match ib.OnTypeNameConflictTryAppendModName with
        | true     ->
            //match allocatedTypeNames |> Set.exists(fun (cl, _, ct) -> cl = l && ct = proposedTypeDefName) with
            match allocatedTypeNames.TryFind key with
            | None -> proposedTypeDefName
            | Some (allocatedModules)  ->
                //match allocatedTypeNames |> Set.exists(fun (cl, cp, ct) -> cl = l && cp = programUnit && ct = proposedTypeDefName) with
                match allocatedModules |> List.exists(fun z -> z = programUnit_for_map) with
                | false ->
                    match proposedTypeDefName.StartsWith typePrefix with
                    | true  -> getValidTypeDefname (typePrefix + programUnit + "_" + proposedTypeDefName.Substring(typePrefix.Length) )
                    | false -> getValidTypeDefname (programUnit + "_" + proposedTypeDefName )
                | true  -> getValidTypeDefname (getNextCount proposedTypeDefName )
        | false   ->
            //match allocatedTypeNames  |> Set.exists(fun (cl, cp, ct) -> cl = l && cp.ToUpper() = programUnit.ToUpper() && ct.ToUpper() = proposedTypeDefName.ToUpper()) with
            match allocatedTypeNames.TryFind key with
            | None -> proposedTypeDefName
            | Some (allocatedModules)  ->  
                match allocatedModules |> List.exists(fun z -> z = programUnit_for_map) with
                | false -> proposedTypeDefName
                | true  -> getValidTypeDefname (getNextCount proposedTypeDefName  )

    let validTypeDefname = getValidTypeDefname proposedTypeDefName
    validTypeDefname, (addAllocatedTypeNames allocatedTypeNames l programUnit validTypeDefname)

let private reserveMasterTypeDefinitionName (us:Asn1AcnMergeState) (id:ReferenceToType) (l:ProgrammingLanguage, ib:ILangBasic)  (programUnit:string) (proposedTypeDefName:string)  =
    match us.temporaryTypesAllocation.TryFind(l,id) with
    | None  -> reserveTypeDefinitionName  us.args.TypePrefix us.allocatedTypeNames (l,ib) programUnit proposedTypeDefName
    | Some typeName -> typeName, us.allocatedTypeNames

//returns the proposed type definition name, does not change the current state
let getProposedTypeDefName (us:Asn1AcnMergeState) l (id:ReferenceToType) =
    let lastNodeName, asn1LastName =
        match id with
        | ReferenceToType path ->
            match path |> List.rev |> List.head with
            | SEQ_CHILD (name,_) -> name, name
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


let temporaryRegisterTypeDefinition (us:Asn1AcnMergeState) (l:ProgrammingLanguage, ib:ILangBasic) (id : ReferenceToType)  programUnit proposedTypeDefName : (string*Asn1AcnMergeState)=
    let typeName, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix us.allocatedTypeNames (l,ib) programUnit proposedTypeDefName
    typeName, {us with allocatedTypeNames = newAllocatedTypeNames; temporaryTypesAllocation = us.temporaryTypesAllocation.Add((l,id), typeName)}

/// Register the typeId
let rec registerPrimitiveTypeDefinition (us: Asn1AcnMergeState) (l: ProgrammingLanguage, ib: ILangBasic) (id: ReferenceToType) (kind: FE_TypeDefinitionKindInternal) getRtlDefinitionFunc: (FE_PrimitiveTypeDefinition*Asn1AcnMergeState) =
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_PrimitiveTypeDefinition v)    ->
        match kind with
        | FEI_NewSubTypeDefinition  subId   ->
            match v.kind with
            | PrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerPrimitiveTypeDefinition us (l,ib) subId FEI_NewTypeDefinition getRtlDefinitionFunc
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
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id (l,ib) programUnit proposedTypeDefName
                let itm = {FE_PrimitiveTypeDefinition.programUnit = programUnit; typeName = typeName; kind=PrimitiveNewTypeDefinition; asn1Name = asn1Name; asn1Module =  Some id.ModName}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_PrimitiveTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerPrimitiveTypeDefinition us (l,ib) subId FEI_NewTypeDefinition getRtlDefinitionFunc
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id (l,ib) programUnit proposedTypeDefName
                let itm = {FE_PrimitiveTypeDefinition.programUnit = programUnit; typeName = typeName; kind=(PrimitiveNewSubTypeDefinition subType); asn1Name = asn1Name; asn1Module =  Some id.ModName }
                itm, {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_PrimitiveTypeDefinition itm))}
            | FEI_Reference2OtherType refId  ->
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerPrimitiveTypeDefinition us (l,ib) refId FEI_NewTypeDefinition getRtlDefinitionFunc
                let itm = {actDef with kind = PrimitiveReference2OtherType}
                itm, ns
                //itm, {ns with allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_PrimitiveTypeDefinition itm))}

        ret, ns


(************ STRING ***********************************)
let rec registerStringTypeDefinition (us:Asn1AcnMergeState) (l:ProgrammingLanguage, ib:ILangBasic) (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_StringTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_StringTypeDefinition v)    ->
        match kind with
        | FEI_NewSubTypeDefinition  subId   ->
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerStringTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
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
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id (l,ib) programUnit proposedTypeDefName
                let encoding_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_alpha_index")
                let index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_index")
                let alpha, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_alpha")
                let alpha_set, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_alpha_set")
                let alpha_index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_alpha_index")
                let itm = {FE_StringTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; encoding_range=encoding_range; index=index; alpha_set=alpha_set; alpha=alpha; alpha_index=alpha_index}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_StringTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerStringTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id (l,ib) programUnit proposedTypeDefName
                let encoding_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_alpha_index")
                let index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_index")
                let alpha, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_alpha")
                let alpha_set, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_alpha_set")
                let alpha_index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_alpha_index")
                let itm = {FE_StringTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); encoding_range=encoding_range; index=index; alpha_set=alpha_set; alpha=alpha; alpha_index=alpha_index}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_StringTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  ->
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerStringTypeDefinition us (l,ib) refId FEI_NewTypeDefinition
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns

(* Sizeable (OCTET STRING, BIT STRING, SEQUENCE OF)*)
let rec registerSizeableTypeDefinition (us:Asn1AcnMergeState) (l:ProgrammingLanguage, ib:ILangBasic) (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_SizeableTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_SizeableTypeDefinition v)    ->
        match kind with
        | FEI_NewSubTypeDefinition  subId   ->
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerSizeableTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
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
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id (l,ib) programUnit proposedTypeDefName
                let index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_index")
                let array, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_array")
                let length_index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_length_index")
                let itm = {FE_SizeableTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; index=index; array=array; length_index=length_index}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_SizeableTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerSizeableTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id (l,ib) programUnit proposedTypeDefName
                let index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_index")
                let array, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_array")
                let length_index, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_length_index")
                let itm = {FE_SizeableTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); index=index; array=array; length_index=length_index}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_SizeableTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  ->
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerSizeableTypeDefinition us (l,ib) refId FEI_NewTypeDefinition
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns


let rec registerSequenceTypeDefinition (us:Asn1AcnMergeState) (l:ProgrammingLanguage, ib:ILangBasic) (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_SequenceTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_SequenceTypeDefinition v)    ->
        match kind with
        | FEI_NewSubTypeDefinition  subId   ->
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerSequenceTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
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
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id (l,ib) programUnit proposedTypeDefName
                let exist, newAllocatedTypeNames = reserveTypeDefinitionName us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_exist")
                let extension_function_positions, newAllocatedTypeNames =
                    reserveTypeDefinitionName us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_extension_function_positions")
                let itm = {FE_SequenceTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module = Some id.ModName; kind=NonPrimitiveNewTypeDefinition; exist=exist; extension_function_positions=extension_function_positions}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_SequenceTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerSequenceTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id (l,ib) programUnit proposedTypeDefName
                let exist, newAllocatedTypeNames = reserveTypeDefinitionName us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_exist")
                let extension_function_positions, newAllocatedTypeNames =
                    reserveTypeDefinitionName us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_extension_function_positions")
                let itm = {FE_SequenceTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module = Some id.ModName; kind = (NonPrimitiveNewSubTypeDefinition subType); exist = exist; extension_function_positions=extension_function_positions}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_SequenceTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  ->
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerSequenceTypeDefinition us (l,ib) refId FEI_NewTypeDefinition
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns


let rec registerChoiceTypeDefinition (us:Asn1AcnMergeState) (l:ProgrammingLanguage, ib:ILangBasic) (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_ChoiceTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_ChoiceTypeDefinition v)    ->
        match kind with
        | FEI_NewSubTypeDefinition  subId   ->
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerChoiceTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
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
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id (l,ib) programUnit proposedTypeDefName
                let index_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_index_range")
                let selection, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_selection")
                let union_name, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_unchecked_union")
                let itm = {FE_ChoiceTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; index_range=index_range; selection=selection; union_name=union_name}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_ChoiceTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerChoiceTypeDefinition us (l, ib) subId FEI_NewTypeDefinition
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id (l,ib) programUnit proposedTypeDefName
                let index_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_index_range")
                let selection, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_selection")
                let union_name, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_unchecked_union")
                let itm = {FE_ChoiceTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); index_range=index_range; selection=selection; union_name=union_name}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_ChoiceTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  ->
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerChoiceTypeDefinition us (l, ib) refId FEI_NewTypeDefinition
                let itm = {actDef with kind = NonPrimitiveReference2OtherType}
                itm, ns
        ret, ns

let rec registerEnumeratedTypeDefinition (us:Asn1AcnMergeState) (l:ProgrammingLanguage, ib:ILangBasic) (id : ReferenceToType) (kind : FE_TypeDefinitionKindInternal) : (FE_EnumeratedTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_EnumeratedTypeDefinition v)    ->
        match kind with
        | FEI_NewSubTypeDefinition  subId   ->
            match v.kind with
            | NonPrimitiveNewSubTypeDefinition _  -> v, us
            | _ ->
                // fix early main type allocation
                let subType, ns1 = registerEnumeratedTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
                let newMap = ns1.allocatedFE_TypeDefinition.Remove (l,id)
                let itm = {v with kind = (NonPrimitiveNewSubTypeDefinition subType)}
                itm, {ns1 with allocatedFE_TypeDefinition = newMap.Add((l,id),(FE_EnumeratedTypeDefinition itm))}
        | _                             -> v, us
    | Some (_)    -> raise (BugErrorException "bug in registerPrimitiveTypeDefinition")
    | None      ->
        let ret, ns =
            match kind with
            | FEI_Reference2RTL          -> raise(BugErrorException "Enumerated types are not defined in RTL")
            | FEI_NewTypeDefinition      ->
                let proposedTypeDefName, asn1Name = getProposedTypeDefName us l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName us id (l,ib) programUnit proposedTypeDefName
                let index_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_index_range")
                let values_array, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_values_array")
                let values_array_count, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_values_array_count")
                let encoded_values_array , newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_encoded_values_array")
                let encoded_values_array_count, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_encoded_values_array_count")
                let itm = {FE_EnumeratedTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=NonPrimitiveNewTypeDefinition; index_range=index_range; values_array=values_array;values_array_count=values_array_count; encoded_values_array=encoded_values_array;encoded_values_array_count=encoded_values_array_count}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_EnumeratedTypeDefinition itm))}
            | FEI_NewSubTypeDefinition subId ->
                let subType, ns1 = registerEnumeratedTypeDefinition us (l,ib) subId FEI_NewTypeDefinition
                let proposedTypeDefName, asn1Name = getProposedTypeDefName ns1 l id
                let typeName, newAllocatedTypeNames = reserveMasterTypeDefinitionName ns1 id (l,ib) programUnit proposedTypeDefName
                let index_range, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_index_range")
                let values_array, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_values_array")
                let values_array_count, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_values_array_count")
                let encoded_values_array , newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_encoded_values_array")
                let encoded_values_array_count, newAllocatedTypeNames = reserveTypeDefinitionName  us.args.TypePrefix newAllocatedTypeNames (l,ib) programUnit (proposedTypeDefName + "_encoded_values_array_count")
                let itm = {FE_EnumeratedTypeDefinition.programUnit = programUnit; typeName = typeName; asn1Name = asn1Name; asn1Module =  Some id.ModName; kind=(NonPrimitiveNewSubTypeDefinition subType); index_range=index_range; values_array=values_array;values_array_count=values_array_count; encoded_values_array=encoded_values_array;encoded_values_array_count=encoded_values_array_count}
                let ns2 = {ns1 with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = ns1.allocatedFE_TypeDefinition.Add((l,id), (FE_EnumeratedTypeDefinition itm))}
                itm, ns2
            | FEI_Reference2OtherType refId  ->
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerEnumeratedTypeDefinition us (l,ib) refId FEI_NewTypeDefinition
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

type GetTypeDefinition_arg = {
    asn1TypeKind : Asn1Ast.Asn1TypeKind
    loc:SrcLoc
    curPath : ScopeNode list
    typeDefPath : ScopeNode list
    enmItemTypeDefPath : ScopeNode list
    inheritInfo : InheritanceInfo option
    typeAssignmentInfo : AssignmentInfo option
    rtlFnc : (ProgrammingLanguage -> (string*string*string)) option
    blm    : (ProgrammingLanguage*ILangBasic) list
}

let getTypedefKind (arg:GetTypeDefinition_arg) =
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
                match arg.curPath.Length > 2 && arg.rtlFnc.IsSome && arg.inheritInfo.IsNone with
                | true  -> FEI_Reference2RTL
                | false -> FEI_Reference2OtherType (ReferenceToType arg.typeDefPath)
    | _                 ->
        // type under a type assignment
        // when curPath  = typeDefPath then in most case it means a new type definition (or new subtype definition).
        // however if curpath is greater than 2 (i.e. child type) and type has rtlFnc then it a reference to RTL
        match arg.curPath = arg.typeDefPath with
        | true  ->
            match arg.inheritInfo with
            | None  ->
                match arg.curPath.Length > 2 && arg.rtlFnc.IsSome with
                | true  -> FEI_Reference2RTL
                | false -> FEI_NewTypeDefinition
            | Some inh -> FEI_NewSubTypeDefinition (ReferenceToType [MD inh.modName; TA inh.tasName])
        | false ->
            //In this case the curPath is different to typedefPath.
            //Normally this is a reference to another type. However, there are two exceptions
            //  (a) if type is child type and rtlFnc is some and inheritInfo.IsNone then is reference to RTL (instead of reference to other type)
            //  (b) if this type is type assignment (the case is A::=B) then we must define a new type (A) which has a subtype (B)
            match arg.typeAssignmentInfo with
            | Some (ValueAssignmentInfo   _)
            | None  ->
                match arg.curPath.Length > 2 && arg.rtlFnc.IsSome && arg.inheritInfo.IsNone with
                | true  -> FEI_Reference2RTL
                | false -> FEI_Reference2OtherType (ReferenceToType arg.typeDefPath)
                    //match arg.inheritInfo with
                    //| None -> FEI_Reference2OtherType (ReferenceToType arg.typeDefPath)
                    //| Some inh -> FEI_NewSubTypeDefinition (ReferenceToType [MD inh.modName; TA inh.tasName])
            | Some (TypeAssignmentInfo    tsInfo)   -> FEI_NewSubTypeDefinition (ReferenceToType arg.typeDefPath)



let getPrimitiveTypeDefinition (arg:GetTypeDefinition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, reference to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.ActiveLanguages |> foldMap (fun us l ->
            let ib = arg.blm |> List.find (fun (l1,_) -> l1 = l) |> snd
            let itm, ns = registerPrimitiveTypeDefinition us (l,ib) (ReferenceToType arg.curPath) typedefKind arg.rtlFnc
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getStringTypeDefinition (arg:GetTypeDefinition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, reference to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.ActiveLanguages |> foldMap (fun us l ->
            let ib = arg.blm |> List.find (fun (l1,_) -> l1 = l) |> snd
            let itm, ns = registerStringTypeDefinition us (l,ib) (ReferenceToType arg.curPath) typedefKind
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getSizeableTypeDefinition (arg:GetTypeDefinition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, reference to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.ActiveLanguages |> foldMap (fun us l ->
            let ib = arg.blm |> List.find (fun (l1,_) -> l1 = l) |> snd
            let itm, ns = registerSizeableTypeDefinition us (l,ib) (ReferenceToType arg.curPath) typedefKind
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getSequenceTypeDefinition (arg:GetTypeDefinition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, reference to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.ActiveLanguages |> foldMap (fun us l ->
            let ib = arg.blm |> List.find (fun (l1,_) -> l1 = l) |> snd
            let itm, ns = registerSequenceTypeDefinition us (l,ib) (ReferenceToType arg.curPath) typedefKind
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getChoiceTypeDefinition (arg:GetTypeDefinition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, reference to other type etc)
    let typedefKind = getTypedefKind arg
    let lanDefs, us1 =
        ProgrammingLanguage.ActiveLanguages |> foldMap (fun us l ->
            let ib = arg.blm |> List.find (fun (l1,_) -> l1 = l) |> snd
            let itm, ns = registerChoiceTypeDefinition us (l,ib) (ReferenceToType arg.curPath) typedefKind
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getEnumeratedTypeDefinition (arg:GetTypeDefinition_arg) (us:Asn1AcnMergeState)=
    //first determine the type definition kind (i.e. if it is a new type definition or reference to rtl, reference to other type etc)
    let typedefKind = getTypedefKind arg
    //let typedefKindEmnItem = getTypedefKind {arg with typeDefPath=arg.enmItemTypeDefPath}
    let lanDefs, us1 =
        ProgrammingLanguage.ActiveLanguages |> foldMap (fun us l ->
            let ib = arg.blm |> List.find (fun (l1,_) -> l1 = l) |> snd
            let itm, ns = registerEnumeratedTypeDefinition us (l,ib) (ReferenceToType arg.curPath) typedefKind
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1

let getReferenceTypeDefinition (asn1:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (arg:GetTypeDefinition_arg) (us:Asn1AcnMergeState) =
    match (Asn1Ast.GetActualType t asn1).Kind with
    | Asn1Ast.Integer                  -> getPrimitiveTypeDefinition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.ObjectIdentifier         -> getPrimitiveTypeDefinition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.RelativeObjectIdentifier -> getPrimitiveTypeDefinition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Real                     -> getPrimitiveTypeDefinition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.NullType                 -> getPrimitiveTypeDefinition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.TimeType _               -> getPrimitiveTypeDefinition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Boolean                  -> getPrimitiveTypeDefinition arg us   |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Enumerated        _      -> getEnumeratedTypeDefinition arg us  |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_EnumeratedTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.OctetString              -> getSizeableTypeDefinition arg us    |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.BitString   _            -> getSizeableTypeDefinition arg us    |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.SequenceOf        _      -> getSizeableTypeDefinition arg us    |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.NumericString            -> getStringTypeDefinition arg us      |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_StringTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.IA5String                -> getStringTypeDefinition arg us      |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_StringTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Sequence          _      -> getSequenceTypeDefinition arg us    |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_SequenceTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.Choice            _      -> getChoiceTypeDefinition arg us      |> (fun (a,b) -> a |> Map.toList |> List.map (fun (l, d) -> (l, FE_ChoiceTypeDefinition d)) |> Map.ofList,b)
    | Asn1Ast.ReferenceType     _      -> raise(BugErrorException "getReferenceTypeDefinition")


(*

open DAst
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

let createInteger (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer)  =
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
    let programUnit = ToC t.id.ModName
    match td.kind with
    | PrimitiveNewTypeDefinition              -> 
        let baseType = declare_Integer()
        let typedefBody () = defineSubType  td.typeName None baseType (getNewRange None baseType) None []
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = typedefBody; privateTypeDefinition=None; baseType=None}
    | PrimitiveNewSubTypeDefinition subDef     ->
        let otherProgramUnit = if td.programUnit = subDef.programUnit then None else (Some subDef.programUnit)
        let typedefBody () = defineSubType td.typeName otherProgramUnit subDef.typeName (getNewRange otherProgramUnit subDef.typeName) None []
        let baseType = {DAst.ReferenceToExistingDefinition.programUnit = (if subDef.programUnit = programUnit then None else Some subDef.programUnit); typedefName=subDef.typeName ; definedInRtl = false}
        TypeDefinition {TypeDefinition.typedefName = td.typeName; typedefBody = typedefBody; privateTypeDefinition=None; baseType=Some baseType}
    | PrimitiveReference2RTL                  -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = true}
    | PrimitiveReference2OtherType            -> 
        ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit =  (if td.programUnit = programUnit then None else Some td.programUnit); typedefName= td.typeName; definedInRtl = false}



let createReal (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type)  (o:Asn1AcnAst.Real)    =
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

let createReal_u (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)   (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real)  (us:State) =
    let aaa = createReal r lm t o 
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
*)