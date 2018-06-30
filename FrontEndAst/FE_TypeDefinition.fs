module FE_TypeDefinition

open System
open System.Linq
open System.Numerics
open Antlr.Acn
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open FsUtils

open Asn1AcnAst

let private reserveTypeDefinitionName (allocatedTypeNames : (ProgrammingLanguage*string*string)  list) (l:ProgrammingLanguage)  (programUnit:string) (proposedTypeDefName:string)  =
    let getNextCount (oldName:string) =
        match oldName.Split('_') |> Seq.toList |> List.rev with
        | []    
        | _::[]     -> oldName + "_1"
        | curN::oldPart   ->
            match Int32.TryParse curN with
            | true, num ->  (oldPart |> List.rev |> Seq.StrJoin "_") + "_" + ((num+1).ToString())
            | _         -> oldName + "_1"
    let rec getValidTypeDefname (proposedTypeDefName:string) = 
        match l with
        | C     ->  
            match allocatedTypeNames |> Seq.exists(fun (cl, _, ct) -> cl = l && ct = proposedTypeDefName) with
            | false -> proposedTypeDefName
            | true  -> 
                match allocatedTypeNames |> Seq.exists(fun (cl, cp, ct) -> cl = l && cp = programUnit && ct = proposedTypeDefName) with
                | false -> getValidTypeDefname (programUnit + "_" + proposedTypeDefName ) 
                | true  -> getValidTypeDefname (getNextCount proposedTypeDefName ) 
        | Spark
        | Ada   ->  
            match allocatedTypeNames  |> Seq.exists(fun (cl, cp, ct) -> cl = l && cp.ToUpper() = programUnit.ToUpper() && ct.ToUpper() = proposedTypeDefName.ToUpper()) with
            | false -> proposedTypeDefName
            | true  -> getValidTypeDefname (getNextCount proposedTypeDefName  ) 
        | _     -> proposedTypeDefName
    
    let validTypeDefname = getValidTypeDefname proposedTypeDefName 
    validTypeDefname, (l, programUnit, validTypeDefname)::allocatedTypeNames


let rec registerPrimitiveTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKind) getRtlDefinitionFunc : (FE_PrimitiveTypeDefinition*Asn1AcnMergeState)=
    let programUnit = ToC id.ModName
    let getProposedTypeDefName (id:ReferenceToType) =
        let lastNodeName = 
            match id with
            | ReferenceToType path -> 
                match path |> List.rev |> List.head with
                | SEQ_CHILD name       -> name
                | CH_CHILD (name,_)    -> name
                | TA name              -> name
                | SQF                   -> "elem"
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
        | None              -> ToC lastNodeName
        | Some  parentDef   -> ToC (parentDef.typeName + "_" + lastNodeName)

    
    match us.allocatedFE_TypeDefinition |> Map.tryFind(l,id) with
    | Some (FE_PrimitiveTypeDefinition v)    -> 
        match kind with
        | FE_NewSubTypeDefinition  _   -> 
            match kind = v.kind with
            | true  -> v, us
            | false ->
                // fix early main type allocation
                let newMap = us.allocatedFE_TypeDefinition.Remove (l,id)
                let itm = {v with kind = kind}
                itm, {us with allocatedFE_TypeDefinition = newMap.Add((l,id),(FE_PrimitiveTypeDefinition itm))}
        | _                             -> v, us
    | Some (_)    -> raise (BugErrorException "bug in registerPrimitiveTypeDefinition")
    | None      -> 
        let ret, ns =
            match kind with
            | FE_Reference2RTL          -> 
                match getRtlDefinitionFunc with
                | None      -> raise(BugErrorException "kind is FE_Reference2RTL but no getRtlDefinitionFunc was provided")
                | Some fnc  -> {FE_PrimitiveTypeDefinition.programUnit = programUnit; typeName = fnc l; kind=kind} , us
            | FE_NewTypeDefinition     
            | FE_NewSubTypeDefinition _ ->
                let proposedTypeDefName = getProposedTypeDefName id
                let typeName, newAllocatedTypeNames = reserveTypeDefinitionName us.allocatedTypeNames l programUnit proposedTypeDefName
                let itm = {FE_PrimitiveTypeDefinition.programUnit = programUnit; typeName = typeName; kind=kind}
                itm, {us with allocatedTypeNames = newAllocatedTypeNames; allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_PrimitiveTypeDefinition itm))}
            | FE_Reference2OtherType refId  -> 
                // initially we register the base type as FE_NewTypeDefinition. It may a be FE_NewSubTypeDefinition though. This will be corrected when
                let actDef, ns = registerPrimitiveTypeDefinition us l refId FE_NewTypeDefinition getRtlDefinitionFunc
                let itm = {actDef with kind = kind}
                itm, {ns with allocatedFE_TypeDefinition = us.allocatedFE_TypeDefinition.Add((l,id), (FE_PrimitiveTypeDefinition itm))}
                
        ret, ns

type GetTypeDifition_arg = {
    asn1TypeKind : Asn1Ast.Asn1TypeKind
    loc:SrcLoc 
    curPath : ScopeNode list 
    typeDefPath : ScopeNode list
    inferitInfo : InheritanceInfo option
    typeAssignmentInfo : AssignmentInfo option
    rtlFnc : (ProgrammingLanguage -> string) option
    
}

let getTypeDifition (arg:GetTypeDifition_arg) (us:Asn1AcnMergeState)=
    let typedefKind =
        match arg.curPath with
        | (MD _)::(VA _)::_ -> 
            //value assignment
            match arg.typeDefPath   with
            | (MD _)::(VA _)::_ -> 
                match arg.asn1TypeKind with
                | Asn1Ast.Integer | Asn1Ast.Real | Asn1Ast.NullType | Asn1Ast.Boolean   -> FE_Reference2RTL
                | _         -> raise(SemanticError(arg.loc, "Unnamed types are not supported in value assignments" ))
            | _     -> 
                FE_Reference2OtherType (ReferenceToType arg.typeDefPath)
        | _                 ->
            match arg.curPath = arg.typeDefPath with
            | true  ->
                match arg.inferitInfo with
                | None  -> FE_NewTypeDefinition
                | Some inh -> FE_NewSubTypeDefinition (ReferenceToType [MD inh.modName; TA inh.tasName])
            | false ->
                //normally this is a reference to another type case.
                //however,  for type assignments A::=B we must define a new type (A) which has a subtype (B)
                match arg.typeAssignmentInfo with
                | Some (TypeAssignmentInfo    tsInfo)   -> FE_NewSubTypeDefinition (ReferenceToType arg.typeDefPath)
                | Some (ValueAssignmentInfo   _)   
                | None  -> FE_Reference2OtherType (ReferenceToType arg.typeDefPath)
    let lanDefs, us1 =
        [C;Ada] |> foldMap (fun us l -> 
            let itm, ns = registerPrimitiveTypeDefinition us l (ReferenceToType arg.curPath) typedefKind arg.rtlFnc 
            (l,itm), ns) us
    lanDefs |> Map.ofList, us1
