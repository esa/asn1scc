(*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*)

module spark_utils
open System.Numerics
open FsUtils
open Ast
open System.IO
open CommonTypes
open VisitTree
open uPER
open CloneTree


type LOCAL_VARIABLE =
    | SEQUENCE_OF_INDEX of int*bool
    | LENGTH
    | EXTENSION_BIT
    | ENUM_IDX
    | CHOICE_IDX
    | CHOICE_TMP_FLD   of string*string
    | REF_TYPE_PARAM   of string*string
    | CHAR_VAL
    | NCOUNT
    | CUR_BLOCK_SIZE
    | CUR_ITEM
    | LEN2
    | UNCONSTRAINT_ASN1_INT_FOR_MAPPING_FUNCTION






let rec HasValidateFunc (t:Asn1Type) (r:AstRoot) =
    let IsFixed (t:Asn1TypeKind) cons=
            match (GetTypeUperRange t cons r) with
            |Concrete(min, max)        -> min = max
            |_                         -> raise(BugErrorException "SEQUENCE OF or OCTET STRING etc has no constraints")
    let hasSingleSizeConstraint =
        t.Constraints |> Seq.exists(fun c -> match c with Ast.SingleValueContraint(_) -> true | _ -> false )
    let res = 
        match t.Kind with
        |SequenceOf(ch)                         -> (not (IsFixed t.Kind t.Constraints)) || (HasValidateFunc ch r)
        |BitString | OctetString                -> not (IsFixed t.Kind t.Constraints)
        |Sequence(children) | Choice(children)  -> 
            let ret1 = not (Seq.isEmpty t.Constraints)
            let retC = children |> Seq.fold(fun s c -> s || (HasValidateFunc c.Type r)) false
            ret1 || retC
        |ReferenceType(mName, tasName, _)          -> HasValidateFunc (GetBaseType t r) r
        |_                                      -> not (Seq.isEmpty t.Constraints)

    hasSingleSizeConstraint || res


let AcnEncodeFuncRequiresResult(t:Asn1Type) (r:AstRoot) (acn: AcnTypes.AcnAstResolved)=
    HasValidateFunc t r 

let UperEncodeFuncRequiresResult(t:Asn1Type) (r:AstRoot) =
    HasValidateFunc t r 


let GetAccessFldPriv vl (path:list<string>) (t:ConstraintType) (r:AstRoot)  = 
    let getTas modName tasName =
        let modl = r.Modules |> Seq.find(fun m -> m.Name.Value = modName)
        modl.TypeAssignments |> Seq.find(fun tas -> tas.Name.Value = tasName)
    let tas() = 
            match path with
            | modName::tasName::rest -> getTas modName tasName
            | _                      -> raise(BugErrorException "Invalid path")
    
    let accessFld =
        match path with
        | single::[]   ->   ToC single
        | _            -> 
            match tas().Type.Kind with
            | Choice(children) ->
                match path with
                | modName::tasName::[] -> vl
                | modName::tasName::fldName::[] ->
                    let sTasName = GetTasCName tasName r.TypePrefix
                    match children |> Seq.tryFind (fun ch -> ch.Name.Value = fldName) with
                    | Some ch   -> sTasName+"_"+(ch.CName ProgrammingLanguage.Spark)+"_get("+vl+")"
                    | None      -> sTasName+"_"+(ToC fldName)+"_get("+vl+")"
                | _                      -> raise(BugErrorException "Invalid path")
            | _         ->
                match path with
                | modName::tasName::rest ->
                    let actPath = vl::rest;
                    let ww = actPath |> foldMap(fun s str -> if str="#" then "Data(I"+(s+1).ToString()+")",(s+1) else str,s) 0 |> fst |> Seq.map ToC
                    let retValue = ww.StrJoin(".")
                    match rest with
                    | []    -> retValue
                    | _     ->
                        let path1 = path |> List.rev |> List.tail |> List.rev
                        let lastName = path |> List.rev |> List.head
                        let parType = GetTypeByAbsPath path1 r
                        match parType.Kind with
                        | Sequence(children)   ->
                            let ch = children |> Seq.find(fun x -> x.Name.Value = lastName)
                            match ch.AcnInsertedField with
                            | true  -> ToC lastName
                            | false -> 
                                vl::([ch.CName ProgrammingLanguage.Spark]) |> Seq.StrJoin "."
                        | _                    -> retValue

                | _                      -> raise(BugErrorException "Invalid path")
    match t with
    | Same(_)       ->  accessFld
    | LengthOf(tp) ->
        let actualTypeAllConsIncluded = GetBaseTypeConsIncluded tp r
        let min,max = uPER.GetSizebaleMinMax actualTypeAllConsIncluded.Kind actualTypeAllConsIncluded.Constraints r
        match (GetActualType tp r).Kind with
        | IA5String | NumericString -> sc.getStringSize(accessFld)
        | OctetString | BitString | SequenceOf(_)   when min=max -> min.ToString() 
        | OctetString | BitString | SequenceOf(_)                -> sc.getSizeableSize accessFld
        | _                                                      -> raise(BugErrorException "Invalid combination type/constraint")
    | AlphabetOf(tp) ->
        match (GetActualType tp r).Kind with
        | IA5String | NumericString -> sc.Print_AlphabetCheckFunc_str()
        | _                          -> raise(BugErrorException "Invalid combination type/constraint")
        
let GetAccessFld (path:list<string>) (t:ConstraintType) (r:AstRoot)  = 
    GetAccessFldPriv "val" path t r

let GetAccessFld2 (path:list<string>) (r:AstRoot)  = 
    let dummyType = {Asn1Type.Kind = NullType; Constraints=[]; AcnProperties=[]; Location= emptyLocation}
    GetAccessFldPriv "val" path (Same dummyType) r

let GetPointAccessPath (p:AcnTypes.Point) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) =
    let dummyType = {Asn1Type.Kind = NullType; Constraints=[]; AcnProperties=[]; Location= emptyLocation}
    match p with
    | AcnTypes.TypePoint(absPath)   ->    GetAccessFld p.AbsPath (Same dummyType) r
    | AcnTypes.ParamPoint(absPath)  ->    absPath.Tail.Tail.Head
    | AcnTypes.TempPoint(absPath)   ->    
        match absPath with
        | x ::[]        -> x
        | m::t::x::xs   -> x
        | _             -> raise(BugErrorException "")

let GetCount (t:Asn1Type) (path:string) (r:AstRoot) =
    let min,max = uPER.GetSizebaleMinMax t.Kind t.Constraints r
    match t.Kind with
    | IA5String | NumericString                                     -> su.str_length path
    | BitString(_) | OctetString | SequenceOf(_) when min=max       -> min.ToString() 
    | BitString(_) | OctetString | SequenceOf(_)                    -> su.bit_oct_sqof_length path
    | _                                                             -> raise(BugErrorException "")


let getMappingFunctionModule (t:Asn1Type) (r:Ast.AstRoot)   =
    let knownMappingFunctions = ["milbus"] |> Set.ofList
    match t.AcnProperties |> List.choose(fun p -> match p with AcnTypes.MappingFunction funcL -> Some funcL.Value | _ -> None) with
    | []    -> ss.rtlModuleName()
    | x::_     -> 
        match knownMappingFunctions.Contains x with
        | true  -> ss.rtlModuleName()
        | false ->
            match r.mappingFunctionsModule with
            | None  -> ss.rtlModuleName()
            | Some x -> x


module SparkDeps =

(* ******************************************************************************************************************** *)
(* ******************************************************************************************************************** 
    K Index depends on Value
    Returns true if the K in out parameter of the encode uper function depends
    on the input value or not.
    For example, K does not depent on INTEGER (1..8) since K will always be increased by thee
    no matter what is the value of INTEGER (i.e. whether it is 1 or 2 or ... 8). On the other hand,
    for the type INTEGER, K does depend on the value. If value is 3 then K will be increased by 16 bits
    (8 bits for the length determinant and 8 bits for the encoding of 3), while if the value is 10000000 K
    will be increased much more
   ******************************************************************************************************************** *)
(* ******************************************************************************************************************** *)

    let KDependsOnValue_uper (t:Asn1Type) (r:AstRoot) =
        let rec KDependsOnValueAux (t:Asn1TypeKind) cons  =
            match t with 
            |Integer        ->
                let hasRootCons = cons |> Seq.filter(fun c -> match c with RootConstraint(_)|RootConstraint2(_) ->true |_ -> false) |> Seq.isEmpty |>not
                match (GetTypeUperRange t cons r) with
                |Concrete(_, _) | Empty            -> hasRootCons
                |NegInf(_) | PosInf(_) | Full      -> true
            |Enumerated(_) | Boolean | NullType-> false
            |Real                    -> true
            |OctetString | BitString ->
                match (GetTypeUperRange t cons r) with
                |Concrete(min, max)        -> not (min=max)
                |_                         -> true
            |IA5String | NumericString  -> 
                match (GetTypeUperRange t cons r) with
                |Concrete(min, max)        -> not (min=max)
                |_                         -> true
            |SequenceOf(child)      ->
                match (GetTypeUperRange t cons r) with
                |Concrete(min, max)        -> (min<>max) || (KDependsOnValueAux child.Kind child.Constraints )
                |_                         -> true
            |Choice(children)       -> 
                true
            |ReferenceType(modName, tasName, _)              ->
                let baseType = GetBaseTypeByName modName tasName r
                KDependsOnValueAux baseType.Kind (baseType.Constraints@cons)
            |Sequence(children)     ->
                let childIsVarSize (c:ChildInfo) =
                    c.Optionality.IsSome || KDependsOnValueAux c.Type.Kind c.Type.Constraints
                children |> Seq.fold (fun b c -> b || childIsVarSize c) false
        if HasValidateFunc t r then true
        else KDependsOnValueAux     t.Kind t.Constraints


    let IntegerEncodingInAcnIsVariableSize (t:Asn1Type) (r:AstRoot) (acn: AcnTypes.AcnAstResolved) =
        let encClass= Acn.GetIntEncodingClass t r acn emptyLocation
        match encClass with
        |Acn.Integer_uPER   -> 
            let cons = t.Constraints
            let hasRootCons = cons |> Seq.filter(fun c -> match c with RootConstraint(_)|RootConstraint2(_) ->true |_ -> false) |> Seq.isEmpty |>not
            match (GetTypeUperRange t.Kind cons r) with
            |Concrete(_, _) | Empty            -> hasRootCons
            |NegInf(_) | PosInf(_) | Full      -> true
        |Acn.PositiveInteger_ConstSize(_)          
        |Acn.PositiveInteger_ConstSize_8               
        |Acn.PositiveInteger_ConstSize_big_endian_16   
        |Acn.PositiveInteger_ConstSize_big_endian_32   
        |Acn.PositiveInteger_ConstSize_big_endian_64   
        |Acn.PositiveInteger_ConstSize_little_endian_16
        |Acn.PositiveInteger_ConstSize_little_endian_32
        |Acn.PositiveInteger_ConstSize_little_endian_64
        |Acn.TwosComplement_ConstSize_8
        |Acn.TwosComplement_ConstSize_big_endian_16
        |Acn.TwosComplement_ConstSize_little_endian_16
        |Acn.TwosComplement_ConstSize_big_endian_32
        |Acn.TwosComplement_ConstSize_little_endian_32
        |Acn.TwosComplement_ConstSize_big_endian_64
        |Acn.TwosComplement_ConstSize_little_endian_64
        |Acn.ASCII_ConstSize(_)
        |Acn.ASCII_UINT_ConstSize _
        |Acn.BCD_ConstSize(_)
        |Acn.TwosComplement_ConstSize(_)                        -> false
        |Acn.PositiveInteger_VarSize_LengthEmbedded
        |Acn.TwosComplement_VarSize_LengthEmbedded
        |Acn.ASCII_VarSize_LengthEmbedded
        |Acn.ASCII_VarSize_NullTerminated _
        |Acn.ASCII_UINT_VarSize_LengthEmbedded
        |Acn.ASCII_UINT_VarSize_NullTerminated _
        |Acn.BCD_VarSize_LengthEmbedded
        |Acn.BCD_VarSize_NullTerminated    _                     -> true


    let rec KDependsOnValue_acn (t:Asn1Type) (path:string list) (r:AstRoot) (acn: AcnTypes.AcnAstResolved)=
        let KDependsOnValueAux_acn (t:Asn1Type) (r:AstRoot) (acn: AcnTypes.AcnAstResolved) =
            match t.Kind with 
            |Integer                                -> IntegerEncodingInAcnIsVariableSize t r acn
            | Boolean | NullType     -> false
            |Enumerated(_)                          -> false
            |Real                    -> 
                match Acn.GetRealEncodingClass t r with
                | Acn.Real_uPER     ->  true
                | _                 ->  false
            |OctetString | BitString ->
                match (GetTypeUperRange t.Kind t.Constraints r) with
                |Concrete(min, max)        -> not (min=max)
                |_                         -> true
            |IA5String | NumericString  -> 
                match (GetTypeUperRange t.Kind t.Constraints r) with
                |Concrete(min, max)        -> not (min=max)
                |_                         -> true
            |SequenceOf(child)      ->
                match (GetTypeUperRange t.Kind t.Constraints r) with
                |Concrete(min, max)        -> (min<>max) || (KDependsOnValue_acn child (path@["#"]) r acn )
                |_                         -> true
            |Choice(children)       -> 
                    // K depends on the active child, which depends on value of the choice
                    // hence K always depends on value for choices
                    true
            |ReferenceType(modName, tasName, _)              ->
                let baseType = GetBaseTypeByName modName tasName r
                let vtype = {baseType with Constraints=baseType.Constraints@t.Constraints; AcnProperties = baseType.AcnProperties@t.AcnProperties}
                KDependsOnValue_acn vtype path r acn
            |Sequence(children)     ->
                let childIsFixed (c:ChildInfo) =
                    c.Optionality.IsNone && not(KDependsOnValue_acn c.Type (path@[c.Name.Value]) r acn)
                children |> Seq.fold (fun b c -> b || not(childIsFixed c)) false

        if HasValidateFunc t r then true
        else    KDependsOnValueAux_acn t r acn


(* ******************************************************************************************************************** *)
(* ******************************************************************************************************************** *)
(* Decoding Result depends on S, i.e. on stream data *)
(* ******************************************************************************************************************** *)
(* ******************************************************************************************************************** *)


    let DecodingResultDependsOnData_uper (a:Asn1Type) (r:AstRoot) =
        let rec DecodingDependsOnDataAux (kind:Asn1TypeKind) cons =
            let IsFixed (t:Asn1TypeKind) cons=
                    match (GetTypeUperRange t cons r) with
                    |Concrete(min, max)        -> min = max
                    |_                         -> false
            match kind with
            | Boolean                           -> false
            | OctetString | BitString -> not(IsFixed kind cons)
            | SequenceOf(child)                 -> 
                match (GetTypeUperRange kind cons r) with
                |Concrete(min, max)       when min=max -> DecodingDependsOnDataAux child.Kind child.Constraints
                |_                                     -> true
            |ReferenceType(modName, tasName, _)              ->
                let baseType = GetBaseTypeByName modName tasName r
                DecodingDependsOnDataAux baseType.Kind (baseType.Constraints@cons)
            |Sequence(children)   ->
                let childDecodingDependsOnData (c:ChildInfo) =
                    c.Optionality.IsSome || DecodingDependsOnDataAux c.Type.Kind c.Type.Constraints
                children |> Seq.fold(fun curRes c -> curRes || childDecodingDependsOnData c) false
            | _                 -> true 
        DecodingDependsOnDataAux a.Kind a.Constraints


    let rec DecodingResultDependsOnData_acn (a:Asn1Type) path (r:AstRoot) (acn: AcnTypes.AcnAstResolved) =
        match a.Kind with
        | Boolean   -> false
        | NullType  -> false
        | Real      -> 
            let encClass = Acn.GetRealEncodingClass a r
            match encClass with
            | Acn.Real_uPER                     -> true
            | Acn.Real_IEEE754_32_big_endian    
            | Acn.Real_IEEE754_64_big_endian
            | Acn.Real_IEEE754_32_little_endian
            | Acn.Real_IEEE754_64_little_endian -> false
        | OctetString | BitString -> 
            let encClass = Acn.GetSizeableEncodingClass_ a path r acn emptyLocation 
            match encClass with
            | Acn.ExternalField(_)  -> false
            | _                     -> DecodingResultDependsOnData_uper a r
        |  SequenceOf(child)-> 
            let encClass = Acn.GetSizeableEncodingClass_ a path r acn emptyLocation 
            match encClass with
            | Acn.ExternalField(_)  
            | Acn.FixedSize(_)      -> DecodingResultDependsOnData_acn child (path@["#"]) r acn
            | _                     -> DecodingResultDependsOnData_uper a r
        |ReferenceType(modName, tasName, _)              ->
            let baseType = GetBaseTypeByName modName tasName r
            let vtype = {baseType with Constraints=baseType.Constraints@a.Constraints; AcnProperties = baseType.AcnProperties@a.AcnProperties}
            DecodingResultDependsOnData_acn vtype path r acn
        | Sequence(children)        ->
            let ChildDependsOnData (ch:ChildInfo) =
                let chPath = path@[ch.Name.Value]
                match ch.Optionality with
                | Some(Optional)   
                | Some(Default(_)) -> 
                    let presenceThrougExtFld = acn.References |> Seq.tryFind(fun x -> x.decType.AbsPath = chPath && x.Kind = AcnTypes.PresenceBool)
                    match presenceThrougExtFld with
                    | None          -> true
                    | Some(_)       -> DecodingResultDependsOnData_acn ch.Type chPath r acn
                | _                 -> DecodingResultDependsOnData_acn ch.Type chPath r acn
            children |> Seq.fold(fun curRes ch -> curRes || (ChildDependsOnData ch)) false
        | _         -> true



(* ******************************************************************************************************************** *)
(* ******************************************************************************************************************** *)
(* K Index depends on S (i.e. the input Data in decoding) 

    As a general rule, K depends on data if 
        the decoded type has variable size 
            or 
        if the decoding type decodes in multiple steps or iterations (e.g. a IA5String, SEQUENCE etc) and the decoding
        result of one these iterations (i.e. the result of the previous functions) depend on data
   ******************************************************************************************************************** *)
(* ******************************************************************************************************************** *)

    let KIndexDependsOnData_uper  (a:Asn1Type) (r:AstRoot) =
        let rec KIndexDependsOnDataAux  (kind:Asn1TypeKind) cons =
            let IsFixed (t:Asn1TypeKind) cons=
                    match (GetTypeUperRange t cons r) with
                    |Concrete(min, max)        -> min = max
                    |_                         -> raise(BugErrorException "SEQUENCE OF or OCTET STRING etc has no constraints")
            
            match kind with
            | Integer                       -> 
                let hasRootCons = cons |> Seq.filter(fun c -> match c with RootConstraint(_)|RootConstraint2(_) ->true |_ -> false) |> Seq.isEmpty |>not
                match (GetTypeUperRange kind cons r) with
                |Concrete(_, _) | Empty            -> hasRootCons
                |NegInf(_) | PosInf(_) | Full      -> true
            | Boolean                                       -> false
            |ReferenceType(modName, tasName, _)                ->
                let baseType = GetBaseTypeByName modName tasName r
                KIndexDependsOnDataAux baseType.Kind (baseType.Constraints@cons)
            | OctetString | BitString                       ->  not (IsFixed kind cons)
            | IA5String | NumericString                     ->  
                // this is always true, even for fixed size strings (i.e. SIZE(10)) because
                // the function used to decode the character value is the
                // spark_asn1_rtl.UPER_Dec_ConstraintWholeNumberInt(S, K, charIndex, 0, 127, 7, result.Success);
                // where the result.success depends on whether charIndex is within [0..127] which depends on the data
                // ==> K always depends on data
                true
            |SequenceOf(child)      -> 
                match (GetTypeUperRange kind cons r) with
                |Concrete(min, max)    when min=max  -> DecodingResultDependsOnData_uper child r 
                |_                                   -> true
            | Choice(children)                              ->  true
            | Sequence(children)  -> 
                let rec seq_aux (chldn : ChildInfo list) =
                    match chldn with
                    | []        -> false
                    | x::[]     -> match x.Optionality with
                                   |Some(_)      -> true
                                   |None         -> KIndexDependsOnDataAux x.Type.Kind x.Type.Constraints
                    | x::xs     ->
                        (x.Optionality.IsSome || DecodingResultDependsOnData_uper x.Type r) || (seq_aux xs)
                seq_aux children
            | Enumerated(_)                                 -> false
            | NullType                                      -> false
            | Real                                          -> true
        KIndexDependsOnDataAux a.Kind a.Constraints 





    let rec KIndexDependsOnData_acn  (a:Asn1Type) path (r:AstRoot) (acn: AcnTypes.AcnAstResolved)=
        match a.Kind with
        |Integer                        -> 
            let encClass= Acn.GetIntEncodingClass a r acn emptyLocation
            match encClass with
            |Acn.ASCII_ConstSize(_)
            |Acn.BCD_ConstSize(_)       -> true
            | _                         -> IntegerEncodingInAcnIsVariableSize a r acn
        |IA5String | NumericString      -> true
        | OctetString | BitString -> 
            let encClass = Acn.GetSizeableEncodingClass_ a path r acn emptyLocation 
            match encClass with
            | Acn.ExternalField(_)  -> false
            | _                     -> KIndexDependsOnData_uper a r
        | SequenceOf(child)      -> 
            let encClass = Acn.GetSizeableEncodingClass_ a path r acn emptyLocation 
            match encClass with
            | Acn.ExternalField(_)   
            | Acn.FixedSize(_)      -> DecodingResultDependsOnData_acn child (path@["#"]) r acn
            | _                     -> KIndexDependsOnData_uper a r
        |ReferenceType(modName, tasName, _)              ->
            let baseType = GetBaseTypeByName modName tasName r
            let vtype = {baseType with Constraints=baseType.Constraints@a.Constraints; AcnProperties = baseType.AcnProperties@a.AcnProperties}
            KIndexDependsOnData_acn vtype path r acn
        | Choice(children)          ->
            match Acn.GetChoiceEncodingClass path children acn with
            | Some(Acn.EnumDeterminant(_)) 
            | Some(Acn.PresentWhenOnChildren)   -> 
                children |> Seq.fold(fun cur ch -> cur || (KIndexDependsOnData_acn ch.Type (path@[ch.Name.Value]) r acn)  ) false
            | None                              -> true
        |Enumerated(_) -> 
            let newType = Acn.GetIntTypeFromEnum a r acn
            KIndexDependsOnData_acn newType path r acn
        | Boolean | NullType-> false
        |Real                    -> 
            match Acn.GetRealEncodingClass a r with
            | Acn.Real_uPER     ->  true
            | _                 ->  false
        |Sequence(children)     ->
            let rec seq_aux (chldn : ChildInfo list) =
                match chldn with
                | []        -> false
                | x::[]     -> match x.Optionality with
                                |Some(_)      -> true
                                |None         -> KIndexDependsOnData_acn x.Type (path@[x.Name.Value]) r acn
                | x::xs     ->
                    (x.Optionality.IsSome || (DecodingResultDependsOnData_acn x.Type (path@[x.Name.Value]) r acn)) || (seq_aux xs)
            seq_aux children








let MoveChoiceVasToPrivateModule (ast:Ast.AstRoot) =
    let OnVisitValueAssignment (vas:ValueAssignment) (m:Asn1Module) (lst:list<(string*ValueAssignment)*(string*string)>) = 
        let actType = Ast.GetActualType vas.Type ast
        match actType.Kind with
        | Choice(_) -> 
            match vas.Type.Kind with
            | ReferenceType(md, ts, _) -> 
                match md.Value = m.Name.Value with
                | true      -> lst
                | false     -> ((m.Name.Value, vas),(md.Value, vas.Name.Value))::lst
            | _                     -> lst
        | _         -> lst
    let vasesToBeMoved = VisitTree  ast {DefaultVisitors with visitValueAssignment=OnVisitValueAssignment} []

    let OnCloneModule (oldRoot:Ast.AstRoot) (old:Asn1Module) cons state  = 
        let newTas, s0 = old.TypeAssignments |> foldMap (fun s t -> cons.cloneTypeAssignment t old cons s) state
        let extraVases = vasesToBeMoved |> List.filter(fun ((oldModName, vas),(newModName, vasName)) -> old.Name.Value = newModName) |> List.map(fun ((oldModName, vas),(newModName, vasName)) -> vas)
        let newVas, s1 = (old.ValueAssignments@extraVases) |> foldMap (fun s v -> cons.cloneValueAssignment v old cons s) s0
        {
            Asn1Module.Name = old.Name;
            TypeAssignments  = newTas
            ValueAssignments = newVas
            Imports = old.Imports 
            Exports = old.Exports
            Comments = old.Comments
        }, s1

    let OnCloneValueAssignment (old:ValueAssignment)  (m:Asn1Module) cons state=
        let newType,s = cons.cloneType old.Type m [m.Name.Value; old.Name.Value] cons state
        {
            ValueAssignment.Name = old.Name
            Type = newType
            Value = match vasesToBeMoved |> Seq.tryFind(fun ((m1,v1),(m2,v2)) -> m.Name.Value=m1 && v1.Name.Value = old.Name.Value) with
                    | None              -> old.Value 
                    | Some(_,(m2,v2))   -> {old.Value with Kind = RefValue(m2.AsLoc,v2.AsLoc)}
            Scope = old.Scope
            c_name = old.c_name
            ada_name = old.ada_name
        },s


    CloneTree ast {defaultConstructors with createModule = OnCloneModule; cloneValueAssignment = OnCloneValueAssignment} () |> fst
    



