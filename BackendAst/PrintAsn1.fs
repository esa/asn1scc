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



module PrintAsn1
open System
open System.IO
open Asn1Ast
open FsUtils
open CommonTypes

let rec PrintAsn1Value (v:Asn1Value) =
    match v.Kind with
    |IntegerValue(v)         -> stg_asn1.Print_IntegerValue v.Value
    |RealValue(v)            -> stg_asn1.Print_RealValue v.Value
    |StringValue(parts,_)          ->
        match parts with
        | (CStringValue v)::[] ->        stg_asn1.Print_StringValue v
        | _     ->        stg_asn1.Print_SeqOfValue (parts |> List.map(fun p -> p.AsAsn1))
    |TimeValue v             ->
        stg_asn1.Print_TimeValue (asn1DateTimeValueToString  v.Value)
    |BooleanValue(v)         -> stg_asn1.Print_BooleanValue v.Value
    |BitStringValue(v)       -> stg_asn1.Print_BitStringValue v.Value
    |OctetStringValue(v)     -> stg_asn1.Print_OctetStringValue (v |> Seq.map(fun x -> x.Value) |> Seq.toArray)
    |RefValue(mn,nm)         -> stg_asn1.Print_RefValue nm.Value
    |SeqOfValue(vals)        -> stg_asn1.Print_SeqOfValue (vals |> Seq.map PrintAsn1Value |> Seq.toArray)
    |SeqValue(vals)          -> stg_asn1.Print_SeqValue (vals |> Seq.map(fun (nm, v) -> stg_asn1.Print_SeqValue_Child nm.Value (PrintAsn1Value v) ) |> Seq.toArray)
    |ChValue(nm,v)           -> stg_asn1.Print_ChValue nm.Value (PrintAsn1Value v)
    |NullValue               -> stg_asn1.Print_NullValue()
    |ObjOrRelObjIdValue coms    ->
        stg_asn1.Print_ObjOrRelObjIdValue (coms |> List.map DAstAsn1.printComponent)


let rec PrintConstraint (c:Asn1Constraint) =
    match c with
    | SingleValueConstraint(_, v)       -> stg_asn1.Print_SingleValueConstraint (PrintAsn1Value v)
    | RangeConstraint(_, v1, v2, b1, b2)        -> stg_asn1.Print_RangeConstraint (PrintAsn1Value v1) (PrintAsn1Value v2) b1 b2
    | RangeConstraint_val_MAX(_, v, b1)     -> stg_asn1.Print_RangeConstraint_val_MAX (PrintAsn1Value v) b1
    | RangeConstraint_MIN_val(_, v, b2)     -> stg_asn1.Print_RangeConstraint_MIN_val (PrintAsn1Value v) b2
    | RangeConstraint_MIN_MAX        -> stg_asn1.Print_RangeConstraint_MIN_MAX()
    | TypeInclusionConstraint(_, mn,nm)->
        stg_asn1.Print_TypeInclusionConstraint nm.Value
    | SizeConstraint(_, c)              -> stg_asn1.Print_SizeConstraint (PrintConstraint c)
    | AlphabetConstraint(_, c)          -> stg_asn1.Print_AlphabetConstraint (PrintConstraint c)
    | UnionConstraint(_, c1,c2,virtualCon)        ->
        match virtualCon with
        | false -> stg_asn1.Print_UnionConstraint (PrintConstraint c1) (PrintConstraint c2)
        | true  -> ""
    | IntersectionConstraint(_, c1,c2) -> stg_asn1.Print_IntersectionConstraint (PrintConstraint c1) (PrintConstraint c2)
    | AllExceptConstraint(_, c)        -> stg_asn1.Print_AllExceptConstraint (PrintConstraint c)
    | ExceptConstraint(_, c1,c2)       -> stg_asn1.Print_ExceptConstraint (PrintConstraint c1) (PrintConstraint c2)
    | RootConstraint(_, c)             -> stg_asn1.Print_RootConstraint  (PrintConstraint c)
    | RootConstraint2(_, c1,c2)        -> stg_asn1.Print_RootConstraint2 (PrintConstraint c1) (PrintConstraint c2)
    | WithComponentConstraint(_, c,_)  -> stg_asn1.Print_WithComponentConstraint (PrintConstraint c)
    | WithComponentsConstraint(_, ncs) ->
        let print_nc (nc:NamedConstraint) =
            let sConstraint = match nc.Constraint with
                              | Some(c1)     -> PrintConstraint c1
                              | None        -> ""
            let sPresMark = match nc.Mark with
                            | NoMark        -> ""
                            | MarkPresent   -> "PRESENT"
                            | MarkAbsent    -> "ABSENT"
                            | MarkOptional  -> "OPTIONAL"
            stg_asn1.Print_WithComponentsConstraint_child nc.Name.Value sConstraint sPresMark
        stg_asn1.Print_WithComponentsConstraint (ncs |> Seq.map  print_nc |> Seq.toArray)

let rec PrintType (t:Asn1Type) (m:Asn1Module) (bPrintInSingleModule:bool) =
    let cons = t.Constraints |> Seq.map PrintConstraint |> Seq.toArray
    match t.Kind with
    |Integer    -> stg_asn1.Print_Integer "" cons
    |Real       -> stg_asn1.Print_Real cons
    |Boolean    -> stg_asn1.Print_Boolean cons
    |BitString  _-> stg_asn1.Print_BitString cons
    |OctetString-> stg_asn1.Print_OctetString cons
    |NullType   -> stg_asn1.Print_NullType cons
    |IA5String  -> stg_asn1.Print_IA5String cons
    |NumericString -> stg_asn1.Print_NumericString cons
    |ObjectIdentifier -> stg_asn1.Print_ObjectIdenitifier cons
    |RelativeObjectIdentifier -> stg_asn1.Print_RelativeObjectIdenitifier cons
    |Enumerated(items)  ->
        let printItem i (it:NamedItem) =
            let arrsMultilineComments, soSingleLineComment  =
                match it.Comments |> Seq.toList with
                | [] -> [], None
                | x::[] -> [], Some x
                | xs  -> xs, None
            let bLastChild = i = items.Length - 1
            stg_asn1.Print_Enumerated_child it.Name.Value it._value.IsSome (if it._value.IsSome then (PrintAsn1Value it._value.Value) else "")  arrsMultilineComments soSingleLineComment bLastChild
        stg_asn1.Print_Enumerated (items |> Seq.mapi printItem |> Seq.toArray) cons
    |Choice(children)   ->
        let printChild i (c:ChildInfo) =
            let arrsMultilineComments, soSingleLineComment  =
                match c.Comments |> Seq.toList with
                | [] -> [], None
                | x::[] -> [], Some x
                | xs  -> xs, None
            let bLastChild = i = children.Length - 1
            stg_asn1.Print_Choice_child c.Name.Value (PrintType c.Type m bPrintInSingleModule) arrsMultilineComments soSingleLineComment bLastChild
        stg_asn1.Print_Choice (children |> Seq.mapi printChild |> Seq.toArray) cons
    |Sequence(children) ->
        let printChild i (c:ChildInfo) =
            let arrsMultilineComments, soSingleLineComment  =
                match c.Comments |> Seq.toList with
                | [] -> [], None
                | x::[] -> [], Some x
                | xs  -> xs, None
            let bLastChild = i = children.Length - 1
            let bIsOptionalOrDefault, soDefValue =
                match c.Optionality with
                |Some(Optional(dv))   -> true, match dv.defaultValue with Some v -> Some (PrintAsn1Value v) | None -> None
                |_                   -> false, None
            stg_asn1.Print_Sequence_child c.Name.Value (PrintType c.Type m bPrintInSingleModule) bIsOptionalOrDefault soDefValue arrsMultilineComments soSingleLineComment bLastChild
        stg_asn1.Print_Sequence (children |> Seq.mapi printChild |> Seq.toArray) cons
    |SequenceOf(child)  -> stg_asn1.Print_SequenceOf (PrintType child m bPrintInSingleModule) cons
    //|ReferenceType(mname, name, _) ->
    |ReferenceType(r) ->
        match bPrintInSingleModule || m.Name.Value = r.modName.Value with
        | true -> stg_asn1.Print_ReferenceType1 r.tasName.Value cons
        | false -> stg_asn1.Print_ReferenceType2 r.modName.Value r.tasName.Value cons
    |TimeType t -> stg_asn1.Print_TimeType cons


let PrintTypeAss (t:TypeAssignment) m bPrintInSingleModule = stg_asn1.PrintTypeAssignment t.Name.Value (PrintType t.Type m bPrintInSingleModule) t.Comments "::="

let PrintValueAss (v:ValueAssignment) m bPrintInSingleModule = stg_asn1.PrintValueAssignment v.Name.Value (PrintType v.Type m bPrintInSingleModule) (PrintAsn1Value v.Value)

let PrintModule (m:Asn1Module) =
    let exports =
        match m.Exports with
        | All               -> "ALL"
        | OnlySome exps     -> exps |> Seq.StrJoin ", "
    let importsFromModule =
        m.Imports |>
        List.map(fun im -> stg_asn1.PrintModuleImportFromModule ( (im.Types @ im.Values) |> List.map(fun s -> s.Value)) im.Name.Value )

    let tases = m.TypeAssignments |> Seq.map(fun x -> PrintTypeAss x m false) |> Seq.toArray
    let vases = m.ValueAssignments |> Seq.map(fun x -> PrintValueAss x m false)|> Seq.toArray
    stg_asn1.PrintModule m.Name.Value tases vases exports importsFromModule m.Comments

let PrintFile (f:Asn1File) outDir newFileExt =
    let modules = f.Modules |> Seq.map PrintModule |> Seq.toArray
    let fileContent = stg_asn1.PrintAsn1File modules
    let outFileName = Path.Combine(outDir, f.FileName+newFileExt)
    File.WriteAllText(outFileName, fileContent.Replace("\r",""))


let DoWork (r:AstRoot) outDir newFileExt =
    r.Files |> Seq.iter(fun f -> PrintFile f outDir newFileExt)




let printInASingleFile (r:AstRoot) outDir newFile (pdu:string option)=

    let rec  getTypeDependencies2 (tsMap:Map<TypeAssignmentInfo,TypeAssignment>) (deep:bool) (t:Asn1Type) : (TypeAssignmentInfo list )    =
        match t.Kind with
        | Integer                    -> []
        | Real                       -> []
        | IA5String                  -> []
        | NumericString              -> []
        | OctetString                -> []
        | NullType                   -> []
        | BitString _                -> []
        | Boolean                    -> []
        | Enumerated _               -> []
        | ObjectIdentifier           -> []
        | RelativeObjectIdentifier   -> []
        | TimeType _                 -> []
        | SequenceOf    sqof         -> (getTypeDependencies2 tsMap deep sqof)
        | Sequence      children     -> (children |> List.collect (fun ch -> getTypeDependencies2 tsMap deep ch.Type))
        | Choice        children     -> (children |> List.collect (fun ch -> getTypeDependencies2 tsMap deep ch.Type))
        | ReferenceType ref          ->
            let thisRef = {TypeAssignmentInfo.modName = ref.modName.Value; TypeAssignmentInfo.tasName = ref.tasName.Value}
            match deep with
            | false -> [thisRef]
            | true  ->
                let ts = tsMap.[thisRef]
                thisRef::(getTypeDependencies2 tsMap deep ts.Type)
    let allTasses =
        r.Files |>
        List.collect(fun f -> f.Modules) |>
        List.collect(fun m ->
            m.TypeAssignments |> List.map(fun ts -> ({TypeAssignmentInfo.modName = m.Name.Value; TypeAssignmentInfo.tasName = ts.Name.Value}, ts)))
    let modMap = r.Files |> List.collect(fun f -> f.Modules) |> List.map(fun m -> m.Name.Value, m) |> Map.ofList
    let tsMap = allTasses |> Map.ofList
    let allVasses =
        r.Files |> List.collect(fun f -> f.Modules) |> List.collect(fun m -> m.ValueAssignments |> List.map(fun vs -> m,vs))
    let allNodesToSort =
        allTasses |> List.map(fun (tasInfo,ts) -> (tasInfo, getTypeDependencies2 tsMap false ts.Type))
    let independentNodes = allNodesToSort |> List.filter(fun (_,list) -> List.isEmpty list) |> List.map(fun (n,l) -> n)
    let dependentNodes = allNodesToSort |> List.filter(fun (_,list) -> not (List.isEmpty list) )
    let sortedTypeAss =
        DoTopologicalSort independentNodes dependentNodes
            (fun cyclicTasses ->
                match cyclicTasses with
                | []    -> BugErrorException "Impossible"
                | (m1,deps) ::_ ->
                    let printTas (md:TypeAssignmentInfo, deps: TypeAssignmentInfo list) =
                        sprintf "Type assignment '%s.%s' depends on : %s" md.modName md.tasName (deps |> List.map(fun z -> "'" + z.modName + "." + z.tasName + "'") |> Seq.StrJoin ", ")
                    let cycTasses = cyclicTasses |> List.map printTas |> Seq.StrJoin "\n\tand\n"
                    SemanticError(emptyLocation, sprintf "Cyclic Types detected:\n%s\n"  cycTasses)                    )

    let tastToPrint =
        match pdu with
        | None      -> sortedTypeAss
        | Some pdu  ->
            match allTasses |> Seq.tryFind(fun (_,ts) -> ts.Name.Value = pdu) with
            | None ->
                Console.Error.WriteLine("No type assignment with name {0} found", pdu)
                sortedTypeAss
            | Some (tsInfo,ts)   ->
                let deps = tsInfo::(getTypeDependencies2 tsMap true ts.Type) |> List.map(fun z -> z.tasName) |> Set.ofList
                sortedTypeAss |> List.filter(fun ts -> deps.Contains ts.tasName)
    let modulesContent =
        let tases = tastToPrint |> Seq.map(fun tsInfo -> PrintTypeAss (tsMap.[tsInfo]) (modMap.[tsInfo.modName]) true) |> Seq.toArray
        let vases = allVasses |> Seq.map(fun (m,x) -> PrintValueAss x m true)|> Seq.toArray
        stg_asn1.PrintModule "SingleModuleName" tases vases null [] []

    let outFileName = Path.Combine(outDir, newFile)
    File.WriteAllText(outFileName, modulesContent.Replace("\r",""))
    tastToPrint


