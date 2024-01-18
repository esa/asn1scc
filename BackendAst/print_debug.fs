module print_debug


open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open System
open System.IO
open Constraints
open DAst
open uPER2

type PRINT_CONTENT =
    | REF
    | CON

let printUperRange (u:uperRange<'a>) =
    match u with
    | Concrete  (a,b)   -> sprintf "[%A .. %A]" a b
    | NegInf    a       -> sprintf "[MIN .. %A]" a        //(-inf, b]
    | PosInf    a       -> sprintf "[%A .. MAX]" a        //(-inf, b]
    | Full              -> "[MIN .. MAX]"                 // (-inf, +inf)

let printCharSet (cs:char array) =
    cs|> Seq.filter (fun c -> not (Char.IsControl c)) |> Seq.StrJoin ""

let printSizeMinMax a b = sprintf "[%d .. %d]" a b

let printGenericConstraint printValue (c:GenericConstraint<'v>)  =
    foldGenericConstraint
        (fun r1 r2 b s      -> stg_asn1.Print_UnionConstraint r1 r2, s)
        (fun r1 r2 s        -> stg_asn1.Print_IntersectionConstraint r1 r2 , s)
        (fun r s            -> stg_asn1.Print_AllExceptConstraint r, s)
        (fun r1 r2 s        -> stg_asn1.Print_ExceptConstraint r1 r2, s)
        (fun r s            -> stg_asn1.Print_RootConstraint r, s)
        (fun r1 r2 s        -> stg_asn1.Print_RootConstraint2 r1 r2, s)
        (fun v rv s         -> stg_asn1.Print_SingleValueConstraint (printValue v),s)
        c
        0

let printRangeConstraint0 printValue printValue2  (c:RangeTypeConstraint<'v1,'v2>) =
    foldRangeTypeConstraint
        (fun r1 r2 b s      -> stg_asn1.Print_UnionConstraint r1 r2, s)
        (fun r1 r2 s        -> stg_asn1.Print_IntersectionConstraint r1 r2 , s)
        (fun r s            -> stg_asn1.Print_AllExceptConstraint r, s)
        (fun r1 r2 s        -> stg_asn1.Print_ExceptConstraint r1 r2, s)
        (fun r s            -> stg_asn1.Print_RootConstraint r, s)
        (fun r1 r2 s        -> stg_asn1.Print_RootConstraint2 r1 r2, s)
        (fun v rv s         -> stg_asn1.Print_SingleValueConstraint (printValue2 v),s)
        (fun v1 v2  b1 b2 s -> stg_asn1.Print_RangeConstraint (printValue v1) (printValue v2) b1 b2, s)
        (fun v1 b s         -> stg_asn1.Print_RangeConstraint_val_MAX (printValue v1) b ,s )
        (fun v2 b s         -> stg_asn1.Print_RangeConstraint_MIN_val (printValue v2) b, s)
        c
        0

let printRangeConstraint printValue (c:RangeTypeConstraint<'v1,'v1>)  =
    printRangeConstraint0 printValue printValue c

let printSizableConstraint printValue (c:SizableTypeConstraint<'v>)  =
    foldSizableTypeConstraint2
        (fun r1 r2 b s      -> stg_asn1.Print_UnionConstraint r1 r2, s)
        (fun r1 r2 s        -> stg_asn1.Print_IntersectionConstraint r1 r2 , s)
        (fun r s            -> stg_asn1.Print_AllExceptConstraint r, s)
        (fun r1 r2 s        -> stg_asn1.Print_ExceptConstraint r1 r2, s)
        (fun r s            -> stg_asn1.Print_RootConstraint r, s)
        (fun r1 r2 s        -> stg_asn1.Print_RootConstraint2 r1 r2, s)
        (fun v rv s         -> stg_asn1.Print_SingleValueConstraint (printValue v),s)
        (fun sc s           ->
            let sizeCon,_ = printRangeConstraint (fun ui -> ui.ToString()) sc
            stg_asn1.Print_SizeConstraint sizeCon, s)
        c
        0

let printAlphaConstraint printValue (c:IA5StringConstraint)  =
    foldStringTypeConstraint2
        (fun r1 r2 b s      -> stg_asn1.Print_UnionConstraint r1 r2, s)
        (fun r1 r2 s        -> stg_asn1.Print_IntersectionConstraint r1 r2 , s)
        (fun r s            -> stg_asn1.Print_AllExceptConstraint r, s)
        (fun r1 r2 s        -> stg_asn1.Print_ExceptConstraint r1 r2, s)
        (fun r s            -> stg_asn1.Print_RootConstraint r, s)
        (fun r1 r2 s        -> stg_asn1.Print_RootConstraint2 r1 r2, s)
        (fun v rv s         -> stg_asn1.Print_SingleValueConstraint (printValue v),s)
        (fun sc s           ->
            let sizeCon,_ = printRangeConstraint (fun ui -> ui.ToString()) sc
            stg_asn1.Print_SizeConstraint sizeCon, s)
        (fun sc s           ->
            let sizeCon,_ = printRangeConstraint0 (fun ui -> "\"" + ui.ToString() + "\"") (fun ui -> "\"" + ui.ToString() + "\"") sc
            stg_asn1.Print_AlphabetConstraint sizeCon, s)
        c
        0


let rec printReferenceToType (r:AstRoot) (p:PRINT_CONTENT) (ReferenceToType path) =
    match p with
    | REF -> path |> Seq.skip 1 |> Seq.toList |> List.map (fun x -> x.StrValue) |> Seq.StrJoin "."
    | CON  -> (PrintType r (r.typesMap.[(ReferenceToType path)])) //+ "--" + (path |> Seq.skip 1 |> Seq.toList |> List.map (fun x -> x.StrValue) |> Seq.StrJoin ".")

and printReferenceToValue (r:AstRoot) (p:PRINT_CONTENT) (ReferenceToValue (path, vpath)) =
    match p with
    | REF ->
        let p1 = path |> Seq.skip 1 |> Seq.toList |> List.map (fun x -> x.StrValue) |> Seq.StrJoin "."
        let p2 = vpath |> List.map (fun x -> x.StrValue) |> Seq.StrJoin "."
        match p2 = "" with
        | true  -> p1
        | false -> p1 + "." + p2
    | CON ->
        PrintAsn1GenericValue r r.valsMap.[(ReferenceToValue (path, vpath))]


and PrintAsn1GenericValue (r:AstRoot) (v:Asn1GenericValue) =
    match v with
    |IntegerValue(v)         -> stg_asn1.Print_IntegerValue v.Value
    |RealValue(v)            -> stg_asn1.Print_RealValue v.Value
    |StringValue(v)          -> stg_asn1.Print_StringValue v.Value
    |EnumValue(v)            -> stg_asn1.Print_StringValue v.Value
    |BooleanValue(v)         -> stg_asn1.Print_BooleanValue v.Value
    |BitStringValue(v)       -> stg_asn1.Print_BitStringValue v.Value
    |OctetStringValue(v)     -> stg_asn1.Print_OctetStringValue (v.Value )
    |SeqOfValue(v)           -> stg_asn1.Print_SeqOfValue (v.Value |> Seq.map (fun v -> printReferenceToValue r CON v.id) |> Seq.toArray)
    |SeqValue(v)             -> stg_asn1.Print_SeqValue (v.Value |> Seq.map(fun nv -> stg_asn1.Print_SeqValue_Child nv.name (printReferenceToValue r CON nv.Value.id) ) |> Seq.toArray)
    |ChValue(v)              -> stg_asn1.Print_ChValue v.Value.name (printReferenceToValue r CON v.Value.Value.id)
    |NullValue   _           -> stg_asn1.Print_NullValue()



and PrintType (r:AstRoot) (t:Asn1Type) =
    let inline cmb (t : ^T) =
        let cons     = (^T : (member Cons     : ^C list)(t))
        let withcons = (^T : (member WithCons : ^C list)(t))
        (cons |> List.map(fun x -> (false,x)))@(withcons |> List.map(fun x -> (true,x)))

    let printCon  printConFunc vGetter (b:bool,c:'con) =
        let s,_ = printConFunc vGetter  c
        match b with
        | true  -> sprintf "[%s]" s
        | false -> s

    match t with
    |Integer x       -> stg_asn1.Print_Integer (printUperRange x.uperRange) ( cmb x  |> List.map (printCon printRangeConstraint (fun x -> x.ToString()) ) )
    |Real  x         -> stg_asn1.Print_Real (cmb x |> List.map (printCon printRangeConstraint (fun x -> x.ToString()) ) )
    |Boolean x       -> stg_asn1.Print_Boolean (cmb x |> List.map (printCon printGenericConstraint (fun x -> x.ToString()) ) )
    |BitString x     -> stg_asn1.Print_BitString (cmb x |> List.map (printCon printSizableConstraint (fun x -> stg_asn1.Print_BitStringValue x.Value ) ) )
    |OctetString  x  -> stg_asn1.Print_OctetString (cmb x  |> List.map (printCon printSizableConstraint (fun x -> stg_asn1.Print_OctetStringValue x.Value) ) )
    |NullType _      -> stg_asn1.Print_NullType []
    |IA5String x  ->
        stg_asn1.Print_IA5String2 (printSizeMinMax x.minSize x.maxSize) (printCharSet x.charSet ) (cmb x |> List.map (printCon printAlphaConstraint (fun x -> x.ToString()) ) )
    |Enumerated x  ->
        let items =
            x.items |> List.map(fun itm -> stg_asn1.Print_Enumerated_child itm.name true (itm.Value.ToString() ))
        let cons = cmb x |> List.map (printCon printGenericConstraint (fun x -> x.ToString()) )
        stg_asn1.Print_Enumerated items  cons
    |Choice x   ->
        let printChild (c:ChChildInfo) = stg_asn1.Print_Choice_child c.name (printReferenceToType r CON c.chType.id)
        let cons = cmb x |> List.map (printCon printGenericConstraint (fun chv -> stg_asn1.Print_ChValue chv.Value.name (printReferenceToValue r CON chv.Value.Value.id)  ) )
        stg_asn1.Print_Choice (x.children |> Seq.map printChild |> Seq.toArray) cons
    |Sequence x ->
        let printChild (c:SeqChildInfo) =
            let bIsOptionalOrDefault, soDefValue =
                match c.optionality with
                | Some(CAst.AlwaysAbsent) ->  true, None
                | Some(CAst.AlwaysPresent) -> true, None
                | Some(CAst.Optional opt)  ->
                    match opt.defaultValue with
                    | None                  -> true, None
                    | Some v                -> true, Some (printReferenceToValue r CON v.id)
                | None                -> false, None
            stg_asn1.Print_Sequence_child c.name (printReferenceToType r CON c.chType.id) bIsOptionalOrDefault soDefValue
        let cons = cmb x |> List.map (printCon printGenericConstraint (fun sqv -> stg_asn1.Print_SeqValue (sqv.Value |> List.map(fun nmv -> stg_asn1.Print_SeqValue_Child nmv.name (printReferenceToValue r CON nmv.Value.id) ) )  ) )
        stg_asn1.Print_Sequence (x.children |> Seq.map printChild |> Seq.toArray) cons
    |SequenceOf x  ->
        let cons = cmb x |> List.map (printCon printSizableConstraint (fun sqofv -> stg_asn1.Print_SeqOfValue (sqofv.Value |> Seq.map (fun v -> printReferenceToValue r CON v.id) |> Seq.toArray) ) )
        stg_asn1.Print_SequenceOf (printReferenceToType r CON x.childType.id) cons


let PrintTypeAss (r:AstRoot) (t:Asn1Type)  =
    let nm = match t.asn1Name with Some x -> x | None -> "anonymous"
    let bnm = t.baseType |> Option.map (fun t -> printReferenceToType r REF t.id)
    stg_asn1.PrintTypeAssignment2 (printReferenceToType r REF t.id) bnm nm (PrintType r t)

let PrintValueAss (r:AstRoot) (v:Asn1GenericValue) =
    stg_asn1.PrintValueAssignment (printReferenceToValue r REF v.id) (printReferenceToType r REF v.refToType) (PrintAsn1GenericValue r v)

let PrintModule (r:AstRoot) (m:Asn1Module) =
    let exports =
        match m.Exports with
        | Ast.All               -> "ALL"
        | Ast.OnlySome exps     -> exps |> Seq.StrJoin ", "
    let importsFromModule =
        m.Imports |>
        List.map(fun im -> stg_asn1.PrintModuleImportFromModule ( (im.Types @ im.Values) |> List.map(fun s -> s.Value)) im.Name.Value )

    let tases = r.TypeAssignments |> Seq.filter(fun x ->x.id.ModName=m.Name) |> Seq.map(fun x -> PrintTypeAss r x ) |> Seq.toArray
    let vases = r.ValueAssignments |> Seq.filter(fun x ->x.id.ModName=m.Name && not x.childValue)|> Seq.map(fun x -> PrintValueAss r x )|> Seq.toArray
    stg_asn1.PrintModule m.Name tases vases exports importsFromModule

let PrintFile (r:AstRoot) (f:Asn1File) outDir newFileExt =
    let modules = f.Modules |> Seq.map (PrintModule r)|> Seq.toArray
    let fileContent = stg_asn1.PrintAsn1File modules
    let outFileName = Path.Combine(outDir, f.FileName+newFileExt)
    File.WriteAllText(outFileName, fileContent.Replace("\r",""))


let DoWork (r:AstRoot) outDir newFileExt=
    r.Files |> Seq.iter(fun f -> PrintFile r f outDir newFileExt)
