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
open Ast
open FsUtils

let rec PrintAsn1Value (v:Asn1Value) = 
    match v.Kind with
    |IntegerValue(v)         -> ASN.Print_IntegerValue v.Value
    |RealValue(v)            -> ASN.Print_RealValue v.Value
    |StringValue(v)          -> ASN.Print_StringValue v.Value
    |BooleanValue(v)         -> ASN.Print_BooleanValue v.Value
    |BitStringValue(v)       -> ASN.Print_BitStringValue v.Value
    |OctetStringValue(v)     -> ASN.Print_OctetStringValue (v |> Seq.map(fun x -> x.Value) |> Seq.toArray)
    |RefValue(mn,nm)         -> ASN.Print_RefValue nm.Value
    |SeqOfValue(vals)        -> ASN.Print_SeqOfValue (vals |> Seq.map PrintAsn1Value |> Seq.toArray)
    |SeqValue(vals)          -> ASN.Print_SeqValue (vals |> Seq.map(fun (nm, v) -> ASN.Print_SeqValue_Child nm.Value (PrintAsn1Value v) ) |> Seq.toArray)
    |ChValue(nm,v)           -> ASN.Print_ChValue nm.Value (PrintAsn1Value v)
    |NullValue               -> ASN.Print_NullValue()


let rec PrintConstraint (c:Asn1Constraint) = 
    match c with
    | SingleValueContraint(v)       -> ASN.Print_SingleValueContraint (PrintAsn1Value v)
    | RangeContraint(v1, v2, b1, b2)        -> ASN.Print_RangeContraint (PrintAsn1Value v1) (PrintAsn1Value v2) b1 b2
    | RangeContraint_val_MAX(v, b1)     -> ASN.Print_RangeContraint_val_MAX (PrintAsn1Value v) b1
    | RangeContraint_MIN_val(v, b2)     -> ASN.Print_RangeContraint_MIN_val (PrintAsn1Value v) b2  
    | RangeContraint_MIN_MAX        -> ASN.Print_RangeContraint_MIN_MAX()
    | TypeInclusionConstraint(mn,nm)-> 
        ASN.Print_TypeInclusionConstraint nm.Value       
    | SizeContraint(c)              -> ASN.Print_SizeContraint (PrintConstraint c)   
    | AlphabetContraint(c)          -> ASN.Print_AlphabetContraint (PrintConstraint c)   
    | UnionConstraint(c1,c2,virtualCon)        -> 
        match virtualCon with
        | false -> ASN.Print_UnionConstraint (PrintConstraint c1) (PrintConstraint c2)   
        | true  -> ""
    | IntersectionConstraint(c1,c2) -> ASN.Print_IntersectionConstraint (PrintConstraint c1) (PrintConstraint c2)          
    | AllExceptConstraint(c)        -> ASN.Print_AllExceptConstraint (PrintConstraint c)      
    | ExceptConstraint(c1,c2)       -> ASN.Print_ExceptConstraint (PrintConstraint c1) (PrintConstraint c2)                 
    | RootConstraint(c)             -> ASN.Print_RootConstraint  (PrintConstraint c)        
    | RootConstraint2(c1,c2)        -> ASN.Print_RootConstraint2 (PrintConstraint c1) (PrintConstraint c2)
    | WithComponentConstraint(c)    -> ASN.Print_WithComponentConstraint (PrintConstraint c)
    | WithComponentsConstraint(ncs) -> 
        let print_nc (nc:NamedConstraint) =
            let sConstraint = match nc.Contraint with
                              | Some(c1)     -> PrintConstraint c1
                              | None        -> ""
            let sPresMark = match nc.Mark with
                            | NoMark        -> ""
                            | MarkPresent   -> "PRESENT"
                            | MarkAbsent    -> "ABSENT"
                            | MarkOptional  -> "OPTIONAL"
            ASN.Print_WithComponentsConstraint_child nc.Name.Value sConstraint sPresMark
        ASN.Print_WithComponentsConstraint (ncs |> Seq.map  print_nc |> Seq.toArray)    

let rec PrintType (t:Asn1Type) (m:Asn1Module) =
    let cons = t.Constraints |> Seq.map PrintConstraint |> Seq.toArray
    match t.Kind with
    |Integer    -> ASN.Print_Integer "" cons
    |Real       -> ASN.Print_Real cons
    |Boolean    -> ASN.Print_Boolean cons
    |BitString  -> ASN.Print_BitString cons
    |OctetString-> ASN.Print_OctetString cons
    |NullType   -> ASN.Print_NullType cons
    |IA5String  -> ASN.Print_IA5String cons
    |NumericString -> ASN.Print_NumericString cons
    |Enumerated(items)  ->
        let printItem (it:NamedItem) = ASN.Print_Enumerated_child it.Name.Value it._value.IsSome (if it._value.IsSome then (PrintAsn1Value it._value.Value) else "")
        ASN.Print_Enumerated (items |> Seq.map printItem |> Seq.toArray) cons
    |Choice(children)   ->
        let printChild (c:ChildInfo) = ASN.Print_Choice_child c.Name.Value (PrintType c.Type m)
        ASN.Print_Choice (children |> Seq.map printChild |> Seq.toArray) cons
    |Sequence(children) ->
        let printChild (c:ChildInfo) = 
            let bIsOptionalOrDefault, soDefValue = 
                match c.Optionality with
                |Some(Optional(_))   -> true, None
                |Some(Default(v))    -> true, Some (PrintAsn1Value v)
                |_                   -> false, None
            ASN.Print_Sequence_child c.Name.Value (PrintType c.Type m) bIsOptionalOrDefault soDefValue
        ASN.Print_Sequence (children |> Seq.map printChild |> Seq.toArray) cons
    |SequenceOf(child)  -> ASN.Print_SequenceOf (PrintType child m) cons
    |ReferenceType(mname, name, _) ->  
        match m.Name.Value = mname.Value with
        | true -> ASN.Print_ReferenceType1 name.Value cons
        | false -> ASN.Print_ReferenceType2 mname.Value name.Value cons
        

let PrintTypeAss (t:TypeAssignment) m = ASN.PrintTypeAssignment t.Name.Value (PrintType t.Type m)

let PrintValueAss (v:ValueAssignment) m = ASN.PrintValueAssignment v.Name.Value (PrintType v.Type m) (PrintAsn1Value v.Value)

let PrintModule (m:Asn1Module) =
    let exports =
        match m.Exports with
        | All               -> "ALL"
        | OnlySome exps     -> exps |> Seq.StrJoin ", "
    let importsFromModule =
        m.Imports |>
        List.map(fun im -> ASN.PrintModuleImportFromModule ( (im.Types @ im.Values) |> List.map(fun s -> s.Value)) im.Name.Value )

    let tases = m.TypeAssignments |> Seq.map(fun x -> PrintTypeAss x m) |> Seq.toArray
    let vases = m.ValueAssignments |> Seq.map(fun x -> PrintValueAss x m)|> Seq.toArray
    ASN.PrintModule m.Name.Value tases vases exports importsFromModule

let PrintFile (f:Asn1File) outDir newFileExt =
    let modules = f.Modules |> Seq.map PrintModule |> Seq.toArray
    let fileContent = ASN.PrintAsn1File modules
    let outFileName = Path.Combine(outDir, f.FileNameWithoutExtension+newFileExt)
    File.WriteAllText(outFileName, fileContent.Replace("\r",""))


let DoWork (r:AstRoot) outDir newFileExt=
    r.Files |> Seq.iter(fun f -> PrintFile f outDir newFileExt)



module AcnPrint =
    open AcnTypes
    let printAcnAsn1Type (x:AcnTypes.AcnAsn1Type) =
        match x with
        | Integer   -> "INTEGER"
        | Boolean   -> "BOOLEAN"
        | NullType  -> "NULL"
        | RefTypeCon(_,tasName) -> tasName.Value
    
    let print_AcnType (t:AcnType) =
        let typeId = t.TypeID.StrJoin(".")
        let props = sprintf "%A" t.Properties
        let implMode = sprintf "Mode = %A" t.ImpMode
        sprintf "TypeID = %s\nProperties = %s\n%s" typeId props implMode

    let print_AcnConstrant(c:AcnConstant) =
        sprintf "%A" c
        
    let print_AcnParameter(p:AcnParameter) =
        sprintf "%s.%s %s:%s" p.ModName p.TasName p.Name (printAcnAsn1Type p.Asn1Type)

    let print_LongReference(p:LongReference) =
        let typeId = p.TypeID.StrJoin(".")
        let relPath = p.LongRef |> Seq.StrJoin "."
        sprintf "TypeID = %s\nLongRef = %s\nKind = %A" typeId relPath p.Kind

    let print_AcnAst(r:AcnAst, fileName:string) =
        use file1 = File.CreateText(fileName)
        file1.WriteLine("=============Acn Types============") 
        for x in r.Types do
            file1.WriteLine (print_AcnType x)
            file1.WriteLine()
        file1.WriteLine("=============Acn Params============") 
        for x in r.Parameters do
            file1.WriteLine (print_AcnParameter x)
            file1.WriteLine()
        file1.WriteLine("=============Long References============") 
        for x in r.References do
            file1.WriteLine (print_LongReference x)
            file1.WriteLine()
        file1.WriteLine("=============Acn Constants============") 
        for x in r.Constants do
            file1.WriteLine (print_AcnConstrant x)
            file1.WriteLine()

    let print_LongReferenceResolved (acn:AcnAstResolved) (r:LongReferenceResolved) =
        let print_point  = function
            | TypePoint(path)   -> "TY("+path.Tail.StrJoin(".")+")"
            | ParamPoint(path)  -> "PP("+path.Tail.StrJoin(".")+")"
            | TempPoint(path)   -> "TE("+path.Tail.StrJoin(".")+")"
        let print_kind = function
            | SizeDeterminant        -> "SizeDeterminant"
            | RefTypeArgument(pName) -> sprintf "RefTypeArgument %s" pName
            | PresenceBool           -> "PresenceBool"
            | PresenceInt(nVal)      -> sprintf  "PresenceInt %s" ((AcnTypes.EvaluateConstant acn.Constants nVal).ToString())
            | PresenceStr(sVal)      -> sprintf "PresenceStr %s" sVal
            | ChoiceDeteterminant    -> "ChoiceDeteterminant"

        sprintf "%-45s%-45s%s" (print_point r.decType) (print_point r.determinant) (print_kind r.Kind)

    let print_AcnAst2(r:AcnAstResolved, fileName:string) =
        use file1 = File.CreateText(fileName)
        file1.WriteLine("=============Acn Params============") 
        for x in r.Parameters do
            file1.WriteLine (print_AcnParameter x)
        file1.WriteLine("=============Resolved References============") 
        for x in r.References do
            file1.WriteLine (print_LongReferenceResolved r x)
        file1.WriteLine("=============Acn Constants============") 
        for x in r.Constants do
            file1.WriteLine (print_AcnConstrant x)

    let printProperty (acn:AcnTypes.AcnAstResolved) (p:AcnTypes.AcnProperty) =
        match p with
        | Encoding PosInt           -> ACN.PP_Encoding_PosInt()
        | Encoding TwosComplement   -> ACN.PP_Encoding_TwosComplement()
        | Encoding Ascii            -> ACN.PP_Encoding_Ascii()
        | Encoding BCD              -> ACN.PP_Encoding_BCD()
        | Encoding IEEE754_32       -> ACN.PP_Encoding_IEEE754_32()
        | Encoding IEEE754_64       -> ACN.PP_Encoding_IEEE754_64()
//        | SizeProperty Auto         -> ACN.PP_Size_Auto()
        | SizeProperty (Fixed vl)   -> ACN.PP_Size_Fixed (AcnTypes.EvaluateConstant acn.Constants vl)
        | SizeProperty (NullTerminated _)-> ACN.PP_Size_NullTerminated()
//        | Adjust(vl)                -> ACN.PP_Adjust (AcnTypes.EvaluateConstant acn.Constants vl)
        | Aligment NextByte         -> ACN.PP_Aligment_byte()
        | Aligment NextWord         -> ACN.PP_Aligment_word()
        | Aligment NextDWord        -> ACN.PP_Aligment_dword()
        | EncodeValues              -> ACN.PP_EncodeValues()
        | BooleanEncoding(TrueValue sVal)  -> ACN.PP_TrueValue sVal.Value
        | BooleanEncoding(FalseValue sVal)  -> ACN.PP_FalseValue sVal.Value
        | NullValue(sVal)           -> ACN.PP_NullValue sVal.Value
        | Endianness LittleEndianness -> ACN.PP_Endianness_little()
        | Endianness BigEndianness  -> ACN.PP_Endianness_big()
        | EnumeratorResetValue(newVal,ff)   -> newVal.ToString()+"="+ff.ToString() //raise(BugErrorException "")
        | MappingFunction funcName          -> ACN.PP_MappingFunction funcName.Value

    let printReferenceAsProperty (acn:AcnTypes.AcnAstResolved) (r:AcnTypes.LongReferenceResolved) =
        let printDeterminant (p:AcnTypes.Point) =
            match p with
            | TypePoint(pth)    -> pth.Tail.Tail.StrJoin "."
            | ParamPoint(pth)   -> pth |> List.rev |> List.head
            | TempPoint(pth)    -> pth |> List.rev |> List.head
        let lngFld = printDeterminant r.determinant
        match r.Kind with
        | SizeDeterminant               -> ACN.PP_Sixe_Fld lngFld
        | RefTypeArgument(sParamName)   -> raise(BugErrorException "not a property")
        | PresenceBool                  -> ACN.PP_PresentWhenConditionBool lngFld           
        | PresenceInt(nVal)             -> ACN.PP_PresentWhenConditionInt lngFld (AcnTypes.EvaluateConstant acn.Constants nVal)
        | PresenceStr(sVal)             -> ACN.PP_PresentWhenConditionStr lngFld sVal
        | ChoiceDeteterminant           -> ACN.PP_ChoiceDeterminant lngFld

    let rec PrintAcnType (asn1:AstRoot) (acn:AcnTypes.AcnAstResolved) (m:Asn1Module) (t:Asn1Type) key =
        let props =  t.AcnProperties |> Seq.map(fun p -> printProperty acn p ) |> Seq.toList
        let props2 = acn.References 
                     |> Seq.filter(fun x -> match x.Kind with RefTypeArgument(_) -> false | _ -> true)
                     |> Seq.filter(fun x -> x.decType.AbsPath = key) 
                     |> Seq.map(fun r -> printReferenceAsProperty acn r) |> Seq.toList
        let len = Seq.length key
        let arrChildren = seq {
            match t.Kind with                
                | Sequence(children) | Choice(children) ->
                        for ch in children do
                            let newKey = key@[ch.Name.Value]
                            let args = acn.References 
                                       |> Seq.filter(fun x -> match x.Kind with RefTypeArgument(_) -> true |_ -> false )
                                       |> Seq.filter(fun x -> x.decType.AbsPath = newKey )
                                       |> Seq.map(fun r -> r.determinant.AbsPath.StrJoin ".")
                            let asn1TypeName = match ch.Type.Kind with
                                               | ReferenceType(_)|Ast.Integer|Ast.NullType|Ast.Boolean ->(PrintType ch.Type m)
                                               | _      -> ""
                            let impMode = match ch.AcnInsertedField with
                                          | false   -> asn1TypeName
                                          | true    -> asn1TypeName+ "(*)"
                            let sType = PrintAcnType asn1 acn m ch.Type newKey
                            yield ACN.PrintTypeChild ch.Name.Value args impMode sType 
                | SequenceOf(child) ->
                        let newKey = key@["#"]
                        let sType = PrintAcnType asn1 acn m child newKey
                        yield ACN.PrintTypeChild (PrintType child m) [] "" sType
                |_          -> () 
            }
                
        ACN.PrintType (props@props2) arrChildren


    let PrintTas (asn1:AstRoot) (acn:AcnTypes.AcnAstResolved) (m:Asn1Module) (tas:TypeAssignment) =
        let parms = acn.Parameters |> Seq.filter(fun p -> p.ModName = m.Name.Value && p.TasName = tas.Name.Value)
        let PrintParamMode p = match p with DecodeMode -> "DecIN" |EncodeDecodeMode -> "EncIN_DecOUT"
        let arParams = parms |> Seq.map(fun x -> ACN.PrintParam x.Name (printAcnAsn1Type x.Asn1Type) (PrintParamMode x.Mode) )
        let arTempTypes = acn.TmpTypes 
                          |> Seq.filter(fun x -> x.ModName = m.Name.Value && x.TasName = tas.Name.Value)
                          |> Seq.map(fun x -> ACN.PrintTemp x.Name (printAcnAsn1Type x.Asn1Type) )
        let tp = PrintAcnType asn1 acn m tas.Type [m.Name.Value; tas.Name.Value]
        ACN.PrintTypeAssignment tas.Name.Value arParams arTempTypes tp

    let PrintModule (asn1:AstRoot) (acn:AcnTypes.AcnAstResolved)  (m:Asn1Module) =
        ACN.PrintModule m.Name.Value [for tas in m.TypeAssignments -> PrintTas asn1 acn m tas ]

    

//    let DoWork (asn1:AstRoot) (prms : seq<AcnParameter>)  (refs : seq<LongReference>) outFileName =
//        let modContents = asn1.Modules |> Seq.map(fun m -> PrintModule asn1 (prms|>Seq.toList) (refs|>Seq.toList) m)
//        let fileContent = ACN.PrintAsn1File modContents
//        File.WriteAllText(outFileName, fileContent.Replace("\r",""))


let DebugPrintAsn1Acn (asn1:AstRoot)  (acn:AcnTypes.AcnAstResolved) outDir newFileExt =
    let PrintFile f =
        let modules = f.Modules |> Seq.map PrintModule |> Seq.toArray
        let fileContent = ASN.PrintAsn1File modules
        let outFileName = Path.Combine(outDir, f.FileNameWithoutExtension+newFileExt)
        use file1 = File.CreateText(outFileName)
        file1.NewLine <- "\n"
        file1.WriteLine(fileContent.Replace("\r",""))
        file1.WriteLine("/* ======== ACN SPEC ======== */");
        let modContents = f.Modules |> Seq.map(fun m -> AcnPrint.PrintModule asn1 acn  m)
        let acnfileContent = ACN.PrintAsn1File modContents
        file1.WriteLine(acnfileContent.Replace("\r",""))
        file1.WriteLine("/* ======== References ======== */");
        for x in acn.References do
            let tmp= AcnPrint.print_LongReferenceResolved acn x
            file1.WriteLine (tmp)
        file1.WriteLine("/* ======== Parameters ======== */");
        for x in acn.Parameters do
            let tmp = AcnPrint.print_AcnParameter x
            file1.WriteLine(tmp)
        file1.WriteLine("/* ======== Virtual References ======== */");
        for x in acn.References do
            match x.determinant with
            | AcnTypes.TypePoint(pth)    when Acn.aux.IsVirtualPath asn1 pth -> 
                let tmp= AcnPrint.print_LongReferenceResolved acn x
                file1.WriteLine (tmp)
                let points = Acn.aux.BreakVPath asn1 pth
                for (refAbsPath, (md,tas)) in points do
                    file1.WriteLine("\t{0}\t{1}.{2}", refAbsPath.StrJoin("."), md, tas)
                file1.WriteLine()
            | _                 -> ()

    asn1.Files |> Seq.iter PrintFile
    