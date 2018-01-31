module GrammarGenerator

open System
open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open FsUtils
open AcnCreateFromAntlr
open System.IO
open AcnCreateFromAntlr
open ParameterizedAsn1Ast

let toIdxList lst = lst |> List.mapi(fun i x -> (i,x))


type GenTas = {
    name    : string
    Type    : ParameterizedAsn1Ast.Asn1Type
    acnProps: GenericAcnProperty list
}

type GenMod = {
    name    : string
    tases   : GenTas list
}

type GenFile = {
    name    : string
    mods    : GenMod list
}

let genrateAcnIntProps (c:ParameterizedAsn1Ast.Asn1Constraint Option)  =
    seq {
        yield []
        for encoding in [GP_PosInt; GP_TwosComplement; GP_Ascii; GP_BCD] do
            for sizeProp in [GP_Fixed (IntLoc.ByValue 64I); GP_NullTerminated ] do
                yield [ENCODING encoding; SIZE sizeProp]
                for endianess in [Asn1AcnAst.LittleEndianness; Asn1AcnAst.BigEndianness] do
                    yield [ENCODING encoding; SIZE sizeProp; ENDIANNES endianess]
                    for align in [Asn1AcnAst.NextByte; Asn1AcnAst.NextWord; Asn1AcnAst.NextDWord] do
                        yield [ENCODING encoding; SIZE sizeProp; ENDIANNES endianess; ALIGNTONEXT align]

    } |> Seq.toList



let generatedIntConstraints (v1:BigInteger option) (v2:BigInteger option)= 
    let gv v = {ParameterizedAsn1Ast.Asn1Value.Kind = ParameterizedAsn1Ast.IntegerValue (IntLoc.ByValue v); ParameterizedAsn1Ast.Asn1Value.Location = emptyLocation}
    let a = v1 |> Option.map gv
    let b = v1 |> Option.map gv
    let svcs = 
        seq {
            match a, b with
            | Some a, Some b ->
                yield ParameterizedAsn1Ast.SingleValueContraint a
                yield ParameterizedAsn1Ast.SingleValueContraint b
                yield ParameterizedAsn1Ast.RangeContraint(a,b,true, true)
                yield ParameterizedAsn1Ast.RangeContraint(a,b,true, false)
                yield ParameterizedAsn1Ast.RangeContraint(a,b,false, true)
                yield ParameterizedAsn1Ast.RangeContraint(a,b,false, false)
                yield ParameterizedAsn1Ast.RangeContraint_val_MAX(a,true)
                yield ParameterizedAsn1Ast.RangeContraint_val_MAX(a,false)
                yield ParameterizedAsn1Ast.RangeContraint_val_MAX(b,true)
                yield ParameterizedAsn1Ast.RangeContraint_val_MAX(b,false)

                yield ParameterizedAsn1Ast.RangeContraint_MIN_val(a,true)
                yield ParameterizedAsn1Ast.RangeContraint_MIN_val(a,false)
                yield ParameterizedAsn1Ast.RangeContraint_MIN_val(b,true)
                yield ParameterizedAsn1Ast.RangeContraint_MIN_val(b,false)
            | Some a, None  ->
                yield ParameterizedAsn1Ast.SingleValueContraint a
                yield ParameterizedAsn1Ast.RangeContraint_val_MAX(a,true)
                yield ParameterizedAsn1Ast.RangeContraint_val_MAX(a,false)
                yield ParameterizedAsn1Ast.RangeContraint_MIN_val(a,true)
                yield ParameterizedAsn1Ast.RangeContraint_MIN_val(a,false)
            | None, Some b      ->
                yield ParameterizedAsn1Ast.SingleValueContraint b
                yield ParameterizedAsn1Ast.RangeContraint_val_MAX(b,true)
                yield ParameterizedAsn1Ast.RangeContraint_val_MAX(b,false)
                yield ParameterizedAsn1Ast.RangeContraint_MIN_val(b,true)
                yield ParameterizedAsn1Ast.RangeContraint_MIN_val(b,false)
            | None, None        -> ()

        } |> Seq.toList
    svcs


let genetateIntegers (name:string) =
    let nums =  [
                BigInteger Int64.MinValue; BigInteger Int32.MinValue; BigInteger (int Int16.MinValue); BigInteger (int SByte.MinValue); 0I; 
                BigInteger (int SByte.MaxValue); BigInteger (int Byte.MaxValue); BigInteger (int Int16.MaxValue); BigInteger (int UInt16.MaxValue)
                BigInteger Int32.MaxValue; BigInteger UInt32.MaxValue; BigInteger Int64.MaxValue; BigInteger UInt64.MaxValue]
    let nums = [BigInteger Int64.MinValue; BigInteger Int64.MaxValue]
    
    let pairs = 
        seq {
            for a  in nums do
                yield (Some a, None)
                yield (None, Some a)
                for b  in nums do
                    if (a<=b) then
                        yield (Some a,Some b)
        } |> Seq.toList
    seq {
            for (i1,(a,b))  in (toIdxList pairs) do
                let cons = generatedIntConstraints a b |> List.map(fun c -> Some c)
                for (i2, c) in (toIdxList (None::cons)) do
                    let asn1Type =  {ParameterizedAsn1Ast.Asn1Type.Kind = ParameterizedAsn1Ast.Integer; ParameterizedAsn1Ast.Constraints = Option.toList c ; ParameterizedAsn1Ast.Location=emptyLocation; parameterizedTypeInstance = false}    
                    let acnSpec = genrateAcnIntProps c
                    for (i3,s) in (toIdxList acnSpec) do
                        let newName = sprintf "%s-%d-%d-%d" name i1 i2 i3
                        yield (newName, asn1Type, s)
    } |> Seq.toList
    

let createAst files =
    seq {
        for f in files do
            let intTasses = genetateIntegers "INT" |> List.map (fun (newName, asn1Type, acnProps) -> {GenTas.name = newName; Type = asn1Type; acnProps = acnProps})
            let modu = {GenMod.tases = intTasses; name = "TEST-CASE"}
            let file = {GenFile.name = f; mods = [modu]}
            yield file
    } |> Seq.toList



let rec printValue (v:Asn1Value) : string =
    match v.Kind with
    |   IntegerValue        v       -> stg_asn1.Print_IntegerValue v.Value
    |   RealValue           v       -> stg_asn1.Print_RealValue v.Value
    |   StringValue         v       -> stg_asn1.Print_StringValue v.Value
    |   BooleanValue        v       -> stg_asn1.Print_BooleanValue v.Value
    |   BitStringValue      v       -> stg_asn1.Print_BitStringValue v.Value
    |   OctetStringValue    v       -> stg_asn1.Print_OctetStringValue (v |> List.map (fun b -> b.Value))
    |   RefValue            (md,ts) -> stg_asn1.Print_RefValue  ts.Value
    |   SeqOfValue          vals    -> stg_asn1.Print_SeqOfValue (vals |> List.map printValue)
    |   SeqValue            vals    -> stg_asn1.Print_SeqValue (vals |> List.map (fun (nm, v) -> stg_asn1.Print_SeqValue_Child nm.Value (printValue v)))
    |   ChValue             (nm,v)  -> stg_asn1.Print_ChValue nm.Value (printValue v)
    |   NullValue                   -> stg_asn1.Print_NullValue ()
    |   EmptyList                   -> sprintf "{}"

let generatedAsn1Grammar (outDir:string) (ast:GenFile list) =

    let rec printConstraint (c:Asn1Constraint) : string = 
        match c with
        | SingleValueContraint      v               -> stg_asn1.Print_SingleValueContraint (printValue v)
        | RangeContraint            (a,b,b1,b2)     -> stg_asn1.Print_RangeContraint (printValue a) (printValue b) b1 b2
        | RangeContraint_val_MAX    (a, b1)         -> stg_asn1.Print_RangeContraint_val_MAX (printValue a) b1
        | RangeContraint_MIN_val    (a, b1)         -> stg_asn1.Print_RangeContraint_MIN_val (printValue a) b1
        | TypeInclusionConstraint   (md,ts)         -> stg_asn1.Print_TypeInclusionConstraint(ts.Value)
        | SizeContraint             sc              -> stg_asn1.Print_SizeContraint (printConstraint sc)
        | AlphabetContraint         alpha           -> stg_asn1.Print_AlphabetContraint (printConstraint alpha)
        | UnionConstraint           (c1,c2, _)      -> stg_asn1.Print_UnionConstraint (printConstraint c1) (printConstraint c2)
        | IntersectionConstraint    (c1,c2)         -> stg_asn1.Print_IntersectionConstraint (printConstraint c1) (printConstraint c2)
        | AllExceptConstraint       c1              -> stg_asn1.Print_AllExceptConstraint (printConstraint c1)
        | ExceptConstraint          (c1,c2)         -> stg_asn1.Print_ExceptConstraint (printConstraint c1) (printConstraint c2)
        | RootConstraint            c1              -> stg_asn1.Print_RootConstraint (printConstraint c1)
        | RootConstraint2           (c1,c2)         -> stg_asn1.Print_RootConstraint2 (printConstraint c1) (printConstraint c2)
        | WithComponentConstraint   c1              -> stg_asn1.Print_WithComponentConstraint (printConstraint c1)
        | WithComponentsConstraint  ncs             -> 
            let print nc =
                stg_asn1.Print_WithComponentsConstraint_child nc.Name.Value (match nc.Contraint with Some c -> printConstraint c | None -> "") (sprintf "%A" nc.Mark)
            stg_asn1.Print_WithComponentsConstraint (ncs |> List.map print)

    let printAsn1Type (t:Asn1Type) : string = 
        match t.Kind with
        | Integer   -> stg_asn1.Print_Integer "" (t.Constraints |> List.map printConstraint)
        | _         -> raise(BugErrorException "Not Implemented yet")
    let printTas (tas:GenTas) : string=
        stg_asn1.PrintTypeAssignment tas.name (printAsn1Type tas.Type)
    let printModule (m:GenMod) : string=
        stg_asn1.PrintModule m.name (m.tases |> List.map printTas) [] "" []
    let generateAsn1File (f:GenFile) =
        let content = stg_asn1.PrintAsn1File (f.mods |> List.map printModule)
        let outCFileName = Path.Combine(outDir, f.name + ".asn1")
        File.WriteAllText(outCFileName, content.Replace("\r",""))
    ast |> List.iter generateAsn1File


let generatedAcnGrammar (outDir:string) (ast:GenFile list) =
    let printAcnProp (p:GenericAcnProperty) : string =
        match p with
        | ENCODING GP_PosInt                    -> stg_acn.PP_Encoding_PosInt ()
        | ENCODING GP_TwosComplement            -> stg_acn.PP_Encoding_TwosComplement()
        | ENCODING GP_Ascii                     -> stg_acn.PP_Encoding_Ascii()
        | ENCODING GP_BCD                       -> stg_acn.PP_Encoding_BCD()
        | SIZE (GP_Fixed i)                     -> stg_acn.PP_Size_Fixed (i.Value)
        | SIZE (GP_SizeDeterminant  fld)        -> stg_acn.PP_Sixe_Fld (fld.AsString)
        | SIZE GP_NullTerminated                -> stg_acn.PP_Size_NullTerminated ()
        | ALIGNTONEXT Asn1AcnAst.NextByte       -> stg_acn.PP_Aligment_byte ()
        | ALIGNTONEXT Asn1AcnAst.NextWord       -> stg_acn.PP_Aligment_word ()
        | ALIGNTONEXT Asn1AcnAst.NextDWord      -> stg_acn.PP_Aligment_dword ()
        | ENDIANNES Asn1AcnAst.LittleEndianness -> stg_acn.PP_Endianness_little ()
        | ENDIANNES Asn1AcnAst.BigEndianness    -> stg_acn.PP_Endianness_big ()
        | _         -> raise(BugErrorException "Not Implemented yet")

    let printAcnType (t:ParameterizedAsn1Ast.Asn1Type) (acnProps: GenericAcnProperty list): string =
        let props = acnProps |> List.map printAcnProp
        match t.Kind with
        | Integer   -> stg_acn.PrintType props []
        | _         -> raise(BugErrorException "Not Implemented yet")
    let printTas (tas:GenTas) : string=
        stg_acn.PrintTypeAssignment tas.name [] [] (printAcnType tas.Type tas.acnProps)
    let printModule (m:GenMod) =
        stg_acn.PrintModule m.name (m.tases |> List.map printTas)
    let generateAcnFile (f:GenFile) =
        let content = stg_acn.PrintAsn1File (f.mods |> List.map printModule)
        let outCFileName = Path.Combine(outDir, f.name + ".acn")
        File.WriteAllText(outCFileName, content.Replace("\r",""))
    ast |> List.iter generateAcnFile

let generateGrammars (path:string) =
    
    let genAst = createAst ["test-grammar"]
    generatedAsn1Grammar path genAst
    generatedAcnGrammar path genAst
    