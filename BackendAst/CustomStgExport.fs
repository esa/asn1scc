module CustomStgExport

open System.Globalization
open System
open System.Numerics
open System.IO
open FsUtils
open CommonTypes
open DAst
open DastFold
open DAstUtilFunctions





let GetMinMax uperRange =
    match uperRange with
    | Asn1AcnAst.Concrete(min, max)      -> min.ToString(), max.ToString()
    | Asn1AcnAst.PosInf(a)               -> a.ToString(), "MAX"
    | Asn1AcnAst.NegInf(max)             -> "MIN", max.ToString()
    | Asn1AcnAst.Full                    -> "MIN", "MAX"

let handTypeWithMinMax name uperRange func stgFileName =
    let sMin, sMax = GetMinMax uperRange
    func name sMin sMax (sMin=sMax) stgFileName


let handTypeWithMinMax_real name (uperRange:Asn1AcnAst.uperRange<double>) func stgFileName =
    let GetMinMax (uperRange:Asn1AcnAst.uperRange<double>) =
        match uperRange with
        | Asn1AcnAst.Concrete(min, max)      -> min.ToString("E20", NumberFormatInfo.InvariantInfo), max.ToString("E20", NumberFormatInfo.InvariantInfo)
        | Asn1AcnAst.PosInf(a)               -> a.ToString("E20", NumberFormatInfo.InvariantInfo), "MAX"
        | Asn1AcnAst.NegInf(max)             -> "MIN", max.ToString("E20", NumberFormatInfo.InvariantInfo)
        | Asn1AcnAst.Full                    -> "MIN", "MAX"
    let sMin, sMax = GetMinMax uperRange
    func name sMin sMax (sMin=sMax) stgFileName


let PrintCustomAsn1Value (vas: ValueAssignment) stgFileName =
    let rec PrintValue (v: Asn1Value) =
        match v.kind with
        |IntegerValue(v)         -> gen.Print_IntegerValue v stgFileName
        |RealValue(v)            -> gen.Print_RealValue v stgFileName
        |StringValue(v)          -> gen.Print_StringValue v stgFileName
        |EnumValue enmv          -> gen.Print_RefValue enmv stgFileName //gen.Print_EnmValueValue enmv stgFileName
        |BooleanValue(v)         -> if v = true then gen.Print_TrueValue () stgFileName else gen.Print_FalseValue () stgFileName
        |BitStringValue(v)       -> gen.Print_BitStringValue v stgFileName
        |OctetStringValue(v)     -> gen.Print_OctetStringValue (v |> Seq.map(fun x -> x) |> Seq.toArray) stgFileName
        |RefValue((mn,nm),_)     -> gen.Print_RefValue nm stgFileName
        |SeqOfValue(vals)        -> gen.Print_SeqOfValue (vals |> Seq.map PrintValue |> Seq.toArray) stgFileName
        |SeqValue(vals)          -> gen.Print_SeqValue (vals |> Seq.map(fun nmv -> gen.Print_SeqValue_Child nmv.name (PrintValue nmv.Value) stgFileName ) |> Seq.toArray) stgFileName
        |ChValue(nmv)            -> gen.Print_ChValue nmv.name (PrintValue nmv.Value) stgFileName
        |NullValue _             -> gen.Print_NullValue() stgFileName
    PrintValue vas.Value

let PrintContract (tas:TypeAssignment) (r:AstRoot) (stgFileName:string) =
    let PrintPattern (tas:TypeAssignment) =
        let t = tas.Type
        match t.Kind with
        | Integer _ | BitString _ | OctetString _ | Real _ | IA5String _ |  SequenceOf(_)     -> gen.TypePatternCommonTypes () stgFileName
        | Boolean _ | NullType  _ | Enumerated(_)      | Choice(_)                                     -> null
        | Sequence seqInfo    ->
            let emitChild (c:SeqChildInfo) =
                match c with
                | Asn1Child c -> gen.SequencePatternChild c.Name.Value (ToC c.Name.Value) stgFileName
                | AcnChild  c -> null
            gen.TypePatternSequence tas.Name.Value (ToC tas.Name.Value) (seqInfo.children |> Seq.map emitChild) stgFileName
        | ReferenceType(_) -> null
    let rec PrintExpression (t:Asn1Type) (pattern:string) =
        match t.Kind with
        | Integer   intInfo     -> handTypeWithMinMax pattern intInfo.baseInfo.uperRange gen.ContractExprMinMax stgFileName
        | Real      realInfo    -> handTypeWithMinMax_real pattern realInfo.baseInfo.uperRange gen.ContractExprMinMax stgFileName
        | OctetString info      -> handTypeWithMinMax pattern (Asn1AcnAst.Concrete (info.baseInfo.minSize, info.baseInfo.maxSize)) gen.ContractExprSize stgFileName
        | IA5String   info      -> handTypeWithMinMax pattern (Asn1AcnAst.Concrete (info.baseInfo.minSize, info.baseInfo.maxSize)) gen.ContractExprSize stgFileName  
        | BitString   info      -> handTypeWithMinMax pattern (Asn1AcnAst.Concrete (info.baseInfo.minSize, info.baseInfo.maxSize)) gen.ContractExprSize stgFileName
        | Boolean   _
        | NullType  _
        | Choice _
        | Enumerated _          -> null
        | Sequence(seqInfo)    ->
             let emitChild (c:SeqChildInfo) =
                match c with
                | Asn1Child c -> PrintExpression c.Type (gen.SequencePatternChild c.Name.Value (ToC c.Name.Value) stgFileName)
                | AcnChild  c -> null
             let childArray = seqInfo.children |> Seq.map emitChild |> Seq.filter (fun x -> x <> null)
             gen.ContractExprSequence childArray stgFileName
        | SequenceOf info         ->
            let sMin, sMax = info.baseInfo.minSize.ToString(), info.baseInfo.maxSize.ToString()
            gen.ContractExprSize pattern sMin sMax (sMin = sMax) stgFileName
        | ReferenceType(_) -> null
    let pattern = PrintPattern tas
    let expression = PrintExpression tas.Type pattern
    gen.Contract pattern (if String.length(expression) > 0 then expression else null) stgFileName


let rec PrintType (r:AstRoot) (f:Asn1File) (stgFileName:string) modName (t:Asn1Type)    =
    let rec printAsn1Value (v:Asn1AcnAst.Asn1Value) = 
        match v.kind with
        | Asn1AcnAst.IntegerValue        v       -> stg_asn1.Print_IntegerValue v.Value
        | Asn1AcnAst.EnumValue           v       -> v.Value
        | Asn1AcnAst.RealValue           v       -> stg_asn1.Print_RealValue v.Value
        | Asn1AcnAst.StringValue         v       -> stg_asn1.Print_StringValue v.Value
        | Asn1AcnAst.BooleanValue        v       -> stg_asn1.Print_BooleanValue v.Value
        | Asn1AcnAst.BitStringValue      v       -> stg_asn1.Print_BitStringValue v.Value
        | Asn1AcnAst.OctetStringValue    v       -> stg_asn1.Print_OctetStringValue (v |> List.map (fun b -> b.Value))
        | Asn1AcnAst.RefValue       ((md,ts),_)  -> stg_asn1.Print_RefValue  ts.Value
        | Asn1AcnAst.SeqOfValue          vals    -> stg_asn1.Print_SeqOfValue (vals |> List.map printAsn1Value)
        | Asn1AcnAst.SeqValue            vals    -> stg_asn1.Print_SeqValue (vals |> List.map (fun nmv -> stg_asn1.Print_SeqValue_Child nmv.name.Value (printAsn1Value nmv.Value)))
        | Asn1AcnAst.ChValue             nmv     -> stg_asn1.Print_ChValue nmv.name.Value (printAsn1Value nmv.Value)
        | Asn1AcnAst.NullValue           _       -> stg_asn1.Print_NullValue ()

    let PrintTypeAux (t:Asn1Type) =
        match t.Kind with
        | Integer           i    -> handTypeWithMinMax (gen.IntegerType () stgFileName)         i.baseInfo.uperRange gen.MinMaxType stgFileName
        | BitString         i    -> handTypeWithMinMax (gen.BitStringType () stgFileName)       (Asn1AcnAst.Concrete (i.baseInfo.minSize, i.baseInfo.maxSize)) gen.MinMaxType2 stgFileName
        | OctetString       i    -> handTypeWithMinMax (gen.OctetStringType () stgFileName)     (Asn1AcnAst.Concrete (i.baseInfo.minSize, i.baseInfo.maxSize)) gen.MinMaxType2 stgFileName
        | Real              i    -> handTypeWithMinMax_real (gen.RealType () stgFileName)       i.baseInfo.uperRange gen.MinMaxType stgFileName
        | IA5String         i    -> handTypeWithMinMax (gen.IA5StringType () stgFileName)       (Asn1AcnAst.Concrete (i.baseInfo.minSize, i.baseInfo.maxSize)) gen.MinMaxType2 stgFileName
        | Boolean           i    -> gen.BooleanType () stgFileName
        | NullType          i    -> gen.NullType () stgFileName
        | Choice(chInfo)      ->
            let emitChild (c:ChChildInfo) =
                gen.ChoiceChild c.Name.Value (ToC c.Name.Value) (BigInteger c.Name.Location.srcLine) (BigInteger c.Name.Location.charPos) (PrintType r f stgFileName modName  c.chType ) (c.presentWhenName (Some c.chType.typeDefintionOrReference) C) stgFileName
            gen.ChoiceType (chInfo.children |> Seq.map emitChild) stgFileName
        | Sequence(seqInfo)    ->
            let emitChild (c:SeqChildInfo) =
                match c with
                | Asn1Child c -> 
                    match c.Optionality with
                    | Some(Asn1AcnAst.Optional(optVal)) when optVal.defaultValue.IsSome -> 
                           gen.SequenceChild c.Name.Value (ToC c.Name.Value) true (printAsn1Value optVal.defaultValue.Value) (BigInteger c.Name.Location.srcLine) (BigInteger c.Name.Location.charPos) (PrintType r f stgFileName modName c.Type  ) stgFileName
                    | _ -> gen.SequenceChild c.Name.Value (ToC c.Name.Value) c.Optionality.IsSome null (BigInteger c.Name.Location.srcLine) (BigInteger c.Name.Location.charPos) (PrintType r f stgFileName modName c.Type  ) stgFileName
                | AcnChild  c -> null
            gen.SequenceType (seqInfo.children |> Seq.map emitChild) stgFileName
        | Enumerated(enmInfo)     ->
            let emitItem (it : Asn1AcnAst.NamedItem) =
                gen.EnumItem it.Name.Value (ToC it.Name.Value) it.definitionValue (BigInteger it.Name.Location.srcLine) (BigInteger it.Name.Location.charPos) (it.CEnumName  C) stgFileName
            gen.EnumType (enmInfo.baseInfo.items |> Seq.map emitItem) stgFileName
        | SequenceOf info     ->
            let sMin, sMax = info.baseInfo.minSize.ToString(), info.baseInfo.maxSize.ToString()
            gen.SequenceOfType sMin sMax  (PrintType r f stgFileName modName  info.childType ) stgFileName
        | ReferenceType info ->
            let uperRange = 
                match (t.ActualType).Kind with
                | Integer       i         -> Some (GetMinMax i.baseInfo.uperRange)
                | BitString     i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | OctetString   i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | IA5String     i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | SequenceOf    i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | Real          i         -> Some (GetMinMax i.baseInfo.uperRange)
                | Boolean _ | NullType _ | Choice _ | Enumerated _ | Sequence _ | ReferenceType _       -> None
            let sModName = if info.baseInfo.modName.Value=modName then null else info.baseInfo.modName.Value
            let sCModName = if sModName <> null then (ToC sModName) else null
            match uperRange with
            | Some(sMin, sMax)  -> gen.RefTypeMinMax sMin sMax info.baseInfo.tasName.Value sModName (ToC info.baseInfo.tasName.Value) sCModName stgFileName
            | None              -> gen.RefType info.baseInfo.tasName.Value sModName (ToC info.baseInfo.tasName.Value) sCModName stgFileName

    gen.TypeGeneric (BigInteger t.Location.srcLine) (BigInteger t.Location.charPos) f.FileName (PrintTypeAux t) stgFileName

let exportFile (r:AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (stgFileName:string) (outFileName:string) =
    let AssigOp (t: Asn1Type) =
        match t.Kind with
        | Sequence(_) -> gen.AssigOpSpecialType () stgFileName
        | _           -> gen.AssigOpNormalType () stgFileName
    let PrintVas (f:Asn1File) (vas: ValueAssignment) modName =
        gen.VasXml vas.Name.Value (BigInteger vas.Name.Location.srcLine) (BigInteger vas.Name.Location.charPos) (PrintType r f stgFileName modName  vas.Type ) (PrintCustomAsn1Value vas stgFileName) (ToC vas.c_name)  stgFileName
    let PrintTas (f:Asn1File) (tas:TypeAssignment) modName =
        gen.TasXml tas.Name.Value (BigInteger tas.Name.Location.srcLine) (BigInteger tas.Name.Location.charPos) (PrintType r f stgFileName modName tas.Type ) (ToC tas.Name.Value) (AssigOp tas.Type) (PrintContract tas r stgFileName) stgFileName
    let PrintModule (f:Asn1File) (m:Asn1Module) =
        let PrintImpModule (im:Asn1Ast.ImportedModule) =
            gen.ImportedMod im.Name.Value (ToC im.Name.Value) (im.Types |> Seq.map(fun x -> x.Value)) (im.Values |> Seq.map(fun x -> x.Value)) stgFileName
        gen.ModuleXml m.Name.Value (ToC m.Name.Value) (m.Imports |> Seq.map PrintImpModule) m.ExportedTypes m.ExportedVars (m.TypeAssignments |> Seq.map (fun t -> PrintTas f t m.Name.Value)) (m.ValueAssignments |> Seq.map (fun t -> PrintVas f t m.Name.Value)) stgFileName

    let PrintFile (f:Asn1File) =
        gen.FileXml f.FileName (f.Modules |> Seq.map (PrintModule f)) stgFileName

    let content = gen.RootXml (r.Files |> Seq.map PrintFile) stgFileName
    File.WriteAllText(outFileName, content.Replace("\r",""))
