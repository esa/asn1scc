module CustomStgExport
#nowarn "3536"

open System.Globalization
open System
open System.Numerics
open System.IO
open FsUtils
open CommonTypes
open AbstractMacros
open DAst
open DastFold
open DAstUtilFunctions



let GetMinMax uperRange =
    match uperRange with
    | CommonTypes.Concrete(min, max)      -> min.ToString(), max.ToString()
    | CommonTypes.PosInf(a)               -> a.ToString(), "MAX"
    | CommonTypes.NegInf(max)             -> "MIN", max.ToString()
    | CommonTypes.Full                    -> "MIN", "MAX"

let handTypeWithMinMax name uperRange func  stgFileName =
    let sMin, sMax = GetMinMax uperRange
    func name sMin sMax (sMin=sMax) stgFileName


let handTypeWithMinMax_real name (uperRange:Asn1AcnAst.DoubleUperRange) func stgFileName =
    let GetMinMax (uperRange:Asn1AcnAst.DoubleUperRange) =
        match uperRange with
        | CommonTypes.Concrete(min, max)      -> min.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo), max.ToString("E20", NumberFormatInfo.InvariantInfo)
        | CommonTypes.PosInf(a)               -> a.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo), "MAX"
        | CommonTypes.NegInf(max)             -> "MIN", max.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo)
        | CommonTypes.Full                    -> "MIN", "MAX"
    let sMin, sMax = GetMinMax uperRange
    func name sMin sMax (sMin=sMax) stgFileName

let internal PrintCustomAsn1Value_aux (bPrintAsAttr:bool) (v: Asn1Value) stgFileName =
    let rec PrintValue (bChildVal:bool) (v: Asn1Value) =
        match v.kind with
        |IntegerValue(v)         -> gen.Print_IntegerValue v stgFileName
        |RealValue(v)            -> gen.Print_RealValue v stgFileName
        |StringValue(v,_)          ->
//            match bPrintAsAttr with
//            | true   ->
//                //printfn "%s\n" v
//                let retVal = v.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;").Replace("\"", "&quot;").Replace("'", "&apos;")
//                //printfn "%s\n" retVal
//                match bChildVal with
//                | true  ->  "&quot;" + retVal + "&quot;"
//                | false -> retVal
//            | false  ->
            let strVal = CommonTypes.StringValue2String v
            gen.Print_StringValue strVal stgFileName
        |EnumValue enmv          -> gen.Print_RefValue enmv stgFileName //gen.Print_EnmValueValue enmv stgFileName
        |BooleanValue(v)         -> if v = true then gen.Print_TrueValue () stgFileName else gen.Print_FalseValue () stgFileName
        |BitStringValue(v)       -> gen.Print_BitStringValue v stgFileName
        |OctetStringValue(v)     -> gen.Print_OctetStringValue (v |> Seq.map(fun x -> x) |> Seq.toArray) stgFileName
        |RefValue((mn,nm),_)     -> gen.Print_RefValue nm stgFileName
        |SeqOfValue(vals)        -> gen.Print_SeqOfValue (vals |> Seq.map (PrintValue true) |> Seq.toArray) stgFileName
        |SeqValue(vals)          -> gen.Print_SeqValue (vals |> Seq.map(fun nmv -> gen.Print_SeqValue_Child nmv.name (PrintValue true nmv.Value) stgFileName ) |> Seq.toArray) stgFileName
        |ChValue(nmv)            -> gen.Print_ChValue nmv.name (PrintValue true nmv.Value) stgFileName
        |NullValue _             -> gen.Print_NullValue() stgFileName
        |ObjOrRelObjIdValue v    -> gen.Print_ObjectIdentifierValue((v.Values |> List.map fst)) stgFileName
        |other -> raise (BugErrorException $"Unsupported kind for PrintValue {other}")
    PrintValue false v

let PrintCustomAsn1Value  (vas: ValueAssignment) stgFileName =
    PrintCustomAsn1Value_aux false vas.Value stgFileName

//let rec printAsn1ValueAsXmlAttribute (v: Asn1Value) stgFileName =
//    PrintCustomAsn1Value_aux true v stgFileName

let PrintContract (r:AstRoot) (stgFileName:string) (asn1Name:string) (backendName:string) (t:Asn1Type)=
    let PrintPattern () =
        //let t = tas.Type
        match t.Kind with
        | Integer _ | BitString _ | OctetString _ | Real _ | IA5String _ |  SequenceOf(_)  | ObjectIdentifier _    -> gen.TypePatternCommonTypes () stgFileName
        | Boolean _ | NullType  _ | Enumerated(_)      | Choice(_)                                     -> null
        | Sequence seqInfo    ->
            let emitChild (c:SeqChildInfo) =
                match c with
                | Asn1Child c -> gen.SequencePatternChild c.Name.Value (ToC c.Name.Value) stgFileName
                | AcnChild  c -> null
            gen.TypePatternSequence asn1Name backendName (seqInfo.children |> Seq.map emitChild) stgFileName
        | ReferenceType(_) -> null
        | other -> raise (BugErrorException $"Unsupported kind for PrintPattern {other}")
    let rec PrintExpression (t:Asn1Type) (pattern:string) =
        match t.Kind with
        | Integer   intInfo     -> handTypeWithMinMax pattern intInfo.baseInfo.uperRange gen.ContractExprMinMax stgFileName
        | Real      realInfo    -> handTypeWithMinMax_real pattern realInfo.baseInfo.uperRange gen.ContractExprMinMax stgFileName
        | OctetString info      -> handTypeWithMinMax pattern (CommonTypes.Concrete (info.baseInfo.minSize, info.baseInfo.maxSize)) gen.ContractExprSize stgFileName
        | IA5String   info      -> handTypeWithMinMax pattern (CommonTypes.Concrete (info.baseInfo.minSize, info.baseInfo.maxSize)) gen.ContractExprSize stgFileName
        | BitString   info      -> handTypeWithMinMax pattern (CommonTypes.Concrete (info.baseInfo.minSize, info.baseInfo.maxSize)) gen.ContractExprSize stgFileName
        | ObjectIdentifier _
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
        | other -> raise (BugErrorException $"Unsupported kind for PrintExpression {other}")
    let pattern = PrintPattern ()
    let expression = PrintExpression t pattern
    gen.Contract pattern (if String.length(expression) > 0 then expression else null) stgFileName


let rec PrintType (r:AstRoot) (f:Asn1File) (stgFileName:string) modName (deepRecursion:bool) (t:Asn1Type)  : string  =
    let printChildTypeAsReferencedType (t:Asn1Type) =
        match t.typeDefinitionOrReference, t.inheritInfo, t.Kind with
        | ReferenceToExistingDefinition _, None, Integer _
        | ReferenceToExistingDefinition _, None, Real _
        | ReferenceToExistingDefinition _, None, Boolean _
        | ReferenceToExistingDefinition _, None, NullType _ -> PrintType r f stgFileName modName deepRecursion t
        | _                ->
            let uperRange =
                match (t.ActualType).Kind with
                | Integer       i         -> Some (GetMinMax i.baseInfo.uperRange)
                | BitString     i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | OctetString   i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | IA5String     i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | SequenceOf    i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | Real          i         -> Some (GetMinMax i.baseInfo.uperRange)
                | Boolean _ | NullType _ | Choice _ | Enumerated _ | Sequence _ | ReferenceType _ | ObjectIdentifier _ | TimeType _ -> None
            let sModName=
                match t.typeDefinitionOrReference with
                | ReferenceToExistingDefinition  refEx  ->
                    match refEx.programUnit with
                    | Some x -> x.Replace("_","-")
                    | None -> null
                | TypeDefinition   td                   -> null
            let asn1Name = t.typeDefinitionOrReference.getAsn1Name r.args.TypePrefix
            let sCModName = if sModName <> null then (ToC sModName) else null
            let sResolvedType = None
            let refTypeContent =
                match uperRange with
                | Some(sMin, sMax)  -> gen.RefTypeMinMax sMin sMax asn1Name sModName (ToC asn1Name) (*typedefName*) sCModName (sMin = sMax) sResolvedType stgFileName
                | None              -> gen.RefType asn1Name sModName (ToC asn1Name) (*typedefName*) sCModName sResolvedType stgFileName

            let cName = t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName
            let scalaName = t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName
            let adaName = t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName
            gen.TypeGeneric (BigInteger t.Location.srcLine) (BigInteger t.Location.charPos) f.FileName refTypeContent (t.acnDecFunction.IsSome && t.acnDecFunction.Value.funcName.IsSome) cName scalaName adaName stgFileName

    let PrintTypeAux (t:Asn1Type) =
        match t.Kind with                                                                                            //func name sMin sMax (sMin=sMax) stgFileName
        | Integer           i    -> handTypeWithMinMax (gen.IntegerType () stgFileName)         i.baseInfo.uperRange (fun name sMin sMax bFixedSize stgFileName -> gen.MinMaxType name sMin sMax bFixedSize i.baseInfo.isUnsigned false stgFileName ) stgFileName
        | OctetString       i    -> handTypeWithMinMax (gen.OctetStringType () stgFileName)     (CommonTypes.Concrete (i.baseInfo.minSize, i.baseInfo.maxSize)) gen.MinMaxType2 stgFileName
        | Real              i    -> handTypeWithMinMax_real (gen.RealType () stgFileName)       i.baseInfo.uperRange (fun name sMin sMax bFixedSize stgFileName -> gen.MinMaxType name sMin sMax bFixedSize false true stgFileName ) stgFileName
        | IA5String         i    -> handTypeWithMinMax (gen.IA5StringType () stgFileName)       (CommonTypes.Concrete (i.baseInfo.minSize, i.baseInfo.maxSize)) gen.MinMaxType2 stgFileName
        | Boolean           i    -> gen.BooleanType () stgFileName
        | NullType          i    -> gen.NullType () stgFileName
        | ObjectIdentifier i     -> gen.ObjectIdentifierType () stgFileName
        | Choice(chInfo)      ->
            let emitChild (c:ChChildInfo) =
                let bRemovedChild =
                    match c.Optionality with
                    | None  -> false
                    | Some (Asn1AcnAst.ChoiceAlwaysAbsent)    -> true
                    | Some (Asn1AcnAst.ChoiceAlwaysPresent)   -> false

                let childTypeExp =
                    match deepRecursion with
                    |true   -> PrintType r f stgFileName modName  deepRecursion c.chType
                    |false  -> printChildTypeAsReferencedType c.chType
                gen.ChoiceChild c.Name.Value (ToC (c._c_name)) (ToC (c._scala_name)) (ToC (c._ada_name)) (BigInteger c.Name.Location.srcLine) (BigInteger c.Name.Location.charPos) childTypeExp (c.presentWhenName (Some c.chType.typeDefinitionOrReference) C) bRemovedChild stgFileName
            gen.ChoiceType (chInfo.children |> Seq.map emitChild) stgFileName
        | Sequence(seqInfo)    ->
            let emitChild (c:SeqChildInfo) =

                match c with
                | Asn1Child c ->
                    let childTypeExp =
                        match deepRecursion with
                        |true   -> PrintType r f stgFileName modName  deepRecursion c.Type
                        |false  -> printChildTypeAsReferencedType c.Type
                    let bAlwaysPresent, bAlwaysAbsent =
                        match c.Optionality with
                        | None  -> true, false
                        | Some (Asn1AcnAst.AlwaysAbsent)    -> false, true
                        | Some (Asn1AcnAst.AlwaysPresent)   -> true, false
                        | Some (Asn1AcnAst.Optional _)      -> false, false
                    match c.Optionality with
                    | Some(Asn1AcnAst.Optional(optVal)) when optVal.defaultValue.IsSome ->
                        let defValueAsAsn1Value = DAstAsn1.printAsn1Value optVal.defaultValue.Value
                        let defValueAsAsn1Value =
                            match defValueAsAsn1Value.StartsWith("\"") && defValueAsAsn1Value.EndsWith("\"") with
                            | false -> defValueAsAsn1Value
                            | true  ->
                                defValueAsAsn1Value.Substring(1,defValueAsAsn1Value.Length-2)
                        gen.SequenceChild c.Name.Value (ToC (c._c_name)) (ToC (c._scala_name)) (ToC (c._ada_name)) true defValueAsAsn1Value (BigInteger c.Name.Location.srcLine) (BigInteger c.Name.Location.charPos) childTypeExp bAlwaysPresent bAlwaysAbsent stgFileName
                        //gen.SequenceChild c.Name.Value (ToC c.Name.Value) true (printAsn1ValueAsXmlAttribute (DAstUtilFunctions.mapValue optVal.defaultValue.Value) stgFileName) (BigInteger c.Name.Location.srcLine) (BigInteger c.Name.Location.charPos) childTypeExp stgFileName
                    | _ -> gen.SequenceChild c.Name.Value (ToC (c._c_name)) (ToC (c._scala_name)) (ToC (c._ada_name)) c.Optionality.IsSome null (BigInteger c.Name.Location.srcLine) (BigInteger c.Name.Location.charPos) childTypeExp bAlwaysPresent bAlwaysAbsent  stgFileName
                | AcnChild  c -> null
            gen.SequenceType (seqInfo.children |> Seq.map emitChild) stgFileName
        | BitString         i    -> 
            let emitNamedBit (n:CommonTypes.NamedBit1) =
                gen.BitStringNamedBit n.Name.Value  n.resolvedValue (BigInteger n.Name.Location.srcLine) (BigInteger n.Name.Location.charPos) stgFileName
            let uperRange = CommonTypes.Concrete (i.baseInfo.minSize, i.baseInfo.maxSize)
            let sMin, sMax = GetMinMax uperRange
            let arrNamedBits = i.baseInfo.namedBitList |> Seq.map emitNamedBit |> Seq.toArray
            gen.BitStringType sMin sMax (sMin=sMax) arrNamedBits stgFileName
        | Enumerated(enmInfo)     ->
            let emitItem (it : Asn1AcnAst.NamedItem) =
                gen.EnumItem it.Name.Value (ToC it.Name.Value) it.definitionValue (BigInteger it.Name.Location.srcLine) (BigInteger it.Name.Location.charPos) (it.CEnumName  C) stgFileName
            gen.EnumType (enmInfo.baseInfo.items |> Seq.map emitItem) stgFileName
        | SequenceOf info     ->
            let childTypeExp =
                match deepRecursion with
                |true   -> PrintType r f stgFileName modName  deepRecursion info.childType
                |false  -> printChildTypeAsReferencedType info.childType

            let sMin, sMax = info.baseInfo.minSize.ToString(), info.baseInfo.maxSize.ToString()
            gen.SequenceOfType sMin sMax  childTypeExp (sMin=sMax) stgFileName
        | ReferenceType info ->
            let uperRange =
                match (t.ActualType).Kind with
                | Integer       i         -> Some (GetMinMax i.baseInfo.uperRange)
                | BitString     i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | OctetString   i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | IA5String     i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | SequenceOf    i         -> Some (i.baseInfo.minSize.ToString(), i.baseInfo.maxSize.ToString())
                | Real          i         -> Some (GetMinMax i.baseInfo.uperRange)
                | Boolean _ | NullType _ | Choice _ | Enumerated _ | Sequence _ | ReferenceType _   | ObjectIdentifier _ | TimeType _   -> None
            let sModName = if info.baseInfo.modName.Value=modName then null else info.baseInfo.modName.Value
            let sCModName = if sModName <> null then (ToC sModName) else null
            let resolvedType = PrintType r f stgFileName modName deepRecursion info.resolvedType
            match uperRange with
            | Some(sMin, sMax)  -> gen.RefTypeMinMax sMin sMax info.baseInfo.tasName.Value sModName (ToC info.baseInfo.tasName.Value) sCModName  (sMin=sMax) (Some resolvedType) stgFileName
            | None              -> gen.RefType info.baseInfo.tasName.Value sModName (ToC info.baseInfo.tasName.Value) sCModName (Some resolvedType) stgFileName
        | other -> raise (BugErrorException $"Unsupported kind for PrintTypeAux {other}")
    let cName = t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName
    let scalaName = t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName
    let adaName = t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName
    gen.TypeGeneric (BigInteger t.Location.srcLine) (BigInteger t.Location.charPos) f.FileName  (PrintTypeAux t) (t.acnDecFunction.IsSome && t.acnDecFunction.Value.funcName.IsSome) cName scalaName adaName stgFileName





let exportFile (r:AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (stgFileName:string) (outFileName:string) =
    let AssignOp (t: Asn1Type) =
        match t.Kind with
        | Sequence(_) -> gen.AssignOpSpecialType () stgFileName
        | _           -> gen.AssignOpNormalType () stgFileName

    let typesMap =
        r.Files |>
        List.collect(fun f -> f.Modules) |>
        List.collect(fun m ->
            m.TypeAssignments |> List.map(fun tas -> tas.AsTypeAssignmentInfo m.Name.Value, tas)
        ) |> Map.ofList

    let PrintVas (f:Asn1File) (vas: ValueAssignment) modName =
        gen.VasXml vas.Name.Value (BigInteger vas.Name.Location.srcLine) (BigInteger vas.Name.Location.charPos) (PrintType r f stgFileName modName  false vas.Type ) (PrintCustomAsn1Value vas stgFileName) (ToC vas.c_name)  stgFileName
    let deepRecursion = r.args.custom_Stg_Ast_Version = 1
    let getInnerTypes (t:Asn1Type) =
        match deepRecursion with
        | true      -> [t]
        | false     -> GetMySelfAndChildren t
    let PrintTas (f:Asn1File) (tas:TypeAssignment) modName =
        let innerTypeDef =
             getInnerTypes tas.Type |>
             List.choose(fun t ->
                match t.typeDefinitionOrReference with
                | ReferenceToExistingDefinition _ -> None
                | TypeDefinition td               ->
                    let asn1Name = t.typeDefinitionOrReference.getAsn1Name r.args.TypePrefix
                    let ret = gen.TasXml asn1Name t.Location.srcLine.AsBigInt t.Location.charPos.AsBigInt (PrintType r f stgFileName modName deepRecursion t ) (ToC asn1Name) (*td.typedefName*) (AssignOp t) (PrintContract r stgFileName asn1Name td.typedefName t) (t.id <> tas.Type.id) stgFileName
                    Some ret) |> Seq.StrJoin "\n"
        innerTypeDef
    let PrintModule (f:Asn1File) (m:Asn1Module) =
        let PrintImpModule (im:Asn1Ast.ImportedModule) =
            gen.ImportedMod im.Name.Value (ToC im.Name.Value) (im.Types |> Seq.map(fun x -> x.Value)) (im.Values |> Seq.map(fun x -> x.Value)) stgFileName
        let exportedTypes =
            m.ExportedTypes |>
            List.collect(fun n ->
                match m.TypeAssignments |> Seq.tryFind(fun z -> z.Name.Value = n) with
                | Some tas ->
                    getInnerTypes tas.Type |>
                    List.choose(fun t ->
                        match t.typeDefinitionOrReference with
                        | ReferenceToExistingDefinition _ -> None
                        | TypeDefinition td               -> Some (t.typeDefinitionOrReference.getAsn1Name r.args.TypePrefix))
                | None     -> [])


        let moduleTypes = m.TypeAssignments |> List.map(fun x -> x.Type)
        let importedTypes =
            m.Imports |>
            Seq.collect(fun imp -> imp.Types |> List.map (fun impType ->{TypeAssignmentInfo.modName = imp.Name.Value; tasName = impType.Value})) |>
            Seq.distinct |> Seq.toList
        let sortedTypes = DAstProgramUnit.sortTypes moduleTypes importedTypes |> List.filter(fun z -> z.modName = m.Name.Value) |> List.map(fun ref -> typesMap.[ref])

        gen.ModuleXml m.Name.Value (ToC m.Name.Value) (m.Imports |> Seq.map PrintImpModule) exportedTypes m.ExportedVars (sortedTypes |> Seq.map (fun t -> PrintTas f t m.Name.Value)) (m.ValueAssignments |> Seq.map (fun t -> PrintVas f t m.Name.Value)) stgFileName

    let PrintFile (f:Asn1File) =
        gen.FileXml f.FileName (f.Modules |> Seq.map (PrintModule f)) stgFileName

    let allTypes =
        r.Files |>
        List.collect(fun f -> f.Modules) |>
        List.collect(fun m ->
            m.TypeAssignments |> List.map(fun tas -> tas.Type)
        )
    let globalSortedTypes =  DAstProgramUnit.sortTypes allTypes []
    let arrsSortedTypeAssignmentRefs = globalSortedTypes |> List.map(fun a -> gen.TypeAssignmentReference a.modName a.tasName stgFileName)
    let content = gen.RootXml (r.Files |> Seq.map PrintFile) arrsSortedTypeAssignmentRefs stgFileName
    File.WriteAllText(outFileName, content.Replace("\r",""))
