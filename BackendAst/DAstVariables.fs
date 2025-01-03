module DAstVariables

open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions
open Language


let printOctetStringValueAsCompoundLiteral  (lm:LanguageMacros) curProgramUnitName  (o:Asn1AcnAst.OctetString) (bytes : byte list) =
    let printOct = lm.vars.PrintBitOrOctetStringValueAsCompoundLiteral

    let td = (lm.lg.getSizeableTypeDefinition  o.typeDef).longTypedefName2 lm.lg.hasModules curProgramUnitName
    printOct td (o.minSize.uper = o.maxSize.uper) bytes (BigInteger bytes.Length)

let printTimeValue (lm:LanguageMacros) (td) (v:TimeValue) =
    match v with
    |Asn1LocalTimeValue                  tv        -> lm.vars.PrintTimeValue_Asn1LocalTime td tv
    |Asn1UtcTimeValue                    tv        -> lm.vars.PrintTimeValue_Asn1UtcTime td tv
    |Asn1LocalTimeWithTimeZoneValue      (tv,tz)   -> lm.vars.PrintTimeValue_Asn1LocalTimeWithTimeZone td tv tz
    |Asn1DateValue                       dt        -> lm.vars.PrintTimeValue_Asn1Date td dt
    |Asn1Date_LocalTimeValue             (dt,tv)   -> lm.vars.PrintTimeValue_Asn1Date_LocalTime td dt tv
    |Asn1Date_UtcTimeValue               (dt,tv)   -> lm.vars.PrintTimeValue_Asn1Date_UtcTime td dt tv
    |Asn1Date_LocalTimeWithTimeZoneValue (dt,tv,tz)-> lm.vars.PrintTimeValue_Asn1Date_LocalTimeWithTimeZone td dt tv tz


let printBitStringValueAsCompoundLiteral  (lm:LanguageMacros) curProgramUnitName  (o:Asn1AcnAst.BitString) (v : BitStringValue) =
    let printOct =  lm.vars.PrintBitOrOctetStringValueAsCompoundLiteral
    let td = (lm.lg.getSizeableTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules curProgramUnitName
    let bytes = lm.lg.bitStringValueToByteArray v
    printOct td (o.minSize.uper = o.maxSize.uper) bytes o.minSize.uper

let convertStringValue2TargetLangStringLiteral (lm:LanguageMacros) mxSizeUper (v:StringValue) =
    let (parts,_) = v
    let vStr = CommonTypes.StringValue2String parts
    let arrNulls = [0 .. ((int mxSizeUper) - vStr.Length)]|>Seq.map(fun x -> lm.vars.PrintStringValueNull())
    let pParts =
        parts |>
        List.map(fun s ->
            match s with
            | CStringValue  sv -> lm.vars.PrintSingleStringValue (sv.Replace("\"","\"\""))
            | SpecialCharacter  CarriageReturn -> lm.vars.PrintCR ()
            | SpecialCharacter  LineFeed       -> lm.vars.PrintLF ()
            | SpecialCharacter  HorizontalTab  -> lm.vars.PrintHT ()
            | SpecialCharacter  NullCharacter  -> lm.vars.PrintStringValueNull ()
        )
    lm.vars.PrintStringValue pParts arrNulls

let rec printValue (r:DAst.AstRoot)  (lm:LanguageMacros) (curProgramUnitName:string)  (t:Asn1Type) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | IntegerValue      v ->
            let intClass =
                match t.ActualType.Kind with
                | Integer int -> int.baseInfo.intClass
                | _ -> raise(BugErrorException "unexpected type")
            lm.lg.intValueToString v intClass
        | RealValue         v -> lm.vars.PrintRealValue v
        | BooleanValue      v -> lm.vars.PrintBooleanValue v
        | StringValue       v ->
            match t.ActualType.Kind with
            | IA5String st  -> convertStringValue2TargetLangStringLiteral lm (int st.baseInfo.maxSize.uper) v
            | _             -> raise(BugErrorException "unexpected type")

        | BitStringValue    v ->
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            match t.ActualType.Kind with
            | OctetString os    ->
                let td =  lm.lg.getSizeableTypeDefinition os.baseInfo.typeDef
                lm.vars.PrintOctetStringValue td (os.baseInfo.minSize.uper = os.baseInfo.maxSize.uper) bytes (BigInteger bytes.Length)
            | BitString   bs    ->
                let td =  lm.lg.getSizeableTypeDefinition bs.baseInfo.typeDef
                let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
                lm.vars.PrintBitStringValue td (bs.baseInfo.minSize.uper = bs.baseInfo.maxSize.uper) arBits (BigInteger arBits.Length) bytes (BigInteger bytes.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | OctetStringValue  v ->
            match t.ActualType.Kind with
            | OctetString os    ->
                let td =  lm.lg.getSizeableTypeDefinition os.baseInfo.typeDef
                lm.vars.PrintOctetStringValue td (os.baseInfo.minSize.uper = os.baseInfo.maxSize.uper) v (BigInteger v.Length)
            | BitString   bs    ->
                let td =  lm.lg.getSizeableTypeDefinition bs.baseInfo.typeDef
                let bitstring = byteArrayToBitStringValue v
                let arBits = bitstring.ToCharArray() |> Array.map(fun x -> x.ToString())
                let maxLen = if (arBits.Length > int bs.baseInfo.maxSize.uper) then ((int bs.baseInfo.maxSize.uper)-1) else (arBits.Length-1)
                let arBits = arBits.[0 .. maxLen]
                lm.vars.PrintBitStringValue td (bs.baseInfo.minSize.uper = bs.baseInfo.maxSize.uper) arBits (BigInteger arBits.Length) v (BigInteger v.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | EnumValue         v ->
            match t.ActualType.Kind with
            | Enumerated enm    ->
                let typeModName = (t.getActualType r).id.ModName
                let itm = enm.baseInfo.items |> Seq.find(fun x -> x.Name.Value = v)
                let itmName = lm.lg.getNamedItemBackendName2 t.ActualType.moduleName curProgramUnitName itm
                lm.vars.PrintEnumValue itmName
            | _         -> raise(BugErrorException "unexpected type")
        | NullValue         v -> lm.vars.PrintNullValue ()
        | ObjOrRelObjIdValue v  ->
            match t.ActualType.Kind with
            | ObjectIdentifier oi   ->
                let aa = lm.lg.typeDef oi.baseInfo.typeDef
                lm.vars.PrintObjectIdentifierValue aa (v.Values |> List.map fst) (BigInteger v.Values.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | TimeValue v       ->
            match t.ActualType.Kind with
            | TimeType tt   ->
                let td = lm.lg.typeDef tt.baseInfo.typeDef
                printTimeValue lm td v
            | _         -> raise(BugErrorException "unexpected type")
        | SeqOfValue        v ->
            match t.ActualType.Kind with
            | SequenceOf so ->
                let td =  lm.lg.getSizeableTypeDefinition so.baseInfo.typeDef
                let childVals = v |> List.map (fun chv -> printValue r lm curProgramUnitName so.childType (Some gv) chv.kind)
                let sDefValue = so.childType.initFunction.initExpressionFnc ()
                lm.vars.PrintSequenceOfValue td (so.baseInfo.minSize.uper = so.baseInfo.maxSize.uper) (BigInteger v.Length) childVals sDefValue
            | _         -> raise(BugErrorException "unexpected type")
        | SeqValue          v ->
            match t.ActualType.Kind with
            | Sequence s ->
                let td = lm.lg.getSequenceTypeDefinition s.baseInfo.typeDef
                let typeDefName  = lm.lg.getLongTypedefName t.typeDefinitionOrReference
                let optChildren =
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.filter(fun ch -> ch.Optionality.IsSome) |>
                    List.map(fun x ->

                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some _    -> lm.vars.PrintSequenceValue_child_exists (lm.lg.getAsn1ChildBackendName x) "1"
                        | None      -> lm.vars.PrintSequenceValue_child_exists (lm.lg.getAsn1ChildBackendName x) "0")
                let arrChildren =
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.choose(fun x ->
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some v    ->
                            let childType = x.Type
                            Some (lm.vars.PrintSequenceValueChild (lm.lg.getAsn1ChildBackendName x) (printValue r lm curProgramUnitName childType (Some gv) v.Value.kind))
                        | None      ->
                            let chV =
                                match x.Optionality with
                                | Some(Asn1AcnAst.Optional opt)    ->
                                    match opt.defaultValue with
                                    | Some v    ->
                                        let chV = (mapValue v).kind
                                        Some (printValue r lm curProgramUnitName x.Type None chV)
                                    | None      -> if lm.lg.supportsInitExpressions then (Some (x.Type.initFunction.initExpressionFnc ())) else None
                                | _             -> if lm.lg.supportsInitExpressions then (Some (x.Type.initFunction.initExpressionFnc ())) else None
                            match chV with
                            | None -> None
                            | Some chV -> Some (lm.vars.PrintSequenceValueChild (lm.lg.getAsn1ChildBackendName x) chV ))
                lm.vars.PrintSequenceValue td typeDefName arrChildren optChildren
            | _         -> raise(BugErrorException "unexpected type")
        | ChValue           v ->
            match t.ActualType.Kind with
            | Choice s ->
                let typeDefName  = lm.lg.getLongTypedefName t.typeDefinitionOrReference
                s.children |>
                List.filter(fun x -> x.Name.Value = v.name)  |>
                List.map(fun x ->
                    let chValue = printValue r  lm curProgramUnitName x.chType (Some gv) v.Value.kind
                    let sChildNamePresent = lm.lg.presentWhenName  (Some t.typeDefinitionOrReference) x
                    lm.vars.PrintChoiceValue typeDefName (lm.lg.getAsn1ChChildBackendName x) chValue sChildNamePresent true ) |>
                List.head
            | _         -> raise(BugErrorException "unexpected type")
        | RefValue ((md,vs),v)         ->
            match t.ActualType.Kind with
            | Integer _
            | Real  _   ->
                printValue r  lm  curProgramUnitName t parentValue v.kind
            | _         ->
                match lm.lg.hasModules with
                | false ->
                    printValue r  lm  curProgramUnitName t parentValue v.kind
                | true ->
                    let vas = r.getValueAssignmentByName md vs
                    match (ToC md) = curProgramUnitName with
                    | true  -> lm.lg.getValueAssignmentName vas
                    | false -> curProgramUnitName + "." + (lm.lg.getValueAssignmentName vas)

let createIntegerFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer)  =
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | IntegerValue i    -> lm.vars.PrintIntValue i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createRealFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real)  =
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | RealValue i    -> lm.vars.PrintRealValue i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean)  =
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | BooleanValue i    -> lm.vars.PrintBooleanValue i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue



let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefinitionOrReference)  =
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | EnumValue i    ->
            let itm = o.items |> Seq.find(fun x -> x.Name.Value = i)
            lm.vars.PrintEnumValue (lm.lg.getNamedItemBackendName (Some defOrRef) itm)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType)  =
    let stgMacro = lm.vars.PrintNullValue
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | NullValue _    -> stgMacro ()
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createStringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  =
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | StringValue v    ->
            convertStringValue2TargetLangStringLiteral lm (int o.maxSize.uper) v
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")

    printValue

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (defOrRef:TypeDefinitionOrReference) =
    let PrintOctetStringValue = lm.vars.PrintOctetStringValue
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | OctetStringValue  v ->
            let td = lm.lg.getSizeableTypeDefinition o.typeDef
            PrintOctetStringValue td (o.minSize.uper = o.maxSize.uper) v (BigInteger v.Length)
        | BitStringValue    v ->
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v) |> Seq.toList
            let td = lm.lg.getSizeableTypeDefinition o.typeDef
            PrintOctetStringValue td (o.minSize.uper = o.maxSize.uper) bytes (BigInteger bytes.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (defOrRef:TypeDefinitionOrReference) =
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        let td = lm.lg.typeDef o.typeDef
        match v with
        | ObjOrRelObjIdValue  v ->
            lm.vars.PrintObjectIdentifierValue  td (v.Values |> List.map fst) (BigInteger v.Values.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (defOrRef:TypeDefinitionOrReference) =
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        let td = lm.lg.typeDef o.typeDef
        match v with
        | TimeValue  v            -> printTimeValue lm td v
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (defOrRef:TypeDefinitionOrReference) =
    let PrintBitStringValue = lm.vars.PrintBitStringValue
    let PrintOctetStringValue = lm.vars.PrintOctetStringValue
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | BitStringValue    v ->
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            let td = lm.lg.getSizeableTypeDefinition o.typeDef
            let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
            PrintBitStringValue td (o.minSize.uper = o.maxSize.uper) arBits (BigInteger arBits.Length) bytes (BigInteger bytes.Length)
        | OctetStringValue  v ->
            let td = lm.lg.getSizeableTypeDefinition o.typeDef
            PrintOctetStringValue td (o.minSize.uper = o.maxSize.uper) v (BigInteger v.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (defOrRef:TypeDefinitionOrReference) (childType:Asn1Type) =
    let stgMacro = lm.vars.PrintBooleanValue
    let PrintSequenceOfValue = lm.vars.PrintSequenceOfValue
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | SeqOfValue chVals    ->
            let childVals = chVals |> List.map (fun chv -> childType.printValue curProgramUnitName (Some gv) chv.kind)
            let sDefValue =  childType.initFunction.initExpressionFnc ()
            let td = lm.lg.getSizeableTypeDefinition o.typeDef
            PrintSequenceOfValue td (o.minSize.uper = o.maxSize.uper) (BigInteger chVals.Length) childVals sDefValue

        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createSequenceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (defOrRef:TypeDefinitionOrReference) (children:SeqChildInfo list) =
    let PrintSequenceValue_child_exists = lm.vars.PrintSequenceValue_child_exists
    let PrintSequenceValueChild = lm.vars.PrintSequenceValueChild
    let PrintSequenceValue = lm.vars.PrintSequenceValue

    let optChildren =
        children |>
        List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
        List.filter(fun ch -> ch.Optionality.IsSome)

    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | SeqValue  v ->
                let td = lm.lg.getSequenceTypeDefinition o.typeDef
                let typeDefName = lm.lg.getLongTypedefName defOrRef
                let optChildren =
                    optChildren |>
                    List.map(fun x ->
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some _    -> PrintSequenceValue_child_exists (lm.lg.getAsn1ChildBackendName x) "1"
                        | None      -> PrintSequenceValue_child_exists (lm.lg.getAsn1ChildBackendName x) "0")
                let arrChildren =
                    children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.choose(fun x ->
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some v    ->
                            let childValue = x.Type.printValue curProgramUnitName (Some gv) v.Value.kind
                            Some (lm.vars.PrintSequenceValueChild (lm.lg.getAsn1ChildBackendName x) childValue)
                        | None      ->
                            let childValue =
                                match x.Optionality with
                                | Some(Asn1AcnAst.Optional opt)    ->
                                    match opt.defaultValue with
                                    | Some zz    ->
                                        let v = (mapValue zz).kind
                                        Some(x.Type.printValue curProgramUnitName (Some gv) v)
                                    | None      -> match lm.lg.supportsInitExpressions with false -> None | true -> Some (x.Type.initFunction.initExpressionFnc ())
                                | _             -> match lm.lg.supportsInitExpressions with false -> None | true -> Some (x.Type.initFunction.initExpressionFnc ())
                            match childValue with
                            | None  -> None
                            | Some childValue -> Some (PrintSequenceValueChild (lm.lg.getAsn1ChildBackendName x) childValue) )
                PrintSequenceValue td typeDefName arrChildren optChildren
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createChoiceFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (defOrRef:TypeDefinitionOrReference) (children:ChChildInfo list) =
    let PrintChoiceValue = lm.vars.PrintChoiceValue
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | ChValue chVal    ->
            let typeDefName  = lm.lg.getLongTypedefName defOrRef
            children |>
            List.filter(fun x -> x.Name.Value = chVal.name)  |>
            List.map(fun x ->

                let childValue = (x.chType.printValue curProgramUnitName (Some gv) chVal.Value.kind)
                let sChildNamePresent = lm.lg.presentWhenName (Some defOrRef) x
                lm.vars.PrintChoiceValue typeDefName (lm.lg.getAsn1ChChildBackendName x) childValue   sChildNamePresent true
                ) |>
            List.head
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (defOrRef:TypeDefinitionOrReference) (baseType:Asn1Type)   =
    baseType.printValue

let createReferenceTypeFunction2 (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (defOrRef:TypeDefinitionOrReference)    =
    let printValue (curProgramUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) = ""
    printValue
