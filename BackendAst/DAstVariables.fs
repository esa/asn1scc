module DAstVariables

open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions
open Language


//let getDefaultValueByType  (t:Asn1Type)  =  t.initialValue

let printOctetStringValueAsCompoundLitteral  (lm:LanguageMacros) curProgamUnitName  (o:Asn1AcnAst.OctetString) (bytes : byte list) =
    let printOct = lm.vars.PrintBitOrOctetStringValueAsCompoundLitteral

    let td = (lm.lg.getSizeableTypeDefinition  o.typeDef).longTypedefName2 lm.lg.hasModules curProgamUnitName
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

    
let printBitStringValueAsCompoundLitteral  (lm:LanguageMacros) curProgamUnitName  (o:Asn1AcnAst.BitString) (v : BitStringValue) =
    let printOct =  lm.vars.PrintBitOrOctetStringValueAsCompoundLitteral
    let td = (lm.lg.getSizeableTypeDefinition o.typeDef).longTypedefName2 lm.lg.hasModules curProgamUnitName
    let bytes = lm.lg.bitStringValueToByteArray v
    printOct td (o.minSize.uper = o.maxSize.uper) bytes o.minSize.uper


let rec printValue (r:DAst.AstRoot)  (l:ProgrammingLanguage) (lm:LanguageMacros) (curProgamUnitName:string)  (t:Asn1Type) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
    match l with
    | C ->
        match gv with
        | IntegerValue      v -> variables_c.PrintIntValue v
        | RealValue         v -> variables_c.PrintRealValue v
        | StringValue       v -> variables_c.PrintStringValue v []
        | BooleanValue      v -> variables_c.PrintBooleanValue v
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            match t.ActualType.Kind with
            | OctetString os    -> 
                let td = (os.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                variables_c.PrintOctetStringValue td (os.baseInfo.minSize.uper = os.baseInfo.maxSize.uper) bytes (BigInteger v.Length)
            | BitString   bs    -> 
                let td = (bs.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
                variables_c.PrintBitStringValue td (bs.baseInfo.minSize.uper = bs.baseInfo.maxSize.uper) arBits (BigInteger arBits.Length) bytes (BigInteger bytes.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | OctetStringValue  v -> 
            match t.ActualType.Kind with
            | OctetString os    -> 
                let td = (os.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                variables_c.PrintOctetStringValue td (os.baseInfo.minSize.uper = os.baseInfo.maxSize.uper) v (BigInteger v.Length)
            | BitString   bs    -> 
                let td = (bs.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let bittring = byteArrayToBitStringValue v
                let arBits = bittring.ToCharArray() |> Array.map(fun x -> x.ToString())
                variables_c.PrintBitStringValue td (bs.baseInfo.minSize.uper = bs.baseInfo.maxSize.uper) arBits (BigInteger arBits.Length) v (BigInteger v.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | EnumValue         v -> 
            match t.ActualType.Kind with
            | Enumerated enm    -> 
                let itm = enm.baseInfo.items |> Seq.find(fun x -> x.Name.Value = v)
                variables_c.PrintEnumValue (itm.getBackendName (Some t.typeDefintionOrReference) l)
            | _         -> raise(BugErrorException "unexpected type")
        | NullValue         v -> variables_c.PrintNullValue ()
        | SeqOfValue        v -> 
            match t.ActualType.Kind with
            | SequenceOf so -> 
                let childVals = v |> List.map (fun chv -> printValue r l lm curProgamUnitName so.childType (Some gv) chv.kind)
                let td = (so.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l //if parentValue.IsSome then so.typeDefinition.typeDefinitionBodyWithinSeq else so.typeDefinition.name
                let sDefValue = so.childType.initFunction.initExpression //printValue r l lm curProgamUnitName so.childType None (getDefaultValueByType so.childType)
                
                variables_c.PrintSequenceOfValue td (so.baseInfo.minSize.uper = so.baseInfo.maxSize.uper) (BigInteger v.Length) childVals sDefValue
            | _         -> raise(BugErrorException "unexpected type")
        | SeqValue          v -> 
            match t.ActualType.Kind with
            | Sequence s -> 
                let optChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.filter(fun ch -> ch.Optionality.IsSome) |>
                    List.map(fun x ->
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some _    -> variables_c.PrintSequenceValue_child_exists (x.getBackendName l) "1"
                        | None      -> variables_c.PrintSequenceValue_child_exists (x.getBackendName l) "0")
                let arrChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.choose(fun cht -> 
                        match v |> Seq.tryFind(fun chv -> chv.name = cht.Name.Value) with
                        | Some v    -> Some (variables_c.PrintSequenceValueChild (cht.getBackendName l) (printValue r l lm curProgamUnitName cht.Type (Some gv) v.Value.kind))
                        | None      -> 
                            match cht.Optionality with
                            | Some(Asn1AcnAst.Optional opt)    -> 
                                match opt.defaultValue with
                                | Some v    -> Some (variables_c.PrintSequenceValueChild (cht.getBackendName l) (printValue r l lm curProgamUnitName cht.Type (Some gv) (mapValue v).kind ))                    
                                | None      -> None
                            | _             -> None)
                let td = (s.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then s.typeDefinition.typeDefinitionBodyWithinSeq else s.typeDefinition.name

                variables_c.PrintSequenceValue td typeDefName arrChildren optChildren
            | _         -> raise(BugErrorException "unexpected type")

        | ChValue           v -> 
            match t.ActualType.Kind with
            | Choice s -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l
                s.children |>
                List.filter(fun x -> x.Name.Value = v.name)  |>
                List.map(fun x -> 
                    let childValue = printValue r l lm curProgamUnitName x.chType (Some gv) v.Value.kind
                    let sChildNamePresent = x.presentWhenName (Some t.typeDefintionOrReference) l
                    variables_c.PrintChoiceValue typeDefName (x.getBackendName l) childValue   sChildNamePresent true) |>
                List.head
            | _         -> raise(BugErrorException "unexpected type")
        | ObjOrRelObjIdValue v  ->
            match t.ActualType.Kind with
            | ObjectIdentifier oi   ->
                let aa = oi.baseInfo.typeDef.[l]
                variables_c.PrintObjectIdentifierValue aa (v.Values |> List.map fst) (BigInteger v.Values.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | TimeValue v       ->
            match t.ActualType.Kind with
            | TimeType tt   ->
                let td = tt.baseInfo.typeDef.[l]
                printTimeValue lm td v
            | _         -> raise(BugErrorException "unexpected type")
        | RefValue ((md,vs),v)         ->
            printValue r  l  lm curProgamUnitName t parentValue v.kind
            //the following code has been commented out because of the following issue
            //https://stackoverflow.com/questions/3025050/error-initializer-element-is-not-constant-when-trying-to-initialize-variable-w
            //Error “initializer element is not constant” when trying to initialize variable with const

//            match t.ActualType.Kind with
//            | Integer _
//            | Real  _   ->
//                printValue r  l  curProgamUnitName t parentValue v.kind
//            | _         ->
//                let vas = r.getValueAssignmentByName md vs
//                vas.c_name
    | Ada ->
        match gv with
        | IntegerValue      v -> variables_a.PrintIntValue v
        | RealValue         v -> variables_a.PrintRealValue v
        | StringValue       v -> 
            match t.ActualType.Kind with
            | IA5String st  ->
                let arrNuls = [0 .. ((int st.baseInfo.maxSize.uper) - v.Length)]|>Seq.map(fun x -> variables_a.PrintStringValueNull())
                variables_a.PrintStringValue (v.Replace("\"","\"\"")) arrNuls
            | _             -> raise(BugErrorException "unexpected type")
        | BooleanValue      v -> variables_a.PrintBooleanValue v
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            match t.ActualType.Kind with
            | OctetString os    -> 
                let td = (os.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then os.typeDefinition.typeDefinitionBodyWithinSeq else os.typeDefinition.name
                let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
                variables_a.PrintOctetStringValue td (os.baseInfo.minSize.uper = os.baseInfo.maxSize.uper) bytes (BigInteger bytes.Length)
            | BitString   bs    -> 
                let td = (bs.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l //if parentValue.IsSome then bs.typeDefinition.typeDefinitionBodyWithinSeq else bs.typeDefinition.name
                let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
                variables_a.PrintBitStringValue td (bs.baseInfo.minSize.uper = bs.baseInfo.maxSize.uper) arBits (BigInteger arBits.Length) bytes (BigInteger bytes.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | OctetStringValue  v -> 
            match t.ActualType.Kind with
            | OctetString os    -> 
                let td = (os.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then os.typeDefinition.typeDefinitionBodyWithinSeq else os.typeDefinition.name
                variables_a.PrintOctetStringValue td (os.baseInfo.minSize.uper = os.baseInfo.maxSize.uper) v (BigInteger v.Length)
            | BitString   bs    -> 
                let td = (bs.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then bs.typeDefinition.typeDefinitionBodyWithinSeq else bs.typeDefinition.name
                let bittring = byteArrayToBitStringValue v
                let arBits = bittring.ToCharArray() |> Array.map(fun x -> x.ToString()) 
                let maxLen = if (arBits.Length > int bs.baseInfo.maxSize.uper) then ((int bs.baseInfo.maxSize.uper)-1) else (arBits.Length-1)
                let arBits = arBits.[0 .. maxLen]
                variables_a.PrintBitStringValue td (bs.baseInfo.minSize.uper = bs.baseInfo.maxSize.uper) arBits (BigInteger arBits.Length) v (BigInteger v.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | EnumValue         v -> 
            match t.ActualType.Kind with
            | Enumerated enm    -> 
                let typeModName = (t.getActualType r).id.ModName
                let itm = enm.baseInfo.items |> Seq.find(fun x -> x.Name.Value = v)
                //variables_a.PrintEnumValue (itm.getBackendName (Some t.typeDefintionOrReference) l)
                match (ToC typeModName) = curProgamUnitName with
                | true  -> variables_a.PrintEnumValue (itm.getBackendName (Some t.typeDefintionOrReference) l)
                | false -> variables_a.PrintEnumValue  ((ToC typeModName) + "." + (ToC itm.ada_name))
            | _         -> raise(BugErrorException "unexpected type")
        | NullValue         v -> variables_a.PrintNullValue ()
        | ObjOrRelObjIdValue v  ->
            match t.ActualType.Kind with
            | ObjectIdentifier oi   ->
                let aa = oi.baseInfo.typeDef.[l]
                variables_a.PrintObjectIdentifierValue aa (v.Values |> List.map fst) (BigInteger v.Values.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | TimeValue v       ->
            match t.ActualType.Kind with
            | TimeType tt   ->
                let td = tt.baseInfo.typeDef.[l]
                printTimeValue lm td v
            | _         -> raise(BugErrorException "unexpected type")
        | SeqOfValue        v -> 
            match t.ActualType.Kind with
            | SequenceOf so -> 
                let td = (so.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l //if parentValue.IsSome then so.typeDefinition.typeDefinitionBodyWithinSeq else so.typeDefinition.name
                let childVals = v |> List.map (fun chv -> printValue r l lm curProgamUnitName so.childType (Some gv) chv.kind)
                let sDefValue = so.childType.initFunction.initExpression// printValue r l lm curProgamUnitName so.childType None (getDefaultValueByType so.childType)
                variables_a.PrintSequenceOfValue td (so.baseInfo.minSize.uper = so.baseInfo.maxSize.uper) (BigInteger v.Length) childVals sDefValue

            | _         -> raise(BugErrorException "unexpected type")

        | SeqValue          v -> 
            match t.ActualType.Kind with
            | Sequence s -> 
                let td = (s.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then s.typeDefinition.typeDefinitionBodyWithinSeq else s.typeDefinition.name
                let optChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.filter(fun ch -> ch.Optionality.IsSome) |>
                    List.map(fun x ->
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some _    -> variables_a.PrintSequenceValue_child_exists (x.getBackendName l) "1"
                        | None      -> variables_a.PrintSequenceValue_child_exists (x.getBackendName l) "0")
                let arrChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.map(fun x -> 
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some v    -> variables_a.PrintSequenceValueChild (x.getBackendName l) (printValue r l lm curProgamUnitName x.Type (Some gv) v.Value.kind)
                        | None      -> 
                            let chV = 
                                match x.Optionality with
                                | Some(Asn1AcnAst.Optional opt)    -> 
                                    match opt.defaultValue with
                                    | Some v    -> 
                                        let chV = (mapValue v).kind
                                        printValue r l lm curProgamUnitName x.Type None chV
                                    | None      -> x.Type.initFunction.initExpression
                                | _             -> x.Type.initFunction.initExpression
                            variables_a.PrintSequenceValueChild (x.getBackendName l) chV )
                //let allChildren = match Seq.isEmpty optChildren with
                //                  | true     -> arrChildren
                //                  | false    -> arrChildren @ [variables_a.PrintSequenceValue_Exists td optChildren]
                //variables_a.PrintSequenceValue typeDefName allChildren
                variables_a.PrintSequenceValue td typeDefName arrChildren optChildren
            | _         -> raise(BugErrorException "unexpected type")
        | ChValue           v -> 
            match t.ActualType.Kind with
            | Choice s -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l
                s.children |>
                List.filter(fun x -> x.Name.Value = v.name)  |>
                List.map(fun x -> variables_a.PrintChoiceValue typeDefName (x.getBackendName l) (printValue r l  lm curProgamUnitName x.chType (Some gv) v.Value.kind) (x.presentWhenName (Some t.typeDefintionOrReference) l) true ) |>
                List.head
            | _         -> raise(BugErrorException "unexpected type")

        | RefValue ((md,vs),v)         ->
            match t.ActualType.Kind with
            | Integer _
            | Real  _   ->
                printValue r  l lm  curProgamUnitName t parentValue v.kind
            | _         ->
                let vas = r.getValueAssignmentByName md vs
                match (ToC md) = curProgamUnitName with
                | true  -> vas.ada_name
                | false -> curProgamUnitName + "." + vas.ada_name


let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer)  =
    let stgMacro = match l with C -> variables_c.PrintIntValue | Ada -> variables_a.PrintIntValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | IntegerValue i    -> stgMacro i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real)  =
    let stgMacro = match l with C -> variables_c.PrintRealValue | Ada -> variables_a.PrintRealValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | RealValue i    -> stgMacro i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean)  =
    let stgMacro = match l with C -> variables_c.PrintBooleanValue | Ada -> variables_a.PrintBooleanValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | BooleanValue i    -> stgMacro i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (defOrRef:TypeDefintionOrReference)  =
    let stgMacro = match l with C -> variables_c.PrintEnumValue | Ada -> variables_a.PrintEnumValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | EnumValue i    -> 
            let itm = o.items |> Seq.find(fun x -> x.Name.Value = i)
            stgMacro (itm.getBackendName (Some defOrRef) l)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType)  =
    let stgMacro = match l with C -> variables_c.PrintNullValue | Ada -> variables_a.PrintNullValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | NullValue _    -> stgMacro ()
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType)  =
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | StringValue v    -> 
            match l with 
            | C ->  variables_c.PrintStringValue v []
            | Ada ->
                let arrNuls = [0 .. (int o.maxSize.uper - v.Length)] |> Seq.map(fun x -> variables_a.PrintStringValueNull())
                variables_a.PrintStringValue (v.Replace("\"","\"\"")) arrNuls

        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")

    printValue

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (defOrRef:TypeDefintionOrReference) =
    let PrintOctetStringValue = match l with C -> variables_c.PrintOctetStringValue | Ada -> variables_a.PrintOctetStringValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | OctetStringValue  v -> 
            let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
            let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
            PrintOctetStringValue td (o.minSize.uper = o.maxSize.uper) v (BigInteger v.Length)
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v) |> Seq.toList
            let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
            let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
            PrintOctetStringValue td (o.minSize.uper = o.maxSize.uper) bytes (BigInteger bytes.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createObjectIdentifierFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (defOrRef:TypeDefintionOrReference) =
    let PrintObjectIdentifierValue = match l with C -> variables_c.PrintObjectIdentifierValue | Ada -> variables_a.PrintObjectIdentifierValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        let td = o.typeDef.[l]
        match v with
        | ObjOrRelObjIdValue  v -> 
            PrintObjectIdentifierValue  td (v.Values |> List.map fst) (BigInteger v.Values.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createTimeTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (defOrRef:TypeDefintionOrReference) =
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        let td = o.typeDef.[l]
        match v with
        | TimeValue  v            -> printTimeValue lm td v
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (defOrRef:TypeDefintionOrReference) =
    let PrintBitStringValue = match l with C -> variables_c.PrintBitStringValue | Ada -> variables_a.PrintBitStringValue
    let PrintOctetStringValue = match l with C -> variables_c.PrintOctetStringValue | Ada -> variables_a.PrintOctetStringValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
            let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
            let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
            PrintBitStringValue td (o.minSize.uper = o.maxSize.uper) arBits (BigInteger arBits.Length) bytes (BigInteger bytes.Length)
        | OctetStringValue  v -> 
            let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
            let typeDefName  = defOrRef.longTypedefName l //if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
            PrintOctetStringValue td (o.minSize.uper = o.maxSize.uper) v (BigInteger v.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (defOrRef:TypeDefintionOrReference) (childType:Asn1Type) =
    let stgMacro = match l with C -> variables_c.PrintBooleanValue | Ada -> variables_a.PrintBooleanValue
    let PrintSequenceOfValue = match l with C -> variables_c.PrintSequenceOfValue | Ada -> variables_a.PrintSequenceOfValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | SeqOfValue chVals    -> 
            let childVals = chVals |> List.map (fun chv -> childType.printValue curProgamUnitName (Some gv) chv.kind)
            let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
            let sDefValue =  childType.initFunction.initExpression //childType.printValue curProgamUnitName  None childType.initialValue 
            let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
            PrintSequenceOfValue td (o.minSize.uper = o.maxSize.uper) (BigInteger chVals.Length) childVals sDefValue

        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (defOrRef:TypeDefintionOrReference) (children:SeqChildInfo list) =
    let PrintSequenceValue_child_exists = match l with C -> variables_c.PrintSequenceValue_child_exists | Ada -> variables_a.PrintSequenceValue_child_exists
    let PrintSequenceValueChild = match l with C -> variables_c.PrintSequenceValueChild | Ada -> variables_a.PrintSequenceValueChild
    let PrintSequenceValue = match l with C ->  variables_c.PrintSequenceValue | Ada ->  variables_a.PrintSequenceValue
    
    let optChildren = 
        children |>
        List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
        List.filter(fun ch -> ch.Optionality.IsSome) 
    
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | SeqValue  v -> 
                let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                let optChildren = 
                    optChildren |>
                    List.map(fun x ->
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some _    -> PrintSequenceValue_child_exists (x.getBackendName l) "1"
                        | None      -> PrintSequenceValue_child_exists (x.getBackendName l) "0")
                let arrChildren = 
                    children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.choose(fun x -> 
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some v    -> 
                            let childValue = x.Type.printValue curProgamUnitName (Some gv) v.Value.kind
                            Some (variables_a.PrintSequenceValueChild (x.getBackendName l) childValue)
                        | None      -> 
                            let childValue = 
                                match x.Optionality with
                                | Some(Asn1AcnAst.Optional opt)    -> 
                                    match opt.defaultValue with
                                    | Some zz    -> 
                                        let v = (mapValue zz).kind
                                        Some(x.Type.printValue curProgamUnitName (Some gv) v)
                                    | None      -> match l with C -> None | Ada -> Some (x.Type.initFunction.initExpression)
                                | _             -> match l with C -> None | Ada -> Some (x.Type.initFunction.initExpression)
                            match childValue with
                            | None  -> None
                            | Some childValue -> Some (PrintSequenceValueChild (x.getBackendName l) childValue) )
                PrintSequenceValue td typeDefName arrChildren optChildren
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (defOrRef:TypeDefintionOrReference) (children:ChChildInfo list) =
    let PrintChoiceValue = match l with C -> variables_c.PrintChoiceValue | Ada -> variables_a.PrintChoiceValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | ChValue chVal    -> 
            let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
            children |>
            List.filter(fun x -> x.Name.Value = chVal.name)  |>
            List.map(fun x -> 
                
                let childValue = (x.chType.printValue curProgamUnitName (Some gv) chVal.Value.kind)
                let sChildNamePresent = x.presentWhenName (Some defOrRef) l
                variables_c.PrintChoiceValue typeDefName (x.getBackendName l) childValue   sChildNamePresent true
                ) |>
            List.head
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (defOrRef:TypeDefintionOrReference) (baseType:Asn1Type)   =
    baseType.printValue

