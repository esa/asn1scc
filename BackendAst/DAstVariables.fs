﻿module DAstVariables

open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions



let getDefaultValueByType  (t:Asn1Type)  =  t.initialValue

let rec printValue (r:DAst.AstRoot) (l:ProgrammingLanguage)  (t:Asn1Type) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
    match l with
    | C ->
        match gv with
        | IntegerValue      v -> variables_c.PrintIntValue v
        | RealValue         v -> variables_c.PrintRealValue v
        | StringValue       v -> variables_c.PrintStringValue v
        | BooleanValue      v -> variables_c.PrintBooleanValue v
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            match t.ActualType.Kind with
            | OctetString os    -> variables_c.PrintBitOrOctetStringValue (os.baseInfo.minSize = os.baseInfo.maxSize) bytes (BigInteger bytes.Length)
            | BitString   bs    -> variables_c.PrintBitOrOctetStringValue (bs.baseInfo.minSize = bs.baseInfo.maxSize) bytes (BigInteger bytes.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | OctetStringValue  v -> 
            match t.ActualType.Kind with
            | OctetString os    -> variables_c.PrintBitOrOctetStringValue (os.baseInfo.minSize = os.baseInfo.maxSize) v (BigInteger v.Length)
            | BitString   bs    -> variables_c.PrintBitOrOctetStringValue (bs.baseInfo.minSize = bs.baseInfo.maxSize) v (BigInteger v.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | EnumValue         v -> 
            match t.ActualType.Kind with
            | Enumerated enm    -> 
                let itm = enm.baseInfo.items |> Seq.find(fun x -> x.Name.Value = v)
                variables_c.PrintEnumValue (itm.getBackendName l)
            | _         -> raise(BugErrorException "unexpected type")
        | NullValue         v -> variables_c.PrintNullValue ()
        | SeqOfValue        v -> 
            match t.ActualType.Kind with
            | SequenceOf so -> 
                let childVals = v |> List.map (fun chv -> printValue r l so.childType (Some gv) chv.kind)
                variables_c.PrintSequenceOfValue (so.baseInfo.minSize = so.baseInfo.maxSize) childVals
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
                        | Some _    -> variables_c.PrintSequenceValue_child_exists x.c_name "1"
                        | None      -> variables_c.PrintSequenceValue_child_exists x.c_name "0")
                let arrChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.choose(fun cht -> 
                        match v |> Seq.tryFind(fun chv -> chv.name = cht.Name.Value) with
                        | Some v    -> Some (variables_c.PrintSequenceValueChild cht.c_name (printValue r l cht.Type (Some gv) v.Value.kind))
                        | None      -> 
                            match cht.Optionality with
                            | Some(Asn1AcnAst.Optional opt)    -> 
                                match opt.defaultValue with
                                | Some v    -> Some (variables_c.PrintSequenceValueChild cht.c_name (printValue r l cht.Type (Some gv) (mapValue v).kind ))                    
                                | None      -> None
                            | _             -> None)
                variables_c.PrintSequenceValue arrChildren optChildren
            | _         -> raise(BugErrorException "unexpected type")

        | ChValue           v -> 
            match t.ActualType.Kind with
            | Choice s -> 
                s.children |>
                List.filter(fun x -> x.Name.Value = v.name)  |>
                List.map(fun x -> variables_c.PrintChoiceValue x.presentWhenName x.c_name (printValue r l x.chType (Some gv) v.Value.kind)) |>
                List.head
            | _         -> raise(BugErrorException "unexpected type")
        | RefValue ((md,vs),v)         ->
            let vas = r.getValueAssignmentByName md vs
            vas.c_name
    | Ada ->
        match gv with
        | IntegerValue      v -> variables_a.PrintIntValue v
        | RealValue         v -> variables_a.PrintRealValue v
        | StringValue       v -> 
            match t.ActualType.Kind with
            | IA5String st  ->
                let arrNuls = [0 .. (st.baseInfo.maxSize- v.Length)]|>Seq.map(fun x -> variables_a.PrintStringValueNull())
                variables_a.PrintStringValue (v.Replace("\"","\"\"")) arrNuls
            | _             -> raise(BugErrorException "unexpected type")
        | BooleanValue      v -> variables_a.PrintBooleanValue v
        | BitStringValue    v -> 
            match t.ActualType.Kind with
            | OctetString os    -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then os.typeDefinition.typeDefinitionBodyWithinSeq else os.typeDefinition.name
                let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
                variables_a.PrintOctetStringValue typeDefName (os.baseInfo.minSize = os.baseInfo.maxSize) bytes (BigInteger bytes.Length)
            | BitString   bs    -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l //if parentValue.IsSome then bs.typeDefinition.typeDefinitionBodyWithinSeq else bs.typeDefinition.name
                let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
                variables_a.PrintBitStringValue typeDefName (bs.baseInfo.minSize = bs.baseInfo.maxSize) arBits (BigInteger arBits.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | OctetStringValue  v -> 
            match t.ActualType.Kind with
            | OctetString os    -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then os.typeDefinition.typeDefinitionBodyWithinSeq else os.typeDefinition.name
                variables_a.PrintOctetStringValue typeDefName (os.baseInfo.minSize = os.baseInfo.maxSize) v (BigInteger v.Length)
            | BitString   bs    -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then bs.typeDefinition.typeDefinitionBodyWithinSeq else bs.typeDefinition.name
                let bittring = byteArrayToBitStringValue v
                let arBits = bittring.ToCharArray() |> Array.map(fun x -> x.ToString()) 
                let maxLen = if (arBits.Length > bs.baseInfo.maxSize) then (bs.baseInfo.maxSize-1) else (arBits.Length-1)
                let arBits = arBits.[0..maxLen]
                variables_a.PrintBitStringValue typeDefName (bs.baseInfo.minSize = bs.baseInfo.maxSize) arBits (BigInteger arBits.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | EnumValue         v -> 
            match t.ActualType.Kind with
            | Enumerated enm    -> 
                let itm = enm.baseInfo.items |> Seq.find(fun x -> x.Name.Value = v)
                variables_a.PrintEnumValue (itm.getBackendName l)
            | _         -> raise(BugErrorException "unexpected type")
        | NullValue         v -> variables_a.PrintNullValue ()
        | SeqOfValue        v -> 
            match t.ActualType.Kind with
            | SequenceOf so -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l //if parentValue.IsSome then so.typeDefinition.typeDefinitionBodyWithinSeq else so.typeDefinition.name
                let childVals = v |> List.map (fun chv -> printValue r l so.childType (Some gv) chv.kind)
                let sDefValue = printValue r l so.childType None (getDefaultValueByType so.childType)
                variables_a.PrintSequenceOfValue typeDefName (so.baseInfo.minSize = so.baseInfo.maxSize) (BigInteger v.Length) childVals sDefValue
            | _         -> raise(BugErrorException "unexpected type")

        | SeqValue          v -> 
            match t.ActualType.Kind with
            | Sequence s -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then s.typeDefinition.typeDefinitionBodyWithinSeq else s.typeDefinition.name
                let optChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.filter(fun ch -> ch.Optionality.IsSome) |>
                    List.map(fun x ->
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some _    -> variables_a.PrintSequenceValue_child_exists x.c_name "1"
                        | None      -> variables_a.PrintSequenceValue_child_exists x.c_name "0")
                let arrChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.map(fun x -> 
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some v    -> variables_a.PrintSequenceValueChild x.c_name (printValue r l x.Type (Some gv) v.Value.kind)
                        | None      -> 
                            let chV = 
                                match x.Optionality with
                                | Some(Asn1AcnAst.Optional opt)    -> 
                                    match opt.defaultValue with
                                    | Some v    -> (mapValue v).kind
                                    | None      -> getDefaultValueByType x.Type
                                | _             -> getDefaultValueByType x.Type
                            variables_a.PrintSequenceValueChild x.c_name (printValue r l x.Type None chV) )
                let allChildren = match Seq.isEmpty optChildren with
                                  | true     -> arrChildren
                                  | false    -> arrChildren @ [variables_a.PrintSequenceValue_Exists typeDefName optChildren]
                variables_a.PrintSequenceValue typeDefName allChildren
            | _         -> raise(BugErrorException "unexpected type")
        | ChValue           v -> 
            match t.ActualType.Kind with
            | Choice s -> 
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l
                    (*
                    match t.tasInfo with
                    | Some tasInfo  -> ToC2(r.args.TypePrefix + tasInfo.tasName)
                    | None          ->
                        match t.Kind with
                        | ReferenceType ref ->     ToC2(r.args.TypePrefix + ref.baseInfo.tasName.Value)
                        | _                 ->
                            if parentValue.IsSome then s.typeDefinition.typeDefinitionBodyWithinSeq else s.typeDefinition.name*)
                s.children |>
                List.filter(fun x -> x.Name.Value = v.name)  |>
                List.map(fun x -> variables_a.PrintChoiceValue typeDefName x.c_name (printValue r l  x.chType (Some gv) v.Value.kind) x.presentWhenName) |>
                List.head
            | _         -> raise(BugErrorException "unexpected type")

        | RefValue ((md,vs),v)         ->
            let vas = r.getValueAssignmentByName md vs
            vas.ada_name
    | Python ->
        match gv with
        | IntegerValue      v -> variables_p.PrintIntValue v
        | RealValue         v -> variables_p.PrintRealValue v
        | StringValue       v -> variables_p.PrintStringValue v
        | BooleanValue      v -> variables_p.PrintBooleanValue v
        | BitStringValue    v -> 
            match t.ActualType.Kind with
            | OctetString os    -> 
                let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
                variables_p.PrintOctetStringValue (os.baseInfo.minSize = os.baseInfo.maxSize) bytes (BigInteger bytes.Length)
            | BitString   bs    -> 
                let bytes = v.ToCharArray() |> Seq.map(fun x -> if x='0' then 0uy else 1uy) |> Seq.toList
                variables_p.PrintBitStringValue (bs.baseInfo.minSize = bs.baseInfo.maxSize) bytes (BigInteger bytes.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | OctetStringValue  v -> 
            match t.ActualType.Kind with
            | OctetString os    -> variables_p.PrintOctetStringValue (os.baseInfo.minSize = os.baseInfo.maxSize) v (BigInteger v.Length)
            | BitString   bs    -> variables_p.PrintOctetStringValue (bs.baseInfo.minSize = bs.baseInfo.maxSize) v (BigInteger v.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | EnumValue         v -> 
            match t.ActualType.Kind with
            | Enumerated enm    -> 
                let itm = enm.baseInfo.items |> Seq.find(fun x -> x.Name.Value = v)
                variables_p.PrintEnumValue (itm.definitionValue.ToString())
            | _         -> raise(BugErrorException "unexpected type")
        | NullValue         v -> variables_p.PrintNullValue ()
        | SeqOfValue        v -> 
            match t.ActualType.Kind with
            | SequenceOf so -> 
                let childVals = v |> List.map (fun chv -> printValue r l so.childType (Some gv) chv.kind)
                variables_p.PrintSequenceOfValue (so.baseInfo.minSize = so.baseInfo.maxSize) childVals
            | _         -> raise(BugErrorException "unexpected type")
        | SeqValue          v -> 
            match t.ActualType.Kind with
            | Sequence s -> 
                let arrChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.choose(fun cht -> 
                        match v |> Seq.tryFind(fun chv -> chv.name = cht.Name.Value) with
                        | Some v    -> Some (variables_p.PrintSequenceValueChild cht.c_name (printValue r l cht.Type (Some gv) v.Value.kind))
                        | None      -> 
                            match cht.Optionality with
                            | Some(Asn1AcnAst.Optional opt)    -> 
                                match opt.defaultValue with
                                | Some v    -> Some (variables_p.PrintSequenceValueChild cht.c_name (printValue r l cht.Type (Some gv) (mapValue v).kind ))                    
                                | None      -> None
                            | _             -> None)
                variables_p.PrintSequenceValue arrChildren
            | _         -> raise(BugErrorException "unexpected type")

        | ChValue           v -> 
            match t.ActualType.Kind with
            | Choice s -> 
                s.children |>
                List.filter(fun x -> x.Name.Value = v.name)  |>
                List.map(fun x -> variables_p.PrintChoiceValue x.presentWhenName x.c_name (printValue r l x.chType (Some gv) v.Value.kind)) |>
                List.head
            | _         -> raise(BugErrorException "unexpected type")
        | RefValue ((md,vs),v)         ->
            let vas = r.getValueAssignmentByName md vs
            vas.c_name


let createIntegerFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefinitionCommon) =
    let stgMacro = match l with C -> variables_c.PrintIntValue | Ada -> variables_a.PrintIntValue | Python -> variables_p.PrintIntValue
    let printValue (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | IntegerValue i    -> stgMacro i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createRealFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (typeDefinition:TypeDefinitionCommon) =
    let stgMacro = match l with C -> variables_c.PrintRealValue | Ada -> variables_a.PrintRealValue | Python -> variables_p.PrintRealValue
    let printValue (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | RealValue i    -> stgMacro i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createBooleanFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (typeDefinition:TypeDefinitionCommon) =
    let stgMacro = match l with C -> variables_c.PrintBooleanValue | Ada -> variables_a.PrintBooleanValue | Python -> variables_p.PrintBooleanValue
    let printValue (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | BooleanValue i    -> stgMacro i
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createEnumeratedFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (typeDefinition:TypeDefinitionCommon) =
    let stgMacro = match l with C -> variables_c.PrintEnumValue | Ada -> variables_a.PrintEnumValue | Python -> variables_p.PrintEnumValue
    let printValue (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | EnumValue i    -> 
            let itm = o.items |> Seq.find(fun x -> x.Name.Value = i)
            stgMacro (itm.getBackendName l)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createNullTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (typeDefinition:TypeDefinitionCommon) =
    let stgMacro = match l with C -> variables_c.PrintNullValue | Ada -> variables_a.PrintNullValue | Python -> variables_p.PrintNullValue
    let printValue (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | NullValue _    -> stgMacro ()
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (typeDefinition:TypeDefinitionCommon) =
    let printValue (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | StringValue v    -> 
            match l with 
            | C ->  variables_c.PrintStringValue v
            | Ada ->
                let arrNuls = [0 .. (o.maxSize- v.Length)] |> Seq.map(fun x -> variables_a.PrintStringValueNull())
                variables_a.PrintStringValue (v.Replace("\"","\"\"")) arrNuls
            | Python -> variables_p.PrintStringValue v

        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")

    printValue

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (typeDefinition:TypeDefinitionCommon) =
    let printValue (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | OctetStringValue  v -> 
            match l with 
            | C ->  variables_c.PrintBitOrOctetStringValue (o.minSize = o.maxSize) v (BigInteger v.Length)
            | Ada -> 
                let typeDefName  = if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                variables_a.PrintOctetStringValue typeDefName (o.minSize = o.maxSize) v (BigInteger v.Length)
            | Python ->  variables_p.PrintOctetStringValue (o.minSize = o.maxSize) v (BigInteger v.Length)
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            match l with 
            | C ->  
                variables_c.PrintBitOrOctetStringValue (o.minSize = o.maxSize) bytes (BigInteger bytes.Length)
            | Ada -> 
                let typeDefName  = if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                variables_a.PrintOctetStringValue typeDefName (o.minSize = o.maxSize) bytes (BigInteger bytes.Length)
            | Python ->
                variables_p.PrintOctetStringValue (o.minSize = o.maxSize) bytes (BigInteger bytes.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (typeDefinition:TypeDefinitionCommon) =
    let printValue (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            match l with 
            | C ->  
                variables_c.PrintBitOrOctetStringValue (o.minSize = o.maxSize) bytes (BigInteger bytes.Length)
            | Ada -> 
                let typeDefName  = if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
                variables_a.PrintBitStringValue typeDefName (o.minSize = o.maxSize) arBits (BigInteger arBits.Length)
            | Python ->  
                let bytes = v.ToCharArray() |> Seq.map(fun x -> if x='0' then 0uy else 1uy) |> Seq.toList
                variables_p.PrintBitStringValue (o.minSize = o.maxSize) bytes (BigInteger bytes.Length)

        | OctetStringValue  v -> 
            match l with 
            | C ->  variables_c.PrintBitOrOctetStringValue (o.minSize = o.maxSize) v (BigInteger v.Length)
            | Ada -> 
                let typeDefName  = if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                variables_a.PrintOctetStringValue typeDefName (o.minSize = o.maxSize) v (BigInteger v.Length)
            | Python ->  variables_p.PrintOctetStringValue (o.minSize = o.maxSize) v (BigInteger v.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (typeDefinition:TypeDefinitionCommon) (childType:Asn1Type) =
    let stgMacro = match l with C -> variables_c.PrintBooleanValue | Ada -> variables_a.PrintBooleanValue | Python -> variables_p.PrintBooleanValue
    let printValue (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | SeqOfValue chVals    -> 
            let childVals = chVals |> List.map (fun chv -> childType.printValue (Some gv) chv.kind)
            match l with 
            | C ->   variables_c.PrintSequenceOfValue (o.minSize = o.maxSize)childVals
            | Ada ->
                let typeDefName  = if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                let sDefValue =  childType.printValue None childType.initialValue 
                variables_a.PrintSequenceOfValue typeDefName (o.minSize = o.maxSize) (BigInteger chVals.Length) childVals sDefValue
            | Python ->   variables_p.PrintSequenceOfValue (o.minSize = o.maxSize)childVals
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionCommon) (children:SeqChildInfo list) =
    let PrintSequenceValue_child_exists = match l with C -> variables_c.PrintSequenceValue_child_exists | Ada -> variables_a.PrintSequenceValue_child_exists    | Python -> (fun _ _ -> "")
    let PrintSequenceValueChild         = match l with C -> variables_c.PrintSequenceValueChild         | Ada -> variables_a.PrintSequenceValueChild            | Python -> variables_p.PrintSequenceValueChild
    
    let optChildren = 
        children |>
        List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
        List.filter(fun ch -> ch.Optionality.IsSome) 
    
    let printValue (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | SeqValue  v -> 
                let typeDefName  = if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                let optChildren = 
                    optChildren |>
                    List.map(fun x ->
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some _    -> PrintSequenceValue_child_exists x.c_name "1"
                        | None      -> PrintSequenceValue_child_exists x.c_name "0")
                let arrChildren = 
                    children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.choose(fun x -> 
                        match v |> Seq.tryFind(fun chv -> chv.name = x.Name.Value) with
                        | Some v    -> 
                            let childValue = x.Type.printValue (Some gv) v.Value.kind
                            Some (variables_a.PrintSequenceValueChild x.c_name childValue)
                        | None      -> 
                            let childValue = 
                                match x.Optionality with
                                | Some(Asn1AcnAst.Optional opt)    -> 
                                    match opt.defaultValue with
                                    | Some zz    -> 
                                        let v = (mapValue zz).kind
                                        Some(x.Type.printValue (Some gv) v)
                                    | None      -> match l with C -> None | Ada -> Some (x.Type.printValue (Some gv) x.Type.initialValue) | Python -> None
                                | _             -> match l with C -> None | Ada -> Some (x.Type.printValue (Some gv) x.Type.initialValue) | Python -> None
                            match childValue with
                            | None  -> None
                            | Some childValue -> Some (PrintSequenceValueChild x.c_name childValue) )
                                            
                match l with 
                | C -> 
                    variables_c.PrintSequenceValue arrChildren optChildren
                | Ada -> 
                    let allChildren = match Seq.isEmpty optChildren with
                                      | true     -> arrChildren
                                      | false    -> arrChildren @ [variables_a.PrintSequenceValue_Exists typeDefName optChildren]
                    variables_a.PrintSequenceValue typeDefName allChildren
                | Python -> 
                    variables_p.PrintSequenceValue arrChildren
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (typeDefinition:TypeDefinitionCommon) (children:ChChildInfo list) =
    let printValue (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | ChValue chVal    -> 
            let typeDefName  = if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
            match l with
            | C ->
                children |>
                List.filter(fun x -> x.Name.Value = chVal.name)  |>
                List.map(fun x -> variables_c.PrintChoiceValue x.presentWhenName x.c_name (x.chType.printValue (Some gv) chVal.Value.kind)) |>
                List.head
            | Ada   ->
                children |>
                List.filter(fun x -> x.Name.Value = chVal.name)  |>
                List.map(fun x -> variables_a.PrintChoiceValue typeDefName x.c_name (x.chType.printValue (Some gv) chVal.Value.kind) x.presentWhenName) |>
                List.head
            | Python ->
                children |>
                List.filter(fun x -> x.Name.Value = chVal.name)  |>
                List.map(fun x -> variables_p.PrintChoiceValue x.presentWhenName x.c_name (x.chType.printValue (Some gv) chVal.Value.kind)) |>
                List.head
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefinitionCommon) (baseType:Asn1Type)   =
    baseType.printValue
