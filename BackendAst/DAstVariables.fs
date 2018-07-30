module DAstVariables

open System
open System.Numerics
open System.IO

open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions



let getDefaultValueByType  (t:Asn1Type)  =  t.initialValue

let rec printValue (r:DAst.AstRoot)  (l:ProgrammingLanguage)  (curProgamUnitName:string)  (t:Asn1Type) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
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
                variables_c.PrintEnumValue (itm.getBackendName (Some t.typeDefintionOrReference) l)
            | _         -> raise(BugErrorException "unexpected type")
        | NullValue         v -> variables_c.PrintNullValue ()
        | SeqOfValue        v -> 
            match t.ActualType.Kind with
            | SequenceOf so -> 
                let childVals = v |> List.map (fun chv -> printValue r l curProgamUnitName so.childType (Some gv) chv.kind)
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
                        | Some _    -> variables_c.PrintSequenceValue_child_exists (x.getBackendName l) "1"
                        | None      -> variables_c.PrintSequenceValue_child_exists (x.getBackendName l) "0")
                let arrChildren = 
                    s.children |>
                    List.choose(fun ch -> match ch with Asn1Child a -> Some a | AcnChild _ -> None) |>
                    List.choose(fun cht -> 
                        match v |> Seq.tryFind(fun chv -> chv.name = cht.Name.Value) with
                        | Some v    -> Some (variables_c.PrintSequenceValueChild (cht.getBackendName l) (printValue r l curProgamUnitName cht.Type (Some gv) v.Value.kind))
                        | None      -> 
                            match cht.Optionality with
                            | Some(Asn1AcnAst.Optional opt)    -> 
                                match opt.defaultValue with
                                | Some v    -> Some (variables_c.PrintSequenceValueChild (cht.getBackendName l) (printValue r l curProgamUnitName cht.Type (Some gv) (mapValue v).kind ))                    
                                | None      -> None
                            | _             -> None)
                variables_c.PrintSequenceValue arrChildren optChildren
            | _         -> raise(BugErrorException "unexpected type")

        | ChValue           v -> 
            match t.ActualType.Kind with
            | Choice s -> 
                s.children |>
                List.filter(fun x -> x.Name.Value = v.name)  |>
                List.map(fun x -> variables_c.PrintChoiceValue (x.presentWhenName (Some t.typeDefintionOrReference) l) (x.getBackendName l) (printValue r l curProgamUnitName x.chType (Some gv) v.Value.kind)) |>
                List.head
            | _         -> raise(BugErrorException "unexpected type")
        | RefValue ((md,vs),v)         ->
            printValue r  l  curProgamUnitName t parentValue v.kind
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
                let arrNuls = [0 .. ((int st.baseInfo.maxSize) - v.Length)]|>Seq.map(fun x -> variables_a.PrintStringValueNull())
                variables_a.PrintStringValue (v.Replace("\"","\"\"")) arrNuls
            | _             -> raise(BugErrorException "unexpected type")
        | BooleanValue      v -> variables_a.PrintBooleanValue v
        | BitStringValue    v -> 
            match t.ActualType.Kind with
            | OctetString os    -> 
                let td = (os.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then os.typeDefinition.typeDefinitionBodyWithinSeq else os.typeDefinition.name
                let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
                variables_a.PrintOctetStringValue td (os.baseInfo.minSize = os.baseInfo.maxSize) bytes (BigInteger bytes.Length)
            | BitString   bs    -> 
                let td = (bs.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l //if parentValue.IsSome then bs.typeDefinition.typeDefinitionBodyWithinSeq else bs.typeDefinition.name
                let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
                variables_a.PrintBitStringValue td (bs.baseInfo.minSize = bs.baseInfo.maxSize) arBits (BigInteger arBits.Length)
            | _         -> raise(BugErrorException "unexpected type")
        | OctetStringValue  v -> 
            match t.ActualType.Kind with
            | OctetString os    -> 
                let td = (os.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then os.typeDefinition.typeDefinitionBodyWithinSeq else os.typeDefinition.name
                variables_a.PrintOctetStringValue td (os.baseInfo.minSize = os.baseInfo.maxSize) v (BigInteger v.Length)
            | BitString   bs    -> 
                let td = (bs.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l//if parentValue.IsSome then bs.typeDefinition.typeDefinitionBodyWithinSeq else bs.typeDefinition.name
                let bittring = byteArrayToBitStringValue v
                let arBits = bittring.ToCharArray() |> Array.map(fun x -> x.ToString()) 
                let maxLen = if (arBits.Length > int bs.baseInfo.maxSize) then ((int bs.baseInfo.maxSize)-1) else (arBits.Length-1)
                let arBits = arBits.[0 .. maxLen]
                variables_a.PrintBitStringValue td (bs.baseInfo.minSize = bs.baseInfo.maxSize) arBits (BigInteger arBits.Length)
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
        | SeqOfValue        v -> 
            match t.ActualType.Kind with
            | SequenceOf so -> 
                let td = (so.baseInfo.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = t.typeDefintionOrReference.longTypedefName l //if parentValue.IsSome then so.typeDefinition.typeDefinitionBodyWithinSeq else so.typeDefinition.name
                let childVals = v |> List.map (fun chv -> printValue r l curProgamUnitName so.childType (Some gv) chv.kind)
                let sDefValue = printValue r l curProgamUnitName so.childType None (getDefaultValueByType so.childType)
                variables_a.PrintSequenceOfValue td (so.baseInfo.minSize = so.baseInfo.maxSize) (BigInteger v.Length) childVals sDefValue
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
                        | Some v    -> variables_a.PrintSequenceValueChild (x.getBackendName l) (printValue r l curProgamUnitName x.Type (Some gv) v.Value.kind)
                        | None      -> 
                            let chV = 
                                match x.Optionality with
                                | Some(Asn1AcnAst.Optional opt)    -> 
                                    match opt.defaultValue with
                                    | Some v    -> (mapValue v).kind
                                    | None      -> getDefaultValueByType x.Type
                                | _             -> getDefaultValueByType x.Type
                            variables_a.PrintSequenceValueChild (x.getBackendName l) (printValue r l curProgamUnitName x.Type None chV) )
                let allChildren = match Seq.isEmpty optChildren with
                                  | true     -> arrChildren
                                  | false    -> arrChildren @ [variables_a.PrintSequenceValue_Exists td optChildren]
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
                List.map(fun x -> variables_a.PrintChoiceValue typeDefName (x.getBackendName l) (printValue r l  curProgamUnitName x.chType (Some gv) v.Value.kind) (x.presentWhenName (Some t.typeDefintionOrReference) l) ) |>
                List.head
            | _         -> raise(BugErrorException "unexpected type")

        | RefValue ((md,vs),v)         ->
            match t.ActualType.Kind with
            | Integer _
            | Real  _   ->
                printValue r  l  curProgamUnitName t parentValue v.kind
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
            | C ->  variables_c.PrintStringValue v
            | Ada ->
                let arrNuls = [0 .. (int o.maxSize - v.Length)] |> Seq.map(fun x -> variables_a.PrintStringValueNull())
                variables_a.PrintStringValue (v.Replace("\"","\"\"")) arrNuls

        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")

    printValue

let createOctetStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (defOrRef:TypeDefintionOrReference) =
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | OctetStringValue  v -> 
            match l with 
            | C ->  variables_c.PrintBitOrOctetStringValue (o.minSize = o.maxSize) v (BigInteger v.Length)
            | Ada -> 
                let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                variables_a.PrintOctetStringValue td (o.minSize = o.maxSize) v (BigInteger v.Length)
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            match l with 
            | C ->  
                variables_c.PrintBitOrOctetStringValue (o.minSize = o.maxSize) bytes (BigInteger bytes.Length)
            | Ada -> 
                let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                variables_a.PrintOctetStringValue td (o.minSize = o.maxSize) bytes (BigInteger bytes.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createBitStringFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (defOrRef:TypeDefintionOrReference) =
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (v:Asn1ValueKind) =
        match v with
        | BitStringValue    v -> 
            let bytes = bitStringValueToByteArray (StringLoc.ByValue v)
            match l with 
            | C ->  
                variables_c.PrintBitOrOctetStringValue (o.minSize = o.maxSize) bytes (BigInteger bytes.Length)
            | Ada -> 
                let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                let arBits = v.ToCharArray() |> Array.map(fun x -> x.ToString())
                variables_a.PrintBitStringValue td (o.minSize = o.maxSize) arBits (BigInteger arBits.Length)

        | OctetStringValue  v -> 
            match l with 
            | C ->  variables_c.PrintBitOrOctetStringValue (o.minSize = o.maxSize) v (BigInteger v.Length)
            | Ada -> 
                let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
                let typeDefName  = defOrRef.longTypedefName l //if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                variables_a.PrintOctetStringValue td (o.minSize = o.maxSize) v (BigInteger v.Length)
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createSequenceOfFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (defOrRef:TypeDefintionOrReference) (childType:Asn1Type) =
    let stgMacro = match l with C -> variables_c.PrintBooleanValue | Ada -> variables_a.PrintBooleanValue
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | SeqOfValue chVals    -> 
            let childVals = chVals |> List.map (fun chv -> childType.printValue curProgamUnitName (Some gv) chv.kind)
            match l with 
            | C ->   variables_c.PrintSequenceOfValue (o.minSize = o.maxSize)childVals
            | Ada ->
                let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
                let sDefValue =  childType.printValue curProgamUnitName  None childType.initialValue 
                let td = (o.typeDef.[l]).longTypedefName l curProgamUnitName
                variables_a.PrintSequenceOfValue td (o.minSize = o.maxSize) (BigInteger chVals.Length) childVals sDefValue
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createSequenceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (defOrRef:TypeDefintionOrReference) (children:SeqChildInfo list) =
    let PrintSequenceValue_child_exists = match l with C -> variables_c.PrintSequenceValue_child_exists | Ada -> variables_a.PrintSequenceValue_child_exists
    let PrintSequenceValueChild = match l with C -> variables_c.PrintSequenceValueChild | Ada -> variables_a.PrintSequenceValueChild
    
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
                                    | None      -> match l with C -> None | Ada -> Some (x.Type.printValue curProgamUnitName (Some gv) x.Type.initialValue)
                                | _             -> match l with C -> None | Ada -> Some (x.Type.printValue curProgamUnitName (Some gv) x.Type.initialValue)
                            match childValue with
                            | None  -> None
                            | Some childValue -> Some (PrintSequenceValueChild (x.getBackendName l) childValue) )
                                            
                match l with 
                | C -> 
                    variables_c.PrintSequenceValue arrChildren optChildren
                | Ada -> 
                    let allChildren = match Seq.isEmpty optChildren with
                                      | true     -> arrChildren
                                      | false    -> arrChildren @ [variables_a.PrintSequenceValue_Exists td optChildren]
                    variables_a.PrintSequenceValue typeDefName allChildren
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue


let createChoiceFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (defOrRef:TypeDefintionOrReference) (children:ChChildInfo list) =
    let printValue (curProgamUnitName:string) (parentValue:Asn1ValueKind option) (gv:Asn1ValueKind) =
        match gv with
        | ChValue chVal    -> 
            let typeDefName  = defOrRef.longTypedefName l//if parentValue.IsSome then typeDefinition.typeDefinitionBodyWithinSeq else typeDefinition.name
            match l with
            | C ->
                children |>
                List.filter(fun x -> x.Name.Value = chVal.name)  |>
                List.map(fun x -> variables_c.PrintChoiceValue (x.presentWhenName (Some defOrRef) l) (x.getBackendName l) (x.chType.printValue curProgamUnitName (Some gv) chVal.Value.kind)) |>
                List.head
            | Ada   ->
                children |>
                List.filter(fun x -> x.Name.Value = chVal.name)  |>
                List.map(fun x -> variables_a.PrintChoiceValue typeDefName (x.getBackendName l) (x.chType.printValue curProgamUnitName (Some gv) chVal.Value.kind) (x.presentWhenName (Some defOrRef) l)) |>
                List.head
        | RefValue ((md,vs),ov)   -> vs
        | _                 -> raise(BugErrorException "unexpected value")
    printValue

let createReferenceTypeFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (defOrRef:TypeDefintionOrReference) (baseType:Asn1Type)   =
    baseType.printValue
