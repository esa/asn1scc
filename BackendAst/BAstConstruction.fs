module BAstConstruction
open System
open CommonTypes
open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open Constraints
open uPER2
open FsUtils
open BAst

type State = {
    anonymousTypes : Asn1Type list
    anonymousValues : Asn1GenericValue list
}
//with 
//    member this.add (other:State) =
//        {State.anonymousTypes = this.anonymousTypes@other.anonymousTypes; anonymousValues = this.anonymousValues@other.anonymousValues}


let tryGetType (s:State) typeId = s.anonymousTypes |> Seq.tryFind (fun x -> x.id = typeId )

let getAsInteger       (t:Asn1Type option) = match t with None -> None | Some(Integer      i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsReal          (t:Asn1Type option) = match t with None -> None | Some(Real         i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsIA5String     (t:Asn1Type option) = match t with None -> None | Some(IA5String    i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsOctetString   (t:Asn1Type option) = match t with None -> None | Some(OctetString  i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsNullType      (t:Asn1Type option) = match t with None -> None | Some(NullType     i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsBitString     (t:Asn1Type option) = match t with None -> None | Some(BitString    i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsBoolean       (t:Asn1Type option) = match t with None -> None | Some(Boolean      i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsEnumerated    (t:Asn1Type option) = match t with None -> None | Some(Enumerated   i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsSequenceOf    (t:Asn1Type option) = match t with None -> None | Some(SequenceOf   i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsSequence      (t:Asn1Type option) = match t with None -> None | Some(Sequence     i) -> Some i | _  -> raise(Exception "unexpected type")
let getAsChoice        (t:Asn1Type option) = match t with None -> None | Some(Choice       i) -> Some i | _  -> raise(Exception "unexpected type")





type InterimTypeKind =
    |InterimInteger           of Integer option
    |InterimReal              of Real option
    |InterimIA5String         of StringType option
    |InterimNumericString     of StringType option
    |InterimOctetString       of OctetString option
    |InterimNullType          of NullType option
    |InterimBitString         of BitString option
    |InterimBoolean           of Boolean option
    |InterimEnumerated        of (Enumerated option) * (EnumItem list) * bool
    |InterimSequenceOf        of (SequenceOf option) * Asn1Type
    |InterimSequence          of (Sequence option) * (ChildInfo list)
    |InterimChoice            of (Choice option) * (ChildInfo list)

//let combineStates (states:State list)= 
//    states |> List.fold (fun s c -> {State.anonymousTypes = s.anonymousTypes@c.anonymousTypes; anonymousValues = s.anonymousValues@c.anonymousValues}) {State.anonymousTypes =[]; anonymousValues = []}

let addValue (s:State) (v:Asn1GenericValue)=
    {s with anonymousValues=s.anonymousValues@[v]}

let smap = GenericFold2.foldMap



let createAstRoot (s:State) (sr:Ast.AstRoot) (dfiles: Asn1File list)  (*(acn:AcnTypes.AcnAst)*) =
    {
        AstRoot.Files = dfiles 
        args = sr.args
        TypeAssignments = s.anonymousTypes |> List.filter (fun x -> x.asn1Name.IsSome)
        ValueAssignments = s.anonymousValues 
        valsMap = 
            let aa = s.anonymousValues |> List.map(fun v -> v.id, v) 
            aa |> Seq.groupBy(fun (id,t) -> id) |> Seq.filter(fun (id, gr) -> gr |> (*Seq.distinct |>*) Seq.length > 1) |> Seq.iter (fun x -> printfn "%A" x)
            aa |> Map.ofList
        typesMap = 
            let aa = s.anonymousTypes |> List.map(fun v -> v.id, v)
            aa |> Seq.groupBy(fun (id,t) -> id) |> Seq.filter(fun (id, gr) -> gr |> (*Seq.distinct |>*) Seq.length > 1) |> Seq.iter (fun x -> printfn "%A" x)
            aa |> Map.ofList
    }

let createAsn1File (s:State) (r:Ast.AstRoot) (f:Ast.Asn1File) (newMods:Asn1Module list)  = 
    {
        Asn1File.FileName = f.FileName;
        Tokens = f.Tokens
        Modules  = newMods 
    },s

let createAsn1Module (s:State) (r:Ast.AstRoot) (f:Ast.Asn1File) (m:Ast.Asn1Module) (newtases: Asn1Type list ) (vases:Asn1GenericValue list ) = 
    {
        Asn1Module.Name = m.Name.Value
        Imports = m.Imports
        Exports = m.Exports
        Comments = m.Comments
    }, s


let createTypeAssignment (s:State) (r:Ast.AstRoot) (f:Ast.Asn1File) (m:Ast.Asn1Module) (tas:Ast.TypeAssignment) (newType:Asn1Type) = 
    newType,s

let createValueAssignment (s:State) (r:Ast.AstRoot) (f:Ast.Asn1File) (m:Ast.Asn1Module) (vas:Ast.ValueAssignment) (t:Asn1Type) (v:Asn1GenericValue) = 
    v, s

     

let createChildInfo (st:State) s (ch:Ast.ChildInfo) (newType:Asn1Type) (newOptionality:Asn1Optionality option) = 
    {
        ChildInfo.Name = ch.Name.Value
        chType = newType
        Optionality = newOptionality
        Comments = ch.Comments |> Seq.toList
        acnInsertetField    = false
        Location = ch.Name.Location

    }, st

let createChoiceChildInfo (st:State) s (ch:Ast.ChildInfo) (newType:Asn1Type) = 
    {
        ChildInfo.Name = ch.Name.Value
        chType = newType
        Optionality = None
        Comments = ch.Comments |> Seq.toList
        acnInsertetField    = false
        Location = ch.Name.Location
    }, st


let createType (s:State) (ts:GenericFold2.UserDefinedTypeScope) (oldType:Ast.Asn1Type) (newCons:((Asn1AnyConstraint) option ) list, fromWithComps:((Asn1AnyConstraint) option ) list)  (newKind:InterimTypeKind) integerSizeInBytes =
    let newTypeId = ReferenceToType ts 
    match tryGetType s newTypeId with
    | Some t    -> t,s
    | None      ->
        let numericStringDefaultConstraint = 
            let zeroToNine = Constraints.RangeContraint (('0',Literal), ('9',Literal),true,true)
            let space      = Constraints.RangeSingleValueConstraint (" ", Literal)
            let numericCon = Constraints.AlphabetContraint (Constraints.RangeUnionConstraint (zeroToNine, space, true))
            numericCon
        let rec inheritedCons f1 f2 par = 
            match par with
            | None      -> []
            | Some p    -> (f1 p)@(inheritedCons f1 f2 (f2 p))
        let getSizeMinAndMaxValue sizeUperRange =
            match sizeUperRange with
            | Concrete(a,b) -> int a, int b
            | _             -> raise(SemanticError(oldType.Location,"Declared type may have infinite size. Use size constraints to limit the upper bound"))
        let getRequiredBitsForIntUperEncoding  uperRange =
            match uperRange with
            | Concrete(a,b)                   -> int32 (GetNumberOfBitsForNonNegativeInteger(b-a)), int32 (GetNumberOfBitsForNonNegativeInteger(b-a))
            | Full | PosInf(_) |  NegInf(_)   -> 8, (integerSizeInBytes+1)*8
        let getSizeableTypeSize a b internalSize =
            let lenSize (a:int) (b:int) = int32 (GetNumberOfBitsForNonNegativeInteger(BigInteger(b)-BigInteger(a)))
            match a with
            | _ when a=b  && b<65536 -> a*internalSize                , b*internalSize
            | _ when a<>b && b<65536 -> a*internalSize + (lenSize a b), b*internalSize + (lenSize a b)
            | _                      -> a*internalSize + (lenSize a b), b*internalSize + (b / 65536 + 3) * 8
        let tasInfo = newTypeId.Asn1TypeName |> Option.map(fun x -> {TypeAssignmentInfo.modName = x.moduName; tasName = x.tasName})
        let ret = 
            match newKind with
            | InterimInteger baseType -> 
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getIntegerTypeConstraint 
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getIntegerTypeConstraint 
                let inhCons  = inheritedCons (fun (x:Integer) -> x.cons) (fun x -> x.baseType) baseType
                let uperCons, rootCons = cons@inhCons |> List.split(fun c -> match c with RangeRootConstraint _  | RangeRootConstraint2 _ -> false | _ -> true)
                let uperR    = getIntTypeConstraintUperRange uperCons  oldType.Location
                let uperMinSizeInBits, uperMaxSizeInBits = 
                    match rootCons with
                    | []  -> getRequiredBitsForIntUperEncoding uperR
                    | _   -> 
                        let mn,mx = getRequiredBitsForIntUperEncoding uperR
                        1 + mn, 1 + mx
                Integer      {Integer.baseType = baseType; cons = cons; withcons = withcons; uperRange = uperR; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits}
            | InterimReal      baseType                  -> 
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getRealTypeConstraint 
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getRealTypeConstraint 
                let inhCons  = inheritedCons (fun (x:Real) -> x.cons) (fun x -> x.baseType) baseType
                let uperR    = getRealTypeConstraintUperRange (cons@inhCons) oldType.Location
                Real         {Real.baseType = baseType; cons = cons; withcons = withcons; uperRange = uperR; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=(5+integerSizeInBytes)*8; uperMinSizeInBits=8}
            | InterimIA5String     baseType              -> 
                let defaultCharSet = [|for i in 0..127 -> System.Convert.ToChar(i) |]
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getIA5StringConstraint
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getIA5StringConstraint
                let inhCons  = inheritedCons (fun (x:StringType) -> x.cons) (fun x -> x.baseType) baseType
                let sizeUperRange = getSrtingSizeUperRange (cons@inhCons) oldType.Location
                let charSet = getSrtingAlphaUperRange (cons@inhCons) defaultCharSet oldType.Location
                let minSize, maxSize = getSizeMinAndMaxValue sizeUperRange
                let charSize =  int (GetNumberOfBitsForNonNegativeInteger (BigInteger (charSet.Length-1)))
                let uperMinSizeInBits, uperMaxSizeInBits = getSizeableTypeSize minSize maxSize charSize


                IA5String    {StringType.baseType=baseType; cons = cons; withcons = withcons; minSize=minSize; maxSize=maxSize; charSet=charSet; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits}
            | InterimNumericString baseType              -> 
                let defaultCharSet = [| ' ';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|]
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getIA5StringConstraint
                let cons = match baseType with None -> numericStringDefaultConstraint::cons | Some _ -> cons
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getIA5StringConstraint
                let inhCons  = inheritedCons (fun (x:StringType) -> x.cons) (fun x -> x.baseType) baseType
                let sizeUperRange = getSrtingSizeUperRange (cons@inhCons) oldType.Location
                let charSet = getSrtingAlphaUperRange (cons@inhCons) defaultCharSet oldType.Location
                let minSize, maxSize = getSizeMinAndMaxValue sizeUperRange
                let charSize =  int (GetNumberOfBitsForNonNegativeInteger (BigInteger (charSet.Length-1)))
                let uperMinSizeInBits, uperMaxSizeInBits = getSizeableTypeSize minSize maxSize charSize

                IA5String    {StringType.baseType=baseType; cons = cons; withcons = withcons; minSize=minSize; maxSize=maxSize; charSet=charSet; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits}
            | InterimOctetString   baseType              -> 
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getOctetStringConstraint
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getOctetStringConstraint
                let inhCons  = inheritedCons (fun (x:OctetString) -> x.cons) (fun x -> x.baseType) baseType
                let sizeUperRange = getOctetStringUperRange (cons@inhCons) oldType.Location
                let minSize, maxSize = getSizeMinAndMaxValue sizeUperRange
                let uperMinSizeInBits, uperMaxSizeInBits = getSizeableTypeSize minSize maxSize 8
                OctetString  {OctetString.baseType=baseType; cons = cons; withcons = withcons; minSize=minSize; maxSize=maxSize; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits}
            | InterimNullType     baseType              -> NullType {NullType.baseType=baseType; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=0; uperMinSizeInBits=0}
            | InterimBitString    baseType               -> 
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getBitStringConstraint 
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getBitStringConstraint
                let inhCons  = inheritedCons (fun (x:BitString) -> x.cons) (fun x -> x.baseType) baseType
                let sizeUperRange = getBitStringUperRange (cons@inhCons) oldType.Location
                let minSize, maxSize = getSizeMinAndMaxValue sizeUperRange
                let uperMinSizeInBits, uperMaxSizeInBits = getSizeableTypeSize minSize maxSize 1
                BitString    {BitString.baseType=baseType; cons = cons; withcons = withcons; minSize=minSize; maxSize=maxSize; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits}
            | InterimBoolean      baseType               -> 
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getBoolConstraint
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getBoolConstraint
                Boolean    {Boolean.baseType=baseType; cons=cons; withcons = withcons; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=1; uperMinSizeInBits=1}
            | InterimEnumerated   (baseType, items, userDefinedValues) -> 
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getEnumConstraint
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getEnumConstraint
                let uperSizeInBits = int32(GetNumberOfBitsForNonNegativeInteger(BigInteger((Seq.length items) - 1)))
                Enumerated  {Enumerated.baseType=baseType; items=items;userDefinedValues=userDefinedValues; cons = cons; withcons = withcons;  Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=uperSizeInBits; uperMinSizeInBits=uperSizeInBits}
            | InterimSequenceOf    (baseType,childType)        -> 
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getSequenceOfConstraint
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getSequenceOfConstraint
                let inhCons  = inheritedCons (fun (x:SequenceOf) -> x.cons) (fun x -> x.baseType) baseType
                let sizeUperRange = getSequenceOfUperRange (cons@inhCons) oldType.Location
                let minSize, maxSize = getSizeMinAndMaxValue sizeUperRange
                let uperMinSizeInBits, _ = getSizeableTypeSize minSize maxSize childType.uperMinSizeInBits
                let _, uperMaxSizeInBits = getSizeableTypeSize minSize maxSize childType.uperMaxSizeInBits
                SequenceOf  {SequenceOf.baseType=baseType; childType=childType; cons=cons; withcons = withcons; minSize=minSize; maxSize=maxSize; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits}
            | InterimSequence      (baseType,children)             -> 
                let optionalChildren = children |> Seq.filter(fun c -> c.Optionality.IsSome)
                let bitMaskSize = Seq.length optionalChildren
                let maxChildrenSize = children |> List.map(fun x -> x.chType.uperMaxSizeInBits) |> Seq.sum
                let minChildrenSize = children |> List.filter(fun x -> x.Optionality.IsNone) |> List.map(fun x -> x.chType.uperMinSizeInBits) |> Seq.sum
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getSequenceConstraint 
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getSequenceConstraint
                Sequence    {Sequence.baseType = baseType; children=children; cons=cons; withcons = withcons;  Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=bitMaskSize+maxChildrenSize; uperMinSizeInBits=bitMaskSize+minChildrenSize }
            | InterimChoice        (baseType,children)             -> 
                let indexSize = int (GetNumberOfBitsForNonNegativeInteger(BigInteger(Seq.length children)))
                let minChildSize = children  |> List.map(fun x -> x.chType.uperMinSizeInBits) |> Seq.min
                let maxChildSize = children  |> List.map(fun x -> x.chType.uperMaxSizeInBits) |> Seq.max
                let cons     = newCons       |> List.choose id |> List.map ConstraintsMapping.getChoiceConstraint
                let withcons = fromWithComps |> List.choose id |> List.map ConstraintsMapping.getChoiceConstraint
                Choice      {Choice.baseType = baseType; children=children; cons=cons; withcons = withcons; Location=oldType.Location; id=newTypeId; tasInfo= tasInfo; uperMaxSizeInBits=indexSize+maxChildSize; uperMinSizeInBits=indexSize+minChildSize }

        ret, {s with anonymousTypes = s.anonymousTypes@[ret]}

let createValue (us:State) (asn1ValName:(StringLoc*StringLoc) option) (ts:GenericFold2.UserDefinedTypeScope) (vs:GenericFold2.UserDefinedVarScope) (v:Asn1GenericValue) =
    (*let ret = 
        {
            Asn1GenericValue.asn1Name = asn1ValName 
            id = ReferenceToValue (ts, vs)
            //baseValue = baseValue
            refToType = ReferenceToType ts
            Kind = kind
        }
    ret, {us with anonymousValues=us.anonymousValues@[ret]}
    *)
    v, {us with anonymousValues=us.anonymousValues@[v]}


let Asn1typeToInterimType (t:Asn1Type) =
    match t with
    | Integer      t     ->  InterimInteger     t.baseType   
    | Real         t     ->  InterimReal        t.baseType    
    | IA5String    t     ->  InterimIA5String   t.baseType
    | OctetString  t     ->  InterimOctetString t.baseType
    | NullType     t     ->  InterimNullType    t.baseType
    | BitString    t     ->  InterimBitString   t.baseType
    | Boolean      t     ->  InterimBoolean     t.baseType
    | Enumerated   t     ->  InterimEnumerated  (t.baseType, t.items, t.userDefinedValues)
    | SequenceOf   t     ->  InterimSequenceOf  (t.baseType, t.childType)
    | Sequence     t     ->  InterimSequence    (t.baseType, t.children)
    | Choice       t     ->  InterimChoice      (t.baseType, t.children)


let createValidationAst (lang:ProgrammingLanguage) (app:Ast.AstRoot) (*(acn:AcnTypes.AcnAst)*) =
    let l_aux (asn1ValName: (StringLoc*StringLoc) option) = 
        match asn1ValName with
        | None          -> Literal
        | Some (md,vs)  ->  ReferenceToAsn1NamedValue  {Asn1ValueName.moduName=md.Value; vasName=vs.Value}
    let s_aux v us = 
        v, {us with anonymousValues=us.anonymousValues@[v]}
    GenericFold2.foldAstRoot
        //1. rootFunc r files
        (fun s sr dfiles  -> createAstRoot s sr dfiles (*acn*) )

        //2. fileFunc r f modules
        createAsn1File

        //3. modFunc r f m tases vases
        createAsn1Module

        //4. tasFunc r f m tas asn1Type
        createTypeAssignment
        
        //5. vasFunc r f m vas asn1Type asn1Value
        createValueAssignment

        //6. typeFunc s t newTypeKind baseTypeId (newCons,fromWithComps)
        (fun ustate s t newTypeKind (newCons,fromWithComps) -> 
            createType ustate s t (newCons,fromWithComps)  newTypeKind app.args.integerSizeInBytes)

        //7. refTypeFunc s mdName tasName tabularized 
        (*
        (fun ustate  mdName tasName tabularized newBaseType -> 
            let retKind = 
                match newBaseType with
                | Integer      ti   -> InterimInteger      (Some ti)
                | Real         ti   -> InterimReal         (Some ti)
                | IA5String    ti   -> InterimIA5String    (Some ti)
                | OctetString  ti   -> InterimOctetString  (Some ti)
                | NullType     ti   -> InterimNullType     (Some ti) 
                | BitString    ti   -> InterimBitString    (Some ti)
                | Boolean      ti   -> InterimBoolean      (Some ti)
                | Enumerated   ti   -> InterimEnumerated   (Some ti, ti.items, ti.userDefinedValues)
                | SequenceOf   ti   -> InterimSequenceOf   (Some ti, ti.childType)
                | Sequence     ti   -> InterimSequence     (Some ti, ti.children)
                | Choice       ti   -> InterimChoice       (Some ti, ti.children)
            retKind, ustate)
         *)

        //8 integerFunc s 
        (fun newBaseType ustate  ->  InterimInteger (getAsInteger newBaseType),ustate)

        //9 realFunc s 
        (fun newBaseType ustate  -> InterimReal (getAsReal newBaseType), ustate)

        //10 ia5StringFunc s 
        (fun newBaseType ustate  -> InterimIA5String (getAsIA5String newBaseType), ustate)

        //11 numericStringFunc s
        (fun newBaseType ustate  -> InterimNumericString (getAsIA5String newBaseType), ustate)

        //12 octetStringFunc
        (fun newBaseType ustate -> InterimOctetString (getAsOctetString newBaseType), ustate)

        //13 nullTypeFunc
        (fun newBaseType ustate -> InterimNullType (getAsNullType newBaseType), ustate)

        //14 bitStringFunc
        (fun newBaseType ustate -> InterimBitString (getAsBitString newBaseType), ustate)

        //15 booleanFunc
        (fun newBaseType ustate -> InterimBoolean (getAsBoolean newBaseType), ustate)

        //16 enumeratedFunc 
        (fun ustate newBaseType (enmItems : Ast.NamedItem list)-> 
            let newEnmItems, userDefinedValues = 
                match enmItems |> Seq.exists (fun nm -> nm._value.IsSome) with
                | false ->
                    enmItems |> List.mapi(fun i x -> {EnumItem.name = x.Name.Value; c_name = x.c_name; ada_name = x.ada_name;  Value = BigInteger i; comments = x.Comments|> Seq.toList} ), false
                | true  ->
                    let withVals = Ast.allocatedValuesToAllEnumItems enmItems app 
                    withVals |> List.mapi(fun i x -> {EnumItem.name = x.Name.Value; c_name = x.c_name; ada_name = x.ada_name;  Value = BigInteger i; comments = x.Comments|> Seq.toList} ), true
            InterimEnumerated ((getAsEnumerated newBaseType), newEnmItems, userDefinedValues), ustate)

        //17 enmItemFunc
        (fun ustate ni newVal -> 0, ustate)

        //18 seqOfTypeFunc 
        (fun ustate newInnerType newBaseType -> InterimSequenceOf ((getAsSequenceOf newBaseType), newInnerType), ustate)

        //19 seqTypeFunc 
        (fun ustate newChildren newBaseType ->  InterimSequence ((getAsSequence newBaseType), newChildren) , ustate)

        //20 chTypeFunc 
        (fun ustate newChildren newBaseType -> InterimChoice ((getAsChoice newBaseType), newChildren), ustate)

        //21 sequenceChildFunc 
        createChildInfo

        //22 alwaysAbsentFunc
        (fun s -> Asn1Optionality.AlwaysAbsent)

        //23 alwaysPresentFunc
        (fun s -> Asn1Optionality.AlwaysPresent)

        //24 optionalFunc
        (fun s -> Asn1Optionality.Optional)

        //25 defaultFunc
        (fun s (newValue) -> 
            Asn1Optionality.Default newValue)

        //26 choiceChildFunc
        createChoiceChildInfo

        //27 refValueFunc 
        (fun _ -> 0) 

        //28 enumValueFunc
        (fun us asn1ValName ts vs v nmItem childValue -> 
            let ret = EnumValue {EnumValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=nmItem.Value; childValue=childValue}
            s_aux ret us)

        //29 intValFunc
        (fun us asn1ValName ts vs v bi childValue -> 
            let ret = IntegerValue {IntegerValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=bi.Value; childValue=childValue}
            s_aux ret us)

        //30 realValFunc
        (fun us asn1ValName ts vs v bi childValue -> 
            let ret = RealValue {RealValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=bi.Value; childValue=childValue}
            s_aux ret us)

        //31 ia5StringValFunc 
        (fun us asn1ValName ts vs v str childValue-> 
            let ret = StringValue {StringValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=str.Value; childValue=childValue}
            s_aux ret us)

        //32 numStringValFunc 
        (fun us asn1ValName ts vs v str childValue-> 
            let ret = StringValue {StringValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=str.Value; childValue=childValue}
            s_aux ret us)

        //33 boolValFunc 
        (fun us asn1ValName ts vs v b childValue -> 
            let ret = BooleanValue {BooleanValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=b.Value; childValue=childValue}
            s_aux ret us)

        //34 octetStringValueFunc
        (fun us asn1ValName ts vs v bytes childValue-> 
            let ret = OctetStringValue {OctetStringValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=bytes |> List.map(fun b->b.Value); childValue=childValue}
            s_aux ret us)
            //createValue us asn1ValName ts vs  (OctetStringValue (bytes |> List.map(fun b->b.Value))) )

        //35 bitStringValueFunc
        (fun us asn1ValName ts vs v b childValue -> 
            let ret = BitStringValue {BitStringValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=b.Value; childValue=childValue}
            s_aux ret us)

        //36 nullValueFunc
        (fun us asn1ValName  ts vs v  childValue -> 
            let ret = NullValue {NullValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=(); childValue=childValue}
            s_aux ret us)

        //37 seqOfValueFunc s t v actType  newVals
        (fun us asn1ValName ts vs v  newVals childValue -> 
            let ret = SeqOfValue {SeqOfValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=newVals; childValue=childValue}
            s_aux ret us)

        //38 seqValueFunc 
        (fun us asn1ValName ts vs v  newVals childValue -> 
            let newVals = newVals |> List.map (fun (nm, (v)) -> {NamedValue.name=nm.Value;Value=v})
            let ret = SeqValue {SeqValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value=newVals; childValue=childValue}
            s_aux ret us)

        //39 chValueFunc s t v actType name newValue
        (fun us asn1ValName ts vs v name newChValue childValue -> 
            let ret = ChValue {ChValue.litOrRef=l_aux asn1ValName; id=ReferenceToValue (ts, vs); refToType=ReferenceToType ts; Value={NamedValue.name=name.Value;Value=newChValue}; childValue=childValue}
            s_aux ret us)

        //40 singleValueContraintFunc s t checContent actType newValue
        (fun us ts t checContent newValue-> 
            Some (AnySingleValueContraint newValue), us)

        //41 rangeContraintFunc s t checContent actType newValue1 newValue2 b1 b2
        (fun us ts t checContent (newValue1) (newValue2) b1 b2 -> 
            Some (AnyRangeContraint(newValue1, newValue2, b1, b2)), us)

        //42 rangeContraint_val_MAXFunc s t checContent actType newValue  b
        (fun us ts t checContent (newValue) b -> 
            Some (AnyRangeContraint_val_MAX (newValue,b)), us)

        //43 rangeContraint_MIN_valFunc s t checContent actType newValue  b
        (fun us ts t checContent (newValue) b  -> 
            Some (AnyRangeContraint_MIN_val(newValue,b)), us )

        //44 rangeContraint_MIN_MAXFunc  s t checContent actType
        (fun us ts t checContent -> None, us)

        //45 typeInclConstraintFunc s t actType (md,tas)
        (fun us ts t otherCon -> 
            match otherCon with
            | Some x -> x, us
            | None    -> None, us
         )

        //46 unionConstraintFunc s t actType nc1 nc2
        (fun us ts t x1 x2 virtualCon -> 
            match x1, x2 with
            | Some (nc1), Some (nc2)  -> Some (AnyUnionConstraint (nc1, nc2, virtualCon)), us
            | Some (nc1), None        -> None, us
            | None, Some (nc2)        -> None, us
            | None, None              -> None, us)

        //47 intersectionConstraintFunc s t actType nc1 nc2
        (fun us ts t x1 x2 -> 
            match x1, x2 with
            | Some (nc1), Some (nc2)  -> Some (AnyIntersectionConstraint (nc1,nc2)), us
            | Some (nc1), None        -> Some (nc1), us
            | None, Some (nc2)        -> Some (nc2), us
            | None, None              -> None, us)

        //48 allExceptConstraintFunc s t actType nc
        (fun  us ts t x1 -> 
            match x1 with
            | Some (nc1)   -> Some (AnyAllExceptConstraint nc1), us
            | None            -> raise(SemanticError(t.Location, (sprintf "EXCEPT constraint makes type to allow no values. Consider changing the EXCET part"))) )

        //49 exceptConstraintFunc s t actType nc1 nc2
        (fun us ts t x1 x2 -> 
            match x1, x2 with
            | Some (nc1), Some (nc2)  -> Some (AnyExceptConstraint(nc1,nc2)), us
            | Some (nc1), None           -> raise(SemanticError(t.Location, (sprintf "EXCEPT constraint makes type to allow no values. Consider changing the EXCET part")))
            | None, Some (nc2)           -> Some (nc2), us
            | None, None                 -> raise(SemanticError(t.Location, (sprintf "EXCEPT constraint makes type to allow no values. Consider changing the EXCET part")))
        )

        //50 rootConstraintFunc s t actType nc
        (fun us ts t x1 -> x1 |> Option.map (fun (nc) -> AnyRootConstraint nc), us )

        //51 rootConstraint2Func s t actType nc1 nc2
        (fun us ts t x1 x2 -> 
            match x1, x2 with
            | Some (nc1), Some (nc2)     -> Some (AnyRootConstraint2 (nc1,nc2)), us
            | Some (nc1), None           -> None, us
            | None, Some (nc2)           -> None, us
            | None, None                 -> None, us)
        
        //52 sizeContraint 
        (fun us ts t x1 -> x1 |> Option.map (fun (nc) -> AnySizeContraint nc), us )
        
        //53 alphabetContraint
        (fun us ts t x1 -> x1 |> Option.map (fun (nc) -> AnyAlphabetContraint nc), us )

        //54 withComponentConstraint ts t
        (fun us ts t -> None, us)

        //55 withComponentConstraints
        (fun us ts t  -> None, us)

        //56 globalIntType
        (fun _ -> [], InterimInteger None)

        //57 getSequenceOfTypeChild
        (fun us newTypeKind -> 
            match newTypeKind with
            | InterimSequenceOf  (_,sq)   -> 
                //let retType = us.anonymousTypes |> Seq.find(fun at -> at.id = sq.childTypeRef)
                let (ReferenceToType referenceToType) = sq.id
                (referenceToType, Asn1typeToInterimType sq) 
            | _                             -> raise(BugErrorException(sprintf "Expecting SequenceOf, found %A" newTypeKind)))

        //58 getChoiceTypeChild
        (fun us newTypeKind chName ->
            match newTypeKind with
            | InterimChoice  (_,children)   -> 
                match children |> List.tryFind (fun c -> c.Name = chName.Value) with
                | Some ch       -> 
                    let (ReferenceToType referenceToType) = ch.chType.id
                    //let retType = us.anonymousTypes |> Seq.find(fun at -> at.id = (ReferenceToType referenceToType))
                    (referenceToType, Asn1typeToInterimType ch.chType) 
                | None          -> raise(SemanticError(chName.Location, (sprintf "CHOICE type has no alternative with name '%s'" chName.Value)))
            | _                 -> raise(BugErrorException(sprintf "Expecting Choice, found %A" newTypeKind)) )
        

        //59 getSequenceTypeChild
        (fun us newTypeKind chName ->
            match newTypeKind with
            | InterimSequence  (_,children)   -> 
                match children |> List.tryFind (fun c -> c.Name = chName.Value) with
                | Some ch   -> 
                    let (ReferenceToType referenceToType) = ch.chType.id
                    //let retType = us.anonymousTypes |> Seq.find(fun at -> at.id = (ReferenceToType referenceToType))
                    (referenceToType, Asn1typeToInterimType ch.chType)  
                | None          -> raise(SemanticError(chName.Location, (sprintf "SEQUENCE type has no alternative with name '%s'" chName.Value)))
            | _                 -> raise(BugErrorException(sprintf "Expecting SEQUENCE, found %A" newTypeKind)) )

        //60 getTypeKind
        (fun newT -> Asn1typeToInterimType newT)

        (fun us s -> 
            let newTypeId = ReferenceToType s 
            match tryGetType us newTypeId with
            | Some t    -> Some(t,us)
            | None      -> None  )


        app {State.anonymousTypes =[]; anonymousValues = []}