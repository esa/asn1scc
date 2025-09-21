module DAstUtilFunctions
open System
open System.Numerics
open FsUtils
open CommonTypes

open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open Language
open System.Xml.Serialization
open System.IO
//open Newtonsoft.Json

let getAccessFromScopeNodeList (ReferenceToType nodes)  (childTypeIsString: bool) (lm:LanguageMacros) (pVal : CodegenScope) =
    let handleNode zeroBasedSequenceOfLevel (pVal : CodegenScope) (n:ScopeNode) (childTypeIsString: bool) =
        match n with
        | MD _
        | TA _
        | PRM _
        | VA _              -> raise(BugErrorException "getAccessFromScopeNodeList")
        | SEQ_CHILD (chName, chOpt)  ->
            let isPresent =
                match chOpt with
                | true ->  [lm.lg.getSeqChildIsPresent pVal.accessPath (ToC chName)] //[sprintf "%s%sexist.%s" pVal.arg.p (lm.lg.getAccess pVal.arg) chName]
                | false -> []
            isPresent, {pVal with accessPath = lm.lg.getSeqChild pVal.accessPath (ToC chName) childTypeIsString chOpt}
        | CH_CHILD (chName, pre_name, chParent)  ->
            let chChildIsPresent = lm.lg.getChChildIsPresent pVal.accessPath chParent pre_name
            [chChildIsPresent], {pVal with accessPath = lm.lg.getChChild pVal.accessPath (ToC chName) childTypeIsString}
        | SQF               ->
            let curIdx = sprintf "i%d" (zeroBasedSequenceOfLevel + 1)
            [], {pVal with accessPath = lm.lg.getArrayItem pVal.accessPath curIdx childTypeIsString}

    match nodes with
    | (MD md)::(TA tas)::(PRM prm)::[]  -> ({CodegenScope.modName = pVal.modName; accessPath = AccessPath.valueEmptyPath (ToC (md + "_" + tas + "_" + prm))}, [])
    | (MD md)::(TA tas):: xs            ->
        let length = Seq.length xs
        let ret =
            xs |>
            List.fold(fun (curPath, curCheckExp, zeroBasedSequenceOfLevel, idx) n ->
                let checkPath, newPath = handleNode zeroBasedSequenceOfLevel curPath n (childTypeIsString && idx=length)
                let zeroBasedSequenceOfLevel = match n with SQF -> zeroBasedSequenceOfLevel + 1 | _ -> zeroBasedSequenceOfLevel
                (newPath, checkPath@curCheckExp, zeroBasedSequenceOfLevel, idx+1)) (pVal,[], 0, 1) |> (fun (a,checkPath,_,_) -> a, checkPath)
        ret
    | _                                 -> raise(BugErrorException "getAccessFromScopeNodeList")


let rec extractDefaultInitValue (childType: Asn1TypeKind): String =
    match childType with
    | Integer i -> i.baseInfo.defaultInitVal
    | Real r -> r.baseInfo.defaultInitVal
    | NullType n -> n.baseInfo.defaultInitVal
    | Boolean b -> b.baseInfo.defaultInitVal
    | ReferenceType rt -> extractDefaultInitValue rt.resolvedType.Kind
    | _ -> "null"

let rec resolveReferenceType(t: Asn1TypeKind): Asn1TypeKind =
    match t with
    | ReferenceType rt -> resolveReferenceType rt.resolvedType.Kind
    | _ -> t

let isJVMPrimitive (t: Asn1TypeKind) =
    match resolveReferenceType t with
    | Integer _ | Real _ | NullType _ | Boolean _ -> true
    | _ -> false

let defOrRef (r:Asn1AcnAst.AstRoot) (m: Asn1AcnAst.Asn1Module) (a:Asn1AcnAst.AcnReferenceToEnumerated) =
    match m.Name.Value = a.modName.Value with
    | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName.Value) ; definedInRtl = false}
    | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName.Value); typedefName = ToC (r.args.TypePrefix + a.tasName.Value); definedInRtl = false}

let defOrRef2 (r:Asn1AcnAst.AstRoot) (m: Asn1AcnAst.Asn1Module) (a:Asn1AcnAst.ReferenceToEnumerated) =
    match m.Name.Value = a.modName with
    | true  -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = None; typedefName = ToC (r.args.TypePrefix + a.tasName); definedInRtl = false}
    | false -> ReferenceToExistingDefinition {ReferenceToExistingDefinition.programUnit = Some (ToC a.modName); typedefName = ToC (r.args.TypePrefix + a.tasName) ; definedInRtl = false}


let hasInitMethSuffix (initMethName: string) (suffix: string): bool =
    initMethName.EndsWith(suffix)

let isArrayInitialiser(initMethName: string): bool =
    initMethName.Contains("Array.fill(")

type LocalVariable with
    member this.VarName =
        match this with
        | SequenceOfIndex (i,_)   -> sprintf "i%d" i
        | IntegerLocalVariable(name,_)    -> name
        | Asn1SIntLocalVariable(name,_)   -> name
        | Asn1UIntLocalVariable(name,_)   -> name
        | FlagLocalVariable(name,_)       -> name
        | AcnInsertedChild(name,_,_)      -> name
        | BooleanLocalVariable(name,_)    -> name
        | GenericLocalVariable lv         -> lv.name

type TypeDefinitionOrReference with

    member this.longTypedefName2 bHasModules =
        match this with
        | TypeDefinition  td ->
            td.typedefName
        | ReferenceToExistingDefinition ref ->
            match ref.programUnit with
            | Some pu ->
                match bHasModules with
                | true   -> pu + "." + ref.typedefName
                | false     -> ref.typedefName
            | None    -> ref.typedefName

    member this.getAsn1Name (typePrefix : string) =
        let typedefName =
            match this with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref -> ref.typedefName
        let idx = typedefName.IndexOf typePrefix
        match idx < 0 with
        | true      -> typedefName.Replace("_","-")
        | false     -> typedefName.Remove(idx, typePrefix.Length).Replace("_","-")

type Integer with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons
    //member this.IsUnsigned = isUnsigned this.uperRange

type Enumerated with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type Real with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type Boolean with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type StringType with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type OctetString with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type BitString with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type SequenceOf with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type ObjectIdentifier with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons


type Sequence with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons
    member this.Asn1Children =
        this.children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)

type Choice with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons


type ReferenceType with
    member ref.AsTypeAssignmentInfo =  {TypeAssignmentInfo.modName = ref.baseInfo.modName.Value; tasName = ref.baseInfo.tasName.Value}

type TypeAssignment with
    member ref.AsTypeAssignmentInfo modName=  {TypeAssignmentInfo.modName = modName; tasName = ref.Name.Value}

type Asn1AcnAst.ChChildInfo with
    member this.presentWhenName = (ToC this.present_when_name) + "_PRESENT"

type ChChildInfo with
    member this.presentWhenName (defOrRef:TypeDefinitionOrReference option) l =
        match l with
        | C     -> (ToC this._present_when_name_private) + "_PRESENT"
        | Scala -> (ToC this._present_when_name_private) + "_PRESENT" // TODO: Scala
        | Ada   ->
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + ((ToC this._present_when_name_private) + "_PRESENT")
            | _       -> (ToC this._present_when_name_private) + "_PRESENT"


type Asn1AcnAst.NamedItem      with
    member this.CEnumName l =
        match l with
        | C     -> this.c_name
        | Scala -> this.scala_name
        | Ada   -> this.ada_name


type Asn1AcnAst.Asn1Type with
    member this.getParameterExpr (suf:string) (c:Codec) =
        match this.Kind with
        | Asn1AcnAst.Integer         _ -> TC_ReferenceToVariable(TC_INTEGER, "val" + suf)
        | Asn1AcnAst.Real            _ -> TC_ReferenceToVariable(TC_REAL, "val" + suf)
        | Asn1AcnAst.IA5String       _ -> TC_ReferenceToVariable(TC_STRING, "val" + suf)
        | Asn1AcnAst.NumericString   _ -> TC_ReferenceToVariable(TC_STRING, "val" + suf)
        | Asn1AcnAst.OctetString     _ -> TC_ReferenceToVariable(TC_COMPLEX, "val" + suf)
        | Asn1AcnAst.NullType        _ -> TC_ReferenceToVariable(TC_INTEGER, "val" + suf)
        | Asn1AcnAst.BitString       _ -> TC_ReferenceToVariable(TC_COMPLEX, "val" + suf)
        | Asn1AcnAst.Boolean         _ -> TC_ReferenceToVariable(TC_BOOL, "val" + suf)
        | Asn1AcnAst.Enumerated      _ -> TC_ReferenceToVariable(TC_INTEGER, "val" + suf)
        | Asn1AcnAst.SequenceOf      _ -> TC_ReferenceToVariable(TC_COMPLEX, "val" + suf)
        | Asn1AcnAst.Sequence        _ -> TC_ReferenceToVariable(TC_COMPLEX, "val" + suf)
        | Asn1AcnAst.Choice          _ -> TC_ReferenceToVariable(TC_COMPLEX, "val" + suf)
        | Asn1AcnAst.ObjectIdentifier _ ->TC_ReferenceToVariable(TC_COMPLEX, "val" + suf)
        | Asn1AcnAst.TimeType _         ->TC_ReferenceToVariable(TC_COMPLEX, "val" + suf)
        | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParameterExpr suf c


type Asn1Type
with
    member this.getActualType (r:AstRoot) =
        match this.Kind with
        | ReferenceType t->
            let md =  r.Files |> List.collect(fun f -> f.Modules) |> Seq.find(fun m -> m.Name.Value = t.baseInfo.modName.Value) //t.baseInfo.modName
            let ts = md.TypeAssignments |> Seq.find(fun ts -> ts.Name.Value = t.baseInfo.tasName.Value) //t.baseInfo.modName
            ts.Type.getActualType r
        | Integer      _ -> this
        | Real         _ -> this
        | IA5String    _ -> this
        | OctetString  _ -> this
        | NullType     _ -> this
        | BitString    _ -> this
        | Boolean      _ -> this
        | Enumerated   _ -> this
        | SequenceOf   _ -> this
        | Sequence     _ -> this
        | Choice       _ -> this
        | ObjectIdentifier _ -> this
        | TimeType     _  -> this


    member this.isComplexType =
        match this.Kind with
        | ReferenceType t-> t.resolvedType.isComplexType
        | Integer      _ -> false
        | Real         _ -> false
        | IA5String    _ -> false
        | OctetString  _ -> false
        | NullType     _ -> false
        | BitString    _ -> false
        | Boolean      _ -> false
        | Enumerated   _ -> false
        | SequenceOf   _ -> true
        | Sequence     _ -> true
        | Choice       _ -> true
        | ObjectIdentifier _ -> false
        | TimeType     _  -> false

    member this.ActualType =
        match this.Kind with
        | ReferenceType t-> t.resolvedType.ActualType
        | Integer      _ -> this
        | Real         _ -> this
        | IA5String    _ -> this
        | OctetString  _ -> this
        | NullType     _ -> this
        | BitString    _ -> this
        | Boolean      _ -> this
        | Enumerated   _ -> this
        | SequenceOf   _ -> this
        | Sequence     _ -> this
        | Choice       _ -> this
        | ObjectIdentifier _ -> this
        | TimeType     _  -> this

    member this.FT_TypeDefinition =
        match this.Kind with
        | Integer      t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | Real         t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | ObjectIdentifier t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | TimeType t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | IA5String    t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_StringTypeDefinition d)) |> Map.ofList
        | OctetString  t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | NullType     t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | BitString    t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | Boolean      t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | Enumerated   t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_EnumeratedTypeDefinition d)) |> Map.ofList
        | SequenceOf   t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | Sequence     t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_SequenceTypeDefinition d)) |> Map.ofList
        | Choice       t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_ChoiceTypeDefinition d)) |> Map.ofList
        | ReferenceType t-> t.baseInfo.typeDef

    member this.printValue =
        match this.Kind with
        | Integer      t -> t.printValue
        | Real         t -> t.printValue
        | IA5String    t -> t.printValue
        | OctetString  t -> t.printValue
        | NullType     t -> t.printValue
        | BitString    t -> t.printValue
        | Boolean      t -> t.printValue
        | Enumerated   t -> t.printValue
        | SequenceOf   t -> t.printValue
        | Sequence     t -> t.printValue
        | Choice       t -> t.printValue
        | ReferenceType t-> t.printValue
        | ObjectIdentifier t -> t.printValue
        | TimeType  t       -> t.printValue


    member this.ConstraintsAsn1Str =
        match this.Kind with
        | Integer      t -> t.constraintsAsn1Str
        | Real         t -> t.constraintsAsn1Str
        | IA5String    t -> t.constraintsAsn1Str
        | OctetString  t -> t.constraintsAsn1Str
        | NullType     t -> t.constraintsAsn1Str
        | BitString    t -> t.constraintsAsn1Str
        | Boolean      t -> t.constraintsAsn1Str
        | Enumerated   t -> t.constraintsAsn1Str
        | SequenceOf   t -> t.constraintsAsn1Str
        | Sequence     t -> t.constraintsAsn1Str
        | Choice       t -> t.constraintsAsn1Str
        | ReferenceType t-> t.constraintsAsn1Str
        | ObjectIdentifier t -> t.constraintsAsn1Str
        | TimeType t        -> t.constraintsAsn1Str


    member this.initFunction =
        match this.Kind with
        | Integer      t -> t.initFunction
        | Real         t -> t.initFunction
        | IA5String    t -> t.initFunction
        | OctetString  t -> t.initFunction
        | NullType     t -> t.initFunction
        | BitString    t -> t.initFunction
        | Boolean      t -> t.initFunction
        | Enumerated   t -> t.initFunction
        | SequenceOf   t -> t.initFunction
        | Sequence     t -> t.initFunction
        | Choice       t -> t.initFunction
        | ReferenceType t-> t.initFunction
        | ObjectIdentifier t -> t.initFunction
        | TimeType t        -> t.initFunction

    member this.equalFunction =
        match this.Kind with
        | Integer      t -> t.equalFunction
        | Real         t -> t.equalFunction
        | IA5String    t -> t.equalFunction
        | OctetString  t -> t.equalFunction
        | NullType     t -> t.equalFunction
        | BitString    t -> t.equalFunction
        | Boolean      t -> t.equalFunction
        | Enumerated   t -> t.equalFunction
        | SequenceOf   t -> t.equalFunction
        | Sequence     t -> t.equalFunction
        | Choice       t -> t.equalFunction
        | ReferenceType t-> t.equalFunction
        | ObjectIdentifier t -> t.equalFunction
        | TimeType t        -> t.equalFunction

    member this.isValidFunction =
        match this.Kind with
        | Integer      t -> t.isValidFunction
        | Real         t -> t.isValidFunction
        | IA5String    t -> t.isValidFunction
        | OctetString  t -> t.isValidFunction
        | NullType     t -> None
        | BitString    t -> t.isValidFunction
        | Boolean      t -> t.isValidFunction
        | Enumerated   t -> t.isValidFunction
        | ObjectIdentifier t-> t.isValidFunction
        | SequenceOf   t -> t.isValidFunction
        | Sequence     t -> t.isValidFunction
        | Choice       t -> t.isValidFunction
        | ReferenceType t-> t.isValidFunction
        | TimeType t        -> t.isValidFunction

    member this.getUperFunction (l:CommonTypes.Codec) =
        match l with
        | CommonTypes.Encode   -> this.uperEncFunction
        | CommonTypes.Decode   -> this.uperDecFunction
    member this.getXerFunction (l:CommonTypes.Codec) =
        match l with
        | CommonTypes.Encode   -> this.xerEncFunction
        | CommonTypes.Decode   -> this.xerDecFunction

    member this.uperEncFunction =
         match this.Kind with
         | Integer      t ->t.uperEncFunction
         | Real         t ->t.uperEncFunction
         | IA5String    t ->t.uperEncFunction
         | OctetString  t ->t.uperEncFunction
         | NullType     t ->t.uperEncFunction
         | BitString    t ->t.uperEncFunction
         | Boolean      t ->t.uperEncFunction
         | Enumerated   t ->t.uperEncFunction
         | SequenceOf   t ->t.uperEncFunction
         | Sequence     t ->t.uperEncFunction
         | Choice       t ->t.uperEncFunction
         | ReferenceType t->t.uperEncFunction
         | ObjectIdentifier t -> t.uperEncFunction
         | TimeType t        -> t.uperEncFunction

    member this.uperDecFunction =
         match this.Kind with
         | Integer      t -> t.uperDecFunction
         | Real         t -> t.uperDecFunction
         | IA5String    t -> t.uperDecFunction
         | OctetString  t -> t.uperDecFunction
         | NullType     t -> t.uperDecFunction
         | BitString    t -> t.uperDecFunction
         | Boolean      t -> t.uperDecFunction
         | Enumerated   t -> t.uperDecFunction
         | SequenceOf   t -> t.uperDecFunction
         | Sequence     t -> t.uperDecFunction
         | Choice       t -> t.uperDecFunction
         | ReferenceType t-> t.uperDecFunction
         | ObjectIdentifier t -> t.uperDecFunction
         | TimeType t        -> t.uperDecFunction


    member this.xerEncFunction =
         match this.Kind with
         | Integer      t ->t.xerEncFunction
         | Real         t ->t.xerEncFunction
         | IA5String    t ->t.xerEncFunction
         | OctetString  t ->t.xerEncFunction
         | NullType     t ->t.xerEncFunction
         | BitString    t ->t.xerEncFunction
         | Boolean      t ->t.xerEncFunction
         | Enumerated   t ->t.xerEncFunction
         | SequenceOf   t ->t.xerEncFunction
         | Sequence     t ->t.xerEncFunction
         | Choice       t ->t.xerEncFunction
         | ReferenceType t->t.xerEncFunction
         | ObjectIdentifier t -> t.xerEncFunction
         | TimeType t        -> t.xerEncFunction

    member this.xerDecFunction =
         match this.Kind with
         | Integer      t -> t.xerDecFunction
         | Real         t -> t.xerDecFunction
         | IA5String    t -> t.xerDecFunction
         | OctetString  t -> t.xerDecFunction
         | NullType     t -> t.xerDecFunction
         | BitString    t -> t.xerDecFunction
         | Boolean      t -> t.xerDecFunction
         | Enumerated   t -> t.xerDecFunction
         | SequenceOf   t -> t.xerDecFunction
         | Sequence     t -> t.xerDecFunction
         | Choice       t -> t.xerDecFunction
         | ReferenceType t-> t.xerDecFunction
         | ObjectIdentifier t -> t.xerDecFunction
         | TimeType t        -> t.xerDecFunction


    member this.uperMaxSizeInBits =
        match this.Kind with
        | Integer      t -> t.baseInfo.uperMaxSizeInBits
        | Real         t -> t.baseInfo.uperMaxSizeInBits
        | IA5String    t -> t.baseInfo.uperMaxSizeInBits
        | OctetString  t -> t.baseInfo.uperMaxSizeInBits
        | NullType     t -> t.baseInfo.uperMaxSizeInBits
        | BitString    t -> t.baseInfo.uperMaxSizeInBits
        | Boolean      t -> t.baseInfo.uperMaxSizeInBits
        | Enumerated   t -> t.baseInfo.uperMaxSizeInBits
        | SequenceOf   t -> t.baseInfo.uperMaxSizeInBits
        | Sequence     t -> t.baseInfo.uperMaxSizeInBits
        | Choice       t -> t.baseInfo.uperMaxSizeInBits
        | ReferenceType ref -> ref.baseInfo.uperMaxSizeInBits
        | ObjectIdentifier t -> t.baseInfo.uperMaxSizeInBits
        | TimeType t        -> t.baseInfo.uperMaxSizeInBits

    member this.uperMinSizeInBits =
        match this.Kind with
        | Integer      t -> t.baseInfo.uperMinSizeInBits
        | Real         t -> t.baseInfo.uperMinSizeInBits
        | IA5String    t -> t.baseInfo.uperMinSizeInBits
        | OctetString  t -> t.baseInfo.uperMinSizeInBits
        | NullType     t -> t.baseInfo.uperMinSizeInBits
        | BitString    t -> t.baseInfo.uperMinSizeInBits
        | Boolean      t -> t.baseInfo.uperMinSizeInBits
        | Enumerated   t -> t.baseInfo.uperMinSizeInBits
        | SequenceOf   t -> t.baseInfo.uperMinSizeInBits
        | Sequence     t -> t.baseInfo.uperMinSizeInBits
        | Choice       t -> t.baseInfo.uperMinSizeInBits
        | ObjectIdentifier t -> t.baseInfo.uperMinSizeInBits
        | ReferenceType ref -> ref.baseInfo.uperMinSizeInBits
        | TimeType t        -> t.baseInfo.uperMinSizeInBits


    member this.acnMaxSizeInBits =
        match this.Kind with
        | Integer      t -> t.baseInfo.acnMaxSizeInBits
        | Real         t -> t.baseInfo.acnMaxSizeInBits
        | IA5String    t -> t.baseInfo.acnMaxSizeInBits
        | OctetString  t -> t.baseInfo.acnMaxSizeInBits
        | NullType     t -> t.baseInfo.acnMaxSizeInBits
        | BitString    t -> t.baseInfo.acnMaxSizeInBits
        | Boolean      t -> t.baseInfo.acnMaxSizeInBits
        | Enumerated   t -> t.baseInfo.acnMaxSizeInBits
        | SequenceOf   t -> t.baseInfo.acnMaxSizeInBits
        | Sequence     t -> t.baseInfo.acnMaxSizeInBits
        | Choice       t -> t.baseInfo.acnMaxSizeInBits
        | ObjectIdentifier t -> t.baseInfo.acnMaxSizeInBits
        | ReferenceType ref -> ref.baseInfo.acnMaxSizeInBits
        | TimeType t        -> t.baseInfo.acnMaxSizeInBits

    member this.acnMinSizeInBits =
        match this.Kind with
        | Integer      t -> t.baseInfo.acnMinSizeInBits
        | Real         t -> t.baseInfo.acnMinSizeInBits
        | IA5String    t -> t.baseInfo.acnMinSizeInBits
        | OctetString  t -> t.baseInfo.acnMinSizeInBits
        | NullType     t -> t.baseInfo.acnMinSizeInBits
        | BitString    t -> t.baseInfo.acnMinSizeInBits
        | Boolean      t -> t.baseInfo.acnMinSizeInBits
        | Enumerated   t -> t.baseInfo.acnMinSizeInBits
        | SequenceOf   t -> t.baseInfo.acnMinSizeInBits
        | Sequence     t -> t.baseInfo.acnMinSizeInBits
        | Choice       t -> t.baseInfo.acnMinSizeInBits
        | ObjectIdentifier t -> t.baseInfo.acnMinSizeInBits
        | ReferenceType ref -> ref.baseInfo.acnMinSizeInBits
        | TimeType t        -> t.baseInfo.acnMinSizeInBits

    member this.MappingFunctionsModules =
        match this.Kind with
        | Sequence     t ->
            let ret1 =
                match t.baseInfo.acnProperties.postEncodingFunction, t.baseInfo.acnProperties.preDecodingFunction with
                | Some (AcnGenericTypes.PostEncodingFunction (a, _) ), Some (AcnGenericTypes.PreDecodingFunction (c, _)) -> [a;c] |> List.choose id |> List.map(fun z -> z.Value)
                | Some (AcnGenericTypes.PostEncodingFunction (a, _) ), None -> [a] |> List.choose id |> List.map(fun z -> z.Value)
                | None, Some (AcnGenericTypes.PreDecodingFunction (c, _)) -> [c] |> List.choose id |> List.map(fun z -> z.Value)
                | None, None -> []
            let ret2 =
                t.children |>
                List.choose(fun c ->
                    match c with
                    | Asn1Child _   -> None
                    | AcnChild  ancC ->
                        match ancC.Type with
                        | AcnInteger a ->
                            match a.acnProperties.mappingFunction with
                            | Some (AcnGenericTypes.MappingFunction (a, _)) -> a
                            | None                                          -> None

                        | _            -> None)
                    |> List.map(fun z -> z.Value)
            ret1@ret2
        | Integer      t ->
            match t.baseInfo.acnProperties.mappingFunction with
            | Some (AcnGenericTypes.MappingFunction (a, _)) -> [a] |> List.choose id |> List.map(fun z -> z.Value)
            | None                                          -> []
        | Real         _ -> []
        | IA5String    _ -> []
        | OctetString  _ -> []
        | NullType     _ -> []
        | BitString    _ -> []
        | Boolean      _ -> []
        | Enumerated   _ -> []
        | SequenceOf   _ -> []
        | Choice       _ -> []
        | ObjectIdentifier _ -> []
        | ReferenceType ref -> ref.resolvedType.MappingFunctionsModules
        | TimeType _        -> []

    member this.icdTas =
        match this.Kind with
        | Integer           t -> t.acnEncFunction.icdTas
        | Real              t -> t.acnEncFunction.icdTas
        | IA5String         t -> t.acnEncFunction.icdTas
        | OctetString       t -> t.acnEncFunction.icdTas
        | NullType          t -> t.acnEncFunction.icdTas
        | BitString         t -> t.acnEncFunction.icdTas
        | Boolean           t -> t.acnEncFunction.icdTas
        | Enumerated        t -> t.acnEncFunction.icdTas
        | SequenceOf        t -> t.acnEncFunction.icdTas
        | Sequence          t -> t.acnEncFunction.icdTas
        | Choice            t -> t.acnEncFunction.icdTas
        | ReferenceType     t -> t.acnEncFunction.icdTas
        | ObjectIdentifier  t -> t.acnEncFunction.icdTas
        | TimeType          t -> t.acnEncFunction.icdTas

    member this.acnEncFunction : AcnFunction option =
        match this.Kind with
        | Integer      t -> Some (t.acnEncFunction)
        | Real         t -> Some (t.acnEncFunction)
        | IA5String    t -> Some (t.acnEncFunction)
        | OctetString  t -> Some (t.acnEncFunction)
        | NullType     t -> Some (t.acnEncFunction)
        | BitString    t -> Some (t.acnEncFunction)
        | Boolean      t -> Some (t.acnEncFunction)
        | Enumerated   t -> Some (t.acnEncFunction)
        | SequenceOf   t -> Some (t.acnEncFunction)
        | Sequence     t -> Some (t.acnEncFunction)
        | Choice       t -> Some (t.acnEncFunction)
        | ReferenceType t-> Some (t.acnEncFunction)
        | ObjectIdentifier t -> Some (t.acnEncFunction)
        | TimeType t        -> Some (t.acnEncFunction)

    member this.acnDecFunction : AcnFunction option =
        match this.Kind with
        | Integer      t -> Some (t.acnDecFunction)
        | Real         t -> Some (t.acnDecFunction)
        | IA5String    t -> Some (t.acnDecFunction)
        | OctetString  t -> Some (t.acnDecFunction)
        | NullType     t -> Some (t.acnDecFunction)
        | BitString    t -> Some (t.acnDecFunction)
        | Boolean      t -> Some (t.acnDecFunction)
        | Enumerated   t -> Some (t.acnDecFunction)
        | SequenceOf   t -> Some (t.acnDecFunction)
        | Sequence     t -> Some (t.acnDecFunction)
        | Choice       t -> Some (t.acnDecFunction)
        | ObjectIdentifier t -> Some (t.acnDecFunction)
        | ReferenceType t-> Some (t.acnDecFunction)
        | TimeType t        -> Some (t.acnDecFunction)

    member this.getAcnFunction (l:CommonTypes.Codec) =
        match l with
        | CommonTypes.Encode   -> this.acnEncFunction
        | CommonTypes.Decode   -> this.acnDecFunction

//    uperEncDecTestFunc  : EncodeDecodeTestFunc
//    acnEncDecTestFunc   : EncodeDecodeTestFunc
    member this.uperEncDecTestFunc =
        match this.Kind with
        | Integer      t -> t.uperEncDecTestFunc
        | Real         t -> t.uperEncDecTestFunc
        | IA5String    t -> t.uperEncDecTestFunc
        | OctetString  t -> t.uperEncDecTestFunc
        | NullType     t -> t.uperEncDecTestFunc
        | BitString    t -> t.uperEncDecTestFunc
        | Boolean      t -> t.uperEncDecTestFunc
        | Enumerated   t -> t.uperEncDecTestFunc
        | SequenceOf   t -> t.uperEncDecTestFunc
        | Sequence     t -> t.uperEncDecTestFunc
        | Choice       t -> t.uperEncDecTestFunc
        | ObjectIdentifier t -> t.uperEncDecTestFunc
        | ReferenceType t-> t.uperEncDecTestFunc
        | TimeType t        -> t.uperEncDecTestFunc

    member this.acnEncDecTestFunc =
        match this.Kind with
        | Integer      t -> t.acnEncDecTestFunc
        | Real         t -> t.acnEncDecTestFunc
        | IA5String    t -> t.acnEncDecTestFunc
        | OctetString  t -> t.acnEncDecTestFunc
        | NullType     t -> t.acnEncDecTestFunc
        | BitString    t -> t.acnEncDecTestFunc
        | Boolean      t -> t.acnEncDecTestFunc
        | Enumerated   t -> t.acnEncDecTestFunc
        | SequenceOf   t -> t.acnEncDecTestFunc
        | Sequence     t -> t.acnEncDecTestFunc
        | Choice       t -> t.acnEncDecTestFunc
        | ObjectIdentifier t -> t.acnEncDecTestFunc
        | ReferenceType t-> t.acnEncDecTestFunc
        | TimeType t        -> t.acnEncDecTestFunc

    member this.xerEncDecTestFunc =
        match this.Kind with
        | Integer      t -> t.xerEncDecTestFunc
        | Real         t -> t.xerEncDecTestFunc
        | IA5String    t -> t.xerEncDecTestFunc
        | OctetString  t -> t.xerEncDecTestFunc
        | NullType     t -> t.xerEncDecTestFunc
        | BitString    t -> t.xerEncDecTestFunc
        | Boolean      t -> t.xerEncDecTestFunc
        | Enumerated   t -> t.xerEncDecTestFunc
        | SequenceOf   t -> t.xerEncDecTestFunc
        | Sequence     t -> t.xerEncDecTestFunc
        | Choice       t -> t.xerEncDecTestFunc
        | ObjectIdentifier t -> t.xerEncDecTestFunc
        | ReferenceType t-> t.xerEncDecTestFunc
        | TimeType t        -> t.xerEncDecTestFunc

    member this.getEncDecTestFunc (e:Asn1Encoding) =
        match e with
        | Asn1Encoding.UPER  -> this.uperEncDecTestFunc
        | Asn1Encoding.ACN   -> this.acnEncDecTestFunc
        | Asn1Encoding.XER   -> this.xerEncDecTestFunc
        | Asn1Encoding.BER   -> None


    member this.typeDefinitionOrReference : TypeDefinitionOrReference =
        match this.Kind with
        | Integer      t -> t.definitionOrRef
        | Real         t -> t.definitionOrRef
        | IA5String    t -> t.definitionOrRef
        | OctetString  t -> t.definitionOrRef
        | NullType     t -> t.definitionOrRef
        | BitString    t -> t.definitionOrRef
        | Boolean      t -> t.definitionOrRef
        | Enumerated   t -> t.definitionOrRef
        | SequenceOf   t -> t.definitionOrRef
        | Sequence     t -> t.definitionOrRef
        | Choice       t -> t.definitionOrRef
        | ObjectIdentifier t -> t.definitionOrRef
        | ReferenceType t-> t.definitionOrRef
        | TimeType t        -> t.definitionOrRef

    member this.isIA5String =
        match this.Kind with
        | IA5String    _    -> true
        | ReferenceType r   -> this.ActualType.isIA5String
        | _                 -> false

    member this.asn1Name =
        match this.id with
        | ReferenceToType((MD _)::(TA tasName)::[])   -> Some tasName
        | _                                                                     -> None


    member this.tasInfo =
        match this.typeAssignmentInfo with
        | Some (TypeAssignmentInfo tasInfo)  -> Some tasInfo
        | Some (ValueAssignmentInfo _)  -> None
        | None          ->
            match this.inheritInfo with
            | Some tasInfo  -> Some tasInfo.AsTasInfo
            | None          -> None

type Asn1Child with
    member this.getBackendName l =
        match l with
        | C         -> this._c_name
        | Scala     -> this._scala_name
        | Ada       -> this._ada_name
    member this.acnMinSizeInBits =
        match this.Optionality with
        | Some(AlwaysAbsent) -> 0I
        | _ -> this.Type.acnMinSizeInBits

    member this.acnMaxSizeInBits =
        match this.Optionality with
        | Some(AlwaysAbsent) -> 0I
        | _ -> this.Type.acnMaxSizeInBits

    member this.uperMinSizeInBits =
        match this.Optionality with
        | Some(AlwaysAbsent) -> 0I
        | _ -> this.Type.uperMinSizeInBits

    member this.uperMaxSizeInBits =
        match this.Optionality with
        | Some(AlwaysAbsent) -> 0I
        | _ -> this.Type.uperMaxSizeInBits

    member this.maxSizeInBits (enc: Asn1Encoding): BigInteger =
        match enc with
        | UPER -> this.uperMaxSizeInBits
        | ACN -> this.acnMaxSizeInBits
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

type Asn1Module with
    member this.ExportedTypes =
        match this.Exports with
        | Asn1Ast.All   ->
            let importedTypes = this.Imports |> List.collect(fun imp -> imp.Types) |> List.map(fun x -> x.Value)
            (this.TypeAssignments |> List.map(fun x -> x.Name.Value))@importedTypes
        | Asn1Ast.OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> System.Char.IsUpper (x.Chars 0))
    member this.ExportedVars =
        match this.Exports with
        | Asn1Ast.All   -> this.ValueAssignments |> List.map(fun x -> x.Name.Value)
        | Asn1Ast.OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> not (System.Char.IsUpper (x.Chars 0)))



type AstRoot with
    member this.getValueAssignmentByName (modName:String) (vasName:string) =
        match this.Files |> Seq.collect(fun f -> f.Modules) |> Seq.tryFind(fun m -> m.Name.Value = modName) with
        | None  -> raise(SemanticError(emptyLocation, (sprintf "No module exists with name '%s'" modName)))
        | Some m ->
            match m.ValueAssignments |> Seq.tryFind(fun vas -> vas.Name.Value = vasName) with
            |None   -> raise(SemanticError(emptyLocation, (sprintf "No value assignment exists with name '%s'" vasName)))
            | Some vas -> vas

    member r.Modules = r.Files |> List.collect(fun f -> f.Modules)
    member r.getModuleByName (name:StringLoc)  =
        let (n,loc) = name.AsTupple
        match r.Modules |> Seq.tryFind( fun m -> m.Name = name)  with
        | Some(m) -> m
        | None    -> raise(SemanticError(loc, sprintf "No Module Defined with name: %s" n ))


type Asn1File with
    member this.FileNameWithoutExtension = System.IO.Path.GetFileNameWithoutExtension this.FileName
    member this.TypeAssignments = this.Modules |> List.collect(fun m -> m.TypeAssignments)

let getValueByUperRange (r:uperRange<'T>) (z:'T) =
    match r with
    | Concrete (a,b)    -> if a <= z && z <= b then z else a
    | NegInf  b         -> if z <= b then z else b              //(-inf, b]
    | PosInf a          -> if a <= z then z else a               //[a, +inf)
    | Full              -> z

let rec mapValue (v:Asn1AcnAst.Asn1Value) =
    let newVKind =
        match v.kind with
        | Asn1AcnAst.IntegerValue     v ->  IntegerValue        v.Value
        | Asn1AcnAst.RealValue        v ->  RealValue           v.Value
        | Asn1AcnAst.StringValue      v ->  StringValue         v
        | Asn1AcnAst.BooleanValue     v ->  BooleanValue        v.Value
        | Asn1AcnAst.BitStringValue   v ->  BitStringValue      v.Value
        | Asn1AcnAst.OctetStringValue v ->  OctetStringValue    (v |> List.map(fun z -> z.Value))
        | Asn1AcnAst.EnumValue        v ->  EnumValue           v.Value
        | Asn1AcnAst.SeqOfValue       v ->  SeqOfValue          (v |> List.map mapValue)
        | Asn1AcnAst.SeqValue         v ->  SeqValue            (v |> List.map (fun n -> {NamedValue.name = n.name.Value; Value = mapValue n.Value}))
        | Asn1AcnAst.ChValue          n ->  ChValue             {NamedValue.name = n.name.Value; Value = mapValue n.Value}
        | Asn1AcnAst.NullValue        v ->  NullValue           v
        | Asn1AcnAst.RefValue     ((md,ts),v) ->  RefValue            ((md.Value, ts.Value), mapValue v)
        | Asn1AcnAst.ObjOrRelObjIdValue (a,b)   -> ObjOrRelObjIdValue (Asn1DefinedObjectIdentifierValue(a,b))
        | Asn1AcnAst.TimeValue        r  -> TimeValue r.Value
    {Asn1Value.kind = newVKind; id=v.id; loc = v.loc;moduleName    = v.moduleName}


let emitComponent (c:ResolvedObjectIdentifierValueComponent) =
    match c with
    | ResObjInteger            nVal             -> (nVal.Value, None)
    | ResObjNamedDefValue      (label,_,nVal)   -> (nVal, Some label.Value)
    | ResObjNamedIntValue      (label,nVal)   -> (nVal.Value, Some label.Value)
    | ResObjRegisteredKeyword  (label,nVal)   -> (nVal, Some label.Value)
    | ResObjDefinedValue       (_,_,nVal)     -> (nVal, None)

type ObjectIdentifierValue with
    member this.Values =
        match this with
        | InternalObjectIdentifierValue intList      -> intList |> List.map(fun i -> (i, None))
        | Asn1DefinedObjectIdentifierValue (resolvedComponents, _)  ->
            resolvedComponents |> List.map emitComponent

type Asn1Value with
    member this.getBackendName (l:ProgrammingLanguage) =
        match this.id with
        | ReferenceToValue (typePath,(VA2 vasName)::[]) -> ToC vasName
        | ReferenceToValue (typePath, vasPath)      ->
            let longName = (typePath.Tail |> List.map (fun i -> i.StrValue))@ (vasPath |> List.map (fun i -> i.StrValue))  |> Seq.StrJoin "_"
            ToC2(longName.Replace("#","elem").L1)

    member this.BaseValue =
        match this.kind with
        | RefValue(_, bv)   -> bv
        | _                 -> this
    member this.ActualValue =
        match this.kind with
        | RefValue(_, bv)   -> bv.ActualValue
        | _                 -> this

type Asn1ValueKind with
    member this.BaseValue =
        match this with
        | RefValue(_, bv)   -> bv.kind
        | _                 -> this
    member this.ActualValue =
        match this with
        | RefValue(_, bv)   -> bv.kind.ActualValue
        | _                 -> this

type SeqChildInfo with
    member this.acnInsertedField =
        match this with
        | Asn1Child _    -> false
        | AcnChild _     -> true

    member this.Name =
        match this with
        | Asn1Child x    -> x.Name.Value
        | AcnChild x     -> x.Name.Value
    member this.savePosition =
        match this with
        | AcnChild z ->
            match z.Type with
            | Asn1AcnAst.AcnNullType nt when nt.acnProperties.savePosition   ->  true
            | _     -> false
        | Asn1Child z ->
            match z.Type.Kind with
            | NullType nt when nt.baseInfo.acnProperties.savePosition         -> true
            | _                     -> false
    member this.Optionality =
        match this with
        | Asn1Child x    -> x.Optionality
        | AcnChild x     -> None
    member this.acnMaxSizeInBits =
        match this with
        | Asn1Child x    -> x.acnMaxSizeInBits // Takes into account ALWAYS ABSENT
        | AcnChild x     -> x.Type.acnMaxSizeInBits
    member this.acnMinSizeInBits =
        match this with
        | Asn1Child x    -> x.acnMinSizeInBits
        | AcnChild x     -> x.Type.acnMinSizeInBits
    member this.Comments =
        match this with
        | Asn1Child x    -> x.Comments
        | AcnChild x     -> [||]

    member this.maxSizeInBits (enc: Asn1Encoding): BigInteger =
        match enc with
        | UPER ->
            match this with
            | Asn1Child x -> x.uperMaxSizeInBits
            | AcnChild x -> raise (BugErrorException $"Unexpected UPER encoding for ACN child {x.Name}")
        | ACN -> this.acnMaxSizeInBits
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

let hasAcnEncodeFunction (encFunc: AcnFunction option) acnParameters (tasInfo: TypeAssignmentInfo option) =
    match encFunc with
    | None  -> false
    | Some fnc ->
        match acnParameters, tasInfo with
        | [], Some _ ->
            let p = {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
            let ret,_ = fnc.funcBody emptyState [] (NestingScope.init 0I 0I []) p
            match ret with
            | None   -> false
            | Some _ -> true
        | _     -> false

let hasUperEncodeFunction (encFunc : UPerFunction option)  =
    match encFunc with
    | None  -> false
    | Some fnc ->
            let p = {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
            match fnc.funcBody (NestingScope.init 0I 0I []) p false with
            | None   -> false
            | Some _ -> true

let hasXerEncodeFunction (encFunc : XerFunction option)  =
    match encFunc with
    | None  -> false
    | Some (XerFunction fnc) ->
            let p = {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
            let errCode = {ErrorCode.errCodeName = "DUMMY_ERR"; errCodeValue=0; comment=None}
            match fnc.funcBody_e errCode p None  with
            | None   -> false
            | Some _ -> true
    | Some XerFunctionDummy -> false


let AdaUses (r:AstRoot) =
    seq {
        for f in r.Files do
            for m in f.Modules do
                for tas in m.TypeAssignments do
                    yield sprintf "%s:%s" tas.Name.Value (ToC m.Name.Value);
    } |> Seq.iter(fun l -> System.Console.WriteLine l)

let rec GetMySelfAndChildren (t:Asn1Type) =
    seq {
        match t.Kind with
        | SequenceOf(conType) ->  yield! GetMySelfAndChildren conType.childType
        | Sequence seq ->
            for ch in seq.Asn1Children do
                yield! GetMySelfAndChildren ch.Type
        | Choice(ch)->
            for ch in ch.children do
                yield! GetMySelfAndChildren ch.chType
        |_ -> ()
        yield t
    } |> Seq.toList


let rec GetMySelfAndChildren2 (lm:Language.LanguageMacros) (t:Asn1Type) (p:CodegenScope)=
    seq {
        match t.Kind with
        | SequenceOf(conType) ->
            let ii = t.id.SequenceOfLevel + 1
            let i = "0" //sprintf "i%d" ii

            yield! GetMySelfAndChildren2 lm conType.childType ({p with accessPath = lm.lg.getArrayItem p.accessPath i conType.childType.isIA5String})
        | Sequence seq ->
            for ch in seq.Asn1Children do
                yield! GetMySelfAndChildren2 lm ch.Type ({p with accessPath = lm.lg.getSeqChild p.accessPath (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String ch.Optionality.IsSome})
        | Choice(ch)->
            for ch in ch.children do
                yield! GetMySelfAndChildren2 lm ch.chType ({p with accessPath = lm.lg.getChChild p.accessPath (lm.lg.getAsn1ChChildBackendName ch) ch.chType.isIA5String})
        |_ -> ()
        yield (t,p)
    } |> Seq.toList

let rec GetMySelfAndChildren3 visitChildPredicate (t:Asn1Type) =
    seq {
        if visitChildPredicate t then
            match t.Kind with
            | SequenceOf(conType) ->
                yield! GetMySelfAndChildren conType.childType
            | Sequence seq ->
                for ch in seq.Asn1Children do
                    yield! GetMySelfAndChildren ch.Type
            | Choice(ch)->
                for ch in ch.children do
                    yield! GetMySelfAndChildren ch.chType
            |_ -> ()
        yield t
    } |> Seq.toList


let getFuncNameGeneric (typeDefinition:TypeDefinitionOrReference) nameSuffix  =
    match typeDefinition with
    | ReferenceToExistingDefinition  refEx  -> None
    | TypeDefinition   td                   -> Some (td.typedefName + nameSuffix)

let getFuncNameGeneric2 (typeDefinition:TypeDefinitionOrReference) =
    match typeDefinition with
    | ReferenceToExistingDefinition  refEx  -> None
    | TypeDefinition   td                   -> Some (td.typedefName)


let nestItems joinItems2 children =
    let printChild (content:string) (soNestedContent:string option) =
        match soNestedContent with
        | None                -> content
        | Some sNestedContent -> joinItems2 content sNestedContent
    let rec printChildren children : Option<string> =
        match children with
        |[]     -> None
        |x::xs  -> Some (printChild x (printChildren xs))
    printChildren children


let nestItems_ret (lm:LanguageMacros) children =
    nestItems  lm.isvalid.JoinTwoIfFirstOk children


let getBaseFuncName (lm:LanguageMacros) (typeDefinition:TypeDefinitionOrReference) (o:Asn1AcnAst.ReferenceType) (id:ReferenceToType) (methodSuffix:string) (codec:CommonTypes.Codec) =
    let moduleName, typeDefinitionName0 =
        match typeDefinition with
        | ReferenceToExistingDefinition refToExist   ->
            match refToExist.programUnit with
            | Some md -> md, refToExist.typedefName
            | None    -> ToC id.ModName, refToExist.typedefName
        | TypeDefinition                tdDef        ->
            match tdDef.baseType with
            | None -> ToC id.ModName, tdDef.typedefName
            | Some refToExist ->
                match refToExist.programUnit with
                | Some md -> md, refToExist.typedefName
                | None    -> ToC id.ModName, refToExist.typedefName

    let baseTypeDefinitionName =
        match lm.lg.hasModules with
        | false     -> typeDefinitionName0
        | true   ->
            match id.ModName = o.modName.Value with
            | true  -> typeDefinitionName0
            | false -> moduleName + "." + typeDefinitionName0
    baseTypeDefinitionName, baseTypeDefinitionName + methodSuffix + codec.suffix


let serializeIcdTasToXml (icdTypeAss: IcdTypeAss) =
    let serializer = XmlSerializer(typeof<IcdTypeAss>)
    use stringWriter = new StringWriter()
    serializer.Serialize(stringWriter, icdTypeAss)
    stringWriter.ToString()

//let serializeIcdTasToJson (icdTypeAss: IcdTypeAss) =
//    JsonConvert.SerializeObject(icdTypeAss)

(*
and IcdTypeAss = {
    linkId  : ReferenceToType
    tasInfo : TypeAssignmentInfo option
    asn1Link : string option
    acnLink : string option
    name : string
    kind : string
    comments : string list
    rows : IcdRow list
    compositeChildren : IcdTypeAss list
    minLengthInBytes : BigInteger
    maxLengthInBytes : BigInteger
    hash            : string
    canBeEmbedded  : bool
    hasAcnDefinition : bool


let md5 = System.Security.Cryptography.MD5.Create()
let calcIcdTypeAssHash (t1:IcdTypeAss) =
    let rec calcIcdTypeAssHash_aux (t1:IcdTypeAss) =
        let rws =
            t1.rows |>
            Seq.map(fun r -> sprintf "%A%A%A%A%A%A%A%A%A%A" r.idxOffset r.fieldName r.comments r.sPresent r.sType r.sConstraint r.minLengthInBits r.maxLengthInBits r.sUnits r.rowType) |>
            Seq.StrJoin ""
        let aa = sprintf"%A%A%A%A%A%A%A%A%A" t1.acnLink t1.asn1Link  t1.name t1.kind t1.comments t1.minLengthInBytes t1.maxLengthInBytes (rws) ("")
        let bytes = md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes aa)
        Convert.ToHexString bytes
    calcIcdTypeAssHash_aux t1
*)

let serializeIcdTasToText (icdTypeAss: IcdTypeAss) =
    let printRow (r:IcdRow) =
        sprintf """
            idxOffset = %A
            fieldName = %A
            comments = %A
            sPresent = %A
            sType = %A
            sConstraint = %A
            minLengthInBits = %A
            maxLengthInBits = %A
            sUnits = %A
            rowType = %A
            """ r.idxOffset r.fieldName r.comments r.sPresent r.sType r.sConstraint r.minLengthInBits r.maxLengthInBits r.sUnits r.rowType
    
    //let aa = sprintf"%A%A%A%A%A%A%A%A%A" t1.acnLink t1.asn1Link  t1.name t1.kind t1.comments t1.minLengthInBytes t1.maxLengthInBytes (rws) ("")
    let rws = icdTypeAss.rows |> Seq.map printRow |> Seq.StrJoin "\n"
    sprintf """
        typeId = %s
        acnLink = %A
        asn1Link = %A
        name = %A
        kind = %A
        comments = %A
        minLengthInBytes = %A
        maxLengthInBytes = %A
        rows = 
            %A
        """ icdTypeAss.typeId.AsString icdTypeAss.acnLink icdTypeAss.asn1Link  icdTypeAss.name icdTypeAss.kind icdTypeAss.comments icdTypeAss.minLengthInBytes icdTypeAss.maxLengthInBytes rws
