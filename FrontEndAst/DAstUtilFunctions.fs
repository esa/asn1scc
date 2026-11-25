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
                // the checks of the outer nodes first: they are joined with && / and then, and a check below a CHOICE
                // alternative reads a member that exists only when the alternative is selected
                (newPath, curCheckExp@checkPath, zeroBasedSequenceOfLevel, idx+1)) (pVal,[], 0, 1) |> (fun (a,checkPath,_,_) -> a, checkPath)
        ret
    | _                                 -> raise(BugErrorException "getAccessFromScopeNodeList")


let rec extractDefaultInitValue (lm:LanguageMacros) (childType: Asn1TypeKind): String =
    match childType with
    | Integer i -> i.baseInfo.defaultInitVal
    | Real r -> r.baseInfo.defaultInitVal
    | NullType n -> n.baseInfo.defaultInitVal
    | Boolean b -> b.baseInfo.defaultInitVal
    | ReferenceType rt -> extractDefaultInitValue lm rt.resolvedType.Kind
    | _ -> lm.lg.complexTypeDefaultInit

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

    member this.longTypedefName2 (lg: ILangGeneric option) (hasModules: bool) (moduleName: string) =
        let moduleName = ToC moduleName
        match lg with
        | Some l -> l.longTypedefName2 this hasModules moduleName
        | None ->
            match this with
            | TypeDefinition  td ->
                td.typedefName
            | ReferenceToExistingDefinition ref ->
                match ref.programUnit with
                | Some pu ->
                    match hasModules with
                    | true   ->
                        match pu with
                        | "" -> ref.typedefName
                        | k when k = moduleName -> ref.typedefName
                        | _ -> pu + "." + ref.typedefName
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
        | C      -> (ToC this._present_when_name_private) + "_PRESENT"
        | Scala  -> (ToC this._present_when_name_private) + "_PRESENT" // TODO: Scala
        | Python -> (ToC this._present_when_name_private) + "_PRESENT" // TODO: Python
        | Rust   -> (ToC this._present_when_name_private)
        | Ada    ->
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + ((ToC this._present_when_name_private) + "_PRESENT")
            | _       -> (ToC this._present_when_name_private) + "_PRESENT"


type Asn1AcnAst.NamedItem      with
    member this.CEnumName l =
        match l with
        | C      -> this.c_name
        | Scala  -> this.scala_name
        | Ada    -> this.ada_name
        | Rust   -> this.rust_name
        | Python -> this.python_name


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
        | Rust      -> this._rust_name
        | Python    -> this._python_name
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
            ToC2(longName.Replace("#","elm").L1)

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
    // funcName = None: no standalone ACN function (--acn-v2 exported determinants)
    | Some fnc when fnc.funcName.IsNone -> false
    | Some fnc ->
        match acnParameters, tasInfo with
        | [], Some _ ->
            // A zero-bit encoding can have no inline body and still expose a
            // callable codec. Automatic tests exercise that emitted function.
            fnc.funcName.IsSome && fnc.func.IsSome
        | _     -> false

let hasUperEncodeFunction (encFunc : UPerFunction option)  =
    match encFunc with
    | None  -> false
    | Some fnc ->
        fnc.funcName.IsSome && fnc.func.IsSome

let hasXerEncodeFunction (encFunc : XerFunction option)  =
    match encFunc with
    | None  -> false
    | Some (XerFunction fnc) ->
            let p = {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
            let errCode = {ErrorCode.errCodeName = "DUMMY_ERR"; errCodeValue=0; comment=None; fieldPath=""}
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
        // References can emit initializers for their resolved children. Visit
        // those children, but yield the reference itself only once below.
        match t.ActualType.Kind with
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


/// The bLengthEnforced argument of the variable-size encode macros (coverage step 14b2).
let lengthEnforced (lengthCheck:VarSizeLengthCheck) =
    match lengthCheck with
    | LengthChecked             -> false
    | LengthEnforcedByTypedef   -> true

let nestItems joinItems2 children =
    let printChild (content:string) (soNestedContent:string option) =
        match soNestedContent with
        | None                -> content
        | Some sNestedContent when String.IsNullOrWhiteSpace sNestedContent -> content
        | Some sNestedContent -> joinItems2 content sNestedContent
    let rec printChildren children : Option<string> =
        match children with
        |[]     -> None
        |x::xs  -> Some (printChild x (printChildren xs))
    printChildren children

/// nestItems for decode parts whose outcome is known (coverage step 10b, Ada): after a part that may fail
/// the rest is nested under the success check (JoinItems2); after a part that cannot fail and sets result,
/// the check becomes an assertion (JoinItemsAssert); after a part that does not touch result
/// (_FullRange calls), the rest simply follows (JoinItemsNoCheck).
let nestItemsByOutcome (lm:LanguageMacros) (children: (string * DecodeOutcome) list) =
    let printChild (content:string) (outcome:DecodeOutcome) (soNestedContent:string option) =
        match soNestedContent with
        | None                -> content
        | Some sNestedContent when String.IsNullOrWhiteSpace sNestedContent -> content
        | Some sNestedContent ->
            match outcome with
            | MayFail                       -> lm.isvalid.JoinItems2 content sNestedContent
            | AlwaysSucceeds WritesResult   -> lm.isvalid.JoinItemsAssert content sNestedContent
            | AlwaysSucceeds NoResultWrite  -> lm.isvalid.JoinItemsNoCheck content sNestedContent
    let rec printChildren children : Option<string> =
        match children with
        |[]           -> None
        |(x, o)::xs   -> Some (printChild x o (printChildren xs))
    printChildren children


/// Coverage step 12b: the outcome of an encode part (decode bodies: unknown, not used).
let encodeOutcomeOf (codec:CommonTypes.Codec) (o:EncodeOutcome) : EncodeOutcome =
    match codec with
    | CommonTypes.Encode -> o
    | CommonTypes.Decode -> EncodeOutcome.unknown

/// Coverage step 14b: the key of an encoder in State.encodeCallOutcomes. With modules (Ada) a call into another module
/// names the encoder with its package (getBaseTypeDefName), and two modules may define a type of the same name
/// (q/issues.md Issue 57), so the key is always the qualified name; a name that is already qualified stays as is.
let encodeCallKey (lm:LanguageMacros) (sModName:string) (sFuncName:string) : string =
    match lm.lg.hasModules, sFuncName.Contains "." with
    | true, false               -> ToC sModName + "." + sFuncName
    | true, true | false, _     -> sFuncName

/// Coverage step 12b: what a call of the encoder sFuncName does (recorded when the type assignment's encoder was
/// created; when it was not, a call that may fail and sets the result, 14b). sCallerModName: the module of the type whose body makes the call.
let encodeCallOutcome (lm:LanguageMacros) (sCallerModName:string) (us:State) (codec:CommonTypes.Codec) (sFuncName:string) : EncodeOutcome =
    match codec, us.encodeCallOutcomes.TryFind (encodeCallKey lm sCallerModName sFuncName) with
    | CommonTypes.Encode, Some o    -> o
    | CommonTypes.Encode, None      -> EncodeOutcome.unknownCall
    | CommonTypes.Decode, _         -> EncodeOutcome.unknown

/// Coverage step 14q (Q3 b): whether the encoding of a SEQUENCE child reads the value through its presence (the
/// Exist flag of an OPTIONAL child); an ALWAYS ABSENT child encodes nothing (an encoder body without the validation may
/// then not reference val: Ada pragma Unreferenced).
let presenceReadsVal (opt:Asn1AcnAst.Asn1Optionality option) =
    match opt with
    | None                          -> false
    | Some Asn1AcnAst.AlwaysAbsent  -> false
    | Some Asn1AcnAst.AlwaysPresent
    | Some (Asn1AcnAst.Optional _)  -> true

/// Coverage step 14q (Q3 b): the function an encoder of the reference site siteId calls for the type assignment encoder
/// sFuncName, and what the call does: the encoder's body (without its entry validation) when the encoder has one
/// (encodeBodies), it is in the caller's package, and the site's own validation calls the same IsConstraintValid as the
/// encoder's _aux (siteValidatorCalls: the caller's entry validation has checked the value at this site, by
/// construction); else the encoder itself (encodeCallOutcome).
let encodeCallTarget (lm:LanguageMacros) (sCallerModName:string) (us:State) (codec:CommonTypes.Codec) (siteId:ReferenceToType) (sFuncName:string) : string * EncodeOutcome =
    let viaEncoder () = sFuncName, encodeCallOutcome lm sCallerModName us codec sFuncName
    match codec with
    | CommonTypes.Decode -> viaEncoder ()
    | CommonTypes.Encode ->
        match us.encodeBodies.TryFind (encodeCallKey lm sCallerModName sFuncName), us.siteValidatorCalls.TryFind siteId.AsString with
        | Some b, Some v when v = b.validatorKey && b.modName = ToC sCallerModName -> b.bodyFuncName, b.outcome
        | Some _, _
        | None, _                                                                  -> viaEncoder ()

/// Coverage step 12a / cycle 15a2: whether the IsConstraintValid that a type's encoder or decoder calls (the function
/// emitted under the name of isValidFunc) cannot fail for any value (11b outcome AlwaysValid).
/// emitted = State.emittedValidatorOutcomes (cycle 15a2): the outcome of the functions the type assignment under
/// construction emits, from its first construction (DAstConstruction.mapTas).
let private calledValidatorCannotFail (lm:LanguageMacros) (emitted: Map<string, ValidationOutcome>) (isValidFunc: IsValidFunction option) (t:Asn1AcnAst.Asn1Type) : bool =
    // The merged type of a reference with constraints at the site (Outer ::= Alias ('010203'H), ByteFlags ::= BitFlags
    // (SIZE(8))) builds the encoder (decoder) that the type assignment lends (DAstUPer.createReferenceFunction when the
    // encodings differ, AcnReference.createReferenceFunction_inline), but the IsConstraintValid emitted under the name
    // the function calls is the type assignment's, created after it; a chained alias creates one more validator per
    // alias in between, under the same name. The function's own isValidFunc is the one of the level that builds it, so
    // the decision reads the outcome of the function emitted under that name when the first construction knows it.
    let calledFunctionsOutcome =
        match isValidFunc with
        | Some {funcName = Some fn}  -> emitted.TryFind (encodeCallKey lm t.id.ModName fn)
        | Some {funcName = None}
        | None                       -> None
    let alwaysValid =
        match calledFunctionsOutcome, isValidFunc with
        | Some AlwaysValid, _                                       -> true
        | Some MayBeInvalid, _                                      -> false
        | None, Some {funcName = Some _; outcome = AlwaysValid}     -> true
        | None, Some {funcName = Some _; outcome = MayBeInvalid}
        | None, Some {funcName = None}
        | None, None                                                -> false
    // Outside the map (no first construction, or a function of another type assignment): the type's own outcome is
    // the called function's unless the type is the merged copy of a reference with constraints at the site, whose
    // function is the reference's (it calls the referenced type's function and checks the site's constraints; in Ada
    // a site range that the subtype enforces (MyPDU ::= T1 (0 .. 100)) makes the merged type AlwaysValid while the
    // reference's function still carries the check): keep the call then.
    let outcomeIsTheCalledFunctions =
        match calledFunctionsOutcome, t.inheritInfo with
        | Some _, _                                         -> true
        | None, Some {hasAdditionalConstraints = true}      -> false
        | None, Some {hasAdditionalConstraints = false}
        | None, None                                        -> true
    alwaysValid && outcomeIsTheCalledFunctions

/// Coverage step 12a: the encoder of a type whose IsConstraintValid cannot fail (11b outcome AlwaysValid) does not
/// call it; the template emits a comment instead of the call and its check. Decoders: decoderSkipsValidation.
let encoderSkipsValidation (lm:LanguageMacros) (codec:CommonTypes.Codec) (emitted: Map<string, ValidationOutcome>) (isValidFunc: IsValidFunction option) (t:Asn1AcnAst.Asn1Type) (bodyOutcome:EncodeOutcome) : bool =
    let cannotFail = calledValidatorCannotFail lm emitted isValidFunc t
    // (a variable-size OCTET / BIT STRING encoding starts with its own length check, which sets result); since 12b
    // also any body whose writes are known: the encoder initialises result only where the body needs it
    let encodingWritesNoResult =
        match t.Kind with
        | Asn1AcnAst.Integer _ | Asn1AcnAst.Real _ | Asn1AcnAst.IA5String _ | Asn1AcnAst.NumericString _
        | Asn1AcnAst.NullType _ | Asn1AcnAst.TimeType _ | Asn1AcnAst.Boolean _ | Asn1AcnAst.Enumerated _
        | Asn1AcnAst.ObjectIdentifier _                                                        -> true
        | Asn1AcnAst.OctetString o                                                             -> o.isFixedSize
        | Asn1AcnAst.BitString o                                                               -> o.isFixedSize
        | Asn1AcnAst.SequenceOf _ | Asn1AcnAst.Sequence _ | Asn1AcnAst.Choice _ | Asn1AcnAst.ReferenceType _ -> false
    let bodyWritesKnown = bodyOutcome.writes <> WritesUnknown
    match codec, lm.lg.encoderValidationModel with
    | CommonTypes.Encode, EncodersSkipAlwaysValid           -> cannotFail
    | CommonTypes.Encode, EncodersSkipAlwaysValidPrimitives -> cannotFail && (encodingWritesNoResult || bodyWritesKnown)
    | CommonTypes.Encode, EncodersCallEveryValidation
    | CommonTypes.Decode, _                                 -> false

/// Cycle 15j (stage A): the public decoder of a type (uPER `T_Decode`, ACN `T_ACN_Decode`) does not end with a call of
/// an IsConstraintValid that cannot fail for any value; the template returns `ret` with a comment naming the function.
/// The decision is the encoders' (12a / 15a2: the outcome of the function emitted under the name the decoder calls,
/// calledValidatorCannotFail); the decoded value itself is not looked at. Kept separate from encoderSkipsValidation
/// so that the encoder models (Ada EncodersSkipAlwaysValidPrimitives) never reach the decoders. The skipped C validator
/// also wrote `*pErrCode = 0`: without it a successful decode leaves the code its body wrote last, which must be 0 (the
/// templates write the code on their failure paths only; the C `-atc` tests check `*pErrCode == 0` after every decode).
let decoderSkipsValidation (lm:LanguageMacros) (emitted: Map<string, ValidationOutcome>) (isValidFunc: IsValidFunction option) (t:Asn1AcnAst.Asn1Type) : bool =
    match lm.lg.decoderValidationModel with
    | DecodersSkipAlwaysValid      -> calledValidatorCannotFail lm emitted isValidFunc t
    | DecodersCallEveryValidation  -> false

/// Cycle 15j round 2: whether the generated code of a decoder body (its statements and local declarations) names the
/// decoder's value parameter. Used only where a decoder omits its final validity call (decoderSkipsValidation): that
/// call was the template's only use of the parameter, so a body that does not name it needs `(void)<param>;` (C
/// `-Wextra -Werror`: unused parameter; an ACN SEQUENCE of NULLs with a pattern or an alignment). Read from the text,
/// not from the bodies' bValIsUnReferenced: the decode producers do not compute it (an ACN SEQUENCE reports it only for
/// Encode, coverage step 14p). Comments are removed first; a false "named" here would be a compile error, a false
/// "not named" only a redundant `(void)`.
let decoderBodyNamesValue (varName:string) (code: string seq) : bool =
    let noComments (s:string) =
        let s = Text.RegularExpressions.Regex.Replace (s, @"/\*.*?\*/", " ", Text.RegularExpressions.RegexOptions.Singleline)
        Text.RegularExpressions.Regex.Replace (s, @"//[^\n]*", " ")
    let rx = Text.RegularExpressions.Regex (@"(?<!->)(?<![\w.])" + Text.RegularExpressions.Regex.Escape varName + @"(?!\w)")
    code |> Seq.exists (fun c -> rx.IsMatch (noComments c))

/// Whether the encoder (12a) or the public decoder (15j) of a type omits its validity call; bValidityCannotFail of the
/// type-assignment templates.
let codecSkipsValidation (lm:LanguageMacros) (codec:CommonTypes.Codec) (emitted: Map<string, ValidationOutcome>) (isValidFunc: IsValidFunction option) (t:Asn1AcnAst.Asn1Type) (bodyOutcome:EncodeOutcome) : bool =
    match codec with
    | CommonTypes.Encode -> encoderSkipsValidation lm codec emitted isValidFunc t bodyOutcome
    | CommonTypes.Decode -> decoderSkipsValidation lm emitted isValidFunc t


/// nestItems for encode parts whose outcome is known (coverage step 12b): after a part that may fail the rest is
/// nested under the check (JoinItems2); after a part that cannot fail and writes the result, C has no check and Ada
/// asserts it (JoinItemsCannotFail, whose comment names the encoder the part calls); after a part that does not
/// write the result, no check (JoinItemsNoWrite; only the first such join of the body carries a comment).
let nestEncodeParts (lm:LanguageMacros) (parts: (string * EncodeOutcome) list) : string option =
    let parts = List.toArray parts
    let hasContentAfter i = parts |> Array.skip (i + 1) |> Array.exists (fun (s, _) -> not (String.IsNullOrWhiteSpace s))
    let firstNoWriteJoin =
        parts |> Array.tryFindIndex (fun (_, o) -> not o.canFail && o.writes = NoWrite) |>
        Option.filter hasContentAfter
    let rec printFrom i : string option =
        match i < parts.Length with
        | false -> None
        | true  ->
            let content, o = parts.[i]
            match printFrom (i + 1) with
            | None                                                          -> Some content
            | Some sNested when String.IsNullOrWhiteSpace sNested           -> Some content
            | Some sNested ->
                match o.canFail, o.writes with
                | true, _       -> Some (lm.isvalid.JoinItems2 content sNested)
                | false, NoWrite -> Some (lm.isvalid.JoinItemsNoWrite content sNested (firstNoWriteJoin = Some i))
                | false, (AlwaysWrites | SometimesWrites | ReadsFirst | WritesUnknown) ->
                    let soCallee = o.callee |> Option.map (fun c -> c.name)
                    let bAssumed =
                        match o.callee with
                        | Some {basis = ByCompilerAnalysis} -> true
                        | Some {basis = ByContract}
                        | None                              -> false
                    Some (lm.isvalid.JoinItemsCannotFail content sNested soCallee bAssumed)
    printFrom 0

/// Coverage step 14q: a template that guards what follows a part with a check of its result (CONTAINING: the call or
/// inline body of the contents, then the length / padding / PatchDet of the region) drops the check when the part
/// cannot fail. None: the template keeps the check (decoding, or a part that may fail); Some note: the text that takes
/// its place, as nestEncodeParts joins (C a comment; Ada a comment, and pragma Assert / Assume (result.Success) after
/// a part that writes the result).
let encodeNoCheckNote (lm:LanguageMacros) (codec:CommonTypes.Codec) (o:EncodeOutcome) : string option =
    match codec, o.canFail with
    | CommonTypes.Decode, _
    | CommonTypes.Encode, true  -> None
    | CommonTypes.Encode, false ->
        let soCallee = o.callee |> Option.map (fun c -> c.name)
        let bAssumed =
            match o.callee with
            | Some {basis = ByCompilerAnalysis} -> true
            | Some {basis = ByContract}
            | None                              -> false
        let bWritesResult =
            match o.writes with
            | NoWrite                                                       -> false
            | AlwaysWrites | SometimesWrites | ReadsFirst | WritesUnknown   -> true
        Some (lm.isvalid.EncodeNoCheckNote soCallee bAssumed bWritesResult)

let nestItems_ret (lm:LanguageMacros) children =
    nestItems  lm.isvalid.JoinTwoIfFirstOk children


let getBaseTypeDefName (lm:LanguageMacros) (baseTypeDefinitionName: string) (moduleName: string) (t: Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType): string =
    match lm.lg.hasModules with
    | false     -> baseTypeDefinitionName
    | true   ->
        // Compare ToC'd module names: if the type being generated lives in a different
        // module than where the ultimate typedef is defined (moduleName), add the prefix.
        // Using moduleName (not o.modName.Value) handles multi-hop aliases correctly:
        // e.g. Baz ::= Bar ::= Foo where Foo is in module A, Baz/Bar in module B.
        match ToC t.id.ModName = moduleName with
        | true  -> baseTypeDefinitionName
        | false -> lm.lg.constructCrossModuleFuncPrefix moduleName + baseTypeDefinitionName

let getBaseFuncName (lm:LanguageMacros) (typeDefinition:TypeDefinitionOrReference) (o:Asn1AcnAst.ReferenceType) (t: Asn1AcnAst.Asn1Type) (methodSuffix:string) (codec:CommonTypes.Codec) =
    let moduleName, typeDefinitionName0 =
        match typeDefinition with
        | ReferenceToExistingDefinition refToExist   ->
            match refToExist.programUnit with
            | Some md -> md, refToExist.typedefName
            | None    -> ToC t.id.ModName, refToExist.typedefName
        | TypeDefinition                tdDef        ->
            match tdDef.baseType with
            | None -> ToC t.id.ModName, tdDef.typedefName
            | Some refToExist ->
                match refToExist.programUnit with
                | Some md -> md, refToExist.typedefName
                | None    -> ToC t.id.ModName, refToExist.typedefName

    let baseTypeDefinitionName = getBaseTypeDefName lm typeDefinitionName0 moduleName t o
    baseTypeDefinitionName, lm.lg.constructFuncName baseTypeDefinitionName methodSuffix (lm.lg.codecSuffix codec)

/// The type assignment whose own function a call from a reference site must use when the chain of type assignments
/// from the referenced one reaches a CONTAINING (Issue 91, see getReferenceCallFuncName); None otherwise. Independent of
/// the site's own encoding options: the contents of a CONTAINING site (`f OCTET STRING (CONTAINING Box)`, `Box ::= OCTET
/// STRING (CONTAINING T)`) are Box's encoding, not T's (q/issues.md Issue 134).
let chainCalleeTas (r:Asn1AcnAst.AstRoot) (o:Asn1AcnAst.ReferenceType) : Asn1AcnAst.TypeAssignment option =
    let rec reachesContaining (modName:StringLoc) (tasName:StringLoc) (visited:Set<string*string>) =
        let key = (modName.Value, tasName.Value)
        match visited.Contains key with
        | true  -> false
        | false ->
            let tas = (r.GetModuleByName modName).GetTypeAssignmentByName tasName r
            match tas.Type.Kind with
            | Asn1AcnAst.ReferenceType rf ->
                match rf.encodingOptions with
                | Some _ -> true
                | None   -> reachesContaining rf.modName rf.tasName (visited.Add key)
            | Asn1AcnAst.Integer _ | Asn1AcnAst.Real _ | Asn1AcnAst.IA5String _ | Asn1AcnAst.NumericString _
            | Asn1AcnAst.OctetString _ | Asn1AcnAst.NullType _ | Asn1AcnAst.BitString _ | Asn1AcnAst.Boolean _
            | Asn1AcnAst.Enumerated _ | Asn1AcnAst.ObjectIdentifier _ | Asn1AcnAst.SequenceOf _ | Asn1AcnAst.Sequence _
            | Asn1AcnAst.Choice _ | Asn1AcnAst.TimeType _ -> false
    match reachesContaining o.modName o.tasName Set.empty with
    | false -> None
    | true  -> Some ((r.GetModuleByName o.modName).GetTypeAssignmentByName o.tasName r)

/// The type assignment whose own function a reference site without encoding options calls when the chain of type
/// assignments reaches a CONTAINING (Issue 91, see getReferenceCallFuncName); None otherwise, and None for a site with
/// encoding options of its own (a CONTAINING site: its contents call is containedCalleeTas). The single predicate for
/// the call's name (containingTasTypeName) and for what the call does: the decode classifiers of
/// BackendAst/DecodeOutcome.fs classify such a call by this type assignment, not by the contents that
/// GetActualTypeByName reaches through the CONTAINING (q/issues.md Issue 133: the site was classified "cannot fail"
/// after a call of a CONTAINING decoder that fails on a bad length).
let containingCalleeTas (r:Asn1AcnAst.AstRoot) (o:Asn1AcnAst.ReferenceType) : Asn1AcnAst.TypeAssignment option =
    match o.encodingOptions with
    | Some _ -> None
    | None   -> chainCalleeTas r o

/// q/issues.md Issue 134 / Issue 138: the type assignment whose function encodes / decodes the contents of a CONTAINING
/// site (a site with encoding options): the referenced type assignment itself (`f OCTET STRING (CONTAINING Sub)`: Sub);
/// None for a site without encoding options. Its name is also the prefix of the site's temporary buffer constant
/// (`<Sub>_REQUIRED_BYTES_FOR_[ACN_]ENCODING`). Every type assignment has functions and constants of its own, and the
/// site's contents (without a site specification that overrides them, Issue 135: containingSiteSpecialised) are exactly
/// its encoding. Before, the call and the buffer were named after the typedef the referenced type assignment shares
/// (getBaseFuncName), while the region range came from the referenced type assignment: `Box ::= OCTET STRING
/// (CONTAINING T)` (Issue 134), `Sub ::= T (WITH COMPONENTS ...)` and `Sub ::= T` with an ACN specification of its own
/// (Issue 138) all share `typedef T`, so the site called T's functions (another encoding: --acn-v2 wrote a wrong
/// stream, the legacy decoders wrote past the buffer).
let containedCalleeTas (r:Asn1AcnAst.AstRoot) (o:Asn1AcnAst.ReferenceType) : Asn1AcnAst.TypeAssignment option =
    match o.encodingOptions with
    | None   -> None
    | Some _ -> Some ((r.GetModuleByName o.modName).GetTypeAssignmentByName o.tasName r)

/// q/issues.md Issue 135: --acn-v2 gives an embedded-length or fixed-size CONTAINING site whose ACN specification
/// overrides the encoding of the contents (ReferenceType.contentsSpecOverride: `f [] { a [size 32] }`, also on the
/// CONTAINING type assignment itself, `Box [] { a [size 32] }`) a function of its own with the merged contents
/// (`<Path>_Containing_ACN_Encode/Decode`, DAstACNDeferred), as the external-size class does through closure conversion;
/// the region then calls it instead of the type assignment's function (containedCalleeTas), whose encoding is another
/// one. Not for contents with ACN parameters (DAstACNDeferred rejects an override of them) nor in legacy (which rejects
/// a site whose sizes the type assignment's encoding does not fit, AcnReference.fs). The single predicate for the
/// function the site calls (DAstACNDeferred) and for the classification of that call (BackendAst/DecodeOutcome.fs).
let containingSiteSpecialised (r:Asn1AcnAst.AstRoot) (o:Asn1AcnAst.ReferenceType) : bool =
    match r.args.acnDeferred, o.encodingOptions with
    | false, _ | true, None -> false
    | true, Some eo ->
        match eo.acnEncodingClass with
        | Asn1AcnAst.SZ_EC_FIXED_SIZE | Asn1AcnAst.SZ_EC_LENGTH_EMBEDDED _ ->
            o.contentsSpecOverride && o.acnArguments.IsEmpty && o.resolvedType.acnParameters.IsEmpty
        | Asn1AcnAst.SZ_EC_ExternalField _ | Asn1AcnAst.SZ_EC_TerminationPattern _ | Asn1AcnAst.SZ_EC_Deduced -> false

/// The contents type whose encoding the function called by a CONTAINING site has (Issues 134, 135, 138): the site's own
/// merged contents when the site is specialised (containingSiteSpecialised), else the referenced type assignment's
/// (containedCalleeTas). For the outcome of the call (DecodeOutcome) and for size checks against the region.
let containedCalleeType (r:Asn1AcnAst.AstRoot) (o:Asn1AcnAst.ReferenceType) : Asn1AcnAst.Asn1Type =
    match containingSiteSpecialised r o, containedCalleeTas r o with
    | true, _         -> o.resolvedType
    | false, Some tas -> tas.Type
    | false, None     -> Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName

/// The type name of a type assignment as a call from the module of t names it (module-qualified across modules).
let private tasTypeNameFrom (lm:LanguageMacros) (t: Asn1AcnAst.Asn1Type) (tas:Asn1AcnAst.TypeAssignment) =
    let tasTypeDef = lm.lg.getTypeDefinition tas.Type.FT_TypeDefinition
    match lm.lg.hasModules && ToC t.id.ModName <> tasTypeDef.programUnit with
    | true  -> lm.lg.constructCrossModuleFuncPrefix tasTypeDef.programUnit + tasTypeDef.typeName
    | false -> tasTypeDef.typeName

/// The (module-qualified) type name of the type assignment a reference site without encoding options names when the
/// chain of type assignments reaches a CONTAINING (Issue 91, see getReferenceCallFuncName); None otherwise.
let containingTasTypeName (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (o:Asn1AcnAst.ReferenceType) (t: Asn1AcnAst.Asn1Type) =
    containingCalleeTas r o |> Option.map (tasTypeNameFrom lm t)

/// q/issues.md Issue 91 (step 14k): the function a reference site without encoding options of its own calls. A type
/// assignment `OctBox ::= OCTET STRING (CONTAINING Inner)` shares the contents' typedef (`typedef Inner OctBox`), so
/// getBaseFuncName names the call after Inner and the site (`x OctBox`, also a CHOICE alternative, a SEQUENCE OF element,
/// an alias `Box2 ::= OctBox`) encoded Inner without the OCTET / BIT STRING that OctBox's own function writes (and that
/// REQUIRED_BITS and the ICD count). When the chain of type assignments reaches a CONTAINING, call the referenced type
/// assignment's own function; otherwise getBaseFuncName.
/// Issue 134: the same rule for the contents of a CONTAINING site (containedCalleeTas); there the first component (the
/// buffer constant prefix of the CONTAINING arms) is the type assignment's name too. For a site without encoding options
/// the first component is not read and stays getBaseFuncName's.
let getReferenceCallFuncName (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (typeDefinition:TypeDefinitionOrReference) (o:Asn1AcnAst.ReferenceType) (t: Asn1AcnAst.Asn1Type) (methodSuffix:string) (codec:CommonTypes.Codec) =
    let baseTypeDefinitionName, baseFncName = getBaseFuncName lm typeDefinition o t methodSuffix codec
    match containingTasTypeName r lm o t, containedCalleeTas r o with
    | Some tasName, _ -> baseTypeDefinitionName, lm.lg.constructFuncName tasName methodSuffix (lm.lg.codecSuffix codec)
    | None, Some tas  ->
        let tasName = tasTypeNameFrom lm t tas
        tasName, lm.lg.constructFuncName tasName methodSuffix (lm.lg.codecSuffix codec)
    | None, None      -> baseTypeDefinitionName, baseFncName


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

/// The typedef that declares the value's Ada type: the type's own, or, for a type that resolves a reference to a
/// type assignment without new constraints (inheritInfo), the type assignment's own (the type shares it, with the
/// same constraints). None for an RTL type or anything else.
let declaringTypedef (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) : TypeDefinition option =
    match typeDefinition, t.inheritInfo with
    | TypeDefinition td, _                     -> Some td
    | ReferenceToExistingDefinition rd, Some inh when not rd.definedInRtl && not inh.hasAdditionalConstraints ->
        let tasDefinition =
            r.modulesMap.TryFind inh.modName |>
            Option.bind (fun m -> m.typeAssignmentsMap.TryFind inh.tasName) |>
            Option.bind (fun tas ->
                match tas.Type.Kind with
                | Asn1AcnAst.Integer o      -> Some (l.lg.definitionOrRef o.definitionOrRef)
                | Asn1AcnAst.OctetString o  -> Some (l.lg.definitionOrRef o.definitionOrRef)
                | Asn1AcnAst.BitString o    -> Some (l.lg.definitionOrRef o.definitionOrRef)
                | Asn1AcnAst.SequenceOf o   -> Some (l.lg.definitionOrRef o.definitionOrRef)
                | Asn1AcnAst.Real _ | Asn1AcnAst.IA5String _ | Asn1AcnAst.NumericString _ | Asn1AcnAst.NullType _
                | Asn1AcnAst.Boolean _ | Asn1AcnAst.Enumerated _ | Asn1AcnAst.ObjectIdentifier _ | Asn1AcnAst.TimeType _
                | Asn1AcnAst.Sequence _ | Asn1AcnAst.Choice _ | Asn1AcnAst.ReferenceType _ -> None)
        match tasDefinition with
        | Some (TypeDefinition td)                   -> Some td
        | Some (ReferenceToExistingDefinition _)
        | None                                       -> None
    | ReferenceToExistingDefinition _, _       -> None

/// Coverage step 14i (E2): the language type of a value of this type holds exactly one value, so two values of it are
/// always equal. Only where the typedefs enforce ranges (Ada): an INTEGER whose typedef is the ranged subtype
/// `range a .. a` (DAstTypeDefinition.createInteger declares the uPER range; a subtype narrows it), an ENUMERATED type
/// with one item (the enumeration has only that literal; a constrained subtype of a wider ENUMERATED keeps all items).
let typeHoldsOneValue (r:Asn1AcnAst.AstRoot) (l:LanguageMacros) (t:Asn1AcnAst.Asn1Type) =
    match l.lg.typedefConstraintModel with
    | TypedefsAreBaseTypes  -> false
    | TypedefsEnforceRanges ->
        let at = t.ActualType
        match at.Kind with
        | Asn1AcnAst.Integer o ->
            match o.uperRange with
            | Concrete (a, b) when a = b -> (declaringTypedef r l at (l.lg.definitionOrRef o.definitionOrRef)).IsSome
            | Concrete _ | NegInf _ | PosInf _ | Full -> false
        | Asn1AcnAst.Enumerated o -> o.items.Length = 1
        | Asn1AcnAst.Real _ | Asn1AcnAst.IA5String _ | Asn1AcnAst.NumericString _ | Asn1AcnAst.OctetString _
        | Asn1AcnAst.NullType _ | Asn1AcnAst.BitString _ | Asn1AcnAst.Boolean _ | Asn1AcnAst.ObjectIdentifier _
        | Asn1AcnAst.TimeType _ | Asn1AcnAst.SequenceOf _ | Asn1AcnAst.Sequence _ | Asn1AcnAst.Choice _
        | Asn1AcnAst.ReferenceType _ -> false

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
        acnParameters = %A
        minLengthInBytes = %A
        maxLengthInBytes = %A
        rows =
            %A
        """ icdTypeAss.typeId.AsString icdTypeAss.acnLink icdTypeAss.asn1Link  icdTypeAss.name icdTypeAss.kind icdTypeAss.comments icdTypeAss.acnParameters icdTypeAss.minLengthInBytes icdTypeAss.maxLengthInBytes rws
