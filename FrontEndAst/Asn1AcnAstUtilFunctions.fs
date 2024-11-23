module Asn1AcnAstUtilFunctions

open System.Numerics
open Antlr.Runtime.Tree
open Antlr.Runtime
open System
open FsUtils
open CommonTypes
open Asn1AcnAst



let toByte sizeInBits =
    sizeInBits/8I + (if sizeInBits % 8I = 0I then 0I else 1I)

type Asn1TypeKind with
    member this.ActualType =
        match this with
        | ReferenceType t -> t.resolvedType.Kind.ActualType
        | _ -> this

    member this.uperMinSizeInBits =
        match this with
        | Integer        x -> x.uperMinSizeInBits
        | Real           x -> x.uperMinSizeInBits
        | IA5String      x -> x.uperMinSizeInBits
        | NumericString  x -> x.uperMinSizeInBits
        | OctetString    x -> x.uperMinSizeInBits
        | TimeType       x -> x.uperMinSizeInBits
        | NullType       x -> x.uperMinSizeInBits
        | BitString      x -> x.uperMinSizeInBits
        | Boolean        x -> x.uperMinSizeInBits
        | Enumerated     x -> x.uperMinSizeInBits
        | SequenceOf     x -> x.uperMinSizeInBits
        | Sequence       x -> x.uperMinSizeInBits
        | Choice         x -> x.uperMinSizeInBits
        | ObjectIdentifier x -> x.uperMinSizeInBits
        | ReferenceType  x -> x.uperMinSizeInBits


    member this.uperMaxSizeInBits =
        match this with
        | Integer        x -> x.uperMaxSizeInBits
        | Real           x -> x.uperMaxSizeInBits
        | IA5String      x -> x.uperMaxSizeInBits
        | NumericString  x -> x.uperMaxSizeInBits
        | OctetString    x -> x.uperMaxSizeInBits
        | TimeType       x -> x.uperMaxSizeInBits
        | NullType       x -> x.uperMaxSizeInBits
        | BitString      x -> x.uperMaxSizeInBits
        | Boolean        x -> x.uperMaxSizeInBits
        | Enumerated     x -> x.uperMaxSizeInBits
        | SequenceOf     x -> x.uperMaxSizeInBits
        | Sequence       x -> x.uperMaxSizeInBits
        | Choice         x -> x.uperMaxSizeInBits
        | ObjectIdentifier x -> x.uperMaxSizeInBits
        | ReferenceType  x -> x.uperMaxSizeInBits

    member this.acnMinSizeInBits =
        match this with
        | Integer        x -> x.acnMinSizeInBits
        | Real           x -> x.acnMinSizeInBits
        | IA5String      x -> x.acnMinSizeInBits
        | NumericString  x -> x.acnMinSizeInBits
        | OctetString    x -> x.acnMinSizeInBits
        | TimeType       x -> x.acnMinSizeInBits
        | NullType       x -> x.acnMinSizeInBits
        | BitString      x -> x.acnMinSizeInBits
        | Boolean        x -> x.acnMinSizeInBits
        | Enumerated     x -> x.acnMinSizeInBits
        | SequenceOf     x -> x.acnMinSizeInBits
        | Sequence       x -> x.acnMinSizeInBits
        | Choice         x -> x.acnMinSizeInBits
        | ObjectIdentifier x -> x.acnMinSizeInBits
        | ReferenceType  x -> x.acnMinSizeInBits

    member this.acnMaxSizeInBits =
        match this with
        | Integer        x -> x.acnMaxSizeInBits
        | Real           x -> x.acnMaxSizeInBits
        | IA5String      x -> x.acnMaxSizeInBits
        | NumericString  x -> x.acnMaxSizeInBits
        | OctetString    x -> x.acnMaxSizeInBits
        | TimeType       x -> x.acnMaxSizeInBits
        | NullType       x -> x.acnMaxSizeInBits
        | BitString      x -> x.acnMaxSizeInBits
        | Boolean        x -> x.acnMaxSizeInBits
        | Enumerated     x -> x.acnMaxSizeInBits
        | SequenceOf     x -> x.acnMaxSizeInBits
        | Sequence       x -> x.acnMaxSizeInBits
        | Choice         x -> x.acnMaxSizeInBits
        | ObjectIdentifier x -> x.acnMaxSizeInBits
        | ReferenceType  x -> x.acnMaxSizeInBits

type Asn1Type with
    member this.uperMinSizeInBits = this.Kind.uperMinSizeInBits

    member this.uperMaxSizeInBits = this.Kind.uperMaxSizeInBits

    member this.acnMinSizeInBits = this.Kind.acnMinSizeInBits

    member this.acnMaxSizeInBits = this.Kind.acnMaxSizeInBits

    member this.maxSizeInBits (enc: Asn1Encoding): BigInteger =
        match enc with
        | UPER -> this.uperMaxSizeInBits
        | ACN -> this.acnMaxSizeInBits
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

    member this.ActualType =
        match this.Kind with
        | ReferenceType t-> t.resolvedType.ActualType
        | _ -> this


    member this.isComplexType =
        match this.Kind with
        | ReferenceType t-> t.resolvedType.isComplexType
        | Integer      _ -> false
        | Real         _ -> false
        | IA5String    _ -> false
        | NumericString _ -> false
        | OctetString  _ -> false
        | NullType     _ -> false
        | TimeType     _ -> false
        | BitString    _ -> false
        | Boolean      _ -> false
        | Enumerated   _ -> false
        | SequenceOf   _ -> true
        | Sequence     _ -> true
        | Choice       _ -> true
        | ObjectIdentifier _ -> false

    member this.isStringType =
        match this.Kind with
        | IA5String    _
        | NumericString _ -> true
        | ReferenceType t-> t.resolvedType.isStringType
        | _             -> false



    member this.FT_TypeDefinition =
        match this.Kind with
        | Integer      o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | ObjectIdentifier      o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | Real         o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | IA5String    o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_StringTypeDefinition d)) |> Map.ofList
        | NumericString o -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_StringTypeDefinition d)) |> Map.ofList
        | OctetString  o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | TimeType     o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | NullType     o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | BitString    o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | Boolean      o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | Enumerated   o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_EnumeratedTypeDefinition d)) |> Map.ofList
        | SequenceOf   o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | Sequence     o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_SequenceTypeDefinition d)) |> Map.ofList
        | Choice       o  -> o.typeDef |> Map.toList |> List.map (fun (l, d) -> (l, FE_ChoiceTypeDefinition d)) |> Map.ofList
        | ReferenceType o->  o.typeDef

    member this.SaveBitStreamPosition =
        match this.Kind with
        | ReferenceType t-> t.resolvedType.SaveBitStreamPosition
        | Integer      _ -> false
        | Real         _ -> false
        | IA5String    _ -> false
        | NumericString _ ->false
        | OctetString  _ -> false
        | TimeType     _ -> false
        | NullType     o -> o.acnProperties.savePosition
        | BitString    _ -> false
        | Boolean      _ -> false
        | Enumerated   _ -> false
        | SequenceOf   _ -> false
        | Sequence     _ -> false
        | Choice       _ -> false
        | ObjectIdentifier _ -> false


//    member this.tasInfo =
//        match this.typeAssignmentInfo with
//        | Some (TypeAssignmentInfo tasInfo)  -> Some tasInfo
//        | Some (ValueAssignmentInfo tasInfo)  -> None
//        | None          ->
//            match this.inheritInfo with
//            | Some tasInfo  -> Some tasInfo.AsTasInfo
//            | None          -> None

type AcnInsertedType with
    member this.acnMinSizeInBits =
        match this with
        | AcnInteger        x -> x.acnMinSizeInBits
        | AcnNullType       x -> x.acnMinSizeInBits
        | AcnBoolean        x -> x.acnMinSizeInBits
        | AcnReferenceToEnumerated x -> x.enumerated.acnMinSizeInBits
        | AcnReferenceToIA5String x -> x.str.acnMinSizeInBits
    member this.acnMaxSizeInBits =
        match this with
        | AcnInteger        x -> x.acnMaxSizeInBits
        | AcnNullType       x -> x.acnMaxSizeInBits
        | AcnBoolean        x -> x.acnMaxSizeInBits
        | AcnReferenceToEnumerated x -> x.enumerated.acnMaxSizeInBits
        | AcnReferenceToIA5String x -> x.str.acnMaxSizeInBits
    member this.acnAlignment =
        match this with
        | AcnInteger        x -> x.acnAlignment
        | AcnNullType       x -> x.acnAlignment
        | AcnBoolean        x -> x.acnAlignment
        | AcnReferenceToEnumerated x -> x.acnAlignment
        | AcnReferenceToIA5String x -> x.acnAlignment
    member this.Location =
        match this with
        | AcnInteger        x -> x.Location
        | AcnNullType       x -> x.Location
        | AcnBoolean        x -> x.Location
        | AcnReferenceToEnumerated x -> x.tasName.Location
        | AcnReferenceToIA5String x -> x.tasName.Location

type BitString with
    member this.MaxOctets = int (ceil ((double this.maxSize.uper)/8.0))

type Asn1Child with
    member this.getBackendName0 l =
        match l with
        | CommonTypes.C         -> this._c_name
        | CommonTypes.Scala     -> this._scala_name
        | CommonTypes.Ada       -> this._ada_name

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

type SeqChildInfo with
    member this.Name =
        match this with
        | Asn1Child x   -> x.Name
        | AcnChild  x   -> x.Name

    member this.acnMinSizeInBits =
        match this with
        | Asn1Child x -> x.acnMinSizeInBits
        | AcnChild  x -> x.Type.acnMinSizeInBits
    member this.acnMaxSizeInBits =
        match this with
        | Asn1Child x -> x.acnMaxSizeInBits
        | AcnChild  x -> x.Type.acnMaxSizeInBits
    member this.acnAlignment =
        match this with
        | Asn1Child x   -> x.Type.acnAlignment
        | AcnChild  x   -> x.Type.acnAlignment
    member this.Optionality =
        match this with
        | Asn1Child x   -> x.Optionality
        | AcnChild  x   -> None

    member this.maxSizeInBits (enc: Asn1Encoding): BigInteger =
        match enc with
        | UPER ->
            match this with
            | Asn1Child x -> x.uperMaxSizeInBits
            | AcnChild x -> raise (BugErrorException $"Unexpected UPER encoding for ACN child {x.Name}")
        | ACN -> this.acnMaxSizeInBits
        | _ -> raise (BugErrorException $"Unexpected encoding: {enc}")

let rec getASN1Name  (t:Asn1Type) =
    match t.Kind with
    | Integer       _  -> "INTEGER"
    | Real          _  -> "REAL"
    | IA5String     _  -> "IA5String"
    | NumericString _  -> "NumericString"
    | OctetString   _  -> "OCTET STRING"
    | NullType      _  -> "NULL"
    | BitString     _  -> "BIT STRING"
    | Boolean       _  -> "BOOLEAN"
    | Enumerated    _  -> "ENUMERATED"
    | SequenceOf    _  -> "SEQUENCE OF"
    | Sequence      _  -> "SEQUENCE"
    | Choice        _  -> "CHOICE"
    | TimeType      _  -> "TIME"
    | ObjectIdentifier o when o.relativeObjectId    -> "RELATIVE-OID"
    | ObjectIdentifier _    -> "OBJECT IDENTIFIER"
    | ReferenceType r  -> getASN1Name r.resolvedType


type AcnReferenceToEnumerated with
    member this.getType (r:AstRoot) =
        match r.Files |> Seq.collect(fun f -> f.Modules) |> Seq.tryFind (fun m -> m.Name.Value = this.modName.Value) with
        | None  -> raise (SemanticError(this.modName.Location, (sprintf "No module defined with name '%s'" this.modName.Value)))
        | Some m ->
            match m.TypeAssignments |> Seq.tryFind(fun ts -> ts.Name.Value = this.tasName.Value) with
            | None -> raise (SemanticError(this.modName.Location, (sprintf "No type assignment with name '%s' is defined in mode '%s'" this.tasName.Value this.modName.Value)))
            | Some tas -> tas.Type


let getIntEncodingClassByUperRange (args:CommandLineSettings) (uperRange:BigIntegerUperRange) =
    let int64  = ASN1SCC_Int64 (BigInteger System.Int64.MinValue,  BigInteger System.Int64.MaxValue)
    let int32  = ASN1SCC_Int32 (BigInteger System.Int32.MinValue,  BigInteger System.Int32.MaxValue)
    let int16  = ASN1SCC_Int16 (BigInteger System.Int16.MinValue,  BigInteger System.Int16.MaxValue)
    let  int8  = ASN1SCC_Int8  (BigInteger System.SByte.MinValue,  BigInteger System.SByte.MaxValue)

    let uint64 = ASN1SCC_UInt64 (0I,  BigInteger System.UInt64.MaxValue)
    let uint32 = ASN1SCC_UInt32 (0I,  BigInteger System.UInt32.MaxValue)
    let uint16 = ASN1SCC_UInt16 (0I,  BigInteger System.UInt16.MaxValue)
    let  uint8 = ASN1SCC_UInt8  (0I,  BigInteger System.Byte.MaxValue)

    let fat_uint = ASN1SCC_UInt (0I,  args.UIntMax)
    let fat_int = ASN1SCC_Int (args.SIntMin,  args.SIntMax)

    let getUClass (x:BigInteger) =
        match args.slim with
        | true ->
            if   x > BigInteger System.UInt32.MaxValue then uint64
            elif x > BigInteger System.UInt16.MaxValue then uint32
            elif x > BigInteger System.Byte.MaxValue then   uint16
            else uint8
        | false -> fat_uint
    let getSClass (a:BigInteger) (b:BigInteger)=
        match args.slim with
        | true ->
            if   BigInteger System.SByte.MinValue <= a && b <= BigInteger System.SByte.MaxValue then int8
            elif BigInteger System.Int16.MinValue <= a && b <= BigInteger System.Int16.MaxValue then int16
            elif BigInteger System.Int32.MinValue <= a && b <= BigInteger System.Int32.MaxValue then int32
            else
                int64
        | false -> (ASN1SCC_Int (args.SIntMin,  args.SIntMax))

    let foo slim8 slim4 fat =
        match args.slim with
        | true -> if args.integerSizeInBytes = 8I then slim8 else slim4
        | false -> fat

    match uperRange with
    | Concrete  (a,b) when a >= 0I -> getUClass b
    | Concrete  (a,b)              -> getSClass a b
    | NegInf    _                  -> foo int64 int32 fat_int
    | PosInf   a when a >= 0I      -> foo uint64 uint32 fat_uint
    | PosInf  _                    -> foo int64 int32 fat_int
    | Full                         -> foo int64 int32 fat_int


type Integer with
    member this.AllCons  = this.cons@this.withcons
    //member this.getClass (args:CommandLineSettings)  =   getIntEncodingClassByUperRange args this.uperRange

type IntegerClass with
    member this.Min =
        match this with
        | ASN1SCC_Int8      (a,_) -> a
        | ASN1SCC_Int16     (a,_) -> a
        | ASN1SCC_Int32     (a,_) -> a
        | ASN1SCC_Int64     (a,_) -> a
        | ASN1SCC_Int       (a,_) -> a
        | ASN1SCC_UInt8     (a,_) -> a
        | ASN1SCC_UInt16    (a,_) -> a
        | ASN1SCC_UInt32    (a,_) -> a
        | ASN1SCC_UInt64    (a,_) -> a
        | ASN1SCC_UInt      (a,_) -> a
    member this.Max =
        match this with
        | ASN1SCC_Int8      (_,b) -> b
        | ASN1SCC_Int16     (_,b) -> b
        | ASN1SCC_Int32     (_,b) -> b
        | ASN1SCC_Int64     (_,b) -> b
        | ASN1SCC_Int       (_,b) -> b
        | ASN1SCC_UInt8     (_,b) -> b
        | ASN1SCC_UInt16    (_,b) -> b
        | ASN1SCC_UInt32    (_,b) -> b
        | ASN1SCC_UInt64    (_,b) -> b
        | ASN1SCC_UInt      (_,b) -> b
    member this.IsPositive =
        match this with
        | ASN1SCC_Int8      (_) -> false
        | ASN1SCC_Int16     (_) -> false
        | ASN1SCC_Int32     (_) -> false
        | ASN1SCC_Int64     (_) -> false
        | ASN1SCC_Int       (_) -> false
        | ASN1SCC_UInt8     (_) -> true
        | ASN1SCC_UInt16    (_) -> true
        | ASN1SCC_UInt32    (_) -> true
        | ASN1SCC_UInt64    (_) -> true
        | ASN1SCC_UInt      (_) -> true


let getAcnIntegerClass (args:CommandLineSettings) (i:AcnInteger) =
    getIntEncodingClassByUperRange args i.uperRange

type ObjectIdentifier with
    member this.AllCons  = this.cons@this.withcons

type TimeType with
    member this.AllCons  = this.cons@this.withcons


type Real             with
    member this.AllCons  = this.cons@this.withcons
    member this.getClass (args:CommandLineSettings)  =
        match args.slim with
        | true ->
            match this.acnEncodingClass with
            | Real_uPER                         -> ASN1SCC_REAL
            | Real_IEEE754_32_big_endian        -> ASN1SCC_FP32
            | Real_IEEE754_64_big_endian        -> ASN1SCC_FP64
            | Real_IEEE754_32_little_endian     -> ASN1SCC_FP32
            | Real_IEEE754_64_little_endian     -> ASN1SCC_FP64
        | false -> ASN1SCC_REAL


type StringType       with
    member this.AllCons  = this.cons@this.withcons
    member this.isFixedSize = this.minSize.uper = this.maxSize.uper
    member this.isVariableSize = not this.isFixedSize


type OctetString      with
    member this.AllCons  = this.cons@this.withcons
    member this.isFixedSize = this.minSize.uper = this.maxSize.uper
    member this.isVariableSize = not this.isFixedSize

type NullType         with
    member this.AllCons  = []

type BitString        with
    member this.AllCons  = this.cons@this.withcons
    member this.isFixedSize = this.minSize.uper = this.maxSize.uper
    member this.isVariableSize = not this.isFixedSize

type Boolean          with
    member this.AllCons  = this.cons@this.withcons

type Enumerated       with
    member this.AllCons  = this.cons@this.withcons

type SequenceOf       with
    member this.AllCons  = this.cons@this.withcons
    member this.isFixedSize = this.minSize.uper = this.maxSize.uper
    member this.isVariableSize = not this.isFixedSize

type Sequence         with
    member this.AllCons  = this.cons@this.withcons

type Choice           with
    member this.AllCons  = this.cons@this.withcons

//type ReferenceType    with
//    member this.AllCons  = this.cons@this.withcons


type Asn1Value with
    member this.getBackendName () =
        "unnamed_variable"



type Asn1OrAcnOrPrmType =
    | ACN_INSERTED_TYPE of AcnInsertedType
    | ASN1_TYPE         of Asn1Type
    | ACN_PARAMETER     of Asn1Type*AcnGenericTypes.AcnParameter

(*
let locateTypeByRefId (r:AstRoot) (ReferenceToType nodes) =
    let origPath = ReferenceToType nodes
    let rec locateType (parent:Asn1OrAcnOrPrmType) (nodes:ScopeNode list) =
        match nodes with
        | []                        -> parent
        | (SEQ_CHILD chName)::rest  ->
            match parent with
            | ASN1_TYPE t ->
                match t.Kind with
                | Sequence  seq ->
                    match seq.children |> Seq.tryFind(fun ch -> ch.Name.Value = chName) with
                    | Some (Asn1Child x)   -> locateType (ASN1_TYPE x.Type) rest
                    | Some (AcnChild  x)   -> locateType (ACN_INSERTED_TYPE x.Type) rest
                    | None      -> raise(UserException(sprintf "Invalid child name '%s'" chName ))
                | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
            | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
        | (CH_CHILD (chName, _))::rest  ->
            match parent with
            | ASN1_TYPE t ->
                match t.Kind with
                | Choice  choice ->
                    match choice.children |> Seq.tryFind(fun ch -> ch.Name.Value = chName) with
                    | Some x   -> locateType (ASN1_TYPE x.Type) rest
                    | None     -> raise(UserException(sprintf "Invalid child name '%s'" chName ))
                | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
            | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
        | SQF::rest ->
            match parent with
            | ASN1_TYPE t ->
                match t.Kind with
                | SequenceOf sqof -> locateType (ASN1_TYPE sqof.child) rest
                | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
            | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
        | (PRM prmName)::[] ->
            match parent with
            | ASN1_TYPE t ->
                match t.acnParameters |> Seq.tryFind(fun p -> p.name = prmName) with
                | Some p    -> ACN_PARAMETER (t, p)
                | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
            | _  -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))
        | _     -> raise(UserException(sprintf "Invalid path '%s'" origPath.AsString ))

    match nodes with
    | (MD mdName)::(TA tasName)::restPath    ->
        let md =
            match r.Files |> List.collect(fun f -> f.Modules) |> Seq.tryFind (fun m -> m.Name.Value = mdName) with
            | Some md -> md
            | None    -> raise(UserException(sprintf "Invalid module name '%s'" mdName ))
        let tas =
            match md.TypeAssignments |> Seq.tryFind(fun tas -> tas.Name.Value = tasName) with
            | Some tas -> tas
            | None    -> raise(UserException(sprintf "Invalid tas name '%s'" tasName ))
        locateType (ASN1_TYPE tas.Type) restPath
    | _                 -> raise(UserException(sprintf "Invalid module name " ))

*)

type AstRoot with
    member r.Modules = r.Files |> List.collect(fun f -> f.Modules)
    member r.GetModuleByName(name:StringLoc)  =
        let (n,loc) = name.AsTupple
        match r.Modules |> Seq.tryFind( fun m -> m.Name = name)  with
        | Some(m) -> m
        | None    -> raise(SemanticError(loc, sprintf "No Module Defined with name: %s" n ))

type Asn1Module with
    member this.ExportedTypes =
        match this.Exports with
        | Asn1Ast.All   ->
            let importedTypes = this.Imports |> List.collect(fun imp -> imp.Types) |> List.map(fun x -> x.Value)
            (this.TypeAssignments |> List.map(fun x -> x.Name.Value))@importedTypes
        | Asn1Ast.OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> System.Char.IsUpper (x.Chars 0))
    member this.ExportedValues =
        match this.Exports with
        | Asn1Ast.All   -> this.ValueAssignments |> List.map(fun x -> x.Name.Value)
        | Asn1Ast.OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> not (System.Char.IsUpper (x.Chars 0)))

    member m.TryGetTypeAssignmentByName name (r:AstRoot) =
        match m.TypeAssignments|> Seq.tryFind(fun x -> x.Name = name) with
        | Some t   -> Some t
        | None      ->
            let othMods = m.Imports |> Seq.filter(fun imp -> imp.Types |> Seq.exists((=) name))
                                |> Seq.map(fun imp -> imp.Name) |> Seq.toList
            match othMods with
            | firstMod::_   ->
                match r.Modules |> Seq.tryFind( fun m -> m.Name = firstMod)  with
                | Some(m) -> m.TryGetTypeAssignmentByName name r
                | None    -> None
            | []            -> None

    member m.GetTypeAssignmentByName name (r:AstRoot) =
        match m.TypeAssignments|> Seq.tryFind(fun x -> x.Name = name) with
        | Some(t)   -> t
        | None      ->
            let othMods = m.Imports |> Seq.filter(fun imp -> imp.Types |> Seq.exists((=) name))
                                |> Seq.map(fun imp -> imp.Name) |> Seq.toList
            match othMods with
            | firstMod::tail   -> r.GetModuleByName(firstMod).GetTypeAssignmentByName name r
            | []               ->
                let (n,loc) = name.AsTupple
                raise(SemanticError(loc, sprintf "No Type Assignment with name: %s is defined in Module %s" n m.Name.Value))
    member m.GetValueAsigByName(name:StringLoc) (r:AstRoot) =
        let (n,loc) = name.AsTupple
        let value = m.ValueAssignments |> Seq.tryFind(fun x -> x.Name = name)
        match value with
        | Some(v)       -> v
        | None          ->
            let othMods = m.Imports
                          |> Seq.filter(fun imp -> imp.Values |> Seq.exists(fun vname -> vname = name))
                          |> Seq.map(fun imp -> imp.Name) |> Seq.toList
            match othMods with
            | firstMod::tail   -> r.GetModuleByName(firstMod).GetValueAsigByName name r
            | []               -> raise (SemanticError(loc, sprintf "No value assignment with name '%s' exists" n))

let rec GetActualType (t:Asn1Type) (r:AstRoot) =
    match t.Kind with
    | ReferenceType ref ->
        let newmod = r.GetModuleByName(ref.modName)
        let tas = newmod.GetTypeAssignmentByName ref.tasName r
        GetActualType tas.Type r
    | _                         -> t

let GetActualTypeByName (r:AstRoot) modName tasName  =
    let mdl = r.GetModuleByName(modName)
    let tas = mdl.GetTypeAssignmentByName tasName r
    GetActualType tas.Type r

type Asn1Type with
    member this.getBaseType  (r:AstRoot) =
        match this.inheritInfo with
        | None          -> None
        | Some inf      ->
            let mdl = r.GetModuleByName(inf.modName.AsLoc)
            let tas = mdl.GetTypeAssignmentByName inf.tasName.AsLoc r
            match tas.Type.inheritInfo with
            | None  -> Some tas.Type
            | Some _ -> tas.Type.getBaseType r
