/// The bounds a decoder checks for a size read from an external ACN field (moved from Acn/AcnExternalField.fs in
/// coverage step 14c2b, unchanged: DecodeOutcome needs the same decision as the producers, AcnOctetBitStrings /
/// AcnSequenceOf, and is compiled before them).
module SizeDeterminantBounds

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions

/// Whether the expression getExternalField gives for the size determinant of asn1TypeIdWithDependency has a signed
/// type, so a decoded size may be negative and the decoder must check the lower bound also for SIZE(0..max)
/// (q/issues.md Issue 64: a twos-complement size field passed `len <= max` with a negative value; Ada then raised
/// Constraint_Error, or with -gnatp used the negative length). The local of a signed ACN INTEGER field (legacy follows
/// the parameter chain to it, as getExternalField0 does); an --acn-v2 parameter is an AcnInsertedFieldRef whose value
/// is unsigned (a negative field is stored as its two's complement and fails `<= max`).
let extFieldMayBeNegative (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (asn1TypeIdWithDependency: ReferenceToType) : bool =
    let rec rootField (det: Determinant) : Asn1AcnAst.AcnChild option =
        match det with
        | AcnChildDeterminant child -> Some child
        | AcnParameterDeterminant prm ->
            match r.args.acnDeferred with
            | true  -> None
            | false ->
                deps.acnDependencies
                |> List.tryPick (fun d ->
                    match d.dependencyKind with
                    | AcnDepRefTypeArgument p when p.id = prm.id -> Some d.determinant
                    | _ -> None)
                |> Option.bind rootField
    match deps.acnDependencies |> List.tryFind (fun d -> d.asn1Type = asn1TypeIdWithDependency) with
    | None -> false
    | Some dependency ->
        match rootField dependency.determinant |> Option.map (fun c -> c.Type) with
        | Some (AcnInsertedType.AcnInteger int) -> not int.isUnsigned
        | Some (AcnInsertedType.AcnNullType _ | AcnInsertedType.AcnBoolean _ | AcnInsertedType.AcnReferenceToEnumerated _ | AcnInsertedType.AcnReferenceToIA5String _)
        | None -> false

/// The lower bound a decoder checks for a size read from an external field: the SIZE minimum, or 0 when the field may
/// be negative (Issue 64); None when there is nothing to check.
let extFieldSizeMin (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (asn1TypeIdWithDependency: ReferenceToType) (minSize: System.Numerics.BigInteger) : System.Numerics.BigInteger option =
    match minSize = 0I, extFieldMayBeNegative r deps asn1TypeIdWithDependency with
    | false, _    -> Some minSize
    | true, true  -> Some 0I
    | true, false -> None

//The largest value the decoder can return for the size determinant of asn1TypeIdWithDependency,
//or None when its encoding does not bound it. The bound comes from the encoding width, not from
//the ASN.1 constraint of the determinant (uperRange): the C and Rust fixed-size integer decoders
//return every value of the width (ESACERT #74995). When the determinant is an ACN parameter,
//every argument passed to it must be bounded.
let getSizeDeterminantWireMax (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (asn1TypeIdWithDependency: ReferenceToType) : System.Numerics.BigInteger option =
    let unsignedMax (nBits:System.Numerics.BigInteger) = Some (System.Numerics.BigInteger.Pow(2I, int nBits) - 1I)
    let signedMax (nBits:System.Numerics.BigInteger) = Some (System.Numerics.BigInteger.Pow(2I, int nBits - 1) - 1I)
    let encodingMax (int: Asn1AcnAst.AcnInteger) =
        match int.acnProperties.mappingFunction with
        | Some _ -> None
        | None   ->
            match int.acnEncodingClass with
            | PositiveInteger_ConstSize_8                  -> unsignedMax 8I
            | PositiveInteger_ConstSize_big_endian_16      -> unsignedMax 16I
            | PositiveInteger_ConstSize_little_endian_16   -> unsignedMax 16I
            | PositiveInteger_ConstSize_big_endian_32      -> unsignedMax 32I
            | PositiveInteger_ConstSize_little_endian_32   -> unsignedMax 32I
            | PositiveInteger_ConstSize_big_endian_64      -> unsignedMax 64I
            | PositiveInteger_ConstSize_little_endian_64   -> unsignedMax 64I
            | PositiveInteger_ConstSize nBits              -> unsignedMax nBits
            | TwosComplement_ConstSize_8                   -> signedMax 8I
            | TwosComplement_ConstSize_big_endian_16       -> signedMax 16I
            | TwosComplement_ConstSize_little_endian_16    -> signedMax 16I
            | TwosComplement_ConstSize_big_endian_32       -> signedMax 32I
            | TwosComplement_ConstSize_little_endian_32    -> signedMax 32I
            | TwosComplement_ConstSize_big_endian_64       -> signedMax 64I
            | TwosComplement_ConstSize_little_endian_64    -> signedMax 64I
            | TwosComplement_ConstSize nBits               -> signedMax nBits
            | Integer_uPER
            | ASCII_ConstSize _
            | ASCII_VarSize_NullTerminated _
            | ASCII_UINT_ConstSize _
            | ASCII_UINT_VarSize_NullTerminated _
            | BCD_ConstSize _
            | BCD_VarSize_NullTerminated _                 -> None
    let determinantMax (det: Determinant) =
        match det with
        | AcnChildDeterminant child ->
            match child.Type with
            | AcnInsertedType.AcnInteger int              -> encodingMax int
            | AcnInsertedType.AcnNullType _
            | AcnInsertedType.AcnBoolean _
            | AcnInsertedType.AcnReferenceToEnumerated _
            | AcnInsertedType.AcnReferenceToIA5String _   -> None
        | AcnParameterDeterminant _ -> None
    let dependency = deps.acnDependencies |> List.find(fun d -> d.asn1Type = asn1TypeIdWithDependency)
    let nodes = match dependency.determinant.id with ReferenceToType nodes -> nodes
    let candidates =
        match nodes |> List.last with
        | PRM _ ->
            let args =
                deps.acnDependencies |>
                List.choose(fun d ->
                    match d.dependencyKind with
                    | AcnDepRefTypeArgument prm when prm.id = dependency.determinant.id -> Some d.determinant
                    | _ -> None)
            match args with
            | [] -> [dependency.determinant]
            | _  -> args
        | _ -> [dependency.determinant]
    candidates |> List.fold (fun acc det ->
        match acc, determinantMax det with
        | Some m1, Some m2 -> Some (max m1 m2)
        | _                -> None) (determinantMax candidates.Head)

/// The checks of an OCTET / BIT STRING whose size is an external field (oct_external_field / bit_string_external_field):
/// the lower bound (extFieldSizeMin) and the upper bound, omitted when the field's encoding cannot exceed the SIZE
/// maximum (issue #360, ESACERT #74995: the bound comes from the encoding width). (noSizeMin, noSizeMax).
let externalSizeChecks (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (asn1TypeIdWithDependency: ReferenceToType) (minSize: System.Numerics.BigInteger) (maxSize: System.Numerics.BigInteger) : System.Numerics.BigInteger option * System.Numerics.BigInteger option =
    let noSizeMin = extFieldSizeMin r deps asn1TypeIdWithDependency minSize
    let noSizeMax =
        match getSizeDeterminantWireMax deps asn1TypeIdWithDependency with
        | Some detMax when detMax <= maxSize -> None
        | _ -> Some maxSize
    noSizeMin, noSizeMax
