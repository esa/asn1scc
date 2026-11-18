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

/// Coverage cycle 15a (A-str), positive rule: the values the Ada read of the ACN INTEGER field `fld` returns, as (low,
/// high), when the read's RTL Post states them (`readPostsBound`, ILangGeneric.acnIntReadPostsBoundValue); None otherwise.
/// Whitelist (no mapping function, an integer class without -slim narrowing, i.e. the unsuffixed RTL procedures):
/// - Integer_uPER with a concrete range [a, b] and no extension marker: UPER_Dec_ConstraintWholeNumber /
///   UPER_Dec_ConstraintPosWholeNumber (Post: Success and IntVal in MinVal .. MaxVal, or not Success and IntVal = MinVal),
///   their _FullRange twins (Post: MinVal <= IntVal <= MinVal + 2**nBits - 1, the whole range), IntNoneRequired (a = b,
///   assigned): [a, b];
/// - PositiveInteger_ConstSize* / TwosComplement_ConstSize*: Acn_Dec_Int_PositiveInteger_ConstSize* /
///   Acn_Dec_Int_TwosComplement_ConstSize* (Post: Success and IntVal in minVal .. maxVal, or not Success and IntVal = minVal;
///   minVal .. maxVal = the field's range within the encoding width) and their _FullRange twins (Post: IntVal within the
///   encoding width, used when the field's range holds the whole width): the width, intersected with a concrete range.
/// Not whitelisted: ASCII / BCD encodings, Integer_uPER with an open range, mapping functions, and every narrowed
/// (-slim) class (Int8 .. Int32, UInt8 .. UInt32: suffixed procedures such as _FullRangeUInt32, whose Post states no
/// bound).
let private acnIntReadInterval (readPostsBound: bool) (fld: Asn1AcnAst.AcnInteger) : (System.Numerics.BigInteger * System.Numerics.BigInteger) option =
    let unsigned (nBits:System.Numerics.BigInteger) = Some (0I, System.Numerics.BigInteger.Pow(2I, int nBits) - 1I)
    let signed (nBits:System.Numerics.BigInteger) = Some (-System.Numerics.BigInteger.Pow(2I, int nBits - 1), System.Numerics.BigInteger.Pow(2I, int nBits - 1) - 1I)
    let unsuffixedClass =
        match fld.intClass with
        | ASN1SCC_Int64 _ | ASN1SCC_Int _ | ASN1SCC_UInt64 _ | ASN1SCC_UInt _ -> true
        | ASN1SCC_Int8 _ | ASN1SCC_Int16 _ | ASN1SCC_Int32 _
        | ASN1SCC_UInt8 _ | ASN1SCC_UInt16 _ | ASN1SCC_UInt32 _ -> false
    let noExtension =
        fld.cons |> List.forall (fun c ->
            match c with
            | RangeRootConstraint _ | RangeRootConstraint2 _ -> false
            | RangeUnionConstraint _ | RangeIntersectionConstraint _ | RangeAllExceptConstraint _ | RangeExceptConstraint _
            | RangeSingleValueConstraint _ | RangeConstraint _ | RangeConstraint_val_MAX _ | RangeConstraint_MIN_val _ -> true)
    let width =
        match fld.acnEncodingClass with
        | PositiveInteger_ConstSize_8                  -> unsigned 8I
        | PositiveInteger_ConstSize_big_endian_16      -> unsigned 16I
        | PositiveInteger_ConstSize_little_endian_16   -> unsigned 16I
        | PositiveInteger_ConstSize_big_endian_32      -> unsigned 32I
        | PositiveInteger_ConstSize_little_endian_32   -> unsigned 32I
        | PositiveInteger_ConstSize_big_endian_64      -> unsigned 64I
        | PositiveInteger_ConstSize_little_endian_64   -> unsigned 64I
        | PositiveInteger_ConstSize nBits              -> unsigned nBits
        | TwosComplement_ConstSize_8                   -> signed 8I
        | TwosComplement_ConstSize_big_endian_16       -> signed 16I
        | TwosComplement_ConstSize_little_endian_16    -> signed 16I
        | TwosComplement_ConstSize_big_endian_32       -> signed 32I
        | TwosComplement_ConstSize_little_endian_32    -> signed 32I
        | TwosComplement_ConstSize_big_endian_64       -> signed 64I
        | TwosComplement_ConstSize_little_endian_64    -> signed 64I
        | TwosComplement_ConstSize nBits               -> signed nBits
        | Integer_uPER
        | ASCII_ConstSize _
        | ASCII_VarSize_NullTerminated _
        | ASCII_UINT_ConstSize _
        | ASCII_UINT_VarSize_NullTerminated _
        | BCD_ConstSize _
        | BCD_VarSize_NullTerminated _                 -> None
    match readPostsBound && unsuffixedClass && noExtension, fld.acnProperties.mappingFunction, fld.acnEncodingClass, fld.uperRange, width with
    | false, _, _, _, _ | _, Some _, _, _, _                          -> None
    | true, None, Integer_uPER, Concrete (a, b), _                   -> Some (a, b)
    | true, None, Integer_uPER, (NegInf _ | PosInf _ | Full), _      -> None
    | true, None, _, Concrete (a, b), Some (wl, wh)                  -> Some (max a wl, min b wh)
    | true, None, _, (NegInf _ | PosInf _ | Full), Some (wl, wh)     -> Some (wl, wh)
    | true, None, _, _, None                                         -> None

/// Coverage cycle 15a (A-str): the bounds (low, high) of the size field of the string asn1TypeIdWithDependency, when
/// they make the decoder's `<field> in 0 .. maxSize` check (the Ada string external-field templates, Issue 64) always
/// TRUE: the field was decoded before the string on every path that decodes the string, in the same function, and every
/// value its read returns is within 0 .. maxSize. Conditions:
/// - the determinant is an ACN INTEGER field of a SEQUENCE (AcnChildDeterminant; not a parameter, i.e. not an --acn-v2
///   AcnInsertedFieldRef from another function, nor a legacy parameter chain): the decoder of the string reads the
///   field's local;
/// - the read dominates the string: the SEQUENCE holding the field is an ancestor of the string, reached through
///   mandatory SEQUENCE components only (every node of the string's id after the holder's is SEQ_CHILD (_, false); no
///   OPTIONAL / ALWAYS ABSENT / ALWAYS PRESENT component, CHOICE alternative or SEQUENCE OF element), and the front end
///   requires an ACN field before its dependent, so every path that decodes the string has decoded the field first;
/// - the read's interval (acnIntReadInterval) is bounded and within 0 .. maxSize.
let localSizeFieldWithinSize (readPostsBound: bool) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (asn1TypeIdWithDependency: ReferenceToType) (maxSize: System.Numerics.BigInteger) : (System.Numerics.BigInteger * System.Numerics.BigInteger) option =
    match deps.acnDependencies |> List.tryFind (fun d -> d.asn1Type = asn1TypeIdWithDependency) with
    | None -> None
    | Some dependency ->
        match dependency.determinant with
        | AcnParameterDeterminant _ -> None
        | AcnChildDeterminant field ->
            let fieldPath = field.id.ToScopeNodeList
            let holder = List.take (fieldPath.Length - 1) fieldPath
            let consumerPath = asn1TypeIdWithDependency.ToScopeNodeList
            let dominates =
                holder.Length < consumerPath.Length
                && List.take holder.Length consumerPath = holder
                && consumerPath |> List.skip holder.Length |> List.forall (fun n ->
                    match n with
                    | SEQ_CHILD (_, false)                                  -> true
                    | SEQ_CHILD (_, true) | MD _ | TA _ | VA _ | CH_CHILD _ | PRM _ | SQF -> false)
            let interval =
                match field.Type with
                | AcnInsertedType.AcnInteger fld              -> acnIntReadInterval readPostsBound fld
                | AcnInsertedType.AcnNullType _
                | AcnInsertedType.AcnBoolean _
                | AcnInsertedType.AcnReferenceToEnumerated _
                | AcnInsertedType.AcnReferenceToIA5String _   -> None
            match dominates, interval with
            | true, Some (lo, hi) when lo >= 0I && hi <= maxSize -> Some (lo, hi)
            | _ -> None
