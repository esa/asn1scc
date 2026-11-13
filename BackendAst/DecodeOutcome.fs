/// Coverage step 10b: which decode parts cannot fail (q/details-docs/coverage/step10b-plan.md, "Step 3 design").
/// One classifier, asked both by the primitive that emits a call (the _FullRange twin of a full-range INTEGER)
/// and by the SEQUENCE that joins the parts (DAstUtilFunctions.nestItemsByOutcome), so the two ends agree.
/// With ReadsCanFail (C, Scala, Python, Rust) every part is MayFail and the generated code is unchanged.
/// A wrong AlwaysSucceeds is caught by gnatprove on the RUN_SPARK files (pragma Assert, the twins' bounds, the
/// decoder Post), so when in doubt a kind stays MayFail.
module DecodeOutcome

open System.Numerics
open FsUtils
open CommonTypes
open DAst                       // DecodeOutcome, ResultWrite, ReadFailureModel; opened first: Asn1AcnAst wins below
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open Language
open AcnGenericTypes

/// Whether an INTEGER decode may use the RTL _FullRange twin.
type FullRangeUse =
    | UseFullRange          // an ASN.1 INTEGER: the twin when its field accepts every bit pattern
    | KeepCheckedCall       // an ACN inserted field or an ENUMERATED stub: its part is MayFail, the checked call stays

/// A uPER INTEGER whose range fills its field (max - min + 1 = 2^nBits): decoded with the RTL _FullRange
/// twin, which has no Result. Mirrors DAstUPer.getIntfuncBodyByCons (fully constrained, no root extension)
/// and the twins' preconditions (nBits < word size, signed nBits <= word size - 2).
type UperFullRangeInt = {
    nMin  : BigInteger
    nBits : BigInteger
}

let uperIntegerFullRange (r:AstRoot) (lm:LanguageMacros) (uperRange:BigIntegerUperRange) (intClass:IntegerClass) (cons:IntegerTypeConstraint list) : UperFullRangeInt option =
    let hasRootExtension =
        cons |> List.exists (fun c ->
            match c with
            | RangeRootConstraint _ | RangeRootConstraint2 _ -> true
            | _                                              -> false)
    match lm.lg.decodeReadFailureModel, hasRootExtension, uperRange with
    | ReadsCanFail, _, _
    | ReadsCannotFail, true, _              -> None
    | ReadsCannotFail, false, Concrete (min, max) when min < max ->
        let nBits = GetNumberOfBitsForNonNegativeInteger (max - min)
        let wordBits = BigInteger (int r.args.integerSizeInBytes * 8)
        let fillsField = max - min + 1I = BigInteger.Pow (2I, int nBits)
        let withinTwin = if intClass.IsPositive then nBits < wordBits else nBits <= wordBits - 2I
        match fillsField && withinTwin with
        | true  -> Some { nMin = min; nBits = nBits }
        | false -> None
    | ReadsCannotFail, false, Concrete _
    | ReadsCannotFail, false, PosInf _
    | ReadsCannotFail, false, NegInf _
    | ReadsCannotFail, false, Full          -> None

/// A fixed-size OCTET / BIT STRING decoded by one RTL call that cannot fail (kind 3 of the analysis:
/// BitStream_DecodeOctetString_no_length, BitStream_ReadBitArray, whose Post states success); the
/// -adaBitStringAlignment variant (BitStream_ReadBits) does not state it. Decoded with *_noCheck (no failure branch).
type FixedStringKind =
    | FixedOctets
    | FixedBits

let fixedStringCannotFail (lm:LanguageMacros) (kind:FixedStringKind) (fixedNotFragmented:bool) : bool =
    match lm.lg.decodeReadFailureModel, kind with
    | ReadsCanFail, _               -> false
    | ReadsCannotFail, FixedOctets  -> fixedNotFragmented
    | ReadsCannotFail, FixedBits    -> fixedNotFragmented && not CommonTypes.bitStringAlignment

/// Coverage step 14c2b (B2): a length minSize .. maxSize (minSize < maxSize) whose field of nBits bits holds exactly its
/// values (maxSize - minSize + 1 = 2^nBits): read with UPER_Dec_ConstraintWholeNumberInt_FullRange (Integer target, whose
/// Pre allows nBits <= 30), so the read cannot fail and its Post bounds the length to the SIZE range.
let lengthFullRange (lm:LanguageMacros) (minSize:BigInteger) (maxSize:BigInteger) (nBits:BigInteger) : bool =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> false
    | ReadsCannotFail   ->
        minSize >= 0I && minSize < maxSize && nBits <= 30I && maxSize - minSize + 1I = BigInteger.Pow (2I, int nBits)

/// 14c2b (B2): the character index of a uPER string (0 .. n - 1 for an alphabet of n characters; the full 128-character
/// set in 7 bits) fills its field when n is a power of two.
let charIndexFullRange (lm:LanguageMacros) (charSetLength:int) : bool =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> false
    | ReadsCannotFail   ->
        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (charSetLength - 1))
        charSetLength >= 1 && BigInteger charSetLength = BigInteger.Pow (2I, int nBits)

/// 14c2b (B2): a uPER IA5String / NumericString (not fragmented) whose decode cannot fail: a full character index and a
/// fixed size or a full-range length (DAstUPer.createIA5StringFunction: str_FixedSize_fullRange / str_VarSize_fullRange).
/// A part that may fail keeps the checked reads (it must write result.Success on every path).
let uperStringCannotFail (lm:LanguageMacros) (o:StringType) : bool =
    o.maxSize.uper < 65536I && charIndexFullRange lm o.uperCharSet.Length
    && (o.minSize.uper = o.maxSize.uper
        || lengthFullRange lm o.minSize.uper o.maxSize.uper (GetNumberOfBitsForNonNegativeInteger (o.maxSize.uper - o.minSize.uper)))

/// 14c2b (B2): a uPER variable-size OCTET / BIT STRING (not fragmented) whose length fills its field: the length read
/// cannot fail, the content read states success in its Post (BitStream_DecodeOctetString_no_length,
/// BitStream_ReadBitArray; not BitStream_ReadBits of -adaBitStringAlignment): octet_VarSize_fullRange /
/// bitString_VarSize_fullRange.
let varSizeStringCannotFail (lm:LanguageMacros) (kind:FixedStringKind) (isFixedSize:bool) (minSize:BigInteger) (maxSize:BigInteger) (nSizeInBits:BigInteger) : bool =
    not isFixedSize && maxSize < 65536I && lengthFullRange lm minSize maxSize nSizeInBits
    && (match kind with FixedOctets -> true | FixedBits -> not CommonTypes.bitStringAlignment)

/// Coverage step 14c2a: an index (uPER ENUMERATED, CHOICE) that fills its field: n = 2^k items. The index is read
/// with the RTL twin UPER_Dec_ConstraintWholeNumber_FullRange (no Result; its Post bounds the index to 0 .. 2^k - 1), so
/// the `case` on it needs no guard. Returns k. Since 14j (G4) also k = 0 (one item / alternative): the twin reads 0 bits
/// (its Pre allows nBits = 0, its Post gives the index 0); the value is still read, not assigned (C:
/// decodeOneValueWithoutRead), so `bs` stays an argument of an RTL call.
let fullIndexBits (lm:LanguageMacros) (n:int) : BigInteger option =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> None
    | ReadsCannotFail   ->
        let nBits = GetNumberOfBitsForNonNegativeInteger (BigInteger (n - 1))
        match n >= 1 && BigInteger n = BigInteger.Pow (2I, int nBits) with
        | true  -> Some nBits
        | false -> None

/// 14c2a: a uPER ENUMERATED with 2^k items (DAstUPer.createEnumeratedFunction: the index 0 .. n - 1 in k bits; the
/// switch-free encoding is not this shape). Returns k.
let uperEnumFullIndex (r:AstRoot) (lm:LanguageMacros) (o:Enumerated) : BigInteger option =
    match r.args.isEnumEfficientEnabled o.items.Length with
    | true  -> None
    | false -> fullIndexBits lm o.items.Length

/// Coverage step 14c3 (ILangGeneric.decodeOneValueWithoutRead, C): a uPER ENUMERATED with one item is not read, the
/// decoder assigns the item (DAstUPer.createEnumeratedFunction, macro one_value_assign); the RTL read of 0 bits could
/// never fail. Not for the switch-free encoding (-eee).
let uperEnumOneItem (r:AstRoot) (lm:LanguageMacros) (o:Enumerated) : bool =
    lm.lg.decodeOneValueWithoutRead && o.items.Length = 1 && not (r.args.isEnumEfficientEnabled o.items.Length)

/// 14c3: the same for an ACN ENUMERATED with the uPER-like encoding (AcnEnum.createEnumCommon); any other encoding
/// reads its field.
let acnEnumOneItem (r:AstRoot) (lm:LanguageMacros) (o:Enumerated) : bool =
    match o.acnEncodingClass with
    | Integer_uPER -> uperEnumOneItem r lm o
    | PositiveInteger_ConstSize_8
    | PositiveInteger_ConstSize_big_endian_16 | PositiveInteger_ConstSize_little_endian_16
    | PositiveInteger_ConstSize_big_endian_32 | PositiveInteger_ConstSize_little_endian_32
    | PositiveInteger_ConstSize_big_endian_64 | PositiveInteger_ConstSize_little_endian_64
    | PositiveInteger_ConstSize _
    | TwosComplement_ConstSize_8
    | TwosComplement_ConstSize_big_endian_16 | TwosComplement_ConstSize_little_endian_16
    | TwosComplement_ConstSize_big_endian_32 | TwosComplement_ConstSize_little_endian_32
    | TwosComplement_ConstSize_big_endian_64 | TwosComplement_ConstSize_little_endian_64
    | TwosComplement_ConstSize _
    | ASCII_ConstSize _ | ASCII_UINT_ConstSize _ | BCD_ConstSize _
    | ASCII_VarSize_NullTerminated _ | ASCII_UINT_VarSize_NullTerminated _ | BCD_VarSize_NullTerminated _ -> false

/// 14c3: a CHOICE with one alternative whose index is coded (uPER, ACN without determinant): the index is not read,
/// the decoder selects the alternative (macro choice_one_alternative).
let choiceOneAlternative (lm:LanguageMacros) (o:Choice) : bool =
    lm.lg.decodeOneValueWithoutRead && o.children.Length = 1

/// 14c3 (ILangGeneric.decodeJoinsByWrites, C): what the decode part of a SEQUENCE child writes, for the joins of the
/// parts (DAstUtilFunctions.nestEncodeParts): nothing for a child that is not read (ALWAYS ABSENT: its presence flag
/// only; a one-value type decoded in place: an assignment); unknown otherwise, so the check after it stays. An
/// OPTIONAL child stays unknown (conservative: present-when conditions).
let decodeChildPartWrites (oneValue: Asn1Type -> bool) (optionality: Asn1Optionality option) (childType:Asn1Type) : EncodeOutcome =
    match optionality with
    | Some AlwaysAbsent                 -> EncodeOutcome.noWrite
    | Some (Optional _)                 -> EncodeOutcome.unknown
    | None | Some AlwaysPresent         ->
        match oneValue childType with
        | true  -> EncodeOutcome.noWrite
        | false -> EncodeOutcome.unknown

/// 14c3: a type decoded in place without a read (uPER).
let uperOneValue (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : bool =
    match t.Kind with
    | Enumerated o -> uperEnumOneItem r lm o
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | NullType _ | BitString _ | Boolean _
    | SequenceOf _ | Sequence _ | Choice _ | ObjectIdentifier _ | TimeType _ | ReferenceType _ -> false

/// 14c3: a type decoded in place without a read (ACN).
let acnOneValue (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : bool =
    match t.Kind with
    | Enumerated o -> acnEnumOneItem r lm o
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | NullType _ | BitString _ | Boolean _
    | SequenceOf _ | Sequence _ | Choice _ | ObjectIdentifier _ | TimeType _ | ReferenceType _ -> false

/// 14c3: an ACN inserted field decoded without a read (an ENUMERATED field with one item, uPER-like).
let acnInsertedFieldOneValue (r:AstRoot) (lm:LanguageMacros) (o:AcnInsertedType) : bool =
    match o with
    | AcnReferenceToEnumerated e -> acnEnumOneItem r lm e.enumerated
    | AcnInteger _ | AcnNullType _ | AcnBoolean _ | AcnReferenceToIA5String _ -> false

/// The outcome of a part made of several parts (a SEQUENCE body): MayFail if one part may fail; otherwise
/// WritesResult when one part writes result on every path (a presence bit, a mandatory child that writes result),
/// NoResultWrite when no part touches result.
let private combine (alwaysWriting: DecodeOutcome list) (conditional: DecodeOutcome list) : DecodeOutcome =
    let all = alwaysWriting @ conditional
    match all |> List.exists (fun o -> o = MayFail) with
    | true  -> MayFail
    | false ->
        match alwaysWriting |> List.exists (fun o -> o = AlwaysSucceeds WritesResult) with
        | true  -> AlwaysSucceeds WritesResult
        | false -> AlwaysSucceeds NoResultWrite

/// 14c2b: varSizeStringCannotFail with the uPER length field (DAstUPer: GetNumberOfBitsForNonNegativeInteger (max - min)).
let uperVarSizeCannotFail (lm:LanguageMacros) (kind:FixedStringKind) (isFixedSize:bool) (minSize:BigInteger) (maxSize:BigInteger) : bool =
    varSizeStringCannotFail lm kind isFixedSize minSize maxSize (GetNumberOfBitsForNonNegativeInteger (maxSize - minSize))

/// uPER decode outcome of a type (as decoded in place: a reference that is decoded by a call to the type
/// assignment's function is a part that writes result).
let rec uper (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : DecodeOutcome =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> MayFail
    | ReadsCannotFail   ->
        match t.Kind with
        | Integer o ->
            match o.uperRange with
            | Concrete (min, max) when min = max -> AlwaysSucceeds WritesResult      // IntNoneRequired: result := (true, 0)
            | Concrete _ | PosInf _ | NegInf _ | Full ->
                match uperIntegerFullRange r lm o.uperRange o.intClass o.cons with
                | Some _ -> AlwaysSucceeds NoResultWrite
                | None   -> MayFail
        | Boolean _ -> AlwaysSucceeds WritesResult                                  // UPER_Dec_boolean: Post result
        | Sequence o ->
            let asn1Children = o.children |> List.choose (fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
            // every child that has an optionality has a presence bit: BitStream_ReadBit, Post result
            let presenceBits =
                asn1Children |> List.choose (fun c -> c.Optionality |> Option.map (fun _ -> AlwaysSucceeds WritesResult))
            let mandatory, conditional =
                asn1Children |> List.partition (fun c -> c.Optionality.IsNone)
            let hasStatements = not presenceBits.IsEmpty || asn1Children |> List.exists (fun c -> uperChildHasStatement c.Type)
            match hasStatements with
            | false -> AlwaysSucceeds WritesResult      // decode_empty_sequence / decode_nullType: result := (true, 0)
            | true  ->
                combine (presenceBits @ (mandatory |> List.map (fun c -> uperChild r lm c.Type)))
                        (conditional |> List.map (fun c -> uperChild r lm c.Type))
        | ReferenceType o ->
            match o.encodingOptions with
            | Some _ -> MayFail                                                     // CONTAINING
            | None   ->
                let t1 = GetActualTypeByName r o.modName o.tasName
                match TypesEquivalence.uperEquivalence t1 o.resolvedType with
                | true  ->                                                          // call of T_Decode_aux
                    match uper r lm t1 with
                    | AlwaysSucceeds _ -> AlwaysSucceeds WritesResult
                    | MayFail          -> MayFail
                | false -> uper r lm o.resolvedType                                // decoded in place
        | OctetString o ->
            match fixedStringCannotFail lm FixedOctets (o.isFixedSize && o.maxSize.uper < 65536I)
                  || uperVarSizeCannotFail lm FixedOctets o.isFixedSize o.minSize.uper o.maxSize.uper with
            | true  -> AlwaysSucceeds WritesResult
            | false -> MayFail
        | BitString o ->
            match fixedStringCannotFail lm FixedBits (o.isFixedSize && o.maxSize.uper < 65536I)
                  || uperVarSizeCannotFail lm FixedBits o.isFixedSize o.minSize.uper o.maxSize.uper with
            | true  -> AlwaysSucceeds WritesResult
            | false -> MayFail
        | Enumerated o ->
            match uperEnumFullIndex r lm o with
            | Some _ -> AlwaysSucceeds NoResultWrite                                // _FullRange index, unguarded case
            | None   -> MayFail
        | Choice o ->
            match uperChoiceFullIndex r lm o with
            | Some (_, w) -> AlwaysSucceeds w
            | None       -> MayFail
        | IA5String o | NumericString o ->
            match uperStringCannotFail lm o with
            | true  -> AlwaysSucceeds NoResultWrite                                 // 14c2b: full-range length and chars
            | false -> MayFail
        | SequenceOf o ->
            match uperSeqOfFixedElement r lm o with                                 // 14j (G1): the fixed-size loop
            | AlwaysSucceeds _ -> AlwaysSucceeds WritesResult                       // result := (true, 0) before the loop
            | MayFail          -> MayFail
        | Real _ | NullType _ | TimeType _
        | ObjectIdentifier _ -> MayFail

/// Coverage step 14j (G1): the decode outcome of the element of a uPER SEQUENCE OF decoded by the fixed-size loop
/// (DAstUPer.createSequenceOfFunction: seqOf_FixedSize, size < 65536, i.e. not fragmented) whose element has a
/// statement; MayFail for every other SEQUENCE OF (variable size, fragmentation). An element that cannot fail drops
/// `AND result.Success` from the loop (loopFixedItem_dec, as the encoders since 12b) and the SEQUENCE OF cannot fail:
/// seqOf_FixedSize_decode sets result := (true, 0) before the loop, and the element keeps it TRUE.
and uperSeqOfFixedElement (r:AstRoot) (lm:LanguageMacros) (o:SequenceOf) : DecodeOutcome =
    match o.maxSize.uper < 65536I && o.maxSize.uper = o.minSize.uper && uperChildHasStatement o.child with
    | true  -> uper r lm o.child
    | false -> MayFail

/// 14c2a: a uPER CHOICE with 2^k alternatives that all cannot fail and treat result alike (each writes it on every path,
/// or none touches it): the index is read with the _FullRange twin and the `case` is unguarded (choice_fullIndex). A
/// CHOICE whose alternatives treat result differently stays MayFail (George, 2026-11-02, step14c2a-plan.md question B):
/// a MayFail part must write result.Success on every path. Returns k and the alternatives' ResultWrite.
and uperChoiceFullIndex (r:AstRoot) (lm:LanguageMacros) (o:Choice) : (BigInteger * ResultWrite) option =
    fullIndexBits lm o.children.Length |> Option.bind (fun nBits ->
        match o.children |> List.map (fun c -> uperChild r lm c.Type) with
        | AlwaysSucceeds w :: rest when rest |> List.forall (fun x -> x = AlwaysSucceeds w) -> Some (nBits, w)
        | _ -> None)

/// A direct NULL child has no decode statement in Ada (a reference to a NULL type is a call).
and uperChildHasStatement (childType:Asn1Type) : bool =
    match childType.Kind with
    | NullType _ -> false
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | BitString _ | Boolean _ | TimeType _
    | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | Choice _ | ReferenceType _ -> true

/// uPER decode outcome of a SEQUENCE child's part (an OPTIONAL child's part is wrapped in `if exist then`:
/// same outcome, its writes come after the presence bits).
and uperChild (r:AstRoot) (lm:LanguageMacros) (childType:Asn1Type) : DecodeOutcome =
    match uperChildHasStatement childType with
    | false -> AlwaysSucceeds NoResultWrite
    | true  -> uper r lm childType

/// How a decode part treats result.ErrorCode (Ada uPER templates): writesE = after the part, ErrorCode is set on
/// every path; exitNeedsE = inside the part, a failure exit (`if result.Success` after a part that may fail; flow
/// analysis also follows its FALSE branch) can be reached before the part sets ErrorCode.
type ErrorCodeSummary = {
    writesE    : bool
    exitNeedsE : bool
}

/// ErrorCode summary of decoding t in place (uPER, Ada). A decoder starts with result := (true, 0)
/// (DAstUPer.createUperFunction) exactly when its body has exitNeedsE or not writesE: then the initialisation is
/// read at some exit or at the end, so it is never "unused" (gnatprove flow), and ErrorCode is defined everywhere.
/// The parts that cannot fail (presence bits, BOOLEAN, _FullRange calls) do not set ErrorCode.
let rec uperErrorCodeSummary (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : ErrorCodeSummary =
    let leaf writesE = { writesE = writesE; exitNeedsE = false }
    match t.Kind with
    | Integer o ->
        match o.uperRange with
        | Concrete (min, max) when min = max -> leaf true                             // result := (true, 0)
        | Concrete _ | PosInf _ | NegInf _ | Full ->
            match uperIntegerFullRange r lm o.uperRange o.intClass o.cons with
            | Some _ -> leaf false                                                    // _FullRange call
            | None   -> leaf true                                                     // result.ErrorCode := ...
    | Boolean _ -> leaf false                                                         // UPER_Dec_boolean only
    | BitString o ->                                                                  // fixed: only on failure, or never (_noCheck)
        leaf (not (o.isFixedSize && o.maxSize.uper < 65536I) && not (uperVarSizeCannotFail lm FixedBits o.isFixedSize o.minSize.uper o.maxSize.uper))
    | OctetString o ->                                                                // octet_FixedSize_noCheck, 14c2b octet_VarSize_fullRange
        leaf (not (fixedStringCannotFail lm FixedOctets (o.isFixedSize && o.maxSize.uper < 65536I)) && not (uperVarSizeCannotFail lm FixedOctets o.isFixedSize o.minSize.uper o.maxSize.uper))
    | NullType _ -> leaf false
    | Enumerated o -> leaf (uperEnumFullIndex r lm o).IsNone                          // _FullRange index: no ErrorCode
    | Choice o ->
        match uperChoiceFullIndex r lm o with
        | None   -> leaf true                                                         // choice_decode: ErrorCode before the index
        | Some _ ->
            // choice_fullIndex writes nothing before the alternatives: one of them runs (the index covers them all)
            let alternatives = o.children |> List.map (fun c -> uperErrorCodeSummary r lm c.Type)
            { writesE    = alternatives |> List.forall (fun s -> s.writesE)
              exitNeedsE = alternatives |> List.exists (fun s -> s.exitNeedsE) }
    | IA5String o | NumericString o -> leaf (not (uperStringCannotFail lm o))         // 14c2b: the _fullRange forms write nothing
    | Real _ | TimeType _ | ObjectIdentifier _
    | SequenceOf _ -> leaf true
    | Sequence o ->
        let asn1Children = o.children |> List.choose (fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
        let hasPresenceBits = asn1Children |> List.exists (fun c -> c.Optionality.IsSome)
        let withStatements = asn1Children |> List.filter (fun c -> uperChildHasStatement c.Type)
        match withStatements, hasPresenceBits with
        | [], false -> leaf true                                                      // decode_empty_sequence: result := (true, 0)
        | _, _      ->
            // the parts in the order of DAstUPer.createSequenceFunction: presence bits, then the children
            let presenceParts = asn1Children |> List.filter (fun c -> c.Optionality.IsSome) |> List.map (fun _ -> AlwaysSucceeds WritesResult, leaf false)
            let childParts =
                withStatements |> List.map (fun c ->
                    let s = uperErrorCodeSummary r lm c.Type
                    let outcome = uperChild r lm c.Type
                    match c.Optionality with
                    | None   -> outcome, s
                    | Some _ -> outcome, { s with writesE = false })                  // inside `if exist then`
            let parts = presenceParts @ childParts
            let lastIx = parts.Length - 1
            let defE, exitNeedsE =
                parts |> List.indexed |> List.fold (fun (defE, needs) (ix, (outcome, s)) ->
                    let needs = needs || (not defE && s.exitNeedsE)
                    let defE = defE || s.writesE
                    // the join after the part (none after the last one): `if result.Success` exits on FALSE
                    let needs =
                        match outcome, ix = lastIx with
                        | MayFail, false -> needs || not defE
                        | MayFail, true
                        | AlwaysSucceeds _, _ -> needs
                    defE, needs) (false, false)
            { writesE = defE; exitNeedsE = exitNeedsE }
    | ReferenceType o ->
        match o.encodingOptions with
        | Some _ -> leaf true
        | None   ->
            let t1 = GetActualTypeByName r o.modName o.tasName
            match TypesEquivalence.uperEquivalence t1 o.resolvedType with
            | true  -> leaf true                                                      // call: the callee sets result
            | false -> uperErrorCodeSummary r lm o.resolvedType

/// A uPER decoder needs result := (true, 0) at its start (see uperErrorCodeSummary).
let uperNeedsResultInit (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : bool =
    let s = uperErrorCodeSummary r lm t
    s.exitNeedsE || not s.writesE

/// An ACN INTEGER of an ASN.1 type whose checked call (Acn_Dec_Int_<Kind>_ConstSize<Width>[suffix], minVal..maxVal)
/// accepts every bit pattern of the field: decoded with the RTL _FullRange twin
/// (Acn_Dec_Int_<sKind>_ConstSize<sWidth>_FullRange<suffix>). Mirrors the bounds of AcnPrimitives.createAcnIntegerFunctionInternal
/// (unsigned max(0, min) .. min(2^w - 1, max), signed max(-2^(w-1), min) .. min(2^(w-1) - 1, max)) and the twins that exist:
/// the integer-class suffix must be the base type or a type of the field's size (at least nBits for the nBits form).
type AcnFullRangeInt = {
    sKind  : string                 // PositiveInteger | TwosComplement
    sWidth : string                 // _8, _big_endian_16, ... or "" for the nBits form
    noBits : BigInteger option      // the nBits form
}

let acnIntegerFullRange (r:AstRoot) (lm:LanguageMacros) (uperRange:BigIntegerUperRange) (intClass:IntegerClass) (encClass:IntEncodingClass) (hasMappingFunction:bool) : AcnFullRangeInt option =
    let nUperMin, nUperMax =
        match uperRange with
        | Concrete (a, b) -> a, b
        | NegInf b        -> r.args.SIntMin, b
        | PosInf a        -> a, r.args.IntMax (a >= 0I)
        | Full            -> r.args.SIntMin, r.args.SIntMax
    let wordBits = int r.args.integerSizeInBytes * 8
    let classFits (signed:bool) (exact:bool) (w:int) =
        let sized (bits:int) (isSigned:bool) = isSigned = signed && (if exact then w = bits else w <= bits)
        match intClass with
        | ASN1SCC_UInt8 _                   -> sized 8 false
        | ASN1SCC_UInt16 _                  -> sized 16 false
        | ASN1SCC_UInt32 _                  -> sized 32 false
        | ASN1SCC_UInt64 _ | ASN1SCC_UInt _ -> not signed
        | ASN1SCC_Int8 _                    -> sized 8 true
        | ASN1SCC_Int16 _                   -> sized 16 true
        | ASN1SCC_Int32 _                   -> sized 32 true
        | ASN1SCC_Int64 _ | ASN1SCC_Int _   -> signed
    let unsignedField (w:int) (sWidth:string) (exact:bool) (noBits:BigInteger option) =
        match nUperMin <= 0I && nUperMax >= BigInteger.Pow (2I, w) - 1I && classFits false exact w with
        | true  -> Some { sKind = "PositiveInteger"; sWidth = sWidth; noBits = noBits }
        | false -> None
    let signedField (w:int) (sWidth:string) (exact:bool) (noBits:BigInteger option) =
        match nUperMin <= -(BigInteger.Pow (2I, w - 1)) && nUperMax >= BigInteger.Pow (2I, w - 1) - 1I && classFits true exact w with
        | true  -> Some { sKind = "TwosComplement"; sWidth = sWidth; noBits = noBits }
        | false -> None
    match lm.lg.decodeReadFailureModel, hasMappingFunction with
    | ReadsCanFail, _
    | ReadsCannotFail, true  -> None
    | ReadsCannotFail, false ->
        match encClass with
        | PositiveInteger_ConstSize_8                  -> unsignedField 8 "_8" true None
        | PositiveInteger_ConstSize_big_endian_16      -> unsignedField 16 "_big_endian_16" true None
        | PositiveInteger_ConstSize_little_endian_16   -> unsignedField 16 "_little_endian_16" true None
        | PositiveInteger_ConstSize_big_endian_32      -> unsignedField 32 "_big_endian_32" true None
        | PositiveInteger_ConstSize_little_endian_32   -> unsignedField 32 "_little_endian_32" true None
        | PositiveInteger_ConstSize_big_endian_64      -> unsignedField 64 "_big_endian_64" true None
        | PositiveInteger_ConstSize_little_endian_64   -> unsignedField 64 "_little_endian_64" true None
        | PositiveInteger_ConstSize n when n >= 1I && n <= BigInteger wordBits -> unsignedField (int n) "" false (Some n)
        | TwosComplement_ConstSize_8                   -> signedField 8 "_8" true None
        | TwosComplement_ConstSize_big_endian_16       -> signedField 16 "_big_endian_16" true None
        | TwosComplement_ConstSize_little_endian_16    -> signedField 16 "_little_endian_16" true None
        | TwosComplement_ConstSize_big_endian_32       -> signedField 32 "_big_endian_32" true None
        | TwosComplement_ConstSize_little_endian_32    -> signedField 32 "_little_endian_32" true None
        | TwosComplement_ConstSize_big_endian_64       -> signedField 64 "_big_endian_64" true None
        | TwosComplement_ConstSize_little_endian_64    -> signedField 64 "_little_endian_64" true None
        | TwosComplement_ConstSize n when n >= 1I && n <= BigInteger wordBits -> signedField (int n) "" false (Some n)
        | PositiveInteger_ConstSize _ | TwosComplement_ConstSize _
        | Integer_uPER                                 // the uPER rule (uperIntegerFullRange)
        | ASCII_ConstSize _ | ASCII_VarSize_NullTerminated _ | ASCII_UINT_ConstSize _ | ASCII_UINT_VarSize_NullTerminated _
        | BCD_ConstSize _ | BCD_VarSize_NullTerminated _ -> None

/// The integer range of the scaled-integer encoding of a REAL (issue #339): (intMin, intMax, isUnsigned), the whole
/// range of the field (AcnPrimitives.createRealFunction maps the REAL range linearly onto it).
let scaledIntRange (c:IntEncodingClass) : BigInteger * BigInteger * bool =
    let unsignedRange (nBits:int) = 0I, BigInteger.Pow(2I, nBits) - 1I, true
    let signedRange (nBits:int) = -BigInteger.Pow(2I, nBits-1), BigInteger.Pow(2I, nBits-1) - 1I, false
    match c with
    | PositiveInteger_ConstSize_8                   -> unsignedRange 8
    | PositiveInteger_ConstSize_big_endian_16       -> unsignedRange 16
    | PositiveInteger_ConstSize_little_endian_16    -> unsignedRange 16
    | PositiveInteger_ConstSize_big_endian_32       -> unsignedRange 32
    | PositiveInteger_ConstSize_little_endian_32    -> unsignedRange 32
    | PositiveInteger_ConstSize_big_endian_64       -> unsignedRange 64
    | PositiveInteger_ConstSize_little_endian_64    -> unsignedRange 64
    | PositiveInteger_ConstSize bitSize             -> unsignedRange (int bitSize)
    | TwosComplement_ConstSize_8                    -> signedRange 8
    | TwosComplement_ConstSize_big_endian_16        -> signedRange 16
    | TwosComplement_ConstSize_little_endian_16     -> signedRange 16
    | TwosComplement_ConstSize_big_endian_32        -> signedRange 32
    | TwosComplement_ConstSize_little_endian_32     -> signedRange 32
    | TwosComplement_ConstSize_big_endian_64        -> signedRange 64
    | TwosComplement_ConstSize_little_endian_64     -> signedRange 64
    | TwosComplement_ConstSize bitSize              -> signedRange (int bitSize)
    | Integer_uPER
    | ASCII_ConstSize _
    | ASCII_VarSize_NullTerminated _
    | ASCII_UINT_ConstSize _
    | ASCII_UINT_VarSize_NullTerminated _
    | BCD_ConstSize _
    | BCD_VarSize_NullTerminated _                  -> raise(BugErrorException "getScaledIntRange: Real_ScaledInt can only contain a PositiveInteger_ConstSize* or TwosComplement_ConstSize* class")

/// Coverage step 14j (G3a): the integer of a scaled-integer REAL always fills its field (scaledIntRange), so its read is
/// the ACN _FullRange twin wherever one exists for the class (acnIntegerFullRange, with the integer class the producer
/// uses: AcnPrimitives.createRealFunction); the decoder then cannot fail and writes no result (macros
/// Real_ScaledInt_uint_cannotFail / _sint_cannotFail). None for C (ReadsCanFail).
let realScaledIntFullRange (r:AstRoot) (lm:LanguageMacros) (encClass:IntEncodingClass) : AcnFullRangeInt option =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> None
    | ReadsCannotFail   ->
        let intMin, intMax, isUnsigned = scaledIntRange encClass
        let intClass =
            match isUnsigned with
            | true  -> ASN1SCC_UInt (intMin, intMax)
            | false -> ASN1SCC_Int (intMin, intMax)
        acnIntegerFullRange r lm (Concrete (intMin, intMax)) intClass encClass false

/// 14c2a (B3): an ACN IEEE 754 64-bit REAL decode (Acn_Dec_Real_IEEE754_64_*: Post `if Asn1Real'Digits >= Long_Float'Digits
/// then Result.Success`) cannot fail where Asn1Real is Long_Float. The board decides Asn1Real (x86: Long_Float; stm32,
/// msp430: Float, ADA_RTL2/boards), so only when x86 is the only board generated (-t absent or x86), and with
/// -fpWordSize 8 (George, 2026-11-01: MayFail on msp430).
let real64CannotFail (r:AstRoot) (lm:LanguageMacros) : bool =
    let onlyX86 =
        match r.args.target with
        | None | Some X86                               -> true
        | Some Stm32 | Some Msp430 | Some AllBoards     -> false
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> false
    | ReadsCannotFail   -> onlyX86 && r.args.floatingPointSizeInBytes = 8I

/// Coverage step 14j (G5): an ACN IA5String / NumericString with `encoding ASCII` and a fixed size
/// (Acn_Enc_String_uPER_Ascii, the AcnStrings fixed-size branch): Acn_Dec_String_Ascii_FixSize reads one byte per
/// character and accepts every byte (as the C twin, whose only reject is the end of the stream), its Post states
/// success. Ada only (ReadsCannotFail); producer AcnStrings (macro Acn_String_Ascii_FixSize_cannotFail).
let acnStringAsciiFixSizeCannotFail (lm:LanguageMacros) (o:StringType) : bool =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> false
    | ReadsCannotFail   ->
        match o.acnEncodingClass with
        | Acn_Enc_String_uPER_Ascii _ -> o.maxSize.uper = o.minSize.uper
        | Acn_Enc_String_uPER _ | Acn_Enc_String_Ascii_Null_Terminated _ | Acn_Enc_String_Ascii_External_Field_Determinant _
        | Acn_Enc_String_CharIndex_External_Field_Determinant _ | Acn_Enc_String_Ascii_Deduced _ -> false

/// Coverage step 14j (G2): an ACN IEEE 754 32-bit REAL decoded into Asn1Real (the RTL procedure without the _fp32 suffix,
/// i.e. not the FP32 class of -slim): Asn1Real is Float or Long_Float on every board (ADA_RTL2/boards), the RTL Post states
/// success. Ada only (ReadsCannotFail); producer AcnPrimitives (macro Real_32_cannotFail).
let real32CannotFail (r:AstRoot) (lm:LanguageMacros) (o:Real) : bool =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> false
    | ReadsCannotFail   -> lm.lg.getRealEncodingSuffix r.args.floatingPointSizeInBytes (o.getClass r.args) = ""

/// 14c2a: how the value of an ACN ENUMERATED whose item values are exactly 0 .. n - 1, n = 2^k, is read without a Result.
type AcnEnumFullIndex =
    | EnumUperIndex of BigInteger       // Integer_uPER: UPER_Dec_ConstraintPosWholeNumber_FullRange, k bits
    | EnumAcnInt of AcnFullRangeInt     // pos-int of a fixed size that the values fill: the ACN _FullRange twin

/// 14c2a: an ACN ENUMERATED (an ASN.1 type; ACN inserted fields keep the checked read, 14c2b) whose item values fill the
/// field: the read cannot fail, the `case` is unguarded and its `when others` only keeps val defined
/// (EnumeratedEncValues_fullIndex, ASN1SCC-COV-001). AcnEnum.enumIntFuncBody applies no mapping-function.
let acnEnumFullIndex (r:AstRoot) (lm:LanguageMacros) (o:Enumerated) : AcnEnumFullIndex option =
    let n = o.items.Length
    let values = o.items |> List.map (fun x -> x.acnEncodeValue) |> Set.ofList
    let exactlyIndexes = values.Count = n && values = Set.ofList [0I .. BigInteger (n - 1)]
    match r.args.isEnumEfficientEnabled n, exactlyIndexes, o.acnEncodingClass with
    | true, _, _
    | false, false, _               -> None
    | false, true, Integer_uPER     -> fullIndexBits lm n |> Option.map EnumUperIndex
    | false, true, encClass         ->
        let range = Concrete (0I, BigInteger (n - 1))
        acnIntegerFullRange r lm range (getIntEncodingClassByUperRange r.args range) encClass false |> Option.map EnumAcnInt

/// Coverage step 14c2b: whether the read of an ACN inserted field may use a _FullRange twin (an INTEGER without
/// mapping-function, an ENUMERATED: the producers then decide by the same rules as for ASN.1 types). The field's
/// consumer (size, presence, CHOICE determinant) checks the value in its own part.
let acnInsertedFieldFullRangeUse (o:AcnInsertedType) : FullRangeUse =
    match o with
    | AcnInteger i ->
        match i.acnProperties.mappingFunction with
        | None   -> UseFullRange
        | Some _ -> KeepCheckedCall                     // mapped through mapFuncIntVal (Issue 58): the checked call stays
    | AcnReferenceToEnumerated _                        -> UseFullRange
    | AcnNullType _ | AcnBoolean _ | AcnReferenceToIA5String _ -> KeepCheckedCall

/// 14c2b: the decode part of an ACN inserted field (AcnSequence.handleAcnChild: only its read, also in --acn-v2, where
/// the copy into the AcnInsertedFieldRef follows the read's outcome: acn_deferred_det_copy_*_tmp, bReadCannotFail).
/// INTEGER: the twin of AcnPrimitives.createAcnIntegerFunction (Integer_uPER through DAstUPer.getIntfuncBodyByCons);
/// ENUMERATED: acnEnumFullIndex (14c2a); BOOLEAN: UPER_Dec_boolean (AcnPrimitives.createAcnBooleanFunction, Post
/// success, no error code); NULL without pattern: no statement; NULL with pattern and IA5String reject bit patterns.
let acnInsertedFieldPart (r:AstRoot) (lm:LanguageMacros) (o:AcnInsertedType) : DecodeOutcome * ErrorCodeSummary =
    let leaf writesE = { writesE = writesE; exitNeedsE = false }
    let cannotFail =
        match lm.lg.decodeReadFailureModel, o, acnInsertedFieldFullRangeUse o with
        | ReadsCanFail, _, _                            -> false
        | ReadsCannotFail, _, KeepCheckedCall           -> false
        | ReadsCannotFail, AcnInteger i, UseFullRange   ->
            match i.acnEncodingClass with
            | Integer_uPER  -> (uperIntegerFullRange r lm i.uperRange (getAcnIntegerClass r.args i) i.cons).IsSome
            | _             -> (acnIntegerFullRange r lm i.uperRange i.intClass i.acnEncodingClass false).IsSome
        | ReadsCannotFail, AcnReferenceToEnumerated e, UseFullRange -> (acnEnumFullIndex r lm e.enumerated).IsSome
        | ReadsCannotFail, (AcnNullType _ | AcnBoolean _ | AcnReferenceToIA5String _), UseFullRange -> false
    match lm.lg.decodeReadFailureModel, o with
    | ReadsCanFail, _                                   -> MayFail, leaf true
    | ReadsCannotFail, AcnBoolean b                     ->
        // AcnPrimitives.createAcnBooleanFunction: the uPER bit (no error code), or since Issue 65 the pattern templates
        // of an ASN.1 BOOLEAN (the `acn` / acnErrorCodeSummary rules of Boolean)
        match b.acnProperties.encodingPattern with
        | None                                              -> AlwaysSucceeds WritesResult, leaf false
        | Some pattern when pattern.isUperLike              -> AlwaysSucceeds WritesResult, leaf false
        | Some (TrueValueEncoding _ | FalseValueEncoding _) -> AlwaysSucceeds WritesResult, leaf true  // BooleanPatternNoErrCode
        | Some (TrueFalseValueEncoding _)                   -> MayFail, leaf true
    | ReadsCannotFail, AcnNullType n when n.acnProperties.encodingPattern.IsNone -> AlwaysSucceeds NoResultWrite, leaf false
    | ReadsCannotFail, (AcnInteger _ | AcnReferenceToEnumerated _ | AcnNullType _ | AcnReferenceToIA5String _) ->
        match cannotFail with
        | true  -> AlwaysSucceeds NoResultWrite, leaf false                 // _FullRange read: no result, no error code
        | false -> MayFail, leaf true                                       // the call writes result, then ErrorCode

/// Coverage step 14c2b (B3): a variable-size OCTET / BIT STRING whose size is an external field without range check
/// (SizeDeterminantBounds.externalSizeChecks: the field's encoding cannot exceed the SIZE range, issue #360) is decoded by
/// `Length := field` and one content read whose Post states success (BitStream_DecodeOctetString_no_length,
/// BitStream_ReadBitArray; not BitStream_ReadBits of -adaBitStringAlignment). Some true: no range check (writes
/// result.Success only), the bool = the content read cannot fail; None: the template checks the size (writes result).
let externalFieldUnchecked (r:AstRoot) (deps:AcnInsertedFieldDependencies) (t:Asn1Type) (kind:FixedStringKind) (minSize:BigInteger) (maxSize:BigInteger) (isFixed:bool) : bool option =
    match isFixed, deps.acnDependencies |> List.exists (fun d -> d.asn1Type = t.id) with
    | true, _ | _, false -> None                    // fixed size: always checked; no dependency: not this form
    | false, true ->
        match SizeDeterminantBounds.externalSizeChecks r deps t.id minSize maxSize with
        | None, None    ->
            match kind with
            | FixedOctets   -> Some true
            | FixedBits     -> Some (not CommonTypes.bitStringAlignment)
        | Some _, _ | _, Some _ -> None

/// ACN decode outcome of a type decoded in place (first scope of step 10b: SEQUENCEs with present-when, pre-decoding
/// functions, and every kind not listed stay MayFail; ACN inserted fields since 14c2b by acnInsertedFieldPart).
let rec acn (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (t:Asn1Type) : DecodeOutcome =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> MayFail
    | ReadsCannotFail   ->
        match t.Kind with
        | Integer o ->
            match o.acnEncodingClass with
            | Integer_uPER -> uper r lm t
            | _ ->
                match acnIntegerFullRange r lm o.uperRange o.intClass o.acnEncodingClass o.acnProperties.mappingFunction.IsSome with
                | Some _ -> AlwaysSucceeds NoResultWrite
                | None   -> MayFail
        | Boolean o ->
            match o.acnProperties.encodingPattern with
            | None                              -> AlwaysSucceeds WritesResult      // UPER_Dec_boolean
            | Some (TrueValueEncoding _)
            | Some (FalseValueEncoding _)       -> AlwaysSucceeds WritesResult      // Acn_Dec_Boolean_*_pattern: Post Success
            | Some (TrueFalseValueEncoding _)   -> MayFail
        | Sequence o ->
            // 14c2b: an ACN inserted field is a mandatory part with the outcome of its read (acnInsertedFieldPart)
            let acnParts = o.children |> List.choose (fun c -> match c with AcnChild a -> Some (fst (acnInsertedFieldPart r lm a.Type)) | Asn1Child _ -> None)
            match o.acnProperties.preDecodingFunction, o.acnProperties.postEncodingFunction with
            | Some _, _ | _, Some _ -> MayFail
            | None, None ->
                let asn1Children = o.children |> List.choose (fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
                let presenceBits = asn1Children |> List.filter acnHasPresenceBit |> List.map (fun _ -> AlwaysSucceeds WritesResult)
                let parts = asn1Children |> List.choose (fun c -> acnChildPart r deps lm c.Optionality c.Type |> Option.map (fun (outcome, _) -> c, outcome))
                // a present-when child writes result on every path (its presence statement, 14j G6)
                let unconditional, conditional =
                    parts |> List.partition (fun (c, _) ->
                        match c.Optionality with
                        | Some (Optional opt) ->
                            match opt.acnPresentWhen with
                            | Some _ -> true
                            | None   -> false
                        | None | Some AlwaysPresent | Some AlwaysAbsent -> true)
                match presenceBits, parts, acnParts with
                | [], [], [] -> AlwaysSucceeds WritesResult                         // decode_empty_sequence: result := (true, 0)
                | [], [], _  -> MayFail                                             // only ACN fields: kept as before 14c2b
                | _, _, _    -> combine (presenceBits @ acnParts @ (unconditional |> List.map snd)) (conditional |> List.map snd)
        | ReferenceType o ->
            match o.encodingOptions, o.hasExtraConstrainsOrChildrenOrAcnArgs with
            // 14j (G7a): --acn-v2 decodes a fixed-size CONTAINING without ACN arguments by a plain call of the contained
            // type assignment's T_ACN_Decode_aux (DAstACNDeferred: octet / bit_string_containing_deferred_fixed_func;
            // the OCTET form then only moves the position past the padding): the outcome of that call
            | Some enc, _ when r.args.acnDeferred && enc.acnEncodingClass = SZ_EC_FIXED_SIZE && o.acnArguments.IsEmpty ->
                match acn r deps lm (GetActualTypeByName r o.modName o.tasName) with
                | AlwaysSucceeds _ -> AlwaysSucceeds WritesResult
                | MayFail          -> MayFail
            | Some _, _         -> MayFail                                          // CONTAINING
            | None, true when r.args.acnDeferred && not o.acnArguments.IsEmpty ->
                MayFail                                                             // --acn-v2: call of a specialised function
            | None, true        -> acn r deps lm o.resolvedType                          // the base body inlined
            | None, false       ->
                match isLiveDeduced o.resolvedType with
                | true  -> MayFail                                                  // inlined with a trailing constant
                | false ->                                                          // call of T_ACN_Decode_aux
                    match acn r deps lm (GetActualTypeByName r o.modName o.tasName) with
                    | AlwaysSucceeds _ -> AlwaysSucceeds WritesResult
                    | MayFail          -> MayFail
        | OctetString o ->
            match o.acnEncodingClass with
            | SZ_EC_FIXED_SIZE when fixedStringCannotFail lm FixedOctets true -> AlwaysSucceeds WritesResult
            | SZ_EC_ExternalField _ when externalFieldUnchecked r deps t FixedOctets o.minSize.acn o.maxSize.acn o.isFixedSize = Some true ->
                AlwaysSucceeds WritesResult                                         // 14c2b: no range check, content Post
            | SZ_EC_LENGTH_EMBEDDED nBits when varSizeStringCannotFail lm FixedOctets o.isFixedSize o.minSize.acn o.maxSize.acn nBits ->
                AlwaysSucceeds WritesResult                                         // 14c2b: full-range length, content Post
            | SZ_EC_FIXED_SIZE | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> MayFail
        | BitString o ->
            match o.acnEncodingClass with
            | SZ_EC_FIXED_SIZE when fixedStringCannotFail lm FixedBits true -> AlwaysSucceeds WritesResult
            | SZ_EC_ExternalField _ when externalFieldUnchecked r deps t FixedBits o.minSize.acn o.maxSize.acn (o.minSize.uper = o.maxSize.uper) = Some true ->
                AlwaysSucceeds WritesResult                                         // 14c2b: no range check, content Post
            | SZ_EC_LENGTH_EMBEDDED nBits when varSizeStringCannotFail lm FixedBits o.isFixedSize o.minSize.acn o.maxSize.acn nBits ->
                AlwaysSucceeds WritesResult                                         // 14c2b: full-range length, content Post
            | SZ_EC_FIXED_SIZE | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> MayFail
        | Enumerated o ->
            match acnEnumFullIndex r lm o with
            | Some _ -> AlwaysSucceeds NoResultWrite                                // _FullRange read, unguarded case
            | None   -> MayFail
        | Choice o ->
            match acnChoiceFullIndex r deps lm o with
            | Some (_, w) -> AlwaysSucceeds w
            | None       -> MayFail
        | Real o ->
            match o.acnEncodingClass with
            | Real_IEEE754_64_big_endian
            | Real_IEEE754_64_little_endian when real64CannotFail r lm -> AlwaysSucceeds WritesResult   // Real_64_cannotFail
            | Real_IEEE754_32_big_endian
            | Real_IEEE754_32_little_endian when real32CannotFail r lm o -> AlwaysSucceeds WritesResult  // Real_32_cannotFail (14j G2)
            | Real_ScaledInt enc when (realScaledIntFullRange r lm enc).IsSome -> AlwaysSucceeds NoResultWrite  // 14j G3a
            | Real_IEEE754_64_big_endian | Real_IEEE754_64_little_endian
            | Real_uPER | Real_IEEE754_32_big_endian | Real_IEEE754_32_little_endian | Real_ScaledInt _ -> MayFail
        | IA5String o | NumericString o ->
            match o.acnEncodingClass with
            | Acn_Enc_String_uPER _ -> uper r lm t                                  // 14c2b: the uPER body (AcnStrings)
            | _ when acnStringAsciiFixSizeCannotFail lm o -> AlwaysSucceeds WritesResult   // Acn_String_Ascii_FixSize_cannotFail (14j G5)
            | _                     -> MayFail
        | SequenceOf o ->
            match acnSeqOfFixedElement r deps lm o with                             // 14j (G1): the fixed-size loop
            | AlwaysSucceeds _ -> AlwaysSucceeds WritesResult                       // result := (true, 0) before the loop
            | MayFail          -> MayFail
        | NullType _ | TimeType _
        | ObjectIdentifier _ -> MayFail

/// Coverage step 14j (G1): uperSeqOfFixedElement for an ACN SEQUENCE OF decoded by the fixed-size loop
/// (AcnSequenceOf.createSequenceOfFunction: SZ_EC_FIXED_SIZE / SZ_EC_LENGTH_EMBEDDED with a fixed size, the uPER
/// template seqOf_FixedSize) whose element has a statement.
and acnSeqOfFixedElement (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (o:SequenceOf) : DecodeOutcome =
    match o.acnEncodingClass with
    | SZ_EC_FIXED_SIZE | SZ_EC_LENGTH_EMBEDDED _ when o.isFixedSize && acnHasDecodeContent o.child -> acn r deps lm o.child
    | SZ_EC_FIXED_SIZE | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> MayFail

/// 14c2a: an ACN CHOICE decoded like uPER (no determinant, no present-when; AcnChoice CEC_uper) without ALWAYS ABSENT
/// alternatives (their arm writes a failure) whose 2^k alternatives all cannot fail and treat result alike: the index is
/// read with the _FullRange twin, the `case` is unguarded (Choice_fullIndex). An alternative without ACN content is
/// decode_nullType / decode_empty_sequence (result := (true, 0)). Returns k and the alternatives' ResultWrite.
and acnChoiceFullIndex (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (o:Choice) : (BigInteger * ResultWrite) option =
    let selfSelected =
        o.acnProperties.enumDeterminant.IsNone
        && o.children |> List.forall (fun c -> c.acnPresentWhenConditions.IsEmpty && c.Optionality <> Some ChoiceAlwaysAbsent)
    match selfSelected with
    | false -> None
    | true  ->
        fullIndexBits lm o.children.Length |> Option.bind (fun nBits ->
            match o.children |> List.map (fun c -> acnChoiceAlternative r deps lm c.Type |> fst) with
            | AlwaysSucceeds w :: rest when rest |> List.forall (fun x -> x = AlwaysSucceeds w) -> Some (nBits, w)
            | _ -> None)

/// The decode part of a CHOICE alternative (AcnChoice.handleChild): its outcome and ErrorCode summary.
and acnChoiceAlternative (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (childType:Asn1Type) : DecodeOutcome * ErrorCodeSummary =
    match acnHasDecodeContent childType with
    | true  -> acn r deps lm childType, acnErrorCodeSummary r deps lm childType
    | false -> AlwaysSucceeds WritesResult, { writesE = true; exitNeedsE = false }    // decode_nullType: result := (true, 0)

/// An OPTIONAL child without present-when has a presence bit (AcnSequence.printPresenceBit: BitStream_ReadBit).
and acnHasPresenceBit (c:Asn1Child) : bool =
    match c.Optionality with
    | Some (Optional opt) -> opt.acnPresentWhen.IsNone
    | Some AlwaysAbsent | Some AlwaysPresent | None -> false

/// Whether the ACN decoder of a type has a statement (a NULL without pattern or save-position has none).
and acnHasDecodeContent (t:Asn1Type) : bool =
    match t.ActualType.Kind with
    | NullType n -> n.acnProperties.encodingPattern.IsSome || t.SaveBitStreamPosition
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | BitString _ | Boolean _ | TimeType _
    | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | Choice _ | ReferenceType _ -> true

/// The decode part of an ASN.1 child of an ACN SEQUENCE (AcnSequence.handleAsn1Child): its outcome and ErrorCode
/// summary, None when the child has no statement. An ALWAYS ABSENT child only clears its flag and sets result :=
/// (true, 0) (sequence_always_absent_child); a child without content (NULL without pattern) has no statement, except
/// ALWAYS PRESENT (sets its flag) and present-when (the presence expression); an OPTIONAL child's part runs under
/// `if exist`, so it sets the error code only on some paths.
and acnChildPart (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (optionality:Asn1Optionality option) (childType:Asn1Type) : (DecodeOutcome * ErrorCodeSummary) option =
    let leaf writesE = { writesE = writesE; exitNeedsE = false }
    match optionality, acnHasDecodeContent childType with
    | Some AlwaysAbsent, _                                      -> Some (AlwaysSucceeds WritesResult, leaf true)
    | Some AlwaysPresent, false                                 -> Some (AlwaysSucceeds NoResultWrite, leaf false)
    // 14j (G6): the decoding of a present-when (AcnSequence: sequence_presence_optChild_pres_bool /
    // _pres_acn_expression) only sets Exist from a decoded field or an expression and writes result.Success := true
    // (not ErrorCode), then the child is decoded under `if Exist = 1`: the part cannot fail when the child cannot, and
    // it writes result on every path
    | Some (Optional opt), false when opt.acnPresentWhen.IsSome -> Some (AlwaysSucceeds WritesResult, leaf false)
    | Some (Optional _), false
    | None, false                                               -> None
    | Some (Optional opt), true ->
        let outcome =
            match opt.acnPresentWhen, acn r deps lm childType with
            | Some _, AlwaysSucceeds _  -> AlwaysSucceeds WritesResult
            | Some _, MayFail           -> MayFail
            | None, childOutcome        -> childOutcome
        Some (outcome, { acnErrorCodeSummary r deps lm childType with writesE = false })
    | Some AlwaysPresent, true
    | None, true                -> Some (acn r deps lm childType, acnErrorCodeSummary r deps lm childType)

/// ErrorCode summary of an ACN decode part (see uperErrorCodeSummary; ACN templates).
and acnErrorCodeSummary (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (t:Asn1Type) : ErrorCodeSummary =
    let leaf writesE = { writesE = writesE; exitNeedsE = false }
    match t.Kind with
    | Integer o ->
        match o.acnEncodingClass with
        | Integer_uPER -> uperErrorCodeSummary r lm t
        | _ ->
            match acnIntegerFullRange r lm o.uperRange o.intClass o.acnEncodingClass o.acnProperties.mappingFunction.IsSome with
            | Some _ -> leaf false                                                    // _FullRange call
            | None   -> leaf true                                                     // the call writes result, then ErrorCode
    | Boolean o ->
        match o.acnProperties.encodingPattern with
        | None   -> leaf false                                                        // UPER_Dec_boolean only
        | Some _ -> leaf true                                                         // the pattern call writes result
    | Sequence o ->
        let asn1Children = o.children |> List.choose (fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
        let hasAcnChild = o.children |> List.exists (fun c -> match c with AcnChild _ -> true | Asn1Child _ -> false)
        match asn1Children, hasAcnChild with
        | [], false -> leaf true                                                      // decode_empty_sequence
        | _, _      ->
            // AcnSequence parts: presence bits, then the children in order (an ACN inserted field is decoded by a call
            // that writes result); with a pre-decoding function the ACN decoder keeps the old joins (all MayFail)
            let oldJoins = o.acnProperties.preDecodingFunction.IsSome
            let presenceParts =
                asn1Children |> List.filter acnHasPresenceBit
                |> List.map (fun _ -> (match oldJoins with true -> MayFail | false -> AlwaysSucceeds WritesResult), leaf oldJoins)
            let childParts =
                o.children |> List.choose (fun c ->
                    match c with
                    | AcnChild a  ->
                        match oldJoins with
                        | true  -> Some (MayFail, leaf true)
                        | false -> Some (acnInsertedFieldPart r lm a.Type)                  // 14c2b
                    | Asn1Child c ->
                        acnChildPart r deps lm c.Optionality c.Type
                        |> Option.map (fun (outcome, s) ->
                            match oldJoins with
                            | true  -> MayFail, s
                            | false -> outcome, s))
            let parts = presenceParts @ childParts
            match parts with
            | [] -> leaf true                                                         // decode_empty_sequence / decode_nullType
            | _  ->
                let lastIx = parts.Length - 1
                let defE, exitNeedsE =
                    parts |> List.indexed |> List.fold (fun (defE, needs) (ix, (outcome, s)) ->
                        let needs = needs || (not defE && s.exitNeedsE)
                        let defE = defE || s.writesE
                        let needs =
                            match outcome, ix = lastIx with
                            | MayFail, false -> needs || not defE
                            | MayFail, true
                            | AlwaysSucceeds _, _ -> needs
                        defE, needs) (false, false)
                { writesE = defE; exitNeedsE = exitNeedsE }
    | ReferenceType o ->
        match o.encodingOptions, o.hasExtraConstrainsOrChildrenOrAcnArgs with
        | Some _, _     -> leaf true
        | None, true when r.args.acnDeferred && not o.acnArguments.IsEmpty ->
            leaf true                                                                 // --acn-v2: the specialised callee sets result
        | None, true    -> acnErrorCodeSummary r deps lm o.resolvedType
        | None, false   ->
            match isLiveDeduced o.resolvedType with
            | true  -> acnErrorCodeSummary r deps lm o.resolvedType
            | false -> leaf true                                                      // call: the callee sets result
    | BitString o ->
        match o.acnEncodingClass with
        | SZ_EC_FIXED_SIZE -> leaf false                                            // bitString_FixSize: only on failure (ACN does not fragment)
        | SZ_EC_ExternalField _ when (externalFieldUnchecked r deps t FixedBits o.minSize.acn o.maxSize.acn (o.minSize.uper = o.maxSize.uper)).IsSome ->
            leaf false                                                              // 14c2b: no range check, the read writes Success only
        | SZ_EC_LENGTH_EMBEDDED nBits when varSizeStringCannotFail lm FixedBits o.isFixedSize o.minSize.acn o.maxSize.acn nBits ->
            leaf false                                                              // 14c2b: bitString_VarSize_fullRange
        | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> leaf true
    | NullType n -> leaf n.acnProperties.encodingPattern.IsSome                       // Null_pattern: the call, then ErrorCode
    | Choice ch ->
        match ch.acnProperties.enumDeterminant, acnChoiceFullIndex r deps lm ch with
        | None, Some _ ->
            // Choice_fullIndex writes nothing before the alternatives: one of them runs (the index covers them all)
            let alternatives = ch.children |> List.map (fun c -> acnChoiceAlternative r deps lm c.Type |> snd)
            { writesE    = alternatives |> List.forall (fun s -> s.writesE)
              exitNeedsE = alternatives |> List.exists (fun s -> s.exitNeedsE) }
        | None, None   -> leaf true // Choice_decode sets ErrorCode before the index, Choice_preWhen_decode result := (true, 0)
        | Some _, _ ->
            // Choice_Enum_decode (`case <determinant>`, also --acn-v2 deferred): nothing is written before the
            // alternatives, so every alternative must set ErrorCode (a _FullRange alternative does not touch result)
            let alternatives = ch.children |> List.map (fun c -> c.Type, acnErrorCodeSummary r deps lm c.Type)
            // an alternative without content (NULL) is decode_nullType: result := (true, 0)
            { writesE    = alternatives |> List.forall (fun (altType, s) -> s.writesE || not (acnHasDecodeContent altType))
              exitNeedsE = alternatives |> List.exists (fun (_, s) -> s.exitNeedsE) }
    | OctetString o ->
        match o.acnEncodingClass with
        | SZ_EC_FIXED_SIZE -> leaf (not (fixedStringCannotFail lm FixedOctets true))   // octet_FixedSize_noCheck: never
        | SZ_EC_ExternalField _ when (externalFieldUnchecked r deps t FixedOctets o.minSize.acn o.maxSize.acn o.isFixedSize).IsSome ->
            leaf false                                                              // 14c2b: no range check, the read writes Success only
        | SZ_EC_LENGTH_EMBEDDED nBits when varSizeStringCannotFail lm FixedOctets o.isFixedSize o.minSize.acn o.maxSize.acn nBits ->
            leaf false                                                              // 14c2b: octet_VarSize_fullRange
        | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> leaf true
    | Enumerated o -> leaf (acnEnumFullIndex r lm o).IsNone                            // _FullRange read: no ErrorCode
    | IA5String o | NumericString o ->
        match o.acnEncodingClass with
        | Acn_Enc_String_uPER _ -> uperErrorCodeSummary r lm t                        // 14c2b: the uPER body
        | _                     -> leaf true
    | Real o ->
        match o.acnEncodingClass with
        | Real_ScaledInt enc when (realScaledIntFullRange r lm enc).IsSome -> leaf false   // 14j G3a: _FullRange read
        | Real_uPER | Real_IEEE754_32_big_endian | Real_IEEE754_32_little_endian | Real_IEEE754_64_big_endian
        | Real_IEEE754_64_little_endian | Real_ScaledInt _ -> leaf true
    | TimeType _ | ObjectIdentifier _
    | SequenceOf _ -> leaf true

/// An ACN decoder needs result := (true, 0) at its start (see uperErrorCodeSummary).
let acnNeedsResultInit (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (t:Asn1Type) : bool =
    let s = acnErrorCodeSummary r deps lm t
    s.exitNeedsE || not s.writesE

/// Coverage step 10c (q/details-docs/coverage/step10c-plan.md): whether a decoder writes all of val on every path.
/// gnatprove flow requires an `out` parameter to be fully written on return; a decoder that can fail, decodes an
/// array in a loop (flow analysis does not track array elements) or passes val to an RTL `in out` buffer leaves val
/// partly written. Such a *_Decode_aux takes val as `in out`, and its wrapper T_Decode initialises val once.
/// FullWrite keeps `out` (the code before 10c); a type that is MayLeavePartial while its decoder writes everything
/// shows as gnatprove "unused initial value of val" (step10b/flowdiff.sh), a wrong FullWrite as "might not be
/// initialized". With ReadsCanFail (C, Scala, Python, Rust) everything is FullWrite: code unchanged.
type ValWrite =
    | FullWrite
    | MayLeavePartial

let rec private valWrite (outcome: Asn1Type -> DecodeOutcome) (choiceAssigns: Asn1Type -> bool) (acnFieldOutcome: AcnInsertedType -> DecodeOutcome) (hasStatement: Asn1Type -> bool) (acnChildrenDecoded:bool) (acnDeferred:bool) (lm:LanguageMacros) (t:Asn1Type) : ValWrite =
    match lm.lg.decodeReadFailureModel with
    | ReadsCanFail      -> FullWrite
    | ReadsCannotFail   ->
        match t.Kind with
        | Integer _ | Real _ | Boolean _ | Enumerated _ | NullType _
        | ObjectIdentifier _ -> FullWrite                       // one write of val (an RTL `out` parameter or an assignment)
        | BitString o when o.isFixedSize && CommonTypes.bitStringAlignment -> FullWrite   // val.Data := (others => 0) first
        | Sequence o ->
            // the children in decode order: an ASN.1 child is written when it is mandatory, has a statement, is fully
            // written by its own decoder, and no part before it may fail (a failure skips the rest of the body); an ACN
            // inserted field writes nothing of val and may fail (14c2b: unless its read cannot fail)
            let full, _ =
                o.children |> List.fold (fun (full, failedBefore) c ->
                    match c with
                    | AcnChild a  -> full, failedBefore || (acnChildrenDecoded && acnFieldOutcome a.Type = MayFail)
                    | Asn1Child c ->
                        // 14j (G4): a CHOICE child decoded in place (inline, or a reference whose body is inlined; not a
                        // call) whose selection always reaches an arm (choiceAssigns) writes the whole component: every arm starts with `val.c := T'(kind =>
                        // ..., x => init)`; a component is not a parameter, so its discriminant is not read. Before, a
                        // SEQUENCE with such a child (14c2a full index, or an ENUMERATED determinant field that 14c2b
                        // reads with a _FullRange twin) stayed `in out` and gnatprove flow reported "unused initial value
                        // of val". A failure inside the alternative comes after the assignment (outcome below).
                        let childWrite =
                            match choiceAssigns c.Type with
                            | true  -> FullWrite
                            | false -> valWrite outcome choiceAssigns acnFieldOutcome hasStatement acnChildrenDecoded acnDeferred lm c.Type
                        let written =
                            c.Optionality.IsNone && hasStatement c.Type && not failedBefore && childWrite = FullWrite
                        full && written, failedBefore || outcome c.Type = MayFail) (true, false)
            // a SEQUENCE whose children are all NULL without a statement: decode_empty_sequence writes them
            let onlyNullFields =
                o.children |> List.forall (fun c ->
                    match c with
                    | AcnChild _  -> not acnChildrenDecoded
                    | Asn1Child c -> c.Optionality.IsNone && not (hasStatement c.Type))
            match full || onlyNullFields with
            | true  -> FullWrite
            | false -> MayLeavePartial
        | ReferenceType o ->
            match o.encodingOptions with
            | None     -> valWrite outcome choiceAssigns acnFieldOutcome hasStatement acnChildrenDecoded acnDeferred lm o.resolvedType
            | Some enc ->                                                               // CONTAINING
                // --acn-v2 decodes a fixed-size CONTAINING by a plain call of the contained type's decoder
                // (DAstACNDeferred: octet / bit_string_containing_deferred_fixed_func); everything else checks
                // the region first
                match acnDeferred, enc.acnEncodingClass with
                | true, SZ_EC_FIXED_SIZE -> valWrite outcome choiceAssigns acnFieldOutcome hasStatement acnChildrenDecoded acnDeferred lm o.resolvedType
                // 14c2b: a CONTAINING with ACN arguments is a call of a specialised function, whose val has the mode of
                // the contained type (DAstACNDeferred: acnValWrite of resolvedType); an ACN field before it no longer
                // hides this since its read cannot fail (25-ACNV2-BOUNDARIES/002: "unused initial value of val")
                | true, (SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced) when not o.acnArguments.IsEmpty ->
                    valWrite outcome choiceAssigns acnFieldOutcome hasStatement acnChildrenDecoded acnDeferred lm o.resolvedType
                | true, (SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced)
                | false, _ -> MayLeavePartial
        // a CHOICE is a variant record: the discriminant of an `out` parameter is read at the call, so the caller would
        // pass an unset `kind` (gnatprove "val.x.kind is not set"); it stays `in out` even when every arm assigns val
        // (14c2a, full index)
        | IA5String _ | NumericString _ | OctetString _ | BitString _ | TimeType _
        | SequenceOf _ | Choice _ -> MayLeavePartial

/// 14j (G4): whether the ACN decoder of a CHOICE reaches an arm on every path, i.e. assigns the whole value
/// (ChoiceChild_Enum_decode / ChoiceChild_decode start with `<p> := T'(kind => ..., x => init)`): a full index
/// (Choice_fullIndex, no guard), or an ENUMERATED determinant held in a local of the enumeration type
/// (Choice_Enum_decode without `when others`: not an --acn-v2 deferred ref, AcnChoice.isDeferredDeterminant) and no
/// ALWAYS ABSENT alternative (its Ada arm only writes a failure). Choice_decode (guarded index) and present-when
/// (else arm fails) may skip every arm.
let acnChoiceSelectionAssigns (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (t:Asn1Type) : bool =
    match t.Kind with
    | Choice o ->
        let noAlwaysAbsent = o.children |> List.forall (fun c -> c.Optionality <> Some ChoiceAlwaysAbsent)
        match o.acnProperties.enumDeterminant with
        | None   -> (acnChoiceFullIndex r deps lm o).IsSome
        | Some _ ->
            match deps.acnDependencies |> List.tryFind (fun d -> d.asn1Type = t.id) with
            | None   -> false
            | Some d ->
                match r.args.acnDeferred, d.determinant with
                | true, AcnParameterDeterminant _   -> false                        // deferred ref: `when others` fails
                | true, AcnChildDeterminant _
                | false, _                          -> noAlwaysAbsent
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | BitString _ | Boolean _ | TimeType _
    | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | NullType _ | ReferenceType _ -> false

/// 14j: a CHOICE child decoded in place (an inline CHOICE, or a reference whose body is inlined: the reference rules of
/// acnValInitUnused) whose selection always reaches an arm (acnChoiceSelectionAssigns); a call passes val on.
let rec acnChoiceChildAssigns (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (t:Asn1Type) : bool =
    match t.Kind with
    | Choice _ -> acnChoiceSelectionAssigns r deps lm t
    | ReferenceType o ->
        match o.encodingOptions, o.hasExtraConstrainsOrChildrenOrAcnArgs with
        | Some _, _         -> false
        | None, true when r.args.acnDeferred && not o.acnArguments.IsEmpty -> false     // call of a specialised function
        | None, true        -> acnChoiceChildAssigns r deps lm o.resolvedType                // the base body inlined
        | None, false       ->
            match isLiveDeduced o.resolvedType with
            | true  -> acnChoiceChildAssigns r deps lm o.resolvedType                        // inlined
            | false -> false                                                            // call of T_ACN_Decode_aux
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | BitString _ | Boolean _ | TimeType _
    | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | NullType _ -> false

/// uPER *_Decode_aux: does it write all of val on every path?
let uperValWrite (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : ValWrite =
    // a uPER CHOICE decoded in place reaches an arm on every path only with a full index (choice_fullIndex: no guard);
    // a reference is decoded in place when it is not equivalent to its type assignment (uperValInitUnused)
    let rec choiceAssigns (c:Asn1Type) =
        match c.Kind with
        | Choice o -> (uperChoiceFullIndex r lm o).IsSome
        | ReferenceType o ->
            match o.encodingOptions with
            | Some _ -> false
            | None   ->
                match TypesEquivalence.uperEquivalence (GetActualTypeByName r o.modName o.tasName) o.resolvedType with
                | true  -> false                                                    // call of T_Decode_aux
                | false -> choiceAssigns o.resolvedType                             // decoded in place
        | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | BitString _ | Boolean _ | TimeType _
        | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | NullType _ -> false
    valWrite (uper r lm) choiceAssigns (fun _ -> MayFail) uperChildHasStatement false false lm t

/// ACN *_ACN_Decode_aux (also the --acn-v2 specialised functions): does it write all of val on every path?
let acnValWrite (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (t:Asn1Type) : ValWrite =
    valWrite (acn r deps lm) (acnChoiceChildAssigns r deps lm) (fun a -> fst (acnInsertedFieldPart r lm a)) acnHasDecodeContent true r.args.acnDeferred lm t

/// Coverage step 14c2a: a decoder whose body is a CHOICE with a full index writes all of val on every path (each arm
/// assigns `val := T'(kind => ..., x => init)`), yet val stays `in out`: a CHOICE is a variant record, and the
/// discriminant of an `out` parameter is read at the call (gnatprove "val.x.kind is not set" in the caller). Its
/// initial value is then never read: gnatprove "unused initial value of val", suppressed on the declaration
/// (EmitTypeAssignment_def / EmitTypeAssignment_primitive_def: bValInitUnused; George, 2026-11-02). A reference
/// decoded by a call passes val on (a read); one decoded in place has the referenced body.
let rec uperValInitUnused (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : bool =
    match t.Kind with
    | Choice _ ->
        match uper r lm t with
        | AlwaysSucceeds _ -> true
        | MayFail          -> false
    | ReferenceType o ->
        match o.encodingOptions with
        | Some _ -> false
        | None   ->
            match TypesEquivalence.uperEquivalence (GetActualTypeByName r o.modName o.tasName) o.resolvedType with
            | true  -> false                                                        // call of T_Decode_aux
            | false -> uperValInitUnused r lm o.resolvedType                        // decoded in place
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | BitString _ | Boolean _ | TimeType _
    | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | NullType _ -> false

/// ACN twin of uperValInitUnused (the reference rules of acn).
let rec acnValInitUnused (r:AstRoot) (deps:AcnInsertedFieldDependencies) (lm:LanguageMacros) (t:Asn1Type) : bool =
    match t.Kind with
    | Choice _ ->
        match acn r deps lm t with
        | AlwaysSucceeds _ -> true
        | MayFail          -> false
    | ReferenceType o ->
        match o.encodingOptions, o.hasExtraConstrainsOrChildrenOrAcnArgs with
        | Some _, _         -> false
        | None, true when r.args.acnDeferred && not o.acnArguments.IsEmpty -> false     // call of a specialised function
        | None, true        -> acnValInitUnused r deps lm o.resolvedType                     // the base body inlined
        | None, false       ->
            match isLiveDeduced o.resolvedType with
            | true  -> acnValInitUnused r deps lm o.resolvedType                             // inlined
            | false -> false                                                            // call of T_ACN_Decode_aux
    | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | BitString _ | Boolean _ | TimeType _
    | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | NullType _ -> false

/// The `in out` mode of val needs the init function the wrapper calls.
let valInOut (vw:ValWrite) (soInitFuncName:string option) : bool =
    match vw, soInitFuncName with
    | MayLeavePartial, Some _ -> true
    | MayLeavePartial, None
    | FullWrite, _            -> false
