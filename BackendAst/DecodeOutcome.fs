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
            match fixedStringCannotFail lm FixedOctets (o.isFixedSize && o.maxSize.uper < 65536I) with
            | true  -> AlwaysSucceeds WritesResult
            | false -> MayFail
        | BitString o ->
            match fixedStringCannotFail lm FixedBits (o.isFixedSize && o.maxSize.uper < 65536I) with
            | true  -> AlwaysSucceeds WritesResult
            | false -> MayFail
        | Real _ | IA5String _ | NumericString _ | NullType _ | TimeType _
        | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Choice _ -> MayFail

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
    | BitString o -> leaf (not (o.isFixedSize && o.maxSize.uper < 65536I))            // fixed: only on failure, or never (_noCheck)
    | OctetString o -> leaf (not (fixedStringCannotFail lm FixedOctets (o.isFixedSize && o.maxSize.uper < 65536I)))  // octet_FixedSize_noCheck
    | NullType _ -> leaf false
    | Real _ | IA5String _ | NumericString _ | TimeType _ | Enumerated _ | ObjectIdentifier _
    | SequenceOf _ | Choice _ -> leaf true
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

/// ACN decode outcome of a type decoded in place (first scope of step 10b: SEQUENCEs with ACN inserted fields,
/// present-when, pre-decoding functions, and every kind not listed stay MayFail).
let rec acn (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : DecodeOutcome =
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
            let hasAcnChild = o.children |> List.exists (fun c -> match c with AcnChild _ -> true | Asn1Child _ -> false)
            match hasAcnChild, o.acnProperties.preDecodingFunction, o.acnProperties.postEncodingFunction with
            | true, _, _ | _, Some _, _ | _, _, Some _ -> MayFail
            | false, None, None ->
                let asn1Children = o.children |> List.choose (fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
                let presenceBits = asn1Children |> List.filter acnHasPresenceBit |> List.map (fun _ -> AlwaysSucceeds WritesResult)
                let parts = asn1Children |> List.choose (fun c -> acnChildPart r lm c.Optionality c.Type |> Option.map (fun (outcome, _) -> c, outcome))
                let unconditional, conditional =
                    parts |> List.partition (fun (c, _) -> match c.Optionality with Some (Optional _) -> false | None | Some AlwaysPresent | Some AlwaysAbsent -> true)
                match presenceBits, parts with
                | [], [] -> AlwaysSucceeds WritesResult                             // decode_empty_sequence: result := (true, 0)
                | _, _   -> combine (presenceBits @ (unconditional |> List.map snd)) (conditional |> List.map snd)
        | ReferenceType o ->
            match o.encodingOptions, o.hasExtraConstrainsOrChildrenOrAcnArgs with
            | Some _, _         -> MayFail                                          // CONTAINING
            | None, true when r.args.acnDeferred && not o.acnArguments.IsEmpty ->
                MayFail                                                             // --acn-v2: call of a specialised function
            | None, true        -> acn r lm o.resolvedType                          // the base body inlined
            | None, false       ->
                match isLiveDeduced o.resolvedType with
                | true  -> MayFail                                                  // inlined with a trailing constant
                | false ->                                                          // call of T_ACN_Decode_aux
                    match acn r lm (GetActualTypeByName r o.modName o.tasName) with
                    | AlwaysSucceeds _ -> AlwaysSucceeds WritesResult
                    | MayFail          -> MayFail
        | OctetString o ->
            match o.acnEncodingClass with
            | SZ_EC_FIXED_SIZE when fixedStringCannotFail lm FixedOctets true -> AlwaysSucceeds WritesResult
            | SZ_EC_FIXED_SIZE | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> MayFail
        | BitString o ->
            match o.acnEncodingClass with
            | SZ_EC_FIXED_SIZE when fixedStringCannotFail lm FixedBits true -> AlwaysSucceeds WritesResult
            | SZ_EC_FIXED_SIZE | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> MayFail
        | Real _ | IA5String _ | NumericString _ | NullType _ | TimeType _
        | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Choice _ -> MayFail

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
and acnChildPart (r:AstRoot) (lm:LanguageMacros) (optionality:Asn1Optionality option) (childType:Asn1Type) : (DecodeOutcome * ErrorCodeSummary) option =
    let leaf writesE = { writesE = writesE; exitNeedsE = false }
    match optionality, acnHasDecodeContent childType with
    | Some AlwaysAbsent, _                                      -> Some (AlwaysSucceeds WritesResult, leaf true)
    | Some AlwaysPresent, false                                 -> Some (AlwaysSucceeds NoResultWrite, leaf false)
    | Some (Optional opt), false when opt.acnPresentWhen.IsSome -> Some (MayFail, leaf false)
    | Some (Optional _), false
    | None, false                                               -> None
    | Some (Optional opt), true ->
        let outcome =
            match opt.acnPresentWhen with
            | Some _ -> MayFail
            | None   -> acn r lm childType
        Some (outcome, { acnErrorCodeSummary r lm childType with writesE = false })
    | Some AlwaysPresent, true
    | None, true                -> Some (acn r lm childType, acnErrorCodeSummary r lm childType)

/// ErrorCode summary of an ACN decode part (see uperErrorCodeSummary; ACN templates).
and acnErrorCodeSummary (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : ErrorCodeSummary =
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
                    | AcnChild _  -> Some (MayFail, leaf true)
                    | Asn1Child c ->
                        acnChildPart r lm c.Optionality c.Type
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
        | None, true    -> acnErrorCodeSummary r lm o.resolvedType
        | None, false   ->
            match isLiveDeduced o.resolvedType with
            | true  -> acnErrorCodeSummary r lm o.resolvedType
            | false -> leaf true                                                      // call: the callee sets result
    | BitString o ->
        match o.acnEncodingClass with
        | SZ_EC_FIXED_SIZE -> leaf false                                            // bitString_FixSize: only on failure (ACN does not fragment)
        | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> leaf true
    | NullType n -> leaf n.acnProperties.encodingPattern.IsSome                       // Null_pattern: the call, then ErrorCode
    | Choice ch ->
        match ch.acnProperties.enumDeterminant with
        | None   -> leaf true       // Choice_decode sets ErrorCode before the index, Choice_preWhen_decode result := (true, 0)
        | Some _ ->
            // Choice_Enum_decode (`case <determinant>`, also --acn-v2 deferred): nothing is written before the
            // alternatives, so every alternative must set ErrorCode (a _FullRange alternative does not touch result)
            let alternatives = ch.children |> List.map (fun c -> c.Type, acnErrorCodeSummary r lm c.Type)
            // an alternative without content (NULL) is decode_nullType: result := (true, 0)
            { writesE    = alternatives |> List.forall (fun (altType, s) -> s.writesE || not (acnHasDecodeContent altType))
              exitNeedsE = alternatives |> List.exists (fun (_, s) -> s.exitNeedsE) }
    | OctetString o ->
        match o.acnEncodingClass with
        | SZ_EC_FIXED_SIZE -> leaf (not (fixedStringCannotFail lm FixedOctets true))   // octet_FixedSize_noCheck: never
        | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> leaf true
    | Real _ | IA5String _ | NumericString _ | TimeType _ | Enumerated _ | ObjectIdentifier _
    | SequenceOf _ -> leaf true

/// An ACN decoder needs result := (true, 0) at its start (see uperErrorCodeSummary).
let acnNeedsResultInit (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : bool =
    let s = acnErrorCodeSummary r lm t
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

let rec private valWrite (outcome: Asn1Type -> DecodeOutcome) (hasStatement: Asn1Type -> bool) (acnChildrenDecoded:bool) (acnDeferred:bool) (lm:LanguageMacros) (t:Asn1Type) : ValWrite =
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
            // inserted field writes nothing of val and may fail
            let full, _ =
                o.children |> List.fold (fun (full, failedBefore) c ->
                    match c with
                    | AcnChild _  -> full, failedBefore || acnChildrenDecoded
                    | Asn1Child c ->
                        let written =
                            c.Optionality.IsNone && hasStatement c.Type && not failedBefore
                            && valWrite outcome hasStatement acnChildrenDecoded acnDeferred lm c.Type = FullWrite
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
            | None     -> valWrite outcome hasStatement acnChildrenDecoded acnDeferred lm o.resolvedType
            | Some enc ->                                                               // CONTAINING
                // --acn-v2 decodes a fixed-size CONTAINING by a plain call of the contained type's decoder
                // (DAstACNDeferred: octet / bit_string_containing_deferred_fixed_func); everything else checks
                // the region first
                match acnDeferred, enc.acnEncodingClass with
                | true, SZ_EC_FIXED_SIZE -> valWrite outcome hasStatement acnChildrenDecoded acnDeferred lm o.resolvedType
                | true, (SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_ExternalField _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced)
                | false, _ -> MayLeavePartial
        | IA5String _ | NumericString _ | OctetString _ | BitString _ | TimeType _
        | SequenceOf _ | Choice _ -> MayLeavePartial

/// uPER *_Decode_aux: does it write all of val on every path?
let uperValWrite (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : ValWrite =
    valWrite (uper r lm) uperChildHasStatement false false lm t

/// ACN *_ACN_Decode_aux (also the --acn-v2 specialised functions): does it write all of val on every path?
let acnValWrite (r:AstRoot) (lm:LanguageMacros) (t:Asn1Type) : ValWrite =
    valWrite (acn r lm) acnHasDecodeContent true r.args.acnDeferred lm t

/// The `in out` mode of val needs the init function the wrapper calls.
let valInOut (vw:ValWrite) (soInitFuncName:string option) : bool =
    match vw, soInitFuncName with
    | MayLeavePartial, Some _ -> true
    | MayLeavePartial, None
    | FullWrite, _            -> false
