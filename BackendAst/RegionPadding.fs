/// Coverage cycle 15e: the residues mod 8 of the sizes of every ACN encoding of the contents of an OCTET STRING
/// CONTAINING region, computed from the AST, and what they decide about the zero padding of the region to whole octets.
///
/// The encoders of an OCTET STRING (CONTAINING X) region pad the contents to a whole number of octets from the region's
/// start. When every encoding of X is a whole number of octets (residues {0}) the padding is never needed; when none is
/// (0 not a residue) it is always needed; otherwise both happen. The templates then drop the test of whether padding is
/// needed (the decision a test can never take both ways) but keep a padding statement whose amount is computed from the
/// stream position, so a wrong claim cannot change a single written bit: it only hides or leaves a decision. Ada states
/// the claimed residues with `pragma Assume`, which the `-gnata` runs check at every encoding.
///
/// The analysis is a sound over-approximation: every rule gives a superset of the residues an encoder can write, and a
/// shape it does not model (alignment, fragmentation, a size that is not min + k * step) gives ResiduesUnknown, which
/// makes no claim. It analyses the type whose encoder actually writes the region (q/issues.md Issues 135 / 138: a
/// legacy site calls the referenced type assignment's encoder, acn-v2 a specialised one with the merged contents), the
/// union of the candidates when more than one may run.
module RegionPadding

open System.Numerics
open FsUtils
open CommonTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions

/// The residues mod 8 (each in 0 .. 7, never empty) of the sizes in bits of every encoding of a type, or unknown.
type Residues =
    | Residues of Set<int>
    | ResiduesUnknown

/// What the analysis decides about the zero padding of an OCTET STRING CONTAINING region (external size or embedded
/// length, and the region of a parameterised CHOICE).
type RegionPadding =
    /// coverage step 14e (E3): every encoding of the contents takes this many whole octets (one size), no padding code
    | PadFixedOctets of BigInteger
    /// every size has one of these residues mod 8: either {0} (never padded) or a set without 0 (always padded); the
    /// test whether padding is needed is dropped, the computed amount stays (0 bits when no padding is needed)
    | PadDecided of Set<int>
    /// both outcomes are possible, or the residues are unknown: the test stays
    | PadChecked

/// The padding of a C fixed-size (SZ_EC_FIXED_SIZE) OCTET STRING CONTAINING region of n octets.
type FixedRegionPadding =
    /// coverage step 14e (E3): every encoding of the contents fills the n octets
    | FixedFilled of BigInteger
    /// the contents take at most this many bits, fewer than 8 n: the padding amount is always positive
    | FixedPadAlways of BigInteger
    | FixedPadChecked

let private modulo8 (b: BigInteger) : int = int (((b % 8I) + 8I) % 8I)

let private ofSize (b: BigInteger) : Residues = Residues (Set.singleton (modulo8 b))

let private union (a: Residues) (b: Residues) : Residues =
    match a, b with
    | Residues x, Residues y -> Residues (Set.union x y)
    | ResiduesUnknown, _ | _, ResiduesUnknown -> ResiduesUnknown

/// the residues of x + y, x of a, y of b
let private add (a: Residues) (b: Residues) : Residues =
    match a, b with
    | Residues x, Residues y -> Residues (Set.ofSeq (seq { for i in x do for j in y do yield (i + j) % 8 }))
    | ResiduesUnknown, _ | _, ResiduesUnknown -> ResiduesUnknown

/// the union over n in [lo, hi] of the residues of a sum of n sizes, each with a residue of e. The sums s_n = s_(n-1) + e
/// run over at most 256 sets, so they are periodic from some index on: the union over 257 consecutive n holds every set
/// that occurs from lo on.
let private repeated (lo: BigInteger) (hi: BigInteger) (e: Residues) : Residues =
    match e, hi < lo || lo < 0I with
    | ResiduesUnknown, _ | _, true -> ResiduesUnknown
    | Residues _, false ->
        // the states s_0, s_1, ... up to the first repetition
        let rec states (acc: Residues list) (cur: Residues) =
            match List.tryFindIndex ((=) cur) acc with
            | Some i -> List.rev acc, i
            | None   -> states (cur :: acc) (add cur e)
        let seen, cycleStartFromEnd = states [] (Residues (Set.singleton 0))
        let n0 = seen.Length
        let cycleStart = n0 - 1 - cycleStartFromEnd
        let period = BigInteger (n0 - cycleStart)
        let stateAt (n: BigInteger) : Residues =
            match n < BigInteger cycleStart with
            | true  -> seen.[int n]
            | false -> seen.[cycleStart + int ((n - BigInteger cycleStart) % period)]
        let last = BigInteger.Min (hi, lo + 256I)
        Seq.unfold (fun n -> match n > last with true -> None | false -> Some (stateAt n, n + 1I)) lo
        |> Seq.fold union (Residues Set.empty)

/// sizes mn + k * u, 0 <= k <= (mx - mn) / u
let private stepped (mn: BigInteger) (mx: BigInteger) (u: BigInteger) : Residues =
    match mx < mn || u <= 0I || (mx - mn) % u <> 0I with
    | true  -> ResiduesUnknown
    | false -> add (ofSize mn) (repeated 0I ((mx - mn) / u) (ofSize u))

let private fixedOrUnknown (mn: BigInteger) (mx: BigInteger) : Residues =
    match mn = mx with
    | true  -> ofSize mn
    | false -> ResiduesUnknown

let private alignedOrNot (alignment: AcnGenericTypes.AcnAlignment option) (f: unit -> Residues) : Residues =
    match alignment with
    | Some _ -> ResiduesUnknown   // the size depends on where the encoding starts
    | None   -> f ()

/// INTEGER / ENUMERATED / ACN INTEGER: a fixed size, or the null-terminated ASCII (8 bits per digit) / BCD (4 bits)
/// classes; the variable uPER class (unconstrained or extensible: `1 + root` or `9 + 8 k`) and a constant-size class
/// with different sizes (not expected) are unknown
let private intResidues (ec: IntEncodingClass) (mn: BigInteger) (mx: BigInteger) : Residues =
    match mn = mx with
    | true  -> ofSize mn
    | false ->
        match ec with
        | ASCII_VarSize_NullTerminated _ | ASCII_UINT_VarSize_NullTerminated _ -> stepped mn mx 8I
        | BCD_VarSize_NullTerminated _ -> stepped mn mx 4I
        | Integer_uPER
        | PositiveInteger_ConstSize_8 | PositiveInteger_ConstSize_big_endian_16 | PositiveInteger_ConstSize_little_endian_16
        | PositiveInteger_ConstSize_big_endian_32 | PositiveInteger_ConstSize_little_endian_32
        | PositiveInteger_ConstSize_big_endian_64 | PositiveInteger_ConstSize_little_endian_64 | PositiveInteger_ConstSize _
        | TwosComplement_ConstSize_8 | TwosComplement_ConstSize_big_endian_16 | TwosComplement_ConstSize_little_endian_16
        | TwosComplement_ConstSize_big_endian_32 | TwosComplement_ConstSize_little_endian_32
        | TwosComplement_ConstSize_big_endian_64 | TwosComplement_ConstSize_little_endian_64 | TwosComplement_ConstSize _
        | ASCII_ConstSize _ | ASCII_UINT_ConstSize _ | BCD_ConstSize _ -> ResiduesUnknown

/// the smallest SIZE maximum whose uPER-like encoding fragments (blocks of 16K units with headers, uPER.getSizeableTypeSize)
let private fragmentationFrom = 65536I

/// IA5String / NumericString: a length field (or terminator) of fixed width + characters of charSizeInBits each
let private stringResidues (s: StringType) : Residues =
    match s.acnMinSizeInBits = s.acnMaxSizeInBits, s.maxSize.uper >= fragmentationFrom || s.maxSize.acn >= fragmentationFrom with
    | true, _      -> ofSize s.acnMinSizeInBits
    | false, true  -> ResiduesUnknown   // fragmentation headers
    | false, false ->
        match s.acnEncodingClass with
        | Acn_Enc_String_uPER c
        | Acn_Enc_String_uPER_Ascii c
        | Acn_Enc_String_Ascii_Null_Terminated (c, _)
        | Acn_Enc_String_Ascii_External_Field_Determinant (c, _)
        | Acn_Enc_String_CharIndex_External_Field_Determinant (c, _)
        | Acn_Enc_String_Ascii_Deduced c -> stepped s.acnMinSizeInBits s.acnMaxSizeInBits c

/// the bits a SEQUENCE OF / CONTAINING region writes besides its items / contents
let private sizeableOverhead (ec: SizeableAcnEncodingClass) : BigInteger =
    match ec with
    | SZ_EC_FIXED_SIZE | SZ_EC_ExternalField _ | SZ_EC_Deduced -> 0I
    | SZ_EC_LENGTH_EMBEDDED w -> w
    | SZ_EC_TerminationPattern tp -> BigInteger tp.Value.Length

/// OCTET / BIT STRING: items of `unit` bits after a fixed overhead (sizes min + k * unit)
let private sizeableStringResidues (mn: BigInteger) (mx: BigInteger) (maxSize: SIZE) (unit: BigInteger) : Residues =
    match mn = mx, maxSize.uper >= fragmentationFrom || maxSize.acn >= fragmentationFrom with
    | true, _      -> ofSize mn
    | false, true  -> ResiduesUnknown
    | false, false -> stepped mn mx unit

/// The alternatives of a CHOICE that its encoding can select (an ALWAYS ABSENT alternative is not encoded, AcnChoice
/// isAcnChild; all of them when every one is ALWAYS ABSENT, as AcnEncodingClasses.GetChoiceEncodingClass counts).
let private encodedAlternatives (ch: Choice) : ChChildInfo list =
    let isEncoded (c: ChChildInfo) =
        match c.Optionality with
        | Some ChoiceAlwaysAbsent -> false
        | Some ChoiceAlwaysPresent | None -> true
    match ch.children |> List.filter isEncoded with
    | []      -> ch.children
    | encoded -> encoded

/// A CHOICE alternative that is a CONTAINING sized by the region parameter of its CHOICE: it writes only its contents,
/// the wrapper around the CHOICE measures / pads the region (DAstACNDeferred, Choice.containingRegion).
let private sizedByRegion (regionPrm: string option) (rf: ReferenceType) : bool =
    match rf.encodingOptions, regionPrm with
    | None, _ | Some _, None -> false
    | Some eo, Some prm ->
        match eo.acnEncodingClass with
        | SZ_EC_ExternalField rp -> rp.asStringList = [prm]
        | SZ_EC_FIXED_SIZE | SZ_EC_LENGTH_EMBEDDED _ | SZ_EC_TerminationPattern _ | SZ_EC_Deduced -> false

let rec private typeResidues (r: AstRoot) (t: Asn1Type) : Residues =
    alignedOrNot t.acnAlignment (fun () ->
        match t.Kind with
        | Integer i    -> intResidues i.acnEncodingClass i.acnMinSizeInBits i.acnMaxSizeInBits
        | Enumerated e -> intResidues e.acnEncodingClass e.acnMinSizeInBits e.acnMaxSizeInBits
        | Real rl ->
            match rl.acnMinSizeInBits = rl.acnMaxSizeInBits, rl.acnEncodingClass with
            | true, _ -> ofSize rl.acnMinSizeInBits
            | false, Real_ScaledInt ic -> intResidues ic rl.acnMinSizeInBits rl.acnMaxSizeInBits
            | false, Real_uPER -> stepped rl.acnMinSizeInBits rl.acnMaxSizeInBits 8I   // a length octet + content octets
            | false, (Real_IEEE754_32_big_endian | Real_IEEE754_64_big_endian | Real_IEEE754_32_little_endian | Real_IEEE754_64_little_endian) ->
                ResiduesUnknown
        | IA5String s | NumericString s -> stringResidues s
        | OctetString o -> sizeableStringResidues o.acnMinSizeInBits o.acnMaxSizeInBits o.maxSize 8I
        | BitString b   -> sizeableStringResidues b.acnMinSizeInBits b.acnMaxSizeInBits b.maxSize 1I
        | NullType n         -> fixedOrUnknown n.acnMinSizeInBits n.acnMaxSizeInBits
        | Boolean b          -> fixedOrUnknown b.acnMinSizeInBits b.acnMaxSizeInBits
        | ObjectIdentifier o -> fixedOrUnknown o.acnMinSizeInBits o.acnMaxSizeInBits
        | TimeType tt        -> fixedOrUnknown tt.acnMinSizeInBits tt.acnMaxSizeInBits
        | SequenceOf sq ->
            match sq.maxSize.uper >= fragmentationFrom || sq.maxSize.acn >= fragmentationFrom with
            | true  -> ResiduesUnknown
            | false -> add (ofSize (sizeableOverhead sq.acnEncodingClass)) (repeated sq.minSize.acn sq.maxSize.acn (typeResidues r sq.child))
        | Sequence sq ->
            // the presence bits: OPTIONAL children without present-when (AcnCreateFromAntlr, acnBitMaskSize)
            let presenceBits =
                sq.children |> List.sumBy (fun c ->
                    match c with
                    | Asn1Child a ->
                        match a.Optionality with
                        | Some (Optional p) ->
                            match p.acnPresentWhen with
                            | None   -> 1
                            | Some _ -> 0
                        | Some AlwaysAbsent | Some AlwaysPresent | None -> 0
                    | AcnChild _ -> 0)
            sq.children |> List.fold (fun acc c ->
                let childResidues =
                    match c with
                    | Asn1Child a ->
                        match a.Optionality with
                        | None | Some AlwaysPresent -> typeResidues r a.Type
                        | Some AlwaysAbsent -> ofSize 0I
                        | Some (Optional _) -> union (ofSize 0I) (typeResidues r a.Type)
                    | AcnChild a -> acnInsertedResidues a.Type
                add acc childResidues) (ofSize (BigInteger presenceBits))
        | Choice ch ->
            match ch.containingRegion with
            | Some (_, ContainedInOctString) -> ofSize 0I   // --acn-v2: the region around the CHOICE pads it to whole octets
            | Some (prm, ContainedInBitString) -> choiceBodyResiduesOf r ch (Some prm)
            | None -> choiceBodyResiduesOf r ch None
        | ReferenceType rf ->
            match rf.encodingOptions with
            | None ->
                // q/issues.md Issue 91: a site whose chain reaches a CONTAINING calls that type assignment's function
                match DAstUtilFunctions.containingCalleeTas r rf with
                | Some tas -> union (typeResidues r rf.resolvedType) (typeResidues r tas.Type)
                | None     -> typeResidues r rf.resolvedType
            | Some eo ->
                match eo.octOrBitStr with
                | ContainedInOctString -> ofSize (sizeableOverhead eo.acnEncodingClass)   // a region of whole octets
                | ContainedInBitString ->
                    // a region of exactly the bits of its contents, written by the site's merged contents (acn-v2,
                    // external size) or by the referenced type assignment's encoder (legacy embedded / fixed)
                    add (ofSize (sizeableOverhead eo.acnEncodingClass)) (calledContentsResidues r rf))

/// The body of a CHOICE (index + one alternative). In the region of a parameterised CHOICE (regionPrm), an alternative
/// that is a CONTAINING sized by that parameter writes only its contents (the wrapper around the CHOICE measures / pads).
and private choiceBodyResiduesOf (r: AstRoot) (ch: Choice) (regionPrm: string option) : Residues =
    let encoded = encodedAlternatives ch
    // the index the CHOICE writes (AcnEncodingClasses.GetChoiceEncodingClass, AcnChoice nIndexSizeInBits)
    let indexBits =
        match ch.acnProperties.enumDeterminant, encoded |> List.exists (fun c -> not c.acnPresentWhenConditions.IsEmpty) with
        | Some _, _ | None, true -> 0I
        | None, false -> GetChoiceUperDeterminantLengthInBits (BigInteger encoded.Length)
    let alternative (c: ChChildInfo) : Residues =
        match c.Type.Kind with
        | ReferenceType rf ->
            match sizedByRegion regionPrm rf with
            | true  -> alignedOrNot c.Type.acnAlignment (fun () -> calledContentsResidues r rf)
            | false -> typeResidues r c.Type
        | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | NullType _ | BitString _ | Boolean _
        | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | Choice _ | TimeType _ -> typeResidues r c.Type
    add (ofSize indexBits) (encoded |> List.map alternative |> List.fold union (Residues Set.empty))

/// The residues of the contents of a CONTAINING site in place (no alignment inside: unknown otherwise): the union of the
/// site's merged contents and the contents whose encoder the site calls (DAstUtilFunctions.containedCalleeType).
and private calledContentsResidues (r: AstRoot) (rf: ReferenceType) : Residues =
    let callee = DAstUtilFunctions.containedCalleeType r rf
    match rf.resolvedType.maxAlignment, callee.maxAlignment with
    | None, None -> union (typeResidues r rf.resolvedType) (typeResidues r callee)
    | Some _, _ | _, Some _ -> ResiduesUnknown

and private acnInsertedResidues (a: AcnInsertedType) : Residues =
    alignedOrNot a.acnAlignment (fun () ->
        match a with
        | AcnInteger i -> intResidues i.acnEncodingClass i.acnMinSizeInBits i.acnMaxSizeInBits
        | AcnNullType n -> fixedOrUnknown n.acnMinSizeInBits n.acnMaxSizeInBits
        | AcnBoolean b -> fixedOrUnknown b.acnMinSizeInBits b.acnMaxSizeInBits
        | AcnReferenceToEnumerated e -> intResidues e.enumerated.acnEncodingClass e.enumerated.acnMinSizeInBits e.enumerated.acnMaxSizeInBits
        | AcnReferenceToIA5String s -> stringResidues s.str)

/// The residues of a type as the contents of a region: unknown with an alignment anywhere inside (its size then depends
/// on where the region starts), and unknown when the front end's smallest or largest size is not among them (the sizes
/// REQUIRED_BITS uses must agree with the analysis; a cheap cross-check, q/details-docs/coverage/cycles/15e Fable F4).
let private contentsResidues (r: AstRoot) (contents: Asn1Type) : Residues =
    match contents.maxAlignment with
    | Some _ -> ResiduesUnknown
    | None ->
        match typeResidues r contents with
        | ResiduesUnknown -> ResiduesUnknown
        | Residues s ->
            match s.Contains (modulo8 contents.acnMinSizeInBits) && s.Contains (modulo8 contents.acnMaxSizeInBits) with
            | true  -> Residues s
            | false -> ResiduesUnknown

/// The union of the residues of several candidate contents (when the analysis cannot tell which encoder runs).
let private unionOf (r: AstRoot) (candidates: Asn1Type list) : Residues =
    candidates |> List.map (contentsResidues r) |> List.fold union (Residues Set.empty)

let private decide (fixedOctets: BigInteger option) (residues: Residues) : RegionPadding =
    match fixedOctets, residues with
    | Some n, _ -> PadFixedOctets n
    | None, ResiduesUnknown -> PadChecked
    | None, Residues s ->
        match Set.toList s with
        | []  -> PadChecked
        | [0] -> PadDecided s     // never padded
        | l   ->
            match List.contains 0 l with
            | true  -> PadChecked     // padded or not: both outcomes are real behaviours (tests)
            | false -> PadDecided s   // always padded

/// The contents an OCTET STRING CONTAINING site writes: the site's merged contents (acn-v2 specialised sites, external
/// sizes, the inline body of a wrapper) and the type whose encoder the site calls (legacy embedded-length / fixed-size
/// sites: the referenced type assignment, Issues 134 / 138; DAstUtilFunctions.containedCalleeType). Both are analysed
/// and united: when they are the same encoding nothing is lost, when they differ the union usually makes no claim.
let private siteContents (r: AstRoot) (o: ReferenceType) : Asn1Type list =
    [o.resolvedType; DAstUtilFunctions.containedCalleeType r o]

/// The padding of an OCTET STRING CONTAINING region at a reference site (acn-v2 external size / embedded length, legacy
/// embedded length / fixed size); fixedOctets = the E3 result (containingFixedOctets), which keeps its output.
let regionPadding (r: AstRoot) (o: ReferenceType) (fixedOctets: BigInteger option) : RegionPadding =
    decide fixedOctets (unionOf r (siteContents r o))

/// The legacy embedded-length / fixed-size region (AcnReference, octet_string_containing_func): the contents are
/// encoded into the temporary stream by the function the site calls, which is always the referenced type assignment's
/// (DAstUtilFunctions.getReferenceCallFuncName → containedCalleeTas for a site with encoding options; --acn-v2 never
/// reaches this template for these classes, DAstACNDeferred has its own arms), whatever the site's specification says:
/// its residues are those of containedCalleeType alone (cycle 15e review r1, P3-2: the union with the merged contents
/// made no claim at a site with a child override although only one encoding is ever written).
let legacyCalledRegionPadding (r: AstRoot) (o: ReferenceType) : RegionPadding =
    decide None (unionOf r [DAstUtilFunctions.containedCalleeType r o])

/// The legacy size update of an OCTET STRING CONTAINING with an external size (AcnDependencies
/// handleSizeDeterminantContaining): it encodes the inline body of o.resolvedType.ActualType.
let legacySizeUpdatePadding (r: AstRoot) (o: ReferenceType) : RegionPadding =
    decide None (unionOf r [o.resolvedType; o.resolvedType.ActualType])

/// The region of a parameterised CHOICE (the CHOICE wrapper of DAstACNDeferred): it holds the CHOICE body; an alignment
/// of the CHOICE is inside the region. Each alternative sized by the region writes only its contents, which must pass
/// the cross-check of contentsResidues (the CHOICE's own sizes count the padding, Issue 141, so they cannot be used);
/// every other encoded alternative must pass it with its own sizes.
let choiceRegionPadding (r: AstRoot) (choiceType: Asn1Type) (ch: Choice) (regionPrm: string) (fixedOctets: BigInteger option) : RegionPadding =
    let passes (residues: Residues) =
        match residues with
        | Residues _      -> true
        | ResiduesUnknown -> false
    let alternativesPassCrossCheck =
        encodedAlternatives ch |> List.forall (fun c ->
            match c.Type.Kind with
            | ReferenceType rf ->
                match sizedByRegion (Some regionPrm) rf with
                | true  -> passes (unionOf r (siteContents r rf))
                | false -> passes (contentsResidues r c.Type)
            | Integer _ | Real _ | IA5String _ | NumericString _ | OctetString _ | NullType _ | BitString _ | Boolean _
            | Enumerated _ | ObjectIdentifier _ | SequenceOf _ | Sequence _ | Choice _ | TimeType _ ->
                passes (contentsResidues r c.Type))
    let residues =
        match choiceType.maxAlignment, alternativesPassCrossCheck with
        | None, true -> choiceBodyResiduesOf r ch (Some regionPrm)
        | Some _, _ | None, false -> ResiduesUnknown
    decide fixedOctets residues

/// The C fixed-size region of n octets (acn-v2): no padding test when the contents (the site's merged contents and the
/// callee's) take at most fewer than 8 n bits; filled = the E3 result.
let fixedRegionPadding (r: AstRoot) (o: ReferenceType) (nOctets: BigInteger) (filled: BigInteger option) : FixedRegionPadding =
    match filled with
    | Some n -> FixedFilled n
    | None ->
        let contents = siteContents r o
        let maxBits = contents |> List.map (fun t -> t.acnMaxSizeInBits) |> List.max
        let aligned = contents |> List.exists (fun t -> t.maxAlignment.IsSome)
        match aligned, maxBits < 8I * nOctets with
        | false, true -> FixedPadAlways maxBits
        | true, _ | false, false -> FixedPadChecked

/// The residues a PadDecided claims, as template arguments (empty = the test stays).
let padResidues (p: RegionPadding) : BigInteger list =
    match p with
    | PadDecided s -> s |> Set.toList |> List.map BigInteger
    | PadFixedOctets _ | PadChecked -> []

/// The E3 template argument (soFixedOctets).
let padFixedOctets (p: RegionPadding) : string option =
    match p with
    | PadFixedOctets n -> Some (n.ToString())
    | PadDecided _ | PadChecked -> None
