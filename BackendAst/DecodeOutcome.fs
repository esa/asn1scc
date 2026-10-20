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
        | Real _ | IA5String _ | NumericString _ | OctetString _ | NullType _ | BitString _ | TimeType _
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
    | BitString o -> leaf (not (o.minSize.uper = o.maxSize.uper && o.maxSize.uper < 65536I))  // bitString_FixSize: only on failure
    | NullType _ -> leaf false
    | Real _ | IA5String _ | NumericString _ | OctetString _ | TimeType _ | Enumerated _ | ObjectIdentifier _
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
