/// Language-specific runtime-name provider for ACN deferred patching.
///
/// The deferred-patching feature reserves space in the parent SEQUENCE with
/// `Acn_InitDet_*` calls and patches values later via `Acn_PatchDet_*` calls.
/// Those wrapper names are defined by the C runtime's DEFINE_ACN_DET_ENCODERS
/// macros in `asn1crt_encoding_acn.h`.  The mapping from an ACN encoding
/// class / inserted-field type to the (init, patch) pair is therefore
/// language-specific.
///
/// This module exposes:
///   - `IDeferredRuntimeNames` — the per-language provider contract.
///   - `CDeferredRuntime`     — the C-backend implementation (current
///                              `--acn-v2` is C-only).
///
/// When Ada / Scala deferred-patching support is added, an
/// `AdaDeferredRuntime` / `ScalaDeferredRuntime` instance is supposed to
/// implement the same interface; the closure-conversion / AST-rewriting
/// logic in `DAstACNDeferred.fs` is already language-agnostic and resolves
/// names through this interface.
module AcnDeferredRuntime

open System.Numerics

open CommonTypes
open Asn1AcnAst
open DAst
open Language


/// Triple-plus returned by the InitDet/PatchDet name lookup:
///   (initFuncName, patchFuncName, nBitsOpt, uperMinOffset)
/// - `nBitsOpt`     is `Some nBits` for generic ConstSize encodings that
///                  need an extra bit-width argument, `None` for fixed-size
///                  encodings (U8, U16, …).
/// - `uperMinOffset` is the UPER minimum-value offset (`0I` outside UPER).
///                  When non-zero, PatchDet must encode `(value - offset)`.
type DetFunctionNames = string * string * BigInteger option * BigInteger


/// Per-language provider for ACN deferred-patching runtime names and
/// fallback-value expressions.
type IDeferredRuntimeNames =
    /// Map an `AcnInsertedType` to its `(InitDet, PatchDet, nBitsOpt, uperMinOffset)`
    /// runtime-function names.  Returns `None` when the determinant type has
    /// no deferred-patching implementation in the target language runtime.
    abstract member GetDetFunctionsForAcnInsertedType :
        AcnInsertedType -> DetFunctionNames option

    /// Compute a valid default wire value for a deferred determinant that
    /// was never patched at runtime.  See `DAstACNDeferred.computeFallbackDetValue`
    /// for the detailed rationale (consumers absent / CHOICE branch not
    /// taken / etc.).  The returned string is a target-language source-code
    /// expression valid for the determinant's wire encoding.
    abstract member ComputeFallbackDetValue :
        LanguageMacros -> AcnInsertedType -> BigInteger -> string


// ---------------------------------------------------------------------------
//  C-backend implementation
// ---------------------------------------------------------------------------

/// Maps an ACN integer encoding class (from `AcnInteger.acnEncodingClass`)
/// to the corresponding `Acn_InitDet_*` / `Acn_PatchDet_*` wrapper names
/// defined by the `DEFINE_ACN_DET_ENCODERS` macro in
/// `asn1crt_encoding_acn.h`.  C-runtime specific.
let private mapIntEncodingClassToDetFunctions (enc: IntEncodingClass) : DetFunctionNames option =
    match enc with
    | PositiveInteger_ConstSize_8                    -> Some ("Acn_InitDet_U8",     "Acn_PatchDet_U8", None, 0I)
    | PositiveInteger_ConstSize_big_endian_16        -> Some ("Acn_InitDet_U16_BE", "Acn_PatchDet_U16_BE", None, 0I)
    | PositiveInteger_ConstSize_big_endian_32        -> Some ("Acn_InitDet_U32_BE", "Acn_PatchDet_U32_BE", None, 0I)
    | PositiveInteger_ConstSize_big_endian_64        -> Some ("Acn_InitDet_U64_BE", "Acn_PatchDet_U64_BE", None, 0I)
    | PositiveInteger_ConstSize_little_endian_16     -> Some ("Acn_InitDet_U16_LE", "Acn_PatchDet_U16_LE", None, 0I)
    | PositiveInteger_ConstSize_little_endian_32     -> Some ("Acn_InitDet_U32_LE", "Acn_PatchDet_U32_LE", None, 0I)
    | PositiveInteger_ConstSize_little_endian_64     -> Some ("Acn_InitDet_U64_LE", "Acn_PatchDet_U64_LE", None, 0I)
    | TwosComplement_ConstSize_8                     -> Some ("Acn_InitDet_I8",     "Acn_PatchDet_I8", None, 0I)
    | TwosComplement_ConstSize_big_endian_16         -> Some ("Acn_InitDet_I16_BE", "Acn_PatchDet_I16_BE", None, 0I)
    | TwosComplement_ConstSize_big_endian_32         -> Some ("Acn_InitDet_I32_BE", "Acn_PatchDet_I32_BE", None, 0I)
    | TwosComplement_ConstSize_big_endian_64         -> Some ("Acn_InitDet_I64_BE", "Acn_PatchDet_I64_BE", None, 0I)
    | PositiveInteger_ConstSize nBits                -> Some ("Acn_InitDet_ConstSize", "Acn_PatchDet_ConstSize", Some nBits, 0I)
    | TwosComplement_ConstSize nBits                 -> Some ("Acn_InitDet_TwosComplement_ConstSize", "Acn_PatchDet_TwosComplement_ConstSize", Some nBits, 0I)
    // Encoding classes without a deferred patching implementation
    | _ -> None


type CDeferredRuntime() =
    interface IDeferredRuntimeNames with
        member _.GetDetFunctionsForAcnInsertedType (acnType: AcnInsertedType) : DetFunctionNames option =
            match acnType with
            | AcnInteger ai ->
                match ai.acnEncodingClass with
                | Integer_uPER when ai.acnMinSizeInBits = ai.acnMaxSizeInBits ->
                    // Fixed-size UPER encoding → use ConstSize deferred patching.
                    // UPER encodes (value - min), so PatchDet must subtract the min offset.
                    let uperMinOffset =
                        match ai.uperRange with
                        | Concrete (minVal, _) -> minVal
                        | _ -> 0I
                    Some ("Acn_InitDet_ConstSize", "Acn_PatchDet_ConstSize", Some ai.acnMaxSizeInBits, uperMinOffset)
                | _ -> mapIntEncodingClassToDetFunctions ai.acnEncodingClass
            | AcnBoolean bln ->
                match bln.acnProperties.encodingPattern with
                | None -> Some ("Acn_InitDet_BOOL1", "Acn_PatchDet_BOOL1", None, 0I)
                | Some _ -> None  // Custom true-value/false-value patterns: not yet supported
            | AcnReferenceToEnumerated enm ->
                match enm.enumerated.acnEncodingClass with
                | Integer_uPER ->
                    // For enums with UPER encoding: indices are always [0, N-1],
                    // so ConstSize with ceil(log2(N)) bits is equivalent (offset = 0).
                    let nItems = enm.enumerated.items.Length
                    if nItems <= 1 then
                        Some ("Acn_InitDet_ConstSize", "Acn_PatchDet_ConstSize", Some 0I, 0I)
                    else
                        let nBits = bigint (int (System.Math.Ceiling(System.Math.Log(float nItems, 2.0))))
                        Some ("Acn_InitDet_ConstSize", "Acn_PatchDet_ConstSize", Some nBits, 0I)
                | _ ->
                    mapIntEncodingClassToDetFunctions enm.enumerated.acnEncodingClass
            | AcnReferenceToIA5String ref ->
                // IA5String determinant: fixed-size ASCII, 7 bits per character.
                // nChars is passed via the nBits slot of the _with_size calling convention.
                // maxSize.acn is already the character count (not bits).
                let nChars = ref.str.maxSize.acn
                Some ("Acn_InitDet_IA5String_FixSize", "Acn_PatchDet_IA5String_FixSize", Some nChars, 0I)
            | _ -> None

        member _.ComputeFallbackDetValue (lm: LanguageMacros) (acnType: AcnInsertedType) (uperMinOffset: BigInteger) : string =
            match acnType with
            | AcnInteger _ ->
                if uperMinOffset > 0I then uperMinOffset.ToString()
                else "0"
            | AcnBoolean _ -> "0"
            | AcnReferenceToEnumerated enm ->
                match enm.enumerated.items with
                | firstItem :: _ -> lm.lg.getNamedItemBackendName None firstItem
                | [] -> "0"
            | AcnReferenceToIA5String _ -> "\"\""
            | _ -> "0"


/// Singleton C-backend instance.  Use this from `DAstACNDeferred.fs` to
/// resolve InitDet/PatchDet names; future Ada/Scala backends are supposed
/// to provide their own instance and pick the correct one based on
/// `lm.lg`.
let cDeferredRuntime : IDeferredRuntimeNames = CDeferredRuntime() :> IDeferredRuntimeNames
