module AcnPrimitiveFactory

open System.Numerics

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language

open AcnHelpers


// Common factory for the four "primitive" ACN/ASN.1 type pairs whose outer
// scaffolding is identical: BOOLEAN, NULL, ENUMERATED, IA5String/NumericString.
// Each type comes in two flavours:
//   * createAcnXFunction       — used when the type is referenced standalone
//                                via an AcnRef (no Asn1Type, just a typeId).
//                                Allocates an error code from typeId.AcnAbsPath
//                                and returns (funcBody errCode, ns).
//   * createXFunction          — used when the type is part of a normal
//                                Asn1Type tree.  Wraps the inner funcBody in
//                                AcnFunctionWrapper.createAcnFunction.
//
// INTEGER is intentionally excluded — its encoding-class explosion is
// handled by the table-based approach in AcnPrimitives.

/// Inner per-call body shape (errCode already captured via closure).
type PrimitiveAcnInnerFn =
    (RelativePath * AcnParameter) list -> NestingScope -> CodegenScope -> AcnFuncBodyResult option

/// Outer shape of a primitive funcBody: takes the error code and returns the
/// per-call inner function.
type PrimitiveAcnFuncBody = ErrorCode -> PrimitiveAcnInnerFn


/// Build the standard "ERR_ACN<E|D>_<dotted-path>" error code from a typeId.
let primitiveErrCode (lm:LanguageMacros) (codec:CommonTypes.Codec) (typeId:ReferenceToType) (us:State) =
    let errCodeName = ToC ("ERR_ACN" + ((lm.lg.codecSuffix codec).ToUpper()) + "_" + ((typeId.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")))
    let errFieldPath = match typeId.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." (first :: rest)).Replace("#","elm")
    getNextValidErrorCode us errCodeName None errFieldPath


/// Wrapper for ACN-only primitives (AcnBoolean / AcnNullType / AcnReferenceToEnumerated
/// / AcnReferenceToIA5String).  Allocates the error code from the typeId and
/// applies the supplied builder.
let createAcnOnlyPrimitive (lm:LanguageMacros)
                           (codec:CommonTypes.Codec)
                           (typeId:ReferenceToType)
                           (us:State)
                           (mkBody: ErrorCode -> PrimitiveAcnInnerFn) =
    let errCode, ns = primitiveErrCode lm codec typeId us
    (mkBody errCode), ns


/// Wrapper for ASN.1-side primitives (Boolean / NullType / Enumerated).
/// Builds the standard SPARK annotations and delegates to
/// AcnFunctionWrapper.createAcnFunction with the supplied funcBody.
let createAsn1Primitive (r:Asn1AcnAst.AstRoot)
                        (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
                        (lm:LanguageMacros)
                        (codec:CommonTypes.Codec)
                        (t:Asn1AcnAst.Asn1Type)
                        (typeDefinition:TypeDefinitionOrReference)
                        (isValidFunc:IsValidFunction option)
                        (funcDefAnnots:string list)
                        (us:State)
                        (funcBody:PrimitiveAcnFuncBody) =
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc
        (fun us e acnArgs nestingScope p -> funcBody e acnArgs nestingScope p, us)
        (fun atc -> true) soSparkAnnotations funcDefAnnots us


/// State-threading variant of createAsn1Primitive for primitives whose inner
/// funcBody threads State (e.g. StringType, which calls uperFunc.funcBody_e
/// internally).
let createAsn1PrimitiveStateful (r:Asn1AcnAst.AstRoot)
                                (deps:Asn1AcnAst.AcnInsertedFieldDependencies)
                                (lm:LanguageMacros)
                                (codec:CommonTypes.Codec)
                                (t:Asn1AcnAst.Asn1Type)
                                (typeDefinition:TypeDefinitionOrReference)
                                (isValidFunc:IsValidFunction option)
                                (funcDefAnnots:string list)
                                (us:State)
                                (funcBody:AcnAlignment.FuncBody) =
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)
    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition isValidFunc
        funcBody (fun atc -> true) soSparkAnnotations funcDefAnnots us


/// Build the standard 1-row IcdArgAux for a primitive type.  The default
/// rowsFunc emits a single FieldRow with the supplied type/constraint/units.
/// acnAlignment is the type's own align-to-next property: its padding bits
/// are folded into maxBits by AcnEncodingClasses but presented as a separate
/// PaddingRow (AcnAlignment.fs), so they are removed from the field row here.
let buildPrimitiveIcdAux (acnAlignment:AcnGenericTypes.AcnAlignment option)
                         (icdType:string)
                         (icdName:string)
                         (sConstraint:string option)
                         (minBits:BigInteger)
                         (maxBits:BigInteger)
                         (units:string option) =
    let icdFnc fieldName sPresent comments =
        [{IcdRow.fieldName = fieldName; comments = comments; sPresent = sPresent;
          sType = IcdPlainType icdType; sConstraint = sConstraint;
          minLengthInBits = minBits; maxLengthInBits = icdMaxSizeWithoutAlignment acnAlignment maxBits; sUnits = units;
          rowType = IcdRowType.FieldRow; idxOffset = None}], []
    {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = icdName; rowsFunc = icdFnc;
     commentsForTas = []; scope = "type"; name = None}


// ---------------------------------------------------------------------------
// Sizeable leaf types — IA5String / OCTET STRING / BIT STRING (roadmap B1).
// Mirrors the SEQUENCE OF presentation (AcnSequenceOf.icdFnc): an explicit
// Length-determinant row when the length is embedded in the stream, an
// explicit Terminator row when a termination pattern marks the end, and a
// per-encoding-class comment on the content row otherwise (external field /
// fixed size / deduced).  The content row carries the remaining bits, so the
// rows always sum up to the type's acnMin/MaxSizeInBits totals.
// ---------------------------------------------------------------------------

/// Row-breakdown parts of a sizeable leaf type, interpreted from its ACN
/// encoding class by buildStringIcdAux / buildOctetOrBitStringIcdAux below.
type SizeableIcdParts = {
    /// Length-determinant row prefix: (size in bits, comment).
    lengthRow       : (BigInteger * string) option
    /// Termination row suffix: (size in bits, comment).
    terminatorRow   : (BigInteger * string) option
    /// Per-encoding-class explanation appended to the content row comments.
    contentComments : string list
}

let buildSizeableLeafIcdAux (acnAlignment:AcnGenericTypes.AcnAlignment option)
                            (icdType:string)
                            (icdName:string)
                            (sConstraint:string option)
                            (minBits:BigInteger)
                            (maxBits:BigInteger)
                            (units:string option)
                            (parts:SizeableIcdParts) : IcdArgAux =
    let nDeterminantBits =
        (match parts.lengthRow     with Some (n, _) -> n | None -> 0I) +
        (match parts.terminatorRow with Some (n, _) -> n | None -> 0I)
    let icdFnc fieldName sPresent comments =
        let lengthRows =
            match parts.lengthRow with
            | None -> []
            | Some (nBits, sComment) ->
                [{IcdRow.fieldName = "Length"; comments = [sComment]; sPresent = sPresent;
                  sType = IcdPlainType "INTEGER"; sConstraint = sConstraint;
                  minLengthInBits = nBits; maxLengthInBits = nBits; sUnits = None;
                  rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]
        let contentRow =
            {IcdRow.fieldName = fieldName; comments = comments@parts.contentComments; sPresent = sPresent;
             sType = IcdPlainType icdType; sConstraint = sConstraint;
             minLengthInBits = minBits - nDeterminantBits; maxLengthInBits = (icdMaxSizeWithoutAlignment acnAlignment maxBits) - nDeterminantBits;
             sUnits = units; rowType = IcdRowType.FieldRow; idxOffset = None}
        let terminatorRows =
            match parts.terminatorRow with
            | None -> []
            | Some (nBits, sComment) ->
                [{IcdRow.fieldName = "Terminator"; comments = [sComment]; sPresent = sPresent;
                  sType = IcdPlainType "bit pattern"; sConstraint = None;
                  minLengthInBits = nBits; maxLengthInBits = nBits; sUnits = None;
                  rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]
        lengthRows@[contentRow]@terminatorRows |> List.mapi (fun i rw -> {rw with idxOffset = Some (i+1)}), []
    {IcdArgAux.canBeEmbedded = true; baseAsn1Kind = icdName; rowsFunc = icdFnc;
     commentsForTas = []; scope = "type"; name = None}


let private deducedLengthComment =
    "Length is deduced from the size of the enclosing container/PDU (no length determinant is encoded)."

/// IA5String / NumericString row breakdown per StringAcnEncodingClass (B1).
let buildStringIcdAux (acnAlignment:AcnGenericTypes.AcnAlignment option)
                      (o:Asn1AcnAst.StringType)
                      (icdType:string)
                      (icdName:string)
                      (sConstraint:string option)
                      (units:string option) : IcdArgAux =
    let charBits = o.acnEncodingClass.charSizeInBits
    let sCharComment =
        match o.acnEncodingClass with
        | Acn_Enc_String_uPER                                 _
        | Acn_Enc_String_CharIndex_External_Field_Determinant _ -> $"Each character occupies {charBits} bits."
        | Acn_Enc_String_uPER_Ascii                           _
        | Acn_Enc_String_Ascii_Null_Terminated                _
        | Acn_Enc_String_Ascii_External_Field_Determinant     _
        | Acn_Enc_String_Ascii_Deduced                        _ -> $"Each character is encoded as {charBits}-bit ASCII."
    let sFixedComment = $"Length is fixed to {o.maxSize.acn} characters (no length determinant is needed)."
    let parts =
        match o.acnEncodingClass with
        | Acn_Enc_String_uPER _ ->
            match o.minSize.uper = o.maxSize.uper with
            | true  -> {SizeableIcdParts.lengthRow = None; terminatorRow = None; contentComments = [sFixedComment; sCharComment]}
            | false ->
                match o.maxSize.uper < 65536I with
                | true  ->
                    let nLenBits = GetNumberOfBitsForNonNegativeInteger (o.maxSize.uper - o.minSize.uper)
                    {SizeableIcdParts.lengthRow = Some (nLenBits, "The number of characters"); terminatorRow = None; contentComments = [sCharComment]}
                | false ->
                    // uPER fragmentation: the length is interleaved with the data
                    // in 16k blocks, so there is no single Length-determinant row.
                    {SizeableIcdParts.lengthRow = None; terminatorRow = None;
                     contentComments = ["Length is encoded with the uPER fragmentation procedures (the maximum size is 64k characters or more)."; sCharComment]}
        | Acn_Enc_String_uPER_Ascii _ ->
            match o.minSize.uper = o.maxSize.uper with
            | true  -> {SizeableIcdParts.lengthRow = None; terminatorRow = None; contentComments = [sFixedComment; sCharComment]}
            | false ->
                let nLenBits = GetNumberOfBitsForNonNegativeInteger (o.maxSize.acn - o.minSize.acn)
                {SizeableIcdParts.lengthRow = Some (nLenBits, "The number of characters"); terminatorRow = None; contentComments = [sCharComment]}
        | Acn_Enc_String_Ascii_Null_Terminated (_, nullChars) ->
            let sPattern = nullChars |> Seq.map (sprintf "%02X") |> Seq.StrJoin ""
            {SizeableIcdParts.lengthRow = None;
             terminatorRow = Some ((8 * nullChars.Length).AsBigInt, $"Null terminator '%s{sPattern}'H marking the end of the string");
             contentComments = [sCharComment]}
        | Acn_Enc_String_Ascii_External_Field_Determinant     (_, relPath)
        | Acn_Enc_String_CharIndex_External_Field_Determinant (_, relPath) ->
            {SizeableIcdParts.lengthRow = None; terminatorRow = None;
             contentComments = [$"Length is determined by the external field: %s{relPath.AsString}"; sCharComment]}
        | Acn_Enc_String_Ascii_Deduced _ ->
            {SizeableIcdParts.lengthRow = None; terminatorRow = None; contentComments = [deducedLengthComment; sCharComment]}
    buildSizeableLeafIcdAux acnAlignment icdType icdName sConstraint o.acnMinSizeInBits o.acnMaxSizeInBits units parts

/// OCTET STRING / BIT STRING row breakdown per SizeableAcnEncodingClass (B1).
/// sSizeUnit is "bytes" (octet string) or "bits" (bit string); nMaxItems is
/// the ASN.1 size upper bound in that unit.
let buildOctetOrBitStringIcdAux (acnAlignment:AcnGenericTypes.AcnAlignment option)
                                (encClass:Asn1AcnAst.SizeableAcnEncodingClass)
                                (sSizeUnit:string)
                                (nMaxItems:BigInteger)
                                (icdType:string)
                                (icdName:string)
                                (sConstraint:string option)
                                (minBits:BigInteger)
                                (maxBits:BigInteger)
                                (units:string option) : IcdArgAux =
    let sFixedComment = $"Length is fixed to {nMaxItems} {sSizeUnit} (no length determinant is needed)."
    let parts =
        match encClass with
        | SZ_EC_FIXED_SIZE ->
            {SizeableIcdParts.lengthRow = None; terminatorRow = None; contentComments = [sFixedComment]}
        | SZ_EC_LENGTH_EMBEDDED nLenBits when nLenBits > 0I ->
            {SizeableIcdParts.lengthRow = Some (nLenBits, $"The number of {sSizeUnit}"); terminatorRow = None; contentComments = []}
        | SZ_EC_LENGTH_EMBEDDED _ ->
            // Degenerate embedded length of 0 bits (min size = max size).
            {SizeableIcdParts.lengthRow = None; terminatorRow = None; contentComments = [sFixedComment]}
        | SZ_EC_ExternalField relPath ->
            {SizeableIcdParts.lengthRow = None; terminatorRow = None;
             contentComments = [$"Length is determined by the external field: %s{relPath.AsString}"]}
        | SZ_EC_TerminationPattern bitPattern ->
            {SizeableIcdParts.lengthRow = None;
             terminatorRow = Some (bitPattern.Value.Length.AsBigInt, $"Termination pattern '%s{bitPattern.Value}'B marking the end of the field");
             contentComments = []}
        | SZ_EC_Deduced ->
            {SizeableIcdParts.lengthRow = None; terminatorRow = None; contentComments = [deducedLengthComment]}
    buildSizeableLeafIcdAux acnAlignment icdType icdName sConstraint minBits maxBits units parts
