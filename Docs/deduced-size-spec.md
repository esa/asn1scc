# ACN `size deduced` — Feature Specification

**Deduced field size for PUS / CFDP style protocols**

| | |
|---|---|
| Status | Draft specification — for review, not yet implemented |
| Supersedes | `Docs/deduced-size.md` (May 2023 proposal) |
| Target languages | C, Ada (SPARK-compatible). Scala: empty template stubs + explicit "not supported" compile-time error |
| Codecs affected | ACN only. uPER, XER, BER are unaffected |
| ACN modes | Legacy (default) and `--acn-v2` (deferred patching) |

---

## 1. Motivation and scope

ACN currently offers four ways to determine the number of elements of a sizeable type
(SEQUENCE OF, OCTET STRING, BIT STRING, IA5String):

1. **Fixed size** — `SIZE(N)` constraint, no size information on the wire.
2. **Embedded length determinant** — the uPER-style default for variable-size types.
3. **External length determinant** — `[size fieldref]`, an ACN-inserted INTEGER field.
4. **Termination pattern** — `[size null-terminated, termination-pattern '...'H]`.

Many space protocols (PUS packet data fields, CFDP TLV lists, filestore request lists)
use a fifth convention: **the element count is not transmitted at all; the receiver
derives it from the number of bytes of the enclosing structure**, which is known from a
byte-count header field or from the size of the received datagram itself. Both
real-case grammars already in the regression suite exhibit exactly this gap:
`v4Tests/test-cases/acn/14-RealCases/02-PUS.asn1` (`packet-length` is not — and cannot
be — wired as a determinant) and `CFDP-PROTOCOL.asn1` (`PDUDataFieldLength`).

This document specifies a new ACN size-property value, **`size deduced`**, that closes
this gap, and defines it rigorously enough that every statement is implementable in the
current code base (post ACN-v2, post CONTAINING completion, post `BackendAst/Acn/*`
refactoring). Section 12 lists all deliberate deviations from the 2023 proposal.

---

## 2. Definitions

**Deduced type** — a SEQUENCE OF, OCTET STRING, IA5String or NumericString whose ACN
specification carries `size deduced`.

**Decoding region (region)** — the contiguous span of the bitstream from which a
deduced type derives its element count. A region is always established by one of the
**region boundaries** below; the deduced type consumes the region's bits from its
current position up to the region's end, minus the *trailing constant* (see below).

**Region boundary** — one of:

* **B1 — CONTAINING container**: an `OCTET STRING (CONTAINING X)` or
  `BIT STRING (CONTAINING X)` wrapper. The region is the container's content, whose
  size in bytes (octet) or bits (bit) is always known before the contained decode runs
  (fixed size, embedded length, or `[size fieldref]`).
* **B2 — top-level PDU**: the type assignment whose generated decode function the
  application calls directly. The region is the entire bitstream attached by the
  caller (see §9 API contract).

**Trailing constant (`trailingBits`)** — the total ACN size in bits of all components
that follow the deduced component on the path from the deduced type up to its region
boundary. The static-semantics rules (§4) force every such component to have a
compile-time-constant size, so `trailingBits` is a per-usage compile-time constant.
When the deduced type is the last thing in its region, `trailingBits = 0`.

**Budget end (`budgetEnd`)** — the bit position where the deduced type must stop:
`budgetEnd = regionEndBits − trailingBits`. §6 defines how each backend materializes
this expression. `availableBits = budgetEnd − currentBitPosition`.

**Slack** — `budgetEnd − position` after the deduced decode loop finishes. For
byte-granular regions (octet container, top-level PDU) a slack of 0–7 bits is legal
padding; slack ≥ 8 is a decode error. For bit-granular regions (bit container) slack
must be exactly 0.

**Implicitly deduced type** — a SEQUENCE that contains (directly or through nested
SEQUENCEs, without an intervening CONTAINING boundary) a deduced component. Its own
decoding also consumes "the rest of the region minus its trailing constant", so every
rule that applies to a deduced type's placement applies to it transitively.

---

## 3. Syntax

### 3.1 Grammar

`size deduced` becomes the fourth alternative of the existing `size` property:

```
MySequenceOf [size deduced]
```

Changes in `Antlr/acn.g`:

* new lexer token `DEDUCED : 'deduced';` (next to `NULL_TERMINATED`, ~line 426);
* new alternative in `sizeProp` (lines 191–197): `| SIZE^ DEDUCED`;
* `deduced` added to the `asn1LID` keyword-fallback list (lines 369–390) so existing
  grammars with fields named `deduced` keep compiling.

Ordering note: the alternative must be listed before `SIZE^ longFld` so that
`size deduced` is not parsed as a field reference named `deduced` (ANTLR resolves the
token vs. LID conflict through the lexer, but the fallback list makes the intent
explicit).

Rationale for the syntax (vs. the 2023 proposal's standalone `[deduced-size]`
property): the feature *is* a size determination method, exactly parallel to
`size null-terminated`. Reusing the `size` property slot gives mutual exclusion with
the other size forms for free (one `SIZE` property per encoding spec), keeps the ACN
quick-reference table one-dimensional, and reuses the whole existing
`GenSizeProperty → AcnSizeableSizeProperty → SizeableAcnEncodingClass` pipeline.

### 3.2 AST representation

Following the existing pipeline, one new case per layer:

| Layer | Type | New case |
|---|---|---|
| `CommonTypes/AcnGenericTypes.fs:471` | `GenSizeProperty` | `GP_Deduced` |
| `CommonTypes/AcnGenericTypes.fs:361` | `AcnSizeableSizeProperty` | `SzDeduced` |
| `CommonTypes/AcnGenericTypes.fs:349` | `AcnStringSizeProperty` | `StrDeduced` |
| `FrontEndAst/Asn1AcnAst.fs:364` | `SizeableAcnEncodingClass` | `SZ_EC_Deduced` |
| `FrontEndAst/Asn1AcnAst.fs:349` | `StringAcnEncodingClass` | `Acn_Enc_String_Ascii_Deduced of BigInteger` (char size in bits) |

No `RelativePath` payload, no ACN dependency, no ACN parameter — like the
termination-pattern cases, `size deduced` creates **no** `AcnDependency` in
`CheckLongReferences.fs` (the new match arms at lines 309–337 and 396–407 map to
`None`).

F# exhaustive matching then enumerates every consumer that needs a new branch
(`AcnSequenceOf.fs`, `AcnOctetBitStrings.fs`, `AcnStrings.fs`, `AcnReference.fs`,
`DAstACNDeferred.fs`, `GenerateAcnIcd.fs`, `ExportToXml.fs`, `LspAst.fs`, …) — this is
the intended discovery mechanism for the implementation.

---

## 4. Static semantics

### 4.1 Applicability rules (checked at ACN merge time, `AcnCreateFromAntlr.fs`)

| # | Rule | Error condition |
|---|---|---|
| A1 | `size deduced` is applicable to SEQUENCE OF, OCTET STRING, IA5String and NumericString only. | On BIT STRING, INTEGER, ENUMERATED, REAL: *"'size deduced' is not applicable to this type"*. BIT STRING is excluded because its element (one bit) is smaller than the byte-granular padding resolution (see A4). |
| A2 | The ASN.1 SIZE constraint must be a bounded, non-fixed range (`SIZE(a..b)`, `a < b`, `b` finite). The existing "declared type may have infinite size" check (`uPER.fs:264`) remains in force; `b` is still needed for array allocation. | Fixed `SIZE(N)`: *"'size deduced' is pointless for a fixed-size type; remove the size property"*. |
| A3 | On IA5String/NumericString, `size deduced` requires `encoding ASCII` (8-bit characters). The default character-index encoding of IA5String produces 7-bit (or smaller) characters and is rejected — same rule and same wording style as the existing `size null-terminated` + ASCII restriction (`AcnEncodingClasses.fs:214`). | *"'size deduced' on string types requires 'encoding ASCII'"*. |
| A4 | On SEQUENCE OF, the element's minimum ACN encoding size must satisfy `child.acnMinSizeInBits ≥ 8`. This guarantees that after the last element, at most 7 bits of byte padding can remain, so padding can never be mistaken for one more element. | *"'size deduced' requires the SEQUENCE OF element to occupy at least 8 bits (its minimum ACN size is N bits)"*. |
| A5 | `termination-pattern` must not be combined with `size deduced`. | *"'termination-pattern' requires 'size null-terminated'"*. |
| A6 | Target language Scala is not supported. | Compile-time error when `-Scala` is requested and the grammar contains `size deduced`. C and Ada are fully supported. |
| A7 | Streaming mode (`ASN1SCC_STREAMING` / fetch-callback bitstreams) is not supported: the decoder relies on the stream view having a definite end. | Documented limitation; additionally the generated C guards the deduced macros with `#ifndef ASN1SCC_STREAMING` + `#error`. |

Note on `acnMinSizeInBits/acnMaxSizeInBits`: in `AcnEncodingClasses.GetOctetBitSeqofEncodingClass`
(`AcnEncodingClasses.fs:232-248`) the new arm computes
`SZ_EC_Deduced, asn1Min*internalMinSize, asn1Max*internalMaxSize` — no determinant or
terminator overhead, i.e. the same size arithmetic as the external-field arm.

### 4.2 Placement (region) rules — checked per usage path

These rules are checked by a **new front-end validation pass** (natural home: a new
function in `FrontEndAst/CheckLongReferences.fs` or a sibling module invoked from
`FrontEntMain.fs` right after `CheckLongReferences.checkAst`). The pass walks every
usage path from each deduced type upward until it reaches a region boundary (B1 or
B2), and classifies every ancestor.

| # | Rule | Error condition |
|---|---|---|
| R1 | Every ancestor between the deduced type and its region boundary must be a SEQUENCE (possibly through ReferenceTypes without CONTAINING). CHOICE and SEQUENCE OF ancestors are not allowed in v1 — a deduced type may sit inside a CHOICE alternative or a SEQUENCE OF element **only** behind a CONTAINING boundary (the 2023 proposal's "Case a"). | *"a 'size deduced' type may only be nested inside SEQUENCEs up to the enclosing CONTAINING container or top-level PDU; here it is inside a CHOICE / SEQUENCE OF"*. |
| R2 | At every SEQUENCE level on the path, every component **after** the on-path child must: (a) be non-OPTIONAL (no `OPTIONAL`/`DEFAULT`, no `present-when`), (b) have a compile-time-constant ACN size (`acnMinSizeInBits = acnMaxSizeInBits`), (c) carry no `align-to-next` property anywhere inside it, and (d) not be or contain a deduced type. ACN-inserted fields count as components and must satisfy the same conditions (fixed-size inserted INTEGERs, NULL patterns etc. are fine). | *"component 'X' follows the 'size deduced' component 'Y' and must be a non-optional, fixed-size component (its ACN size is a..b bits / it is optional / it uses align-to-next)"*. |
| R3 | The deduced component itself (and every implicitly-deduced ancestor, at its own level) must be non-OPTIONAL. | *"a 'size deduced' component may not be OPTIONAL"*. |
| R4 | At most one deduced component can be "live" in a region. This is *implied* by R2 (a second deduced sibling after the first would violate R2's fixed-size requirement, and an implicitly-deduced component is variable-size), so no separate check is needed — but the error message for R2 should mention it when the offending trailing component is itself deduced. | — |
| R5 | `align-to-next` on the deduced type itself **is** allowed (the alignment padding is consumed before the element loop starts and does not affect `budgetEnd`). `align-to-next` after it is forbidden by R2(c). | — |
| R6 | A deduced type (or implicitly deduced SEQUENCE) used **directly as an element of a SEQUENCE OF, as a CHOICE alternative, or as an OPTIONAL component** anywhere in the schema is an error on that usage path (this is R1/R3 seen from the other end; every usage of a TAS is validated independently). | as R1/R3 |

Notes:

* Components **before** the deduced component are unrestricted (they may be variable
  size, optional, etc.) — they are decoded first and simply advance the stream.
* The pass also computes, per usage, the **trailing constant** `trailingBits` =
  Σ `acnMaxSizeInBits` (= `acnMinSizeInBits`, by R2b) of all following components,
  accumulated across all SEQUENCE levels up to the boundary. The backend recomputes
  the same value at code-generation time from `NestingScope`; the front-end pass
  exists to produce clean compile errors instead of malformed code.
* Under `--acn-v2`, this pass must run **after** `AcnClosureConversion` so that it
  sees the same specialized usage paths the backend will generate code for.

### 4.3 Examples of accepted / rejected grammars

```asn1
MYINT        ::= INTEGER (0..16383)                    -- ACN: 16 bits (see below)
TLVList      ::= SEQUENCE (SIZE(0..100)) OF MYINT
Payload      ::= SEQUENCE { hdr INTEGER (0..255), items TLVList, crc INTEGER (0..4294967295) }
PDU-A        ::= SEQUENCE { len INTEGER (0..65535), body OCTET STRING (CONTAINING TLVList) }
```

```acn
MYINT   [encoding pos-int, size 16]
TLVList [size deduced]                                 -- OK: element min size 16 ≥ 8

PDU-A [] {                                             -- OK (Case a / B1)
    len  [encoding pos-int, size 16],
    body [size len]
}

Payload [] {                                           -- OK (Case b / B2, trailingBits = 32)
    hdr   [encoding pos-int, size 8],
    items [],
    crc   [encoding pos-int, size 32]
}
```

Rejected:

```acn
Bad1 ::= SEQUENCE (SIZE(0..8)) OF BOOLEAN              -- A4: element is 1 bit
Bad1 [size deduced]

Bad2 [] { items [], trailer OCTET STRING (SIZE(0..4)) } -- R2b: variable-size trailer
Bad3 [] { items [], note IA5String OPTIONAL }           -- R2a: OPTIONAL after deduced
Bad4 ::= SEQUENCE (SIZE(0..10)) OF Payload              -- R1/R6: implicitly-deduced element
```

---

## 5. Dynamic semantics

### 5.1 Wire format

A deduced type contributes **only its elements** to the encoding: no length
determinant, no terminator. Elements are encoded back to back, exactly as the
external-field encoding class encodes them (the encode side of `size deduced` is
byte-for-byte identical to `[size someField]`; only the determinant field itself is
absent from the wire).

**Padding.** When a region is byte-granular (octet container or top-level PDU) and the
total content size is not a whole number of bytes, the region is padded to the next
byte boundary. The **encoder MUST advance the stream cursor to the byte boundary at
the region end, writing zero bits** (see §8 for the pre-existing v2 alignment gap this
fixes). The **decoder MUST accept any padding bit values** (it never reads them) —
robustness against foreign encoders — but asn1scc's own encoders emit zeros for
reproducibility and SPARK-friendliness. Bit-granular regions (bit container) have no
padding.

### 5.2 Decoding — general algorithm

All decode variants share the same skeleton, parameterized by the compile-time values
`elemMinBits` (= `child.acnMinSizeInBits`), `maxSize` (ASN.1 upper bound) and the
`budgetEnd` expression (§6):

```
pos       := currentBitPosition()
nCount    := 0
while (budgetEnd − pos ≥ elemMinBits) and (nCount < maxSize):
    decode one element                       -- may fail → propagate error
    pos := currentBitPosition()
    if pos > budgetEnd: ERROR                 -- element overran the region  (E1)
    nCount := nCount + 1
slack := budgetEnd − pos
if slack ≥ 8 (byte-granular region)           -- unaccounted data            (E2)
   or slack ≠ 0 (bit-granular region):
    ERROR
-- decoding continues at pos (NOT at budgetEnd): trailing fixed components
-- follow immediately; the 0–7 slack bits, when present, are the region's own
-- byte padding located at the region end and are skipped by the region owner
-- (the CONTAINING wrapper repositions to the region end; at top level the
-- remaining bits are simply never read).
```

Error conditions:

* **E1 — element overrun**: a variable-size element consumed bits beyond `budgetEnd`.
  Possible only with variable-size elements; the check is one comparison per
  iteration.
* **E2 — inconsistent region**: after the loop, at least one full byte (or, in a bit
  region, at least one bit) remains that is neither padding nor a decodable element.
  This also covers the "stream claims more than `maxSize` elements" case: the loop
  stops at `maxSize` and the residue triggers E2.

Both map to one new per-type error constant (existing errcode machinery), e.g.
`ERR_ACN_DECODE_DEDUCED_SIZE_INCONSISTENT_<TYPE>`.

**No look-ahead functions.** Unlike the 2023 proposal, no generated `X_LA()` predicate
is needed: rule A4 makes `remaining ≥ elemMinBits` a sound loop guard, and E1 catches
mid-element overruns after the fact. This removes an entire generated-function family
(which for CHOICE/variable elements would have amounted to a full speculative decode)
at no loss of decoding power.

### 5.3 Decoding — closed-form specializations

Two common cases need no incremental loop at all, because the element count is
computable up front:

1. **OCTET STRING / ASCII string** (`elem = 8` bits exactly):
   `nCount = availableBits / 8` (the remainder 0–7 is the padding, automatically
   legal). Check `nCount ≤ maxSize` (else E2), then reuse the **existing**
   external-field decode body with the computed `nCount` — i.e.
   `Acn_Dec_String_Ascii_External_Field_Determinant` / the octet-string
   external-field macro with a locally computed count instead of a field reference.

2. **SEQUENCE OF with fixed-size elements** (`elemMinBits = elemMaxBits = E`):
   `nCount = availableBits / E`; check `nCount ≤ maxSize` and
   `availableBits − nCount·E < 8` (bit regions: `= 0`), then run the standard
   count-bounded element loop (`loopFixedItem`).

Only **SEQUENCE OF with variable-size elements** requires the incremental loop of
§5.2. This split keeps the generated code and the SPARK proofs simple: the closed-form
cases are provable trivially, and the incremental loop mirrors the invariants already
proven for `oct_sqf_null_terminated_decode` (`StgAda/acn_a.stg:902-928`:
`bs.Current_Bit_Pos ≤ 'Loop_Entry + elemMax·(i−1)` plus the new
`bs.Current_Bit_Pos ≤ budgetEnd`).

### 5.4 Encoding

Encode `nCount` elements back to back (the existing external-field encode loop shape,
without any determinant emission). Constraint checking of `nCount` against the SIZE
constraint is unchanged. Region padding is the responsibility of the region owner
(CONTAINING wrapper / application), per §5.1.

---

## 6. The `budgetEnd` expression — code generation model

The single unifying idea replacing the 2023 proposal's extra `totalEncodedBytes`
function parameter: **the bitstream view itself carries the region, and `budgetEnd` is
always `⟨end of stream view⟩ − ⟨compile-time trailing constant⟩`.**

```
budgetEnd(bits) =  C:   pBitStrm->count * 8 − pBitStrm->unusedTailBits − trailingBits
                   Ada: bs.End_Bit_Pos                                 − trailingBits
```

The end of the view is exact to the bit: a region need not end on a byte boundary (an
OCTET STRING CONTAINING region that does not start on one, §6.1). C: `unusedTailBits`
(0..7) = the bits of the last byte of the view (`buf[count - 1]`) that lie outside it, 0
after `BitStream_Init` / `BitStream_AttachBuffer`. Ada: `End_Bit_Pos` = the first bit
after the view, `Size_In_Bytes * 8` after `BitStream_init` (`Size_In_Bytes` is a
discriminant and cannot be reduced). Only the deduced decoders read these fields.

No decode-function signature changes anywhere. What makes this sound in each context:

### 6.1 Context B1 — CONTAINING container

* **`--acn-v2` (deferred), octet container**: the region is whole octets from its first
  bit, decoded in place. The C decode wrapper limits the stream view to it (`count` =
  the bytes it touches, `unusedTailBits` = the bits of its last byte after its end, the
  `octet_string_containing_deferred_*_decode` macros of `StgC/acn_c.stg`); the Ada
  wrapper sets `bs.End_Bit_Pos` to the region end. Both restore the view and reposition
  to the region end afterwards. Inside the contained decode, `budgetEnd` is the region
  end. Before 4.9.76.0 the view ended at the byte after the region: off a byte boundary
  the deduced decoder saw up to 7 bits more and could fail or decode one element too many
  (the Ada decoders copied the region to the stack, and rejected or mis-measured an
  unaligned one).
* **Legacy, octet container**: decode copies the region into a temporary bitstream.
  Today the temp stream is initialized with the *maximum* capacity
  (`BitStream_Init(&bitStrm, arr, sizeof(arr))`, `StgC/acn_c.stg:1516-1523`); when the
  contained type is deduced (directly or implicitly), the temp stream must instead be
  initialized with the **actual** byte count (`nCount` / `<sExtField>`), so that the
  stream view equals the region. One new macro variant (or a boolean macro parameter)
  per container macro family. Ada legacy identically: `tmpBs : Bitstream :=
  BitStream_init(nCount)` — a dynamic discriminant in a declare block instead of the
  static `<sReqBytesForAcnEncoding>`.
* **Bit container** (spec'd, may be phased later): v2 already limits the view at bit
  granularity; `slack = 0` semantics apply. Legacy bit-container temp streams are
  byte-padded copies — the exact bit length is known (`nCount` bits), so `budgetEnd =
  regionStartBits + nCount` is emitted as a local instead of `count*8`; this is the
  only context where `budgetEnd` is not literally derived from the stream view.

### 6.2 Context B2 — top-level PDU and enclosing SEQUENCEs

* The deduced (or implicitly deduced) PDU's generated decode function uses
  `budgetEnd = count*8 − trailingBits` where `trailingBits` is the constant computed
  from the components after the deduced child **within this function's own body**.
  The API contract (§9) guarantees `count*8` is the region end.
* **Nesting**: when SEQUENCEs nest without a CONTAINING boundary, the constants
  accumulate. The natural implementation is a new field on `NestingScope`
  (`FrontEndAst/DAst.fs:439-460`, which already threads the static `acnOuterMaxSize`/
  `acnOffset` values): e.g. `deducedTrailingBits : BigInteger option`. `None` = "not
  inside a deduced region" (the sizeable builders then reject `SZ_EC_Deduced` with a
  bug-error — the front-end pass §4.2 should have caught it); `Some n` = emit
  `budgetEnd` with constant `n`. Each SEQUENCE builder adds its own trailing sum
  before descending into the on-path child.

### 6.3 Per-usage specialization

Because `trailingBits` is a *usage-path* property while generated functions are
*per-type*, the following holds:

* A deduced/implicitly-deduced TAS's **standalone** decode function is always emitted
  with `trailingBits = 0` — correct for direct application calls (B2 with the type as
  the whole PDU) and for CONTAINING-wrapped calls (B1, view already exact).
* When such a TAS is referenced **mid-SEQUENCE with a non-zero trailing constant**,
  the reference must be emitted **inline** (legacy: `createReferenceFunction_inline`
  already inlines base bodies via the `funcBody` closure; the deduced case forces this
  path even when a standalone function exists) or as a **specialized function**
  (`--acn-v2`: closure conversion already creates per-usage specialized functions —
  the constant is simply baked into the specialized body). No runtime value is ever
  passed.

This mirrors precisely how ACN v2 resolved the analogous "cross-scope determinant"
problem — compile-time specialization instead of runtime threading — and is the reason
the feature composes with both modes.

### 6.4 Consequences that fall out for free

* **No change to `AcnFunction`/`AcnFuncBody` signatures**, no new macro formal on
  `EmitTypeAssignment_primitive*` — the `arrsAcnPrms` slot (already triple-purposed by
  user ACN params and v2 det-refs) is untouched.
* **`-atc` keeps working**: deduced TASes still have parameterless standalone
  functions, so `hasAcnEncodeFunction` (`DAstUtilFunctions.fs:918-929`) stays true and
  the `Codec_Decode` call arity (`StgC/test_cases_c.stg:78-83`) is unchanged. The only
  harness change: attach the decode stream with the **actual encoded length** instead
  of `_REQUIRED_BYTES_FOR_ACN_ENCODING` (§10).
* **Runtime additions: one field per bitstream (4.9.76.0).** C: position =
  `currentByte*8 + currentBit` (existing idiom), view end = `count*8 − unusedTailBits`
  (the last member of `BitStream`, so a zero-initialized struct keeps the old meaning).
  Ada: `bs.Current_Bit_Pos`, view end = `bs.End_Bit_Pos` (a component of `Bitstream`: an
  application that writes a full `Bitstream'(...)` aggregate must give it, e.g.
  `End_Bit_Pos => <size> * 8`; `BitStream_init` sets it).

---

## 7. Generated-code sketches

### 7.1 C — SEQUENCE OF, variable-size elements (canonical incremental loop)

New STG macro pair `sqf_deduced` / (encode reuses the external-field element loop),
modeled on `oct_sqf_null_terminated` (`StgC/acn_c.stg:705-728`):

```c
/* decode: TLVList [size deduced], inside a region with trailingBits = <nTrailingBits> */
{
    long deducedBudgetEnd = pBitStrm->count * 8L - <nTrailingBits>;
    long deducedPos = pBitStrm->currentByte * 8L + pBitStrm->currentBit;
    nCount = 0;
    ret = TRUE;
    while (ret && (deducedPos + <nIntItemMinSize> <= deducedBudgetEnd) && (nCount < <nSizeMax>)) {
        <sInternalItem>                                    /* decode one element */
        deducedPos = pBitStrm->currentByte * 8L + pBitStrm->currentBit;
        if (deducedPos > deducedBudgetEnd) {               /* E1 */
            ret = FALSE;
            *pErrCode = <sErrCode>;
        }
        nCount++;
    }
    if (ret) {
        <p><sAcc>nCount = nCount;
        if (deducedBudgetEnd - deducedPos >= 8) {          /* E2 */
            ret = FALSE;
            *pErrCode = <sErrCode>;
        }
    }
}
```

### 7.2 C — OCTET STRING / ASCII string (closed form)

```c
/* decode: data OCTET STRING [size deduced] */
{
    long deducedAvailBits = (pBitStrm->count * 8L - <nTrailingBits>)
                          - (pBitStrm->currentByte * 8L + pBitStrm->currentBit);
    asn1SccSint nCount = deducedAvailBits / 8;
    ret = (nCount >= <nSizeMin>) && (nCount <= <nSizeMax>);
    *pErrCode = ret ? 0 : <sErrCode>;
    if (ret) {
        /* existing external-field body with the computed nCount */
        ret = BitStream_DecodeOctetString_no_length(pBitStrm, <p><sAcc>arr, (int)nCount);
        ...
    }
}
```

### 7.3 Ada — SEQUENCE OF, variable-size elements

Modeled on `oct_sqf_null_terminated_decode` (`StgAda/acn_a.stg:902-928`), preserving
its SPARK loop-invariant style:

```ada
declare
   Deduced_Budget_End : constant Natural := bs.Size_In_Bytes * 8 - <nTrailingBits>;
begin
   <p>.nCount := 0;
   result := ASN1_RESULT'(Success => True, ErrorCode => 0);
   while result.Success
     and then <p>.nCount < <nSizeMax>
     and then bs.Current_Bit_Pos + <nIntItemMinSize> <= Deduced_Budget_End
   loop
      pragma Loop_Invariant (<p>.nCount >= 0 and <p>.nCount <= <nSizeMax>);
      pragma Loop_Invariant (bs.Current_Bit_Pos >= bs.Current_Bit_Pos'Loop_Entry);
      pragma Loop_Invariant (bs.Current_Bit_Pos <= Deduced_Budget_End);
      <sInternalItem>
      if bs.Current_Bit_Pos > Deduced_Budget_End then      -- E1
         result := ASN1_RESULT'(Success => False, ErrorCode => <sErrCode>);
      end if;
      <p>.nCount := <p>.nCount + 1;
   end loop;
   if result.Success and then Deduced_Budget_End - bs.Current_Bit_Pos >= 8 then  -- E2
      result := ASN1_RESULT'(Success => False, ErrorCode => <sErrCode>);
   end if;
end;
```

(Exact invariants to be settled during the Ada phase with `gnatprove`; the
null-terminated loop is the proven precedent.)

### 7.4 Scala

All new macros are added to `StgScala/acn_scala.stg` as **empty stubs** (signature
parity is mandatory — `parseStg2` generates one `AbstractMacros.fs` interface for all
three backends; see `q/details-docs/stg-macro-guide.md`). The front-end guard A6
ensures they are never invoked.

---

## 8. Interaction with `--acn-v2` (deferred patching)

* **Decode-side**: v2 does not change decoding structure — determinants are read
  before content naturally — so the deduced decode loops are identical in both modes.
  The only v2-specific benefit is that the octet/bit CONTAINING decode wrappers
  already limit the stream view (§6.1), so B1 needs zero new v2 work.
* **Encode-side**: deduced types write no determinant, so there is nothing to
  InitDet/PatchDet. The `SZ_EC_Deduced` class joins `SZ_EC_TerminationPattern` in the
  "no determinant" family and must be excluded wherever v2 enumerates patchable size
  dependencies (`DAstACNDeferred.appendPatchDetCalls`, lines 651-731) — which happens
  automatically since no `AcnDependency` is ever created (§3.2).
* **Specialization**: mid-SEQUENCE usages with non-zero trailing constants ride the
  existing closure-conversion specialization (§6.3). `AcnClosureConversion.fs` needs
  to treat "referenced deduced type with non-zero trailing constant" as a
  specialization trigger (today the trigger is cross-scope determinants).
* **Pre-existing gap that this feature must fix (v2, octet CONTAINING encode)**: the
  deferred encode wrappers (`octet_string_containing_deferred_func_encode`,
  `.._embedded_func_encode`, `StgC/acn_c.stg:1253-1304`) leave the stream cursor at
  `acn_data_end`, which is **not byte-aligned** when the contained encoding is not a
  whole-byte multiple, while the decode wrappers reposition to the *byte-rounded*
  region end (`currentBit = 0`). For today's test corpus the contents happen to be
  byte-multiples; a deduced SEQUENCE OF with, say, 12-bit elements would expose the
  mismatch immediately. The encode wrappers must advance the cursor to
  `acn_data_start + nCount*8`, zero-filling — this is the §5.1 padding rule and is a
  correctness fix independent of (but prerequisite for) this feature.

---

## 9. Application-level API contract

The generated decode functions keep their exact current signatures. In exchange, a
**documented contract** applies to any TAS that is deduced or implicitly deduced:

> The `BitStream` passed to `X_ACN_Decode` must be attached to exactly the received
> PDU: `count` (C) / `Size_In_Bytes` (Ada) must equal the PDU's size in bytes as
> reported by the transport layer (with `unusedTailBits` = 0 / `End_Bit_Pos` =
> `Size_In_Bytes * 8`, as `BitStream_AttachBuffer` / `BitStream_init` set them).

This replaces the 2023 proposal's extra `totalEncodedBytes` parameter: the value the
application would have passed is exactly the value it must already use to attach the
buffer. For non-deduced types nothing changes (attaching a larger scratch buffer keeps
working, as today).

The compiler makes the contract visible by emitting a comment block above the decode
prototype of every affected TAS in the generated header:

```c
/* NOTE: 'X' uses ACN deduced-size decoding. The BitStream attached to pBitStrm
   must contain exactly the received PDU (count == PDU size in bytes). */
```

Encoding is unaffected: the application obtains the PDU size from
`BitStream_GetLength` as usual and transmits that many bytes.

---

## 10. Test infrastructure impact

* **`-atc` harness**: `Codec_Decode` (`StgC/test_cases_c.stg:78-83`) attaches the
  decode stream with `<sTasName>_REQUIRED_BYTES_FOR_<sEnc>ENCODING`. For deduced PDUs
  the attach length must be the **actual** encoded length (available from the encode
  stream: `BitStream_GetLength(&bitStrm)`). Implementation: a new macro variant (or a
  boolean parameter) selected by the F# test-case builder when the TAS is
  deduced/implicitly deduced; same change in `StgAda/test_cases_a.stg`. This is safe
  for round-trip testing of all cases including non-byte-multiple totals (the encoder
  pads per §5.1).
* **New v4Tests** (directive-driven, per `q/details-docs/test-infrastructure.md`):
  * `11-SEQUENCE-OF/`: `--TCLS` cases — deduced with fixed-size elements, with
    variable-size elements (SEQUENCE with OPTIONAL inside the element), deduced as
    last PDU component, deduced with trailing fixed components (crc), two nesting
    levels; `--TCLFC` cases — each rule A1–A5, R1–R3 with the expected error text.
  * `06-OCTET-STRING/`, `03-IA5String/`: closed-form cases incl. `SIZE(0..N)` empty
    payload round-trips.
  * `23-CONTAINING/`: deduced inside `OCTET STRING (CONTAINING ...)` with all three
    container size classes (fixed / embedded / `[size fieldref]`), incl. a CHOICE
    around the container (the 2023 doc's Case-a example) and a non-byte-multiple
    content case (12-bit elements) to lock in the padding rule.
  * `--TCLFE` cases: malformed streams triggering E1/E2 (requires the encode side of
    a different variable — these are easier as hand-written C harness tests under
    `acnv2/`-style directories if the directive system cannot express them).
  * Everything above run for C and Ada, legacy and `--acn-v2` (C), matching the
    issue-workflow test matrix.
* **Real-case demos**: extend `14-RealCases/02-PUS` and `CFDP-PROTOCOL` with a variant
  that actually wires the data field through `OCTET STRING (CONTAINING ...)` +
  `size deduced` — the feature's raison d'être and the best regression sentinel.

---

## 11. Secondary touchpoints

| Area | Change |
|---|---|
| ICD (`BackendAst/GenerateAcnIcd.fs:520-558`, `AcnSequenceOf.fs` icdFnc) | New arm: *"Length is deduced from the size of the enclosing container/PDU (no length field is encoded)."* |
| XML export (`FrontEndAst/ExportToXml.fs:269-300`) | New size-property case. |
| LSP autocomplete (`FrontEndAst/LspAst.fs:141-151`) | Add `"size deduced"` to the sizeable-type suggestion lists. |
| ACN reference (`q/details-docs/acn-reference.md`) | New size value in §5.6, §5.7, §5.9, quick-reference §11, new appendix pattern (PUS data field). |
| RTL pruning (`asn1scc/GenerateRTL.fs`, `StgC/LangGeneric_c.fs`) | Only if new RTL helpers are added (none required — §6.4). |

---

## 12. Differences from the 2023 proposal (`Docs/deduced-size.md`)

| # | 2023 proposal | This spec | Why |
|---|---|---|---|
| D1 | Standalone property `[deduced-size]` | `[size deduced]` — a value of the existing `size` property | Mutual exclusion with other size forms for free; one pipeline (`GenSizeProperty → … → SZ_EC_*`); consistent with `size null-terminated`. |
| D2 | New decode parameter `const int totalEncodedBytes` on every affected function, propagated through parents | **No signature changes.** Region = bitstream view; `budgetEnd = viewEnd − compile-time trailing constant`; per-usage inline/specialization for non-zero constants (§6) | The parameter would (a) break the fixed-arity `-atc` harness call, (b) collide with the shared `arrsAcnPrms` slot that user ACN params and v2 det-refs already share, (c) force encode/decode signature asymmetry through a channel shared by both codecs, (d) require Ada/SPARK contract churn. The stream view carries the same information; ACN v2's CONTAINING decode already works exactly this way. |
| D3 | Generated look-ahead functions `X_LA()` per element type | None. Loop guard `remaining ≥ elemMinBits` + post-hoc overrun check E1 + residue check E2 | A sound LA for variable-size/CHOICE elements is a full speculative decode; the min-size guard plus A4 (`elemMin ≥ 8`) gives the same decoding power with two integer comparisons. |
| D4 | "availableBits should be 0..7 at the end, otherwise error" (informal) | Formal slack rule: `< 8` for byte-granular regions, `= 0` for bit-granular; padding written as zeros by encoders, ignored by decoders (§5.1, §5.2) | The 2023 text mixed bytes and bits in its Case-b arithmetic (`totalEncodedBytes − (endPos−startPos)*8 − 32`); this spec defines all arithmetic in bits. |
| D5 | BIT STRING CONTAINING not discussed / assumed unavailable | Bit containers spec'd (slack = 0), implementation optionally phased | BIT STRING CONTAINING was completed under acn-v2 (Phase 7) after the proposal was written. |
| D6 | IA5String allowed unless alphabet-constrained below 8 bits | `encoding ASCII` required (A3) | Default IA5String char-index encoding is ≤ 7 bits *always* (charset ≤ 128); the 2023 wording would have allowed nothing in practice. Mirrors the existing null-terminated + ASCII rule. |
| D7 | Limitations listed informally | Full rule set A1–A7, R1–R6 with error messages and checking locations (§4) | Required for implementability; notably the "deduced must be the last variable-size consumer of its region" property had no enforcement story in 2023. |
| D8 | — | Encoder byte-padding rule + v2 CONTAINING encode alignment fix (§5.1, §8) | Discovered during this analysis; prerequisite for correctness with non-byte-multiple contents. |
| D9 | — | Scala: empty stubs + explicit unsupported error (A6) | Project decision (C/Ada targets only). |

---

## 13. Limitations (v1)

1. No BIT STRING as the deduced type itself (A1) — 1-bit elements are below padding
   resolution. (BIT STRING *as the container* is supported per D5.)
2. No sub-byte elements (A4/A3).
3. No CHOICE / SEQUENCE OF ancestors without a CONTAINING boundary (R1); no OPTIONAL
   deduced components (R3). Both are relaxable later (per-alternative trailing
   constants resp. present-when handling) without wire-format changes.
4. No `align-to-next` after the deduced component (R2c).
5. No streaming-mode bitstreams (A7).
6. Scala not supported (A6).

---

## 14. Implementation plan

Phased so that every phase compiles, passes the full existing regression suite, and
adds its own tests. Estimates assume familiarity with the ACN backend.

### Phase 1 — Front end (grammar → encoding classes → validation)

* `Antlr/acn.g`: `DEDUCED` token, `sizeProp` alternative, `asn1LID` entry.
* `AcnGenericCreateFromAntlr.fs:268-280`: `GP_Deduced` arm.
* `AcnGenericTypes.fs`: `GP_Deduced`, `SzDeduced`, `StrDeduced`.
* `AcnCreateFromAntlr.fs`: arms in `getStringSizeProperty` (58-81),
  `getSizeableSizeProperty` (83-99); rules A1–A5 where the existing sibling rules
  live; reject in `getIntSizeProperty`.
* `Asn1AcnAst.fs`: `SZ_EC_Deduced`, `Acn_Enc_String_Ascii_Deduced`.
* `AcnEncodingClasses.fs`: bounds arms (232-257, 196-219).
* `CheckLongReferences.fs`: no-dependency arms (309-337, 396-407) **plus the new
  region-rules pass** (R1–R6, trailing-constant computation, per usage path).
* A6 Scala guard; A7 note.
* Fix all exhaustive-match sites flagged by the compiler with explicit
  "not yet implemented in backend" errors (temporary), keeping master green.
* Tests: all `--TCLFC` negative cases.

### Phase 2 — C backend, region contexts B2 + B1

* `NestingScope`: `deducedTrailingBits` threading; SEQUENCE builder
  (`AcnSequence.fs`) accumulates trailing constants.
* `AcnSequenceOf.fs` / `AcnOctetBitStrings.fs` / `AcnStrings.fs`: `SZ_EC_Deduced` /
  `Acn_Enc_String_Ascii_Deduced` branches → new STG macros
  (`sqf_deduced`, `oct_deduced`, `str_deduced`, closed-form + incremental variants),
  per `q/details-docs/stg-macro-guide.md` (stubs in all three backends,
  `parseStg2` regeneration, `AbstractMacros.fs`).
* Legacy CONTAINING decode: exact-length temp-stream initialization when content is
  deduced (§6.1); v2 CONTAINING encode byte-alignment fix (§8) + zero-padding.
* `AcnReference.fs` / `DAstACNDeferred.fs` / `AcnClosureConversion.fs`: inline /
  specialization for non-zero trailing constants (§6.3).
* Generated-header contract comment (§9).
* Tests: all C `--TCLS` cases, legacy + `--acn-v2`; `-atc` harness attach-length
  change (`test_cases_c.stg` + `EncodeDecodeTestCase.fs`).

### Phase 3 — Ada backend

* `StgAda/acn_a.stg` macro bodies (closed-form + incremental with SPARK invariants,
  §7.3); exact-length temp streams; `test_cases_a.stg` harness change.
* `gnatprove` pass at the project's established level for the new loops
  (per `q/details-docs/ada-spark-workflow.md`).
* Tests: Ada `--TCLS` matrix.

### Phase 4 — Polish + real cases

* ICD, XML export, LSP (§11); `acn-reference.md` update.
* `14-RealCases` PUS/CFDP deduced variants; E1/E2 malformed-stream tests.
* Full Docker regression + issue-workflow test matrix; retire
  `Docs/deduced-size.md` in favor of this spec.

### Risks

| Risk | Mitigation |
|---|---|
| v2 CONTAINING encode alignment fix changes existing wire output for non-byte-multiple contents | It only changes cases that today round-trip incorrectly; add a dedicated before/after test in `acnv2/`. |
| Trailing-constant accumulation across ReferenceType boundaries (named types mid-chain) meets the v2 "named alias / two nested refs at same id" subtlety (`DAstACNDeferred.fs:963-972`) | Region-rules pass and codegen both keyed on the post-closure-conversion tree; explicit test with a named implicitly-deduced type used from two PDUs with different trailing constants. |
| SPARK proofs for the incremental loop | Invariants modeled on the proven null-terminated loop; closed-form specializations (§5.3) keep the hard case rare. |
| `-atc` attach-length change interacts with XER/uPER test paths | The variant macro is selected only for ACN + deduced TASes; all other paths untouched. |
