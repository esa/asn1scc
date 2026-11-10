# Statement-coverage exemptions of the generated code

The automatic tests of asn1scc (`-atc`) aim to execute every statement of the
generated encoders and decoders. A statement that no test executes must be in
one of two groups:

- **reachable but not yet tested**: there is an input that executes it, and a
  test for it is still missing;
- **proven unreachable**: no input can execute it. The statement is kept on
  purpose (defensive code), and the proof is written down in this document.

This document lists the second group. Each entry has a stable identifier
(`ASN1SCC-COV-NNN`, never reused), the templates that emit the code, an example
of the generated code, the proof, the conditions under which the proof holds,
and the reason the code is kept. Nothing enters this list without a proof; the
`COVERAGE_IGNORE` comments in the templates are **not** proofs (they only tell
the regression line gate to skip a line) and most of them mark reachable error
paths.

The coverage results are therefore reported as three numbers: statements
covered, statements proven unreachable (by entry), and statements neither
covered nor proven.

## How the exemptions are identified

- **In the asn1scc test suite** (`v4Tests/coverage/`): the statement lane
  measures the generated code with GNATcoverage `--level=stmt`;
  `classifyStatements.py` checks the precondition of each entry for every
  missed statement and reports it under the entry's identifier. The checks are
  tested in `testClassifyStatements.py`, including near misses that must *not*
  be exempted.
- **In the generated code** (since asn1scc 4.9.18.0): the code of every entry
  is emitted inside a GNATcoverage exemption region that cites the identifier,
  so that a user who measures the generated code with GNATcoverage gets the
  exemption and its justification in the tool's own report, without any
  asn1scc tool. In C the region is a pair of comments on the existing lines:

  ```c
  default:  /*COVERAGE_IGNORE*/ /* GNATCOV_EXEMPT_ON "ASN1SCC-COV-001: value range enforced by the RTL decode, see Docs/statement-coverage-exemptions.md" */
      *pErrCode = ERR_UPER_DECODE_COLOR;     /*COVERAGE_IGNORE*/
      ret = FALSE;                /*COVERAGE_IGNORE*/ /* GNATCOV_EXEMPT_OFF */
  ```

  In Ada it is a pair of pragmas, which the compiler and GNATprove ignore:

  ```ada
  when others => pragma Annotate (Xcov, Exempt_On, "ASN1SCC-COV-001: value range enforced by the RTL decode, see Docs/statement-coverage-exemptions.md");   -- COVERAGE_IGNORE
      val := MyPDU_alpha;   -- COVERAGE_IGNORE
      result := adaasn1rtl.ASN1_RESULT'(Success => False, ErrorCode => ERR_ACN_DECODE_MYPDU); pragma Annotate (Xcov, Exempt_Off);   -- COVERAGE_IGNORE
  ```

  For the templates whose proof depends on the instance (ACN ENUMERATED, see
  ASN1SCC-COV-001 and -003; the CHOICE and determinant switches of
  ASN1SCC-COV-004, -005, -008 and -009; the region checks of ASN1SCC-COV-010 and -011)
  the compiler emits the region only when the conditions of the entry hold;
  otherwise the arm is reachable and is covered by an automatic test. The
  asn1scc test suite checks that every region it measures is exactly the set of
  statements whose precondition `classifyStatements.py` confirms, and that no
  region is ever executed.

An exemption region records a claim; GNATcoverage does not check it. It reports
the region as "exempted, with violations" while the code stays unexecuted and
as "exempted, no violation" if a test ever executes it, which would mean the
proof no longer holds. Decision and MC/DC obligations inside a region are
exempted the same way (ASN1SCC-COV-002 covers the guard decision itself).

---

## ASN1SCC-COV-001: default arm after a range-checked index decode (C, Ada)

**Code.** A decoder reads an index or a value with
`BitStream_DecodeConstraintWholeNumber(pBitStrm, &v, min, max)` or
`BitStream_DecodeConstraintPosWholeNumber(pBitStrm, &v, min, max)` (or one of
their `Int8` / `Int16` / `Int32` / `UInt8` / `UInt16` / `UInt32` variants, used
for smaller integer types, which call the base function and only cast its
result), then
switches on `v`. The switch has one `case` for every value `min..max`, plus a
`default:` arm that reports an error. The `default:` arm cannot execute.

**Templates** (`StgC/`):

| template | macro | decoded value |
|---|---|---|
| `uper_c.stg` | `Enumerated_decode` | uPER index of an ENUMERATED item |
| `uper_c.stg` | `choice_decode` | uPER index of a CHOICE alternative |
| `acn_c.stg` | `Choice_decode` | ACN CHOICE index (no determinant) |
| `acn_c.stg` | `EnumeratedEncValues_decode` | ACN ENUMERATED value, **only** when the value is decoded with one of the two functions above and the item values fill `min..max` |
| `StgAda/acn_a.stg` | `EnumeratedEncValues_decode` (`when others` arm) | ACN ENUMERATED value in Ada whose item values fill `min..max`, decoded with `UPER_Dec_ConstraintPosWholeNumber` or with a fixed-size `Acn_Dec_*_ConstSize*` procedure |
| `StgAda/acn_a.stg` | `EnumeratedEncValues_fullIndex` (`when others` arm) | ACN ENUMERATED value in Ada whose item values are exactly the values of its field (`0..2^k-1` in `k` bits), decoded with a `_FullRange` procedure (asn1scc 4.9.70.0 and later) |

For `EnumeratedEncValues_decode` the proof depends on the instance: an ACN
ENUMERATED whose values leave gaps (e.g. `{a(0), b(5)}` in 3 bits) can deliver
a value without a `case`. So can, in C only, one whose integer is decoded with a
fixed-size function such as `Acn_Dec_Int_PositiveInteger_ConstSize_8`: the C
fixed-size decoders return every value of the bit width. Those `default:` arms
are reachable with an invalid message and are covered by automatic tests
(`test_case_invalid_stream_*`), not by an exemption.

**Example** (uPER, `Color ::= ENUMERATED { red, green, blue }`):

```c
{
    asn1SccSint enumIndex;
    ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &enumIndex, 0, 2);
    *pErrCode = ret ? 0 : ERR_UPER_DECODE_COLOR;
    if (ret) {
        switch(enumIndex)
        {
            case 0:
                (*(pVal)) = red;
                break;
            case 1:
                (*(pVal)) = green;
                break;
            case 2:
                (*(pVal)) = blue;
                break;
            default:                        /*COVERAGE_IGNORE*/    /* <- exempted */
                *pErrCode = ERR_UPER_DECODE_COLOR;     /*COVERAGE_IGNORE*/
                ret = FALSE;                /*COVERAGE_IGNORE*/
        }
    }
}
```

**Proof.** The switch is entered only when `ret` is TRUE. Both RTL functions
(`asn1crt/asn1crt_encoding.c`) return TRUE only with a value in `min..max`
(abridged, comments added):

```c
flag BitStream_DecodeConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint* v, asn1SccSint min, asn1SccSint max)
{
    ...
    asn1SccUint range = (asn1SccUint)max - (asn1SccUint)min;
    ASSERT_OR_RETURN_FALSE(min <= max);
    *v = 0;
    if (!range) {
        *v = min;                       /* the only value */
        return TRUE;
    }
    nRangeBits = GetNumberOfBitsForNonNegativeInteger(range);
    if (BitStream_DecodeNonNegativeInteger(pBitStrm, &uv, nRangeBits))
    {
        if (uv > range)
            return FALSE;               /* values above max are rejected */
        *v = uint2int(uv + (asn1SccUint)min, WORD_SIZE);
        return TRUE;                    /* min <= *v <= max */
    }
    return FALSE;
}
```

`BitStream_DecodeConstraintPosWholeNumber` is the same function for unsigned
values (`*v = uv + min`). Since `0 <= uv <= range = max - min`, the value is in
`min..max`, and every value of `min..max` has a `case`. So no input reaches
`default:`.

**Conditions.** The proof holds while:

1. both RTL functions keep rejecting `uv > range` and return `min..max` on success;
2. the switch selector is the variable just decoded, with nothing in between
   that assigns it;
3. the `case` labels of the switch are exactly `min..max`.

`classifyStatements.py` checks 2 and 3 for every missed statement (the decode
call must be among the four lines before the `switch`, with no assignment to the
selector in between; the labels of that switch — not of a nested one — must
equal `min..max`); condition 1 is a property of the RTL source above.

**Why the code stays.** Defense in depth: the arm keeps the decoder safe if the
RTL changes, if the template is used with another decoding function, or if the
index variable is corrupted in memory (e.g. by a single-event upset) between
the decode and the switch. MISRA C:2012 Rule 16.4 also requires a `default`
label in every switch.

**Ada.** The uPER ENUMERATED / CHOICE and the ACN CHOICE templates have no
such arm: the decoded integer is converted to the index subtype
(`case <index_range>(intVal) is`), the `case` must cover the whole subtype, and
the postcondition of `UPER_Dec_ConstraintWholeNumber` (`Result and IntVal in
MinVal .. MaxVal`, `ADA_RTL2/src/adaasn1rtl-encoding-uper.ads`) lets GNATprove
show that the conversion cannot fail.

The ACN ENUMERATED template does have one: it switches on the decoded
`Asn1UInt` value, and Ada requires `when others` for a `case` on a type wider
than its choices:

```ada
adaasn1rtl.encoding.uper.UPER_Dec_ConstraintPosWholeNumber(bs, intVal_val, 0, 1, 1, result.Success);
if result.Success then
    case intVal_val is
        when 0 => val := MyPDU_alpha;
        when 1 => val := MyPDU_beta;
    when others =>                    -- COVERAGE_IGNORE    <- exempted
        val := MyPDU_alpha;           -- COVERAGE_IGNORE
        result := adaasn1rtl.ASN1_RESULT'(Success => False, ErrorCode => ERR_ACN_DECODE_MYPDU);   -- COVERAGE_IGNORE
    end case;
else
    ...
```

The postcondition of `UPER_Dec_ConstraintPosWholeNumber` is `(Result and IntVal
in MinVal .. MaxVal) or (not Result and IntVal = MinVal)`, so inside
`if result.Success` the value is in `min..max`, and every value has a `when`.
Unlike C, the Ada fixed-size ACN decoders take the bounds too and have the same
kind of postcondition (all `Acn_Dec_*_ConstSize*` procedures of
`ADA_RTL2/src/adaasn1rtl-encoding-acn.ads`), for example:

```ada
procedure Acn_Dec_Int_PositiveInteger_ConstSize
  (bs     : in out Bitstream; IntVal : out Asn1UInt; minVal : Asn1UInt;
   maxVal : Asn1UInt; nSizeInBits : Integer; Result : out ASN1_RESULT) with
   ...
   Post => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nSizeInBits and
   ((Result.Success and IntVal >= minVal and IntVal <= maxVal) or
    (not Result.Success and IntVal = minVal));
```

The template passes the smallest and the largest item value as `minVal` and
`maxVal`, so with item values that fill that range the `when others` arm cannot
execute either. The choices must be exactly `min..max` (sparse values are
reachable and tested). `classifyStatements.py` checks the Ada shape in the same
way as the C one.

Since asn1scc 4.9.70.0, an ACN ENUMERATED whose item values are exactly
`0..2^k-1` and whose field is `k` bits wide (the uPER-like encoding, or a
fixed-size `pos-int` encoding of `k` bits) is decoded with a `_FullRange`
procedure, which has no `Result` and cannot fail: every value of the field is
an item. The `case` then has no success branch around it, and the `when
others` arm only keeps `val` defined:

```ada
-- cannot fail, no result to check: every bit pattern of the field is a valid value
adaasn1rtl.encoding.acn.Acn_Dec_Int_PositiveInteger_ConstSize_FullRange(bs, intVal_val, 2);
case intVal_val is
    when 0 => val := red;
    when 1 => val := green;
    when 2 => val := blue;
    when 3 => val := black;
when others =>                        -- COVERAGE_IGNORE    <- exempted
    -- unreachable: the read returns only the values of the field, each of them an item
    val := red;                       -- COVERAGE_IGNORE
end case;
```

`Acn_Dec_Int_PositiveInteger_ConstSize_FullRange` (and its fixed-width
twins `..._ConstSize_8_FullRange`, `..._big_endian_16_FullRange`, ...)
reads exactly the bits of the field and returns them as an unsigned value,
so the value is in `0..2^k-1`; the postcondition states it
(`IntVal < Shift_Left (1, nSizeInBits)`, `IntVal <= 16#FF#`, ...).
`UPER_Dec_ConstraintPosWholeNumber_FullRange(bs, v, min, nBits)` returns
`min` plus an `nBits`-bit value, `min..min+2^nBits-1` (postcondition
`IntVal >= MinVal and IntVal - MinVal < Shift_Left (1, nBits)`). The variants
for smaller target types (`...FullRangeUInt8`, ...) read the same bits. With
choices `0..2^k-1` every value has a `when`. `classifyStatements.py` accepts
this form when the selector was just decoded by such a call and the choices
equal the values it returns.

---

## ASN1SCC-COV-002: character index guard in alphabet-constrained string decoding (C)

**Code.** Strings with a permitted alphabet (`FROM(...)`) are decoded character
by character as an index into the alphabet. After the index decode a guard
checks the index again and reports an error if it is outside the alphabet. The
body of the guard cannot execute.

**Template**: `StgC/uper_c.stg`, macro `InternalItem_string_with_alpha_decode`
(used by uPER and ACN string decoding).

**Example** (`IA5String (SIZE(1..4)) (FROM("A".."C"))`):

```c
asn1SccSint charIndex = 0;
ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &charIndex, 0, 2);
*pErrCode = ret ? 0 : ERR_UPER_DECODE_SHAPE_LABEL;
if (ret && (charIndex < 0 || charIndex > 2)) { ret = FALSE; *pErrCode = ERR_UPER_DECODE_SHAPE_LABEL; }
pVal->u.label[i1] = ret ? allowedCharSet[charIndex] : '\0' ;
```

Only the two statements inside the braces are exempted; the `if` itself is
executed by every decode.

**Proof.** As for ASN1SCC-COV-001: when `ret` is TRUE,
`BitStream_DecodeConstraintWholeNumber(.., 0, N)` has returned a value in
`0..N`, so `charIndex < 0 || charIndex > N` is false.

**Conditions.** The RTL property of ASN1SCC-COV-001, and the guard bound `N` is
the `max` of the decode call right before it (checked by
`classifyStatements.py`).

**Why the code stays.** The guard protects the array access
`allowedCharSet[charIndex]` independently of the RTL. It was added with the
memory-safety fixes of March 2026 (commit `fe6ab763`) and is kept as defense in
depth.

**Ada.** The Ada template has no guard. `UPER_Dec_ConstraintWholeNumberInt`
has the postcondition `(Result and IntVal in MinVal .. MaxVal) or (not Result
and IntVal = MinVal)`, so `charIndex` is in `0..N` even when the decode fails,
and GNATprove proves the alphabet access `alpha_set(charIndex + 1)` without a
run-time check.

---

## ASN1SCC-COV-003: default arm after a fixed-size unsigned decode that fills its width (C)

**Code.** An ACN ENUMERATED encoded as an unsigned integer of `N` bits
(`encoding pos-int, size N`) whose item values are exactly `0 .. 2^N - 1`, for
example two items in one bit or four items in two bits. The C decoder reads
the value with `Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, &v, N)` (or
the `_8`, `_big_endian_16/32/64`, `_little_endian_16/32/64` variants) and
switches on it; every value that `N` bits can hold has a `case`, so the
`default:` arm cannot execute.

**Template**: `StgC/acn_c.stg`, macro `EnumeratedEncValues_decode`, for those
instances only. With item values that leave a value of the width unused the
arm is reachable and covered by an automatic test (`test_case_invalid_stream_*`,
see ASN1SCC-COV-001).

**Example** (`DeliveryCode ::= ENUMERATED {data-complete (0), data-incomplete (1)}`,
`DeliveryCode [encoding pos-int, size 1]`):

```c
ret = Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, (&(intVal_pVal)), 1);
*pErrCode = ret ? 0 : ERR_ACN_DECODE_DELIVERYCODE;
if (ret) {
    switch (intVal_pVal) {
        case 0:
            (*(pVal)) = data_complete;
            break;
        case 1:
            (*(pVal)) = data_incomplete;
            break;
    default:                                    /*COVERAGE_IGNORE*/    /* <- exempted */
        ret = FALSE;                            /*COVERAGE_IGNORE*/
        *pErrCode = ERR_ACN_DECODE_DELIVERYCODE;                 /*COVERAGE_IGNORE*/
    }
} /*COVERAGE_IGNORE*/
```

**Proof.** `Acn_Dec_Int_PositiveInteger_ConstSize` (`asn1crt/asn1crt_encoding_acn.c`)
stores the result of `BitStream_DecodeNonNegativeInteger(pBitStrm, &tmp, N)`
(`asn1crt/asn1crt_encoding.c`). That function builds the value from `N` bits
only: whole bytes are shifted in eight bits at a time, and the last `N mod 8`
bits come from `BitStream_ReadPartialByte`, which masks its result with
`masksb[nbits]` on both of its paths. The `_8`, `_big_endian_16/32/64` and
`_little_endian_16/32/64` variants assemble the value from 1, 2, 4 or 8 bytes
read with `BitStream_ReadByte`. The value is therefore below `2^N`, and every
value below `2^N` has a `case`.

**Conditions.** The decoder reads exactly `N` bits into an unsigned value; the
selector is the variable just decoded, with no assignment in between; the
`case` labels of that switch are exactly `0 .. 2^N - 1` (all checked by
`classifyStatements.py`, the first from the decoder's name and `N`).

**Why the code stays.** Defense in depth, as for ASN1SCC-COV-001, and MISRA
C:2012 Rule 16.4.

**Ada.** Covered by ASN1SCC-COV-001: the Ada fixed-size decoders check the item
range themselves (postcondition), which is stronger than the width argument.

---

## ASN1SCC-COV-004: default arm of a switch on a CHOICE selector that an earlier switch of the same encoder already checked (C)

**Code.** An ACN encoder switches twice on the selector (`kind`) of the same
CHOICE. The first switch has a `case` for every alternative it accepts and a
`default:` arm that sets `ret = FALSE`; the second switch runs only when the
first one let `ret` stay TRUE, and has a `case` for every label of the first.
The `default:` arm of the second switch cannot execute. There are two shapes:

- **Legacy ACN, and `--acn-v2` determinants of the same SEQUENCE.** The
  update of an ACN determinant (the inserted field that selects the CHOICE,
  through an ENUMERATED value or `present-when` conditions) maps the
  selector to the determinant's value before the determinant is encoded. The
  CHOICE itself is encoded later in the same function and switches on the
  selector again to encode the alternative.
- **`--acn-v2` deferred determinants.** The determinant was reserved earlier
  in the bitstream (`InitDet`) and is patched (`PatchDet`) after the
  function body that encodes the CHOICE succeeded; the value to patch is
  computed by a second switch on the selector.

**Templates** (`StgC/acn_c.stg`): `Choice_Enum_encode` and
`Choice_preWhen_encode` (first shape: the second switch is the CHOICE's),
`acn_deferred_det_switch_int` and `acn_deferred_det_switch_str` (second shape:
the second switch computes the patch value; the first is the CHOICE's own).
The first switch of the first shape is emitted by `ChoiceDependencyEnum` or
`ChoiceDependencyPres`; when a CHOICE selects several determinants (e.g. a
service type and a subtype), the update switches of the later ones are second
switches too.

**Example** (legacy; `Kind ::= ENUMERATED {a, b}`, `Msg ::= SEQUENCE { chr Ch }`,
ACN `Msg [] { kind Kind [encoding pos-int, size 8], chr <kind> [] }`,
`Ch <Kind:kind> [determinant kind]`):

```c
switch (pVal->chr.kind) {               /* update of the determinant */
    case Ch_a_PRESENT:
        Msg_kind = Kind_a;
        break;
    case Ch_b_PRESENT:
        Msg_kind = Kind_b;
        break;
    default:
        ret = FALSE;                            /*COVERAGE_IGNORE*/
        *pErrCode = ERR_INVALID_ENUM_VALUE;     /*COVERAGE_IGNORE*/
}
if (ret) {
    /* ... encoding of Msg_kind ... */
}
if (ret) {
    /*Encode chr */
    switch(pVal->chr.kind)
    {
        case Ch_a_PRESENT:
            BitStream_EncodeConstraintPosWholeNumber(pBitStrm, pVal->chr.u.a, 0, 10);
            break;
        case Ch_b_PRESENT:
            BitStream_AppendBit(pBitStrm,pVal->chr.u.b);
            break;
    default:                                    /*COVERAGE_IGNORE*/    /* <- exempted */
        *pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/
        ret = FALSE;                    /*COVERAGE_IGNORE*/
    }
}
```

`--acn-v2` (the specialised encoder of `chr`, which receives the reserved
determinant as a parameter):

```c
switch(pVal->kind)
{
    /* ... one case per alternative ... */
default:
    *pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/
    ret = FALSE;                    /*COVERAGE_IGNORE*/
}
if (ret) {
    {
    asn1SccUint patchDetVal;
    switch(pVal->kind) {
        case Ch_a_PRESENT: patchDetVal = 0; break;
        case Ch_b_PRESENT: patchDetVal = 1; break;
        default:                                    /*COVERAGE_IGNORE*/    /* <- exempted */
            *pErrCode = ERR_INVALID_CHOICE_ALTERNATIVE; /*COVERAGE_IGNORE*/
            return FALSE;                           /*COVERAGE_IGNORE*/
    }
    ret = Acn_PatchDet_U8((asn1SccUint)patchDetVal, pBitStrm, Msg_chr_kind, pErrCode);
    }
}
```

**Proof.** The encoder receives the value through a pointer to `const` and
writes no part of it, so both switches read the same selector value. When the
first switch takes its `default:` arm, `ret` becomes FALSE. Between the two
switches, every statement that could set `ret` back to TRUE runs only inside an
`if (ret)` block opened after the first switch, which is not entered once `ret`
is FALSE; the second switch is inside such a block. So when the second switch runs, the first one took one of its
`case` arms, and the value is one of its labels, which all have a `case` in the
second switch.

**Conditions** (checked by `classifyStatements.py` on the generated code):
the selector is read through the `const` parameter `pVal` (no array index);
the first switch is in the same function, before the second, its `default:`
arm sets `ret = FALSE` and nothing else to `ret`, and its labels are labels
of the second switch; the second switch is inside an `if (ret)` block that
opens after the first switch, and no statement between the two sets `ret` to
anything but FALSE outside such a block; every condition (`if`, `case`) that
guards the first switch but not the second is also a condition of the second
(`if (E == L)` and `case L:` of `switch (E)` count as the same), or is an
`if (ret)`; a condition `a && b` counts as both `a` and `b`. The compiler
emits the region only when its own record of the encoder shows the same: the
update of a determinant ran earlier in the same function on the same selector,
and the CHOICE has no always-absent alternative (the second switch would have
no `case` for it); for `--acn-v2`, the patch runs after a body that may fail,
under the checks of the path to the CHOICE (the `exist` flag of every
OPTIONAL component, the selection of every CHOICE alternative on the way),
which contain the conditions of the CHOICE's own switch.

In both modes, a CHOICE in an `ALWAYS PRESENT` component does not meet the
conditions: the update checks the component's `exist` flag, the encoding of
the CHOICE does not. Those arms are not exempted.

**Why the code stays.** The second switch is generated by a template that is
also used where no earlier switch exists (a CHOICE encoder called directly),
and MISRA C:2012 Rule 16.4 requires a `default:` label in every switch.

**Ada.** No exemption: the Ada CHOICE encoders and the legacy determinant
updates use exhaustive `case` statements without `when others`, and the
`--acn-v2` patch value is a `case` expression (`when others => 0`), which has
no statement of its own.

---

## ASN1SCC-COV-005: default arm of the encoding of an ACN determinant that every update sets to an item (C)

**Code.** An ACN inserted field of an ENUMERATED type that selects a CHOICE is
held in a local of the encoder. The update of the determinant assigns the local
one item of the ENUMERATED per CHOICE alternative; the encoding of the
determinant then maps the local to its encoded value with a switch that has a
`case` for every item and a failing `default:` arm. The `default:` arm cannot
execute.

**Template**: `StgC/acn_c.stg`, macro `EnumeratedEncValues_encode`, for the
determinant locals whose every update is a CHOICE selector update
(`ChoiceDependencyEnum`, possibly several of them combined by
`MultiAcnUpdate`). Not when an update copies a value from the encoded value
(`pVal`), e.g. an ENUMERATED component passed as an ACN argument.

**Example** (the legacy example of ASN1SCC-COV-004, encoding of `Msg_kind`):

```c
if (ret) {
    /*Encode Msg_kind */
    /* no check of Msg_kind_is_initialized: its update above sets Msg_kind on every path that reaches here */
    switch(Msg_kind) {
        case Kind_a:
            intVal_Msg_kind = 0UL;
            break;
        case Kind_b:
            intVal_Msg_kind = 1UL;
            break;
        default:                                    /*COVERAGE_IGNORE*/    /* <- exempted */
            ret = FALSE;                            /*COVERAGE_IGNORE*/
            *pErrCode = ERR_ACN_ENCODE_MSG_KIND;                 /*COVERAGE_IGNORE*/
    }
    if (ret) {
        Acn_Enc_Int_PositiveInteger_ConstSize_8(pBitStrm, intVal_Msg_kind);
    }
}
```

**Proof.** Every assignment of the local in the encoder stores an item that
has a `case` (or a copy of another local of which the same holds, when one
determinant selects several CHOICEs). The switch reads the local only after
an assignment: either under its `_is_initialized` flag, which starts FALSE and
is set only right after an assignment of the local, or after a construct that
assigns it on every path on which `ret` stays TRUE (a switch with an
assignment in every `case` and a failing `default:`, or an `if` / `else if`
chain with an assignment in every branch and a final `else` that sets `ret =
FALSE`), with the same `ret` argument as ASN1SCC-COV-004.

**Conditions** (checked by `classifyStatements.py`): the switch is in an
encoder and its selector is a local declared without an initializer in that
encoder; every assignment of the local is a label of the switch, or a local
whose assignments are all labels of the switch and which is read only under
its own `_is_initialized` flag (set only after an assignment of it); and one of
the two forms of "read only after an assignment" above holds. The compiler
emits the region only for a determinant whose updates all are CHOICE selector
updates.

**Why the code stays.** The same template encodes ENUMERATED components of the
encoded value, whose `default:` arm is reachable with an invalid value, and
MISRA C:2012 Rule 16.4.

**Ada.** No exemption needed: the Ada encoding is an exhaustive `case`
statement without `when others`.

---

## ASN1SCC-COV-008: default arm of a CHOICE decoded after its ENUMERATED determinant in the same decoder (C)

**Code.** An ACN SEQUENCE has an inserted field of an ENUMERATED type that
selects a CHOICE component (`<field>` given as the CHOICE's determinant). The
decoder of the SEQUENCE decodes the field into a local, then decodes the CHOICE
with a switch on that local that has a `case` for every item of the field and a
failing `default:` arm. The `default:` arm cannot execute. This is the decoding
twin of ASN1SCC-COV-004 / -005.

**Template**: `StgC/acn_c.stg`, macro `Choice_Enum_decode`, for the CHOICEs
whose determinant the same function decoded before (an inserted field of the
SEQUENCE being decoded, or of a mandatory SEQUENCE component decoded in the
same function), and whose alternatives cover every item of the field (none
ALWAYS ABSENT).

**Example** (`AcceptanceFailureCode ::= ENUMERATED {error1, error2}`, a CHOICE
`AcceptanceFailureData` with the alternatives `error1` and `error2`,
`AcceptanceFailureNotice ::= SEQUENCE { data AcceptanceFailureData }`, ACN
`AcceptanceFailureNotice [] { code AcceptanceFailureCode [encoding pos-int, size 8], data <code> [] }`):

```c
ASN1SCC_AcceptanceFailureCode AcceptanceFailureNotice_code;
...
/*Decode AcceptanceFailureNotice_code */
ret = Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm, (&(intVal_AcceptanceFailureNotice_code)));
*pErrCode = ret ? 0 : ERR_ACN_DECODE_ACCEPTANCEFAILURENOTICE_CODE;
if (ret) {
    switch (intVal_AcceptanceFailureNotice_code) {
        case 0:
            AcceptanceFailureNotice_code = AcceptanceFailureCode_error1;
            break;
        case 1:
            AcceptanceFailureNotice_code = AcceptanceFailureCode_error2;
            break;
    default:                                    /*COVERAGE_IGNORE*/
        ret = FALSE;                            /*COVERAGE_IGNORE*/
        *pErrCode = ERR_ACN_DECODE_ACCEPTANCEFAILURENOTICE_CODE;                 /*COVERAGE_IGNORE*/
    }
} /*COVERAGE_IGNORE*/
if (ret) {
    /*Decode data */
    switch(AcceptanceFailureNotice_code)
    {
        case AcceptanceFailureCode_error1:
            /* ... */
            break;
        case AcceptanceFailureCode_error2:
            /* ... */
            break;
    default:                                    /*COVERAGE_IGNORE*/    /* <- exempted */
        *pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/
        ret = FALSE;                    /*COVERAGE_IGNORE*/
    }
}   /*COVERAGE_IGNORE*/
```

When the ENUMERATED type has one item and the uPER-like encoding, nothing is on
the wire and the decoder assigns the item instead of the first switch
(`AcceptanceFailureNotice_code = AcceptanceFailureCode_error1;`).

**Proof.** The local is declared without an initializer and every assignment of
it in the decoder stores an item of the field's type, which has a `case` in the
CHOICE switch. The CHOICE switch reads the local only after an assignment: either
after an assignment in the same block or an enclosing one (one item), or inside
an `if (ret)` block opened after the field's decoding switch, which assigns the
local in every `case` and sets `ret = FALSE` in its `default:` arm; between the
two, every statement that could set `ret` back to TRUE runs only inside an
`if (ret)` block, with the same argument as ASN1SCC-COV-004. So the local holds
one of the `case` labels.

**Conditions** (checked by `classifyStatements.py` on the generated code): the
switch is in a decoder and its selector is a local declared without an
initializer in that decoder; every assignment of the local is a label of the
switch; and one of the two forms of "read only after an assignment" above holds.
The compiler emits the region only for a CHOICE whose determinant is an
ENUMERATED field decoded earlier in the same function (not with the switch-free
encoding `-eee`), and whose alternatives are exactly the items of the field.

Not exempted here: a CHOICE that receives its determinant as a parameter (a
reference to a CHOICE type with an ACN parameter, and the `--acn-v2` specialised
functions, which switch on the value of an `AcnInsertedFieldRef`): the proof needs
every caller; for the `--acn-v2` specialised functions it is ASN1SCC-COV-009.

**Why the code stays.** The same template decodes CHOICEs whose determinant is
a parameter, where the arm is not proven, and MISRA C:2012 Rule 16.4.

**Ada.** No exemption needed: the Ada decoding is an exhaustive `case` statement
on the ENUMERATED local, without `when others`.

## ASN1SCC-COV-009: default arm of an `--acn-v2` specialised CHOICE decoder whose callers decoded its ENUMERATED determinant (C, Ada)

**Code.** With `--acn-v2`, a CHOICE whose determinant is an ACN inserted field
of an enclosing SEQUENCE is decoded by a specialised function that receives the
field as an `AcnInsertedFieldRef` parameter and switches on its value, with a
`case` per alternative (the value the field writes for that item) and a failing
`default:` arm. The decoder of the SEQUENCE that holds the field decodes the
field (as for ASN1SCC-COV-008: a switch over the wire values with a failing
`default:` arm, or the only item of a one-item type), stores the item's value in
the reference, and calls the specialised function only afterwards. The
`default:` arm cannot execute.

**Template**: `StgC/acn_c.stg`, macro `Choice_Enum_decode`, for the specialised
CHOICE decoders of `--acn-v2` whose determinant is an ACN inserted ENUMERATED
field (not with the switch-free encoding `-eee`) of a SEQUENCE that holds the
CHOICE, and whose alternatives are exactly the items of the field.

**Example** (`Code ::= ENUMERATED {a, b}`, a CHOICE `Data` with the alternatives
`a` and `b`, ACN `P [] { code Code [encoding pos-int, size 8], data <code> [] }`):

```c
flag ASN1SCC_P_data_ACN_Decode(ASN1SCC_Data* pVal, BitStream* pBitStrm, int* pErrCode, AcnInsertedFieldRef* P_data_code)
{
    ...
    switch(P_data_code->value)
    {
        case 0: /* ... */ break;
        case 1: /* ... */ break;
    default:                                    /*COVERAGE_IGNORE*/    /* <- exempted */
        *pErrCode = ERR_INVALID_ENUM_VALUE;         /*COVERAGE_IGNORE*/
        ret = FALSE;                    /*COVERAGE_IGNORE*/
    }
    ...
}

flag ASN1SCC_P_ACN_Decode(ASN1SCC_P* pVal, BitStream* pBitStrm, int* pErrCode)
{
    ASN1SCC_Code P_code_tmp;
    AcnInsertedFieldRef code = {0};
    ...
    if (ret) {
        switch (intVal_P_code_tmp) {
            case 0: P_code_tmp = Code_a; break;
            case 1: P_code_tmp = Code_b; break;
        default:                                    /*COVERAGE_IGNORE*/
            ret = FALSE;                            /*COVERAGE_IGNORE*/
            *pErrCode = ERR_ACN_DECODE_P_CODE;      /*COVERAGE_IGNORE*/
        }
    }
    if (ret) {
        code.value = (asn1SccUint)P_code_tmp;
    }
    if (ret) {
        ret = ASN1SCC_P_data_ACN_Decode((&(pVal->data)), pBitStrm, pErrCode, &code);
    }
    ...
}
```

When the field's values differ from the C enum values (e.g. `encode-values`), the
store is a switch over the local with one `case` per item
(`case Code_a: code.value = 0; break;`); for a one-item type the local is assigned
its item without a read, and the store and the call follow without a check of
`ret`, which nothing has set before.

**Proof.** The reference is a local of the caller initialised to zero; in
decoding it is written only by the store after the field's decoding (the PatchDet
calls of `--acn-v2` run only in encoding). The field's local holds an item, as in
ASN1SCC-COV-008, so the store writes the value of an item, which is a `case`
label of the specialised decoder. The call runs only after the store: inside an
`if (ret)` block opened after it, with `ret` unable to turn TRUE again on the way
(the argument of ASN1SCC-COV-004), or after an `if (ret)` block that nothing
before it in the function can have skipped. A specialised function that passes
its own reference parameter on to the CHOICE decoder is covered when every call
of it satisfies the same condition. ACN inserted fields are never optional and
the front end requires a determinant before its dependent, so a field of a
SEQUENCE that holds the CHOICE is always decoded before it.

**Conditions** (checked by `classifyStatements.py` on the generated code): the
switch is in a decoder with an `AcnInsertedFieldRef` parameter, its selector is
that parameter's `value` and its labels are integers; every call of the decoder
in the file passes either `&L`, a local reference of the caller whose every
`value` assignment stores a label as above (the casted local holding only enum
items whose values, read from the unit's C headers, are labels; or a switch on
such a local with a `case` for each value the local can hold), with the call
after the store; or a reference parameter of the caller, for which every call of
the caller is checked the same way. At least one call must exist.

**Why the code stays.** The same template decodes CHOICEs whose determinant is
a parameter not covered here (a field exposed by a referenced type, or `-eee`),
and MISRA C:2012 Rule 16.4.

**Ada** (asn1scc 4.9.99.0 and later). The Ada specialised decoder has the same
arm as `when others` of a `case` on the reference's `Value` (an `Asn1UInt`, so the
`case` needs it; template `StgAda/acn_a.stg`, macro `Choice_Enum_decode`, the
same instances as in C). The caller decodes the field into a local of the Ada
enumeration type of the field and stores its value:

```ada
procedure ASN1SCC_P_data_ACN_Decode_aux(val: in out ASN1SCC_Data; bs : in out adaasn1rtl.encoding.Bitstream; result : OUT adaasn1rtl.ASN1_RESULT; P_data_code : in out adaasn1rtl.encoding.acn.AcnInsertedFieldRef)
is
begin
    result := adaasn1rtl.ASN1_RESULT'(Success => true, ErrorCode => 0);
    case P_data_code.Value is
        when 1 =>
            ...
        when 5 =>
            ...
    when others => pragma Annotate (Xcov, Exempt_On, "ASN1SCC-COV-009: ...");   -- COVERAGE_IGNORE
            result := adaasn1rtl.ASN1_RESULT'(Success => False, ErrorCode => ERR_ACN_DECODE_P_DATA); pragma Annotate (Xcov, Exempt_Off); -- COVERAGE_IGNORE
    end case;
end ASN1SCC_P_data_ACN_Decode_aux;

procedure ASN1SCC_P_ACN_Decode_aux(val: in out ASN1SCC_P; bs : in out adaasn1rtl.encoding.Bitstream; result : OUT adaasn1rtl.ASN1_RESULT)
is
    P_code_tmp : ASN1SCC_Code;
    code : adaasn1rtl.encoding.acn.AcnInsertedFieldRef := (others => <>);
    intVal_P_code_tmp : adaasn1rtl.Asn1UInt;
begin
    adaasn1rtl.encoding.acn.Acn_Dec_Int_PositiveInteger_ConstSize_8(bs, intVal_P_code_tmp, 1, 5, result);
    result.ErrorCode := ERR_ACN_DECODE_P_CODE;
    if result.Success then
        case intVal_P_code_tmp is
            when 1 => P_code_tmp := Code_a;
            when 5 => P_code_tmp := Code_b;
        when others =>
            P_code_tmp := Code_a;
            result := adaasn1rtl.ASN1_RESULT'(Success => False, ErrorCode => ERR_ACN_DECODE_P_CODE);
        end case;
    else
        P_code_tmp := Code_a;
        result.ErrorCode := ERR_ACN_DECODE_P_CODE;
    end if;
    if result.Success then
        code.Value := adaasn1rtl.Asn1UInt(ASN1SCC_Code'Enum_Rep(P_code_tmp));
    end if;
    if result.Success then
        ASN1SCC_P_data_ACN_Decode_aux(val.data, bs, result, code);
    end if;
end ASN1SCC_P_ACN_Decode_aux;
```

with `for ASN1SCC_Code use (Code_a => 1, Code_b => 5);` in the specification.
When the field's values differ from the representation values, the store is a
case expression over the local with one value per item
(`code.Value := (case P_code_tmp is when Code_a => adaasn1rtl.Asn1UInt(0), ...);`);
when the decoding cannot fail, store and call follow without `if result.Success`.

*Proof (Ada).* The local has the enumeration type of the field and is assigned
an item on every path of the field's decoding (every arm of the `case`, the
`when others` arm and the `else` part), so it holds an item at the store, and
`Enum_Rep` of an item is one of the values of the representation clause (or the
case expression yields one of its literals). Those values are the `when` labels
of the specialised decoder. The reference is written only by the store (its other
uses are arguments of procedures that do not write its `Value`), and the call runs
only after the store: inside the `if result.Success then` block that directly
follows the store's own `if result.Success then` block (the store does not change
`result`), or later in the same statement list when the store is not guarded. A
caller that passes its own reference parameter on is covered when every call of
it satisfies the same condition. The proof does not depend on what the reads can
return.

*Conditions (Ada)*, checked by `classifyStatements.py` on the generated code: the
statement is in the `when others` arm of `case P.Value is` with integer labels, P
an `AcnInsertedFieldRef` parameter of a decoder; every call of the decoder in the
unit's bodies passes a reference parameter of its caller (checked recursively) or
a local reference initialised with `(others => <>)` that is used only by stores of
the two forms above with values among the labels (read from the representation
clause in the unit's specifications, or the positions without one) and by calls of
procedures that do not write its `Value`; the local of the store is declared
without initializer and assigned on every path by the statements before the
store's statement in the same list; and the call follows the store as above. At
least one call must exist.


## ASN1SCC-COV-010: "contents read past the region" check after `--acn-v2` OCTET / BIT STRING CONTAINING contents that end with a 'size deduced' part (C, Ada)

**Code.** With `--acn-v2`, the contents of an OCTET STRING CONTAINING region
are decoded in place. The region decoder computes the end of the region from
its length (`acn_expected_end`, `acn_end_bit`), lets the contents see the stream
only up to that end, decodes them, and fails if they ended after it:

```c
pBitStrm->count = acn_expected_end + (acn_end_bit != 0);
/*the region ends at bit acn_end_bit of its last byte: 'size deduced' contents must not see the rest of it*/
pBitStrm->unusedTailBits = (8 - acn_end_bit) % 8;
ret = ASN1SCC_TLVList_ACN_Decode((&(pVal->payload)), pBitStrm, pErrCode);
pBitStrm->count = acn_saved_count;
pBitStrm->unusedTailBits = acn_saved_tail_bits;
if (ret && pBitStrm->currentByte * 8L + pBitStrm->currentBit > acn_expected_end * 8L + acn_end_bit) {
    /*the contents read past the end of the region (into the rest of its last byte)*/
    ret = FALSE; /*COVERAGE_IGNORE*/                         /* <- exempted */
    *pErrCode = ERR_ACN_DECODE_MYPDU2_PAYLOAD_2; /*COVERAGE_IGNORE*/   /* <- exempted */
}
```

```ada
bs.End_Bit_Pos := acn_expected_end;
ASN1SCC_TLVList_ACN_Decode_aux(val.payload, bs, result);
bs.End_Bit_Pos := acn_saved_end;
if result.Success and then bs.Current_Bit_Pos > acn_expected_end then
    result := adaasn1rtl.ASN1_RESULT'(Success => False, ErrorCode => ERR_ACN_DECODE_MYPDU2_PAYLOAD_2);   -- <- exempted
end if;
```

**Template**: `StgC/acn_c.stg`, macros `octet_string_containing_deferred_func`,
`_embedded_func` and `_wrapper` (decode); `StgAda/acn_a.stg`,
`octet_string_containing_deferred_region_decode` (used by `_func` and `_wrapper`),
`octet_string_containing_deferred_embedded_func` (decode), and for BIT STRING
regions `bit_string_containing_deferred_region_decode` (used by
`bit_string_containing_deferred_func` and `_wrapper`; Ada only: the C BIT STRING
region ends its view at a byte and sets no `unusedTailBits`, so its check is not
exempted). The compiler emits
the region when the decoding of the contents ends with a live 'size deduced' part
(`isLiveDeduced`; for the region around a CHOICE whose alternatives share one size
parameter, the contents of every alternative). Example `24-DEDUCED-SIZE/004`:
`TLVList ::= SEQUENCE (SIZE(1..100)) OF MyInt12` with `TLVList [size deduced]`, the
contents of `MyPDU2 { seqNr, payload OCTET STRING (CONTAINING TLVList) }`.

**Proof.** The region decoder sets the end of the stream to the end of the region:
C `count` and `unusedTailBits` (the view ends at bit `count * 8 - unusedTailBits` =
`acn_expected_end * 8 + acn_end_bit`), Ada `bs.End_Bit_Pos := acn_expected_end`. The
'size deduced' decoders (`sqf_deduced_fix_elem`, `sqf_deduced_var_elem`,
`oct_deduced`, `str_ascii_deduced`) compute their budget from that end minus the
compile-time size of the components after them (`nTrailingBits`):
`ded_avail_bits = (count * 8 - unusedTailBits - nTrailingBits) - position`
(Ada: `(bs.End_Bit_Pos - nTrailingBits) - bs.Current_Bit_Pos`). They fail when the
budget is negative, that is when what was decoded before them already passed it;
the fixed-size ones read `ded_count` elements with `ded_count * size <= ded_avail_bits`,
the variable-size one fails as soon as an element ends after the budget end. The
placement rules of `Docs/deduced-size-spec.md`, checked by the front end
(`CheckLongReferences.checkDeducedSizePlacement`), make this hold on every decoding
path of the contents: the deduced part sits only inside SEQUENCEs up to the region
(R1: not inside a CHOICE alternative or a SEQUENCE OF element unless behind another
CONTAINING) and is not optional (R3), so it is always decoded; every component after
it, at every level up to the region, is mandatory, of a fixed ACN size and without
`align-to-next` (R2), so the rest of the contents reads exactly `nTrailingBits` bits.
When the deduced decoder succeeds, the contents therefore end at or before the end of
the region, and the condition of the check is false.

**Conditions** (checked by `classifyStatements.py` on the generated code): the
statement is in the `if` block of the check above, inside a region decoder that sets
the deduced end of the stream (C: `pBitStrm->unusedTailBits = (8 - acn_end_bit) % 8;`,
Ada: `bs.End_Bit_Pos := acn_expected_end;`) before the contents and restores it after
them, and the contents (the code between, or a decoder it calls, followed through the
unit's sources) contain a 'size deduced' decoding that reads that end
(`ded_avail_bits` / `ded_budget_end` from `pBitStrm->count * 8L - pBitStrm->unusedTailBits`,
Ada `Ded_Avail_Bits` / `Ded_Budget_End` from `bs.End_Bit_Pos`). That the deduced part
is on every path is guaranteed by the front-end rules R1–R3 above, not checked again on
the generated text.

**Why the code stays.** Defense in depth for a decoder of untrusted input, and the
same templates decode contents without a 'size deduced' part, where the check is
reachable with a corrupted length (Ada; C when the region starts inside a byte).
For contents that always take the same whole number of octets C has no such check
(the read past the region fails first; coverage step 14e).


## ASN1SCC-COV-011: "region does not fit the stream" arm after an `--acn-v2` OCTET STRING CONTAINING length embedded in the encoding (Ada)

**Code.** With `--acn-v2`, an OCTET STRING CONTAINING region whose length is
embedded (no external size field) is decoded in place in Ada. The decoder reads
the length, checks that the region fits the rest of the stream, and fails with
`ERR_INSUFFICIENT_DATA` otherwise:

```ada
adaasn1rtl.encoding.uper.UPER_Dec_ConstraintWholeNumberInt(bs, acn_nCount, 1, 3, 2, result.Success);
if result.Success then
    if bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 and then
       acn_nCount >= 0 and then acn_nCount <= (bs.Size_In_Bytes * 8 - bs.Current_Bit_Pos) / 8
    then
        ...                                   -- decode the contents in place
    else
        result := adaasn1rtl.ASN1_RESULT'(Success => False, ErrorCode => adaasn1rtl.ERR_INSUFFICIENT_DATA);   -- <- exempted
    end if;
end if;
```

**Template**: `StgAda/acn_a.stg`, macro `octet_string_containing_deferred_embedded_func`
(decode); the compiler emits the region for every embedded-length region
(`SZ_EC_LENGTH_EMBEDDED`). Example `23-CONTAINING/007`: `MyPDU ::= OCTET STRING
(SIZE(1..3)) (CONTAINING MySeqType)`, the length in 2 bits.

**Proof.** (1) `UPER_Dec_ConstraintWholeNumberInt(bs, acn_nCount, min, max, ...)`
succeeds only with `acn_nCount` in `min .. max` (its postcondition), where `max` is
the SIZE maximum of the region, so after it `0 <= acn_nCount <= max`.
(2) The Ada decoder of a type assignment requires a stream that holds its maximum
encoding from the current position (`Pre => bs.Current_Bit_Pos +
<T>_REQUIRED_BITS_FOR_ACN_ENCODING <= bs.Size_In_Bytes * 8`); every decoding reached
from it, including the `--acn-v2` specialised functions it calls, reads at most the
maximum size of what it decodes ('size deduced' parts at most their SIZE maximum).
The maximum size of the type assignment includes the maximum of everything before
the region plus `max` octets for the region, so at the check at least `8 * max`
bits are left: `bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8` and `acn_nCount <=
(bs.Size_In_Bytes * 8 - bs.Current_Bit_Pos) / 8` both hold, and the `else` arm
cannot run.

**Conditions** (checked by `classifyStatements.py` on the generated code): the
statement is in the `else` arm of exactly that check, which directly follows
`if result.Success then` and the decoding of `acn_nCount` with
`UPER_Dec_ConstraintWholeNumberInt(bs, acn_nCount, min, max, ...)`, `0 <= min <=
max`. Part (2) rests on the decoder precondition and the maximum sizes computed by
the compiler, not on the generated text.

**Why the code stays.** Defense in depth: a caller that does not respect the
precondition (a stream shorter than the maximum, with run-time checks disabled)
still gets an error code instead of a read past the buffer. The C decoders work on
streams of the encoded length, where the same check is reachable (truncated
streams) and is not exempted.
