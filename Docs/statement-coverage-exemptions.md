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
- **In the generated code** (planned): the templates will emit GNATcoverage
  exemption regions that cite the identifier, so that a user who measures the
  generated code with GNATcoverage gets the exemption and its justification in
  the tool's own report:

  ```c
  /* GNATCOV_EXEMPT_ON "ASN1SCC-COV-001: index range enforced by the RTL, see Docs/statement-coverage-exemptions.md" */
  ...
  /* GNATCOV_EXEMPT_OFF */
  ```

  In Ada the equivalent is `pragma Annotate (Xcov, Exempt_On, "...")` /
  `pragma Annotate (Xcov, Exempt_Off)`. No Ada entry exists so far (see the
  notes of each entry).

An exemption region records a claim; GNATcoverage does not check it. It reports
the region as "exempted, with violations" while the code stays unexecuted and
as "exempted, no violation" if a test ever executes it, which would mean the
proof no longer holds.

---

## ASN1SCC-COV-001: default arm after a range-checked index decode (C)

**Code.** A decoder reads an index or a value with
`BitStream_DecodeConstraintWholeNumber(pBitStrm, &v, min, max)` or
`BitStream_DecodeConstraintPosWholeNumber(pBitStrm, &v, min, max)`, then
switches on `v`. The switch has one `case` for every value `min..max`, plus a
`default:` arm that reports an error. The `default:` arm cannot execute.

**Templates** (`StgC/`):

| template | macro | decoded value |
|---|---|---|
| `uper_c.stg` | `Enumerated_decode` | uPER index of an ENUMERATED item |
| `uper_c.stg` | `choice_decode` | uPER index of a CHOICE alternative |
| `acn_c.stg` | `Choice_decode` | ACN CHOICE index (no determinant) |
| `acn_c.stg` | `EnumeratedEncValues_decode` | ACN ENUMERATED value, **only** when the value is decoded with one of the two functions above and the item values fill `min..max` |

For `EnumeratedEncValues_decode` the proof depends on the instance: an ACN
ENUMERATED whose values leave gaps (e.g. `{a(0), b(5)}` in 3 bits), or whose
integer is decoded with a fixed-size function such as
`Acn_Dec_Int_PositiveInteger_ConstSize_8`, can deliver a value without a
`case`. That `default:` arm is reachable with an invalid message and needs a
test, not an exemption.

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

**Ada.** The Ada templates have no such arm: the decoded integer is converted
to the index subtype (`case <index_range>(intVal) is`), the `case` must cover
the whole subtype, and the postcondition of
`UPER_Dec_ConstraintWholeNumber` (`Result and IntVal in MinVal .. MaxVal`,
`ADA_RTL2/src/adaasn1rtl-encoding-uper.ads`) lets GNATprove show that the
conversion cannot fail.

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
