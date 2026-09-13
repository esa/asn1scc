# Security Issue: C Runtime Bounds and Decoder Error Propagation

Found by an internal memory-safety review of the C runtime (`asn1crt/`), September 2026,
after ESACERT tickets #71109 and #74508 had shown that fixed-size buffers guarded only by
`assert()` or by nothing existed in the runtime. Not externally reported.

## Summary

The review traced every runtime function from the C templates in `StgC/` to find out what
generated code passes for each size, length and pointer argument, and then fed malformed
input to the runtime under AddressSanitizer/UndefinedBehaviorSanitizer with `-DNDEBUG`.
It found two memory-safety defects reachable from generated XER decoders that had not been
reported before, a family of end-of-buffer over-reads and over-writes in the bitstream
primitives, an infinite loop in the XER REAL encoder, several places where a failed or
partial decode was reported as success, and undefined signed-integer arithmetic on extreme
values. The BER decoders had the same class of defects but are not called by any generated
code.

## Location

- **Files:** `asn1crt/asn1crt.c`, `asn1crt_encoding.c`, `asn1crt_encoding_uper.c`,
  `asn1crt_encoding_acn.c`, `asn1crt_encoding_xer.c`, `asn1crt_encoding_ber.c`
- **Templates:** `StgC/xer_c.stg` (`String_decode`, `BitString_decode`, `Enumerated_decode`,
  `CHOICE_decode`, `SequenceOf_decode`), `BackendAst/DAstXer.fs`

## Findings reachable from generated code

| Function / template | Defect | Effect with malformed input |
| --- | --- | --- |
| `xer_c.stg` `String_decode` | Passed `sizeof(<p>)` as the capacity. For a top-level string type `p` is the array parameter, which decays to `char*`, so the capacity was 8 regardless of the declared size. | Stack/heap overflow of `char[max+1]` for any `IA5String` with `max < 7` decoded as a top-level type (`<T>_XER_Decode`). Fields inside SEQUENCE/CHOICE were not affected. |
| `xer_c.stg` `BitString_decode`, `Xer_DecodeBitString()` | Passed the maximum size in **bits** where the runtime expects **bytes**; the runtime used it both for `memset` and as the loop bound. | Overflow of `arr[]` by up to 8×, e.g. `BIT STRING (SIZE(8))` fed 16 bits writes 2 bytes into a 1-byte array. |
| `BitStream_ReadBit()`, `BitStream_PeekBit()` | No bounds check. | One-byte over-read when a BOOLEAN or any single bit is decoded at the end of a buffer shorter than the encoding (uPER and ACN). |
| `BitStream_ReadByte/ReadByteArray/ReadPartialByte/AppendByte0/AppendByteArray()` | End-of-buffer edge cases (`currentBit != 0` with one byte left, negative lengths). | One-byte over-read or over-write. |
| `Acn_Dec_String_Ascii_Null_Terminated()`, `_mult()` | Loop `while (i <= max)` stored `strVal[max]` and then wrote the terminator at `strVal[max+1]`. | One-byte write past `char[max+1]` and a string without NUL terminator when the input is longer than the declared maximum. |
| `ByteStream_AppendString()`, `Xer_EncodeXmlHeader()` | `strcat` onto `buf[currentByte]` assumed a NUL byte there; header `strcpy` unbounded. | Out-of-bounds write when the caller attached a buffer with `ByteStream_AttachBuffer` (no zeroing). |
| `Double2String()` (XER REAL encoder) | `while (fabs(v) >= 10) v /= 10;` never terminates for ±INF. | Encoding an infinite REAL hangs the process. |
| `xer_c.stg` `Enumerated_decode` | Unknown label set `*pErrCode` but left `ret` TRUE. | Decode succeeded with an uninitialised enumeration value. |
| `xer_c.stg` `CHOICE_decode` | End-tag check ran unconditionally after the alternative. | A failed alternative was overwritten with TRUE when the end tag followed. |
| `xer_c.stg` `SequenceOf_decode` (fixed size) | Element count not checked in `ret`. | Fewer elements than the fixed size decoded as success with stale trailing elements. |
| `Xer_DecodeInteger/PosInteger/Real/ObjectIdentifier()` | `atoll`/`strtoull`/`atof` without validation; the OID parser tokenised the wrong buffer and never set `nCount`. | Junk, empty text, overflow and negative unsigned values accepted; OBJECT IDENTIFIER decode returned garbage. |
| uPER/ACN integer and REAL arithmetic | `-v - 1` for `MIN_INT`, `max - min` for full-range constraints, left shifts of negative accumulators. | Undefined behaviour on extreme values (UBSan reports). |
| ACN ASCII/BCD decoders, `-slim` narrowing wrappers | Digits not validated, accumulation overflow; wrappers truncated to the narrow type silently. | Wrong values accepted, e.g. 256 decoded into a `uint8_t` field as 0 and passing the range check. |
| `BitStream_DecodeOctetString()` | Ignored the result of the payload copy. | Success with a partially filled array on a short buffer. |
| `ASN1SCC_STREAMING` paths | Octet-string copies never advanced the source/destination pointer across chunks; `ReadByteArray` kept a raw pointer across a refill. | Wrong data in streaming mode. |
| uPER OBJECT IDENTIFIER, REAL | Subidentifier and length overflows, truncated subidentifier accepted, exponent-length form `expLen == 4` misparsed, unbounded mantissa loop. | Wrong values; long loops. |

## Findings reachable only through direct calls of the runtime API

- BER (`asn1crt_encoding_ber.c`): `ByteStream_PutByte/GetByte` checked `currentByte > count`
  instead of `>=` (one-byte over-read/over-write at the end of every BER buffer);
  `BerDecodeLength` overflow; `BerDecodeInteger`, `BerDecodeIA5String`, `BerDecodeBitString`,
  `BerDecodeOctetString` did not check the decoded length against the destination. No C
  template or backend module calls the BER runtime.
- NULL, zero and negative argument guards in `Xer_DecodePrimitiveElement`, `uint2int`,
  `BitStream_DecodeNonNegativeInteger` and the octet/byte primitives.

## Prerequisites

1. Application generated by asn1scc and built with the C runtime.
2. Application decodes data from an untrusted source; the XER findings need `-XER`, the
   bitstream findings need a buffer shorter than the encoding to be attached (the `-atc`
   test harness attaches `REQUIRED_BYTES` and does not show them).
3. The `Double2String` hang needs an infinite REAL value on the encode side.

The Ada and Scala runtimes were not reviewed and are not changed.

## Fix

Every read and write is bounded against the attached buffer and every length, count and
digit decoded from the stream is validated before use; a function that cannot complete
returns FALSE and leaves the destination unmodified where possible. Specifically:

- XER templates pass the declared maximum length (`<nMaxLength> + 1`, a new template
  parameter filled from the grammar) and `sizeof(arr)` for BIT STRING; constraint checks run
  only after a successful primitive decode; `ret` is cleared on unknown enumeration labels,
  failed CHOICE alternatives and short fixed-size SEQUENCE OF; the generated decoder returns
  a non-zero error code on every failure.
- Bitstream primitives check `currentByte`, `currentBit`, remaining bytes and argument
  ranges up front; signed values are accumulated unsigned and converted with `uint2int()`.
- XER text is parsed with `strtoimax`/`strtoumax`/`strtod`, end-pointer and range checks;
  the OBJECT IDENTIFIER parser was rewritten; `Double2String` handles ±INF/NaN and 0
  explicitly.
- ACN string decoders fail when the input exceeds the declared maximum instead of clamping
  or overrunning; ASCII/BCD decoders validate digits and detect overflow; the `-slim`
  wrappers fail when the value does not fit the narrow type.
- uPER OBJECT IDENTIFIER and REAL decoders validate lengths, terminate correctly and bound
  the exponent/mantissa; encoders return without output for invalid values.
- BER primitives use `>=`, bound lengths against the destination and require terminator
  space for IA5String.

No public runtime signature changes. `Xer_DecodeReal`, `BerEncodeReal` and `BerDecodeReal`
now take `asn1Real*` as their headers already declared.

## Behaviour changes

- OBJECT IDENTIFIER validity follows X.660: the second arc is not limited to 39 when the
  first arc is 2, and the uPER decoder splits the first subidentifier accordingly. The Ada
  runtime still applies the old rule.
- XER integer, real and OID text is parsed strictly: trailing junk, empty content, negative
  unsigned values, odd hexadecimal digit counts and non-binary BIT STRING characters are
  decode errors.
- XER REAL output prints values with `|v| < 1e-17` instead of `0`, and `INF`/`-INF`/`NaN`
  for the special values.
- An ACN external size determinant larger than the declared maximum fails the decode
  instead of being clamped; BCD digits above 9 are errors and only `0xF` terminates a
  null-terminated BCD field; termination patterns longer than 10 bytes are rejected.
- Hand-written BER callers must include terminator space in `maxLength`.

## Testing

`reproduce_issue.sh` in this directory builds everything with `-DNDEBUG`, AddressSanitizer
and UndefinedBehaviorSanitizer (`-fno-sanitize-recover=all`), each sub-test in its own
process with a positive control:

- `runtime_tests.c`: 15 groups of direct runtime calls (bit and byte primitives, octet
  string status, signed arithmetic, ASCII/BCD, `-slim` narrowing, ACN strings, XER bit and
  octet strings, XER scalars and output, OBJECT IDENTIFIER, REAL, BER), built for
  `WORD_SIZE`/`FP_WORD_SIZE` 8 and 4, with and without `NDEBUG`.
- `streaming_tests.c`: octet-string and byte-array transfers across 2-byte chunks with
  `ASN1SCC_STREAMING`.
- `generated_tests.c`: real decoders generated from `a.asn`/`a.acn` with `-uPER -ACN -XER`
  in legacy, `-slim` and `--acn-v2` mode (XER bit string, XER scalars incl. enumerated,
  fixed-size SEQUENCE OF and CHOICE, short binary buffers, ACN null-terminated string, XER
  REAL infinity round trip).

Against the runtime of commit `84ea20f3` every group fails: seven with a sanitizer report
(`bits`, `byte-arrays`, `acn-string`, `xer-bits`, `xer-output`, `ber`: stack-buffer-overflow;
`signed`, `ascii`, `real`: signed overflow / negation / shift), the rest with a wrong result.
