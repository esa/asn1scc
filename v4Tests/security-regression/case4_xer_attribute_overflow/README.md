# Case 4 – XER `AddAttribute()` Unbounded Write (ESACERT #74508)

This directory contains a security regression test for an out-of-bounds write that
previously existed in the XER attribute decoder of the C runtime (`asn1crt`).

`Xer_DecodeAttributes()` looped over the attributes of an XML start tag without any
cap, and `AddAttribute()` protected the fixed-capacity `XmlAttributeArray` (20 entries,
`Name[50]`, `Value[100]`) only with an `assert()`. Release builds compiled with
`-DNDEBUG` remove the assert, so a crafted document could write attacker-controlled
bytes past the end of the array. In addition, attribute names longer than 49 characters
overflowed `Name[50]` into the adjacent `Value` field even when the array was not full.

This test verifies that the decoder now **fails safely** (returns `FALSE` with
`ERR_INVALID_XML_FILE`) instead of overflowing memory or dereferencing a NULL array.

## Reachable behaviour before the fix

- **Generated decoders** (`<Type>_XER_Decode`) always pass a `NULL` attribute array to
  `Xer_DecodeComplexElementStart()`. Any attribute on a complex element therefore made
  `AddAttribute()` dereference `NULL` (crash / denial of service).
- **Callers that provide their own `XmlAttributeArray`** (for example hand-written code
  calling `Xer_DecodeAttributes()`, the scenario of the original report) got a linear
  overflow past the array once more than 20 attributes were supplied.

## Contents

- `a.asn`
  Minimal ASN.1 grammar used to generate a XER decoder.

- `reproduce_issue.sh`
  Script that:
  1. Runs `asn1scc -XER -c` on `a.asn`
  2. Builds a runner against the generated code with `-DNDEBUG` (release-build emulation)
     and AddressSanitizer when available
  3. Executes eight sub-tests, each in its own process: three through the generated
     `PDU_XER_Decode()` (valid document, one attribute, forty attributes) and five through
     `Xer_DecodeAttributes()` directly (forty attributes, a 90-character attribute name,
     exactly twenty attributes, three attributes with content check, `NULL` array)

- `SECURITY_ISSUE_4_XER_ATTRIBUTE_OVERFLOW.md`
  Summary of the report and of the fix.

## How to run

From this directory (`asn1scc` must be on `PATH`, or set `ASN1SCC=/path/to/asn1scc`):

```bash
./reproduce_issue.sh
```

The script exits with `0` when every sub-test passes. Against the vulnerable runtime the
sub-tests with attributes crash under AddressSanitizer (`SEGV` in `AddAttribute`) and the
long-name sub-test is silently accepted.
