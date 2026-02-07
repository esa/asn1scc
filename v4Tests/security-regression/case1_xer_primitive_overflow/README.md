# Case 1 – XER Primitive Decoder Buffer Overflow (Issue #367)

This directory contains a security regression test for a buffer overflow that previously existed in the XER primitive decoder.

The issue affected `Xer_DecodePrimitiveElement()`, which copied decoded XML content into a fixed-size buffer without bounds checking. A malicious XER/XML input with oversized element content could trigger a buffer overflow and crash the decoder.

This test verifies that the decoder now **fails safely** (returns an error) instead of overflowing memory.

## Contents

- `a.asn`  
  Minimal ASN.1 grammar used to generate a XER decoder.

- `malicious.xml`  
  Crafted XER input with oversized element content intended to trigger the overflow.

- `reproduce_issue.sh`  
  Script that:
  1. Runs `asn1scc` with XER support
  2. Builds the generated code
  3. Invokes the decoder on `malicious.xml`

- `SECURITY_ISSUE_1_XER_BUFFER_OVERFLOW.md`  
  Original security report and proposed fix.

## How to run

From this directory:

```bash
./reproduce_issue.sh
```
This will execute the test and demonstrate that the decoder now handles the malicious input without crashing, confirming that the buffer overflow has been mitigated.