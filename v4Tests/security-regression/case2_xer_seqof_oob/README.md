# Case 2 – XER SEQUENCE OF Out-of-Bounds Write (Issue #367)

This directory contains a security regression test for an out-of-bounds write that previously existed in the XER `SEQUENCE OF` decoder.

The generated decode loop wrote elements into a fixed-size array without checking that the current index remained within the maximum allowed size. The bounds check occurred only after the loop completed, allowing malformed XER input with too many elements to write past the end of the array.

This test verifies that the decoder now **detects the condition early and fails safely**.

## Contents

- `a.asn`  
  Minimal ASN.1 grammar containing a `SEQUENCE OF` with a size constraint.

- `reproduce_issue.sh`  
  Script that:
  1. Runs `asn1scc` with XER support
  2. Builds the generated code
  3. Generates a malicious XER/XML input with more elements than allowed
  4. Invokes the decoder on that input

- `SECURITY_ISSUE_2_XER_SEQUENCEOF_OOB.md`  
  Original security report and proposed fix.

## How to run

From this directory:

```bash
./reproduce_issue.sh
```
This will execute the test and print the results, demonstrating that the decoder correctly identifies the out-of-bounds condition and fails gracefully without crashing or writing past the end of the array.