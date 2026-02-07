# Case 3 – ACN Null-Terminated String Buffer Overflow (Issue #367)

This directory documents a previously identified buffer overflow in ACN null-terminated string decoders.

The issue involved the use of a fixed-size temporary buffer combined with an unclamped termination-pattern size, which could lead to a stack buffer overflow when the pattern exceeded the allowed length.

Unlike the XER-related issues, this case **can be exercised using the standard test framework** and is covered by an automated test case.

## Test coverage

The issue is covered by the following test case:

- `v4Tests/test-cases/acn/03-IA5String/020.asn1`

This test verifies that termination patterns exceeding the supported size are handled safely and do not result in memory corruption.

## Contents

- `SECURITY_ISSUE_3_ACN_NULL_TERM_OVERFLOW.md`  
  Original security report and proposed fix.

## Notes

This directory exists for documentation and traceability purposes and does not contain standalone test scripts.
