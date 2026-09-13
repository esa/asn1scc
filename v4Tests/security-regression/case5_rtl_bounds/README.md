# Case 5 – C runtime bounds and decoder error propagation (internal review, September 2026)

This directory contains the regression tests for the memory-safety review of the C
runtime (`asn1crt`) carried out after ESACERT #74508. The review found, among others, two
overflows reachable from generated XER decoders (top-level `IA5String` capacity taken from
`sizeof` of a pointer parameter, `BIT STRING` capacity passed in bits instead of bytes),
one-byte over-reads and over-writes at the end of the buffer in the bitstream primitives,
an infinite loop when encoding an infinite REAL in XER, and several decode failures that
were reported as success. The full list and the fixes are in
`SECURITY_ISSUE_5_RTL_BOUNDS.md`.

The tests verify that the runtime now **fails closed** (returns `FALSE`, leaves the
destination within bounds) and that well-formed input still decodes (positive controls).

## Contents

- `a.asn`, `a.acn`
  Small grammar covering BIT STRING, OCTET STRING, BOOLEAN, INTEGER (constrained and
  unconstrained), REAL, OBJECT IDENTIFIER, a null-terminated ACN `IA5String`, ENUMERATED,
  fixed-size SEQUENCE OF and CHOICE.

- `runtime_tests.c`
  15 groups of direct runtime calls, one group per process: `bits`, `byte-arrays`,
  `octet-status`, `signed`, `ascii`, `bcd`, `narrowing`, `acn-string`, `xer-bits`,
  `xer-octets`, `xer-scalars`, `xer-output`, `oid`, `real`, `ber`.

- `streaming_tests.c`
  Octet-string and byte-array transfers across 2-byte chunks with `-DASN1SCC_STREAMING`.

- `generated_tests.c`
  Tests through the decoders generated from `a.asn`/`a.acn`: `xer-bits`, `xer-scalars`,
  `binary-bounds`, `acn-string`, `xer-real`.

- `reproduce_issue.sh`
  Script that:
  1. Builds `runtime_tests.c` and `streaming_tests.c` against `../../../asn1crt` for
     `WORD_SIZE`/`FP_WORD_SIZE` 8 and 4, with and without `-DNDEBUG`, always with
     AddressSanitizer and UndefinedBehaviorSanitizer (`-fno-sanitize-recover=all`), and runs
     every group in its own process under a 20 s timeout
  2. Runs `asn1scc -c -uPER -ACN -XER -atc` on `a.asn a.acn` in legacy, `-slim` and
     `--acn-v2` mode and runs `generated_tests.c` against each output with `-DNDEBUG` and
     the sanitizers

- `SECURITY_ISSUE_5_RTL_BOUNDS.md`
  Summary of the findings and of the fixes.

## How to run

From this directory (`asn1scc` must be on `PATH`, or set `ASN1SCC=/path/to/asn1scc`; GCC
with AddressSanitizer/UndefinedBehaviorSanitizer is required, the script does not fall
back to an unsanitized build):

```bash
./reproduce_issue.sh                 # everything
./reproduce_issue.sh --direct-only   # runtime and streaming tests only, no code generation
RTL_DIR=/path/to/old/asn1crt ./reproduce_issue.sh --direct-only   # run against another runtime
```

The script exits with `0` when every sub-test passes. Against the runtime of commit
`84ea20f3` every group fails, seven of them with a sanitizer report. Build artifacts are
kept in the ignored `build/` directory.
