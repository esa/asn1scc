# Case 6 – ACN size determinant decoded without a bounds check (ESACERT #74995)

An OCTET STRING or BIT STRING whose length comes from an ACN determinant
(`data [size len]`) was decoded without checking `len` against the SIZE bound
when the ASN.1 constraint of the determinant already fitted inside that bound.
The ACN fixed-size integer decoders of the C (and Rust) runtime return every
value of the encoding width and do not check the ASN.1 constraint, so a 4-bit
`INTEGER (0..10)` determinant can carry 15. The C decoder then copied 15 bytes
(or bits) into a 10-element array. The constraint check of the decoded value
runs only after the copy.

The check was dropped in 4.7.0.2 (issue #360) and is restored in 4.9.11.0. It is
now omitted only when the encoding width itself bounds the determinant, e.g. an
8-bit `pos-int` determinant for `SIZE (0..255)`.

Ada was not affected: its ACN integer decoders reject values outside the ASN.1
constraint. Rust stopped with a panic (slice index out of range).

## Contents

- `a.asn`, `a.acn` – pos-int (4 and 32 bits), two's complement and parameter
  determinants for OCTET STRING and BIT STRING, plus the #360 case (`Full8`).
- `generated_tests.c` – decodes malformed streams (determinant above the SIZE
  bound, negative determinant) and expects failure with a non-zero error code;
  decodes valid streams and expects success.
- `reproduce_issue.sh` – generates C code (legacy, `-slim`, `--acn-v2`,
  `--acn-v2 -slim`) and runs the tests under AddressSanitizer/UBSan.

```bash
ASN1SCC=/path/to/asn1scc ./reproduce_issue.sh
```

A compiler older than 4.9.11.0 (from 4.7.0.2) stops with an AddressSanitizer
`stack-buffer-overflow` in `Oct4_ACN_Decode`.

The valid encodings are also covered by `v4Tests/test-cases/acn/06-OCTET-STRING/013.asn1`
and `v4Tests/test-cases/acn/08-BIT-STRING/009.asn1`.
