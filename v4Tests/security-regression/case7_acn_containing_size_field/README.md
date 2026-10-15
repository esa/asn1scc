# Case 7 – CONTAINING length used before its bounds check

An OCTET STRING or BIT STRING that contains another type and takes its length
from an ACN field (`payload OCTET STRING (CONTAINING X)` with `payload [size len]`)
is decoded through a static temporary buffer as large as the largest encoding
of `X`. The C decoder (legacy ACN, also `-slim`) initialised the temporary
stream with the length read from the stream (`BitStream_Init(&bitStrm, arr, len)`,
which zero-fills `len` bytes) and only then compared the length with the
buffer size. A length above the buffer size wrote zeros past the static
buffer; a length that is negative after the cast to `int` (two's complement
determinant, or a 32-bit value of 2^31 and above) made the zero-fill size huge.

The bounds check now comes first and the length is compared as an unsigned
value, so a negative length fails too.

The defect was introduced in 4.8.0.0 with 'size deduced' support (the stream
view of the contents was limited to the region length); before, the temporary
stream was initialised with the buffer size. `--acn-v2` decodes the contents in
place after checking that the stream holds the region and was not affected.
Ada decodes into a fixed-size temporary stream and was not affected.

## Contents

- `a.asn`, `a.acn` – 16-bit, 32-bit and two's complement length fields for an
  OCTET STRING containing a fixed-size type, an OCTET STRING with 'size
  deduced' contents, and a BIT STRING with a length in bits.
- `generated_tests.c` – decodes malformed streams (length above the buffer size,
  negative after a cast to `int`) and expects failure with a non-zero error
  code; decodes valid streams and expects success.
- `reproduce_issue.sh` – generates C code (legacy, `-slim`, `--acn-v2`,
  `--acn-v2 -slim`) and runs the tests under AddressSanitizer/UBSan.

```bash
ASN1SCC=/path/to/asn1scc ./reproduce_issue.sh
```

A compiler from 4.8.0.0 up to and including 4.9.18.0 stops with an
AddressSanitizer `global-buffer-overflow` in `memset` (`BitStream_Init`) called
from `OctLen16_ACN_Decode`.
