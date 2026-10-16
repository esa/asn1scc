# Case 8 – Ada CONTAINING region shorter than its contents

An OCTET STRING that contains another type (`payload OCTET STRING (CONTAINING X)`)
is decoded from a region whose length is on the wire: an ACN size field
(`payload [size len]`) or the uPER length of the default encoding. The Ada
decoders (legacy ACN, also `-slim`, and `--acn-v2`) copied the region into a
temporary stream exactly as long as the region and decoded the contents from it.
The contents decoder has the precondition that the stream holds its largest
encoding, and the run-time library reads under that precondition without
further checks. A region shorter than the contents made the decoder read past
the temporary stream:

- with run-time checks, `Constraint_Error` (index or range check) instead of a
  decoding error;
- with checks suppressed (`-gnatp`), a read past the stream object. Where the
  contents hold a byte-aligned OCTET STRING with a length field, the decoder
  copied that many bytes from beyond the region into the decoded value and
  returned success.

The temporary stream is again as large as the largest contents encoding (zero
after the region), as before 4.8.0.0, and the decoder fails with the type's
error code when the contents read past the region. Contents with 'size deduced'
must see the end of their region and keep a stream as long as the region; for
them, and as a second line of defence, the read primitives of the run-time
library never read outside the stream buffer (they return zero bits there),
and the aligned OCTET STRING copy fails instead of copying past it.

The defect was introduced in 4.8.0.0 with 'size deduced' support. The C
decoders read through bounds-checked primitives and are not affected by this
case (see case 7 for the C defect of the same origin).

## Contents

- `a.asn`, `a.acn` – OCTET STRING CONTAINING with an ACN size field and with an
  embedded uPER length, each with contents that hold a length-prefixed OCTET
  STRING and with 'size deduced' contents.
- `generated_tests.adb` – decodes regions that are shorter than their contents
  and expects failure with a non-zero error code (no exception); decodes valid
  streams and expects success.
- `reproduce_issue.sh` – generates Ada code (legacy, `-slim`, `--acn-v2`,
  `--acn-v2 -slim`), builds the tests with run-time checks and with `-gnatp`,
  and runs them; exits non-zero on any failure.

```bash
ASN1SCC=/path/to/asn1scc ./reproduce_issue.sh
```

A compiler from 4.8.0.0 up to and including 4.9.19.0 fails: `CONSTRAINT_ERROR`
in `adaasn1rtl-encoding-uper.adb` / `adaasn1rtl-encoding.adb` with checks, and
short regions decoded with `Success=TRUE` with `-gnatp`.
