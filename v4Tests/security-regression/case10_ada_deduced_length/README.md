# Case 10 – Ada: 'size deduced' SEQUENCE OF with fewer elements than SIZE min

A SEQUENCE OF with `[size deduced]` takes its element count from the end of the
decoding view (`End_Bit_Pos`). For variable-size elements the Ada decoder
(`sqf_deduced_var_elem_decode`, legacy ACN and `--acn-v2`) decodes elements
while they fit and then stored the count in `Length` before it checked the
count against SIZE min. `Length` has the subtype `min .. max`, so a view that
holds fewer than `min` elements (a short or malformed message, or a CONTAINING
region whose length field was corrupted) made the decoder raise
`Constraint_Error` instead of returning a decoding error. With checks
suppressed (`-gnatp`) an out-of-range count was stored in `Length` before the
decoder failed. Decoders of fixed-size elements check the count first and were
not affected; the C decoders store the count in an `int`.

The count is now checked before it is stored.

`reproduce_issue.sh` generates the Ada decoders of `a.asn` / `a.acn` (legacy,
`--acn-v2`), builds `generated_tests.adb` with run-time checks and with `-gnatp`,
and decodes:

- `EList ::= SEQUENCE (SIZE(1..20)) OF Elem` from views of 0 .. 8 bits (an
  element needs at least 9);
- `Box`, whose `SEQUENCE (SIZE(2..5)) OF Elem` sees one element;
- a valid encoding of two elements (must decode).

Every short view must fail with a non-zero error code and no exception. Run it
with `ASN1SCC=<compiler> ./reproduce_issue.sh`; a compiler without the fix
fails the legacy and `--acn-v2` legs with run-time checks.
