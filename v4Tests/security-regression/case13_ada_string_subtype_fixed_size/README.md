# Case 13 – Ada: fixed-size IA5String subtype with ACN 'encoding ASCII' encoded with its typedef's length

A string subtype such as `Sub ::= Str (SIZE (3))` of `Str ::= IA5String
(SIZE (1 .. 10))` shares the typedef of its base type: a `String (1 .. 11)` in
Ada. With ACN `encoding ASCII` and a fixed size, the Ada encoder passed the
whole array to `Acn_Enc_String_Ascii_FixSize`, which writes
`strVal'Length - 1` characters: 10 characters into a stream sized for the 3 of
the subtype. With run-time checks this raised `Constraint_Error`; with checks
suppressed (`-gnatp`) the encoder wrote past the end of the stream buffer. The
decoder read 10 characters as well. Legacy ACN and `--acn-v2`; C and uPER are
not affected.

The Ada calls now take only the subtype's characters plus the terminating
position (also the embedded-length, null-terminated and external-size ASCII
calls, whose preconditions could not be proved for such subtypes).

`reproduce_issue.sh` generates the Ada codecs of `a.asn` / `a.acn` (legacy,
`--acn-v2`), builds `generated_tests.adb` with run-time checks and with
`-gnatp`, encodes "ABC" into a stream followed by a guard and decodes it. Every
line must be `PASS`.
