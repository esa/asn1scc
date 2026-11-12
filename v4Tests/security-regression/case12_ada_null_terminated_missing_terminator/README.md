# Case 12 – Ada: null-terminated ASCII string / integer without the terminator

An ACN `IA5String` or `INTEGER` with `[encoding ASCII, size null-terminated]`
ends with a terminator (`termination-pattern`, default `00`H). The Ada decoders
(`Acn_Dec_String_Ascii_Null_Terminated`,
`Acn_Dec_UInt_ASCII_VarSize_NullTerminated` and the signed variant that calls
it; legacy ACN and `--acn-v2`) read at most the longest value (`SIZE` max
characters, 20 digits) and then returned success without checking that the
bytes after it are the terminator. A stream without the terminator was
accepted and the bytes after the value were consumed, so a following field was
decoded from the wrong position. The C decoders reject such a stream. No
memory outside the stream is read or written (the decoder precondition covers
every byte).

The decoders now check the terminator after the longest value.

`reproduce_issue.sh` generates the decoders of `a.asn` / `a.acn` (legacy,
`--acn-v2`), builds `generated_tests.adb` with run-time checks and with
`-gnatp` and `generated_tests.c`, and decodes valid encodings (must succeed)
and the longest value followed by bytes that are not the terminator (must fail
with a non-zero error code). Every line must be `PASS`.
