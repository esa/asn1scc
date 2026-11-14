# Case 14 – Ada uPER: decoders of a fragmented length

The Ada RTL procedures `BitStream_DecodeBitString_fragmentation` and
`BitStream_DecodeOctetString_fragmentation` (`adaasn1rtl-encoding-uper.adb`)
decode the general length of X.691 10.9.3.8 (fragments C1 .. C4, then a one- or
two-octet length). The generated uPER code calls them for
`BIT STRING (CONTAINING X)` / `OCTET STRING (CONTAINING X)` when X may exceed
64K bits / octets. The BIT STRING decoder did not compare a fragment with the
maximum and the size of the target buffer before reading it, and computed the
last part's slice one byte short when that part was not a multiple of 8 bits.
Both decoders read C0 and C5 .. FF as a two-octet length, and when the last part
exceeded the maximum they skipped it and still returned success. With run-time
checks the BIT STRING decoder raised `Constraint_Error` (also for a valid value
of 72014 bits); with checks suppressed (`-gnatp`) a fragment above the maximum
was written past the target buffer. The C and Rust runtimes check every part.

Every part is now checked against the maximum, the target buffer and the rest of
the stream before it is read; invalid headers and an oversized last part fail.

`reproduce_issue.sh` generates the Ada uPER code of `a.asn`, builds
`generated_tests.adb` with run-time checks and with `-gnatp`, and runs:

- round trips of valid values (BIT STRING contents of 72014 bits, OCTET STRING
  contents of 70003 octets);
- decodes of lengths above the maximum, and of the headers C0 / C5.

Every malformed length must fail with a non-zero error code and no exception.
Run it with `ASN1SCC=<compiler> ./reproduce_issue.sh`; a compiler without the
fix fails 7 of the 9 checks with run-time checks.
