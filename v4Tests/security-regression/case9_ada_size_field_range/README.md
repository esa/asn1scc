# Case 9 – Ada: ACN size field outside 0 .. Integer'Last

A field whose length comes from an ACN size field (`v [size len]`) is decoded
with the value of that field. When the field is a twos-complement integer, the
decoded value may be negative; when it is a pos-int integer of 32 or 64 bits, it
may exceed `Integer'Last`. The Ada decoders (legacy ACN, also `-slim`, and
`--acn-v2`) converted the value to `Integer` / `Natural` before checking it, or
checked only its upper bound:

- OCTET STRING, BIT STRING, SEQUENCE OF: only `len <= max` was checked, so a
  negative length passed;
- IA5String: the value was converted to `Asn1Int`, then to `Integer` in the
  run-time library, before any check;
- legacy OCTET / BIT STRING (CONTAINING): the check compared `Integer (len)`,
  and the OCTET STRING decoder then cleared
  `tmpBs.Buffer (Integer (len) + 1 .. Last)`;
- `--acn-v2` BIT STRING (CONTAINING): the size of the region was not checked;
- `--acn-v2` OCTET STRING (CONTAINING) with 'size deduced' contents: the value
  was converted to `Natural` before the check.

With run-time checks the decoders raised `Constraint_Error` instead of returning
a decoding error. With checks suppressed (`-gnatp`) most of them returned
success with an invalid length, and the legacy OCTET STRING (CONTAINING) decoder
wrote zeros to memory below its temporary stream.

The size is now checked in the type of the size field before it is converted
(`len in 0 .. max`, also the lower bound for a signed field), and the
`--acn-v2` BIT STRING (CONTAINING) region is checked against the rest of the
stream. The C decoders fail on such values through their run-time library and
the final validation; they now also check the lower bound of a signed size
field.

## Contents

- `a.asn`, `a.acn` – one type per kind of sized field, with a twos-complement
  16-bit size field or a pos-int 32 / 64-bit size field.
- `generated_tests.adb` – decodes streams whose size field is -16, -32768,
  `2**31`, `16#FFFF_FFF0#`, `2**63` or `16#FFFF_FFFF_FFFF_FFF0#` and expects
  failure with a non-zero error code (no exception); round-trips the initial
  value of each type and expects success.
- `reproduce_issue.sh` – generates Ada code (legacy, `-slim`, `--acn-v2`,
  `--acn-v2 -slim`), builds the tests with run-time checks and with `-gnatp`,
  and runs them; exits non-zero on any failure.

```bash
ASN1SCC=/path/to/asn1scc ./reproduce_issue.sh
```

A compiler before the fix fails every leg: exceptions with run-time checks,
`Success=TRUE` decodes and `STORAGE_ERROR` with `-gnatp`.
