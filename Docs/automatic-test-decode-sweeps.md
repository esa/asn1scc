# Decode fault sweeps in the automatic tests

Since version 4.9.33.0 the automatic tests (`-atc`) of the C and Ada backends also check how the uPER and
ACN decoders handle damaged input. Each automatic test encodes a value, decodes it, validates the result
and compares it with the original. When this round trip succeeds, the test now decodes damaged copies of
the same encoding:

| sweep | what is decoded | expected result | C | Ada |
|---|---|---|---|---|
| truncation | the first `k` bytes of the encoding, for `k = 0, 1, ...`, from a buffer of exactly `k` bytes | the decoder fails with a nonzero error code | yes | no |
| corruption | the whole encoding with byte `i` set to `0x00`, then to `0xFF` (restored after each decode) | the decoder fails with a nonzero error code, or succeeds with a value that passes `IsConstraintValid`; it must not crash or raise an exception | yes | yes |

The sweeps know nothing about the structure of the type. A damaged byte always belongs to some field, so
the decoder of that field fails, however deep it is nested, and the failure travels back through every
enclosing decoder. This exercises the error paths of the generated decoders, which a round trip of valid
values never reaches.

Ada runs only the corruption sweep. Every generated Ada decoder has a precondition that the stream holds
enough bytes for the largest encoding of its type, so a truncated stream is outside the decoder's contract.
PDUs that contain an ACN `size deduced` field are swept in C only, and a truncated copy of such a PDU may
also decode successfully (to a valid value with fewer elements).

## Failures

A test that violates one of these rules fails like any other automatic test. The output names the
position:

    Msg_ACN_enc_dec: decoding the first 3 of 5 bytes returned TRUE with error code 0
    Test case 'test_case_ACN_000002' failed. A truncated copy of the encoded value was decoded without an error code.

In C the test returns error code 6 (truncation) or 7 (corruption); in Ada the test fails in the decoding
step (`TC_DECODE`) with error code 7, after the line printed by the sweep. A failure means that a decoder accepted input it should have rejected, rejected it
without setting an error code, or crashed. Please report it.

## Cost and the budget

For an encoding of `L` bytes a full sweep needs `L` truncated and up to `2 L` corrupted decodes. To keep
run times low for large values, each sweep tries at most 128 positions at the start and 128 at the end of
the encoding, and stops after the decoders have read about 1 MB in total (512 KB per end). At least two
positions are always tried at each end. Encodings of up to 256 bytes are swept completely.

The budget can be changed with `-atcSweepBudget <bytes>`:

    asn1scc -c -ACN -atc -atcSweepBudget 10485760 -o out sample.asn1 sample.acn

The value `0` keeps the minimum of two positions per end (with a warning); the sweeps cannot be switched
off. For C the budget can also be set when the tests are compiled, without regenerating them, with
`-DASN1SCC_SWEEP_BUDGET=<bytes>`.
