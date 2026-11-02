# Decode fault sweeps in the automatic tests

Since version 4.9.33.0 the automatic tests (`-atc`) of the C and Ada backends also check how the uPER and
ACN decoders handle damaged input. Each automatic test encodes a value, decodes it, validates the result
and compares it with the original. When this round trip succeeds, the test now decodes damaged copies of
the same encoding (the truncation at a bit offset and the single-bit flips since 4.9.67.0):

| sweep | what is decoded | expected result | C | Ada |
|---|---|---|---|---|
| truncation | the first `k` bytes of the encoding, for `k = 0, 1, ...`, from a buffer of exactly `k` bytes | the decoder fails with a nonzero error code | yes | no |
| truncation at a bit offset | the value encoded again starting at bit `s` of the buffer, for `s = 1 .. 7`; first the whole encoding, then its first `k` bytes, each decoded from bit `s` | the whole encoding decodes to a value that passes `IsConstraintValid`; a truncated copy fails with a nonzero error code | yes | no |
| corruption | the whole encoding with byte `i` set to `0x00`, then to `0xFF`, then with each of its 8 bits flipped in turn (restored after each decode) | the decoder fails with a nonzero error code, or succeeds with a value that passes `IsConstraintValid`; it must not crash or raise an exception | yes | yes |

The sweeps know nothing about the structure of the type. A damaged byte always belongs to some field, so
the decoder of that field fails, however deep it is nested, and the failure travels back through every
enclosing decoder. This exercises the error paths of the generated decoders, which a round trip of valid
values never reaches.

A stream that ends at a byte boundary can only end inside a field that contains the first bit of that byte.
A field that lies inside one byte without starting it (for example a 4-bit integer after a presence bit) is
never cut by the byte truncation; encoded from another bit offset, it is. The bit-offset truncation also
checks that the generated encoders and decoders work when the value does not start at the first bit of the
buffer, as happens when a decoder is called in the middle of a stream. Single-bit flips reach the value
checks (ranges, ENUMERATED codes, CHOICE indexes, lengths) that setting a whole byte to `0x00` or `0xFF` misses.

Ada runs only the corruption sweep (with the bit flips). Every generated Ada decoder has a precondition that the stream holds
enough bytes for the largest encoding of its type, so a truncated stream is outside the decoder's contract.
PDUs that contain an ACN `size deduced` field are swept in C only, and a truncated copy of such a PDU may
also decode successfully (to a valid value with fewer elements).

## Failures

A test that violates one of these rules fails like any other automatic test. The output names the
position:

    Msg_ACN_enc_dec: decoding the first 3 of 5 bytes from bit 0 returned TRUE with error code 0
    Test case 'test_case_ACN_000002' failed. A truncated copy of the encoded value was decoded without an error code.

In C the test returns error code 6 (truncation), 7 (corruption) or 8 (the value encoded from a bit offset
other than 0 did not decode to a valid value); in Ada the test fails in the step
`TC_DECODE_CORRUPTED` (error code 7), after the line printed by the sweep. A failure means that a decoder accepted input it should have rejected, rejected it
without setting an error code, or crashed. Please report it.

## Cost and the budget

For an encoding of `L` bytes a full sweep needs `8 L` truncated (byte boundary and 7 bit offsets) and up to
`10 L` corrupted decodes. To keep run times low for large values, each sweep tries at most 128 positions at
the start and 128 at the end of the encoding, and stops after the decoders have read about 1 MB in total
(512 KB per end); the truncations at the 7 bit offsets share one more such budget. At least two positions
are always tried at each end. Encodings of up to 256 bytes are swept completely.

The budget can be changed with `-atcSweepBudget <bytes>`:

    asn1scc -c -ACN -atc -atcSweepBudget 10485760 -o out sample.asn1 sample.acn

The value `0` keeps the minimum of two positions per end (with a warning); the sweeps cannot be switched
off. For C the budget can also be set when the tests are compiled, without regenerating them, with
`-DASN1SCC_SWEEP_BUDGET=<bytes>`.
