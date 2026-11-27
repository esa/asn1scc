# Bound-checked writes of the C runtime

The C encoders (uPER and ACN, also with `--acn-v2`) write into a `BitStream` through a few functions of the C
runtime (`asn1crt_encoding.c`, `asn1crt_encoding_acn.c`). Each of them checks, before it writes or moves the
position, that what it writes fits in the buffer given to `BitStream_Init` / `BitStream_AttachBuffer`. A write that
does not fit:

- writes nothing and leaves the position where it was, so the position never passes the end of the buffer;
- sets the member `overflow` of the `BitStream` (it stays set until the next `BitStream_Init` / `BitStream_AttachBuffer`);
- freezes the stream: from then on no runtime function writes into the buffer or moves the position, also when a
  later, smaller write would fit;
- makes the encoder return `FALSE`, usually with the error code `ERR_BITSTREAM_OVERFLOW` (`asn1crt.h`, value `-1`).

Every generated encoder ends with

```c
    return BitStream_EncodeResult(pBitStrm, ret, pErrCode);
```

which returns `ret` when nothing overflowed. After an overflow it returns `FALSE`; it keeps an error code that the
encoder already set for a failure of its own, and sets `ERR_BITSTREAM_OVERFLOW` otherwise.

After a `FALSE` result with `overflow` set, the position is where the first refused write would have started, and the
buffer up to that position holds the encoding as far as it got: for an encoder that writes every bit once, in stream
order, these are the first bits of the full encoding. The `--acn-v2` encoders also write some values a second time, at
a position saved before (a deferred determinant, the length of an OCTET / BIT STRING CONTAINING region, written after
the data that follows it); those second writes are refused too once the stream is frozen
(`Acn_BitStream_SetPos` does not move a frozen stream), so such a field keeps the placeholder written first. The
encoding is not usable either way: test the result.

**The `overflow` member of the `BitStream` is the test for "the buffer was too short"**, not the error code. A check of
the generated code that runs after a refused write can fail on what that write left behind and report its own code: an
`--acn-v2` length field shared by the CONTAINING regions of several SEQUENCE OF elements, for example, is patched with
each region's length computed from positions; when the contents of a later element were refused, its length differs
from the first one's and the encoder returns the code of that consistency check, with `overflow` set. A program that
must tell "buffer too small" from "invalid value" tests `overflow` after a `FALSE` result.

## When it matters

A buffer of `<Type>_REQUIRED_BYTES_FOR_ENCODING` / `<Type>_REQUIRED_BYTES_FOR_ACN_ENCODING` bytes holds every valid
value, and with such a buffer the checks never fail. They are a second barrier for the cases where that does not hold:

- a caller passes a shorter buffer (for example the space left in a larger frame);
- a maximum size computed by the compiler is too small for some value (a compiler defect);
- a value outside its constraints is encoded with `bCheckConstraints = FALSE` (for example an INTEGER above its range
  gets a negative number of leading zero bits; the encoder now refuses that move instead of moving the position back).

Before, the encoders did not check most writes: a short buffer could be written past, a run of leading zero bits could
wrap the position and the encoder return `TRUE` with a truncated encoding, and the variable-size OCTET STRING write
returned `FALSE` without an error code. An uPER OCTET / BIT STRING (CONTAINING X) whose contents encode longer than the
maximum the compiler computed for them also fails with `ERR_BITSTREAM_OVERFLOW` now.

## Error code

`ERR_BITSTREAM_OVERFLOW` is negative so that it cannot be equal to an error code of the generated code (they count
from 1 over the whole project). The generated code and the runtime only test an error code against 0. A program that
stores error codes in an unsigned field should map it explicitly.

## `BitStream` layout

The member `overflow` was added at the end of `BitStream`. Every unit that includes `asn1crt.h` must be compiled
again with the new runtime. A `BitStream` set up with `BitStream_Init` / `BitStream_AttachBuffer` (or the `...2`
variants) has it cleared; a zero-initialized structure has it cleared too. A `BitStream` whose members are set one by
one without these functions and without zero-initialization has an indeterminate `overflow`, and its encoders may
return `FALSE` with `ERR_BITSTREAM_OVERFLOW` for no reason: set `overflow = FALSE` too, or use the functions.

Reusing a stream: after a failed encode, setting `currentByte` / `currentBit` back by hand does not clear `overflow`,
so every later encode into that stream fails as well. Call `BitStream_Init` / `BitStream_AttachBuffer` again (or clear
`overflow`) before the next encode.

## Streaming (`ASN1SCC_STREAMING`)

- Without a push callback (`pushDataPrm == NULL`) the buffer is checked as above. An octet array is written byte by
  byte there, so the bytes that fit before the refused one are written.
- With a push callback the buffer is a chunk that is pushed (and starts again at byte 0) whenever it is full; there is
  no size to check. The other refusals (a negative number of leading zero bits, CONTAINING contents above their
  maximum) still set `overflow` and freeze the stream: nothing more is written or pushed. The runtime pushes a full
  chunk before the position passes its end, also inside a run of zero bits and inside the padding of an `align-to-next
  word` / `dword`. Known limitations, as before: the alignment is computed from the position inside the chunk (use a
  chunk size that is a multiple of 4 to get the same encoding as without streaming), the bits that a run of zeros or a
  padding skips keep what the chunk buffer held before, and the `--acn-v2` encoders, which write a length or a
  determinant again after the data that follows it, need the whole encoding in one buffer.

## Turning the checks off: `ASN1SCC_UNCHECKED_WRITES`

Compiling every unit (runtime and generated code) with `-DASN1SCC_UNCHECKED_WRITES` restores the unchecked writes of
earlier versions: the write functions and position moves do not test the buffer size, and `BitStream_EncodeResult` is
`ret` (no call), and the stream does not freeze. What stays: the `overflow` member; a negative number of leading zero
bits is still refused (and the encoder result then ignores it); the runtime functions that already returned a result
set the flag when they refuse a write; the uPER CONTAINING functions (`BitStream_EncodeContainingOctets` / `Bits`)
still return `FALSE` when the flag is set; and the generated encoders of CONTAINING contents still call
`BitStream_TakeOverflow` after encoding the contents into their temporary stream, so their code is a few bytes larger
than with earlier versions. The automatic tests do not cover this configuration (their short-buffer sweep is off), so
the coverage of the generated code is evidence for the default, checked runtime only.

The decision whether the checks are on is made in one place, `asn1crt.h`:

```c
#ifndef ASN1SCC_UNCHECKED_WRITES
#define ASN1SCC_CHECKED_WRITES
#endif
```

## Cost

The checks are a comparison per write in the runtime (the buffer size and the overflow flag) and one call at the end
of each encoder. Measured on the PUS-C test grammars and a small benchmark, `.text` of the runtime and the generated
codecs with `-DNDEBUG`: +2 % to +6 % at `-Os` and -2 % to +5 % at `-O2` (x86-64, Cortex-M0, msp430, AVR). Encode time
on an msp430 simulator (instructions per encode, `-Os` / `-O2`): +19 % to +23 % for encoders made of small integer
fields (a 13-field packet header, a SEQUENCE OF 256 small integers in uPER), +13 % to +16 % for the ACN SEQUENCE OF,
below +0.5 % for OCTET STRING contents, which are written in blocks. Instructions are not cycles, and these encoders
were called without their constraint check, which the percentages do not include.

## Automatic tests

The automatic tests (`-atc`) of the C backend encode every test value again into buffers shorter than its encoding
and require `FALSE`, a nonzero error code and a position inside the buffer (the short-buffer sweep, error code 9, see
[automatic-test-decode-sweeps.md](automatic-test-decode-sweeps.md)). This executes the failure paths of the generated
encoders after a refused write, which a valid value in a buffer of the full size never reaches.
