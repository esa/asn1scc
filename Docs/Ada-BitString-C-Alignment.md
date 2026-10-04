# C-aligned Ada BIT STRING (`--ada-bitstring-alignment`)

## 1. Motivation

The C and Ada backends used to store a `BIT STRING` value differently.

* **C** packs the bits *MSB-first*: ASN.1 bit `k` lives in byte `k/8`, at bit
  position `7 - (k mod 8)` — i.e. bit 0 is `0x80`.
* **Ada** used `adaasn1rtl.BitArray`, a packed array with
  `Component_Size = 1`. Such an array always packs *LSB-first*: ASN.1 bit `k`
  lives at bit position `k mod 8` — i.e. bit 0 is `0x01`.

Both encodings put the same bits on the wire (uPER/ACN/XER are identical), so
interoperability over the codecs is fine. But a `BIT STRING` buffer produced by
C and read by Ada (or vice versa) without going through the codec was
**per-byte bit-reversed**. In C, bit 0 means `0x80`; in Ada, bit 0 meant `0x01`.

`--ada-bitstring-alignment` makes the Ada representation byte-for-byte
compatible with the C representation, while keeping an array-of-bits style API
for the application (through named fields).

## 2. Enabling it

```
asn1scc -Ada -uPER -ACN --ada-bitstring-alignment -o out grammar.asn
```

It is **off by default**; the default Ada output is byte-for-byte unchanged.

The option is Ada-only: passing it together with `-c`, `-Rust`, `-Scala` or
`-python` is rejected.

## 3. What the generated type looks like

Given

```
BS ::= BIT STRING {bit0(0), bit7(7)} (SIZE (8))
```

**Before (default):**

```ada
subtype BS_array is adaasn1rtl.BitArray(BS_index);
type BS is record
    Data  : BS_array;
end record;
```

**After (`--ada-bitstring-alignment`):**

```ada
type BS_data is record
    Bit0, Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7 : adaasn1rtl.BIT;
end record;
for BS_data'Bit_Order use System.High_Order_First;
for BS_data use record
       Bit0 at 0 range 0 .. 0;
       Bit1 at 0 range 1 .. 1;
       ...
       Bit7 at 0 range 7 .. 7;
end record;
for BS_data'Size use 1 * 8;
for BS_data'Alignment use 1;

type BS is record
    Data  : BS_data;
end record;
```

There is now **one named field per bit**, `Bit0 .. Bit(N-1)` for an `N`-bit
type, placed with `High_Order_First` and explicit component clauses so that
ASN.1 bit `k` is the same memory bit as in C. The record has the same size and
alignment as the C `struct { byte arr[ceil(N/8)]; }`, so a C buffer and an Ada
value are the same bytes (modulo the variable-size length field, see §6).

## 4. Application code migration (this is the API break)

The generated **named-bit setters keep their names and meaning**:

```ada
BS_set_bit0 (X);   --  unchanged
BS_set_bit7 (X);   --  unchanged
```

What changes is how the application reaches an individual bit. The bit array
`Data (i)` becomes a record of fields `Data.Bit<i-1>`:

| default (BitArray)            | with `--ada-bitstring-alignment` | ASN.1 bit |
|-------------------------------|----------------------------------|-----------|
| `X.Data (1) := 1;`            | `X.Data.Bit0 := 1;`              | bit 0     |
| `X.Data (2) := 1;`            | `X.Data.Bit1 := 1;`              | bit 1     |
| `B := X.Data (1);`            | `B := X.Data.Bit0;`              | bit 0     |
| `X.Data'First`, `X.Data'Length` | replace with explicit bit indices / the type size | — |
| `(Data => (others => 0))`     | `(Data => (others => 0))` (still works) | — |

So `X.Data (k)` becomes `X.Data.Bit(k-1)`. This is a source-level change and
every existing Ada application that indexes `Data` **must** be updated;
recompilation alone is not enough. This is why the option is opt-in.

### Why it must be a field, not an index

`pragma Constant_Indexing` / `Variable_Indexing` (which would allow
`X.Data (k)`) can only be applied to a **tagged** type. A tagged record carries
a tag that overlaps the bits, so it can no longer be 1 byte/`ceil(N/8)` bytes
and stops matching C. Named fields are a plain record: `X.Data.Bit0 := 1` is a
normal assignment, the whole-record `=` works, `(others => 0)` works, and the
record is `Unchecked_Conversion`-compatible with the C byte buffer.

## 5. What does *not* change

* The wire format. uPER, ACN and XER produce exactly the same bytes/bits as
  before; they are written from the record's storage using a byte overlay.
* `X_Init`, `X_set_bitN`, `X_IsConstraintValid`, `X_Equal`, the encode/decode
  subprogram names and signatures.
* C, Rust, Scala and Python output — the option is Ada-only.
* The default Ada output (option off) is byte-for-byte identical.

## 6. Notes and limitations

* **Variable size.** For `BIT STRING (SIZE (m..n))` the Ada record keeps its
  `Length : ..._length_index` field and the `Data` record has `n` fields; the
  low `floor(Length/8)` whole bytes have the same layout as C, and the top
  partial byte has the same *set of bits* but the unused high bits are zero
  (C leaves them unspecified). When exchanging raw buffers with C for
  non-byte-multiple sizes, mask the top byte if C sets that padding.
* **Size.** One named field per bit. For very wide bit strings (hundreds of
  bits) the generated record is correspondingly wide. This is intended for the
  usual telecommand/telemetry bit strings, not for multi-kilobit strings.
* This option changes the generated **Ada data type**, so it is not
  source-compatible with existing Ada applications (see §4), and it is not
  layout-compatible with Ada code generated **without** the option. Use it
  consistently for every component that shares a raw `BIT STRING` buffer.
* The C backend has an unrelated bug in the generated named-bit setters
  (`X_set_bitN` emits the mask as bare hex, e.g. `80` which C parses as decimal
  `0x50`). That is independent of this option and unaffected by it: it concerns
  the C setter code, not the Ada representation.
