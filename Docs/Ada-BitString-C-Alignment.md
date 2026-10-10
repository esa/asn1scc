# C-aligned Ada BIT STRING (`--ada-bitstring-alignment`)

## 1. Motivation

The C and Ada backends store a `BIT STRING` value differently.

* **C** stores it as an array of whole bytes, packed *MSB-first*:
  ASN.1 bit `k` is bit `7 - (k mod 8)` of byte `k / 8` (counting from the
  most-significant bit). Bit 0 is `0x80`.
* **Ada** used `adaasn1rtl.BitArray`, a packed array with
  `Component_Size = 1`. Such an array always packs *LSB-first*: ASN.1 bit `k`
  is bit `k mod 8` of byte `k / 8`. Bit 0 is `0x01`.

Both put the same bits on the wire (uPER/ACN/XER are identical), so
interoperability over the codecs is fine. But a `BIT STRING` buffer produced by
C and read by Ada (or vice versa) without going through the codec was
**per-byte bit-reversed**: C's bit 0 is `0x80`, Ada's bit 0 was `0x01`.

`--ada-bitstring-alignment` makes the Ada representation *the same type as C* —
an array of bytes — so that both the memory layout and the API line up.

## 2. Enabling it

```
asn1scc -Ada -uPER -ACN --ada-bitstring-alignment -o out grammar.asn
```

It is **off by default**; the default Ada output is byte-for-byte unchanged.
It is Ada-only: passing it with `-c`, `-Rust`, `-Scala` or `-python` is
rejected.

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
subtype BS_array is adaasn1rtl.OctetBuffer (1 .. 1);   --  ceil(8 / 8) bytes
type BS is record
    Data  : BS_array;
end record;
```

This is exactly C's `struct { byte arr[1]; }` — the same bytes, the same
shape. `OctetBuffer` is `array (Natural range <>) of Unsigned_8`, i.e. `byte[]`.

Variable size keeps the length *in bits* next to the byte array:

```
BSVar ::= BIT STRING (SIZE (1..20))
-->
type BSVar is record
    Length : BSVar_length_index;               --  1 .. 20  (bits)
    Data   : adaasn1rtl.OctetBuffer (1 .. 3);  --  ceil(20 / 8) bytes
end record;
```

This mirrors C's `struct { int nCount; byte arr[3]; }`.

### Addressing a bit

ASN.1 bit `k` (0-based) lives in:

| | expression |
|---|---|
| byte index (1-based, like C `arr[k/8]`) | `k / 8 + 1` |
| mask (like C `0x80 >> (k%8)`) | `16#80# / 2**(k mod 8)` = `Shift_Right (16#80#, k mod 8)` |

```ada
--  set bit k
X.Data (k / 8 + 1) := X.Data (k / 8 + 1) or  Shift_Right (16#80#, k mod 8);

--  test bit k
if (X.Data (k / 8 + 1) and Shift_Right (16#80#, k mod 8)) /= 0 then ...
```

`Shift_Right` / the bit operators are visible once the unit has
`with Interfaces; use Interfaces;` (the generated packages already make the
`adaasn1rtl` byte operators directly visible).

The runtime library has the same operations ready to use, with a 0-based bit
index:

```ada
adaasn1rtl.BitString_Set_Bit (X.Data, k, 1);          --  set bit k
B := adaasn1rtl.BitString_Get_Bit (X.Data, k);        --  read bit k (0 or 1)
```

## 4. What the application must change (this is the API break)

The generated **named-bit setters keep their names and meanings**:

```ada
BS_set_bit0 (X);   --  sets Data(1) bit 7, i.e. 0x80  (was already bit 0 in C)
BS_set_bit7 (X);   --  sets Data(1) bit 0, i.e. 0x01
```

Every other access changes. The old `Data : BitArray` (indexed by *bit*,
`X.Data (1)` = bit 0) becomes `Data : OctetBuffer` (indexed by *byte*):

| default (BitArray, per bit)      | with `--ada-bitstring-alignment` (per byte) |
|----------------------------------|---------------------------------------------|
| `X.Data (1) := 1;` (bit 0)       | `X.Data (1) := 16#80#;`                     |
| `X.Data (2) := 1;` (bit 1)       | `X.Data (1) := 16#40#;`                     |
| `X.Data (9) := 1;` (bit 8)       | `X.Data (2) := 16#80#;`                     |
| `B := X.Data (1);`               | `B := (X.Data (1) and 16#80#) /= 0;`        |
| `X.Data'Length` (N bits)         | `X.Data'Length` (ceil(N/8) bytes)           |
| `X.Length` (variable size)       | `X.Length` — unchanged, still in bits       |

This is a source-level change: every existing Ada application that indexes
`Data` **must** be updated. That is why the option is opt-in.

## 5. What does *not* change

* The wire format. uPER, ACN and XER produce exactly the same bytes/bits as
  before (C and Ada are byte-identical on the wire).
* `X_Init`, `X_set_bitN`, `X_IsConstraintValid`, `X_Equal`, and the
  encode/decode subprogram names and signatures. `X_Equal` and the
  single-value constraints of `X_IsConstraintValid` compare the first
  `Length` (or `N`) bits and ignore the unused bits of the last byte, as in C
  (`adaasn1rtl.BitString_Bytes_Equal`).
* C, Rust, Scala and Python output — the option is Ada-only.
* The default Ada output (option off) is byte-for-byte identical.

## 6. Implementation notes

* `spec_a.stg` emits `subtype X_array is OctetBuffer (1 .. ceil(Nmax/8))`
  and `Data : X_array`.
* uPER/ACN encode and decode call the byte-oriented
  `BitStream_AppendBits` / `BitStream_ReadBits` directly (the C codec path),
  with no conversion. Decode clears `Data` first so a short variable-size
  string compares equal.
* XER (and ACN null-terminated) still go through the bit-oriented RTL, so the
  RTL gained two helpers, `BitString_BitArray_To_Bytes` /
  `BitString_Bytes_To_BitArray`, mirroring the C byte handling. The generated
  code keeps an array-of-bits API there, using a temporary `BitArray`.
* uPER fragmentation (more than 64K bits) keeps its bit-by-bit loop, reading
  and writing the bits with `BitString_Get_Bit` / `BitString_Set_Bit`.
* `--ada-bitstring-alignment` is a normal, off-by-default code-generation flag
  (see `CommonTypes.bitStringAlignment`), injected into the Ada templates by
  `ST.call`.
* Tests: a corpus file whose first line contains `ADA_BITSTRING_ALIGNMENT`
  runs only for Ada, with the flag (`v4Tests/scripts/runTests.py` and
  `regression/Program.fs`), e.g. `v4Tests/test-cases/acn/08-BIT-STRING/010.asn1`.

## 7. Limitations

* The type is `ceil(N/8)` bytes, so a non-byte-multiple size carries up to 7
  unused bits in the last byte. They are zeroed on decode and ignored by
  `X_Equal`; C leaves them unspecified, so mask the last byte if you compare
  raw buffers coming from C.
* `Data'Length` is a byte count, not a bit count. For a fixed-size type the bit
  count is the compile-time constant `N`; for a variable-size type it is
  `X.Length`.
