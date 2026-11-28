# Python Backend

The asn1scc Python backend generates Python classes for ASN.1 data structures, together with a self-contained runtime library for encoding and decoding them over uPER, ACN, and XER, and for validating them against their ASN.1 constraints. It is the fifth code-generation target, after C, Ada, Scala, and Rust, and was developed under ESA's MBEP activity.

This page is a usage-oriented extract of MBEP-ATS-AC-TN-003 (*ASN1SCC Python Backend User Manual and Technical Report*), which remains the reference for further detail; see [Reference](#reference).

## Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Generating a package](#generating-a-package)
- [Generated package layout](#generated-package-layout)
- [Using the generated code](#using-the-generated-code)
- [API quick reference](#api-quick-reference)
- [Error handling](#error-handling)
- [Supported types and ACN attributes](#supported-types-and-acn-attributes)
- [Known limitations](#known-limitations)
- [Testing](#testing)
- [Reference](#reference)

---

## Overview

The backend has two parts:

- a code generator, added to the existing F#/.NET asn1scc compiler: a StringTemplate template set (`StgPython/`) together with a language plug-in that maps the compiler's language-agnostic abstract model onto Python syntax and naming. The encoding and decoding algorithms themselves are not duplicated per language - they live in the compiler's shared back end, so the Python backend reuses the same ACN, uPER, and XER logic as the C, Ada, and Scala backends.
- a hand-written Python runtime library, `asn1python`, that the generated code depends on: the codec classes, the bit-stream buffer, the common `Asn1Base` class, the result types, and the exception hierarchy. A generation run emits a self-contained package combining this runtime with the generated per-module code, so the output has no runtime dependency to track.

The workflow is:

1. Author your ASN.1 models, plus ACN files where the wire format is fixed.
2. Run `asn1scc -python` on them; this produces an `asn1pylib/` package.
3. Make the package importable by putting the *parent* directory of `asn1pylib/` on the import path.
4. Construct values, validate them, and encode/decode them through the generated classes.

All three encoding rules are supported end to end: uPER, ACN, and XER.

---

## Prerequisites

- A working asn1scc build, from the .NET SDK: `dotnet build asn1scc.sln` produces the `asn1scc` binary.
- Python 3.11 or newer. The generated code and the runtime use only the standard library; no third-party packages are needed (pytest only if you run the generated test cases).

---

## Generating a package

Pass `-python` and at least one encoding flag, together with your ASN.1 and ACN files:

```bash
# minimal: uPER only
asn1scc -python -uPER -o . Temperature-Module.asn1

# typical: all encodings, with automatic test cases
asn1scc -python -uPER -ACN -XER -atc -typePrefix T -o . Temperature-Module.asn1 Temperature-Module.acn
```

| Flag | Purpose |
| --- | --- |
| `-python` | Select the Python backend. |
| `-uPER`, `-ACN`, `-XER` | Enable the uPER, ACN, and XER encodings; pass the ones you need. |
| `-atc` | Also generate automatic test cases, a sample `mainprogram.py`, and a `pyproject.toml`. |
| `-typePrefix T` | Prefix all generated class names (`Temperature` becomes `TTemperature`); recommended, since ASN.1 type names tend to collide with Python identifiers. |
| `-o DIR` | Output directory. The package is created as `DIR/asn1pylib/`. |

Note that the backend *appends* `asn1pylib/` to the `-o` directory: with `-o .` the package lands in `./asn1pylib/`, so point `-o` at the directory that should *contain* the package, not at the package directory itself.

ACN files are matched to the ASN.1 definitions by module and type name, and like ASN.1 files they carry a module wrapper:

```asn1
Temperature-Module DEFINITIONS ::= BEGIN
    Temperature [size 8, encoding pos-int]
END
```

Regenerate the package whenever the models change.

---

## Generated package layout

```text
asn1pylib/
├── __init__.py
├── pyproject.toml             (only with -atc)
├── asn1src/                   generated types
│   ├── __init__.py
│   ├── <Module>.py            one module per ASN.1 module
│   ├── <Module>_auto_tcs.py   (only with -atc) generated test values
│   ├── test_case_NNN.py       (only with -atc) runnable test cases
│   └── mainprogram.py         (only with -atc) sample program
└── asn1python/                runtime library (shared, hand-written)
    ├── __init__.py
    ├── codec.py               Encoding enum + Codec base + result types
    ├── codec_uper.py          UPEREncoder / UPERDecoder
    ├── acn_encoder.py         ACNEncoder
    ├── acn_decoder.py         ACNDecoder
    ├── xer_encoder.py         XEREncoder      (only with -XER)
    ├── xer_decoder.py         XERDecoder      (only with -XER)
    ├── xer_codec.py                           (only with -XER)
    ├── encoder.py, decoder.py low-level Encoder / Decoder primitives
    ├── bitstream.py           BitStream buffer
    ├── asn1_types.py          Asn1Base + result types
    ├── asn1_exceptions.py     Asn1Exception hierarchy
    ├── asn1_constants.py
    ├── helper.py
    └── verification.py
```

The generated code uses absolute imports under the `asn1pylib.` root, so the package is importable as soon as the directory *containing* `asn1pylib/` is on the import path (`PYTHONPATH`, `sys.path`, or your IDE's configuration):

```bash
export PYTHONPATH=/path/to/dir-containing-asn1pylib
```

```python
from asn1pylib.asn1src.Temperature_Module import TTemperature
from asn1pylib.asn1python import Encoding, ACNEncoder, ACNDecoder
```

Hyphens in ASN.1 module and type names become underscores in Python (`PUS-Packet-Module` becomes `PUS_Packet_Module`, `PUS-Header` becomes `TPUS_Header` with `-typePrefix T`).

---

## Using the generated code

### Minimal example

Given a module with a single constrained integer,

```asn1
Temperature-Module DEFINITIONS ::= BEGIN
    Temperature ::= INTEGER (0..255)
END
```

and the ACN file shown [above](#generating-a-package) fixing its wire form to one unsigned byte, generate the package:

```bash
asn1scc -python -uPER -ACN -XER -atc -typePrefix T -o . Temperature-Module.asn1 Temperature-Module.acn
```

and use it:

```python
from asn1pylib.asn1src.Temperature_Module import TTemperature
from asn1pylib.asn1python import Encoding

temp = TTemperature(200)              # within (0..255)

assert temp.is_constraint_valid()     # constraint validation

acn  = temp.encode(Encoding.ACN)      # bytearray(b'\xc8')
uper = temp.encode(Encoding.uPER)     # bytearray(b'\xc8')
xer  = temp.encode(Encoding.XER)      # XML: <Temperature>200</Temperature>

assert TTemperature.decode(Encoding.ACN, acn) == temp
assert TTemperature.decode(Encoding.uPER, uper) == temp
assert TTemperature.decode(Encoding.XER, xer) == temp
```

### Constructing values

Generated classes are dataclass-style values. Two patterns are worth knowing:

- Components with constraints get their own generated class, named `<Type>_<component>` - for example `TPUS_Header_serviceType` for the `serviceType` component of `TPUS_Header`. Pass instances of these classes, not plain `int`s: plain values fail constraint validation, and encoding validates by default.
- OCTET STRING and SEQUENCE OF components are represented by a class holding the element count and the elements, constructed as `Cls(n_count, arr)`.

Both patterns are visible in the following example; the auto-generated test cases (`<Module>_auto_tcs.py` with `-atc`) are a reliable reference for how to construct any generated type.

### A typical task: building and decoding PUS packets

```asn1
PUS-Packet-Module DEFINITIONS ::= BEGIN
    PUS-Header ::= SEQUENCE {
        serviceType    INTEGER (0..255),
        serviceSubtype INTEGER (0..255)
    }
    PUS-Packet ::= SEQUENCE {
        header  PUS-Header,
        payload OCTET STRING (SIZE(0..64))
    }
END
```

with a companion ACN file fixing the wire layout to single-byte header fields and a length-prefixed payload:

```asn1
PUS-Packet-Module DEFINITIONS ::= BEGIN
    PUS-Header [] {
        serviceType    [size 8, encoding pos-int],
        serviceSubtype [size 8, encoding pos-int]
    }
    PUS-Packet [] {
        header  [],
        payload []
    }
END
```

```bash
asn1scc -python -uPER -ACN -XER -atc -typePrefix T -o . PUS-Packet-Module.asn1 PUS-Packet-Module.acn
```

Building a PUS service 17 / subservice 1 "are-you-alive" telecommand, sending it, and reading it back:

```python
from asn1pylib.asn1src.PUS_Packet_Module import (
    TPUS_Packet, TPUS_Header,
    TPUS_Header_serviceType, TPUS_Header_serviceSubtype,
    TPUS_Packet_payload)
from asn1pylib.asn1python import Encoding, ACNEncoder, ACNDecoder

tc = TPUS_Packet(
    header=TPUS_Header(
        serviceType=TPUS_Header_serviceType(17),
        serviceSubtype=TPUS_Header_serviceSubtype(1)),
    payload=TPUS_Packet_payload(n_count=0, arr=[]))

tc_bits = tc.encode(Encoding.ACN)     # bytearray(b'\x11\x01\x00')

decoded = TPUS_Packet.decode(Encoding.ACN, tc_bits)
assert decoded == tc
assert decoded.header.serviceType == 17
assert decoded.header.serviceSubtype == 1

# the same decode through the codec classes
decoder = ACNDecoder.from_buffer(tc_bits)
assert TPUS_Packet.decode_acn(decoder) == tc

# codecs work on a caller-supplied buffer
encoder = ACNEncoder.of_size(64)
tc.encode_acn(encoder)                # validates constraints, then encodes
buf = encoder.get_bitstream_buffer()  # the whole 64-byte buffer, zero-padded
```

The dispatching forms `value.encode(Encoding.X)` and `Type.decode(Encoding.X, data)` are equivalent to constructing the corresponding codec class and calling the per-rule methods (`encode_acn`, `decode_acn`, `encode_uper`, ...); which form to use is a matter of preference. Note that encoders write into a caller-supplied buffer, so `get_bitstream_buffer()` returns that whole buffer - slice it to the encoded length when handing it to a link layer.

---

## API quick reference

Every generated class (via `Asn1Base`) provides:

| Member | Description |
| --- | --- |
| `value.encode(Encoding.X)` | Encode to a `bytearray`; validates constraints first. |
| `Type.decode(Encoding.X, data)` | Class method; decode from a `bytearray`. |
| `value.encode_uper(codec)` / `Type.decode_uper(codec)` | uPER through an explicit `UPEREncoder`/`UPERDecoder`. |
| `value.encode_acn(codec)` / `Type.decode_acn(codec)` | ACN through an explicit `ACNEncoder`/`ACNDecoder`. |
| `value.encode_xer(codec)` / `Type.decode_xer(codec)` | XER through an explicit `XEREncoder`/`XERDecoder`. |
| `value.is_constraint_valid()` | Returns an `Asn1ConstraintValidResult` (truthy when valid). |

The per-rule encode/decode methods take a `check_constraints: bool = True` argument; with the default, both encoding and decoding validate constraints and raise on violation.

Codec classes (`UPEREncoder`, `UPERDecoder`, `ACNEncoder`, `ACNDecoder`, `XEREncoder`, `XERDecoder`) share the `Codec` base with the constructors:

| Constructor | Description |
| --- | --- |
| `Codec.of_size(n)` | New codec over a zeroed buffer of `n` bytes (default 1 MiB). |
| `Codec.from_buffer(bytearray)` | New codec over an existing buffer. |
| `Codec.from_codec(codec)` | New codec sharing another codec's bitstream. |

`codec.get_bitstream_buffer()` returns the underlying `bytearray`. All runtime names are re-exported from `asn1pylib.asn1python`.

---

## Error handling

Two classes of errors exist:

- **Generation-time errors** (invalid ASN.1/ACN, inconsistent constraints) are reported by asn1scc on the console with a non-zero exit status; no package is produced.
- **Runtime errors** are raised as exceptions from the `Asn1Exception` hierarchy in `asn1python`:

| Exception | Raised when |
| --- | --- |
| `Asn1Exception` | Base class of the runtime errors. |
| `Asn1ValueOutOfRangeException` | A value violates a range constraint (encoding or decoding with `check_constraints=True`). |
| `Asn1UnexpectedEndOfDataException` | The input buffer ends prematurely during decoding. |
| `Asn1InvalidValueException` | A decoded value is structurally invalid (unknown enum index, invalid choice discriminant). |
| `Asn1OverflowException` | An arithmetic operation would overflow. |

Lower-level operations report through an `ErrorCode` enum (`SUCCESS`, `INSUFFICIENT_DATA`, `INVALID_VALUE`, `CONSTRAINT_VIOLATION`, `BUFFER_OVERFLOW`, `UNSUPPORTED_OPERATION`) carried by the `EncodeResult`/`DecodeResult` types, and a non-exception `Asn1SccError` value-wrapper exposes `get_error_code()` for code that prefers error codes over try/except.

The generated test cases raise the `Asn1TestcaseError` family (`Asn1TestcaseEncodeFailedError`, `Asn1TestcaseDecodeFailedError`, `Asn1TestcaseConstraintFailedError`, `Asn1TestcaseDifferentResultError`); these indicate a backend problem, not an application error.

---

## Supported types and ACN attributes

The Python backend supports the following ASN.1 types:

| ASN.1 type | Support details |
| --- | --- |
| BOOLEAN, ENUMERATED, NULL | Fully supported. |
| INTEGER | Constrained, semi-constrained, unconstrained and positive; multiple ACN bit widths; big- and little-endian; positive-integer or two's-complement encodings. |
| REAL | IEEE-754 binary32 and binary64; big- and little-endian. |
| SEQUENCE | Including OPTIONAL and DEFAULT components. |
| SEQUENCE OF | Supported. |
| CHOICE | ACN determinant selects the active alternative. |
| IA5String, NumericString | With size constraints. |
| BIT STRING, OCTET STRING | With size constraints and ACN size. |
| OBJECT IDENTIFIER | Supported. |
| DATE/TIME family | Supported. |

Type aliases, subtyping, parameterized types, and module imports / cross-module references are also supported, so a multi-module ASN.1 project generates as a consistent set of cross-referencing Python modules.

On the ACN side, the backend supports the following attributes:

| ACN attribute | Notes |
| --- | --- |
| Size and presence determinants | Including external and internal field determinants. |
| Alignment | Align-to-next. |
| Endianness selection | Big- and little-endian. |
| Integer encodings | As listed for INTEGER above. |
| Encode-values | Supported. |

These attributes are what pin a generated type's wire representation to an existing packet format such as PUS-C TC/TM.

---

## Known limitations

- The backend does not provide a `decode_pure` operation. This is a deliberate design difference from the Scala backend, which uses `decode_pure` for static, effect-typed codec-state safety - a concern that does not apply in idiomatic Python.
- The `pyproject.toml` emitted with `-atc` does not yet support `pip install` of the generated package: the generated modules import each other through absolute `asn1pylib.…` paths, while the declared packages install as top-level `asn1src` and `asn1python`. Until this is resolved, make the package importable by putting the parent directory of `asn1pylib/` on the import path, as shown in [Generated package layout](#generated-package-layout).

---

## Testing

With `-atc`, asn1scc generates executable test cases (`asn1pylib/asn1src/test_case_NNN.py`) that construct values from your models, encode and decode them over every enabled encoding, and check the round trips. Run them with pytest from the directory given to `-o` (so that `asn1pylib` is importable):

```bash
cd <directory-passed-to--o>
python3 -m pytest
```

The repository's regression harness runs the same tests for all supported languages; for the Python backend:

```bash
python3 v4Tests/scripts/runTests.py -l python
```

---

## Reference

- **MBEP-ATS-AC-TN-003** - *ASN1SCC Python Backend User Manual and Technical Report* (Issue 1 Rev 1): the full user manual, including a detailed reference manual of the runtime API, a PUS-C tutorial, the development summary, the formal verification of the core encoding routines (with ETH Zürich), and the requirements compliance record.
- Backend sources in this repository: `StgPython/` (StringTemplate code generator) and `asn1python/` (runtime library source, copied into every generated package).
