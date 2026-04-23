# ASN.1 Python Runtime Library

This is the Python runtime library for the ASN.1 SCC compiler, providing ASN.1-specific types and encoding/decoding operations.

## Overview

The `asn1python` library provides:

- **Sized Integer Types**: `UInt8`, `UInt16`, `UInt32`, `UInt64`, `Int8`, `Int16`, `Int32`, `Int64` with overflow checking
- **ASN.1 Semantic Types**: `Asn1Real`, `Asn1Boolean`, `NullType`, `Asn1ObjectIdentifier`
- **Time Types**: `Asn1Date`, `Asn1LocalTime`, `Asn1UtcTime`, `Asn1DateLocalTime`
- **Encoding/Decoding**: Support for UPER, ACN, XER, and BER encoding rules
- **Bitstream Operations**: Precise bit-level manipulation
- **Cross-Language Compatibility**: Identical binary output with C, Ada, and Scala implementations

## Installation

```bash
pip install -e .
```

## Usage

This library is primarily used by code generated from the ASN.1 SCC compiler. For manual usage:

```python
from asn1python import UInt8, UInt16, BitStream, UPERCodec

# Create sized integers
value = UInt8(255)  # 8-bit unsigned integer

# Bitstream operations
stream = BitStream()
stream.write_bits(value.value, 8)

# UPER encoding
codec = UPERCodec()
encoded = codec.encode_integer(value, min_val=0, max_val=255)
```

## Generated Code Integration

This library is automatically used by Python code generated from ASN.1 specifications:

```python
# Generated Python code uses runtime types
from asn1python import UInt8, UInt16, Asn1Real

@dataclass
class MySequence:
    field1: UInt8
    field2: UInt16
    field3: Asn1Real
```

## Development

Run tests:
```bash
pytest tests/
```

Type checking:
```bash
mypy src/asn1python/
```

Code formatting:
```bash
black src/ tests/
```

## Architecture

The library follows the same architectural patterns as the C, Ada, and Scala runtime libraries:

- **types.py**: Core ASN.1 types and operations
- **bitstream.py**: Bit-level operations
- **codec*.py**: Encoding/decoding for different ASN.1 encoding rules
- **helper.py**: Utility functions
- **verification.py**: Constraint validation

## License

This library is part of the ASN.1 SCC project and follows the same licensing terms.