import math
from enum import Enum

import pytest
import random
from random import choice
from string import printable
from asn1python.acn_encoder import ACNEncoder, IA5_CHAR_SET


@pytest.fixture
def acn_encoder() -> ACNEncoder:
    return ACNEncoder.of_size(1024*1024) # TODO: add possibility of exact size later.

@pytest.fixture
def seed() -> int:
    seed = random.randint(0, 1_000_000)
    random.seed(seed)
    return seed

class Version(Enum):
    ALL_FIELDS = 0
    RANGE_ONLY = 1

def pytest_generate_tests(metafunc):
    if "bit" in metafunc.fixturenames:
        metafunc.parametrize("bit", [1, 2, 3, 4, 6, 8, 10, 12, 16, 32, 48, 64])
    if "byte" in metafunc.fixturenames:
        metafunc.parametrize("byte", [1, 2, 3, 4, 6, 8, 10, 12, 16, 32, 48, 64])
    if "nibble" in metafunc.fixturenames:
        metafunc.parametrize("nibble", [1, 2, 3, 4, 5, 6])
    if "signed" in metafunc.fixturenames:
        metafunc.parametrize("signed", [True, False])
    if "max_length" in metafunc.fixturenames:
        metafunc.parametrize("max_length", [1, 2, 3, 4, 5, 6, 7, 8, 16, 32, 64, 128, 256, 512, 1024])
    if "version" in metafunc.fixturenames:
        metafunc.parametrize("version", [Version.ALL_FIELDS, Version.RANGE_ONLY])
    if "null_characters" in metafunc.fixturenames:
        metafunc.parametrize("null_characters", [bytes(1), bytes(2), bytes(4), bytes(8)])
    if "charset" in metafunc.fixturenames:
        metafunc.parametrize("charset", [
            BINARY_CHARSET,
            NUMERIC_CHARSET, 
            HEX_CHARSET,
            ALPHA_UPPERCASE_CHARSET,
            ALPHA_LOWERCASE_CHARSET,
            DNA_CHARSET,
            MORSE_CHARSET,
            BOOLEAN_CHARSET
        ])
    if "test_charset" in metafunc.fixturenames:
        metafunc.parametrize("test_charset", [
            (BINARY_CHARSET, "binary"),
            (NUMERIC_CHARSET, "numeric"), 
            (HEX_CHARSET, "hex_upper"),
            (HEX_LOWERCASE_CHARSET, "hex_lower"),
            (ALPHA_UPPERCASE_CHARSET, "alpha_upper"),
            (ALPHA_LOWERCASE_CHARSET, "alpha_lower"),
            (DNA_CHARSET, "dna"),
            (RNA_CHARSET, "rna"),
            (MORSE_CHARSET, "morse"),
            (BOOLEAN_CHARSET, "boolean")
        ])

# Utility functions for integer range calculations
def get_unsigned_max(bits: int) -> int:
    """Get maximum value for unsigned integer with given bit size."""
    return (2 ** bits) - 1

def get_unsigned_min(bits: int) -> int:
    """Get minimum value for unsigned integer with given bit size."""
    return 0

def get_signed_max(bits: int) -> int:
    """Get maximum value for signed integer with given bit size."""
    return (2 ** (bits - 1)) - 1

def get_signed_min(bits: int) -> int:
    """Get minimum value for signed integer with given bit size."""
    return -(2 ** (bits - 1))

def get_random_unsigned(bits: int) -> int:
    """Get random unsigned integer for given bit size."""
    return random.randint(get_unsigned_min(bits), get_unsigned_max(bits))

def get_random_signed(bits: int) -> int:
    """Get random signed integer for given bit size."""
    return random.randint(get_signed_min(bits), get_signed_max(bits))

def get_random_float_positive() -> float:
    """Get random positive real."""
    return random.random()

def get_random_float_negative() -> float:
    """Get random negative real."""
    return -random.random()

def get_random_float_unsigned(bits: int) -> float:
    """Get random unsigned real."""
    return random.uniform(0.0, get_unsigned_max(bits))

def get_random_float_signed(bits: int) -> float:
    """Get random signed real."""
    return random.uniform(get_signed_min(bits), get_signed_max(bits))

def get_random_float(precision: str = "double", positive_only: bool = False, negative_only: bool = False) -> float:
    """Get random IEEE 754 floating-point number for testing (normal values only).
    
    Args:
        precision: "single" for 32-bit or "double" for 64-bit precision testing
        positive_only: If True, return only positive values
        negative_only: If True, return only negative values
        
    Returns:
        Random normal float value (positive, negative, or both based on parameters)
    """
    # Define ranges based on precision
    if precision == "single":
        # IEEE 754 single precision ranges
        min_normal = 1.175494e-38
        max_normal = 3.402823e+38
    else:  # double precision (default)
        # IEEE 754 double precision ranges  
        min_normal = 2.2250738585072014e-308
        max_normal = 1.7976931348623157e+308
    
    # Generate positive value
    value = random.uniform(min_normal, max_normal / 1e100)
    
    # Apply sign based on parameters
    if negative_only:
        return -value
    elif positive_only:
        return value
    else:
        # Random sign
        return value if random.choice([True, False]) else -value

def get_small_float(precision: str = "double", positive_only: bool = False, negative_only: bool = False) -> float:
    """Get random small IEEE 754 floating-point number (near zero).
    
    Args:
        precision: "single" for 32-bit or "double" for 64-bit precision testing
        positive_only: If True, return only positive values
        negative_only: If True, return only negative values
        
    Returns:
        Random small float value (positive, negative, or both based on parameters)
    """
    # Define ranges based on precision
    if precision == "single":
        # IEEE 754 single precision ranges
        min_normal = 1.175494e-38
        min_subnormal = 1.401298e-45
    else:  # double precision (default)
        # IEEE 754 double precision ranges  
        min_normal = 2.2250738585072014e-308
        min_subnormal = 5e-324
    
    # Generate positive small value
    small_value = random.uniform(min_subnormal, min_normal)
    
    # Apply sign based on parameters
    if negative_only:
        return -small_value
    elif positive_only:
        return small_value
    else:
        # Random sign
        return small_value if random.choice([True, False]) else -small_value

def get_big_float(precision: str = "double", positive: bool = True) -> float:
    """Get random large IEEE 754 floating-point number (near limits).
    
    Args:
        precision: "single" for 32-bit or "double" for 64-bit precision testing
        positive: If True, return only positive values
        
    Returns:
        Random large float value (positive, negative, or both based on parameters)
    """
    # Define ranges based on precision
    if precision == "single":
        # IEEE 754 single precision ranges
        max_normal = 3.402823e+38
    else:  # double precision (default)
        # IEEE 754 double precision ranges  
        max_normal = 1.7976931348623157e+308
    
    # Generate positive large value
    large_value = random.uniform(max_normal / 1e10, max_normal)
    
    # Apply sign based on parameters

    if positive:
        return large_value
    else:
        return -large_value

def get_zero_and_special_floats() -> list[float]:
    """Get zero or special IEEE 754 floating-point values.
    
    Returns:
        Random special float value (±0, ±∞, NaN, unity values, constants)
    """
    return [
        0.0, -0.0,              # Positive and negative zero
        float('inf'),           # Positive infinity
        float('-inf'),          # Negative infinity
        1.0, -1.0,             # Simple unity values
        math.pi, -math.pi,      # Mathematical constants
        math.e, -math.e
    ]

def get_random_max_length_digits(length: int) -> int:
    """Get random integer between 0 and maximum value with given number of digits.
    
    Args:
        length: Number of digits (e.g., 4 means max value is 9999)
        
    Returns:
        Random integer between 0 and (10^length - 1)
        
    Examples:
        get_random_max_digits(1) -> random int between 0 and 9
        get_random_max_digits(4) -> random int between 0 and 9999
    """
    if length <= 0:
        raise ValueError("Length must be positive")
    
    max_value = (10 ** length) - 1
    return random.randint(0, max_value)

def get_nibble_max_digit(length: int) -> int:
    if length <= 0:
        raise ValueError("Length must be positive")

    return (10 ** length) - 1

def get_random_string_random_length(max_length: int) -> str:
    return get_random_string(random.randint(1, max_length))

def get_random_string(length: int) -> str:
    # Use printable ASCII characters (letters, digits, punctuation, whitespace)
    # Excludes control characters for easier debugging
    return ''.join(choice(printable.rstrip()) for _ in range(length))

def get_null_terminator_string_random_size(length: int) -> str:
    return ''.join('\0' for _ in range(random.randint(1, length)))

def get_null_terminator_string(length: int) -> str:
    return ''.join('\0' for _ in range(length))

# ============================================================================
# CHARACTER SET DEFINITIONS FOR ACN STRING TESTING
# ============================================================================

# Common character sets for ACN string encoding tests
BINARY_CHARSET = "01"
NUMERIC_CHARSET = "0123456789"
HEX_CHARSET = "0123456789ABCDEF"
HEX_LOWERCASE_CHARSET = "0123456789abcdef"
ALPHA_UPPERCASE_CHARSET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHA_LOWERCASE_CHARSET = "abcdefghijklmnopqrstuvwxyz"
ALPHA_CHARSET = ALPHA_UPPERCASE_CHARSET + ALPHA_LOWERCASE_CHARSET
ALPHANUMERIC_CHARSET = ALPHA_CHARSET + NUMERIC_CHARSET
ASCII_PRINTABLE_CHARSET = "".join(chr(i) for i in range(32, 127))  # Space to ~
DNA_CHARSET = "ATCG"
RNA_CHARSET = "AUCG"

# Specialized character sets
MORSE_CHARSET = ".-/ "  # Morse code characters
BOOLEAN_CHARSET = "TF"  # True/False
YESNO_CHARSET = "YN"   # Yes/No

def get_charset_bits_per_char(charset: str) -> int:
    """Calculate minimum bits needed per character for given charset."""
    import math
    return math.ceil(math.log2(len(charset))) if len(charset) > 1 else 1

def generate_test_string_random_length(charset: str, length: int) -> str:
    """Generate random test string from given character set."""
    return generate_test_string(charset, random.randint(1, length))

def generate_test_string(charset: str, length: int) -> str:
    """Generate random test string from given character set."""
    return ''.join(random.choice(charset) for _ in range(length))

def generate_test_string_with_null(charset: str, max_length: int) -> str:
    """Generate test string that ends with null terminator."""
    length = random.randint(1, max_length - 1)
    return generate_test_string(charset, length) + '\0'

def generate_ia5_string(length: int) -> str:
    """Generate test string with IA5 charset."""
    return ''.join(random.choice(str(IA5_CHAR_SET)) for _ in range (length))

def generate_ia5_string_random_length(length: int) -> str:
    """Generate test string with IA5 charset."""
    return generate_ia5_string(random.randint(1, length))

def charset_to_bytes(charset: str) -> bytes:
    """Convert character set string to bytes object for ACN encoding."""
    return charset.encode('ascii')

def get_min_sint_ascii(byte: int) -> int:
    """Get minimum signed integer value that can be encoded in ASCII with given byte size.
    
    Args:
        byte: Number of bytes available for ASCII encoding (includes 1 byte for sign)
        
    Returns:
        Minimum negative value that can be represented
        
    Examples:
        get_min_sint_ascii(2) -> -9 (sign + 1 digit)
        get_min_sint_ascii(3) -> -99 (sign + 2 digits)
        get_min_sint_ascii(4) -> -999 (sign + 3 digits)
    """
    if byte < 1:
        raise ValueError("Byte size must be at least 1 for sign character")
    
    # One byte is reserved for sign character ('+' or '-')
    # Remaining bytes can hold decimal digits
    digit_bytes = byte - 1
    
    if digit_bytes == 0:
        return 0  # Only sign, no digits possible
    
    # Maximum magnitude is 10^digit_bytes - 1
    # Return negative of this value
    max_magnitude = (10 ** digit_bytes) - 1
    return -max_magnitude

def get_max_sint_ascii(byte: int) -> int:
    """Get maximum signed integer value that can be encoded in ASCII with given byte size.
    
    Args:
        byte: Number of bytes available for ASCII encoding (includes 1 byte for sign)
        
    Returns:
        Maximum positive value that can be represented
        
    Examples:
        get_max_sint_ascii(2) -> 9 (sign + 1 digit)
        get_max_sint_ascii(3) -> 99 (sign + 2 digits)
        get_max_sint_ascii(4) -> 999 (sign + 3 digits)
    """
    if byte < 1:
        raise ValueError("Byte size must be at least 1 for sign character")
    
    # One byte is reserved for sign character ('+' or '-')
    # Remaining bytes can hold decimal digits
    digit_bytes = byte - 1
    
    if digit_bytes == 0:
        return 0  # Only sign, no digits possible
    
    # Maximum positive value is 10^digit_bytes - 1
    max_value = (10 ** digit_bytes) - 1
    return max_value

def get_random_sint_ascii(byte: int) -> int:
    """Get random signed integer that can be encoded in ASCII with given byte size.
    
    Args:
        byte: Number of bytes available for ASCII encoding (includes 1 byte for sign)
        
    Returns:
        Random signed integer within the valid range
        
    Examples:
        get_random_sint_ascii(2) -> random int between -9 and 9
        get_random_sint_ascii(3) -> random int between -99 and 99  
        get_random_sint_ascii(4) -> random int between -999 and 999
    """
    if byte < 1:
        raise ValueError("Byte size must be at least 1 for sign character")
    
    min_val = get_min_sint_ascii(byte)
    max_val = get_max_sint_ascii(byte)
    
    return random.randint(min_val, max_val)

def get_min_uint_ascii(byte: int) -> int:
    """Get minimum unsigned integer value that can be encoded in ASCII with given byte size.
    
    Args:
        byte: Number of bytes available for ASCII encoding (all bytes for digits)
        
    Returns:
        Always returns 0 (minimum unsigned value)
        
    Examples:
        get_min_uint_ascii(1) -> 0
        get_min_uint_ascii(3) -> 0
    """
    if byte < 1:
        raise ValueError("Byte size must be at least 1")
    
    return 0

def get_max_uint_ascii(byte: int) -> int:
    """Get maximum unsigned integer value that can be encoded in ASCII with given byte size.
    
    Args:
        byte: Number of bytes available for ASCII encoding (all bytes for digits)
        
    Returns:
        Maximum positive value that can be represented
        
    Examples:
        get_max_uint_ascii(1) -> 9 (1 digit)
        get_max_uint_ascii(2) -> 99 (2 digits)
        get_max_uint_ascii(3) -> 999 (3 digits)
    """
    if byte < 1:
        raise ValueError("Byte size must be at least 1")
    
    # All bytes can be used for decimal digits (no sign character)
    max_value = (10 ** byte) - 1
    return max_value

def get_random_uint_ascii(byte: int) -> int:
    """Get random unsigned integer that can be encoded in ASCII with given byte size.
    
    Args:
        byte: Number of bytes available for ASCII encoding (all bytes for digits)
        
    Returns:
        Random unsigned integer within the valid range
        
    Examples:
        get_random_uint_ascii(1) -> random int between 0 and 9
        get_random_uint_ascii(2) -> random int between 0 and 99  
        get_random_uint_ascii(3) -> random int between 0 and 999
    """
    if byte < 1:
        raise ValueError("Byte size must be at least 1")
    
    min_val = get_min_uint_ascii(byte)
    max_val = get_max_uint_ascii(byte)
    
    return random.randint(min_val, max_val)