"""
ASN.1 Python Runtime Library - Helper Functions

This module provides utility functions for ASN.1 operations.
"""

from typing import List, Union, Optional

def calculate_asn1_int_size_in_bytes(value: int) -> int:
    """
    Calculate the minimum number of bytes needed to represent an ASN.1 integer.

    This function determines the size needed for ASN.1 INTEGER encoding,
    taking into account the sign bit for two's complement representation.

    Args:
        value: The integer value to analyze

    Returns:
        The minimum number of bytes needed
    """
    if value == 0:
        return 1

    if value > 0:
        # Positive number: need one extra bit for sign if MSB would be 1
        bit_length = value.bit_length()
        # Add 1 for sign bit, then round up to bytes
        return (bit_length + 8) // 8
    else:
        # Negative number: use two's complement
        # For negative numbers, we need enough bits to represent the magnitude
        # plus one for the sign bit
        bit_length = (abs(value) - 1).bit_length()
        return (bit_length + 8) // 8


def get_number_of_bits_for_non_negative_integer(value: int) -> int:
    """
    Get the number of bits needed to represent a non-negative integer.

    This is commonly used for determining encoding sizes in ASN.1.

    Args:
        value: The non-negative integer value

    Returns:
        The number of bits needed

    Raises:
        ValueError: If value is negative
    """
    if value < 0:
        raise ValueError(f"Value must be non-negative, got {value}")

    if value == 0:
        return 1

    return value.bit_length()


def bit_string_to_byte_array(bit_string: str) -> bytearray:
    """
    Convert a bit string (e.g., "10110100") to a byte array.

    The bit string is padded to byte boundaries if necessary.

    Args:
        bit_string: String containing only '0' and '1' characters

    Returns:
        Byte array representation of the bit string

    Raises:
        ValueError: If bit_string contains invalid characters
    """
    if not bit_string:
        return bytearray()

    # Validate bit string
    if not all(c in '01' for c in bit_string):
        raise ValueError("Bit string must contain only '0' and '1' characters")

    # Pad to byte boundary
    padded_length = ((len(bit_string) + 7) // 8) * 8
    padded_bits = bit_string.ljust(padded_length, '0')

    # Convert to bytes
    result = bytearray()
    for i in range(0, len(padded_bits), 8):
        byte_bits = padded_bits[i:i+8]
        byte_value = int(byte_bits, 2)
        result.append(byte_value)

    return result


def byte_array_to_bit_string(byte_array: Union[bytearray, bytes], 
                            bit_length: Optional[int] = None) -> str:
    """
    Convert a byte array to a bit string.

    Args:
        byte_array: The bytes to convert
        bit_length: Optional length to truncate the result to

    Returns:
        Bit string representation
    """
    if not byte_array:
        return ""

    # Convert each byte to 8-bit binary string
    bit_string = ""
    for byte in byte_array:
        bit_string += f"{byte:08b}"

    # Truncate to specified length if provided
    if bit_length is not None:
        bit_string = bit_string[:bit_length]

    return bit_string


def calculate_bits_for_range(min_val: int, max_val: int) -> int:
    """
    Calculate the number of bits needed to represent a range of values.

    This is commonly used in ASN.1 encoding for constrained integers.

    Args:
        min_val: Minimum value in the range
        max_val: Maximum value in the range

    Returns:
        Number of bits needed

    Raises:
        ValueError: If min_val > max_val
    """
    if min_val > max_val:
        raise ValueError(f"min_val ({min_val}) must be <= max_val ({max_val})")

    range_size = max_val - min_val + 1

    if range_size == 1:
        return 0  # Single value needs no bits

    return (range_size - 1).bit_length()


def align_to_byte_boundary(bit_position: int) -> int:
    """
    Calculate the next byte boundary position.

    Args:
        bit_position: Current bit position

    Returns:
        Next position aligned to byte boundary
    """
    return ((bit_position + 7) // 8) * 8


def is_power_of_two(n: int) -> bool:
    """
    Check if a number is a power of two.

    Args:
        n: Number to check

    Returns:
        True if n is a power of two, False otherwise
    """
    return n > 0 and (n & (n - 1)) == 0


def encode_length_determinant(length: int) -> List[int]:
    """
    Encode a length determinant according to ASN.1 rules.

    Args:
        length: The length value to encode

    Returns:
        List of bytes representing the encoded length

    Raises:
        ValueError: If length is negative or too large
    """
    if length < 0:
        raise ValueError(f"Length cannot be negative: {length}")

    if length < 128:
        # Short form: 0xxxxxxx
        return [length]
    elif length < 256:
        # Long form: 10000001 xxxxxxxx
        return [0x81, length]
    elif length < 65536:
        # Long form: 10000010 xxxxxxxx xxxxxxxx
        return [0x82, (length >> 8) & 0xFF, length & 0xFF]
    else:
        raise ValueError(f"Length {length} too large for encoding")


def decode_length_determinant(data: Union[List[int], bytearray, bytes], 
                             offset: int = 0) -> tuple:
    """
    Decode a length determinant from ASN.1 encoded data.

    Args:
        data: The encoded data
        offset: Starting offset in the data

    Returns:
        Tuple of (length_value, bytes_consumed)

    Raises:
        ValueError: If data is insufficient or invalid
    """
    if offset >= len(data):
        raise ValueError("Insufficient data for length determinant")

    first_byte = data[offset]

    if (first_byte & 0x80) == 0:
        # Short form: 0xxxxxxx
        return (first_byte, 1)
    else:
        # Long form: 1xxxxxxx indicates number of octets
        length_octets = first_byte & 0x7F

        if length_octets == 0:
            raise ValueError("Indefinite length not supported")

        if offset + 1 + length_octets > len(data):
            raise ValueError("Insufficient data for long form length")

        length_value = 0
        for i in range(length_octets):
            length_value = (length_value << 8) | data[offset + 1 + i]

        return (length_value, 1 + length_octets)


def calculate_string_size_in_bits(string_value: str, 
                                 char_size_in_bits: int) -> int:
    """
    Calculate the size in bits needed for a string value.

    Args:
        string_value: The string to measure
        char_size_in_bits: Number of bits per character

    Returns:
        Total size in bits
    """
    return len(string_value) * char_size_in_bits


def pad_to_byte_boundary(data: bytearray, current_bit_length: int) -> bytearray:
    """
    Pad data to the next byte boundary.

    Args:
        data: The data to pad
        current_bit_length: Current length in bits

    Returns:
        Padded data
    """
    if current_bit_length % 8 == 0:
        return data  # Already aligned

    # Calculate padding needed
    padding_bits = 8 - (current_bit_length % 8)

    # Add padding bits (zeros) to the last byte
    if data:
        # Shift existing bits in last byte to make room for padding
        last_byte = data[-1]
        data[-1] = last_byte & ~((1 << padding_bits) - 1)

    return data


def validate_bit_string(bit_string: str) -> bool:
    """
    Validate that a string contains only valid bit characters.

    Args:
        bit_string: String to validate

    Returns:
        True if valid, False otherwise
    """
    if not bit_string:
        return True  # Empty string is valid

    return all(c in '01' for c in bit_string)


def reverse_bits_in_byte(byte_value: int) -> int:
    """
    Reverse the order of bits in a byte.

    Args:
        byte_value: Byte value (0-255)

    Returns:
        Byte with reversed bit order

    Raises:
        ValueError: If byte_value is out of range
    """
    if not (0 <= byte_value <= 255):
        raise ValueError(f"Byte value {byte_value} out of range [0, 255]")

    result = 0
    for i in range(8):
        if byte_value & (1 << i):
            result |= (1 << (7 - i))

    return result


def int_to_bytes(value: int, length: int, signed: bool = False) -> bytearray:
    """
    Convert an integer to a byte array with specified length.

    Args:
        value: Integer value to convert
        length: Number of bytes in result
        signed: Whether to use signed representation

    Returns:
        Byte array representation

    Raises:
        ValueError: If value doesn't fit in specified length
    """
    if signed:
        # Two's complement representation
        if value >= 0:
            max_val = (1 << (length * 8 - 1)) - 1
            if value > max_val:
                raise ValueError(f"Value {value} too large for {length} signed bytes")
        else:
            min_val = -(1 << (length * 8 - 1))
            if value < min_val:
                raise ValueError(f"Value {value} too small for {length} signed bytes")

            # Convert to unsigned representation
            value = (1 << (length * 8)) + value
    else:
        # Unsigned representation
        if value < 0:
            raise ValueError(f"Negative value {value} not allowed for unsigned bytes")

        max_val = (1 << (length * 8)) - 1
        if value > max_val:
            raise ValueError(f"Value {value} too large for {length} unsigned bytes")

    # Convert to bytes (big-endian)
    result = bytearray()
    for i in range(length - 1, -1, -1):
        result.append((value >> (i * 8)) & 0xFF)

    return result


def bytes_to_int(data: Union[bytearray, bytes], signed: bool = False) -> int:
    """
    Convert a byte array to an integer.

    Args:
        data: Byte array to convert
        signed: Whether to interpret as signed

    Returns:
        Integer value
    """
    if not data:
        return 0

    # Convert from big-endian bytes
    result = 0
    for byte in data:
        result = (result << 8) | byte

    if signed and len(data) > 0:
        # Check if sign bit is set
        sign_bit = 1 << (len(data) * 8 - 1)
        if result & sign_bit:
            # Convert from two's complement
            result = result - (1 << (len(data) * 8))

    return result

class Asn1SccError:
    def __init__(self, error_code: int):
        self.error_code = error_code

    def get_error_code(self) -> int:
        return self.error_code