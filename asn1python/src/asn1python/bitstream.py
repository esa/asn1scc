"""
ASN.1 Python Runtime Library - BitStream Operations

This module provides bit-level reading and writing operations
that match the behavior of the C and Scala bitstream implementations.
"""

from typing import List
from .asn1_constants import NO_OF_BITS_IN_BYTE

class BitStreamError(Exception):
    """Base class for bitstream errors"""
    pass


class BitStream:
    """
    Bit-level reading and writing operations for ASN.1 encoding/decoding.

    This class provides precise bit manipulation capabilities required for
    ASN.1 encoding rules like UPER and ACN.
    """
    
    @staticmethod
    def position_invariant(bit_position: int, byte_position: int, buf_length: int) -> bool:
        return (bit_position >= 0 and bit_position < NO_OF_BITS_IN_BYTE and
                byte_position >= 0 and ((byte_position < buf_length) or (bit_position == 0 and byte_position == buf_length)))

    def __init__(self, data: bytearray):
        """
        Initialize a BitStream.

        Args:
            data: Initial data buffer
        """
        self._buffer = bytearray(data)
        self._current_bit = 0  # Current bit within byte (0-7)
        self._current_byte = 0  # Current byte position (0-based)

    @classmethod
    def from_bitstream(cls, other: 'BitStream') -> 'BitStream':
        """Method to create a BitStream from an existing BitStream. Copies buffer and segments"""
        result = cls(other._buffer)
        return result

    def get_data(self) -> bytearray:
        """Get the used data buffer"""
        used_bytes = self.current_used_bytes
        data = self._buffer[:used_bytes]
        return data
    
    def to_hex_string(self) -> str:
        """Convert the bitstream data to a hex string"""
        return self.get_data().hex()   

    #region Properties

    @property
    def current_bit_position(self) -> int:
        """Get the current bit position"""
        return self._current_bit

    @property
    def current_byte_position(self) -> int:
        """Get the current byte position"""
        return self._current_byte

    @property
    def buffer_size(self) -> int:
        """Get the buffer size in bytes"""
        return len(self._buffer)

    @property
    def current_used_bits(self) -> int:
        return self.current_byte_position * NO_OF_BITS_IN_BYTE + self.current_bit_position

    @property
    def current_used_bytes(self) -> int:
        return self.current_byte_position + (1 if self.current_bit_position > 0 else 0)

    @property
    def remaining_bits(self) -> int:
        return self.buffer_size * NO_OF_BITS_IN_BYTE - self.current_used_bits

    #endregion

    def set_position(self, bit_position: int, byte_position: int) -> None:
        """Set the current bit and byte position"""
        
        if not BitStream.position_invariant(bit_position, byte_position, self.buffer_size):
            raise BitStreamError(f"Position {byte_position}.{bit_position} out of range for buffer of size {self.buffer_size}")
        
        self._current_bit = bit_position
        self._current_byte = byte_position

    def reset(self) -> None:
        """Reset the bit position to the beginning"""
        self.set_position(0, 0)

    def set_bit_index(self, bit_index: int) -> None:
        """Set the current position from a bit index across the whole Bitstream"""
        bit_position = bit_index % NO_OF_BITS_IN_BYTE
        byte_position = bit_index // NO_OF_BITS_IN_BYTE
        self.set_position(bit_position, byte_position)

    def _shift_bit_index(self, count: int = 1) -> None:

        if count > self.remaining_bits:
            raise BitStreamError(f"Position out of range for buffer of size {self.buffer_size}")

        new_index = self.current_used_bits + count        
        self.set_bit_index(new_index)

    def write_align_to_byte(self) -> int:
        length = (NO_OF_BITS_IN_BYTE - self.current_bit_position) % NO_OF_BITS_IN_BYTE
        self._shift_bit_index(length)
        return length
    
    def read_align_to_byte(self) -> int:
        length = (NO_OF_BITS_IN_BYTE - self.current_bit_position) % NO_OF_BITS_IN_BYTE
        self._shift_bit_index(length)   
        return length

    #region Read
    
    def __read_current_bit_pure(self) -> bool:
        return bool((self._buffer[self._current_byte] >> (7 - self._current_bit)) % 2)
    
    def __read_bit(self) -> bool:        
        if self.remaining_bits < 1:
            raise BitStreamError("Cannot read beyond end of bitstream")
        
        res = self.__read_current_bit_pure()
        self._shift_bit_index(1)
        return res
    
    def read_bit(self) -> bool:
        """Read a single bit"""
        return self.__read_bit()

    def read_bits(self, bit_count: int) -> int:
        """Read multiple bits and return as integer"""
        if bit_count < 0 or bit_count > 64:
            raise BitStreamError(f"Bit count {bit_count} out of range [0, 64]")

        if self.remaining_bits < bit_count:
            raise BitStreamError("Cannot read beyond end of bitstream")

        value = 0
        i = 0
        while i < bit_count:
            next_bit = int(self.__read_bit())
            value = (value << 1) + next_bit
            i = i + 1

        return value

    def read_byte(self) -> int:
        """Read a complete byte"""
        return self.read_bits(8)

    #endregion
    #region Write

    def __byte_set_bit(self, byte: int, bit: bool, position: int) -> int:
        if bit:
            return byte | (1 << (7 - position))
        else:
            return byte & ~(1 << (7 - position))

    # cur bit = 3
    #
    # |x|x|x|b|?|?|?|?|
    #  0 1 2 3 4 5 6 7
    def __write_bit(self, bit: bool) -> None:
        """Write a single bit"""        
        val = self._buffer[self._current_byte]
        self._buffer[self._current_byte] = self.__byte_set_bit(val, bit, self._current_bit)
        
        self._shift_bit_index(1)
        
    def write_bit(self, bit: bool) -> None:
        if self.remaining_bits < 1:
            raise BitStreamError("Cannot write beyond end of bitstream")
        
        return self.__write_bit(bit)

    def write_bits(self, value: int, bit_count: int) -> None:
        """Write multiple bits from an integer value"""
        if bit_count < 0 or bit_count > 64:
            raise BitStreamError(f"Bit count {bit_count} out of range [0, 64]")

        if self.remaining_bits < bit_count:
            raise BitStreamError("Cannot write beyond end of bitstream")

        # Check if value fits in bit_count bits
        if value < 0 or value >= (1 << bit_count):
            raise BitStreamError(f"Value {value} does not fit in {bit_count} bits")
        
        i: int = 0

        while i < bit_count:
            bit = bool((value >> (bit_count - 1 - i)) % 2)
            self.__write_bit(bit)
            i = i + 1

    def write_byte(self, byte_value: int) -> None:
        """Write a complete byte"""
        if byte_value < 0 or byte_value > 255:
            raise BitStreamError(f"Byte value {byte_value} out of range [0, 255]")

        self.write_bits(byte_value, 8)

    #endregion

    def __str__(self) -> str:
        """String representation for debugging"""
        return f"BitStream(size={self.buffer_size} bytes, pos={self.current_used_bits}, data={self._buffer[:self.current_used_bytes].hex()})"

    def to_binary_string(self) -> str:
        """Convert the bitstream data to a binary string"""
        result: List[str] = []
        for byte in self._buffer[:self.current_used_bytes]:
            result.append(f"{byte:08b}")
        return "".join(result)[:self.current_used_bits]