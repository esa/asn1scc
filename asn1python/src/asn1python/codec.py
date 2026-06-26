"""
ASN.1 Python Runtime Library - Base Codec Framework

This module provides the base codec framework for ASN.1 encoding/decoding operations.
"""

from abc import ABC
from typing import Optional, Self, TypeVar, Generic
from dataclasses import dataclass
from enum import IntEnum
from .bitstream import BitStream, BitStreamError
from .asn1_exceptions import *


class ErrorCode(IntEnum):
    """Error codes for encoding/decoding operations"""
    SUCCESS = 0
    INSUFFICIENT_DATA = 1
    INVALID_VALUE = 2
    CONSTRAINT_VIOLATION = 3
    BUFFER_OVERFLOW = 4
    UNSUPPORTED_OPERATION = 5


# Constants for common error codes
ENCODE_OK = ErrorCode.SUCCESS
DECODE_OK = ErrorCode.SUCCESS
ERROR_INSUFFICIENT_DATA = ErrorCode.INSUFFICIENT_DATA
ERROR_INVALID_VALUE = ErrorCode.INVALID_VALUE
ERROR_CONSTRAINT_VIOLATION = ErrorCode.CONSTRAINT_VIOLATION
ERROR_BUFFER_OVERFLOW = ErrorCode.BUFFER_OVERFLOW
ERROR_UNSUPPORTED_OPERATION = ErrorCode.UNSUPPORTED_OPERATION


class Encoding(IntEnum):
    uPER = 0
    ACN = 1

@dataclass(frozen=True)
class EncodeResult:
    """Result of an encoding operation"""
    success: bool
    error_code: ErrorCode
    bits_encoded: int = 0
    error_message: Optional[str] = None

    def __bool__(self) -> bool:
        return self.success


TDVal = TypeVar('TDVal')

@dataclass(frozen=True)
class DecodeResult(Generic[TDVal]):
    """Result of a decoding operation"""
    success: bool
    error_code: ErrorCode
    decoded_value: Optional[TDVal] = None
    bits_consumed: int = 0
    error_message: Optional[str] = None

    def __bool__(self) -> bool:
        return self.success


class CodecError(Asn1Exception):
    """Base class for codec errors"""
    pass


class Codec(ABC):
    """
    Base class for ASN.1 codecs.

    This class provides common functionality for all ASN.1 encoding rules
    including UPER, ACN, XER, and BER.
    """

    def __init__(self, bitstream: BitStream) -> None:
        """
        Creates a new Codec from the provided Bitstream
        """
        self._bitstream = BitStream.from_bitstream(bitstream)

    @classmethod
    def from_codec(cls, codec: 'Codec') -> Self:
        instance = cls(codec._bitstream)
        return instance

    @classmethod
    def from_buffer(cls, buffer: bytearray) -> Self:        
        return cls(BitStream(buffer))

    @classmethod
    def of_size(cls, buffer_byte_size: int = 1024 * 1024) -> Self:
        """Create a new codec with a buffer of length buffer_byte_size."""        
        return cls.from_buffer(bytearray(buffer_byte_size))
    
    def get_bitstream_buffer(self) -> bytearray:
        return self._bitstream.get_data()

    @property
    def buffer_size(self) -> int:
        """Get the buffer size in bytes"""
        return self._bitstream.buffer_size

    @property
    def bit_index(self) -> int:
        """
        Get the current bit position in the bitstream.

        Matches C: BitStream.currentBit + BitStream.currentByte * 8
        Matches Scala: BitStream.bitIndex
        Used by: ACN for tracking position, calculating sizes

        Returns:
            Current bit position (0-based index)
        """
        return self._bitstream.current_used_bits
    
    @property
    def remaining_bits(self) -> int:
        return self._bitstream.remaining_bits