"""
ASN.1 Python Runtime Library - ACN Encoder

This module provides ACN (ASN.1 Control Notation) encoding functionality.
ACN allows custom binary encodings for ASN.1 types to support legacy protocols.
"""

import struct

from .acn_decoder import ACNDecoder
from .bitstream import BitStreamError
from .codec import EncodeResult, ENCODE_OK, ERROR_INVALID_VALUE
from .encoder import Encoder


# Global IA5 character set (International Alphabet No. 5 - 7-bit ASCII 0-127)
# Defined with individual byte values to match Scala reference implementation
IA5_CHAR_SET = bytes([
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
    0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
    0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
    0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
    0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
    0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
    0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
    0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
    0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
    0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
    0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
    0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
])


class ACNEncoder(Encoder):
    """
    ACN (ASN.1 Control Notation) encoder implementation.

    This encoder provides flexible binary encoding for ASN.1 types
    following custom ACN encoding rules to support legacy protocols.
    """

    def __init__(self, buffer: bytearray) -> None:
        super().__init__(buffer)

    @classmethod
    def of_size(cls, buffer_byte_size: int = 1024 * 1024) -> 'ACNEncoder':
        return cls(bytearray(buffer_byte_size))

    @classmethod
    def _construct(cls, buffer: bytearray) -> 'ACNEncoder':
        return cls(buffer)

    def get_decoder(self) -> ACNDecoder:
        return ACNDecoder(self.get_bitstream_buffer())

    # ============================================================================
    # INTEGER ENCODING - POSITIVE INTEGER
    # ============================================================================

    def enc_int_positive_integer_const_size(self, int_val: int,
                                            encoded_size_in_bits: int) -> EncodeResult:
        """Encode positive integer with constant size in bits."""
        return self.encode_integer(int_val, min_val=0, max_val=(1 << encoded_size_in_bits) - 1, size_in_bits=encoded_size_in_bits)

    def enc_int_positive_integer_var_size_length_embedded(self, int_val: int) -> EncodeResult:
        """Encode positive integer with variable size (length embedded)."""
        if int_val < 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Value {int_val} must be non-negative"
            )

        try:
            if int_val == 0:
                bytes_needed = 1
            else:
                bytes_needed = (int_val.bit_length() + 7) // 8


            length_result = self.enc_length(bytes_needed, 8)
            if not length_result.success:
                return length_result

            bits_encoded = length_result.bits_encoded

            # Use base append_byte method instead of direct bitstream access
            for i in range(bytes_needed - 1, -1, -1):
                byte_val = (int_val >> (i * 8)) & 0xFF
                result = self.append_byte(byte_val)
                if not result.success:
                    return result
                bits_encoded += 8

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_int_positive_integer_const_size_big_endian(self, int_val: int, num_bits: int) -> EncodeResult:
        """Encode positive integer with constant size in big-endian byte order.

        Args:
            int_val: Unsigned integer value to encode
            num_bits: Number of bits to encode (must be multiple of 8: 16, 32, 64)
        """
        if num_bits % 8 != 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )
        return self.encode_integer_big_endian(int_val, num_bits, False)

    def enc_int_positive_integer_const_size_little_endian(self, int_val: int, num_bits: int) -> EncodeResult:
        """Encode positive integer with constant size in little-endian byte order.

        Args:
            int_val: Unsigned integer value to encode
            num_bits: Number of bits to encode (must be multiple of 8: 16, 32, 64)
        """
        if num_bits % 8 != 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )
        return self._encode_integer_little_endian(int_val, num_bits, False)

    # ============================================================================
    # INTEGER ENCODING - TWO'S COMPLEMENT
    # ============================================================================

    def enc_int_twos_complement_const_size(self, int_val: int, format_bit_length: int) -> EncodeResult:
        """Encode signed integer using two's complement with constant size.

        Based on C implementation (asn1crt_encoding_acn.c:316-328):
        - For positive values: encode as unsigned with leading zeros
        - For negative values: encode two's complement representation
        """
        try:
            # Convert to two's complement representation for negative values
            if int_val < 0:
                # Two's complement: invert bits and add 1, but we can use modulo arithmetic
                unsigned_val = (1 << format_bit_length) + int_val
            else:
                unsigned_val = int_val

            # Encode as unsigned integer
            return self.encode_unsigned_integer(unsigned_val, format_bit_length)

        except Exception as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_int_twos_complement_const_size_big_endian(self, int_val: int, num_bits: int) -> EncodeResult:
        """Encode signed integer (two's complement) with constant size in big-endian byte order.

        Args:
            int_val: Signed integer value to encode
            num_bits: Number of bits to encode (must be multiple of 8: 16, 32, 64)
        """
        if num_bits % 8 != 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )
        return self.encode_integer_big_endian(int_val, num_bits, True)

    def enc_int_twos_complement_const_size_little_endian(self, int_val: int, num_bits: int) -> EncodeResult:
        """Encode signed integer (two's complement) with constant size in little-endian byte order.

        Args:
            int_val: Signed integer value to encode
            num_bits: Number of bits to encode (must be multiple of 8: 16, 32, 64)
        """
        if num_bits % 8 != 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )
        return self._encode_integer_little_endian(int_val, num_bits, True)

    def enc_int_twos_complement_var_size_length_embedded(self, int_val: int) -> EncodeResult:
        """Encode signed integer with variable size (length embedded)."""
        try:
            if int_val >= 0:
                bit_length = int_val.bit_length()
                bytes_needed = (bit_length + 8) // 8
            else:
                bit_length = (abs(int_val) - 1).bit_length()
                bytes_needed = (bit_length + 8) // 8

            if bytes_needed == 0:
                bytes_needed = 1

            length_result = self.enc_length(bytes_needed, 8)
            if not length_result.success:
                return length_result

            bits_encoded = length_result.bits_encoded

            if int_val >= 0:
                twos_complement = int_val
            else:
                twos_complement = (1 << (bytes_needed * 8)) + int_val

            # Use base append_byte method
            for i in range(bytes_needed - 1, -1, -1):
                byte_val = (twos_complement >> (i * 8)) & 0xFF
                result = self.append_byte(byte_val)
                if not result.success:
                    return result
                bits_encoded += 8

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # INTEGER ENCODING - BCD (Binary Coded Decimal)
    # ============================================================================

    def enc_int_bcd_const_size(self, int_val: int, encoded_size_in_nibbles: int) -> EncodeResult:
        """Encode integer in BCD format with constant size in nibbles."""
        if int_val < 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"BCD encoding requires non-negative value, got {int_val}"
            )

        try:
            digits = []
            temp_val = int_val
            while temp_val > 0:
                digits.append(temp_val % 10)
                temp_val //= 10

            while len(digits) < encoded_size_in_nibbles:
                digits.append(0)

            if len(digits) > encoded_size_in_nibbles:
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Value {int_val} requires more than {encoded_size_in_nibbles} BCD digits"
                )

            bits_encoded = 0
            for i in range(encoded_size_in_nibbles - 1, -1, -1):
                result = self.encode_unsigned_integer(digits[i], 4)
                if not result.success:
                    return result
                bits_encoded += 4

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_int_bcd_var_size_length_embedded(self, int_val: int) -> EncodeResult:
        """Encode integer in BCD format with variable size (length embedded)."""
        if int_val < 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"BCD encoding requires non-negative value, got {int_val}"
            )

        try:
            if int_val == 0:
                nibbles_needed = 1
            else:
                nibbles_needed = 0
                temp_val = int_val
                while temp_val > 0:
                    nibbles_needed += 1
                    temp_val //= 10

            length_result = self.enc_length(nibbles_needed, 8)
            if not length_result.success:
                return length_result

            bcd_result = self.enc_int_bcd_const_size(int_val, nibbles_needed)
            if not bcd_result.success:
                return bcd_result

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=length_result.bits_encoded + bcd_result.bits_encoded
            )
        except Exception as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_int_bcd_var_size_null_terminated(self, int_val: int) -> EncodeResult:
        """Encode integer in BCD format with null termination (0xF)."""
        if int_val < 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"BCD encoding requires non-negative value, got {int_val}"
            )

        try:
            if int_val == 0:
                nibbles_needed = 1
            else:
                nibbles_needed = 0
                temp_val = int_val
                while temp_val > 0:
                    nibbles_needed += 1
                    temp_val //= 10

            bcd_result = self.enc_int_bcd_const_size(int_val, nibbles_needed)
            if not bcd_result.success:
                return bcd_result

            result = self.encode_unsigned_integer(0xF, 4)
            if not result.success:
                return result

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bcd_result.bits_encoded + 4
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # REAL ENCODING - IEEE 754
    # ============================================================================

    def enc_real_ieee754_32_big_endian(self, real_val: float) -> EncodeResult:
        """Encode 32-bit IEEE 754 float (big-endian).
        
        Args:
            real_val: Single-precision float (32-bit) - Python will truncate from double precision
        """
        try:
            packed = struct.pack('>f', real_val)
            # Use base append_byte_array method
            result = self.append_byte_array(packed, len(packed))
            if not result.success:
                return result

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=32
            )
        except (BitStreamError, struct.error) as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_real_ieee754_32_little_endian(self, real_val: float) -> EncodeResult:
        """Encode 32-bit IEEE 754 float (little-endian).
        
        Args:
            real_val: Single-precision float (32-bit) - Python will truncate from double precision
        """
        try:
            packed = struct.pack('<f', real_val)
            # Use base append_byte_array method
            result = self.append_byte_array(packed, len(packed))
            if not result.success:
                return result

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=32
            )
        except (BitStreamError, struct.error) as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_real_ieee754_64_big_endian(self, real_val: float) -> EncodeResult:
        """Encode 64-bit IEEE 754 double (big-endian).
        
        Args:
            real_val: Double-precision float (64-bit) - Python's native float precision
        """
        try:
            packed = struct.pack('>d', real_val)
            # Use base append_byte_array method
            result = self.append_byte_array(packed, len(packed))
            if not result.success:
                return result

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=64
            )
        except (BitStreamError, struct.error) as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_real_ieee754_64_little_endian(self, real_val: float) -> EncodeResult:
        """Encode 64-bit IEEE 754 double (little-endian).
        
        Args:
            real_val: Double-precision float (64-bit) - Python's native float precision
        """
        try:
            packed = struct.pack('<d', real_val)
            # Use base append_byte_array method
            result = self.append_byte_array(packed, len(packed))
            if not result.success:
                return result

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=64
            )
        except (BitStreamError, struct.error) as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # LENGTH ENCODING
    # ============================================================================

    def enc_length(self, length_val: int, length_size_in_bits: int) -> EncodeResult:
        """Encode length value with specified size in bits."""
        if length_val < 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Length cannot be negative: {length_val}"
            )

        max_value = (1 << length_size_in_bits) - 1
        if length_val > max_value:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Length {length_val} exceeds {length_size_in_bits} bits"
            )

        try:
            return self.encode_unsigned_integer(length_val, length_size_in_bits)
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # STRING ENCODING
    # ============================================================================

    def enc_string_ascii_fix_size(self, max_len: int, str_val: str) -> EncodeResult:
        """Encode ASCII string with fixed size.
        
        Encodes exactly max_len characters, padding with nulls if necessary.
        
        Args:
            max_len: Number of characters to encode (fixed size)
            str_val: String value to encode
        """
        if not isinstance(str_val, str):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="String value must be a string"
            )
            
        try:
            # Convert string to bytes using ASCII encoding
            str_bytes = str_val.encode('ascii')
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        
        if len(str_bytes) > max_len:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String length {len(str_bytes)} exceeds max_len {max_len}"
            )
            
        try:
            # Use common string writing logic
            chars_written, bits_encoded = self._enc_string_ascii_private(max_len, str_val)
            
            # Pad remaining bytes with null terminators (fixed size specific behavior)
            for i in range(chars_written, max_len):
                result = self.append_byte(0)
                if not result.success:
                    return result
                bits_encoded += 8
                
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_string_ascii_null_terminated(self, max_len: int, null_character: int, str_val: str) -> EncodeResult:
        """Encode ASCII string with null termination.
        
        Encodes string characters up to null terminator or max_len, then appends null_character.
        
        Args:
            max_len: Maximum number of characters to encode from string
            null_character: Null termination character (0-255)
            str_val: String value to encode
        """
        if not isinstance(str_val, str):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="String value must be a string"
            )
            
        if not (0 <= null_character <= 255):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Null character must be 0-255, got {null_character}"
            )
            
        try:
            # Use common string writing logic
            chars_written, bits_encoded = self._enc_string_ascii_private(max_len, str_val)
            
            # Append the null termination character (null terminated specific behavior)
            result = self.append_byte(null_character)
            if not result.success:
                return result
            bits_encoded += 8
                
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_string_ascii_null_terminated_mult(self, max_len: int, null_characters: bytearray, str_val: str) -> EncodeResult:
        """Encode ASCII string with multiple null characters.
        
        Encodes string characters up to null terminator or max_len, then appends null_characters sequence.
        
        Args:
            max_len: Maximum number of characters to encode from string
            null_characters: Null termination byte sequence
            str_val: String value to encode
        """
        if not isinstance(str_val, str):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="String value must be a string"
            )
            
        if not isinstance(null_characters, (bytes, bytearray)):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Null characters must be bytes or bytearray"
            )
            
        try:
            # Use common string writing logic
            chars_written, bits_encoded = self._enc_string_ascii_private(max_len, str_val)
            
            # Append the null termination character sequence (multi-byte null terminated specific behavior)
            result = self.append_byte_array(null_characters, len(null_characters))
            if not result.success:
                return result
            bits_encoded += len(null_characters) * 8
                
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_string_ascii_external_field_determinant(self, max_len: int, str_val: str) -> EncodeResult:
        """Encode ASCII string with external field determinant.
        
        Encodes string characters without length prefix (length determined externally).
        Stops at null terminator or max_len.
        
        Args:
            max_len: Maximum number of characters to encode from string
            str_val: String value to encode
        """
        if not isinstance(str_val, str):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="String value must be a string"
            )
            
        try:
            # Use common string writing logic (external field determinant has no additional behavior)
            chars_written, bits_encoded = self._enc_string_ascii_private(max_len, str_val)
                
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_string_ascii_internal_field_determinant(self, max_len: int, min_len: int, str_val: str) -> EncodeResult:
        """Encode ASCII string with internal field determinant.
        
        Encodes the string length first as a constrained integer, then the string characters.
        
        Args:
            max_len: Maximum string length
            min_len: Minimum string length
            str_val: String value to encode
        """
        if not isinstance(str_val, str):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="String value must be a string"
            )
            
        if min_len > max_len:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"min_len {min_len} cannot exceed max_len {max_len}"
            )
            
        try:
            # Convert string to bytes using ASCII encoding
            str_bytes = str_val.encode('ascii')
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
            
        # Calculate effective string length (up to first null or max_len)
        effective_len = 0
        for i in range(min(len(str_bytes), max_len)):
            if str_bytes[i] == 0:  # Stop at first null character
                break
            effective_len += 1
            
        if effective_len < min_len:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String length {effective_len} is less than min_len {min_len}"
            )
            
        try:
            # Encode length as constrained integer
            length_result = self.encode_integer(effective_len, min_val=min_len, max_val=max_len)
            if not length_result.success:
                return length_result
                
            bits_encoded = length_result.bits_encoded
            
            # Use common string writing logic
            chars_written, string_bits = self._enc_string_ascii_private(max_len, str_val)
            bits_encoded += string_bits
                
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_string_char_index_fix_size(self, max_len: int, allowed_char_set: bytearray, str_val: str) -> EncodeResult:
        """Encode string using character index with fixed size.
        
        Each character is encoded as its index position in the allowed_char_set,
        using the minimum bits needed to represent all possible indices.
        Encodes exactly max_len characters, padding with index 0 if needed.
        
        Args:
            max_len: Number of characters to encode (fixed size)
            allowed_char_set: bytearray containing allowed characters
            str_val: String value to encode
        """
        if not isinstance(allowed_char_set, (bytes, bytearray)):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Allowed character set must be bytes or bytearray"
            )
            
        if len(allowed_char_set) == 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Allowed character set cannot be empty"
            )
            
        try:
            # Fixed size method: encode exactly max_len characters (do NOT stop at null terminators)
            str_bytes = str_val.encode('ascii')
            char_set_size = len(allowed_char_set)
            bits_per_char = self._get_bits_per_char(char_set_size)
            bits_encoded = 0
            
            # Always encode exactly max_len characters
            for i in range(max_len):
                if i < len(str_bytes):
                    # Find character index in allowed set (including null terminators if they exist)
                    char_index = self._get_char_index(str_bytes[i], allowed_char_set)
                    if char_index == -1:
                        return EncodeResult(
                            success=False,
                            error_code=ERROR_INVALID_VALUE,
                            error_message=f"Character '{chr(str_bytes[i])}' (0x{str_bytes[i]:02x}) not found in allowed character set"
                        )
                else:
                    # Pad with index 0 (first character in set)
                    char_index = 0
                
                # Encode index as constrained integer (0 to char_set_size-1)
                result = self.encode_unsigned_integer(char_index, bits_per_char)
                if not result.success:
                    return result
                bits_encoded += bits_per_char
                    
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_string_char_index_external_field_determinant(self, max_len: int, allowed_char_set: bytearray, str_val: str) -> EncodeResult:
        """Encode string using character index with external field determinant.
        
        Each character is encoded as its index position in the allowed_char_set.
        Length is determined externally (no length encoding, no padding).
        
        Args:
            max_len: Maximum number of characters to encode
            allowed_char_set: bytearray containing allowed characters
            str_val: String value to encode
        """
        if not isinstance(allowed_char_set, (bytes, bytearray)):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Allowed character set must be bytes or bytearray"
            )
            
        if len(allowed_char_set) == 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Allowed character set cannot be empty"
            )
            
        try:
            # Use common character index encoding logic (external field determinant has no additional behavior)
            chars_written, bits_encoded = self._enc_string_char_index_private(max_len, allowed_char_set, str_val)
                
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_string_char_index_internal_field_determinant(self, max_len: int, allowed_char_set: bytearray, min_len: int, str_val: str) -> EncodeResult:
        """Encode string using character index with internal field determinant.
        
        Encodes the string length first as a constrained integer, then each character
        as its index position in the allowed_char_set.
        
        Args:
            max_len: Maximum string length
            allowed_char_set: bytearray containing allowed characters
            min_len: Minimum string length
            str_val: String value to encode
        """
        if not isinstance(allowed_char_set, (bytes, bytearray)):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Allowed character set must be bytes or bytearray"
            )
            
        if len(allowed_char_set) == 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Allowed character set cannot be empty"
            )
            
        if min_len > max_len:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"min_len {min_len} cannot exceed max_len {max_len}"
            )
            
        try:
            # Calculate effective string length (up to first null or max_len) for validation
            str_bytes = str_val.encode('ascii')
            effective_len = 0
            for i in range(min(len(str_bytes), max_len)):
                if str_bytes[i] == 0:  # Stop at first null character
                    break
                effective_len += 1
                
            if effective_len < min_len:
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"String length {effective_len} is less than min_len {min_len}"
                )
                
            # Encode length as constrained integer (internal field determinant specific behavior)
            length_result = self.encode_integer(effective_len, min_val=min_len, max_val=max_len)
            if not length_result.success:
                return length_result
                
            bits_encoded = length_result.bits_encoded
            
            # Use common character index encoding logic
            chars_written, string_bits = self._enc_string_char_index_private(max_len, allowed_char_set, str_val)
            bits_encoded += string_bits
                
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_ia5_string_char_index_external_field_determinant(self, max_len: int, str_val: str) -> EncodeResult:
        """Encode IA5 string using character index with external field determinant.
        
        IA5 (International Alphabet No. 5) is equivalent to 7-bit ASCII (0-127).
        Uses the full IA5 character set for character index encoding.
        
        Args:
            max_len: Maximum number of characters to encode
            str_val: String value to encode
        """
        try:
            chars_written, bits_encoded = self._enc_string_char_index_private(max_len, IA5_CHAR_SET, str_val)
            
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except (ValueError, BitStreamError) as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_ia5_string_char_index_internal_field_determinant(self, max_len: int, min_len: int, str_val: str) -> EncodeResult:
        """Encode IA5 string using character index with internal field determinant.
        
        IA5 (International Alphabet No. 5) is equivalent to 7-bit ASCII (0-127).
        Uses the full IA5 character set for character index encoding.
        
        Args:
            max_len: Maximum string length
            min_len: Minimum string length
            str_val: String value to encode
        """
        if min_len > max_len:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"min_len {min_len} cannot exceed max_len {max_len}"
            )
            
        try:
            str_bytes = str_val.encode('ascii')
            effective_len = 0
            for i in range(min(len(str_bytes), max_len)):
                if str_bytes[i] == 0:  # Stop at first null character
                    break
                effective_len += 1
                
            if effective_len < min_len:
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"String length {effective_len} is less than min_len {min_len}"
                )
                
            # Encode length as constrained integer (internal field determinant specific behavior)
            length_result = self.encode_integer(effective_len, min_val=min_len, max_val=max_len)
            if not length_result.success:
                return length_result
                
            bits_encoded = length_result.bits_encoded
            
            # Call private method directly (matches C/Scala implementation pattern)
            chars_written, string_bits = self._enc_string_char_index_private(max_len, IA5_CHAR_SET, str_val)
            bits_encoded += string_bits
                
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except UnicodeEncodeError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"String contains non-ASCII characters: {e}"
            )
        except (ValueError, BitStreamError) as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # ASCII INTEGER ENCODING - SIGNED
    # ============================================================================

    def enc_sint_ascii_const_size(self, int_val: int, encoded_size_in_bytes: int) -> EncodeResult:
        """Encode signed integer as ASCII with constant size."""
        if encoded_size_in_bytes < 1:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Encoded size must be at least 1 byte, got {encoded_size_in_bytes}"
            )

        try:
            # Encode sign
            sign_char = ord('+') if int_val >= 0 else ord('-')
            result = self.append_byte(sign_char)
            if not result.success:
                return result
            
            # Encode absolute value with remaining bytes
            abs_val = abs(int_val)
            remaining_bytes = encoded_size_in_bytes - 1
            
            # Use the unsigned ASCII encoding for the absolute value
            return self._enc_uint_ascii_const_size_impl(abs_val, remaining_bytes, total_bits=encoded_size_in_bytes * 8)
            
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_sint_ascii_var_size_length_embedded(self, int_val: int) -> EncodeResult:
        """Encode signed integer as ASCII with variable size (length embedded)."""
        try:
            # Get digits for absolute value
            abs_val = abs(int_val)
            digits_str = str(abs_val)
            
            # Total length includes sign character + digits
            total_length = len(digits_str) + 1
            
            # Encode length first (1 byte)
            result = self.append_byte(total_length)
            if not result.success:
                return result
            bits_encoded = 8

            # Encode sign
            sign_char = ord('+') if int_val >= 0 else ord('-')
            result = self.append_byte(sign_char)
            if not result.success:
                return result
            bits_encoded += 8

            # Encode digits
            for digit_char in digits_str:
                result = self.append_byte(ord(digit_char))
                if not result.success:
                    return result
                bits_encoded += 8
            
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
            
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_sint_ascii_var_size_null_terminated(self, int_val: int, null_characters: bytearray) -> EncodeResult:
        """Encode signed integer as ASCII with null termination."""
        if not isinstance(null_characters, (bytes, bytearray)):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Null characters must be bytes or bytearray"
            )
        
        try:
            bits_encoded = 0
            
            # Encode sign
            sign_char = ord('+') if int_val >= 0 else ord('-')
            result = self.append_byte(sign_char)
            if not result.success:
                return result
            bits_encoded += 8

            # Encode absolute value using unsigned null terminated encoding
            abs_val = abs(int_val)
            digits_str = str(abs_val)

            # Encode digits
            for digit_char in digits_str:
                result = self.append_byte(ord(digit_char))
                if not result.success:
                    return result
                bits_encoded += 8

            # Append null termination characters
            result = self.append_byte_array(null_characters, len(null_characters))
            if not result.success:
                return result
            bits_encoded += len(null_characters) * 8
            
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
            
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # ASCII INTEGER ENCODING - UNSIGNED
    # ============================================================================

    def enc_uint_ascii_const_size(self, int_val: int, encoded_size_in_bytes: int) -> EncodeResult:
        """Encode unsigned integer as ASCII with constant size."""
        if int_val < 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Value must be non-negative for unsigned encoding, got {int_val}"
            )
        
        if encoded_size_in_bytes < 1:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Encoded size must be at least 1 byte, got {encoded_size_in_bytes}"
            )

        # Check if value fits in the available digits
        max_value = (10 ** encoded_size_in_bytes) - 1
        if int_val > max_value:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Value {int_val} exceeds maximum {max_value} for {encoded_size_in_bytes} bytes"
            )

        try:
            # Extract digits in reverse order (like C implementation)
            digits = []
            temp_val = int_val
            
            if temp_val == 0:
                digits.append(0)
            else:
                while temp_val > 0:
                    digits.append(temp_val % 10)
                    temp_val //= 10
            
            # Pad with leading zeros if needed
            while len(digits) < encoded_size_in_bytes:
                digits.append(0)
            
            # Encode digits from most significant to least significant (reverse order)
            bits_encoded = 0
            for i in range(encoded_size_in_bytes - 1, -1, -1):
                digit_char = ord('0') + digits[i]
                result = self.append_byte(digit_char)
                if not result.success:
                    return result
                bits_encoded += 8
            
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
            
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_uint_ascii_var_size_length_embedded(self, int_val: int) -> EncodeResult:
        """Encode unsigned integer as ASCII with variable size (length embedded)."""
        if int_val < 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Value must be non-negative for unsigned encoding, got {int_val}"
            )

        try:
            # Get digits for the value
            if int_val == 0:
                digits_str = "0"
            else:
                digits_str = str(int_val)
            
            # Total length is just the number of digits (no sign character)
            total_length = len(digits_str)
            
            # Encode length first (1 byte)
            result = self.append_byte(total_length)
            if not result.success:
                return result
            bits_encoded = 8

            # Encode digits
            for digit_char in digits_str:
                result = self.append_byte(ord(digit_char))
                if not result.success:
                    return result
                bits_encoded += 8
            
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
            
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def enc_uint_ascii_var_size_null_terminated(self, int_val: int, null_characters: bytearray) -> EncodeResult:
        """Encode unsigned integer as ASCII with null termination."""
        if int_val < 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Value must be non-negative for unsigned encoding, got {int_val}"
            )
        
        if not isinstance(null_characters, (bytes, bytearray)):
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Null characters must be bytes or bytearray"
            )
        
        try:
            # Get digits for the value
            if int_val == 0:
                digits_str = "0"
            else:
                digits_str = str(int_val)
            
            bits_encoded = 0
            
            # Encode digits (no sign character needed)
            for digit_char in digits_str:
                result = self.append_byte(ord(digit_char))
                if not result.success:
                    return result
                bits_encoded += 8

            # Append null termination characters
            result = self.append_byte_array(null_characters, len(null_characters))
            if not result.success:
                return result
            bits_encoded += len(null_characters) * 8
            
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
            
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # HELPER METHODS
    # ============================================================================

    def _enc_string_ascii_private(self, max_len: int, str_val: str) -> tuple[int, int]:
        """Private helper method to encode string characters until null terminator or max_len.
        
        This method implements the common logic used by all string ASCII encoding methods:
        - Converts string to ASCII bytes
        - Writes characters until first null terminator (0) or max_len is reached
        - Returns the number of characters written and bits encoded
        
        Args:
            max_len: Maximum number of characters to write
            str_val: String value to encode
            
        Returns:
            Tuple of (characters_written, bits_encoded)
            
        Raises:
            UnicodeEncodeError: If string contains non-ASCII characters
            BitStreamError: If bitstream write fails
        """
        # Convert string to bytes using ASCII encoding (may raise UnicodeEncodeError)
        str_bytes = str_val.encode('ascii')
        
        bits_encoded = 0
        chars_written = 0
        
        # Write string characters until null terminator (0) or max_len
        for i in range(min(len(str_bytes), max_len)):
            if str_bytes[i] == 0:  # Stop at first null character
                break
            result = self.append_byte(str_bytes[i])
            if not result.success:
                raise BitStreamError(result.error_message)
            bits_encoded += 8
            chars_written += 1
            
        return chars_written, bits_encoded

    def _enc_string_char_index_private(self, max_len: int, allowed_char_set: bytearray, str_val: str) -> tuple[int, int]:
        """Private helper method to encode string using character indices until null terminator or max_len.
        
        This method implements the common logic for character index string encoding:
        - Converts string to ASCII bytes
        - Maps each character to its index in the allowed_char_set
        - Writes character indices using minimum bits needed
        - Stops at first null terminator (0) or max_len
        
        Args:
            max_len: Maximum number of characters to encode
            allowed_char_set: bytearray containing allowed characters
            str_val: String value to encode
            
        Returns:
            Tuple of (characters_written, bits_encoded)
            
        Raises:
            UnicodeEncodeError: If string contains non-ASCII characters
            ValueError: If string contains characters not in allowed_char_set
            BitStreamError: If bitstream write fails
        """
        # Convert string to bytes using ASCII encoding (may raise UnicodeEncodeError)
        str_bytes = str_val.encode('ascii')
        
        # Calculate bits needed per character index
        char_set_size = len(allowed_char_set)
        bits_per_char = self._get_bits_per_char(char_set_size)
        
        bits_encoded = 0
        chars_written = 0
        
        # Write string characters as indices until null terminator (0) or max_len
        for i in range(min(len(str_bytes), max_len)):
            if str_bytes[i] == 0:  # Stop at first null character
                break
                
            # Find character index in allowed set
            char_index = self._get_char_index(str_bytes[i], allowed_char_set)
            if char_index == -1:
                raise ValueError(f"Character '{chr(str_bytes[i])}' (0x{str_bytes[i]:02x}) not found in allowed character set")
                
            # Encode index as constrained integer (0 to char_set_size-1)
            result = self.encode_unsigned_integer(char_index, bits_per_char)
            if not result.success:
                raise BitStreamError(result.error_message)
            bits_encoded += bits_per_char
            chars_written += 1
            
        return chars_written, bits_encoded

    @staticmethod
    def _get_char_index(char_byte: int, allowed_char_set: bytearray) -> int:
        """Get the index of a character byte in the allowed character set.
        
        Args:
            char_byte: Byte value of the character (0-255)
            allowed_char_set: bytearray containing allowed characters
            
        Returns:
            Index position in allowed_char_set, or -1 if not found
        """
        try:
            return allowed_char_set.index(char_byte)
        except ValueError:
            return -1
    
    @staticmethod
    def _get_bits_per_char(char_set_size: int) -> int:
        """Calculate minimum bits needed to represent indices in a character set.
        
        Args:
            char_set_size: Size of the character set
            
        Returns:
            Number of bits needed per character index
        """
        if char_set_size <= 1:
            return 1  # Special case: need at least 1 bit
        
        # Calculate ceil(log2(char_set_size))
        bits_needed = 0
        temp = char_set_size - 1
        while temp > 0:
            bits_needed += 1
            temp >>= 1
        return bits_needed

    def _enc_uint_ascii_const_size_impl(self, int_val: int, encoded_size_in_bytes: int, total_bits: int) -> EncodeResult:
        """Helper method to encode unsigned integer as ASCII with constant size."""
        # Based on C implementation in asn1crt_encoding_acn.c:646-667
        try:
            bits_encoded = 8  # Already encoded sign in calling function
            
            # Extract digits in reverse order (like C implementation)
            digits = []
            temp_val = int_val
            while temp_val > 0:
                digits.append(temp_val % 10)
                temp_val //= 10
            
            # Pad with zeros if needed
            while len(digits) < encoded_size_in_bytes:
                digits.append(0)
                
            if len(digits) > encoded_size_in_bytes:
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Value {int_val} requires {len(digits)} digits but only {encoded_size_in_bytes} bytes available"
                )
            
            # Encode digits from most significant to least significant (reverse order)
            for i in range(encoded_size_in_bytes - 1, -1, -1):
                digit_char = ord('0') + digits[i]
                result = self.append_byte(digit_char)
                if not result.success:
                    return result
                bits_encoded += 8
            
            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
            
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )
    # ============================================================================
    # MILBUS FUNCTIONS
    # ============================================================================

    def encode_integer_big_endian(self, int_val: int, num_bits: int, is_twos_complement: bool) -> EncodeResult:
        """Encode integer in big-endian byte order.

        Args:
            int_val: Integer value to encode
            num_bits: Number of bits to encode (must be multiple of 8)
            is_twos_complement: True for signed two's complement, False for unsigned
        """
        if num_bits % 8 != 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )

        num_bytes = num_bits // 8

        # Validate range
        if is_twos_complement:
            min_val = -(1 << (num_bits - 1))
            max_val = (1 << (num_bits - 1)) - 1
            if not (min_val <= int_val <= max_val):
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Value {int_val} out of range [{min_val}, {max_val}] for {num_bits}-bit signed integer"
                )
            # Convert to unsigned representation (two's complement)
            if int_val < 0:
                unsigned_val = (1 << num_bits) + int_val
            else:
                unsigned_val = int_val
        else:
            max_val = (1 << num_bits) - 1
            if not (0 <= int_val <= max_val):
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Value {int_val} out of range [0, {max_val}] for {num_bits}-bit unsigned integer"
                )
            unsigned_val = int_val

        try:
            # Encode bytes in big-endian order (most significant byte first)
            bits_encoded = 0
            for i in range(num_bytes - 1, -1, -1):
                byte_val = (unsigned_val >> (i * 8)) & 0xFF
                result = self.append_byte(byte_val)
                if not result.success:
                    return result
                bits_encoded += 8

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def _encode_integer_little_endian(self, int_val: int, num_bits: int, is_twos_complement: bool) -> EncodeResult:
        """Encode integer in little-endian byte order.

        Args:
            int_val: Integer value to encode
            num_bits: Number of bits to encode (must be multiple of 8)
            is_twos_complement: True for signed two's complement, False for unsigned
        """
        if num_bits % 8 != 0:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )

        num_bytes = num_bits // 8

        # Validate range
        if is_twos_complement:
            min_val = -(1 << (num_bits - 1))
            max_val = (1 << (num_bits - 1)) - 1
            if not (min_val <= int_val <= max_val):
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Value {int_val} out of range [{min_val}, {max_val}] for {num_bits}-bit signed integer"
                )
            # Convert to unsigned representation (two's complement)
            if int_val < 0:
                unsigned_val = (1 << num_bits) + int_val
            else:
                unsigned_val = int_val
        else:
            max_val = (1 << num_bits) - 1
            if not (0 <= int_val <= max_val):
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Value {int_val} out of range [0, {max_val}] for {num_bits}-bit unsigned integer"
                )
            unsigned_val = int_val

        try:
            # Encode bytes in little-endian order (least significant byte first)
            bits_encoded = 0
            for i in range(num_bytes):
                byte_val = (unsigned_val >> (i * 8)) & 0xFF
                result = self.append_byte(byte_val)
                if not result.success:
                    return result
                bits_encoded += 8

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )
        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )