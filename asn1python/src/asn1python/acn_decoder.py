"""
ASN.1 Python Runtime Library - ACN Decoder

This module provides ACN (ASN.1 Control Notation) decoding functionality.
ACN allows custom binary encodings for ASN.1 types to support legacy protocols.
"""

import struct
from .asn1_types import NO_OF_BITS_IN_BYTE
from .decoder import Decoder
from .codec import DecodeResult, DECODE_OK, ERROR_INVALID_VALUE
from .bitstream import BitStreamError


class ACNDecoder(Decoder):
    """
    ACN (ASN.1 Control Notation) decoder implementation.

    This decoder provides flexible binary decoding for ASN.1 types
    following custom ACN encoding rules to support legacy protocols.
    """

    def __init__(self, buffer: bytearray) -> None:
        super().__init__(buffer=buffer)
           
    @classmethod
    def of_size(cls, buffer_byte_size: int = 1024 * 1024) -> 'ACNDecoder':
        return cls(bytearray(buffer_byte_size))
     
    @classmethod
    def _construct(cls, buffer: bytearray) -> 'ACNDecoder':
        return cls(buffer)

    # ============================================================================
    # INTEGER DECODING - POSITIVE INTEGER
    # ============================================================================

    def dec_int_positive_integer_const_size(self, encoded_size_in_bits: int) -> DecodeResult[int]:
        """Decode positive integer with constant size in bits."""
        return self.decode_integer(min_val=0, max_val=(1 << encoded_size_in_bits) - 1,
                                  size_in_bits=encoded_size_in_bits)

    def dec_int_positive_integer_const_size_big_endian(self, num_bits: int) -> DecodeResult[int]:
        """Decode positive integer with constant size in big-endian byte order.

        Args:
            num_bits: Number of bits to decode (must be multiple of 8: 16, 32, 64)
        """
        if num_bits % 8 != 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )
        return self._decode_integer_big_endian(num_bits, False)

    def dec_int_positive_integer_const_size_little_endian(self, num_bits: int) -> DecodeResult[int]:
        """Decode positive integer with constant size in little-endian byte order.

        Args:
            num_bits: Number of bits to decode (must be multiple of 8: 16, 32, 64)
        """
        if num_bits % 8 != 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )
        return self._decode_integer_little_endian(num_bits, False)

    def dec_int_positive_integer_var_size_length_embedded(self) -> DecodeResult[int]:
        """Decode positive integer with variable size (length embedded)."""
        try:
            length_result = self.dec_length(8)
            if not length_result.success:
                return length_result

            assert isinstance(length_result.decoded_value, int)
            bytes_count = length_result.decoded_value
            bits_consumed = length_result.bits_consumed

            if self._bitstream.remaining_bits < bytes_count * 8:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Insufficient data: need {bytes_count} bytes"
                )

            value = 0
            for i in range(bytes_count):
                result = self.read_byte()
                if not result.success:
                    return result
                byte_val = result.decoded_value
                value = (value << 8) | byte_val
                bits_consumed += 8

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits_consumed
            )
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # INTEGER DECODING - TWO'S COMPLEMENT
    # ============================================================================

    def dec_int_twos_complement_const_size(self, format_bit_length: int) -> DecodeResult[int]:
        """Decode signed integer using two's complement with constant size."""
        # Decode as unsigned first
        result = self.decode_unsigned_integer(format_bit_length)
        if not result.success:
            return result

        unsigned_val = result.decoded_value

        # Convert from two's complement if sign bit is set
        sign_bit = 1 << (format_bit_length - 1)
        if unsigned_val & sign_bit:
            signed_val = unsigned_val - (1 << format_bit_length)
        else:
            signed_val = unsigned_val

        return DecodeResult(
            success=True,
            error_code=DECODE_OK,
            decoded_value=signed_val,
            bits_consumed=format_bit_length
        )

    def dec_int_twos_complement_const_size_big_endian(self, num_bits: int) -> DecodeResult[int]:
        """Decode signed integer (two's complement) with constant size in big-endian byte order.

        Args:
            num_bits: Number of bits to decode (must be multiple of 8: 16, 32, 64)
        """
        if num_bits % 8 != 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )
        return self._decode_integer_big_endian(num_bits, True)

    def dec_int_twos_complement_const_size_little_endian(self, num_bits: int) -> DecodeResult[int]:
        """Decode signed integer (two's complement) with constant size in little-endian byte order.

        Args:
            num_bits: Number of bits to decode (must be multiple of 8: 16, 32, 64)
        """
        if num_bits % 8 != 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"num_bits must be multiple of 8, got {num_bits}"
            )
        return self._decode_integer_little_endian(num_bits, True)

    def dec_int_twos_complement_var_size_length_embedded(self) -> DecodeResult[int]:
        """Decode signed integer with variable size (length embedded)."""
        try:
            length_result = self.dec_length(8)
            if not length_result.success:
                return length_result
            
            assert isinstance(length_result.decoded_value, int)
            bytes_count = length_result.decoded_value
            bits_consumed = length_result.bits_consumed

            if self._bitstream.remaining_bits < bytes_count * 8:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Insufficient data: need {bytes_count} bytes"
                )

            twos_complement = 0
            for i in range(bytes_count):
                result = self.read_byte()
                if not result.success:
                    return result
                byte_val = result.decoded_value
                twos_complement = (twos_complement << 8) | byte_val
                bits_consumed += 8

            sign_bit = 1 << (bytes_count * 8 - 1)
            if twos_complement & sign_bit:
                value = twos_complement - (1 << (bytes_count * 8))
            else:
                value = twos_complement

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits_consumed
            )
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # INTEGER DECODING - BCD (Binary Coded Decimal)
    # ============================================================================

    def dec_int_bcd_const_size(self, encoded_size_in_nibbles: int) -> DecodeResult[int]:
        """Decode integer from BCD format with constant size in nibbles."""
        try:
            if self._bitstream.remaining_bits < encoded_size_in_nibbles * 4:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Insufficient data: need {encoded_size_in_nibbles} nibbles"
                )

            value: int = 0
            bits_consumed = 0

            for i in range(encoded_size_in_nibbles):
                result = self.decode_unsigned_integer(4)
                if not result.success:
                    return result
                digit = result.decoded_value
                if digit > 9:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Invalid BCD digit: {digit}"
                    )
                value = value * 10 + digit
                bits_consumed += 4

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits_consumed
            )
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_int_bcd_var_size_length_embedded(self) -> DecodeResult[int]:
        """Decode integer from BCD format with variable size (length embedded)."""
        try:
            length_result = self.dec_length(8)
            if not length_result.success:
                return length_result
            
            assert isinstance(length_result.decoded_value, int)
            nibbles_count = length_result.decoded_value

            bcd_result = self.dec_int_bcd_const_size(nibbles_count)
            if not bcd_result.success:
                return bcd_result

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=bcd_result.decoded_value,
                bits_consumed=length_result.bits_consumed + bcd_result.bits_consumed
            )
        except Exception as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_int_bcd_var_size_null_terminated(self) -> DecodeResult[int]:
        """Decode integer from BCD format with null termination (0xF)."""
        try:
            value = 0
            bits_consumed = 0

            while True:
                if self._bitstream.remaining_bits < 4:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message="Unexpected end of data while reading BCD"
                    )

                result = self.decode_unsigned_integer(4)
                if not result.success:
                    return result
                digit = result.decoded_value
                bits_consumed += 4

                if digit == 0xF:
                    break

                if digit > 9:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Invalid BCD digit: {digit}"
                    )

                value = value * 10 + digit

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits_consumed
            )
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # REAL DECODING - IEEE 754
    # ============================================================================

    def dec_real_ieee754_32_big_endian(self) -> DecodeResult[float]:
        """Decode 32-bit IEEE 754 float (big-endian)."""
        try:
            if self._bitstream.remaining_bits < 32:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message="Insufficient data for IEEE 754 float"
                )

            result = self.read_byte_array(4)
            if not result.success:
                return result
            bytes_data = result.decoded_value

            value: float = struct.unpack('>f', bytes_data)[0]

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=32
            )
        except (BitStreamError, struct.error) as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_real_ieee754_32_little_endian(self) -> DecodeResult[float]:
        """Decode 32-bit IEEE 754 float (little-endian)."""
        try:
            if self._bitstream.remaining_bits < 32:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message="Insufficient data for IEEE 754 float"
                )

            result = self.read_byte_array(4)
            if not result.success:
                return result
            bytes_data = result.decoded_value

            value: float = struct.unpack('<f', bytes_data)[0]

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=32
            )
        except (BitStreamError, struct.error) as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_real_ieee754_64_big_endian(self) -> DecodeResult[float]:
        """Decode 64-bit IEEE 754 double (big-endian)."""
        try:
            if self._bitstream.remaining_bits < 64:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message="Insufficient data for IEEE 754 double"
                )

            result = self.read_byte_array(8)
            if not result.success:
                return result
            bytes_data = result.decoded_value

            value: float = struct.unpack('>d', bytes_data)[0]

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=64
            )
        except (BitStreamError, struct.error) as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_real_ieee754_64_little_endian(self) -> DecodeResult[float]:
        """Decode 64-bit IEEE 754 double (little-endian)."""
        try:
            if self._bitstream.remaining_bits < 64:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message="Insufficient data for IEEE 754 double"
                )

            result = self.read_byte_array(8)
            if not result.success:
                return result
            bytes_data = result.decoded_value

            value: float = struct.unpack('<d', bytes_data)[0]

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=64
            )
        except (BitStreamError, struct.error) as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # LENGTH DECODING
    # ============================================================================

    def dec_length(self, length_size_in_bits: int) -> DecodeResult[int]:
        """Decode length value with specified size in bits."""
        try:
            if self._bitstream.remaining_bits < length_size_in_bits:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Insufficient data for length: need {length_size_in_bits} bits"
                )

            return self.decode_unsigned_integer(length_size_in_bits)
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # STRING DECODING
    # ============================================================================

    def dec_string_ascii_fix_size(self, max_len: int) -> DecodeResult[str]:
        """Decode ASCII string with fixed size.
        
        Decodes exactly max_len characters from the bitstream.
        
        Args:
            max_len: Number of characters to decode (fixed size)
        """
        return self._dec_string_ascii_private(max_len, max_len)

    def dec_string_ascii_null_terminated(self, max_len: int, null_character: int) -> DecodeResult[str]:
        """Decode ASCII string with null termination.
        
        Reads characters until finding the null character or reaching max_len.
        
        Args:
            max_len: Maximum number of characters to read
            null_character: Null termination character (0-255)
        """
        if not (0 <= null_character <= 255):
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Null character must be 0-255, got {null_character}"
            )
            
        try:
            chars = []
            bits_consumed = 0
            end_reached = False
            i = 0
            
            # Read characters until null terminator or max_len (null terminated specific logic)
            while i <= max_len and not end_reached:
                if self._bitstream.remaining_bits < 8:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message="Insufficient data for character"
                    )

                result = self.read_byte()
                if not result.success:
                    return result
                decoded_char = result.decoded_value
                bits_consumed += 8
                
                if decoded_char != null_character:
                    chars.append(decoded_char)
                    i += 1
                else:
                    end_reached = True
                    
            # Convert bytes to ASCII string using common helper
            return self._bytes_to_ascii_string(chars, bits_consumed)
                
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_string_ascii_null_terminated_mult(self, max_len: int, null_characters: bytearray) -> DecodeResult[str]:
        """Decode ASCII string with multiple null characters.
        
        Uses a sliding window approach to detect multi-byte null terminator sequence.
        
        Args:
            max_len: Maximum number of characters to read
            null_characters: Multi-byte null termination sequence
        """
        if not isinstance(null_characters, (bytes, bytearray)):
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Null characters must be bytes or bytearray"
            )
            
        if len(null_characters) == 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Null characters sequence cannot be empty"
            )
            
        try:
            null_size = len(null_characters)
            tmp = bytearray(null_size)  # Sliding window buffer
            chars = []
            bits_consumed = 0
            
            # Read initial null_size characters into tmp buffer
            for j in range(null_size):
                if self._bitstream.remaining_bits < 8:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message="Insufficient data for initial characters"
                    )
                result = self.read_byte()
                if not result.success:
                    return result
                tmp[j] = result.decoded_value
                bits_consumed += 8
                
            i = 0
            while i <= max_len and tmp != null_characters:
                # Add first character of tmp to result
                chars.append(tmp[0])
                i += 1
                
                # Shift tmp buffer left by one position
                for j in range(null_size - 1):
                    tmp[j] = tmp[j + 1]
                    
                # Read next character into last position of tmp
                if self._bitstream.remaining_bits < 8:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message="Insufficient data for next character"
                    )
                result = self.read_byte()
                if not result.success:
                    return result
                tmp[null_size - 1] = result.decoded_value
                bits_consumed += 8
                
            # Convert bytes to ASCII string using common helper
            return self._bytes_to_ascii_string(chars, bits_consumed)
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_string_ascii_external_field_determinant(self, max_len: int, ext_size_determinant_fld: int) -> DecodeResult[str]:
        """Decode ASCII string with external field determinant.
        
        Uses an external field value to determine string length, capped at max_len.
        
        Args:
            max_len: Maximum allowed string length
            ext_size_determinant_fld: External field determining actual string length
        """
        if ext_size_determinant_fld < 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"External size determinant cannot be negative: {ext_size_determinant_fld}"
            )
            
        # Use external field value, capped at max_len
        characters_to_decode = min(ext_size_determinant_fld, max_len)
        return self._dec_string_ascii_private(max_len, characters_to_decode)

    def dec_string_ascii_internal_field_determinant(self, max_len: int, min_len: int) -> DecodeResult[str]:
        """Decode ASCII string with internal field determinant.
        
        Decodes a constrained integer first to determine the string length, then decodes the string.
        
        Args:
            max_len: Maximum string length
            min_len: Minimum string length
        """
        if min_len > max_len:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"min_len {min_len} cannot exceed max_len {max_len}"
            )
            
        # Decode constrained integer to get string length
        length_result = self.decode_integer(min_val=min_len, max_val=max_len)
        if not length_result.success:
            return DecodeResult(
                success=False,
                error_code=length_result.error_code,
                error_message=f"Failed to decode string length: {length_result.error_message}"
            )
            
        assert isinstance(length_result.decoded_value, int)
        n_count = length_result.decoded_value
        
        # Use decoded length, capped at max_len
        characters_to_decode = min(n_count, max_len)
        
        string_result = self._dec_string_ascii_private(max_len, characters_to_decode)
        if not string_result.success:
            return string_result
            
        # Combine bits consumed from length and string decoding
        return DecodeResult(
            success=True,
            error_code=DECODE_OK,
            decoded_value=string_result.decoded_value,
            bits_consumed=length_result.bits_consumed + string_result.bits_consumed
        )

    def dec_string_char_index_fix_size(self, max_len: int, allowed_char_set: bytearray) -> DecodeResult[str]:
        """Decode string using character index with fixed size.
        
        Decodes exactly max_len characters from character indices.
        
        Args:
            max_len: Number of characters to decode (fixed size)
            allowed_char_set: bytearray containing allowed characters
        """
        return self._dec_string_char_index_private(max_len, allowed_char_set, max_len)

    def dec_string_char_index_external_field_determinant(self, max_len: int, allowed_char_set: bytearray, ext_size_determinant_fld: int) -> DecodeResult[str]:
        """Decode string using character index with external field determinant.
        
        Uses an external field value to determine string length, capped at max_len.
        
        Args:
            max_len: Maximum allowed string length
            allowed_char_set: bytearray containing allowed characters
            ext_size_determinant_fld: External field determining actual string length
        """
        if ext_size_determinant_fld < 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"External size determinant cannot be negative: {ext_size_determinant_fld}"
            )
            
        # Use external field value, capped at max_len
        characters_to_decode = min(ext_size_determinant_fld, max_len)
        return self._dec_string_char_index_private(max_len, allowed_char_set, characters_to_decode)

    def dec_string_char_index_internal_field_determinant(self, max_len: int, allowed_char_set: bytearray, min_len: int) -> DecodeResult[str]:
        """Decode string using character index with internal field determinant.
        
        Decodes a constrained integer first to determine the string length,
        then decodes the string from character indices.
        
        Args:
            max_len: Maximum string length
            allowed_char_set: bytearray containing allowed characters
            min_len: Minimum string length
        """
        if min_len > max_len:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"min_len {min_len} cannot exceed max_len {max_len}"
            )
            
        # Decode constrained integer to get string length
        length_result = self.decode_integer(min_val=min_len, max_val=max_len)
        if not length_result.success:
            return DecodeResult(
                success=False,
                error_code=length_result.error_code,
                error_message=f"Failed to decode string length: {length_result.error_message}"
            )
            
        assert isinstance(length_result.decoded_value, int)
        n_count = length_result.decoded_value
        
        # Use decoded length, capped at max_len
        characters_to_decode = min(n_count, max_len)
        
        string_result = self._dec_string_char_index_private(max_len, allowed_char_set, characters_to_decode)
        if not string_result.success:
            return string_result
            
        # Combine bits consumed from length and string decoding
        return DecodeResult(
            success=True,
            error_code=DECODE_OK,
            decoded_value=string_result.decoded_value,
            bits_consumed=length_result.bits_consumed + string_result.bits_consumed
        )

    def dec_ia5_string_char_index_external_field_determinant(self, max_len: int, ext_size_determinant_fld: int) -> DecodeResult[str]:
        """Decode IA5 string using character index with external field determinant.
        
        IA5 (International Alphabet No. 5) is equivalent to 7-bit ASCII (0-127).
        Uses the full IA5 character set for character index decoding.
        
        Args:
            max_len: Maximum allowed string length
            ext_size_determinant_fld: External field determining actual string length
        """
        # Import IA5 character set from encoder to ensure consistency
        from .acn_encoder import IA5_CHAR_SET
        
        if ext_size_determinant_fld < 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"External size determinant cannot be negative: {ext_size_determinant_fld}"
            )
            
        # Use external field value, capped at max_len (matches C/Scala pattern)
        characters_to_decode = min(ext_size_determinant_fld, max_len)
        return self._dec_string_char_index_private(max_len, IA5_CHAR_SET, characters_to_decode)

    def dec_ia5_string_char_index_internal_field_determinant(self, max_len: int, min_len: int) -> DecodeResult[str]:
        """Decode IA5 string using character index with internal field determinant.
        
        IA5 (International Alphabet No. 5) is equivalent to 7-bit ASCII (0-127).
        Uses the full IA5 character set for character index decoding.
        
        Args:
            max_len: Maximum string length
            min_len: Minimum string length
        """
        # Import IA5 character set from encoder to ensure consistency
        from .acn_encoder import IA5_CHAR_SET
        
        if min_len > max_len:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"min_len {min_len} cannot exceed max_len {max_len}"
            )
        
        # Decode length as constrained integer
        length_result = self.decode_integer(min_val=min_len, max_val=max_len)
        if not length_result.success:
            return DecodeResult(
                success=False,
                error_code=length_result.error_code,
                error_message=f"Failed to decode string length: {length_result.error_message}",
                bits_consumed=length_result.bits_consumed
            )
        
        n_count = length_result.decoded_value
        
        # Use decoded length, capped at max_len (matches C/Scala pattern)
        characters_to_decode = min(n_count, max_len)
        
        string_result = self._dec_string_char_index_private(max_len, IA5_CHAR_SET, characters_to_decode)
        if not string_result.success:
            return string_result
            
        # Combine bits consumed from length and string decoding
        return DecodeResult(
            success=True,
            error_code=string_result.error_code,
            decoded_value=string_result.decoded_value,
            bits_consumed=length_result.bits_consumed + string_result.bits_consumed
        )

    # ============================================================================
    # ASCII INTEGER DECODING - SIGNED
    # ============================================================================

    def dec_sint_ascii_const_size(self, encoded_size_in_bytes: int) -> DecodeResult:
        """Decode signed integer from ASCII with constant size."""
        if encoded_size_in_bytes < 1:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Encoded size must be at least 1 byte, got {encoded_size_in_bytes}"
            )

        try:
            # Read sign character first
            result = self.read_byte()
            if not result.success:
                return result
            sign_char = result.decoded_value
            if sign_char == ord('+'):
                sign = 1
            elif sign_char == ord('-'):
                sign = -1
            else:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Invalid sign character: {chr(sign_char)}"
                )
            
            # Decode remaining bytes as unsigned integer
            remaining_bytes = encoded_size_in_bytes - 1
            uint_result = self._dec_uint_ascii_const_size_impl(remaining_bytes)
            if not uint_result.success:
                return uint_result
            
            # Apply sign
            value = sign * uint_result.decoded_value
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=encoded_size_in_bytes * 8
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_sint_ascii_var_size_length_embedded(self) -> DecodeResult:
        """Decode signed integer from ASCII with variable size (length embedded)."""
        try:
            # Read length first (1 byte)
            result = self.read_byte()
            if not result.success:
                return result
            total_length = result.decoded_value
            if total_length < 1:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Invalid length: {total_length}"
                )
            
            # Decode the signed integer with the specified length
            sint_result = self.dec_sint_ascii_const_size(total_length)
            if not sint_result.success:
                return sint_result
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=sint_result.decoded_value,
                bits_consumed=8 + sint_result.bits_consumed  # length byte + integer bytes
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_sint_ascii_var_size_null_terminated(self, null_characters: bytearray) -> DecodeResult:
        """Decode signed integer from ASCII with null termination."""
        if not isinstance(null_characters, (bytes, bytearray)):
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Null characters must be bytes or bytearray"
            )
        
        try:
            bits_decoded = 0

            # Read sign character first
            result = self.read_byte()
            if not result.success:
                return result
            sign_char = result.decoded_value
            bits_decoded += 8
            if sign_char == ord('+'):
                is_negative = False
            elif sign_char == ord('-'):
                is_negative = True
            else:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Invalid sign character: {chr(sign_char)}"
                )
            
            # Decode unsigned integer part using null termination
            uint_result = self._dec_uint_ascii_var_size_null_terminated_impl(null_characters)
            if not uint_result.success:
                return uint_result
            
            bits_decoded += uint_result.bits_consumed
            
            # Apply sign
            value = -uint_result.decoded_value if is_negative else uint_result.decoded_value
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits_decoded
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # ASCII INTEGER DECODING - UNSIGNED
    # ============================================================================

    def dec_uint_ascii_const_size(self, encoded_size_in_bytes: int) -> DecodeResult:
        """Decode unsigned integer from ASCII with constant size."""
        if encoded_size_in_bytes < 1:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Encoded size must be at least 1 byte, got {encoded_size_in_bytes}"
            )

        try:
            # Use the helper method to decode unsigned integer directly
            uint_result = self._dec_uint_ascii_const_size_impl(encoded_size_in_bytes)
            if not uint_result.success:
                return uint_result
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=uint_result.decoded_value,
                bits_consumed=uint_result.bits_consumed
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_uint_ascii_var_size_length_embedded(self) -> DecodeResult:
        """Decode unsigned integer from ASCII with variable size (length embedded)."""
        try:
            # Read length first (1 byte)
            result = self.read_byte()
            if not result.success:
                return result
            total_length = result.decoded_value
            if total_length < 1:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Invalid length: {total_length}"
                )
            
            # Decode the unsigned integer with the specified length
            uint_result = self._dec_uint_ascii_const_size_impl(total_length)
            if not uint_result.success:
                return uint_result
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=uint_result.decoded_value,
                bits_consumed=8 + uint_result.bits_consumed  # length byte + integer bytes
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def dec_uint_ascii_var_size_null_terminated(self, null_characters: bytearray) -> DecodeResult:
        """Decode unsigned integer from ASCII with null termination."""
        if not isinstance(null_characters, (bytes, bytearray)):
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Null characters must be bytes or bytearray"
            )
        
        try:
            # Use the helper method to decode unsigned integer with null termination
            uint_result = self._dec_uint_ascii_var_size_null_terminated_impl(null_characters)
            if not uint_result.success:
                return uint_result
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=uint_result.decoded_value,
                bits_consumed=uint_result.bits_consumed
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # TYPED INTEGER DECODING FUNCTIONS (C Type Compatibility)
    # ============================================================================
    # BOOLEAN DECODING
    # ============================================================================

    def read_bit_pattern(self, pattern_to_read: bytearray, n_bits_to_read: int) -> DecodeResult[bool]:
        """Read bit pattern and return boolean value."""
        try:
            prev_bits = self._bitstream.current_used_bits
            bytes_to_read = n_bits_to_read // NO_OF_BITS_IN_BYTE
            remaining_bits_to_read = n_bits_to_read % NO_OF_BITS_IN_BYTE
            needed_bytes = bytes_to_read + (0 if remaining_bits_to_read == 0 else 1)
            
            bool_result: bool = True
            i: int = 0
            
            while i < bytes_to_read:
                read: int
                if self._bitstream.remaining_bits < NO_OF_BITS_IN_BYTE:
                    read = self._bitstream.read_bits(self._bitstream.remaining_bits)
                else:
                    read = self._bitstream.read_byte()
                
                if read != pattern_to_read[i]:
                    bool_result = False
                    
                i += 1
                
            if remaining_bits_to_read > 0:
                read = self._bitstream.read_bits(remaining_bits_to_read)
                pattern = pattern_to_read[bytes_to_read] >> (NO_OF_BITS_IN_BYTE - remaining_bits_to_read)
                if  read != pattern:
                    bool_result = False
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=bool_result,
                bits_consumed= self._bitstream.current_used_bits - prev_bits
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def decode_true_false_boolean(self, true_pattern: bytearray, false_pattern: bytearray, n_bits_to_read: int) -> DecodeResult[bool]:
        """Decode boolean using true/false patterns."""
        try:
            prev_bits = self._bitstream.current_used_bits
            bytes_to_read = n_bits_to_read // NO_OF_BITS_IN_BYTE
            remaining_bits_to_read = n_bits_to_read % NO_OF_BITS_IN_BYTE
            
            bool_result: bool | None = None
            
            for i in range(bytes_to_read):
                read = self._bitstream.read_byte() if self._bitstream.remaining_bits >= NO_OF_BITS_IN_BYTE else self._bitstream.read_bits(self._bitstream.remaining_bits)
                
                # Validate: byte must match either true or false pattern
                matches_true = (read == true_pattern[i])
                matches_false = (read == false_pattern[i])
                
                if not matches_true and not matches_false:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Invalid pattern: {read}"
                    )
                
                # First byte determines the expected pattern
                if i == 0:
                    bool_result = matches_true
                else:
                    # Subsequent bytes must be consistent with first byte's determination
                    if matches_true and not bool_result:
                        return DecodeResult(
                            success=False,
                            error_code=ERROR_INVALID_VALUE,
                            error_message=f"Byte {i} matches true pattern but earlier bytes matched false pattern"
                        )
                    if matches_false and bool_result:
                        return DecodeResult(
                            success=False,
                            error_code=ERROR_INVALID_VALUE,
                            error_message=f"Byte {i} matches false pattern but earlier bytes matched true pattern"
                        )
            
            if remaining_bits_to_read > 0:
                read = self._bitstream.read_bits(remaining_bits_to_read)
                true_partial = true_pattern[bytes_to_read] >> (NO_OF_BITS_IN_BYTE - remaining_bits_to_read)
                false_partial = false_pattern[bytes_to_read] >> (NO_OF_BITS_IN_BYTE - remaining_bits_to_read)
                
                matches_true = (read == true_partial)
                matches_false = (read == false_partial)
                
                if not matches_true and not matches_false:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Invalid pattern: {read}"
                    )
                
                if bytes_to_read == 0:
                    bool_result = matches_true
                else:
                    if matches_true and not bool_result:
                        return DecodeResult(
                            success=False,
                            error_code=ERROR_INVALID_VALUE,
                            error_message="Remaining bits match true pattern but earlier bytes matched false pattern"
                        )
                    if matches_false and bool_result:
                        return DecodeResult(
                            success=False,
                            error_code=ERROR_INVALID_VALUE,
                            error_message="Remaining bits match false pattern but earlier bytes matched true pattern"
                        )
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=bool_result,
                bits_consumed=self._bitstream.current_used_bits - prev_bits
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # NULL TYPE FUNCTIONS
    # ============================================================================

    def read_bit_pattern_ignore_value(self, n_bits_to_read: int) -> DecodeResult[None]:
        """Read bit pattern and ignore the value."""
        try:
            prev_bits = self._bitstream.current_used_bits
            bytes_to_read = n_bits_to_read // NO_OF_BITS_IN_BYTE
            remaining_bits_to_read = n_bits_to_read % NO_OF_BITS_IN_BYTE
            needed_bytes = bytes_to_read + (0 if remaining_bits_to_read == 0 else 1)
            
            i: int = 0
            while i < bytes_to_read:
                self._bitstream.read_byte()
                i += 1
                
            if remaining_bits_to_read > 0:
                self._bitstream.read_bits(remaining_bits_to_read)
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                bits_consumed= self._bitstream.current_used_bits - prev_bits
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # IEEE 754 REAL DECODING WITH FLOAT PRECISION
    # ============================================================================
    # HELPER METHODS
    # ============================================================================

    def _dec_string_ascii_private(self, max_len: int, characters_to_decode: int) -> DecodeResult[str]:
        """Private helper method to decode a specific number of ASCII characters.
        
        Args:
            max_len: Maximum allowed string length (for validation)
            characters_to_decode: Actual number of characters to decode
        """
        if characters_to_decode < 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Characters to decode cannot be negative: {characters_to_decode}"
            )
            
        if characters_to_decode > max_len:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Characters to decode {characters_to_decode} exceeds max_len {max_len}"
            )
            
        try:
            chars = []
            bits_consumed = 0
            
            for i in range(characters_to_decode):
                if self._bitstream.remaining_bits < 8:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Insufficient data: need {characters_to_decode - i} more characters"
                    )

                result = self.read_byte()
                if not result.success:
                    return result
                char_byte = result.decoded_value
                chars.append(char_byte)
                bits_consumed += 8
                
            # Convert bytes to ASCII string
            return self._bytes_to_ascii_string(chars, bits_consumed)
                
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )
    
    def _bytes_to_ascii_string(self, chars: list[int], bits_consumed: int) -> DecodeResult[str]:
        """Convert list of byte values to ASCII string with proper error handling.
        
        Args:
            chars: List of byte values (0-255)
            bits_consumed: Number of bits consumed to read these characters
            
        Returns:
            DecodeResult with ASCII string or error
        """
        try:
            result_str = bytes(chars).decode('ascii')
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=result_str,
                bits_consumed=bits_consumed
            )
        except UnicodeDecodeError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Non-ASCII character encountered: {e}"
            )

    def _decode_integer_big_endian(self, bits: int, signed: bool) -> DecodeResult[int]:
        """Helper method to decode integer in big-endian format."""
        try:
            if self._bitstream.remaining_bits < bits:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Insufficient data: need {bits} bits"
                )

            unsigned_val = 0
            bytes_count = bits // 8
            for i in range(bytes_count):
                result = self.read_byte()
                if not result.success:
                    return result
                byte_val = result.decoded_value
                unsigned_val = (unsigned_val << 8) | byte_val

            if signed:
                sign_bit = 1 << (bits - 1)
                if unsigned_val & sign_bit:
                    signed_val = unsigned_val - (1 << bits)
                else:
                    signed_val = unsigned_val
                value = signed_val
            else:
                value = unsigned_val

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits
            )
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def _decode_integer_little_endian(self, bits: int, signed: bool) -> DecodeResult[int]:
        """Helper method to decode integer in little-endian format."""
        try:
            if self._bitstream.remaining_bits < bits:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Insufficient data: need {bits} bits"
                )

            unsigned_val = 0
            bytes_count = bits // 8
            for i in range(bytes_count):
                result = self.read_byte()
                if not result.success:
                    return result
                byte_val = result.decoded_value
                unsigned_val |= (byte_val << (i * 8))

            if signed:
                sign_bit = 1 << (bits - 1)
                if unsigned_val & sign_bit:
                    signed_val = unsigned_val - (1 << bits)
                else:
                    signed_val = unsigned_val
                value = signed_val
            else:
                value = unsigned_val

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits
            )
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    # ============================================================================
    # MILBUS FUNCTIONS
    # ============================================================================
    # CHARACTER INDEX STRING DECODING HELPER METHODS
    # ============================================================================

    def _dec_string_char_index_private(self, max_len: int, allowed_char_set: bytearray, characters_to_decode: int) -> DecodeResult[str]:
        """Private helper method to decode a specific number of characters from character indices.
        
        Args:
            max_len: Maximum allowed string length (for validation)
            allowed_char_set: bytearray containing allowed characters
            characters_to_decode: Actual number of characters to decode
        """
        if not isinstance(allowed_char_set, (bytes, bytearray)):
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Allowed character set must be bytes or bytearray"
            )
            
        if len(allowed_char_set) == 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message="Allowed character set cannot be empty"
            )
            
        if characters_to_decode < 0:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Characters to decode cannot be negative: {characters_to_decode}"
            )
            
        if characters_to_decode > max_len:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Characters to decode {characters_to_decode} exceeds max_len {max_len}"
            )
            
        try:
            # Calculate bits needed per character index
            char_set_size = len(allowed_char_set)
            bits_per_char = self._get_bits_per_char_decode(char_set_size)
            
            chars = []
            bits_consumed = 0
            
            for i in range(characters_to_decode):
                if self._bitstream.remaining_bits < bits_per_char:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Insufficient data: need {characters_to_decode - i} more character indices"
                    )
                    
                # Read character index as constrained integer (0 to char_set_size-1)
                result = self.decode_unsigned_integer(bits_per_char)
                if not result.success:
                    return result
                char_index = result.decoded_value

                # Validate index is within allowed range
                if char_index >= char_set_size:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Character index {char_index} exceeds allowed range 0-{char_set_size-1}"
                    )
                    
                # Map index back to character
                char_byte = allowed_char_set[char_index]
                chars.append(char_byte)
                bits_consumed += bits_per_char
                
            # Convert bytes to ASCII string using common helper
            return self._bytes_to_ascii_string(chars, bits_consumed)
                
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )
    
    def _get_bits_per_char_decode(self, char_set_size: int) -> int:
        """Calculate minimum bits needed to represent indices in a character set (decoder version).
        
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

    def _dec_uint_ascii_const_size_impl(self, encoded_size_in_bytes: int) -> DecodeResult:
        """Helper method to decode unsigned integer from ASCII with constant size."""
        # Based on C implementation in asn1crt_encoding_acn.c:684-704
        try:
            value = 0

            # Read each digit character and convert to integer
            for _ in range(encoded_size_in_bytes):
                result = self.read_byte()
                if not result.success:
                    return result
                digit_char = result.decoded_value

                # Validate digit character is in valid range '0'-'9'
                if digit_char < ord('0') or digit_char > ord('9'):
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Invalid digit character: {chr(digit_char)}"
                    )
                
                # Convert to digit and accumulate value
                digit = digit_char - ord('0')
                value = value * 10 + digit
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=encoded_size_in_bytes * 8
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def _dec_uint_ascii_var_size_null_terminated_impl(self, null_characters: bytearray) -> DecodeResult:
        """Helper method to decode unsigned integer from ASCII with null termination."""
        # Based on C implementation sliding window approach
        try:
            value = 0
            bits_decoded = 0
            null_size = len(null_characters)
            
            # Read initial buffer to match null terminator size
            buffer = []
            for _ in range(null_size):
                result = self.read_byte()
                if not result.success:
                    return result
                byte_val = result.decoded_value
                buffer.append(byte_val)
                bits_decoded += 8
            
            # Process digits until null terminator pattern is found
            while bytes(buffer) != null_characters:
                # First byte in buffer is a digit
                digit_char = buffer[0]
                
                # Validate digit character
                if digit_char < ord('0') or digit_char > ord('9'):
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message=f"Invalid digit character: {chr(digit_char)}"
                    )
                
                # Convert to digit and accumulate value
                digit = digit_char - ord('0')
                value = value * 10 + digit
                
                # Shift buffer left and read next byte
                buffer = buffer[1:]
                result = self.read_byte()
                if not result.success:
                    return result
                byte_val = result.decoded_value
                buffer.append(byte_val)
                bits_decoded += 8
            
            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits_decoded
            )
            
        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )