"""
ASN.1 Python Runtime Library - UPER Codec

This module provides UPER (Unaligned Packed Encoding Rules) encoding and decoding.
"""

from .codec import Codec, EncodeResult, DecodeResult, ENCODE_OK, DECODE_OK, ERROR_INVALID_VALUE
from .bitstream import BitStreamError


class UPERCodec(Codec):
    """
    UPER (Unaligned Packed Encoding Rules) codec implementation.

    This codec provides efficient bit-packed encoding/decoding for ASN.1 types
    following the UPER standard.
    """

    def __init__(self) -> None:
        super().__init__()

    def copy(self) -> 'UPERCodec':
        """Creates and returns a copy of this codec instance"""
        current_data = self._bitstream.get_data_copy()
        curret_position = self._bitstream.current_bit_position

        new_codec = UPERCodec()
        new_codec._bitstream.reset()
        if len(current_data) > 0:
            new_codec._bitstream.write_bytes(current_data)
        new_codec._bitstream.set_bit_index(curret_position)

        return new_codec

    def encode_constrained_integer(self, value: int, min_val: int, max_val: int) -> EncodeResult:
        """
        Encode a constrained integer using UPER rules.

        For constrained integers, UPER uses the minimum number of bits
        needed to represent the range.
        """
        return self.encode_integer(value, min_val=min_val, max_val=max_val)

    def decode_constrained_integer(self, min_val: int, max_val: int) -> DecodeResult[int]:
        """Decode a constrained integer using UPER rules"""
        return self.decode_integer(min_val=min_val, max_val=max_val)

    def encode_semi_constrained_integer(self, value: int, min_val: int) -> EncodeResult:
        """
        Encode a semi-constrained integer (only minimum bound) using UPER rules.

        Semi-constrained integers use a length determinant followed by octets.
        """
        if value < min_val:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=f"Value {value} below minimum {min_val}"
            )

        try:
            # Encode as offset from minimum
            offset_value = value - min_val

            # Determine number of octets needed
            if offset_value == 0:
                octets_needed = 1
            else:
                octets_needed = (offset_value.bit_length() + 7) // 8

            # Encode length determinant
            length_result = self._encode_length_determinant(octets_needed)
            if not length_result.success:
                return length_result

            bits_encoded = length_result.bits_encoded

            # Encode the integer value in octets
            for i in range(octets_needed - 1, -1, -1):
                octet = (offset_value >> (i * 8)) & 0xFF
                self._bitstream.write_bits(octet, 8)
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

    def decode_semi_constrained_integer(self, min_val: int) -> DecodeResult[int]:
        """Decode a semi-constrained integer using UPER rules"""
        try:
            # Decode length determinant
            length_result = self._decode_length_determinant()
            if not length_result.success or length_result.decoded_value is None:
                return length_result

            octets_count = length_result.decoded_value
            bits_consumed = length_result.bits_consumed

            # Decode the integer value from octets
            if self._bitstream.remaining_bits < octets_count * 8:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Insufficient data: need {octets_count} octets"
                )

            offset_value = 0
            for _ in range(octets_count):
                octet = self._bitstream.read_bits(8)
                offset_value = (offset_value << 8) | octet
                bits_consumed += 8

            value = offset_value + min_val

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

    def encode_unconstrained_integer(self, value: int) -> EncodeResult:
        """
        Encode an unconstrained integer using UPER rules.

        Unconstrained integers use a length determinant followed by 
        two's complement representation.
        """
        try:
            # Determine number of octets needed for two's complement
            if value >= 0:
                # Positive: need extra bit for sign if MSB is 1
                bit_length = value.bit_length()
                octets_needed = (bit_length + 8) // 8  # +1 for sign bit, then round up
            else:
                # Negative: use two's complement
                bit_length = (abs(value) - 1).bit_length()
                octets_needed = (bit_length + 8) // 8

            if octets_needed == 0:
                octets_needed = 1

            # Encode length determinant
            length_result = self._encode_length_determinant(octets_needed)
            if not length_result.success:
                return length_result

            bits_encoded = length_result.bits_encoded

            # Convert to two's complement representation
            if value >= 0:
                twos_complement = value
            else:
                twos_complement = (1 << (octets_needed * 8)) + value

            # Encode the integer value in octets (big-endian)
            for i in range(octets_needed - 1, -1, -1):
                octet = (twos_complement >> (i * 8)) & 0xFF
                self._bitstream.write_bits(octet, 8)
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

    def decode_unconstrained_integer(self) -> DecodeResult[int]:
        """Decode an unconstrained integer using UPER rules"""
        try:
            # Decode length determinant
            length_result = self._decode_length_determinant()
            if not length_result.success or length_result.decoded_value is None:
                return length_result

            octets_count = length_result.decoded_value
            bits_consumed = length_result.bits_consumed

            # Decode the integer value from octets
            if self._bitstream.remaining_bits < octets_count * 8:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Insufficient data: need {octets_count} octets"
                )

            # Read octets (big-endian)
            twos_complement = 0
            for _ in range(octets_count):
                octet = self._bitstream.read_bits(8)
                twos_complement = (twos_complement << 8) | octet
                bits_consumed += 8

            # Convert from two's complement
            sign_bit = 1 << (octets_count * 8 - 1)
            if twos_complement & sign_bit:
                # Negative number
                value = twos_complement - (1 << (octets_count * 8))
            else:
                # Positive number
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

    def encode_real(self, value: float) -> EncodeResult:
        """
        Encode a REAL value using UPER rules.

        UPER encoding of REAL uses IEEE 754 double precision format.
        """
        try:
            import struct

            # Handle special cases
            if value == 0.0:
                # Zero is encoded as empty (length 0)
                return self._encode_length_determinant(0)

            # Convert to IEEE 754 double precision
            ieee_bytes = struct.pack('>d', value)  # Big-endian double

            # Encode length determinant (8 octets for IEEE double)
            length_result = self._encode_length_determinant(8)
            if not length_result.success:
                return length_result

            bits_encoded = length_result.bits_encoded

            # Encode the IEEE 754 bytes
            for byte in ieee_bytes:
                self._bitstream.write_bits(byte, 8)
                bits_encoded += 8

            return EncodeResult(
                success=True,
                error_code=ENCODE_OK,
                encoded_data=self._bitstream.get_data_copy(),
                bits_encoded=bits_encoded
            )

        except (BitStreamError, struct.error) as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def decode_real(self) -> DecodeResult[float]:
        """Decode a REAL value using UPER rules"""
        try:
            import struct

            # Decode length determinant
            length_result = self._decode_length_determinant()
            if not length_result.success:
                return DecodeResult(
                    success=length_result.success,
                    error_code=length_result.error_code,
                    error_message=length_result.error_message,
                    decoded_value=length_result.decoded_value
                )

            octets_count = length_result.decoded_value
            bits_consumed = length_result.bits_consumed

            # Handle special case of zero
            if octets_count == 0:
                return DecodeResult(
                    success=True,
                    error_code=DECODE_OK,
                    decoded_value=0.0,
                    bits_consumed=bits_consumed
                )

            # Expect 8 octets for IEEE double
            if octets_count != 8:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Expected 8 octets for IEEE double, got {octets_count}"
                )

            # Read IEEE 754 bytes
            if self._bitstream.remaining_bits < 64:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message="Insufficient data for IEEE double"
                )

            ieee_bytes = bytearray()
            for _ in range(8):
                octet = self._bitstream.read_bits(8)
                ieee_bytes.append(octet)
                bits_consumed += 8

            # Convert from IEEE 754 double precision
            value: float = struct.unpack('>d', ieee_bytes)[0]

            return DecodeResult(
                success=True,
                error_code=DECODE_OK,
                decoded_value=value,
                bits_consumed=bits_consumed
            )

        except (BitStreamError, struct.error) as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def _encode_length_determinant(self, length: int) -> EncodeResult:
        """
        Encode a length determinant according to UPER rules.

        Length determinants are used for variable-length encodings.
        """
        try:
            if length < 0:
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Length cannot be negative: {length}"
                )

            if length < 128:
                # Short form: 0xxxxxxx
                self._bitstream.write_bits(length, 8)
                return EncodeResult(
                    success=True,
                    error_code=ENCODE_OK,
                    encoded_data=self._bitstream.get_data_copy(),
                    bits_encoded=8
                )
            elif length < 16384:
                # Medium form: 10xxxxxx xxxxxxxx
                self._bitstream.write_bits(0x8000 | length, 16)
                return EncodeResult(
                    success=True,
                    error_code=ENCODE_OK,
                    encoded_data=self._bitstream.get_data_copy(),
                    bits_encoded=16
                )
            else:
                # Long form: 11xxxxxx + length in octets
                return EncodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message=f"Length {length} too large for UPER encoding"
                )

        except BitStreamError as e:
            return EncodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )

    def _decode_length_determinant(self) -> DecodeResult[int]:
        """Decode a length determinant according to UPER rules"""
        try:
            if self._bitstream.remaining_bits < 8:
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message="Insufficient data for length determinant"
                )

            first_byte = self._bitstream.read_bits(8)

            if (first_byte & 0x80) == 0:
                # Short form: 0xxxxxxx
                return DecodeResult(
                    success=True,
                    error_code=DECODE_OK,
                    decoded_value=first_byte,
                    bits_consumed=8
                )
            elif (first_byte & 0xC0) == 0x80:
                # Medium form: 10xxxxxx xxxxxxxx
                if self._bitstream.remaining_bits < 8:
                    return DecodeResult(
                        success=False,
                        error_code=ERROR_INVALID_VALUE,
                        error_message="Insufficient data for medium length determinant"
                    )

                second_byte = self._bitstream.read_bits(8)
                length = ((first_byte & 0x3F) << 8) | second_byte

                return DecodeResult(
                    success=True,
                    error_code=DECODE_OK,
                    decoded_value=length,
                    bits_consumed=16
                )
            else:
                # Long form: 11xxxxxx - not supported in this implementation
                return DecodeResult(
                    success=False,
                    error_code=ERROR_INVALID_VALUE,
                    error_message="Long form length determinant not supported"
                )

        except BitStreamError as e:
            return DecodeResult(
                success=False,
                error_code=ERROR_INVALID_VALUE,
                error_message=str(e)
            )