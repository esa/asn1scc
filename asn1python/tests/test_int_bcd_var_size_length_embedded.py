import random

from asn1python.acn_decoder import ACNDecoder
from asn1python.acn_encoder import ACNEncoder
from conftest import get_random_max_length_digits, get_nibble_max_digit


def _encode_and_decode_single_bcd(acn_encoder: ACNEncoder, input_number: int) -> tuple[bool, int]:
    """Helper function to encode and decode a single BCD value.

    Returns:
        Tuple of (success, decoded_value)
    """
    encoded_res = acn_encoder.enc_int_bcd_var_size_length_embedded(input_number)
    if not encoded_res.success:
        return False, 0

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_int_bcd_var_size_length_embedded()

    if not decoded_res.success:
        return False, 0

    return True, decoded_res.decoded_value


def _encode_and_decode_multiple_bcd(acn_encoder: ACNEncoder, input_numbers: list[int]) -> tuple[bool, list[int]]:
    """Helper function to encode and decode multiple BCD values.

    Returns:
        Tuple of (success, decoded_values_list)
    """
    # Encode all values first
    for input_number in input_numbers:
        encoded_res = acn_encoder.enc_int_bcd_var_size_length_embedded(input_number)
        if not encoded_res.success:
            return False, []

    # Decode all values
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_values = []

    for _ in range(len(input_numbers)):
        decoded_res = acn_decoder.dec_int_bcd_var_size_length_embedded()
        if not decoded_res.success:
            return False, []
        decoded_values.append(decoded_res.decoded_value)

    return True, decoded_values


def _test_single_bcd_value(acn_encoder: ACNEncoder, input_number: int, nibble: int) -> None:
    """Helper function to test encoding/decoding of a single BCD value."""
    success, decoded_value = _encode_and_decode_single_bcd(acn_encoder, input_number)
    assert success, f"Encoding/decoding failed for input {input_number}"
    assert input_number == decoded_value


def _test_multiple_bcd_values(acn_encoder: ACNEncoder, input_numbers: list[int], nibble: int) -> None:
    """Helper function to test encoding/decoding of multiple BCD values."""
    success, decoded_values = _encode_and_decode_multiple_bcd(acn_encoder, input_numbers)
    assert success, f"Encoding/decoding failed for input {input_numbers}"
    assert input_numbers == decoded_values


def test_enc_dec_int_bcd_var_size_length_embedded_single_value(acn_encoder: ACNEncoder, seed: int, nibble: int) -> None:
    input_number: int = get_random_max_length_digits(nibble)
    _test_single_bcd_value(acn_encoder, input_number, nibble)


def test_enc_dec_int_bcd_var_size_length_embedded_multiple_values(acn_encoder: ACNEncoder, seed: int, nibble: int) -> None:
    input_numbers: list[int] = []
    for i in range(random.randint(3, 10)):
        input_numbers.append(get_random_max_length_digits(nibble))

    _test_multiple_bcd_values(acn_encoder, input_numbers, nibble)


def test_enc_dec_int_bcd_var_size_length_embedded_zero(acn_encoder: ACNEncoder, seed: int, nibble: int) -> None:
    input_number: int = 0
    _test_single_bcd_value(acn_encoder, input_number, nibble)


def test_enc_dec_int_bcd_var_size_length_embedded_max_value(acn_encoder: ACNEncoder, seed: int, nibble: int) -> None:
    input_number: int = get_nibble_max_digit(nibble)
    _test_single_bcd_value(acn_encoder, input_number, nibble)


def test_enc_dec_int_bcd_var_size_length_embedded_exceed_max_value(acn_encoder: ACNEncoder, seed: int, nibble: int) -> None:
    input_number: int = get_nibble_max_digit(nibble) + 1
    encoded_res = acn_encoder.enc_int_bcd_var_size_length_embedded(input_number)
    assert encoded_res.success
