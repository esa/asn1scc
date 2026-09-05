import random
import pytest

from asn1python.acn_decoder import ACNDecoder
from asn1python.acn_encoder import ACNEncoder

from conftest import get_random_string, get_null_terminator_string, get_random_string_random_length


def _encode_and_decode_single_string(acn_encoder: ACNEncoder, input_string: str, min_length: int, max_length: int) -> tuple[bool, str]:
    """Helper function to encode and decode a single string.

    Returns:
        Tuple of (success, decoded_value)
    """
    encoded_res = acn_encoder.enc_string_ascii_internal_field_determinant(max_length, min_length, input_string)
    if not encoded_res.success:
        return False, encoded_res.error_message

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_string_ascii_internal_field_determinant(max_length, min_length)

    # assert decoded_res.bits_consumed == 8*len(input_string)

    if not decoded_res.success:
        return False, decoded_res.error_message

    return True, decoded_res.decoded_value


def _encode_and_decode_multiple_strings(acn_encoder: ACNEncoder, input_strings: list[str], min_length: int, max_length: int) -> tuple[bool, list[str]]:
    """Helper function to encode and decode multiple strings.

    Returns:
        Tuple of (success, decoded_values_list)
    """
    # Encode all values first
    for input_number in input_strings:
        encoded_res = acn_encoder.enc_string_ascii_internal_field_determinant(max_length, min_length, input_number)
        if not encoded_res.success:
            return False, []

    # Decode all values
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_values = []

    for _ in range(len(input_strings)):
        decoded_res = acn_decoder.dec_string_ascii_internal_field_determinant(max_length, min_length)
        if not decoded_res.success:
            return False, []
        decoded_values.append(decoded_res.decoded_value)

    return True, decoded_values


def _test_single_string(acn_encoder: ACNEncoder, input_string: str, min_length: int, max_length: int) -> None:
    """Helper function to test encoding/decoding of a single string."""
    success, decoded_value = _encode_and_decode_single_string(acn_encoder, input_string, min_length, max_length)
    assert success, f"Encoding/decoding failed for input {input_string}"
    print(f"Input: {input_string}, decoded {decoded_value}, Passed: {input_string == decoded_value}")
    assert input_string == decoded_value

def _test_single_string_starts_with(acn_encoder: ACNEncoder, input_string: str, min_length: int, max_length: int) -> None:
    """Helper function to test encoding/decoding of a single string, where the decoded value starts with the input string."""
    success, decoded_value = _encode_and_decode_single_string(acn_encoder, input_string, min_length, max_length)
    assert success, f"Encoding/decoding failed for input {input_string}"
    print(f"Input: {input_string}, decoded {decoded_value}, Passed: {input_string == decoded_value}")
    assert decoded_value.startswith(input_string)

def _test_multiple_strings(acn_encoder: ACNEncoder, input_numbers: list[str], min_length: int, max_length: int) -> None:
    """Helper function to test encoding/decoding of multiple strings"""
    success, decoded_values = _encode_and_decode_multiple_strings(acn_encoder, input_numbers, min_length, max_length)
    assert success, f"Encoding/decoding failed for input {input_numbers}"

    print(f"Input: {input_numbers}, decoded {decoded_values}, Passed: {input_numbers == decoded_values}")
    assert input_numbers == decoded_values

def test_enc_dec_string_ascii_internal_field_determinant_single_value(acn_encoder: ACNEncoder, seed: int, max_length: int) -> None:
    input_string: str = get_random_string(max_length)
    _test_single_string(acn_encoder, input_string, 0, max_length)

def test_enc_dec_string_ascii_internal_field_determinant_single_value_random_range(acn_encoder: ACNEncoder, seed: int, max_length: int) -> None:
    input_string: str = get_random_string_random_length(max_length)
    min_val: int = len(input_string) - random.randint(0, len(input_string))
    max_val: int = len(input_string) + random.randint(0, max_length - len(input_string))
    _test_single_string(acn_encoder, input_string, min_val, max_val)

def test_enc_dec_string_ascii_internal_field_determinant_single_value_var_length(acn_encoder: ACNEncoder, seed: int, max_length: int) -> None:
    input_string: str = get_random_string_random_length(max_length)
    _test_single_string_starts_with(acn_encoder, input_string, 0, max_length)

def test_enc_dec_string_ascii_internal_field_determinant_multiple_values(acn_encoder: ACNEncoder, seed: int, max_length: int) -> None:
    input_strings: list[str] = []
    for i in range(random.randint(3, 10)):
        input_strings.append(get_random_string(max_length))

    _test_multiple_strings(acn_encoder, input_strings, 0, max_length)

def test_enc_dec_string_ascii_internal_field_determinant_zero_length(acn_encoder: ACNEncoder, seed: int) -> None:
    input_string: str = ""
    encoded_res = acn_encoder.enc_string_ascii_internal_field_determinant(0, 0, input_string)
    assert encoded_res.success
    assert encoded_res.bits_encoded == 0

def test_enc_dec_string_ascii_v_field_determinant_null_terminator_symbol(acn_encoder: ACNEncoder, seed: int, max_length: int) -> None:
    input_string: str = get_null_terminator_string(max_length)

    encoded_res = acn_encoder.enc_string_ascii_internal_field_determinant(max_length, 0, input_string)
    assert encoded_res.success

def test_enc_dec_string_ascii_internal_field_determinant_too_long(acn_encoder: ACNEncoder, seed: int, max_length: int) -> None:
    n = random.randint(1, max_length)
    input_string: str = get_random_string(max_length + n)
    success, decoded_value = _encode_and_decode_single_string(acn_encoder, input_string, 0, max_length)
    assert success
    assert input_string[:max_length] == decoded_value

def test_enc_dec_string_ascii_internal_field_determinant_too_short(acn_encoder: ACNEncoder, seed: int, max_length: int, charset: str) -> None:
    if max_length > 1:
        input_string: str = get_random_string_random_length(max_length-1)
        min_val: int = len(input_string) + 1

        assert len(input_string) < min_val <= max_length
        success, error_msg = _encode_and_decode_single_string(acn_encoder, input_string, min_val, max_length)
        assert success == False
        assert "is less than min_len" in error_msg

def test_enc_dec_string_ascii_internal_field_determinant_wrong_range(acn_encoder: ACNEncoder, seed: int, max_length: int, charset: str) -> None:
    input_string: str = get_random_string_random_length(max_length)
    min_val: int = len(input_string) - random.randint(1, len(input_string))
    max_val: int = len(input_string) + random.randint(0, max_length - len(input_string))

    # Hand over max_val and min_val in the wrong order!
    encoded_res = acn_encoder.enc_string_ascii_internal_field_determinant(min_val, max_val, input_string)

    assert min_val < max_val
    assert encoded_res.success == False
