import random
import pytest

from asn1python.acn_decoder import ACNDecoder
from asn1python.acn_encoder import ACNEncoder

from conftest import get_random_string, get_null_terminator_string, generate_test_string, \
    generate_test_string_random_length, charset_to_bytes


def _encode_and_decode_single_string(acn_encoder: ACNEncoder, input_string: str, max_length: int, charset: str) -> tuple[bool, str]:
    """Helper function to encode and decode a single string.

    Returns:
        Tuple of (success, decoded_value)
    """
    encoded_res = acn_encoder.enc_string_char_index_external_field_determinant(max_length, charset_to_bytes(charset), input_string)
    if not encoded_res.success:
        return False, ""

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_string_char_index_external_field_determinant(max_length, charset_to_bytes(charset), len(input_string))

    if not decoded_res.success:
        return False, ""

    return True, decoded_res.decoded_value


def _encode_and_decode_multiple_strings(acn_encoder: ACNEncoder, input_strings: list[str], max_length: int, charset: str) -> tuple[bool, list[str]]:
    """Helper function to encode and decode multiple strings.

    Returns:
        Tuple of (success, decoded_values_list)
    """
    # Encode all values first
    for input_string in input_strings:
        encoded_res = acn_encoder.enc_string_char_index_external_field_determinant(max_length, charset_to_bytes(charset), input_string)
        if not encoded_res.success:
            return False, []

    # Decode all values
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_values = []

    for _ in range(len(input_strings)):
        decoded_res = acn_decoder.dec_string_char_index_external_field_determinant(max_length, charset_to_bytes(charset), max_length)
        if not decoded_res.success:
            return False, []
        decoded_values.append(decoded_res.decoded_value)

    return True, decoded_values


def _test_single_string(acn_encoder: ACNEncoder, input_string: str, max_length: int, charset: str) -> None:
    """Helper function to test encoding/decoding of a single string."""
    success, decoded_value = _encode_and_decode_single_string(acn_encoder, input_string, max_length, charset)
    assert success, f"Encoding/decoding failed for input {input_string}"
    print(f"Input: {input_string}, decoded {decoded_value}, Passed: {input_string == decoded_value}")
    assert input_string == decoded_value

def _test_single_string_starts_with(acn_encoder: ACNEncoder, input_string: str, max_length: int, charset: str) -> None:
    """Helper function to test encoding/decoding of a single string."""
    success, decoded_value = _encode_and_decode_single_string(acn_encoder, input_string, max_length, charset)
    assert success, f"Encoding/decoding failed for input {input_string}"
    print(f"Input: {input_string}, decoded {decoded_value}, Passed: {input_string == decoded_value}")
    assert decoded_value.startswith(input_string)

def _test_multiple_strings(acn_encoder: ACNEncoder, input_numbers: list[str], max_length: int, charset: str) -> None:
    """Helper function to test encoding/decoding of multiple strings."""
    success, decoded_values = _encode_and_decode_multiple_strings(acn_encoder, input_numbers, max_length, charset)
    assert success, f"Encoding/decoding failed for input {input_numbers}"

    print(f"Input: {input_numbers}, decoded {decoded_values}, Passed: {input_numbers == decoded_values}")
    assert input_numbers == decoded_values

def test_enc_dec_string_char_index_external_field_determinant_single_value(acn_encoder: ACNEncoder, seed: int, max_length: int, charset: str) -> None:
    input_string: str = generate_test_string(charset, max_length)
    _test_single_string(acn_encoder, input_string, max_length, charset)

def test_enc_dec_string_char_index_external_field_determinant_single_value_var_length(acn_encoder: ACNEncoder, seed: int, max_length: int, charset: str) -> None:
    input_string: str = generate_test_string_random_length(charset, max_length)
    _test_single_string_starts_with(acn_encoder, input_string, max_length, charset)

def test_enc_dec_string_char_index_external_field_determinant_multiple_values(acn_encoder: ACNEncoder, seed: int, max_length: int, charset: str) -> None:
    input_strings: list[str] = []
    for i in range(random.randint(3, 10)):
        input_strings.append(generate_test_string(charset, max_length))

    _test_multiple_strings(acn_encoder, input_strings, max_length, charset)

def test_enc_dec_string_char_index_external_field_determinant_zero_length(acn_encoder: ACNEncoder, seed: int, charset: str) -> None:
    input_string: str = ""
    encoded_res = acn_encoder.enc_string_char_index_external_field_determinant(0, charset_to_bytes(charset), input_string)
    assert encoded_res.success
    assert encoded_res.bits_encoded == 0

def test_enc_dec_string_char_index_external_field_determinant_null_terminator_symbol(acn_encoder: ACNEncoder, seed: int, max_length: int, charset: str) -> None:
    input_string: str = get_null_terminator_string(max_length)
    encoded_res = acn_encoder.enc_string_char_index_external_field_determinant(0, charset_to_bytes(charset),                                                                            input_string)
    assert encoded_res.success, f"Encoding/decoding did not fail for input {input_string} with charset {charset}"
    assert encoded_res.bits_encoded == 0

def test_enc_dec_string_char_index_external_field_determinant_too_long(acn_encoder: ACNEncoder, seed: int, max_length: int, charset: str) -> None:
    n = random.randint(1, max_length)
    input_string: str = generate_test_string(charset, max_length + n)
    success, decoded_value = _encode_and_decode_single_string(acn_encoder, input_string, max_length, charset)
    assert success, f"Encoding for {input_string} and max_length {max_length} should be possible!"
    assert len(decoded_value) == max_length
    assert decoded_value == input_string[:max_length]

