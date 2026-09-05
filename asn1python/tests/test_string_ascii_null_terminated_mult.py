import random

from asn1python.acn_decoder import ACNDecoder
from asn1python.acn_encoder import ACNEncoder

from conftest import get_random_string, get_null_terminator_string, get_random_string_random_length, \
    get_null_terminator_string_random_size


def _encode_and_decode_single_string_null_terminated(acn_encoder: ACNEncoder, input_string: str, max_length: int, null_characters: bytes) -> tuple[bool, str]:
    """Helper function to encode and decode a single string.

    Returns:
        Tuple of (success, decoded_value)
    """
    encoded_res = acn_encoder.enc_string_ascii_null_terminated_mult(max_length, null_characters, input_string)
    if not encoded_res.success:
        return False, ""

    assert encoded_res.bits_encoded == (len(input_string) + len(null_characters)) * 8

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_string_ascii_null_terminated_mult(max_length, null_characters)

    if not decoded_res.success:
        return False, ""

    assert len(decoded_res.decoded_value) == len(input_string)
    assert decoded_res.bits_consumed ==  (len(input_string) + len(null_characters)) * 8

    return True, decoded_res.decoded_value

def _encode_and_decode_single_string_null_terminated_without_checks(acn_encoder: ACNEncoder, input_string: str, max_length: int, null_characters: bytes) -> tuple[bool, str]:
    """Helper function to encode and decode a single string without adding any assertions.

    Returns:
        Tuple of (success, decoded_value)
    """
    encoded_res = acn_encoder.enc_string_ascii_null_terminated_mult(max_length, null_characters, input_string)
    if not encoded_res.success:
        return False, ""

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_string_ascii_null_terminated_mult(max_length, null_characters)

    if not decoded_res.success:
        return False, ""

    return True, decoded_res.decoded_value



def _encode_and_decode_multiple_strings_null_terminated(acn_encoder: ACNEncoder, input_strings: list[str], max_length: int, null_characters: bytes) -> tuple[bool, list[str]]:
    """Helper function to encode and decode multiple strings.

    Returns:
        Tuple of (success, decoded_values_list)
    """
    # Encode all values first
    for input_number in input_strings:
        encoded_res = acn_encoder.enc_string_ascii_null_terminated_mult(max_length, null_characters, input_number)
        if not encoded_res.success:
            return False, []

    # Decode all values
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_values = []

    for _ in range(len(input_strings)):
        decoded_res = acn_decoder.dec_string_ascii_null_terminated_mult(max_length, null_characters)
        if not decoded_res.success:
            return False, []
        decoded_values.append(decoded_res.decoded_value)

    return True, decoded_values


def _test_single_string_null_terminated(acn_encoder: ACNEncoder, input_string: str, max_length: int, null_characters: bytes) -> None:
    """Helper function to test encoding/decoding of a single string."""
    success, decoded_value = _encode_and_decode_single_string_null_terminated(acn_encoder, input_string, max_length, null_characters)
    assert success, f"Encoding/decoding failed for input {input_string}"

    print(f"Input: {input_string}, decoded {decoded_value}, Passed: {input_string == decoded_value}")
    assert input_string == decoded_value


def _test_multiple_strings_null_terminated(acn_encoder: ACNEncoder, input_numbers: list[str], max_length: int, null_characters: bytes) -> None:
    """Helper function to test encoding/decoding of multiple strings."""
    success, decoded_values = _encode_and_decode_multiple_strings_null_terminated(acn_encoder, input_numbers, max_length, null_characters)
    assert success, f"Encoding/decoding failed for input {input_numbers}"

    print(f"Input: {input_numbers}, decoded {decoded_values}, Passed: {input_numbers == decoded_values}")
    assert input_numbers == decoded_values


def test_enc_dec_string_ascii_null_terminated_mult_single_value(acn_encoder: ACNEncoder, seed: int, max_length: int, null_characters: bytes) -> None:
    input_string: str = get_random_string_random_length(max_length)
    _test_single_string_null_terminated(acn_encoder, input_string, max_length, null_characters)


def test_enc_dec_string_ascii_null_terminated_mult_multiple_values(acn_encoder: ACNEncoder, seed: int, max_length: int, null_characters: bytes) -> None:
    input_strings: list[str] = []
    for i in range(random.randint(3, 10)):
        input_strings.append(get_random_string_random_length(max_length))

    _test_multiple_strings_null_terminated(acn_encoder, input_strings, max_length, null_characters)

def test_enc_dec_string_ascii_null_terminated_mult_zero_length(acn_encoder: ACNEncoder, seed: int, null_characters: bytes) -> None:
    input_string: str = ""
    _test_single_string_null_terminated(acn_encoder, input_string, 0, null_characters)

def test_enc_dec_string_ascii_null_terminated_mult_null_terminator_symbol(acn_encoder: ACNEncoder, seed: int, max_length: int, null_characters: bytes) -> None:
    input_string: str = get_null_terminator_string_random_size(max_length)
    success, decoded_value = _encode_and_decode_single_string_null_terminated_without_checks(acn_encoder, input_string, max_length, null_characters)
    assert success, f"Encoding/decoding failed for input {input_string}"
    assert input_string != decoded_value
    assert len(decoded_value) == 0

def test_enc_dec_string_ascii_null_terminated_mult_too_long(acn_encoder: ACNEncoder, seed: int, max_length: int, null_characters: bytes) -> None:
    n = random.randint(1, max_length)
    input_string: str = get_random_string(max_length + n)
    success, decoded_value = _encode_and_decode_single_string_null_terminated_without_checks(acn_encoder, input_string, max_length, null_characters)
    assert success, f"Encoding/decoding failed for input {input_string}"
    assert len(decoded_value) == max_length
    print(f"Input: {input_string}, decoded {decoded_value}, Passed: {input_string[:-n] == decoded_value}")
    assert input_string[:-n] == decoded_value

def test_enc_dec_string_ascii_null_terminated_mult_check_encoded_length(acn_encoder: ACNEncoder, seed: int, max_length: int, null_characters: bytes) -> None:
    n = random.randint(1, max_length)
    input_string: str = get_random_string(max_length + n)
    encoded_res = acn_encoder.enc_string_ascii_null_terminated_mult(max_length, null_characters, input_string)
    assert encoded_res.bits_encoded == (max_length + len(null_characters)) * 8 # string up to (max_length + nullterminator) * 8 bit each

