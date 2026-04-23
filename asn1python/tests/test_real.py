"""
Test REAL encoding and decoding
"""
import math
import sys
import random

from asn1python.acn_encoder import ACNEncoder
from asn1python.acn_decoder import ACNDecoder
from conftest import get_random_float_positive, get_random_float_negative, get_random_float_signed, get_random_float_unsigned


def _encode_and_decode_real(acn_encoder: ACNEncoder, input_number: float) -> tuple[bool, str | None] | tuple[
    bool, float | None]:
    """Helper function to encode and decode a single real value.

    Returns:
        Tuple of (success, decoded_value)
    """

    encoded_res = acn_encoder.enc_real(input_number)

    if not encoded_res.success:
        return False, encoded_res.error_message

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()

    decoded_res = acn_decoder.dec_real()

    if not decoded_res.success:
        return False, decoded_res.error_message

    return True, decoded_res.decoded_value


def _encode_and_decode_reals(acn_encoder: ACNEncoder, input_numbers: list[float]) -> tuple[bool, list[float]]:
    """Helper function to encode and decode multiple real values.

    Returns:
        Tuple of (success, decoded_values_list)
    """
    # Encode all values first
    for input_number in input_numbers:
        encoded_res = acn_encoder.enc_real(input_number)
        if not encoded_res.success:
            return False, []

    # Decode all values
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_values = []

    for _ in range(len(input_numbers)):
        decoded_res = acn_decoder.dec_real()
        if not decoded_res.success:
            return False, []
        decoded_values.append(decoded_res.decoded_value)

    return True, decoded_values


def _test_single_real(acn_encoder: ACNEncoder, input_number: float) -> None:
    """Helper function to test encoding/decoding of a single positive integer value."""
    success, decoded_value = _encode_and_decode_real(acn_encoder, input_number)
    assert success, f"Encoding/decoding failed for input {input_number}"

    print(f"Input: {input_number}, decoded {decoded_value}, Passed: {input_number == decoded_value}")
    assert input_number == decoded_value


def _test_multiple_real(acn_encoder: ACNEncoder, input_numbers: list[float]) -> None:
    """Helper function to test encoding/decoding of multiple positive integer values."""
    success, decoded_values = _encode_and_decode_reals(acn_encoder, input_numbers)
    assert success, f"Encoding/decoding failed for input {input_numbers}"

    print(f"Input: {input_numbers}, decoded {decoded_values}, Passed: {input_numbers == decoded_values}")
    assert input_numbers == decoded_values


def test_enc_dec_real_single_value(acn_encoder: ACNEncoder, seed: int, signed: bool) -> None:
    if signed:
        input_number = get_random_float_positive()
    else:
        input_number = get_random_float_negative()
    _test_single_real(acn_encoder, input_number)

def test_enc_dec_real_single_value_2(acn_encoder: ACNEncoder, seed: int, signed: bool, bit: int) -> None:
    if signed:
        input_number = get_random_float_signed(bit)
    else:
        input_number = get_random_float_unsigned(bit)
    _test_single_real(acn_encoder, input_number)

def test_enc_dec_real_multiple_values(acn_encoder: ACNEncoder, seed: int, signed: bool) -> None:
    input_numbers: list[float] = []
    for i in range(random.randint(3, 10)):
        if signed:
            input_numbers.append(get_random_float_positive())
        else:
            input_numbers.append(get_random_float_negative())

    _test_multiple_real(acn_encoder, input_numbers)

def test_enc_dec_real_multiple_values_2(acn_encoder: ACNEncoder, seed: int, signed: bool, bit: int) -> None:
    input_numbers: list[float] = []
    for i in range(random.randint(3, 10)):
        if signed:
            input_numbers.append(get_random_float_signed(bit))
        else:
            input_numbers.append(get_random_float_unsigned(bit))

    _test_multiple_real(acn_encoder, input_numbers)


def test_enc_dec_real_zero(acn_encoder: ACNEncoder, seed: int) -> None:
    input_number: float = 0.0
    _test_single_real(acn_encoder, input_number)

def test_enc_dec_real_negative_zero(acn_encoder: ACNEncoder, seed: int) -> None:
    input_number: float = -0.0
    _test_single_real(acn_encoder, input_number)

def test_enc_dec_real_inf_value(acn_encoder: ACNEncoder, seed: int, signed: bool) -> None:
    if signed:
        input_number: float = math.inf
    else:
        input_number: float = -math.inf
    _test_single_real(acn_encoder, input_number)

def test_enc_dec_real_nan_value(acn_encoder: ACNEncoder, seed: int) -> None:
    input_number: float = math.nan
    success, decoded_value = _encode_and_decode_real(acn_encoder, input_number)
    assert success, f"Encoding/decoding failed for input {input_number}"

    print(f"Input: {input_number}, decoded {decoded_value}, Passed: {input_number == decoded_value}")
    assert math.isnan(decoded_value), f"Encoding/decoding failed for input {input_number}"

def test_enc_dec_real_max_value(acn_encoder: ACNEncoder, seed: int, signed: bool) -> None:
    if signed:
        input_number: float = sys.float_info.max
    else:
        input_number: float = sys.float_info.min
    _test_single_real(acn_encoder, input_number)

# def test_enc_dec_real_max_value_exceeded(acn_encoder: ACNEncoder, seed: int) -> None:
#     input_number: float = sys.float_info.max
#     input_number += 10.0
#     encoded_res = acn_encoder.enc_real(input_number)
#     print(input_number)
#     # assert not encoded_res.success
