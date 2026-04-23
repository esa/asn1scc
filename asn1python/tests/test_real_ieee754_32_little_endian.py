import ctypes
import random

from asn1python.acn_decoder import ACNDecoder
from asn1python.acn_encoder import ACNEncoder

from conftest import get_big_float, get_zero_and_special_floats
from conftest import get_random_float


def _encode_and_decode_single_float(acn_encoder: ACNEncoder, input_number: float) -> tuple[bool, float]:
    """Helper function to encode and decode a single float value.

    Returns:
        Tuple of (success, decoded_value)
    """
    encoded_res = acn_encoder.enc_real_ieee754_32_little_endian(input_number)
    if not encoded_res.success:
        return False, 0

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_real_ieee754_32_little_endian()

    if not decoded_res.success:
        return False, 0

    return True, decoded_res.decoded_value


def _encode_and_decode_multiple_floats(acn_encoder: ACNEncoder, input_numbers: list[float]) -> tuple[bool, list[float]]:
    """Helper function to encode and decode multiple float values.

    Returns:
        Tuple of (success, decoded_values_list)
    """
    # Encode all values first
    for input_number in input_numbers:
        encoded_res = acn_encoder.enc_real_ieee754_32_little_endian(input_number)
        if not encoded_res.success:
            return False, []

    # Decode all values
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_values = []

    for _ in range(len(input_numbers)):
        decoded_res = acn_decoder.dec_real_ieee754_32_little_endian()
        if not decoded_res.success:
            return False, []
        decoded_values.append(decoded_res.decoded_value)

    return True, decoded_values


def _test_single_float(acn_encoder: ACNEncoder, input_number: float) -> None:
    """Helper function to test encoding/decoding of a single float value."""
    # Important: convert float to ctypes.c_float in order to know what the 32 bit representation is.
    input_number = float(ctypes.c_float(input_number).value)

    success, decoded_value = _encode_and_decode_single_float(acn_encoder, input_number)
    assert success, f"Encoding/decoding failed for input {input_number}"

    print(f"Input: {input_number}, decoded {decoded_value}, Passed: {input_number == decoded_value}")
    assert input_number == decoded_value

def _test_multiple_floats(acn_encoder: ACNEncoder, input_numbers: list[float]) -> None:
    """Helper function to test encoding/decoding of multiple float values."""
    # Important: convert float to ctypes.c_float in order to know what the 32 bit representation is.
    for i in range(len(input_numbers)):
        input_numbers[i] = float(ctypes.c_float(input_numbers[i]).value)

    success, decoded_values = _encode_and_decode_multiple_floats(acn_encoder, input_numbers)
    assert success, f"Encoding/decoding failed for input {input_numbers}"

    print(f"Input: {input_numbers}, decoded {decoded_values}, Passed: {input_numbers == decoded_values}")
    assert input_numbers == decoded_values


def test_enc_dec_real_ieee754_32__endian_single_value(acn_encoder: ACNEncoder, seed: int) -> None:
    input_number: float = get_random_float(precision="single")
    _test_single_float(acn_encoder, input_number)


def test_enc_dec_real_ieee754_32_little_endian_multiple_values(acn_encoder: ACNEncoder, seed: int) -> None:
    input_numbers: list[float] = []
    for i in range(random.randint(3, 10)):
        input_numbers.append(get_random_float(precision="single"))

    _test_multiple_floats(acn_encoder, input_numbers)


def test_enc_dec_real_ieee754_32_little_endian_zeros(acn_encoder: ACNEncoder, seed: int) -> None:
    input_number: list[float] = get_zero_and_special_floats()
    _test_multiple_floats(acn_encoder, input_number)


def test_enc_dec_real_ieee754_32_little_endian_max_value(acn_encoder: ACNEncoder, seed: int) -> None:
    input_number: list[float] = [get_big_float(precision="single", positive=True), get_big_float(precision="single", positive=False)]
    _test_multiple_floats(acn_encoder, input_number)

def test_enc_dec_real_ieee754_32_little_endian_exceed_max_value(acn_encoder: ACNEncoder, seed: int) -> None:
    input_number: float = get_big_float(precision="single", positive=True) + 1
    encoded_res = acn_encoder.enc_real_ieee754_32_little_endian(input_number)
    assert encoded_res.success

    input_number: float = get_big_float(precision="single", positive=False) - 1
    encoded_res = acn_encoder.enc_real_ieee754_32_little_endian(input_number)
    assert encoded_res.success
