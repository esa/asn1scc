import random

from asn1python.acn_decoder import ACNDecoder
from asn1python.acn_encoder import ACNEncoder
from conftest import get_random_signed, get_signed_max, get_signed_min, get_unsigned_max, get_unsigned_min


def _encode_and_decode_single_twos_complement(acn_encoder: ACNEncoder, input_number: int, bit: int) -> tuple[bool, int]:
    """Helper function to encode and decode a single two's complement value.
    
    Returns:
        Tuple of (success, decoded_value)
    """
    encoded_res = acn_encoder.enc_int_twos_complement_const_size(input_number, bit)
    if not encoded_res.success:
        return False, 0
        
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_int_twos_complement_const_size(bit)
    
    if not decoded_res.success:
        return False, 0
        
    return True, decoded_res.decoded_value


def _encode_and_decode_multiple_twos_complement(acn_encoder: ACNEncoder, input_numbers: list[int], bit: int) -> tuple[bool, list[int]]:
    """Helper function to encode and decode multiple two's complement values.
    
    Returns:
        Tuple of (success, decoded_values_list)
    """
    # Encode all values first
    for input_number in input_numbers:
        encoded_res = acn_encoder.enc_int_twos_complement_const_size(input_number, bit)
        if not encoded_res.success:
            return False, []
    
    # Decode all values
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_values = []
    
    for _ in range(len(input_numbers)):
        decoded_res = acn_decoder.dec_int_twos_complement_const_size(bit)
        if not decoded_res.success:
            return False, []
        decoded_values.append(decoded_res.decoded_value)
    
    return True, decoded_values


def _test_single_twos_complement(acn_encoder: ACNEncoder, input_number: int, bit: int) -> None:
    """Helper function to test encoding/decoding of a single two's complement value."""
    success, decoded_value = _encode_and_decode_single_twos_complement(acn_encoder, input_number, bit)
    assert success, f"Encoding/decoding failed for input {input_number}"
    
    print(f"Input: {input_number}, decoded {decoded_value}, Passed: {input_number == decoded_value}")
    assert input_number == decoded_value


def _test_multiple_twos_complement(acn_encoder: ACNEncoder, input_numbers: list[int], bit: int) -> None:
    """Helper function to test encoding/decoding of multiple two's complement values."""
    success, decoded_values = _encode_and_decode_multiple_twos_complement(acn_encoder, input_numbers, bit)
    assert success, f"Encoding/decoding failed for input {input_numbers}"
    
    print(f"Input: {input_numbers}, decoded {decoded_values}, Passed: {input_numbers == decoded_values}")
    assert input_numbers == decoded_values

def test_enc_dec_int_twos_complement_const_size_single_value(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_number: int = get_random_signed(bit)
    _test_single_twos_complement(acn_encoder, input_number, bit)


def test_enc_dec_int_twos_complement_const_size_multiple_values(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_numbers: list[int] = []
    for i in range(random.randint(3, 10)):
        input_numbers.append(get_random_signed(bit))
    
    _test_multiple_twos_complement(acn_encoder, input_numbers, bit)


def test_enc_dec_int_twos_complement_const_size_zero(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_number: int = 0
    _test_single_twos_complement(acn_encoder, input_number, bit)


def test_enc_dec_int_twos_complement_const_size_max_value(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_numbers: list[int] = [get_signed_max(bit), get_signed_min(bit)]
    _test_multiple_twos_complement(acn_encoder, input_numbers, bit)


def test_enc_dec_int_twos_complement_const_size_exceed_max_value(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_number: int = get_unsigned_max(bit) + 1
    encoded_res = acn_encoder.enc_int_twos_complement_const_size(input_number, bit)
    assert not encoded_res.success

    input_number: int = -get_unsigned_max(bit) - 2
    encoded_res = acn_encoder.enc_int_twos_complement_const_size(input_number, bit)
    assert not encoded_res.success
