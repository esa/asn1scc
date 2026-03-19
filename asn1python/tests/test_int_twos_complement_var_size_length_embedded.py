import random

from asn1python.acn_decoder import ACNDecoder
from asn1python.acn_encoder import ACNEncoder
from conftest import get_random_signed, get_signed_max, get_signed_min

def test_enc_dec_int_twos_complement_var_size_length_embedded_single_value(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_number: int = get_random_signed(bit)
    encoded_res = acn_encoder.enc_int_twos_complement_var_size_length_embedded(input_number)
    assert encoded_res.success

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()

    decoded_res = acn_decoder.dec_int_twos_complement_var_size_length_embedded()
    assert decoded_res.success
    print(f"Input: {input_number}, decoded {decoded_res.decoded_value}, Passed: {input_number == decoded_res.decoded_value}")
    assert input_number == decoded_res.decoded_value

def test_enc_dec_int_twos_complement_var_size_length_embedded_multiple_values(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_numbers: list[int] = []
    for i in range(random.randint(3, 10)):
        input_numbers.append(get_random_signed(bit))
    for i in input_numbers:
        encoded_res = acn_encoder.enc_int_twos_complement_var_size_length_embedded(i)
        assert encoded_res.success

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()

    decoded_res = []
    for _ in range(len(input_numbers)):
        res = acn_decoder.dec_int_twos_complement_var_size_length_embedded()
        assert res.success
        decoded_res.append(res.decoded_value)
    print(f"Input: {input_numbers}, decoded {decoded_res}, Passed: {input_numbers == decoded_res}")
    assert input_numbers == decoded_res

def test_enc_dec_int_twos_complement_var_size_length_embedded_zero(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_number: int = 0

    encoded_res = acn_encoder.enc_int_twos_complement_var_size_length_embedded(input_number)
    assert encoded_res.success
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_int_twos_complement_var_size_length_embedded()
    print(f"Input: {input_number}, decoded {decoded_res.decoded_value}, Passed: {input_number == decoded_res.decoded_value}")
    assert input_number == decoded_res.decoded_value

def test_enc_dec_int_twos_complement_var_size_length_embedded_max_value(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_numbers: list[int] = [get_signed_max(bit), get_signed_min(bit)]

    for inp_num in input_numbers:
        encoded_res = acn_encoder.enc_int_twos_complement_var_size_length_embedded(inp_num)
        assert encoded_res.success
    acn_decoder: ACNDecoder = acn_encoder.get_decoder()

    decoded_res: list[int]= []
    for _ in range(len(input_numbers)):
        res = acn_decoder.dec_int_twos_complement_var_size_length_embedded()
        assert res.success
        decoded_res.append(res.decoded_value)

    print(f"Input: {input_numbers}, decoded {decoded_res}, Passed: {input_numbers == decoded_res}")
    assert input_numbers == decoded_res

def test_enc_dec_int_twos_complement_var_size_length_embedded_exceed_max_value(acn_encoder: ACNEncoder, seed: int, bit: int) -> None:
    input_number: int = get_signed_max(bit) + 1
    encoded_res = acn_encoder.enc_int_twos_complement_var_size_length_embedded(input_number)
    assert encoded_res.success

    input_number: int = get_signed_min(bit) - 1
    encoded_res = acn_encoder.enc_int_twos_complement_var_size_length_embedded(input_number)
    assert encoded_res.success
