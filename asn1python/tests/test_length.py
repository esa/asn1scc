"""
Unit tests for dec_length, enc_length

TODO: Implement tests for the following methods:
  - dec_length
  - enc_length
"""

import pytest
from asn1python.acn_encoder import ACNEncoder
from asn1python.acn_decoder import ACNDecoder
from asn1python.encoder import Encoder
from asn1python.decoder import Decoder
from conftest import get_random_unsigned, get_unsigned_max


def test_enc_dec_length(acn_encoder: ACNEncoder, bit: int) -> None:
    val: int = get_random_unsigned(bit)
    encoded_res = acn_encoder.enc_length(val, bit)

    assert encoded_res.success
    assert encoded_res.bits_encoded == bit

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_length(bit)

    assert decoded_res.success
    assert decoded_res.decoded_value == val

def test_enc_dec_length_max(acn_encoder: ACNEncoder, bit: int) -> None:
    val: int = get_unsigned_max(bit)
    encoded_res = acn_encoder.enc_length(val, bit)

    assert encoded_res.success
    assert encoded_res.bits_encoded == bit

    acn_decoder: ACNDecoder = acn_encoder.get_decoder()
    decoded_res = acn_decoder.dec_length(bit)

    assert decoded_res.success
    assert decoded_res.decoded_value == val

def test_enc_dec_length_max_exceeded(acn_encoder: ACNEncoder, bit: int) -> None:
    val: int = get_unsigned_max(bit) + 1
    encoded_res = acn_encoder.enc_length(val, bit)

    assert not encoded_res.success
    assert encoded_res.bits_encoded == 0

def test_enc_dec_length_min_exceeded(acn_encoder: ACNEncoder) -> None:
    val: int = -1
    encoded_res = acn_encoder.enc_length(val, 8)

    assert not encoded_res.success
    assert encoded_res.bits_encoded == 0
