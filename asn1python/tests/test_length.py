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


# General length (X.691 10.9) of uPER CONTAINING: only the layouts an encoder writes are accepted (Issue 154).
def _general_length_stream(parts) -> bytearray:
    """header octets / two-octet lengths (> 0xFF), each followed by its units (zero octets)"""
    buf = bytearray()
    for header, n in parts:
        buf += header.to_bytes(2 if header > 0xFF else 1, "big") + bytes(n)
    return buf + bytes(8)


@pytest.mark.parametrize("parts", [
    [(0xC2, 32768), (0x00, 0)],
    [(0xC1, 16384), (0x02, 2)],
    [(0xC1, 16384), (0x8080, 128)],
    [(0x8080, 128)],
    [(0x7F, 127)],
])
def test_decode_general_length_canonical(parts) -> None:
    from asn1python.codec_uper import UPERDecoder
    decoder = UPERDecoder.from_buffer(_general_length_stream(parts))
    res = decoder.decode_general_length(32768, 8)
    assert res.success
    assert len(res.decoded_value) == sum(n for _, n in parts)
    assert decoder.bit_index == 8 * sum(n + (2 if h > 0xFF else 1) for h, n in parts)


@pytest.mark.parametrize("parts, bit_index", [
    # (1) a fragment header after a fragment of 16K .. 48K units: rejected right after that header
    ([(0xC1, 16384), (0xC1, 16384), (0x00, 0)], 8 * 16386),
    ([(0xC2, 32768), (0xC1, 0)], 8 * 32770),
    # (2) a two-octet length below 128: rejected right after its second octet
    ([(0x8005, 5)], 16),
])
def test_decode_general_length_non_canonical(parts, bit_index: int) -> None:
    from asn1python.codec_uper import UPERDecoder
    decoder = UPERDecoder.from_buffer(_general_length_stream(parts))
    res = decoder.decode_general_length(32768, 8)
    assert not res.success
    assert decoder.bit_index == bit_index
