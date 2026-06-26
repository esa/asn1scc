"""
Unit tests for null

Methods to test:
  - encode_null
  - decode_null
"""

import pytest
from asn1python.acn_encoder import ACNEncoder
from asn1python.acn_decoder import ACNDecoder
from asn1python.encoder import Encoder
from asn1python.decoder import Decoder

def test_encode_decode_null(acn_encoder: ACNEncoder):
    encoded_res = acn_encoder.encode_null()
    assert encoded_res.success
    assert encoded_res.bits_encoded == 0
