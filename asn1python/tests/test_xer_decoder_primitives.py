# asn1python/tests/test_xer_decoder_primitives.py
from asn1python.xer_encoder import XEREncoder
from asn1python.xer_decoder import XERDecoder

def _rt_int(v):
    e = XEREncoder.of_size(); e.encode_integer("x", v, 0)
    return e.get_decoder().decode_integer("x")

def test_integer_roundtrip():
    assert _rt_int(-1234) == -1234

def test_boolean_roundtrip():
    # Wrap in a root element so iterparse gets valid single-root XML.
    e = XEREncoder.of_size()
    e.complex_start("wrap", 0)
    e.encode_boolean("b", True, 1)
    e.encode_boolean("b", False, 1)
    e.complex_end("wrap", 0)
    d = e.get_decoder()
    d.complex_start("wrap")
    assert d.decode_boolean("b") is True
    assert d.decode_boolean("b") is False
    d.complex_end("wrap")

def test_null_roundtrip():
    e = XEREncoder.of_size(); e.encode_null("n", 0)
    assert e.get_decoder().decode_null("n") is None

def test_octet_string_roundtrip():
    e = XEREncoder.of_size(); e.encode_octet_string("o", bytes([0x0a, 0xff]), 2, 0)
    assert e.get_decoder().decode_octet_string("o") == bytes([0x0a, 0xff])

def test_enumerated_roundtrip():
    e = XEREncoder.of_size(); e.encode_enumerated("c", "green", 0)
    assert e.get_decoder().decode_enumerated("c") == "green"
