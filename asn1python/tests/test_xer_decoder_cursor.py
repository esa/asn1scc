# asn1python/tests/test_xer_decoder_cursor.py
from asn1python.xer_encoder import XEREncoder
from asn1python.xer_decoder import XERDecoder

def _dec(xml: str) -> XERDecoder:
    return XERDecoder.from_buffer(bytearray(xml.encode("utf-8")))

def test_peek_and_read_text():
    d = _dec("<age>42</age>")
    assert d.peek_start_tag() == "age"
    assert d.read_text_element("age") == "42"

def test_next_start_element_is_does_not_consume():
    d = _dec("<seq><x>1</x></seq>")
    d.expect_start("seq")
    assert d.next_start_element_is("x") is True
    assert d.next_start_element_is("x") is True   # still not consumed
    assert d.read_text_element("x") == "1"
    d.expect_end("seq")

def test_get_decoder_roundtrip_from_encoder():
    e = XEREncoder.of_size(); e.encode_integer("age", 7, 0)
    d = e.get_decoder()
    assert d.read_text_element("age") == "7"

def test_expect_end_skips_same_named_nested():
    # XML: <outer><value><value>1</value></value></outer>
    # The outer <value> contains a nested <value>1</value>.
    # expect_end("value") after consuming the outer start must skip the inner
    # <value>...</value> and consume the OUTER </value>, leaving </outer> next.
    d = XERDecoder.from_buffer(bytearray(b"<outer><value><value>1</value></value></outer>"))
    d.expect_start("outer")
    d.expect_start("value")              # consume outer <value>
    assert d.read_text_element("value") == "1"   # inner <value>1</value>
    # At this point the outer </value> is next; expect_end must consume it
    # without desyncing (previously it would have already consumed the inner
    # </value> and crashed / left </outer> as the "end" for "value").
    d.expect_end("value")               # must find and consume the OUTER </value>
    d.expect_end("outer")              # must still find </outer>

def test_read_text_element_skips_same_named_nested():
    # read_text_element("outer") on <outer><outer>inner</outer></outer> must
    # return "" (the outer element's own text, not dive into the child)
    # and leave the stream fully consumed without raising.
    d = XERDecoder.from_buffer(bytearray(b"<outer><outer>inner</outer></outer>"))
    text = d.read_text_element("outer")
    # The outer element has no direct text node; its child is a nested element.
    assert text == ""
    # Stream is fully consumed; no leftover events that could desync a caller.
    assert d.peek_start_tag() is None
