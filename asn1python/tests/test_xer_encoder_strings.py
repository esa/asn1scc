from asn1python.xer_encoder import XEREncoder

def test_octet_string_hex_upper():
    e = XEREncoder.of_size()
    e.encode_octet_string("o", bytes([0x0a, 0x1b, 0xff]), 3, 0)
    assert e.get_xml() == "<o>0A1BFF</o>\n"

def test_octet_string_respects_num_bytes():
    e = XEREncoder.of_size()
    e.encode_octet_string("o", bytes([1, 2, 3, 4]), 2, 0)
    assert e.get_xml() == "<o>0102</o>\n"

def test_bit_string():
    e = XEREncoder.of_size()
    e.encode_bit_string("bs", "10110000", 4, 0)
    assert e.get_xml() == "<bs>1011</bs>\n"
