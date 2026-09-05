from asn1python.xer_encoder import XEREncoder


def test_integer():
    e = XEREncoder.of_size(); e.encode_integer("age", 42, 0)
    assert e.get_xml() == "<age>42</age>\n"


def test_boolean_true_false():
    e = XEREncoder.of_size()
    e.encode_boolean("b", True, 0); e.encode_boolean("b", False, 0)
    assert e.get_xml() == "<b><true/></b>\n<b><false/></b>\n"


def test_null():
    e = XEREncoder.of_size(); e.encode_null("n", 0)
    assert e.get_xml() == "<n />\n"


def test_string_escaped():
    e = XEREncoder.of_size(); e.encode_string("s", "a<b&c", 0)
    assert e.get_xml() == "<s>a&lt;b&amp;c</s>\n"


def test_enumerated():
    e = XEREncoder.of_size(); e.encode_enumerated("color", "red", 0)
    assert e.get_xml() == "<color><red /></color>\n"


def test_complex_start_end_with_indent():
    e = XEREncoder.of_size()
    e.complex_start("seq", 0); e.encode_integer("x", 1, 1); e.complex_end("seq", 0)
    assert e.get_xml() == "<seq>\n    <x>1</x>\n</seq>\n"
