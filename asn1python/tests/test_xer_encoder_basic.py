from asn1python.xer_encoder import XEREncoder


def test_of_size_does_not_preallocate_and_starts_empty():
    enc = XEREncoder.of_size(10_000_000_000)  # huge hint must NOT allocate
    assert enc.get_xml() == ""


def test_write_raw_accumulates():
    enc = XEREncoder.of_size()
    enc.write_raw("<a>")
    enc.write_raw("1")
    enc.write_raw("</a>")
    assert enc.get_xml() == "<a>1</a>"


def test_get_bitstream_buffer_is_utf8():
    enc = XEREncoder.of_size()
    enc.write_raw("<x>é</x>")
    assert enc.get_bitstream_buffer() == bytearray("<x>é</x>".encode("utf-8"))
