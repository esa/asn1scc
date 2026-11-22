import pytest

from asn1python.asn1_types import Asn1ObjectIdentifier
from asn1python.codec_uper import UPERDecoder, UPEREncoder


# uPER OBJECT IDENTIFIER / RELATIVE-OID (X.691 23 / 24): a general length (10.9: one octet below 128, two octets
# 10xxxxxx from 128) and the subidentifiers of X.690 8.19 (Issues 155 / 156). The decoders accept exactly what the
# encoders write; a rejection stops at the same bit position as the C runtime.
A9 = 1 << 62    # 9 contents octets
A10 = 1 << 63   # 10 contents octets
MAX = (1 << 64) - 1


def _oid(arcs) -> Asn1ObjectIdentifier:
    return Asn1ObjectIdentifier(len(arcs), list(arcs) + [0] * (20 - len(arcs)))


def _subid(v: int) -> bytes:
    """X.690 8.19.2 in base 128, bit 8 set on every octet but the last (independent of the runtime)"""
    out = [v & 0x7F]
    v >>= 7
    while v:
        out.insert(0, 0x80 | (v & 0x7F))
        v >>= 7
    return bytes(out)


def _contents(arcs, relative: bool) -> bytes:
    if relative:
        return b"".join(_subid(a) for a in arcs)
    return _subid(arcs[0] * 40 + arcs[1]) + b"".join(_subid(a) for a in arcs[2:])


def _with_length(contents: bytes) -> bytes:
    n = len(contents)
    return (bytes([n]) if n < 128 else bytes([0x80 | (n >> 8), n & 0xFF])) + contents


def _decode(stream: bytes, relative: bool):
    dec = UPERDecoder.from_buffer(bytearray(stream) + bytearray(400))
    res = dec.RelativeOID_decode() if relative else dec.ObjectIdentifier_decode()
    return res, dec.bit_index


@pytest.mark.parametrize("arcs, relative, length_octets, contents", [
    ([1, 2] + [A9] * 14, False, b"\x7f", 127),
    ([1, 2] + [A9] * 14 + [5], False, b"\x80\x80", 128),
    ([2, A10] + [A10] * 18, False, b"\x80\xbe", 190),
    ([2, MAX - 80] + [MAX] * 18, False, b"\x80\xbe", 190),
    ([A9] * 14 + [7, 8], True, b"\x80\x80", 128),
    ([MAX] * 20, True, b"\x80\xc8", 200),
])
def test_oid_length_forms(arcs, relative, length_octets, contents) -> None:
    enc = UPEREncoder.of_size(400)
    res = enc.RelativeOID_encode(_oid(arcs)) if relative else enc.ObjectIdentifier_encode(_oid(arcs))
    assert res.success
    buf = bytes(enc.get_bitstream_buffer()[:len(length_octets) + contents])
    assert buf == _with_length(_contents(arcs, relative))
    assert buf[:len(length_octets)] == length_octets
    assert enc.bit_index == 8 * (len(length_octets) + contents)
    res, at = _decode(buf, relative)
    assert res.success
    assert res.decoded_value.values[:len(arcs)] == arcs
    assert at == enc.bit_index


@pytest.mark.parametrize("stream, relative, bit_index", [
    # a two-octet length below 128: rejected after the second octet
    (b"\x80\x03\x2a\x03\x04", False, 16),
    (b"\x80\x01\x05", True, 16),
    (b"\x80\x00", False, 16),
    # a fragment header: rejected after its octet
    (b"\xc1" + bytes(16), False, 8),
    (b"\xff\xff", True, 8),
])
def test_oid_length_non_canonical(stream, relative, bit_index) -> None:
    res, at = _decode(stream, relative)
    assert not res.success
    assert at == bit_index


@pytest.mark.parametrize("relative", [False, True])
def test_oid_length_zero(relative) -> None:
    """no subidentifier: no OBJECT IDENTIFIER (2 arcs) nor RELATIVE-OID (1 arc) value, rejected by the runtime"""
    res, at = _decode(b"\x00", relative)
    assert not res.success
    assert at == 8


@pytest.mark.parametrize("stream, relative, bit_index", [
    # the contents length ends inside a subidentifier
    (b"\x02\x2a\x85", False, 24),
    (b"\x01\x85", True, 16),
    (b"\x80\x80" + _contents([2, A10] + [A10] * 18, False), False, 1040),
    # an arc beyond 64 bits: rejected at the octet that overflows
    (_with_length(_subid(1 << 64)), True, 88),
    (_with_length(_subid(1 << 64)), False, 88),
    (_with_length(_subid(1 << 70)), True, 96),
])
def test_oid_subidentifier_rejections(stream, relative, bit_index) -> None:
    res, at = _decode(stream, relative)
    assert not res.success
    assert at == bit_index


def test_oid_largest_arcs() -> None:
    res, at = _decode(_with_length(_subid(MAX)), True)
    assert res.success and res.decoded_value.values[:1] == [MAX] and at == 88
    res, at = _decode(_with_length(_subid(MAX)), False)
    assert res.success and res.decoded_value.values[:2] == [2, MAX - 80] and at == 88
