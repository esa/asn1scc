"""
Unit tests for the .empty() encoder initializer.

.empty() creates an encoder over a fresh, growable buffer without requiring a
size estimate up front. These tests cover the backlog acceptance criteria:
  - each encoder can be created via .empty()
  - encoding into an .empty() encoder yields the same bytes as an of_size() one
  - a round-trip (encode with .empty(), decode with from_buffer) is lossless
and guard the bounded-buffer semantics that of_size()/from_buffer() rely on.
"""
import pytest

from asn1python.acn_encoder import ACNEncoder
from asn1python.acn_decoder import ACNDecoder
from asn1python.codec_uper import UPEREncoder, UPERDecoder
from asn1python.bitstream import BitStream, BitStreamError

try:
    from asn1python.xer_encoder import XEREncoder
    HAS_XER = True
except ImportError:
    HAS_XER = False


BINARY_ENCODERS = [ACNEncoder, UPEREncoder]


def _encode_sample(encoder) -> None:
    """Encode a fixed, mixed-width sequence of values into `encoder`."""
    assert encoder.encode_integer(42, min_val=0, max_val=255).success
    assert encoder.append_bit(True).success
    assert encoder.encode_integer(-7, min_val=-128, max_val=127).success
    assert encoder.append_byte(0xAB).success
    assert encoder.encode_integer(1000, min_val=0, max_val=65535).success


@pytest.mark.parametrize("encoder_cls", BINARY_ENCODERS)
def test_empty_can_be_created(encoder_cls) -> None:
    encoder = encoder_cls.empty()
    assert isinstance(encoder, encoder_cls)


@pytest.mark.parametrize("encoder_cls", BINARY_ENCODERS)
def test_empty_matches_of_size_output(encoder_cls) -> None:
    empty_encoder = encoder_cls.empty()
    sized_encoder = encoder_cls.of_size(1024)

    _encode_sample(empty_encoder)
    _encode_sample(sized_encoder)

    assert empty_encoder.get_bitstream_buffer() == sized_encoder.get_bitstream_buffer()


@pytest.mark.parametrize("encoder_cls,decoder_cls", [
    (ACNEncoder, ACNDecoder),
    (UPEREncoder, UPERDecoder),
])
def test_empty_round_trip(encoder_cls, decoder_cls) -> None:
    encoder = encoder_cls.empty()
    assert encoder.encode_integer(12345, min_val=0, max_val=100000).success

    buffer = encoder.get_bitstream_buffer()
    decoder = decoder_cls.from_buffer(buffer)
    result = decoder.decode_integer(min_val=0, max_val=100000)

    assert result.success
    assert result.decoded_value == 12345


@pytest.mark.parametrize("encoder_cls", BINARY_ENCODERS)
def test_empty_grows_far_beyond_initial_capacity(encoder_cls) -> None:
    """An .empty() encoder must accept far more than any small initial buffer."""
    encoder = encoder_cls.empty()

    num_bytes = 5000
    for i in range(num_bytes):
        assert encoder.append_byte(i % 256).success

    buffer = encoder.get_bitstream_buffer()
    assert len(buffer) == num_bytes
    assert all(buffer[i] == i % 256 for i in range(num_bytes))


def test_fixed_buffer_still_raises_on_overflow() -> None:
    """Non-growable buffers (of_size / from_buffer / decoders) stay bounded."""
    stream = BitStream(bytearray(1))  # one byte, not growable
    stream.write_byte(0xFF)
    with pytest.raises(BitStreamError):
        stream.write_bit(True)


def test_growable_flag_survives_copy() -> None:
    """from_bitstream must preserve the growable flag (used by Codec.__init__)."""
    growable = BitStream(bytearray(), growable=True)
    copy = BitStream.from_bitstream(growable)
    copy.write_byte(0x01)  # would raise on a zero-length fixed buffer
    assert copy.get_data() == bytearray([0x01])

    fixed = BitStream(bytearray(1))
    fixed_copy = BitStream.from_bitstream(fixed)
    assert fixed_copy._growable is False


@pytest.mark.skipif(not HAS_XER, reason="XER backend not available")
def test_xer_empty() -> None:
    empty_encoder = XEREncoder.empty()
    sized_encoder = XEREncoder.of_size()

    for enc in (empty_encoder, sized_encoder):
        enc.encode_integer("value", 7, 0)

    assert empty_encoder.get_xml() == sized_encoder.get_xml()
