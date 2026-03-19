from .encoder import Encoder

class UPEREncoder(Encoder):
    """
    UPER encoder implementation.

    This encoder provides flexible binary encoding for ASN.1 types
    following custom UPER encoding rules to support legacy protocols.
    """

    def __init__(self, buffer: bytearray) -> None:
        super().__init__(buffer)

    @classmethod
    def of_size(cls, buffer_byte_size: int = 1024 * 1024) -> 'UPEREncoder':
        return cls(bytearray(buffer_byte_size))

    @classmethod
    def _construct(cls, buffer: bytearray) -> 'UPEREncoder':
        return cls(buffer)

    def get_decoder(self) -> 'UPERDecoder':
        from .uper_decoder import UPERDecoder
        return UPERDecoder(self.get_bitstream_buffer())