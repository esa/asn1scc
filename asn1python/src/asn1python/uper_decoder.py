from .decoder import Decoder
class UPERDecoder(Decoder):
    """
    UPER decoder implementation.

    This decoder provides flexible binary decoding for ASN.1 types
    following custom UPER encoding rules to support legacy protocols.
    """

    def __init__(self, buffer: bytearray) -> None:
        super().__init__(buffer=buffer)

    @classmethod
    def of_size(cls, buffer_byte_size: int = 1024 * 1024) -> 'UPERDecoder':
        return cls(bytearray(buffer_byte_size))

    @classmethod
    def _construct(cls, buffer: bytearray) -> 'UPERDecoder':
        return cls(buffer)