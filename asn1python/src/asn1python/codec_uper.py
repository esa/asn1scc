"""
ASN.1 Python Runtime Library - UPER Codec

This module provides UPER (Unaligned Packed Encoding Rules) encoding and decoding.
"""
from .decoder import Decoder
from .encoder import Encoder

class UPERDecoder(Decoder):
    """
    UPER decoder implementation.

    This decoder provides flexible binary decoding for ASN.1 types
    following custom UPER encoding rules to support legacy protocols.
    """
    pass

class UPEREncoder(Encoder):
    """
    UPER encoder implementation.

    This encoder provides flexible binary encoding for ASN.1 types
    following custom UPER encoding rules to support legacy protocols.
    """

    def get_decoder(self) -> UPERDecoder:        
        instance = UPERDecoder.from_codec(self)
        assert isinstance(instance, UPERDecoder)
        return instance