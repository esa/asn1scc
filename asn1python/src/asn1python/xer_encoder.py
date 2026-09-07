"""
ASN.1 XER (XML Encoding Rules) encoder.

This module provides the XEREncoder for encoding ASN.1 values to XML.
"""

from typing import List
from .xer_codec import XER_INDENT_UNIT, xml_escape


class XEREncoder:
    """
    XER encoder that builds XML text incrementally.

    XER is text-based, so we accumulate output in a list and join at the end.
    No pre-allocation occurs — the size hint to of_size() is ignored.
    """

    def __init__(self) -> None:
        self._parts: List[str] = []

    @classmethod
    def of_size(cls, buffer_byte_size: int = 0) -> "XEREncoder":
        """
        Create a new XER encoder.

        XER is text: build incrementally, never pre-allocate the worst case.
        The buffer_byte_size hint is ignored.

        Args:
            buffer_byte_size: Ignored; provided for API consistency with other codecs.

        Returns:
            A new XEREncoder instance.
        """
        return cls()

    @classmethod
    def empty(cls) -> "XEREncoder":
        """
        Create a new XER encoder over a fresh, growable buffer.

        This is the go-to way to create an encoder when the output size is not
        known in advance. XER is text and always builds incrementally, so this
        is equivalent to of_size() without a hint; it exists for API consistency
        with the binary encoders.

        Returns:
            A new XEREncoder instance.
        """
        return cls()

    def write_raw(self, text: str) -> None:
        """
        Write raw text to the encoder.

        Args:
            text: The text to append to the output.
        """
        self._parts.append(text)

    def indent(self, level: int) -> None:
        """
        Write indentation for the given nesting level.

        Args:
            level: The nesting level; each level is 4 spaces.
        """
        if level > 0:
            self._parts.append(XER_INDENT_UNIT * level)

    def get_xml(self) -> str:
        """
        Get the accumulated XML text.

        Returns:
            The complete XML as a string.
        """
        return "".join(self._parts)

    def get_bitstream_buffer(self) -> bytearray:
        """
        Get the XML as a UTF-8 encoded bytearray.

        This allows the existing template layer (e.g., Codec_write_bitstreamToFile)
        to work unchanged by encoding the XML text as bytes.

        Returns:
            The XML encoded as UTF-8 bytes in a bytearray.
        """
        return bytearray(self.get_xml().encode("utf-8"))

    def encode_primitive(self, tag: str, value_text: str, level: int) -> None:
        """
        Emit a simple element with text content: <indent><tag>value</tag>\n

        Args:
            tag: The element tag name.
            value_text: The text content (already formatted for XML).
            level: The nesting level for indentation.
        """
        self.indent(level)
        if tag:
            self._parts.append(f"<{tag}>{value_text}</{tag}>\n")
        else:
            # Empty tag: emit the value content directly (e.g. Boolean with no wrapper tag in SEQUENCE OF)
            self._parts.append(f"{value_text}\n")

    def encode_integer(self, tag: str, value: int, level: int) -> None:
        """
        Encode an integer as <tag>value</tag>.

        Args:
            tag: The element tag name.
            value: The integer value.
            level: The nesting level for indentation.
        """
        self.encode_primitive(tag, str(int(value)), level)

    def encode_boolean(self, tag: str, value: bool, level: int) -> None:
        """
        Encode a boolean as <tag><true/></tag> or <tag><false/></tag>.

        Args:
            tag: The element tag name.
            value: The boolean value.
            level: The nesting level for indentation.
        """
        self.encode_primitive(tag, "<true/>" if value else "<false/>", level)

    def encode_real(self, tag: str, value: float, level: int) -> None:
        """
        Encode a floating-point number using Python's repr().

        Args:
            tag: The element tag name.
            value: The floating-point value.
            level: The nesting level for indentation.
        """
        self.encode_primitive(tag, repr(float(value)), level)

    def encode_null(self, tag: str, level: int) -> None:
        """
        Emit a self-closing element: <indent><tag />\n

        Args:
            tag: The element tag name.
            level: The nesting level for indentation.
        """
        self.indent(level)
        self._parts.append(f"<{tag} />\n")

    def encode_string(self, tag: str, value: str, level: int) -> None:
        """
        Encode a string with XML escaping: <tag>escaped_value</tag>.

        Args:
            tag: The element tag name.
            value: The string value.
            level: The nesting level for indentation.
        """
        self.encode_primitive(tag, xml_escape(value), level)

    def encode_enumerated(self, tag: str, xer_value: str, level: int) -> None:
        """
        Encode an enumerated value as <tag><xer_value /></tag>.

        Args:
            tag: The element tag name.
            xer_value: The enumeration value name (unescaped tag).
            level: The nesting level for indentation.
        """
        self.encode_primitive(tag, f"<{xer_value} />", level)

    def complex_start(self, tag: str, level: int) -> None:
        """
        Emit an opening element: <indent><tag>\n

        Args:
            tag: The element tag name.
            level: The nesting level for indentation.
        """
        self.indent(level)
        self._parts.append(f"<{tag}>\n")

    def complex_end(self, tag: str, level: int) -> None:
        """
        Emit a closing element: <indent></tag>\n

        Args:
            tag: The element tag name.
            level: The nesting level for indentation.
        """
        self.indent(level)
        self._parts.append(f"</{tag}>\n")

    def encode_octet_string(self, tag: str, data: bytes, num_bytes: int, level: int) -> None:
        """
        Encode an octet string as uppercase hex: <tag>0A1B</tag>.

        Args:
            tag: The element tag name.
            data: The bytes to encode.
            num_bytes: The number of bytes to encode.
            level: The nesting level for indentation.
        """
        hex_text = bytes(data[:num_bytes]).hex().upper()
        self.encode_primitive(tag, hex_text, level)

    def encode_bit_string(self, tag: str, bits: str, num_bits: int, level: int) -> None:
        """
        Encode a bit string as a sequence of 0/1 characters: <tag>1011</tag>.

        Args:
            tag: The element tag name.
            bits: A string of '0' and '1' characters.
            num_bits: The number of bits to encode (first num_bits chars).
            level: The nesting level for indentation.
        """
        self.encode_primitive(tag, bits[:num_bits], level)

    def get_decoder(self) -> "XERDecoder":
        """
        Create an XERDecoder that reads back the XML accumulated in this encoder.

        Returns:
            An XERDecoder positioned at the start of the encoded XML.
        """
        from .xer_decoder import XERDecoder
        return XERDecoder.from_codec(self)
