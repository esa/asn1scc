"""
ASN.1 XER (XML Encoding Rules) decoder.

This module provides the XERDecoder for decoding ASN.1 values from XML,
using xml.etree.ElementTree.iterparse for bounded-memory streaming decoding.
"""

from io import BytesIO
from typing import Optional, Tuple
import xml.etree.ElementTree as ET
from .asn1_exceptions import Asn1InvalidValueException


def _local(tag: str) -> str:
    """Strip any XML namespace prefix, returning only the local tag name."""
    return tag.rsplit("}", 1)[-1]


class XERDecoder:
    """
    XER decoder that streams XML events via iterparse.

    Maintains a one-event lookahead so callers can ask "is the next start
    element <tag>?" without consuming it. Calls elem.clear() on end events
    to keep memory bounded.
    """

    def __init__(self, buffer: bytearray) -> None:
        self._iter = ET.iterparse(BytesIO(bytes(buffer)), events=("start", "end"))
        self._lookahead: Optional[Tuple[str, ET.Element]] = None
        self._advance()

    @classmethod
    def from_buffer(cls, buffer: bytearray) -> "XERDecoder":
        """
        Create a decoder from a bytearray of XML-encoded data.

        Args:
            buffer: UTF-8 encoded XML bytes.

        Returns:
            A new XERDecoder instance.
        """
        return cls(buffer)

    @classmethod
    def from_codec(cls, codec) -> "XERDecoder":
        """
        Create a decoder from an XEREncoder instance.

        Args:
            codec: An XEREncoder whose get_bitstream_buffer() provides the XML.

        Returns:
            A new XERDecoder instance.
        """
        return cls(codec.get_bitstream_buffer())

    def _advance(self) -> None:
        """Pull the next event from the iterator into the lookahead slot."""
        try:
            ev, el = next(self._iter)
            self._lookahead = (ev, el)
        except StopIteration:
            self._lookahead = None

    def peek_start_tag(self) -> Optional[str]:
        """
        Return the local tag name of the next start element, or None.

        Does not consume the event.
        """
        if self._lookahead and self._lookahead[0] == "start":
            return _local(self._lookahead[1].tag)
        return None

    def next_start_element_is(self, tag: str) -> bool:
        """
        Return True if the next event is a start element with the given local tag.

        Does not consume the event.
        """
        return self.peek_start_tag() == tag

    def at_end_element(self, tag: str) -> bool:
        """
        Return True if the next event is the end of the element with the given local tag.

        Used for SEQUENCE OF loop termination.
        """
        return (
            self._lookahead is not None
            and self._lookahead[0] == "end"
            and _local(self._lookahead[1].tag) == tag
        )

    def expect_start(self, tag: str) -> ET.Element:
        """
        Consume the start event for <tag>.

        Args:
            tag: The expected element local tag name.

        Returns:
            The Element object (for text access etc.).

        Raises:
            Asn1InvalidValueException: If the next event is not a start of <tag>.
        """
        if not self.next_start_element_is(tag):
            raise Asn1InvalidValueException(
                f"XER decode: expected <{tag}>, got {self.peek_start_tag()}",
                field_name=tag,
            )
        el = self._lookahead[1]
        self._advance()
        return el

    def expect_end(self, tag: str) -> None:
        """
        Consume the matching end event for </tag>.

        Tracks nesting depth so that nested elements with the same tag name are
        correctly skipped rather than stopping at an inner </tag>.

        Args:
            tag: The element local tag name whose end to consume.

        Raises:
            Asn1InvalidValueException: If the stream ends before the end event is found.
        """
        depth = 0
        while True:
            if self._lookahead is None:
                raise Asn1InvalidValueException(
                    f"XER decode: missing </{tag}>", field_name=tag
                )
            ev, el = self._lookahead
            local = _local(el.tag)
            if ev == "start" and local == tag:
                depth += 1
                self._advance()
            elif ev == "end" and local == tag:
                if depth == 0:
                    el.clear()
                    self._advance()
                    return
                depth -= 1
                self._advance()
            else:
                self._advance()

    def read_text_element(self, tag: str) -> str:
        """
        Consume <tag>...text...</tag> and return the text content.

        Tracks nesting depth so that nested elements with the same tag name are
        correctly skipped rather than stopping at an inner </tag>.

        Args:
            tag: The element local tag name.

        Returns:
            The text content of the element, or "" if empty.

        Raises:
            Asn1InvalidValueException: If the expected start or end tags are missing.
        """
        el = self.expect_start(tag)
        # Outer start is already consumed; track depth for any nested same-named elements.
        depth = 0
        while True:
            if self._lookahead is None:
                raise Asn1InvalidValueException(
                    f"XER decode: missing </{tag}>", field_name=tag
                )
            ev, cur = self._lookahead
            local = _local(cur.tag)
            if ev == "start" and local == tag:
                depth += 1
                self._advance()
            elif ev == "end" and local == tag:
                if depth == 0:
                    text = el.text or ""
                    el.clear()
                    self._advance()  # consume the outer end event
                    return text
                depth -= 1
                self._advance()
            else:
                self._advance()

    def decode_integer(self, tag: str) -> int:
        return int(self.read_text_element(tag).strip())

    def decode_real(self, tag: str) -> float:
        return float(self.read_text_element(tag).strip())

    def decode_string(self, tag: str) -> str:
        return self.read_text_element(tag)

    def decode_null(self, tag: str) -> None:
        self.expect_start(tag)
        self.expect_end(tag)
        return None

    def _read_single_child_tag(self, tag: str) -> str:
        self.expect_start(tag)
        child = self.peek_start_tag()
        if child is None:
            raise Asn1InvalidValueException(
                f"XER decode: <{tag}> expected a child element", field_name=tag)
        # consume the (empty) child: start then its end
        self.expect_start(child)
        self.expect_end(child)
        self.expect_end(tag)
        return child

    def decode_boolean(self, tag: str) -> bool:
        if tag:
            return self._read_single_child_tag(tag) == "true"
        else:
            # Empty tag: the boolean is encoded as a naked <true/> or <false/> element.
            child = self.peek_start_tag()
            self.expect_start(child)
            self.expect_end(child)
            return child == "true"

    def decode_enumerated(self, tag: str) -> str:
        return self._read_single_child_tag(tag)

    def decode_octet_string(self, tag: str) -> bytes:
        text = self.read_text_element(tag).strip()
        return bytes.fromhex(text) if text else b""

    def decode_bit_string(self, tag: str) -> str:
        return self.read_text_element(tag).strip()

    def complex_start(self, tag: str) -> ET.Element:
        return self.expect_start(tag)

    def complex_end(self, tag: str) -> None:
        self.expect_end(tag)
