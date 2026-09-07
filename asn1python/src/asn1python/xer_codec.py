"""
ASN.1 XER (XML Encoding Rules) codec base module.

This module provides shared XER functionality including indentation and XML escaping.
"""

XER_INDENT_UNIT = "    "  # 4 spaces per nesting level


def xml_escape(text: str) -> str:
    """Escape special XML characters in text."""
    return (text.replace("&", "&amp;").replace("<", "&lt;")
                .replace(">", "&gt;"))
