//! ASN1SCC Rust runtime — XER (XML Encoding Rules) encode / decode.
//!
//! This module is the Rust equivalent of the C runtime's
//! `asn1crt_encoding_xer.h` / `asn1crt_encoding_xer.c`.  It provides
//! XML-based encoding and decoding for all ASN.1 primitive types as well
//! as the complex-element scaffolding and the XML state-machine file
//! loader used by the XER test harness.
//!
//! # Design notes
//!
//! The C implementation uses a `ByteStream` for both reading and writing
//! XML text.  We follow the same model: encode functions write into a
//! `&mut ByteStream`, decode functions read from a `&mut ByteStream`.
//!
//! A small lexical scanner (`nt` / `la`) tokenises the XML stream on
//! demand.  Tokens are either single special characters (`<`, `>`, `/`,
//! `=`, `"`) or "words" (`WORD_ID`) composed of alphanumeric characters
//! plus `.`, `+`, `-`, `_`.
//!
//! The XML file loader (`load_xml_file`) strips insignificant whitespace
//! from an XML file by running a 7-state finite-state machine over the
//! raw character stream, emitting only significant characters into the
//! output `ByteStream`.

use crate::*;
use std::fs::File;
use std::io::Read;

// ─────────────────────────────────────────────────────────────────────────
//  Constants
// ─────────────────────────────────────────────────────────────────────────

/// Token ID for a "word" (identifier / value), mirroring `WORD_ID` in C.
const WORD_ID: i32 = 1000;

/// Maximum token value length (must match `Token::value` array size in lib.rs).
const TOKEN_VALUE_MAX: usize = 100;

/// Temporary buffer size used by `load_xml_file`.
const TMP_BUFFER_SIZE: usize = 4096;

/// Default XML header written by `encode_xml_header`.
const DEFAULT_XML_HEADER: &str = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";

// ─────────────────────────────────────────────────────────────────────────
//  XML State machine states
// ─────────────────────────────────────────────────────────────────────────

/// States of the whitespace-stripping XML state machine in `load_xml_file`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
enum XmlState {
    /// Initial state — outside any tag.
    Start = 0,
    /// Inside `<?xml … ?>` header.
    Header = 1,
    /// Inside `<tag …>`.
    StartTag = 2,
    /// Inside element content (between `>` and `<`).
    Content = 3,
    /// Inside `</tag>`.
    EndTag = 4,
    /// Between sibling elements (mixed content / whitespace).
    MixedContent = 5,
    /// Inside `<!-- … -->`.
    Comment = 6,
}

// ─────────────────────────────────────────────────────────────────────────
//  Helper: string ↔ fixed-size byte array conversions
// ─────────────────────────────────────────────────────────────────────────

/// Copy a Rust string slice into a fixed-size byte buffer (NUL-terminated
/// style, matching the C `char` arrays in `Token` / `XmlAttribute`).
/// Returns the number of bytes written (excluding the implicit NUL fill).
fn write_str_to_buf(buf: &mut [u8], s: &str) -> usize {
    let bytes = s.as_bytes();
    let len = bytes.len().min(buf.len().saturating_sub(1));
    buf[..len].copy_from_slice(&bytes[..len]);
    for b in &mut buf[len..] {
        *b = 0;
    }
    len
}

/// Read a NUL-terminated C-style string from a byte slice as a Rust `String`.
fn read_str_from_buf(buf: &[u8]) -> String {
    let len = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    String::from_utf8_lossy(&buf[..len]).into_owned()
}

// ─────────────────────────────────────────────────────────────────────────
//  Number → string helpers (replace C Int2String / UInt2String / Double2String)
// ─────────────────────────────────────────────────────────────────────────

/// Format a signed integer as a decimal string.
fn int_to_string(v: Asn1SccSint) -> String {
    v.to_string()
}

/// Format an unsigned integer as a decimal string.
fn uint_to_string(v: Asn1SccUint) -> String {
    v.to_string()
}

/// Format a `f64` in the canonical XER form used by asn1scc.
///
/// Near-zero values produce `"0"`.  Otherwise the value is normalised
/// so that `1.0 <= |v| < 10.0`, and the mantissa is printed with enough
/// digits (1 if integral, up to 17 otherwise) followed by `E<exponent>`.
fn double_to_string(mut v: f64) -> String {
    if v.abs() < 1e-17 {
        return "0".to_string();
    }

    let mut exponent: i32 = 0;

    while v.abs() >= 10.0 {
        v /= 10.0;
        exponent += 1;
    }
    while v.abs() < 1.0 {
        v *= 10.0;
        exponent -= 1;
    }

    let truncated = v.trunc();
    if (v.abs() - truncated.abs()).abs() < 1e-17 {
        // Integral mantissa → one decimal place.
        format!("{:.1}E{}", v, exponent)
    } else {
        format!("{:.17}E{}", v, exponent)
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  ByteStream character I/O helpers
// ─────────────────────────────────────────────────────────────────────────

/// Read the next character from the stream, advancing the cursor.
/// Returns `Some(char)` on success, `None` at end of stream.
///
/// Mirrors C `GetNextChar`.
fn get_next_char(p_strm: &mut ByteStream) -> Option<u8> {
    if p_strm.current_byte >= p_strm.count {
        return None;
    }
    let c = p_strm.buf[p_strm.current_byte as usize];
    p_strm.current_byte += 1;
    Some(c)
}

/// Push back the cursor by one byte.
///
/// Mirrors C `PushBackChar`.  The C version asserts `currentByte >= 0`;
/// in Rust we saturate at 0 to avoid panics on underflow in release builds.
fn push_back_char(p_strm: &mut ByteStream) {
    if p_strm.current_byte > 0 {
        p_strm.current_byte -= 1;
    }
}

/// Write `2 * level` space characters (indentation) to the stream.
/// Returns `false` on buffer overflow.
///
/// Mirrors C `ByteStream_PutSpace`.
fn put_space(p_strm: &mut ByteStream, level: i32) -> bool {
    if !p_strm.encode_white_space {
        return true;
    }
    if level < 0 {
        return true;
    }
    let len = 2i64 * level as i64;
    if p_strm.current_byte + len > p_strm.count {
        return false;
    }
    for _ in 0..len {
        p_strm.buf[p_strm.current_byte as usize] = b' ';
        p_strm.current_byte += 1;
    }
    true
}

/// Write a newline character to the stream.
/// Returns `false` on buffer overflow.
///
/// Mirrors C `ByteStream_PutNL`.
fn put_nl(p_strm: &mut ByteStream) -> bool {
    if !p_strm.encode_white_space {
        return true;
    }
    if p_strm.current_byte >= p_strm.count {
        return false;
    }
    p_strm.buf[p_strm.current_byte as usize] = b'\n';
    p_strm.current_byte += 1;
    true
}

/// Append a string to the stream.
/// Returns `false` on buffer overflow.
///
/// Mirrors C `ByteStream_AppendString`.
fn append_string(p_strm: &mut ByteStream, v: &str) -> bool {
    let bytes = v.as_bytes();
    let len = bytes.len() as i64;
    if p_strm.current_byte + len > p_strm.count {
        return false;
    }
    let start = p_strm.current_byte as usize;
    p_strm.buf[start..start + bytes.len()].copy_from_slice(bytes);
    p_strm.current_byte += len;
    true
}

/// Append a single character to the stream.
/// Returns `false` on buffer overflow.
///
/// Mirrors C `ByteStream_AppendChar` (defined locally in the C XER file).
fn append_char(p_strm: &mut ByteStream, v: u8) -> bool {
    if p_strm.current_byte >= p_strm.count {
        return false;
    }
    p_strm.buf[p_strm.current_byte as usize] = v;
    p_strm.current_byte += 1;
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Token helpers
// ─────────────────────────────────────────────────────────────────────────

/// Returns `true` when `c` is a valid character within an XML "word"
/// (identifier or value).  Mirrors C `isPartOfID`.
fn is_part_of_id(c: u8) -> bool {
    c.is_ascii_alphabetic()
        || c.is_ascii_digit()
        || c == b'.'
        || c == b'+'
        || c == b'-'
        || c == b'_'
}

/// Skip whitespace in the stream (spaces, tabs, newlines, carriage returns).
fn skip_whitespace(p_strm: &mut ByteStream) {
    while p_strm.current_byte < p_strm.count
        && (p_strm.buf[p_strm.current_byte as usize] as char).is_whitespace()
    {
        p_strm.current_byte += 1;
    }
}

/// Read the next token from the XML stream.
///
/// This is the core lexical analyser, mirroring C `NT`.  It skips leading
/// whitespace, transparently consumes XML declarations (`<?…?>`) and
/// comments (`<!--…-->`), then returns either a single special character
/// token (`<`, `>`, `/`, `=`, `"`) or a `WORD_ID` token containing up to
/// `TOKEN_VALUE_MAX - 1` characters.
fn nt(p_strm: &mut ByteStream) -> Token {
    let mut ret = Token::new();

    // Skip whitespace.
    skip_whitespace(p_strm);

    if p_strm.current_byte >= p_strm.count {
        return ret; // token_id == 0 (EOF)
    }

    // Skip XML declaration <? ... ?>
    if p_strm.current_byte + 1 < p_strm.count
        && p_strm.buf[p_strm.current_byte as usize] == b'<'
        && p_strm.buf[(p_strm.current_byte + 1) as usize] == b'?'
    {
        p_strm.current_byte += 1; // consume '<'
        while p_strm.current_byte < p_strm.count
            && !(p_strm.buf[(p_strm.current_byte - 1) as usize] == b'?'
                && p_strm.buf[p_strm.current_byte as usize] == b'>')
        {
            p_strm.current_byte += 1;
        }
        p_strm.current_byte += 1; // consume '>'
        skip_whitespace(p_strm);
    }

    if p_strm.current_byte >= p_strm.count {
        return ret;
    }

    // Skip comments <!-- ... -->
    if p_strm.current_byte + 3 < p_strm.count
        && p_strm.buf[p_strm.current_byte as usize] == b'<'
        && p_strm.buf[(p_strm.current_byte + 1) as usize] == b'!'
        && p_strm.buf[(p_strm.current_byte + 2) as usize] == b'-'
        && p_strm.buf[(p_strm.current_byte + 3) as usize] == b'-'
    {
        p_strm.current_byte += 1; // consume '<'
        p_strm.current_byte += 1; // consume '!'
        while p_strm.current_byte < p_strm.count
            && !(p_strm.buf[(p_strm.current_byte - 2) as usize] == b'-'
                && p_strm.buf[(p_strm.current_byte - 1) as usize] == b'-'
                && p_strm.buf[p_strm.current_byte as usize] == b'>')
        {
            p_strm.current_byte += 1;
        }
        p_strm.current_byte += 1; // consume '>'
        skip_whitespace(p_strm);
    }

    if p_strm.current_byte >= p_strm.count {
        return ret;
    }

    // Single special character tokens.
    let c = p_strm.buf[p_strm.current_byte as usize];
    if c == b'<' || c == b'>' || c == b'/' || c == b'=' || c == b'"' {
        ret.token_id = c as i32;
        ret.value[0] = c;
        p_strm.current_byte += 1;
        return ret;
    }

    // Word token.
    let mut written = 0usize;
    while p_strm.current_byte < p_strm.count
        && is_part_of_id(p_strm.buf[p_strm.current_byte as usize])
        && written < TOKEN_VALUE_MAX - 1
    {
        ret.token_id = WORD_ID;
        ret.value[written] = p_strm.buf[p_strm.current_byte as usize];
        written += 1;
        p_strm.current_byte += 1;
    }

    ret
}

/// Look-ahead: return the next token *without* consuming it.
///
/// Mirrors C `LA`.  Saves and restores `current_byte`.
fn la(p_strm: &mut ByteStream) -> Token {
    let save = p_strm.current_byte;
    let ret = nt(p_strm);
    p_strm.current_byte = save;
    ret
}

// ─────────────────────────────────────────────────────────────────────────
//  Attribute helpers
// ─────────────────────────────────────────────────────────────────────────

/// Add a name/value attribute pair to an `XmlAttributeArray`.
///
/// Mirrors C `AddAttribute`.  The C version asserts capacity; we silently
/// ignore overflow to avoid panics.
fn add_attribute(p_attr_array: &mut XmlAttributeArray, attr: &str, val: &str) {
    if (p_attr_array.n_count as usize) >= p_attr_array.attrs.len() {
        return;
    }
    let idx = p_attr_array.n_count as usize;
    write_str_to_buf(&mut p_attr_array.attrs[idx].name, attr);
    write_str_to_buf(&mut p_attr_array.attrs[idx].value, val);
    p_attr_array.n_count += 1;
}

// ─────────────────────────────────────────────────────────────────────────
//  XML header & comments
// ─────────────────────────────────────────────────────────────────────────

/// Write the XML declaration header at the beginning of the stream.
///
/// If `xml_header` is `None`, a default `<?xml version="1.0"
/// encoding="UTF-8"?>` header is used.  The header is written starting at
/// `current_byte = 0` (resetting the stream position), followed by a
/// newline.
///
/// Mirrors C `Xer_EncodeXmlHeader`.
pub fn encode_xml_header(p_byte_strm: &mut ByteStream, xml_header: Option<&str>) {
    let hdr = xml_header.unwrap_or(DEFAULT_XML_HEADER);
    let bytes = hdr.as_bytes();
    let len = bytes.len() as i64;
    // C does strcpy at buf[0]; we do the same — reset cursor.
    p_byte_strm.current_byte = 0;
    p_byte_strm.buf[..bytes.len()].copy_from_slice(bytes);
    p_byte_strm.current_byte = len;
    if p_byte_strm.current_byte < p_byte_strm.count {
        p_byte_strm.buf[p_byte_strm.current_byte as usize] = b'\n';
        p_byte_strm.current_byte += 1;
    }
}

/// Write an XML comment `<!--comment-->` to the stream.
/// Returns `false` on buffer overflow.
///
/// Mirrors C `Xer_EncodeComment`.
pub fn encode_comment(p_byte_strm: &mut ByteStream, comment: &str) -> bool {
    if !append_string(p_byte_strm, "<!--") {
        return false;
    }
    if !append_string(p_byte_strm, comment) {
        return false;
    }
    if !append_string(p_byte_strm, "-->") {
        return false;
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Primitive element encode / decode
// ─────────────────────────────────────────────────────────────────────────

/// Encode a primitive XER element: `<elementTag>value</elementTag>` or
/// `<elementTag/>` when `value` is empty.
///
/// Returns `true` on success.  `level` controls indentation (negative =
/// no indentation / no trailing newline).
///
/// Mirrors C `Xer_EncodePrimitiveElement`.
pub fn encode_primitive_element(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: Option<&str>,
    level: i32,
) -> bool {
    if !put_space(p_byte_strm, level) {
        return false;
    }
    if !append_string(p_byte_strm, "<") {
        return false;
    }
    if !append_string(p_byte_strm, element_tag) {
        return false;
    }

    match value {
        None | Some("") => {
            // Empty → self-closing tag.
            if !append_string(p_byte_strm, "/>") {
                return false;
            }
            if level >= 0 && !put_nl(p_byte_strm) {
                return false;
            }
            true
        }
        Some(v) => {
            if !append_string(p_byte_strm, ">") {
                return false;
            }
            if !append_string(p_byte_strm, v) {
                return false;
            }
            if !append_string(p_byte_strm, "</") {
                return false;
            }
            if !append_string(p_byte_strm, element_tag) {
                return false;
            }
            if !append_string(p_byte_strm, ">") {
                return false;
            }
            if level >= 0 && !put_nl(p_byte_strm) {
                return false;
            }
            true
        }
    }
}

/// Decode a primitive XER element, extracting the text content between
/// `<elementTag>` and `</elementTag>` (or recognising `<elementTag/>`).
///
/// On success, the decoded value is written into `p_decoded_value` as a
/// NUL-terminated string and `true` is returned.  `max_len` is the
/// maximum number of bytes (including NUL terminator) that can be
/// written.
///
/// Mirrors C `Xer_DecodePrimitiveElement`.
pub fn decode_primitive_element(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    p_decoded_value: &mut [u8],
    max_len: usize,
) -> bool {
    let mut written: usize = 0;

    // Expect '<'.
    if nt(p_byte_strm).token_id != b'<' as i32 {
        return false;
    }

    // Expect element tag word.
    let t = nt(p_byte_strm);
    if t.token_id != WORD_ID || read_str_from_buf(&t.value) != element_tag {
        return false;
    }

    // Expect '>' or '/>'.
    let t = nt(p_byte_strm);
    if t.token_id == b'/' as i32 {
        if nt(p_byte_strm).token_id != b'>' as i32 {
            return false;
        }
        // Self-closing → empty value.
        if !p_decoded_value.is_empty() {
            p_decoded_value[0] = 0;
        }
        return true;
    } else if t.token_id != b'>' as i32 {
        return false;
    }

    // Read content until '<'.
    let mut c: u8 = 0;
    loop {
        c = match get_next_char(p_byte_strm) {
            Some(ch) => ch,
            None => return false,
        };
        if c == b'<' {
            break;
        }
        if written >= max_len.saturating_sub(1) {
            return false;
        }
        p_decoded_value[written] = c;
        written += 1;
    }

    // NUL-terminate the value.
    if written < max_len {
        p_decoded_value[written] = 0;
    }

    // We consumed the '<' — push it back so the closing-tag scan can read it.
    push_back_char(p_byte_strm);

    // Expect '</'.
    if nt(p_byte_strm).token_id != b'<' as i32 {
        return false;
    }
    if nt(p_byte_strm).token_id != b'/' as i32 {
        return false;
    }

    // Expect matching element tag.
    let t = nt(p_byte_strm);
    if t.token_id != WORD_ID || read_str_from_buf(&t.value) != element_tag {
        return false;
    }

    // Expect '>'.
    if nt(p_byte_strm).token_id != b'>' as i32 {
        return false;
    }

    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Attribute decode
// ─────────────────────────────────────────────────────────────────────────

/// Decode zero or more `name="value"` attribute pairs that appear inside
/// a start tag before the closing `>`.
///
/// Mirrors C `Xer_DecodeAttributes`.
pub fn decode_attributes(
    p_byte_strm: &mut ByteStream,
    p_attrs: &mut XmlAttributeArray,
) -> bool {
    while la(p_byte_strm).token_id != b'>' as i32 {
        let t1 = nt(p_byte_strm);
        if t1.token_id != WORD_ID {
            return false;
        }
        if nt(p_byte_strm).token_id != b'=' as i32 {
            return false;
        }
        if nt(p_byte_strm).token_id != b'"' as i32 {
            return false;
        }
        let t2 = nt(p_byte_strm);
        if t2.token_id != WORD_ID {
            return false;
        }
        if nt(p_byte_strm).token_id != b'"' as i32 {
            return false;
        }
        let name = read_str_from_buf(&t1.value);
        let val = read_str_from_buf(&t2.value);
        add_attribute(p_attrs, &name, &val);
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Complex element start / end
// ─────────────────────────────────────────────────────────────────────────

/// Encode the opening tag of a complex (structured) element, with optional
/// attributes: `<elementTag attr1="val1" attr2="val2">`.
///
/// Returns `true` on success.  `level` controls indentation.
///
/// Mirrors C `Xer_EncodeComplexElementStart`.
pub fn encode_complex_element_start(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    p_attrs: Option<&XmlAttributeArray>,
    level: i32,
) -> bool {
    if element_tag.is_empty() {
        return true;
    }

    if !put_space(p_byte_strm, level) {
        return false;
    }
    if !append_string(p_byte_strm, "<") {
        return false;
    }
    if !append_string(p_byte_strm, element_tag) {
        return false;
    }

    if let Some(attrs) = p_attrs {
        for i in 0..attrs.n_count as usize {
            let name = read_str_from_buf(&attrs.attrs[i].name);
            let val = read_str_from_buf(&attrs.attrs[i].value);
            if !append_string(p_byte_strm, " ") {
                return false;
            }
            if !append_string(p_byte_strm, &name) {
                return false;
            }
            if !append_string(p_byte_strm, "=") {
                return false;
            }
            if !append_string(p_byte_strm, "\"") {
                return false;
            }
            if !append_string(p_byte_strm, &val) {
                return false;
            }
            if !append_string(p_byte_strm, "\"") {
                return false;
            }
        }
    }

    if !append_string(p_byte_strm, ">") {
        return false;
    }
    if level >= 0 && !put_nl(p_byte_strm) {
        return false;
    }
    true
}

/// Decode the opening tag of a complex element, extracting any attributes.
///
/// Returns `true` on success.  Handles both `<tag>…` and `<tag/>`.
///
/// Mirrors C `Xer_DecodeComplexElementStart`.
pub fn decode_complex_element_start(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    p_attrs: Option<&mut XmlAttributeArray>,
) -> bool {
    if element_tag.is_empty() {
        return true;
    }

    if nt(p_byte_strm).token_id != b'<' as i32 {
        return false;
    }

    let t = nt(p_byte_strm);
    if t.token_id != WORD_ID || read_str_from_buf(&t.value) != element_tag {
        return false;
    }

    // Decode attributes (if buffer provided).
    if let Some(attrs) = p_attrs {
        if !decode_attributes(p_byte_strm, attrs) {
            return false;
        }
    } else {
        // Still need to skip attributes.
        let mut dummy = XmlAttributeArray::new();
        if !decode_attributes(p_byte_strm, &mut dummy) {
            return false;
        }
    }

    let t = nt(p_byte_strm);
    if t.token_id == b'/' as i32 {
        if nt(p_byte_strm).token_id != b'>' as i32 {
            return false;
        }
        return true;
    } else if t.token_id != b'>' as i32 {
        return false;
    }

    true
}

/// Encode the closing tag of a complex element: `</elementTag>`.
///
/// Returns `true` on success.  `level` controls indentation.
///
/// Mirrors C `Xer_EncodeComplexElementEnd`.
pub fn encode_complex_element_end(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    level: i32,
) -> bool {
    if element_tag.is_empty() {
        return true;
    }
    if !put_space(p_byte_strm, level) {
        return false;
    }
    if !append_string(p_byte_strm, "</") {
        return false;
    }
    if !append_string(p_byte_strm, element_tag) {
        return false;
    }
    if !append_string(p_byte_strm, ">") {
        return false;
    }
    if level >= 0 && !put_nl(p_byte_strm) {
        return false;
    }
    true
}

/// Decode the closing tag `</elementTag>`.
///
/// Returns `true` on success.
///
/// Mirrors C `Xer_DecodeComplexElementEnd`.
pub fn decode_complex_element_end(p_byte_strm: &mut ByteStream, element_tag: &str) -> bool {
    if element_tag.is_empty() {
        return true;
    }

    if nt(p_byte_strm).token_id != b'<' as i32 {
        return false;
    }
    if nt(p_byte_strm).token_id != b'/' as i32 {
        return false;
    }

    let t = nt(p_byte_strm);
    if t.token_id != WORD_ID || read_str_from_buf(&t.value) != element_tag {
        return false;
    }

    if nt(p_byte_strm).token_id != b'>' as i32 {
        return false;
    }

    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Lookahead / navigation
// ─────────────────────────────────────────────────────────────────────────

/// Returns `true` if the next token sequence in the stream is
/// `</elementTag>` **without consuming** those tokens.
///
/// Mirrors C `Xer_NextEndElementIs`.
pub fn next_end_element_is(p_byte_strm: &mut ByteStream, element_tag: &str) -> bool {
    let save = p_byte_strm.current_byte;

    if nt(p_byte_strm).token_id != b'<' as i32 {
        p_byte_strm.current_byte = save;
        return false;
    }
    if nt(p_byte_strm).token_id != b'/' as i32 {
        p_byte_strm.current_byte = save;
        return false;
    }

    let t = nt(p_byte_strm);
    if t.token_id != WORD_ID || read_str_from_buf(&t.value) != element_tag {
        p_byte_strm.current_byte = save;
        return false;
    }

    if nt(p_byte_strm).token_id != b'>' as i32 {
        p_byte_strm.current_byte = save;
        return false;
    }

    p_byte_strm.current_byte = save;
    true
}

/// Returns `true` if the next token sequence in the stream is
/// `<elementTag>`, `<elementTag/>`, or `<elementTag …>` **without
/// consuming** those tokens.
///
/// Mirrors C `Xer_NextStartElementIs`.
pub fn next_start_element_is(p_byte_strm: &mut ByteStream, element_tag: &str) -> bool {
    let save = p_byte_strm.current_byte;

    if nt(p_byte_strm).token_id != b'<' as i32 {
        p_byte_strm.current_byte = save;
        return false;
    }

    let t = nt(p_byte_strm);
    if t.token_id != WORD_ID || read_str_from_buf(&t.value) != element_tag {
        p_byte_strm.current_byte = save;
        return false;
    }

    let t = nt(p_byte_strm);
    if t.token_id == b'/' as i32 {
        if nt(p_byte_strm).token_id == b'>' as i32 {
            p_byte_strm.current_byte = save;
            return true;
        } else {
            p_byte_strm.current_byte = save;
            return false;
        }
    } else if t.token_id != b'>' as i32 {
        // Could be an attribute — accept if we see '>' eventually.
        // The C code only accepts '>' directly or '/' followed by '>'.
        // For attributes, the C code returns false here too.
        p_byte_strm.current_byte = save;
        return false;
    }

    p_byte_strm.current_byte = save;
    true
}

/// Look-ahead: extract the tag name of the next element in the stream
/// **without consuming** it.  Writes the tag name into `element_tag`.
///
/// Mirrors C `Xer_LA_NextElementTag`.
pub fn la_next_element_tag(p_byte_strm: &mut ByteStream, element_tag: &mut [u8]) -> bool {
    let save = p_byte_strm.current_byte;

    if nt(p_byte_strm).token_id != b'<' as i32 {
        p_byte_strm.current_byte = save;
        return false;
    }

    let t = nt(p_byte_strm);
    // Copy tag name to output buffer.
    let name = read_str_from_buf(&t.value);
    write_str_to_buf(element_tag, &name);

    if t.token_id != WORD_ID {
        p_byte_strm.current_byte = save;
        return false;
    }

    let t = nt(p_byte_strm);
    if t.token_id == b'/' as i32 {
        if nt(p_byte_strm).token_id == b'>' as i32 {
            p_byte_strm.current_byte = save;
            return true;
        } else {
            p_byte_strm.current_byte = save;
            return false;
        }
    } else if t.token_id != b'>' as i32 {
        // Attributes present — skip to '>'.
        // The C code returns false if the next token is not '>' or '/' → '>'.
        // But in practice, elements with attributes should still be recognised.
        // We loop skipping attribute tokens until we see '>'.
        loop {
            let tk = nt(p_byte_strm);
            if tk.token_id == b'>' as i32 {
                break;
            }
            if p_strm_at_end(p_byte_strm) {
                p_byte_strm.current_byte = save;
                return false;
            }
        }
    }

    p_byte_strm.current_byte = save;
    true
}

/// Check whether the stream cursor is at or past the end.
fn p_strm_at_end(p_strm: &ByteStream) -> bool {
    p_strm.current_byte >= p_strm.count
}

// ─────────────────────────────────────────────────────────────────────────
//  Char → nibble helper
// ─────────────────────────────────────────────────────────────────────────

/// Convert a hex character to its numeric nibble value.
/// Returns `Some(nibble)` on success, `None` for non-hex characters.
///
/// Mirrors C `CharToNibble`.
pub fn char_to_nibble(c: u8) -> Option<u8> {
    if c.is_ascii_digit() {
        return Some(c - b'0');
    }
    if (b'A'..=b'F').contains(&c) {
        return Some(c - b'A' + 10);
    }
    if (b'a'..=b'f').contains(&c) {
        return Some(c - b'a' + 10);
    }
    None
}

// ─────────────────────────────────────────────────────────────────────────
//  Primitive type encode functions
// ─────────────────────────────────────────────────────────────────────────

/// Encode an ASN.1 NULL value as an empty self-closing element.
///
/// Mirrors C `Xer_EncodeNull`.
pub fn encode_null(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    _value: NullType,
    level: i32,
) -> bool {
    encode_primitive_element(p_byte_strm, element_tag, None, level)
}

/// Encode an ASN.1 INTEGER value.
///
/// Mirrors C `Xer_EncodeInteger`.
pub fn encode_integer(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: Asn1SccSint,
    level: i32,
) -> bool {
    let s = int_to_string(value);
    encode_primitive_element(p_byte_strm, element_tag, Some(&s), level)
}

/// Encode a non-negative ASN.1 INTEGER value.
///
/// Mirrors C `Xer_EncodePosInteger`.
pub fn encode_pos_integer(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: Asn1SccUint,
    level: i32,
) -> bool {
    let s = uint_to_string(value);
    encode_primitive_element(p_byte_strm, element_tag, Some(&s), level)
}

/// Encode an ASN.1 BOOLEAN value.
///
/// When `element_tag` is non-empty, the boolean is encoded as
/// `<elementTag><true/></elementTag>` or `<elementTag><false/></elementTag>`.
/// When `element_tag` is empty, it is encoded as `<true/>` or `<false/>`.
///
/// Mirrors C `Xer_EncodeBoolean`.
pub fn encode_boolean(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: bool,
    level: i32,
) -> bool {
    if element_tag.is_empty() {
        if value {
            encode_primitive_element(p_byte_strm, "true", Some(""), level)
        } else {
            encode_primitive_element(p_byte_strm, "false", Some(""), level)
        }
    } else if value {
        encode_primitive_element(p_byte_strm, element_tag, Some("<true/>"), level)
    } else {
        encode_primitive_element(p_byte_strm, element_tag, Some("<false/>"), level)
    }
}

/// Encode an ASN.1 ENUMERATED value.
///
/// The value is the enumeration item's name (e.g. `"red"`).  When
/// `element_tag` is non-empty, the result is:
/// ```xml
/// <elementTag>
///   <red/>
/// </elementTag>
/// ```
/// When `element_tag` is empty, just `<red/>` is emitted.
///
/// Mirrors C `Xer_EncodeEnumerated`.
pub fn encode_enumerated(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: &str,
    level: i32,
) -> bool {
    if element_tag.is_empty() {
        return encode_primitive_element(p_byte_strm, value, Some(""), level);
    }

    if !put_space(p_byte_strm, level) {
        return false;
    }

    // <elementTag>
    if !encode_complex_element_start(p_byte_strm, element_tag, None, -1) {
        return false;
    }
    // <value/>
    if !encode_primitive_element(p_byte_strm, value, Some(""), -1) {
        return false;
    }
    // </elementTag>
    if !encode_complex_element_end(p_byte_strm, element_tag, -1) {
        return false;
    }

    if !put_nl(p_byte_strm) {
        return false;
    }
    true
}

/// Encode an ASN.1 REAL (double) value.
///
/// Mirrors C `Xer_EncodeReal`.
pub fn encode_real(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: Asn1Real,
    level: i32,
) -> bool {
    let s = double_to_string(value);
    encode_primitive_element(p_byte_strm, element_tag, Some(&s), level)
}

/// Encode an ASN.1 string value.
///
/// Mirrors C `Xer_EncodeString`.
pub fn encode_string(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: &str,
    level: i32,
) -> bool {
    encode_primitive_element(p_byte_strm, element_tag, Some(value), level)
}

/// Encode an ASN.1 OCTET STRING as hex digits inside a complex element.
///
/// Produces:
/// ```xml
/// <elementTag>A1B2C3</elementTag>
/// ```
///
/// Mirrors C `Xer_EncodeOctetString`.
pub fn encode_octet_string(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: &[u8],
    n_count: i32,
    level: i32,
) -> bool {
    if !put_space(p_byte_strm, level) {
        return false;
    }
    if !encode_complex_element_start(p_byte_strm, element_tag, None, -1) {
        return false;
    }

    for i in 0..n_count as usize {
        let hex = format!("{:02X}", value[i]);
        if !append_string(p_byte_strm, &hex) {
            return false;
        }
    }

    if !encode_complex_element_end(p_byte_strm, element_tag, -1) {
        return false;
    }
    if !put_nl(p_byte_strm) {
        return false;
    }
    true
}

/// Encode an ASN.1 BIT STRING as binary digits inside a complex element.
///
/// Produces:
/// ```xml
/// <elementTag>10110010</elementTag>
/// ```
///
/// Mirrors C `Xer_EncodeBitString`.
pub fn encode_bit_string(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: &[u8],
    n_count: i32,
    level: i32,
) -> bool {
    if !put_space(p_byte_strm, level) {
        return false;
    }
    if !encode_complex_element_start(p_byte_strm, element_tag, None, -1) {
        return false;
    }

    for i in 0..n_count as usize {
        let cur_byte = i / 8;
        let cur_bit = 7 - (i % 8);
        let bit_char = if value[cur_byte] & (1u8 << cur_bit) != 0 {
            b'1'
        } else {
            b'0'
        };
        if !append_char(p_byte_strm, bit_char) {
            return false;
        }
    }

    if !encode_complex_element_end(p_byte_strm, element_tag, -1) {
        return false;
    }
    if !put_nl(p_byte_strm) {
        return false;
    }
    true
}

/// Encode an OBJECT IDENTIFIER as dot-separated arc values inside a
/// complex element.
///
/// Produces:
/// ```xml
/// <elementTag>1.3.6.1.4.1</elementTag>
/// ```
///
/// Mirrors C `Xer_EncodeObjectIdentifier`.
pub fn encode_object_identifier(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    p_val: &Asn1ObjectIdentifier,
    level: i32,
) -> bool {
    if !put_space(p_byte_strm, level) {
        return false;
    }
    if !encode_complex_element_start(p_byte_strm, element_tag, None, -1) {
        return false;
    }

    if p_val.n_count > 0 {
        let s = format!("{}", p_val.values[0]);
        if !append_string(p_byte_strm, &s) {
            return false;
        }
    }

    for i in 1..p_val.n_count as usize {
        let s = format!(".{}", p_val.values[i]);
        if !append_string(p_byte_strm, &s) {
            return false;
        }
    }

    if !encode_complex_element_end(p_byte_strm, element_tag, -1) {
        return false;
    }
    if !put_nl(p_byte_strm) {
        return false;
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Primitive type decode functions
// ─────────────────────────────────────────────────────────────────────────

/// Decode an ASN.1 NULL value.
///
/// Mirrors C `Xer_DecodeNull`.
pub fn decode_null(p_byte_strm: &mut ByteStream, element_tag: &str) -> bool {
    let mut tmp = [0u8; 256];
    let max_len = tmp.len();
    decode_primitive_element(p_byte_strm, element_tag, &mut tmp, max_len)
}

/// Decode an ASN.1 INTEGER value.
///
/// Returns `Some(value)` on success, `None` on error.
///
/// Mirrors C `Xer_DecodeInteger`.
pub fn decode_integer(p_byte_strm: &mut ByteStream, element_tag: &str) -> Option<Asn1SccSint> {
    let mut tmp = [0u8; 256];
    let max_len = tmp.len();
    if !decode_primitive_element(p_byte_strm, element_tag, &mut tmp, max_len) {
        return None;
    }
    let s = read_str_from_buf(&tmp);
    // Parse as signed integer (equivalent to C atoll).
    s.trim().parse::<Asn1SccSint>().ok()
}

/// Decode a non-negative ASN.1 INTEGER value.
///
/// Returns `Some(value)` on success, `None` on error.
///
/// Mirrors C `Xer_DecodePosInteger`.
pub fn decode_pos_integer(p_byte_strm: &mut ByteStream, element_tag: &str) -> Option<Asn1SccUint> {
    let mut tmp = [0u8; 256];
    let max_len = tmp.len();
    if !decode_primitive_element(p_byte_strm, element_tag, &mut tmp, max_len) {
        return None;
    }
    let s = read_str_from_buf(&tmp);
    // Parse as unsigned integer (equivalent to C strtoull).
    s.trim().parse::<Asn1SccUint>().ok()
}

/// Decode an ASN.1 BOOLEAN value.
///
/// Returns `Some(bool)` on success, `None` on error.
///
/// Mirrors C `Xer_DecodeBoolean`.
pub fn decode_boolean(p_byte_strm: &mut ByteStream, element_tag: &str) -> Option<bool> {
    let has_ext_tag = !element_tag.is_empty();

    if has_ext_tag {
        if !decode_complex_element_start(p_byte_strm, element_tag, None) {
            return None;
        }
    }

    let mut tmp_tag = [0u8; 256];
    if !la_next_element_tag(p_byte_strm, &mut tmp_tag) {
        return None;
    }
    let tag = read_str_from_buf(&tmp_tag);

    let mut tmp_val = [0u8; 256];
    let tmp_val_len = tmp_val.len();
    let result;
    if tag == "true" {
        if !decode_primitive_element(p_byte_strm, "true", &mut tmp_val, tmp_val_len) {
            return None;
        }
        result = true;
    } else {
        if !decode_primitive_element(p_byte_strm, "false", &mut tmp_val, tmp_val_len) {
            return None;
        }
        result = false;
    }

    if has_ext_tag {
        if !decode_complex_element_end(p_byte_strm, element_tag) {
            return None;
        }
    }

    Some(result)
}

/// Decode an ASN.1 ENUMERATED value.
///
/// On success, writes the enumeration item name into `value` and returns
/// `true`.  The caller provides a buffer large enough for the longest
/// enumeration name.
///
/// Mirrors C `Xer_DecodeEnumerated`.
pub fn decode_enumerated(p_byte_strm: &mut ByteStream, element_tag: &str, value: &mut [u8]) -> bool {
    let has_ext_tag = !element_tag.is_empty();

    if has_ext_tag {
        if !decode_complex_element_start(p_byte_strm, element_tag, None) {
            return false;
        }
    }

    // Look-ahead to get the enum value tag name.
    let mut tmp_tag = [0u8; 256];
    if !la_next_element_tag(p_byte_strm, &mut tmp_tag) {
        return false;
    }
    let tag_name = read_str_from_buf(&tmp_tag);
    write_str_to_buf(value, &tag_name);

    let mut tmp_val = [0u8; 256];
    let tmp_val_len = tmp_val.len();
    if !decode_primitive_element(p_byte_strm, &tag_name, &mut tmp_val, tmp_val_len) {
        return false;
    }

    if has_ext_tag {
        if !decode_complex_element_end(p_byte_strm, element_tag) {
            return false;
        }
    }

    true
}

/// Decode an ASN.1 REAL (double) value.
///
/// Returns `Some(value)` on success, `None` on error.
///
/// Mirrors C `Xer_DecodeReal`.
pub fn decode_real(p_byte_strm: &mut ByteStream, element_tag: &str) -> Option<Asn1Real> {
    let mut tmp = [0u8; 256];
    let max_len = tmp.len();
    if !decode_primitive_element(p_byte_strm, element_tag, &mut tmp, max_len) {
        return None;
    }
    let s = read_str_from_buf(&tmp);
    // Parse as f64 (equivalent to C atof).
    s.trim().parse::<Asn1Real>().ok()
}

/// Decode an ASN.1 string value into `value`.
///
/// Returns `true` on success.
///
/// Mirrors C `Xer_DecodeString`.
pub fn decode_string(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: &mut [u8],
    val_size: usize,
) -> bool {
    decode_primitive_element(p_byte_strm, element_tag, value, val_size)
}

/// Decode an ASN.1 OCTET STRING from hex digits.
///
/// On success, writes the decoded bytes into `value` and sets `n_count`
/// to the number of bytes decoded.  Returns `true` on success.
///
/// Mirrors C `Xer_DecodeOctetString`.
pub fn decode_octet_string(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: &mut [u8],
    buffer_max_size: i32,
    n_count: &mut i32,
) -> bool {
    let mut tmp = [0u8; 1024];
    let max_len = tmp.len();
    if !decode_primitive_element(p_byte_strm, element_tag, &mut tmp, max_len) {
        return false;
    }

    // Read the decoded string, stripping whitespace.
    let mut cleaned = [0u8; 1024];
    let mut j = 0usize;
    let raw = read_str_from_buf(&tmp);
    for c in raw.bytes() {
        if !(c as char).is_whitespace() {
            if j >= cleaned.len() {
                return false;
            }
            cleaned[j] = c;
            j += 1;
        }
    }

    // Convert hex pairs to bytes.
    let mut _byte_count = 0i32;
    let mut i = 0usize;
    while i < j && (i / 2) < buffer_max_size as usize {
        let nibble = match char_to_nibble(cleaned[i]) {
            Some(n) => n,
            None => return false,
        };
        if i % 2 == 0 {
            value[i / 2] = nibble << 4;
        } else {
            value[i / 2] |= nibble;
        }
        i += 1;
        _byte_count = ((i + 1) / 2) as i32;
    }

    *n_count = j as i32 / 2 + (if j % 2 != 0 { 1 } else { 0 });
    true
}

/// Decode an ASN.1 BIT STRING from binary digits.
///
/// On success, writes the decoded bytes into `value` and sets `n_count`
/// to the number of *bits* decoded.  Returns `true` on success.
///
/// Mirrors C `Xer_DecodeBitString`.
pub fn decode_bit_string(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    value: &mut [u8],
    buffer_max_size: i32,
    n_count: &mut i32,
) -> bool {
    let mut tmp = [0u8; 2048];
    let max_len = tmp.len();
    if !decode_primitive_element(p_byte_strm, element_tag, &mut tmp, max_len) {
        return false;
    }

    // Strip whitespace.
    let mut cleaned = [0u8; 2048];
    let mut j = 0usize;
    let raw = read_str_from_buf(&tmp);
    for c in raw.bytes() {
        if !(c as char).is_whitespace() {
            if j >= cleaned.len() {
                return false;
            }
            cleaned[j] = c;
            j += 1;
        }
    }

    // Calculate bytes needed.
    let mut bytes = (j / 8) as i32;
    if j % 8 != 0 {
        bytes += 1;
    }

    // Zero the output.
    for b in value.iter_mut().take(bytes as usize) {
        *b = 0;
    }

    // Convert bits to bytes.
    let mut i = 0usize;
    while i < j && (i / 8) < buffer_max_size as usize {
        let cur_val = (cleaned[i] - b'0') as u8;
        let cur_bit = 7 - (i % 8) as i32;
        value[i / 8] |= cur_val << cur_bit;
        i += 1;
    }

    *n_count = j as i32;
    true
}

/// Decode an OBJECT IDENTIFIER from dot-separated arc values.
///
/// On success, populates `p_val` and returns `true`.
///
/// Mirrors C `Xer_DecodeObjectIdentifier`.
pub fn decode_object_identifier(
    p_byte_strm: &mut ByteStream,
    element_tag: &str,
    p_val: &mut Asn1ObjectIdentifier,
) -> bool {
    let mut tmp = [0u8; 1024];
    let max_len = tmp.len();
    if !decode_primitive_element(p_byte_strm, element_tag, &mut tmp, max_len) {
        return false;
    }

    // Strip whitespace.
    let mut cleaned = [0u8; 1024];
    let mut j = 0usize;
    let raw = read_str_from_buf(&tmp);
    for c in raw.bytes() {
        if !(c as char).is_whitespace() {
            if j >= cleaned.len() {
                return false;
            }
            cleaned[j] = c;
            j += 1;
        }
    }

    // Split on '.' and parse each arc.
    *p_val = Asn1ObjectIdentifier::init();
    let cleaned_str = std::str::from_utf8(&cleaned[..j]).unwrap_or("");
    let mut i = 0usize;
    for part in cleaned_str.split('.') {
        if i >= OBJECT_IDENTIFIER_MAX_LENGTH {
            break;
        }
        if let Ok(arc) = part.parse::<Asn1SccUint>() {
            p_val.values[i] = arc;
            i += 1;
        } else {
            return false;
        }
    }
    p_val.n_count = i as i32;

    true
}

// ─────────────────────────────────────────────────────────────────────────
//  XML file loading (whitespace-stripping state machine)
// ─────────────────────────────────────────────────────────────────────────

/// State-machine handler for `XmlStart`.
///
/// At the start of the document (or between elements), we look for `<`.
/// If followed by `!`, we transition to comment mode.  If followed by
/// `?`, we transition to header mode.  Otherwise, we emit `<` and enter
/// the start-tag state.
///
/// Returns `(success, new_state)`.
///
/// Mirrors C `OnXmlStart`.
fn on_xml_start(
    c: u8,
    l1: u8,
    p_strm: &mut ByteStream,
    _p_tmp_strm: &mut ByteStream,
) -> (bool, XmlState) {
    if c != b'<' {
        return (true, XmlState::Start);
    }

    if l1 == b'!' {
        return (true, XmlState::Comment);
    }

    if l1 == b'?' {
        return (true, XmlState::Header);
    }

    if !append_char(p_strm, c) {
        return (false, XmlState::Start);
    }

    (true, XmlState::StartTag)
}

/// State-machine handler for `XmlHeader`.
///
/// Inside `<?…?>`, we consume characters until we see `?>`.  Nothing is
/// emitted to the output stream.
///
/// Returns `(success, new_state, consumed_l1)`.
/// `consumed_l1` is `true` when `l1` (the `>`) should be consumed from
/// the input, matching the C `fgetc(xmlFile)` call.
///
/// Mirrors C `OnXmlHeader`.
fn on_xml_header(c: u8, l1: u8) -> (bool, XmlState, bool) {
    if c == b'?' && l1 == b'>' {
        (true, XmlState::Start, true)
    } else {
        (true, XmlState::Header, false)
    }
}

/// State-machine handler for `XmlStartTag`.
///
/// Inside `<tag …>`, every character is emitted until we see `>`, which
/// transitions to content state.
///
/// Mirrors C `OnXmlStartTag`.
fn on_xml_start_tag(
    c: u8,
    p_strm: &mut ByteStream,
) -> (bool, XmlState) {
    let new_state = if c == b'>' {
        XmlState::Content
    } else {
        XmlState::StartTag
    };

    if !append_char(p_strm, c) {
        return (false, new_state);
    }
    (true, new_state)
}

/// State-machine handler for `XmlContent`.
///
/// Inside element content (between `>` and `<`), characters are written
/// to a *temporary* buffer.  When `<` is encountered:
/// - If followed by `!`, transition to comment.
/// - If followed by `/`, copy the temp buffer to the main stream and
///   transition to end-tag.
/// - Otherwise, discard the temp buffer and transition to start-tag.
///
/// Mirrors C `OnXmlContent`.
fn on_xml_content(
    c: u8,
    l1: u8,
    p_strm: &mut ByteStream,
    p_tmp_strm: &mut ByteStream,
) -> (bool, XmlState) {
    if c != b'<' {
        if !append_char(p_tmp_strm, c) {
            return (false, XmlState::Content);
        }
        return (true, XmlState::Content);
    }

    // c == '<'
    if l1 == b'!' {
        return (true, XmlState::Comment);
    }

    if l1 == b'/' {
        // Copy temp buffer to main stream.
        for i in 0..p_tmp_strm.current_byte as usize {
            if !append_char(p_strm, p_tmp_strm.buf[i]) {
                return (false, XmlState::EndTag);
            }
        }
        // Discard temp buffer.
        p_tmp_strm.current_byte = 0;

        if !append_char(p_strm, c) {
            return (false, XmlState::EndTag);
        }
        return (true, XmlState::EndTag);
    }

    // Start of a new child element — discard temp buffer.
    p_tmp_strm.current_byte = 0;

    if !append_char(p_strm, c) {
        return (false, XmlState::StartTag);
    }
    (true, XmlState::StartTag)
}

/// State-machine handler for `XmlEndTag`.
///
/// Inside `</tag>`, every character is emitted until `>` is seen, which
/// transitions to mixed-content state.
///
/// Mirrors C `OnXmlEndTag`.
fn on_xml_end_tag(c: u8, p_strm: &mut ByteStream) -> (bool, XmlState) {
    let new_state = if c == b'>' {
        XmlState::MixedContent
    } else {
        XmlState::EndTag
    };

    if !append_char(p_strm, c) {
        return (false, new_state);
    }
    (true, new_state)
}

/// State-machine handler for `XmlMixedContent`.
///
/// Between sibling elements (after a `>` and before the next `<`).
/// Characters are discarded (whitespace).  On `<`:
/// - `!` → comment.
/// - `/` → end-tag.
/// - otherwise → start-tag (emit `<`).
///
/// Mirrors C `OnXmlMixedContent`.
fn on_xml_mixed_content(
    c: u8,
    l1: u8,
    p_strm: &mut ByteStream,
) -> (bool, XmlState) {
    if c != b'<' {
        return (true, XmlState::MixedContent);
    }

    if l1 == b'!' {
        return (true, XmlState::Comment);
    }

    let new_state = if l1 == b'/' {
        XmlState::EndTag
    } else {
        XmlState::StartTag
    };

    if !append_char(p_strm, c) {
        return (false, new_state);
    }
    (true, new_state)
}

/// State-machine handler for `XmlComment`.
///
/// Inside `<!--…-->`, characters are consumed (not emitted) until `-->`
/// is seen, which transitions back to the previous state.
///
/// Returns `(success, new_state, consumed_l1)`.
/// `consumed_l1` is `true` when `l1` (the `>`) should be consumed from
/// the input, matching the C `fgetc(xmlFile)` call.
///
/// Mirrors C `OnXmlComment`.
fn on_xml_comment(c: u8, l1: u8, previous_state: XmlState) -> (bool, XmlState, bool) {
    if c == b'-' && l1 == b'>' {
        (true, previous_state, true)
    } else {
        (true, XmlState::Comment, false)
    }
}

/// Load an XML file, stripping insignificant whitespace, into a `ByteStream`.
///
/// The file is read character by character and processed through a
/// 7-state finite-state machine.  Only significant characters (tag names,
/// attribute values, element content) are written to the output stream.
/// XML comments and declarations are discarded, and inter-element
/// whitespace is removed.
///
/// On success, sets `n_bytes_loaded` to the number of significant bytes
/// written and resets `p_strm.current_byte` to 0.  Returns `true` on
/// success, `false` if the file cannot be opened or a buffer overflow
/// occurs.
///
/// Mirrors C `LoadXmlFile`.
pub fn load_xml_file(
    file_name: &str,
    p_strm: &mut ByteStream,
    n_bytes_loaded: &mut i32,
) -> bool {
    let mut file = match File::open(file_name) {
        Ok(f) => f,
        Err(_) => return false,
    };

    // Read entire file into memory.
    let mut file_contents = Vec::new();
    if file.read_to_end(&mut file_contents).is_err() {
        return false;
    }

    // Temp buffer for element content (not the output stream).
    let mut tmp_buffer = [0u8; TMP_BUFFER_SIZE];
    let mut tmp_strm = ByteStream {
        buf: &mut tmp_buffer,
        count: TMP_BUFFER_SIZE as i64,
        current_byte: 0,
        encode_white_space: false,
    };

    let mut cur_state = XmlState::Start;
    let mut previous_state = XmlState::Start; // saved state for comment return

    let mut pos = 0usize;
    while pos < file_contents.len() {
        let c = file_contents[pos];
        pos += 1;

        // Look-ahead: next character (without consuming).
        let l1 = if pos < file_contents.len() {
            file_contents[pos]
        } else {
            0
        };

        let (success, new_state) = match cur_state {
            XmlState::Start => {
                let (ok, st) = on_xml_start(c, l1, p_strm, &mut tmp_strm);
                if st == XmlState::Comment {
                    previous_state = XmlState::Start;
                }
                (ok, st)
            }
            XmlState::Header => {
                let (ok, st, consume_l1) = on_xml_header(c, l1);
                if consume_l1 && pos < file_contents.len() {
                    pos += 1; // discard '>'
                }
                (ok, st)
            }
            XmlState::StartTag => {
                on_xml_start_tag(c, p_strm)
            }
            XmlState::Content => {
                let (ok, st) = on_xml_content(c, l1, p_strm, &mut tmp_strm);
                if st == XmlState::Comment {
                    previous_state = XmlState::Content;
                }
                (ok, st)
            }
            XmlState::EndTag => {
                on_xml_end_tag(c, p_strm)
            }
            XmlState::MixedContent => {
                let (ok, st) = on_xml_mixed_content(c, l1, p_strm);
                if st == XmlState::Comment {
                    previous_state = XmlState::MixedContent;
                }
                (ok, st)
            }
            XmlState::Comment => {
                let (ok, st, consume_l1) = on_xml_comment(c, l1, previous_state);
                if consume_l1 && pos < file_contents.len() {
                    pos += 1; // discard '>'
                }
                (ok, st)
            }
        };

        if !success {
            return false;
        }
        cur_state = new_state;
    }

    *n_bytes_loaded = p_strm.current_byte as i32;
    p_strm.current_byte = 0;
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Tests
// ─────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── Helper to create an encode ByteStream ──
    fn make_encode_buf(size: usize) -> Vec<u8> {
        vec![0u8; size]
    }

    // ── encode_xml_header ──
    #[test]
    fn test_encode_xml_header_default() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        encode_xml_header(&mut strm, None);
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    }

    #[test]
    fn test_encode_xml_header_custom() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        encode_xml_header(&mut strm, Some("<?xml version=\"1.1\"?>"));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<?xml version=\"1.1\"?>\n");
    }

    // ── encode_comment ──
    #[test]
    fn test_encode_comment() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        assert!(encode_comment(&mut strm, "hello"));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<!--hello-->");
    }

    // ── encode_integer / decode_integer ──
    #[test]
    fn test_encode_integer() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        assert!(encode_integer(&mut strm, "int-val", 42, 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<int-val>42</int-val>\n");
    }

    #[test]
    fn test_encode_integer_negative() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        assert!(encode_integer(&mut strm, "n", -123, 1));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "  <n>-123</n>\n");
    }

    #[test]
    fn test_decode_integer() {
        let xml = b"<int-val>42</int-val>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let val = decode_integer(&mut strm, "int-val");
        assert_eq!(val, Some(42));
    }

    #[test]
    fn test_decode_integer_negative() {
        let xml = b"<n>-123</n>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let val = decode_integer(&mut strm, "n");
        assert_eq!(val, Some(-123));
    }

    // ── encode_pos_integer / decode_pos_integer ──
    #[test]
    fn test_encode_decode_pos_integer() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        assert!(encode_pos_integer(&mut strm, "u", 99999, 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<u>99999</u>\n");

        // Decode.
        let mut d_buf = s.as_bytes().to_vec();
        let mut d_strm = ByteStream {
            buf: &mut d_buf,
            count: s.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert_eq!(decode_pos_integer(&mut d_strm, "u"), Some(99999));
    }

    // ── encode_boolean / decode_boolean ──
    #[test]
    fn test_encode_boolean_true() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        assert!(encode_boolean(&mut strm, "flag", true, 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<flag><true/></flag>\n");
    }

    #[test]
    fn test_encode_boolean_false() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        assert!(encode_boolean(&mut strm, "flag", false, 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<flag><false/></flag>\n");
    }

    #[test]
    fn test_encode_boolean_no_tag() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        assert!(encode_boolean(&mut strm, "", true, -1));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<true/>");
    }

    #[test]
    fn test_decode_boolean_true() {
        let xml = b"<flag><true/></flag>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert_eq!(decode_boolean(&mut strm, "flag"), Some(true));
    }

    #[test]
    fn test_decode_boolean_false() {
        let xml = b"<flag><false/></flag>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert_eq!(decode_boolean(&mut strm, "flag"), Some(false));
    }

    // ── encode_null / decode_null ──
    #[test]
    fn test_encode_null() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        assert!(encode_null(&mut strm, "n", NullType, 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<n/>\n");
    }

    #[test]
    fn test_decode_null() {
        let xml = b"<n/>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert!(decode_null(&mut strm, "n"));
    }

    // ── encode_string / decode_string ──
    #[test]
    fn test_encode_string() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        assert!(encode_string(&mut strm, "s", "hello world", 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<s>hello world</s>\n");
    }

    #[test]
    fn test_decode_string() {
        let xml = b"<s>hello world</s>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let mut val = [0u8; 256];
        let val_len = val.len();
        assert!(decode_string(&mut strm, "s", &mut val, val_len));
        assert_eq!(read_str_from_buf(&val), "hello world");
    }

    // ── encode_real / decode_real ──
    #[test]
    fn test_encode_real_zero() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        assert!(encode_real(&mut strm, "r", 0.0, -1));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<r>0</r>");
    }

    #[test]
    fn test_encode_real_nonzero() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        assert!(encode_real(&mut strm, "r", 3.14, -1));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        // 3.14 normalises to [1,10) → 3.14E0
        assert!(s.starts_with("<r>3.14"));
    }

    #[test]
    fn test_decode_real() {
        let xml = b"<r>3.14</r>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let val = decode_real(&mut strm, "r");
        assert!(val.is_some());
        assert!((val.unwrap() - 3.14).abs() < 1e-10);
    }

    // ── encode_enumerated / decode_enumerated ──
    #[test]
    fn test_encode_enumerated() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        assert!(encode_enumerated(&mut strm, "color", "red", 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<color><red/></color>\n");
    }

    #[test]
    fn test_encode_enumerated_no_tag() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        assert!(encode_enumerated(&mut strm, "", "red", -1));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<red/>");
    }

    #[test]
    fn test_decode_enumerated() {
        let xml = b"<color><red/></color>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let mut val = [0u8; 256];
        assert!(decode_enumerated(&mut strm, "color", &mut val));
        assert_eq!(read_str_from_buf(&val), "red");
    }

    // ── encode_octet_string / decode_octet_string ──
    #[test]
    fn test_encode_octet_string() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        let data = [0xDE, 0xAD, 0xBE, 0xEF];
        assert!(encode_octet_string(&mut strm, "os", &data, 4, 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<os>DEADBEEF</os>\n");
    }

    #[test]
    fn test_decode_octet_string() {
        let xml = b"<os>DEADBEEF</os>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let mut value = [0u8; 16];
        let mut n_count = 0i32;
        assert!(decode_octet_string(&mut strm, "os", &mut value, 16, &mut n_count));
        assert_eq!(n_count, 4);
        assert_eq!(&value[..4], &[0xDE, 0xAD, 0xBE, 0xEF]);
    }

    // ── encode_bit_string / decode_bit_string ──
    #[test]
    fn test_encode_bit_string() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        // 8 bits: 10110010
        let data = [0xB2u8];
        assert!(encode_bit_string(&mut strm, "bs", &data, 8, 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<bs>10110010</bs>\n");
    }

    #[test]
    fn test_decode_bit_string() {
        let xml = b"<bs>10110010</bs>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let mut value = [0u8; 4];
        let mut n_count = 0i32;
        assert!(decode_bit_string(&mut strm, "bs", &mut value, 4, &mut n_count));
        assert_eq!(n_count, 8);
        assert_eq!(value[0], 0xB2);
    }

    // ── encode_object_identifier / decode_object_identifier ──
    #[test]
    fn test_encode_object_identifier() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;
        let mut oid = Asn1ObjectIdentifier::init();
        oid.values[0] = 1;
        oid.values[1] = 3;
        oid.values[2] = 6;
        oid.values[3] = 1;
        oid.values[4] = 4;
        oid.values[5] = 1;
        oid.n_count = 6;
        assert!(encode_object_identifier(&mut strm, "oid", &oid, 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<oid>1.3.6.1.4.1</oid>\n");
    }

    #[test]
    fn test_decode_object_identifier() {
        let xml = b"<oid>1.3.6.1.4.1</oid>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let mut oid = Asn1ObjectIdentifier::init();
        assert!(decode_object_identifier(&mut strm, "oid", &mut oid));
        assert_eq!(oid.n_count, 6);
        assert_eq!(oid.values[0], 1);
        assert_eq!(oid.values[1], 3);
        assert_eq!(oid.values[2], 6);
        assert_eq!(oid.values[3], 1);
        assert_eq!(oid.values[4], 4);
        assert_eq!(oid.values[5], 1);
    }

    // ── encode_complex_element_start / end ──
    #[test]
    fn test_encode_complex_element() {
        let mut buf = make_encode_buf(512);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;

        assert!(encode_complex_element_start(&mut strm, "seq", None, 0));
        assert!(encode_integer(&mut strm, "a", 1, 1));
        assert!(encode_integer(&mut strm, "b", 2, 1));
        assert!(encode_complex_element_end(&mut strm, "seq", 0));

        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<seq>\n  <a>1</a>\n  <b>2</b>\n</seq>\n");
    }

    // ── decode_complex_element_start / end ──
    #[test]
    fn test_decode_complex_element() {
        let xml = b"<seq><a>1</a><b>2</b></seq>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert!(decode_complex_element_start(&mut strm, "seq", None));
        assert_eq!(decode_integer(&mut strm, "a"), Some(1));
        assert_eq!(decode_integer(&mut strm, "b"), Some(2));
        assert!(decode_complex_element_end(&mut strm, "seq"));
    }

    // ── next_end_element_is ──
    #[test]
    fn test_next_end_element_is_true() {
        let xml = b"</seq>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert!(next_end_element_is(&mut strm, "seq"));
        // Should not have consumed.
        assert_eq!(strm.current_byte, 0);
    }

    #[test]
    fn test_next_end_element_is_false() {
        let xml = b"<seq>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert!(!next_end_element_is(&mut strm, "seq"));
        assert_eq!(strm.current_byte, 0);
    }

    // ── next_start_element_is ──
    #[test]
    fn test_next_start_element_is_true() {
        let xml = b"<seq>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert!(next_start_element_is(&mut strm, "seq"));
        assert_eq!(strm.current_byte, 0);
    }

    #[test]
    fn test_next_start_element_is_self_closing() {
        let xml = b"<seq/>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        assert!(next_start_element_is(&mut strm, "seq"));
        assert_eq!(strm.current_byte, 0);
    }

    // ── la_next_element_tag ──
    #[test]
    fn test_la_next_element_tag() {
        let xml = b"<seq>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let mut tag = [0u8; 100];
        assert!(la_next_element_tag(&mut strm, &mut tag));
        assert_eq!(read_str_from_buf(&tag), "seq");
        assert_eq!(strm.current_byte, 0);
    }

    // ── char_to_nibble ──
    #[test]
    fn test_char_to_nibble() {
        assert_eq!(char_to_nibble(b'0'), Some(0));
        assert_eq!(char_to_nibble(b'9'), Some(9));
        assert_eq!(char_to_nibble(b'A'), Some(10));
        assert_eq!(char_to_nibble(b'F'), Some(15));
        assert_eq!(char_to_nibble(b'a'), Some(10));
        assert_eq!(char_to_nibble(b'f'), Some(15));
        assert_eq!(char_to_nibble(b'g'), None);
        assert_eq!(char_to_nibble(b' '), None);
    }

    // ── is_part_of_id ──
    #[test]
    fn test_is_part_of_id() {
        assert!(is_part_of_id(b'a'));
        assert!(is_part_of_id(b'Z'));
        assert!(is_part_of_id(b'0'));
        assert!(is_part_of_id(b'9'));
        assert!(is_part_of_id(b'.'));
        assert!(is_part_of_id(b'+'));
        assert!(is_part_of_id(b'-'));
        assert!(is_part_of_id(b'_'));
        assert!(!is_part_of_id(b' '));
        assert!(!is_part_of_id(b'<'));
        assert!(!is_part_of_id(b'/'));
    }

    // ── double_to_string ──
    #[test]
    fn test_double_to_string_zero() {
        assert_eq!(double_to_string(0.0), "0");
        assert_eq!(double_to_string(-0.0), "0");
    }

    #[test]
    fn test_double_to_string_integral() {
        // 5.0 → normalises to 5.0E0
        let s = double_to_string(5.0);
        assert_eq!(s, "5.0E0");
    }

    #[test]
    fn test_double_to_string_large() {
        // 100.0 → normalises to 1.0E2
        let s = double_to_string(100.0);
        assert_eq!(s, "1.0E2");
    }

    #[test]
    fn test_double_to_string_small() {
        // 0.01 → normalises to 1.0E-2
        let s = double_to_string(0.01);
        assert_eq!(s, "1.0E-2");
    }

    // ── add_attribute ──
    #[test]
    fn test_add_attribute() {
        let mut attrs = XmlAttributeArray::new();
        add_attribute(&mut attrs, "name1", "val1");
        add_attribute(&mut attrs, "name2", "val2");
        assert_eq!(attrs.n_count, 2);
        assert_eq!(read_str_from_buf(&attrs.attrs[0].name), "name1");
        assert_eq!(read_str_from_buf(&attrs.attrs[0].value), "val1");
        assert_eq!(read_str_from_buf(&attrs.attrs[1].name), "name2");
        assert_eq!(read_str_from_buf(&attrs.attrs[1].value), "val2");
    }

    // ── encode with attributes ──
    #[test]
    fn test_encode_complex_element_with_attrs() {
        let mut buf = make_encode_buf(256);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;

        let mut attrs = XmlAttributeArray::new();
        add_attribute(&mut attrs, "xmlns", "http://example.com");

        assert!(encode_complex_element_start(&mut strm, "root", Some(&attrs), 0));
        let len = strm.current_byte as usize;
        let s = std::str::from_utf8(&strm.buf[..len]).unwrap();
        assert_eq!(s, "<root xmlns=\"http://example.com\">\n");
    }

    // ── decode with attributes ──
    #[test]
    fn test_decode_complex_element_with_attrs() {
        // Attribute values are limited to ID characters (letters, digits,
        // '.', '+', '-', '_') by the token scanner — same limitation as C.
        let xml = b"<root xmlns=\"my-ns\"><a>1</a></root>";
        let mut buf = xml.to_vec();
        let mut strm = ByteStream {
            buf: &mut buf,
            count: xml.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };
        let mut attrs = XmlAttributeArray::new();
        assert!(decode_complex_element_start(&mut strm, "root", Some(&mut attrs)));
        assert_eq!(attrs.n_count, 1);
        assert_eq!(read_str_from_buf(&attrs.attrs[0].name), "xmlns");
        assert_eq!(read_str_from_buf(&attrs.attrs[0].value), "my-ns");
    }

    // ── Full roundtrip: encode then decode a complex structure ──
    #[test]
    fn test_full_roundtrip() {
        let mut buf = make_encode_buf(1024);
        let mut strm = ByteStream::init(&mut buf);
        strm.encode_white_space = true;

        // Encode a simple structure.
        assert!(encode_complex_element_start(&mut strm, "Person", None, 0));
        assert!(encode_string(&mut strm, "name", "Alice", 1));
        assert!(encode_integer(&mut strm, "age", 30, 1));
        assert!(encode_boolean(&mut strm, "active", true, 1));
        assert!(encode_complex_element_end(&mut strm, "Person", 0));

        let len = strm.current_byte as usize;
        let encoded = strm.buf[..len].to_vec();
        let encoded_str = std::str::from_utf8(&encoded).unwrap().to_string();

        // Decode by wrapping the encoded bytes (without whitespace, since
        // the decoder skips whitespace via the token scanner).
        let mut d_buf = encoded.clone();
        let mut d_strm = ByteStream {
            buf: &mut d_buf,
            count: encoded.len() as i64,
            current_byte: 0,
            encode_white_space: false,
        };

        assert!(decode_complex_element_start(&mut d_strm, "Person", None));
        let mut name = [0u8; 256];
        assert!(decode_string(&mut d_strm, "name", &mut name, 256));
        assert_eq!(read_str_from_buf(&name), "Alice");
        assert_eq!(decode_integer(&mut d_strm, "age"), Some(30));
        assert_eq!(decode_boolean(&mut d_strm, "active"), Some(true));
        assert!(decode_complex_element_end(&mut d_strm, "Person"));

        // Verify the encoded string looks right.
        assert!(encoded_str.contains("<Person>"));
        assert!(encoded_str.contains("<name>Alice</name>"));
        assert!(encoded_str.contains("<age>30</age>"));
        assert!(encoded_str.contains("<active><true/></active>"));
        assert!(encoded_str.contains("</Person>"));
    }
}
