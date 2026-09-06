//! ASN1SCC Rust runtime — BER (Basic Encoding Rules) encode / decode.
//!
//! This module is the Rust equivalent of the C runtime's
//! `asn1crt_encoding_ber.h` / `asn1crt_encoding_ber.c`.  It provides
//! BER (ITU-T X.690) tag, length, and type encoding/decoding for all
//! ASN.1 primitive types supported by ASN1SCC.
//!
//! # Design notes
//!
//! The C implementation operates on `ByteStream` (a flat byte buffer with
//! a cursor) for all BER operations, and additionally uses a `BitStream`
//! for the binary real encoding within `BerEncodeReal` / `BerDecodeReal`.
//! We follow the same model.
//!
//! All encode functions return `bool` (success / failure) and take a
//! `&mut ByteStream` for writing, mirroring the C `flag` return + `ByteStream*`
//! convention.  Decode functions similarly return `bool` and write the decoded
//! value through mutable references.
//!
//! Error reporting uses `&mut ErrorCode` (an out-parameter) just like C's
//! `int *pErrCode`.

use crate::*;

// ─────────────────────────────────────────────────────────────────────────
//  Byte-stream helpers (static in C's ber.c)
// ─────────────────────────────────────────────────────────────────────────

/// Write a single byte `v` to the stream at the current cursor position,
/// then advance the cursor.  Returns `false` if the cursor is past the end
/// of the buffer.
///
/// Mirrors C's static `ByteStream_PutByte`.
fn byte_stream_put_byte(p_strm: &mut ByteStream, v: u8) -> bool {
    if p_strm.current_byte + 1 > p_strm.count + 1 {
        return false;
    }
    p_strm.buf[p_strm.current_byte as usize] = v;
    p_strm.current_byte += 1;
    true
}

/// Read a single byte from the stream at the current cursor position into
/// `v`, then advance the cursor.  Returns `false` if the cursor is past
/// the end of the buffer.
///
/// Mirrors C's static `ByteStream_GetByte`.
fn byte_stream_get_byte(p_strm: &mut ByteStream) -> Option<u8> {
    if p_strm.current_byte + 1 > p_strm.count + 1 {
        return None;
    }
    let v = p_strm.buf[p_strm.current_byte as usize];
    p_strm.current_byte += 1;
    Some(v)
}

// ─────────────────────────────────────────────────────────────────────────
//  Internal unsigned-integer encoders
// ─────────────────────────────────────────────────────────────────────────

/// Encode an unsigned integer as the minimal number of big-endian bytes
/// (leading zero bytes are suppressed).  If `value` is zero, a single `0x00`
/// byte is written.
///
/// Mirrors C's static `BerEncodeUInt`.
fn ber_encode_uint(p_strm: &mut ByteStream, value: Asn1SccUint, p_err_code: &mut ErrorCode) -> bool {
    if value == 0 {
        if !byte_stream_put_byte(p_strm, 0) {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
        return true;
    }

    let mut w_flag = false;
    for i in (0..WORD_SIZE).rev() {
        let cur_byte = ((value & BER_AUX[i as usize]) >> (8 * i)) as u8;
        if cur_byte != 0 {
            w_flag = true;
        }
        if w_flag {
            if !byte_stream_put_byte(p_strm, cur_byte) {
                *p_err_code = ErrorCode::InsufficientData;
                return false;
            }
        }
    }
    true
}

/// Encode an unsigned integer as exactly `int_size` big-endian bytes
/// (no leading-byte suppression).  Used when the length is already known.
///
/// Mirrors C's static `BerEncodeUInt2`.
fn ber_encode_uint2(
    p_strm: &mut ByteStream,
    value: Asn1SccUint,
    int_size: i32,
    p_err_code: &mut ErrorCode,
) -> bool {
    for i in (0..int_size).rev() {
        let cur_byte = ((value & BER_AUX[i as usize]) >> (8 * i)) as u8;
        if !byte_stream_put_byte(p_strm, cur_byte) {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    }
    true
}

/// Encode a BER length determinant.
///
/// - If `value` ≤ 0x7F: a single short-form length byte.
/// - Otherwise: the long form — a leading byte (0x80 | number-of-length-bytes)
///   followed by the big-endian length octets.
///
/// Mirrors C's static `BerEncodeLength`.
fn ber_encode_length(p_strm: &mut ByteStream, value: i32, p_err_code: &mut ErrorCode) -> bool {
    if value <= 0x7F {
        if !byte_stream_put_byte(p_strm, value as u8) {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
        return true;
    }

    let uv = value as Asn1SccUint;
    let mut uv1 = value as u32;
    let mut len_len: u8 = 0;
    while uv1 > 0 {
        len_len += 1;
        uv1 >>= 8;
    }
    if !byte_stream_put_byte(p_strm, len_len | 0x80) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    ber_encode_uint(p_strm, uv, p_err_code)
}

// ─────────────────────────────────────────────────────────────────────────
//  Tag handling
// ─────────────────────────────────────────────────────────────────────────

/// Encode a BER tag as a minimal big-endian unsigned integer.
///
/// Mirrors C `BerEncodeTag`.
pub fn ber_encode_tag(p_strm: &mut ByteStream, tag: BerTag, p_err_code: &mut ErrorCode) -> bool {
    ber_encode_uint(p_strm, tag, p_err_code)
}

/// Decode and verify a BER tag from the stream.  The expected `tag` is
/// compared against the decoded bytes, ignoring the constructed/primitive
/// bit (0x20) which may differ depending on context.
///
/// Returns `false` on mismatch or insufficient data.
///
/// Mirrors C `BerDecodeTag`.
pub fn ber_decode_tag(p_strm: &mut ByteStream, tag: BerTag, p_err_code: &mut ErrorCode) -> bool {
    let mut tag_size: i32 = 0;
    let mut tg_copy: BerTag = tag;
    let primitive_bit: BerTag;

    if tg_copy == 0 {
        tag_size = 1;
    }
    while tg_copy > 0 {
        tg_copy >>= 8;
        tag_size += 1;
    }
    primitive_bit = (0x20u64) << ((tag_size - 1) * 8);

    let mut read_value: BerTag = 0;
    while tag_size > 0 {
        let cur_byte = match byte_stream_get_byte(p_strm) {
            Some(b) => b,
            None => {
                *p_err_code = ErrorCode::InsufficientData;
                return false;
            }
        };
        read_value <<= 8;
        read_value |= cur_byte as BerTag;
        tag_size -= 1;
    }

    if (tag | primitive_bit) != (read_value | primitive_bit) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    true
}

/// Peek at the stream without consuming: attempt to decode `tag` and
/// restore the cursor afterwards.  Returns `true` if the next bytes
/// match `tag`.
///
/// Mirrors C `NextTagMatches`.
pub fn next_tag_matches(p_strm: &mut ByteStream, tag: BerTag) -> bool {
    let saved_byte = p_strm.current_byte;
    let mut err = ErrorCode::InsufficientData; // dummy, not used by caller
    let ret = ber_decode_tag(p_strm, tag, &mut err);
    p_strm.current_byte = saved_byte;
    ret
}

// ─────────────────────────────────────────────────────────────────────────
//  Length handling
// ─────────────────────────────────────────────────────────────────────────

/// Write the indefinite-length marker `0x80` to the stream.
///
/// In BER, constructed types may use indefinite-length encoding, which
/// starts with `0x80` and is terminated by two zero bytes (`0x00 0x00`).
///
/// Mirrors C `BerEncodeLengthStart`.
pub fn ber_encode_length_start(p_strm: &mut ByteStream, p_err_code: &mut ErrorCode) -> bool {
    if !byte_stream_put_byte(p_strm, 0x80) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    true
}

/// Write the end-of-contents marker (two zero bytes `0x00 0x00`) that
/// terminates an indefinite-length encoding.
///
/// Mirrors C `BerEncodeLengthEnd`.
pub fn ber_encode_length_end(p_strm: &mut ByteStream, p_err_code: &mut ErrorCode) -> bool {
    if !byte_stream_put_byte(p_strm, 0x00) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    if !byte_stream_put_byte(p_strm, 0x00) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    true
}

/// Decode a BER length determinant.
///
/// - Short form (high bit clear): the byte itself is the length.
/// - Long form (high bit set): the low 7 bits give the number of
///   subsequent length bytes; those bytes are the big-endian length.
/// - If the 7-bit count is zero, this is the indefinite-length marker;
///   `value` is set to `-1`.
///
/// Mirrors C `BerDecodeLength`.
pub fn ber_decode_length(p_strm: &mut ByteStream, value: &mut i32, p_err_code: &mut ErrorCode) -> bool {
    let cur_byte = match byte_stream_get_byte(p_strm) {
        Some(b) => b,
        None => {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    };

    if (cur_byte & 0x80) == 0 {
        *value = (cur_byte & 0x7F) as i32;
        return true;
    }

    let len_len = cur_byte & 0x7F;
    if len_len == 0 {
        *value = -1; // indefinite length
        return true;
    }

    let mut ret: u32 = 0;
    for _ in 0..len_len {
        let b = match byte_stream_get_byte(p_strm) {
            Some(b) => b,
            None => {
                *p_err_code = ErrorCode::InsufficientData;
                return false;
            }
        };
        ret <<= 8;
        ret |= b as u32;
    }
    *value = ret as i32;
    true
}

/// Read and verify that the next two bytes in the stream are both zero
/// (the end-of-contents marker for indefinite-length encoding).
///
/// Mirrors C `BerDecodeTwoZeroes`.
pub fn ber_decode_two_zeroes(p_strm: &mut ByteStream, p_err_code: &mut ErrorCode) -> bool {
    let cur_byte = match byte_stream_get_byte(p_strm) {
        Some(b) => b,
        None => {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    };
    if cur_byte != 0 {
        *p_err_code = ErrorCode::BerLengthMismatch;
        return false;
    }

    let cur_byte = match byte_stream_get_byte(p_strm) {
        Some(b) => b,
        None => {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    };
    if cur_byte != 0 {
        *p_err_code = ErrorCode::BerLengthMismatch;
        return false;
    }
    true
}

/// Peek at the stream without consuming: attempt `ber_decode_two_zeroes`
/// and restore the cursor afterwards.  Returns `true` if the next two
/// bytes are both zero.
///
/// Mirrors C `LA_Next_Two_Bytes_00`.
pub fn la_next_two_bytes_00(p_strm: &mut ByteStream) -> bool {
    let saved_byte = p_strm.current_byte;
    let mut err = ErrorCode::InsufficientData; // dummy
    let ret = ber_decode_two_zeroes(p_strm, &mut err);
    p_strm.current_byte = saved_byte;
    ret
}

// ─────────────────────────────────────────────────────────────────────────
//  Type encode / decode: INTEGER
// ─────────────────────────────────────────────────────────────────────────

/// Encode a signed integer with tag, length, and big-endian content bytes.
///
/// The content length is determined by `get_length_in_bytes_of_sint`
/// (minimal two's-complement representation).  The value is converted to
/// unsigned via `int2uint` before writing.
///
/// Mirrors C `BerEncodeInteger`.
pub fn ber_encode_integer(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: Asn1SccSint,
    p_err_code: &mut ErrorCode,
) -> bool {
    if !ber_encode_tag(p_strm, tag, p_err_code) {
        return false;
    }

    let length = get_length_in_bytes_of_sint(value) as u8;

    if !byte_stream_put_byte(p_strm, length) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }

    let v = int2uint(value);
    ber_encode_uint2(p_strm, v, length as i32, p_err_code)
}

/// Decode a signed integer: read tag, read length, then read `length`
/// big-endian bytes and convert back to signed via `uint2int`.
///
/// Mirrors C `BerDecodeInteger`.
pub fn ber_decode_integer(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &mut Asn1SccSint,
    p_err_code: &mut ErrorCode,
) -> bool {
    if !ber_decode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    let mut length: i32 = 0;
    if !ber_decode_length(p_strm, &mut length, p_err_code) {
        return false;
    }

    let mut ret: Asn1SccUint = 0;
    for _ in 0..length {
        let cur_byte = match byte_stream_get_byte(p_strm) {
            Some(b) => b,
            None => {
                *p_err_code = ErrorCode::InsufficientData;
                return false;
            }
        };
        ret <<= 8;
        ret |= cur_byte as Asn1SccUint;
    }

    *value = uint2int(ret, length);
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Type encode / decode: BOOLEAN
// ─────────────────────────────────────────────────────────────────────────

/// Encode a boolean: tag, length (1), and a single content byte
/// (`0xFF` for true, `0x00` for false).
///
/// Mirrors C `BerEncodeBoolean`.
pub fn ber_encode_boolean(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: bool,
    p_err_code: &mut ErrorCode,
) -> bool {
    if !ber_encode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    if !byte_stream_put_byte(p_strm, 1) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    let content = if value { 0xFFu8 } else { 0x00u8 };
    if !byte_stream_put_byte(p_strm, content) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    true
}

/// Decode a boolean: read tag, read length (must be 1), read one content
/// byte.  Any non-zero byte is interpreted as `true`.
///
/// Mirrors C `BerDecodeBoolean`.
pub fn ber_decode_boolean(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &mut bool,
    p_err_code: &mut ErrorCode,
) -> bool {
    *value = false;

    if !ber_decode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    let mut length: i32 = 0;
    if !ber_decode_length(p_strm, &mut length, p_err_code) {
        return false;
    }
    if length != 1 {
        *p_err_code = ErrorCode::BerLengthMismatch;
        return false;
    }

    let data = match byte_stream_get_byte(p_strm) {
        Some(b) => b,
        None => {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    };
    *value = data != 0;
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Type encode / decode: REAL
// ─────────────────────────────────────────────────────────────────────────

/// Encode a real value using the ASN.1 binary real encoding (via a
/// temporary `BitStream`) and write the encoded bytes after the tag.
///
/// Note: unlike most other BER types, the C implementation does **not**
/// write a separate length byte here — the real encoding itself is
/// self-delimiting (the first content byte encodes the total content
/// length).  We faithfully reproduce this behaviour.
///
/// Mirrors C `BerEncodeReal`.
pub fn ber_encode_real(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: Asn1Real,
    p_err_code: &mut ErrorCode,
) -> bool {
    let mut buf = [0u8; 100];
    let mut tmp = BitStream::new(&mut buf);
    tmp.encode_real(value);
    let length = tmp.get_length() as usize;

    if !ber_encode_tag(p_strm, tag, p_err_code) {
        return false;
    }

    for i in 0..length {
        if !byte_stream_put_byte(p_strm, buf[i]) {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    }
    true
}

/// Decode a real value by attaching a `BitStream` to the remaining buffer
/// and calling `BitStream::decode_real`, then advancing the `ByteStream`
/// cursor by the number of bytes consumed.
///
/// Mirrors C `BerDecodeReal`.
pub fn ber_decode_real(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &mut Asn1Real,
    p_err_code: &mut ErrorCode,
) -> bool {
    if !ber_decode_tag(p_strm, tag, p_err_code) {
        return false;
    }

    // Attach a BitStream to the remaining bytes in the ByteStream buffer.
    let remaining = &mut p_strm.buf[p_strm.current_byte as usize..];
    let mut tmp = BitStream::attach_buffer_no_zero(remaining);

    let (decoded, ok) = tmp.decode_real();
    if !ok {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    *value = decoded;

    p_strm.current_byte += tmp.get_length();

    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Type encode / decode: IA5String
// ─────────────────────────────────────────────────────────────────────────

/// Encode an IA5String: tag, length, and the raw character bytes.
///
/// `value` is the string content as a byte slice; `length` is the number
/// of bytes to encode (may be less than the slice length).
///
/// Mirrors C `BerEncodeIA5String`.
pub fn ber_encode_ia5_string(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &[u8],
    length: i32,
    p_err_code: &mut ErrorCode,
) -> bool {
    if !ber_encode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    if !ber_encode_length(p_strm, length, p_err_code) {
        return false;
    }
    for i in 0..length as usize {
        if !byte_stream_put_byte(p_strm, value[i]) {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    }
    true
}

/// Decode an IA5String: read tag, read length, then read that many bytes
/// into `value` (truncating to `max_length`).
///
/// The `value` buffer is zeroed before filling.  Returns the decoded
/// characters up to `max_length` bytes.
///
/// Mirrors C `BerDecodeIA5String`.
pub fn ber_decode_ia5_string(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &mut [u8],
    max_length: i32,
    p_err_code: &mut ErrorCode,
) -> bool {
    // Zero the output buffer (matching C's memset).
    for b in value[..max_length as usize].iter_mut() {
        *b = 0;
    }

    if !ber_decode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    let mut length: i32 = 0;
    if !ber_decode_length(p_strm, &mut length, p_err_code) {
        return false;
    }

    for i in 0..length as usize {
        let cur_byte = match byte_stream_get_byte(p_strm) {
            Some(b) => b,
            None => {
                *p_err_code = ErrorCode::InsufficientData;
                return false;
            }
        };
        if i < max_length as usize {
            value[i] = cur_byte as u8;
        }
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Type encode / decode: NULL
// ─────────────────────────────────────────────────────────────────────────

/// Encode NULL: tag and a single zero length byte.
///
/// Mirrors C `BerEncodeNull`.
pub fn ber_encode_null(p_strm: &mut ByteStream, tag: BerTag, p_err_code: &mut ErrorCode) -> bool {
    if !ber_encode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    if !byte_stream_put_byte(p_strm, 0) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    true
}

/// Decode NULL: read tag, read length (must be 0).
///
/// Mirrors C `BerDecodeNull`.
pub fn ber_decode_null(p_strm: &mut ByteStream, tag: BerTag, p_err_code: &mut ErrorCode) -> bool {
    let mut length: i32 = 0;

    if !ber_decode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    if !ber_decode_length(p_strm, &mut length, p_err_code) {
        return false;
    }
    if length != 0 {
        *p_err_code = ErrorCode::BerLengthMismatch;
        return false;
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Type encode / decode: BIT STRING
// ─────────────────────────────────────────────────────────────────────────

/// Encode a bit string: tag, length (content bytes + 1 for the
/// unused-bits byte), the unused-bits byte, then the content bytes.
///
/// The content length is `ceil(bit_count / 8)`.  The first content byte
/// is the number of unused bits in the final octet (0 if `bit_count`
/// is a multiple of 8).
///
/// Mirrors C `BerEncodeBitString`.
pub fn ber_encode_bit_string(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &[u8],
    bit_count: i32,
    p_err_code: &mut ErrorCode,
) -> bool {
    let mut length = bit_count / 8;
    let mut last_byte_unused_bits = 8 - bit_count % 8;
    if last_byte_unused_bits == 8 {
        last_byte_unused_bits = 0;
    }
    if bit_count % 8 != 0 {
        length += 1;
    }

    if !ber_encode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    if !ber_encode_length(p_strm, length + 1, p_err_code) {
        return false;
    }
    if !byte_stream_put_byte(p_strm, last_byte_unused_bits as u8) {
        *p_err_code = ErrorCode::InsufficientData;
        return false;
    }
    for i in 0..length as usize {
        if !byte_stream_put_byte(p_strm, value[i]) {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    }
    true
}

/// Decode a bit string: read tag, read length, read the unused-bits byte,
/// then read `length - 1` content bytes into `value` (truncating to
/// `max_bit_count`).  The actual bit count is written to `bit_count`.
///
/// Mirrors C `BerDecodeBitString`.
pub fn ber_decode_bit_string(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &mut [u8],
    bit_count: &mut i32,
    max_bit_count: i32,
    p_err_code: &mut ErrorCode,
) -> bool {
    let mut length: i32 = 0;
    let mut n_bit_cnt: i32 = 0;
    let mut max_bytes_len = max_bit_count / 8;
    if max_bit_count % 8 != 0 {
        max_bytes_len += 1;
    }

    if !ber_decode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    if !ber_decode_length(p_strm, &mut length, p_err_code) {
        return false;
    }

    let mut last_byte_unused_bits = match byte_stream_get_byte(p_strm) {
        Some(b) => b,
        None => {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    };

    for i in 0..length - 1 {
        let cur_byte = match byte_stream_get_byte(p_strm) {
            Some(b) => b,
            None => {
                *p_err_code = ErrorCode::InsufficientData;
                return false;
            }
        };
        if i < max_bytes_len {
            n_bit_cnt += 8;
            value[i as usize] = cur_byte;
        } else {
            last_byte_unused_bits = 0;
        }
    }

    n_bit_cnt -= last_byte_unused_bits as i32;
    *bit_count = n_bit_cnt;
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Type encode / decode: OCTET STRING
// ─────────────────────────────────────────────────────────────────────────

/// Encode an octet string: tag, length, and the raw content bytes.
///
/// Mirrors C `BerEncodeOctetString`.
pub fn ber_encode_octet_string(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &[u8],
    oct_count: i32,
    p_err_code: &mut ErrorCode,
) -> bool {
    if !ber_encode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    if !ber_encode_length(p_strm, oct_count, p_err_code) {
        return false;
    }
    for i in 0..oct_count as usize {
        if !byte_stream_put_byte(p_strm, value[i]) {
            *p_err_code = ErrorCode::InsufficientData;
            return false;
        }
    }
    true
}

/// Decode an octet string: read tag, read length, then read that many
/// bytes into `value` (truncating to `max_oct_count`).  The actual
/// octet count (clamped to `max_oct_count`) is written to `oct_count`.
///
/// The `value` buffer is zeroed before filling.
///
/// Mirrors C `BerDecodeOctetString`.
pub fn ber_decode_octet_string(
    p_strm: &mut ByteStream,
    tag: BerTag,
    value: &mut [u8],
    oct_count: &mut i32,
    max_oct_count: i32,
    p_err_code: &mut ErrorCode,
) -> bool {
    let mut length: i32 = 0;

    // Zero the output buffer (matching C's memset).
    for b in value[..max_oct_count as usize].iter_mut() {
        *b = 0;
    }

    if !ber_decode_tag(p_strm, tag, p_err_code) {
        return false;
    }
    if !ber_decode_length(p_strm, &mut length, p_err_code) {
        return false;
    }

    *oct_count = if length <= max_oct_count { length } else { max_oct_count };

    for i in 0..length as usize {
        let cur_byte = match byte_stream_get_byte(p_strm) {
            Some(b) => b,
            None => {
                *p_err_code = ErrorCode::InsufficientData;
                return false;
            }
        };
        if i < max_oct_count as usize {
            value[i] = cur_byte;
        }
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  Tests
// ─────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Standard BER tag for INTEGER (universal, primitive, tag 0x02).
    const TAG_INTEGER: BerTag = 0x02;
    /// Standard BER tag for BOOLEAN (universal, primitive, tag 0x01).
    const TAG_BOOLEAN: BerTag = 0x01;
    /// Standard BER tag for REAL (universal, primitive, tag 0x09).
    const TAG_REAL: BerTag = 0x09;
    /// Standard BER tag for NULL (universal, primitive, tag 0x05).
    const TAG_NULL: BerTag = 0x05;
    /// Standard BER tag for IA5String (universal, primitive, tag 0x16).
    const TAG_IA5STRING: BerTag = 0x16;
    /// Standard BER tag for BIT STRING (universal, primitive, tag 0x03).
    const TAG_BIT_STRING: BerTag = 0x03;
    /// Standard BER tag for OCTET STRING (universal, primitive, tag 0x04).
    const TAG_OCTET_STRING: BerTag = 0x04;
    /// Standard BER tag for SEQUENCE (universal, constructed, tag 0x10).
    const TAG_SEQUENCE: BerTag = 0x30;

    #[test]
    fn test_ber_encode_decode_tag() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_tag(&mut strm, TAG_INTEGER, &mut err));
        assert_eq!(strm.current_byte, 1);
        assert_eq!(strm.buf[0], 0x02);

        // Reset for decode
        strm.current_byte = 0;
        assert!(ber_decode_tag(&mut strm, TAG_INTEGER, &mut err));
        assert_eq!(strm.current_byte, 1);
    }

    #[test]
    fn test_ber_encode_decode_tag_multibyte() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        // A tag that requires 2 bytes
        let tag: BerTag = 0x0103;
        assert!(ber_encode_tag(&mut strm, tag, &mut err));
        assert_eq!(strm.current_byte, 2);

        strm.current_byte = 0;
        assert!(ber_decode_tag(&mut strm, tag, &mut err));
    }

    #[test]
    fn test_ber_decode_tag_mismatch() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        ber_encode_tag(&mut strm, TAG_INTEGER, &mut err);
        strm.current_byte = 0;
        assert!(!ber_decode_tag(&mut strm, TAG_BOOLEAN, &mut err));
        assert_eq!(err, ErrorCode::InsufficientData);
    }

    #[test]
    fn test_ber_decode_tag_constructed_vs_primitive() {
        // Tag 0x30 (SEQUENCE, constructed) should match expected 0x10 (SEQUENCE, primitive)
        // because the primitive/constructed bit (0x20) is ignored.
        let mut buf = [0u8; 16];
        buf[0] = 0x30;
        let mut strm = ByteStream::init(&mut buf);
        // Re-write the byte since init zeroes the buffer
        strm.buf[0] = 0x30;
        strm.current_byte = 0;
        strm.count = 16;

        let mut err = ErrorCode::InsufficientData;
        assert!(ber_decode_tag(&mut strm, 0x10, &mut err));
    }

    #[test]
    fn test_next_tag_matches() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        ber_encode_tag(&mut strm, TAG_INTEGER, &mut err);
        strm.current_byte = 0;

        assert!(next_tag_matches(&mut strm, TAG_INTEGER));
        assert_eq!(strm.current_byte, 0); // cursor restored
        assert!(!next_tag_matches(&mut strm, TAG_BOOLEAN));
    }

    #[test]
    fn test_ber_encode_decode_length_start_end() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_length_start(&mut strm, &mut err));
        assert_eq!(strm.buf[0], 0x80);
        assert!(ber_encode_length_end(&mut strm, &mut err));
        assert_eq!(strm.buf[1], 0x00);
        assert_eq!(strm.buf[2], 0x00);
    }

    #[test]
    fn test_ber_decode_length_short() {
        let mut buf = [0u8; 16];
        buf[0] = 0x05; // short form, length = 5
        let mut strm = ByteStream::init(&mut buf);
        strm.buf[0] = 0x05;
        strm.current_byte = 0;
        strm.count = 16;

        let mut err = ErrorCode::InsufficientData;
        let mut value: i32 = 0;
        assert!(ber_decode_length(&mut strm, &mut value, &mut err));
        assert_eq!(value, 5);
    }

    #[test]
    fn test_ber_decode_length_long() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        // Long form: 0x82 means 2 length bytes follow
        strm.buf[0] = 0x82;
        strm.buf[1] = 0x01;
        strm.buf[2] = 0x00; // length = 256
        strm.current_byte = 0;
        strm.count = 16;

        let mut value: i32 = 0;
        assert!(ber_decode_length(&mut strm, &mut value, &mut err));
        assert_eq!(value, 256);
    }

    #[test]
    fn test_ber_decode_length_indefinite() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        strm.buf[0] = 0x80; // indefinite length
        strm.current_byte = 0;
        strm.count = 16;

        let mut value: i32 = 0;
        assert!(ber_decode_length(&mut strm, &mut value, &mut err));
        assert_eq!(value, -1);
    }

    #[test]
    fn test_ber_decode_two_zeroes() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        strm.buf[0] = 0x00;
        strm.buf[1] = 0x00;
        strm.current_byte = 0;
        strm.count = 16;

        assert!(ber_decode_two_zeroes(&mut strm, &mut err));
        assert_eq!(strm.current_byte, 2);
    }

    #[test]
    fn test_ber_decode_two_zeroes_fail() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        strm.buf[0] = 0x01;
        strm.buf[1] = 0x00;
        strm.current_byte = 0;
        strm.count = 16;

        assert!(!ber_decode_two_zeroes(&mut strm, &mut err));
        assert_eq!(err, ErrorCode::BerLengthMismatch);
    }

    #[test]
    fn test_la_next_two_bytes_00() {
        let mut buf = [0u8; 16];
        let mut strm = ByteStream::init(&mut buf);
        strm.buf[0] = 0x00;
        strm.buf[1] = 0x00;
        strm.buf[2] = 0xFF;
        strm.current_byte = 0;
        strm.count = 16;

        assert!(la_next_two_bytes_00(&mut strm));
        assert_eq!(strm.current_byte, 0); // cursor restored

        // Non-zero should return false
        strm.buf[0] = 0x01;
        assert!(!la_next_two_bytes_00(&mut strm));
    }

    #[test]
    fn test_ber_encode_decode_integer_positive() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_integer(&mut strm, TAG_INTEGER, 42, &mut err));
        // tag(1) + length(1) + content(1) = 3 bytes
        assert_eq!(strm.current_byte, 3);
        assert_eq!(strm.buf[0], TAG_INTEGER as u8);
        assert_eq!(strm.buf[1], 1); // length
        assert_eq!(strm.buf[2], 42);

        // Decode
        strm.current_byte = 0;
        let mut value: Asn1SccSint = 0;
        assert!(ber_decode_integer(&mut strm, TAG_INTEGER, &mut value, &mut err));
        assert_eq!(value, 42);
    }

    #[test]
    fn test_ber_encode_decode_integer_negative() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_integer(&mut strm, TAG_INTEGER, -1, &mut err));
        // -1 in two's complement: 0xFF as a single byte
        assert_eq!(strm.buf[1], 1); // length = 1
        assert_eq!(strm.buf[2], 0xFF);

        strm.current_byte = 0;
        let mut value: Asn1SccSint = 0;
        assert!(ber_decode_integer(&mut strm, TAG_INTEGER, &mut value, &mut err));
        assert_eq!(value, -1);
    }

    #[test]
    fn test_ber_encode_decode_integer_large() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        let val: Asn1SccSint = 300;
        assert!(ber_encode_integer(&mut strm, TAG_INTEGER, val, &mut err));
        assert_eq!(strm.buf[1], 2); // length = 2
        assert_eq!(strm.buf[2], 0x01);
        assert_eq!(strm.buf[3], 0x2C);

        strm.current_byte = 0;
        let mut value: Asn1SccSint = 0;
        assert!(ber_decode_integer(&mut strm, TAG_INTEGER, &mut value, &mut err));
        assert_eq!(value, 300);
    }

    #[test]
    fn test_ber_encode_decode_integer_zero() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_integer(&mut strm, TAG_INTEGER, 0, &mut err));
        assert_eq!(strm.buf[1], 1); // length = 1
        assert_eq!(strm.buf[2], 0);

        strm.current_byte = 0;
        let mut value: Asn1SccSint = -1;
        assert!(ber_decode_integer(&mut strm, TAG_INTEGER, &mut value, &mut err));
        assert_eq!(value, 0);
    }

    #[test]
    fn test_ber_encode_decode_boolean_true() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_boolean(&mut strm, TAG_BOOLEAN, true, &mut err));
        assert_eq!(strm.buf[0], TAG_BOOLEAN as u8);
        assert_eq!(strm.buf[1], 1); // length = 1
        assert_eq!(strm.buf[2], 0xFF);

        strm.current_byte = 0;
        let mut value: bool = false;
        assert!(ber_decode_boolean(&mut strm, TAG_BOOLEAN, &mut value, &mut err));
        assert!(value);
    }

    #[test]
    fn test_ber_encode_decode_boolean_false() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_boolean(&mut strm, TAG_BOOLEAN, false, &mut err));
        assert_eq!(strm.buf[2], 0x00);

        strm.current_byte = 0;
        let mut value: bool = true;
        assert!(ber_decode_boolean(&mut strm, TAG_BOOLEAN, &mut value, &mut err));
        assert!(!value);
    }

    #[test]
    fn test_ber_encode_decode_null() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_null(&mut strm, TAG_NULL, &mut err));
        assert_eq!(strm.buf[0], TAG_NULL as u8);
        assert_eq!(strm.buf[1], 0); // length = 0
        assert_eq!(strm.current_byte, 2);

        strm.current_byte = 0;
        assert!(ber_decode_null(&mut strm, TAG_NULL, &mut err));
    }

    #[test]
    fn test_ber_decode_null_nonzero_length() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        strm.buf[0] = TAG_NULL as u8;
        strm.buf[1] = 1; // length = 1 (should be 0 for NULL)
        strm.current_byte = 0;
        strm.count = 64;

        assert!(!ber_decode_null(&mut strm, TAG_NULL, &mut err));
        assert_eq!(err, ErrorCode::BerLengthMismatch);
    }

    #[test]
    fn test_ber_encode_decode_real_zero() {
        let mut buf = [0u8; 128];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        assert!(ber_encode_real(&mut strm, TAG_REAL, 0.0, &mut err));
        assert_eq!(strm.buf[0], TAG_REAL as u8);
        // For zero, the content is a single 0x00 byte (length 0 = zero value)
        assert_eq!(strm.buf[1], 0);

        strm.current_byte = 0;
        let mut value: Asn1Real = 42.0;
        assert!(ber_decode_real(&mut strm, TAG_REAL, &mut value, &mut err));
        assert_eq!(value, 0.0);
    }

    #[test]
    fn test_ber_encode_decode_real_nonzero() {
        let mut buf = [0u8; 128];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        let val: Asn1Real = 3.14;
        assert!(ber_encode_real(&mut strm, TAG_REAL, val, &mut err));
        assert_eq!(strm.buf[0], TAG_REAL as u8);

        strm.current_byte = 0;
        let mut value: Asn1Real = 0.0;
        assert!(ber_decode_real(&mut strm, TAG_REAL, &mut value, &mut err));
        assert!((value - val).abs() < 1e-10);
    }

    #[test]
    fn test_ber_encode_decode_real_negative() {
        let mut buf = [0u8; 128];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        let val: Asn1Real = -2.5;
        assert!(ber_encode_real(&mut strm, TAG_REAL, val, &mut err));

        strm.current_byte = 0;
        let mut value: Asn1Real = 0.0;
        assert!(ber_decode_real(&mut strm, TAG_REAL, &mut value, &mut err));
        assert!((value - val).abs() < 1e-10);
    }

    #[test]
    fn test_ber_encode_decode_ia5_string() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        let text = b"Hello";
        assert!(ber_encode_ia5_string(&mut strm, TAG_IA5STRING, text, 5, &mut err));
        assert_eq!(strm.buf[0], TAG_IA5STRING as u8);
        assert_eq!(strm.buf[1], 5); // length = 5
        assert_eq!(&strm.buf[2..7], b"Hello");

        strm.current_byte = 0;
        let mut value = [0u8; 16];
        assert!(ber_decode_ia5_string(&mut strm, TAG_IA5STRING, &mut value, 16, &mut err));
        assert_eq!(&value[..5], b"Hello");
    }

    #[test]
    fn test_ber_encode_decode_ia5_string_truncated() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        let text = b"Hello World";
        assert!(ber_encode_ia5_string(&mut strm, TAG_IA5STRING, text, 11, &mut err));

        strm.current_byte = 0;
        let mut value = [0u8; 5]; // max_length = 5
        assert!(ber_decode_ia5_string(&mut strm, TAG_IA5STRING, &mut value, 5, &mut err));
        assert_eq!(&value, b"Hello");
    }

    #[test]
    fn test_ber_encode_decode_bit_string() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        // 10 bits = 2 bytes, last byte has 6 unused bits
        let data = [0xAA, 0xC0]; // 10101010 11000000 (10 bits: 10101010 11)
        assert!(ber_encode_bit_string(&mut strm, TAG_BIT_STRING, &data, 10, &mut err));
        assert_eq!(strm.buf[0], TAG_BIT_STRING as u8);
        // length = 2 content bytes + 1 unused-bits byte = 3
        assert_eq!(strm.buf[1], 3);
        assert_eq!(strm.buf[2], 6); // 6 unused bits in last byte
        assert_eq!(strm.buf[3], 0xAA);
        assert_eq!(strm.buf[4], 0xC0);

        strm.current_byte = 0;
        let mut value = [0u8; 16];
        let mut bit_count: i32 = 0;
        assert!(ber_decode_bit_string(
            &mut strm,
            TAG_BIT_STRING,
            &mut value,
            &mut bit_count,
            128,
            &mut err
        ));
        assert_eq!(bit_count, 10);
        assert_eq!(value[0], 0xAA);
        assert_eq!(value[1], 0xC0);
    }

    #[test]
    fn test_ber_encode_decode_bit_string_byte_aligned() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        // 16 bits = 2 bytes, 0 unused bits
        let data = [0xFF, 0xAA];
        assert!(ber_encode_bit_string(&mut strm, TAG_BIT_STRING, &data, 16, &mut err));
        assert_eq!(strm.buf[2], 0); // 0 unused bits

        strm.current_byte = 0;
        let mut value = [0u8; 16];
        let mut bit_count: i32 = 0;
        assert!(ber_decode_bit_string(
            &mut strm,
            TAG_BIT_STRING,
            &mut value,
            &mut bit_count,
            128,
            &mut err
        ));
        assert_eq!(bit_count, 16);
        assert_eq!(value[0], 0xFF);
        assert_eq!(value[1], 0xAA);
    }

    #[test]
    fn test_ber_encode_decode_octet_string() {
        let mut buf = [0u8; 64];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        let data = [0xDE, 0xAD, 0xBE, 0xEF];
        assert!(ber_encode_octet_string(&mut strm, TAG_OCTET_STRING, &data, 4, &mut err));
        assert_eq!(strm.buf[0], TAG_OCTET_STRING as u8);
        assert_eq!(strm.buf[1], 4); // length = 4
        assert_eq!(&strm.buf[2..6], &[0xDE, 0xAD, 0xBE, 0xEF]);

        strm.current_byte = 0;
        let mut value = [0u8; 16];
        let mut oct_count: i32 = 0;
        assert!(ber_decode_octet_string(
            &mut strm,
            TAG_OCTET_STRING,
            &mut value,
            &mut oct_count,
            16,
            &mut err
        ));
        assert_eq!(oct_count, 4);
        assert_eq!(&value[..4], &[0xDE, 0xAD, 0xBE, 0xEF]);
    }

    #[test]
    fn test_ber_encode_decode_octet_string_long() {
        let mut buf = [0u8; 256];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        // 200 bytes requires long-form length
        let data = [0x42u8; 200];
        assert!(ber_encode_octet_string(&mut strm, TAG_OCTET_STRING, &data, 200, &mut err));
        assert_eq!(strm.buf[0], TAG_OCTET_STRING as u8);
        // Long form: 0x81 means 1 length byte follows
        assert_eq!(strm.buf[1], 0x81);
        assert_eq!(strm.buf[2], 200);

        strm.current_byte = 0;
        let mut value = [0u8; 256];
        let mut oct_count: i32 = 0;
        assert!(ber_decode_octet_string(
            &mut strm,
            TAG_OCTET_STRING,
            &mut value,
            &mut oct_count,
            256,
            &mut err
        ));
        assert_eq!(oct_count, 200);
        for i in 0..200 {
            assert_eq!(value[i], 0x42);
        }
    }

    #[test]
    fn test_ber_encode_decode_integer_roundtrip_various() {
        let values: &[Asn1SccSint] = &[0, 1, -1, 127, 128, -128, 255, 256, -256,
                                        32767, 32768, -32768, 65535, 65536,
                                        i32::MAX as i64, i32::MIN as i64,
                                        i64::MAX, i64::MIN];

        for &val in values {
            let mut buf = [0u8; 128];
            let mut strm = ByteStream::init(&mut buf);
            let mut err = ErrorCode::InsufficientData;

            assert!(ber_encode_integer(&mut strm, TAG_INTEGER, val, &mut err),
                    "encode failed for {}", val);

            strm.current_byte = 0;
            let mut decoded: Asn1SccSint = 0;
            assert!(ber_decode_integer(&mut strm, TAG_INTEGER, &mut decoded, &mut err),
                    "decode failed for {}", val);
            assert_eq!(decoded, val, "roundtrip mismatch for {}", val);
        }
    }

    #[test]
    fn test_ber_encode_decode_sequence_with_indefinite_length() {
        // Simulate a constructed SEQUENCE with indefinite length
        let mut buf = [0u8; 128];
        let mut strm = ByteStream::init(&mut buf);
        let mut err = ErrorCode::InsufficientData;

        // Encode a SEQUENCE containing an INTEGER
        assert!(ber_encode_tag(&mut strm, TAG_SEQUENCE, &mut err));
        assert!(ber_encode_length_start(&mut strm, &mut err));
        assert!(ber_encode_integer(&mut strm, TAG_INTEGER, 42, &mut err));
        assert!(ber_encode_length_end(&mut strm, &mut err));

        // Now decode it back
        strm.current_byte = 0;
        assert!(ber_decode_tag(&mut strm, TAG_SEQUENCE, &mut err));

        let mut length: i32 = 0;
        assert!(ber_decode_length(&mut strm, &mut length, &mut err));
        assert_eq!(length, -1); // indefinite

        let mut value: Asn1SccSint = 0;
        assert!(ber_decode_integer(&mut strm, TAG_INTEGER, &mut value, &mut err));
        assert_eq!(value, 42);

        assert!(ber_decode_two_zeroes(&mut strm, &mut err));
    }
}
