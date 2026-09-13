//! ASN1SCC Rust runtime — ACN encoding / decoding.
//!
//! This module is the Rust equivalent of the C runtime's
//! `asn1crt_encoding_acn.h` / `asn1crt_encoding_acn.c` (~2 660 lines).
//! It provides all ACN-specific encode / decode functions: alignment,
//! positive-integer / two's-complement / BCD / ASCII integer encodings,
//! boolean patterns, NULL patterns, IEEE-754 reals, scaled reals, string
//! encodings, length determinants, MILBUS and the **deferred-patching**
//! infrastructure used when `--acn-deferred` is enabled.
//!
//! All functions build on the `BitStream` type and free helpers defined
//! in `lib.rs`.

#![allow(clippy::too_many_arguments)]
#![allow(clippy::needless_range_loop)]
#![allow(non_snake_case)]
#![allow(unused_assignments)]

use crate::*;

// ─────────────────────────────────────────────────────────────────────────
//  Endianness helper
// ─────────────────────────────────────────────────────────────────────────

/// Returns `true` on little-endian targets (mirrors C `RequiresReverse`).
fn requires_reverse() -> bool {
    // 0x0001 as a u16: on little-endian the first byte is 0x01.
    let word: u16 = 0x0001;
    word.to_le_bytes()[0] == 1
}

// ─────────────────────────────────────────────────────────────────────────
//  Alignment functions
// ─────────────────────────────────────────────────────────────────────────

/// Align the bit stream cursor to the next byte boundary.
/// Mirrors C `Acn_AlignToNextByte`.
pub fn acn_align_to_next_byte(p_bit_strm: &mut BitStream, b_encode: bool) {
    if p_bit_strm.current_bit != 0 {
        p_bit_strm.current_bit = 0;
        p_bit_strm.current_byte += 1;
        if b_encode {
            p_bit_strm.push_data_if_required();
        } else {
            p_bit_strm.fetch_data_if_required();
        }
    }
}

#[allow(non_snake_case)]
pub fn acn_align_toNextByte(p_bit_strm: &mut BitStream, b_encode: bool) {
    acn_align_to_next_byte(p_bit_strm, b_encode);
}

/// Align the bit stream cursor to the next 16-bit word boundary.
/// Mirrors C `Acn_AlignToNextWord`.
pub fn acn_align_to_next_word(p_bit_strm: &mut BitStream, b_encode: bool) {
    acn_align_to_next_byte(p_bit_strm, b_encode);
    let no_bytes = NO_OF_BYTES_IN_INT16 as i64;
    p_bit_strm.current_byte =
        ((p_bit_strm.current_byte + (no_bytes - 1)) / no_bytes) * no_bytes;
    if b_encode {
        p_bit_strm.push_data_if_required();
    } else {
        p_bit_strm.fetch_data_if_required();
    }
}

#[allow(non_snake_case)]
pub fn acn_align_toNextWord(p_bit_strm: &mut BitStream, b_encode: bool) {
    acn_align_to_next_word(p_bit_strm, b_encode);
}

/// Align the bit stream cursor to the next 32-bit double-word boundary.
/// Mirrors C `Acn_AlignToNextDWord`.
pub fn acn_align_to_next_dword(p_bit_strm: &mut BitStream, b_encode: bool) {
    acn_align_to_next_byte(p_bit_strm, b_encode);
    let no_bytes = NO_OF_BYTES_IN_INT32 as i64;
    p_bit_strm.current_byte =
        ((p_bit_strm.current_byte + (no_bytes - 1)) / no_bytes) * no_bytes;
    if b_encode {
        p_bit_strm.push_data_if_required();
    } else {
        p_bit_strm.fetch_data_if_required();
    }
}

#[allow(non_snake_case)]
pub fn acn_align_toNextDWord(p_bit_strm: &mut BitStream, b_encode: bool) {
    acn_align_to_next_dword(p_bit_strm, b_encode);
}

// ─────────────────────────────────────────────────────────────────────────
//  PositiveInteger encoding
// ─────────────────────────────────────────────────────────────────────────

/// Encode a positive integer using a constant-size bit field.
/// Mirrors C `Acn_Enc_Int_PositiveInteger_ConstSize`.
pub fn acn_enc_int_positive_integer_const_size(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
    encoded_size_in_bits: i32,
) {
    if encoded_size_in_bits == 0 {
        return;
    }
    let n_bits = get_number_of_bits_for_non_negative_integer(int_val);
    p_bit_strm.append_n_bit_zero(encoded_size_in_bits - n_bits);
    p_bit_strm.encode_non_negative_integer(int_val);
}

/// Encode a positive integer in 8 bits (one byte).
/// Mirrors C `Acn_Enc_Int_PositiveInteger_ConstSize_8`.
pub fn acn_enc_int_positive_integer_const_size_8(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    p_bit_strm.append_byte0(int_val as u8);
}

/// Encode a positive integer in big-endian byte order (generic helper).
/// Mirrors C `Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B`.
fn acn_enc_int_positive_integer_const_size_big_endian_b(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
    size: i32,
) {
    let tmp = int_val;
    let mut mask: Asn1SccUint = 0xFF;
    mask <<= (size - 1) * 8;
    for i in 0..size {
        let byte_to_encode = ((tmp & mask) >> ((size - i - 1) * 8)) as u8;
        p_bit_strm.append_byte0(byte_to_encode);
        mask >>= 8;
    }
}

/// Encode a positive integer in big-endian 16-bit.
pub fn acn_enc_int_positive_integer_const_size_big_endian_16(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    acn_enc_int_positive_integer_const_size_big_endian_b(p_bit_strm, int_val, 2);
}

/// Encode a positive integer in big-endian 32-bit.
pub fn acn_enc_int_positive_integer_const_size_big_endian_32(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    acn_enc_int_positive_integer_const_size_big_endian_b(p_bit_strm, int_val, 4);
}

/// Encode a positive integer in big-endian 64-bit.
pub fn acn_enc_int_positive_integer_const_size_big_endian_64(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    acn_enc_int_positive_integer_const_size_big_endian_b(p_bit_strm, int_val, WORD_SIZE);
}

/// Encode a positive integer in little-endian byte order (generic helper).
/// Mirrors C `Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N`.
fn acn_enc_int_positive_integer_const_size_little_endian_n(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
    size: i32,
) {
    let mut tmp = int_val;
    for _ in 0..size {
        let byte_to_encode = tmp as u8;
        p_bit_strm.append_byte0(byte_to_encode);
        tmp >>= 8;
    }
}

/// Encode a positive integer in little-endian 16-bit.
pub fn acn_enc_int_positive_integer_const_size_little_endian_16(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    acn_enc_int_positive_integer_const_size_little_endian_n(p_bit_strm, int_val, 2);
}

/// Encode a positive integer in little-endian 32-bit.
pub fn acn_enc_int_positive_integer_const_size_little_endian_32(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    acn_enc_int_positive_integer_const_size_little_endian_n(p_bit_strm, int_val, 4);
}

/// Encode a positive integer in little-endian 64-bit.
pub fn acn_enc_int_positive_integer_const_size_little_endian_64(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    acn_enc_int_positive_integer_const_size_little_endian_n(p_bit_strm, int_val, WORD_SIZE);
}

// ─────────────────────────────────────────────────────────────────────────
//  PositiveInteger decoding
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from a constant-size bit field.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize`.
pub fn acn_dec_int_positive_integer_const_size(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bits: i32,
) -> (Asn1SccUint, bool) {
    p_bit_strm.decode_non_negative_integer(encoded_size_in_bits)
}

/// Decode a positive integer from 8 bits (one byte).
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_8`.
pub fn acn_dec_int_positive_integer_const_size_8(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    let (v, ok) = p_bit_strm.read_byte();
    (v as Asn1SccUint, ok)
}

/// Decode a positive integer from big-endian bytes (generic helper).
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N`.
fn acn_dec_int_positive_integer_const_size_big_endian_n(
    p_bit_strm: &mut BitStream,
    size_in_bytes: i32,
) -> (Asn1SccUint, bool) {
    let mut ret: Asn1SccUint = 0;
    for _ in 0..size_in_bytes {
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return (0, false);
        }
        ret <<= 8;
        ret |= b as Asn1SccUint;
    }
    (ret, true)
}

/// Decode a positive integer from big-endian 16-bit.
pub fn acn_dec_int_positive_integer_const_size_big_endian_16(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    acn_dec_int_positive_integer_const_size_big_endian_n(p_bit_strm, 2)
}

/// Decode a positive integer from big-endian 32-bit.
pub fn acn_dec_int_positive_integer_const_size_big_endian_32(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    acn_dec_int_positive_integer_const_size_big_endian_n(p_bit_strm, 4)
}

/// Decode a positive integer from big-endian 64-bit.
pub fn acn_dec_int_positive_integer_const_size_big_endian_64(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    // C skips (8 - WORD_SIZE) leading bytes; WORD_SIZE is 8 so no skip.
    acn_dec_int_positive_integer_const_size_big_endian_n(p_bit_strm, WORD_SIZE)
}

/// Decode a positive integer from little-endian bytes (generic helper).
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N`.
fn acn_dec_int_positive_integer_const_size_little_endian_n(
    p_bit_strm: &mut BitStream,
    size_in_bytes: i32,
) -> (Asn1SccUint, bool) {
    let mut ret: Asn1SccUint = 0;
    for i in 0..size_in_bytes {
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return (0, false);
        }
        let tmp = (b as Asn1SccUint) << (i * 8);
        ret |= tmp;
    }
    (ret, true)
}

/// Decode a positive integer from little-endian 16-bit.
pub fn acn_dec_int_positive_integer_const_size_little_endian_16(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    acn_dec_int_positive_integer_const_size_little_endian_n(p_bit_strm, 2)
}

/// Decode a positive integer from little-endian 32-bit.
pub fn acn_dec_int_positive_integer_const_size_little_endian_32(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    acn_dec_int_positive_integer_const_size_little_endian_n(p_bit_strm, 4)
}

/// Decode a positive integer from little-endian 64-bit.
pub fn acn_dec_int_positive_integer_const_size_little_endian_64(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    let (ret, ok) =
        acn_dec_int_positive_integer_const_size_little_endian_n(p_bit_strm, WORD_SIZE);
    // C advances current_byte by (8 - WORD_SIZE) after reading; WORD_SIZE=8 so no-op.
    (ret, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  PositiveInteger var-size length-embedded
// ─────────────────────────────────────────────────────────────────────────

/// Helper: encode an unsigned integer value in `n_bytes` bytes
/// (big-endian, left-justified within the word).
/// Mirrors C `Encode_UnsignedInteger`.
fn encode_unsigned_integer(p_bit_strm: &mut BitStream, mut val: Asn1SccUint, n_bytes: u8) {
    debug_assert!(n_bytes <= 8);
    val <<= (WORD_SIZE as u32 * 8 - n_bytes as u32 * 8) as u32;
    for _ in 0..n_bytes {
        let byte_to_encode = ((val & 0xFF000000_00000000) >> ((WORD_SIZE - 1) * 8)) as u8;
        p_bit_strm.append_byte0(byte_to_encode);
        val <<= 8;
    }
}

/// Encode a positive integer with variable size, length-embedded.
/// Mirrors C `Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded`.
pub fn acn_enc_int_positive_integer_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    let n_bytes = get_length_in_bytes_of_uint(int_val) as u8;
    p_bit_strm.append_byte0(n_bytes);
    encode_unsigned_integer(p_bit_strm, int_val, n_bytes);
}

/// Decode a positive integer with variable size, length-embedded.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded`.
pub fn acn_dec_int_positive_integer_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    let (n_bytes, ok) = p_bit_strm.read_byte();
    if !ok {
        return (0, false);
    }
    if n_bytes as i32 > WORD_SIZE {
        return (0, false);
    }
    let mut v: Asn1SccUint = 0;
    for _ in 0..n_bytes {
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return (0, false);
        }
        v = (v << 8) | b as Asn1SccUint;
    }
    (v, true)
}

// ─────────────────────────────────────────────────────────────────────────
//  TwosComplement (signed) encoding
// ─────────────────────────────────────────────────────────────────────────

/// Encode a signed integer using two's complement in a constant-size bit field.
/// Mirrors C `Acn_Enc_Int_TwosComplement_ConstSize`.
pub fn acn_enc_int_twos_complement_const_size(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
    encoded_size_in_bits: i32,
) {
    if int_val >= 0 {
        let n_bits = get_number_of_bits_for_non_negative_integer(int_val as Asn1SccUint);
        p_bit_strm.append_n_bit_zero(encoded_size_in_bits - n_bits);
        p_bit_strm.encode_non_negative_integer(int_val as Asn1SccUint);
    } else {
        let abs_val = (int_val.wrapping_neg() - 1) as Asn1SccUint;
        let n_bits = get_number_of_bits_for_non_negative_integer(abs_val);
        p_bit_strm.append_n_bit_one(encoded_size_in_bits - n_bits);
        p_bit_strm.encode_non_negative_integer_neg(abs_val, true);
    }
}

/// Encode a signed integer in 8 bits (one byte).
pub fn acn_enc_int_twos_complement_const_size_8(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    acn_enc_int_positive_integer_const_size_8(p_bit_strm, int2uint(int_val));
}

/// Encode a signed integer in big-endian 16-bit.
pub fn acn_enc_int_twos_complement_const_size_big_endian_16(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    acn_enc_int_positive_integer_const_size_big_endian_16(p_bit_strm, int2uint(int_val));
}

/// Encode a signed integer in big-endian 32-bit.
pub fn acn_enc_int_twos_complement_const_size_big_endian_32(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    acn_enc_int_positive_integer_const_size_big_endian_32(p_bit_strm, int2uint(int_val));
}

/// Encode a signed integer in big-endian 64-bit.
pub fn acn_enc_int_twos_complement_const_size_big_endian_64(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    acn_enc_int_positive_integer_const_size_big_endian_64(p_bit_strm, int2uint(int_val));
}

/// Encode a signed integer in little-endian 16-bit.
pub fn acn_enc_int_twos_complement_const_size_little_endian_16(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    acn_enc_int_positive_integer_const_size_little_endian_16(p_bit_strm, int2uint(int_val));
}

/// Encode a signed integer in little-endian 32-bit.
pub fn acn_enc_int_twos_complement_const_size_little_endian_32(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    acn_enc_int_positive_integer_const_size_little_endian_32(p_bit_strm, int2uint(int_val));
}

/// Encode a signed integer in little-endian 64-bit.
pub fn acn_enc_int_twos_complement_const_size_little_endian_64(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    acn_enc_int_positive_integer_const_size_little_endian_64(p_bit_strm, int2uint(int_val));
}

// ─────────────────────────────────────────────────────────────────────────
//  TwosComplement (signed) decoding
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from a two's complement constant-size bit field.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize`.
pub fn acn_dec_int_twos_complement_const_size(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bits: i32,
) -> (Asn1SccSint, bool) {
    let val_is_negative = p_bit_strm.peek_bit();
    let n_bytes = encoded_size_in_bits / 8;
    let rst_bits = encoded_size_in_bits % 8;
    let mut result: Asn1SccSint = if val_is_negative { MAX_INT as Asn1SccSint } else { 0 };
    for _ in 0..n_bytes {
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return (0, false);
        }
        result = (result << 8) | b as Asn1SccSint;
    }
    if rst_bits > 0 {
        let (b, ok) = p_bit_strm.read_partial_byte(rst_bits as u8);
        if !ok {
            return (0, false);
        }
        result = (result << rst_bits) | b as Asn1SccSint;
    }
    (result, true)
}

/// Decode a signed integer from 8 bits (one byte).
pub fn acn_dec_int_twos_complement_const_size_8(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (tmp, ok) = acn_dec_int_positive_integer_const_size_8(p_bit_strm);
    if !ok {
        return (0, false);
    }
    (uint2int(tmp, 1), true)
}

/// Decode a signed integer from big-endian 16-bit.
pub fn acn_dec_int_twos_complement_const_size_big_endian_16(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (tmp, ok) = acn_dec_int_positive_integer_const_size_big_endian_16(p_bit_strm);
    if !ok {
        return (0, false);
    }
    (uint2int(tmp, 2), true)
}

/// Decode a signed integer from big-endian 32-bit.
pub fn acn_dec_int_twos_complement_const_size_big_endian_32(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (tmp, ok) = acn_dec_int_positive_integer_const_size_big_endian_32(p_bit_strm);
    if !ok {
        return (0, false);
    }
    (uint2int(tmp, 4), true)
}

/// Decode a signed integer from big-endian 64-bit.
pub fn acn_dec_int_twos_complement_const_size_big_endian_64(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (tmp, ok) = acn_dec_int_positive_integer_const_size_big_endian_64(p_bit_strm);
    if !ok {
        return (0, false);
    }
    (uint2int(tmp, WORD_SIZE), true)
}

/// Decode a signed integer from little-endian 16-bit.
pub fn acn_dec_int_twos_complement_const_size_little_endian_16(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (tmp, ok) = acn_dec_int_positive_integer_const_size_little_endian_16(p_bit_strm);
    if !ok {
        return (0, false);
    }
    (uint2int(tmp, 2), true)
}

/// Decode a signed integer from little-endian 32-bit.
pub fn acn_dec_int_twos_complement_const_size_little_endian_32(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (tmp, ok) = acn_dec_int_positive_integer_const_size_little_endian_32(p_bit_strm);
    if !ok {
        return (0, false);
    }
    (uint2int(tmp, 4), true)
}

/// Decode a signed integer from little-endian 64-bit.
pub fn acn_dec_int_twos_complement_const_size_little_endian_64(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (tmp, ok) = acn_dec_int_positive_integer_const_size_little_endian_64(p_bit_strm);
    if !ok {
        return (0, false);
    }
    (uint2int(tmp, WORD_SIZE), true)
}

// ─────────────────────────────────────────────────────────────────────────
//  TwosComplement var-size length-embedded
// ─────────────────────────────────────────────────────────────────────────

/// Helper: convert signed int to unsigned (two's complement).
/// Mirrors C `To_UInt`.
fn to_uint(int_val: Asn1SccSint) -> Asn1SccUint {
    if int_val < 0 {
        let ret = (int_val.wrapping_neg() - 1) as Asn1SccUint;
        !ret
    } else {
        int_val as Asn1SccUint
    }
}

/// Encode a signed integer with variable size, length-embedded.
/// Mirrors C `Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded`.
pub fn acn_enc_int_twos_complement_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    let n_bytes = get_length_in_bytes_of_sint(int_val) as u8;
    p_bit_strm.append_byte0(n_bytes);
    encode_unsigned_integer(p_bit_strm, to_uint(int_val), n_bytes);
}

/// Decode a signed integer with variable size, length-embedded.
/// Mirrors C `Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded`.
pub fn acn_dec_int_twos_complement_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (n_bytes, ok) = p_bit_strm.read_byte();
    if !ok {
        return (0, false);
    }
    if n_bytes as i32 > WORD_SIZE {
        return (0, false);
    }
    let mut v: Asn1SccUint = 0;
    let mut is_negative = false;
    for i in 0..n_bytes {
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return (0, false);
        }
        if i == 0 && (b & 0x80) != 0 {
            v = MAX_INT;
            is_negative = true;
        }
        v = (v << 8) | b as Asn1SccUint;
    }
    let result = if is_negative {
        -((!v) as Asn1SccSint) - 1
    } else {
        v as Asn1SccSint
    };
    (result, true)
}

// ─────────────────────────────────────────────────────────────────────────
//  BCD integer encoding
// ─────────────────────────────────────────────────────────────────────────

/// Returns the number of nibbles needed to represent `int_val` in BCD.
/// Mirrors C `Acn_Get_Int_Size_BCD`.
fn acn_get_int_size_bcd(mut int_val: Asn1SccUint) -> i32 {
    let mut ret = 0;
    while int_val > 0 {
        int_val /= 10;
        ret += 1;
    }
    ret
}

/// Encode a positive integer in BCD with a constant number of nibbles.
/// Mirrors C `Acn_Enc_Int_BCD_ConstSize`.
pub fn acn_enc_int_bcd_const_size(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
    encoded_size_in_nibbles: i32,
) {
    let mut tmp = [0u8; 100];
    let mut total_nibbles: i32 = 0;
    let mut val = int_val;
    while val > 0 {
        tmp[total_nibbles as usize] = (val % 10) as u8;
        total_nibbles += 1;
        val /= 10;
    }
    debug_assert!(encoded_size_in_nibbles >= total_nibbles);
    for i in (0..encoded_size_in_nibbles).rev() {
        p_bit_strm.append_partial_byte(tmp[i as usize], 4, false);
    }
}

/// Decode a positive integer from BCD with a constant number of nibbles.
/// Mirrors C `Acn_Dec_Int_BCD_ConstSize`.
pub fn acn_dec_int_bcd_const_size(
    p_bit_strm: &mut BitStream,
    encoded_size_in_nibbles: i32,
) -> (Asn1SccUint, bool) {
    let mut ret: Asn1SccUint = 0;
    let mut remaining = encoded_size_in_nibbles;
    while remaining > 0 {
        let (digit, ok) = p_bit_strm.read_partial_byte(4);
        if !ok {
            return (0, false);
        }
        ret *= 10;
        ret += digit as Asn1SccUint;
        remaining -= 1;
    }
    (ret, true)
}

/// Encode a positive integer in BCD with variable size, length-embedded.
/// Mirrors C `Acn_Enc_Int_BCD_VarSize_LengthEmbedded`.
pub fn acn_enc_int_bcd_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    let n_nibbles = acn_get_int_size_bcd(int_val);
    p_bit_strm.append_byte0(n_nibbles as u8);
    acn_enc_int_bcd_const_size(p_bit_strm, int_val, n_nibbles);
}

/// Decode a positive integer from BCD with variable size, length-embedded.
/// Mirrors C `Acn_Dec_Int_BCD_VarSize_LengthEmbedded`.
pub fn acn_dec_int_bcd_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    let (n_nibbles, ok) = p_bit_strm.read_byte();
    if !ok {
        return (0, false);
    }
    if n_nibbles as i32 > 2 * WORD_SIZE {
        return (0, false);
    }
    acn_dec_int_bcd_const_size(p_bit_strm, n_nibbles as i32)
}

/// Encode a positive integer in BCD, null-terminated (terminator = 0xF).
/// Mirrors C `Acn_Enc_Int_BCD_VarSize_NullTerminated`.
pub fn acn_enc_int_bcd_var_size_null_terminated(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    let n_nibbles = acn_get_int_size_bcd(int_val);
    acn_enc_int_bcd_const_size(p_bit_strm, int_val, n_nibbles);
    p_bit_strm.append_partial_byte(0xF, 4, false);
}

/// Decode a positive integer from BCD, null-terminated (terminator > 9).
/// Mirrors C `Acn_Dec_Int_BCD_VarSize_NullTerminated`.
pub fn acn_dec_int_bcd_var_size_null_terminated(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    let mut ret: Asn1SccUint = 0;
    loop {
        let (digit, ok) = p_bit_strm.read_partial_byte(4);
        if !ok {
            return (0, false);
        }
        if digit > 9 {
            break;
        }
        ret *= 10;
        ret += digit as Asn1SccUint;
    }
    (ret, true)
}

// ─────────────────────────────────────────────────────────────────────────
//  ASCII integer encoding (unsigned)
// ─────────────────────────────────────────────────────────────────────────

/// Encode an unsigned integer as ASCII decimal digits with a constant size.
/// Mirrors C `Acn_Enc_UInt_ASCII_ConstSize`.
pub fn acn_enc_uint_ascii_const_size(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
    encoded_size_in_bytes: i32,
) {
    let mut tmp = [0u8; 100];
    let mut total_nibbles: i32 = 0;
    let mut val = int_val;
    while val > 0 {
        tmp[total_nibbles as usize] = (val % 10) as u8;
        total_nibbles += 1;
        val /= 10;
    }
    debug_assert!(encoded_size_in_bytes >= total_nibbles);
    for i in (0..encoded_size_in_bytes).rev() {
        p_bit_strm.append_byte0(tmp[i as usize] + b'0');
    }
}

/// Decode an unsigned integer from ASCII decimal digits with a constant size.
/// Mirrors C `Acn_Dec_UInt_ASCII_ConstSize`.
pub fn acn_dec_uint_ascii_const_size(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bytes: i32,
) -> (Asn1SccUint, bool) {
    let mut ret: Asn1SccUint = 0;
    let mut remaining = encoded_size_in_bytes;
    while remaining > 0 {
        let (digit, ok) = p_bit_strm.read_byte();
        if !ok {
            return (0, false);
        }
        if !(digit >= b'0' && digit <= b'9') {
            return (0, false);
        }
        let d = (digit - b'0') as Asn1SccUint;
        ret *= 10;
        ret += d;
        remaining -= 1;
    }
    (ret, true)
}

// ─────────────────────────────────────────────────────────────────────────
//  ASCII integer encoding (signed)
// ─────────────────────────────────────────────────────────────────────────

/// Encode a signed integer as ASCII decimal digits with a constant size.
/// First byte is the sign ('+' or '-'), remaining bytes are digits.
/// Mirrors C `Acn_Enc_SInt_ASCII_ConstSize`.
pub fn acn_enc_sint_ascii_const_size(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
    encoded_size_in_bytes: i32,
) {
    let abs_int_val = if int_val >= 0 {
        int_val as Asn1SccUint
    } else {
        int_val.wrapping_neg() as Asn1SccUint
    };
    p_bit_strm.append_byte0(if int_val >= 0 { b'+' } else { b'-' });
    acn_enc_uint_ascii_const_size(p_bit_strm, abs_int_val, encoded_size_in_bytes - 1);
}

/// Decode a signed integer from ASCII decimal digits with a constant size.
/// First byte is the sign ('+' or '-'), remaining bytes are digits.
/// Mirrors C `Acn_Dec_SInt_ASCII_ConstSize`.
pub fn acn_dec_sint_ascii_const_size(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bytes: i32,
) -> (Asn1SccSint, bool) {
    let (digit, ok) = p_bit_strm.read_byte();
    if !ok {
        return (0, false);
    }
    let sign: Asn1SccSint = match digit {
        b'+' => 1,
        b'-' => -1,
        _ => return (0, false),
    };
    let (ret, ok) = acn_dec_uint_ascii_const_size(p_bit_strm, encoded_size_in_bytes - 1);
    if !ok {
        return (0, false);
    }
    let result = if sign < 0 {
        (ret as Asn1SccSint).wrapping_neg()
    } else {
        ret as Asn1SccSint
    };
    (result, true)
}

// ─────────────────────────────────────────────────────────────────────────
//  ASCII integer encoding — var-size length-embedded
// ─────────────────────────────────────────────────────────────────────────

/// Helper: convert an unsigned integer to ASCII digit bytes.
/// Mirrors C `getIntegerDigits`.
fn get_integer_digits(mut int_val: Asn1SccUint) -> ([u8; 100], u8) {
    let mut reversed = [0u8; 100];
    let mut digits = [0u8; 100];
    let mut total: u8 = 0;
    if int_val > 0 {
        while int_val > 0 && (total as usize) < 100 {
            reversed[total as usize] = b'0' + (int_val % 10) as u8;
            total += 1;
            int_val /= 10;
        }
        for i in 0..total as usize {
            digits[i] = reversed[(total as usize - 1) - i];
        }
    } else {
        digits[0] = b'0';
        total = 1;
    }
    (digits, total)
}

/// Encode a signed integer as ASCII with variable size, length-embedded.
/// Mirrors C `Acn_Enc_SInt_ASCII_VarSize_LengthEmbedded`.
pub fn acn_enc_sint_ascii_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
) {
    let abs_int_val = if int_val >= 0 {
        int_val as Asn1SccUint
    } else {
        int_val.wrapping_neg() as Asn1SccUint
    };
    let (digits, n_chars) = get_integer_digits(abs_int_val);
    // encode length (plus 1 for sign)
    p_bit_strm.append_byte0(n_chars + 1);
    // encode sign
    p_bit_strm.append_byte0(if int_val >= 0 { b'+' } else { b'-' });
    // encode digits
    let mut i = 0;
    while i < 100 && digits[i] != 0 {
        p_bit_strm.append_byte0(digits[i]);
        i += 1;
    }
}

/// Encode an unsigned integer as ASCII with variable size, length-embedded.
/// Mirrors C `Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded`.
pub fn acn_enc_uint_ascii_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
) {
    let (digits, n_chars) = get_integer_digits(int_val);
    p_bit_strm.append_byte0(n_chars);
    let mut i = 0;
    while i < 100 && digits[i] != 0 {
        p_bit_strm.append_byte0(digits[i]);
        i += 1;
    }
}

/// Decode an unsigned integer from ASCII with variable size, length-embedded.
/// Mirrors C `Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded`.
pub fn acn_dec_uint_ascii_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccUint, bool) {
    let (n_chars, ok) = p_bit_strm.read_byte();
    if !ok {
        return (0, false);
    }
    acn_dec_uint_ascii_const_size(p_bit_strm, n_chars as i32)
}

/// Decode a signed integer from ASCII with variable size, length-embedded.
/// Mirrors C `Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded`.
pub fn acn_dec_sint_ascii_var_size_length_embedded(
    p_bit_strm: &mut BitStream,
) -> (Asn1SccSint, bool) {
    let (n_chars, ok) = p_bit_strm.read_byte();
    if !ok {
        return (0, false);
    }
    acn_dec_sint_ascii_const_size(p_bit_strm, n_chars as i32)
}

// ─────────────────────────────────────────────────────────────────────────
//  ASCII integer encoding — var-size null-terminated
// ─────────────────────────────────────────────────────────────────────────

/// Encode an unsigned integer as ASCII, null-terminated.
/// Mirrors C `Acn_Enc_UInt_ASCII_VarSize_NullTerminated`.
pub fn acn_enc_uint_ascii_var_size_null_terminated(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccUint,
    null_characters: &[u8],
) {
    let (digits, _n_chars) = get_integer_digits(int_val);
    let mut i = 0;
    while i < 100 && digits[i] != 0 {
        p_bit_strm.append_byte0(digits[i]);
        i += 1;
    }
    for &nc in null_characters {
        p_bit_strm.append_byte0(nc);
    }
}

/// Encode a signed integer as ASCII, null-terminated.
/// Mirrors C `Acn_Enc_SInt_ASCII_VarSize_NullTerminated`.
pub fn acn_enc_sint_ascii_var_size_null_terminated(
    p_bit_strm: &mut BitStream,
    int_val: Asn1SccSint,
    null_characters: &[u8],
) {
    let abs_value = if int_val >= 0 {
        int_val as Asn1SccUint
    } else {
        int_val.wrapping_neg() as Asn1SccUint
    };
    p_bit_strm.append_byte0(if int_val >= 0 { b'+' } else { b'-' });
    acn_enc_uint_ascii_var_size_null_terminated(p_bit_strm, abs_value, null_characters);
}

/// Decode an unsigned integer from ASCII, null-terminated.
/// Mirrors C `Acn_Dec_UInt_ASCII_VarSize_NullTerminated`.
pub fn acn_dec_uint_ascii_var_size_null_terminated(
    p_bit_strm: &mut BitStream,
    null_characters: &[u8],
) -> (Asn1SccUint, bool) {
    let mut ret: Asn1SccUint = 0;
    let sz = null_characters.len().min(10);
    let mut tmp = [0u8; 10];
    // Read null_character_size characters into the tmp buffer
    for j in 0..sz {
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return (0, false);
        }
        tmp[j] = b;
    }
    while &null_characters[..sz] != &tmp[..sz] {
        let digit = tmp[0];
        // shift left
        for j in 0..sz - 1 {
            tmp[j] = tmp[j + 1];
        }
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return (0, false);
        }
        tmp[sz - 1] = b;
        let d = (digit.wrapping_sub(b'0')) as Asn1SccUint;
        ret *= 10;
        ret += d;
    }
    (ret, true)
}

/// Decode a signed integer from ASCII, null-terminated.
/// Mirrors C `Acn_Dec_SInt_ASCII_VarSize_NullTerminated`.
pub fn acn_dec_sint_ascii_var_size_null_terminated(
    p_bit_strm: &mut BitStream,
    null_characters: &[u8],
) -> (Asn1SccSint, bool) {
    let (digit, ok) = p_bit_strm.read_byte();
    if !ok {
        return (0, false);
    }
    if digit != b'-' && digit != b'+' {
        return (0, false);
    }
    let is_negative = digit == b'-';
    let (ret, ok) = acn_dec_uint_ascii_var_size_null_terminated(p_bit_strm, null_characters);
    if !ok {
        return (0, false);
    }
    let mut result = ret as Asn1SccSint;
    if is_negative {
        result = result.wrapping_neg();
    }
    (result, true)
}

// ─────────────────────────────────────────────────────────────────────────
//  Boolean pattern decode
// ─────────────────────────────────────────────────────────────────────────

/// Read `n_bits_to_read` bits and compare them to `bit_pattern`.
/// Returns `(bool_value, success)` where `bool_value` is `true` if the bits
/// match the pattern.  Mirrors C `BitStream_ReadBitPattern`.
pub fn bit_stream_read_bit_pattern(
    p_bit_strm: &mut BitStream,
    bit_pattern: &[u8],
    n_bits_to_read: i32,
) -> (bool, bool) {
    let n_bytes_to_read = n_bits_to_read / 8;
    let n_remaining_bits = n_bits_to_read % 8;
    let mut bool_value = true;
    for i in 0..n_bytes_to_read {
        let (cur_byte, ok) = p_bit_strm.read_byte();
        if !ok {
            return (false, false);
        }
        bool_value = bool_value && (cur_byte == bit_pattern[i as usize]);
    }
    if n_remaining_bits > 0 {
        let (cur_byte, ok) = p_bit_strm.read_partial_byte(n_remaining_bits as u8);
        if !ok {
            return (false, false);
        }
        bool_value = bool_value
            && (cur_byte == (bit_pattern[n_bytes_to_read as usize] >> (8 - n_remaining_bits)));
    }
    (bool_value, true)
}

/// Read `n_bits_to_read` bits and ignore them.
/// Mirrors C `BitStream_ReadBitPattern_ignore_value`.
pub fn bit_stream_read_bit_pattern_ignore_value(
    p_bit_strm: &mut BitStream,
    n_bits_to_read: i32,
) -> bool {
    let n_bytes_to_read = n_bits_to_read / 8;
    let n_remaining_bits = n_bits_to_read % 8;
    for _ in 0..n_bytes_to_read {
        let (_cur_byte, ok) = p_bit_strm.read_byte();
        if !ok {
            return false;
        }
    }
    if n_remaining_bits > 0 {
        let (_cur_byte, ok) = p_bit_strm.read_partial_byte(n_remaining_bits as u8);
        if !ok {
            return false;
        }
    }
    true
}

/// Decode a boolean value using true/false bit patterns.
/// Mirrors C `BitStream_DecodeTrueFalseBoolean`.
pub fn bit_stream_decode_true_false_boolean(
    p_bit_strm: &mut BitStream,
    true_pattern: &[u8],
    false_pattern: &[u8],
    n_bits_to_read: i32,
) -> (bool, bool) {
    let n_bytes_to_read = n_bits_to_read / 8;
    let n_remaining_bits = n_bits_to_read % 8;
    let mut is_true = true;
    let mut is_false = true;
    for i in 0..n_bytes_to_read {
        let (cur_byte, ok) = p_bit_strm.read_byte();
        if !ok {
            return (false, false);
        }
        is_true = is_true && (cur_byte == true_pattern[i as usize]);
        is_false = is_false && (cur_byte == false_pattern[i as usize]);
    }
    if n_remaining_bits > 0 {
        let (cur_byte, ok) = p_bit_strm.read_partial_byte(n_remaining_bits as u8);
        if !ok {
            return (false, false);
        }
        is_true = is_true
            && (cur_byte == (true_pattern[n_bytes_to_read as usize] >> (8 - n_remaining_bits)));
        is_false = is_false
            && (cur_byte == (false_pattern[n_bytes_to_read as usize] >> (8 - n_remaining_bits)));
    }
    if is_true {
        (true, true)
    } else if is_false {
        (false, true)
    } else {
        (false, false)
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  IEEE 754 Real encoding
// ─────────────────────────────────────────────────────────────────────────

/// Encode a 32-bit float in big-endian byte order.
/// Mirrors C `Acn_Enc_Real_IEEE754_32_big_endian`.
pub fn acn_enc_real_ieee754_32_big_endian(p_bit_strm: &mut BitStream, real_value: Asn1Real) {
    let bytes = (real_value as f32).to_ne_bytes();
    if !requires_reverse() {
        // native big-endian → write as-is
        for &b in &bytes {
            p_bit_strm.append_byte0(b);
        }
    } else {
        // native little-endian → reverse for big-endian output
        for &b in bytes.iter().rev() {
            p_bit_strm.append_byte0(b);
        }
    }
}

/// Encode a 64-bit double in big-endian byte order.
/// Mirrors C `Acn_Enc_Real_IEEE754_64_big_endian`.
pub fn acn_enc_real_ieee754_64_big_endian(p_bit_strm: &mut BitStream, real_value: Asn1Real) {
    let bytes = real_value.to_ne_bytes();
    if !requires_reverse() {
        for &b in &bytes {
            p_bit_strm.append_byte0(b);
        }
    } else {
        for &b in bytes.iter().rev() {
            p_bit_strm.append_byte0(b);
        }
    }
}

/// Encode a 32-bit float in little-endian byte order.
/// Mirrors C `Acn_Enc_Real_IEEE754_32_little_endian`.
pub fn acn_enc_real_ieee754_32_little_endian(p_bit_strm: &mut BitStream, real_value: Asn1Real) {
    let bytes = (real_value as f32).to_ne_bytes();
    if requires_reverse() {
        for &b in &bytes {
            p_bit_strm.append_byte0(b);
        }
    } else {
        for &b in bytes.iter().rev() {
            p_bit_strm.append_byte0(b);
        }
    }
}

/// Encode a 64-bit double in little-endian byte order.
/// Mirrors C `Acn_Enc_Real_IEEE754_64_little_endian`.
pub fn acn_enc_real_ieee754_64_little_endian(p_bit_strm: &mut BitStream, real_value: Asn1Real) {
    let bytes = real_value.to_ne_bytes();
    if requires_reverse() {
        for &b in &bytes {
            p_bit_strm.append_byte0(b);
        }
    } else {
        for &b in bytes.iter().rev() {
            p_bit_strm.append_byte0(b);
        }
    }
}

/// Decode a 32-bit float from big-endian bytes.
/// Mirrors C `Acn_Dec_Real_IEEE754_32_big_endian`.
pub fn acn_dec_real_ieee754_32_big_endian(
    p_bit_strm: &mut BitStream,
) -> (Asn1Real, bool) {
    let mut bytes = [0u8; 4];
    if !requires_reverse() {
        for i in 0..4 {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    } else {
        for i in (0..4).rev() {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    }
    (f32::from_ne_bytes(bytes) as Asn1Real, true)
}

/// Decode a 32-bit float from big-endian bytes (fp32 return type).
/// Mirrors C `Acn_Dec_Real_IEEE754_32_big_endian_fp32`.
pub fn acn_dec_real_ieee754_32_big_endian_fp32(
    p_bit_strm: &mut BitStream,
) -> (Asn1Real32, bool) {
    let mut bytes = [0u8; 4];
    if !requires_reverse() {
        for i in 0..4 {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    } else {
        for i in (0..4).rev() {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    }
    (f32::from_ne_bytes(bytes), true)
}

/// Decode a 64-bit double from big-endian bytes.
/// Mirrors C `Acn_Dec_Real_IEEE754_64_big_endian`.
pub fn acn_dec_real_ieee754_64_big_endian(
    p_bit_strm: &mut BitStream,
) -> (Asn1Real, bool) {
    let mut bytes = [0u8; 8];
    if !requires_reverse() {
        for i in 0..8 {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    } else {
        for i in (0..8).rev() {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    }
    (f64::from_ne_bytes(bytes), true)
}

/// Decode a 32-bit float from little-endian bytes.
/// Mirrors C `Acn_Dec_Real_IEEE754_32_little_endian`.
pub fn acn_dec_real_ieee754_32_little_endian(
    p_bit_strm: &mut BitStream,
) -> (Asn1Real, bool) {
    let mut bytes = [0u8; 4];
    if requires_reverse() {
        for i in 0..4 {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    } else {
        for i in (0..4).rev() {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    }
    (f32::from_ne_bytes(bytes) as Asn1Real, true)
}

/// Decode a 32-bit float from little-endian bytes (fp32 return type).
/// Mirrors C `Acn_Dec_Real_IEEE754_32_little_endian_fp32`.
pub fn acn_dec_real_ieee754_32_little_endian_fp32(
    p_bit_strm: &mut BitStream,
) -> (Asn1Real32, bool) {
    let mut bytes = [0u8; 4];
    if requires_reverse() {
        for i in 0..4 {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    } else {
        for i in (0..4).rev() {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    }
    (f32::from_ne_bytes(bytes), true)
}

/// Decode a 64-bit double from little-endian bytes.
/// Mirrors C `Acn_Dec_Real_IEEE754_64_little_endian`.
pub fn acn_dec_real_ieee754_64_little_endian(
    p_bit_strm: &mut BitStream,
) -> (Asn1Real, bool) {
    let mut bytes = [0u8; 8];
    if requires_reverse() {
        for i in 0..8 {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    } else {
        for i in (0..8).rev() {
            let (b, ok) = p_bit_strm.read_byte();
            if !ok {
                return (0.0, false);
            }
            bytes[i] = b;
        }
    }
    (f64::from_ne_bytes(bytes), true)
}

// ─────────────────────────────────────────────────────────────────────────
//  Scaled real encoding
// ─────────────────────────────────────────────────────────────────────────

/// Convert a real value to a scaled unsigned integer.
/// Mirrors C `Acn_Real2ScaledUInt`.
pub fn acn_real_2_scaled_uint(
    real_value: Asn1Real,
    low: Asn1Real,
    scale: Asn1Real,
    uint_max: Asn1SccUint,
) -> Asn1SccUint {
    let x = (real_value - low) / scale;
    if !(x > 0.0) {
        return 0;
    }
    let x = x + 0.5;
    if x >= uint_max as Asn1Real {
        return uint_max;
    }
    x as Asn1SccUint
}

/// Convert a real value to a scaled signed integer.
/// Mirrors C `Acn_Real2ScaledSInt`.
pub fn acn_real_2_scaled_sint(
    real_value: Asn1Real,
    low: Asn1Real,
    scale: Asn1Real,
    int_min: Asn1SccSint,
    int_max: Asn1SccSint,
) -> Asn1SccSint {
    let range = int2uint(int_max).wrapping_sub(int2uint(int_min));
    let delta = acn_real_2_scaled_uint(real_value, low, scale, range);
    uint2int(int2uint(int_min).wrapping_add(delta), WORD_SIZE)
}

/// Convert a scaled unsigned integer back to a real value.
/// Mirrors C `Acn_ScaledUInt2Real`.
pub fn acn_scaled_uint_2_real(
    int_val: Asn1SccUint,
    low: Asn1Real,
    scale: Asn1Real,
) -> Asn1Real {
    low + int_val as Asn1Real * scale
}

/// Convert a scaled signed integer back to a real value.
/// Mirrors C `Acn_ScaledSInt2Real`.
pub fn acn_scaled_sint_2_real(
    int_val: Asn1SccSint,
    low: Asn1Real,
    scale: Asn1Real,
    int_min: Asn1SccSint,
) -> Asn1Real {
    let delta = int2uint(int_val).wrapping_sub(int2uint(int_min));
    low + delta as Asn1Real * scale
}

// ─────────────────────────────────────────────────────────────────────────
//  String encoding — ASCII
// ─────────────────────────────────────────────────────────────────────────

/// Encode an ASCII string with a fixed size.
/// Mirrors C `Acn_Enc_String_Ascii_FixSize`.
pub fn acn_enc_string_ascii_fix_size(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    str_val: &[u8],
) {
    let mut i: Asn1SccSint = 0;
    while i < max {
        p_bit_strm.append_byte(str_val[i as usize], false);
        i += 1;
    }
}

/// Helper: encode ASCII string characters up to `max` or NUL.
/// Mirrors C `Acn_Enc_String_Ascii_private`.
fn acn_enc_string_ascii_private(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    str_val: &[u8],
) -> Asn1SccSint {
    let mut i: Asn1SccSint = 0;
    while i < max && str_val[i as usize] != 0 {
        p_bit_strm.append_byte(str_val[i as usize], false);
        i += 1;
    }
    i
}

/// Encode an ASCII string, null-terminated (single null character).
/// Mirrors C `Acn_Enc_String_Ascii_Null_Terminated`.
pub fn acn_enc_string_ascii_null_terminated(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    null_character: u8,
    str_val: &[u8],
) {
    acn_enc_string_ascii_private(p_bit_strm, max, str_val);
    p_bit_strm.append_byte(null_character, false);
}

/// Encode an ASCII string, null-terminated (multiple null characters).
/// Mirrors C `Acn_Enc_String_Ascii_Null_Terminated_mult`.
pub fn acn_enc_string_ascii_null_terminated_mult(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    null_character: &[u8],
    str_val: &[u8],
) {
    acn_enc_string_ascii_private(p_bit_strm, max, str_val);
    for &nc in null_character {
        p_bit_strm.append_byte(nc, false);
    }
}

/// Encode an ASCII string with an external field size determinant.
/// Mirrors C `Acn_Enc_String_Ascii_External_Field_Determinant`.
pub fn acn_enc_string_ascii_external_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    str_val: &[u8],
) {
    acn_enc_string_ascii_private(p_bit_strm, max, str_val);
}

/// Encode an ASCII string with an internal field size determinant.
/// Mirrors C `Acn_Enc_String_Ascii_Internal_Field_Determinant`.
pub fn acn_enc_string_ascii_internal_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    min: Asn1SccSint,
    str_val: &[u8],
) {
    let str_len = str_val.iter().position(|&c| c == 0).unwrap_or(str_val.len()) as Asn1SccSint;
    let len_to_encode = if str_len <= max { str_len } else { max };
    p_bit_strm.encode_constraint_whole_number(len_to_encode, min, max);
    acn_enc_string_ascii_private(p_bit_strm, max, str_val);
}

// ─────────────────────────────────────────────────────────────────────────
//  String encoding — CharIndex
// ─────────────────────────────────────────────────────────────────────────

/// Encode a string using character indexing with a fixed size.
/// Mirrors C `Acn_Enc_String_CharIndex_FixSize`.
pub fn acn_enc_string_char_index_fix_size(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    allowed_char_set: &[u8],
    str_val: &[u8],
) {
    let char_set_size = allowed_char_set.len() as Asn1SccSint;
    let mut i: Asn1SccSint = 0;
    while i < max {
        let char_index = get_char_index(str_val[i as usize] as char, allowed_char_set);
        p_bit_strm.encode_constraint_whole_number(char_index as Asn1SccSint, 0, char_set_size - 1);
        i += 1;
    }
}

/// Helper: encode a string using character indexing up to `max` or NUL.
/// Mirrors C `Acn_Enc_String_CharIndex_private`.
fn acn_enc_string_char_index_private(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    allowed_char_set: &[u8],
    str_val: &[u8],
) -> Asn1SccSint {
    let char_set_size = allowed_char_set.len() as Asn1SccSint;
    let mut i: Asn1SccSint = 0;
    while i < max && str_val[i as usize] != 0 {
        let char_index = get_char_index(str_val[i as usize] as char, allowed_char_set);
        p_bit_strm.encode_constraint_whole_number(char_index as Asn1SccSint, 0, char_set_size - 1);
        i += 1;
    }
    i
}

/// Encode a string using character indexing with an external field determinant.
/// Mirrors C `Acn_Enc_String_CharIndex_External_Field_Determinant`.
pub fn acn_enc_string_char_index_external_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    allowed_char_set: &[u8],
    str_val: &[u8],
) {
    acn_enc_string_char_index_private(p_bit_strm, max, allowed_char_set, str_val);
}

/// Encode a string using character indexing with an internal field determinant.
/// Mirrors C `Acn_Enc_String_CharIndex_Internal_Field_Determinant`.
pub fn acn_enc_string_char_index_internal_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    allowed_char_set: &[u8],
    min: Asn1SccSint,
    str_val: &[u8],
) {
    let str_len = str_val.iter().position(|&c| c == 0).unwrap_or(str_val.len()) as Asn1SccSint;
    let len_to_encode = if str_len <= max { str_len } else { max };
    p_bit_strm.encode_constraint_whole_number(len_to_encode, min, max);
    acn_enc_string_char_index_private(p_bit_strm, max, allowed_char_set, str_val);
}

// ─────────────────────────────────────────────────────────────────────────
//  String encoding — IA5String CharIndex
// ─────────────────────────────────────────────────────────────────────────

/// The full IA5 (ASCII 0x00–0x7F) character set, 128 entries.
const IA5_CHAR_SET: [u8; 128] = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
    0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
];

/// Encode an IA5String using character indexing with an external field determinant.
/// Mirrors C `Acn_Enc_IA5String_CharIndex_External_Field_Determinant`.
pub fn acn_enc_ia5string_char_index_external_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    str_val: &[u8],
) {
    acn_enc_string_char_index_private(p_bit_strm, max, &IA5_CHAR_SET, str_val);
}

/// Encode an IA5String using character indexing with an internal field determinant.
/// Mirrors C `Acn_Enc_IA5String_CharIndex_Internal_Field_Determinant`.
pub fn acn_enc_ia5string_char_index_internal_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    min: Asn1SccSint,
    str_val: &[u8],
) {
    let str_len = str_val.iter().position(|&c| c == 0).unwrap_or(str_val.len()) as Asn1SccSint;
    let len_to_encode = if str_len <= max { str_len } else { max };
    p_bit_strm.encode_constraint_whole_number(len_to_encode, min, max);
    acn_enc_string_char_index_private(p_bit_strm, max, &IA5_CHAR_SET, str_val);
}

// ─────────────────────────────────────────────────────────────────────────
//  String decoding — ASCII
// ─────────────────────────────────────────────────────────────────────────

/// Helper: decode `characters_to_decode` ASCII characters into `str_val`.
/// Mirrors C `Acn_Dec_String_Ascii_private`.
/// Returns `true` on success.  `str_val` is zero-filled first.
fn acn_dec_string_ascii_private(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    characters_to_decode: Asn1SccSint,
    str_val: &mut [u8],
) -> bool {
    // Zero-fill up to max+1
    let fill_len = (max as usize + 1).min(str_val.len());
    for j in 0..fill_len {
        str_val[j] = 0;
    }
    let mut i: Asn1SccSint = 0;
    while i < characters_to_decode {
        let (decoded_char, ok) = p_bit_strm.read_byte();
        if !ok {
            return false;
        }
        str_val[i as usize] = decoded_char;
        i += 1;
    }
    true
}

/// Decode an ASCII string with a fixed size.
/// Mirrors C `Acn_Dec_String_Ascii_FixSize`.
pub fn acn_dec_string_ascii_fix_size(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    str_val: &mut [u8],
) -> bool {
    acn_dec_string_ascii_private(p_bit_strm, max, max, str_val)
}

/// Decode an ASCII string, null-terminated (single null character).
/// Mirrors C `Acn_Dec_String_Ascii_Null_Terminated`.
pub fn acn_dec_string_ascii_null_terminated(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    null_character: u8,
    str_val: &mut [u8],
) -> bool {
    let fill_len = (max as usize + 1).min(str_val.len());
    for j in 0..fill_len {
        str_val[j] = 0;
    }
    let mut i: Asn1SccSint = 0;
    while i <= max {
        let (decoded_char, ok) = p_bit_strm.read_byte();
        if !ok {
            return false;
        }
        if decoded_char != null_character {
            str_val[i as usize] = decoded_char;
            i += 1;
        } else {
            str_val[i as usize] = 0;
            return true;
        }
    }
    false
}

/// Decode an ASCII string, null-terminated (multiple null characters).
/// Mirrors C `Acn_Dec_String_Ascii_Null_Terminated_mult`.
pub fn acn_dec_string_ascii_null_terminated_mult(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    null_character: &[u8],
    str_val: &mut [u8],
) -> bool {
    let sz = null_character.len().min(10);
    let mut tmp = [0u8; 10];
    let fill_len = (max as usize + 1).min(str_val.len());
    for j in 0..fill_len {
        str_val[j] = 0;
    }
    for j in 0..sz {
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return false;
        }
        tmp[j] = b;
    }
    let mut i: Asn1SccSint = 0;
    while i <= max && &null_character[..sz] != &tmp[..sz] {
        str_val[i as usize] = tmp[0];
        i += 1;
        for j in 0..sz - 1 {
            tmp[j] = tmp[j + 1];
        }
        let (b, ok) = p_bit_strm.read_byte();
        if !ok {
            return false;
        }
        tmp[sz - 1] = b;
    }
    str_val[i as usize] = 0;
    &null_character[..sz] == &tmp[..sz]
}

/// Decode an ASCII string with an external field size determinant.
/// Mirrors C `Acn_Dec_String_Ascii_External_Field_Determinant`.
pub fn acn_dec_string_ascii_external_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    ext_size_determinant_fld: Asn1SccSint,
    str_val: &mut [u8],
) -> bool {
    let chars = if ext_size_determinant_fld <= max {
        ext_size_determinant_fld
    } else {
        max
    };
    acn_dec_string_ascii_private(p_bit_strm, max, chars, str_val)
}

/// Decode an ASCII string with an internal field size determinant.
/// Mirrors C `Acn_Dec_String_Ascii_Internal_Field_Determinant`.
pub fn acn_dec_string_ascii_internal_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    min: Asn1SccSint,
    str_val: &mut [u8],
) -> bool {
    let (n_count, ok) = p_bit_strm.decode_constraint_whole_number(min, max);
    if !ok {
        return false;
    }
    let chars = if n_count <= max { n_count } else { max };
    acn_dec_string_ascii_private(p_bit_strm, max, chars, str_val)
}

// ─────────────────────────────────────────────────────────────────────────
//  String decoding — CharIndex
// ─────────────────────────────────────────────────────────────────────────

/// Helper: decode `characters_to_decode` characters using CharIndex.
/// Mirrors C `Acn_Dec_String_CharIndex_private`.
fn acn_dec_string_char_index_private(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    characters_to_decode: Asn1SccSint,
    allowed_char_set: &[u8],
    str_val: &mut [u8],
) -> bool {
    let char_set_size = allowed_char_set.len() as Asn1SccSint;
    let fill_len = (max as usize + 1).min(str_val.len());
    for j in 0..fill_len {
        str_val[j] = 0;
    }
    let mut i: Asn1SccSint = 0;
    while i < characters_to_decode {
        let (char_index, ok) =
            p_bit_strm.decode_constraint_whole_number(0, char_set_size - 1);
        if !ok {
            return false;
        }
        if char_index < 0 || char_index >= char_set_size {
            return false;
        }
        str_val[i as usize] = allowed_char_set[char_index as usize];
        i += 1;
    }
    true
}

/// Decode a string using character indexing with a fixed size.
/// Mirrors C `Acn_Dec_String_CharIndex_FixSize`.
pub fn acn_dec_string_char_index_fix_size(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    allowed_char_set: &[u8],
    str_val: &mut [u8],
) -> bool {
    acn_dec_string_char_index_private(p_bit_strm, max, max, allowed_char_set, str_val)
}

/// Decode a string using character indexing with an external field determinant.
/// Mirrors C `Acn_Dec_String_CharIndex_External_Field_Determinant`.
pub fn acn_dec_string_char_index_external_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    allowed_char_set: &[u8],
    ext_size_determinant_fld: Asn1SccSint,
    str_val: &mut [u8],
) -> bool {
    let chars = if ext_size_determinant_fld <= max {
        ext_size_determinant_fld
    } else {
        max
    };
    acn_dec_string_char_index_private(p_bit_strm, max, chars, allowed_char_set, str_val)
}

/// Decode a string using character indexing with an internal field determinant.
/// Mirrors C `Acn_Dec_String_CharIndex_Internal_Field_Determinant`.
pub fn acn_dec_string_char_index_internal_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    allowed_char_set: &[u8],
    min: Asn1SccSint,
    str_val: &mut [u8],
) -> bool {
    let (n_count, ok) = p_bit_strm.decode_constraint_whole_number(min, max);
    if !ok {
        return false;
    }
    let chars = if n_count <= max { n_count } else { max };
    acn_dec_string_char_index_private(p_bit_strm, max, chars, allowed_char_set, str_val)
}

// ─────────────────────────────────────────────────────────────────────────
//  String decoding — IA5String CharIndex
// ─────────────────────────────────────────────────────────────────────────

/// Decode an IA5String using character indexing with an external field determinant.
/// Mirrors C `Acn_Dec_IA5String_CharIndex_External_Field_Determinant`.
pub fn acn_dec_ia5string_char_index_external_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    ext_size_determinant_fld: Asn1SccSint,
    str_val: &mut [u8],
) -> bool {
    let chars = if ext_size_determinant_fld <= max {
        ext_size_determinant_fld
    } else {
        max
    };
    acn_dec_string_char_index_private(p_bit_strm, max, chars, &IA5_CHAR_SET, str_val)
}

/// Decode an IA5String using character indexing with an internal field determinant.
/// Mirrors C `Acn_Dec_IA5String_CharIndex_Internal_Field_Determinant`.
pub fn acn_dec_ia5string_char_index_internal_field_determinant(
    p_bit_strm: &mut BitStream,
    max: Asn1SccSint,
    min: Asn1SccSint,
    str_val: &mut [u8],
) -> bool {
    let (n_count, ok) = p_bit_strm.decode_constraint_whole_number(min, max);
    if !ok {
        return false;
    }
    let chars = if n_count <= max { n_count } else { max };
    acn_dec_string_char_index_private(p_bit_strm, max, chars, &IA5_CHAR_SET, str_val)
}

// ─────────────────────────────────────────────────────────────────────────
//  Length determinant functions
// ─────────────────────────────────────────────────────────────────────────

/// Encode a length determinant.
/// Mirrors C `Acn_Enc_Length`.
pub fn acn_enc_length(
    p_bit_strm: &mut BitStream,
    length_value: Asn1SccUint,
    length_size_in_bits: i32,
) {
    acn_enc_int_positive_integer_const_size(p_bit_strm, length_value, length_size_in_bits);
}

/// Decode a length determinant.
/// Mirrors C `Acn_Dec_Length`.
pub fn acn_dec_length(
    p_bit_strm: &mut BitStream,
    length_size_in_bits: i32,
) -> (Asn1SccUint, bool) {
    acn_dec_int_positive_integer_const_size(p_bit_strm, length_size_in_bits)
}

// ─────────────────────────────────────────────────────────────────────────
//  MILBUS
// ─────────────────────────────────────────────────────────────────────────

/// MILBUS encode: value 32 encodes as 0.
/// Mirrors C `milbus_encode`.
pub fn milbus_encode(val: Asn1SccSint) -> Asn1SccSint {
    if val == 32 { 0 } else { val }
}

/// MILBUS decode: value 0 decodes as 32.
/// Mirrors C `milbus_decode`.
pub fn milbus_decode(val: Asn1SccSint) -> Asn1SccSint {
    if val == 0 { 32 } else { val }
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger ConstSize
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from a constant-size bit field into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSizeUInt8`.
pub fn acn_dec_int_positive_integer_const_size_u8(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bits: i32,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size(p_bit_strm, encoded_size_in_bits);
    (v as u8, ok)
}

/// Decode a positive integer from a constant-size bit field into a `u16`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSizeUInt16`.
pub fn acn_dec_int_positive_integer_const_size_u16(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bits: i32,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size(p_bit_strm, encoded_size_in_bits);
    (v as u16, ok)
}

/// Decode a positive integer from a constant-size bit field into a `u32`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSizeUInt32`.
pub fn acn_dec_int_positive_integer_const_size_u32(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bits: i32,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size(p_bit_strm, encoded_size_in_bits);
    (v as u32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger ConstSize_8
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from 8 bits into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_8UInt8`.
pub fn acn_dec_int_positive_integer_const_size_8_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_8(p_bit_strm);
    (v as u8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger big_endian_16
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from big-endian 16-bit into a `u16`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt16`.
pub fn acn_dec_int_positive_integer_const_size_big_endian_16_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_big_endian_16(p_bit_strm);
    (v as u16, ok)
}

/// Decode a positive integer from big-endian 16-bit into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8`.
pub fn acn_dec_int_positive_integer_const_size_big_endian_16_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_big_endian_16(p_bit_strm);
    (v as u8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger big_endian_32
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from big-endian 32-bit into a `u32`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt32`.
pub fn acn_dec_int_positive_integer_const_size_big_endian_32_u32(
    p_bit_strm: &mut BitStream,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_big_endian_32(p_bit_strm);
    (v as u32, ok)
}

/// Decode a positive integer from big-endian 32-bit into a `u16`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt16`.
pub fn acn_dec_int_positive_integer_const_size_big_endian_32_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_big_endian_32(p_bit_strm);
    (v as u16, ok)
}

/// Decode a positive integer from big-endian 32-bit into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt8`.
pub fn acn_dec_int_positive_integer_const_size_big_endian_32_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_big_endian_32(p_bit_strm);
    (v as u8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger big_endian_64
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from big-endian 64-bit into a `u32`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt32`.
pub fn acn_dec_int_positive_integer_const_size_big_endian_64_u32(
    p_bit_strm: &mut BitStream,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_big_endian_64(p_bit_strm);
    (v as u32, ok)
}

/// Decode a positive integer from big-endian 64-bit into a `u16`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt16`.
pub fn acn_dec_int_positive_integer_const_size_big_endian_64_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_big_endian_64(p_bit_strm);
    (v as u16, ok)
}

/// Decode a positive integer from big-endian 64-bit into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt8`.
pub fn acn_dec_int_positive_integer_const_size_big_endian_64_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_big_endian_64(p_bit_strm);
    (v as u8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger little_endian_16
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from little-endian 16-bit into a `u16`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt16`.
pub fn acn_dec_int_positive_integer_const_size_little_endian_16_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_little_endian_16(p_bit_strm);
    (v as u16, ok)
}

/// Decode a positive integer from little-endian 16-bit into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt8`.
pub fn acn_dec_int_positive_integer_const_size_little_endian_16_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_little_endian_16(p_bit_strm);
    (v as u8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger little_endian_32
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from little-endian 32-bit into a `u32`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt32`.
pub fn acn_dec_int_positive_integer_const_size_little_endian_32_u32(
    p_bit_strm: &mut BitStream,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_little_endian_32(p_bit_strm);
    (v as u32, ok)
}

/// Decode a positive integer from little-endian 32-bit into a `u16`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt16`.
pub fn acn_dec_int_positive_integer_const_size_little_endian_32_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_little_endian_32(p_bit_strm);
    (v as u16, ok)
}

/// Decode a positive integer from little-endian 32-bit into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt8`.
pub fn acn_dec_int_positive_integer_const_size_little_endian_32_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_little_endian_32(p_bit_strm);
    (v as u8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger little_endian_64
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer from little-endian 64-bit into a `u32`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt32`.
pub fn acn_dec_int_positive_integer_const_size_little_endian_64_u32(
    p_bit_strm: &mut BitStream,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_little_endian_64(p_bit_strm);
    (v as u32, ok)
}

/// Decode a positive integer from little-endian 64-bit into a `u16`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt16`.
pub fn acn_dec_int_positive_integer_const_size_little_endian_64_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_little_endian_64(p_bit_strm);
    (v as u16, ok)
}

/// Decode a positive integer from little-endian 64-bit into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt8`.
pub fn acn_dec_int_positive_integer_const_size_little_endian_64_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_const_size_little_endian_64(p_bit_strm);
    (v as u8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — PositiveInteger VarSize_LengthEmbedded
// ─────────────────────────────────────────────────────────────────────────

/// Decode a positive integer (var-size length-embedded) into a `u8`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt8`.
pub fn acn_dec_int_positive_integer_var_size_length_embedded_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_positive_integer_var_size_length_embedded(p_bit_strm);
    (v as u8, ok)
}

/// Decode a positive integer (var-size length-embedded) into a `u16`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt16`.
pub fn acn_dec_int_positive_integer_var_size_length_embedded_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_positive_integer_var_size_length_embedded(p_bit_strm);
    (v as u16, ok)
}

/// Decode a positive integer (var-size length-embedded) into a `u32`.
/// Mirrors C `Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt32`.
pub fn acn_dec_int_positive_integer_var_size_length_embedded_u32(
    p_bit_strm: &mut BitStream,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_positive_integer_var_size_length_embedded(p_bit_strm);
    (v as u32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement ConstSize
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from a two's complement constant-size bit field into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSizeInt8`.
pub fn acn_dec_int_twos_complement_const_size_i8(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bits: i32,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size(p_bit_strm, encoded_size_in_bits);
    (v as i8, ok)
}

/// Decode a signed integer from a two's complement constant-size bit field into an `i16`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSizeInt16`.
pub fn acn_dec_int_twos_complement_const_size_i16(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bits: i32,
) -> (i16, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size(p_bit_strm, encoded_size_in_bits);
    (v as i16, ok)
}

/// Decode a signed integer from a two's complement constant-size bit field into an `i32`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSizeInt32`.
pub fn acn_dec_int_twos_complement_const_size_i32(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bits: i32,
) -> (i32, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size(p_bit_strm, encoded_size_in_bits);
    (v as i32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement ConstSize_8
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from 8 bits into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_8Int8`.
pub fn acn_dec_int_twos_complement_const_size_8_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_8(p_bit_strm);
    (v as i8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement big_endian_16
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from big-endian 16-bit into an `i16`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int16`.
pub fn acn_dec_int_twos_complement_const_size_big_endian_16_i16(
    p_bit_strm: &mut BitStream,
) -> (i16, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_big_endian_16(p_bit_strm);
    (v as i16, ok)
}

/// Decode a signed integer from big-endian 16-bit into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int8`.
pub fn acn_dec_int_twos_complement_const_size_big_endian_16_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_big_endian_16(p_bit_strm);
    (v as i8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement big_endian_32
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from big-endian 32-bit into an `i32`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int32`.
pub fn acn_dec_int_twos_complement_const_size_big_endian_32_i32(
    p_bit_strm: &mut BitStream,
) -> (i32, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_big_endian_32(p_bit_strm);
    (v as i32, ok)
}

/// Decode a signed integer from big-endian 32-bit into an `i16`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int16`.
pub fn acn_dec_int_twos_complement_const_size_big_endian_32_i16(
    p_bit_strm: &mut BitStream,
) -> (i16, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_big_endian_32(p_bit_strm);
    (v as i16, ok)
}

/// Decode a signed integer from big-endian 32-bit into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int8`.
pub fn acn_dec_int_twos_complement_const_size_big_endian_32_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_big_endian_32(p_bit_strm);
    (v as i8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement big_endian_64
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from big-endian 64-bit into an `i32`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int32`.
pub fn acn_dec_int_twos_complement_const_size_big_endian_64_i32(
    p_bit_strm: &mut BitStream,
) -> (i32, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_big_endian_64(p_bit_strm);
    (v as i32, ok)
}

/// Decode a signed integer from big-endian 64-bit into an `i16`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int16`.
pub fn acn_dec_int_twos_complement_const_size_big_endian_64_i16(
    p_bit_strm: &mut BitStream,
) -> (i16, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_big_endian_64(p_bit_strm);
    (v as i16, ok)
}

/// Decode a signed integer from big-endian 64-bit into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int8`.
pub fn acn_dec_int_twos_complement_const_size_big_endian_64_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_big_endian_64(p_bit_strm);
    (v as i8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement little_endian_16
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from little-endian 16-bit into an `i16`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int16`.
pub fn acn_dec_int_twos_complement_const_size_little_endian_16_i16(
    p_bit_strm: &mut BitStream,
) -> (i16, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_little_endian_16(p_bit_strm);
    (v as i16, ok)
}

/// Decode a signed integer from little-endian 16-bit into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int8`.
pub fn acn_dec_int_twos_complement_const_size_little_endian_16_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_little_endian_16(p_bit_strm);
    (v as i8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement little_endian_32
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from little-endian 32-bit into an `i32`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int32`.
pub fn acn_dec_int_twos_complement_const_size_little_endian_32_i32(
    p_bit_strm: &mut BitStream,
) -> (i32, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_little_endian_32(p_bit_strm);
    (v as i32, ok)
}

/// Decode a signed integer from little-endian 32-bit into an `i16`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int16`.
pub fn acn_dec_int_twos_complement_const_size_little_endian_32_i16(
    p_bit_strm: &mut BitStream,
) -> (i16, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_little_endian_32(p_bit_strm);
    (v as i16, ok)
}

/// Decode a signed integer from little-endian 32-bit into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int8`.
pub fn acn_dec_int_twos_complement_const_size_little_endian_32_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_little_endian_32(p_bit_strm);
    (v as i8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement little_endian_64
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer from little-endian 64-bit into an `i32`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int32`.
pub fn acn_dec_int_twos_complement_const_size_little_endian_64_i32(
    p_bit_strm: &mut BitStream,
) -> (i32, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_little_endian_64(p_bit_strm);
    (v as i32, ok)
}

/// Decode a signed integer from little-endian 64-bit into an `i16`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int16`.
pub fn acn_dec_int_twos_complement_const_size_little_endian_64_i16(
    p_bit_strm: &mut BitStream,
) -> (i16, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_little_endian_64(p_bit_strm);
    (v as i16, ok)
}

/// Decode a signed integer from little-endian 64-bit into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int8`.
pub fn acn_dec_int_twos_complement_const_size_little_endian_64_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_const_size_little_endian_64(p_bit_strm);
    (v as i8, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — TwosComplement VarSize_LengthEmbedded
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed integer (var-size length-embedded) into an `i8`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt8`.
pub fn acn_dec_int_twos_complement_var_size_length_embedded_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_int_twos_complement_var_size_length_embedded(p_bit_strm);
    (v as i8, ok)
}

/// Decode a signed integer (var-size length-embedded) into an `i16`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt16`.
pub fn acn_dec_int_twos_complement_var_size_length_embedded_i16(
    p_bit_strm: &mut BitStream,
) -> (i16, bool) {
    let (v, ok) = acn_dec_int_twos_complement_var_size_length_embedded(p_bit_strm);
    (v as i16, ok)
}

/// Decode a signed integer (var-size length-embedded) into an `i32`.
/// Mirrors C `Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt32`.
pub fn acn_dec_int_twos_complement_var_size_length_embedded_i32(
    p_bit_strm: &mut BitStream,
) -> (i32, bool) {
    let (v, ok) = acn_dec_int_twos_complement_var_size_length_embedded(p_bit_strm);
    (v as i32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — BCD ConstSize
// ─────────────────────────────────────────────────────────────────────────

/// Decode a BCD integer (const size) into a `u8`.
/// Mirrors C `Acn_Dec_Int_BCD_ConstSizeUInt8`.
pub fn acn_dec_int_bcd_const_size_u8(
    p_bit_strm: &mut BitStream,
    encoded_size_in_nibbles: i32,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_bcd_const_size(p_bit_strm, encoded_size_in_nibbles);
    (v as u8, ok)
}

/// Decode a BCD integer (const size) into a `u16`.
/// Mirrors C `Acn_Dec_Int_BCD_ConstSizeUInt16`.
pub fn acn_dec_int_bcd_const_size_u16(
    p_bit_strm: &mut BitStream,
    encoded_size_in_nibbles: i32,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_bcd_const_size(p_bit_strm, encoded_size_in_nibbles);
    (v as u16, ok)
}

/// Decode a BCD integer (const size) into a `u32`.
/// Mirrors C `Acn_Dec_Int_BCD_ConstSizeUInt32`.
pub fn acn_dec_int_bcd_const_size_u32(
    p_bit_strm: &mut BitStream,
    encoded_size_in_nibbles: i32,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_bcd_const_size(p_bit_strm, encoded_size_in_nibbles);
    (v as u32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — BCD VarSize_LengthEmbedded
// ─────────────────────────────────────────────────────────────────────────

/// Decode a BCD integer (var-size length-embedded) into a `u8`.
/// Mirrors C `Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt8`.
pub fn acn_dec_int_bcd_var_size_length_embedded_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_bcd_var_size_length_embedded(p_bit_strm);
    (v as u8, ok)
}

/// Decode a BCD integer (var-size length-embedded) into a `u16`.
/// Mirrors C `Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt16`.
pub fn acn_dec_int_bcd_var_size_length_embedded_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_bcd_var_size_length_embedded(p_bit_strm);
    (v as u16, ok)
}

/// Decode a BCD integer (var-size length-embedded) into a `u32`.
/// Mirrors C `Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt32`.
pub fn acn_dec_int_bcd_var_size_length_embedded_u32(
    p_bit_strm: &mut BitStream,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_bcd_var_size_length_embedded(p_bit_strm);
    (v as u32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — BCD VarSize_NullTerminated
// ─────────────────────────────────────────────────────────────────────────

/// Decode a BCD integer (var-size null-terminated) into a `u8`.
/// Mirrors C `Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt8`.
pub fn acn_dec_int_bcd_var_size_null_terminated_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_int_bcd_var_size_null_terminated(p_bit_strm);
    (v as u8, ok)
}

/// Decode a BCD integer (var-size null-terminated) into a `u16`.
/// Mirrors C `Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt16`.
pub fn acn_dec_int_bcd_var_size_null_terminated_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_int_bcd_var_size_null_terminated(p_bit_strm);
    (v as u16, ok)
}

/// Decode a BCD integer (var-size null-terminated) into a `u32`.
/// Mirrors C `Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt32`.
pub fn acn_dec_int_bcd_var_size_null_terminated_u32(
    p_bit_strm: &mut BitStream,
) -> (u32, bool) {
    let (v, ok) = acn_dec_int_bcd_var_size_null_terminated(p_bit_strm);
    (v as u32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — SInt ASCII ConstSize
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed ASCII integer (const size) into an `i8`.
/// Mirrors C `Acn_Dec_SInt_ASCII_ConstSizeInt8`.
pub fn acn_dec_sint_ascii_const_size_i8(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bytes: i32,
) -> (i8, bool) {
    let (v, ok) = acn_dec_sint_ascii_const_size(p_bit_strm, encoded_size_in_bytes);
    (v as i8, ok)
}

/// Decode a signed ASCII integer (const size) into an `i16`.
/// Mirrors C `Acn_Dec_SInt_ASCII_ConstSizeInt16`.
pub fn acn_dec_sint_ascii_const_size_i16(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bytes: i32,
) -> (i16, bool) {
    let (v, ok) = acn_dec_sint_ascii_const_size(p_bit_strm, encoded_size_in_bytes);
    (v as i16, ok)
}

/// Decode a signed ASCII integer (const size) into an `i32`.
/// Mirrors C `Acn_Dec_SInt_ASCII_ConstSizeInt32`.
pub fn acn_dec_sint_ascii_const_size_i32(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bytes: i32,
) -> (i32, bool) {
    let (v, ok) = acn_dec_sint_ascii_const_size(p_bit_strm, encoded_size_in_bytes);
    (v as i32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — SInt ASCII VarSize_LengthEmbedded
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed ASCII integer (var-size length-embedded) into an `i8`.
/// Mirrors C `Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt8`.
pub fn acn_dec_sint_ascii_var_size_length_embedded_i8(
    p_bit_strm: &mut BitStream,
) -> (i8, bool) {
    let (v, ok) = acn_dec_sint_ascii_var_size_length_embedded(p_bit_strm);
    (v as i8, ok)
}

/// Decode a signed ASCII integer (var-size length-embedded) into an `i16`.
/// Mirrors C `Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt16`.
pub fn acn_dec_sint_ascii_var_size_length_embedded_i16(
    p_bit_strm: &mut BitStream,
) -> (i16, bool) {
    let (v, ok) = acn_dec_sint_ascii_var_size_length_embedded(p_bit_strm);
    (v as i16, ok)
}

/// Decode a signed ASCII integer (var-size length-embedded) into an `i32`.
/// Mirrors C `Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt32`.
pub fn acn_dec_sint_ascii_var_size_length_embedded_i32(
    p_bit_strm: &mut BitStream,
) -> (i32, bool) {
    let (v, ok) = acn_dec_sint_ascii_var_size_length_embedded(p_bit_strm);
    (v as i32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — SInt ASCII VarSize_NullTerminated
// ─────────────────────────────────────────────────────────────────────────

/// Decode a signed ASCII integer (var-size null-terminated) into an `i8`.
/// Mirrors C `Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt8`.
pub fn acn_dec_sint_ascii_var_size_null_terminated_i8(
    p_bit_strm: &mut BitStream,
    null_characters: &[u8],
) -> (i8, bool) {
    let (v, ok) = acn_dec_sint_ascii_var_size_null_terminated(p_bit_strm, null_characters);
    (v as i8, ok)
}

/// Decode a signed ASCII integer (var-size null-terminated) into an `i16`.
/// Mirrors C `Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt16`.
pub fn acn_dec_sint_ascii_var_size_null_terminated_i16(
    p_bit_strm: &mut BitStream,
    null_characters: &[u8],
) -> (i16, bool) {
    let (v, ok) = acn_dec_sint_ascii_var_size_null_terminated(p_bit_strm, null_characters);
    (v as i16, ok)
}

/// Decode a signed ASCII integer (var-size null-terminated) into an `i32`.
/// Mirrors C `Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt32`.
pub fn acn_dec_sint_ascii_var_size_null_terminated_i32(
    p_bit_strm: &mut BitStream,
    null_characters: &[u8],
) -> (i32, bool) {
    let (v, ok) = acn_dec_sint_ascii_var_size_null_terminated(p_bit_strm, null_characters);
    (v as i32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — UInt ASCII ConstSize
// ─────────────────────────────────────────────────────────────────────────

/// Decode an unsigned ASCII integer (const size) into a `u8`.
/// Mirrors C `Acn_Dec_UInt_ASCII_ConstSizeUInt8`.
pub fn acn_dec_uint_ascii_const_size_u8(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bytes: i32,
) -> (u8, bool) {
    let (v, ok) = acn_dec_uint_ascii_const_size(p_bit_strm, encoded_size_in_bytes);
    (v as u8, ok)
}

/// Decode an unsigned ASCII integer (const size) into a `u16`.
/// Mirrors C `Acn_Dec_UInt_ASCII_ConstSizeUInt16`.
pub fn acn_dec_uint_ascii_const_size_u16(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bytes: i32,
) -> (u16, bool) {
    let (v, ok) = acn_dec_uint_ascii_const_size(p_bit_strm, encoded_size_in_bytes);
    (v as u16, ok)
}

/// Decode an unsigned ASCII integer (const size) into a `u32`.
/// Mirrors C `Acn_Dec_UInt_ASCII_ConstSizeUInt32`.
pub fn acn_dec_uint_ascii_const_size_u32(
    p_bit_strm: &mut BitStream,
    encoded_size_in_bytes: i32,
) -> (u32, bool) {
    let (v, ok) = acn_dec_uint_ascii_const_size(p_bit_strm, encoded_size_in_bytes);
    (v as u32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — UInt ASCII VarSize_LengthEmbedded
// ─────────────────────────────────────────────────────────────────────────

/// Decode an unsigned ASCII integer (var-size length-embedded) into a `u8`.
/// Mirrors C `Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt8`.
pub fn acn_dec_uint_ascii_var_size_length_embedded_u8(
    p_bit_strm: &mut BitStream,
) -> (u8, bool) {
    let (v, ok) = acn_dec_uint_ascii_var_size_length_embedded(p_bit_strm);
    (v as u8, ok)
}

/// Decode an unsigned ASCII integer (var-size length-embedded) into a `u16`.
/// Mirrors C `Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt16`.
pub fn acn_dec_uint_ascii_var_size_length_embedded_u16(
    p_bit_strm: &mut BitStream,
) -> (u16, bool) {
    let (v, ok) = acn_dec_uint_ascii_var_size_length_embedded(p_bit_strm);
    (v as u16, ok)
}

/// Decode an unsigned ASCII integer (var-size length-embedded) into a `u32`.
/// Mirrors C `Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt32`.
pub fn acn_dec_uint_ascii_var_size_length_embedded_u32(
    p_bit_strm: &mut BitStream,
) -> (u32, bool) {
    let (v, ok) = acn_dec_uint_ascii_var_size_length_embedded(p_bit_strm);
    (v as u32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  Typed decode variants — UInt ASCII VarSize_NullTerminated
// ─────────────────────────────────────────────────────────────────────────

/// Decode an unsigned ASCII integer (var-size null-terminated) into a `u8`.
/// Mirrors C `Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt8`.
pub fn acn_dec_uint_ascii_var_size_null_terminated_u8(
    p_bit_strm: &mut BitStream,
    null_characters: &[u8],
) -> (u8, bool) {
    let (v, ok) = acn_dec_uint_ascii_var_size_null_terminated(p_bit_strm, null_characters);
    (v as u8, ok)
}

/// Decode an unsigned ASCII integer (var-size null-terminated) into a `u16`.
/// Mirrors C `Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt16`.
pub fn acn_dec_uint_ascii_var_size_null_terminated_u16(
    p_bit_strm: &mut BitStream,
    null_characters: &[u8],
) -> (u16, bool) {
    let (v, ok) = acn_dec_uint_ascii_var_size_null_terminated(p_bit_strm, null_characters);
    (v as u16, ok)
}

/// Decode an unsigned ASCII integer (var-size null-terminated) into a `u32`.
/// Mirrors C `Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt32`.
pub fn acn_dec_uint_ascii_var_size_null_terminated_u32(
    p_bit_strm: &mut BitStream,
    null_characters: &[u8],
) -> (u32, bool) {
    let (v, ok) = acn_dec_uint_ascii_var_size_null_terminated(p_bit_strm, null_characters);
    (v as u32, ok)
}

// ─────────────────────────────────────────────────────────────────────────
//  ACN Deferred Patching — types and helpers
// ─────────────────────────────────────────────────────────────────────────

/// Position in a bit stream — used to remember where to patch later.
/// Mirrors C `AcnBitStreamPos`.
#[derive(Debug, Clone, Copy, Default)]
pub struct AcnBitStreamPos {
    pub current_byte: i64,
    pub current_bit: i32,
}

/// Reference to an ACN-inserted field that will be patched later.
/// `is_set` + `value` enable consistency checking for shared determinants.
/// Mirrors C `AcnInsertedFieldRef`.
#[derive(Debug, Clone)]
pub struct AcnInsertedFieldRef {
    /// Where in the bitstream the field was reserved.
    pub pos: AcnBitStreamPos,
    /// `true` after first `patch_det` call.
    pub is_set: bool,
    /// The value that was written (for consistency checking).
    pub value: Asn1SccUint,
    /// For IA5String determinants (consistency checking).
    pub str_value: [u8; 256],
    /// Number of valid bytes in `str_value`.
    pub str_value_len: usize,
}

impl Default for AcnInsertedFieldRef {
    fn default() -> Self {
        Self {
            pos: AcnBitStreamPos::default(),
            is_set: false,
            value: 0,
            str_value: [0u8; 256],
            str_value_len: 0,
        }
    }
}

impl AcnInsertedFieldRef {
    /// Create a fresh, unset reference.
    pub fn new() -> Self {
        Self::default()
    }
}

/// Get the current bit-stream position.
/// Mirrors C `Acn_BitStream_GetPos`.
pub fn acn_bit_stream_get_pos(bs: &BitStream) -> AcnBitStreamPos {
    AcnBitStreamPos {
        current_byte: bs.current_byte,
        current_bit: bs.current_bit,
    }
}

/// Set the bit-stream position.
/// Mirrors C `Acn_BitStream_SetPos`.
pub fn acn_bit_stream_set_pos(bs: &mut BitStream, p: AcnBitStreamPos) {
    bs.current_byte = p.current_byte;
    bs.current_bit = p.current_bit;
}

/// Compute the distance in bytes between two bit-stream positions (rounded up).
/// Mirrors C `Acn_BitStream_DistanceInBytes`.
pub fn acn_bit_stream_distance_in_bytes(start: AcnBitStreamPos, end: AcnBitStreamPos) -> Asn1SccUint {
    let start_bits = start.current_byte * 8 + start.current_bit as i64;
    let end_bits = end.current_byte * 8 + end.current_bit as i64;
    ((end_bits - start_bits + 7) / 8) as Asn1SccUint
}

/// Compute the distance in bits between two bit-stream positions.
/// Mirrors C `Acn_BitStream_DistanceInBits`.
pub fn acn_bit_stream_distance_in_bits(start: AcnBitStreamPos, end: AcnBitStreamPos) -> Asn1SccUint {
    let start_bits = start.current_byte * 8 + start.current_bit as i64;
    let end_bits = end.current_byte * 8 + end.current_bit as i64;
    (end_bits - start_bits) as Asn1SccUint
}

// ─────────────────────────────────────────────────────────────────────────
//  ACN Deferred Patching — InitDet / PatchDet for unsigned encoders
//  (In C these are generated by DEFINE_ACN_DET_ENCODERS macro)
// ─────────────────────────────────────────────────────────────────────────

/// InitDet for U8: reserve 8 bits (one byte) of placeholder zeros.
pub fn acn_init_det_u8(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_positive_integer_const_size_8(bs, 0);
}

/// PatchDet for U8: write the actual value at the reserved position.
/// Returns `(success, err_code)`.  On consistency mismatch for shared
/// determinants, `err_code` is set to `AcnDetConsistencyMismatch`.
pub fn acn_patch_det_u8(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_positive_integer_const_size_8(bs, v);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for U16_BE: reserve 16 bits of placeholder zeros.
pub fn acn_init_det_u16_be(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_positive_integer_const_size_big_endian_16(bs, 0);
}

/// PatchDet for U16_BE.
pub fn acn_patch_det_u16_be(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_positive_integer_const_size_big_endian_16(bs, v);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for U32_BE: reserve 32 bits of placeholder zeros.
pub fn acn_init_det_u32_be(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_positive_integer_const_size_big_endian_32(bs, 0);
}

/// PatchDet for U32_BE.
pub fn acn_patch_det_u32_be(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_positive_integer_const_size_big_endian_32(bs, v);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for U64_BE: reserve 64 bits of placeholder zeros.
pub fn acn_init_det_u64_be(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_positive_integer_const_size_big_endian_64(bs, 0);
}

/// PatchDet for U64_BE.
pub fn acn_patch_det_u64_be(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_positive_integer_const_size_big_endian_64(bs, v);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for U16_LE.
pub fn acn_init_det_u16_le(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_positive_integer_const_size_little_endian_16(bs, 0);
}

/// PatchDet for U16_LE.
pub fn acn_patch_det_u16_le(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_positive_integer_const_size_little_endian_16(bs, v);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for U32_LE.
pub fn acn_init_det_u32_le(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_positive_integer_const_size_little_endian_32(bs, 0);
}

/// PatchDet for U32_LE.
pub fn acn_patch_det_u32_le(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_positive_integer_const_size_little_endian_32(bs, v);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for U64_LE.
pub fn acn_init_det_u64_le(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_positive_integer_const_size_little_endian_64(bs, 0);
}

/// PatchDet for U64_LE.
pub fn acn_patch_det_u64_le(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_positive_integer_const_size_little_endian_64(bs, v);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  ACN Deferred Patching — InitDet / PatchDet for signed encoders
//  (In C these are generated by DEFINE_ACN_DET_ENCODERS_SIGNED macro)
// ─────────────────────────────────────────────────────────────────────────

/// InitDet for I8: reserve 8 bits of placeholder zeros.
pub fn acn_init_det_i8(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_twos_complement_const_size_8(bs, 0);
}

/// PatchDet for I8: write the actual signed value (cast to unsigned) at the
/// reserved position.
pub fn acn_patch_det_i8(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_twos_complement_const_size_8(bs, v as Asn1SccSint);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for I16_BE.
pub fn acn_init_det_i16_be(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_twos_complement_const_size_big_endian_16(bs, 0);
}

/// PatchDet for I16_BE.
pub fn acn_patch_det_i16_be(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_twos_complement_const_size_big_endian_16(bs, v as Asn1SccSint);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for I32_BE.
pub fn acn_init_det_i32_be(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_twos_complement_const_size_big_endian_32(bs, 0);
}

/// PatchDet for I32_BE.
pub fn acn_patch_det_i32_be(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_twos_complement_const_size_big_endian_32(bs, v as Asn1SccSint);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

/// InitDet for I64_BE.
pub fn acn_init_det_i64_be(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_twos_complement_const_size_big_endian_64(bs, 0);
}

/// PatchDet for I64_BE.
pub fn acn_patch_det_i64_be(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_int_twos_complement_const_size_big_endian_64(bs, v as Asn1SccSint);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  ACN Deferred Patching — BOOL1 (1-bit boolean determinant)
// ─────────────────────────────────────────────────────────────────────────

/// Encode a 1-bit boolean value.  Mirrors C `Acn_Enc_Bool_1bit`.
fn acn_enc_bool_1bit(bs: &mut BitStream, v: Asn1SccUint) {
    bs.append_bit(v != 0);
}

/// InitDet for BOOL1: reserve 1 bit of placeholder zero.
pub fn acn_init_det_bool1(bs: &mut BitStream, det: &mut AcnInsertedFieldRef) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_bool_1bit(bs, 0);
}

/// PatchDet for BOOL1: write the actual 1-bit value.
pub fn acn_patch_det_bool1(
    v: Asn1SccUint,
    bs: &mut BitStream,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        acn_enc_bool_1bit(bs, v);
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  ACN Deferred Patching — ConstSize (arbitrary bit width, unsigned)
//  (In C generated by DEFINE_ACN_DET_ENCODERS_CONSTSIZE macro)
// ─────────────────────────────────────────────────────────────────────────

/// InitDet for ConstSize: reserve `n_bits` bits of placeholder zeros.
pub fn acn_init_det_const_size(
    bs: &mut BitStream,
    n_bits: i32,
    det: &mut AcnInsertedFieldRef,
) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_positive_integer_const_size(bs, 0, n_bits);
}

/// PatchDet for ConstSize: write the value bit-by-bit at the reserved position.
/// Uses bit-by-bit overwrite (same as C macro) because `append_partial_byte`
/// clears adjacent bits when crossing a byte boundary — unsafe for patching
/// in the middle of a stream.
pub fn acn_patch_det_const_size(
    v: Asn1SccUint,
    bs: &mut BitStream,
    n_bits: i32,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        for i in 0..n_bits {
            bs.append_bit(((v >> (n_bits - 1 - i)) & 1) != 0);
        }
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  ACN Deferred Patching — TwosComplement_ConstSize (arbitrary bit width, signed)
//  (In C generated by DEFINE_ACN_DET_ENCODERS_SIGNED_CONSTSIZE macro)
// ─────────────────────────────────────────────────────────────────────────

/// InitDet for TwosComplement_ConstSize: reserve `n_bits` bits of placeholder zeros.
pub fn acn_init_det_twos_complement_const_size(
    bs: &mut BitStream,
    n_bits: i32,
    det: &mut AcnInsertedFieldRef,
) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    acn_enc_int_twos_complement_const_size(bs, 0, n_bits);
}

/// PatchDet for TwosComplement_ConstSize: write the value bit-by-bit at the
/// reserved position.  Uses bit-by-bit overwrite (same as C macro).
pub fn acn_patch_det_twos_complement_const_size(
    v: Asn1SccUint,
    bs: &mut BitStream,
    n_bits: i32,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        for i in 0..n_bits {
            bs.append_bit(((v >> (n_bits - 1 - i)) & 1) != 0);
        }
        acn_bit_stream_set_pos(bs, cur);
        det.value = v;
        det.is_set = true;
        (true, None)
    } else if det.value != v {
        (false, Some(ErrorCode::AcnDetConsistencyMismatch))
    } else {
        (true, None)
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  ACN Deferred Patching — IA5String_FixSize
// ─────────────────────────────────────────────────────────────────────────

/// InitDet for IA5String_FixSize: reserve `n_chars * 7` bits of placeholder zeros.
/// Mirrors C `Acn_InitDet_IA5String_FixSize`.
pub fn acn_init_det_ia5string_fix_size(
    bs: &mut BitStream,
    n_chars: i32,
    det: &mut AcnInsertedFieldRef,
) {
    det.pos = acn_bit_stream_get_pos(bs);
    det.is_set = false;
    det.value = 0;
    det.str_value[0] = 0;
    det.str_value_len = 0;
    for _ in 0..n_chars * 7 {
        bs.append_bit(false);
    }
}

/// PatchDet for IA5String_FixSize: write the string value at the reserved position.
/// Each character is encoded as 7 bits (MSB first).  Shorter strings are
/// padded with spaces (0x20).  Mirrors C `Acn_PatchDet_IA5String_FixSize`.
pub fn acn_patch_det_ia5string_fix_size(
    str_val: &[u8],
    bs: &mut BitStream,
    n_chars: i32,
    det: &mut AcnInsertedFieldRef,
) -> (bool, Option<ErrorCode>) {
    if !det.is_set {
        let slen = str_val
            .iter()
            .position(|&c| c == 0)
            .unwrap_or(str_val.len());
        let cur = acn_bit_stream_get_pos(bs);
        acn_bit_stream_set_pos(bs, det.pos);
        for i in 0..n_chars {
            let ch = if (i as usize) < slen {
                str_val[i as usize]
            } else {
                0x20 // space padding
            };
            for b in (0..7).rev() {
                bs.append_bit(((ch >> b) & 1) != 0);
            }
        }
        acn_bit_stream_set_pos(bs, cur);
        // Store string for consistency checking
        let copy_len = slen.min(255);
        det.str_value[..copy_len].copy_from_slice(&str_val[..copy_len]);
        det.str_value[copy_len] = 0;
        det.str_value_len = copy_len;
        det.is_set = true;
        (true, None)
    } else {
        let slen = str_val
            .iter()
            .position(|&c| c == 0)
            .unwrap_or(str_val.len())
            .min(n_chars as usize);
        if &det.str_value[..det.str_value_len] != &str_val[..slen] {
            (false, Some(ErrorCode::AcnDetConsistencyMismatch))
        } else {
            (true, None)
        }
    }
}
