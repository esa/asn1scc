#[cfg(test)]
mod tests {
    use crate::*;
    use crate::acn::*;

    fn fresh_stream(buf: &mut [u8]) -> BitStream {
        BitStream::new(buf)
    }

    fn reset_for_decode(buf: &mut [u8]) -> BitStream {
        BitStream::attach_buffer_no_zero(buf)
    }

    // ─── PositiveInteger ConstSize round-trips ───

    #[test]
    fn positive_integer_const_size_roundtrip() {
        for &(val, bits) in &[
            (0u64, 8), (1, 8), (255, 8), (0, 16), (65535, 16),
            (0, 32), (0xDEADBEEF_u64, 32), (0, 64), (u64::MAX, 64),
            (42, 12), (1023, 10),
        ] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_const_size(&mut bs, val, bits);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_const_size(&mut bs, bits);
            assert!(ok, "decode failed for val={val} bits={bits}");
            assert_eq!(decoded, val, "mismatch for val={val} bits={bits}");
        }
    }

    #[test]
    fn positive_integer_const_size_8_roundtrip() {
        for &val in &[0u64, 1, 127, 255] {
            let mut buf = [0u8; 4];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_const_size_8(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_const_size_8(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    #[test]
    fn positive_integer_big_endian_16_roundtrip() {
        for &val in &[0u64, 1, 0x1234, 0xFFFF] {
            let mut buf = [0u8; 4];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_const_size_big_endian_16(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_const_size_big_endian_16(&mut bs);
            assert!(ok, "decode failed for val={val:#x}");
            assert_eq!(decoded, val, "mismatch for val={val:#x}");
        }
    }

    #[test]
    fn positive_integer_big_endian_32_roundtrip() {
        for &val in &[0u64, 1, 0x12345678, 0xFFFFFFFF] {
            let mut buf = [0u8; 8];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_const_size_big_endian_32(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_const_size_big_endian_32(&mut bs);
            assert!(ok, "decode failed for val={val:#x}");
            assert_eq!(decoded, val, "mismatch for val={val:#x}");
        }
    }

    #[test]
    fn positive_integer_big_endian_64_roundtrip() {
        for &val in &[0u64, 1, 0x123456789ABCDEF0, u64::MAX] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_const_size_big_endian_64(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_const_size_big_endian_64(&mut bs);
            assert!(ok, "decode failed for val={val:#x}");
            assert_eq!(decoded, val, "mismatch for val={val:#x}");
        }
    }

    #[test]
    fn positive_integer_little_endian_16_roundtrip() {
        for &val in &[0u64, 1, 0x1234, 0xFFFF] {
            let mut buf = [0u8; 4];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_const_size_little_endian_16(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_const_size_little_endian_16(&mut bs);
            assert!(ok, "decode failed for val={val:#x}");
            assert_eq!(decoded, val, "mismatch for val={val:#x}");
        }
    }

    #[test]
    fn positive_integer_little_endian_32_roundtrip() {
        for &val in &[0u64, 1, 0x12345678, 0xFFFFFFFF] {
            let mut buf = [0u8; 8];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_const_size_little_endian_32(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_const_size_little_endian_32(&mut bs);
            assert!(ok, "decode failed for val={val:#x}");
            assert_eq!(decoded, val, "mismatch for val={val:#x}");
        }
    }

    #[test]
    fn positive_integer_little_endian_64_roundtrip() {
        for &val in &[0u64, 1, 0x123456789ABCDEF0, u64::MAX] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_const_size_little_endian_64(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_const_size_little_endian_64(&mut bs);
            assert!(ok, "decode failed for val={val:#x}");
            assert_eq!(decoded, val, "mismatch for val={val:#x}");
        }
    }

    // ─── TwosComplement ConstSize round-trips ───

    #[test]
    fn twos_complement_const_size_roundtrip() {
        for &(val, bits) in &[
            (0i64, 8), (1, 8), (-1, 8), (127, 8), (-128, 8),
            (0, 16), (32767, 16), (-32768, 16),
            (0, 32), (i32::MAX as i64, 32), (i32::MIN as i64, 32),
            (0, 64), (i64::MAX, 64), (i64::MIN, 64),
            (42, 12), (-42, 12),
        ] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_const_size(&mut bs, val, bits);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_const_size(&mut bs, bits);
            assert!(ok, "decode failed for val={val} bits={bits}");
            assert_eq!(decoded, val, "mismatch for val={val} bits={bits}");
        }
    }

    #[test]
    fn twos_complement_const_size_8_roundtrip() {
        for &val in &[0i64, 1, -1, 127, -128] {
            let mut buf = [0u8; 4];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_const_size_8(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_const_size_8(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    #[test]
    fn twos_complement_big_endian_16_roundtrip() {
        for &val in &[0i64, 1, -1, 32767, -32768] {
            let mut buf = [0u8; 4];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_const_size_big_endian_16(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_const_size_big_endian_16(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    #[test]
    fn twos_complement_big_endian_32_roundtrip() {
        for &val in &[0i64, 1, -1, i32::MAX as i64, i32::MIN as i64] {
            let mut buf = [0u8; 8];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_const_size_big_endian_32(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_const_size_big_endian_32(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    #[test]
    fn twos_complement_big_endian_64_roundtrip() {
        for &val in &[0i64, 1, -1, i64::MAX, i64::MIN] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_const_size_big_endian_64(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_const_size_big_endian_64(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    #[test]
    fn twos_complement_little_endian_16_roundtrip() {
        for &val in &[0i64, 1, -1, 32767, -32768] {
            let mut buf = [0u8; 4];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_const_size_little_endian_16(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_const_size_little_endian_16(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    #[test]
    fn twos_complement_little_endian_32_roundtrip() {
        for &val in &[0i64, 1, -1, i32::MAX as i64, i32::MIN as i64] {
            let mut buf = [0u8; 8];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_const_size_little_endian_32(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_const_size_little_endian_32(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    #[test]
    fn twos_complement_little_endian_64_roundtrip() {
        for &val in &[0i64, 1, -1, i64::MAX, i64::MIN] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_const_size_little_endian_64(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_const_size_little_endian_64(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    // ─── BCD round-trips ───

    #[test]
    fn bcd_const_size_roundtrip() {
        for &(val, digits) in &[(0u64, 1), (9, 1), (42, 2), (12345, 5), (99999, 5)] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_bcd_const_size(&mut bs, val, digits);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_bcd_const_size(&mut bs, digits);
            assert!(ok, "decode failed for val={val} digits={digits}");
            assert_eq!(decoded, val, "mismatch for val={val} digits={digits}");
        }
    }

    // ─── Alignment ───

    #[test]
    fn align_to_next_byte_encode() {
        let mut buf = [0u8; 8];
        let mut bs = fresh_stream(&mut buf);
        bs.append_bit(true);
        assert_eq!(bs.current_bit, 1);
        acn_align_to_next_byte(&mut bs, true);
        assert_eq!(bs.current_bit, 0);
        assert_eq!(bs.current_byte, 1);
    }

    #[test]
    fn align_to_next_byte_already_aligned() {
        let mut buf = [0u8; 8];
        let mut bs = fresh_stream(&mut buf);
        assert_eq!(bs.current_bit, 0);
        acn_align_to_next_byte(&mut bs, true);
        assert_eq!(bs.current_byte, 0);
    }

    // ─── VarSize LengthEmbedded round-trips ───

    #[test]
    fn positive_integer_var_size_length_embedded_roundtrip() {
        for &val in &[0u64, 1, 255, 256, 65535, 0xFFFFFF, 0xFFFFFFFF] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_positive_integer_var_size_length_embedded(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_positive_integer_var_size_length_embedded(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    #[test]
    fn twos_complement_var_size_length_embedded_roundtrip() {
        for &val in &[0i64, 1, -1, 127, -128, 32767, -32768, i32::MAX as i64, i32::MIN as i64] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_int_twos_complement_var_size_length_embedded(&mut bs, val);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_int_twos_complement_var_size_length_embedded(&mut bs);
            assert!(ok, "decode failed for val={val}");
            assert_eq!(decoded, val, "mismatch for val={val}");
        }
    }

    // ─── ASCII integer round-trips ───

    #[test]
    fn uint_ascii_const_size_roundtrip() {
        for &(val, digits) in &[(0u64, 1), (9, 1), (42, 2), (12345, 5), (99999, 5)] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_uint_ascii_const_size(&mut bs, val, digits);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_uint_ascii_const_size(&mut bs, digits);
            assert!(ok, "decode failed for val={val} digits={digits}");
            assert_eq!(decoded, val, "mismatch for val={val} digits={digits}");
        }
    }

    #[test]
    fn sint_ascii_const_size_roundtrip() {
        for &(val, digits) in &[(0i64, 2), (9, 2), (-9, 2), (42, 3), (-42, 3), (12345, 6), (-12345, 6)] {
            let mut buf = [0u8; 16];
            {
                let mut bs = fresh_stream(&mut buf);
                acn_enc_sint_ascii_const_size(&mut bs, val, digits);
            }
            let mut bs = reset_for_decode(&mut buf);
            let (decoded, ok) = acn_dec_sint_ascii_const_size(&mut bs, digits);
            assert!(ok, "decode failed for val={val} digits={digits}");
            assert_eq!(decoded, val, "mismatch for val={val} digits={digits}");
        }
    }

    // ─── Deferred patching ───

    #[test]
    fn deferred_patch_u8_roundtrip() {
        let mut buf = [0u8; 16];
        let mut det = AcnInsertedFieldRef::default();
        {
            let mut bs = fresh_stream(&mut buf);
            acn_init_det_u8(&mut bs, &mut det);
            bs.append_byte0(0xAA);
            acn_patch_det_u8(42, &mut bs, &mut det);
        }
        let mut bs = reset_for_decode(&mut buf);
        let (val, ok) = bs.read_byte();
        assert!(ok);
        assert_eq!(val, 42);
    }

    #[test]
    fn deferred_patch_u16_be_roundtrip() {
        let mut buf = [0u8; 16];
        let mut det = AcnInsertedFieldRef::default();
        {
            let mut bs = fresh_stream(&mut buf);
            acn_init_det_u16_be(&mut bs, &mut det);
            bs.append_byte0(0xAA);
            bs.append_byte0(0xBB);
            bs.append_byte0(0xCC);
            acn_patch_det_u16_be(0x1234, &mut bs, &mut det);
        }
        let mut bs = reset_for_decode(&mut buf);
        let (decoded, ok) = acn_dec_int_positive_integer_const_size_big_endian_16(&mut bs);
        assert!(ok);
        assert_eq!(decoded, 0x1234);
    }

    // ─── BitStream position helpers ───

    #[test]
    fn bit_stream_position_distance() {
        let mut buf = [0u8; 32];
        let mut bs = fresh_stream(&mut buf);
        let start = acn_bit_stream_get_pos(&bs);
        bs.append_byte0(0xFF);
        bs.append_byte0(0xFF);
        let end = acn_bit_stream_get_pos(&bs);
        assert_eq!(acn_bit_stream_distance_in_bytes(start, end), 2);
        assert_eq!(acn_bit_stream_distance_in_bits(start, end), 16);
    }

    #[test]
    fn bit_stream_set_pos_restores() {
        let mut buf = [0u8; 16];
        let mut bs = fresh_stream(&mut buf);
        let saved = acn_bit_stream_get_pos(&bs);
        bs.append_byte0(0xAA);
        bs.append_byte0(0xBB);
        acn_bit_stream_set_pos(&mut bs, saved);
        assert_eq!(bs.current_byte, 0);
        assert_eq!(bs.current_bit, 0);
    }

    // ─── Decode on empty/short stream returns false ───

    #[test]
    fn decode_on_empty_stream_returns_false() {
        let mut buf = [0u8; 0];
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let (_, ok) = acn_dec_int_positive_integer_const_size(&mut bs, 8);
        assert!(!ok, "decoding from an empty stream should fail");
    }
}
