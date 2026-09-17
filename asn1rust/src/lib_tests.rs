#[cfg(test)]
mod tests {
    use crate::*;

    fn enc(buf: &mut [u8]) -> BitStream { BitStream::new(buf) }
    fn dec(buf: &mut [u8]) -> BitStream { BitStream::attach_buffer_no_zero(buf) }

    // ─── single bits ───

    #[test]
    fn bits_roundtrip_across_byte_boundary() {
        let pattern = [true, false, true, true, false, false, true, false, true, true, true, false];
        let mut buf = [0u8; 4];
        {
            let mut bs = enc(&mut buf);
            for &b in &pattern { bs.append_bit(b); }
        }
        assert_eq!(buf[0], 0b1011_0010);
        assert_eq!(buf[1], 0b1110_0000);
        let mut bs = dec(&mut buf);
        for (i, &b) in pattern.iter().enumerate() {
            let (v, ok) = bs.read_bit();
            assert!(ok, "read_bit {} failed", i);
            assert_eq!(v, b, "bit {} mismatch", i);
        }
    }

    #[test]
    fn append_bit_one_zero_and_peek() {
        let mut buf = [0u8; 2];
        {
            let mut bs = enc(&mut buf);
            bs.append_bit_one(); bs.append_bit_zero(); bs.append_bit_one();
        }
        assert_eq!(buf[0], 0b1010_0000);
        let mut bs = dec(&mut buf);
        assert!(bs.peek_bit());
        assert_eq!(bs.read_bit(), (true, true));
        assert!(!bs.peek_bit());
        assert_eq!(bs.read_bit(), (false, true));
        assert_eq!(bs.read_bit(), (true, true));
    }

    #[test]
    fn read_bit_past_end_fails() {
        let mut buf = [0xFFu8; 1];
        let mut bs = dec(&mut buf);
        for _ in 0..8 { assert!(bs.read_bit().1); }
        assert!(!bs.read_bit().1, "9th bit of a 1-byte stream must fail");
    }

    #[test]
    fn n_bit_zero_and_one_runs() {
        let mut buf = [0u8; 4];
        {
            let mut bs = enc(&mut buf);
            bs.append_n_bit_one(3);
            bs.append_n_bit_zero(7);
            bs.append_n_bit_one(10);
        }
        // 111 0000000 1111111111 -> 1110 0000 0011 1111 1111 0000
        assert_eq!(&buf[..3], &[0b1110_0000, 0b0011_1111, 0b1111_0000]);
        let mut bs = dec(&mut buf);
        for _ in 0..3 { assert_eq!(bs.read_bit(), (true, true)); }
        for _ in 0..7 { assert_eq!(bs.read_bit(), (false, true)); }
        for _ in 0..10 { assert_eq!(bs.read_bit(), (true, true)); }
    }

    // ─── partial bytes ───

    #[test]
    fn partial_byte_roundtrip_all_widths_unaligned() {
        for width in 1..=7u8 {
            for offset in 0..8 {
                let mut buf = [0u8; 4];
                let value = (0b1011_0110u8 >> (8 - width)) & ((1u16 << width) as u8).wrapping_sub(1);
                {
                    let mut bs = enc(&mut buf);
                    for _ in 0..offset { bs.append_bit_one(); }
                    bs.append_partial_byte(value, width, false);
                }
                let mut bs = dec(&mut buf);
                for _ in 0..offset { bs.read_bit(); }
                let (v, ok) = bs.read_partial_byte(width);
                assert!(ok, "width {} offset {} failed", width, offset);
                assert_eq!(v, value, "width {} offset {} mismatch", width, offset);
            }
        }
    }

    #[test]
    fn partial_byte_negate_complements_low_bits() {
        let mut buf = [0u8; 2];
        {
            let mut bs = enc(&mut buf);
            bs.append_partial_byte(0b101, 3, true); // !101 & 111 = 010
        }
        let mut bs = dec(&mut buf);
        assert_eq!(bs.read_partial_byte(3), (0b010, true));
    }

    #[test]
    fn read_partial_byte_past_end_fails() {
        let mut buf = [0xFFu8; 1];
        let mut bs = dec(&mut buf);
        assert_eq!(bs.read_partial_byte(5).1, true);
        assert_eq!(bs.read_partial_byte(5).1, false, "5+5 bits from a 1-byte stream must fail");
    }

    // ─── bit arrays ───

    #[test]
    fn append_bits_read_bits_roundtrip_unaligned() {
        let src = [0xA5u8, 0x3C, 0xF0];
        for nbits in [1, 7, 8, 9, 15, 16, 17, 20, 24] {
            for offset in 0..8 {
                let mut buf = [0u8; 8];
                {
                    let mut bs = enc(&mut buf);
                    for _ in 0..offset { bs.append_bit_zero(); }
                    bs.append_bits(&src, nbits);
                }
                let mut bs = dec(&mut buf);
                for _ in 0..offset { bs.read_bit(); }
                let mut out = [0u8; 3];
                assert!(bs.read_bits(&mut out, nbits), "nbits {} offset {}", nbits, offset);
                // compare only the nbits that were written
                let full = (nbits / 8) as usize;
                assert_eq!(&out[..full], &src[..full], "nbits {} offset {}", nbits, offset);
                let rem = nbits % 8;
                if rem > 0 {
                    let mask = 0xFFu8 << (8 - rem);
                    assert_eq!(out[full] & mask, src[full] & mask, "nbits {} offset {} tail", nbits, offset);
                }
            }
        }
    }

    // ─── byte arrays ───

    #[test]
    fn byte_array_roundtrip_aligned() {
        for len in [1usize, 2, 3, 5] {
            let src: Vec<u8> = (0..len).map(|i| 0x11 * (i as u8 + 1)).collect();
            let mut buf = vec![0u8; len + 2];
            {
                let mut bs = enc(&mut buf);
                assert!(bs.append_byte_array(&src), "append len {}", len);
            }
            assert_eq!(&buf[..len], &src[..], "buffer content len {}", len);
            let mut bs = dec(&mut buf);
            let mut out = vec![0u8; len];
            assert!(bs.read_byte_array(&mut out), "read len {}", len);
            assert_eq!(out, src, "roundtrip len {}", len);
        }
    }

    #[test]
    fn byte_array_roundtrip_unaligned() {
        let src = [0xDEu8, 0xAD, 0xBE, 0xEF];
        for offset in 1..8 {
            let mut buf = [0u8; 8];
            {
                let mut bs = enc(&mut buf);
                for _ in 0..offset { bs.append_bit_one(); }
                assert!(bs.append_byte_array(&src), "offset {}", offset);
            }
            let mut bs = dec(&mut buf);
            for _ in 0..offset { bs.read_bit(); }
            let mut out = [0u8; 4];
            assert!(bs.read_byte_array(&mut out), "offset {}", offset);
            assert_eq!(out, src, "offset {}", offset);
        }
    }

    #[test]
    fn byte_array_into_exactly_full_buffer_aligned() {
        // Filling the buffer exactly, at bit 0, must succeed without touching
        // a byte past the end.
        let src = [0x12u8, 0x34, 0x56];
        let mut buf = [0u8; 3];
        {
            let mut bs = enc(&mut buf);
            assert!(bs.append_byte_array(&src));
        }
        assert_eq!(buf, src);
        let mut bs = dec(&mut buf);
        let mut out = [0u8; 3];
        assert!(bs.read_byte_array(&mut out));
        assert_eq!(out, src);
    }

    #[test]
    fn byte_array_overflow_is_reported_not_panicked() {
        let src = [1u8, 2, 3, 4];
        let mut buf = [0u8; 3];
        let mut bs = enc(&mut buf);
        assert!(!bs.append_byte_array(&src));
        let mut buf2 = [0u8; 3];
        let mut bs2 = dec(&mut buf2);
        let mut out = [0u8; 4];
        assert!(!bs2.read_byte_array(&mut out));
    }

    // ─── whole numbers ───

    #[test]
    fn constraint_whole_number_boundaries() {
        for &(min, max) in &[(0i64, 0), (-1, 1), (-128, 127), (0, 255), (-1000, -990), (i32::MIN as i64, i32::MAX as i64), (0, i64::MAX / 2)] {
            for &v in &[min, max, (min + max) / 2] {
                let mut buf = [0u8; 16];
                {
                    let mut bs = enc(&mut buf);
                    bs.encode_constraint_whole_number(v, min, max);
                }
                let mut bs = dec(&mut buf);
                let (d, ok) = bs.decode_constraint_whole_number(min, max);
                assert!(ok, "decode failed v={} [{}, {}]", v, min, max);
                assert_eq!(d, v, "mismatch v={} [{}, {}]", v, min, max);
            }
        }
    }

    #[test]
    fn non_negative_integer_full_width_and_zero() {
        for &v in &[0u64, 1, 0xFF, 0x100, 0xFFFF_FFFF, 0x1_0000_0000, u64::MAX >> 1, u64::MAX] {
            let mut buf = [0u8; 16];
            let nbits = if v == 0 { 1 } else { 64 - v.leading_zeros() as i32 };
            {
                let mut bs = enc(&mut buf);
                bs.encode_non_negative_integer(v);
            }
            let mut bs = dec(&mut buf);
            let (d, ok) = bs.decode_non_negative_integer(nbits);
            assert!(ok, "v={:#x} nbits={}", v, nbits);
            assert_eq!(d, v, "v={:#x} nbits={}", v, nbits);
        }
    }

    #[test]
    fn get_length_tracks_bits_rounded_up() {
        let mut buf = [0u8; 4];
        let mut bs = enc(&mut buf);
        assert_eq!(bs.get_length(), 0);
        bs.append_bit_one();
        assert_eq!(bs.get_length(), 1);
        bs.append_n_bit_zero(7);
        assert_eq!(bs.get_length(), 1);
        bs.append_bit_one();
        assert_eq!(bs.get_length(), 2);
    }

    // ─── helpers ───

    #[test]
    fn uint2int_sign_extends_every_width() {
        for width in 1..=8i32 {
            let bits = width * 8;
            let min: i64 = if bits == 64 { i64::MIN } else { -(1i64 << (bits - 1)) };
            let max: i64 = if bits == 64 { i64::MAX } else { (1i64 << (bits - 1)) - 1 };
            for &v in &[min, -1, 0, 1, max] {
                let u = int2uint(v) & if bits == 64 { u64::MAX } else { (1u64 << bits) - 1 };
                assert_eq!(uint2int(u, width), v, "width {} v {}", width, v);
            }
        }
        assert_eq!(uint2int(0xFF, 0), 0);
        assert_eq!(uint2int(0xFF, 9), 0);
    }

    #[test]
    fn bit_string_equal_ignores_trailing_bits() {
        assert!(bit_string_equal(3, &[0b1010_0000], &[0b1011_1111]));
        assert!(!bit_string_equal(4, &[0b1010_0000], &[0b1011_1111]));
        assert!(bit_string_equal(16, &[0xAA, 0x55], &[0xAA, 0x55]));
        assert!(!bit_string_equal(16, &[0xAA, 0x55], &[0xAA, 0x54]));
        assert!(!bit_string_equal(8, &[0xAA], &[0xAA, 0x00]), "length mismatch is unequal");
    }

    #[test]
    fn searches_and_strings() {
        let arr = [-5i64, -1, 0, 3, 8, 13];
        for (i, &v) in arr.iter().enumerate() {
            assert_eq!(binary_search(&arr, v), i as i32);
            assert_eq!(linear_search(&arr, v), i as i32);
        }
        assert_eq!(binary_search(&arr, 4), -1);
        assert_eq!(linear_search(&arr, 4), -1);
        assert_eq!(binary_search(&[], 1), -1);

        let mut s = [0xFFu8; 6];
        set_string(&mut s, "abc");
        assert_eq!(s, [b'a', b'b', b'c', 0, 0, 0]);
        assert!(ia5_string_equal_str(&s, "abc"));
        assert!(!ia5_string_equal_str(&s, "abcd"));
        assert!(ia5_string_equal(&s, b"abc\0zz"));
        set_string(&mut s, "toolongstring");
        assert_eq!(&s, b"toolon");
    }
}
