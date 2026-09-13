//! ASN1SCC Rust runtime — UPER encoding for OBJECT IDENTIFIER and RELATIVE-OID.
//!
//! This module is the Rust equivalent of the C runtime's
//! `asn1crt_encoding_uper.h` / `asn1crt_encoding_uper.c`.  It implements
//! the Unaligned Packed Encoding Rules (UPER) encode and decode
//! operations for `OBJECT IDENTIFIER` and `RELATIVE-OID` values, which
//! are the only type-specific UPER functions not already provided by the
//! generic `BitStream` integer / octet-string / bit-string primitives in
//! [`crate`].
//!
//! # Algorithm overview
//!
//! Each arc (sub-identifier) of an OID is encoded as a sequence of
//! 7-bit chunks, with the high bit (0x80) of every octet except the last
//! set to `1`.  The first two arcs of an OID are combined into a single
//! sub-identifier (`arc0 * 40 + arc1`), following ITU-T X.690.  A
//! `RELATIVE-OID` has no such combining.
//!
//! The complete encoded content is preceded by a length determinant:
//! - If the length is ≤ 0x7F, it is encoded as a single constrained
//!   whole number in the range `[0, 0xFF]`.
//! - If the length is > 0x7F, a `1` bit is prepended and the length is
//!   encoded as a constrained whole number in the range `[0, 0x7FFF]`.
//!
//! Decoding reverses the process, reading the length determinant first
//! and then consuming sub-identifier octets until the length is
//! exhausted.

use crate::*;

/// `OBJECT IDENTIFIER` — a thin newtype wrapper around
/// [`Asn1ObjectIdentifier`] that carries the UPER encode/decode methods
/// with the naming expected by generated code.
#[repr(transparent)]
#[derive(Clone, Copy, Default, Debug)]
pub struct ObjectIdentifier(pub Asn1ObjectIdentifier);

/// `RELATIVE-OID` — a thin newtype wrapper around
/// [`Asn1ObjectIdentifier`] whose UPER encoding differs from
/// [`ObjectIdentifier`] only by the absence of first-two-arc combining.
#[repr(transparent)]
#[derive(Clone, Copy, Default, Debug)]
pub struct RelativeOID(pub Asn1ObjectIdentifier);

impl ObjectIdentifier {
    /// Initialise to a zero-length OID.
    pub fn init() -> Self {
        Self(Asn1ObjectIdentifier::init())
    }
    /// Borrow the underlying [`Asn1ObjectIdentifier`].
    pub fn inner(&self) -> &Asn1ObjectIdentifier {
        &self.0
    }
    /// Mutably borrow the underlying [`Asn1ObjectIdentifier`].
    pub fn inner_mut(&mut self) -> &mut Asn1ObjectIdentifier {
        &mut self.0
    }
}

impl RelativeOID {
    /// Initialise to a zero-length relative OID.
    pub fn init() -> Self {
        Self(Asn1ObjectIdentifier::init())
    }
    /// Borrow the underlying [`Asn1ObjectIdentifier`].
    pub fn inner(&self) -> &Asn1ObjectIdentifier {
        &self.0
    }
    /// Mutably borrow the underlying [`Asn1ObjectIdentifier`].
    pub fn inner_mut(&mut self) -> &mut Asn1ObjectIdentifier {
        &mut self.0
    }
}

impl PartialEq for ObjectIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.0.equal(&other.0)
    }
}
impl Eq for ObjectIdentifier {}

impl PartialEq for RelativeOID {
    fn eq(&self, other: &Self) -> bool {
        self.0.equal(&other.0)
    }
}
impl Eq for RelativeOID {}

// ─────────────────────────────────────────────────────────────────────────
//  Internal helpers
// ─────────────────────────────────────────────────────────────────────────

/// Encode a single sub-identifier (arc value) into a byte buffer using
/// 7-bit chunking.
///
/// Each chunk occupies the low 7 bits of an octet; the high bit (0x80)
/// is set on every octet except the last to signal continuation.  The
/// octets are written in big-endian (most-significant chunk first)
/// order.
///
/// `encoding_buf` receives the encoded octets; `p_size` is both the
/// current write offset (updated in place) and the total size so far.
///
/// Mirrors the C `ObjectIdentifier_subidentifiers_uper_encode`.
fn subidentifiers_uper_encode(encoding_buf: &mut [u8], p_size: &mut usize, mut si_value: Asn1SccUint) {
    let mut tmp = [0u8; 16];
    let mut n_size: usize = 0;

    // Split the value into 7-bit chunks, least-significant first.
    loop {
        let cur_byte = (si_value % 128) as u8;
        si_value /= 128;
        let last_octet = si_value == 0;
        tmp[n_size] = cur_byte;
        n_size += 1;
        if last_octet {
            break;
        }
    }

    // Emit in reverse (most-significant chunk first), setting 0x80
    // on every octet except the last.
    for i in 0..n_size {
        let cur_byte = if i == n_size - 1 {
            tmp[n_size - 1 - i]
        } else {
            tmp[n_size - 1 - i] | 0x80
        };
        encoding_buf[*p_size] = cur_byte;
        *p_size += 1;
    }
}

/// Decode a single sub-identifier (arc value) from the bit stream.
///
/// Reads octets one at a time, consuming from `p_remaining_octets`,
/// until an octet with the high bit clear is encountered (the last
/// octet of this sub-identifier).  The 7-bit payloads are accumulated
/// big-endian into `si_value`.
///
/// Returns `false` if the stream is exhausted before a complete
/// sub-identifier is read.
///
/// Mirrors the C `ObjectIdentifier_subidentifiers_uper_decode`.
fn subidentifiers_uper_decode(
    bs: &mut BitStream,
    p_remaining_octets: &mut Asn1SccSint,
    si_value: &mut Asn1SccUint,
) -> bool {
    let mut b_last_octet = false;
    let mut cur_octet_value: u8;
    *si_value = 0;

    while *p_remaining_octets > 0 && !b_last_octet {
        let (cur_byte, ok) = bs.read_byte();
        if !ok {
            return false;
        }
        *p_remaining_octets -= 1;

        b_last_octet = (cur_byte & 0x80) == 0;
        cur_octet_value = cur_byte & 0x7F;
        *si_value <<= 7;
        *si_value |= cur_octet_value as Asn1SccUint;
    }
    true
}

/// Decode the length determinant that precedes the OID content octets.
///
/// First reads a constrained whole number in `[0, 0xFF]`.  If the value
/// exceeds 0x7F, a second byte is read and the two are combined as a
/// 15-bit length (`(first << 8 | second) & 0x7FFF`).
///
/// Returns `false` on stream exhaustion.
///
/// Mirrors the C `ObjectIdentifier_uper_decode_length`.
fn decode_length(bs: &mut BitStream, total_size: &mut Asn1SccSint) -> bool {
    let (ts, ok) = bs.decode_constraint_whole_number(0, 0xFF);
    if !ok {
        return false;
    }
    *total_size = ts;
    if *total_size > 0x7F {
        let (len2, ok2) = bs.decode_constraint_whole_number(0, 0xFF);
        if !ok2 {
            return false;
        }
        *total_size <<= 8;
        *total_size |= len2;
        *total_size &= 0x7FFF;
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  OBJECT IDENTIFIER UPER encode / decode
// ─────────────────────────────────────────────────────────────────────────

impl ObjectIdentifier {
    /// UPER-encode the OID into `bs`.
    ///
    /// The first two arcs are combined into `values[0] * 40 + values[1]`;
    /// each remaining arc is encoded independently.  The resulting content
    /// octets are prefixed with a length determinant.
    ///
    /// Returns `true` on success, `false` if the bit stream is exhausted.
    ///
    /// Mirrors the C `ObjectIdentifier_uper_encode`.
    pub fn uper_encode(&self, bs: &mut BitStream) -> bool {
        // A sub-identifier takes at most sizeof(asn1SccUint) + 2 octets.
        let mut tmp = [0u8; OBJECT_IDENTIFIER_MAX_LENGTH * (WORD_SIZE as usize + 2)];
        let mut total_size: usize = 0;

        // Combine the first two arcs.
        subidentifiers_uper_encode(&mut tmp, &mut total_size, self.0.values[0] * 40 + self.0.values[1]);
        for i in 2..self.0.n_count as usize {
            subidentifiers_uper_encode(&mut tmp, &mut total_size, self.0.values[i]);
        }

        // Length determinant.
        if total_size <= 0x7F {
            bs.encode_constraint_whole_number(total_size as Asn1SccSint, 0, 0xFF);
        } else {
            bs.append_bit(true);
            bs.encode_constraint_whole_number(total_size as Asn1SccSint, 0, 0x7FFF);
        }

        // Content octets.
        for i in 0..total_size {
            if !bs.append_byte0(tmp[i]) {
                return false;
            }
        }
        true
    }

    /// UPER-decode an OID from `bs` into `self`.
    ///
    /// The first decoded sub-identifier is split back into two arcs
    /// (`value / 40`, `value % 40`); subsequent sub-identifiers become
    /// individual arcs.  `self` is initialised (zeroed) before decoding.
    ///
    /// Returns `true` on success, `false` if the stream is exhausted or
    /// the encoded content exceeds
    /// [`OBJECT_IDENTIFIER_MAX_LENGTH`].
    ///
    /// Mirrors the C `ObjectIdentifier_uper_decode`.
    pub fn uper_decode(&mut self, bs: &mut BitStream) -> bool {
        self.0 = Asn1ObjectIdentifier::init();
        let mut total_size: Asn1SccSint = 0;
        let mut si: Asn1SccUint = 0;

        if !decode_length(bs, &mut total_size) {
            return false;
        }

        // First sub-identifier → first two arcs.
        if !subidentifiers_uper_decode(bs, &mut total_size, &mut si) {
            return false;
        }
        self.0.n_count = 2;
        self.0.values[0] = si / 40;
        self.0.values[1] = si % 40;

        // Remaining sub-identifiers.
        while total_size > 0 && self.0.n_count < OBJECT_IDENTIFIER_MAX_LENGTH as i32 {
            if !subidentifiers_uper_decode(bs, &mut total_size, &mut si) {
                return false;
            }
            self.0.values[self.0.n_count as usize] = si;
            self.0.n_count += 1;
        }

        // True only if all octets were consumed.
        total_size == 0
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  RELATIVE-OID UPER encode / decode
// ─────────────────────────────────────────────────────────────────────────

impl RelativeOID {
    /// UPER-encode the RELATIVE-OID into `bs`.
    ///
    /// Same as [`ObjectIdentifier::uper_encode`] but **without** the
    /// first-two-arc combining — every arc is encoded as its own
    /// sub-identifier.
    ///
    /// Returns `true` on success, `false` if the bit stream is exhausted.
    ///
    /// Mirrors the C `RelativeOID_uper_encode`.
    pub fn uper_encode(&self, bs: &mut BitStream) -> bool {
        let mut tmp = [0u8; OBJECT_IDENTIFIER_MAX_LENGTH * (WORD_SIZE as usize + 2)];
        let mut total_size: usize = 0;

        for i in 0..self.0.n_count as usize {
            subidentifiers_uper_encode(&mut tmp, &mut total_size, self.0.values[i]);
        }

        if total_size <= 0x7F {
            bs.encode_constraint_whole_number(total_size as Asn1SccSint, 0, 0xFF);
        } else {
            bs.append_bit(true);
            bs.encode_constraint_whole_number(total_size as Asn1SccSint, 0, 0x7FFF);
        }

        for i in 0..total_size {
            if !bs.append_byte0(tmp[i]) {
                return false;
            }
        }
        true
    }

    /// UPER-decode a RELATIVE-OID from `bs` into `self`.
    ///
    /// Same as [`ObjectIdentifier::uper_decode`] but without the
    /// first-two-arc splitting — each sub-identifier becomes one arc.
    /// `self` is initialised (zeroed) before decoding.
    ///
    /// Returns `true` on success, `false` if the stream is exhausted or
    /// the encoded content exceeds
    /// [`OBJECT_IDENTIFIER_MAX_LENGTH`].
    ///
    /// Mirrors the C `RelativeOID_uper_decode`.
    pub fn uper_decode(&mut self, bs: &mut BitStream) -> bool {
        self.0 = Asn1ObjectIdentifier::init();
        let mut total_size: Asn1SccSint = 0;
        let mut si: Asn1SccUint = 0;

        if !decode_length(bs, &mut total_size) {
            return false;
        }

        while total_size > 0 && self.0.n_count < OBJECT_IDENTIFIER_MAX_LENGTH as i32 {
            if !subidentifiers_uper_decode(bs, &mut total_size, &mut si) {
                return false;
            }
            self.0.values[self.0.n_count as usize] = si;
            self.0.n_count += 1;
        }

        total_size == 0
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  Tests
// ─────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Build an OID from a slice of arc values.
    fn make_oid(arcs: &[Asn1SccUint]) -> ObjectIdentifier {
        let mut oid = Asn1ObjectIdentifier::init();
        for (i, &v) in arcs.iter().enumerate() {
            oid.values[i] = v;
        }
        oid.n_count = arcs.len() as i32;
        ObjectIdentifier(oid)
    }

    fn make_relative_oid(arcs: &[Asn1SccUint]) -> RelativeOID {
        let mut oid = Asn1ObjectIdentifier::init();
        for (i, &v) in arcs.iter().enumerate() {
            oid.values[i] = v;
        }
        oid.n_count = arcs.len() as i32;
        RelativeOID(oid)
    }

    #[test]
    fn test_oid_uper_roundtrip_simple() {
        // 1.3.6.1.4.1 — a classic OID
        let original = make_oid(&[1, 3, 6, 1, 4, 1]);
        let mut buf = [0u8; 64];
        {
            let mut bs = BitStream::new(&mut buf);
            assert!(original.uper_encode(&mut bs));
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let mut decoded = ObjectIdentifier::default();
        assert!(decoded.uper_decode(&mut bs));
        assert_eq!(decoded.inner().n_count, original.inner().n_count);
        for i in 0..decoded.inner().n_count as usize {
            assert_eq!(decoded.inner().values[i], original.inner().values[i], "arc {} mismatch", i);
        }
    }

    #[test]
    fn test_oid_uper_roundtrip_large_arcs() {
        // Arcs that require multi-octet sub-identifiers (values > 127).
        let original = make_oid(&[2, 16, 840, 113549, 1, 1, 11]);
        let mut buf = [0u8; 128];
        {
            let mut bs = BitStream::new(&mut buf);
            assert!(original.uper_encode(&mut bs));
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let mut decoded = ObjectIdentifier::default();
        assert!(decoded.uper_decode(&mut bs));
        assert_eq!(decoded.inner().n_count, original.inner().n_count);
        for i in 0..decoded.inner().n_count as usize {
            assert_eq!(decoded.inner().values[i], original.inner().values[i], "arc {} mismatch", i);
        }
    }

    #[test]
    fn test_oid_uper_two_arcs() {
        // Minimal OID with exactly two arcs.
        let original = make_oid(&[0, 0]);
        let mut buf = [0u8; 32];
        {
            let mut bs = BitStream::new(&mut buf);
            assert!(original.uper_encode(&mut bs));
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let mut decoded = ObjectIdentifier::default();
        assert!(decoded.uper_decode(&mut bs));
        assert_eq!(decoded.inner().n_count, 2);
        assert_eq!(decoded.inner().values[0], 0);
        assert_eq!(decoded.inner().values[1], 0);
    }

    #[test]
    fn test_relative_oid_uper_roundtrip() {
        let original = make_relative_oid(&[113549, 1, 1, 11]);
        let mut buf = [0u8; 64];
        {
            let mut bs = BitStream::new(&mut buf);
            assert!(original.uper_encode(&mut bs));
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let mut decoded = RelativeOID::default();
        assert!(decoded.uper_decode(&mut bs));
        assert_eq!(decoded.inner().n_count, original.inner().n_count);
        for i in 0..decoded.inner().n_count as usize {
            assert_eq!(decoded.inner().values[i], original.inner().values[i], "arc {} mismatch", i);
        }
    }

    #[test]
    fn test_relative_oid_uper_single_arc() {
        let original = make_relative_oid(&[42]);
        let mut buf = [0u8; 32];
        {
            let mut bs = BitStream::new(&mut buf);
            assert!(original.uper_encode(&mut bs));
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let mut decoded = RelativeOID::default();
        assert!(decoded.uper_decode(&mut bs));
        assert_eq!(decoded.inner().n_count, 1);
        assert_eq!(decoded.inner().values[0], 42);
    }

    #[test]
    fn test_subidentifiers_encode_decode() {
        // Verify 7-bit chunking for a value that needs 3 octets.
        let mut buf = [0u8; 16];
        let mut size = 0usize;
        subidentifiers_uper_encode(&mut buf, &mut size, 113549);
        // 113549 = 0x1BBAD → needs three 7-bit chunks
        assert_eq!(size, 3);
        // First octet has continuation bit set.
        assert!(buf[0] & 0x80 != 0);
        // Last octet has continuation bit clear.
        assert_eq!(buf[2] & 0x80, 0);

        // Decode back.
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let mut remaining = 3i64;
        let mut si = 0u64;
        assert!(subidentifiers_uper_decode(&mut bs, &mut remaining, &mut si));
        assert_eq!(si, 113549);
        assert_eq!(remaining, 0);
    }
}
