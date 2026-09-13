//! ASN1SCC Rust runtime — core types and bit-stream primitives.
//!
//! This module is the Rust equivalent of the C runtime's `asn1crt.h` +
//! `asn1crt.c` + `asn1crt_encoding.c`. It defines the fundamental types
//! (`Asn1SccSint`, `Asn1Real`, …), the `BitStream` / `ByteStream` structures
//! and every bit-level primitive used by the generated PER / ACN / BER / XER
//! encode/decode code.
//!
//! Encoding backends that need higher-level operations (UPER, ACN, XER, BER)
//! live in sibling modules (`uper`, `acn`, `xer`, `ber`) and build on top of
//! the primitives declared here.

#![allow(clippy::too_many_arguments)]
#![allow(clippy::needless_range_loop)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]

pub mod acn;
pub mod ber;
pub mod uper;
pub mod xer;

// ─────────────────────────────────────────────────────────────────────────
//  Type aliases
// ─────────────────────────────────────────────────────────────────────────

/// Signed integer used throughout the runtime (64-bit, mirrors C `asn1SccSint`).
pub type Asn1SccSint = i64;
/// Unsigned integer used throughout the runtime (64-bit, mirrors C `asn1SccUint`).
pub type Asn1SccUint = u64;
/// Real (floating-point) type, mirrors C `asn1Real` (double precision).
pub type Asn1Real = f64;
/// 32-bit real (float), used by `decode_real_fp32`.
pub type Asn1Real32 = f32;
/// Single byte, mirrors C `byte`.
pub type Byte = u8;
/// Boolean flag, mirrors C `flag`.
pub type Flag = bool;

/// 32-bit signed integer.
pub type Asn1SccSint32 = i32;
/// 32-bit unsigned integer.
pub type Asn1SccUint32 = u32;
/// 64-bit signed integer.
pub type Asn1SccSint64 = i64;
/// 64-bit unsigned integer.
pub type Asn1SccUint64 = u64;

/// BER tag type (the C runtime uses `asn1SccUint`).
pub type BerTag = Asn1SccUint;

/// Maximum number of components in an OBJECT IDENTIFIER.
pub const OBJECT_IDENTIFIER_MAX_LENGTH: usize = 20;

pub const NO_OF_BITS_IN_BYTE: i32 = 8;
pub const NO_OF_BYTES_IN_INT16: i32 = 2;
pub const NO_OF_BYTES_IN_INT32: i32 = 4;
pub const NO_OF_BITS_IN_INT16: i32 = 16;
pub const NO_OF_BITS_IN_INT32: i32 = 32;

/// Word size in bytes (C runtime uses a compile-time `WORD_SIZE`; we fix 8).
pub const WORD_SIZE: i32 = 8;
pub const MAX_INT: Asn1SccUint = 0xFFFF_FFFF_FFFF_FFFF;

// ─────────────────────────────────────────────────────────────────────────
//  Error codes
// ─────────────────────────────────────────────────────────────────────────

/// Error codes returned by decode functions, mirrors the `#define ERR_*`
/// constants in `asn1crt.h`.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCode {
    InsufficientData = 101,
    IncorrectPerStream = 102,
    InvalidChoiceAlternative = 103,
    InvalidEnumValue = 104,
    InvalidXmlFile = 200,
    InvalidBerFile = 201,
    BerLengthMismatch = 202,
    AcnDetConsistencyMismatch = 203,
}

// ─────────────────────────────────────────────────────────────────────────
//  NullType
// ─────────────────────────────────────────────────────────────────────────

/// ASN.1 NULL — a zero-sized placeholder, mirrors C `NullType` (which was
/// `char`). Using a unit struct is more idiomatic in Rust.
pub struct NullType;

// ─────────────────────────────────────────────────────────────────────────
//  Object Identifier
// ─────────────────────────────────────────────────────────────────────────

/// An OBJECT IDENTIFIER / RELATIVE-OID value.
///
/// `values` holds up to [`OBJECT_IDENTIFIER_MAX_LENGTH`] arc values; `n_count`
/// is the number of valid arcs (0 when uninitialised).
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Asn1ObjectIdentifier {
    pub values: [Asn1SccUint; OBJECT_IDENTIFIER_MAX_LENGTH],
    pub n_count: i32,
}

impl Default for Asn1ObjectIdentifier {
    fn default() -> Self {
        Self::init()
    }
}

impl Asn1ObjectIdentifier {
    /// Initialise to a zero-length OID (all arcs cleared).
    pub fn init() -> Self {
        Asn1ObjectIdentifier {
            values: [0; OBJECT_IDENTIFIER_MAX_LENGTH],
            n_count: 0,
        }
    }

    /// Returns `true` when the OID is structurally valid: at least two arcs,
    /// first arc ≤ 2, second arc ≤ 39.
    pub fn is_valid(&self) -> bool {
        self.n_count >= 2
            && self.values[0] <= 2
            && self.values[1] <= 39
    }

    /// Returns `true` when the value is a valid relative OID (at least one arc).
    pub fn relative_oid_is_valid(&self) -> bool {
        self.n_count > 0
    }

    /// Returns `true` when two OIDs are equal (same arc count and arc values).
    pub fn equal(&self, other: &Self) -> bool {
        if self.n_count != other.n_count
            || self.n_count < 0
            || (self.n_count as usize) > OBJECT_IDENTIFIER_MAX_LENGTH
        {
            return false;
        }
        let n = self.n_count as usize;
        for i in 0..n {
            if self.values[i] != other.values[i] {
                return false;
            }
        }
        true
    }
}

#[allow(non_snake_case)]
pub fn ObjectIdentifier_isValid(pVal: Asn1ObjectIdentifier) -> bool {
    pVal.n_count >= 2 && pVal.values[0] <= 2 && pVal.values[1] <= 39
}

#[allow(non_snake_case)]
pub fn RelativeOID_isValid(_pVal: Asn1ObjectIdentifier) -> bool {
    true
}

impl PartialEq for Asn1ObjectIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.equal(other)
    }
}
impl Eq for Asn1ObjectIdentifier {}

// UPER encode/decode convenience methods that delegate to the uper module wrappers.
impl Asn1ObjectIdentifier {
    /// UPER-encode this OID (combining first two arcs) via `uper::ObjectIdentifier`.
    pub fn uper_encode(&self, bs: &mut BitStream) -> bool {
        uper::ObjectIdentifier(self.clone()).uper_encode(bs)
    }

    /// UPER-decode into this OID (combining first two arcs) via `uper::ObjectIdentifier`.
    pub fn uper_decode(&mut self, bs: &mut BitStream) -> bool {
        let mut wrapper = uper::ObjectIdentifier(self.clone());
        let ok = wrapper.uper_decode(bs);
        *self = wrapper.0;
        ok
    }

    /// UPER-encode this relative OID via `uper::RelativeOID`.
    pub fn relative_uper_encode(&self, bs: &mut BitStream) -> bool {
        uper::RelativeOID(self.clone()).uper_encode(bs)
    }

    /// UPER-decode into this relative OID via `uper::RelativeOID`.
    pub fn relative_uper_decode(&mut self, bs: &mut BitStream) -> bool {
        let mut wrapper = uper::RelativeOID(self.clone());
        let ok = wrapper.uper_decode(bs);
        *self = wrapper.0;
        ok
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  Time types
// ─────────────────────────────────────────────────────────────────────────

/// Time-zone descriptor (sign ±1, hours, minutes).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Asn1TimeZone {
    pub sign: i32,  // -1 or +1
    pub hours: i32,
    pub mins: i32,
}

/// Time of day with an explicit time zone.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Asn1TimeWithTimeZone {
    pub hours: i32,
    pub mins: i32,
    pub secs: i32,
    pub fraction: i32,
    pub tz: Asn1TimeZone,
}

/// UTC time of day (no time zone).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Asn1UtcTime {
    pub hours: i32,
    pub mins: i32,
    pub secs: i32,
    pub fraction: i32,
}

/// Local time of day (no time zone).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Asn1LocalTime {
    pub hours: i32,
    pub mins: i32,
    pub secs: i32,
    pub fraction: i32,
}

/// Calendar date (year, month, day).
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Asn1Date {
    pub years: i32,
    pub months: i32,
    pub days: i32,
}

/// Date + local time.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Asn1DateLocalTime {
    pub date: Asn1Date,
    pub time: Asn1LocalTime,
}

/// Date + UTC time.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Asn1DateUtcTime {
    pub date: Asn1Date,
    pub time: Asn1UtcTime,
}

/// Date + time with time zone.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Asn1DateTimeWithTimeZone {
    pub date: Asn1Date,
    pub time: Asn1TimeWithTimeZone,
}

/// Distinguishes local-time-stamp / UTC-time-stamp / local-time-with-TZ stamp.
#[repr(i32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Asn1TimeZoneClass {
    LocalTimeStamp = 0,
    UtcTimeStamp = 1,
    LocalTimeTZStamp = 2,
}

// ─────────────────────────────────────────────────────────────────────────
//  Token / XmlAttribute
// ─────────────────────────────────────────────────────────────────────────

/// A scanned XML/XER token: an ID plus a short value string.
#[repr(C)]
pub struct Token {
    pub token_id: i32,
    pub value: [u8; 100],
}

impl Token {
    pub fn new() -> Self {
        Token {
            token_id: 0,
            value: [0; 100],
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self::new()
    }
}

/// A single XML attribute (name + value).
#[repr(C)]
pub struct XmlAttribute {
    pub name: [u8; 50],
    pub value: [u8; 100],
}

impl XmlAttribute {
    pub fn new() -> Self {
        XmlAttribute {
            name: [0; 50],
            value: [0; 100],
        }
    }
}

impl Default for XmlAttribute {
    fn default() -> Self {
        Self::new()
    }
}

/// A collection of up to 20 `XmlAttribute`s.
#[repr(C)]
pub struct XmlAttributeArray {
    pub attrs: [XmlAttribute; 20],
    pub n_count: i32,
}

impl XmlAttributeArray {
    pub fn new() -> Self {
        XmlAttributeArray {
            attrs: Default::default(),
            n_count: 0,
        }
    }
}

impl Default for XmlAttributeArray {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  Helper free functions
// ─────────────────────────────────────────────────────────────────────────

/// Convert a signed value to its unsigned bit-pattern (two's complement).
///
/// Mirrors C `int2uint`.
#[inline]
pub fn int2uint(v: Asn1SccSint) -> Asn1SccUint {
    if v < 0 {
        // C: ret = (uint)(-v - 1); ret = ~ret;
        // Using unsigned_abs() to avoid overflow on i64::MIN.
        let ret = v.unsigned_abs() - 1;
        !ret
    } else {
        v as Asn1SccUint
    }
}

/// Convert an unsigned value back to a signed integer, given the byte size of
/// the original signed value.  Mirrors C `uint2int`.
#[inline]
pub fn uint2int(v: Asn1SccUint, uint_size_in_bytes: i32) -> Asn1SccSint {
    let tmp: Asn1SccUint = 0x80;
    let is_negative = (v & (tmp << ((uint_size_in_bytes - 1) * 8))) > 0;
    if !is_negative {
        return v as Asn1SccSint;
    }
    let mut vv = v;
    let mut i = WORD_SIZE - 1; // 7
    while i >= uint_size_in_bytes {
        vv |= BER_AUX[i as usize];
        i -= 1;
    }
    -((!(vv)) as Asn1SccSint) - 1
}

/// BER byte-position masks (`0xFF`, `0xFF00`, …) used by `uint2int`.
pub const BER_AUX: [Asn1SccUint; 8] = [
    0xFF,
    0xFF00,
    0xFF_0000,
    0xFF_0000_00,
    0xFF_0000_0000,
    0xFF_0000_0000_00,
    0xFF_0000_0000_0000,
    0xFF00_0000_0000_0000,
];

/// Lookup the index of character `c` within the sorted byte set `alpha` using
/// a binary search.
///
/// Returns the index if found, otherwise `-1` (unlike the C version which
/// returns 0 on miss; callers that need the C semantics can treat -1 as 0).
pub fn get_char_index(c: char, alpha: &[u8]) -> i32 {
    let target = c as u8;
    let mut low: i32 = 0;
    let mut high: i32 = alpha.len() as i32 - 1;
    while low <= high {
        let mid = low + (high - low) / 2;
        let mid_val = alpha[mid as usize];
        if mid_val == target {
            return mid;
        } else if mid_val < target {
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }
    -1
}

/// Compare two `f64` values with a small relative tolerance, mirroring C
/// `Asn1Real_Equal`.
pub fn asn1_real_equal(left: f64, right: f64) -> bool {
    if left == right {
        return true;
    }
    if left == 0.0 {
        return right == 0.0;
    }
    if (left > 0.0 && right < 0.0) || (left < 0.0 && right > 0.0) {
        return false;
    }
    let (la, ra) = (left.abs(), right.abs());
    if la > ra {
        ra / la >= 0.99999
    } else {
        la / ra >= 0.99999
    }
}

/// Binary search over a sorted `Asn1SccSint` slice.  Returns the index of
/// `value` or `-1`.
pub fn binary_search(arr: &[Asn1SccSint], value: Asn1SccSint) -> i32 {
    let mut low: i32 = 0;
    let mut high: i32 = arr.len() as i32 - 1;
    while low <= high {
        let mid = low + (high - low) / 2;
        let mv = arr[mid as usize];
        if mv == value {
            return mid;
        } else if mv < value {
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }
    -1
}

/// Linear search over an `Asn1SccSint` slice.  Returns the index of `value`
/// or `-1`.
pub fn linear_search(arr: &[Asn1SccSint], value: Asn1SccSint) -> i32 {
    for (i, &v) in arr.iter().enumerate() {
        if v == value {
            return i as i32;
        }
    }
    -1
}

/// Returns `true` when two byte slices of equal length are equal — the C
/// `OctetString_equal`.
pub fn octet_string_equal(arr1: &[u8], arr2: &[u8]) -> bool {
    arr1 == arr2
}

/// Returns `true` when two C-style null-terminated byte arrays are equal.
pub fn ia5_string_equal(arr1: &[u8], arr2: &[u8]) -> bool {
    let len1 = arr1.iter().position(|&b| b == 0).unwrap_or(arr1.len());
    let len2 = arr2.iter().position(|&b| b == 0).unwrap_or(arr2.len());
    arr1[..len1] == arr2[..len2]
}

/// Returns `true` when a C-style null-terminated byte array matches a &str.
pub fn ia5_string_equal_str(arr: &[u8], s: &str) -> bool {
    let len = arr.iter().position(|&b| b == 0).unwrap_or(arr.len());
    &arr[..len] == s.as_bytes()
}

/// Sets a C-style null-terminated string byte array from a &str.
pub fn set_string(arr: &mut [u8], s: &str) {
    arr.fill(0);
    let bytes = s.as_bytes();
    let len = bytes.len().min(arr.len());
    arr[..len].copy_from_slice(&bytes[..len]);
}

/// Returns `true` when two bit strings of the same bit-length are equal —
/// the C `BitString_equal`.
pub fn bit_string_equal(n_bits_length1: i32, arr1: &[u8], arr2: &[u8]) -> bool {
    if arr1.len() != arr2.len() {
        return false;
    }
    let full_bytes = (n_bits_length1 / 8) as usize;
    if full_bytes > 0 && arr1[..full_bytes] != arr2[..full_bytes] {
        return false;
    }
    let rem = n_bits_length1 % 8;
    if rem > 0 {
        let shift = 8 - rem;
        let i = full_bytes;
        if arr1[i] >> shift != arr2[i] >> shift {
            return false;
        }
    }
    true
}

// ─────────────────────────────────────────────────────────────────────────
//  ByteStream
// ─────────────────────────────────────────────────────────────────────────

/// Byte-aligned stream used by XER / BER encoders.
///
/// Borrows a mutable byte buffer and tracks the current write position. The
/// buffer is *not* zeroed on attach (matching C `ByteStream_AttachBuffer`).
pub struct ByteStream<'a> {
    pub buf: &'a mut [u8],
    pub count: i64,
    pub current_byte: i64,
    pub encode_white_space: bool,
}

impl<'a> ByteStream<'a> {
    /// Create a `ByteStream` over `buf`, zeroing it and resetting the cursor.
    /// Mirrors C `ByteStream_Init`.
    pub fn init(buf: &'a mut [u8]) -> Self {
        let count = buf.len() as i64;
        for b in buf.iter_mut() {
            *b = 0;
        }
        ByteStream {
            buf,
            count,
            current_byte: 0,
            encode_white_space: false,
        }
    }

    /// Re-attach a new buffer and reset the cursor (no zeroing).
    /// Mirrors C `ByteStream_AttachBuffer`.
    pub fn attach_buffer(&mut self, buf: &'a mut [u8]) {
        self.count = buf.len() as i64;
        self.buf = buf;
        self.current_byte = 0;
    }

    /// Number of bytes written so far.
    pub fn get_length(&self) -> Asn1SccSint {
        self.current_byte
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  BitStream
// ─────────────────────────────────────────────────────────────────────────

/// Bit mask table, mirrors C `masks[]` (MSB-first within each byte).
const MASKS: [u8; 8] = [0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01];
/// Bit-count mask table, mirrors C `masksb[]` (low-n-bits mask).
const MASKSB: [u8; 9] = [0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF];
/// Byte-position masks for the low byte of a 32-bit word, mirrors C `masks2[]`.
const MASKS2: [Asn1SccUint32; 5] = [0x0, 0xFF, 0xFF00, 0xFF_0000, 0xFF00_0000];

/// Callback type for streaming push-data (flush) operations.
pub type PushDataFn = fn(&mut BitStream, *mut u8);
/// Callback type for streaming fetch-data (refill) operations.
pub type FetchDataFn = fn(&mut BitStream, *mut u8);

/// Bit-level stream used by every PER / ACN / BER encoder and decoder.
///
/// The stream borrows a mutable byte buffer and tracks the current bit/byte
/// position.  Optional `push_data` / `fetch_data` callbacks enable streaming
/// (flushing / refilling the buffer when the cursor wraps) — matching the
/// C runtime's `pushDataPrm` / `fetchDataPrm` mechanism.
pub struct BitStream<'a> {
    pub buf: &'a mut [u8],
    pub count: i64,
    pub current_byte: i64,
    /// Next available bit for writing (0..7, 0 = MSB of current byte).
    pub current_bit: i32,
    /// Optional flush callback + its user parameter.
    pub push_data: Option<PushDataFn>,
    pub push_data_prm: *mut u8,
    /// Optional refill callback + its user parameter.
    pub fetch_data: Option<FetchDataFn>,
    pub fetch_data_prm: *mut u8,
}

impl<'a> BitStream<'a> {
    /// Create a `BitStream` over `buf`, zeroing it and resetting the cursor.
    /// Mirrors C `BitStream_Init`.
    pub fn new(buf: &'a mut [u8]) -> Self {
        let count = buf.len() as i64;
        for b in buf.iter_mut() {
            *b = 0;
        }
        BitStream {
            buf,
            count,
            current_byte: 0,
            current_bit: 0,
            push_data: None,
            push_data_prm: std::ptr::null_mut(),
            fetch_data: None,
            fetch_data_prm: std::ptr::null_mut(),
        }
    }

    /// Alias for `new` — matches the C function name `BitStream_Init`.
    pub fn init(buf: &'a mut [u8]) -> Self {
        Self::new(buf)
    }

    /// Create a `BitStream` over `buf` **without** zeroing it (for reading back
    /// already-encoded data).  This is the counterpart to `new` for decode
    /// paths — in C the same `BitStream_AttachBuffer` serves both purposes.
    pub fn attach_buffer_no_zero(buf: &'a mut [u8]) -> Self {
        BitStream {
            count: buf.len() as i64,
            buf,
            current_byte: 0,
            current_bit: 0,
            push_data: None,
            push_data_prm: std::ptr::null_mut(),
            fetch_data: None,
            fetch_data_prm: std::ptr::null_mut(),
        }
    }

    /// Re-attach a new buffer and reset the cursor (no zeroing, no callbacks).
    /// Mirrors C `BitStream_AttachBuffer`.
    pub fn attach_buffer(&mut self, buf: &'a mut [u8]) {
        self.count = buf.len() as i64;
        self.buf = buf;
        self.current_byte = 0;
        self.current_bit = 0;
        self.push_data = None;
        self.push_data_prm = std::ptr::null_mut();
        self.fetch_data = None;
        self.fetch_data_prm = std::ptr::null_mut();
    }

    /// Number of bytes consumed/produced so far (rounds up to the next byte).
    pub fn get_length(&self) -> Asn1SccSint {
        let mut ret = self.current_byte;
        if self.current_bit > 0 {
            ret += 1;
        }
        ret
    }

    // ── streaming helpers ────────────────────────────────────────────────

    /// If the cursor reached the end of the buffer and a flush callback is
    /// registered, invoke it and reset the cursor.  Mirrors C
    /// `bitstream_push_data_if_required`.
    fn push_data_if_required(&mut self) {
        if self.current_byte == self.count {
            if let Some(fnc) = self.push_data {
                fnc(self, self.push_data_prm);
                self.current_byte = 0;
            }
        }
    }

    /// If the cursor reached the end of the buffer and a refill callback is
    /// registered, invoke it and reset the cursor.  Mirrors C
    /// `bitstream_fetch_data_if_required`.
    fn fetch_data_if_required(&mut self) {
        if self.current_byte == self.count {
            if let Some(fnc) = self.fetch_data {
                fnc(self, self.fetch_data_prm);
                self.current_byte = 0;
            }
        }
    }

    // ── single-bit operations ────────────────────────────────────────────

    /// Append a single bit.  Mirrors C `BitStream_AppendBit`.
    pub fn append_bit(&mut self, v: bool) {
        let idx = self.current_bit as usize;
        if v {
            self.buf[self.current_byte as usize] |= MASKS[idx];
        } else {
            self.buf[self.current_byte as usize] &= !MASKS[idx];
        }
        if self.current_bit < 7 {
            self.current_bit += 1;
        } else {
            self.current_bit = 0;
            self.current_byte += 1;
            self.push_data_if_required();
        }
        debug_assert!(
            self.current_byte * 8 + self.current_bit as i64 <= self.count * 8
        );
    }

    /// Append a `1` bit.  Mirrors C `BitStream_AppendBitOne`.
    pub fn append_bit_one(&mut self) {
        let idx = self.current_bit as usize;
        self.buf[self.current_byte as usize] |= MASKS[idx];
        if self.current_bit < 7 {
            self.current_bit += 1;
        } else {
            self.current_bit = 0;
            self.current_byte += 1;
            self.push_data_if_required();
        }
        debug_assert!(
            self.current_byte * 8 + self.current_bit as i64 <= self.count * 8
        );
    }

    /// Append a `0` bit.  Mirrors C `BitStream_AppendBitZero`.
    pub fn append_bit_zero(&mut self) {
        let idx = self.current_bit as usize;
        self.buf[self.current_byte as usize] &= !MASKS[idx];
        if self.current_bit < 7 {
            self.current_bit += 1;
        } else {
            self.current_bit = 0;
            self.current_byte += 1;
            self.push_data_if_required();
        }
        debug_assert!(
            self.current_byte * 8 + self.current_bit as i64 <= self.count * 8
        );
    }

    /// Read a single bit, returning `false` if the stream is exhausted.
    /// Mirrors C `BitStream_ReadBit`.
    pub fn read_bit(&mut self) -> (bool, bool) {
        // returns (value, success)
        let idx = self.current_bit as usize;
        let v = self.buf[self.current_byte as usize] & MASKS[idx];
        if self.current_bit < 7 {
            self.current_bit += 1;
        } else {
            self.current_bit = 0;
            self.current_byte += 1;
            self.fetch_data_if_required();
        }
        let success = self.current_byte * 8 + self.current_bit as i64 <= self.count * 8;
        (v != 0, success)
    }

    /// Peek the current bit without advancing the cursor.
    /// Mirrors C `BitStream_PeekBit`.
    pub fn peek_bit(&self) -> bool {
        let idx = self.current_bit as usize;
        self.buf[self.current_byte as usize] & MASKS[idx] != 0
    }

    // ── multi-bit / byte operations ──────────────────────────────────────

    /// Append `nbits` zero bits.  Mirrors C `BitStream_AppendNBitZero`.
    pub fn append_n_bit_zero(&mut self, nbits: i32) {
        let total_bits = self.current_bit + nbits;
        let total_bytes = total_bits / 8;
        self.current_bit = total_bits % 8;
        if self.current_byte + total_bytes as i64 <= self.count {
            self.current_byte += total_bytes as i64;
            self.push_data_if_required();
        } else {
            let extra_bytes = self.current_byte + total_bytes as i64 - self.count;
            self.current_byte = self.count;
            self.push_data_if_required();
            self.current_byte = extra_bytes;
        }
    }

    /// Append `nbits` one bits.  Mirrors C `BitStream_AppendNBitOne`.
    pub fn append_n_bit_one(&mut self, mut nbits: i32) {
        while nbits >= 8 {
            self.append_byte(0xFF, false);
            nbits -= 8;
        }
        for _ in 0..nbits {
            self.append_bit_one();
        }
    }

    /// Append a full byte (optionally negated).  Mirrors C
    /// `BitStream_AppendByte`.
    pub fn append_byte(&mut self, mut v: u8, negate: bool) {
        let cb = self.current_bit;
        let ncb = 8 - cb;
        if negate {
            v = !v;
        }
        let mask = !MASKSB[ncb as usize];

        let cur = self.current_byte as usize;
        self.buf[cur] &= mask;
        self.buf[cur] |= v >> cb;
        self.current_byte += 1;
        self.push_data_if_required();
        debug_assert!(
            self.current_byte * 8 + self.current_bit as i64 <= self.count * 8
        );

        if cb > 0 {
            let cur2 = self.current_byte as usize;
            let nmask = !mask;
            self.buf[cur2] &= nmask;
            self.buf[cur2] |= v << ncb;
        }
    }

    /// Append a full byte, returning `false` on overflow.  Mirrors C
    /// `BitStream_AppendByte0`.
    pub fn append_byte0(&mut self, v: u8) -> bool {
        let cb = self.current_bit;
        let ncb = 8 - cb;
        let mask = !MASKSB[ncb as usize];

        let cur = self.current_byte as usize;
        self.buf[cur] &= mask;
        self.buf[cur] |= v >> cb;
        self.current_byte += 1;
        self.push_data_if_required();

        if cb > 0 {
            if self.current_byte >= self.count {
                return false;
            }
            let cur2 = self.current_byte as usize;
            let nmask = !mask;
            self.buf[cur2] &= nmask;
            self.buf[cur2] |= v << ncb;
        }
        true
    }

    /// Read a full byte, returning `false` on overflow.  Mirrors C
    /// `BitStream_ReadByte`.
    pub fn read_byte(&mut self) -> (u8, bool) {
        let cb = self.current_bit;
        let required_bytes = if cb > 0 { 2 } else { 1 };
        let available_bytes = self.count - self.current_byte;
        if available_bytes < required_bytes {
            return (0, false);
        }
        let cur = self.current_byte as usize;
        let mut v = self.buf[cur] << cb;
        self.current_byte += 1;
        self.fetch_data_if_required();
        if cb > 0 {
            let ncb = 8 - cb;
            let cur2 = self.current_byte as usize;
            v |= self.buf[cur2] >> ncb;
        }
        (v, true)
    }

    /// Append a partial byte of `nbits` bits (1..7), optionally negated.
    /// Mirrors C `BitStream_AppendPartialByte`.
    pub fn append_partial_byte(&mut self, mut v: u8, nbits: u8, negate: bool) {
        let cb = self.current_bit;
        let total_bits = cb + nbits as i32;
        let ncb = 8 - cb;
        if negate {
            v = MASKSB[nbits as usize] & !v;
        }
        let mask1 = !MASKSB[ncb as usize];

        if total_bits <= 8 {
            let mask2 = MASKSB[(8 - total_bits) as usize];
            let mask = mask1 | mask2;
            let cur = self.current_byte as usize;
            self.buf[cur] &= mask;
            self.buf[cur] |= v << (8 - total_bits);
            self.current_bit += nbits as i32;
            if self.current_bit == 8 {
                self.current_bit = 0;
                self.current_byte += 1;
                self.push_data_if_required();
            }
        } else {
            let total_bits_for_next_byte = total_bits - 8;
            let cur = self.current_byte as usize;
            self.buf[cur] &= mask1;
            self.buf[cur] |= v >> total_bits_for_next_byte;
            self.current_byte += 1;
            self.push_data_if_required();
            let mask = !MASKSB[(8 - total_bits_for_next_byte) as usize];
            let cur2 = self.current_byte as usize;
            self.buf[cur2] &= mask;
            self.buf[cur2] |= v << (8 - total_bits_for_next_byte);
            self.current_bit = total_bits_for_next_byte;
        }
        debug_assert!(
            self.current_byte * 8 + self.current_bit as i64 <= self.count * 8
        );
    }

    /// Read a partial byte of `nbits` bits (1..7).  Mirrors C
    /// `BitStream_ReadPartialByte`.
    pub fn read_partial_byte(&mut self, nbits: u8) -> (u8, bool) {
        let cb = self.current_bit;
        let total_bits = cb + nbits as i32;
        let mut v: u8;

        if total_bits <= 8 {
            let cur = self.current_byte as usize;
            v = (self.buf[cur] >> (8 - total_bits)) & MASKSB[nbits as usize];
            self.current_bit += nbits as i32;
            if self.current_bit == 8 {
                self.current_bit = 0;
                self.current_byte += 1;
                self.fetch_data_if_required();
            }
        } else {
            let total_bits_for_next_byte = total_bits - 8;
            let cur = self.current_byte as usize;
            v = self.buf[cur] << total_bits_for_next_byte;
            self.current_byte += 1;
            self.fetch_data_if_required();
            let cur2 = self.current_byte as usize;
            v |= self.buf[cur2] >> (8 - total_bits_for_next_byte);
            v &= MASKSB[nbits as usize];
            self.current_bit = total_bits_for_next_byte;
        }
        let success = self.current_byte * 8 + self.current_bit as i64 <= self.count * 8;
        (v, success)
    }

    /// Append `nbits` from `src_buffer` (MSB-first).  Mirrors C
    /// `BitStream_AppendBits`.
    pub fn append_bits(&mut self, src_buffer: &[u8], nbits: i32) {
        let bytes_to_encode = (nbits / 8) as usize;
        let remaining_bits = (nbits % 8) as i32;

        self.octet_string_encode_no_length(&src_buffer[..bytes_to_encode], bytes_to_encode);

        if remaining_bits > 0 {
            let last_byte = src_buffer[bytes_to_encode] >> (8 - remaining_bits);
            self.append_partial_byte(last_byte, remaining_bits as u8, false);
        }
    }

    /// Read `nbits` bits into `buff_to_write` (MSB-first).  Mirrors C
    /// `BitStream_ReadBits`.
    pub fn read_bits(&mut self, buff_to_write: &mut [u8], nbits: i32) -> bool {
        let bytes_to_read = (nbits / 8) as usize;
        let remaining_bits = (nbits % 8) as i32;

        let ret = self.octet_string_decode_no_length(&mut buff_to_write[..bytes_to_read], bytes_to_read);

        if ret && remaining_bits > 0 {
            let (mut b, ok) = self.read_partial_byte(remaining_bits as u8);
            if !ok {
                return false;
            }
            b <<= 8 - remaining_bits;
            buff_to_write[bytes_to_read] = b;
        }
        ret
    }

    /// Append a byte array, returning `false` on overflow.  Mirrors C
    /// `BitStream_AppendByteArray`.
    pub fn append_byte_array(&mut self, arr: &[u8]) -> bool {
        let arr_len = arr.len();
        let cb = self.current_bit;
        let ncb = 8 - cb;
        let mask = !MASKSB[ncb as usize];
        let nmask = !mask;

        if (self.current_byte + arr_len as i64) * 8 + cb as i64 > self.count * 8 {
            return false;
        }

        if arr_len > 0 {
            let v = arr[0];
            let cur = self.current_byte as usize;
            self.buf[cur] &= mask;
            self.buf[cur] |= v >> cb;
            self.current_byte += 1;
            self.push_data_if_required();

            let cur2 = self.current_byte as usize;
            self.buf[cur2] &= nmask;
            self.buf[cur2] |= v << ncb;
        }

        for i in 1..arr_len.saturating_sub(1) {
            let v = arr[i];
            let v1 = v >> cb;
            let v2 = v << ncb;
            let cur = self.current_byte as usize;
            self.buf[cur] |= v1;
            self.current_byte += 1;
            self.push_data_if_required();
            let cur2 = self.current_byte as usize;
            self.buf[cur2] |= v2;
        }
        if arr_len > 1 {
            let v = arr[arr_len - 1];
            let cur = self.current_byte as usize;
            self.buf[cur] &= mask;
            self.buf[cur] |= v >> cb;
            self.current_byte += 1;
            self.push_data_if_required();
            if cb > 0 {
                let cur2 = self.current_byte as usize;
                self.buf[cur2] &= nmask;
                self.buf[cur2] |= v << ncb;
            }
        }
        true
    }

    /// Read a byte array, returning `false` on overflow.  Mirrors C
    /// `BitStream_ReadByteArray`.
    pub fn read_byte_array(&mut self, arr: &mut [u8]) -> bool {
        let arr_len = arr.len();
        let cb = self.current_bit;
        let ncb = 8 - cb;

        if (self.current_byte + arr_len as i64) * 8 + cb as i64 > self.count * 8 {
            return false;
        }

        for i in 0..arr_len {
            let rb_idx = (self.current_byte + i as i64) as usize;
            let rb_next_idx = (self.current_byte + i as i64 + 1) as usize;
            // Note: in streaming mode the buffer may wrap, but in non-streaming
            // mode we have the whole buffer available.
            arr[i] = self.buf[rb_idx] << cb;
            if rb_next_idx < self.count as usize {
                arr[i] |= self.buf[rb_next_idx] >> ncb;
            }
        }
        self.current_byte += arr_len as i64;
        true
    }

    // ── non-negative integer helpers ─────────────────────────────────────

    fn encode_non_negative_integer32_neg(&mut self, v: Asn1SccUint32, negate: bool) {
        if v == 0 {
            return;
        }
        let mut cc: i32;
        let mut cur_mask: Asn1SccUint32;
        if v < 0x100 {
            cc = 8;
            cur_mask = 0x80;
        } else if v < 0x1_0000 {
            cc = 16;
            cur_mask = 0x8000;
        } else if v < 0x0100_0000 {
            cc = 24;
            cur_mask = 0x800000;
        } else {
            cc = 32;
            cur_mask = 0x8000_0000;
        }
        while (v & cur_mask) == 0 {
            cur_mask >>= 1;
            cc -= 1;
        }
        let pbits = cc % 8;
        if pbits > 0 {
            cc -= pbits;
            self.append_partial_byte((v >> cc) as u8, pbits as u8, negate);
        }
        while cc > 0 {
            let t1 = v & MASKS2[(cc >> 3) as usize];
            cc -= 8;
            self.append_byte((t1 >> cc) as u8, negate);
        }
    }

    /// Encode a non-negative integer using the minimum number of bits.
    /// Mirrors C `BitStream_EncodeNonNegativeInteger`.
    pub fn encode_non_negative_integer(&mut self, v: Asn1SccUint) {
        if v < 0x1_0000_0000 {
            self.encode_non_negative_integer32_neg(v as Asn1SccUint32, false);
        } else {
            let hi = (v >> 32) as Asn1SccUint32;
            let lo = v as Asn1SccUint32;
            self.encode_non_negative_integer32_neg(hi, false);
            let n_bits = get_number_of_bits_for_non_negative_integer(lo as Asn1SccUint);
            self.append_n_bit_zero(32 - n_bits);
            self.encode_non_negative_integer32_neg(lo, false);
        }
    }

    /// Encode a non-negative integer, optionally negating the bits.
    /// Mirrors C `BitStream_EncodeNonNegativeIntegerNeg`.
    pub fn encode_non_negative_integer_neg(&mut self, v: Asn1SccUint, negate: bool) {
        if v < 0x1_0000_0000 {
            self.encode_non_negative_integer32_neg(v as Asn1SccUint32, negate);
        } else {
            let hi = (v >> 32) as Asn1SccUint32;
            let mut lo = v as Asn1SccUint32;
            self.encode_non_negative_integer32_neg(hi, negate);
            if negate {
                lo = !lo;
            }
            let n_bits = get_number_of_bits_for_non_negative_integer(lo as Asn1SccUint);
            self.append_n_bit_zero(32 - n_bits);
            self.encode_non_negative_integer32_neg(lo, false);
        }
    }

    /// Decode a non-negative integer of `n_bits` bits.
    /// Mirrors C `BitStream_DecodeNonNegativeInteger`.
    pub fn decode_non_negative_integer(&mut self, n_bits: i32) -> (Asn1SccUint, bool) {
        if n_bits <= 32 {
            let (lo, ok) = self.decode_non_negative_integer32_neg(n_bits);
            return (lo as Asn1SccUint, ok);
        }
        let (hi, ok1) = self.decode_non_negative_integer32_neg(32);
        let (lo, ok2) = self.decode_non_negative_integer32_neg(n_bits - 32);
        let mut v = hi as Asn1SccUint;
        v <<= n_bits - 32;
        v |= lo as Asn1SccUint;
        (v, ok1 && ok2)
    }

    fn decode_non_negative_integer32_neg(&mut self, mut n_bits: i32) -> (Asn1SccUint32, bool) {
        let mut v: Asn1SccUint32 = 0;
        while n_bits >= 8 {
            v <<= 8;
            let (b, ok) = self.read_byte();
            if !ok {
                return (0, false);
            }
            v |= b as Asn1SccUint32;
            n_bits -= 8;
        }
        if n_bits > 0 {
            v <<= n_bits;
            let (b, ok) = self.read_partial_byte(n_bits as u8);
            if !ok {
                return (0, false);
            }
            v |= b as Asn1SccUint32;
        }
        (v, true)
    }

    // ── constrained / unconstrained / semi-constrained integers ──────────

    /// Encode a whole number within a known [min, max] range.
    /// Mirrors C `BitStream_EncodeConstraintWholeNumber`.
    pub fn encode_constraint_whole_number(&mut self, v: Asn1SccSint, min: Asn1SccSint, max: Asn1SccSint) {
        debug_assert!(min <= max);
        let range = (max as u64).wrapping_sub(min as u64);
        if range == 0 {
            return;
        }
        let val_offset = (v as u64).wrapping_sub(min as u64);
        let n_range_bits = get_number_of_bits_for_non_negative_integer(range);
        let n_bits = get_number_of_bits_for_non_negative_integer(val_offset);
        self.append_n_bit_zero(n_range_bits - n_bits);
        self.encode_non_negative_integer(val_offset);
    }

    /// Decode a whole number within a known [min, max] range.
    /// Mirrors C `BitStream_DecodeConstraintWholeNumber`.
    pub fn decode_constraint_whole_number(&mut self, min: Asn1SccSint, max: Asn1SccSint) -> (Asn1SccSint, bool) {
        if min > max {
            return (0, false);
        }
        let range = (max as u64).wrapping_sub(min as u64);
        if range == 0 {
            return (min, true);
        }
        let n_range_bits = get_number_of_bits_for_non_negative_integer(range);
        let (uv, ok) = self.decode_non_negative_integer(n_range_bits);
        if !ok {
            return (0, false);
        }
        if uv > range {
            return (0, false);
        }
        (uv.wrapping_add(min as u64) as Asn1SccSint, true)
    }

    /// Encode a positive whole number within a known [min, max] range.
    /// Mirrors C `BitStream_EncodeConstraintPosWholeNumber`.
    pub fn encode_constraint_pos_whole_number(&mut self, v: Asn1SccUint, min: Asn1SccUint, max: Asn1SccUint) {
        debug_assert!(min <= v && v <= max);
        let range = max - min;
        if range == 0 {
            return;
        }
        let n_range_bits = get_number_of_bits_for_non_negative_integer(range);
        let n_bits = get_number_of_bits_for_non_negative_integer(v - min);
        self.append_n_bit_zero(n_range_bits - n_bits);
        self.encode_non_negative_integer(v - min);
    }

    /// Decode a positive whole number within a known [min, max] range.
    /// Mirrors C `BitStream_DecodeConstraintPosWholeNumber`.
    pub fn decode_constraint_pos_whole_number(&mut self, min: Asn1SccUint, max: Asn1SccUint) -> (Asn1SccUint, bool) {
        if min > max {
            return (0, false);
        }
        let range = max - min;
        if range == 0 {
            return (min, true);
        }
        let n_range_bits = get_number_of_bits_for_non_negative_integer(range);
        let (uv, ok) = self.decode_non_negative_integer(n_range_bits);
        if !ok {
            return (0, false);
        }
        if uv > range {
            return (0, false);
        }
        (uv + min, true)
    }

    /// Encode an unconstrained whole number (length-prefixed).
    /// Mirrors C `BitStream_EncodeUnConstraintWholeNumber`.
    pub fn encode_unconstrained_whole_number(&mut self, v: Asn1SccSint) {
        let n_bytes = get_length_in_bytes_of_sint(v);
        self.encode_constraint_whole_number(n_bytes as Asn1SccSint, 0, 255);
        if v >= 0 {
            self.append_n_bit_zero(
                n_bytes * 8 - get_number_of_bits_for_non_negative_integer(v as Asn1SccUint),
            );
            self.encode_non_negative_integer(v as Asn1SccUint);
        } else {
            self.append_n_bit_one(
                n_bytes * 8 - get_number_of_bits_for_non_negative_integer(v.unsigned_abs() - 1),
            );
            self.encode_non_negative_integer_neg(v.unsigned_abs() - 1, true);
        }
    }

    /// Decode an unconstrained whole number (length-prefixed).
    /// Mirrors C `BitStream_DecodeUnConstraintWholeNumber`.
    pub fn decode_unconstrained_whole_number(&mut self) -> (Asn1SccSint, bool) {
        let (n_bytes_signed, ok) = self.decode_constraint_whole_number(0, 255);
        if !ok {
            return (0, false);
        }
        let n_bytes = n_bytes_signed as i32;
        if n_bytes > WORD_SIZE {
            return (0, false);
        }
        let val_is_negative = self.peek_bit();
        let mut v: Asn1SccSint = if val_is_negative { -1 } else { 0 };
        for _ in 0..n_bytes {
            let (b, ok2) = self.read_byte();
            if !ok2 {
                return (0, false);
            }
            v = (v << 8) | b as i64;
        }
        (v, true)
    }

    /// Encode a semi-constrained whole number (min known, max unbounded).
    /// Mirrors C `BitStream_EncodeSemiConstraintWholeNumber`.
    pub fn encode_semi_constraint_whole_number(&mut self, v: Asn1SccSint, min: Asn1SccSint) {
        debug_assert!(v >= min);
        let val_offset = (v as u64).wrapping_sub(min as u64);
        let n_bytes = get_length_in_bytes_of_uint(val_offset);
        self.encode_constraint_whole_number(n_bytes as Asn1SccSint, 0, 255);
        self.append_n_bit_zero(
            n_bytes * 8 - get_number_of_bits_for_non_negative_integer(val_offset),
        );
        self.encode_non_negative_integer(val_offset);
    }

    /// Decode a semi-constrained whole number (min known, max unbounded).
    /// Mirrors C `BitStream_DecodeSemiConstraintWholeNumber`.
    pub fn decode_semi_constraint_whole_number(&mut self, min: Asn1SccSint) -> (Asn1SccSint, bool) {
        let (n_bytes_signed, ok) = self.decode_constraint_whole_number(0, 255);
        if !ok {
            return (0, false);
        }
        let n_bytes = n_bytes_signed as i32;
        if n_bytes > WORD_SIZE {
            return (0, false);
        }
        let mut v: Asn1SccSint = 0;
        for _ in 0..n_bytes {
            let (b, ok2) = self.read_byte();
            if !ok2 {
                return (0, false);
            }
            v = (v << 8) | b as i64;
        }
        (v.wrapping_add(min), true)
    }

    /// Encode a semi-constrained positive whole number.
    /// Mirrors C `BitStream_EncodeSemiConstraintPosWholeNumber`.
    pub fn encode_semi_constraint_pos_whole_number(&mut self, v: Asn1SccUint, min: Asn1SccUint) {
        debug_assert!(v >= min);
        let n_bytes = get_length_in_bytes_of_uint(v - min);
        self.encode_constraint_whole_number(n_bytes as Asn1SccSint, 0, 255);
        self.append_n_bit_zero(
            n_bytes * 8 - get_number_of_bits_for_non_negative_integer(v - min),
        );
        self.encode_non_negative_integer(v - min);
    }

    /// Decode a semi-constrained positive whole number.
    /// Mirrors C `BitStream_DecodeSemiConstraintPosWholeNumber`.
    pub fn decode_semi_constraint_pos_whole_number(&mut self, min: Asn1SccUint) -> (Asn1SccUint, bool) {
        let (n_bytes_signed, ok) = self.decode_constraint_whole_number(0, 255);
        if !ok {
            return (0, false);
        }
        let n_bytes = n_bytes_signed as i32;
        if n_bytes > WORD_SIZE {
            return (0, false);
        }
        let mut v: Asn1SccUint = 0;
        for _ in 0..n_bytes {
            let (b, ok2) = self.read_byte();
            if !ok2 {
                return (0, false);
            }
            v = (v << 8) | b as u64;
        }
        (v + min, true)
    }

    // ── typed constrained integer variants ──────────────────────────────

    /// Decode a constrained `i8`.  Mirrors C
    /// `BitStream_DecodeConstraintWholeNumberInt8`.
    pub fn decode_constraint_whole_number_i8(&mut self, min: i8, max: i8) -> (i8, bool) {
        let (bv, ok) = self.decode_constraint_whole_number(min as Asn1SccSint, max as Asn1SccSint);
        (bv as i8, ok)
    }

    /// Decode a constrained `i16`.  Mirrors C
    /// `BitStream_DecodeConstraintWholeNumberInt16`.
    pub fn decode_constraint_whole_number_i16(&mut self, min: i16, max: i16) -> (i16, bool) {
        let (bv, ok) = self.decode_constraint_whole_number(min as Asn1SccSint, max as Asn1SccSint);
        (bv as i16, ok)
    }

    /// Decode a constrained `i32`.  Mirrors C
    /// `BitStream_DecodeConstraintWholeNumberInt32`.
    pub fn decode_constraint_whole_number_i32(&mut self, min: i32, max: i32) -> (i32, bool) {
        let (bv, ok) = self.decode_constraint_whole_number(min as Asn1SccSint, max as Asn1SccSint);
        (bv as i32, ok)
    }

    /// Decode a constrained `u8`.  Mirrors C
    /// `BitStream_DecodeConstraintPosWholeNumberUInt8`.
    pub fn decode_constraint_pos_whole_number_u8(&mut self, min: u8, max: u8) -> (u8, bool) {
        let (bv, ok) = self.decode_constraint_pos_whole_number(min as Asn1SccUint, max as Asn1SccUint);
        (bv as u8, ok)
    }

    /// Decode a constrained `u16`.  Mirrors C
    /// `BitStream_DecodeConstraintPosWholeNumberUInt16`.
    pub fn decode_constraint_pos_whole_number_u16(&mut self, min: u16, max: u16) -> (u16, bool) {
        let (bv, ok) = self.decode_constraint_pos_whole_number(min as Asn1SccUint, max as Asn1SccUint);
        (bv as u16, ok)
    }

    /// Decode a constrained `u32`.  Mirrors C
    /// `BitStream_DecodeConstraintPosWholeNumberUInt32`.
    pub fn decode_constraint_pos_whole_number_u32(&mut self, min: u32, max: u32) -> (u32, bool) {
        let (bv, ok) = self.decode_constraint_pos_whole_number(min as Asn1SccUint, max as Asn1SccUint);
        (bv as u32, ok)
    }

    // ── real encoding ────────────────────────────────────────────────────

    /// Decompose a double into mantissa and (biased) exponent.
    /// Mirrors C `CalculateMantissaAndExponent`.
    pub fn calculate_mantissa_and_exponent(d: f64) -> (i32, Asn1SccUint64) {
        let bits = d.to_bits();
        let exponent = ((bits & EXPO_BIT_MASK) >> 52) as i64 - 1023 - 52;
        let mut mantissa = bits & MANT_BIT_MASK;
        mantissa |= MANTISA_EXTRA_BIT;
        (exponent as i32, mantissa)
    }

    /// Reconstruct a double from mantissa and exponent.
    /// Mirrors C `GetDoubleByMantissaAndExp`.
    pub fn get_double_by_mantissa_and_exp(mantissa: Asn1SccUint, exponent: i32) -> f64 {
        if mantissa == 0 {
            return 0.0;
        }
        let mut ret: f64 = 1.0;
        if exponent >= 0 {
            let mut e = exponent;
            while e > 0 {
                ret *= 2.0;
                e -= 1;
            }
            mantissa as f64 * ret
        } else {
            let mut e = -exponent;
            while e > 0 {
                ret *= 2.0;
                e -= 1;
            }
            mantissa as f64 / ret
        }
    }

    /// Encode a real value using ASN.1 binary encoding.
    /// Mirrors C `BitStream_EncodeReal`.
    pub fn encode_real(&mut self, v: f64) {
        let mut header: u8 = 0x80;

        if v.is_nan() {
            self.encode_constraint_whole_number(1, 0, 0xFF);
            self.encode_constraint_whole_number(0x42, 0, 0xFF);
            return;
        }
        if v == 0.0 {
            self.encode_constraint_whole_number(0, 0, 0xFF);
            return;
        }
        if v.is_sign_negative() && v == 0.0 {
            // negative zero
            self.encode_constraint_whole_number(1, 0, 0xFF);
            self.encode_constraint_whole_number(0x43, 0, 0xFF);
            return;
        }
        if v.is_infinite() && v > 0.0 {
            self.encode_constraint_whole_number(1, 0, 0xFF);
            self.encode_constraint_whole_number(0x40, 0, 0xFF);
            return;
        }
        if v.is_infinite() && v < 0.0 {
            self.encode_constraint_whole_number(1, 0, 0xFF);
            self.encode_constraint_whole_number(0x41, 0, 0xFF);
            return;
        }

        let mut v = v;
        if v < 0.0 {
            header |= 0x40;
            v = -v;
        }

        let (exponent, mantissa) = Self::calculate_mantissa_and_exponent(v);
        let n_exp_len = get_length_in_bytes_of_sint(exponent as Asn1SccSint);
        let n_man_len = get_length_in_bytes_of_uint(mantissa);
        debug_assert!(n_exp_len <= 3);
        if n_exp_len == 2 {
            header |= 1;
        } else if n_exp_len == 3 {
            header |= 2;
        }

        self.encode_constraint_whole_number((1 + n_exp_len + n_man_len) as Asn1SccSint, 0, 0xFF);
        self.encode_constraint_whole_number(header as Asn1SccSint, 0, 0xFF);

        if exponent >= 0 {
            self.append_n_bit_zero(
                n_exp_len * 8 - get_number_of_bits_for_non_negative_integer(exponent as Asn1SccUint),
            );
            self.encode_non_negative_integer(exponent as Asn1SccUint);
        } else {
            self.append_n_bit_one(
                n_exp_len * 8
                    - get_number_of_bits_for_non_negative_integer(((-exponent) - 1) as Asn1SccUint),
            );
            self.encode_non_negative_integer_neg(((-exponent) - 1) as Asn1SccUint, true);
        }

        self.append_n_bit_zero(
            n_man_len * 8 - get_number_of_bits_for_non_negative_integer(mantissa),
        );
        self.encode_non_negative_integer(mantissa);
    }

    /// Decode a real value.
    /// Mirrors C `BitStream_DecodeReal`.
    pub fn decode_real(&mut self) -> (f64, bool) {
        let (length_byte, ok) = self.read_byte();
        if !ok {
            return (0.0, false);
        }
        let length = length_byte as i32;
        if length == 0 {
            return (0.0, true);
        }
        let (header_byte, ok2) = self.read_byte();
        if !ok2 {
            return (0.0, false);
        }
        let header = header_byte;
        if header == 0x40 {
            return (f64::INFINITY, true);
        }
        if header == 0x41 {
            return (f64::NEG_INFINITY, true);
        }
        if header == 0x42 {
            return (f64::NAN, true);
        }
        if header == 0x43 {
            return (-0.0, true);
        }
        self.decode_real_as_binary_encoding(length - 1, header)
    }

    fn decode_real_as_binary_encoding(&mut self, mut length: i32, header: u8) -> (f64, bool) {
        let mut sign = 1;
        let mut exp_factor = 1;
        if header & 0x40 != 0 {
            sign = -1;
        }
        if header & 0x10 != 0 {
            exp_factor = 3;
        } else if header & 0x20 != 0 {
            exp_factor = 4;
        }
        let f = (header & 0x0C) >> 2;
        let factor: u32 = 1 << f;
        let exp_len = (header & 0x03) as i32 + 1;

        if exp_len > length {
            return (0.0, false);
        }
        let exp_is_negative = self.peek_bit();
        let mut exponent: i32 = if exp_is_negative { -1 } else { 0 };
        for _ in 0..exp_len {
            let (b, ok) = self.read_byte();
            if !ok {
                return (0.0, false);
            }
            exponent = (exponent << 8) | b as i32;
        }
        length -= exp_len;

        let mut n: Asn1SccUint = 0;
        for _ in 0..length {
            let (b, ok) = self.read_byte();
            if !ok {
                return (0.0, false);
            }
            n = (n << 8) | b as Asn1SccUint;
        }

        let mut v = Self::get_double_by_mantissa_and_exp(n * factor as Asn1SccUint, exp_factor * exponent);
        if sign < 0 {
            v = -v;
        }
        (v, true)
    }

    /// Decode a real value as `f32`.
    /// Mirrors C `BitStream_DecodeReal_fp32`.
    pub fn decode_real_fp32(&mut self) -> (f32, bool) {
        let (rv, ok) = self.decode_real();
        (rv as f32, ok)
    }

    // ── octet string encode/decode ──────────────────────────────────────

    /// Encode an octet string without a length prefix.
    /// Mirrors C `BitStream_EncodeOctetString_no_length`.
    pub fn octet_string_encode_no_length(&mut self, arr: &[u8], n_count: usize) -> bool {
        let cb = self.current_bit;
        if cb == 0 {
            if self.current_byte + n_count as i64 > self.count {
                return false;
            }
            let cur = self.current_byte as usize;
            self.buf[cur..cur + n_count].copy_from_slice(&arr[..n_count]);
            self.current_byte += n_count as i64;
            self.push_data_if_required();
            true
        } else {
            self.append_byte_array(&arr[..n_count])
        }
    }

    /// Decode an octet string without a length prefix.
    /// Mirrors C `BitStream_DecodeOctetString_no_length`.
    pub fn octet_string_decode_no_length(&mut self, arr: &mut [u8], n_count: usize) -> bool {
        let cb = self.current_bit;
        if cb == 0 {
            if self.current_byte + n_count as i64 > self.count {
                return false;
            }
            let cur = self.current_byte as usize;
            arr[..n_count].copy_from_slice(&self.buf[cur..cur + n_count]);
            self.current_byte += n_count as i64;
            self.fetch_data_if_required();
            true
        } else {
            self.read_byte_array(&mut arr[..n_count])
        }
    }

    /// Encode an octet string with fragmentation (for large sizes).
    /// Mirrors C `BitStream_EncodeOctetString_fragmentation`.
    pub fn octet_string_encode_fragmentation(&mut self, arr: &[u8], n_count: i32) -> bool {
        let mut n_remaining = n_count;
        let mut n_cur_block_size: i32;
        let mut n_cur_offset: i32 = 0;
        let mut ret = n_count >= 0;

        while n_remaining >= 0x4000 && ret {
            if n_remaining >= 0x10000 {
                n_cur_block_size = 0x10000;
                self.encode_constraint_whole_number(0xC4, 0, 0xFF);
            } else if n_remaining >= 0xC000 {
                n_cur_block_size = 0xC000;
                self.encode_constraint_whole_number(0xC3, 0, 0xFF);
            } else if n_remaining >= 0x8000 {
                n_cur_block_size = 0x8000;
                self.encode_constraint_whole_number(0xC2, 0, 0xFF);
            } else {
                n_cur_block_size = 0x4000;
                self.encode_constraint_whole_number(0xC1, 0, 0xFF);
            }
            for i in n_cur_offset..n_cur_block_size + n_cur_offset {
                ret = self.append_byte0(arr[i as usize]);
                if !ret {
                    break;
                }
            }
            n_cur_offset += n_cur_block_size;
            n_remaining -= n_cur_block_size;
        }
        if ret {
            if n_remaining <= 0x7F {
                self.encode_constraint_whole_number(n_remaining as Asn1SccSint, 0, 0xFF);
            } else {
                self.append_bit(true);
                self.encode_constraint_whole_number(n_remaining as Asn1SccSint, 0, 0x7FFF);
            }
            for i in n_cur_offset..n_cur_offset + n_remaining {
                ret = self.append_byte0(arr[i as usize]);
                if !ret {
                    break;
                }
            }
        }
        ret
    }

    /// Decode an octet string with fragmentation (for large sizes).
    /// Mirrors C `BitStream_DecodeOctetString_fragmentation`.
    pub fn octet_string_decode_fragmentation(&mut self, arr: &mut [u8], asn1_size_max: Asn1SccSint) -> (i32, bool) {
        let mut ret = true;
        let mut n_length_tmp: Asn1SccSint = 0;
        let mut n_remaining: Asn1SccSint;
        let mut n_cur_block_size: Asn1SccSint = 0;
        let mut n_cur_offset: Asn1SccSint = 0;

        let (n_rem_signed, ok) = self.decode_constraint_whole_number(0, 0xFF);
        if !ok {
            return (0, false);
        }
        n_remaining = n_rem_signed;
        while ret && (n_remaining & 0xC0) == 0xC0 {
            if n_remaining == 0xC4 {
                n_cur_block_size = 0x10000;
            } else if n_remaining == 0xC3 {
                n_cur_block_size = 0xC000;
            } else if n_remaining == 0xC2 {
                n_cur_block_size = 0x8000;
            } else if n_remaining == 0xC1 {
                n_cur_block_size = 0x4000;
            } else {
                ret = false;
            }
            if ret {
                ret = n_cur_offset + n_cur_block_size <= asn1_size_max;
                for i in n_cur_offset..n_cur_offset + n_cur_block_size {
                    if !ret {
                        break;
                    }
                    let (b, ok2) = self.read_byte();
                    if !ok2 {
                        ret = false;
                        break;
                    }
                    arr[i as usize] = b;
                }
                if ret {
                    n_length_tmp += n_cur_block_size;
                    n_cur_offset += n_cur_block_size;
                    let (nr, ok3) = self.decode_constraint_whole_number(0, 0xFF);
                    if !ok3 {
                        return (0, false);
                    }
                    n_remaining = nr;
                }
            }
        }
        if ret {
            if (n_remaining & 0x80) > 0 {
                n_remaining <<= 8;
                let (len2, ok4) = self.decode_constraint_whole_number(0, 0xFF);
                if !ok4 {
                    return (0, false);
                }
                n_remaining |= len2;
                n_remaining &= 0x7FFF;
            }
            ret = ret && (n_cur_offset + n_remaining <= asn1_size_max);
            if ret {
                for i in n_cur_offset..n_cur_offset + n_remaining {
                    if !ret {
                        break;
                    }
                    let (b, ok5) = self.read_byte();
                    if !ok5 {
                        ret = false;
                        break;
                    }
                    arr[i as usize] = b;
                }
                if ret {
                    n_length_tmp += n_remaining;
                    if n_length_tmp >= 1 && n_length_tmp <= asn1_size_max {
                        return (n_length_tmp as i32, true);
                    } else {
                        ret = false;
                    }
                }
            }
        }
        (0, ret)
    }

    /// Encode a constrained octet string.
    /// Mirrors C `BitStream_EncodeOctetString`.
    pub fn octet_string_encode(&mut self, arr: &[u8], n_count: i32, asn1_size_min: Asn1SccSint, asn1_size_max: Asn1SccSint) -> bool {
        let ret = n_count as Asn1SccSint >= asn1_size_min && n_count as Asn1SccSint <= asn1_size_max;
        if !ret {
            return false;
        }
        if asn1_size_max < 65536 {
            if asn1_size_min != asn1_size_max {
                self.encode_constraint_whole_number(n_count as Asn1SccSint, asn1_size_min, asn1_size_max);
            }
            self.octet_string_encode_no_length(arr, n_count as usize)
        } else {
            self.octet_string_encode_fragmentation(arr, n_count)
        }
    }

    /// Decode a constrained octet string.
    /// Mirrors C `BitStream_DecodeOctetString`.
    pub fn octet_string_decode(&mut self, arr: &mut [u8], asn1_size_min: Asn1SccSint, asn1_size_max: Asn1SccSint) -> (i32, bool) {
        if asn1_size_max < 65536 {
            let n_count: Asn1SccSint;
            if asn1_size_min < asn1_size_max {
                let (nc, ok) = self.decode_constraint_whole_number(asn1_size_min, asn1_size_max);
                if !ok {
                    return (0, false);
                }
                n_count = nc;
            } else {
                n_count = asn1_size_min;
            }
            let ret = n_count >= asn1_size_min && n_count <= asn1_size_max;
            if !ret {
                return (0, false);
            }
            if !self.octet_string_decode_no_length(arr, n_count as usize) {
                return (0, false);
            }
            (n_count as i32, true)
        } else {
            self.octet_string_decode_fragmentation(arr, asn1_size_max)
        }
    }

    // ── bit string encode/decode ─────────────────────────────────────────

    /// Encode a constrained bit string.
    /// Mirrors C `BitStream_EncodeBitString`.
    pub fn bit_string_encode(&mut self, arr: &[u8], n_count: i32, asn1_size_min: Asn1SccSint, asn1_size_max: Asn1SccSint) -> bool {
        if asn1_size_max < 65536 {
            if asn1_size_min != asn1_size_max {
                self.encode_constraint_whole_number(n_count as Asn1SccSint, asn1_size_min, asn1_size_max);
            }
            self.append_bits(arr, n_count);
        } else {
            let mut n_remaining: Asn1SccSint = n_count as Asn1SccSint;
            let mut n_cur_block_size: Asn1SccSint = 0;
            let mut n_cur_offset: Asn1SccSint = 0;
            while n_remaining >= 0x4000 {
                if n_remaining >= 0x10000 {
                    n_cur_block_size = 0x10000;
                    self.encode_constraint_whole_number(0xC4, 0, 0xFF);
                } else if n_remaining >= 0xC000 {
                    n_cur_block_size = 0xC000;
                    self.encode_constraint_whole_number(0xC3, 0, 0xFF);
                } else if n_remaining >= 0x8000 {
                    n_cur_block_size = 0x8000;
                    self.encode_constraint_whole_number(0xC2, 0, 0xFF);
                } else {
                    n_cur_block_size = 0x4000;
                    self.encode_constraint_whole_number(0xC1, 0, 0xFF);
                }
                let offset_byte = (n_cur_offset / 8) as usize;
                self.append_bits(&arr[offset_byte..], n_cur_block_size as i32);
                n_cur_offset += n_cur_block_size;
                n_remaining -= n_cur_block_size;
            }
            if n_remaining <= 0x7F {
                self.encode_constraint_whole_number(n_remaining, 0, 0xFF);
            } else {
                self.append_bit(true);
                self.encode_constraint_whole_number(n_remaining, 0, 0x7FFF);
            }
            let offset_byte = (n_cur_offset / 8) as usize;
            self.append_bits(&arr[offset_byte..], n_remaining as i32);
        }
        true
    }

    /// Decode a constrained bit string.
    /// Mirrors C `BitStream_DecodeBitString`.
    pub fn bit_string_decode(&mut self, arr: &mut [u8], asn1_size_min: Asn1SccSint, asn1_size_max: Asn1SccSint) -> (i32, bool) {
        if asn1_size_max < 65536 {
            let n_count: Asn1SccSint;
            if asn1_size_min != asn1_size_max {
                let (nc, ok) = self.decode_constraint_whole_number(asn1_size_min, asn1_size_max);
                if !ok {
                    return (0, false);
                }
                n_count = nc;
            } else {
                n_count = asn1_size_min;
            }
            if !self.read_bits(arr, n_count as i32) {
                return (0, false);
            }
            (n_count as i32, true)
        } else {
            let mut n_length_tmp: Asn1SccSint = 0;
            let mut n_remaining: Asn1SccSint;
            let mut n_cur_block_size: Asn1SccSint = 0;
            let mut n_cur_offset: Asn1SccSint = 0;
            let mut ret = true;

            let (nr, ok) = self.decode_constraint_whole_number(0, 0xFF);
            if !ok {
                return (0, false);
            }
            n_remaining = nr;
            while ret && (n_remaining & 0xC0) == 0xC0 {
                if n_remaining == 0xC4 {
                    n_cur_block_size = 0x10000;
                } else if n_remaining == 0xC3 {
                    n_cur_block_size = 0xC000;
                } else if n_remaining == 0xC2 {
                    n_cur_block_size = 0x8000;
                } else if n_remaining == 0xC1 {
                    n_cur_block_size = 0x4000;
                } else {
                    return (0, false);
                }
                if n_cur_offset + n_cur_block_size > asn1_size_max {
                    return (0, false);
                }
                let offset_byte = (n_cur_offset / 8) as usize;
                let block_bits = n_cur_block_size as i32;
                if !self.read_bits(&mut arr[offset_byte..], block_bits) {
                    return (0, false);
                }
                n_length_tmp += n_cur_block_size;
                n_cur_offset += n_cur_block_size;
                let (nr2, ok2) = self.decode_constraint_whole_number(0, 0xFF);
                if !ok2 {
                    return (0, false);
                }
                n_remaining = nr2;
            }
            if ret {
                if (n_remaining & 0x80) > 0 {
                    n_remaining <<= 8;
                    let (len2, ok3) = self.decode_constraint_whole_number(0, 0xFF);
                    if !ok3 {
                        return (0, false);
                    }
                    n_remaining |= len2;
                    n_remaining &= 0x7FFF;
                }
                ret = ret && (n_cur_offset + n_remaining <= asn1_size_max);
                if ret {
                    let offset_byte = (n_cur_offset / 8) as usize;
                    if !self.read_bits(&mut arr[offset_byte..], n_remaining as i32) {
                        return (0, false);
                    }
                    n_length_tmp += n_remaining;
                    if n_length_tmp >= 1 && n_length_tmp <= asn1_size_max {
                        return (n_length_tmp as i32, true);
                    } else {
                        ret = false;
                    }
                }
            }
            (0, ret)
        }
    }

    // ── bit-pattern helpers ───────────────────────────────────────────────

    /// Check whether a specific bit pattern is immediately present in the
    /// stream.  Returns `0` on error (end of stream), `1` if the pattern does
    /// not match (cursor is rewound), `2` if the pattern matches (cursor is
    /// advanced past it).  Mirrors C `BitStream_checkBitPatternPresent`.
    pub fn check_bit_pattern_present(&mut self, bit_terminated_pattern: &[u8], mut bit_terminated_pattern_size_in_bits: usize) -> i32 {
        let tmp_current_byte = self.current_byte;
        let tmp_current_bit = self.current_bit;
        let mut i = 0usize;

        if self.current_byte * 8 + self.current_bit as i64 + bit_terminated_pattern_size_in_bits as i64 > self.count * 8 {
            return 0;
        }

        while bit_terminated_pattern_size_in_bits >= 8 {
            let (tmp_byte, ok) = self.read_byte();
            if !ok {
                return 0;
            }
            bit_terminated_pattern_size_in_bits -= 8;
            if bit_terminated_pattern[i] != tmp_byte {
                self.current_byte = tmp_current_byte;
                self.current_bit = tmp_current_bit;
                return 1;
            }
            i += 1;
        }

        if bit_terminated_pattern_size_in_bits > 0 {
            let (mut tmp_byte, ok) = self.read_partial_byte(bit_terminated_pattern_size_in_bits as u8);
            if !ok {
                return 0;
            }
            tmp_byte <<= 8 - bit_terminated_pattern_size_in_bits as u8;
            if bit_terminated_pattern[i] != tmp_byte {
                self.current_byte = tmp_current_byte;
                self.current_bit = tmp_current_bit;
                return 1;
            }
        }
        2
    }

    /// Read bits until a terminating bit pattern is found or `n_max_read_bits`
    /// is reached.  Mirrors C `BitStream_ReadBits_nullterminated`.
    pub fn read_bits_nullterminated(
        &mut self,
        bit_terminated_pattern: &[u8],
        bit_terminated_pattern_size_in_bits: usize,
        buff_to_write: &mut [u8],
        n_max_read_bits: i32,
    ) -> (i32, bool) {
        let mut bits_read: i32 = 0;
        let mut ret = true;
        let buf_len = if n_max_read_bits % 8 == 0 {
            n_max_read_bits / 8
        } else {
            n_max_read_bits / 8 + 1
        } as usize;
        let tmp_buf = &mut buff_to_write[..buf_len];
        // zero the temp buffer
        for b in tmp_buf.iter_mut() {
            *b = 0;
        }
        let mut tmp_strm = BitStream::new(tmp_buf);

        let mut check_result = self.check_bit_pattern_present(bit_terminated_pattern, bit_terminated_pattern_size_in_bits);
        while ret && bits_read < n_max_read_bits && check_result == 1 {
            let (bit_val, ok) = self.read_bit();
            if !ok {
                ret = false;
                break;
            }
            tmp_strm.append_bit(bit_val);
            bits_read += 1;
            if ret && bits_read < n_max_read_bits {
                check_result = self.check_bit_pattern_present(bit_terminated_pattern, bit_terminated_pattern_size_in_bits);
            }
        }
        if ret && bits_read == n_max_read_bits && check_result == 1 {
            check_result = self.check_bit_pattern_present(bit_terminated_pattern, bit_terminated_pattern_size_in_bits);
        }
        (bits_read, ret && check_result == 2)
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  Free-function integer helpers (mirroring C static functions)
// ─────────────────────────────────────────────────────────────────────────

/// Returns the minimum number of bits needed to represent a 32-bit unsigned
/// value.  Mirrors C `GetNumberOfBitsForNonNegativeInteger32`.
pub fn get_number_of_bits_for_non_negative_integer32(mut v: Asn1SccUint32) -> i32 {
    let mut ret: i32;
    if v < 0x100 {
        ret = 0;
    } else if v < 0x1_0000 {
        ret = 8;
        v >>= 8;
    } else if v < 0x0100_0000 {
        ret = 16;
        v >>= 16;
    } else {
        ret = 24;
        v >>= 24;
    }
    while v > 0 {
        v >>= 1;
        ret += 1;
    }
    ret
}

/// Returns the minimum number of bits needed to represent `v` (≥ 0).
/// Mirrors C `GetNumberOfBitsForNonNegativeInteger`.
pub fn get_number_of_bits_for_non_negative_integer(v: Asn1SccUint) -> i32 {
    if v < 0x1_0000_0000 {
        get_number_of_bits_for_non_negative_integer32(v as Asn1SccUint32)
    } else {
        let hi = (v >> 32) as Asn1SccUint32;
        32 + get_number_of_bits_for_non_negative_integer32(hi)
    }
}

/// Returns the minimum number of bytes needed to represent the unsigned
/// value `v`.  Mirrors C `GetLengthInBytesOfUInt`.
pub fn get_length_in_bytes_of_uint(v: Asn1SccUint64) -> i32 {
    let mut ret = 0;
    let mut v32 = v as Asn1SccUint32;
    if v > 0xFFFF_FFFF {
        ret = 4;
        v32 = (v >> 32) as Asn1SccUint32;
    }
    if v32 < 0x100 {
        ret + 1
    } else if v32 < 0x1_0000 {
        ret + 2
    } else if v32 < 0x0100_0000 {
        ret + 3
    } else {
        ret + 4
    }
}

fn get_length_sint_helper(v: Asn1SccUint) -> i32 {
    let mut ret = 0;
    let mut v32 = v as Asn1SccUint32;
    if v > 0x7FFF_FFFF {
        ret = 4;
        v32 = (v >> 32) as Asn1SccUint32;
    }
    if v32 <= 0x7F {
        ret + 1
    } else if v32 <= 0x7FFF {
        ret + 2
    } else if v32 <= 0x7F_FFFF {
        ret + 3
    } else {
        ret + 4
    }
}

/// Returns the minimum number of bytes needed to represent the signed
/// value `v`.  Mirrors C `GetLengthInBytesOfSInt`.
pub fn get_length_in_bytes_of_sint(v: Asn1SccSint) -> i32 {
    if v >= 0 {
        get_length_sint_helper(v as Asn1SccUint)
    } else {
        // C: GetLengthSIntHelper((uint)(-v - 1))
        get_length_sint_helper(v.unsigned_abs() - 1)
    }
}

/// Decompose a double into mantissa and (biased) exponent.
/// Mirrors C `CalculateMantissaAndExponent`.
pub fn calculate_mantissa_and_exponent(d: f64) -> (i32, Asn1SccUint64) {
    let bits = d.to_bits();
    let exponent = ((bits & EXPO_BIT_MASK) >> 52) as i64 - 1023 - 52;
    let mut mantissa = bits & MANT_BIT_MASK;
    mantissa |= MANTISA_EXTRA_BIT;
    (exponent as i32, mantissa)
}

/// Reconstruct a double from mantissa and exponent.
/// Mirrors C `GetDoubleByMantissaAndExp`.
pub fn get_double_by_mantissa_and_exp(mantissa: Asn1SccUint, exponent: i32) -> f64 {
    if mantissa == 0 {
        return 0.0;
    }
    let mut ret: f64 = 1.0;
    if exponent >= 0 {
        let mut e = exponent;
        while e > 0 {
            ret *= 2.0;
            e -= 1;
        }
        mantissa as f64 * ret
    } else {
        let mut e = -exponent;
        while e > 0 {
            ret *= 2.0;
            e -= 1;
        }
        mantissa as f64 / ret
    }
}

// ─────────────────────────────────────────────────────────────────────────
//  Floating-point bit masks (IEEE 754 double)
// ─────────────────────────────────────────────────────────────────────────

const EXPO_BIT_MASK: u64 = 0x7FF0_0000_0000_0000;
const MANT_BIT_MASK: u64 = 0x000F_FFFF_FFFF_FFFF;
const MANTISA_EXTRA_BIT: u64 = 0x0010_0000_0000_0000;

// ─────────────────────────────────────────────────────────────────────────
//  Tests
// ─────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int2uint_uint2int() {
        assert_eq!(int2uint(0), 0);
        assert_eq!(int2uint(1), 1);
        assert_eq!(int2uint(-1), Asn1SccUint::MAX);
        assert_eq!(int2uint(42), 42);

        assert_eq!(uint2int(0, 8), 0);
        assert_eq!(uint2int(42, 8), 42);
        assert_eq!(uint2int(Asn1SccUint::MAX, 8), -1);
    }

    #[test]
    fn test_oid() {
        let oid = Asn1ObjectIdentifier::init();
        assert_eq!(oid.n_count, 0);
        assert!(!oid.is_valid());

        let mut oid2 = Asn1ObjectIdentifier::init();
        oid2.n_count = 2;
        oid2.values[0] = 1;
        oid2.values[1] = 3;
        assert!(oid2.is_valid());
        assert!(oid2.relative_oid_is_valid());

        let mut oid3 = Asn1ObjectIdentifier::init();
        oid3.n_count = 2;
        oid3.values[0] = 1;
        oid3.values[1] = 3;
        assert!(oid2.equal(&oid3));

        let mut oid4 = Asn1ObjectIdentifier::init();
        oid4.n_count = 2;
        oid4.values[0] = 1;
        oid4.values[1] = 40;
        assert!(!oid4.is_valid());
    }

    #[test]
    fn test_bitstream_basic() {
        let mut buf = [0u8; 16];
        let mut bs = BitStream::new(&mut buf);
        bs.append_bit_one();
        bs.append_bit_zero();
        bs.append_bit(true);
        assert!(bs.get_length() >= 1);

        let mut buf2 = [0u8; 16];
        let mut bs2 = BitStream::new(&mut buf2);
        bs2.append_byte(0xAB, false);
        bs2.append_byte(0xCD, false);
        assert_eq!(bs2.get_length(), 2);
        assert_eq!(buf2[0], 0xAB);
        assert_eq!(buf2[1], 0xCD);
    }

    #[test]
    fn test_bitstream_read_write_byte() {
        let mut buf = [0u8; 16];
        {
            let mut bs = BitStream::new(&mut buf);
            bs.append_byte(0x42, false);
            bs.append_byte(0x99, false);
        }
        // Re-attach to the written buffer for reading (no zeroing).
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let (v1, ok1) = bs.read_byte();
        let (v2, ok2) = bs.read_byte();
        assert!(ok1 && ok2);
        assert_eq!(v1, 0x42);
        assert_eq!(v2, 0x99);
    }

    #[test]
    fn test_constraint_whole_number() {
        let mut buf = [0u8; 64];
        {
            let mut bs = BitStream::new(&mut buf);
            bs.encode_constraint_whole_number(5, 0, 10);
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let (v, ok) = bs.decode_constraint_whole_number(0, 10);
        assert!(ok);
        assert_eq!(v, 5);
    }

    #[test]
    fn test_unconstrained_whole_number() {
        for &val in &[0i64, 1, -1, 42, -42, 255, 256, -256, 65535, -65536] {
            let mut buf = [0u8; 64];
            {
                let mut bs = BitStream::new(&mut buf);
                bs.encode_unconstrained_whole_number(val);
            }
            let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
            let (v, ok) = bs.decode_unconstrained_whole_number();
            assert!(ok, "decode failed for val={}", val);
            assert_eq!(v, val, "mismatch for val={}", val);
        }
    }

    #[test]
    fn test_semi_constraint_whole_number() {
        let mut buf = [0u8; 64];
        {
            let mut bs = BitStream::new(&mut buf);
            bs.encode_semi_constraint_whole_number(105, 100);
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let (v, ok) = bs.decode_semi_constraint_whole_number(100);
        assert!(ok);
        assert_eq!(v, 105);
    }

    #[test]
    fn test_non_negative_integer_bits() {
        assert_eq!(get_number_of_bits_for_non_negative_integer(0), 0);
        assert_eq!(get_number_of_bits_for_non_negative_integer(1), 1);
        assert_eq!(get_number_of_bits_for_non_negative_integer(7), 3);
        assert_eq!(get_number_of_bits_for_non_negative_integer(255), 8);
        assert_eq!(get_number_of_bits_for_non_negative_integer(256), 9);
    }

    #[test]
    fn test_real_equal() {
        assert!(asn1_real_equal(1.0, 1.0));
        assert!(asn1_real_equal(0.0, 0.0));
        assert!(asn1_real_equal(1.0, 1.000001));
        assert!(!asn1_real_equal(1.0, -1.0));
        assert!(!asn1_real_equal(1.0, 2.0));
    }

    #[test]
    fn test_get_char_index() {
        let alpha = b"0123456789ABCDEF";
        assert_eq!(get_char_index('0', alpha), 0);
        assert_eq!(get_char_index('A', alpha), 10);
        assert_eq!(get_char_index('F', alpha), 15);
        assert_eq!(get_char_index('G', alpha), -1);
    }

    #[test]
    fn test_encode_decode_real_zero() {
        let mut buf = [0u8; 64];
        {
            let mut bs = BitStream::new(&mut buf);
            bs.encode_real(0.0);
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let (v, ok) = bs.decode_real();
        assert!(ok);
        assert_eq!(v, 0.0);
    }

    #[test]
    fn test_octet_string_no_length() {
        let data = [0x01u8, 0x02, 0x03, 0x04];
        let mut buf = [0u8; 32];
        {
            let mut bs = BitStream::new(&mut buf);
            assert!(bs.octet_string_encode_no_length(&data, 4));
        }
        let mut bs = BitStream::attach_buffer_no_zero(&mut buf);
        let mut out = [0u8; 4];
        assert!(bs.octet_string_decode_no_length(&mut out, 4));
        assert_eq!(&out, &data);
    }
}


pub trait Asn1Assign<T> {
    fn asn1_assign(&mut self, val: T);
}

impl<T> Asn1Assign<T> for T {
    fn asn1_assign(&mut self, val: T) {
        *self = val;
    }
}

#[macro_export]
macro_rules! asn1scc_assign {
    ( $val:expr => $var:expr ) => {
        $var.asn1_assign($val);
    };
}

