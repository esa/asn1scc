package asn1crt

import stainless.math.BitVectors._

case class Ref[T](var x: T) {}

val WORD_SIZE = 8
val OBJECT_IDENTIFIER_MAX_LENGTH = 20

type uint8_t = UInt8
type int32_t = Int32
type uint32_t = UInt32
type int64_t = Int64
type uint64_t = UInt64

type asn1Real32 = Float
type asn1Real64 = Double
type byte = uint8_t
type asn1SccSint32 = int32_t
type asn1SccUint32 = uint32_t
type asn1SccSint64 = int64_t
type asn1SccUint64 = uint64_t

// TODO: depends on word size (asn1SccUint32)
type asn1SccSint = asn1SccSint64
type asn1SccUint = asn1SccUint64

type flag = Boolean

case class BitStream (
  buf: Array[byte],
  count: Long,
  currentByte: Long,
  currentBit: Int
) { }

case class Asn1ObjectIdentifier (
  nCount: Int,
  values: Array[asn1SccUint]
) {
  require(
    values.length == OBJECT_IDENTIFIER_MAX_LENGTH
  )
}
