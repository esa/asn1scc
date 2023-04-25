package asn1crt

import stainless.math.BitVectors._

case class Ref[T](var x: T) {}

val WORD_SIZE = 8
val OBJECT_IDENTIFIER_MAX_LENGTH = 20

type flag = Boolean

case class BitStream (
  var buf: Array[UInt8],
  var count: Int, // TODO: was Long
  var currentByte: Int, // TODO: was Long
  var currentBit: Int
) { }

case class Asn1ObjectIdentifier (
  nCount: Int,
  values: Array[UInt64]
) {
  require(
    values.length == OBJECT_IDENTIFIER_MAX_LENGTH
  )
}
