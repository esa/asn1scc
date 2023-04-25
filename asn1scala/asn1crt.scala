package asn1crt

import stainless.math.BitVectors._
import stainless.lang.*

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
  var nCount: Int,
  values: Array[UInt64]
) {
  require(
    values.length == OBJECT_IDENTIFIER_MAX_LENGTH
  )
}

// TODO: Ref?
def ObjectIdentifier_Init(pVal: Asn1ObjectIdentifier): Unit = {
  var i: Int = 0
  while i < OBJECT_IDENTIFIER_MAX_LENGTH do
    decreases(OBJECT_IDENTIFIER_MAX_LENGTH - i)
    pVal.values(i) = 0
    i += 1

  pVal.nCount = 0
}