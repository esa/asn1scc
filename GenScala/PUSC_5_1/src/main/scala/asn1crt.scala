//import stainless.math.BitVectors._
import stainless.lang._

// Unsigned datatypes that have no native JVM support

type UByte = Byte
type UShort = Short
type UInt = Int
type ULong = Long
type RealNoRTL = Float
type BooleanNoRTL = Boolean
type nullNoRTL = Null

val WORD_SIZE = 8
val OBJECT_IDENTIFIER_MAX_LENGTH = 20

case class BitStream (
  var buf: Array[Byte],
  var currentByte: Int,
  var currentBit: Int,
  // TODO
  //var pushDataPrm: Option[Any],
  //var fetchDataPrm: Option[Any],
) {
  // TODO: currentByte==buf.length temp possible, but with bitstream_push_data_if_required set to 0 again
  require(currentByte >= 0 && currentByte < buf.length)
  require(currentBit >= 0 && currentBit < 8)
}

case class ByteStream (
  var buf: Array[Byte],
  var currentByte: Int,
  var EncodeWhiteSpace: Boolean
) {
  require(currentByte >= 0 && currentByte < buf.length)
}

case class Asn1ObjectIdentifier (
  var nCount: Int,
  values: Array[Long]
) {
  require(values.length == OBJECT_IDENTIFIER_MAX_LENGTH)
  require(nCount >= 0)
}

// TODO: Ref?
def ObjectIdentifier_Init(pVal: Asn1ObjectIdentifier): Unit = {
  var i: Int = 0
  (while i < OBJECT_IDENTIFIER_MAX_LENGTH do
    decreases(OBJECT_IDENTIFIER_MAX_LENGTH - i)
    pVal.values(i) = 0
    i += 1
  ).invariant(i >= 0 &&& i <= OBJECT_IDENTIFIER_MAX_LENGTH)
  pVal.nCount = 0
} ensuring (_ => pVal.nCount == 0)
