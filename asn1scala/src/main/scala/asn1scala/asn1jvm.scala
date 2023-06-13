package asn1scala

import stainless.lang.{None, Option => _, _}

// type used in ErrorCases
type ErrorCode = Int

// Unsigned datatypes that have no native JVM support
type UByte = Byte
type UShort = Short
type UInt = Int
type ULong = Long
type RealNoRTL = Float
type BooleanNoRTL = Boolean
//type nullNoRTL = nulltype // TODO

// TODO
type LongNoRTL = Long
type ULongNoRTL = ULong

val WORD_SIZE = 8
val OBJECT_IDENTIFIER_MAX_LENGTH = 20

case class Ref[T](var x: T) {}


val ber_aux: Array[ULong] = Array(
  0xFFL,
  0xFF00L,
  0xFF0000L,
  0xFF000000L,
  0xFF00000000L,
  0xFF0000000000L,
  0xFF000000000000L,
  0xFF00000000000000L
)

// TODO: check types and if neccesary as we don't have unsigned types
def int2uint(v: Long): ULong = {
  var ret: ULong = 0
  if v < 0 then
    ret = -v - 1
    ret = ~ret
  else
    ret = v

  ret
}

def uint2int(v: ULong, uintSizeInBytes: Int): Long = {
  var vv = v
  val tmp: ULong = 0x80
  val bIsNegative: Boolean = (vv & (tmp << ((uintSizeInBytes - 1) * 8))) > 0

  if !bIsNegative then
    return v

  var i: Int = WORD_SIZE-1
  while i >= uintSizeInBytes do
    vv |= ber_aux(i)
    i -= 1
  -(~vv) - 1
}

def GetCharIndex(ch: Char, Set: Array[Byte]): Int =
{
  var i: Int = 0
  while i < Set.length do
    if ch == Set(i) then
      return i
    i += 1
  0
}


case class BitStream (
  var buf: Array[Byte], // UByte
  var currentByte: Int,
  var currentBit: Int,
  // TODO
  var pushDataPrm: Option[Any],
  var fetchDataPrm: Option[Any],
) {
  // TODO: currentByte==buf.length temp possible, but with bitstream_push_data_if_required set to 0 again
  require(currentByte >= 0 && currentByte < buf.length)
  require(currentBit >= 0 && currentBit < 8)
}

case class ByteStream (
  var buf: Array[Byte], // UByte
  var currentByte: Int,
  var EncodeWhiteSpace: Boolean
) {
  require(currentByte >= 0 && currentByte < buf.length)
}



/**

#######                                      ###
#     # #####       # ######  ####  #####     #  #####  ###### #    # ##### # ###### # ###### #####
#     # #    #      # #      #    #   #       #  #    # #      ##   #   #   # #      # #      #    #
#     # #####       # #####  #        #       #  #    # #####  # #  #   #   # #####  # #####  #    #
#     # #    #      # #      #        #       #  #    # #      #  # #   #   # #      # #      #####
#     # #    # #    # #      #    #   #       #  #    # #      #   ##   #   # #      # #      #   #
####### #####   ####  ######  ####    #      ### #####  ###### #    #   #   # #      # ###### #    #

Object Identifier

**/


case class Asn1ObjectIdentifier (
  var nCount: Int,
  values: Array[Long] // ULong
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

def ObjectIdentifier_isValid(pVal: Asn1ObjectIdentifier): Boolean = {
  return (pVal.nCount >= 2) && (pVal.values(0) <= 2) && (pVal.values(1) <= 39)
}

def RelativeOID_isValid (pVal: Asn1ObjectIdentifier): Boolean = {
  return pVal.nCount > 0
}

def ObjectIdentifier_equal (pVal1: Asn1ObjectIdentifier, pVal2: Asn1ObjectIdentifier): Boolean = {
  var i: Int = 0
  if pVal1.nCount == pVal2.nCount && pVal1.nCount <= OBJECT_IDENTIFIER_MAX_LENGTH then // TODO: (pVal1 != NULL) && (pVal2 != NULL)
    var ret: Boolean = true
    while i < pVal1.nCount && ret do
      decreases(pVal1.nCount - i)
      ret = (pVal1.values(i) == pVal2.values(i))
      i += 1

    return ret
  else
    return false
}

def CHECK_BIT_STREAM(pBitStrm: BitStream): Unit = {
  assert(pBitStrm.currentByte*8 + pBitStrm.currentBit <= pBitStrm.buf.length*8)
}