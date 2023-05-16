package asn1scala

import stainless.lang._

// Unsigned datatypes that have no native JVM support
type UByte = Byte
type UShort = Short
type UInt = Int
type ULong = Long
type RealNoRTL = Float
type BooleanNoRTL = Boolean
//type nullNoRTL = nulltype

val WORD_SIZE = 8
val OBJECT_IDENTIFIER_MAX_LENGTH = 20

case class Ref[T](var x: T) {}

case class BitStream (
  var buf: Array[Byte], // UByte
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


object playground {
  def main(args: Array[String]): Unit = {
    val v: Array[Long] = Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH)(1)
    var c = 5
    val x = Asn1ObjectIdentifier(c, v)
    ObjectIdentifier_Init(x)
    println(x.nCount)
    println(x.values.mkString(","))
  }
}