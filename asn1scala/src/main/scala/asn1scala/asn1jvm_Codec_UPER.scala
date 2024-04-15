package asn1scala

import stainless.math.BitVectors._
import stainless.lang.StaticChecks._
import stainless.lang.{None => None, Option => Option, Some => Some, _}
import stainless.annotation._


// SAM Ignored for safety verification

/**
 * Get an instance of a UPER coded bitstream
 * @param count of elements in underlaying buffer
 * @return UPER coded bitstream
 */
def initUPERCodec(count: Int): UPER = {
   //SAM guard to ensure the property
   if count <= 0 then
      UPER(Codec(BitStream(Array.fill(0)(0))))
   else
      UPER(Codec(BitStream(Array.fill(count)(0))))
}
object UPER {
   @ghost @pure
   def reader(w1: UPER, w2: UPER): (UPER, UPER) = {
      require(w1.base.bitStream.isPrefixOf(w2.base.bitStream))
      val (r1, r2) = BitStream.reader(w1.base.bitStream, w2.base.bitStream)
      (UPER(Codec(r1)), UPER(Codec(r2)))
   }
}
case class UPER private [asn1scala](base: Codec) {
   import BitStream.*
   import UPER.*
   import base.*

   @ghost @pure @inline
   def resetAt(other: UPER): UPER = {
      require(bitStream.buf.length == other.base.bitStream.buf.length)
      UPER(Codec(bitStream.resetAt(other.base.bitStream)))
   }

   @ghost @pure @inline
   def withMovedByteIndex(diffInBytes: Int): UPER = {
      require(moveByteIndexPrecond(bitStream, diffInBytes))
      UPER(Codec(bitStream.withMovedByteIndex(diffInBytes)))
   }

   @ghost @pure @inline
   def withMovedBitIndex(diffInBits: Int): UPER = {
      require(moveBitIndexPrecond(bitStream, diffInBits))
      UPER(Codec(bitStream.withMovedBitIndex(diffInBits)))
   }

   @pure @inline
   def isPrefixOf(uper2: UPER): Boolean = bitStream.isPrefixOf(uper2.base.bitStream)

   private def objectIdentifier_subIdentifiers_encode(encodingBuf: Array[UByte], pSizeVal: Int, siValueVal: ULong): Int = {
      var lastOctet: Boolean = false
      val tmp: Array[UByte] = Array.fill(16)(0.toRawUByte)
      var nSize: Int = 0

      var siValue = siValueVal.toRaw
      var pSize = pSizeVal

      while !lastOctet do
         decreases(siValue)
         val curByte: UByte = (siValue % 128).toByte.toRawUByte
         siValue = siValue / 128
         lastOctet = siValue.toInt == 0
         tmp(nSize) = curByte
         nSize += 1

      var i: Int = 0
      while i < nSize do
         decreases(nSize - i)
         val curByte: UByte = if i == nSize - 1 then tmp(nSize - i - 1) else (tmp(nSize - i - 1).toRaw | 0x80).toByte.toRawUByte
         encodingBuf(pSize) = curByte
         pSize += 1
         i += 1

      pSize
   }

   def ObjectIdentifier_encode(pVal: Asn1ObjectIdentifier): Unit = {
      val tmp: Array[UByte] = Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH * (NO_OF_BYTES_IN_JVM_LONG + 2))(0.toRawUByte)
      var totalSize: Int = 0

      var i: Int = 0
      totalSize = objectIdentifier_subIdentifiers_encode(tmp, totalSize, (pVal.values(0) * 40 + pVal.values(1)).toRawULong)

      i = 0
      while i < pVal.nCount do
         decreases(pVal.nCount - i)
         totalSize = objectIdentifier_subIdentifiers_encode(tmp, totalSize, pVal.values(i).toRawULong)
         i += 1

      if totalSize <= 0x7F then
         encodeConstrainedWholeNumber(totalSize.toLong, 0, 0xFF)
      else
         appendBit(true)
         encodeConstrainedWholeNumber(totalSize.toLong, 0, 0x7FFF)

      i = 0
      while i < totalSize do
         decreases(totalSize - i)
         appendByte(tmp(i))
         i += 1
   }

   def relativeOID_encode(pVal: Asn1ObjectIdentifier): Unit = {
      //a subIdentifier (i.e. a component) should not take more than size(asn1SccUint) + 2 to be encoded
      //(the above statement is true for 8 byte integers. If we ever user larger integer then it should be adjusted)
      val tmp: Array[UByte] = Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH * (NO_OF_BYTES_IN_JVM_LONG + 2))(0.toRawUByte)
      var totalSize: Int = 0

      var i: Int = 0

      while i < pVal.nCount do
         decreases(pVal.nCount - i)
         totalSize = objectIdentifier_subIdentifiers_encode(tmp, totalSize, pVal.values(i).toRawULong)
         i += 1


      if totalSize <= 0x7F then
         encodeConstrainedWholeNumber(totalSize.toLong, 0, 0xFF)
      else
         appendBit(true)
         encodeConstrainedWholeNumber(totalSize.toLong, 0, 0x7FFF)

      i = 0
      while i < totalSize do
         decreases(totalSize - i)
         appendByte(tmp(i))
         i += 1
   }

   def objectIdentifier_subIdentifiers_decode(pRemainingOctetsVal: Long): (Long, ULong) = {
      var bLastOctet: Boolean = false
      var curOctetValue: Long = 0
      var siValue: Long = 0
      var pRemainingOctets: Long = pRemainingOctetsVal

      (while pRemainingOctets > 0 && !bLastOctet do
         decreases(pRemainingOctets)
         val curByte = readByte().toRaw
         pRemainingOctets -= 1

         bLastOctet = (curByte & 0x80) == 0
         curOctetValue = (curByte & 0x7F).toLong
         siValue = siValue << 7
         siValue |= curOctetValue
      ).invariant(true) // TODO

      (pRemainingOctets, ULong.fromRaw(siValue))
   }


   def objectIdentifier_decode_length(): Long = {

      var totalSize = decodeConstrainedWholeNumber(0, 0xFF)

      if totalSize > 0x7F then
         totalSize <<= 8
         totalSize |= decodeConstrainedWholeNumber(0, 0xFF)
         totalSize &= 0x7FFF

      totalSize
   }

   def objectIdentifier_decode(): Asn1ObjectIdentifier = {
      val pVal = ObjectIdentifier_Init()
      var (totalSize, si) = objectIdentifier_subIdentifiers_decode(objectIdentifier_decode_length())

      pVal.nCount = 2
      pVal.values(0) = si.toRaw / 40
      pVal.values(1) = si.toRaw % 40
      (while totalSize > 0 && pVal.nCount < OBJECT_IDENTIFIER_MAX_LENGTH do
         decreases(OBJECT_IDENTIFIER_MAX_LENGTH - pVal.nCount)

         val tpl = objectIdentifier_subIdentifiers_decode(totalSize)

         totalSize = tpl._1
         si = tpl._2

         pVal.values(pVal.nCount) = si.toRaw
         pVal.nCount += 1
      ).invariant(true) // TODO

      //return true, if totalSize reduced to zero. Otherwise we exit the loop because more components we present than OBJECT_IDENTIFIER_MAX_LENGTH
      assert(totalSize == 0)

      pVal
   }

   def relativeOID_decode(): Asn1ObjectIdentifier = {
      val pVal: Asn1ObjectIdentifier = ObjectIdentifier_Init()

      var totalSize = objectIdentifier_decode_length()
      var si: Long = 0

      (while totalSize > 0 && pVal.nCount < OBJECT_IDENTIFIER_MAX_LENGTH do
         decreases(OBJECT_IDENTIFIER_MAX_LENGTH - pVal.nCount)
         val tpl = objectIdentifier_subIdentifiers_decode(totalSize)

         totalSize = tpl._1
         si = tpl._2.toRaw

         pVal.values(pVal.nCount) = si
         pVal.nCount += 1
      ).invariant(true) // TODO

      //return true, if totalSize is zero. Otherwise we exit the loop because more components were present than OBJECT_IDENTIFIER_MAX_LENGTH
      assert(totalSize == 0)
      pVal
   }
}
