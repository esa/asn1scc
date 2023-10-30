package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.*
import StaticChecks.*

// TODO move to Bitstream if only used there
val masksb: Array[UByte] = Array(
   0x00, //   0 / 0000 0000 / x00
   0x01, //   1 / 0000 0001 / x01
   0x03, //   3 / 0000 0011 / x03
   0x07, //   7 / 0000 0111 / x07
   0x0F, //  15 / 0000 1111 / x0F
   0x1F, //  31 / 0001 1111 / x1F
   0x3F, //  63 / 0011 1111 / x3F
   0x7F, // 127 / 0111 1111 / x7F
   -0x1, //   -1 / 1111 1111 / xFF
)

val masks2: Array[UInt] = Array(
   0x00000000, //         0 / 0000 0000 0000 0000 0000 0000 0000 0000 / 0x0000 0000
   0x000000FF, //       255 / 0000 0000 0000 0000 0000 0000 1111 1111 / 0x0000 00FF
   0x0000FF00, //     65280 / 0000 0000 0000 0000 1111 1111 0000 0000 / 0x0000 FF00
   0x00FF0000, //  16711680 / 0000 0000 1111 1111 0000 0000 0000 0000 / 0x00FF 0000
   0xFF000000, // -16777216 / 1111 1111 0000 0000 0000 0000 0000 0000 / 0xFF00 0000
)


/***********************************************************************************************/
/**    Byte Stream Functions                                                                  **/
/***********************************************************************************************/
def ByteStream_Init(count: Int): ByteStream = {
   ByteStream(Array.fill(count)(0), 0, false)
}

@extern
def ByteStream_AttachBuffer(pStrm: ByteStream, buf: Array[UByte]): Unit = {
   pStrm.buf = buf // Illegal aliasing, therefore we need to workaround with this @extern...
   pStrm.currentByte = 0
}.ensuring(_ => pStrm.buf == buf && pStrm.currentByte == 0 && pStrm.EncodeWhiteSpace == old(pStrm).EncodeWhiteSpace)

def ByteStream_GetLength(pStrm: ByteStream): Int = {
   pStrm.currentByte
}

/***********************************************************************************************/
/**    Bit Stream Functions                                                                   **/
/***********************************************************************************************/
def BitString_equal(arr1: Array[UByte], arr2: Array[UByte]): Boolean = {
   arraySameElements(arr1, arr2)
   //return
   //    (nBitsLength1 == nBitsLength2) &&
   //        (nBitsLength1 / 8 == 0 || memcmp(arr1, arr2, nBitsLength1 / 8) == 0) &&
   //        (nBitsLength1 % 8 > 0 ? (arr1[nBitsLength1 / 8] >>> (8 - nBitsLength1 % 8) == arr2[nBitsLength1 / 8] >>> (8 - nBitsLength1 % 8)): TRUE);
}


// TODO remove
def BitStream_Init(count: Int): BitStream = {
   BitStream(Array.fill(count)(0), 0, 0)
}

/**
 * Parent class for the PER Codec that is used by ACN and UPER
 *
 * @param count represents the number of bytes in the internal buffer
 */
@mutable
trait Codec {

   def bitStream: BitStream

   /** ******************************************************************************************** */
   /** ******************************************************************************************** */
   /** ******************************************************************************************** */
   /** ******************************************************************************************** */
   /** Integer Functions                                                                     * */
   /** ******************************************************************************************** */
   /** ******************************************************************************************** */
   /** ******************************************************************************************** */

   /** ******************************************************************************************** */
   def encodeNonNegativeInteger32Neg(v: Int, negate: Boolean): Unit = {
      var cc: UInt = 0
      var curMask: UInt = 0
      var pbits: UInt = 0

      if v == 0 then
         return ()

      if v >>> 8 == 0 then
         cc = 8
         curMask = 0x80
      else if v >>> 16 == 0 then
         cc = 16
         curMask = 0x8000
      else if v >>> 24 == 0 then
         cc = 24
         curMask = 0x800000
      else
         cc = 32
         curMask = 0x80000000

      while (v & curMask) == 0 do
         decreases(cc)
         curMask >>>= 1
         cc -= 1

      pbits = cc % 8
      if pbits > 0 then
         cc -= pbits
         bitStream.appendPartialByte((v >>> cc).toByte, pbits.toByte, negate)

      while cc > 0 do
         decreases(cc)
         val t1: UInt = v.toInt & masks2(cc >>> 3)
         cc -= 8
         bitStream.appendByte((t1 >>> cc).toByte, negate)
   }

   def decodeNonNegativeInteger32Neg(nBitsVal : Int): Option[UInt] = {

      var v: UInt = 0

      var nBits = nBitsVal
      while nBits >= 8 do
         decreases(nBits)
         v = v << 8

         bitStream.readByte() match
            case None() => return None()
            case Some(ub) =>
               // mask the Byte-Bits, becuase negative values eg. -1 (1111 1111)
               // will be casted to an Int -1 (1111 ... 1111)
               v = v | (ub & 0xFF)

         nBits -= 8

      if nBits != 0 then
         v = v << nBits
         bitStream.readPartialByte(nBits.toByte) match
            case None() => return None()
            case Some(ub) => v = v | (ub & 0xFF)

      Some(v)
   }

   def encodeNonNegativeInteger(v: ULong): Unit = {
      if v >>> 32 == 0 then
         encodeNonNegativeInteger32Neg(v.toInt, false)
      else
         val hi = (v >>> 32).toInt
         val lo = v.toInt
         encodeNonNegativeInteger32Neg(hi, false)

         val nBits: Int = GetNumberOfBitsForNonNegativeInteger(lo.toLong << 32 >>> 32) // TODO: is this easier?
         bitStream.appendNBitZero(32 - nBits)
         encodeNonNegativeInteger32Neg(lo, false)
   }

   def decodeNonNegativeInteger(nBits: Int): Option[ULong] = {
      if nBits <= 32 then
         decodeNonNegativeInteger32Neg(nBits) match
            case None() => return None()
            case Some(lo) =>
               return Some(lo & 0xFFFFFFFFL)

      val hi_ret = decodeNonNegativeInteger32Neg(32)
      val lo_ret = decodeNonNegativeInteger32Neg(nBits - 32)

      (hi_ret, lo_ret) match
         case (Some(hi), Some(lo)) =>
            var v: ULong = hi & 0xFFFFFFFFL
            v = v << nBits - 32L
            v |= lo & 0xFFFFFFFFL
            return Some(v)
         case _ => return None()
      //else
      //    return decodeNonNegativeInteger32Neg(v, nBits)
   }

   def encodeNonNegativeIntegerNeg(v: ULong, negate: Boolean): Unit = {
      if v >>> 32 == 0 then
         encodeNonNegativeInteger32Neg(v.toInt, negate)
      else
         // TODO: Check Int/Long
         val hi = (v >>> 32).toInt
         var lo = v.toInt
         encodeNonNegativeInteger32Neg(hi, negate)

         /*bug !!!!*/
         if negate then
            lo = ~lo
         val nBits = GetNumberOfBitsForNonNegativeInteger(lo.toLong)
         bitStream.appendNBitZero(32 - nBits)
         encodeNonNegativeInteger32Neg(lo, false)
   }

   def BitStream_EncodeConstraintWholeNumber(v: Long, min: Long, max: Long): Unit = {
      require(min <= max)
      require(min <= v && v <= max)

      val range = max - min
      if range == 0 then
         return

      val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)
      val nBits: Int = GetNumberOfBitsForNonNegativeInteger((v - min))
      bitStream.appendNBitZero(nRangeBits - nBits);
      encodeNonNegativeInteger((v - min))
   }

   def BitStream_EncodeConstraintPosWholeNumber(v: ULong, min: ULong, max: ULong): Unit = {
      require(max >= 0 && max <= Long.MaxValue)
      require(min >= 0 && min <= max)
      require(min <= v && v <= max)

      val range: ULong = (max - min)
      if range == 0 then
         return
      val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)
      val nBits: Int = GetNumberOfBitsForNonNegativeInteger(v - min)
      bitStream.appendNBitZero(nRangeBits - nBits)
      encodeNonNegativeInteger(v - min)
   }

   def BitStream_DecodeConstraintWholeNumber(min: Long, max: Long): Option[Long] = {

      val range: ULong = (max - min)

      //    ASSERT_OR_RETURN_FALSE(min <= max);

      if range == 0 then
         return Some(min)

      val nRangeBits = GetNumberOfBitsForNonNegativeInteger(range)

      decodeNonNegativeInteger(nRangeBits) match
         case None() => return None()
         case Some(ul) => return Some(ul + min)
   }

   def BitStream_DecodeConstraintWholeNumberByte(min: Byte, max: Byte): Option[Byte] = {

      BitStream_DecodeConstraintWholeNumber(min.toLong, max.toLong) match
         case None() => None()
         case Some(l) => Some(l.toByte)
   }

   def BitStream_DecodeConstraintWholeNumberShort(min: Short, max: Short): Option[Short] = {

      BitStream_DecodeConstraintWholeNumber(min, max) match
         case None() => None()
         case Some(l) => Some(l.toShort)
   }

   def BitStream_DecodeConstraintWholeNumberInt(min: Int, max: Int): Option[Int] = {

      BitStream_DecodeConstraintWholeNumber(min, max) match
         case None() => None()
         case Some(l) => Some(l.toInt)
   }

   def BitStream_DecodeConstraintWholeNumberUByte(min: UByte, max: UByte): Option[UByte] = {

      BitStream_DecodeConstraintWholeNumber(min.unsignedToLong, max.unsignedToLong) match
         case None() => None()
         case Some(l) => Some(l.toByte)
   }

   def BitStream_DecodeConstraintWholeNumberUShort(min: UShort, max: UShort): Option[UShort] = {

      BitStream_DecodeConstraintWholeNumber(min.unsignedToLong, max.unsignedToLong) match
         case None() => None()
         case Some(l) => Some(l.toShort)
   }

   def BitStream_DecodeConstraintWholeNumberUInt(min: UInt, max: UInt): Option[UInt] = {

      BitStream_DecodeConstraintWholeNumber(min.unsignedToLong, max.unsignedToLong) match
         case None() => None()
         case Some(l) => Some(l.toInt)
   }

   def BitStream_DecodeConstraintPosWholeNumber(min: ULong, max: ULong): Option[ULong] = {
      require(max >= 0 && max <= Long.MaxValue)
      require(min >= 0 && min <= max)

      val range: ULong = max - min

      if range == 0 then
         return Some(min)

      val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)

      decodeNonNegativeInteger(nRangeBits) match
         case None() => None()
         case Some(uv) => Some(uv + min)
   }

   def BitStream_EncodeSemiConstraintWholeNumber(v: Long, min: Long): Unit = {
      assert(v >= min)
      val nBytes: Int = GetLengthInBytesOfUInt((v - min))

      /* encode length */
      BitStream_EncodeConstraintWholeNumber(nBytes.toLong, 0, 255)
      /*8 bits, first bit is always 0*/
      /* put required zeros*/
      bitStream.appendNBitZero(nBytes * 8 - GetNumberOfBitsForNonNegativeInteger((v - min)))
      /*Encode number */
      encodeNonNegativeInteger((v - min))
   }

   def BitStream_EncodeSemiConstraintPosWholeNumber(v: ULong, min: ULong): Unit = {
      assert(v >= min)
      val nBytes: Int = GetLengthInBytesOfUInt(v - min)

      /* encode length */
      BitStream_EncodeConstraintWholeNumber(nBytes.toLong, 0, 255)
      /*8 bits, first bit is always 0*/
      /* put required zeros*/
      bitStream.appendNBitZero(nBytes * 8 - GetNumberOfBitsForNonNegativeInteger(v - min))
      /*Encode number */
      encodeNonNegativeInteger(v - min)
   }

   def BitStream_DecodeSemiConstraintWholeNumber(min: Long): Option[Long] = {

      var nBytes: Long = 0
      var v: Long = 0

      BitStream_DecodeConstraintWholeNumber(0, 255) match
         case None() => return None()
         case Some(l) => nBytes = l

      var i: Long = 0
      while i < nBytes do
         decreases(nBytes - i)

         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => v = (v << 8) | (ub & 0xFF).toLong

         i += 1

      v += min

      return Some(v)
   }

   def BitStream_DecodeSemiConstraintPosWholeNumber(min: ULong): Option[ULong] = {

      var nBytes: Long = 0
      var v: ULong = 0
      BitStream_DecodeConstraintWholeNumber(0, 255) match
         case None() => return None()
         case Some(l) => nBytes = l

      var i: Long = 0
      while i < nBytes do
         decreases(nBytes - i)

         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => v = (v << 8) | (ub & 0xFF).toLong

         i += 1
      v += min
      return Some(v)
   }

   def BitStream_EncodeUnConstraintWholeNumber(v: Long): Unit = {
      val nBytes: Int = GetLengthInBytesOfSInt(v)

      /* encode length */
      BitStream_EncodeConstraintWholeNumber(nBytes.toLong, 0, 255)
      /*8 bits, first bit is always 0*/

      if v >= 0 then
         bitStream.appendNBitZero(nBytes * 8 - GetNumberOfBitsForNonNegativeInteger(v))
         encodeNonNegativeInteger(v)
      else
         bitStream.appendNBitOne(nBytes * 8 - GetNumberOfBitsForNonNegativeInteger((-v - 1)))
         encodeNonNegativeIntegerNeg((-v - 1), true)
   }


   def BitStream_DecodeUnConstraintWholeNumber(): Option[Long] = {

      var nBytes: Long = 0

      BitStream_DecodeConstraintWholeNumber(0, 255) match
         case None() => return None()
         case Some(l) => nBytes = l

      val valIsNegative: Boolean = bitStream.peekBit()

      var v: Long = if valIsNegative then Long.MaxValue else 0

      var i: Long = 0
      while i < nBytes do
         decreases(nBytes - i)

         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => v = (v << 8) | (ub & 0xFF).toLong

         i += 1

      return Some(v)
   }

   @extern
   def BitStream_EncodeReal(vVal: Double): Unit = {
      BitStream_EncodeRealBitString(java.lang.Double.doubleToRawLongBits(vVal))
   }

   private def BitStream_EncodeRealBitString(vVal: Long): Unit = {
      // according to T-REC-X.690 2021

      var v = vVal

      // 8.5.2 Plus Zero
      if v == DoublePosZeroBitString then
         BitStream_EncodeConstraintWholeNumber(0, 0, 0xFF)
         return;

      // 8.5.3 Minus Zero
      if v == DoubleNegZeroBitString then
         BitStream_EncodeConstraintWholeNumber(1, 0, 0xFF)
         BitStream_EncodeConstraintWholeNumber(0x43, 0, 0xFF)
         return;

      // 8.5.9 SpecialRealValues (2021 standard)
      if (v & ExpoBitMask) == ExpoBitMask then

      // 8.5.9 PLUS-INFINITY
         if v == DoublePosInfBitString then
            BitStream_EncodeConstraintWholeNumber(1, 0, 0xFF)
            BitStream_EncodeConstraintWholeNumber(0x40, 0, 0xFF)
            return;

         // 8.5.9 MINUS-INFINITY
         else if v == DoubleNegInfBitString then
            BitStream_EncodeConstraintWholeNumber(1, 0, 0xFF)
            BitStream_EncodeConstraintWholeNumber(0x41, 0, 0xFF)
            return;

         // 8.5.9 NOT-A-NUMBER
         else
            BitStream_EncodeConstraintWholeNumber(1, 0, 0xFF)
            BitStream_EncodeConstraintWholeNumber(0x42, 0, 0xFF)
            return;

      // 8.5.6 a)
      // fixed encoding style to binary
      // 8.5.7.2 exp has always base 2 - bit 0x20 and 0x10 are always 0
      // 8.5.7.3 F value is always zero - bit 0x08 and 0x04 are always 0
      var header = 0x80

      // 8.5.7.1
      if ((v & SignBitMask) == SignBitMask) { // check sign bit
         header |= 0x40
         v &= InverseSignBitMask // clear sign bit
      }

      val (exponent, mantissa) = CalculateMantissaAndExponent(v)

      val nManLen: Int = GetLengthInBytesOfUInt(mantissa)
      assert(nManLen <= 7) // 52 bit

      val compactExp = RemoveLeadingFFBytesIfNegative(exponent)
      val nExpLen: Int = GetLengthInBytesOfUInt(compactExp)
      assert(nExpLen >= 1 && nExpLen <= 2)

      // 8.5.7.4
      if nExpLen == 2 then
         header |= 0x01
      else if nExpLen == 3 then // this will never happen with this implementation
         header |= 0x02

      /* encode length */
      BitStream_EncodeConstraintWholeNumber(1 + nExpLen + nManLen, 0, 0xFF)

      /* encode header */
      BitStream_EncodeConstraintWholeNumber(header & 0xFF, 0, 0xFF)

      /* encode exponent */
      if exponent >= 0 then
         // fill with zeros to have a whole byte
         bitStream.appendNBitZero(nExpLen * 8 - GetNumberOfBitsForNonNegativeInteger(exponent))
         encodeNonNegativeInteger(exponent)
      else
         encodeNonNegativeInteger(compactExp)

      /* encode mantissa */
      bitStream.appendNBitZero(nManLen * 8 - GetNumberOfBitsForNonNegativeInteger(mantissa))
      encodeNonNegativeInteger(mantissa)
   }

   @extern
   def BitStream_DecodeReal(): Option[Double] = {
      BitStream_DecodeRealBitString() match
         case None() =>
            None()
         case Some(ll) =>
            Some(java.lang.Double.longBitsToDouble(ll))
   }

   private def BitStream_DecodeRealBitString(): Option[Long] = {
      bitStream.readByte() match
         case None() => None()
         case Some(length) =>
            // 8.5.2 Plus Zero
            if length == 0 then
               return Some(0)

            // invalid state
            if length < 0 || length > DoubleMaxLengthOfSentBytes then
               return None()

            bitStream.readByte() match
               case None() => None()
               case Some(header) =>
                  // 8.5.6 a)
                  if (header.unsignedToInt & 0x80) != 0x80 then
                     return None()

                  // 8.5.9 PLUS-INFINITY
                  if header == 0x40 then
                     Some(DoublePosInfBitString)

                  // 8.5.9 MINUS-INFINITY
                  else if header == 0x41 then
                     Some(DoubleNegInfBitString)

                  // 8.5.9 NOT-A-NUMBER
                  else if header == 0x42 then
                     Some(DoubleNotANumber)

                  // 8.5.3 Minus Zero
                  else if header == 0x43 then
                     Some(DoubleNegZeroBitString)

                  // Decode 8.5.7
                  else
                     DecodeRealAsBinaryEncoding(length.toInt - 1, header)
   }

   private def DecodeRealAsBinaryEncoding(lengthVal: Int, header: UByte): Option[Long] = {
      require(lengthVal >= 1 && lengthVal < DoubleMaxLengthOfSentBytes) // without header byte
      require((header.unsignedToInt & 0x80) == 0x80)
      require(bitStream.buf.length > lengthVal)
      require(bitStream.currentByte < bitStream.buf.length - lengthVal)

      // 8.5.7.2 Base
      val expFactor: Int = header.unsignedToInt match
         case x if (x & 0x10) > 0 => 3 // 2^3 = 8
         case x if (x & 0x20) > 0 => 4 // 2^4 = 16
         case _ => 1 // 2^1 = 2

      // 8.5.7.3 Factor F
      val factor = 1 << ((header & 0x0C) >>> 2)

      // 8.5.7.4 Length of Exponent
      val expLen = (header & 0x03) + 1

      // sanity check
      if expLen > lengthVal then
         return None()

      // decode exponent
      val expIsNegative = bitStream.peekBit()
      var exponent: Int = if expIsNegative then 0xFF_FF_FF_FF else 0

      var i: Int = 0
      (while i < expLen do
         decreases(expLen - i)

         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => exponent = exponent << 8 | (ub.toInt & 0xFF)

         i += 1
         ).invariant(i >= 0 && i <= expLen)

      // decode mantissa
      val length = lengthVal - expLen
      var N: ULong = 0
      var j: Int = 0
      (while j < length do
         decreases(length - j)

         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => N = (N << 8) | (ub.toInt & 0xFF)

         j += 1
         ).invariant(j >= 0 && j <= length)

      var v: Long = GetDoubleBitStringByMantissaAndExp(N * factor, expFactor * exponent)

      // 8.5.7.1 Set Sign bit
      if (header & 0x40) > 0 then
         v |= SignBitMask

      Some(v)
   }

   def BitStream_checkBitPatternPresent(bit_terminated_pattern: Array[UByte], bit_terminated_pattern_size_in_bitsVal: UByte): Int = {
      var bit_terminated_pattern_size_in_bits = bit_terminated_pattern_size_in_bitsVal
      val tmp_currentByte: Int = bitStream.currentByte
      val tmp_currentBit: Int = bitStream.currentBit
      var tmp_byte: UByte = 0

      if bitStream.currentByte.toLong * 8 + bitStream.currentBit + bit_terminated_pattern_size_in_bits.toInt > bitStream.buf.length.toLong * 8 then
         return 0

      var i: Int = 0
      while bit_terminated_pattern_size_in_bits >= 8 do
         decreases(bit_terminated_pattern_size_in_bits)

         bitStream.readByte() match
            case None() => return 0
            case Some(ub) => tmp_byte = ub

         bit_terminated_pattern_size_in_bits = 8
         if bit_terminated_pattern(i) != tmp_byte then
            bitStream.currentByte = tmp_currentByte
            bitStream.currentBit = tmp_currentBit
            return 1
         i += 1

      if bit_terminated_pattern_size_in_bits > 0 then
         bitStream.readPartialByte(bit_terminated_pattern_size_in_bits) match
            case None() => return 0
            case Some(ub) => tmp_byte = ub

         tmp_byte = (tmp_byte << (8 - bit_terminated_pattern_size_in_bits)).toByte

         if bit_terminated_pattern(i) != tmp_byte then
            bitStream.currentByte = tmp_currentByte
            bitStream.currentBit = tmp_currentBit
            return 1

      return 2
   }

   def BitStream_ReadBits_nullterminated(bit_terminated_pattern: Array[UByte], bit_terminated_pattern_size_in_bits: UByte, nMaxReadBits: Int): OptionMut[(Array[UByte], Int)] = {
      var checkBitPatternPresentResult: Int = 0

      var bitsRead: Int = 0

      val tmpStrm: BitStream = BitStream_Init(if nMaxReadBits % 8 == 0 then nMaxReadBits / 8 else nMaxReadBits / 8 + 1)

      checkBitPatternPresentResult = BitStream_checkBitPatternPresent(bit_terminated_pattern, bit_terminated_pattern_size_in_bits)
      while (bitsRead < nMaxReadBits) && (checkBitPatternPresentResult == 1) do
         decreases(nMaxReadBits - bitsRead)
         bitStream.readBit() match
            case None() => return NoneMut()
            case Some(bitVal) =>
               tmpStrm.appendBit(bitVal)
               bitsRead += 1

         if bitsRead < nMaxReadBits then
            checkBitPatternPresentResult = BitStream_checkBitPatternPresent(bit_terminated_pattern, bit_terminated_pattern_size_in_bits)

      if (bitsRead == nMaxReadBits) && (checkBitPatternPresentResult == 1) then
         checkBitPatternPresentResult = BitStream_checkBitPatternPresent(bit_terminated_pattern, bit_terminated_pattern_size_in_bits)

      if checkBitPatternPresentResult != 2 then
         return NoneMut()

      return SomeMut((tmpStrm.buf, bitsRead))
   }

   def BitStream_EncodeOctetString_no_length(arr: Array[UByte], nCount: Int): Boolean = {
      val cb = bitStream.currentBit
      var ret: Boolean = false

      if cb == 0 then
         ret = bitStream.currentByte + nCount <= bitStream.buf.length
         if ret then
            copyToArray(arr, bitStream.buf, bitStream.currentByte, nCount)
            bitStream.currentByte += nCount

      else
         ret = bitStream.appendByteArray(arr, nCount)

      ret
   }

   def BitStream_DecodeOctetString_no_length(nCount: Int): OptionMut[Array[UByte]] = {
      val cb: Int = bitStream.currentBit
      val arr: Array[UByte] = Array.fill(nCount + 1)(0)

      if cb == 0 then
         if bitStream.currentByte + nCount > bitStream.buf.length then
            return NoneMut()

         arrayCopyOffset(bitStream.buf, arr, bitStream.currentByte, bitStream.currentByte + nCount, 0)
         bitStream.currentByte += nCount

      else
         bitStream.readByteArray(nCount) match
            case NoneMut() => return NoneMut()
            case SomeMut(a) => arrayCopyOffsetLen(a, arr, 0, 0, a.length)

      SomeMut(arr)
   }

   def BitStream_EncodeOctetString_fragmentation(arr: Array[UByte], nCount: Int): Boolean = {
      var nRemainingItemsVar1: Int = nCount
      var nCurBlockSize1: Int = 0
      var nCurOffset1: Int = 0
      var ret: Boolean = nCount >= 0

      while nRemainingItemsVar1 >= 0x4000 && ret do
         decreases(nRemainingItemsVar1)
         if nRemainingItemsVar1 >= 0x10000 then
            nCurBlockSize1 = 0x10000
            BitStream_EncodeConstraintWholeNumber(0xC4, 0, 0xFF)
         else if nRemainingItemsVar1 >= 0xC000 then
            nCurBlockSize1 = 0xC000
            BitStream_EncodeConstraintWholeNumber(0xC3, 0, 0xFF)
         else if nRemainingItemsVar1 >= 0x8000 then
            nCurBlockSize1 = 0x8000
            BitStream_EncodeConstraintWholeNumber(0xC2, 0, 0xFF)
         else
            nCurBlockSize1 = 0x4000
            BitStream_EncodeConstraintWholeNumber(0xC1, 0, 0xFF)

         var i1: Int = nCurOffset1
         while i1 < nCurBlockSize1 + nCurOffset1 && ret do
            decreases(nCurBlockSize1 + nCurOffset1 - i1)
            ret = bitStream.appendByte0(arr(i1))
            i1 += 1

         nCurOffset1 += nCurBlockSize1
         nRemainingItemsVar1 -= nCurBlockSize1

      if ret then
         if nRemainingItemsVar1 <= 0x7F then
            BitStream_EncodeConstraintWholeNumber(nRemainingItemsVar1.toLong, 0, 0xFF)
         else
            bitStream.appendBit(true)
            BitStream_EncodeConstraintWholeNumber(nRemainingItemsVar1.toLong, 0, 0x7FFF)


         var i1: Int = nCurOffset1
         while i1 < (nCurOffset1 + nRemainingItemsVar1) && ret do
            decreases(nCurOffset1 + nRemainingItemsVar1 - i1)
            ret = bitStream.appendByte0(arr(i1))
            i1 += 1

      return ret
   }

   def BitStream_DecodeOctetString_fragmentation(asn1SizeMax: Long): OptionMut[Array[UByte]] = {
      require(asn1SizeMax >= 0 && asn1SizeMax < Int.MaxValue)

      val arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0)
      var nCount: Int = 0

      var nLengthTmp1: Long = 0
      var nRemainingItemsVar1: Long = 0
      var nCurBlockSize1: Long = 0
      var nCurOffset1: Long = 0

      // get header data
      BitStream_DecodeConstraintWholeNumber(0, 0xFF) match
         case None() => return NoneMut()
         case Some(l) => nRemainingItemsVar1 = l

      // 11xx_xxxx header, there is a next fragment
      while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
         decreases(asn1SizeMax - nCurOffset1) // TODO: check experimental decrease

         // get current block size
         if nRemainingItemsVar1 == 0xC4 then
            nCurBlockSize1 = 0x10000
         else if nRemainingItemsVar1 == 0xC3 then
            nCurBlockSize1 = 0xC000
         else if nRemainingItemsVar1 == 0xC2 then
            nCurBlockSize1 = 0x8000
         else if nRemainingItemsVar1 == 0xC1 then
            nCurBlockSize1 = 0x4000
         else
            return NoneMut()

         // fill current payload fragment into dest
         var i1: Int = nCurOffset1.toInt
         while (nCurOffset1 + nCurBlockSize1 <= asn1SizeMax) && (i1 < (nCurOffset1 + nCurBlockSize1).toInt) do
            decreases((nCurOffset1 + nCurBlockSize1).toInt - i1)
            bitStream.readByte() match
               case None() => return NoneMut()
               case Some(ub) => arr(i1) = ub
            i1 += 1

         // sum combined length
         nLengthTmp1 += nCurBlockSize1
         // set offset for next run
         nCurOffset1 += nCurBlockSize1

         // get next header
         BitStream_DecodeConstraintWholeNumber(0, 0xFF) match
            case None() => return NoneMut()
            case Some(l) => nRemainingItemsVar1 = l

      // 1000_0000 header, last fragment has size bigger than 255 - current byte is upper, need to get lower
      if (nRemainingItemsVar1 & 0x80) > 0 then

         nRemainingItemsVar1 <<= 8 // put upper at correct position
         // get size (lower byte)
         BitStream_DecodeConstraintWholeNumber(0, 0xFF) match
            case None() => return NoneMut()
            case Some(l) =>
               nRemainingItemsVar1 |= l // combine 15bit (7 upper, 8 lower) into size
               nRemainingItemsVar1 &= 0x7FFF // clear the control bit

      if (nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax) then
         var i1: Int = nCurOffset1.toInt

         // fill last payload fragment into dest
         while i1 < (nCurOffset1 + nRemainingItemsVar1).toInt do
            decreases((nCurOffset1 + nRemainingItemsVar1).toInt - i1)
            bitStream.readByte() match
               case None() => return NoneMut()
               case Some(ub) => arr(i1) = ub
            i1 += 1

         // add remainingSize to already written size - this var holds the absolut number in all fragments
         nLengthTmp1 += nRemainingItemsVar1

         // resize output array and copy data
         if (nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax) then
            val newArr: Array[UByte] = Array.fill(nLengthTmp1.toInt)(0)
            arrayCopyOffsetLen(arr, newArr, 0, 0, newArr.length)
            return SomeMut(newArr)
         else
            return NoneMut()

      NoneMut()
   }

   def BitStream_EncodeOctetString(arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long): Boolean = {
      var ret: Boolean = nCount.toLong >= asn1SizeMin && nCount.toLong <= asn1SizeMax

      if ret then
         if asn1SizeMax < 65536 then
            if asn1SizeMin != asn1SizeMax then
               BitStream_EncodeConstraintWholeNumber(nCount.toLong, asn1SizeMin, asn1SizeMax)
            ret = BitStream_EncodeOctetString_no_length(arr, nCount)

         else
            ret = BitStream_EncodeOctetString_fragmentation(arr, nCount)

      return ret
   }

   def BitStream_DecodeOctetString(asn1SizeMin: Long, asn1SizeMax: Long): OptionMut[Array[UByte]] = {

      if asn1SizeMax < 65536 then
         var nCount: Int = 0
         if asn1SizeMin != asn1SizeMax then
            BitStream_DecodeConstraintWholeNumber(asn1SizeMin, asn1SizeMax) match
               case None() => return NoneMut()
               case Some(l) => nCount = l.toInt
         else
            nCount = asn1SizeMin.toInt

         if (nCount >= asn1SizeMin && nCount <= asn1SizeMax) then
            return BitStream_DecodeOctetString_no_length(nCount)
         else
            return NoneMut()

      else
         return BitStream_DecodeOctetString_fragmentation(asn1SizeMax)

   }

   def BitStream_EncodeBitString(arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long): Boolean = {
      if asn1SizeMax < 65536 then
         if asn1SizeMin != asn1SizeMax then
            BitStream_EncodeConstraintWholeNumber(nCount.toLong, asn1SizeMin, asn1SizeMax)

         bitStream.appendBits(arr, nCount)

      else
         var nRemainingItemsVar1: Long = nCount.toLong
         var nCurBlockSize1: Long = 0
         var nCurOffset1: Long = 0
         while nRemainingItemsVar1 >= 0x4000 do
            decreases(nRemainingItemsVar1)

            if nRemainingItemsVar1 >= 0x10000 then
               nCurBlockSize1 = 0x10000
               BitStream_EncodeConstraintWholeNumber(0xC4, 0, 0xFF)

            else if nRemainingItemsVar1 >= 0xC000 then
               nCurBlockSize1 = 0xC000
               BitStream_EncodeConstraintWholeNumber(0xC3, 0, 0xFF)
            else if nRemainingItemsVar1 >= 0x8000 then
               nCurBlockSize1 = 0x8000
               BitStream_EncodeConstraintWholeNumber(0xC2, 0, 0xFF)
            else
               nCurBlockSize1 = 0x4000
               BitStream_EncodeConstraintWholeNumber(0xC1, 0, 0xFF)

            val t: Array[UByte] = Array.fill(nCurBlockSize1.toInt)(0) // STAINLESS: arr.slice((nCurOffset1 / 8).toInt, (nCurOffset1 / 8).toInt + nCurBlockSize1.toInt)
            bitStream.appendBits(t, nCurBlockSize1.toInt)
            nCurOffset1 += nCurBlockSize1
            nRemainingItemsVar1 -= nCurBlockSize1


         if nRemainingItemsVar1 <= 0x7F then
            BitStream_EncodeConstraintWholeNumber(nRemainingItemsVar1, 0, 0xFF)
         else
            bitStream.appendBit(true)
            BitStream_EncodeConstraintWholeNumber(nRemainingItemsVar1, 0, 0x7FFF)

         val t: Array[UByte] = Array.fill(nRemainingItemsVar1.toInt)(0) // STAINLESS: arr.slice((nCurOffset1 / 8).toInt, (nCurOffset1 / 8).toInt + nRemainingItemsVar1.toInt)
         bitStream.appendBits(t, nRemainingItemsVar1.toInt)

      true
   }

   def BitStream_DecodeBitString(asn1SizeMin: Long, asn1SizeMax: Long): OptionMut[Array[UByte]] = {
      require(asn1SizeMax <= Int.MaxValue)

      if (asn1SizeMax < 65536) {
         var nCount: Long = 0
         if asn1SizeMin != asn1SizeMax then
            BitStream_DecodeConstraintWholeNumber(asn1SizeMin, asn1SizeMax) match
               case None() => return NoneMut()
               case Some(l) => nCount = l
         else
            nCount = asn1SizeMin

         return bitStream.readBits(nCount.toInt)

      } else {
         var nRemainingItemsVar1: Long = 0
         var nCurBlockSize1: Long = 0
         var nCurOffset1: Long = 0
         var nLengthTmp1: Long = 0
         BitStream_DecodeConstraintWholeNumber(0, 0xFF) match
            case None() => return NoneMut()
            case Some(l) => nRemainingItemsVar1 = l

         val arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0)
         while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
            decreases(asn1SizeMax - nCurOffset1) // TODO: check experimental decrease
            if nRemainingItemsVar1 == 0xC4 then
               nCurBlockSize1 = 0x10000
            else if nRemainingItemsVar1 == 0xC3 then
               nCurBlockSize1 = 0xC000
            else if nRemainingItemsVar1 == 0xC2 then
               nCurBlockSize1 = 0x8000
            else if nRemainingItemsVar1 == 0xC1 then
               nCurBlockSize1 = 0x4000
            else
               return NoneMut()

            /*COVERAGE_IGNORE*/
            if nCurOffset1 + nCurBlockSize1 > asn1SizeMax then
               return NoneMut()
            /*COVERAGE_IGNORE*/

            bitStream.readBits(nCurBlockSize1.toInt) match
               case NoneMut() => return NoneMut()
               case SomeMut(t) =>
                  arrayCopyOffsetLen(t, arr, 0, (nCurOffset1 / 8).toInt, nCurBlockSize1.toInt)
                  nLengthTmp1 += nCurBlockSize1
                  nCurOffset1 += nCurBlockSize1
                  BitStream_DecodeConstraintWholeNumber(0, 0xFF) match
                     case None() => return NoneMut()
                     case Some(l) => nRemainingItemsVar1 = l

         if (nRemainingItemsVar1 & 0x80) > 0 then
            nRemainingItemsVar1 <<= 8
            BitStream_DecodeConstraintWholeNumber(0, 0xFF) match
               case None() => return NoneMut()
               case Some(l) =>
                  nRemainingItemsVar1 |= l
                  nRemainingItemsVar1 &= 0x7FFF

         if (nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax) then

            bitStream.readBits(nRemainingItemsVar1.toInt) match
               case NoneMut() => return NoneMut()
               case SomeMut(t) =>
                  arrayCopyOffsetLen(t, arr, 0, (nCurOffset1 / 8).toInt, nRemainingItemsVar1.toInt)
                  nLengthTmp1 += nRemainingItemsVar1
                  if (nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax) then
                     return SomeMut(arr)
      }
      return NoneMut()
   }
}
