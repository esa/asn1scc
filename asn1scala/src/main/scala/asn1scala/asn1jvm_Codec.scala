package asn1scala

import stainless.*
import stainless.lang.{None, Option, ghost as ghostExpr, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.*
import StaticChecks.{require as staticRequire, _}
import scala.annotation.static

val masks2: Array[Int] = Array(
   0x00000000, //         0 / 0000 0000 0000 0000 0000 0000 0000 0000 / 0x0000 0000
   0x000000FF, //       255 / 0000 0000 0000 0000 0000 0000 1111 1111 / 0x0000 00FF
   0x0000FF00, //     65280 / 0000 0000 0000 0000 1111 1111 0000 0000 / 0x0000 FF00
   0x00FF0000, //  16711680 / 0000 0000 1111 1111 0000 0000 0000 0000 / 0x00FF 0000
   0xFF000000, // -16777216 / 1111 1111 0000 0000 0000 0000 0000 0000 / 0xFF00 0000
)

val CHAR_MINUS: Byte = 45
val CHAR_PLUS: Byte = 43
val CHAR_ZERO: Byte = 48
val CHAR_NINE: Byte = 57
val CHAR_0000: Byte = 0

@extern
def assume(b: Boolean): Unit = {}.ensuring(_ => b)

/***********************************************************************************************/
/**    Byte Stream Functions                                                                  **/
/***********************************************************************************************/

// TODO is this needed? should always be called through ACN / PER / UPER
//def ByteStream_Init(count: Int): ByteStream = {
//   ByteStream(Array.fill(count)(0), 0, false)
//}

//@extern
//def runtimeAssert(condition: Boolean, s: String =""): Unit = assert(condition, s)

@extern
def writeToStdErr(s: String): Unit = Console.err.println(s)

@extern
def ByteStream_AttachBuffer(pStrm: ByteStream, buf: Array[Byte]): Unit = {
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
}

object Codec {
   import BitStream.{reader => _, *}

   @ghost @pure
   def reader(w1: Codec, w2: Codec): (Codec, Codec) = {
      require(w1.bitStream.isPrefixOf(w2.bitStream))
      val (r1, r2) = BitStream.reader(w1.bitStream, w2.bitStream)
      (Codec(r1), Codec(r2))
   }

   @ghost @pure
   def decodeUnconstrainedWholeNumber_prefixLemma_helper(c1: Codec, c2: Codec): (Codec, Codec, Option[Long], Codec, Option[Long]) = {
      require(c1.bufLength() == c2.bufLength())
      require(BitStream.validate_offset_bytes(c1.bitStream.buf.length.toLong, c1.bitStream.currentByte.toLong, c1.bitStream.currentBit.toLong,1))
      val nBytes = c1.bitStream.readBytePure()._2.unsignedToInt
      require(0 <= nBytes && nBytes <= 8)
      require(BitStream.validate_offset_bytes(c1.bitStream.buf.length.toLong, c1.bitStream.currentByte.toLong, c1.bitStream.currentBit.toLong,1 + nBytes))
      require(arrayBitRangesEq(
         c1.bitStream.buf,
         c2.bitStream.buf,
         0,
         BitStream.bitIndex(c1.bitStream.buf.length, c1.bitStream.currentByte, c1.bitStream.currentBit) + 8 + nBytes * 8
      ))

      val c2Reset = c2.resetAt(c1)
      val (c1Res, l1) = c1.decodeUnconstrainedWholeNumberPure()
      validateOffsetBytesContentIrrelevancyLemma(c1.bitStream, c2.bitStream.buf, 1 + nBytes)
      readByteRangesEq(c1.bitStream, c2.bitStream, BitStream.bitIndex(c1.bitStream.buf.length, c1.bitStream.currentByte, c1.bitStream.currentBit) + 8 + nBytes * 8)
      val (c2Res, l2) = c2Reset.decodeUnconstrainedWholeNumberPure()
      (c2Reset, c1Res, l1, c2Res, l2)
   }

   @ghost @pure @opaque @inlineOnce
   def decodeUnconstrainedWholeNumber_prefixLemma(c1: Codec, c2: Codec): Unit = {
      require(c1.bufLength() == c2.bufLength())
      require(BitStream.validate_offset_bytes(c1.bitStream.buf.length.toLong, c1.bitStream.currentByte.toLong, c1.bitStream.currentBit.toLong,1))
      val nBytes = c1.bitStream.readBytePure()._2.unsignedToInt
      require(0 <= nBytes && nBytes <= 8)
      require(BitStream.validate_offset_bytes(c1.bitStream.buf.length.toLong, c1.bitStream.currentByte.toLong, c1.bitStream.currentBit.toLong,1 + nBytes))
      require(arrayBitRangesEq(
         c1.bitStream.buf,
         c2.bitStream.buf,
         0,
         BitStream.bitIndex(c1.bitStream.buf.length, c1.bitStream.currentByte, c1.bitStream.currentBit) + 8 + nBytes * 8
      ))

      val (c2Reset, c1Res, l1, c2Res, l2) = decodeUnconstrainedWholeNumber_prefixLemma_helper(c1, c2)

      {
         val (c1_2, nBytes1) = c1.bitStream.readBytePure()
         val (c2_2, nBytes2) = c2Reset.bitStream.readBytePure()
         assert(nBytes1 == nBytes2)
         readNLSBBitsMSBFirstPrefixLemma(c1_2, c2_2, nBytes1.unsignedToInt * 8)
      }.ensuring { _ =>
         l1 == l2 && BitStream.bitIndex(c1Res.bitStream.buf.length, c1Res.bitStream.currentByte, c1Res.bitStream.currentBit) == BitStream.bitIndex(c2Res.bitStream.buf.length, c2Res.bitStream.currentByte, c2Res.bitStream.currentBit)
      }
   }

   @ghost @pure @opaque @inlineOnce
   def decodeConstrainedPosWholeNumber_prefixLemma(c1: Codec, c2: Codec, min: ULong, max: ULong): Unit = {
      require(c1.bufLength() == c2.bufLength())
      require(min <= max)
      val nBits = GetBitCountUnsigned(max - min)
      require(BitStream.validate_offset_bits(c1.bitStream.buf.length, c1.bitStream.currentByte, c1.bitStream.currentBit, nBits))
      require(arrayBitRangesEq(
         c1.bitStream.buf,
         c2.bitStream.buf,
         0,
         BitStream.bitIndex(c1.bitStream.buf.length, c1.bitStream.currentByte, c1.bitStream.currentBit) + nBits
      ))

      val c2Reset = c2.resetAt(c1)
      val (c1Res, l1) = c1.decodeConstrainedPosWholeNumberPure(min, max)
      val (c2Res, l2) = c2Reset.decodeConstrainedPosWholeNumberPure(min, max)

      {
         readNLSBBitsMSBFirstPrefixLemma(c1.bitStream, c2.bitStream, nBits)
      }.ensuring { _ =>
         l1 == l2 && BitStream.bitIndex(c1Res.bitStream.buf.length, c1Res.bitStream.currentByte, c1Res.bitStream.currentBit) == BitStream.bitIndex(c2Res.bitStream.buf.length, c2Res.bitStream.currentByte, c2Res.bitStream.currentBit)
      }
   }

   @ghost @pure @opaque @inlineOnce
   def decodeOctetString_no_length_vec_prefixLemma(c1: Codec, c2: Codec, nCount: Int): Unit = {
      require(c1.bufLength() == c2.bufLength())
      require(nCount >= 0 && nCount <= Integer.MAX_VALUE / NO_OF_BITS_IN_BYTE)
      require(c1.validate_offset_bits(8L * nCount))
      require(arrayBitRangesEq(
         c1.bitStream.buf,
         c2.bitStream.buf,
         0,
         c1.bitStream.bitIndex + 8L * nCount
      ))

      val c2Reset = c2.resetAt(c1)
      val (c1Res, v1) = c1.decodeOctetString_no_length_vec_pure(nCount)
      val (c2Res, v2) = c2Reset.decodeOctetString_no_length_vec_pure(nCount)

      {
         ()
      }.ensuring { _ =>
         c1Res.bitStream.bitIndex == c2Res.bitStream.bitIndex && v1 == v2
      }
   }
}

/**
 * Base class for the PER Codec that is used by ACN and UPER
 *
 * @param count represents the number of bytes in the internal buffer
 *
 */
case class Codec(bitStream: BitStream) {
   import Codec.*
   export bitStream.{resetAt => _, withMovedByteIndex => _, withMovedBitIndex => _, isPrefixOf => _, readNLSBBitsMSBFirstPure => _, withAlignedToByte => _, withAlignedToShort => _, withAlignedToInt => _, *}

   @ghost @pure @inline
   def resetAt(other: Codec): Codec = {
      require(bitStream.buf.length == other.bitStream.buf.length)
      Codec(bitStream.resetAt(other.bitStream))
   }

   @ghost @pure @inline
   def withMovedByteIndex(diffInBytes: Int): Codec = {
      require(BitStream.moveByteIndexPrecond(bitStream, diffInBytes))
      Codec(bitStream.withMovedByteIndex(diffInBytes))
   }

   @ghost @pure @inline
   def withMovedBitIndex(diffInBits: Int): Codec = {
      require(BitStream.moveBitIndexPrecond(bitStream, diffInBits))
      Codec(bitStream.withMovedBitIndex(diffInBits))
   }

   @pure @inline
   def bufLength(): Int = bitStream.buf.length

   @pure @inline
   def isPrefixOf(codec2: Codec): Boolean = bitStream.isPrefixOf(codec2.bitStream)

   /** ******************************************************************************************** */
   /** ******************************************************************************************** */
   /** ******************************************************************************************** */
   /** Integer Functions                                                                          * */
   /** ******************************************************************************************** */
   /** ******************************************************************************************** */
   /** ******************************************************************************************** */

   /**
    * Encode number to the bitstream. The number of bits written is
    * specified by the position of the Most Significant set bit in
    * value v. This would be 64 if the value is -1.
    *
    * @param v value that gets encoded
    *
    */
   @opaque @inlineOnce
   def encodeUnsignedInteger(v: ULong): Unit = {
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,GetBitCountUnsigned(v)))
      @ghost val w1 = snapshot(this)
      appendLSBBitsMSBFirst(v.toRaw, GetBitCountUnsigned(v))
      ghostExpr {
         val nBits = GetBitCountUnsigned(v)
         val w2 = this
         assert( w1.bitStream.buf.length == w2.bitStream.buf.length
            && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits
            && w1.isPrefixOf(w2)
         )
         val (r1, r2) = reader(w1, w2)
         BitStream.validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
         val (r2Got, vGot) = r1.bitStream.readNLSBBitsMSBFirstPure(nBits)
         assert(vGot == v.toRaw && r2Got == r2.bitStream)

      }
   } .ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      val nBits = GetBitCountUnsigned(v)
      w1.bitStream.buf.length == w2.bitStream.buf.length && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits
      && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         BitStream.validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
         val (r2Got, vGot) = r1.decodeUnsignedIntegerPure(nBits)
         vGot == v && r2Got == r2
      }
   }

   /**
    * Decode number from the bitstream. Reversal function of encodeUnsignedInteger.
    *
    * @param nBits Number of bits used for decoding. Valid range is from 0 to 64.
    * @return Unsigned integer with nBits decoded from bitstream.
    *
    */
   def decodeUnsignedInteger(nBits: Int): ULong = {
      require(nBits >= 0 && nBits <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBits))

      ULong.fromRaw(readNLSBBitsMSBFirst(nBits))
   }.ensuring(_ => buf == old(this).buf && BitStream.bitIndex(this.bitStream.buf.length, this.bitStream.currentByte, this.bitStream.currentBit) == BitStream.bitIndex(old(this).bitStream.buf.length, old(this).bitStream.currentByte, old(this).bitStream.currentBit) + nBits)

   @ghost @pure
   def decodeUnsignedIntegerPure(nBits: Int): (Codec, ULong) = {
      require(nBits >= 0 && nBits <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBits))

      val cpy = snapshot(this)
      val res = cpy.decodeUnsignedInteger(nBits)
      (cpy, res)
   }

   /**
    * Encode number to the bitstream within the [min,max] range
    * without a length. The number of bits written is specified by the
    * bits needed to represent the whole range. That would be 64bits in the
    * maximal case of min=0 to max=-1.
    *
    * @param v   number that gets encoded, needs to be within [min,max] range
    * @param min lower unsigned boundary of range
    * @param max upper unsigned boundary of range
    *
    * Remarks:
    * v, min and max get interpreted as unsigned numbers.
    *
    */
   @opaque @inlineOnce
   def encodeConstrainedPosWholeNumber(v: ULong, min: ULong, max: ULong): Unit = {
      require(min <= max)
      require(min <= v)
      require(v <= max)
      val range = max - min
      // get number of bits that get written
      val nRangeBits: Int = GetBitCountUnsigned(range)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRangeBits))

      if range != 0.toRawULong then
         // get value that gets written
         val encVal = v - min

         @ghost val nEncValBits = GetBitCountUnsigned(encVal)

         appendLSBBitsMSBFirst(encVal, nRangeBits)
      else
         ghostExpr {
            BitStream.lemmaIsPrefixRefl(bitStream)
         }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      val range = max - min
      val nBits = GetBitCountUnsigned(range)
      w1.bitStream.buf.length == w2.bitStream.buf.length && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         BitStream.validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
         val (r2Got, vGot) = r1.decodeConstrainedPosWholeNumberPure(min, max)
         vGot == v && r2Got == r2
      }
   }

   /**
    * Decode number from the bitstream. Reversal function of encodeConstrainedPosWholeNumber.
    *
    * @param min lower unsigned boundary of range
    * @param max hupper unsigned boundary of range
    * @return number in range [min,max] that gets extracted from the bitstream
    *
    * Remarks:
    * The returned value must be interpreted as an unsigned 64bit integer
    *
    */
   @opaque @inlineOnce
   def decodeConstrainedPosWholeNumber(min: ULong, max: ULong): ULong = {
      require(min <= max)
      staticRequire(
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,GetBitCountUnsigned(max - min))
      )

      val range: ULong = max - min

      // only one possible number
      if range == ULong.fromRaw(0) then
         min
      else
         val nRangeBits: Int = GetBitCountUnsigned(range)
         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRangeBits))
         val decVal = decodeUnsignedInteger(nRangeBits)

         // assert(min + decVal <= max) // TODO: T.O

         (min + decVal): ULong
   }.ensuring(res => buf == old(this).buf && BitStream.bitIndex(this.bitStream.buf.length, this.bitStream.currentByte, this.bitStream.currentBit) == BitStream.bitIndex(old(this).bitStream.buf.length, old(this).bitStream.currentByte, old(this).bitStream.currentBit) + GetBitCountUnsigned(max - min) && min <= res && res <= max)

   @ghost @pure
   def decodeConstrainedPosWholeNumberPure(min: ULong, max: ULong): (Codec, ULong) = {
      staticRequire(min <= max)
      staticRequire(
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,GetBitCountUnsigned(max - min))
      )

      val cpy = snapshot(this)
      val res = cpy.decodeConstrainedPosWholeNumber(min, max)
      (cpy, res)
   }

   /**
    * Encode number to the bitstream within the [min,max] range
    * without a length. The number of bits written is specified by the
    * bits needed to represent the whole range. That would be 64bits in the
    * maximal case of min=Long.Min_Value to max=Long.MaxValue.
    *
    * @param v number that gets encoded, needs to be within [min,max] range
    * @param min lower boundary of range
    * @param max upper boundary of range
    *
    */
   @opaque @inlineOnce
   def encodeConstrainedWholeNumber(v: Long, min: Long, max: Long): Unit = {
      staticRequire(min <= max)
      staticRequire(min <= v && v <= max)
      staticRequire(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong))) // SAM There was a bug here, we checked for N bytes even though the number returned by teh function is bits
      val range: Long = stainless.math.wrapping(max - min)
      // get number of bits that get written
      val nRangeBits: Int = GetBitCountUnsigned(range.toRawULong)

      if range != 0 then
         // get value that gets written
         val encVal = stainless.math.wrapping(v - min).toRawULong

         @ghost val nEncValBits = GetBitCountUnsigned(encVal)
         assert(nRangeBits >= nEncValBits)
         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRangeBits))

         appendLSBBitsMSBFirst(encVal, nRangeBits)
      else
         ghostExpr {
            BitStream.lemmaIsPrefixRefl(bitStream)
         }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      val range = stainless.math.wrapping(max - min)
      val nBits = GetBitCountUnsigned(range.toRawULong)
      w1.bitStream.buf.length == w2.bitStream.buf.length
      && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits
      &&
      w1.isPrefixOf(w2)
      && {
         val (r1, r2) = reader(w1, w2)
         BitStream.validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
         val (r2Got, vGot) = r1.decodeConstrainedWholeNumberPure(min, max)
         vGot == v && r2Got == r2
      }
   }

   /**
    * Whole Number decoding and casts into all data types (meths below).
    * This is the reversal function of encodeConstrainedWholeNumber.
    * encodeConstrainedWholeNumber
    *
    * @param min lower boundary of range
    * @param max upper boundary of range
    * @return decoded value within the range [min,max]
    *
    * Remarks:
    * If the decoded data does not fulfill the range condition
    * this method will fail.
    * SAM Changed to cap the value to the max/min value, so that the post condition holds
    *
   */
   def decodeConstrainedWholeNumber(min: Long, max: Long): Long = {
      require(min <= max)
      staticRequire(
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,
            GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong)
         )
      )

      val range: Long = stainless.math.wrapping(max - min)

      // only one possible value
      if range == 0 then
         min
      else
         val nRangeBits = GetBitCountUnsigned(range.toRawULong)
         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRangeBits))
         val decVal = readNLSBBitsMSBFirst(nRangeBits)

         // assert(min + decVal <= max) // TODO: Invalid

         val res = stainless.math.wrapping(min + decVal) // TODO: Overflows but seems fine?
         if res > max then
            max
         else if res < min then
            min
         else
            res
   }.ensuring( res =>
      buf == old(this).buf &&
      res >= min && res <= max &&
      BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length) &&
      BitStream.bitIndex(this.bitStream.buf.length, this.bitStream.currentByte, this.bitStream.currentBit) == BitStream.bitIndex(old(this).bitStream.buf.length, old(this).bitStream.currentByte, old(this).bitStream.currentBit) + GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong)
   )

   @ghost @pure
   def decodeConstrainedWholeNumberPure(min: Long, max: Long): (Codec, Long) = {
      staticRequire(min <= max)
      staticRequire(
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,
            GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong)
         )
      )

      val cpy = snapshot(this)
      val res = cpy.decodeConstrainedWholeNumber(min, max)
      (cpy, res)
   }

   def decodeConstrainedWholeNumberInt(min: Int, max: Int): Int = {
      require(min <= max)
      staticRequire(
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,
            GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong)
         )
      )
      val i = decodeConstrainedWholeNumber(min, max).cutToInt

      // assert(i >= min &&& i <= max)

      i
   }

   def decodeConstrainedWholeNumberShort(min: Short, max: Short): Short = {
      require(min <= max)
      staticRequire(
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,
            GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong)
         )
      )
      val s = decodeConstrainedWholeNumber(min, max).cutToShort

      // assert(s >= min &&& s <= max)

      s
   }

   def decodeConstrainedWholeNumberByte(min: Byte, max: Byte): Byte = {
      require(min <= max)
      staticRequire(
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,
            GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong)
         )
      )
      val b = decodeConstrainedWholeNumber(min, max).cutToByte

      // assert(b >= min &&& b <= max)

      b
   }

   /**
    * Encode number to the bitstream within the range [min, Long.Max_Value].
    * This function writes full bytes to the bitstream. The length is written as
    * an signed byte in front of the bytes written for the number v.
    *
    * Unused in PUS-C
    * @param v    number that gets encoded to the bitstream. Must be bigger than min.
    * @param min  lower boundary of range
    *
    */
   @opaque @inlineOnce
   def encodeSemiConstrainedWholeNumber(v: Long, min: Long): Unit = {
      require(min <= v)
      staticRequire(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,
         GetLengthForEncodingUnsigned(stainless.math.wrapping(v - min).toRawULong) + 1)
      )

      val encV: ULong = stainless.math.wrapping(v - min).toRawULong
      val nBytes = GetLengthForEncodingUnsigned(encV).toByte
      val nBits = nBytes * NO_OF_BITS_IN_BYTE

      // need space for length and value
      assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBytes + 1))

      @ghost val this1 = snapshot(this)
      // encode length
      appendByte(nBytes.toRawUByte)

      @ghost val this2 = snapshot(this)
      // encode value
      appendLSBBitsMSBFirst(encV.toRaw, nBytes * NO_OF_BITS_IN_BYTE)

      ghostExpr(BitStream.lemmaIsPrefixTransitive(this1.bitStream, this2.bitStream, this.bitStream))

      ghostExpr {
         BitStream.lemmaIsPrefixRefl(bitStream)
      }

      ghostExpr {
         BitStream.lemmaIsPrefixTransitive(this1.bitStream, this2.bitStream, this.bitStream)
         val this2Reset = this2.bitStream.resetAt(this1.bitStream)
         BitStream.readBytePrefixLemma(this2Reset, this.bitStream)
         assert(this2.bitStream.resetAt(this1.bitStream).readBytePure()._2.unsignedToInt == nBytes)
         val (r1, r2) = reader(this1, this)
         BitStream.validateOffsetBytesContentIrrelevancyLemma(this1.bitStream, this.bitStream.buf, nBytes + 1)
         assert(r1 == Codec(BitStream(snapshot(this.bitStream.buf), this1.bitStream.currentByte, this1.bitStream.currentBit)))
         assert(BitStream.validate_offset_bytes(r1.bitStream.buf.length, r1.bitStream.currentByte, r1.bitStream.currentBit, nBytes + 1))
         val (r2Got, vGot) = r1.decodeSemiConstrainedWholeNumberPure(min)
         check(r2Got == r2)
         assert((vGot & onesLSBLong(nBits)) == (v & onesLSBLong(nBits)))
         check(vGot == v)
      }

   }.ensuring(_ =>
      val w1 = old(this)
      val w2 = this
      val encV: ULong = stainless.math.wrapping(v - min).toRawULong
      val nBits = GetLengthForEncodingUnsigned(stainless.math.wrapping(v - min).toRawULong) * 8L + 8L
      buf.length == old(this).buf.length
      &&& BitStream.bitIndex(this.bitStream.buf.length, this.bitStream.currentByte, this.bitStream.currentBit) == BitStream.bitIndex(old(this).bitStream.buf.length, old(this).bitStream.currentByte, old(this).bitStream.currentBit) + GetLengthForEncodingUnsigned(stainless.math.wrapping(v - min).toRawULong) * 8L + 8L
      &&& w1.isPrefixOf(w2)
      &&& {
         val (r1, r2) = reader(w1, w2)
         BitStream.validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
         val (r2Got, vGot) = r1.decodeSemiConstrainedWholeNumberPure(min)
         (vGot == v) && r2Got == r2
      }
   )

   /**
    * Decode number from bitstream that is in range [min, Long.MaxValue].
    * This is the reversal function of encodeSemiConstrainedWholeNumber.
    *
    *  Unused in PUS-C
    * @param min lower boundary of range
    * @return value decoded from the bitstream.
    *
    */
   def decodeSemiConstrainedWholeNumber(min: Long): Long = {
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))

      // decode length
      val nBytes = readByte()
      // check bitstream precondition
      // assert(nBytes.toRaw > 0)
      // assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBytes.toRaw)) // SAM Cannot be proven now.
      // decode number
      // SAM: without an invariant here, it is impossible to prove that nBytes <= 8, so we need
      // an guard here.
      val nBytesRaw = nBytes.toRaw
      val nBits = nBytesRaw * NO_OF_BITS_IN_BYTE
      // SAM GUARD
      val v: ULong = if(!(nBits >= 0 && nBits <= 64) || !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nBits)){
         0L.toRawULong
      } else {
         readNLSBBitsMSBFirst(nBits).toRawULong
      }

      // SAM: here the post condition should be obvious, as ULong are always positive. But we can have
      // overflow, and ULong does not encode the unsigned nature in any way, so cannot work.

      val res = v + min
      if(res < min) then Long.MaxValue else res
   }.ensuring(res => res >= min)

   //  Unused in PUS-C
   @ghost @pure
   def decodeSemiConstrainedWholeNumberPure(min: Long): (Codec, Long) = {
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 1))
      staticRequire {
         val nBytes = bitStream.readBytePure()._2.unsignedToInt
         BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 1 + nBytes) && 0 <= nBytes && nBytes <= 8
      }
      val cpy = snapshot(this)
      val res = cpy.decodeSemiConstrainedWholeNumber(min)
      (cpy, res)
   }


   /**
    * Encode number to the bitstream within the range [min, Long.Max_Value].
    * This function writes full bytes to the bitstream. The length is written as
    * an signed byte in front of the bytes written for the number v.
    *
    * Unused in PUS-C
    * @param v   number that gets encoded to the bitstream. Must be bigger than min.
    * @param min lower unsigned boundary of range
    *
    */
   def encodeSemiConstrainedPosWholeNumber(v: ULong, min: ULong): Unit = {
      require(min <= v)
      staticRequire(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,
         GetLengthForEncodingUnsigned(stainless.math.wrapping(v - min)) + 1)
      )

      val encV = stainless.math.wrapping(v - min)
      val nBytes = GetLengthForEncodingUnsigned(encV.toRawULong).toByte

      /* need space for length and value */
      assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBytes + 1))

      /* encode length */
      appendByte(nBytes.toRawUByte)
      /* encode number */
      appendLSBBitsMSBFirst(encV, nBytes * NO_OF_BITS_IN_BYTE)
   }

   /**
    * Decode unsigned number from the bitstream
    * within the range [min, Long.Max_Value].
    *
    *  Unused in PUS-C
    * @param min lower unsigned boundary of range
    *
    */
   def decodeSemiConstrainedPosWholeNumber(min: ULong): ULong = {
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))

      // decode length
      val nBytes = readByte()
      // check bitstream precondition
      // SAM assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBytes.toRaw))
      // decode number
      val nBytesRaw = nBytes.toRaw
      val nBits = nBytesRaw * NO_OF_BITS_IN_BYTE
      // SAM GUARD
      val v: ULong = if(!(nBits >= 0 && nBits <= 64) || !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nBits)){
         0L.toRawULong
      } else {
         readNLSBBitsMSBFirst(nBits).toRawULong
      }
      val res: ULong = ULong.fromRaw(v + min)
      if(res < min) then
         assert(ULong.fromRaw(-1L)  >= min)
         ULong.fromRaw(Long.MaxValue)
      else
         assert(res >= min)
         res
   }//.ensuring(res => min <= res)

   /**
    * 8.3 Encoding of an integer value
    *
    * The encoding of an integer value shall be primitive.
    * The contents octets shall consist of one or more octets.
    *
    * Unused in PUS-C
    * @param v The value that is always encoded in the smallest possible number of octets.
    */
   @opaque @inlineOnce
   def encodeUnconstrainedWholeNumber(v: Long): Unit = {
      staticRequire(
         BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1 + GetLengthForEncodingSigned(v))
      )

      // call func that fulfills 8.3.2
      val nBytes: Int = GetLengthForEncodingSigned(v)
      val nBits = nBytes * NO_OF_BITS_IN_BYTE
      // need space for length and value
      assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBytes + 1))

      @ghost val this1 = snapshot(this)
      // encode number - single octet
      appendByte(nBytes.toByte.toRawUByte)

      @ghost val this2 = snapshot(this)
      // encode number
      appendLSBBitsMSBFirst(v & onesLSBLong(nBits), nBits)

      ghostExpr {
         BitStream.lemmaIsPrefixTransitive(this1.bitStream, this2.bitStream, this.bitStream)
         val this2Reset = this2.bitStream.resetAt(this1.bitStream)
         BitStream.readBytePrefixLemma(this2Reset, this.bitStream)
         assert(this2.bitStream.resetAt(this1.bitStream).readBytePure()._2.unsignedToInt == nBytes)
         val (r1, r2) = reader(this1, this)
         BitStream.validateOffsetBytesContentIrrelevancyLemma(this1.bitStream, this.bitStream.buf, nBytes + 1)
         assert(r1 == Codec(BitStream(snapshot(this.bitStream.buf), this1.bitStream.currentByte, this1.bitStream.currentBit)))
         assert(BitStream.validate_offset_bytes(r1.bitStream.buf.length, r1.bitStream.currentByte, r1.bitStream.currentBit, nBytes + 1))
         val (r2Got, vGot) = r1.decodeUnconstrainedWholeNumberPure()
         check(r2Got == r2)
         assert(vGot.isEmpty || (vGot.get & onesLSBLong(nBits)) == (v & onesLSBLong(nBits)))
         check(vGot.isEmpty || vGot.get == v)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      val nBits = NO_OF_BITS_IN_BYTE + GetLengthForEncodingSigned(v) * NO_OF_BITS_IN_BYTE
      w1.bitStream.buf.length == w2.bitStream.buf.length && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits
      && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         BitStream.validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
         val (r2Got, vGot) = r1.decodeUnconstrainedWholeNumberPure()
         (vGot.isEmpty || vGot.get == v) && r2Got == r2
      }
   }

   /**
    * 8.3 Encoding of an integer value reverse operation
    *
    * To call this func at least 2 octets have to be available on the bitstream
    * The length n is the first octet, n octets with the value follow
    * Values with n > 8 are not supported
    *
    * Unused in PUS-C
    * @return decoded number
    */
   def decodeUnconstrainedWholeNumber(): Option[Long] = {
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))

      // get length
      val nBytes = readByte().unsignedToInt
      if (!(0 <= nBytes && nBytes <= 8 && BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nBytes)))
         None[Long]()
      else
         val nBits = nBytes * NO_OF_BITS_IN_BYTE
         // check bitstream precondition
         //SAM assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBytes))
         //SAM assert(0 <= nBytes && nBytes <= 8)
         val read = readNLSBBitsMSBFirst(nBits)
         val res =
            if (read == 0 || nBits == 0 || nBits == 64 || (read & (1L << (nBits - 1))) == 0L) read
            else onesMSBLong(64 - nBits) | read // Sign extension
         // A trick to make the postcondition be true and have this function be the inverse of the encoding function
         // Note that if this doesn't hold, then the number was probably not properly encoded
         if (GetLengthForEncodingSigned(res) != nBytes) None[Long]()
         else Some(res)
   }.ensuring { res =>
      val (c2, nBytes0) = old(this).bitStream.readBytePure()
      val nBytes = nBytes0.unsignedToInt
      res match {
         case None() => true
         case Some(res) =>
            0 <= nBytes && nBytes <= 8 && c2.validate_offset_bytes(nBytes) && bitStream.buf == old(this).bitStream.buf &&
            bitIndex == old(this).bitStream.bitIndex + 8L + 8L * nBytes.toLong &&
            GetLengthForEncodingSigned(res) == nBytes
      }
   }

   @ghost @pure
   def decodeUnconstrainedWholeNumberPure(): (Codec, Option[Long]) = {
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))
      staticRequire {
         val nBytes = bitStream.readBytePure()._2.unsignedToInt
         BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1 + nBytes) && 0 <= nBytes && nBytes <= 8
      }
      val cpy = snapshot(this)
      val res = cpy.decodeUnconstrainedWholeNumber()
      (cpy, res)
   }

   /**
    * Facade function for real encoding
    *
    * Unused in PUS-C
    *
    * @param vValDouble real input in IEEE754 double format
    */
   @extern
   def encodeReal(vValDouble: Double): Unit = {
      val vVal = java.lang.Double.doubleToRawLongBits(vValDouble)
      require({
         val rawExp = (vVal & ExpoBitMask) >>> DoubleNoOfMantissaBits
         rawExp >= 0 &&& rawExp <= ((1 << 11) - 2) // 2046, 2047 is the infinity case - never end up here with infinity
      })
      require({
         val (exponent, mantissa) = CalculateMantissaAndExponent(vVal)
         val nManLen: Int = GetLengthForEncodingUnsigned(mantissa)
         val compactExp = RemoveLeadingFFBytesIfNegative(exponent.toRaw)
         val nExpLen: Int = GetLengthForEncodingUnsigned(compactExp.toLong.toRawULong)
         nExpLen >= 1 && nExpLen <= 2 && nManLen <= 7 &&
         (if (vVal == DoublePosZeroBitString ) then
            BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8)
         else if (vVal == DoubleNegZeroBitString || (vVal & ExpoBitMask) == ExpoBitMask) then
            BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 16)
         else
            BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 16 + nManLen * NO_OF_BITS_IN_BYTE  + nExpLen * NO_OF_BITS_IN_BYTE)
         )
      })
      encodeRealBitString(vVal)
   }

   /**
    * Binary encoding will be used
    * REAL = M*B^E
    * where
    * M = S*N*2^F
    *
    * ENCODING is done within three parts
    * part 1 is 1 byte header
    * part 2 is 1 or more byte for exponent
    * part 3 is 3 or more byte for mantissa (N)
    *
    * First byte
    * S :0-->+, S:1-->-1
    * Base will be always be 2 (implied by 6th and 5th bit which are zero)
    * ab: F    (0..3)
    * cd:00 --> 1 byte for exponent as 2's complement
    * cd:01 --> 2 byte for exponent as 2's complement
    * cd:10 --> 3 byte for exponent as 2's complement
    * cd:11 --> 1 byte for encoding the length of the exponent, then the exponent
    *
    * 8 7 6 5 4 3 2 1
    * +-+-+-+-+-+-+-+-+
    * |1|S|0|0|a|b|c|d|
    * +-+-+-+-+-+-+-+-+
    *
    *
    *
    */
   private def encodeRealBitString(vVal: Long): Unit = {
      // Require from CalculateMantissaAndExponent
      require({
         val rawExp = (vVal & ExpoBitMask) >>> DoubleNoOfMantissaBits
         rawExp >= 0 &&& rawExp <= ((1 << 11) - 2) // 2046, 2047 is the infinity case - never end up here with infinity
      })
      require({
         val (exponent, mantissa) = CalculateMantissaAndExponent(vVal)
         val nManLen: Int = GetLengthForEncodingUnsigned(mantissa)
         val compactExp = RemoveLeadingFFBytesIfNegative(exponent.toRaw)
         val nExpLen: Int = GetLengthForEncodingUnsigned(compactExp.toLong.toRawULong)
         nExpLen >= 1 && nExpLen <= 2 && nManLen <= 7 &&
         (if (vVal == DoublePosZeroBitString ) then
            BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8)
         else if (vVal == DoubleNegZeroBitString || (vVal & ExpoBitMask) == ExpoBitMask) then
            BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 16)
         else
            BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 16 + nManLen * NO_OF_BITS_IN_BYTE  + nExpLen * NO_OF_BITS_IN_BYTE)
         )
      })
      // according to T-REC-X.690 2021

      check(GetBitCountUnsigned(ULong.fromRaw(0xFF)) == 8)

      var v = vVal

      // 8.5.2 Plus Zero
      if v == DoublePosZeroBitString then
         check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
         encodeConstrainedWholeNumber(0, 0, 0xFF)


      // 8.5.3 Minus Zero
      else if v == DoubleNegZeroBitString then
         check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16))
         encodeConstrainedWholeNumber(1, 0, 0xFF)
         check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
         encodeConstrainedWholeNumber(0x43, 0, 0xFF)


      // 8.5.9 SpecialRealValues
      else if (v & ExpoBitMask) == ExpoBitMask then

      // 8.5.9 PLUS-INFINITY
         if v == DoublePosInfBitString then
            check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16))
            encodeConstrainedWholeNumber(1, 0, 0xFF)
            check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
            encodeConstrainedWholeNumber(0x40, 0, 0xFF)

         // 8.5.9 MINUS-INFINITY
         else if v == DoubleNegInfBitString then
            check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16))
            encodeConstrainedWholeNumber(1, 0, 0xFF)
            check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
            encodeConstrainedWholeNumber(0x41, 0, 0xFF)

         // 8.5.9 NOT-A-NUMBER
         else
            check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16))
            encodeConstrainedWholeNumber(1, 0, 0xFF)
            check( BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
            encodeConstrainedWholeNumber(0x42, 0, 0xFF)
      else
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

         val nManLen: Int = GetLengthForEncodingUnsigned(mantissa)
         assert(nManLen <= 7) // 52 bit

         val compactExp = RemoveLeadingFFBytesIfNegative(exponent.toRaw)
         val nExpLen: Int = GetLengthForEncodingUnsigned(compactExp.toLong.toRawULong)
         assert(nExpLen >= 1 && nExpLen <= 2)

         // 8.5.7.4
         if nExpLen == 2 then
            header |= 0x01
         else if nExpLen == 3 then // this will never happen with this implementation
            header |= 0x02

         /* encode length */
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16 + nManLen * NO_OF_BITS_IN_BYTE + nExpLen * NO_OF_BITS_IN_BYTE)

         encodeConstrainedWholeNumber(1 + nExpLen + nManLen, 0, 0xFF)

         check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8 + nManLen * NO_OF_BITS_IN_BYTE + nExpLen * NO_OF_BITS_IN_BYTE))
         // /* encode header */
         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8 + nManLen  + nExpLen )

         encodeConstrainedWholeNumber(header & 0xFF, 0, 0xFF)

         BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen  + nExpLen )

         // TODO this might be more complicated, because by removing the require in the bistream class, I'll in fact break the safety there
         /* encode exponent */
         if exponent.toRaw >= 0 then
            // fill with zeros to have a whole byte
            val remaining = nManLen * NO_OF_BITS_IN_BYTE + nExpLen * NO_OF_BITS_IN_BYTE
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, remaining))
            val toFill = nExpLen * NO_OF_BITS_IN_BYTE - GetBitCountUnsigned(exponent.toULong)
            val remainingAfter = remaining - toFill
            @ghost val oldThis = snapshot(this)
            appendNZeroBits(toFill)
            ghostExpr {
               BitStream.validateOffsetBitsIneqLemma(oldThis.bitStream, this.bitStream, remaining, toFill)
            }
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, remainingAfter))
            assert(GetBitCountUnsigned(exponent.toULong) <= remainingAfter)
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(exponent.toULong)))
            encodeUnsignedInteger(exponent.toULong)
            check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nManLen * NO_OF_BITS_IN_BYTE ))
         else
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE + nExpLen * NO_OF_BITS_IN_BYTE))
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE  + GetBitCountUnsigned(compactExp.toLong.toRawULong)))
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(compactExp.toLong.toRawULong)))
            encodeUnsignedInteger(compactExp.toLong.toRawULong)
            check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE ))

         /* encode mantissa */
         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nManLen * NO_OF_BITS_IN_BYTE ))
         @ghost val bitIndexBeforeAppendingZeros = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)
         val mantissaActualSize = GetBitCountUnsigned(mantissa)
         val nZeros = nManLen * NO_OF_BITS_IN_BYTE - mantissaActualSize
         appendNZeroBits(nZeros)
         assert(BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit) == bitIndexBeforeAppendingZeros + nZeros)
         assert(nManLen * NO_OF_BITS_IN_BYTE - nZeros == mantissaActualSize)
         check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, mantissaActualSize))
         encodeUnsignedInteger(mantissa)
   }


   /**
    * facade function for real decoding
    * @return decoded real value in IE754 double format
    */
   @extern
   def decodeReal(): Double = java.lang.Double.longBitsToDouble(decodeRealBitString())

   /**
    * Real decoding implementation according to the PER standard
    * If the buffer is invalid (too short or malformed), has an undefined behaviour, but does not crash.
    * @return decoded double bits as 64 bit integer
    */
   private def decodeRealBitString(): Long = {
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))
      // staticRequire({
      //    val length = readBytePure()._2.toRaw
      //    length >= 0
      //    && length != 1 // Otherwise there's only the space for the header SAM
      //    && length <= DoubleMaxLengthOfSentBytes
      //    && BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1 + length.toInt)

      // })

      // get length
      val length = readByte().toRaw
      if(length >= 0
            && length != 1 // Otherwise there's only the space for the header SAM
            && length <= DoubleMaxLengthOfSentBytes
            && BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1 + length.toInt)) then

         // 8.5.2 PLUS-ZERO
         if length == 0 then
            return 0

         // sanity check
         assert(length > 0 && length <= DoubleMaxLengthOfSentBytes)

         // get value
         assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))
         val header = readByte().toRaw

         // SAM here I don't think we should require that, but more exit if
         // that's not the case, but we can discuss

         if((header.unsignedToInt & 0x80) != 0x80) then
            //  "only binary mode supported"
            0L
         else
            // 8.5.9 PLUS-INFINITY
            if header == 0x40 then
               DoublePosInfBitString
            // 8.5.9 MINUS-INFINITY
            else if header == 0x41 then
               DoubleNegInfBitString
            // 8.5.9 NOT-A-NUMBER
            else if header == 0x42 then
               DoubleNotANumber
            // 8.5.3 MINUS-ZERO
            else if header == 0x43 then
               DoubleNegZeroBitString
            // Decode 8.5.7
            else
               check((header.unsignedToInt & 0x80) == 0x80)
               // SAM GUARD
               if((header.toRaw & 0x03) + 1 > length.toInt - 1) then
                  0L
               else
                  decodeRealFromBitStream(length.toInt - 1, header.toRawUByte)
      else
         0L
   }

   /**
    * Decode real number from bitstream, special cases are decoded by caller
    * The exponent length and other details given in the header have be be
    * decoded before calling this function
    *
    * If the buffer is invalid (too short or malformed), has an undefined behaviour, but does not crash.
    *
    * @param lengthVal already decoded exponent length
    * @param header already decoded header
    * @return decoded real number as 64bit integer
    */
   private def decodeRealFromBitStream(lengthVal: Int, header: UByte): Long = {
      require(lengthVal >= 1 && lengthVal < DoubleMaxLengthOfSentBytes) // without header byte
      require((header.toRaw & 0x03) + 1 <= lengthVal)
      require((header.unsignedToInt & 0x80) == 0x80)
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,lengthVal))

      // 8.5.7.2 Base
      val expFactor: Int = header.unsignedToInt match
         case x if (x & 0x10) > 0 => 3 // 2^3 = 8
         case x if (x & 0x20) > 0 => 4 // 2^4 = 16
         case _ => 1 // 2^1 = 2

      assert(expFactor == 1 || expFactor == 3 || expFactor == 4)

      // 8.5.7.3 Factor F
      val factor: Long = 1 << ((header.toRaw & 0x0C) >>> 2)
      assert(factor <= 8)

      // 8.5.7.4 Length of Exponent
      val expLen = (header.toRaw & 0x03) + 1

      assert(expLen >= 1 && expLen <= 4)

      // sanity check
      assert(expLen <= lengthVal) // TODO for the substraction overflow below

      // decode exponent
      var exponent: Int = if peekBit() then 0xFF_FF_FF_FF else 0
      assert(exponent == 0 || exponent == -1)

      check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,lengthVal))

      var i: Int = 0
      (while i < expLen do
         decreases(expLen - i)
         check(i < expLen && lengthVal - i > 0)
         check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,lengthVal - i))
         check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))

         exponent = exponent << 8 | (readByte().toRaw.toInt & 0xFF)

         i += 1
      ).invariant(i >= 0 && i <= expLen && BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,lengthVal - i))

      check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,lengthVal - expLen))

      // SAM guard if for the exponent read from the bitstream
      // assert(exponent < (Int.MaxValue / 4) && exponent >= -1)
      if(exponent >= (Int.MaxValue / 4) || exponent <= -1) then
         0L
      else
         // decode mantissa parts
         val length = lengthVal - expLen
         var N: Long = 0
         var j: Int = 0
         (while j < length do
            decreases(length - j)

            N = (N << 8) | (readByte().toRaw.toInt & 0xFF)

            j += 1
         ).invariant(
            j >= 0 &&
            j <= length &&
            BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length) &&
            BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, length - j))

         // assert(N < (Long.MaxValue / 8) && N >= 0) // TODO Check the doc to see if N has a max value, otherwise there is a bug here
         // SAM guard if for the mantissa read from the bitstream
         if(N >= (Long.MaxValue / 8) || N < 0) then
            0L
         else
            val x1: ULong = (N * factor).toRawULong
            val x2: Int = expFactor * exponent
            var v: Long = GetDoubleBitStringByMantissaAndExp(x1, x2)

            // 8.5.7.1 Set Sign bit
            if (header.toRaw & 0x40) > 0 then
               v |= SignBitMask

            v
   }

   def encodeOctetString_no_length(arr: Array[UByte], nCount: Int): Unit = {
      require(nCount >= 0 && nCount <= arr.length)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8L * nCount))
      appendByteArray(arr, nCount)
   }

   def decodeOctetString_no_length(nCount: Int): Array[UByte] = {
      require(nCount >= 0 && nCount <= Integer.MAX_VALUE / NO_OF_BITS_IN_BYTE)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8L * nCount))
      readByteArray(nCount)
   }

   def encodeOctetString_no_length_vec(arr: Vector[UByte], nCount: Int): Unit = {
      require(nCount >= 0 && nCount <= arr.length)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8L * nCount))
      appendByteVec(arr, nCount)
   }

   def decodeOctetString_no_length_vec(nCount: Int): Vector[UByte] = {
      require(nCount >= 0 && nCount <= Integer.MAX_VALUE / NO_OF_BITS_IN_BYTE)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8L * nCount))
      readByteVec(nCount)
   }

   @ghost @pure
   def decodeOctetString_no_length_vec_pure(nCount: Int): (Codec, Vector[UByte]) = {
      require(nCount >= 0 && nCount <= Integer.MAX_VALUE / NO_OF_BITS_IN_BYTE)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8L * nCount))
      val cpy = snapshot(this)
      val res = cpy.readByteVec(nCount)
      (cpy, res)
   }

   def encodeOctetString_fragmentation(arr: Array[UByte], nCount: Int) = {
      require(nCount >= 0 && nCount <= arr.length)
      require(nCount < Int.MaxValue / 8 - 2 - (nCount / 0x4000) ) // To avoid overflow of the available length checks
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCount + (nCount / 0x4000) + 2)) // Room for information bytes + data

      var nRemainingItemsVar1: Int = nCount
      var nCurBlockSize1: Int = 0
      var nCurOffset1: Int = 0 // Which represents the currently written bytes number SAM

      @ghost val bufLength = bitStream.buf.length
      @ghost val bitIndexBeforeInnerWhile = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)
      @ghost val currentByteBeforeInnerWhile = bitStream.currentByte
      @ghost val currentBitBeforeInnerWhile = bitStream.currentBit

      val resInner = encodeOctetString_fragmentation_innerWhile(arr, nCount, 0, nRemainingItemsVar1, nCurOffset1, BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ))
      assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, resInner._1 + 2))
      ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, resInner._1 + 2)))

      nRemainingItemsVar1 = resInner._1
      nCurOffset1 = resInner._2

      assert(bufLength == bitStream.buf.length)

      @ghost val bitIndexAfterInnerWhile = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)
      @ghost val currentByteAfterInnerWhile = bitStream.currentByte
      @ghost val currentBitAfterInnerWhile = bitStream.currentBit

      assert(bitIndexAfterInnerWhile <= bitIndexBeforeInnerWhile + (nCount / 0x4000) * 8 + nCount * 8 - nRemainingItemsVar1 * 8)
      assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + 2))
      if nRemainingItemsVar1 <= 0x7F then
         ghostExpr(lemmaGetBitCountUnsignedFFEqualsEight())
         assert(GetBitCountUnsigned(stainless.math.wrapping(0xFF).toRawULong) == 8)
         check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8)) // == GetLengthForEncodingUnsigned(0xFF)
         encodeConstrainedWholeNumber(nRemainingItemsVar1.toLong, 0, 0xFF)

         @ghost val bufLengthAfterEncodingByte = bitStream.buf.length
         @ghost val currentBitAfterEncodingByte = bitStream.currentBit
         @ghost val currentByteAfterEncodingByte = bitStream.currentByte
         @ghost val bitIndexAfterEncodingByte = BitStream.bitIndex(bufLength, currentByteAfterEncodingByte, currentBitAfterEncodingByte)

         assert(bufLengthAfterEncodingByte == bufLength)
         assert(bitIndexAfterEncodingByte == bitIndexAfterInnerWhile + 8)
         assert(BitStream.validate_offset_bytes(bitStream.buf.length, currentByteAfterEncodingByte , currentBitAfterEncodingByte, nRemainingItemsVar1 + 1))
         assert(BitStream.bitIndex(bufLength, bitStream.currentByte, bitStream.currentBit) == bitIndexAfterInnerWhile + 8)
         assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + 1))
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + 1)))
      else
         // SAM here we write 1000000000000000 (16 bits), to indicate that more than 128 elements remain
         ghostExpr(check(BitStream.validate_offset_bits(bufLength, bitStream.currentByte, bitStream.currentBit, 1)))
         appendBit(true)
         ghostExpr(lemmaGetBitCountUnsigned7FFFEquals15())
         assert(GetBitCountUnsigned(stainless.math.wrapping(0x7FFF).toRawULong) == 15)
         assert(BitStream.bitIndex(bufLength, bitStream.currentByte, bitStream.currentBit) == bitIndexAfterInnerWhile + 1)
         ghostExpr(check(BitStream.validate_offset_bits(bufLength, bitStream.currentByte, bitStream.currentBit, 15))) // == GetLengthForEncodingUnsigned(0x7FFF)

         encodeConstrainedWholeNumber(nRemainingItemsVar1.toLong, 0, 0x7FFF)
         assert(BitStream.bitIndex(bufLength, bitStream.currentByte, bitStream.currentBit) == bitIndexAfterInnerWhile + 16)
         assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 ))
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 )))

      assert(bufLength == bitStream.buf.length)
      @ghost val bitIndexAfterLength = BitStream.bitIndex(bufLength, bitStream.currentByte, bitStream.currentBit)
      @ghost val currentByteAfterLength = bitStream.currentByte
      @ghost val currentBitAfterLength = bitStream.currentBit

      assert(bitIndexAfterLength <= bitIndexAfterInnerWhile + 16)
      ghostExpr(check(BitStream.validate_offset_bytes(bufLength, currentByteAfterLength, currentBitAfterLength, nRemainingItemsVar1)))

      // var i1: Int = nCurOffset1
      // (while i1 < (nCurOffset1 + nRemainingItemsVar1) do
      //    decreases(nCurOffset1 + nRemainingItemsVar1 - i1)
      //    appendByte(arr(i1))
      //    i1 += 1
      // ).invariant(
      //    nCurOffset1 + nRemainingItemsVar1 - i1 >= 0 &&
      //    nCurOffset1 + nRemainingItemsVar1 - i1 <= nRemainingItemsVar1 &&
      //    i1 <= nCount &&
      //    i1 <= arr.length &&
      //    BitStream.validate_offset_bytes(bufLength, currentByteAfterLength, currentBitAfterLength, nCurOffset1 + nRemainingItemsVar1 - i1)
      //    )

      if(nRemainingItemsVar1 > 0) then
         encodeOctetString_fragmentation_innerMostWhile(arr, nCurOffset1, nCurOffset1 + nRemainingItemsVar1, BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ))
   }

   def encodeOctetString_fragmentation_innerWhile(arr: Array[UByte], nCount: Int, @ghost currentlyWrittenBlocksOf0x4000: Int, nRemainingItemsVar1: Int, nCurOffset1: Int, bitIndex: Long ): (Int, Int) = {
      require(nRemainingItemsVar1 >= 0)
      staticRequire(currentlyWrittenBlocksOf0x4000 >= 0)
      require(nCurOffset1 >= 0)
      require(nCount >= 0)
      require(nCount < Int.MaxValue - (nCount / 0x4000))
      require(nRemainingItemsVar1 < Int.MaxValue / 8)
      require(nRemainingItemsVar1  < Int.MaxValue / 8 - 2 - (nRemainingItemsVar1 / 0x4000))
      require(nRemainingItemsVar1 <= nCount)
      staticRequire(currentlyWrittenBlocksOf0x4000 < Int.MaxValue / 0x4000) // to avoid overflow
      require(nCount <= arr.length)
      staticRequire(nCount - currentlyWrittenBlocksOf0x4000 * 0x4000 == nRemainingItemsVar1)
      staticRequire(nCurOffset1 == currentlyWrittenBlocksOf0x4000 * 0x4000)
      require(BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) == bitIndex)
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + (nRemainingItemsVar1 / 0x4000) + 2))

      decreases(nRemainingItemsVar1)

      @ghost val bufferLength = bitStream.buf.length

      if (nRemainingItemsVar1 >= 0x4000) then
         val nCurBlockSize1 =
            if nRemainingItemsVar1 >= 0x10000 then
               0x10000
            else if nRemainingItemsVar1 >= 0xC000 then
               0xC000
            else if nRemainingItemsVar1 >= 0x8000 then
               0x8000
            else
               0x4000

         // Write to the buffer the block header and return the number of blocks of 0x4000 it would represent
         val blockSize: Long = nCurBlockSize1 match {
            case 0x10000 =>
               0xC4
            case 0xC000 =>
               0xC3
            case 0x8000 =>
               0xC2
            case 0x4000 =>
               0xC1
         }

         val nNewBlocksOf0x4000 = blockSize match {
            case 0xC4 =>
               4
            case 0xC3 =>
               3
            case 0xC2 =>
               2
            case 0xC1 =>
               1
         }

         @ghost val bitIndexBeforeHeader = BitStream.bitIndex(bufferLength, bitStream.currentByte, bitStream.currentBit )
         assert(GetBitCountUnsigned(0xFF.toRawULong) == 8)

         ghostExpr(check(bitIndexBeforeHeader == bitIndex))

         encodeConstrainedWholeNumber(blockSize, 0, 0xFF)

         assert(bitStream.buf.length == bufferLength)


         @ghost val bitIndexAfterHeader = BitStream.bitIndex(bufferLength, bitStream.currentByte, bitStream.currentBit )
         @ghost val currentByteAfterHeader = bitStream.currentByte
         @ghost val currentBitAfterHeader = bitStream.currentBit

         assert(bitIndexBeforeHeader + NO_OF_BITS_IN_BYTE == bitIndexAfterHeader)
         assert( bitIndexAfterHeader <= bitIndexBeforeHeader + 4 * NO_OF_BITS_IN_BYTE)
         ghostExpr(check(( bitIndexAfterHeader <= bitIndexBeforeHeader + 4 * NO_OF_BITS_IN_BYTE)))

         val validatedOffset1: Int = nRemainingItemsVar1 + (nRemainingItemsVar1 / 0x4000) - 1 + 2

         assert(BitStream.validate_offset_bytes(bufferLength, bitStream.currentByte, bitStream.currentBit, validatedOffset1))
         ghostExpr(check(BitStream.validate_offset_bytes(bufferLength, currentByteAfterHeader, currentBitAfterHeader, validatedOffset1)))
         assert(BitStream.validate_offset_bytes(bufferLength,bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + (nRemainingItemsVar1 / 0x4000) - 1 + 2))

         assert(nCount == nRemainingItemsVar1 + nCurOffset1)
         assert(nRemainingItemsVar1 >= nCurBlockSize1)
         assert(nCurOffset1 + nCurBlockSize1 <= nCount)
         assert(nCurOffset1 + nCurBlockSize1 <= arr.length)
         assert(nCurOffset1 + nCurBlockSize1 >= 0)

         encodeOctetString_fragmentation_innerMostWhile(arr, nCurOffset1, nCurOffset1 + nCurBlockSize1, BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ))

         assert(bitStream.buf.length == bufferLength)

         @ghost val bitIndexAfterAppending = BitStream.bitIndex(bufferLength, bitStream.currentByte, bitStream.currentBit )
         @ghost val currentByteAfterAppending = bitStream.currentByte
         @ghost val currentBitAfterAppending = bitStream.currentBit

         assert(bitIndexAfterAppending == bitIndexAfterHeader + nCurBlockSize1 * 8)
         ghostExpr(check((bitIndexAfterAppending == bitIndexAfterHeader + nCurBlockSize1 * 8)))

         assert(bitIndexAfterAppending - bitIndexAfterHeader == nCurBlockSize1 * 8)

         val newRemaingItems = nRemainingItemsVar1 - nCurBlockSize1
         @ghost val newWrittenBlocks = currentlyWrittenBlocksOf0x4000 + nNewBlocksOf0x4000
         val newOffset = nCurOffset1 + nCurBlockSize1

         val validatedOffset2: Int = newRemaingItems + (newRemaingItems / 0x4000) + 2
         assert(validatedOffset1 >= 0)
         assert(validatedOffset2 >= 0)

         assert(validatedOffset1 - validatedOffset2 == nRemainingItemsVar1 + (nRemainingItemsVar1 / 0x4000) - 1 + 2 - (nRemainingItemsVar1 - nCurBlockSize1) - (nRemainingItemsVar1 - nCurBlockSize1) / 0x4000 - 2)
         assert(validatedOffset1 - validatedOffset2 ==  nCurBlockSize1 - 1 + nCurBlockSize1 / 0x4000)
         assert(validatedOffset1 - validatedOffset2 >= 0)

         assert(BitStream.invariant(currentBitAfterHeader, currentByteAfterHeader, bufferLength))
         assert(BitStream.invariant(currentBitAfterAppending, currentByteAfterAppending, bufferLength))
         assert(validatedOffset1 >= 0)
         assert(validatedOffset2 >= 0)
         assert(bitIndexAfterHeader <= bitIndexAfterAppending)
         assert(validatedOffset2 <= validatedOffset1)
         assert(8*validatedOffset2 <= 8*validatedOffset1)
         assert(BitStream.bitIndex(bufferLength, currentByteAfterHeader, currentBitAfterHeader) == bitIndexAfterHeader)
         assert(BitStream.bitIndex(bufferLength, currentByteAfterAppending, currentBitAfterAppending) == bitIndexAfterAppending)
         assert(bitIndexAfterAppending - bitIndexAfterHeader <= 8*validatedOffset1 - 8*validatedOffset2)

         assert(BitStream.validate_offset_bytes(bufferLength, currentByteAfterHeader, currentBitAfterHeader, validatedOffset1)) // should pass

         assert(BitStream.validate_offset_bits(bufferLength, currentByteAfterHeader, currentBitAfterHeader, 8*validatedOffset1))


         ghostExpr(lemmaAdvanceBitIndexLessMaintainOffset(bufferLength, currentByteAfterHeader, currentBitAfterHeader, bitIndexAfterHeader, currentByteAfterAppending, currentBitAfterAppending, bitIndexAfterAppending, 8*validatedOffset1, 8*validatedOffset2))

         assert((BitStream.validate_offset_bits(bufferLength, bitStream.currentByte, bitStream.currentBit, 8*validatedOffset2)))
         ghostExpr(lemmaValidateOffsetBitsBytesEquiv(bufferLength, bitStream.currentByte, bitStream.currentBit, 8*validatedOffset2, validatedOffset2))
         assert((BitStream.validate_offset_bytes(bufferLength, bitStream.currentByte, bitStream.currentBit, validatedOffset2)))

         val res = encodeOctetString_fragmentation_innerWhile(arr, nCount, newWrittenBlocks , newRemaingItems, newOffset , BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit))
         val resNRemaining = res._1
         val resOffset = res._2
         assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, res._1 + 2))
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, res._1 + 2)))
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, resNRemaining + 2)))
         @ghost val bitIndexAfterRecursive = BitStream.bitIndex(bufferLength, bitStream.currentByte, bitStream.currentBit )

         assert(resNRemaining + resOffset == nCount)
         assert(resNRemaining <= 0x4000)
         assert(bitIndexAfterRecursive <= bitIndexAfterAppending + (newRemaingItems / 0x4000) * 8 + newRemaingItems * 8 - resNRemaining*8)
         assert(newRemaingItems <= nRemainingItemsVar1)
         assert(newRemaingItems ==  nRemainingItemsVar1 - nCurBlockSize1)
         assert(bitIndexAfterAppending == bitIndexAfterHeader + nCurBlockSize1 * 8 )
         assert(bitIndexBeforeHeader == bitIndex)
         assert(bitIndexAfterHeader <= bitIndexBeforeHeader + 4 * NO_OF_BITS_IN_BYTE)
         // assert(bitIndexAfterHeader + (nCurBlockSize1 / 0x4000) * 8 - resNRemaining * 8 <=  bitIndex - resNRemaining*8 )
         // assert(bitIndexAfterHeader + (nCurBlockSize1 / 0x4000) * 8 + ( nRemainingItemsVar1 ) * 8 - resNRemaining* 8 <=  bitIndex + nRemainingItemsVar1 * 8 - resNRemaining*8 )
         // assert(bitIndexAfterHeader + nCurBlockSize1 * 8 + (( nRemainingItemsVar1 - nCurBlockSize1) / 0x4000) * 8 + ( nRemainingItemsVar1 - nCurBlockSize1) * 8 - resNRemaining * 8 <=  bitIndex + (nRemainingItemsVar1 / 0x4000) * 8 + nRemainingItemsVar1 * 8 - resNRemaining*8 )
         assert(bitIndexAfterAppending + (newRemaingItems / 0x4000) * 8 + newRemaingItems * 8 - resNRemaining*8 <=  bitIndex + (nRemainingItemsVar1 / 0x4000) * 8 + nRemainingItemsVar1 * 8 - resNRemaining*8 )
         assert(bitIndexAfterRecursive <= bitIndex + (nRemainingItemsVar1 / 0x4000) * 8 + nRemainingItemsVar1 * 8 - resNRemaining*8)

         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, resNRemaining + 2)))
         ghostExpr(check( BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) <= bitIndexAfterRecursive))
         ghostExpr(check( BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) <= bitIndex + (nRemainingItemsVar1 / 0x4000) * 8 + nRemainingItemsVar1 * 8 - resNRemaining*8))
         ghostExpr(check(resNRemaining + resOffset == nCount ))
         ghostExpr(check(resNRemaining >= 0 ))
         ghostExpr(check(resOffset >= 0 ))
         ghostExpr(check(resNRemaining <= 0x4000 ))

         (resNRemaining, resOffset)
      else
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + 2)))
         ghostExpr(check( BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) <= bitIndex + (nRemainingItemsVar1 / 0x4000) * 8 + nRemainingItemsVar1 * 8 - nRemainingItemsVar1 * 8 ))
         ghostExpr(check(nRemainingItemsVar1 + nCurOffset1 == nCount ))
         ghostExpr(check(nRemainingItemsVar1 >= 0 ))
         ghostExpr(check(nCurOffset1 >= 0 ))
         ghostExpr(check(nRemainingItemsVar1 <= 0x4000 ))
         (nRemainingItemsVar1, nCurOffset1)
   }.ensuring(res =>
         BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) <= bitIndex + (nRemainingItemsVar1 / 0x4000) * 8 + nRemainingItemsVar1 * 8 - res._1 * 8 &&
         BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, res._1 + 2) &&
         BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length) &&
         res._1 + res._2 == nCount &&
         res._1 >= 0 &&
         res._2 >= 0 &&
         res._1 <= 0x4000 &&
         old(this).bitStream.buf.length == bitStream.buf.length
   )

   /**
     * Append the bytes in the given array to the BitStream, starting at from index, to the to index (exclusive)
     *
     * @param bitStream
     * @param arr
     * @param from
     * @param to
     */
   def encodeOctetString_fragmentation_innerMostWhile(arr: Array[UByte], from: Int, to: Int, bitIndex: Long): Unit = {
      require(from >= 0)
      require(to >= 0)
      require(from < to)
      require(to <= arr.length)
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,to - from))
      require(BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) == bitIndex)
      require((to - from)  < Int.MaxValue / NO_OF_BITS_IN_BYTE) // to avoid overflow
      require(bitIndex < Long.MaxValue - (to - from) * NO_OF_BITS_IN_BYTE) // to avoid overflow
      decreases(to - from)

      @ghost val bitIndexBeforeAppending = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )

      @ghost val bufLength = bitStream.buf.length

      bitStream.appendByte(arr(from))

      assert(bitStream.buf.length == bufLength)

      @ghost val bitIndexBeforeRecursive = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )
      assert(bitIndexBeforeRecursive == bitIndexBeforeAppending + NO_OF_BITS_IN_BYTE)

      if from + 1 < to then
         encodeOctetString_fragmentation_innerMostWhile(arr, from + 1, to, bitIndex + NO_OF_BITS_IN_BYTE)

         assert(bitStream.buf.length == bufLength)
         @ghost val bitIndexAfterRecursive = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )
         assert(bitIndexAfterRecursive == bitIndexBeforeRecursive  + (to - from - 1) * NO_OF_BITS_IN_BYTE)
         assert(NO_OF_BITS_IN_BYTE + (to - from - 1) * NO_OF_BITS_IN_BYTE == (to - from) * NO_OF_BITS_IN_BYTE)
         assert(bitIndexAfterRecursive == bitIndexBeforeAppending + NO_OF_BITS_IN_BYTE + (to - from - 1) * NO_OF_BITS_IN_BYTE)
   }.ensuring(_ =>
      val oldBitStream = old(this).bitStream
      BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) == bitIndex + (to - from) * NO_OF_BITS_IN_BYTE &&
      oldBitStream.buf.length == bitStream.buf.length
   )


   @ghost
   @opaque
   @inlineOnce
   def lemmaGetBitCountUnsignedFFEqualsEight(): Unit = {

   }.ensuring(_ => GetBitCountUnsigned(stainless.math.wrapping(0xFF).toRawULong) == 8)

   @ghost
   @opaque
   @inlineOnce
   def lemmaGetBitCountUnsigned7FFFEquals15(): Unit = {

   }.ensuring(_ => GetBitCountUnsigned(stainless.math.wrapping(0x7FFF).toRawULong) == 15)

   @ghost
   @opaque
   @inlineOnce
   def lemmaValidateOffsetBitsBytesEquiv(bufLength: Int, currentByte: Int, currentBit: Int, offsetBits: Int, offsetBytes: Int): Unit = {
      require(BitStream.invariant(currentBit, currentByte, bufLength))
      require(offsetBits >= 0)
      require(offsetBytes >= 0)
      require(offsetBytes < Int.MaxValue / 8)
      require(offsetBits == offsetBytes * 8)
      require(BitStream.validate_offset_bits(bufLength, currentByte, currentBit, offsetBits))

   }.ensuring(_ => BitStream.validate_offset_bytes(bufLength, currentByte, currentBit, offsetBytes))
   /**
     * Takes more than 100sec to verify, that's why it is extracted to a lemma, even though it does not need a specific proof
     *
     * @param bufLength
     * @param currentByte1
     * @param currentBit1
     * @param bitIndex1
     * @param currentByte2
     * @param currentBit2
     * @param bitIndex2
     * @param offset1
     * @param offset2
     */
   @ghost
   @opaque
   @inlineOnce
   def lemmaAdvanceBitIndexLessMaintainOffset(
      bufLength: Int,
      currentByte1: Int, currentBit1: Int, bitIndex1: Long,
      currentByte2: Int, currentBit2: Int, bitIndex2: Long,
      offset1Bits: Int,
      offset2Bits: Int): Unit = {
         require(BitStream.invariant(currentBit1, currentByte1, bufLength))
         require(BitStream.invariant(currentBit2, currentByte2, bufLength))
         require(offset1Bits >= 0)
         require(offset2Bits >= 0)
         require(bitIndex1 <= bitIndex2)
         require(offset2Bits <= offset1Bits)
         require(BitStream.bitIndex(bufLength, currentByte1, currentBit1) == bitIndex1)
         require(BitStream.bitIndex(bufLength, currentByte2, currentBit2) == bitIndex2)
         require(bitIndex2 - bitIndex1 <= offset1Bits - offset2Bits)
         require(BitStream.validate_offset_bits(bufLength, currentByte1, currentBit1, offset1Bits))

      }.ensuring(_ => BitStream.validate_offset_bits(bufLength, currentByte2, currentBit2, offset2Bits))


      /**
        * Undefined behaviour if the bitstream is invalid (i.e., not supported format or wrong)
        *
        * @param asn1SizeMax
        */
   def decodeOctetString_fragmentation(asn1SizeMax: Long): Array[UByte] = {
      require(asn1SizeMax >= 0 && asn1SizeMax < Int.MaxValue)

      assert(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))

      val arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0.toRawUByte)
      var nCount: Int = 0

      assert(arr.length == asn1SizeMax.toInt)

      var nLengthTmp1: Long = 0
      // var nCurBlockSize1: Long = 0
      // var nCurOffset1: Long = 0

      // 11xx_xxxx header, there is a next fragment
      val resInnerWhile: (Long, Long) = decodeOctetString_fragmentation_innerWhile(arr, asn1SizeMax, 0)
      var nRemainingItemsVar1 = resInnerWhile._1
      var nCurOffset1 = resInnerWhile._2
      nLengthTmp1 = nCurOffset1 // In the loops containing code, the nLengthTmp1 is updated along with the current offset, so has the same value after the loop
      if(nRemainingItemsVar1 == -1L || nCurOffset1 == -1L) then
         return arr

      // SAM given the code above in the encode function, this header is written when the number of remaining
      // elements in <= 0x7F which is 127, and not 255
      // 1000_0000 header, last fragment has size bigger than 255 - current byte is upper, need to get lower
      if (nRemainingItemsVar1 & 0x80) > 0 then

         nRemainingItemsVar1 <<= 8 // put upper at correct position

         assert(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
         // SAM Guard if
         if(!BitStream.validate_offset_byte(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)) then
            return arr

         assert(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
         ghostExpr(lemmaGetBitCountUnsignedFFEqualsEight())
         assert(GetBitCountUnsigned(stainless.math.wrapping(0xFF).toRawULong) == 8)
         assert(BitStream.validate_offset_byte(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit))
         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8))

         // get size (lower byte)
         nRemainingItemsVar1 |= decodeConstrainedWholeNumber(0, 0xFF) // combine 15bit (7 upper, 8 lower) into size
         nRemainingItemsVar1 &= 0x7FFF // clear the control bit

      // SAM Guard if
      if(nRemainingItemsVar1 < 0 || nRemainingItemsVar1 > asn1SizeMax || nCurOffset1 > asn1SizeMax - nRemainingItemsVar1 ) then
         return arr

      assert(nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax) // TODO check with C implementation and standard

      var i1: Int = nCurOffset1.toInt


      // This loop is implemented with the same helper function as the inner most loop
      // fill last payload fragment into dest
      // (while i1 < (nCurOffset1 + nRemainingItemsVar1).toInt do
      //    decreases((nCurOffset1 + nRemainingItemsVar1).toInt - i1)
      //    arr(i1) = readByte()
      //    i1 += 1
      // ).invariant(true) // TODO invariant

      assert(arr.length == asn1SizeMax.toInt)

      decodeOctetString_fragmentation_innerMostWhile(arr, asn1SizeMax, nCurOffset1.toInt, (nCurOffset1 + nRemainingItemsVar1).toInt)

      // add remainingSize to already written size - this var holds the absolut number in all fragments
      nLengthTmp1 += nRemainingItemsVar1 // which is equal to curOffset + nRemainingItemsVar1


      // // resize output array and copy data
      assert((nLengthTmp1 >= 0) && (nLengthTmp1 <= asn1SizeMax))
      // assert((nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax)) // TODO check with C implementation and standard
      // SAM according to the C implementation, if the nLengthTmp1 == 0, it should fail, so
      if(nLengthTmp1 == 0) then
         return Array.empty

      val newArr: Array[UByte] = Array.fill(nLengthTmp1.toInt)(0.toRawUByte)
      arrayCopyOffsetLen(arr, newArr, 0, 0, newArr.length)
      newArr
   }.ensuring(_ => BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))


   /**
    * Decodes the fragments until it encounters a header s.t., nRemainingItemsVar1 & 0xC0) != 0xC0
     * Undefined behaviour if the bitstream is invalid (i.e., not supported format or wrong)
     * Returns the remaining number of items and the current offset (or (-1, -1) in case of error)
     *
     * @param arr
     * @param asn1SizeMax
     * @param nRemainingItemsVar1
     * @param nCurOffset1
     */
   def decodeOctetString_fragmentation_innerWhile(arr: Array[UByte], asn1SizeMax: Long, nCurOffset1: Long): (Long, Long) = {
      require(asn1SizeMax >= 0 && asn1SizeMax < Int.MaxValue)
      require(arr.length == asn1SizeMax)
      require(nCurOffset1 >= 0)
      require(nCurOffset1 <= asn1SizeMax)
      staticRequire(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
      decreases(asn1SizeMax - nCurOffset1)

      // Implements the following loop
      // (while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
      //    decreases(asn1SizeMax - nCurOffset1) // TODO: check experimental decrease

      //    // get current block size
      //    if nRemainingItemsVar1 == 0xC4 then
      //       nCurBlockSize1 = 0x10000
      //    else if nRemainingItemsVar1 == 0xC3 then
      //       nCurBlockSize1 = 0xC000
      //    else if nRemainingItemsVar1 == 0xC2 then
      //       nCurBlockSize1 = 0x8000
      //    else if nRemainingItemsVar1 == 0xC1 then
      //       nCurBlockSize1 = 0x4000
      //    else
      //       return arr
      //       assert(false, "unsupported format")

      //    // fill current payload fragment into dest
      //    decodeOctetString_fragmentation_innerMostWhile(arr, asn1SizeMax, nCurOffset1, nCurOffset1 + nCurBlockSize1)

      //    // sum combined length
      //    nLengthTmp1 += nCurBlockSize1
      //    // set offset for next run
      //    nCurOffset1 += nCurBlockSize1

      //    // get next header
      //    nRemainingItemsVar1 = decodeConstrainedWholeNumber(0, 0xFF)

      // ).invariant(true) // TODO invariant

      if(!BitStream.validate_offset_byte(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)) then
         return (-1, -1L)

      assert(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))

      assert(GetBitCountUnsigned(stainless.math.wrapping(0xFF).toRawULong) == 8)
      assert(BitStream.validate_offset_byte(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit))
      assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8))
      val nRemainingItemsVar1: Long = decodeConstrainedWholeNumber(0, 0xFF)


      // get current block size
      val nCurBlockSize1: Long = if nRemainingItemsVar1 == 0xC4 then
         0x10000
      else if nRemainingItemsVar1 == 0xC3 then
         0xC000
      else if nRemainingItemsVar1 == 0xC2 then
         0x8000
      else if nRemainingItemsVar1 == 0xC1 then
         0x4000
      else if (nRemainingItemsVar1 & 0xC0) != 0xC0 then
         // End of the recursion, out with valid values
         1L
      else
         // unsupported format
         -1L
         // assert(false, "unsupported format")

      if nCurBlockSize1 == -1L then
         return (-1, -1L)

      if nCurBlockSize1 == 1L then
         return (nRemainingItemsVar1, nCurOffset1)

      if(nCurOffset1 > (asn1SizeMax.toLong - nCurBlockSize1)) then
         return (-1, -1L)

      assert(arr.length == asn1SizeMax.toInt)
      decodeOctetString_fragmentation_innerMostWhile(arr, asn1SizeMax, nCurOffset1.toInt, nCurOffset1.toInt + nCurBlockSize1.toInt)

      // sum combined length
      // nLengthTmp1 += nCurBlockSize1
      // val newNLengthTmp1 = nLengthTmp1 + nCurBlockSize1
      // set offset for next run
      // nCurOffset1 += nCurBlockSize1
      val newNCurOffset1 = nCurOffset1 + nCurBlockSize1

      // get next header
      // nRemainingItemsVar1 = decodeConstrainedWholeNumber(0, 0xFF)

      assert(arr.length == asn1SizeMax.toInt)
      decodeOctetString_fragmentation_innerWhile(arr, asn1SizeMax, newNCurOffset1)

   }.ensuring(res =>
      (res._2 >= 0 && res._2 <= asn1SizeMax || res == (-1L, -1L)) &&
      BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length) &&
      old(this).bitStream.buf.length == bitStream.buf.length &&
      arr.length == asn1SizeMax.toInt &&
      old(arr).length == arr.length
      )

   /**
     * Read bytes from bitstream, and write them in the array, starting at from and to to index.
     * If the bitstream is invalid, abort.
     *
     * @param arr
     * @param asn1SizeMax
     * @param from
     * @param to
     * @return
     */
   def decodeOctetString_fragmentation_innerMostWhile(arr: Array[UByte], asn1SizeMax: Long, from: Int, to: Int): Unit = {
      require(asn1SizeMax >= 0 && asn1SizeMax < Int.MaxValue)
      require(from >= 0)
      require(to >= 0)
      require(to >= from)
      require(arr.length == asn1SizeMax.toInt)
      staticRequire(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
      decreases(to - from)

      // Implements the following loop
       //    var i1: Int = nCurOffset1.toInt
      //    (while (nCurOffset1 + nCurBlockSize1 <= asn1SizeMax) && (i1 < (nCurOffset1 + nCurBlockSize1).toInt) do
      //       decreases((nCurOffset1 + nCurBlockSize1).toInt - i1)
      //       arr(i1) = readByte()
      //       i1 += 1
      //    ).invariant(true) // TODO invariant

      if(from >= to) then
         return
      // SAM guard ifs
      if(from >= asn1SizeMax) then
         return
      if(!BitStream.validate_offset_byte(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)) then
         return
      val nextByte = readByte()
      arr(from) = nextByte
      decodeOctetString_fragmentation_innerMostWhile(arr, asn1SizeMax, from + 1, to)
      ()

   }.ensuring(_ =>
      old(this).buf.length == bitStream.buf.length &&
      arr.length == asn1SizeMax.toInt &&
      BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length) &&
      old(this).bitStream.buf.length == bitStream.buf.length &&
      old(arr).length == arr.length
      )

   def encodeOctetString(arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long) = {
      require(asn1SizeMin >= 0)
      require(asn1SizeMax >= 0 && asn1SizeMax < Int.MaxValue) // to match the decodeOctetString_fragmentation spec
      require(asn1SizeMin <= asn1SizeMax)
      require(arr.length >= nCount)
      require(nCount >= asn1SizeMin && nCount <= asn1SizeMax)
      require(nCount < Int.MaxValue - GetBitCountUnsigned(stainless.math.wrapping(asn1SizeMax - asn1SizeMin).toRawULong))
      require( nCount < Int.MaxValue / 8 - 2 - (nCount / 0x4000) )
      require(
         if(asn1SizeMax < 65536) then
            BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCount + GetBitCountUnsigned(stainless.math.wrapping(asn1SizeMax - asn1SizeMin).toRawULong))
         else
            BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCount + 8 * (nCount / 0x4000) + 2)
         )

      if asn1SizeMax < 65536 then
         if asn1SizeMin != asn1SizeMax then
            encodeConstrainedWholeNumber(nCount.toLong, asn1SizeMin, asn1SizeMax)
            assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCount))
         encodeOctetString_no_length(arr, nCount)

      else
         encodeOctetString_fragmentation(arr, nCount)
   }

   def decodeOctetString(asn1SizeMin: Long, asn1SizeMax: Long): Array[UByte] = {
      require(asn1SizeMin >= 0)
      require(asn1SizeMax >= 0 && asn1SizeMax < Int.MaxValue)
      require(asn1SizeMin <= asn1SizeMax)

      if asn1SizeMax >= 0x1_00_00 then // 65'536, bigger than 2 unsigned bytes
         return decodeOctetString_fragmentation(asn1SizeMax)

      var nCount: Int = 0
      if asn1SizeMin != asn1SizeMax then
         // SAM guard if
         assert(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
         if(!BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(stainless.math.wrapping(asn1SizeMax - asn1SizeMin).toRawULong))) then
            return Array.empty
         nCount = decodeConstrainedWholeNumber(asn1SizeMin, asn1SizeMax).toInt
      else
         nCount = asn1SizeMin.toInt

      assert(nCount >= asn1SizeMin && nCount <= asn1SizeMax) // TODO check with C implementation and standard

      assert(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
      if(!BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCount)) then
         return Array.empty
      decodeOctetString_no_length(nCount)
   }

   def encodeBitString(arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long): Unit = {
      require(asn1SizeMin >= 0)
      require(asn1SizeMax >= 0)
      require(asn1SizeMin <= asn1SizeMax)
      require(arr.length >= nCount)
      require(nCount <= asn1SizeMax)
      require(nCount >= 0)
      require(asn1SizeMax < Int.MaxValue)
      require(asn1SizeMax < Int.MaxValue - 8*(asn1SizeMax / 0x4000) - 16)
      require(nCount >= asn1SizeMin) // TODO SAM check
      require(arr.length >= 0)
      require(
         if(asn1SizeMax < 65536 && asn1SizeMin != asn1SizeMax) then BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(stainless.math.wrapping(asn1SizeMax - asn1SizeMin).toRawULong) + nCount)
         else if(asn1SizeMax < 65536) then BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCount )
         else BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCount + 8 * (nCount / 0x4000) + 16 )
         )
      if asn1SizeMax < 65536 then
         if asn1SizeMin != asn1SizeMax then
            encodeConstrainedWholeNumber(nCount.toLong, asn1SizeMin, asn1SizeMax)

         appendBitsMSBFirst(arr, nCount)

      else
         var nRemainingItemsVar1: Long = nCount.toLong
         var nCurBlockSize1: Long = 0
         var nCurOffset1: Long = 0

         val res = encodeBitString_while(arr, nCount, asn1SizeMin, asn1SizeMax, nRemainingItemsVar1, nCurOffset1, BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit))
         nRemainingItemsVar1 = res._1
         nCurOffset1 = res._2
         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + 16))
         if nRemainingItemsVar1 <= 0x7F then
            ghostExpr(lemmaGetBitCountUnsignedFFEqualsEight())
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8))
            encodeConstrainedWholeNumber(nRemainingItemsVar1, 0, 0xFF)
         else
            ghostExpr(lemmaGetBitCountUnsigned7FFFEquals15())
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 16))
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 1))
            appendBit(true)
            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 15))
            encodeConstrainedWholeNumber(nRemainingItemsVar1, 0, 0x7FFF)

         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1))

         // val t: Array[UByte] = Array.fill(nRemainingItemsVar1.toInt)(0.toRawUByte) // STAINLESS: arr.slice((nCurOffset1 / 8).toInt, (nCurOffset1 / 8).toInt + nRemainingItemsVar1.toInt)
         appendBitsMSBFirst(arr, nRemainingItemsVar1.toInt, nCurOffset1.toInt)
   }

   def encodeBitString_while(arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long, nRemainingItemsVar1: Long, nCurOffset1: Long, bitIndex: Long): (Long, Long) = {
      require(asn1SizeMin >= 0)
      require(asn1SizeMax >= 0)
      require(asn1SizeMin <= asn1SizeMax)
      require(nCount <= asn1SizeMax)
      require(nCount >= 0)
      require(nCount >= asn1SizeMin) // TODO SAM check
      require(nCount <= arr.length)
      require(asn1SizeMax < Int.MaxValue)
      require(nRemainingItemsVar1 >= 0)
      require(nCurOffset1 >= 0)
      require(nRemainingItemsVar1 == nCount - nCurOffset1)
      require(bitIndex == BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit))
      require(nCount < Int.MaxValue - 8*(nCount / 0x4000) - 16)
      staticRequire(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + 8*(nRemainingItemsVar1 / 0x4000) + 16))


      decreases(nCount - nCurOffset1)

      assert(nCount == nRemainingItemsVar1 + nCurOffset1)
      if(nRemainingItemsVar1 < 0x4000) then
         (nRemainingItemsVar1, nCurOffset1)
      else
         val nCurBlockSize1: Int = if nRemainingItemsVar1 >= 0x10000 then
            0x10000
         else if nRemainingItemsVar1 >= 0xC000 then
            0xC000
         else if nRemainingItemsVar1 >= 0x8000 then
            0x8000
         else
            0x4000

         @ghost val bitIndexBeforeHeader = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)
         @ghost val currentByteBeforeHeader = bitStream.currentByte
         @ghost val currentBitBeforeHeader = bitStream.currentBit
         @ghost val bufferLength = bitStream.buf.length


         ghostExpr(lemmaGetBitCountUnsignedFFEqualsEight())
         assert(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8))
         if nCurBlockSize1 == 0x10000 then
            encodeConstrainedWholeNumber(0xC4, 0, 0xFF)
         else if nCurBlockSize1 == 0xC000 then
            encodeConstrainedWholeNumber(0xC3, 0, 0xFF)
         else if nCurBlockSize1 == 0x8000 then
            encodeConstrainedWholeNumber(0xC2, 0, 0xFF)
         else
            encodeConstrainedWholeNumber(0xC1, 0, 0xFF)

         assert(bitStream.buf.length == bufferLength)

         @ghost val bitIndexAfterHeader = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)
         @ghost val currentByteAfterHeader = bitStream.currentByte
         @ghost val currentBitAfterHeader = bitStream.currentBit

         assert(bitIndexAfterHeader == bitIndexBeforeHeader + 8)

         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + 8*(nRemainingItemsVar1 / 0x4000) + 16 - 8))

         // val t: Array[UByte] = Array.fill(nCurBlockSize1.toInt)(0.toRawUByte) // STAINLESS: arr.slice((nCurOffset1 / 8).toInt, (nCurOffset1 / 8).toInt + nCurBlockSize1.toInt)
         appendBitsMSBFirst(arr, nCurBlockSize1.toInt, nCurOffset1.toInt)
         val newOffset = nCurOffset1 + nCurBlockSize1
         val newRemaingItems = nRemainingItemsVar1 -  nCurBlockSize1

         @ghost val currentBitAfterAppending = bitStream.currentBit
         @ghost val currentByteAfterAppending = bitStream.currentByte
         val newBitIndex = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit)

         assert(newBitIndex == bitIndexAfterHeader + nCurBlockSize1)
         assert(newRemaingItems == nRemainingItemsVar1 - nCurBlockSize1)
         assert(newOffset == nCurOffset1 + nCurBlockSize1)

         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1 + 8*(nRemainingItemsVar1 / 0x4000) + 16 - nCurBlockSize1 - 8)) // REALLY SLOW : > 160sec
         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, newRemaingItems + 8*(newRemaingItems / 0x4000) + 16))

         encodeBitString_while(arr, nCount, asn1SizeMin, asn1SizeMax, newRemaingItems, newOffset, newBitIndex)
   }.ensuring (res =>
      BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length) &&
      // BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) <= bitIndex + (nRemainingItemsVar1 / 0x4000) * 8 + nRemainingItemsVar1 - res._1 &&
      BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, res._1 + 16) &&
      res._1 + res._2 == nCount &&
      res._1 >= 0 &&
      res._2 >= 0 &&
      res._1 <= 0x4000 &&
      old(this).bitStream.buf.length == bitStream.buf.length
      )

   def decodeBitString(asn1SizeMin: Long, asn1SizeMax: Long): Array[UByte] = {
      require(asn1SizeMax >= 0)
      require(asn1SizeMax <= Int.MaxValue)
      require(asn1SizeMin >= 0)
      require(asn1SizeMin <= asn1SizeMax)

      if (asn1SizeMax < 65536) {
         var nCount: Long = 0
         if asn1SizeMin != asn1SizeMax then
            if !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(stainless.math.wrapping(asn1SizeMax - asn1SizeMin).toRawULong)) then
               return Array.empty

            assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(stainless.math.wrapping(asn1SizeMax - asn1SizeMin).toRawULong)))
            nCount = decodeConstrainedWholeNumber(asn1SizeMin, asn1SizeMax)
         else
            nCount = asn1SizeMin

          if !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCount.toInt) then
               return Array.empty

         assert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCount.toInt))
         return readBits(nCount.toInt)

      }

      val arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0.toRawUByte)

      val whileRes = decodeBitString_while(asn1SizeMin, asn1SizeMax, 0, arr)
      var nRemainingItemsVar1 = whileRes._1
      var nCurOffset1 = whileRes._2
      var nLengthTmp1: Long = nCurOffset1

      if nRemainingItemsVar1 == -1L || nCurOffset1 == -1L then
         return Array.empty

      assert(nRemainingItemsVar1 >= 0 && nRemainingItemsVar1 <= 0xFF)
      if (nRemainingItemsVar1 & 0x80) > 0 then
         ghostExpr(lemmaGetBitCountUnsignedFFEqualsEight())
         assert(GetBitCountUnsigned(stainless.math.wrapping(0xFF).toRawULong) == 8)
         if !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8) then
            return Array.empty
         nRemainingItemsVar1 <<= 8
         nRemainingItemsVar1 |= decodeConstrainedWholeNumber(0, 0xFF)
         nRemainingItemsVar1 &= 0x7FFF

      assert(nRemainingItemsVar1 >= 0 && nRemainingItemsVar1 <= 0x7FFF)

      if nCurOffset1 + nRemainingItemsVar1 > asn1SizeMax then
         return Array.empty

      assert(nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax)

      assert(BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length))
      if !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nRemainingItemsVar1.toInt) then
         return Array.empty


      val readBitsArray = readBits(nRemainingItemsVar1.toInt)

      assert(nRemainingItemsVar1.toInt >= 0)
      assert(nRemainingItemsVar1.toInt <= asn1SizeMax.toInt)
      assert(readBitsArray.length == ((nRemainingItemsVar1 + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE).toInt)
      arrayCopyOffsetLen(readBitsArray, arr, 0, (nCurOffset1 / 8).toInt, readBitsArray.length)

      nLengthTmp1 += nRemainingItemsVar1

      // Same as in decodeOctetString, we can only prove >= 0, not >= 1; at least not without assuming things about the buffer
      // assert((nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax))
      assert((nLengthTmp1 >= 0) && (nLengthTmp1 <= asn1SizeMax))
      // SAM same here, according to C implementation it should fail if nLengthTmp1 == 0
      if(nLengthTmp1 == 0) then
         return Array.empty

      arr
   }

   def decodeBitString_while(asn1SizeMin: Long, asn1SizeMax: Long, nCurOffset1: Long, arr: Array[UByte]): (Long, Long) = {
      require(asn1SizeMax >= 0)
      require(asn1SizeMax <= Int.MaxValue)
      require(asn1SizeMin >= 0)
      require(asn1SizeMin <= asn1SizeMax)
      require(arr.length == asn1SizeMax.toInt)
      require(nCurOffset1 >= 0)
      require(nCurOffset1 <= asn1SizeMax)

      decreases(asn1SizeMax - nCurOffset1)

      // Implements this loop

      // (while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
      //    decreases(asn1SizeMax - nCurOffset1) // TODO: check experimental decrease
      //    if nRemainingItemsVar1 == 0xC4 then
      //       nCurBlockSize1 = 0x10000
      //    else if nRemainingItemsVar1 == 0xC3 then
      //       nCurBlockSize1 = 0xC000
      //    else if nRemainingItemsVar1 == 0xC2 then
      //       nCurBlockSize1 = 0x8000
      //    else if nRemainingItemsVar1 == 0xC1 then
      //       nCurBlockSize1 = 0x4000
      //    else
      //       assert(false, "broken State") // TODO check with C implementation and standard

      //    assert(nCurOffset1 + nCurBlockSize1 > asn1SizeMax)

      //    arrayCopyOffsetLen(readBits(nCurBlockSize1.toInt), arr, 0, (nCurOffset1 / 8).toInt, nCurBlockSize1.toInt)
      //    nLengthTmp1 += nCurBlockSize1
      //    nCurOffset1 += nCurBlockSize1
      //    nRemainingItemsVar1 = decodeConstrainedWholeNumber(0, 0xFF)

      // )

      @ghost val arrLength = arr.length
      ghostExpr(lemmaGetBitCountUnsignedFFEqualsEight())
      assert(GetBitCountUnsigned(stainless.math.wrapping(0xFF).toRawULong) == 8)
      if !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, 8) then
         return (-1L, -1L)

      val nRemainingItemsVar1: Long = decodeConstrainedWholeNumber(0, 0xFF)

      if (nRemainingItemsVar1 & 0xC0) != 0xC0 then
         return (nRemainingItemsVar1, nCurOffset1)

      val nCurBlockSize1 = if nRemainingItemsVar1 == 0xC4 then
         0x10000
      else if nRemainingItemsVar1 == 0xC3 then
         0xC000
      else if nRemainingItemsVar1 == 0xC2 then
         0x8000
      else if nRemainingItemsVar1 == 0xC1 then
         0x4000
      else
         0x0 //ERROR
         // assert(false, "broken State") // TODO check with C implementation and standard
      if nCurBlockSize1 == 0 then
         return (-1L, -1L)

      // assert(nCurOffset1 + nCurBlockSize1 > asn1SizeMax) // SAM Why?

      if !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nCurBlockSize1.toInt) then
         return (-1L, -1L)

      val readBitsArrayLength =  ((nCurBlockSize1 + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE).toInt

      if(arr.length < readBitsArrayLength ) then
         // the output array is too small, probably the bitstream is malformed (i.e., to many block headers or too big block sizes)
         return (-1L, -1L)

      if((nCurOffset1 / 8).toInt > arr.length - readBitsArrayLength) then
         // the output array is also too small, probably the bitstream is malformed (i.e., to many block headers or too big block sizes)
         return (-1L, -1L)

      assert(0 <= readBitsArrayLength && readBitsArrayLength <= arr.length)
      assert(0 <= (nCurOffset1 / 8).toInt && (nCurOffset1 / 8).toInt <= arr.length - readBitsArrayLength)
      arrayCopyOffsetLen(readBits(nCurBlockSize1.toInt), arr, 0, (nCurOffset1 / 8).toInt, readBitsArrayLength)
      val newNCurOffset1 = nCurOffset1 + nCurBlockSize1

      assert(arr.length == arrLength)
      assert(arrLength == asn1SizeMax.toInt)

      if(newNCurOffset1 > asn1SizeMax) then
         // the bitstream is malformed, as they are too many blocks
         return (-1L, -1L)
      decodeBitString_while(asn1SizeMin, asn1SizeMax, newNCurOffset1, arr)

   }.ensuring (res =>
      res == (-1L, -1L)
      ||
      res._1 >= 0 && res._1 <= 0xFF &&
      res._2 >= 0 && res._2 <= asn1SizeMax &&
      BitStream.invariant(bitStream.currentBit, bitStream.currentByte, bitStream.buf.length) &&
      old(this).bitStream.buf.length == bitStream.buf.length &&
      arr.length == asn1SizeMax.toInt &&
      old(arr).length == arr.length
      )

   @inline @pure @ghost
   def withAlignedToByte(): Codec = {
      require(BitStream.validate_offset_bits(bitStream.buf.length.toLong, bitStream.currentByte.toLong, bitStream.currentBit.toLong,
         (NO_OF_BITS_IN_BYTE - bitStream.currentBit) & (NO_OF_BITS_IN_BYTE - 1)
      ))
      Codec(bitStream.withAlignedToByte())
   }

   @inline @pure @ghost
   def withAlignedToShort(): Codec = {
      require(BitStream.validate_offset_bits(bitStream.buf.length.toLong, bitStream.currentByte.toLong, bitStream.currentBit.toLong,
         (NO_OF_BITS_IN_SHORT -                                                                                     // max alignment (16) -
            (NO_OF_BITS_IN_BYTE * (bitStream.currentByte & (NO_OF_BYTES_IN_JVM_SHORT - 1)) + bitStream.currentBit)  // current pos
            ) & (NO_OF_BITS_IN_SHORT - 1))                                                                          // edge case (0,0) -> 0
      )
      Codec(bitStream.withAlignedToShort())
   }

   @inline @pure @ghost
   def withAlignedToInt(): Codec = {
      require(BitStream.validate_offset_bits(bitStream.buf.length.toLong, bitStream.currentByte.toLong, bitStream.currentBit.toLong,
         (NO_OF_BITS_IN_INT -                                                                                    // max alignment (32) -
            (NO_OF_BITS_IN_BYTE * (bitStream.currentByte & (NO_OF_BYTES_IN_JVM_INT - 1)) + bitStream.currentBit) // current pos
            ) & (NO_OF_BITS_IN_INT - 1))                                                                         // edge case (0,0) -> 0
      )
      Codec(bitStream.withAlignedToInt())
   }
}
