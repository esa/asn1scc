package asn1scala

import stainless.*
import stainless.lang.{None, Option, ghost as ghostExpr, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.*
import StaticChecks.{require as staticRequire, _}

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
   def decodeUnconstrainedWholeNumber_prefixLemma_helper(c1: Codec, c2: Codec): (Codec, Codec, Long, Codec, Long) = {
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
         readNLeastSignificantBitsPrefixLemma(c1_2, c2_2, nBytes1.unsignedToInt * 8)
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
         readNLeastSignificantBitsPrefixLemma(c1.bitStream, c2.bitStream, nBits)
      }.ensuring { _ =>
         l1 == l2 && BitStream.bitIndex(c1Res.bitStream.buf.length, c1Res.bitStream.currentByte, c1Res.bitStream.currentBit) == BitStream.bitIndex(c2Res.bitStream.buf.length, c2Res.bitStream.currentByte, c2Res.bitStream.currentBit)
      }
   }

}

/**
 * Base class for the PER Codec that is used by ACN and UPER
 *
 * @param count represents the number of bytes in the internal buffer
 *
 */
case class Codec private [asn1scala](bitStream: BitStream) {
   import Codec.*
   import BitStream.{reader => _, *}
   export bitStream.{resetAt => _, withMovedByteIndex => _, withMovedBitIndex => _, isPrefixOf => _, *}

   @ghost @pure @inline
   def resetAt(other: Codec): Codec = {
      require(bitStream.buf.length == other.bitStream.buf.length)
      Codec(bitStream.resetAt(other.bitStream))
   }

   @ghost @pure @inline
   def withMovedByteIndex(diffInBytes: Int): Codec = {
      require(moveByteIndexPrecond(bitStream, diffInBytes))
      Codec(bitStream.withMovedByteIndex(diffInBytes))
   }

   @ghost @pure @inline
   def withMovedBitIndex(diffInBits: Int): Codec = {
      require(moveBitIndexPrecond(bitStream, diffInBits))
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
      appendNLeastSignificantBits(v.toRaw, GetBitCountUnsigned(v))
   } .ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      val nBits = GetBitCountUnsigned(v)
      w1.bitStream.buf.length == w2.bitStream.buf.length && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
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

      readNLeastSignificantBits(nBits).toRawULong
   }

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
         // assert(nRangeBits >= nEncValBits) // TODO: T.O

         appendNLeastSignificantBits(encVal, nRangeBits)
      else
         ghostExpr {
            validReflexiveLemma(bitStream)
         }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      val range = max - min
      val nBits = GetBitCountUnsigned(range)
      w1.bitStream.buf.length == w2.bitStream.buf.length && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
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

         min + decVal
   }

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
      require(min <= max)
      require(min <= v && v <= max)
      require(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong))) // SAM There was a bug here, we checked for N bytes even though the number returned by teh function is bits
      val range: Long = stainless.math.wrapping(max - min)
      // get number of bits that get written
      val nRangeBits: Int = GetBitCountUnsigned(range.toRawULong)

      if range != 0 then
         // get value that gets written
         val encVal = stainless.math.wrapping(v - min).toRawULong

         @ghost val nEncValBits = GetBitCountUnsigned(encVal)
         //SAMassert(nRangeBits >= nEncValBits)
         //SAMassert(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRangeBits))

         appendNLeastSignificantBits(encVal, nRangeBits)
      else
         ghostExpr {
            validReflexiveLemma(bitStream)
         }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      val range = stainless.math.wrapping(max - min)
      val nBits = GetBitCountUnsigned(range.toRawULong)
      w1.bitStream.buf.length == w2.bitStream.buf.length
      && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits && 
      w1.isPrefixOf(w2) 
      && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
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
         val decVal = readNLeastSignificantBits(nRangeBits)

         // assert(min + decVal <= max) // TODO: Invalid

         stainless.math.wrapping(min + decVal) // TODO: Overflows but seems fine?
   }

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
    * @param v    number that gets encoded to the bitstream. Must be bigger than min.
    * @param min  lower boundary of range
    *
    */
   def encodeSemiConstrainedWholeNumber(v: Long, min: Long): Unit = {
      require(min <= v)
      staticRequire(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,
         GetLengthForEncodingUnsigned(stainless.math.wrapping(v - min).toRawULong) + 1)
      )

      val encV: ULong = stainless.math.wrapping(v - min).toRawULong
      val nBytes = GetLengthForEncodingUnsigned(encV).toByte

      // need space for length and value
      assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBytes + 1))

      // encode length
      appendByte(nBytes.toRawUByte)
      // encode value
      appendNLeastSignificantBits(encV.toRaw, nBytes * NO_OF_BITS_IN_BYTE)
   }

   /**
    * Decode number from bitstream that is in range [min, Long.MaxValue].
    * This is the reversal function of encodeSemiConstrainedWholeNumber.
    *
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
         readNLeastSignificantBits(nBits).toRawULong
      }

      // SAM: here the post condition should be obvious, as ULong are always positive. But we can have
      // overflow, and ULong does not encode the unsigned nature in any way, so cannot work.

      v + min
   }// SAM .ensuring(x => x >= min)

   /**
    * Encode number to the bitstream within the range [min, Long.Max_Value].
    * This function writes full bytes to the bitstream. The length is written as
    * an signed byte in front of the bytes written for the number v.
    *
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
      appendNLeastSignificantBits(encV, nBytes * NO_OF_BITS_IN_BYTE)
   }

   /**
    * Decode unsigned number from the bitstream
    * within the range [min, Long.Max_Value].
    *
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
      val v = if(!(nBits >= 0 && nBits <= 64) || !BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nBits)){
         0L.toRawULong
      } else {
         readNLeastSignificantBits(nBits).toRawULong
      }
      val res: ULong = ULong.fromRaw(v + min) // For some reasons, the scala compiler chokes on this being returned
      res
   }//.ensuring(res => min <= res)

   /**
    * 8.3 Encoding of an integer value
    *
    * The encoding of an integer value shall be primitive.
    * The contents octets shall consist of one or more octets.
    *
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
      appendNLeastSignificantBits(v & onesLSBLong(nBits), nBits)
      ghostExpr {
         validTransitiveLemma(this1.bitStream, this2.bitStream, this.bitStream)
         val this2Reset = this2.bitStream.resetAt(this1.bitStream)
         readBytePrefixLemma(this2Reset, this.bitStream)
         assert(this2.bitStream.resetAt(this1.bitStream).readBytePure()._2.unsignedToInt == nBytes)
         val (r1, r2) = reader(this1, this)
         validateOffsetBitsContentIrrelevancyLemma(this1.bitStream, this.bitStream.buf, nBits)
         val (r2Got, vGot) = r1.decodeUnconstrainedWholeNumberPure()
         check(r2Got == r2)
         //SAM assert((vGot & onesLSBLong(nBits)) == (v & onesLSBLong(nBits)))
         check(vGot == v)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      val nBits = NO_OF_BITS_IN_BYTE + GetLengthForEncodingSigned(v) * NO_OF_BITS_IN_BYTE
      w1.bitStream.buf.length == w2.bitStream.buf.length && BitStream.bitIndex(w2.bitStream.buf.length, w2.bitStream.currentByte, w2.bitStream.currentBit) == BitStream.bitIndex(w1.bitStream.buf.length, w1.bitStream.currentByte, w1.bitStream.currentBit) + nBits && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1.bitStream, w2.bitStream.buf, nBits)
         val (r2Got, vGot) = r1.decodeUnconstrainedWholeNumberPure()
         vGot == v && r2Got == r2
      }
   }

   /**
    * 8.3 Encoding of an integer value reverse operation
    *
    * To call this func at least 2 octets have to be available on the bitstream
    * The length n is the first octet, n octets with the value follow
    * Values with n > 8 are not supported
    *
    * @return decoded number
    */
   def decodeUnconstrainedWholeNumber(): Long = {
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))
      staticRequire {
         val nBytes = bitStream.readBytePure()._2.unsignedToInt
         BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1 + nBytes) && 0 <= nBytes && nBytes <= 8
      }

      // get length
      val nBytes = readByte().unsignedToInt
      val nBits = nBytes * NO_OF_BITS_IN_BYTE
      // check bitstream precondition
      //SAM assert(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nBytes))
      //SAM assert(0 <= nBytes && nBytes <= 8)
      // read value
      val read = readNLeastSignificantBits(nBits)
      // TODO: This was added (sign extension)
      if (read == 0 || (read & (1L << (nBits - 1))) == 0L) read
      else onesMSBLong(64 - nBits) | read
   }//.ensuring(_ => bitStream.buf == old(this).bitStream.buf && bitIndex() == old(this).bitIndex() + NO_OF_BITS_IN_BYTE + old(this).bitStream.readBytePure()._2.unsignedToInt * NO_OF_BITS_IN_BYTE)

   @ghost @pure
   def decodeUnconstrainedWholeNumberPure(): (Codec, Long) = {
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
    * @param vVal real input in IEEE754 double format
    */
   @extern
   def encodeReal(vVal: Double): Unit = {
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,2*GetBitCountUnsigned(stainless.math.wrapping(0xFF).toRawULong)))
      encodeRealBitString(java.lang.Double.doubleToRawLongBits(vVal))
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
    */
   private def encodeRealBitString(vVal: Long): Unit = {
      // Require from CalculateManitssaAndExponent
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
            BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8 )
         else if (vVal == DoubleNegZeroBitString || (vVal & ExpoBitMask) == ExpoBitMask) then 
            BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16)
         else
            BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16 + nManLen  + nExpLen )
         )
      })
      // according to T-REC-X.690 2021

      var v = vVal

      // 8.5.2 Plus Zero
      if v == DoublePosZeroBitString then
         check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
         encodeConstrainedWholeNumber(0, 0, 0xFF)
         

      // 8.5.3 Minus Zero
      else if v == DoubleNegZeroBitString then
         check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16))
         encodeConstrainedWholeNumber(1, 0, 0xFF)
         check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
         encodeConstrainedWholeNumber(0x43, 0, 0xFF)
         

      // 8.5.9 SpecialRealValues
      else if (v & ExpoBitMask) == ExpoBitMask then

      // 8.5.9 PLUS-INFINITY
         if v == DoublePosInfBitString then
            check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16))
            encodeConstrainedWholeNumber(1, 0, 0xFF)
            check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
            encodeConstrainedWholeNumber(0x40, 0, 0xFF)

         // 8.5.9 MINUS-INFINITY
         else if v == DoubleNegInfBitString then
            check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16))
            encodeConstrainedWholeNumber(1, 0, 0xFF)
            check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
            encodeConstrainedWholeNumber(0x41, 0, 0xFF)

         // 8.5.9 NOT-A-NUMBER
         else
            check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16))
            encodeConstrainedWholeNumber(1, 0, 0xFF)
            check( BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))
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
         BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,16 + nManLen  + nExpLen )

         encodeConstrainedWholeNumber(1 + nExpLen + nManLen, 0, 0xFF)
         
         check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8 + nManLen * NO_OF_BITS_IN_BYTE + nExpLen * NO_OF_BITS_IN_BYTE))
         // /* encode header */
         BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8 + nManLen  + nExpLen )

         encodeConstrainedWholeNumber(header & 0xFF, 0, 0xFF)
         
          BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen  + nExpLen )

         // TODO this might be more complicated, because by removing the require in the bistream class, I'll in fact break the safety there
         /* encode exponent */
         if exponent.toRaw >= 0 then
            // fill with zeros to have a whole byte   
            // check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE + nExpLen * NO_OF_BITS_IN_BYTE))
            appendNZeroBits(nExpLen * NO_OF_BITS_IN_BYTE - GetBitCountUnsigned(exponent.toULong))
            // check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE + GetBitCountUnsigned(exponent.toULong)))
            // check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,GetBitCountUnsigned(exponent.toULong)))
            encodeUnsignedInteger(exponent.toULong)
            // check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE ))
         else
            check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE + nExpLen * NO_OF_BITS_IN_BYTE))
            check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE  + GetBitCountUnsigned(compactExp.toLong.toRawULong)))
            // check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(compactExp.toLong.toRawULong)))
            encodeUnsignedInteger(compactExp.toLong.toRawULong)
            // check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE ))
            
         /* encode mantissa */
         // check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nManLen * NO_OF_BITS_IN_BYTE ))
         appendNZeroBits(nManLen * NO_OF_BITS_IN_BYTE - GetBitCountUnsigned(mantissa))
         // check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,GetBitCountUnsigned(mantissa)))
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

      // TODO Check doc to see if exponent as a max value, otherwise there is a bug here
      // assert(exponent < (Int.MaxValue / 4) && exponent >= -1)
      // decode mantissa parts
      val length = lengthVal - expLen
      var N: Long = 0
      var j: Int = 0
      (while j < length do
         decreases(length - j)

         N = (N << 8) | (readByte().toRaw.toInt & 0xFF)

         j += 1
      ).invariant(j >= 0 && j <= length && BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,length - j))

      // assert(N < (Long.MaxValue / 8) && N >= 0) // TODO Check the doc to see if N has a max value, otherwise there is a bug here
      
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
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCount))
      appendByteArray(arr, nCount)
   }

   def decodeOctetString_no_length(nCount: Int): Array[UByte] = {
      require(nCount >= 0 && nCount <= Integer.MAX_VALUE / NO_OF_BITS_IN_BYTE)
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCount))
      readByteArray(nCount)
   }

   def encodeOctetString_fragmentation_old(arr: Array[UByte], nCount: Int) = {
      require(nCount >= 0 && nCount <= arr.length)
      require(nCount + 8 * (nCount / 0x4000) + 16 > 0) // To avoid overflow of the available length checks
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCount + 8 * (nCount / 0x4000) + 16)) // Room for information bytes + data

      var nRemainingItemsVar1: Int = nCount
      var nCurBlockSize1: Int = 0
      var nCurOffset1: Int = 0 // Which represents the currently written bytes number SAM

      @ghost var nBlockHeadersWritten = 0

      ghostExpr(check(nCurOffset1 >= 0 ))
      ghostExpr(check(nCount >= 0 ))
      ghostExpr(check(nRemainingItemsVar1 >= 0))
      ghostExpr(check(  nRemainingItemsVar1 <= nCount))
      ghostExpr(check( nRemainingItemsVar1 + nCurOffset1 == nCount ))
      ghostExpr(check(nBlockHeadersWritten <= nCount / 0x4000 ))
      ghostExpr(check(nCurOffset1 + nRemainingItemsVar1 == nCount ))
      ghostExpr(check(nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 ))
      ghostExpr(check(nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16 >= 0 ))
      ghostExpr(check(8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 ))
      ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16)))

      (while nRemainingItemsVar1 >= 0x4000 do
         decreases(nRemainingItemsVar1)
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8))) // == GetLengthForEncodingUnsigned(0xFF)

         @ghost val nBitsOld = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )
         
         if nRemainingItemsVar1 >= 0x10000 then
            nCurBlockSize1 = 0x10000
            encodeConstrainedWholeNumber(0xC4, 0, 0xFF)
         else if nRemainingItemsVar1 >= 0xC000 then
            nCurBlockSize1 = 0xC000
            encodeConstrainedWholeNumber(0xC3, 0, 0xFF)
         else if nRemainingItemsVar1 >= 0x8000 then
            nCurBlockSize1 = 0x8000
            encodeConstrainedWholeNumber(0xC2, 0, 0xFF)
         else
            nCurBlockSize1 = 0x4000
            encodeConstrainedWholeNumber(0xC1, 0, 0xFF)

         ghostExpr(check(BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) == nBitsOld + 8))
         ghostExpr(nBlockHeadersWritten += 1)

         var i1: Int = nCurOffset1
         @ghost var nWrittenBytes = 0
         
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCurBlockSize1)))

         ghostExpr(check(i1 >= 0 ))
         ghostExpr(check(nCurBlockSize1 <= nRemainingItemsVar1 ))
         ghostExpr(check(nCurBlockSize1 >= 0))
         ghostExpr(check(nRemainingItemsVar1 >= 0))
         ghostExpr(check(nCurOffset1 >= 0))
         ghostExpr(check(nCurBlockSize1 + nCurOffset1 >= 0 ))
         ghostExpr(check(i1 <= nCurBlockSize1 + nCurOffset1 ))
         ghostExpr(check(nCurBlockSize1 + nCurOffset1 - i1 >= 0 ))
         ghostExpr(check(nCurBlockSize1 + nCurOffset1 <= nCount ))
         ghostExpr(check(nWrittenBytes == i1 - nCurOffset1 ))
         ghostExpr(check(nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 ))
         ghostExpr(check(nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16 >= 0 ))
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16)))

         (while i1 < nCurBlockSize1 + nCurOffset1 do
            decreases(nCurBlockSize1 + nCurOffset1 - i1)
            check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1)) 
            check(i1 < arr.length)
            check(i1 >= 0)

            appendByte(arr(i1))
            ghostExpr(nWrittenBytes += 1)
            i1 += 1

            ghostExpr(check(i1 >= 0 ))
            ghostExpr(check(nCurBlockSize1 <= nRemainingItemsVar1))
            ghostExpr(check(nCurBlockSize1 <= nRemainingItemsVar1 ))
            ghostExpr(check(nCurBlockSize1 >= 0))
            ghostExpr(check(nRemainingItemsVar1 >= 0))
            ghostExpr(check(nCurOffset1 >= 0))
            ghostExpr(check(nCurBlockSize1 + nCurOffset1 >= 0 ))
            ghostExpr(check(i1 <= nCurBlockSize1 + nCurOffset1 ))
            ghostExpr(check(nCurBlockSize1 + nCurOffset1 - i1 >= 0 ))
            ghostExpr(check(nCurBlockSize1 + nCurOffset1 <= nCount ))
            ghostExpr(check(nWrittenBytes == i1 - nCurOffset1 ))
            ghostExpr(check(nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 ))
            ghostExpr(check(nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16 >= 0 ))
            ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16)))

         ).invariant(
            i1 >= 0 &&
            nCurBlockSize1 <= nRemainingItemsVar1 &&
            nCurBlockSize1 >= 0 &&
            nRemainingItemsVar1 >= 0 &&
            nCurOffset1 >= 0 &&
            nCurBlockSize1 + nCurOffset1 >= 0 &&
            i1 <= nCurBlockSize1 + nCurOffset1 &&
            nCurBlockSize1 + nCurOffset1 - i1 >= 0 &&
            nCurBlockSize1 + nCurOffset1 <= nCount &&
            nWrittenBytes == i1 - nCurOffset1 &&
            nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 && // Because of potential overflows
            nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16 >= 0 && // Because of potential overflows
            BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16)
         )
         ghostExpr(check(nWrittenBytes == nCurBlockSize1))
         ghostExpr(check(nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16 == nCount - (nCurOffset1 + nCurBlockSize1) + 8*(nCount / 0x4000) - 8*nBlockHeadersWritten + 16))
         ghostExpr(check(nCount - nCurOffset1 - nWrittenBytes + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16 >= 0))
         ghostExpr(check( nCount - (nCurOffset1 + nCurBlockSize1) + 8*(nCount / 0x4000) - 8*nBlockHeadersWritten + 16 >= 0))
         check(nCurBlockSize1 <= nRemainingItemsVar1)
         ghostExpr(check(nCurBlockSize1 >= 0))
         ghostExpr(check(nRemainingItemsVar1 >= 0))
         ghostExpr(check(nCurOffset1 >= 0))

         nCurOffset1 += nCurBlockSize1
         nRemainingItemsVar1 -= nCurBlockSize1

         check(nRemainingItemsVar1 < nRemainingItemsVar1 + nCurBlockSize1) // measure decreases TODO

         ghostExpr(check(nCurOffset1 >= 0 ))
         ghostExpr(check(nCount >= 0 ))
         ghostExpr(check(nRemainingItemsVar1 >= 0 ))
         ghostExpr(check(nRemainingItemsVar1 <= nCount)) // TODO
         ghostExpr(check( nRemainingItemsVar1 + nCurOffset1 == nCount )) // TODO
         ghostExpr(check(nBlockHeadersWritten <= nCount / 0x4000 ))
         ghostExpr(check(nCurOffset1 + nRemainingItemsVar1 == nCount ))
         ghostExpr(check(nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 ))
         ghostExpr(check(nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16 >= 0 ))
         ghostExpr(check(8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 ))
         ghostExpr(check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16)))

      ).invariant(
         nCurOffset1 >= 0 &&
         nCount >= 0 &&
         nRemainingItemsVar1 >= 0 &&  nRemainingItemsVar1 <= nCount && nRemainingItemsVar1 + nCurOffset1 == nCount &&
         nBlockHeadersWritten <= nCount / 0x4000 &&
         nCurOffset1 + nRemainingItemsVar1 == nCount &&
         nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 && // Because of potential overflows
         nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16 >= 0 && // Because of potential overflows
         8*(nCount / 0x4000 - nBlockHeadersWritten) >= 0 &&
         BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRemainingItemsVar1 + 8*(nCount / 0x4000 - nBlockHeadersWritten) + 16)
      )

      // if nRemainingItemsVar1 <= 0x7F then
      //    check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8)) // == GetLengthForEncodingUnsigned(0xFF)
      //    encodeConstrainedWholeNumber(nRemainingItemsVar1.toLong, 0, 0xFF)
      // else
      //    check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))
      //    appendBit(true)
      //    check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,15)) // == GetLengthForEncodingUnsigned(0x7FFF)
      //    encodeConstrainedWholeNumber(nRemainingItemsVar1.toLong, 0, 0x7FFF)


      // var i1: Int = nCurOffset1
      // (while i1 < (nCurOffset1 + nRemainingItemsVar1) do
      //    decreases(nCurOffset1 + nRemainingItemsVar1 - i1)
      //    appendByte(arr(i1))
      //    i1 += 1
      // ).invariant(true) // TODO
   }

   def encodeOctetString_fragmentation(arr: Array[UByte], nCount: Int) = {
      require(nCount >= 0 && nCount <= arr.length)
      require(nCount + 8 * (nCount / 0x4000) + 16 > 0) // To avoid overflow of the available length checks
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nCount + 8 * (nCount / 0x4000) + 16)) // Room for information bytes + data

      var nRemainingItemsVar1: Int = nCount
      var nCurBlockSize1: Int = 0
      var nCurOffset1: Int = 0 // Which represents the currently written bytes number SAM

      encodeOctetString_fragmentation_innerWhile(arr, nCount, 0, nRemainingItemsVar1, nCurOffset1, BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ))

      // if nRemainingItemsVar1 <= 0x7F then
      //    check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,8)) // == GetLengthForEncodingUnsigned(0xFF)
      //    encodeConstrainedWholeNumber(nRemainingItemsVar1.toLong, 0, 0xFF)
      // else
      //    check(BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,1))
      //    appendBit(true)
      //    check(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,15)) // == GetLengthForEncodingUnsigned(0x7FFF)
      //    encodeConstrainedWholeNumber(nRemainingItemsVar1.toLong, 0, 0x7FFF)


      // var i1: Int = nCurOffset1
      // (while i1 < (nCurOffset1 + nRemainingItemsVar1) do
      //    decreases(nCurOffset1 + nRemainingItemsVar1 - i1)
      //    appendByte(arr(i1))
      //    i1 += 1
      // ).invariant(true) // TODO
   }

   def encodeOctetString_fragmentation_innerWhile(arr: Array[UByte], nCount: Int, currentlyWrittenBlocksOf0x4000: Int, nRemainingItemsVar1: Int, nCurOffset1: Int, bitIndex: Long ): Unit = {
      require(nRemainingItemsVar1 >= 0)
      require(currentlyWrittenBlocksOf0x4000 >= 0)
      require(nCurOffset1 >= 0)
      require(nCount + (nCount / 0x4000) >= 0)
      require(nRemainingItemsVar1 + (nRemainingItemsVar1 / 0x4000) >= 0)
      require(currentlyWrittenBlocksOf0x4000 < Int.MaxValue / 0x4000) // to avoid overflow
      require(nCount <= arr.length)
      require(nCount == nRemainingItemsVar1 + currentlyWrittenBlocksOf0x4000 * 0x4000)
      require(nCurOffset1 == currentlyWrittenBlocksOf0x4000 * 0x4000)
      require(BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) == bitIndex)
      require(BitStream.validate_offset_bytes(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit,nRemainingItemsVar1 + (nRemainingItemsVar1 / 0x4000)))

      decreases(nRemainingItemsVar1)

      if (nRemainingItemsVar1 >= 0x4000) then
         @ghost val bitIndexBeforeHeader = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )   
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
         val nNewBlocksOf0x4000 = nCurBlockSize1 match {
            case 0x10000 => 
               encodeConstrainedWholeNumber(0xC4, 0, 0xFF)
               4
            case 0xC000 => 
               encodeConstrainedWholeNumber(0xC3, 0, 0xFF)
               3
            case 0x8000 => 
               encodeConstrainedWholeNumber(0xC2, 0, 0xFF)
               2
            case 0x4000 => 
               encodeConstrainedWholeNumber(0xC1, 0, 0xFF)
               1
         }

         @ghost val bitIndexAfterHeader = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )

         assert(bitIndexBeforeHeader + NO_OF_BITS_IN_BYTE == bitIndexAfterHeader)
         assert( bitIndexAfterHeader <= bitIndexBeforeHeader + 4 * NO_OF_BITS_IN_BYTE)

         assert(nCount == nRemainingItemsVar1 + nCurOffset1)
         assert(nRemainingItemsVar1 >= nCurBlockSize1)
         assert(nCurOffset1 + nCurBlockSize1 <= nCount)
         assert(nCurOffset1 + nCurBlockSize1 <= arr.length)
         assert(nCurOffset1 + nCurBlockSize1 >= 0)
         encodeOctetString_fragmentation_innerMostWhile(arr, nCurOffset1, nCurOffset1 + nCurBlockSize1, BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ))

         @ghost val bitIndexAfterAppending = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )

         assert(bitIndexAfterAppending == bitIndexAfterHeader + nCurBlockSize1 * NO_OF_BITS_IN_BYTE)
         
         encodeOctetString_fragmentation_innerWhile(arr, nCount, currentlyWrittenBlocksOf0x4000 + nNewBlocksOf0x4000, nRemainingItemsVar1 - nCurBlockSize1, nCurOffset1 + nCurBlockSize1, BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ))

         @ghost val bitIndexAfterRecursive = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )


   } ensuring(_ => 
         BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) <= bitIndex + currentlyWrittenBlocksOf0x4000 * NO_OF_BITS_IN_BYTE + nRemainingItemsVar1 * NO_OF_BITS_IN_BYTE
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

      bitStream.appendByte(arr(from))

      @ghost val bitIndexBeforeRecursive = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )
      assert(bitIndexBeforeRecursive == bitIndexBeforeAppending + NO_OF_BITS_IN_BYTE)

      if from + 1 < to then
         encodeOctetString_fragmentation_innerMostWhile(arr, from + 1, to, bitIndex + NO_OF_BITS_IN_BYTE)
         @ghost val bitIndexAfterRecursive = BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit )
         assert(bitIndexAfterRecursive == bitIndexBeforeRecursive  + (to - from - 1) * NO_OF_BITS_IN_BYTE)
         assert(NO_OF_BITS_IN_BYTE + (to - from - 1) * NO_OF_BITS_IN_BYTE == (to - from) * NO_OF_BITS_IN_BYTE)
         assert(bitIndexAfterRecursive == bitIndexBeforeAppending + NO_OF_BITS_IN_BYTE + (to - from - 1) * NO_OF_BITS_IN_BYTE)
   } ensuring(_ => 
      val oldBistStream = old(this).bitStream
      BitStream.bitIndex(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit ) == bitIndex + (to - from) * NO_OF_BITS_IN_BYTE
   )

   def decodeOctetString_fragmentation(asn1SizeMax: Long): Array[UByte] = {
      require(asn1SizeMax >= 0 && asn1SizeMax < Int.MaxValue)

      val arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0.toRawUByte)
      var nCount: Int = 0

      var nLengthTmp1: Long = 0
      var nCurBlockSize1: Long = 0
      var nCurOffset1: Long = 0

      // get header data
      var nRemainingItemsVar1: Long = decodeConstrainedWholeNumber(0, 0xFF)

      // 11xx_xxxx header, there is a next fragment
      (while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
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
            assert(false, "unsupported format")

         // fill current payload fragment into dest
         var i1: Int = nCurOffset1.toInt
         (while (nCurOffset1 + nCurBlockSize1 <= asn1SizeMax) && (i1 < (nCurOffset1 + nCurBlockSize1).toInt) do
            decreases((nCurOffset1 + nCurBlockSize1).toInt - i1)
            arr(i1) = readByte()
            i1 += 1
         ).invariant(true) // TODO invariant

         // sum combined length
         nLengthTmp1 += nCurBlockSize1
         // set offset for next run
         nCurOffset1 += nCurBlockSize1

         // get next header
         nRemainingItemsVar1 = decodeConstrainedWholeNumber(0, 0xFF)

      ).invariant(true) // TODO invariant

      // 1000_0000 header, last fragment has size bigger than 255 - current byte is upper, need to get lower
      if (nRemainingItemsVar1 & 0x80) > 0 then

         nRemainingItemsVar1 <<= 8 // put upper at correct position

         // get size (lower byte)
         nRemainingItemsVar1 |= decodeConstrainedWholeNumber(0, 0xFF) // combine 15bit (7 upper, 8 lower) into size
         nRemainingItemsVar1 &= 0x7FFF // clear the control bit

      assert(nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax) // TODO check with C implementation and standard

      var i1: Int = nCurOffset1.toInt

      // fill last payload fragment into dest
      (while i1 < (nCurOffset1 + nRemainingItemsVar1).toInt do
         decreases((nCurOffset1 + nRemainingItemsVar1).toInt - i1)
         arr(i1) = readByte()
         i1 += 1
      ).invariant(true) // TODO invariant

      // add remainingSize to already written size - this var holds the absolut number in all fragments
      nLengthTmp1 += nRemainingItemsVar1

      // resize output array and copy data
      assert((nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax)) // TODO check with C implementation and standard

      val newArr: Array[UByte] = Array.fill(nLengthTmp1.toInt)(0.toRawUByte)
      arrayCopyOffsetLen(arr, newArr, 0, 0, newArr.length)
      newArr
   }

   def encodeOctetString(arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long) = {
      require(nCount >= asn1SizeMin && nCount <= asn1SizeMax)

      if asn1SizeMax < 65536 then
         if asn1SizeMin != asn1SizeMax then
            encodeConstrainedWholeNumber(nCount.toLong, asn1SizeMin, asn1SizeMax)
         encodeOctetString_no_length(arr, nCount)

      else
         encodeOctetString_fragmentation(arr, nCount)
   }

   def decodeOctetString(asn1SizeMin: Long, asn1SizeMax: Long): Array[UByte] = {

      if asn1SizeMax >= 0x1_00_00 then // 65'536, bigger than 2 unsigned bytes
         return decodeOctetString_fragmentation(asn1SizeMax)

      var nCount: Int = 0
      if asn1SizeMin != asn1SizeMax then
         nCount = decodeConstrainedWholeNumber(asn1SizeMin, asn1SizeMax).toInt
      else
         nCount = asn1SizeMin.toInt

      assert(nCount >= asn1SizeMin && nCount <= asn1SizeMax) // TODO check with C implementation and standard

      decodeOctetString_no_length(nCount)
   }

   def encodeBitString(arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long): Unit = {
      if asn1SizeMax < 65536 then
         if asn1SizeMin != asn1SizeMax then
            encodeConstrainedWholeNumber(nCount.toLong, asn1SizeMin, asn1SizeMax)

         appendBitsMSBFirst(arr, nCount)

      else
         var nRemainingItemsVar1: Long = nCount.toLong
         var nCurBlockSize1: Long = 0
         var nCurOffset1: Long = 0
         (while nRemainingItemsVar1 >= 0x4000 do
            decreases(nRemainingItemsVar1)

            if nRemainingItemsVar1 >= 0x10000 then
               nCurBlockSize1 = 0x10000
               encodeConstrainedWholeNumber(0xC4, 0, 0xFF)

            else if nRemainingItemsVar1 >= 0xC000 then
               nCurBlockSize1 = 0xC000
               encodeConstrainedWholeNumber(0xC3, 0, 0xFF)
            else if nRemainingItemsVar1 >= 0x8000 then
               nCurBlockSize1 = 0x8000
               encodeConstrainedWholeNumber(0xC2, 0, 0xFF)
            else
               nCurBlockSize1 = 0x4000
               encodeConstrainedWholeNumber(0xC1, 0, 0xFF)

            val t: Array[UByte] = Array.fill(nCurBlockSize1.toInt)(0.toRawUByte) // STAINLESS: arr.slice((nCurOffset1 / 8).toInt, (nCurOffset1 / 8).toInt + nCurBlockSize1.toInt)
            appendBitsMSBFirst(t, nCurBlockSize1.toInt)
            nCurOffset1 += nCurBlockSize1
            nRemainingItemsVar1 -= nCurBlockSize1
         ).invariant(true) // TODO invariant

         if nRemainingItemsVar1 <= 0x7F then
            encodeConstrainedWholeNumber(nRemainingItemsVar1, 0, 0xFF)
         else
            appendBit(true)
            encodeConstrainedWholeNumber(nRemainingItemsVar1, 0, 0x7FFF)

         val t: Array[UByte] = Array.fill(nRemainingItemsVar1.toInt)(0.toRawUByte) // STAINLESS: arr.slice((nCurOffset1 / 8).toInt, (nCurOffset1 / 8).toInt + nRemainingItemsVar1.toInt)
         appendBitsMSBFirst(t, nRemainingItemsVar1.toInt)
   }

   def decodeBitString(asn1SizeMin: Long, asn1SizeMax: Long): Array[UByte] = {
      require(asn1SizeMax <= Int.MaxValue)
      // TODO enhance precondition

      if (asn1SizeMax < 65536) {
         var nCount: Long = 0
         if asn1SizeMin != asn1SizeMax then
            nCount = decodeConstrainedWholeNumber(asn1SizeMin, asn1SizeMax)
         else
            nCount = asn1SizeMin

         return readBits(nCount.toInt)

      }

      var nCurBlockSize1: Long = 0
      var nCurOffset1: Long = 0
      var nLengthTmp1: Long = 0
      var nRemainingItemsVar1: Long = decodeConstrainedWholeNumber(0, 0xFF)

      val arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0.toRawUByte)
      (while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
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
            assert(false, "broken State") // TODO check with C implementation and standard

         assert(nCurOffset1 + nCurBlockSize1 > asn1SizeMax)

         arrayCopyOffsetLen(readBits(nCurBlockSize1.toInt), arr, 0, (nCurOffset1 / 8).toInt, nCurBlockSize1.toInt)
         nLengthTmp1 += nCurBlockSize1
         nCurOffset1 += nCurBlockSize1
         nRemainingItemsVar1 = decodeConstrainedWholeNumber(0, 0xFF)

      ).invariant(true) // TODO invariant

      if (nRemainingItemsVar1 & 0x80) > 0 then
         nRemainingItemsVar1 <<= 8
         nRemainingItemsVar1 |= decodeConstrainedWholeNumber(0, 0xFF)
         nRemainingItemsVar1 &= 0x7FFF

      assert(nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax)

      arrayCopyOffsetLen(readBits(nRemainingItemsVar1.toInt), arr, 0, (nCurOffset1 / 8).toInt, nRemainingItemsVar1.toInt)
      nLengthTmp1 += nRemainingItemsVar1
      assert((nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax))

      arr
   }
}
