package asn1scala

import stainless.lang.StaticChecks._
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.math.{wrapping => wrappingExpr, _}
import stainless.annotation._
import stainless.proof._

val FAILED_READ_ERR_CODE = 5400

/**
 * Get an instance of a ACN coded bitstream
 * @param count of elements in underlying buffer
 * @return ACN coded bitstream
 */
def initACNCodec(count: Int): ACN = {
   require(count >= 0)
   ACN(Codec(BitStream(Array.fill(count)(0))))
}

object ACN {
   @ghost @pure
   def reader(w1: ACN, w2: ACN): (ACN, ACN) = {
      require(w1.base.bitStream.isPrefixOf(w2.base.bitStream))
      val (r1, r2) = BitStream.reader(w1.base.bitStream, w2.base.bitStream)
      (ACN(Codec(r1)), ACN(Codec(r2)))
   }

   // TODO: Placeholder
   @ghost @pure @opaque @inlineOnce
   def readPrefixLemma_TODO(acn1: ACN, acn2: ACN): Unit = ()

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_big_endian_16_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.bufLength() == acn2.bufLength())
      require(acn1.validate_offset_bits(16))
      require(acn1.bitIndex() + 16 <= acn2.bitIndex()) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.bitIndex() + 16
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()

      {
         val end = (acn1.base.bitStream.bitIndex() / 8 + 2).toInt
         arrayRangesEqImpliesEq(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.currentByte, end)
         arrayRangesEqImpliesEq(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.currentByte + 1, end)
      }.ensuring { _ =>
         acn1Res.bitIndex() == acn2Res.bitIndex() && i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_big_endian_32_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.bufLength() == acn2.bufLength())
      require(acn1.validate_offset_bits(32))
      require(acn1.bitIndex() + 32 <= acn2.bitIndex()) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.bitIndex() + 32
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()

      {
         arrayBitRangesEqSlicedLemma(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.bitIndex() + 32, 0, acn1.base.bitStream.bitIndex() + 16)
         dec_Int_PositiveInteger_ConstSize_big_endian_16_prefixLemma(acn1, acn2.resetAt(acn1).withMovedBitIndex(16))
         val (acn1_2, hi1) = acn1.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()
         val (acn2_2, hi2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()
         assert(hi1 == hi2)
         BitStream.resetAndThenMovedLemma(acn1_2.base.bitStream, acn2_2.base.bitStream, 16)
         dec_Int_PositiveInteger_ConstSize_big_endian_16_prefixLemma(acn1_2, acn2_2.resetAt(acn1_2).withMovedBitIndex(16))
         val (_, lo1) = acn1_2.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()
         BitStream.validateOffsetBitsDifferenceLemma(acn1.base.bitStream, acn2_2.base.bitStream, 32, 16)
         val (_, lo2) = acn2_2.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()
         assert(lo1 == lo2)
      }.ensuring { _ =>
         acn1Res.base.bitStream.bitIndex() == acn2Res.base.bitStream.bitIndex() && i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_big_endian_64_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.bufLength() == acn2.bufLength())
      require(acn1.validate_offset_bits(64))
      require(acn1.bitIndex() + 64 <= acn2.bitIndex()) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.bitIndex() + 64
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_big_endian_64_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_big_endian_64_pure()

      {
         arrayBitRangesEqSlicedLemma(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.bitIndex() + 64, 0, acn1.base.bitStream.bitIndex() + 32)
         dec_Int_PositiveInteger_ConstSize_big_endian_32_prefixLemma(acn1, acn2.resetAt(acn1).withMovedBitIndex(32))
         val (acn1_2, hi1) = acn1.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()
         val (acn2_2, hi2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()
         assert(hi1 == hi2)
         BitStream.resetAndThenMovedLemma(acn1_2.base.bitStream, acn2_2.base.bitStream, 32)
         dec_Int_PositiveInteger_ConstSize_big_endian_32_prefixLemma(acn1_2, acn2_2.resetAt(acn1_2).withMovedBitIndex(32))
         val (_, lo1) = acn1_2.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()
         BitStream.validateOffsetBitsDifferenceLemma(acn1.base.bitStream, acn2_2.base.bitStream, 64, 32)
         val (_, lo2) = acn2_2.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()
         assert(lo1 == lo2)
      }.ensuring { _ =>
         acn1Res.base.bitStream.bitIndex() == acn2Res.base.bitStream.bitIndex() && i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_little_endian_16_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.bufLength() == acn2.bufLength())
      require(acn1.validate_offset_bits(16))
      require(acn1.bitIndex() + 16 <= acn2.bitIndex()) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.bitIndex() + 16
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()

      {
         val end = (acn1.base.bitStream.bitIndex() / 8 + 2).toInt
         arrayRangesEqImpliesEq(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.currentByte, end)
         arrayRangesEqImpliesEq(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.currentByte + 1, end)
      }.ensuring { _ =>
         acn1Res.bitIndex() == acn2Res.bitIndex() && i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_little_endian_32_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.bufLength() == acn2.bufLength())
      require(acn1.validate_offset_bits(32))
      require(acn1.bitIndex() + 32 <= acn2.bitIndex()) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.bitIndex() + 32
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()

      {
         arrayBitRangesEqSlicedLemma(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.bitIndex() + 32, 0, acn1.base.bitStream.bitIndex() + 16)
         dec_Int_PositiveInteger_ConstSize_little_endian_16_prefixLemma(acn1, acn2.resetAt(acn1).withMovedBitIndex(16))
         val (acn1_2, hi1) = acn1.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()
         val (acn2_2, hi2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()
         assert(hi1 == hi2)
         BitStream.resetAndThenMovedLemma(acn1_2.base.bitStream, acn2_2.base.bitStream, 16)
         dec_Int_PositiveInteger_ConstSize_little_endian_16_prefixLemma(acn1_2, acn2_2.resetAt(acn1_2).withMovedBitIndex(16))
         val (_, lo1) = acn1_2.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()
         BitStream.validateOffsetBitsDifferenceLemma(acn1.base.bitStream, acn2_2.base.bitStream, 32, 16)
         val (_, lo2) = acn2_2.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()
         assert(lo1 == lo2)
      }.ensuring { _ =>
         acn1Res.base.bitStream.bitIndex() == acn2Res.base.bitStream.bitIndex() && i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_little_endian_64_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.bufLength() == acn2.bufLength())
      require(acn1.validate_offset_bits(64))
      require(acn1.bitIndex() + 64 <= acn2.bitIndex()) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.bitIndex() + 64
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_little_endian_64_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_little_endian_64_pure()

      {
         arrayBitRangesEqSlicedLemma(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.bitIndex() + 64, 0, acn1.base.bitStream.bitIndex() + 32)
         dec_Int_PositiveInteger_ConstSize_little_endian_32_prefixLemma(acn1, acn2.resetAt(acn1).withMovedBitIndex(32))
         val (acn1_2, hi1) = acn1.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()
         val (acn2_2, hi2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()
         assert(hi1 == hi2)
         BitStream.resetAndThenMovedLemma(acn1_2.base.bitStream, acn2_2.base.bitStream, 32)
         dec_Int_PositiveInteger_ConstSize_little_endian_32_prefixLemma(acn1_2, acn2_2.resetAt(acn1_2).withMovedBitIndex(32))
         val (_, lo1) = acn1_2.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()
         BitStream.validateOffsetBitsDifferenceLemma(acn1.base.bitStream, acn2_2.base.bitStream, 64, 32)
         val (_, lo2) = acn2_2.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()
         assert(lo1 == lo2)
      }.ensuring { _ =>
         acn1Res.base.bitStream.bitIndex() == acn2Res.base.bitStream.bitIndex() && i1 == i2
      }
   }
}
case class ACN(base: Codec) {
   import BitStream.*
   import ACN.*
   import base.*
   export base.{isPrefixOf => _, withMovedBitIndex => _, withMovedByteIndex => _, *}

   /*ACN Integer functions*/

   def enc_Int_PositiveInteger_ConstSize(intVal: Int, encodedSizeInBits: Int): Unit = {
      val nBits: Int = GetBitCountUnsigned(intVal.toLong.toRawULong)
      require(nBits <= encodedSizeInBits && encodedSizeInBits <= 64)
      require(validate_offset_bits(encodedSizeInBits))
      enc_Int_PositiveInteger_ConstSize(intVal.toLong.toRawULong, encodedSizeInBits)
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize(intVal: ULong, encodedSizeInBits: Int): Unit = {
      /* Get number of bits*/
      val nBits: Int = GetBitCountUnsigned(intVal)
      require(nBits <= encodedSizeInBits && encodedSizeInBits <= 64)
      require(validate_offset_bits(encodedSizeInBits))
      if (encodedSizeInBits != 0) {
         @ghost val this1 = snapshot(this)
         /* put required zeros*/
         val diff = encodedSizeInBits - nBits
         appendNZeroBits(diff)
         @ghost val this2 = snapshot(this)
         ghostExpr {
            validateOffsetBitsDifferenceLemma(this1.bitStream, this.bitStream, encodedSizeInBits, diff)
         }
         /*Encode number */
         encodeUnsignedInteger(intVal)
         ghostExpr {
            assert(bitIndex() == this2.bitIndex() + nBits)
            assert(bitIndex() == this1.bitIndex() + encodedSizeInBits)
            validTransitiveLemma(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
            val this2Reset = this2.resetAt(this1)
            val (r1_1, r3_1) = ACN.reader(this1, this)
            validateOffsetBitsContentIrrelevancyLemma(this1.bitStream, this.base.bitStream.buf, encodedSizeInBits)
            val (r3Got, iGot) = r1_1.dec_Int_PositiveInteger_ConstSize_pure(encodedSizeInBits)

            if (encodedSizeInBits != nBits) {
               checkBitsLoopPrefixLemma2(this2Reset.base.bitStream, this.base.bitStream, diff, false, 0)

               val (r2_2, r3_2) = ACN.reader(this2, this)
               assert(r3_1 == r3_2)
               validateOffsetImpliesMoveBits(r1_1.bitStream, diff)
               assert(r2_2 == r1_1.withMovedBitIndex(diff))
               // TODO: Exported symbol not working
               // val (r2Got_2, vGot_2) = r2_2.readNLeastSignificantBitsLoopPure(nBits, 0, 0L)
               val (r2Got_2, vGot_2) = r2_2.base.bitStream.readNLeastSignificantBitsLoopPure(nBits, 0, 0L)
               assert(vGot_2 == intVal.toRaw)

               val (r3Got_3, vGot_3) = r1_1.bitStream.readNLeastSignificantBitsLoopPure(encodedSizeInBits, 0, 0L)
               assert(iGot.toRaw == vGot_3)
               assert(r3Got.bitStream == r3Got_3)
               checkBitsLoopAndReadNLSB(r1_1.bitStream, diff, false)
               readNLeastSignificantBitsLeadingZerosLemma(r1_1.bitStream, encodedSizeInBits, diff)
               check(iGot == intVal)
               check(r3Got == r3_1)
            } else {
               check(iGot == intVal)
               check(r3Got == r3_1)
            }
         }
      } else {
         ghostExpr {
            validReflexiveLemma(bitStream)
         }
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.bufLength() == w3.bufLength() && w3.bitIndex() == w1.bitIndex() + encodedSizeInBits && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, encodedSizeInBits)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_pure(encodedSizeInBits)
         iGot == intVal && r3Got == r3
      }
   }

   @ghost @pure @inline
   def resetAt(other: ACN): ACN = {
      require(bitStream.buf.length == other.base.bitStream.buf.length)
      ACN(Codec(bitStream.resetAt(other.base.bitStream)))
   }

   @ghost @pure @inline
   def withMovedByteIndex(diffInBytes: Int): ACN = {
      require(moveByteIndexPrecond(bitStream, diffInBytes))
      ACN(Codec(bitStream.withMovedByteIndex(diffInBytes)))
   }

   @ghost @pure @inline
   def withMovedBitIndex(diffInBits: Int): ACN = {
      require(moveBitIndexPrecond(bitStream, diffInBits))
      ACN(Codec(bitStream.withMovedBitIndex(diffInBits)))
   }

   @pure @inline
   def isPrefixOf(acn2: ACN): Boolean = bitStream.isPrefixOf(acn2.base.bitStream)

   def enc_Int_PositiveInteger_ConstSize_8(intVal: ULong): Unit = {
      require(bitStream.validate_offset_byte())
      appendByte(wrappingExpr { intVal.toUByte })
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_big_endian_16(uintVal: ULong): Unit = {
      require(bitStream.validate_offset_bits(16))
      require(uintVal <= 65535)
      val intVal = uintVal.toRaw
      assert((intVal >> 16) == 0L)
      @ghost val this1 = snapshot(this)
      appendByte(wrappingExpr { (intVal >> 8).toByte.toRawUByte })
      @ghost val this2 = snapshot(this)
      appendByte(wrappingExpr { intVal.toByte.toRawUByte })
      ghostExpr {
         // For isPrefix
         validTransitiveLemma(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first byte gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.base.bitStream.resetAt(this1.base.bitStream)
         readBytePrefixLemma(this2Reset, this.base.bitStream)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.bufLength() == w3.bufLength() && w3.bitIndex() == w1.bitIndex() + 16 && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 16)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_big_endian_32(uintVal: ULong): Unit = {
      require(bitStream.validate_offset_bits(32))
      require(uintVal <= 4294967295L)
      val intVal = uintVal.toRaw
      assert((intVal >> 32) == 0L)
      @ghost val this1 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_big_endian_16(wrappingExpr { ((intVal >> 16) & 0xFFFFL).toRawULong })
      @ghost val this2 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_big_endian_16(wrappingExpr { (intVal & 0xFFFFL).toRawULong })
      ghostExpr {
         // For isPrefix
         validTransitiveLemma(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first integer gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         dec_Int_PositiveInteger_ConstSize_big_endian_16_prefixLemma(this2Reset, this)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.bufLength() == w3.bufLength() && w3.bitIndex() == w1.bitIndex() + 32 && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 32)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_big_endian_64(uintVal: ULong): Unit = {
      require(bitStream.validate_offset_bits(64))
      val intVal = uintVal.toRaw
      @ghost val this1 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_big_endian_32(wrappingExpr { ((intVal >> 32) & 0xFFFFFFFFL).toRawULong })
      @ghost val this2 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_big_endian_32(wrappingExpr { (intVal & 0xFFFFFFFFL).toRawULong })
      ghostExpr {
         // For isPrefix
         validTransitiveLemma(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first integer gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         dec_Int_PositiveInteger_ConstSize_big_endian_32_prefixLemma(this2Reset, this)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.bufLength() == w3.bufLength() && w3.bitIndex() == w1.bitIndex() + 64 && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 64)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_big_endian_64_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_little_endian_16(uintVal: ULong): Unit = {
      require(bitStream.validate_offset_bits(16))
      require(uintVal <= 65535)
      val intVal = uintVal.toRaw
      assert((intVal >> 16) == 0L)
      @ghost val this1 = snapshot(this)
      appendByte(wrappingExpr { intVal.toUByte })
      @ghost val this2 = snapshot(this)
      appendByte(wrappingExpr { (intVal >> 8).toUByte })
      ghostExpr {
         // For isPrefix
         validTransitiveLemma(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first byte gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         readBytePrefixLemma(this2Reset.base.bitStream, this.base.bitStream)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.bufLength() == w3.bufLength() && w3.bitIndex() == w1.bitIndex() + 16 && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 16)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_little_endian_32(uintVal: ULong): Unit = {
      require(bitStream.validate_offset_bits(32))
      require(uintVal <= 4294967295L)
      val intVal = uintVal.toRaw
      assert((intVal >> 32) == 0L)
      @ghost val this1 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_little_endian_16(wrappingExpr { (intVal & 0xFFFFL).toRawULong })
      @ghost val this2 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_little_endian_16(wrappingExpr { ((intVal >> 16) & 0xFFFFL).toRawULong })
      ghostExpr {
         // For isPrefix
         validTransitiveLemma(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first integer gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         dec_Int_PositiveInteger_ConstSize_little_endian_16_prefixLemma(this2Reset, this)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.bufLength() == w3.bufLength() && w3.bitIndex() == w1.bitIndex() + 32 && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 32)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_little_endian_64(uintVal: ULong): Unit = {
      require(bitStream.validate_offset_bits(64))
      val intVal = uintVal.toRaw
      @ghost val this1 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_little_endian_32(wrappingExpr { (intVal & 0xFFFFFFFFL).toRawULong })
      @ghost val this2 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_little_endian_32(wrappingExpr { ((intVal >> 32) & 0xFFFFFFFFL).toRawULong })
      ghostExpr {
         // For isPrefix
         validTransitiveLemma(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first integer gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         dec_Int_PositiveInteger_ConstSize_little_endian_32_prefixLemma(this2Reset, this)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.bufLength() == w3.bufLength() && w3.bitIndex() == w1.bitIndex() + 64 && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 64)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_little_endian_64_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   def dec_Int_PositiveInteger_ConstSize(encodedSizeInBits: Int): ULong = {
      require(encodedSizeInBits >= 0 && encodedSizeInBits <= NO_OF_BITS_IN_LONG)
      require(bitStream.validate_offset_bits(encodedSizeInBits))
      decodeUnsignedInteger(encodedSizeInBits)
   }

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_pure(encodedSizeInBits: Int): (ACN, ULong) = {
      require(encodedSizeInBits >= 0 && encodedSizeInBits <= NO_OF_BITS_IN_LONG)
      require(bitStream.validate_offset_bits(encodedSizeInBits))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize(encodedSizeInBits)
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_8(): ULong = {
      require(bitStream.validate_offset_byte())
      (readByte().toRaw & 0xFF).toULong
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_16(): ULong = {
      require(bitStream.validate_offset_bits(16))
      val b1 = readByte().toRaw
      val b2 = readByte().toRaw
      ULong.fromRaw((((b1.toLong << 8) & 0xFF00L) | (b2.toLong & 0xFFL)) & 0xFFFFL)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && bitStream.bitIndex() == old(this).base.bitStream.bitIndex() + 16)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_big_endian_16_pure(): (ACN, ULong) = {
      require(bitStream.validate_offset_bits(16))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_big_endian_16()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_32(): ULong = {
      require(bitStream.validate_offset_bits(32))
      val i1 = dec_Int_PositiveInteger_ConstSize_big_endian_16().toRaw
      val i2 = dec_Int_PositiveInteger_ConstSize_big_endian_16().toRaw
      ULong.fromRaw((((i1.toLong << 16) & 0xFFFF0000L) | (i2.toLong & 0xFFFFL)) & 0xFFFFFFFFL)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && bitStream.bitIndex() == old(this).base.bitStream.bitIndex() + 32)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_big_endian_32_pure(): (ACN, ULong) = {
      require(bitStream.validate_offset_bits(32))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_big_endian_32()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_64(): ULong = {
      require(bitStream.validate_offset_bits(64))
      val i1 = dec_Int_PositiveInteger_ConstSize_big_endian_32().toRaw
      val i2 = dec_Int_PositiveInteger_ConstSize_big_endian_32().toRaw
      ULong.fromRaw(((i1.toLong << 32) & 0xFFFFFFFF00000000L) | (i2.toLong & 0xFFFFFFFFL))
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && bitStream.bitIndex() == old(this).base.bitStream.bitIndex() + 64)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_big_endian_64_pure(): (ACN, ULong) = {
      require(bitStream.validate_offset_bits(64))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_big_endian_64()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_16(): ULong = {
      require(bitStream.validate_offset_bits(16))
      val b1 = readByte().toRaw
      val b2 = readByte().toRaw
      ULong.fromRaw((((b2.toLong << 8) & 0xFF00L) | (b1.toLong & 0xFFL)) & 0xFFFFL)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && bitStream.bitIndex() == old(this).base.bitStream.bitIndex() + 16)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_little_endian_16_pure(): (ACN, ULong) = {
      require(bitStream.validate_offset_bits(16))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_little_endian_16()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_32(): ULong = {
      require(bitStream.validate_offset_bits(32))
      val i1 = dec_Int_PositiveInteger_ConstSize_little_endian_16().toRaw
      val i2 = dec_Int_PositiveInteger_ConstSize_little_endian_16().toRaw
      ULong.fromRaw((((i2.toLong << 16) & 0xFFFF0000L) | (i1.toLong & 0xFFFFL)) & 0xFFFFFFFFL)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && bitStream.bitIndex() == old(this).base.bitStream.bitIndex() + 32)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_little_endian_32_pure(): (ACN, ULong) = {
      require(bitStream.validate_offset_bits(32))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_little_endian_32()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_64(): ULong = {
      require(bitStream.validate_offset_bits(64))
      val i1 = dec_Int_PositiveInteger_ConstSize_little_endian_32().toRaw
      val i2 = dec_Int_PositiveInteger_ConstSize_little_endian_32().toRaw
      ULong.fromRaw(((i2.toLong << 32) & 0xFFFFFFFF00000000L) | (i1.toLong & 0xFFFFFFFFL))
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && bitStream.bitIndex() == old(this).base.bitStream.bitIndex() + 64)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_little_endian_64_pure(): (ACN, ULong) = {
      require(bitStream.validate_offset_bits(64))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_little_endian_64()
      (cpy, l)
   }

   def encode_UnsignedInteger(v: ULong, nBytes: Byte): Unit = {
      val MAX_BYTE_MASK = 0xFF00000000000000L
      assert(nBytes <= 8)

      var vv = v.toRaw << (NO_OF_BYTES_IN_JVM_LONG * 8 - nBytes * 8)

      var i: Int = 0
      while i < nBytes do
         val byteToEncode = ((vv & MAX_BYTE_MASK) >>> ((NO_OF_BYTES_IN_JVM_LONG - 1) * 8)).toByte
         appendByte(byteToEncode.toRawUByte)
         vv <<= 8
         i += 1
   }

   def enc_Int_PositiveInteger_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val nBytes = GetLengthForEncodingUnsigned(intVal).toByte

      /* encode length */
      appendByte(nBytes.toRawUByte)
      /* Encode integer data*/
      encode_UnsignedInteger(intVal, nBytes)
   }

   def dec_Int_PositiveInteger_VarSize_LengthEmbedded(): ULong = {
      var v: Long = 0

      val nBytes = readByte().toRaw.toInt
      var i: Int = 0

      (while i < nBytes do
         decreases(nBytes - i)
         v = (v << 8) | readByte().unsignedToInt
         i += 1
      ).invariant(true) // TODO invariant

      ULong.fromRaw(v)
   }

   /**
    * Encode the value v into a two complement n-Bit integer.
    *
    * Example:
    *    v = 12d = 0000'0...0'1100b
    *    formatBitLength = 6
    *                         6-bit Signed Integer
    *                         vv'vvvv
    *    --> encoded value is 00'1100
    *
    * Example 2:
    *    v = -2d = 1111'1...1'1110b
    *    formatBitLength = 5
    *                         5-bit Signed Integer
    *                         v'vvvv
    *    --> encoded value is 1'1110
    *
    * @param v                value that gets encoded
    * @param formatBitLength  number of dataformat bits
    */
   @opaque @inlineOnce
   def enc_Int_TwosComplement_ConstSize(v: Long, formatBitLength: Int): Unit = {
      val nBits = GetBitCountSigned(v)
      require(nBits <= formatBitLength && formatBitLength <= NO_OF_BITS_IN_LONG)
      require(validate_offset_bits(formatBitLength))

      val addedBits = formatBitLength - nBits
      @ghost val this1 = snapshot(this)
      appendNBits(addedBits, v < 0)
      ghostExpr {
         validateOffsetBitsDifferenceLemma(this1.bitStream, this.bitStream, formatBitLength, addedBits)
      }
      @ghost val this2 = snapshot(this)
      appendNLeastSignificantBits(v & onesLSBLong(nBits), nBits)
      ghostExpr {
         assert(bitIndex() == this2.bitIndex() + nBits)
         assert(bitIndex() == this1.bitIndex() + formatBitLength)
         validTransitiveLemma(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         val this2Reset = this2.resetAt(this1)
         val (r1_1, r3_1) = ACN.reader(this1, this)
         validateOffsetBitsContentIrrelevancyLemma(this1.bitStream, this.base.bitStream.buf, formatBitLength)
         val (r3Got, vGot) = r1_1.dec_Int_TwosComplement_ConstSize_pure(formatBitLength)

         if (formatBitLength != nBits) {
            checkBitsLoopPrefixLemma2(this2Reset.base.bitStream, this.base.bitStream, addedBits, v < 0, 0)

            val (r2_2, r3_2) = ACN.reader(this2, this)
            assert(r3_1 == r3_2)
            validateOffsetImpliesMoveBits(r1_1.bitStream, addedBits)
            assert(r2_2 == r1_1.withMovedBitIndex(addedBits))
            val (r2Got_2, vGot_2) = r2_2.base.bitStream.readNLeastSignificantBitsLoopPure(nBits, 0, 0L)
            assert(vGot_2 == (v & onesLSBLong(nBits)))

            val (r3Got_3, vGot_3) = r1_1.bitStream.readNLeastSignificantBitsLoopPure(formatBitLength, 0, 0L)

            checkBitsLoopAndReadNLSB(r1_1.bitStream, addedBits, v < 0)
            readNLeastSignificantBitsLeadingBitsLemma(r1_1.bitStream, v < 0, formatBitLength, addedBits)
            assert(vGot == (bitMSBLong(v < 0, NO_OF_BITS_IN_LONG - formatBitLength) | vGot_3))
            assert(r3Got.bitStream == r3Got_3)
            assert(((vGot_3 & (1L << (formatBitLength - 1))) == 0L) == v >= 0)
            if (v < 0) {
               assert((v & (1L << (formatBitLength - 1))) != 0L)
               check(vGot == v)
            } else {
               assert((v & onesLSBLong(nBits)) == v)
               assert(formatBitLength == 0 || (v & (1L << (formatBitLength - 1))) == 0L)
               check(vGot == v)
            }
            check(r3Got == r3_1)
         } else {
            if (v < 0) {
               assert((onesMSBLong(NO_OF_BITS_IN_LONG - nBits) | (v & onesLSBLong(nBits))) == v)
               assert(((v & onesLSBLong(nBits)) & (1L << (nBits - 1))) != 0L)
               check(vGot == v)
            } else {
               assert((v & onesLSBLong(nBits)) == v)
               assert(nBits == 0 || (v & (1L << (nBits - 1))) == 0L)
               check(vGot == v)
            }
            check(vGot == v)
            check(r3Got == r3_1)
         }
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.bufLength() == w3.bufLength() && w3.bitIndex() == w1.bitIndex() + formatBitLength && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, formatBitLength)
         val (r3Got, vGot) = r1.dec_Int_TwosComplement_ConstSize_pure(formatBitLength)
         vGot == v && r3Got == r3
      }
   }

   def enc_Int_TwosComplement_ConstSize_8(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_8(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_16(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_16(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_32(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_32(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_64(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_64(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_16(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_16(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_32(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_32(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_64(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_64(int2uint(intVal))
   }

   def dec_Int_TwosComplement_ConstSize(encodedSizeInBits: Int): Long = {
      require(encodedSizeInBits >= 0 && encodedSizeInBits <= NO_OF_BITS_IN_LONG)
      require(validate_offset_bits(encodedSizeInBits))

      val res = readNLeastSignificantBits(encodedSizeInBits)
      if encodedSizeInBits == 0 || (res & (1L << (encodedSizeInBits - 1))) == 0L then res
      else onesMSBLong(NO_OF_BITS_IN_LONG - encodedSizeInBits) | res
      /*
      val valIsNegative = peekBit()

      val nBytes: Int = encodedSizeInBits / 8
      val rstBits: Int = encodedSizeInBits % 8

      var pIntVal: Long = if valIsNegative then Long.MaxValue else 0

      var i: Int = 0
      (while i < nBytes do
         decreases(nBytes - i)
         pIntVal = (pIntVal << 8) | (readByte().unsignedToInt)
         i += 1
         ).invariant(true) // TODO invariant

      if rstBits > 0 then
          pIntVal = (pIntVal << rstBits) | (readPartialByte(rstBits.toByte).toRaw & 0xFF)

      pIntVal
      */
   }

   @ghost @pure
   def dec_Int_TwosComplement_ConstSize_pure(encodedSizeInBits: Int): (ACN, Long) = {
      require(encodedSizeInBits >= 0 && encodedSizeInBits <= NO_OF_BITS_IN_LONG)
      require(validate_offset_bits(encodedSizeInBits))

      val cpy = snapshot(this)
      val l = cpy.dec_Int_TwosComplement_ConstSize(encodedSizeInBits)
      (cpy, l)
   }


   def dec_Int_TwosComplement_ConstSize_8(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_8(), 1)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_16(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_16(), NO_OF_BYTES_IN_JVM_SHORT)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_32(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_32(), NO_OF_BYTES_IN_JVM_INT)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_64(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_64(), NO_OF_BYTES_IN_JVM_LONG)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_16(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_little_endian_16(), NO_OF_BYTES_IN_JVM_SHORT)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_32(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_little_endian_32(), NO_OF_BYTES_IN_JVM_INT)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_64(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_little_endian_64(), NO_OF_BYTES_IN_JVM_LONG)
   }

   def enc_Int_TwosComplement_VarSize_LengthEmbedded(intVal: Long): Unit = {
      val nBytes = GetLengthForEncodingSigned(intVal).toByte

      /* encode length */
      appendByte(nBytes.toRawUByte)
      /* Encode integer data*/
      encode_UnsignedInteger(int2uint(intVal), nBytes)
   }

   def dec_Int_TwosComplement_VarSize_LengthEmbedded(): Long = {
      var v: Long = 0
      var isNegative: Boolean = false

      val nBytes = readByte().toRaw.toInt

      var i: Int = 0
      (while i < nBytes do
         decreases(nBytes - i)
         val ub = readByte().toRaw

         if i == 0 && (ub.unsignedToInt) > 0 then
            v = Long.MaxValue
            isNegative = true

         v = (v << 8) | (ub & 0xFF)
         i += 1
      ).invariant(true) // TODO invariant

      if isNegative then
         -(~v) - 1      // TODO fixme
      else
         v
   }

   //return values is in nibbles
   def get_Int_Size_BCD(intVal: ULong): Int = {
      var intVar = intVal.toRaw
      var ret: Int = 0
      while intVar > 0 do
         intVar /= 10
         ret += 1

      ret
   }


   def enc_Int_BCD_ConstSize(intVal: ULong, encodedSizeInNibbles: Int): Unit = {
      var intVar = intVal.toRaw
      var totalNibbles: Int = 0
      val tmp: Array[Byte] = Array.fill(100)(0)

      assert(100 >= encodedSizeInNibbles)

      while intVar > 0 do
         tmp(totalNibbles) = (intVar % 10).cutToByte
         totalNibbles += 1
         intVar /= 10

      assert(encodedSizeInNibbles >= totalNibbles)

      var i: Int = encodedSizeInNibbles - 1
      while i >= 0 do
         appendPartialByte(tmp(i).toRawUByte, 4)
         i -= 1
   }


   def dec_Int_BCD_ConstSize(encodedSizeInNibbles: Int): ULong = {
      var l: Long = 0

      var encodedSizeInNibblesVar = encodedSizeInNibbles

      (while encodedSizeInNibblesVar > 0 do
         decreases(encodedSizeInNibblesVar)

         l *= 10
         l += readPartialByte(4).toRaw
         encodedSizeInNibblesVar -= 1
      ).invariant(true) // TODO invariant
      ULong.fromRaw(l)
   }


   def enc_Int_BCD_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val nNibbles: Int = get_Int_Size_BCD(intVal)
      /* encode length */
      appendByte(nNibbles.toByte.toRawUByte)

      /* Encode Number */
      enc_Int_BCD_ConstSize(intVal, nNibbles)
   }


   def dec_Int_BCD_VarSize_LengthEmbedded(): ULong = {
       dec_Int_BCD_ConstSize(readByte().toRaw.toInt)
   }


   //encoding puts an 'F' at the end
   def enc_Int_BCD_VarSize_NullTerminated(intVal: ULong): Unit = {

      val nNibbles: Int = get_Int_Size_BCD(intVal)

      /* Encode Number */
      enc_Int_BCD_ConstSize(intVal, nNibbles)

      appendPartialByte(0xF.toRawUByte, 4)
   }

   def dec_Int_BCD_VarSize_NullTerminated(): ULong = {
      var l: Long = 0

      while true do
          val digit = readPartialByte(4).toRaw
          if (digit > 9)
            return ULong.fromRaw(l)

          l *= 10
          l += digit

      ULong.fromRaw(l)
   }

   def enc_UInt_ASCII_ConstSize(intVal: ULong, encodedSizeInBytes: Int): Unit = {
      var intVar = intVal.toRaw
      var totalNibbles: Int = 0
      val tmp: Array[Byte] = Array.fill(100)(0)

      assert(100 >= encodedSizeInBytes)

      while intVar > 0 do
         tmp(totalNibbles) = (intVar % 10).cutToByte
         totalNibbles += 1
         intVar /= 10

      assert(encodedSizeInBytes >= totalNibbles)

      var i = encodedSizeInBytes - 1
      while i >= 0 do
         appendByte((tmp(i) + CHAR_ZERO).toByte.toRawUByte)
         i -= 1
   }


   def enc_SInt_ASCII_ConstSize(intVal: Long, encodedSizeInBytes: Int): Unit = {
      val absIntVal: ULong = (if intVal >= 0 then intVal else -intVal).toRawULong

      /* encode sign */
      appendByte((if intVal >= 0 then CHAR_PLUS else CHAR_MINUS).toRawUByte)

      enc_UInt_ASCII_ConstSize(absIntVal, encodedSizeInBytes - 1)
   }

   def dec_UInt_ASCII_ConstSize(encodedSizeInBytes: Int): ULong = {
      var encodedSizeInBytesVar = encodedSizeInBytes
      var ret: Long = 0

      (while encodedSizeInBytesVar > 0 do
         decreases(encodedSizeInBytesVar)
         val digit = readByte().toRaw

         assert(digit >= CHAR_ZERO && digit <= CHAR_NINE)

         ret *= 10
         ret += (digit.toInt - CHAR_ZERO).toByte

         encodedSizeInBytesVar -= 1
      ).invariant(true) // TODO invariant

      ULong.fromRaw(ret)
   }

   def dec_SInt_ASCII_ConstSize(encodedSizeInBytes: Int): Long = {
      val digit = readByte().toRaw

      var sign: Int = 1
      if digit == CHAR_PLUS then
         sign = 1
      else if digit == CHAR_MINUS then
         sign = -1
      else
         assert(false)

      sign * dec_UInt_ASCII_ConstSize(encodedSizeInBytes - 1).toRaw
   }


   def getIntegerDigits(intVal: ULong): (Array[Byte], Byte) = {
      var intVar = intVal.toRaw
      val digitsArray100: Array[Byte] = Array.fill(100)(0)
      val reversedDigitsArray: Array[Byte] = Array.fill(100)(0)
      var totalDigits: Byte = 0


      if intVar > 0 then
         while intVar > 0 && totalDigits < 100 do
            reversedDigitsArray(totalDigits) = (CHAR_ZERO + (intVar % 10)).toByte
            totalDigits = (totalDigits + 1).toByte
            intVar /= 10

         var i: Int = totalDigits - 1
         while i >= 0 do
            digitsArray100(totalDigits - 1 - i) = reversedDigitsArray(i)
            i -= 1

      else
         digitsArray100(0) = CHAR_ZERO
         totalDigits = 1

      (digitsArray100, totalDigits)
   }

   def enc_SInt_ASCII_VarSize_LengthEmbedded(intVal: Long): Unit = {
      val absIntVal: ULong = (if intVal >= 0 then intVal else -intVal).toRawULong
      val (digitsArray100, nChars) = getIntegerDigits(absIntVal)

      /* encode length, plus 1 for sign */
      appendByte((nChars + 1).toByte.toRawUByte)

      /* encode sign */
      appendByte((if intVal >= 0 then CHAR_PLUS else CHAR_MINUS).toRawUByte)

      /* encode digits */
      var i: Int = 0
      while i < 100 && digitsArray100(i) != 0x0 do
         appendByte(digitsArray100(i).toRawUByte)
         i += 1
   }

   def enc_UInt_ASCII_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val (digitsArray100, nChars) = getIntegerDigits(intVal)

      /* encode length */
      appendByte(nChars.toRawUByte)
      /* encode digits */
      var i: Int = 0
      while i < 100 && digitsArray100(i) != 0x0 do
         appendByte(digitsArray100(i).toRawUByte)
         i += 1
   }


   def dec_UInt_ASCII_VarSize_LengthEmbedded(): ULong = dec_UInt_ASCII_ConstSize(readByte().toRaw)
   def dec_SInt_ASCII_VarSize_LengthEmbedded(): Long = dec_SInt_ASCII_ConstSize(readByte().toRaw)

   def enc_UInt_ASCII_VarSize_NullTerminated(intVal: ULong, null_characters: Array[Byte], null_characters_size: Int): Unit = {
      val (digitsArray100, nChars) = getIntegerDigits(intVal)

      var i: Int = 0 // TODO: size_t?
      while i < 100 && digitsArray100(i) != 0x0 do
         appendByte(digitsArray100(i).toRawUByte)
         i += 1

      i = 0
      while i < null_characters_size do
         appendByte(null_characters(i).toRawUByte)
         i += 1
   }

   def enc_SInt_ASCII_VarSize_NullTerminated(intVal: Long, null_characters: Array[Byte], null_characters_size: Int): Unit = {
      val absValue: ULong = (if intVal >= 0 then intVal else -intVal).toRawULong
      appendByte((if intVal >= 0 then CHAR_PLUS else CHAR_MINUS).toRawUByte)

      enc_UInt_ASCII_VarSize_NullTerminated(absValue, null_characters, null_characters_size)
   }

   def dec_UInt_ASCII_VarSize_NullTerminated(null_characters: Array[Byte], null_characters_size: Int): ULong = {
      var digit: Byte = 0
      var ret: Long = 0
      val tmp: Array[Byte] = Array.fill(10)(0)

      val sz: Int = if null_characters_size < 10 then null_characters_size else 10

      //read null_character_size characters into the tmp buffer
      var j: Int = 0
      (while j < null_characters_size do
         decreases(null_characters_size - j)
         tmp(j) = readByte().toRaw
         j += 1
      ).invariant(true) // TODO invariant

      var i: Long = 0
      while !arraySameElements(null_characters, tmp) do
         digit = tmp(0)
         i += 1

         j = 0
         while j < null_characters_size - 1 do
            tmp(j) = tmp(j + 1)
            j += 1

         tmp(null_characters_size - 1) = readByte().toRaw

         digit = (digit - CHAR_ZERO).toByte

         ret *= 10
         ret += digit

      ULong.fromRaw(ret)
   }


   def dec_SInt_ASCII_VarSize_NullTerminated(null_characters: Array[Byte], null_characters_size: Int): Long = {
      var isNegative: Boolean = false

      val digit = readByte().toRaw
      assert(digit == CHAR_MINUS || digit == CHAR_PLUS)
      if digit == CHAR_MINUS then
         isNegative = true

      val ul = dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toRaw
      if isNegative then -ul else ul
   }


   /* Boolean Decode */
   // TODO move to codec?
   def BitStream_ReadBitPattern(patternToRead: Array[UByte], nBitsToRead: Int): Boolean = {
      val nBytesToRead: Int = nBitsToRead / 8
      val nRemainingBitsToRead: Int = nBitsToRead % 8

      var pBoolValue: Boolean = true
      var i: Int = 0
      (while i < nBytesToRead do
         decreases(nBytesToRead - i)
         if readByte() != patternToRead(i) then
            pBoolValue = false
         i += 1
      ).invariant(true) // TODO

      if nRemainingBitsToRead > 0 then
         if readPartialByte(nRemainingBitsToRead.toByte).toRaw != ((patternToRead(nBytesToRead).toRaw & 0xFF) >>> (8 - nRemainingBitsToRead)) then
            pBoolValue = false

      pBoolValue
   }

   // TODO move to codec?
   def BitStream_ReadBitPattern_ignore_value(nBitsToRead: Int): Unit = {
      // TODO replace implementation with readBits(nBitsToRead)?
      val nBytesToRead: Int = nBitsToRead / 8
      val nRemainingBitsToRead: Int = nBitsToRead % 8

      var i: Int = 0
      (while i < nBytesToRead do
         decreases(nBytesToRead - i)
         readByte()
         i += 1
      ).invariant(true) // TODO invariant

      if nRemainingBitsToRead > 0 then
         readPartialByte(nRemainingBitsToRead.toByte)
   }

   /* Real encoding functions */
   @extern
   def enc_Real_IEEE754_32_big_endian(realValue: Float): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(NO_OF_BYTES_IN_JVM_FLOAT).putFloat(realValue).array

      var i: Int = 0
      while i < NO_OF_BYTES_IN_JVM_FLOAT do
         appendByte(b(i).toRawUByte)
         i += 1
   }

   @extern
   def enc_Real_IEEE754_32_little_endian(realValue: Float): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(NO_OF_BYTES_IN_JVM_FLOAT).putFloat(realValue).array

      var i: Int = NO_OF_BYTES_IN_JVM_FLOAT - 1
      while i >= 0 do
         appendByte(b(i).toRawUByte)
         i -= 1
   }

   @extern
   def enc_Real_IEEE754_64_big_endian(realValue: Double): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(NO_OF_BYTES_IN_JVM_DOUBLE).putDouble(realValue).array

      var i: Int = 0
      while i < NO_OF_BYTES_IN_JVM_DOUBLE do
         appendByte(b(i).toRawUByte)
         i += 1
   }

   @extern
   def enc_Real_IEEE754_64_little_endian(realValue: Double): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(NO_OF_BYTES_IN_JVM_DOUBLE).putDouble(realValue).array

      var i: Int = NO_OF_BYTES_IN_JVM_DOUBLE - 1
      while i >= 0 do
         appendByte(b(i).toRawUByte)
         i -= 1
   }

   /* Real decoding functions */
   @extern
   def dec_Real_IEEE754_32_big_endian(): Float = {
      var ret: Int = 0
      var i: Int = 1

      assert(NO_OF_BYTES_IN_JVM_INT == NO_OF_BYTES_IN_JVM_FLOAT)

      (while i <= NO_OF_BYTES_IN_JVM_INT do
         decreases(NO_OF_BYTES_IN_JVM_INT - i)
         ret |= readByte().unsignedToInt << (NO_OF_BYTES_IN_JVM_INT - i) * NO_OF_BITS_IN_BYTE
         i += 1
      ).invariant(true) // TODO

      java.lang.Float.intBitsToFloat(ret)
   }

   @extern
   def dec_Real_IEEE754_32_little_endian(): Float = {
      var ret: Int = 0
      var i: Int = 0

      assert(NO_OF_BYTES_IN_JVM_INT == NO_OF_BYTES_IN_JVM_FLOAT)

      (while i < NO_OF_BYTES_IN_JVM_INT do
         decreases(NO_OF_BYTES_IN_JVM_INT - i)
         ret |= readByte().unsignedToInt << i * NO_OF_BITS_IN_BYTE
         i += 1
      ).invariant(true) // TODO

      java.lang.Float.intBitsToFloat(ret)
   }

   @extern
   def dec_Real_IEEE754_64_big_endian(): Double = {
      var ret: Long = 0
      var i: Int = 1

      assert(NO_OF_BYTES_IN_JVM_LONG == NO_OF_BYTES_IN_JVM_DOUBLE)

      (while i <= NO_OF_BYTES_IN_JVM_LONG do
         decreases(NO_OF_BYTES_IN_JVM_LONG - i)
         ret |= readByte().unsignedToLong << (NO_OF_BYTES_IN_JVM_LONG - i) * NO_OF_BITS_IN_BYTE
         i += 1
      ).invariant(true) // TODO

      java.lang.Double.longBitsToDouble(ret)
   }

   @extern
   def dec_Real_IEEE754_64_little_endian(): Double = {
      var ret: Long = 0
      var i: Int = 0

      assert(NO_OF_BYTES_IN_JVM_LONG == NO_OF_BYTES_IN_JVM_DOUBLE)

      (while i < NO_OF_BYTES_IN_JVM_LONG do
         decreases(NO_OF_BYTES_IN_JVM_LONG - i)
         ret |= readByte().unsignedToLong << i * NO_OF_BITS_IN_BYTE
         i += 1
      ).invariant(true) // TODO

      java.lang.Double.longBitsToDouble(ret)
   }

   /* String functions*/
   def enc_String_Ascii_FixSize(max: Long, strVal: Array[ASCIIChar]): Unit = {
      var i: Long = 0
      while i < max do
         appendByte(strVal(i.toInt))
         i += 1
   }

   def enc_String_Ascii_private(max: Long, strVal: Array[ASCIIChar]): Long = {
      var i: Long = 0
      while (i < max) && (strVal(i.toInt).toRaw != CHAR_0000) do
         appendByte(strVal(i.toInt))
         i += 1

      i
   }

   def enc_String_Ascii_Null_Terminated(max: Long, null_character: Byte, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
      appendByte(null_character.toByte.toRawUByte)
   }

   def enc_String_Ascii_Null_Terminated_mult(max: Long, null_character: Array[Byte], null_character_size: Int, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
      var i: Int = 0
      while i < null_character_size do
         appendByte(null_character(i).toRawUByte)
         i += 1
   }


   def enc_String_Ascii_External_Field_Determinant(max: Long, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
   }

   def enc_String_Ascii_Internal_Field_Determinant(max: Long, min: Long, strVal: Array[ASCIIChar]): Unit = {
      val strLen: Int = strVal.length
      encodeConstrainedWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_Ascii_private(max, strVal)
   }

   def enc_String_CharIndex_FixSize(max: Long, allowedCharSet: Array[UByte], strVal: Array[ASCIIChar]): Unit = {
      var i: Int = 0
      while i < max do
         val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
         encodeConstrainedWholeNumber(charIndex, 0, allowedCharSet.length - 1)
         i += 1
   }

   def enc_String_CharIndex_private(max: Long, allowedCharSet: Array[UByte], strVal: Array[ASCIIChar]): Long = {
      var i: Int = 0
      while (i < max) && (strVal(i).toRaw != CHAR_0000) do
         val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
         encodeConstrainedWholeNumber(charIndex, 0, allowedCharSet.length - 1)
         i += 1

      i
   }


   def enc_String_CharIndex_External_Field_Determinant(max: Long, allowedCharSet: Array[UByte], strVal: Array[ASCIIChar]): Unit = {
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
   }

   def enc_String_CharIndex_Internal_Field_Determinant(max: Long, allowedCharSet: Array[UByte], min: Long, strVal: Array[ASCIIChar]): Unit = {
      val strLen: Int = strVal.length
      encodeConstrainedWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
   }


   def enc_IA5String_CharIndex_External_Field_Determinant(max: Long, strVal: Array[ASCIIChar]): Unit = {
      val allowedCharSet: Array[Byte] = Array(
         0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
         0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
         0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
         0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
         0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
         0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
         0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
         0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
         0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
         0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
         0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
         0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
         0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
      )

      enc_String_CharIndex_private(max, UByte.fromArrayRaws(allowedCharSet), strVal)
   }

   def enc_IA5String_CharIndex_Internal_Field_Determinant(max: Long, min: Long, strVal: Array[ASCIIChar]): Unit = {
      val allowedCharSet: Array[Byte] = Array(
         0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
         0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
         0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
         0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
         0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
         0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
         0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
         0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
         0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
         0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
         0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
         0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
         0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
      )
      val strLen: Int = strVal.length
      encodeConstrainedWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_CharIndex_private(max, UByte.fromArrayRaws(allowedCharSet), strVal)
   }


   def dec_String_Ascii_private(max: Long, charactersToDecode: Long): Array[ASCIIChar] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0.toRawUByte)
      var i: Int = 0
      (while i < charactersToDecode do
         decreases(charactersToDecode - i)
         strVal(i) = readByte()
         i += 1
      ).invariant(true) // TODO

      strVal
   }


   def dec_String_Ascii_FixSize(max: Long): Array[ASCIIChar] = {
      dec_String_Ascii_private(max, max)
   }

   def dec_String_Ascii_Null_Terminated(max: Long, null_character: ASCIIChar): Array[ASCIIChar] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0.toRawUByte)
      var endReached = false
      var i: Int = 0
      (while i <= max && endReached do
         val decodedCharacter = readByte()
         if decodedCharacter != null_character then
            strVal(i) = decodedCharacter
            i += 1
         else
            strVal(i) = 0x0.toRawUByte
            endReached = true
      ).invariant(true) // TODO

      strVal
   }

   def dec_String_Ascii_Null_Terminated_mult(max: Long, null_character: Array[ASCIIChar], null_character_size: Int): Array[ASCIIChar] = {
      val tmp: Array[UByte] = Array.fill(null_character_size)(0.toRawUByte)
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0.toRawUByte)
      //read null_character_size characters into the tmp buffer

      var j: Int = 0
      (while j < null_character_size do
         decreases(null_character_size - j)
         tmp(j) = readByte()
         j += 1
      ).invariant(true) // TODO

      var i: Int = 0
      while i <= max && !arraySameElements(null_character, tmp) do
         strVal(i) = tmp(0)
         i += 1
         j = 0
         (while j < null_character_size - 1 do
            decreases(null_character_size - j)
            tmp(j) = tmp(j + 1)
            j += 1
         ).invariant(true) // TODO
         tmp(null_character_size - 1) = readByte()

      strVal(i) = 0x0.toRawUByte

      assert(arraySameElements(null_character, tmp))

      strVal
   }


   def dec_String_Ascii_External_Field_Determinant(max: Long, extSizeDeterminantFld: Long): Array[ASCIIChar] = {
      dec_String_Ascii_private(max, if extSizeDeterminantFld <= max then extSizeDeterminantFld else max)
   }

   def dec_String_Ascii_Internal_Field_Determinant(max: Long, min: Long): Array[ASCIIChar] = {
      val nCount = decodeConstrainedWholeNumber(min, max)
      dec_String_Ascii_private(max, if nCount <= max then nCount else max)
   }

   def dec_String_CharIndex_private(max: Long, charactersToDecode: Long, allowedCharSet: Array[UByte]): Array[ASCIIChar] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0.toRawUByte)
      var i: Int = 0
      (while i < charactersToDecode do
         decreases(charactersToDecode - i)
         strVal(i) = allowedCharSet(decodeConstrainedWholeNumber(0, allowedCharSet.length - 1).toInt)
         i += 1
      ).invariant(true) // TODO

      strVal
   }

   def dec_String_CharIndex_FixSize(max: Long, allowedCharSet: Array[ASCIIChar]): Array[ASCIIChar] = {
      dec_String_CharIndex_private(max, max, allowedCharSet)
   }

   def dec_String_CharIndex_External_Field_Determinant(max: Long, allowedCharSet: Array[ASCIIChar], extSizeDeterminantFld: Long): Array[ASCIIChar] = {
      dec_String_CharIndex_private(max, if extSizeDeterminantFld <= max then extSizeDeterminantFld else max, allowedCharSet)
   }


   def dec_String_CharIndex_Internal_Field_Determinant(max: Long, allowedCharSet: Array[ASCIIChar], min: Long): Array[ASCIIChar] = {
      val nCount = decodeConstrainedWholeNumber(min, max)
      dec_String_CharIndex_private(max, if nCount <= max then nCount else max, allowedCharSet)
   }


   def dec_IA5String_CharIndex_External_Field_Determinant(max: Long, extSizeDeterminantFld: Long): Array[ASCIIChar] = {
      val allowedCharSet: Array[Byte] = Array(
         0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
         0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
         0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
         0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
         0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
         0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
         0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
         0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
         0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
         0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
         0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
         0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
         0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
      )
      dec_String_CharIndex_private(max, if extSizeDeterminantFld <= max then extSizeDeterminantFld else max, UByte.fromArrayRaws(allowedCharSet))
   }

   def dec_IA5String_CharIndex_Internal_Field_Determinant(max: Long, min: Long): Array[ASCIIChar] = {
      val allowedCharSet: Array[Byte] = Array(
         0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
         0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
         0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
         0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
         0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
         0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
         0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
         0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
         0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
         0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
         0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
         0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
         0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
      )
      val nCount = decodeConstrainedWholeNumber(min, max)
      dec_String_CharIndex_private(max, if nCount <= max then nCount else max, UByte.fromArrayRaws(allowedCharSet))
   }


   /* Length Determinant functions*/
   def enc_Length(lengthValue: ULong, lengthSizeInBits: Int): Unit = {
      /* encode length */
      enc_Int_PositiveInteger_ConstSize(lengthValue, lengthSizeInBits)
   }

   def dec_Length(lengthSizeInBits: Int): ULong = {
      dec_Int_PositiveInteger_ConstSize(lengthSizeInBits)
   }

   def milbus_encode(v: Long): Long = {
      if v == 32 then 0 else v
   }

   def milbus_decode(v: Long): Long = {
      if v == 0 then 32 else v
   }

   def dec_Int_PositiveInteger_ConstSizeUInt8(encodedSizeInBits: Int): UByte = dec_Int_PositiveInteger_ConstSize(encodedSizeInBits).toUByte
   def dec_Int_PositiveInteger_ConstSizeUInt16(encodedSizeInBits: Int): UShort = dec_Int_PositiveInteger_ConstSize(encodedSizeInBits).toUShort
   def dec_Int_PositiveInteger_ConstSizeUInt32(encodedSizeInBits: Int): UInt = dec_Int_PositiveInteger_ConstSize(encodedSizeInBits).toUInt
   def dec_Int_PositiveInteger_ConstSize_8UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_8().toUByte
   def dec_Int_PositiveInteger_ConstSize_big_endian_16UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_big_endian_16().toUShort
   def dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_big_endian_16().toUByte
   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt32(): UInt = dec_Int_PositiveInteger_ConstSize_big_endian_32().toUInt
   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_big_endian_32().toUShort
   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_big_endian_32().toUByte
   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt32(): UInt = dec_Int_PositiveInteger_ConstSize_big_endian_64().toUInt
   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_big_endian_64().toUShort
   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_big_endian_64().toUByte
   def dec_Int_PositiveInteger_ConstSize_little_endian_16UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_little_endian_16().toUShort
   def dec_Int_PositiveInteger_ConstSize_little_endian_16UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_little_endian_16().toUByte
   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt32(): UInt = dec_Int_PositiveInteger_ConstSize_little_endian_32().toUInt
   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_little_endian_32().toUShort
   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_little_endian_32().toUByte
   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt32(): UInt = dec_Int_PositiveInteger_ConstSize_little_endian_64().toUInt
   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_little_endian_64().toUShort
   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_little_endian_64().toUByte
   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt8(): UByte = dec_Int_PositiveInteger_VarSize_LengthEmbedded().toUByte
   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt16(): UShort = dec_Int_PositiveInteger_VarSize_LengthEmbedded().toUShort
   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt32(): UInt = dec_Int_PositiveInteger_VarSize_LengthEmbedded().toUInt
   def dec_Int_TwosComplement_ConstSizeInt8(encodedSizeInBits: Int): Byte = dec_Int_TwosComplement_ConstSize(encodedSizeInBits).toByte
   def dec_Int_TwosComplement_ConstSizeInt16(encodedSizeInBits: Int): Short = dec_Int_TwosComplement_ConstSize(encodedSizeInBits).toShort
   def dec_Int_TwosComplement_ConstSizeInt32(encodedSizeInBits: Int): Int = dec_Int_TwosComplement_ConstSize(encodedSizeInBits).toInt
   def dec_Int_TwosComplement_ConstSize_8Int8(): Byte = dec_Int_TwosComplement_ConstSize_8().toByte
   def dec_Int_TwosComplement_ConstSize_big_endian_16Int16(): Short = dec_Int_TwosComplement_ConstSize_big_endian_16().toShort
   def dec_Int_TwosComplement_ConstSize_big_endian_16Int8(): Byte = dec_Int_TwosComplement_ConstSize_big_endian_16().toByte
   def dec_Int_TwosComplement_ConstSize_big_endian_32Int32(): Int = dec_Int_TwosComplement_ConstSize_big_endian_32().toInt
   def dec_Int_TwosComplement_ConstSize_big_endian_32Int16(): Short = dec_Int_TwosComplement_ConstSize_big_endian_32().toShort
   def dec_Int_TwosComplement_ConstSize_big_endian_32Int8(): Byte = dec_Int_TwosComplement_ConstSize_big_endian_32().toByte
   def dec_Int_TwosComplement_ConstSize_big_endian_64Int32(): Int = dec_Int_TwosComplement_ConstSize_big_endian_64().toInt
   def dec_Int_TwosComplement_ConstSize_big_endian_64Int16(): Short = dec_Int_TwosComplement_ConstSize_big_endian_64().toShort
   def dec_Int_TwosComplement_ConstSize_big_endian_64Int8(): Byte = dec_Int_TwosComplement_ConstSize_big_endian_64().toByte
   def dec_Int_TwosComplement_ConstSize_little_endian_16Int16(): Short = dec_Int_TwosComplement_ConstSize_little_endian_16().toShort
   def dec_Int_TwosComplement_ConstSize_little_endian_16Int8(): Byte = dec_Int_TwosComplement_ConstSize_little_endian_16().toByte
   def dec_Int_TwosComplement_ConstSize_little_endian_32Int32(): Int = dec_Int_TwosComplement_ConstSize_little_endian_32().toInt
   def dec_Int_TwosComplement_ConstSize_little_endian_32Int16(): Short = dec_Int_TwosComplement_ConstSize_little_endian_32().toShort
   def dec_Int_TwosComplement_ConstSize_little_endian_32Int8(): Byte = dec_Int_TwosComplement_ConstSize_little_endian_32().toByte
   def dec_Int_TwosComplement_ConstSize_little_endian_64Int32(): Int = dec_Int_TwosComplement_ConstSize_little_endian_64().toInt
   def dec_Int_TwosComplement_ConstSize_little_endian_64Int16(): Short = dec_Int_TwosComplement_ConstSize_little_endian_64().toShort
   def dec_Int_TwosComplement_ConstSize_little_endian_64Int8(): Byte = dec_Int_TwosComplement_ConstSize_little_endian_64().toByte
   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt8(): Byte = dec_Int_TwosComplement_VarSize_LengthEmbedded().toByte
   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt16(): Short = dec_Int_TwosComplement_VarSize_LengthEmbedded().toShort
   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt32(): Int = dec_Int_TwosComplement_VarSize_LengthEmbedded().toInt
   def dec_Int_BCD_ConstSizeUInt8(encodedSizeInNibbles: Int): UByte = dec_Int_BCD_ConstSize(encodedSizeInNibbles).toUByte
   def dec_Int_BCD_ConstSizeUInt16(encodedSizeInNibbles: Int): UShort = dec_Int_BCD_ConstSize(encodedSizeInNibbles).toUShort
   def dec_Int_BCD_ConstSizeUInt32(encodedSizeInNibbles: Int): UInt = dec_Int_BCD_ConstSize(encodedSizeInNibbles).toUInt
   def dec_Int_BCD_VarSize_LengthEmbeddedUInt8(): UByte = dec_Int_BCD_VarSize_LengthEmbedded().toUByte
   def dec_Int_BCD_VarSize_LengthEmbeddedUInt16(): UShort = dec_Int_BCD_VarSize_LengthEmbedded().toUShort
   def dec_Int_BCD_VarSize_LengthEmbeddedUInt32(): UInt = dec_Int_BCD_VarSize_LengthEmbedded().toUInt
   def dec_Int_BCD_VarSize_NullTerminatedUInt8(): UByte = dec_Int_BCD_VarSize_NullTerminated().toUByte
   def dec_Int_BCD_VarSize_NullTerminatedUInt16(): UShort = dec_Int_BCD_VarSize_NullTerminated().toUShort
   def dec_Int_BCD_VarSize_NullTerminatedUInt32(): UInt = dec_Int_BCD_VarSize_NullTerminated().toUInt
   def dec_SInt_ASCII_ConstSizeInt8(encodedSizeInBytes: Int): Byte = dec_SInt_ASCII_ConstSize(encodedSizeInBytes).toByte
   def dec_SInt_ASCII_ConstSizeInt16(encodedSizeInBytes: Int): Short = dec_SInt_ASCII_ConstSize(encodedSizeInBytes).toShort
   def dec_SInt_ASCII_ConstSizeInt32(encodedSizeInBytes: Int): Int = dec_SInt_ASCII_ConstSize(encodedSizeInBytes).toInt
   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt8(): Byte = dec_SInt_ASCII_VarSize_LengthEmbedded().toByte
   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt16(): Short = dec_SInt_ASCII_VarSize_LengthEmbedded().toShort
   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt32(): Int = dec_SInt_ASCII_VarSize_LengthEmbedded().toInt
   def dec_SInt_ASCII_VarSize_NullTerminatedInt8(null_characters: Array[Byte], null_characters_size: Int): Byte =
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toByte
   def dec_SInt_ASCII_VarSize_NullTerminatedInt16(null_characters: Array[Byte], null_characters_size: Int): Short =
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toShort
   def dec_SInt_ASCII_VarSize_NullTerminatedInt32(null_characters: Array[Byte], null_characters_size: Int): Int =
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toInt
   def dec_UInt_ASCII_ConstSizeUInt8(encodedSizeInBytes: Int): UByte = dec_UInt_ASCII_ConstSize(encodedSizeInBytes).toUByte
   def dec_UInt_ASCII_ConstSizeUInt16(encodedSizeInBytes: Int): UShort = dec_UInt_ASCII_ConstSize(encodedSizeInBytes).toUShort
   def dec_UInt_ASCII_ConstSizeUInt32(encodedSizeInBytes: Int): UInt = dec_UInt_ASCII_ConstSize(encodedSizeInBytes).toUInt
   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt8(): UByte = dec_UInt_ASCII_VarSize_LengthEmbedded().toUByte
   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt16(): UShort = dec_UInt_ASCII_VarSize_LengthEmbedded().toUShort
   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt32(): UInt = dec_UInt_ASCII_VarSize_LengthEmbedded().toUInt
   def dec_UInt_ASCII_VarSize_NullTerminatedUInt8(null_characters: Array[Byte], null_characters_size: Int): UByte =
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toUByte
   def dec_UInt_ASCII_VarSize_NullTerminatedUInt16(null_characters: Array[Byte], null_characters_size: Int): UShort =
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toUShort
   def dec_UInt_ASCII_VarSize_NullTerminatedUInt32(null_characters: Array[Byte], null_characters_size: Int): UInt =
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toUInt
}
